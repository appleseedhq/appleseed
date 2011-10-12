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

/***********************************************************
*
* Test program:	 tattr
*
* Test the attribute functionality
*
*************************************************************/

#include "testhdf5.h"
#include "h5test.h"
#include "hdf5.h"

/*
 * This file needs to access private information from the H5O package.
 * This file also needs to access the object header testing code.
 */
#define H5O_PACKAGE
#define H5O_TESTING
#include "H5Opkg.h"		/* Object headers 			*/

/*
 * This file needs to access private information from the H5A package.
 * This file also needs to access the attribute testing code.
 */
#define H5A_PACKAGE
#define H5A_TESTING
#include "H5Apkg.h"		/* Attributes	 			*/

/*
 * This file needs to access private information from the H5F package.
 * This file also needs to access the file testing code.
 */
#define H5F_PACKAGE
#define H5F_TESTING
#include "H5Fpkg.h"		/* File access	 			*/

#define FILENAME   "tattr.h5"
#define NAME_BUF_SIZE   1024
#define ATTR_NAME_LEN   16
#define ATTR_MAX_DIMS   7
#define ATTR_TMP_NAME   "a really long temp_name"
#define CORDER_ITER_STOP            3

/* 3-D dataset with fixed dimensions */
#define SPACE1_RANK	3
#define SPACE1_DIM1	3
#define SPACE1_DIM2	15
#define SPACE1_DIM3	13

/* Dataset Information */
#define DSET1_NAME "Dataset1"
#define DSET2_NAME "Dataset2"
#define DSET3_NAME "Dataset3"
#define NUM_DSETS       3

/* Group Information */
#define GROUP1_NAME "/Group1"
#define GROUP2_NAME "/Group2"
#define GROUP3_NAME "/Group3"

/* Named Datatype Information */
#define TYPE1_NAME "/Type"

/* Attribute Rank & Dimensions */
#define ATTR1_NAME  "Attr1"
#define ATTR1_RANK	1
#define ATTR1_DIM1	3
int attr_data1[ATTR1_DIM1]={512,-234,98123}; /* Test data for 1st attribute */

/* rank & dimensions for another attribute */
#define ATTR1A_NAME  "Attr1_a"
int attr_data1a[ATTR1_DIM1]={256,11945,-22107};

#define ATTR2_NAME  "Attr2"
#define ATTR2_RANK	2
#define ATTR2_DIM1	2
#define ATTR2_DIM2	2
int attr_data2[ATTR2_DIM1][ATTR2_DIM2]={{7614,-416},{197814,-3}}; /* Test data for 2nd attribute */

#define ATTR3_NAME  "Attr3"
#define ATTR3_RANK	3
#define ATTR3_DIM1	2
#define ATTR3_DIM2	2
#define ATTR3_DIM3	2
double attr_data3[ATTR3_DIM1][ATTR3_DIM2][ATTR3_DIM3]={{{2.3,-26.1},{0.123,-10.0}},{{973.23,-0.91827},{2.0,23.0}}}; /* Test data for 3rd attribute */

#define ATTR4_NAME  "Attr4"
#define ATTR4_RANK	2
#define ATTR4_DIM1	2
#define ATTR4_DIM2	2
#define ATTR4_FIELDNAME1	"i"
#define ATTR4_FIELDNAME2	"d"
#define ATTR4_FIELDNAME3	"c"
size_t attr4_field1_off=0;
size_t attr4_field2_off=0;
size_t attr4_field3_off=0;
struct attr4_struct {
    int i;
    double d;
    char c;
 } attr_data4[ATTR4_DIM1][ATTR4_DIM2]={{{3,-26.1,'d'},{-100000, 0.123,'3'}},
    {{-23,981724.2,'Q'},{0,2.0,'\n'}}}; /* Test data for 4th attribute */

#define ATTR5_NAME  "Attr5"
#define ATTR5_RANK	0
float attr_data5=(float)-5.123;        /* Test data for 5th attribute */

#define ATTR6_RANK	3
#define ATTR6_DIM1	100
#define ATTR6_DIM2	100
#define ATTR6_DIM3	100

#define ATTR7_NAME      "attr 1 - 000000"
#define ATTR8_NAME      "attr 2"

#define NATTR_MANY_OLD  350
#define NATTR_MANY_NEW  35000

#define BUG2_NATTR  100
#define BUG2_NATTR2 16

#define BUG3_DSET_NAME  "dset"
#define BUG3_DT_NAME    "dt"
#define BUG3_ATTR_NAME  "attr"

/* Attribute iteration struct */
typedef struct {
    H5_iter_order_t order;      /* Direction of iteration */
    unsigned ncalled;           /* # of times callback is entered */
    unsigned nskipped;          /* # of attributes skipped */
    int stop;                   /* # of iterations to stop after */
    int64_t curr;               /* Current creation order value */
    size_t max_visit;           /* Size of "visited attribute" flag array */
    hbool_t *visited;           /* Pointer to array of "visited attribute" flags */
} attr_iter_info_t;

static herr_t attr_op1(hid_t loc_id, const char *name, const H5A_info_t *ainfo,
    void *op_data);



/****************************************************************
**
**  test_attr_basic_write(): Test basic H5A (attribute) code.
**      Tests integer attributes on both datasets and groups
**
****************************************************************/
static void
test_attr_basic_write(hid_t fapl)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		group;	    /* Group ID			    */
    hid_t		sid1,sid2;	/* Dataspace ID			*/
    hid_t		attr, attr2;	    /* Attribute ID		*/
    hsize_t             attr_size;  /* storage size for attribute       */
    ssize_t             attr_name_size; /* size of attribute name       */
    char                *attr_name=NULL;    /* name of attribute        */
    hsize_t		dims1[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
    hsize_t		dims2[] = {ATTR1_DIM1};
    hsize_t		dims3[] = {ATTR2_DIM1,ATTR2_DIM2};
    int       read_data1[ATTR1_DIM1]={0}; /* Buffer for reading 1st attribute */
    int         i;
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Scalar Attribute Writing Functions\n"));

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for dataset */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, DSET1_NAME, H5T_NATIVE_UCHAR, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Create dataspace for attribute */
    sid2 = H5Screate_simple(ATTR1_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Try to create an attribute on the file (should create an attribute on root group) */
    attr = H5Acreate2(fid1, ATTR1_NAME, H5T_NATIVE_INT, sid2, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate2");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Open the root group */
    group = H5Gopen2(fid1, "/", H5P_DEFAULT);
    CHECK(group, FAIL, "H5Gopen2");

    /* Open attribute again */
    attr = H5Aopen(group, ATTR1_NAME, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Aopen");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close root group */
    ret = H5Gclose(group);
    CHECK(ret, FAIL, "H5Gclose");

    /* Create an attribute for the dataset */
    attr = H5Acreate2(dataset, ATTR1_NAME, H5T_NATIVE_INT, sid2, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate2");

    /* Try to create the same attribute again (should fail) */
    ret = H5Acreate2(dataset, ATTR1_NAME, H5T_NATIVE_INT, sid2, H5P_DEFAULT, H5P_DEFAULT);
    VERIFY(ret, FAIL, "H5Acreate2");

    /* Write attribute information */
    ret = H5Awrite(attr, H5T_NATIVE_INT, attr_data1);
    CHECK(ret, FAIL, "H5Awrite");

    /* Create an another attribute for the dataset */
    attr2 = H5Acreate2(dataset, ATTR1A_NAME, H5T_NATIVE_INT, sid2, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate2");

    /* Write attribute information */
    ret = H5Awrite(attr2, H5T_NATIVE_INT, attr_data1a);
    CHECK(ret, FAIL, "H5Awrite");

    /* Check storage size for attribute */
    attr_size = H5Aget_storage_size(attr);
    VERIFY(attr_size, (ATTR1_DIM1 * sizeof(int)), "H5A_get_storage_size");

    /* Read attribute information immediately, without closing attribute */
    ret = H5Aread(attr, H5T_NATIVE_INT, read_data1);
    CHECK(ret, FAIL, "H5Aread");

    /* Verify values read in */
    for(i = 0; i < ATTR1_DIM1; i++)
        if(attr_data1[i] != read_data1[i])
            TestErrPrintf("%d: attribute data different: attr_data1[%d]=%d, read_data1[%d]=%d\n", __LINE__, i, attr_data1[i], i, read_data1[i]);

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close attribute */
    ret = H5Aclose(attr2);
    CHECK(ret, FAIL, "H5Aclose");

    /* change attribute name */
    ret = H5Arename(dataset, ATTR1_NAME, ATTR_TMP_NAME);
    CHECK(ret, FAIL, "H5Arename");

    /* Open attribute again */
    attr = H5Aopen(dataset, ATTR_TMP_NAME, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Aopen");

    /* Verify new attribute name */
    attr_name_size = H5Aget_name(attr, (size_t)0, NULL);
    CHECK(attr_name_size, FAIL, "H5Aget_name");

    if(attr_name_size > 0) {
        attr_name = (char*)HDcalloc((size_t)(attr_name_size + 1), sizeof(char));
        CHECK(attr_name, NULL, "HDcalloc");
        
        if(attr_name) {
            ret = (herr_t)H5Aget_name(attr, (size_t)(attr_name_size + 1), attr_name);
            CHECK(ret, FAIL, "H5Aget_name");
            ret = HDstrcmp(attr_name, ATTR_TMP_NAME);
            VERIFY(ret, 0, "HDstrcmp");

            HDfree(attr_name);
            attr_name = NULL;
        } /* end if */
    } /* end if */

    /* Read attribute information immediately, without closing attribute */
    ret = H5Aread(attr, H5T_NATIVE_INT, read_data1);
    CHECK(ret, FAIL, "H5Aread");

    /* Verify values read in */
    for(i = 0; i < ATTR1_DIM1; i++)
        if(attr_data1[i] != read_data1[i])
            TestErrPrintf("%d: attribute data different: attr_data1[%d]=%d, read_data1[%d]=%d\n", __LINE__, i, attr_data1[i], i, read_data1[i]);

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Open the second attribute again */
    attr2 = H5Aopen(dataset, ATTR1A_NAME, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Aopen");

    /* Verify new attribute name */
    attr_name_size = H5Aget_name(attr2, (size_t)0, NULL);
    CHECK(attr_name_size, FAIL, "H5Aget_name");

    if(attr_name_size > 0) {
        attr_name = (char*)HDcalloc((size_t)(attr_name_size+1), sizeof(char));
        CHECK(attr_name, NULL, "HDcalloc");
                
        if(attr_name) {
            ret = (herr_t)H5Aget_name(attr2, (size_t)(attr_name_size + 1), attr_name);
            CHECK(ret, FAIL, "H5Aget_name");
            ret = HDstrcmp(attr_name, ATTR1A_NAME);
            VERIFY(ret, 0, "HDstrcmp");

            HDfree(attr_name);
            attr_name = NULL;
        } /* end if */
    } /* end if */

    /* Read attribute information immediately, without closing attribute */
    ret = H5Aread(attr2, H5T_NATIVE_INT, read_data1);
    CHECK(ret, FAIL, "H5Aread");

    /* Verify values read in */
    for(i = 0; i < ATTR1_DIM1; i++)
        if(attr_data1a[i] != read_data1[i])
            TestErrPrintf("%d: attribute data different: attr_data1a[%d]=%d, read_data1[%d]=%d\n", __LINE__, i, attr_data1a[i], i, read_data1[i]);

    /* Close attribute */
    ret = H5Aclose(attr2);
    CHECK(ret, FAIL, "H5Aclose");

    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create group */
    group = H5Gcreate2(fid1, GROUP1_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(group, FAIL, "H5Gcreate2");

    /* Create dataspace for attribute */
    sid2 = H5Screate_simple(ATTR2_RANK, dims3, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Create an attribute for the group */
    attr = H5Acreate2(group, ATTR2_NAME, H5T_NATIVE_INT, sid2, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate2");

    /* Check storage size for attribute */
    attr_size = H5Aget_storage_size(attr);
    VERIFY(attr_size, (ATTR2_DIM1 * ATTR2_DIM2 * sizeof(int)), "H5Aget_storage_size");

    /* Try to create the same attribute again (should fail) */
    ret = H5Acreate2(group, ATTR2_NAME, H5T_NATIVE_INT, sid2, H5P_DEFAULT, H5P_DEFAULT);
    VERIFY(ret, FAIL, "H5Acreate2");

    /* Write attribute information */
    ret = H5Awrite(attr, H5T_NATIVE_INT, attr_data2);
    CHECK(ret, FAIL, "H5Awrite");

    /* Check storage size for attribute */
    attr_size = H5Aget_storage_size(attr);
    VERIFY(attr_size, (ATTR2_DIM1 * ATTR2_DIM2 * sizeof(int)), "H5A_get_storage_size");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close Attribute dataspace */
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Group */
    ret = H5Gclose(group);
    CHECK(ret, FAIL, "H5Gclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_basic_write() */


/****************************************************************
**
**  test_attr_basic_read(): Test basic H5A (attribute) code.
**
****************************************************************/
static void
test_attr_basic_read(hid_t fapl)
{
    hid_t	fid1;		/* HDF5 File IDs		*/
    hid_t	dataset;	/* Dataset ID			*/
    hid_t	group;	        /* Group ID			*/
    hid_t	attr;	        /* Attribute ID			*/
    H5O_info_t  oinfo;          /* Object info                  */
    int         read_data1[ATTR1_DIM1] = {0}; /* Buffer for reading 1st attribute */
    int         read_data2[ATTR2_DIM1][ATTR2_DIM2] = {{0}}; /* Buffer for reading 2nd attribute */
    int         i, j;           /* Local index variables        */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Attribute Functions\n"));

    /* Create file */
    fid1 = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Open the dataset */
    dataset = H5Dopen2(fid1, DSET1_NAME, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dopen2");

    /* Verify the correct number of attributes */
    ret = H5Oget_info(dataset, &oinfo);
    CHECK(ret, FAIL, "H5Oget_info");
    VERIFY(oinfo.num_attrs, 2, "H5Oget_info");

    /* Open first attribute for the dataset */
    attr = H5Aopen(dataset, ATTR_TMP_NAME, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Aopen");

    /* Read attribute information */
    ret = H5Aread(attr, H5T_NATIVE_INT, read_data1);
    CHECK(ret, FAIL, "H5Aread");

    /* Verify values read in */
    for(i = 0; i < ATTR1_DIM1; i++)
        if(attr_data1[i] != read_data1[i])
            TestErrPrintf("%d: attribute data different: attr_data1[%d]=%d, read_data1[%d]=%d\n", __LINE__, i, attr_data1[i], i, read_data1[i]);

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Open the group */
    group = H5Gopen2(fid1, GROUP1_NAME, H5P_DEFAULT);
    CHECK(group, FAIL, "H5Gopen2");

    /* Verify the correct number of attributes */
    ret = H5Oget_info(group, &oinfo);
    CHECK(ret, FAIL, "H5Oget_info");
    VERIFY(oinfo.num_attrs, 1, "H5Oget_info");

    /* Open the attribute for the group */
    attr = H5Aopen(group, ATTR2_NAME, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Aopen");

    /* Read attribute information */
    ret = H5Aread(attr, H5T_NATIVE_INT, read_data2);
    CHECK(ret, FAIL, "H5Aread");

    /* Verify values read in */
    for(i = 0; i < ATTR2_DIM1; i++)
        for(j = 0; j < ATTR2_DIM2; j++)
            if(attr_data2[i][j] != read_data2[i][j])
                TestErrPrintf("%d: attribute data different: attr_data2[%d][%d]=%d, read_data2[%d][%d]=%d\n", __LINE__,  i, j, attr_data2[i][j], i, j, read_data1[i]);

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close group */
    ret = H5Gclose(group);
    CHECK(ret, FAIL, "H5Gclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_basic_read() */


/****************************************************************
**
**  test_attr_flush(): Test H5A (attribute) code for performing
**                      I/O when H5Fflush is used.
**
****************************************************************/
static void
test_attr_flush(hid_t fapl)
{
    hid_t fil,          /* File ID */
        att,            /* Attribute ID */
        spc,            /* Dataspace ID */
        set;            /* Dataset ID */
    double wdata=3.14159;       /* Data to write */
    double rdata;       /* Data read in */
    herr_t ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Attribute Flushing\n"));

    fil = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(fil, FAIL, "H5Fcreate");

    spc = H5Screate(H5S_SCALAR);
    CHECK(spc, FAIL, "H5Screate");

    set = H5Dcreate2(fil, DSET1_NAME, H5T_NATIVE_DOUBLE, spc, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(set, FAIL, "H5Dcreate2");

    att = H5Acreate2(set, ATTR1_NAME, H5T_NATIVE_DOUBLE, spc, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(att, FAIL, "H5Acreate2");

    ret=H5Aread(att, H5T_NATIVE_DOUBLE, &rdata);
    CHECK(ret, FAIL, "H5Awrite");

    if(!DBL_ABS_EQUAL(rdata,0.0))
        TestErrPrintf("attribute value wrong: rdata=%f, should be %f\n",rdata,0.0);

    ret=H5Fflush(fil, H5F_SCOPE_GLOBAL);
    CHECK(ret, FAIL, "H5Fflush");

    ret=H5Aread(att, H5T_NATIVE_DOUBLE, &rdata);
    CHECK(ret, FAIL, "H5Awrite");

    if(!DBL_ABS_EQUAL(rdata,0.0))
        TestErrPrintf("attribute value wrong: rdata=%f, should be %f\n",rdata,0.0);

    ret=H5Awrite(att, H5T_NATIVE_DOUBLE, &wdata);
    CHECK(ret, FAIL, "H5Awrite");

    ret=H5Aread(att, H5T_NATIVE_DOUBLE, &rdata);
    CHECK(ret, FAIL, "H5Awrite");

    if(!DBL_ABS_EQUAL(rdata,wdata))
        TestErrPrintf("attribute value wrong: rdata=%f, should be %f\n",rdata,wdata);

    ret=H5Sclose(spc);
    CHECK(ret, FAIL, "H5Sclose");
    ret=H5Aclose(att);
    CHECK(ret, FAIL, "H5Aclose");
    ret=H5Dclose(set);
    CHECK(ret, FAIL, "H5Dclose");
    ret=H5Fclose(fil);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_flush() */


/****************************************************************
**
**  test_attr_plist(): Test Attribute Creation Property Lists
**
****************************************************************/
static void
test_attr_plist(hid_t fapl)
{
    hid_t		fid1;           /* HDF5 File IDs		*/
    hid_t		dataset;        /* Dataset ID			*/
    hid_t		sid1,sid2;      /* Dataspace ID			*/
    hid_t		attr;	        /* Attribute ID		*/
    hid_t               plist;          /* Property list ID             */
    hsize_t		dims1[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
    hsize_t		dims2[] = {ATTR1_DIM1};
    H5T_cset_t          cset;           /* Character set for attributes */
    herr_t		ret;            /* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Attribute Property Lists\n"));

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for dataset */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, DSET1_NAME, H5T_NATIVE_UCHAR, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Create dataspace for attribute */
    sid2 = H5Screate_simple(ATTR1_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Create default property list for attribute */
    plist = H5Pcreate(H5P_ATTRIBUTE_CREATE);
    CHECK(plist, FAIL, "H5Pcreate");

    /* Get the character encoding and ensure that it is the default (ASCII) */
    ret = H5Pget_char_encoding(plist, &cset);
    CHECK(ret, FAIL, "H5Pget_char_encoding");
    VERIFY(cset, H5T_CSET_ASCII, "H5Pget_char_encoding");

    /* Create an attribute for the dataset using the property list */
    attr = H5Acreate2(dataset, ATTR1_NAME, H5T_NATIVE_INT, sid2, plist, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate2");

    /* Close the property list, and get the attribute's property list */
    ret = H5Pclose(plist);
    CHECK(ret, FAIL, "H5Pclose");
    plist = H5Aget_create_plist(attr);
    CHECK(plist, FAIL, "H5Aget_create_plist");

    /* Get the character encoding and ensure that it is the default (ASCII) */
    ret = H5Pget_char_encoding(plist, &cset);
    CHECK(ret, FAIL, "H5Pget_char_encoding");
    VERIFY(cset, H5T_CSET_ASCII, "H5Pget_char_encoding");

    /* Close the property list and attribute */
    ret = H5Pclose(plist);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Create a new property list and modify it to use a different encoding */
    plist = H5Pcreate(H5P_ATTRIBUTE_CREATE);
    CHECK(plist, FAIL, "H5Pcreate");
    ret = H5Pset_char_encoding(plist, H5T_CSET_UTF8);
    CHECK(ret, FAIL, "H5Pset_char_encoding");

    /* Get the character encoding and ensure that it has been changed */
    ret = H5Pget_char_encoding(plist, &cset);
    CHECK(ret, FAIL, "H5Pget_char_encoding");
    VERIFY(cset, H5T_CSET_UTF8, "H5Pget_char_encoding");

    /* Create an attribute for the dataset using the modified property list */
    attr = H5Acreate2(dataset, ATTR2_NAME, H5T_NATIVE_INT, sid2, plist, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate2");

    /* Close the property list and attribute */
    ret = H5Pclose(plist);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Re-open the second attribute and ensure that its character encoding is correct */
    attr = H5Aopen(dataset, ATTR2_NAME, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Aopen");
    plist = H5Aget_create_plist(attr);
    CHECK(plist, FAIL, "H5Aget_create_plist");
    ret = H5Pget_char_encoding(plist, &cset);
    CHECK(ret, FAIL, "H5Pget_char_encoding");
    VERIFY(cset, H5T_CSET_UTF8, "H5Pget_char_encoding");

    /* Close everything */
    ret=H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");
    ret=H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Pclose(plist);
    CHECK(ret, FAIL, "H5Pclose");
    ret=H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");
    ret=H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");
    ret=H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
}  /* test_attr_plist() */


/****************************************************************
**
**  test_attr_compound_write(): Test H5A (attribute) code.
**      Tests compound datatype attributes
**
****************************************************************/
static void
test_attr_compound_write(hid_t fapl)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t       tid1;       /* Attribute datatype ID */
    hid_t		sid1,sid2;	/* Dataspace ID			*/
    hid_t		attr;	    /* Attribute ID			*/
    hsize_t		dims1[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
    hsize_t		dims2[] = {ATTR4_DIM1,ATTR4_DIM2};
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Multiple Attribute Functions\n"));

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for dataset */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, DSET1_NAME, H5T_NATIVE_UCHAR, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Close dataset's dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Create the attribute datatype.  */
    tid1 = H5Tcreate(H5T_COMPOUND, sizeof(struct attr4_struct));
    CHECK(tid1, FAIL, "H5Tcreate");
    attr4_field1_off = HOFFSET(struct attr4_struct, i);
    ret = H5Tinsert(tid1, ATTR4_FIELDNAME1, attr4_field1_off, H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");
    attr4_field2_off = HOFFSET(struct attr4_struct, d);
    ret = H5Tinsert(tid1, ATTR4_FIELDNAME2, attr4_field2_off, H5T_NATIVE_DOUBLE);
    CHECK(ret, FAIL, "H5Tinsert");
    attr4_field3_off = HOFFSET(struct attr4_struct, c);
    ret = H5Tinsert(tid1, ATTR4_FIELDNAME3, attr4_field3_off, H5T_NATIVE_SCHAR);
    CHECK(ret, FAIL, "H5Tinsert");

    /* Create dataspace for 1st attribute */
    sid2 = H5Screate_simple(ATTR4_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Create complex attribute for the dataset */
    attr = H5Acreate2(dataset, ATTR4_NAME, tid1, sid2, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate2");

    /* Try to create the same attribute again (should fail) */
    ret = H5Acreate2(dataset, ATTR4_NAME, tid1, sid2, H5P_DEFAULT, H5P_DEFAULT);
    VERIFY(ret, FAIL, "H5Acreate2");

    /* Write complex attribute data */
    ret = H5Awrite(attr, tid1, attr_data4);
    CHECK(ret, FAIL, "H5Awrite");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close attribute's dataspace */
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close attribute's datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_compound_write() */


/****************************************************************
**
**  test_attr_compound_read(): Test basic H5A (attribute) code.
**
****************************************************************/
static void
test_attr_compound_read(hid_t fapl)
{
    hid_t   fid1;	/* HDF5 File ID		*/
    hid_t   dataset;	/* Dataset ID		*/
    hid_t   space;      /* Attribute dataspace  */
    hid_t   type;       /* Attribute datatype   */
    hid_t   attr;	/* Attribute ID         */
    char    attr_name[ATTR_NAME_LEN]; /* Buffer for attribute names */
    int     rank;       /* Attribute rank */
    hsize_t dims[ATTR_MAX_DIMS];    /* Attribute dimensions */
    H5T_class_t t_class;  /* Attribute datatype class */
    H5T_order_t order;  /* Attribute datatype order */
    size_t      size;   /* Attribute datatype size as stored in file */
    int     fields;     /* # of Attribute datatype fields */
    char *fieldname;    /* Name of a field */
    size_t  offset;     /* Attribute datatype field offset */
    hid_t   field;      /* Attribute field datatype */
    struct attr4_struct read_data4[ATTR4_DIM1][ATTR4_DIM2]; /* Buffer for reading 4th attribute */
    size_t  name_len;   /* Length of attribute name */
    H5O_info_t oinfo;   /* Object info */
    int     i, j;       /* Local index variables */
    herr_t  ret;	/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Attribute Functions\n"));

    /* Open file */
    fid1 = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Open the dataset */
    dataset = H5Dopen2(fid1, DSET1_NAME, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dopen2");

    /* Verify the correct number of attributes */
    ret = H5Oget_info(dataset, &oinfo);
    CHECK(ret, FAIL, "H5Oget_info");
    VERIFY(oinfo.num_attrs, 1, "H5Oget_info");

    /* Open 1st attribute for the dataset */
    attr = H5Aopen_by_idx(dataset, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)0, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Aopen_by_idx");

    /* Verify Dataspace */
    space = H5Aget_space(attr);
    CHECK(space, FAIL, "H5Aget_space");
    rank = H5Sget_simple_extent_ndims(space);
    VERIFY(rank, ATTR4_RANK, "H5Sget_simple_extent_ndims");
    ret = H5Sget_simple_extent_dims(space, dims, NULL);
    CHECK(ret, FAIL, "H5Sget_simple_extent_dims");
    if(dims[0] != ATTR4_DIM1)
        TestErrPrintf("attribute dimensions different: dims[0]=%d, should be %d\n", (int)dims[0], ATTR4_DIM1);
    if(dims[1] != ATTR4_DIM2)
        TestErrPrintf("attribute dimensions different: dims[1]=%d, should be %d\n", (int)dims[1], ATTR4_DIM2);
    H5Sclose(space);

    /* Verify Datatype */
    type = H5Aget_type(attr);
    CHECK(type, FAIL, "H5Aget_type");
    t_class = H5Tget_class(type);
    VERIFY(t_class, H5T_COMPOUND, "H5Tget_class");
    fields = H5Tget_nmembers(type);
    VERIFY(fields, 3, "H5Tget_nmembers");
    for(i = 0; i < fields; i++) {
        fieldname = H5Tget_member_name(type, (unsigned)i);
        if(!(HDstrcmp(fieldname, ATTR4_FIELDNAME1) ||
                HDstrcmp(fieldname, ATTR4_FIELDNAME2) ||
                HDstrcmp(fieldname, ATTR4_FIELDNAME3)))
            TestErrPrintf("invalid field name for field #%d: %s\n", i, fieldname);
        free(fieldname);
      } /* end for */
    offset = H5Tget_member_offset(type, 0);
    VERIFY(offset, attr4_field1_off, "H5Tget_member_offset");
    offset = H5Tget_member_offset(type, 1);
    VERIFY(offset, attr4_field2_off, "H5Tget_member_offset");
    offset = H5Tget_member_offset(type, 2);
    VERIFY(offset, attr4_field3_off, "H5Tget_member_offset");

    /* Verify each field's type, class & size */
    field = H5Tget_member_type(type, 0);
    CHECK(field, FAIL, "H5Tget_member_type");
    t_class = H5Tget_class(field);
    VERIFY(t_class, H5T_INTEGER, "H5Tget_class");
    order = H5Tget_order(field);
    VERIFY(order, H5Tget_order(H5T_NATIVE_INT), "H5Tget_order");
    size = H5Tget_size(field);
    VERIFY(size, H5Tget_size(H5T_NATIVE_INT), "H5Tget_size");
    H5Tclose(field);
    field = H5Tget_member_type(type, 1);
    CHECK(field, FAIL, "H5Tget_member_type");
    t_class = H5Tget_class(field);
    VERIFY(t_class, H5T_FLOAT, "H5Tget_class");
    order = H5Tget_order(field);
    VERIFY(order, H5Tget_order(H5T_NATIVE_DOUBLE), "H5Tget_order");
    size = H5Tget_size(field);
    VERIFY(size, H5Tget_size(H5T_NATIVE_DOUBLE), "H5Tget_size");
    H5Tclose(field);
    field = H5Tget_member_type(type, 2);
    CHECK(field, FAIL, "H5Tget_member_type");
    t_class = H5Tget_class(field);
    VERIFY(t_class, H5T_INTEGER, "H5Tget_class");
    order = H5Tget_order(field);
    VERIFY(order, H5Tget_order(H5T_NATIVE_SCHAR), "H5Tget_order");
    size = H5Tget_size(field);
    VERIFY(size, H5Tget_size(H5T_NATIVE_SCHAR), "H5Tget_size");
    H5Tclose(field);

    /* Read attribute information */
    ret = H5Aread(attr, type, read_data4);
    CHECK(ret, FAIL, "H5Aread");

    /* Verify values read in */
    for(i = 0; i < ATTR4_DIM1; i++)
        for(j = 0; j < ATTR4_DIM2; j++)
            if(HDmemcmp(&attr_data4[i][j], &read_data4[i][j], sizeof(struct attr4_struct))) {
                printf("%d: attribute data different: attr_data4[%d][%d].i=%d, read_data4[%d][%d].i=%d\n", __LINE__, i, j, attr_data4[i][j].i, i, j, read_data4[i][j].i);
                printf("%d: attribute data different: attr_data4[%d][%d].d=%f, read_data4[%d][%d].d=%f\n", __LINE__, i, j, attr_data4[i][j].d, i, j, read_data4[i][j].d);
                TestErrPrintf("%d: attribute data different: attr_data4[%d][%d].c=%c, read_data4[%d][%d].c=%c\n", __LINE__, i, j, attr_data4[i][j].c, i, j, read_data4[i][j].c);
             } /* end if */

    /* Verify Name */
    name_len = H5Aget_name(attr, (size_t)ATTR_NAME_LEN, attr_name);
    VERIFY(name_len, HDstrlen(ATTR4_NAME), "H5Aget_name");
    if(HDstrcmp(attr_name, ATTR4_NAME))
        TestErrPrintf("attribute name different: attr_name=%s, should be %s\n", attr_name, ATTR4_NAME);

    /* Close attribute datatype */
    ret = H5Tclose(type);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_compound_read() */


/****************************************************************
**
**  test_attr_scalar_write(): Test scalar H5A (attribute) writing code.
**
****************************************************************/
static void
test_attr_scalar_write(hid_t fapl)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1,sid2;	/* Dataspace ID			*/
    hid_t		attr;	    /* Attribute ID			*/
    hsize_t		dims1[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Attribute Functions\n"));

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for dataset */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, DSET1_NAME, H5T_NATIVE_UCHAR, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Create dataspace for attribute */
    sid2 = H5Screate_simple(ATTR5_RANK, NULL, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Create an attribute for the dataset */
    attr = H5Acreate2(dataset, ATTR5_NAME, H5T_NATIVE_FLOAT, sid2, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate2");

    /* Try to create the same attribute again (should fail) */
    ret = H5Acreate2(dataset, ATTR5_NAME, H5T_NATIVE_FLOAT, sid2, H5P_DEFAULT, H5P_DEFAULT);
    VERIFY(ret, FAIL, "H5Acreate2");

    /* Write attribute information */
    ret = H5Awrite(attr, H5T_NATIVE_FLOAT, &attr_data5);
    CHECK(ret, FAIL, "H5Awrite");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_scalar_write() */


/****************************************************************
**
**  test_attr_scalar_read(): Test scalar H5A (attribute) reading code.
**
****************************************************************/
static void
test_attr_scalar_read(hid_t fapl)
{
    hid_t	fid1;		/* HDF5 File IDs		*/
    hid_t	dataset;	/* Dataset ID			*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	attr;	        /* Attribute ID			*/
    H5S_class_t stype;          /* Dataspace class              */
    float       rdata = 0.0;    /* Buffer for reading 1st attribute */
    H5O_info_t  oinfo;          /* Object info                  */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Scalar Attribute Reading Functions\n"));

    /* Create file */
    fid1 = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Open the dataset */
    dataset = H5Dopen2(fid1, DSET1_NAME, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dopen2");

    /* Verify the correct number of attributes */
    ret = H5Oget_info(dataset, &oinfo);
    CHECK(ret, FAIL, "H5Oget_info");
    VERIFY(oinfo.num_attrs, 1, "H5Oget_info");

    /* Open an attribute for the dataset */
    attr = H5Aopen(dataset, ATTR5_NAME, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Aopen");

    /* Read attribute information */
    ret = H5Aread(attr, H5T_NATIVE_FLOAT, &rdata);
    CHECK(ret, FAIL, "H5Aread");

    /* Verify the floating-poing value in this way to avoid compiler warning. */
    if(!FLT_ABS_EQUAL(rdata, attr_data5))
	printf("*** UNEXPECTED VALUE from %s should be %f, but is %f at line %4d in %s\n",
	    "H5Aread", attr_data5, rdata, (int)__LINE__, __FILE__);

    /* Get the attribute's dataspace */
    sid = H5Aget_space(attr);
    CHECK(sid, FAIL, "H5Aget_space");

    /* Make certain the dataspace is scalar */
    stype = H5Sget_simple_extent_type(sid);
    VERIFY(stype, H5S_SCALAR, "H5Sget_simple_extent_type");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_scalar_read() */


/****************************************************************
**
**  test_attr_mult_write(): Test basic H5A (attribute) code.
**      Tests integer attributes on both datasets and groups
**
****************************************************************/
static void
test_attr_mult_write(hid_t fapl)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1,sid2;	/* Dataspace ID			*/
    hid_t		attr;	    /* Attribute ID			*/
    hsize_t		dims1[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
    hsize_t		dims2[] = {ATTR1_DIM1};
    hsize_t		dims3[] = {ATTR2_DIM1,ATTR2_DIM2};
    hsize_t		dims4[] = {ATTR3_DIM1,ATTR3_DIM2,ATTR3_DIM3};
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Multiple Attribute Functions\n"));

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for dataset */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, DSET1_NAME, H5T_NATIVE_UCHAR, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Close dataset's dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Create dataspace for 1st attribute */
    sid2 = H5Screate_simple(ATTR1_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Create 1st attribute for the dataset */
    attr = H5Acreate2(dataset, ATTR1_NAME, H5T_NATIVE_INT, sid2, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate2");

    /* Try to create the same attribute again (should fail) */
    ret = H5Acreate2(dataset, ATTR1_NAME, H5T_NATIVE_INT, sid2, H5P_DEFAULT, H5P_DEFAULT);
    VERIFY(ret, FAIL, "H5Acreate2");

    /* Write 1st attribute data */
    ret = H5Awrite(attr, H5T_NATIVE_INT, attr_data1);
    CHECK(ret, FAIL, "H5Awrite");

    /* Close 1st attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close attribute's dataspace */
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");

    /* Create dataspace for 2nd attribute */
    sid2 = H5Screate_simple(ATTR2_RANK, dims3, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Create 2nd attribute for the dataset */
    attr = H5Acreate2(dataset, ATTR2_NAME, H5T_NATIVE_INT, sid2, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate2");

    /* Try to create the same attribute again (should fail) */
    ret = H5Acreate2(dataset, ATTR2_NAME, H5T_NATIVE_INT, sid2, H5P_DEFAULT, H5P_DEFAULT);
    VERIFY(ret, FAIL, "H5Acreate2");

    /* Write 2nd attribute information */
    ret = H5Awrite(attr, H5T_NATIVE_INT, attr_data2);
    CHECK(ret, FAIL, "H5Awrite");

    /* Close 2nd attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close 2nd attribute's dataspace */
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");

    /* Create dataspace for 3rd attribute */
    sid2 = H5Screate_simple(ATTR3_RANK, dims4, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Create 3rd attribute for the dataset */
    attr = H5Acreate2(dataset, ATTR3_NAME, H5T_NATIVE_DOUBLE, sid2, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate2");

    /* Try to create the same attribute again (should fail) */
    ret = H5Acreate2(dataset, ATTR3_NAME, H5T_NATIVE_DOUBLE, sid2, H5P_DEFAULT, H5P_DEFAULT);
    VERIFY(ret, FAIL, "H5Acreate2");

    /* Write 3rd attribute information */
    ret = H5Awrite(attr, H5T_NATIVE_DOUBLE, attr_data3);
    CHECK(ret, FAIL, "H5Awrite");

    /* Close 3rd attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close 3rd attribute's dataspace */
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_mult_write() */


/****************************************************************
**
**  test_attr_mult_read(): Test basic H5A (attribute) code.
**
****************************************************************/
static void
test_attr_mult_read(hid_t fapl)
{
    hid_t   fid1;	/* HDF5 File ID		*/
    hid_t   dataset;	/* Dataset ID		*/
    hid_t   space;      /* Attribute dataspace  */
    hid_t   type;       /* Attribute datatype   */
    hid_t   attr;	/* Attribute ID		*/
    char    attr_name[ATTR_NAME_LEN]; /* Buffer for attribute names */
    char    temp_name[ATTR_NAME_LEN]; /* Buffer for mangling attribute names */
    int     rank;       /* Attribute rank */
    hsize_t dims[ATTR_MAX_DIMS];        /* Attribute dimensions */
    H5T_class_t t_class;        /* Attribute datatype class */
    H5T_order_t order;          /* Attribute datatype order */
    size_t      size;           /* Attribute datatype size as stored in file */
    int     read_data1[ATTR1_DIM1] = {0}; /* Buffer for reading 1st attribute */
    int     read_data2[ATTR2_DIM1][ATTR2_DIM2] = {{0}}; /* Buffer for reading 2nd attribute */
    double  read_data3[ATTR3_DIM1][ATTR3_DIM2][ATTR3_DIM3] = {{{0}}}; /* Buffer for reading 3rd attribute */
    size_t  name_len;   /* Length of attribute name */
    H5O_info_t oinfo;   /* Object info */
    int     i, j, k;    /* Local index values */
    herr_t  ret;	/* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Attribute Functions\n"));

    /* Open file */
    fid1 = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Open the dataset */
    dataset = H5Dopen2(fid1, DSET1_NAME, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dopen2");

    /* Verify the correct number of attributes */
    ret = H5Oget_info(dataset, &oinfo);
    CHECK(ret, FAIL, "H5Oget_info");
    VERIFY(oinfo.num_attrs, 3, "H5Oget_info");

    /* Open 1st attribute for the dataset */
    attr = H5Aopen_by_idx(dataset, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)0, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Aopen_by_idx");

    /* Verify Dataspace */
    space = H5Aget_space(attr);
    CHECK(space, FAIL, "H5Aget_space");
    rank = H5Sget_simple_extent_ndims(space);
    VERIFY(rank, ATTR1_RANK, "H5Sget_simple_extent_ndims");
    ret = H5Sget_simple_extent_dims(space, dims, NULL);
    CHECK(ret, FAIL, "H5Sget_simple_extent_dims");
    if(dims[0] != ATTR1_DIM1)
        TestErrPrintf("attribute dimensions different: dims[0]=%d, should be %d\n", (int)dims[0], ATTR1_DIM1);
    H5Sclose(space);

    /* Verify Datatype */
    type = H5Aget_type(attr);
    CHECK(type, FAIL, "H5Aget_type");
    t_class = H5Tget_class(type);
    VERIFY(t_class, H5T_INTEGER, "H5Tget_class");
    order = H5Tget_order(type);
    VERIFY(order, H5Tget_order(H5T_NATIVE_INT), "H5Tget_order");
    size = H5Tget_size(type);
    VERIFY(size, H5Tget_size(H5T_NATIVE_INT), "H5Tget_size");
    H5Tclose(type);

    /* Read attribute information */
    ret = H5Aread(attr, H5T_NATIVE_INT, read_data1);
    CHECK(ret, FAIL, "H5Aread");

    /* Verify values read in */
    for(i = 0; i < ATTR1_DIM1; i++)
        if(attr_data1[i] != read_data1[i])
            TestErrPrintf("%d: attribute data different: attr_data1[%d]=%d, read_data1[%d]=%d\n", __LINE__, i, attr_data1[i], i, read_data1[i]);

    /* Verify Name */
    name_len = H5Aget_name(attr, (size_t)ATTR_NAME_LEN, attr_name);
    VERIFY(name_len, HDstrlen(ATTR1_NAME), "H5Aget_name");
    if(HDstrcmp(attr_name, ATTR1_NAME))
        TestErrPrintf("attribute name different: attr_name=%s, should be %s\n", attr_name, ATTR1_NAME);

    /* Verify Name with too small of a buffer */
    name_len = H5Aget_name(attr,HDstrlen(ATTR1_NAME), attr_name);
    VERIFY(name_len, HDstrlen(ATTR1_NAME), "H5Aget_name");
    HDstrcpy(temp_name, ATTR1_NAME);     /* make a copy of the name */
    temp_name[HDstrlen(ATTR1_NAME) - 1] = '\0';   /* truncate it to match the one retrieved */
    if(HDstrcmp(attr_name, temp_name))
        TestErrPrintf("attribute name different: attr_name=%s, should be %s\n", attr_name, temp_name);

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Open 2nd attribute for the dataset */
    attr = H5Aopen_by_idx(dataset, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)1, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Aopen_by_idx");

    /* Verify Dataspace */
    space = H5Aget_space(attr);
    CHECK(space, FAIL, "H5Aget_space");
    rank = H5Sget_simple_extent_ndims(space);
    VERIFY(rank, ATTR2_RANK, "H5Sget_simple_extent_ndims");
    ret = H5Sget_simple_extent_dims(space, dims, NULL);
    CHECK(ret, FAIL, "H5Sget_simple_extent_dims");
    if(dims[0] != ATTR2_DIM1)
        TestErrPrintf("attribute dimensions different: dims[0]=%d, should be %d\n", (int)dims[0], ATTR2_DIM1);
    if(dims[1] != ATTR2_DIM2)
        TestErrPrintf("attribute dimensions different: dims[1]=%d, should be %d\n", (int)dims[1], ATTR2_DIM2);
    H5Sclose(space);

    /* Verify Datatype */
    type = H5Aget_type(attr);
    CHECK(type, FAIL, "H5Aget_type");
    t_class = H5Tget_class(type);
    VERIFY(t_class, H5T_INTEGER, "H5Tget_class");
    order = H5Tget_order(type);
    VERIFY(order, H5Tget_order(H5T_NATIVE_INT), "H5Tget_order");
    size = H5Tget_size(type);
    VERIFY(size, H5Tget_size(H5T_NATIVE_INT), "H5Tget_size");
    H5Tclose(type);

    /* Read attribute information */
    ret = H5Aread(attr, H5T_NATIVE_INT, read_data2);
    CHECK(ret, FAIL, "H5Aread");

    /* Verify values read in */
    for(i = 0; i < ATTR2_DIM1; i++)
        for(j = 0; j < ATTR2_DIM2; j++)
            if(attr_data2[i][j] != read_data2[i][j])
                TestErrPrintf("%d: attribute data different: attr_data2[%d][%d]=%d, read_data2[%d][%d]=%d\n", __LINE__, i, j, attr_data2[i][j], i, j, read_data2[i][j]);

    /* Verify Name */
    name_len = H5Aget_name(attr, (size_t)ATTR_NAME_LEN, attr_name);
    VERIFY(name_len, HDstrlen(ATTR2_NAME), "H5Aget_name");
    if(HDstrcmp(attr_name, ATTR2_NAME))
        TestErrPrintf("attribute name different: attr_name=%s, should be %s\n", attr_name, ATTR2_NAME);

    /* Verify Name with too small of a buffer */
    name_len = H5Aget_name(attr, HDstrlen(ATTR2_NAME), attr_name);
    VERIFY(name_len, HDstrlen(ATTR2_NAME), "H5Aget_name");
    HDstrcpy(temp_name, ATTR2_NAME);     /* make a copy of the name */
    temp_name[HDstrlen(ATTR2_NAME) - 1] = '\0';   /* truncate it to match the one retrieved */
    if(HDstrcmp(attr_name, temp_name))
        TestErrPrintf("attribute name different: attr_name=%s, should be %s\n", attr_name, temp_name);

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Open 2nd attribute for the dataset */
    attr = H5Aopen_by_idx(dataset, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)2, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Aopen_by_idx");

    /* Verify Dataspace */
    space = H5Aget_space(attr);
    CHECK(space, FAIL, "H5Aget_space");
    rank = H5Sget_simple_extent_ndims(space);
    VERIFY(rank, ATTR3_RANK, "H5Sget_simple_extent_ndims");
    ret = H5Sget_simple_extent_dims(space, dims, NULL);
    CHECK(ret, FAIL, "H5Sget_simple_extent_dims");
    if(dims[0] != ATTR3_DIM1)
        TestErrPrintf("attribute dimensions different: dims[0]=%d, should be %d\n", (int)dims[0], ATTR3_DIM1);
    if(dims[1] != ATTR3_DIM2)
        TestErrPrintf("attribute dimensions different: dims[1]=%d, should be %d\n", (int)dims[1], ATTR3_DIM2);
    if(dims[2] != ATTR3_DIM3)
        TestErrPrintf("attribute dimensions different: dims[2]=%d, should be %d\n", (int)dims[2], ATTR3_DIM3);
    H5Sclose(space);

    /* Verify Datatype */
    type = H5Aget_type(attr);
    CHECK(type, FAIL, "H5Aget_type");
    t_class = H5Tget_class(type);
    VERIFY(t_class, H5T_FLOAT, "H5Tget_class");
    order = H5Tget_order(type);
    VERIFY(order, H5Tget_order(H5T_NATIVE_DOUBLE), "H5Tget_order");
    size = H5Tget_size(type);
    VERIFY(size, H5Tget_size(H5T_NATIVE_DOUBLE), "H5Tget_size");
    H5Tclose(type);

    /* Read attribute information */
    ret = H5Aread(attr, H5T_NATIVE_DOUBLE, read_data3);
    CHECK(ret, FAIL, "H5Aread");

    /* Verify values read in */
    for(i = 0; i < ATTR3_DIM1; i++)
        for(j = 0; j < ATTR3_DIM2; j++)
            for(k = 0; k < ATTR3_DIM3; k++)
                if(!DBL_ABS_EQUAL(attr_data3[i][j][k], read_data3[i][j][k]))
                    TestErrPrintf("%d: attribute data different: attr_data3[%d][%d][%d]=%f, read_data3[%d][%d][%d]=%f\n", __LINE__, i, j, k, attr_data3[i][j][k], i, j, k, read_data3[i][j][k]);

    /* Verify Name */
    name_len = H5Aget_name(attr, (size_t)ATTR_NAME_LEN, attr_name);
    VERIFY(name_len, HDstrlen(ATTR3_NAME), "H5Aget_name");
    if(HDstrcmp(attr_name, ATTR3_NAME))
        TestErrPrintf("attribute name different: attr_name=%s, should be %s\n", attr_name, ATTR3_NAME);

    /* Verify Name with too small of a buffer */
    name_len = H5Aget_name(attr, HDstrlen(ATTR3_NAME), attr_name);
    VERIFY(name_len, HDstrlen(ATTR3_NAME), "H5Aget_name");
    HDstrcpy(temp_name, ATTR3_NAME);     /* make a copy of the name */
    temp_name[HDstrlen(ATTR3_NAME) - 1] = '\0';   /* truncate it to match the one retrieved */
    if(HDstrcmp(attr_name, temp_name))
        TestErrPrintf("attribute name different: attr_name=%s, should be %s\n", attr_name, temp_name);

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_mult_read() */


/****************************************************************
**
**  attr_op1(): Attribute operator
**
****************************************************************/
static herr_t
attr_op1(hid_t UNUSED loc_id, const char *name, const H5A_info_t UNUSED *ainfo,
    void *op_data)
{
    int *count = (int *)op_data;
    herr_t ret = 0;

    switch(*count) {
        case 0:
            if(HDstrcmp(name, ATTR1_NAME))
                TestErrPrintf("attribute name different: name=%s, should be %s\n", name, ATTR1_NAME);
             (*count)++;
             break;

        case 1:
            if(HDstrcmp(name, ATTR2_NAME))
                TestErrPrintf("attribute name different: name=%s, should be %s\n", name, ATTR2_NAME);
             (*count)++;
             break;

        case 2:
            if(HDstrcmp(name, ATTR3_NAME))
                TestErrPrintf("attribute name different: name=%s, should be %s\n", name, ATTR3_NAME);
             (*count)++;
             break;

        default:
            ret = -1;
            break;
    }  /* end switch() */

    return(ret);
} /* end attr_op1() */


/****************************************************************
**
**  test_attr_iterate(): Test H5A (attribute) iterator code.
**
****************************************************************/
static void
test_attr_iterate(hid_t fapl)
{
    hid_t   file;	/* HDF5 File ID 		*/
    hid_t   dataset;	/* Dataset ID			*/
    hid_t   sid;	/* Dataspace ID			*/
    int     count;      /* operator data for the iterator */
    H5O_info_t oinfo;   /* Object info                  */
    herr_t  ret;	/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Attribute Functions\n"));

    /* Open file */
    file = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(file, FAIL, "H5Fopen");

    /* Create a dataspace */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create a new dataset */
    dataset = H5Dcreate2(file, DSET2_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Verify the correct number of attributes */
    ret = H5Oget_info(dataset, &oinfo);
    CHECK(ret, FAIL, "H5Oget_info");
    VERIFY(oinfo.num_attrs, 0, "H5Oget_info");

    /* Iterate over attributes on dataset */
    count = 0;
    ret = H5Aiterate2(dataset, H5_INDEX_NAME, H5_ITER_INC, NULL, attr_op1, &count);
    VERIFY(ret, 0, "H5Aiterate2");

    /* Close dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Open existing dataset w/attributes */
    dataset = H5Dopen2(file, DSET1_NAME, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dopen2");

    /* Verify the correct number of attributes */
    ret = H5Oget_info(dataset, &oinfo);
    CHECK(ret, FAIL, "H5Oget_info");
    VERIFY(oinfo.num_attrs, 3, "H5Oget_info");

    /* Iterate over attributes on dataset */
    count = 0;
    ret = H5Aiterate2(dataset, H5_INDEX_NAME, H5_ITER_INC, NULL, attr_op1, &count);
    VERIFY(ret, 0, "H5Aiterate2");

    /* Close dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_iterate() */


/****************************************************************
**
**  test_attr_delete(): Test H5A (attribute) code for deleting objects.
**
****************************************************************/
static void
test_attr_delete(hid_t fapl)
{
    hid_t   fid1;	/* HDF5 File ID 		*/
    hid_t   dataset;	/* Dataset ID			*/
    hid_t   attr;       /* Attribute ID			*/
    char    attr_name[ATTR_NAME_LEN]; /* Buffer for attribute names */
    size_t  name_len;   /* Length of attribute name     */
    H5O_info_t oinfo;   /* Object info                  */
    herr_t  ret;	/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Attribute Functions\n"));

    /* Open file */
    fid1 = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Open the dataset */
    dataset = H5Dopen2(fid1, DSET1_NAME, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dopen2");

    /* Verify the correct number of attributes */
    ret = H5Oget_info(dataset, &oinfo);
    CHECK(ret, FAIL, "H5Oget_info");
    VERIFY(oinfo.num_attrs, 3, "H5Oget_info");

    /* Try to delete bogus attribute */
    ret = H5Adelete(dataset, "Bogus");
    VERIFY(ret, FAIL, "H5Adelete");

    /* Verify the correct number of attributes */
    ret = H5Oget_info(dataset, &oinfo);
    CHECK(ret, FAIL, "H5Oget_info");
    VERIFY(oinfo.num_attrs, 3, "H5Oget_info");

    /* Delete middle (2nd) attribute */
    ret = H5Adelete(dataset, ATTR2_NAME);
    CHECK(ret, FAIL, "H5Adelete");

    /* Verify the correct number of attributes */
    ret = H5Oget_info(dataset, &oinfo);
    CHECK(ret, FAIL, "H5Oget_info");
    VERIFY(oinfo.num_attrs, 2, "H5Oget_info");

    /* Open 1st attribute for the dataset */
    attr = H5Aopen_by_idx(dataset, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)0, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Aopen_by_idx");

    /* Verify Name */
    name_len = H5Aget_name(attr, (size_t)ATTR_NAME_LEN, attr_name);
    VERIFY(name_len, HDstrlen(ATTR1_NAME), "H5Aget_name");
    if(HDstrcmp(attr_name, ATTR1_NAME))
        TestErrPrintf("attribute name different: attr_name=%s, should be %s\n",attr_name,ATTR1_NAME);

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Open last (formally 3rd) attribute for the dataset */
    attr = H5Aopen_by_idx(dataset, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)1, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Aopen_by_idx");

    /* Verify Name */
    name_len = H5Aget_name(attr, (size_t)ATTR_NAME_LEN, attr_name);
    VERIFY(name_len, HDstrlen(ATTR3_NAME), "H5Aget_name");
    if(HDstrcmp(attr_name, ATTR3_NAME))
        TestErrPrintf("attribute name different: attr_name=%s, should be %s\n",attr_name,ATTR3_NAME);

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Delete first attribute */
    ret = H5Adelete(dataset, ATTR1_NAME);
    CHECK(ret, FAIL, "H5Adelete");

    /* Verify the correct number of attributes */
    ret = H5Oget_info(dataset, &oinfo);
    CHECK(ret, FAIL, "H5Oget_info");
    VERIFY(oinfo.num_attrs, 1, "H5Oget_info");

    /* Open last (formally 3rd) attribute for the dataset */
    attr = H5Aopen_by_idx(dataset, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)0, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Aopen_by_idx");

    /* Verify Name */
    name_len = H5Aget_name(attr, (size_t)ATTR_NAME_LEN, attr_name);
    VERIFY(name_len, HDstrlen(ATTR3_NAME), "H5Aget_name");
    if(HDstrcmp(attr_name, ATTR3_NAME))
        TestErrPrintf("attribute name different: attr_name=%s, should be %s\n",attr_name,ATTR3_NAME);

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Delete first attribute */
    ret = H5Adelete(dataset, ATTR3_NAME);
    CHECK(ret, FAIL, "H5Adelete");

    /* Verify the correct number of attributes */
    ret = H5Oget_info(dataset, &oinfo);
    CHECK(ret, FAIL, "H5Oget_info");
    VERIFY(oinfo.num_attrs, 0, "H5Oget_info");

    /* Close dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_delete() */


/****************************************************************
**
**  test_attr_dtype_shared(): Test H5A (attribute) code for using
**                              shared datatypes in attributes.
**
****************************************************************/
static void
test_attr_dtype_shared(hid_t fapl)
{
    hid_t file_id;              /* File ID */
    hid_t dset_id;              /* Dataset ID */
    hid_t space_id;             /* Dataspace ID for dataset & attribute */
    hid_t type_id;              /* Datatype ID for named datatype */
    hid_t attr_id;              /* Attribute ID */
    int data = 8;               /* Data to write */
    int rdata = 0;              /* Read read in */
    H5O_info_t oinfo;           /* Object's information */
    h5_stat_size_t empty_filesize;       /* Size of empty file */
    h5_stat_size_t filesize;             /* Size of file after modifications */
    herr_t  ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Shared Datatypes with Attributes\n"));

    /* Create a file */
    file_id = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(file_id, FAIL, "H5Fopen");

    /* Close file */
    ret = H5Fclose(file_id);
    CHECK(ret, FAIL, "H5Fclose");

    /* Get size of file */
    empty_filesize = h5_get_file_size(FILENAME, fapl);
    if(empty_filesize < 0)
        TestErrPrintf("Line %d: file size wrong!\n", __LINE__);


    /* Re-open file */
    file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(file_id, FAIL, "H5Fopen");

    /* Create a datatype to commit and use */
    type_id = H5Tcopy(H5T_NATIVE_INT);
    CHECK(type_id, FAIL, "H5Tcopy");

    /* Commit datatype to file */
    ret = H5Tcommit2(file_id, TYPE1_NAME, type_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");

    /* Check reference count on named datatype */
    ret = H5Oget_info_by_name(file_id, TYPE1_NAME, &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.rc, 1, "H5Oget_info_by_name");

    /* Create dataspace for dataset */
    space_id = H5Screate(H5S_SCALAR);
    CHECK(space_id, FAIL, "H5Screate");

    /* Create dataset */
    dset_id = H5Dcreate2(file_id, DSET1_NAME, type_id, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dset_id, FAIL, "H5Dcreate2");

    /* Check reference count on named datatype */
    ret = H5Oget_info_by_name(file_id, TYPE1_NAME, &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.rc, 2, "H5Oget_info_by_name");

    /* Create attribute on dataset */
    attr_id = H5Acreate2(dset_id, ATTR1_NAME, type_id, space_id, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr_id, FAIL, "H5Acreate2");

    /* Check reference count on named datatype */
    ret = H5Oget_info_by_name(file_id, TYPE1_NAME, &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.rc, 3, "H5Oget_info_by_name");

    /* Close attribute */
    ret = H5Aclose(attr_id);
    CHECK(ret, FAIL, "H5Aclose");

    /* Delete attribute */
    ret = H5Adelete(dset_id, ATTR1_NAME);
    CHECK(ret, FAIL, "H5Adelete");

    /* Check reference count on named datatype */
    ret = H5Oget_info_by_name(file_id, TYPE1_NAME, &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.rc, 2, "H5Oget_info_by_name");

    /* Create attribute on dataset */
    attr_id = H5Acreate2(dset_id, ATTR1_NAME, type_id, space_id, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr_id, FAIL, "H5Acreate2");

    /* Check reference count on named datatype */
    ret = H5Oget_info_by_name(file_id, TYPE1_NAME, &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.rc, 3, "H5Oget_info_by_name");

    /* Write data into the attribute */
    ret = H5Awrite(attr_id, H5T_NATIVE_INT, &data);
    CHECK(ret, FAIL, "H5Awrite");

    /* Close attribute */
    ret = H5Aclose(attr_id);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close dataset */
    ret = H5Dclose(dset_id);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close dataspace */
    ret = H5Sclose(space_id);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close datatype */
    ret = H5Tclose(type_id);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close file */
    ret = H5Fclose(file_id);
    CHECK(ret, FAIL, "H5Fclose");


    /* Re-open file */
    file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(file_id, FAIL, "H5Fopen");

    /* Open dataset */
    dset_id = H5Dopen2(file_id, DSET1_NAME, H5P_DEFAULT);
    CHECK(dset_id, FAIL, "H5Dopen2");

    /* Open attribute */
    attr_id = H5Aopen(dset_id, ATTR1_NAME, H5P_DEFAULT);
    CHECK(attr_id, FAIL, "H5Aopen");

    /* Read data from the attribute */
    ret = H5Aread(attr_id, H5T_NATIVE_INT, &rdata);
    CHECK(ret, FAIL, "H5Aread");
    VERIFY(data, rdata, "H5Aread");

    /* Close attribute */
    ret = H5Aclose(attr_id);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close dataset */
    ret = H5Dclose(dset_id);
    CHECK(ret, FAIL, "H5Dclose");

    /* Check reference count on named datatype */
    ret = H5Oget_info_by_name(file_id, TYPE1_NAME, &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.rc, 3, "H5Oget_info_by_name");

    /* Unlink the dataset */
    ret = H5Ldelete(file_id, DSET1_NAME, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Ldelete");

    /* Check reference count on named datatype */
    ret = H5Oget_info_by_name(file_id, TYPE1_NAME, &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.rc, 1, "H5Oget_info_by_name");

    /* Unlink the named datatype */
    ret = H5Ldelete(file_id, TYPE1_NAME, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Ldelete");

    /* Close file */
    ret = H5Fclose(file_id);
    CHECK(ret, FAIL, "H5Fclose");

    /* Check size of file */
    filesize = h5_get_file_size(FILENAME, fapl);
    VERIFY(filesize, empty_filesize, "h5_get_file_size");
}   /* test_attr_dtype_shared() */


/****************************************************************
**
**  test_attr_duplicate_ids(): Test operations with more than
**      one ID handles.
**
****************************************************************/
static void
test_attr_duplicate_ids(hid_t fapl)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		gid1, gid2;	/* Group ID			*/
    hid_t		sid1,sid2;	/* Dataspace ID			*/
    hid_t		attr, attr2;	    /* Attribute ID		*/
    hsize_t		dims1[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
    hsize_t		dims2[] = {ATTR1_DIM1};
    int                 read_data1[ATTR1_DIM1]={0}; /* Buffer for reading 1st attribute */
    int                 rewrite_data[ATTR1_DIM1]={1234, -423, 9907256}; /* Test data for rewrite */
    int                 i;
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing operations with two ID handles\n"));

    /*-----------------------------------------------------------------------------------
     *        Create an attribute in a new file and fill it with fill value.
     */
    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for dataset */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, DSET1_NAME, H5T_NATIVE_UCHAR, sid1, H5P_DEFAULT,
        H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Create dataspace for attribute */
    sid2 = H5Screate_simple(ATTR1_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Try to create an attribute on the dataset */
    attr = H5Acreate2(dataset, ATTR1_NAME, H5T_NATIVE_INT, sid2, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate2");

    /* Open the attribute just created and get a second ID */
    attr2 = H5Aopen(dataset, ATTR1_NAME, H5P_DEFAULT);
    CHECK(attr2, FAIL, "H5Aopen");

    /* Close attribute */
    ret = H5Aclose(attr2);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /*-----------------------------------------------------------------------------------
     *        Reopen the file and verify the fill value for attribute.  Also write
     *        some real data.
     */

    /* Open file */
    fid1 = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Open the dataset */
    dataset = H5Dopen2(fid1, DSET1_NAME, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dopen2");

    /* Open first attribute for the dataset */
    attr = H5Aopen(dataset, ATTR1_NAME, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Aopen");

    /* Read attribute with fill value */
    ret = H5Aread(attr, H5T_NATIVE_INT, read_data1);
    CHECK(ret, FAIL, "H5Aread");

    /* Verify values read in */
    for(i = 0; i < ATTR1_DIM1; i++)
        if(0 != read_data1[i])
            TestErrPrintf("%d: attribute data different: read_data1[%d]=%d\n", __LINE__, i, read_data1[i]);

    /* Open attribute for the second time */
    attr2 = H5Aopen(dataset, ATTR1_NAME, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Aopen");

    /* Write attribute information */
    ret = H5Awrite(attr2, H5T_NATIVE_INT, attr_data1);
    CHECK(ret, FAIL, "H5Awrite");

    /* Close attribute */
    ret = H5Aclose(attr2);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /*-----------------------------------------------------------------------------------
     *        Reopen the file and verify the data.  Also rewrite the data and verify it.
     */

    /* Open file */
    fid1 = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Open the dataset */
    dataset = H5Dopen2(fid1, DSET1_NAME, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dopen2");

    /* Open first attribute for the dataset */
    attr = H5Aopen(dataset, ATTR1_NAME, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Aopen");

    /* Read attribute information */
    ret = H5Aread(attr, H5T_NATIVE_INT, read_data1);
    CHECK(ret, FAIL, "H5Aread");

    /* Verify values read in */
    for(i = 0; i < ATTR1_DIM1; i++)
        if(attr_data1[i] != read_data1[i])
            TestErrPrintf("%d: attribute data different: attr_data1[%d]=%d, read_data1[%d]=%d\n", __LINE__, i, attr_data1[i], i, read_data1[i]);

    /* Open attribute for the second time */
    attr2 = H5Aopen(dataset, ATTR1_NAME, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Aopen");

    /* Write attribute information */
    ret = H5Awrite(attr2, H5T_NATIVE_INT, rewrite_data);
    CHECK(ret, FAIL, "H5Awrite");

    /* Read attribute information */
    ret = H5Aread(attr, H5T_NATIVE_INT, read_data1);
    CHECK(ret, FAIL, "H5Aread");

    /* Verify values read in */
    for(i = 0; i < ATTR1_DIM1; i++)
        if(read_data1[i] != rewrite_data[i])
            TestErrPrintf("%d: attribute data different: read_data1[%d]=%d, rewrite_data[%d]=%d\n", __LINE__, i, read_data1[i], i, rewrite_data[i]);

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    ret = H5Aclose(attr2);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /*-----------------------------------------------------------------------------------
     *        Verify that the attribute being pointed to by different paths shares
     *        the same data.
     */
    /* Open file */
    fid1 = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Create a group */
    gid1 = H5Gcreate2(fid1, GROUP1_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(gid1, FAIL, "H5Gcreate2");

    /* Create hard link to the first group */
    ret = H5Lcreate_hard(gid1, GROUP1_NAME, H5L_SAME_LOC, GROUP2_NAME, H5P_DEFAULT,
        H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Lcreate_hard");

    /* Try to create an attribute on the group */
    attr = H5Acreate2(gid1, ATTR2_NAME, H5T_NATIVE_INT, sid2, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate2");

    /* Open the hard link just created */
    gid2 = H5Gopen2(fid1, GROUP2_NAME, H5P_DEFAULT);
    CHECK(gid2, FAIL, "H5Gopen2");

    /* Open the attribute of the group for the second time */
    attr2 = H5Aopen(gid2, ATTR2_NAME, H5P_DEFAULT);
    CHECK(attr2, FAIL, "H5Aopen");

    /* Write attribute information with the first attribute handle */
    ret = H5Awrite(attr, H5T_NATIVE_INT, attr_data1);
    CHECK(ret, FAIL, "H5Awrite");

    /* Read attribute information with the second attribute handle */
    ret = H5Aread(attr2, H5T_NATIVE_INT, read_data1);
    CHECK(ret, FAIL, "H5Aread");

    /* Verify values read in */
    for(i = 0; i < ATTR1_DIM1; i++)
        if(attr_data1[i] != read_data1[i])
            TestErrPrintf("%d: attribute data different: attr_data1[%d]=%d, read_data1[%d]=%d\n", __LINE__, i, attr_data1[i], i, read_data1[i]);

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    ret = H5Aclose(attr2);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close group */
    ret = H5Gclose(gid1);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Gclose(gid2);
    CHECK(ret, FAIL, "H5Gclose");

    /* Close Attribute dataspace */
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Dataset dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_duplicate_ids() */


/****************************************************************
**
**  test_attr_dense_verify(): Test basic H5A (attribute) code.
**      Verify attributes on object
**
****************************************************************/
static int
test_attr_dense_verify(hid_t loc_id, unsigned max_attr)
{
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute */
    hid_t	attr;	        /* Attribute ID	*/
    unsigned    value;          /* Attribute value */
    unsigned    u;              /* Local index variable */
    int         old_nerrs;      /* Number of errors when entering this check */
    herr_t	ret;		/* Generic return value	*/

    /* Retrieve the current # of reported errors */
    old_nerrs = GetTestNumErrs();

    /* Re-open all the attributes by name and verify the data */
    for(u = 0; u < max_attr; u++) {
        /* Open attribute */
        sprintf(attrname, "attr %02u", u);
        attr = H5Aopen(loc_id, attrname, H5P_DEFAULT);
        CHECK(attr, FAIL, "H5Aopen");

        /* Read data from the attribute */
        ret = H5Aread(attr, H5T_NATIVE_UINT, &value);
        CHECK(ret, FAIL, "H5Aread");
        VERIFY(value, u, "H5Aread");

        /* Close attribute */
        ret = H5Aclose(attr);
        CHECK(ret, FAIL, "H5Aclose");
    } /* end for */

    /* Re-open all the attributes by index and verify the data */
    for(u = 0; u < max_attr; u++) {
        size_t name_len;                /* Length of attribute name */
        char check_name[ATTR_NAME_LEN]; /* Buffer for checking attribute names */

        /* Open attribute */
        attr = H5Aopen_by_idx(loc_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)u, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(attr, FAIL, "H5Aopen_by_idx");

        /* Verify Name */
        sprintf(attrname, "attr %02u", u);
        name_len = H5Aget_name(attr, (size_t)ATTR_NAME_LEN, check_name);
        VERIFY(name_len, HDstrlen(attrname), "H5Aget_name");
        if(HDstrcmp(check_name, attrname))
            TestErrPrintf("attribute name different: attrname = '%s', should be '%s'\n", check_name, attrname);

        /* Read data from the attribute */
        ret = H5Aread(attr, H5T_NATIVE_UINT, &value);
        CHECK(ret, FAIL, "H5Aread");
        VERIFY(value, u, "H5Aread");

        /* Close attribute */
        ret = H5Aclose(attr);
        CHECK(ret, FAIL, "H5Aclose");
    } /* end for */

    /* Retrieve current # of errors */
    if(old_nerrs == GetTestNumErrs())
        return(0);
    else
        return(-1);
}   /* test_attr_dense_verify() */


/****************************************************************
**
**  test_attr_dense_create(): Test basic H5A (attribute) code.
**      Tests "dense" attribute storage creation
**
****************************************************************/
static void
test_attr_dense_create(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dataset;	/* Dataset ID			*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute */
    unsigned    max_compact;    /* Maximum # of attributes to store compactly */
    unsigned    min_dense;      /* Minimum # of attributes to store "densely" */
    htri_t	is_dense;	/* Are attributes stored densely? */
    unsigned    u;              /* Local index variable */
    h5_stat_size_t empty_filesize;       /* Size of empty file */
    h5_stat_size_t filesize;             /* Size of file after modifications */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Dense Attribute Storage Creation\n"));

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Get size of file */
    empty_filesize = h5_get_file_size(FILENAME, fapl);
    if(empty_filesize < 0)
        TestErrPrintf("Line %d: file size wrong!\n", __LINE__);

    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Create dataspace for dataset */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Query the group creation properties */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Create a dataset */
    dataset = H5Dcreate2(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Retrieve limits for compact/dense attribute storage */
    ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");

    /* Close property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

    /* Add attributes, until just before converting to dense storage */
    for(u = 0; u < max_compact; u++) {
        /* Create attribute */
        sprintf(attrname, "attr %02u", u);
        attr = H5Acreate2(dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(attr, FAIL, "H5Acreate2");

        /* Write data into the attribute */
        ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
        CHECK(ret, FAIL, "H5Awrite");

        /* Close attribute */
        ret = H5Aclose(attr);
        CHECK(ret, FAIL, "H5Aclose");
    } /* end for */

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

    /* Add one more attribute, to push into "dense" storage */
    /* Create attribute */
    sprintf(attrname, "attr %02u", u);
    attr = H5Acreate2(dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate2");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

    /* Write data into the attribute */
    ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
    CHECK(ret, FAIL, "H5Awrite");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Attempt to add attribute again, which should fail */
    attr = H5Acreate2(dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
    VERIFY(attr, FAIL, "H5Acreate2");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Unlink dataset with attributes */
    ret = H5Ldelete(fid, DSET1_NAME, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Ldelete");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Check size of file */
    filesize = h5_get_file_size(FILENAME, fapl);
    VERIFY(filesize, empty_filesize, "h5_get_file_size");
}   /* test_attr_dense_create() */


/****************************************************************
**
**  test_attr_dense_open(): Test basic H5A (attribute) code.
**      Tests opening attributes in "dense" storage
**
****************************************************************/
static void
test_attr_dense_open(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dataset;	/* Dataset ID			*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute */
    unsigned    max_compact;    /* Maximum # of attributes to store compactly */
    unsigned    min_dense;      /* Minimum # of attributes to store "densely" */
    htri_t	is_dense;	/* Are attributes stored densely? */
    unsigned    u;              /* Local index variable */
    h5_stat_size_t empty_filesize;       /* Size of empty file */
    h5_stat_size_t filesize;             /* Size of file after modifications */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Opening Attributes in Dense Storage\n"));

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Get size of file */
    empty_filesize = h5_get_file_size(FILENAME, fapl);
    if(empty_filesize < 0)
        TestErrPrintf("Line %d: file size wrong!\n", __LINE__);

    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Create dataspace for dataset */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Query the group creation properties */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Enable creation order tracking on attributes, so creation order tests work */
    ret = H5Pset_attr_creation_order(dcpl, H5P_CRT_ORDER_TRACKED);
    CHECK(ret, FAIL, "H5Pset_attr_creation_order");

    /* Create a dataset */
    dataset = H5Dcreate2(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Retrieve limits for compact/dense attribute storage */
    ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");

    /* Close property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

    /* Add attributes, until just before converting to dense storage */
    for(u = 0; u < max_compact; u++) {
        /* Create attribute */
        sprintf(attrname, "attr %02u", u);
        attr = H5Acreate2(dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(attr, FAIL, "H5Acreate2");

        /* Write data into the attribute */
        ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
        CHECK(ret, FAIL, "H5Awrite");

        /* Close attribute */
        ret = H5Aclose(attr);
        CHECK(ret, FAIL, "H5Aclose");

        /* Verify attributes written so far */
        ret = test_attr_dense_verify(dataset, u);
        CHECK(ret, FAIL, "test_attr_dense_verify");
    } /* end for */

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

    /* Add one more attribute, to push into "dense" storage */
    /* Create attribute */
    sprintf(attrname, "attr %02u", u);
    attr = H5Acreate2(dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate2");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

    /* Write data into the attribute */
    ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
    CHECK(ret, FAIL, "H5Awrite");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Verify all the attributes written */
    ret = test_attr_dense_verify(dataset, (u + 1));
    CHECK(ret, FAIL, "test_attr_dense_verify");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Unlink dataset with attributes */
    ret = H5Ldelete(fid, DSET1_NAME, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Ldelete");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Check size of file */
    filesize = h5_get_file_size(FILENAME, fapl);
    VERIFY(filesize, empty_filesize, "h5_get_file_size");
}   /* test_attr_dense_open() */


/****************************************************************
**
**  test_attr_dense_delete(): Test basic H5A (attribute) code.
**      Tests deleting attributes in "dense" storage
**
****************************************************************/
static void
test_attr_dense_delete(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dataset;	/* Dataset ID			*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute */
    unsigned    max_compact;    /* Maximum # of attributes to store compactly */
    unsigned    min_dense;      /* Minimum # of attributes to store "densely" */
    htri_t	is_dense;	/* Are attributes stored densely? */
    unsigned    u;              /* Local index variable */
    h5_stat_size_t empty_filesize;       /* Size of empty file */
    h5_stat_size_t filesize;             /* Size of file after modifications */
    H5O_info_t  oinfo;          /* Object info                  */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Deleting Attributes in Dense Storage\n"));

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Get size of file */
    empty_filesize = h5_get_file_size(FILENAME, fapl);
    if(empty_filesize < 0)
        TestErrPrintf("Line %d: file size wrong!\n", __LINE__);

    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Create dataspace for dataset */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Query the group creation properties */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Enable creation order tracking on attributes, so creation order tests work */
    ret = H5Pset_attr_creation_order(dcpl, H5P_CRT_ORDER_TRACKED);
    CHECK(ret, FAIL, "H5Pset_attr_creation_order");

    /* Create a dataset */
    dataset = H5Dcreate2(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Retrieve limits for compact/dense attribute storage */
    ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");

    /* Close property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

    /* Add attributes, until well into dense storage */
    for(u = 0; u < (max_compact * 2); u++) {
        /* Create attribute */
        sprintf(attrname, "attr %02u", u);
        attr = H5Acreate2(dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(attr, FAIL, "H5Acreate2");

        /* Write data into the attribute */
        ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
        CHECK(ret, FAIL, "H5Awrite");

        /* Close attribute */
        ret = H5Aclose(attr);
        CHECK(ret, FAIL, "H5Aclose");

        /* Check # of attributes */
        ret = H5Oget_info(dataset, &oinfo);
        CHECK(ret, FAIL, "H5Oget_info");
        VERIFY(oinfo.num_attrs, (u + 1), "H5Oget_info");
    } /* end for */

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Open dataset */
    dataset = H5Dopen2(fid, DSET1_NAME, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dopen2");

    /* Delete attributes until the attributes revert to compact storage again */
    for(u--; u >= min_dense; u--) {
        /* Delete attribute */
        sprintf(attrname, "attr %02u", u);
        ret = H5Adelete(dataset, attrname);
        CHECK(ret, FAIL, "H5Adelete");

        /* Verify attributes still left */
        ret = test_attr_dense_verify(dataset, u);
        CHECK(ret, FAIL, "test_attr_dense_verify");
    } /* end for */

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

    /* Delete one more attribute, which should cause reversion to compact storage */
    sprintf(attrname, "attr %02u", u);
    ret = H5Adelete(dataset, attrname);
    CHECK(ret, FAIL, "H5Adelete");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

    /* Verify attributes still left */
    ret = test_attr_dense_verify(dataset, (u - 1));
    CHECK(ret, FAIL, "test_attr_dense_verify");

    /* Delete another attribute, to verify deletion in compact storage */
    sprintf(attrname, "attr %02u", (u - 1));
    ret = H5Adelete(dataset, attrname);
    CHECK(ret, FAIL, "H5Adelete");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

    /* Verify attributes still left */
    ret = test_attr_dense_verify(dataset, (u - 2));
    CHECK(ret, FAIL, "test_attr_dense_verify");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Unlink dataset with attributes */
    ret = H5Ldelete(fid, DSET1_NAME, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Ldelete");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Check size of file */
    filesize = h5_get_file_size(FILENAME, fapl);
    VERIFY(filesize, empty_filesize, "h5_get_file_size");
}   /* test_attr_dense_delete() */


/****************************************************************
**
**  test_attr_dense_rename(): Test basic H5A (attribute) code.
**      Tests renaming attributes in "dense" storage
**
****************************************************************/
static void
test_attr_dense_rename(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dataset;	/* Dataset ID			*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute */
    char	new_attrname[NAME_BUF_SIZE];    /* New name of attribute */
    unsigned    max_compact;    /* Maximum # of attributes to store compactly */
    unsigned    min_dense;      /* Minimum # of attributes to store "densely" */
    htri_t	is_dense;	/* Are attributes stored densely? */
    h5_stat_size_t empty_filesize;       /* Size of empty file */
    h5_stat_size_t filesize;             /* Size of file after modifications */
    H5O_info_t  oinfo;          /* Object info */
    unsigned    u;              /* Local index variable */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Renaming Attributes in Dense Storage\n"));

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Get size of file */
    empty_filesize = h5_get_file_size(FILENAME, fapl);
    if(empty_filesize < 0)
        TestErrPrintf("Line %d: file size wrong!\n", __LINE__);

    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Create dataspace for dataset */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Query the group creation properties */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Create a dataset */
    dataset = H5Dcreate2(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Retrieve limits for compact/dense attribute storage */
    ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");

    /* Close property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

    /* Add attributes, until well into dense storage */
    for(u = 0; u < (max_compact * 2); u++) {
        /* Create attribute */
        sprintf(attrname, "attr %02u", u);
        attr = H5Acreate2(dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(attr, FAIL, "H5Acreate2");

        /* Write data into the attribute */
        ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
        CHECK(ret, FAIL, "H5Awrite");

        /* Close attribute */
        ret = H5Aclose(attr);
        CHECK(ret, FAIL, "H5Aclose");

        /* Rename attribute */
        sprintf(new_attrname, "new attr %02u", u);

        /* Rename attribute */
        ret = H5Arename_by_name(fid, DSET1_NAME, attrname, new_attrname, H5P_DEFAULT);
        CHECK(ret, FAIL, "H5Arename_by_name");

        /* Check # of attributes */
        ret = H5Oget_info(dataset, &oinfo);
        CHECK(ret, FAIL, "H5Oget_info");
        VERIFY(oinfo.num_attrs, (u + 1), "H5Oget_info");
    } /* end for */

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Open dataset */
    dataset = H5Dopen2(fid, DSET1_NAME, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dopen2");

    /* Verify renamed attributes */
    for(u = 0; u < (max_compact * 2); u++) {
        unsigned    value;          /* Attribute value */

        /* Open attribute */
        sprintf(attrname, "new attr %02u", u);
        attr = H5Aopen(dataset, attrname, H5P_DEFAULT);
        CHECK(attr, FAIL, "H5Aopen");

        /* Read data from the attribute */
        ret = H5Aread(attr, H5T_NATIVE_UINT, &value);
        CHECK(ret, FAIL, "H5Aread");
        VERIFY(value, u, "H5Aread");

        /* Close attribute */
        ret = H5Aclose(attr);
        CHECK(ret, FAIL, "H5Aclose");
    } /* end for */

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Unlink dataset with attributes */
    ret = H5Ldelete(fid, DSET1_NAME, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Ldelete");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Check size of file */
    filesize = h5_get_file_size(FILENAME, fapl);
    VERIFY(filesize, empty_filesize, "h5_get_file_size");
}   /* test_attr_dense_rename() */


/****************************************************************
**
**  test_attr_dense_unlink(): Test basic H5A (attribute) code.
**      Tests unlinking object with attributes in "dense" storage
**
****************************************************************/
static void
test_attr_dense_unlink(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dataset;	/* Dataset ID			*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute */
    unsigned    max_compact;    /* Maximum # of attributes to store compactly */
    unsigned    min_dense;      /* Minimum # of attributes to store "densely" */
    htri_t	is_dense;	/* Are attributes stored densely? */
    size_t      mesg_count;     /* # of shared messages */
    h5_stat_size_t empty_filesize;       /* Size of empty file */
    h5_stat_size_t filesize;             /* Size of file after modifications */
    H5O_info_t  oinfo;          /* Object info */
    unsigned    u;              /* Local index variable */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Unlinking Object with Attributes in Dense Storage\n"));

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Get size of file */
    empty_filesize = h5_get_file_size(FILENAME, fapl);
    if(empty_filesize < 0)
        TestErrPrintf("Line %d: file size wrong!\n", __LINE__);

    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Create dataspace for dataset */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Query the group creation properties */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Create a dataset */
    dataset = H5Dcreate2(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Retrieve limits for compact/dense attribute storage */
    ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");

    /* Close property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

    /* Add attributes, until well into dense storage */
    for(u = 0; u < (max_compact * 2); u++) {
        /* Create attribute */
        sprintf(attrname, "attr %02u", u);
        attr = H5Acreate2(dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(attr, FAIL, "H5Acreate2");

        /* Write data into the attribute */
        ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
        CHECK(ret, FAIL, "H5Awrite");

        /* Close attribute */
        ret = H5Aclose(attr);
        CHECK(ret, FAIL, "H5Aclose");

        /* Check # of attributes */
        ret = H5Oget_info(dataset, &oinfo);
        CHECK(ret, FAIL, "H5Oget_info");
        VERIFY(oinfo.num_attrs, (u + 1), "H5Oget_info");
    } /* end for */

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Unlink dataset */
    ret = H5Ldelete(fid, DSET1_NAME, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Ldelete");

    /* Check on dataset's attribute storage status */
    ret = H5F_get_sohm_mesg_count_test(fid, H5O_ATTR_ID, &mesg_count);
    CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
    VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Check size of file */
    filesize = h5_get_file_size(FILENAME, fapl);
    VERIFY(filesize, empty_filesize, "h5_get_file_size");
}   /* test_attr_dense_unlink() */


/****************************************************************
**
**  test_attr_dense_limits(): Test basic H5A (attribute) code.
**      Tests attribute in "dense" storage limits
**
****************************************************************/
static void
test_attr_dense_limits(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dataset;	/* Dataset ID			*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute */
    unsigned    max_compact, rmax_compact;      /* Maximum # of attributes to store compactly */
    unsigned    min_dense, rmin_dense;          /* Minimum # of attributes to store "densely" */
    htri_t	is_dense;	/* Are attributes stored densely? */
    unsigned    u;              /* Local index variable */
    h5_stat_size_t empty_filesize;       /* Size of empty file */
    h5_stat_size_t filesize;             /* Size of file after modifications */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Phase Change Limits For Attributes in Dense Storage\n"));

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Get size of file */
    empty_filesize = h5_get_file_size(FILENAME, fapl);
    if(empty_filesize < 0)
        TestErrPrintf("Line %d: file size wrong!\n", __LINE__);

    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Create dataspace for dataset */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Query the group creation properties */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Change limits on compact/dense attribute storage */
    max_compact = 0;
    min_dense = 0;
    ret = H5Pset_attr_phase_change(dcpl, max_compact, min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");

    /* Create a dataset */
    dataset = H5Dcreate2(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Retrieve limits for compact/dense attribute storage */
    ret = H5Pget_attr_phase_change(dcpl, &rmax_compact, &rmin_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");
    VERIFY(rmax_compact, max_compact, "H5Pget_attr_phase_change");
    VERIFY(rmin_dense, min_dense, "H5Pget_attr_phase_change");

    /* Close property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");


    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");


    /* Add first attribute, which should be immediately in dense storage */

    /* Create attribute */
    u = 0;
    sprintf(attrname, "attr %02u", u);
    attr = H5Acreate2(dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate2");

    /* Write data into the attribute */
    ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
    CHECK(ret, FAIL, "H5Awrite");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");


    /* Add second attribute, to allow deletions to be checked easily */

    /* Create attribute */
    u = 1;
    sprintf(attrname, "attr %02u", u);
    attr = H5Acreate2(dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate2");

    /* Write data into the attribute */
    ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
    CHECK(ret, FAIL, "H5Awrite");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");


    /* Delete second attribute, attributes should still be stored densely */

    /* Delete attribute */
    ret = H5Adelete(dataset, attrname);
    CHECK(ret, FAIL, "H5Adelete");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");


    /* Delete first attribute, attributes should not be stored densely */

    /* Delete attribute */
    u = 0;
    sprintf(attrname, "attr %02u", u);
    ret = H5Adelete(dataset, attrname);
    CHECK(ret, FAIL, "H5Adelete");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");


    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Unlink dataset */
    ret = H5Ldelete(fid, DSET1_NAME, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Ldelete");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Check size of file */
    filesize = h5_get_file_size(FILENAME, fapl);
    VERIFY(filesize, empty_filesize, "h5_get_file_size");
}   /* test_attr_dense_limits() */


/****************************************************************
**
**  test_attr_dense_dup_ids(): Test operations with multiple ID
**      handles with "dense" attribute storage creation
**
****************************************************************/
static void
test_attr_dense_dup_ids(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dataset;	/* Dataset ID			*/
    hid_t	gid1, gid2;	/* Group ID			*/
    hid_t	sid, sid2;	/* Dataspace ID			*/
    hid_t	attr, attr2, add_attr;	/* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute */
    hsize_t	dims[] = {ATTR1_DIM1};
    int         read_data1[ATTR1_DIM1]={0}; /* Buffer for reading attribute */
    int         rewrite_data[ATTR1_DIM1]={1234, -423, 9907256}; /* Test data for rewrite */
    unsigned    scalar_data = 1317; /* scalar data for attribute */
    unsigned    read_scalar;    /* variable for reading attribute*/
    unsigned    max_compact;    /* Maximum # of attributes to store compactly */
    unsigned    min_dense;      /* Minimum # of attributes to store "densely" */
    htri_t	is_dense;	/* Are attributes stored densely? */
    unsigned    u, i;           /* Local index variable */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing operations with two IDs for Dense Storage\n"));

    /*-----------------------------------------------------------------------------------
     *        Create an attribute in dense storage and fill it with fill value.
     */
    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Create dataspace for dataset */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Query the group creation properties */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Create a dataset */
    dataset = H5Dcreate2(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Retrieve limits for compact/dense attribute storage */
    ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");

    /* Close property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

    /* Add attributes, until just before converting to dense storage */
    for(u = 0; u < max_compact; u++) {
        /* Create attribute */
        sprintf(attrname, "attr %02u", u);
        attr = H5Acreate2(dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(attr, FAIL, "H5Acreate2");

        /* Write data into the attribute */
        ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
        CHECK(ret, FAIL, "H5Awrite");

        /* Close attribute */
        ret = H5Aclose(attr);
        CHECK(ret, FAIL, "H5Aclose");
    } /* end for */

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

    /* Add one more attribute, to push into "dense" storage */
    /* Create dataspace for attribute */
    sid2 = H5Screate_simple(ATTR1_RANK, dims, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Create attribute */
    sprintf(attrname, "attr %02u", u);
    attr = H5Acreate2(dataset, attrname, H5T_NATIVE_INT, sid2, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate2");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

    /* Open the attribute just created and get a second ID */
    attr2 = H5Aopen(dataset, attrname, H5P_DEFAULT);
    CHECK(attr2, FAIL, "H5Aopen");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    ret = H5Aclose(attr2);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /*-----------------------------------------------------------------------------------
     *        Reopen the file and verify the fill value for attribute.  Also write
     *        some real data.
     */
    /* Open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Open the dataset */
    dataset = H5Dopen2(fid, DSET1_NAME, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dopen2");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

    /* Open first attribute for the dataset */
    attr = H5Aopen(dataset, attrname, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Aopen");

    /* Read attribute with fill value */
    ret = H5Aread(attr, H5T_NATIVE_INT, read_data1);
    CHECK(ret, FAIL, "H5Aread");

    /* Verify values read in */
    for(i = 0; i < ATTR1_DIM1; i++)
        if(0 != read_data1[i])
            TestErrPrintf("%d: attribute data different: read_data1[%d]=%d\n", __LINE__,
                i, read_data1[i]);

    /* Open attribute for the second time */
    attr2 = H5Aopen(dataset, attrname, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Aopen");

    /* Write attribute information */
    ret = H5Awrite(attr2, H5T_NATIVE_INT, attr_data1);
    CHECK(ret, FAIL, "H5Awrite");

    /* Close attribute */
    ret = H5Aclose(attr2);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /*-----------------------------------------------------------------------------------
     *        Reopen the file and verify the data.  Also rewrite the data and verify it.
     */
    /* Open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Open the dataset */
    dataset = H5Dopen2(fid, DSET1_NAME, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dopen2");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

    /* Open first attribute for the dataset */
    attr = H5Aopen(dataset, attrname, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Aopen");

    /* Read attribute information */
    ret = H5Aread(attr, H5T_NATIVE_INT, read_data1);
    CHECK(ret, FAIL, "H5Aread");

    /* Verify values read in */
    for(i = 0; i < ATTR1_DIM1; i++)
        if(attr_data1[i] != read_data1[i])
            TestErrPrintf("%d: attribute data different: attr_data1[%d]=%d, read_data1[%d]=%d\n", __LINE__, i, attr_data1[i], i, read_data1[i]);

    /* Open attribute for the second time */
    attr2 = H5Aopen(dataset, attrname, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Aopen");

    /* Write attribute information with the second ID */
    ret = H5Awrite(attr2, H5T_NATIVE_INT, rewrite_data);
    CHECK(ret, FAIL, "H5Awrite");

    /* Read attribute information with the first ID */
    ret = H5Aread(attr, H5T_NATIVE_INT, read_data1);
    CHECK(ret, FAIL, "H5Aread");

    /* Verify values read in */
    for(i = 0; i < ATTR1_DIM1; i++)
        if(read_data1[i] != rewrite_data[i])
            TestErrPrintf("%d: attribute data different: read_data1[%d]=%d, rewrite_data[%d]=%d\n", __LINE__, i, read_data1[i], i, rewrite_data[i]);

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    ret = H5Aclose(attr2);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /*-----------------------------------------------------------------------------------
     *        Open the attribute by index.  Verify the data is shared when the attribute
     *        is opened twice.
     */
    /* Open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Open the dataset */
    dataset = H5Dopen2(fid, DSET1_NAME, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dopen2");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

    /* Open first attribute for the dataset */
    attr = H5Aopen_by_idx(dataset, ".", H5_INDEX_NAME, H5_ITER_INC, 4,
        H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Aopen");

    /* Open attribute for the second time */
    attr2 = H5Aopen_by_idx(dataset, ".", H5_INDEX_NAME, H5_ITER_INC, 4,
        H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Aopen");

    /* Write attribute information with the second ID */
    ret = H5Awrite(attr2, H5T_NATIVE_UINT, &scalar_data);
    CHECK(ret, FAIL, "H5Awrite");

    /* Read attribute information with the first ID */
    ret = H5Aread(attr, H5T_NATIVE_INT, &read_scalar);
    CHECK(ret, FAIL, "H5Aread");

    /* Verify values read in */
    if(read_scalar != scalar_data)
        TestErrPrintf("%d: attribute data different: read_scalar=%d, scalar_data=%d\n",
            __LINE__, read_scalar, scalar_data);

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    ret = H5Aclose(attr2);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /*-----------------------------------------------------------------------------------
     *        Open one attribute.  As it remains open, delete some attributes.  The
     *        attribute storage should switch from dense to compact.  Then open the
     *        same attribute for the second time and verify that the attribute data
     *        is shared.
     */
    /* Open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Open the dataset */
    dataset = H5Dopen2(fid, DSET1_NAME, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dopen2");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

    /* Open attribute of the dataset for the first time */
    attr = H5Aopen_by_idx(dataset, ".", H5_INDEX_NAME, H5_ITER_INC, 2,
        H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Aopen");

    /* Delete a few attributes until the storage switches to compact */
    for(u = max_compact; u >= min_dense - 1; u--) {
        ret = H5Adelete_by_idx(dataset, ".", H5_INDEX_NAME, H5_ITER_INC, u,
            H5P_DEFAULT);
        CHECK(ret, FAIL, "H5Adelete_by_idx");
    }

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

    /* Open attribute for the second time */
    attr2 = H5Aopen_by_idx(dataset, ".", H5_INDEX_NAME, H5_ITER_INC, 2,
        H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Aopen");

    /* Write attribute information with the second ID */
    ret = H5Awrite(attr2, H5T_NATIVE_UINT, &scalar_data);
    CHECK(ret, FAIL, "H5Awrite");

    /* Read attribute information with the first ID */
    ret = H5Aread(attr, H5T_NATIVE_INT, &read_scalar);
    CHECK(ret, FAIL, "H5Aread");

    /* Verify values read in */
    if(read_scalar != scalar_data)
        TestErrPrintf("%d: attribute data different: read_scalar=%d, scalar_data=%d\n",
            __LINE__, read_scalar, scalar_data);

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    ret = H5Aclose(attr2);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /*-----------------------------------------------------------------------------------
     *        Open one attribute.  As it remains open, create some attributes.  The
     *        attribute storage should switch from compact to dense.  Then open the
     *        same attribute for the second time and verify that the attribute data
     *        is shared.
     */
    /* Open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Open the dataset */
    dataset = H5Dopen2(fid, DSET1_NAME, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dopen2");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

    /* Open attribute of the dataset for the first time */
    attr = H5Aopen_by_idx(dataset, ".", H5_INDEX_NAME, H5_ITER_INC, 3,
        H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Aopen");

    /* Delete a few attributes until the storage switches to compact */
    for(u = min_dense-1; u <= max_compact; u++) {
        /* Create attribute */
        sprintf(attrname, "attr %02u", u);
        add_attr = H5Acreate2(dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(add_attr, FAIL, "H5Acreate2");

        /* Write data into the attribute */
        ret = H5Awrite(add_attr, H5T_NATIVE_UINT, &u);
        CHECK(ret, FAIL, "H5Awrite");

        /* Close attribute */
        ret = H5Aclose(add_attr);
        CHECK(ret, FAIL, "H5Aclose");
    }

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

    /* Open attribute for the second time */
    attr2 = H5Aopen_by_idx(dataset, ".", H5_INDEX_NAME, H5_ITER_INC, 3,
        H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Aopen");

    /* Write attribute information with the second ID */
    ret = H5Awrite(attr2, H5T_NATIVE_UINT, &scalar_data);
    CHECK(ret, FAIL, "H5Awrite");

    /* Read attribute information with the first ID */
    ret = H5Aread(attr, H5T_NATIVE_INT, &read_scalar);
    CHECK(ret, FAIL, "H5Aread");

    /* Verify values read in */
    if(read_scalar != scalar_data)
        TestErrPrintf("%d: attribute data different: read_scalar=%d, scalar_data=%d\n",
            __LINE__, read_scalar, scalar_data);

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    ret = H5Aclose(attr2);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /*-----------------------------------------------------------------------------------
     *        Verify that the attribute being pointed to by different paths shares
     *        the same data.
     */
    /* Open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Create a group */
    gid1 = H5Gcreate2(fid, GROUP1_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(gid1, FAIL, "H5Gcreate2");

    /* Create hard link to the first group */
    ret = H5Lcreate_hard(gid1, GROUP1_NAME, H5L_SAME_LOC, GROUP2_NAME, H5P_DEFAULT,
        H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Lcreate_hard");

    /* Add attributes, until just before converting to dense storage */
    for(u = 0; u < max_compact; u++) {
        /* Create attribute */
        sprintf(attrname, "attr %02u", u);
        attr = H5Acreate2(gid1, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(attr, FAIL, "H5Acreate2");

        /* Write data into the attribute */
        ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
        CHECK(ret, FAIL, "H5Awrite");

        /* Close attribute */
        ret = H5Aclose(attr);
        CHECK(ret, FAIL, "H5Aclose");
    } /* end for */

    /* Try to create another attribute to make dense storage */
    attr = H5Acreate2(gid1, ATTR2_NAME, H5T_NATIVE_INT, sid2, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate2");

    /* Check on group's attribute storage status */
    is_dense = H5O_is_attr_dense_test(gid1);
    VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

    /* Open the hard link just created */
    gid2 = H5Gopen2(fid, GROUP2_NAME, H5P_DEFAULT);
    CHECK(gid2, FAIL, "H5Gopen2");

    /* Open the attribute of the group for the second time */
    attr2 = H5Aopen(gid2, ATTR2_NAME, H5P_DEFAULT);
    CHECK(attr2, FAIL, "H5Aopen");

    /* Write attribute information with the first attribute handle */
    ret = H5Awrite(attr, H5T_NATIVE_INT, attr_data1);
    CHECK(ret, FAIL, "H5Awrite");

    /* Read attribute information with the second attribute handle */
    ret = H5Aread(attr2, H5T_NATIVE_INT, read_data1);
    CHECK(ret, FAIL, "H5Aread");

    /* Verify values read in */
    for(i = 0; i < ATTR1_DIM1; i++)
        if(attr_data1[i] != read_data1[i])
            TestErrPrintf("%d: attribute data different: attr_data1[%d]=%d, read_data1[%d]=%d\n", __LINE__, i, attr_data1[i], i, read_data1[i]);

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    ret = H5Aclose(attr2);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close group */
    ret = H5Gclose(gid1);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Gclose(gid2);
    CHECK(ret, FAIL, "H5Gclose");

    /* Close Attribute dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Dataset dataspace */
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_dense_dup_ids() */


/****************************************************************
**
**  test_attr_big(): Test basic H5A (attribute) code.
**      Tests storing "big" attribute in dense storage immediately, if available
**
****************************************************************/
static void
test_attr_big(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dataset;	/* Dataset ID			*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	big_sid;	/* "Big" dataspace ID		*/
    hsize_t     dims[ATTR6_RANK] = {ATTR6_DIM1, ATTR6_DIM2, ATTR6_DIM3};           /* Attribute dimensions */
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute */
    unsigned    max_compact;    /* Maximum # of attributes to store compactly */
    unsigned    min_dense;      /* Minimum # of attributes to store "densely" */
    unsigned    nshared_indices;        /* # of shared message indices */
    H5F_libver_t low, high;     /* File format bounds */
    htri_t	is_empty;	/* Are there any attributes? */
    htri_t	is_dense;	/* Are attributes stored densely? */
    unsigned    u;              /* Local index variable */
    h5_stat_size_t empty_filesize;       /* Size of empty file */
    h5_stat_size_t filesize;             /* Size of file after modifications */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Storing 'Big' Attributes in Dense Storage\n"));

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Get size of file */
    empty_filesize = h5_get_file_size(FILENAME, fapl);
    if(empty_filesize < 0)
        TestErrPrintf("Line %d: file size wrong!\n", __LINE__);

    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Create dataspace for dataset & "small" attributes */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create "big" dataspace for "big" attributes */
    big_sid = H5Screate_simple(ATTR6_RANK, dims, NULL);
    CHECK(big_sid, FAIL, "H5Screate_simple");

    /* Query the group creation properties */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Retrieve limits for compact/dense attribute storage */
    ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");

    /* Retrieve # of shared message indices (ie. whether attributes are shared or not) */
    ret = H5Pget_shared_mesg_nindexes(fcpl, &nshared_indices);
    CHECK(ret, FAIL, "H5Pget_shared_mesg_nindexes");

    /* Retrieve the format bounds for creating objects in the file */
    ret = H5Pget_libver_bounds(fapl, &low, &high);
    CHECK(ret, FAIL, "H5Pget_libver_bounds");

    /* Create a dataset */
    dataset = H5Dcreate2(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Close property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");


    /* Check on dataset's attribute storage status */
    is_empty = H5O_is_attr_empty_test(dataset);
    VERIFY(is_empty, TRUE, "H5O_is_attr_empty_test");
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");


    /* Add first "small" attribute, which should be in compact storage */

    /* Create attribute */
    u = 0;
    sprintf(attrname, "attr %02u", u);
    attr = H5Acreate2(dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate2");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Check on dataset's attribute storage status */
    is_empty = H5O_is_attr_empty_test(dataset);
    VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");


    /* Add second "small" attribute, which should stay in compact storage */

    /* Create attribute */
    u = 1;
    sprintf(attrname, "attr %02u", u);
    attr = H5Acreate2(dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate2");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Check on dataset's attribute storage status */
    is_empty = H5O_is_attr_empty_test(dataset);
    VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");


    /* Add first "big" attribute, which should push storage into dense form */

    /* Create attribute */
    u = 2;
    sprintf(attrname, "attr %02u", u);
    attr = H5Acreate2(dataset, attrname, H5T_NATIVE_UINT, big_sid, H5P_DEFAULT, H5P_DEFAULT);
    if(low == H5F_LIBVER_LATEST) {
        CHECK(attr, FAIL, "H5Acreate2");

        /* Close attribute */
        ret = H5Aclose(attr);
        CHECK(ret, FAIL, "H5Aclose");

        /* Check on dataset's attribute storage status */
        /* (when attributes are shared, the "big" attribute goes into the shared
         *  message heap instead of forcing the attribute storage into the dense
         *  form - QAK)
         */
        is_empty = H5O_is_attr_empty_test(dataset);
        VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
        is_dense = H5O_is_attr_dense_test(dataset);
        VERIFY(is_dense, (nshared_indices ? FALSE : TRUE), "H5O_is_attr_dense_test");


        /* Add second "big" attribute, which should leave storage in dense form */

        /* Create attribute */
        u = 3;
        sprintf(attrname, "attr %02u", u);
        attr = H5Acreate2(dataset, attrname, H5T_NATIVE_UINT, big_sid, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(attr, FAIL, "H5Acreate2");

        /* Close attribute */
        ret = H5Aclose(attr);
        CHECK(ret, FAIL, "H5Aclose");

        /* Check on dataset's attribute storage status */
        /* (when attributes are shared, the "big" attribute goes into the shared
         *  message heap instead of forcing the attribute storage into the dense
         *  form - QAK)
         */
        is_empty = H5O_is_attr_empty_test(dataset);
        VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
        is_dense = H5O_is_attr_dense_test(dataset);
        VERIFY(is_dense, (nshared_indices ? FALSE : TRUE), "H5O_is_attr_dense_test");


        /* Delete second "small" attribute, attributes should still be stored densely */

        /* Delete attribute */
        u = 1;
        sprintf(attrname, "attr %02u", u);
        ret = H5Adelete(dataset, attrname);
        CHECK(ret, FAIL, "H5Adelete");

        /* Check on dataset's attribute storage status */
        is_empty = H5O_is_attr_empty_test(dataset);
        VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
        is_dense = H5O_is_attr_dense_test(dataset);
        VERIFY(is_dense, (nshared_indices ? FALSE : TRUE), "H5O_is_attr_dense_test");


        /* Delete second "big" attribute, attributes should still be stored densely */

        /* Delete attribute */
        u = 3;
        sprintf(attrname, "attr %02u", u);
        ret = H5Adelete(dataset, attrname);
        CHECK(ret, FAIL, "H5Adelete");

        /* Check on dataset's attribute storage status */
        is_empty = H5O_is_attr_empty_test(dataset);
        VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
        is_dense = H5O_is_attr_dense_test(dataset);
        VERIFY(is_dense, (nshared_indices ? FALSE : TRUE), "H5O_is_attr_dense_test");


        /* Delete first "big" attribute, attributes should _not_ be stored densely */

        /* Delete attribute */
        u = 2;
        sprintf(attrname, "attr %02u", u);
        ret = H5Adelete(dataset, attrname);
        CHECK(ret, FAIL, "H5Adelete");

        /* Check on dataset's attribute storage status */
        is_empty = H5O_is_attr_empty_test(dataset);
        VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
        is_dense = H5O_is_attr_dense_test(dataset);
        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");


        /* Delete first "small" attribute, should be no attributes now */

        /* Delete attribute */
        u = 0;
        sprintf(attrname, "attr %02u", u);
        ret = H5Adelete(dataset, attrname);
        CHECK(ret, FAIL, "H5Adelete");

        /* Check on dataset's attribute storage status */
        is_empty = H5O_is_attr_empty_test(dataset);
        VERIFY(is_empty, TRUE, "H5O_is_attr_empty_test");
    } /* end if */
    else {
        /* Shouldn't be able to create "big" attributes with older version of format */
        VERIFY(attr, FAIL, "H5Acreate2");

        /* Check on dataset's attribute storage status */
        /* (when attributes are shared, the "big" attribute goes into the shared
         *  message heap instead of forcing the attribute storage into the dense
         *  form - QAK)
         */
        is_empty = H5O_is_attr_empty_test(dataset);
        VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
        is_dense = H5O_is_attr_dense_test(dataset);
        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
    } /* end else */


    /* Close dataspaces */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(big_sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Unlink dataset */
    ret = H5Ldelete(fid, DSET1_NAME, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Ldelete");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Check size of file */
    filesize = h5_get_file_size(FILENAME, fapl);
    VERIFY(filesize, empty_filesize, "h5_get_file_size");
}   /* test_attr_big() */


/****************************************************************
**
**  test_attr_null_space(): Test basic H5A (attribute) code.
**      Tests storing attribute with "null" dataspace
**
****************************************************************/
static void
test_attr_null_space(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dataset;	/* Dataset ID			*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	null_sid;	/* "null" dataspace ID		*/
    hid_t	attr_sid;	/* Attribute's dataspace ID	*/
    hid_t	attr;	        /* Attribute ID			*/
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute */
    unsigned    value;          /* Attribute value */
    htri_t      cmp;            /* Results of comparison */
    hsize_t     storage_size;   /* Size of storage for attribute */
    H5A_info_t  ainfo;          /* Attribute info */
    h5_stat_size_t empty_filesize;       /* Size of empty file */
    h5_stat_size_t filesize;             /* Size of file after modifications */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Storing Attributes with 'null' dataspace\n"));

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Get size of file */
    empty_filesize = h5_get_file_size(FILENAME, fapl);
    if(empty_filesize < 0)
        TestErrPrintf("Line %d: file size wrong!\n", __LINE__);

    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Create dataspace for dataset attributes */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create "null" dataspace for attribute */
    null_sid = H5Screate(H5S_NULL);
    CHECK(null_sid, FAIL, "H5Screate");

    /* Create a dataset */
    dataset = H5Dcreate2(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");


    /* Add attribute with 'null' dataspace */

    /* Create attribute */
    HDstrcpy(attrname, "null attr");
    attr = H5Acreate2(dataset, attrname, H5T_NATIVE_UINT, null_sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate2");

    /* Try to read data from the attribute */
    /* (shouldn't fail, but should leave buffer alone) */
    value = 23;
    ret = H5Aread(attr, H5T_NATIVE_UINT, &value);
    CHECK(ret, FAIL, "H5Aread");
    VERIFY(value, 23, "H5Aread");

    /* Get the dataspace for the attribute and make certain it's 'null' */
    attr_sid = H5Aget_space(attr);
    CHECK(attr_sid, FAIL, "H5Aget_space");

    /* Compare the dataspaces */
    cmp = H5Sextent_equal(attr_sid, null_sid);
    CHECK(cmp, FAIL, "H5Sextent_equal");
    VERIFY(cmp, TRUE, "H5Sextent_equal");

    /* Close dataspace */
    ret = H5Sclose(attr_sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Check the storage size for the attribute */
    storage_size = H5Aget_storage_size(attr);
    VERIFY(storage_size, 0, "H5Aget_storage_size");

    /* Get the attribute info */
    ret = H5Aget_info(attr, &ainfo);
    CHECK(ret, FAIL, "H5Aget_info");
    VERIFY(ainfo.data_size, storage_size, "H5Aget_info");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");


    /* Add another attribute with 'null' dataspace */

    /* Create attribute */
    HDstrcpy(attrname, "null attr #2");
    attr = H5Acreate2(dataset, attrname, H5T_NATIVE_UINT, null_sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate2");

    /* Try to write data to the attribute */
    /* (shouldn't fail, but should leave buffer alone) */
    value = 23;
    ret = H5Awrite(attr, H5T_NATIVE_UINT, &value);
    CHECK(ret, FAIL, "H5Awrite");
    VERIFY(value, 23, "H5Awrite");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    /* Re-open the file and check on the attributes */

    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Open dataset */
    dataset = H5Dopen2(fid, DSET1_NAME, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dopen2");


    /* Open first attribute */
    HDstrcpy(attrname, "null attr #2");
    attr = H5Aopen(dataset, attrname, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Aopen");

    /* Try to read data from the attribute */
    /* (shouldn't fail, but should leave buffer alone) */
    value = 23;
    ret = H5Aread(attr, H5T_NATIVE_UINT, &value);
    CHECK(ret, FAIL, "H5Aread");
    VERIFY(value, 23, "H5Aread");

    /* Get the dataspace for the attribute and make certain it's 'null' */
    attr_sid = H5Aget_space(attr);
    CHECK(attr_sid, FAIL, "H5Aget_space");

    /* Compare the dataspaces */
    cmp = H5Sextent_equal(attr_sid, null_sid);
    CHECK(cmp, FAIL, "H5Sextent_equal");
    VERIFY(cmp, TRUE, "H5Sextent_equal");

    /* Close dataspace */
    ret = H5Sclose(attr_sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Check the storage size for the attribute */
    storage_size = H5Aget_storage_size(attr);
    VERIFY(storage_size, 0, "H5Aget_storage_size");

    /* Get the attribute info */
    ret = H5Aget_info(attr, &ainfo);
    CHECK(ret, FAIL, "H5Aget_info");
    VERIFY(ainfo.data_size, storage_size, "H5Aget_info");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");


    /* Open second attribute */
    HDstrcpy(attrname, "null attr");
    attr = H5Aopen(dataset, attrname, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Aopen");

    /* Try to write data to the attribute */
    /* (shouldn't fail, but should leave buffer alone) */
    value = 23;
    ret = H5Awrite(attr, H5T_NATIVE_UINT, &value);
    CHECK(ret, FAIL, "H5Awrite");
    VERIFY(value, 23, "H5Awrite");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");


    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Unlink dataset */
    ret = H5Ldelete(fid, DSET1_NAME, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Ldelete");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    /* Close dataspaces */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(null_sid);
    CHECK(ret, FAIL, "H5Sclose");


    /* Check size of file */
    filesize = h5_get_file_size(FILENAME, fapl);
    VERIFY(filesize, empty_filesize, "h5_get_file_size");
}   /* test_attr_null_space() */


/****************************************************************
**
**  test_attr_deprec(): Test basic H5A (attribute) code.
**      Tests deprecated API routines
**
****************************************************************/
static void
test_attr_deprec(hid_t fcpl, hid_t fapl)
{
#ifndef H5_NO_DEPRECATED_SYMBOLS
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dataset;	/* Dataset ID			*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	attr;	        /* Attribute ID			*/
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Deprecated Attribute Routines\n"));

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create dataspace for dataset attributes */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create a dataset */
    dataset = H5Dcreate2(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");


    /* Add attribute to dataset */

    /* Create attribute */
    attr = H5Acreate1(dataset, "attr", H5T_NATIVE_UINT, sid, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate1");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");


    /* Close dataspaces */
    ret = H5Sclose(sid);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    /* Re-open the file and operate on the attribute */

    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Open dataset */
    dataset = H5Dopen2(fid, DSET1_NAME, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dopen2");


    /* Get number of attributes with bad ID */
    ret = H5Aget_num_attrs(-1);
    VERIFY(ret, FAIL, "H5Aget_num_attrs");

    /* Get number of attributes */
    ret = H5Aget_num_attrs(dataset);
    VERIFY(ret, 1, "H5Aget_num_attrs");

    /* Open the attribute by index */
    attr = H5Aopen_idx(dataset, 0);
    CHECK(attr, FAIL, "H5Aopen_idx");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Open the attribute by name */
    attr = H5Aopen_name(dataset, "attr");
    CHECK(attr, FAIL, "H5Aopen_name");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");


    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
#else /* H5_NO_DEPRECATED_SYMBOLS */
    /* Shut compiler up */
    fcpl = fcpl; fapl = fapl;

    /* Output message about test being skipped */
    MESSAGE(5, ("Skipping Test On Deprecated Attribute Routines\n"));

#endif /* H5_NO_DEPRECATED_SYMBOLS */
}   /* test_attr_deprec() */


/****************************************************************
**
**  test_attr_many(): Test basic H5A (attribute) code.
**      Tests storing lots of attributes
**
****************************************************************/
static void
test_attr_many(hbool_t new_format, hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	gid;	        /* Group ID			*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	aid;	        /* Attribute ID			*/
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute */
    unsigned    nattr = (new_format ? NATTR_MANY_NEW : NATTR_MANY_OLD); /* Number of attributes */
    htri_t      exists;         /* Whether the attribute exists or not */
    unsigned    u;              /* Local index variable */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Storing Many Attributes\n"));

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create dataspace for attribute */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create group for attributes */
    gid = H5Gcreate2(fid, GROUP1_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gcreate2");

    /* Create many attributes */
    for(u = 0; u < nattr; u++) {
        sprintf(attrname, "a-%06u", u);

        exists = H5Aexists(gid, attrname);
        VERIFY(exists, FALSE, "H5Aexists");

        exists = H5Aexists_by_name(fid, GROUP1_NAME, attrname, H5P_DEFAULT);
        VERIFY(exists, FALSE, "H5Aexists_by_name");

        aid = H5Acreate2(gid, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(aid, FAIL, "H5Acreate2");

        exists = H5Aexists(gid, attrname);
        VERIFY(exists, TRUE, "H5Aexists");

        exists = H5Aexists_by_name(fid, GROUP1_NAME, attrname, H5P_DEFAULT);
        VERIFY(exists, TRUE, "H5Aexists_by_name");

        ret = H5Awrite(aid, H5T_NATIVE_UINT, &u);
        CHECK(ret, FAIL, "H5Awrite");

        ret = H5Aclose(aid);
        CHECK(ret, FAIL, "H5Aclose");

        exists = H5Aexists(gid, attrname);
        VERIFY(exists, TRUE, "H5Aexists");

        exists = H5Aexists_by_name(fid, GROUP1_NAME, attrname, H5P_DEFAULT);
        VERIFY(exists, TRUE, "H5Aexists_by_name");
    } /* end for */

    /* Close group */
    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    /* Re-open the file and check on the attributes */

    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDONLY, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Re-open group */
    gid = H5Gopen2(fid, GROUP1_NAME, H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gopen2");

    /* Verify attributes */
    for(u = 0; u < nattr; u++) {
        unsigned    value;          /* Attribute value */

        sprintf(attrname, "a-%06u", u);

        exists = H5Aexists(gid, attrname);
        VERIFY(exists, TRUE, "H5Aexists");

        exists = H5Aexists_by_name(fid, GROUP1_NAME, attrname, H5P_DEFAULT);
        VERIFY(exists, TRUE, "H5Aexists_by_name");

        aid = H5Aopen(gid, attrname, H5P_DEFAULT);
        CHECK(aid, FAIL, "H5Aopen");

        exists = H5Aexists(gid, attrname);
        VERIFY(exists, TRUE, "H5Aexists");

        exists = H5Aexists_by_name(fid, GROUP1_NAME, attrname, H5P_DEFAULT);
        VERIFY(exists, TRUE, "H5Aexists_by_name");

        ret = H5Aread(aid, H5T_NATIVE_UINT, &value);
        CHECK(ret, FAIL, "H5Aread");
        VERIFY(value, u, "H5Aread");

        ret = H5Aclose(aid);
        CHECK(ret, FAIL, "H5Aclose");
    } /* end for */

    /* Close group */
    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    /* Close dataspaces */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
}   /* test_attr_many() */


/****************************************************************
**
**  test_attr_corder_create_empty(): Test basic H5A (attribute) code.
**      Tests basic code to create objects with attribute creation order info
**
****************************************************************/
static void
test_attr_corder_create_basic(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dataset;	/* Dataset ID			*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    unsigned    crt_order_flags;/* Creation order flags */
    htri_t	is_empty;	/* Are there any attributes? */
    htri_t	is_dense;	/* Are attributes stored densely? */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Code for Attributes with Creation Order Info\n"));

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create dataset creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Get creation order indexing on object */
    ret = H5Pget_attr_creation_order(dcpl, &crt_order_flags);
    CHECK(ret, FAIL, "H5Pget_attr_creation_order");
    VERIFY(crt_order_flags, 0, "H5Pget_attr_creation_order");

    /* Setting invalid combination of a attribute order creation order indexing on should fail */
    ret = H5Pset_attr_creation_order(dcpl, H5P_CRT_ORDER_INDEXED);
    VERIFY(ret, FAIL, "H5Pset_attr_creation_order");
    ret = H5Pget_attr_creation_order(dcpl, &crt_order_flags);
    CHECK(ret, FAIL, "H5Pget_attr_creation_order");
    VERIFY(crt_order_flags, 0, "H5Pget_attr_creation_order");

    /* Set attribute creation order tracking & indexing for object */
    ret = H5Pset_attr_creation_order(dcpl, (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED));
    CHECK(ret, FAIL, "H5Pset_attr_creation_order");
    ret = H5Pget_attr_creation_order(dcpl, &crt_order_flags);
    CHECK(ret, FAIL, "H5Pget_attr_creation_order");
    VERIFY(crt_order_flags, (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED) , "H5Pget_attr_creation_order");

    /* Create dataspace for dataset */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create a dataset */
    dataset = H5Dcreate2(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Check on dataset's attribute storage status */
    is_empty = H5O_is_attr_empty_test(dataset);
    VERIFY(is_empty, TRUE, "H5O_is_attr_empty_test");
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Open dataset created */
    dataset = H5Dopen2(fid, DSET1_NAME, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dopen2");

    /* Check on dataset's attribute storage status */
    is_empty = H5O_is_attr_empty_test(dataset);
    VERIFY(is_empty, TRUE, "H5O_is_attr_empty_test");
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

    /* Retrieve dataset creation property list for group */
    dcpl = H5Dget_create_plist(dataset);
    CHECK(dcpl, FAIL, "H5Dget_create_plist");

    /* Query the attribute creation properties */
    ret = H5Pget_attr_creation_order(dcpl, &crt_order_flags);
    CHECK(ret, FAIL, "H5Pget_attr_creation_order");
    VERIFY(crt_order_flags, (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED) , "H5Pget_attr_creation_order");

    /* Close property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_corder_create_basic() */


/****************************************************************
**
**  test_attr_corder_create_compact(): Test basic H5A (attribute) code.
**      Tests compact attribute storage on objects with attribute creation order info
**
****************************************************************/
static void
test_attr_corder_create_compact(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dset1, dset2, dset3;	/* Dataset IDs			*/
    hid_t	my_dataset;	/* Current dataset ID		*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    unsigned    max_compact;    /* Maximum # of links to store in group compactly */
    unsigned    min_dense;      /* Minimum # of links to store in group "densely" */
    htri_t	is_empty;	/* Are there any attributes? */
    htri_t	is_dense;	/* Are attributes stored densely? */
    hsize_t     nattrs;         /* Number of attributes on object */
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute */
    unsigned    curr_dset;      /* Current dataset to work on */
    unsigned    u;              /* Local index variable */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Compact Storage of Attributes with Creation Order Info\n"));

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create dataset creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Set attribute creation order tracking & indexing for object */
    ret = H5Pset_attr_creation_order(dcpl, (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED));
    CHECK(ret, FAIL, "H5Pset_attr_creation_order");

    /* Query the attribute creation properties */
    ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");

    /* Create dataspace for dataset & attributes */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create datasets */
    dset1 = H5Dcreate2(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(dset1, FAIL, "H5Dcreate2");
    dset2 = H5Dcreate2(fid, DSET2_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(dset2, FAIL, "H5Dcreate2");
    dset3 = H5Dcreate2(fid, DSET3_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(dset3, FAIL, "H5Dcreate2");

    /* Work on all the datasets */
    for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
        switch(curr_dset) {
            case 0:
                my_dataset = dset1;
                break;

            case 1:
                my_dataset = dset2;
                break;

            case 2:
                my_dataset = dset3;
                break;

            default:
                HDassert(0 && "Too many datasets!");
        } /* end switch */

        /* Check on dataset's attribute storage status */
        is_empty = H5O_is_attr_empty_test(my_dataset);
        VERIFY(is_empty, TRUE, "H5O_is_attr_empty_test");
        is_dense = H5O_is_attr_dense_test(my_dataset);
        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

        /* Create several attributes, but keep storage in compact form */
        for(u = 0; u < max_compact; u++) {
            /* Create attribute */
            sprintf(attrname, "attr %02u", u);
            attr = H5Acreate2(my_dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
            CHECK(attr, FAIL, "H5Acreate2");

            /* Write data into the attribute */
            ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
            CHECK(ret, FAIL, "H5Awrite");

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");

            /* Verify state of object */
            ret = H5O_num_attrs_test(my_dataset, &nattrs);
            CHECK(ret, FAIL, "H5O_num_attrs_test");
            VERIFY(nattrs, (u + 1), "H5O_num_attrs_test");
            is_empty = H5O_is_attr_empty_test(my_dataset);
            VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
            is_dense = H5O_is_attr_dense_test(my_dataset);
            VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
        } /* end for */
    } /* end for */

    /* Close Datasets */
    ret = H5Dclose(dset1);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Dclose(dset2);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Dclose(dset3);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Open datasets created */
    dset1 = H5Dopen2(fid, DSET1_NAME, H5P_DEFAULT);
    CHECK(dset1, FAIL, "H5Dopen2");
    dset2 = H5Dopen2(fid, DSET2_NAME, H5P_DEFAULT);
    CHECK(dset2, FAIL, "H5Dopen2");
    dset3 = H5Dopen2(fid, DSET3_NAME, H5P_DEFAULT);
    CHECK(dset3, FAIL, "H5Dopen2");

    /* Work on all the datasets */
    for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
        switch(curr_dset) {
            case 0:
                my_dataset = dset1;
                break;

            case 1:
                my_dataset = dset2;
                break;

            case 2:
                my_dataset = dset3;
                break;

            default:
                HDassert(0 && "Too many datasets!");
        } /* end switch */

        /* Check on dataset's attribute storage status */
        ret = H5O_num_attrs_test(my_dataset, &nattrs);
        CHECK(ret, FAIL, "H5O_num_attrs_test");
        VERIFY(nattrs, max_compact, "H5O_num_attrs_test");
        is_empty = H5O_is_attr_empty_test(my_dataset);
        VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
        is_dense = H5O_is_attr_dense_test(my_dataset);
        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

        /* Loop through attributes, checking their creation order values */
        /* (the name index is used, but the creation order value is in the same order) */
        for(u = 0; u < max_compact; u++) {
            H5A_info_t ainfo;           /* Attribute information */

            /* Retrieve information for attribute */
            sprintf(attrname, "attr %02u", u);
            ret = H5Aget_info_by_name(my_dataset, ".", attrname, &ainfo, H5P_DEFAULT);
            CHECK(ret, FAIL, "H5Aget_info_by_name");

            /* Verify creation order of attribute */
            VERIFY(ainfo.corder_valid, TRUE, "H5Aget_info_by_name");
            VERIFY(ainfo.corder, u, "H5Aget_info_by_name");
        } /* end for */
    } /* end for */

    /* Close Datasets */
    ret = H5Dclose(dset1);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Dclose(dset2);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Dclose(dset3);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_corder_create_compact() */


/****************************************************************
**
**  test_attr_corder_create_dense(): Test basic H5A (attribute) code.
**      Tests dense attribute storage on objects with attribute creation order info
**
****************************************************************/
static void
test_attr_corder_create_dense(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dset1, dset2, dset3;	/* Dataset IDs			*/
    hid_t	my_dataset;	/* Current dataset ID		*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    unsigned    max_compact;    /* Maximum # of links to store in group compactly */
    unsigned    min_dense;      /* Minimum # of links to store in group "densely" */
    htri_t	is_empty;	/* Are there any attributes? */
    htri_t	is_dense;	/* Are attributes stored densely? */
    hsize_t     nattrs;         /* Number of attributes on object */
    hsize_t     name_count;     /* # of records in name index */
    hsize_t     corder_count;   /* # of records in creation order index */
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute */
    unsigned    curr_dset;      /* Current dataset to work on */
    unsigned    u;              /* Local index variable */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Dense Storage of Attributes with Creation Order Info\n"));

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create dataset creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Set attribute creation order tracking & indexing for object */
    ret = H5Pset_attr_creation_order(dcpl, (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED));
    CHECK(ret, FAIL, "H5Pset_attr_creation_order");

    /* Query the attribute creation properties */
    ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");

    /* Create dataspace for dataset & attributes */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create datasets */
    dset1 = H5Dcreate2(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(dset1, FAIL, "H5Dcreate2");
    dset2 = H5Dcreate2(fid, DSET2_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(dset2, FAIL, "H5Dcreate2");
    dset3 = H5Dcreate2(fid, DSET3_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(dset3, FAIL, "H5Dcreate2");

    /* Work on all the datasets */
    for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
        switch(curr_dset) {
            case 0:
                my_dataset = dset1;
                break;

            case 1:
                my_dataset = dset2;
                break;

            case 2:
                my_dataset = dset3;
                break;

            default:
                HDassert(0 && "Too many datasets!");
        } /* end switch */

        /* Check on dataset's attribute storage status */
        is_empty = H5O_is_attr_empty_test(my_dataset);
        VERIFY(is_empty, TRUE, "H5O_is_attr_empty_test");
        is_dense = H5O_is_attr_dense_test(my_dataset);
        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

        /* Create several attributes, but keep storage in compact form */
        for(u = 0; u < max_compact; u++) {
            /* Create attribute */
            sprintf(attrname, "attr %02u", u);
            attr = H5Acreate2(my_dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
            CHECK(attr, FAIL, "H5Acreate2");

            /* Write data into the attribute */
            ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
            CHECK(ret, FAIL, "H5Awrite");

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");

            /* Verify state of object */
            ret = H5O_num_attrs_test(my_dataset, &nattrs);
            CHECK(ret, FAIL, "H5O_num_attrs_test");
            VERIFY(nattrs, (u + 1), "H5O_num_attrs_test");
            is_empty = H5O_is_attr_empty_test(my_dataset);
            VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
            is_dense = H5O_is_attr_dense_test(my_dataset);
            VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
        } /* end for */

        /* Create another attribute, to push into dense storage */
        sprintf(attrname, "attr %02u", max_compact);
        attr = H5Acreate2(my_dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(attr, FAIL, "H5Acreate2");

        /* Write data into the attribute */
        ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
        CHECK(ret, FAIL, "H5Awrite");

        /* Close attribute */
        ret = H5Aclose(attr);
        CHECK(ret, FAIL, "H5Aclose");

        /* Verify state of object */
        ret = H5O_num_attrs_test(my_dataset, &nattrs);
        CHECK(ret, FAIL, "H5O_num_attrs_test");
        VERIFY(nattrs, (max_compact + 1), "H5O_num_attrs_test");
        is_empty = H5O_is_attr_empty_test(my_dataset);
        VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
        is_dense = H5O_is_attr_dense_test(my_dataset);
        VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

        /* Retrieve & verify # of records in the name & creation order indices */
        ret = H5O_attr_dense_info_test(my_dataset, &name_count, &corder_count);
        CHECK(ret, FAIL, "H5O_attr_dense_info_test");
        VERIFY(name_count, corder_count, "H5O_attr_dense_info_test");
    } /* end for */

    /* Close Datasets */
    ret = H5Dclose(dset1);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Dclose(dset2);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Dclose(dset3);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Open datasets created */
    dset1 = H5Dopen2(fid, DSET1_NAME, H5P_DEFAULT);
    CHECK(dset1, FAIL, "H5Dopen2");
    dset2 = H5Dopen2(fid, DSET2_NAME, H5P_DEFAULT);
    CHECK(dset2, FAIL, "H5Dopen2");
    dset3 = H5Dopen2(fid, DSET3_NAME, H5P_DEFAULT);
    CHECK(dset3, FAIL, "H5Dopen2");

    /* Work on all the datasets */
    for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
        switch(curr_dset) {
            case 0:
                my_dataset = dset1;
                break;

            case 1:
                my_dataset = dset2;
                break;

            case 2:
                my_dataset = dset3;
                break;

            default:
                HDassert(0 && "Too many datasets!");
        } /* end switch */

        /* Check on dataset's attribute storage status */
        ret = H5O_num_attrs_test(my_dataset, &nattrs);
        CHECK(ret, FAIL, "H5O_num_attrs_test");
        VERIFY(nattrs, (max_compact + 1), "H5O_num_attrs_test");
        is_empty = H5O_is_attr_empty_test(my_dataset);
        VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
        is_dense = H5O_is_attr_dense_test(my_dataset);
        VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

        /* Loop through attributes, checking their creation order values */
        /* (the name index is used, but the creation order value is in the same order) */
        for(u = 0; u < (max_compact + 1); u++) {
            H5A_info_t ainfo;           /* Attribute information */

            /* Retrieve information for attribute */
            sprintf(attrname, "attr %02u", u);
            ret = H5Aget_info_by_name(my_dataset, ".", attrname, &ainfo, H5P_DEFAULT);
            CHECK(ret, FAIL, "H5Aget_info_by_name");

            /* Verify creation order of attribute */
            VERIFY(ainfo.corder_valid, TRUE, "H5Aget_info_by_name");
            VERIFY(ainfo.corder, u, "H5Aget_info_by_name");
        } /* end for */
    } /* end for */

    /* Close Datasets */
    ret = H5Dclose(dset1);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Dclose(dset2);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Dclose(dset3);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_corder_create_dense() */


/****************************************************************
**
**  test_attr_corder_create_reopen(): Test basic H5A (attribute) code.
**      Test creating attributes w/reopening file from using new format
**      to using old format
**
****************************************************************/
static void
test_attr_corder_create_reopen(hid_t fcpl, hid_t fapl)
{
    hid_t fid = -1;             /* File ID */
    hid_t gcpl_id = -1;         /* Group creation property list ID */
    hid_t gid = -1;             /* Group ID */
    hid_t sid = -1;             /* Dataspace ID */
    hid_t aid = -1;             /* Attribute ID */
    int buf;                    /* Attribute data */
    herr_t	ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Creating Attributes w/New & Old Format\n"));

    /* Create dataspace for attributes */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create group */
    gcpl_id = H5Pcreate(H5P_GROUP_CREATE);
    CHECK(gcpl_id, FAIL, "H5Pcreate");
    ret = H5Pset_attr_creation_order(gcpl_id, H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED);
    CHECK(ret, FAIL, "H5Pset_attr_creation_order");
    gid = H5Gcreate2(fid, GROUP1_NAME, H5P_DEFAULT, gcpl_id, H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gcreate2");

    /* Create a couple of attributes */
    aid = H5Acreate2(gid, "attr-003", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(aid, FAIL, "H5Acreate2");
    buf = 3;
    ret = H5Awrite(aid, H5T_NATIVE_INT, &buf);
    CHECK(ret, FAIL, "H5Awrite");
    ret = H5Aclose(aid);
    CHECK(ret, FAIL, "H5Aclose");

    aid = H5Acreate2(gid, "attr-004", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(aid, FAIL, "H5Acreate2");
    buf = 4;
    ret = H5Awrite(aid, H5T_NATIVE_INT, &buf);
    CHECK(ret, FAIL, "H5Awrite");
    ret = H5Aclose(aid);
    CHECK(ret, FAIL, "H5Aclose");

    /***** Close group & GCPL *****/
    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");
    ret = H5Pclose(gcpl_id);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    /* Re-open file, without "use the latest format" flag */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /* Re-open group */
    gid = H5Gopen2(fid, GROUP1_NAME, H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gopen2");

    /* Delete attribute */
    ret = H5Adelete(gid, "attr-003");
    CHECK(aid, FAIL, "H5Adelete");

    /* Create some additional attributes */
    aid = H5Acreate2(gid, "attr-008", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(aid, FAIL, "H5Acreate2");
    buf = 8;
    ret = H5Awrite(aid, H5T_NATIVE_INT, &buf);
    CHECK(ret, FAIL, "H5Awrite");
    ret = H5Aclose(aid);
    CHECK(ret, FAIL, "H5Aclose");

    aid = H5Acreate2(gid, "attr-006", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(aid, FAIL, "H5Acreate2");
    buf = 6;
    ret = H5Awrite(aid, H5T_NATIVE_INT, &buf);
    CHECK(ret, FAIL, "H5Awrite");
    ret = H5Aclose(aid);
    CHECK(ret, FAIL, "H5Aclose");

    /***** Close group *****/
    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Close attribute dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
}   /* test_attr_corder_create_reopen() */


/****************************************************************
**
**  test_attr_corder_transition(): Test basic H5A (attribute) code.
**      Tests attribute storage transitions on objects with attribute creation order info
**
****************************************************************/
static void
test_attr_corder_transition(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dset1, dset2, dset3;	/* Dataset IDs			*/
    hid_t	my_dataset;	/* Current dataset ID		*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    unsigned    max_compact;    /* Maximum # of links to store in group compactly */
    unsigned    min_dense;      /* Minimum # of links to store in group "densely" */
    htri_t	is_empty;	/* Are there any attributes? */
    htri_t	is_dense;	/* Are attributes stored densely? */
    hsize_t     nattrs;         /* Number of attributes on object */
    hsize_t     name_count;     /* # of records in name index */
    hsize_t     corder_count;   /* # of records in creation order index */
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute */
    unsigned    curr_dset;      /* Current dataset to work on */
    unsigned    u;              /* Local index variable */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Storage Transitions of Attributes with Creation Order Info\n"));

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create dataset creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Set attribute creation order tracking & indexing for object */
    ret = H5Pset_attr_creation_order(dcpl, (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED));
    CHECK(ret, FAIL, "H5Pset_attr_creation_order");

    /* Query the attribute creation properties */
    ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");

    /* Create dataspace for dataset & attributes */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

/* XXX: Try to find a way to resize dataset's object header so that the object
 *      header can have one chunk, then retrieve "empty" file size and check
 *      that size after everything is deleted -QAK
 */
    /* Create datasets */
    dset1 = H5Dcreate2(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(dset1, FAIL, "H5Dcreate2");
    dset2 = H5Dcreate2(fid, DSET2_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(dset2, FAIL, "H5Dcreate2");
    dset3 = H5Dcreate2(fid, DSET3_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(dset3, FAIL, "H5Dcreate2");

    /* Work on all the datasets */
    for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
        switch(curr_dset) {
            case 0:
                my_dataset = dset1;
                break;

            case 1:
                my_dataset = dset2;
                break;

            case 2:
                my_dataset = dset3;
                break;

            default:
                HDassert(0 && "Too many datasets!");
        } /* end switch */

        /* Check on dataset's attribute storage status */
        is_empty = H5O_is_attr_empty_test(my_dataset);
        VERIFY(is_empty, TRUE, "H5O_is_attr_empty_test");
        is_dense = H5O_is_attr_dense_test(my_dataset);
        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
    } /* end for */

    /* Close Datasets */
    ret = H5Dclose(dset1);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Dclose(dset2);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Dclose(dset3);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Open datasets created */
    dset1 = H5Dopen2(fid, DSET1_NAME, H5P_DEFAULT);
    CHECK(dset1, FAIL, "H5Dopen2");
    dset2 = H5Dopen2(fid, DSET2_NAME, H5P_DEFAULT);
    CHECK(dset2, FAIL, "H5Dopen2");
    dset3 = H5Dopen2(fid, DSET3_NAME, H5P_DEFAULT);
    CHECK(dset3, FAIL, "H5Dopen2");

    /* Work on all the datasets */
    for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
        switch(curr_dset) {
            case 0:
                my_dataset = dset1;
                break;

            case 1:
                my_dataset = dset2;
                break;

            case 2:
                my_dataset = dset3;
                break;

            default:
                HDassert(0 && "Too many datasets!");
        } /* end switch */

        /* Create several attributes, but keep storage in compact form */
        for(u = 0; u < max_compact; u++) {
            /* Create attribute */
            sprintf(attrname, "attr %02u", u);
            attr = H5Acreate2(my_dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
            CHECK(attr, FAIL, "H5Acreate2");

            /* Write data into the attribute */
            ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
            CHECK(ret, FAIL, "H5Awrite");

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");

            /* Verify state of object */
            ret = H5O_num_attrs_test(my_dataset, &nattrs);
            CHECK(ret, FAIL, "H5O_num_attrs_test");
            VERIFY(nattrs, (u + 1), "H5O_num_attrs_test");
            is_empty = H5O_is_attr_empty_test(my_dataset);
            VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
            is_dense = H5O_is_attr_dense_test(my_dataset);
            VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
        } /* end for */

        /* Create another attribute, to push into dense storage */
        sprintf(attrname, "attr %02u", max_compact);
        attr = H5Acreate2(my_dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(attr, FAIL, "H5Acreate2");

        /* Write data into the attribute */
        ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
        CHECK(ret, FAIL, "H5Awrite");

        /* Close attribute */
        ret = H5Aclose(attr);
        CHECK(ret, FAIL, "H5Aclose");

        /* Verify state of object */
        ret = H5O_num_attrs_test(my_dataset, &nattrs);
        CHECK(ret, FAIL, "H5O_num_attrs_test");
        VERIFY(nattrs, (max_compact + 1), "H5O_num_attrs_test");
        is_empty = H5O_is_attr_empty_test(my_dataset);
        VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
        is_dense = H5O_is_attr_dense_test(my_dataset);
        VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

        /* Retrieve & verify # of records in the name & creation order indices */
        ret = H5O_attr_dense_info_test(my_dataset, &name_count, &corder_count);
        CHECK(ret, FAIL, "H5O_attr_dense_info_test");
        VERIFY(name_count, corder_count, "H5O_attr_dense_info_test");

        /* Delete several attributes from object, until attribute storage resumes compact form */
        for(u = max_compact; u >= min_dense; u--) {
            sprintf(attrname, "attr %02u", u);
            ret = H5Adelete(my_dataset, attrname);
            CHECK(ret, FAIL, "H5Adelete");

            /* Verify state of object */
            ret = H5O_num_attrs_test(my_dataset, &nattrs);
            CHECK(ret, FAIL, "H5O_num_attrs_test");
            VERIFY(nattrs, u, "H5O_num_attrs_test");
            is_empty = H5O_is_attr_empty_test(my_dataset);
            VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
            is_dense = H5O_is_attr_dense_test(my_dataset);
            VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

            /* Retrieve & verify # of records in the name & creation order indices */
            ret = H5O_attr_dense_info_test(my_dataset, &name_count, &corder_count);
            CHECK(ret, FAIL, "H5O_attr_dense_info_test");
            VERIFY(name_count, corder_count, "H5O_attr_dense_info_test");
        } /* end for */

        /* Delete another attribute, to push attribute storage into compact form */
        sprintf(attrname, "attr %02u", (min_dense - 1));
        ret = H5Adelete(my_dataset, attrname);
        CHECK(ret, FAIL, "H5Adelete");

        /* Verify state of object */
        ret = H5O_num_attrs_test(my_dataset, &nattrs);
        CHECK(ret, FAIL, "H5O_num_attrs_test");
        VERIFY(nattrs, (min_dense - 1), "H5O_num_attrs_test");
        is_empty = H5O_is_attr_empty_test(my_dataset);
        VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
        is_dense = H5O_is_attr_dense_test(my_dataset);
        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

        /* Re-add attributes to get back into dense form */
        for(u = (min_dense - 1); u < (max_compact + 1); u++) {
            /* Create attribute */
            sprintf(attrname, "attr %02u", u);
            attr = H5Acreate2(my_dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
            CHECK(attr, FAIL, "H5Acreate2");

            /* Write data into the attribute */
            ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
            CHECK(ret, FAIL, "H5Awrite");

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");
        } /* end for */

        /* Verify state of object */
        ret = H5O_num_attrs_test(my_dataset, &nattrs);
        CHECK(ret, FAIL, "H5O_num_attrs_test");
        VERIFY(nattrs, (max_compact + 1), "H5O_num_attrs_test");
        is_empty = H5O_is_attr_empty_test(my_dataset);
        VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
        is_dense = H5O_is_attr_dense_test(my_dataset);
        VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

        /* Retrieve & verify # of records in the name & creation order indices */
        ret = H5O_attr_dense_info_test(my_dataset, &name_count, &corder_count);
        CHECK(ret, FAIL, "H5O_attr_dense_info_test");
        VERIFY(name_count, corder_count, "H5O_attr_dense_info_test");
    } /* end for */

    /* Close Datasets */
    ret = H5Dclose(dset1);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Dclose(dset2);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Dclose(dset3);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Open datasets created */
    dset1 = H5Dopen2(fid, DSET1_NAME, H5P_DEFAULT);
    CHECK(dset1, FAIL, "H5Dopen2");
    dset2 = H5Dopen2(fid, DSET2_NAME, H5P_DEFAULT);
    CHECK(dset2, FAIL, "H5Dopen2");
    dset3 = H5Dopen2(fid, DSET3_NAME, H5P_DEFAULT);
    CHECK(dset3, FAIL, "H5Dopen2");

    /* Work on all the datasets */
    for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
        switch(curr_dset) {
            case 0:
                my_dataset = dset1;
                break;

            case 1:
                my_dataset = dset2;
                break;

            case 2:
                my_dataset = dset3;
                break;

            default:
                HDassert(0 && "Too many datasets!");
        } /* end switch */

        /* Check on dataset's attribute storage status */
        ret = H5O_num_attrs_test(my_dataset, &nattrs);
        CHECK(ret, FAIL, "H5O_num_attrs_test");
        VERIFY(nattrs, (max_compact + 1), "H5O_num_attrs_test");
        is_empty = H5O_is_attr_empty_test(my_dataset);
        VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
        is_dense = H5O_is_attr_dense_test(my_dataset);
        VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

        /* Retrieve & verify # of records in the name & creation order indices */
        ret = H5O_attr_dense_info_test(my_dataset, &name_count, &corder_count);
        CHECK(ret, FAIL, "H5O_attr_dense_info_test");
        VERIFY(name_count, corder_count, "H5O_attr_dense_info_test");

        /* Delete several attributes from object, until attribute storage resumes compact form */
        for(u = max_compact; u >= min_dense; u--) {
            sprintf(attrname, "attr %02u", u);
            ret = H5Adelete(my_dataset, attrname);
            CHECK(ret, FAIL, "H5Adelete");

            /* Verify state of object */
            ret = H5O_num_attrs_test(my_dataset, &nattrs);
            CHECK(ret, FAIL, "H5O_num_attrs_test");
            VERIFY(nattrs, u, "H5O_num_attrs_test");
            is_empty = H5O_is_attr_empty_test(my_dataset);
            VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
            is_dense = H5O_is_attr_dense_test(my_dataset);
            VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

            /* Retrieve & verify # of records in the name & creation order indices */
            ret = H5O_attr_dense_info_test(my_dataset, &name_count, &corder_count);
            CHECK(ret, FAIL, "H5O_attr_dense_info_test");
            VERIFY(name_count, corder_count, "H5O_attr_dense_info_test");
        } /* end for */

        /* Delete another attribute, to push attribute storage into compact form */
        sprintf(attrname, "attr %02u", (min_dense - 1));
        ret = H5Adelete(my_dataset, attrname);
        CHECK(ret, FAIL, "H5Adelete");

        /* Verify state of object */
        ret = H5O_num_attrs_test(my_dataset, &nattrs);
        CHECK(ret, FAIL, "H5O_num_attrs_test");
        VERIFY(nattrs, (min_dense - 1), "H5O_num_attrs_test");
        is_empty = H5O_is_attr_empty_test(my_dataset);
        VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
        is_dense = H5O_is_attr_dense_test(my_dataset);
        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

        /* Re-add attributes to get back into dense form */
        for(u = (min_dense - 1); u < (max_compact + 1); u++) {
            /* Create attribute */
            sprintf(attrname, "attr %02u", u);
            attr = H5Acreate2(my_dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
            CHECK(attr, FAIL, "H5Acreate2");

            /* Write data into the attribute */
            ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
            CHECK(ret, FAIL, "H5Awrite");

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");
        } /* end for */

        /* Verify state of object */
        ret = H5O_num_attrs_test(my_dataset, &nattrs);
        CHECK(ret, FAIL, "H5O_num_attrs_test");
        VERIFY(nattrs, (max_compact + 1), "H5O_num_attrs_test");
        is_empty = H5O_is_attr_empty_test(my_dataset);
        VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
        is_dense = H5O_is_attr_dense_test(my_dataset);
        VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

        /* Retrieve & verify # of records in the name & creation order indices */
        ret = H5O_attr_dense_info_test(my_dataset, &name_count, &corder_count);
        CHECK(ret, FAIL, "H5O_attr_dense_info_test");
        VERIFY(name_count, corder_count, "H5O_attr_dense_info_test");

        /* Delete all attributes */
        for(u = max_compact; u > 0; u--) {
            sprintf(attrname, "attr %02u", u);
            ret = H5Adelete(my_dataset, attrname);
            CHECK(ret, FAIL, "H5Adelete");
        } /* end for */
        sprintf(attrname, "attr %02u", 0);
        ret = H5Adelete(my_dataset, attrname);
        CHECK(ret, FAIL, "H5Adelete");
    } /* end for */

    /* Close Datasets */
    ret = H5Dclose(dset1);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Dclose(dset2);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Dclose(dset3);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
}   /* test_attr_corder_transition() */


/****************************************************************
**
**  test_attr_corder_delete(): Test basic H5A (attribute) code.
**      Tests deleting object w/dense attribute storage on objects with attribute creation order info
**
****************************************************************/
static void
test_attr_corder_delete(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dset1, dset2, dset3;	/* Dataset IDs			*/
    hid_t	my_dataset;	/* Current dataset ID		*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    unsigned    max_compact;    /* Maximum # of links to store in group compactly */
    unsigned    min_dense;      /* Minimum # of links to store in group "densely" */
    htri_t	is_empty;	/* Are there any attributes? */
    htri_t	is_dense;	/* Are attributes stored densely? */
    hsize_t     nattrs;         /* Number of attributes on object */
    hsize_t     name_count;     /* # of records in name index */
    hsize_t     corder_count;   /* # of records in creation order index */
    hbool_t     reopen_file;            /* Whether to re-open the file before deleting group */
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute */
#ifdef LATER
    h5_stat_size_t empty_size;  /* Size of empty file */
    h5_stat_size_t file_size;   /* Size of file after operating on it */
#endif /* LATER */
    unsigned    curr_dset;      /* Current dataset to work on */
    unsigned    u;              /* Local index variable */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Deleting Object w/Dense Attribute Storage and Creation Order Info\n"));

    /* Create dataspace for dataset & attributes */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create dataset creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Set attribute creation order tracking & indexing for object */
    ret = H5Pset_attr_creation_order(dcpl, (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED));
    CHECK(ret, FAIL, "H5Pset_attr_creation_order");

    /* Query the attribute creation properties */
    ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");


/* XXX: Try to find a way to resize dataset's object header so that the object
 *      header can have one chunk, then retrieve "empty" file size and check
 *      that size after everything is deleted -QAK
 */
#ifdef LATER
    /* Create empty file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Get the size of an empty file */
    empty_size = h5_get_file_size(FILENAME);
    CHECK(empty_size, FAIL, "h5_get_file_size");
#endif /* LATER */


    /* Loop to leave file open when deleting dataset, or to close & re-open file
     *  before deleting dataset */
    for(reopen_file = FALSE; reopen_file <= TRUE; reopen_file++) {
        /* Create test file */
        fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
        CHECK(fid, FAIL, "H5Fopen");

        /* Create datasets */
        dset1 = H5Dcreate2(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        CHECK(dset1, FAIL, "H5Dcreate2");
        dset2 = H5Dcreate2(fid, DSET2_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        CHECK(dset2, FAIL, "H5Dcreate2");
        dset3 = H5Dcreate2(fid, DSET3_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        CHECK(dset3, FAIL, "H5Dcreate2");

        /* Work on all the datasets */
        for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
            switch(curr_dset) {
                case 0:
                    my_dataset = dset1;
                    break;

                case 1:
                    my_dataset = dset2;
                    break;

                case 2:
                    my_dataset = dset3;
                    break;

                default:
                    HDassert(0 && "Too many datasets!");
            } /* end switch */

            /* Check on dataset's attribute storage status */
            is_empty = H5O_is_attr_empty_test(my_dataset);
            VERIFY(is_empty, TRUE, "H5O_is_attr_empty_test");
            is_dense = H5O_is_attr_dense_test(my_dataset);
            VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

            /* Create attributes, until attribute storage is in dense form */
            for(u = 0; u < max_compact * 2; u++) {
                /* Create attribute */
                sprintf(attrname, "attr %02u", u);
                attr = H5Acreate2(my_dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate2");

                /* Write data into the attribute */
                ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
                CHECK(ret, FAIL, "H5Awrite");

                /* Close attribute */
                ret = H5Aclose(attr);
                CHECK(ret, FAIL, "H5Aclose");
            } /* end for */

            /* Verify state of object */
            ret = H5O_num_attrs_test(my_dataset, &nattrs);
            CHECK(ret, FAIL, "H5O_num_attrs_test");
            VERIFY(nattrs, (max_compact * 2), "H5O_num_attrs_test");
            is_empty = H5O_is_attr_empty_test(my_dataset);
            VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
            is_dense = H5O_is_attr_dense_test(my_dataset);
            VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

            /* Retrieve & verify # of records in the name & creation order indices */
            ret = H5O_attr_dense_info_test(my_dataset, &name_count, &corder_count);
            CHECK(ret, FAIL, "H5O_attr_dense_info_test");
            VERIFY(name_count, corder_count, "H5O_attr_dense_info_test");
        } /* end for */

        /* Close Datasets */
        ret = H5Dclose(dset1);
        CHECK(ret, FAIL, "H5Dclose");
        ret = H5Dclose(dset2);
        CHECK(ret, FAIL, "H5Dclose");
        ret = H5Dclose(dset3);
        CHECK(ret, FAIL, "H5Dclose");

        /* Check for deleting datasets without re-opening file */
        if(!reopen_file) {
            ret = H5Ldelete(fid, DSET1_NAME, H5P_DEFAULT);
            CHECK(ret, FAIL, "H5Ldelete");
            ret = H5Ldelete(fid, DSET2_NAME, H5P_DEFAULT);
            CHECK(ret, FAIL, "H5Ldelete");
            ret = H5Ldelete(fid, DSET3_NAME, H5P_DEFAULT);
            CHECK(ret, FAIL, "H5Ldelete");
        } /* end if */

        /* Close file */
        ret = H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");

        /* Check for deleting dataset after re-opening file */
        if(reopen_file) {
            /* Re-open file */
            fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
            CHECK(fid, FAIL, "H5Fopen");

            /* Delete the datasets */
            ret = H5Ldelete(fid, DSET1_NAME, H5P_DEFAULT);
            CHECK(ret, FAIL, "H5Ldelete");
            ret = H5Ldelete(fid, DSET2_NAME, H5P_DEFAULT);
            CHECK(ret, FAIL, "H5Ldelete");
            ret = H5Ldelete(fid, DSET3_NAME, H5P_DEFAULT);
            CHECK(ret, FAIL, "H5Ldelete");

            /* Close file */
            ret = H5Fclose(fid);
            CHECK(ret, FAIL, "H5Fclose");
        } /* end if */

#ifdef LATER
        /* Get the size of the file now */
        file_size = h5_get_file_size(FILENAME);
        CHECK(file_size, FAIL, "h5_get_file_size");
        VERIFY(file_size, empty_size, "h5_get_file_size");
#endif /* LATER */
    } /* end for */

    /* Close property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
}   /* test_attr_corder_delete() */


/*-------------------------------------------------------------------------
 * Function:    attr_info_by_idx_check
 *
 * Purpose:     Support routine for attr_info_by_idx, to verify the attribute
 *              info is correct for a attribute
 *
 * Note:	This routine assumes that the attributes have been added to the
 *              object in alphabetical order.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, Februrary 13, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
attr_info_by_idx_check(hid_t obj_id, const char *attrname, hsize_t n,
    hbool_t use_index)
{
    char tmpname[NAME_BUF_SIZE];        /* Temporary attribute name */
    H5A_info_t  ainfo;                  /* Attribute info struct */
    int         old_nerrs;              /* Number of errors when entering this check */
    herr_t	ret;		        /* Generic return value */

    /* Retrieve the current # of reported errors */
    old_nerrs = GetTestNumErrs();

    /* Verify the information for first attribute, in increasing creation order */
    HDmemset(&ainfo, 0, sizeof(ainfo));
    ret = H5Aget_info_by_idx(obj_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)0, &ainfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Aget_info_by_idx");
    VERIFY(ainfo.corder, 0, "H5Aget_info_by_idx");

    /* Verify the information for new attribute, in increasing creation order */
    HDmemset(&ainfo, 0, sizeof(ainfo));
    ret = H5Aget_info_by_idx(obj_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, n, &ainfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Aget_info_by_idx");
    VERIFY(ainfo.corder, n, "H5Aget_info_by_idx");

    /* Verify the name for new link, in increasing creation order */
    HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
    ret = H5Aget_name_by_idx(obj_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, n, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Aget_name_by_idx");
    if(HDstrcmp(attrname, tmpname))
        TestErrPrintf("Line %d: attribute name size wrong!\n", __LINE__);


    /* Don't test "native" order if there is no creation order index, since
     *  there's not a good way to easily predict the attribute's order in the name
     *  index.
     */
    if(use_index) {
        /* Verify the information for first attribute, in native creation order */
        HDmemset(&ainfo, 0, sizeof(ainfo));
        ret = H5Aget_info_by_idx(obj_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_NATIVE, (hsize_t)0, &ainfo, H5P_DEFAULT);
        CHECK(ret, FAIL, "H5Aget_info_by_idx");
        VERIFY(ainfo.corder, 0, "H5Aget_info_by_idx");

        /* Verify the information for new attribute, in native creation order */
        HDmemset(&ainfo, 0, sizeof(ainfo));
        ret = H5Aget_info_by_idx(obj_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_NATIVE, n, &ainfo, H5P_DEFAULT);
        CHECK(ret, FAIL, "H5Aget_info_by_idx");
        VERIFY(ainfo.corder, n, "H5Aget_info_by_idx");

        /* Verify the name for new link, in increasing native order */
        HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
        ret = H5Aget_name_by_idx(obj_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_NATIVE, n, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT);
        CHECK(ret, FAIL, "H5Aget_name_by_idx");
        if(HDstrcmp(attrname, tmpname))
            TestErrPrintf("Line %d: attribute name size wrong!\n", __LINE__);
    } /* end if */


    /* Verify the information for first attribute, in decreasing creation order */
    HDmemset(&ainfo, 0, sizeof(ainfo));
    ret = H5Aget_info_by_idx(obj_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_DEC, n, &ainfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Aget_info_by_idx");
    VERIFY(ainfo.corder, 0, "H5Aget_info_by_idx");

    /* Verify the information for new attribute, in increasing creation order */
    HDmemset(&ainfo, 0, sizeof(ainfo));
    ret = H5Aget_info_by_idx(obj_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_DEC, (hsize_t)0, &ainfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Aget_info_by_idx");
    VERIFY(ainfo.corder, n, "H5Aget_info_by_idx");

    /* Verify the name for new link, in increasing creation order */
    HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
    ret = H5Aget_name_by_idx(obj_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_DEC, (hsize_t)0, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Aget_name_by_idx");
    if(HDstrcmp(attrname, tmpname))
        TestErrPrintf("Line %d: attribute name size wrong!\n", __LINE__);


    /* Verify the information for first attribute, in increasing name order */
    HDmemset(&ainfo, 0, sizeof(ainfo));
    ret = H5Aget_info_by_idx(obj_id, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)0, &ainfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Aget_info_by_idx");
    VERIFY(ainfo.corder, 0, "H5Aget_info_by_idx");

    /* Verify the information for new attribute, in increasing name order */
    HDmemset(&ainfo, 0, sizeof(ainfo));
    ret = H5Aget_info_by_idx(obj_id, ".", H5_INDEX_NAME, H5_ITER_INC, n, &ainfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Aget_info_by_idx");
    VERIFY(ainfo.corder, n, "H5Aget_info_by_idx");

    /* Verify the name for new link, in increasing name order */
    HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
    ret = H5Aget_name_by_idx(obj_id, ".", H5_INDEX_NAME, H5_ITER_INC, n, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Aget_name_by_idx");
    if(HDstrcmp(attrname, tmpname))
        TestErrPrintf("Line %d: attribute name size wrong!\n", __LINE__);


    /* Don't test "native" order queries on link name order, since there's not
     *  a good way to easily predict the order of the links in the name index.
     */


    /* Verify the information for first attribute, in decreasing name order */
    HDmemset(&ainfo, 0, sizeof(ainfo));
    ret = H5Aget_info_by_idx(obj_id, ".", H5_INDEX_NAME, H5_ITER_DEC, n, &ainfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Aget_info_by_idx");
    VERIFY(ainfo.corder, 0, "H5Aget_info_by_idx");

    /* Verify the information for new attribute, in increasing name order */
    HDmemset(&ainfo, 0, sizeof(ainfo));
    ret = H5Aget_info_by_idx(obj_id, ".", H5_INDEX_NAME, H5_ITER_DEC, (hsize_t)0, &ainfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Aget_info_by_idx");
    VERIFY(ainfo.corder, n, "H5Aget_info_by_idx");

    /* Verify the name for new link, in increasing name order */
    HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
    ret = H5Aget_name_by_idx(obj_id, ".", H5_INDEX_NAME, H5_ITER_DEC, (hsize_t)0, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Aget_name_by_idx");
    if(HDstrcmp(attrname, tmpname))
        TestErrPrintf("Line %d: attribute name size wrong!\n", __LINE__);

    /* Retrieve current # of errors */
    if(old_nerrs == GetTestNumErrs())
        return(0);
    else
        return(-1);
} /* end attr_info_by_idx_check() */


/****************************************************************
**
**  test_attr_info_by_idx(): Test basic H5A (attribute) code.
**      Tests querying attribute info by index
**
****************************************************************/
static void
test_attr_info_by_idx(hbool_t new_format, hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dset1, dset2, dset3;	/* Dataset IDs			*/
    hid_t	my_dataset;	/* Current dataset ID		*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    H5A_info_t  ainfo;          /* Attribute information */
    unsigned    max_compact;    /* Maximum # of links to store in group compactly */
    unsigned    min_dense;      /* Minimum # of links to store in group "densely" */
    htri_t	is_empty;	/* Are there any attributes? */
    htri_t	is_dense;	/* Are attributes stored densely? */
    hsize_t     nattrs;         /* Number of attributes on object */
    hsize_t     name_count;     /* # of records in name index */
    hsize_t     corder_count;   /* # of records in creation order index */
    hbool_t     use_index;      /* Use index on creation order values */
    char	attrname[NAME_BUF_SIZE];    /* Name of attribute */
    char        tmpname[NAME_BUF_SIZE];     /* Temporary attribute name */
    unsigned    curr_dset;      /* Current dataset to work on */
    unsigned    u;              /* Local index variable */
    herr_t	ret;		/* Generic return value		*/

    /* Create dataspace for dataset & attributes */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create dataset creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Query the attribute creation properties */
    ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");

    /* Loop over using index for creation order value */
    for(use_index = FALSE; use_index <= TRUE; use_index++) {
        /* Output message about test being performed */
        if(use_index)
            MESSAGE(5, ("Testing Querying Attribute Info By Index w/Creation Order Index\n"))
        else
            MESSAGE(5, ("Testing Querying Attribute Info By Index w/o Creation Order Index\n"))

        /* Create file */
        fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
        CHECK(fid, FAIL, "H5Fcreate");

        /* Set attribute creation order tracking & indexing for object */
        if(new_format == TRUE) {
            ret = H5Pset_attr_creation_order(dcpl, (H5P_CRT_ORDER_TRACKED | (use_index ? H5P_CRT_ORDER_INDEXED : (unsigned)0)));
            CHECK(ret, FAIL, "H5Pset_attr_creation_order");
        } /* end if */

        /* Create datasets */
        dset1 = H5Dcreate2(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        CHECK(dset1, FAIL, "H5Dcreate2");
        dset2 = H5Dcreate2(fid, DSET2_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        CHECK(dset2, FAIL, "H5Dcreate2");
        dset3 = H5Dcreate2(fid, DSET3_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        CHECK(dset3, FAIL, "H5Dcreate2");

        /* Work on all the datasets */
        for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
            switch(curr_dset) {
                case 0:
                    my_dataset = dset1;
                    break;

                case 1:
                    my_dataset = dset2;
                    break;

                case 2:
                    my_dataset = dset3;
                    break;

                default:
                    HDassert(0 && "Too many datasets!");
            } /* end switch */

            /* Check on dataset's attribute storage status */
            is_empty = H5O_is_attr_empty_test(my_dataset);
            VERIFY(is_empty, TRUE, "H5O_is_attr_empty_test");
            is_dense = H5O_is_attr_dense_test(my_dataset);
            VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

            /* Check for query on non-existant attribute */
            ret = H5Aget_info_by_idx(my_dataset, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)0, &ainfo, H5P_DEFAULT);
            VERIFY(ret, FAIL, "H5Aget_info_by_idx");
            ret = H5Aget_name_by_idx(my_dataset, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)0, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT);
            VERIFY(ret, FAIL, "H5Aget_name_by_idx");

            /* Create attributes, up to limit of compact form */
            for(u = 0; u < max_compact; u++) {
                /* Create attribute */
                sprintf(attrname, "attr %02u", u);
                attr = H5Acreate2(my_dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate2");

                /* Write data into the attribute */
                ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
                CHECK(ret, FAIL, "H5Awrite");

                /* Close attribute */
                ret = H5Aclose(attr);
                CHECK(ret, FAIL, "H5Aclose");

                /* Verify information for new attribute */
                ret = attr_info_by_idx_check(my_dataset, attrname, (hsize_t)u, use_index);
                CHECK(ret, FAIL, "attr_info_by_idx_check");
            } /* end for */

            /* Verify state of object */
            ret = H5O_num_attrs_test(my_dataset, &nattrs);
            CHECK(ret, FAIL, "H5O_num_attrs_test");
            VERIFY(nattrs, max_compact, "H5O_num_attrs_test");
            is_empty = H5O_is_attr_empty_test(my_dataset);
            VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
            is_dense = H5O_is_attr_dense_test(my_dataset);
            VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

            /* Check for out of bound offset queries */
            ret = H5Aget_info_by_idx(my_dataset, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)u, &ainfo, H5P_DEFAULT);
            VERIFY(ret, FAIL, "H5Aget_info_by_idx");
            ret = H5Aget_info_by_idx(my_dataset, ".", H5_INDEX_CRT_ORDER, H5_ITER_DEC, (hsize_t)u, &ainfo, H5P_DEFAULT);
            VERIFY(ret, FAIL, "H5Aget_info_by_idx");
            ret = H5Aget_name_by_idx(my_dataset, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)u, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT);
            VERIFY(ret, FAIL, "H5Aget_name_by_idx");

            /* Create more attributes, to push into dense form */
            for(; u < (max_compact * 2); u++) {
                /* Create attribute */
                sprintf(attrname, "attr %02u", u);
                attr = H5Acreate2(my_dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate2");

                /* Write data into the attribute */
                ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
                CHECK(ret, FAIL, "H5Awrite");

                /* Close attribute */
                ret = H5Aclose(attr);
                CHECK(ret, FAIL, "H5Aclose");

                /* Verify state of object */
                is_dense = H5O_is_attr_dense_test(my_dataset);
                VERIFY(is_dense, (new_format ? TRUE : FALSE), "H5O_is_attr_dense_test");

                /* Verify information for new attribute */
                ret = attr_info_by_idx_check(my_dataset, attrname, (hsize_t)u, use_index);
                CHECK(ret, FAIL, "attr_info_by_idx_check");
            } /* end for */

            /* Verify state of object */
            ret = H5O_num_attrs_test(my_dataset, &nattrs);
            CHECK(ret, FAIL, "H5O_num_attrs_test");
            VERIFY(nattrs, (max_compact * 2), "H5O_num_attrs_test");
            is_empty = H5O_is_attr_empty_test(my_dataset);
            VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
            is_dense = H5O_is_attr_dense_test(my_dataset);
            VERIFY(is_dense, (new_format ? TRUE : FALSE), "H5O_is_attr_dense_test");

            if(new_format) {
                /* Retrieve & verify # of records in the name & creation order indices */
                ret = H5O_attr_dense_info_test(my_dataset, &name_count, &corder_count);
                CHECK(ret, FAIL, "H5O_attr_dense_info_test");
                if(use_index)
                    VERIFY(name_count, corder_count, "H5O_attr_dense_info_test");
                VERIFY(name_count, (max_compact * 2), "H5O_attr_dense_info_test");
            } /* end if */

            /* Check for out of bound offset queries */
            ret = H5Aget_info_by_idx(my_dataset, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)u, &ainfo, H5P_DEFAULT);
            VERIFY(ret, FAIL, "H5Aget_info_by_idx");
            ret = H5Aget_info_by_idx(my_dataset, ".", H5_INDEX_CRT_ORDER, H5_ITER_DEC, (hsize_t)u, &ainfo, H5P_DEFAULT);
            VERIFY(ret, FAIL, "H5Aget_info_by_idx");
            ret = H5Aget_name_by_idx(my_dataset, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)u, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT);
            VERIFY(ret, FAIL, "H5Aget_name_by_idx");
        } /* end for */

        /* Close Datasets */
        ret = H5Dclose(dset1);
        CHECK(ret, FAIL, "H5Dclose");
        ret = H5Dclose(dset2);
        CHECK(ret, FAIL, "H5Dclose");
        ret = H5Dclose(dset3);
        CHECK(ret, FAIL, "H5Dclose");

        /* Close file */
        ret = H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");
    } /* end for */

    /* Close property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
}   /* test_attr_info_by_idx() */


/****************************************************************
**
**  test_attr_delete_by_idx(): Test basic H5A (attribute) code.
**      Tests deleting attribute by index
**
****************************************************************/
static void
test_attr_delete_by_idx(hbool_t new_format, hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dset1, dset2, dset3;	/* Dataset IDs			*/
    hid_t	my_dataset;	/* Current dataset ID		*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    H5A_info_t  ainfo;          /* Attribute information */
    unsigned    max_compact;    /* Maximum # of links to store in group compactly */
    unsigned    min_dense;      /* Minimum # of links to store in group "densely" */
    htri_t	is_empty;	/* Are there any attributes? */
    htri_t	is_dense;	/* Are attributes stored densely? */
    hsize_t     nattrs;         /* Number of attributes on object */
    hsize_t     name_count;     /* # of records in name index */
    hsize_t     corder_count;   /* # of records in creation order index */
    H5_index_t idx_type;        /* Type of index to operate on */
    H5_iter_order_t order;      /* Order within in the index */
    hbool_t     use_index;      /* Use index on creation order values */
    char	attrname[NAME_BUF_SIZE];    /* Name of attribute */
    char        tmpname[NAME_BUF_SIZE];     /* Temporary attribute name */
    unsigned    curr_dset;      /* Current dataset to work on */
    unsigned    u;              /* Local index variable */
    herr_t	ret;		/* Generic return value		*/

    /* Create dataspace for dataset & attributes */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create dataset creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Query the attribute creation properties */
    ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");

    /* Loop over operating on different indices on link fields */
    for(idx_type = H5_INDEX_NAME; idx_type <=H5_INDEX_CRT_ORDER; idx_type++) {
        /* Loop over operating in different orders */
        for(order = H5_ITER_INC; order <=H5_ITER_DEC; order++) {
            /* Loop over using index for creation order value */
            for(use_index = FALSE; use_index <= TRUE; use_index++) {
                /* Print appropriate test message */
                if(idx_type == H5_INDEX_CRT_ORDER) {
                    if(order == H5_ITER_INC) {
                        if(use_index)
                            MESSAGE(5, ("Testing Deleting Attribute By Creation Order Index in Increasing Order w/Creation Order Index\n"))
                        else
                            MESSAGE(5, ("Testing Deleting Attribute By Creation Order Index in Increasing Order w/o Creation Order Index\n"))
                    } /* end if */
                    else {
                        if(use_index)
                            MESSAGE(5, ("Testing Deleting Attribute By Creation Order Index in Decreasing Order w/Creation Order Index\n"))
                        else
                            MESSAGE(5, ("Testing Deleting Attribute By Creation Order Index in Decreasing Order w/o Creation Order Index\n"))
                    } /* end else */
                } /* end if */
                else {
                    if(order == H5_ITER_INC) {
                        if(use_index)
                            MESSAGE(5, ("Testing Deleting Attribute By Name Index in Increasing Order w/Creation Order Index\n"))
                        else
                            MESSAGE(5, ("Testing Deleting Attribute By Name Index in Increasing Order w/o Creation Order Index\n"))
                    } /* end if */
                    else {
                        if(use_index)
                            MESSAGE(5, ("Testing Deleting Attribute By Name Index in Decreasing Order w/Creation Order Index\n"))
                        else
                            MESSAGE(5, ("Testing Deleting Attribute By Name Index in Decreasing Order w/o Creation Order Index\n"))
                    } /* end else */
                } /* end else */

                /* Create file */
                fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
                CHECK(fid, FAIL, "H5Fcreate");

                /* Set attribute creation order tracking & indexing for object */
                if(new_format == TRUE) {
                    ret = H5Pset_attr_creation_order(dcpl, (H5P_CRT_ORDER_TRACKED | (use_index ? H5P_CRT_ORDER_INDEXED : (unsigned)0)));
                    CHECK(ret, FAIL, "H5Pset_attr_creation_order");
                } /* end if */

                /* Create datasets */
                dset1 = H5Dcreate2(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
                CHECK(dset1, FAIL, "H5Dcreate2");
                dset2 = H5Dcreate2(fid, DSET2_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
                CHECK(dset2, FAIL, "H5Dcreate2");
                dset3 = H5Dcreate2(fid, DSET3_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
                CHECK(dset3, FAIL, "H5Dcreate2");

                /* Work on all the datasets */
                for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
                    switch(curr_dset) {
                        case 0:
                            my_dataset = dset1;
                            break;

                        case 1:
                            my_dataset = dset2;
                            break;

                        case 2:
                            my_dataset = dset3;
                            break;

                        default:
                            HDassert(0 && "Too many datasets!");
                    } /* end switch */

                    /* Check on dataset's attribute storage status */
                    is_empty = H5O_is_attr_empty_test(my_dataset);
                    VERIFY(is_empty, TRUE, "H5O_is_attr_empty_test");
                    is_dense = H5O_is_attr_dense_test(my_dataset);
                    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

                    /* Check for deleting non-existant attribute */
                    ret = H5Adelete_by_idx(my_dataset, ".", idx_type, order, (hsize_t)0, H5P_DEFAULT);
                    VERIFY(ret, FAIL, "H5Adelete_by_idx");

                    /* Create attributes, up to limit of compact form */
                    for(u = 0; u < max_compact; u++) {
                        /* Create attribute */
                        sprintf(attrname, "attr %02u", u);
                        attr = H5Acreate2(my_dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
                        CHECK(attr, FAIL, "H5Acreate2");

                        /* Write data into the attribute */
                        ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
                        CHECK(ret, FAIL, "H5Awrite");

                        /* Close attribute */
                        ret = H5Aclose(attr);
                        CHECK(ret, FAIL, "H5Aclose");

                        /* Verify information for new attribute */
                        ret = attr_info_by_idx_check(my_dataset, attrname, (hsize_t)u, use_index);
                        CHECK(ret, FAIL, "attr_info_by_idx_check");
                    } /* end for */

                    /* Verify state of object */
                    ret = H5O_num_attrs_test(my_dataset, &nattrs);
                    CHECK(ret, FAIL, "H5O_num_attrs_test");
                    VERIFY(nattrs, max_compact, "H5O_num_attrs_test");
                    is_empty = H5O_is_attr_empty_test(my_dataset);
                    VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
                    is_dense = H5O_is_attr_dense_test(my_dataset);
                    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

                    /* Check for out of bound deletions */
                    ret = H5Adelete_by_idx(my_dataset, ".", idx_type, order, (hsize_t)u, H5P_DEFAULT);
                    VERIFY(ret, FAIL, "H5Adelete_by_idx");
                } /* end for */

                /* Work on all the datasets */
                for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
                    switch(curr_dset) {
                        case 0:
                            my_dataset = dset1;
                            break;

                        case 1:
                            my_dataset = dset2;
                            break;

                        case 2:
                            my_dataset = dset3;
                            break;

                        default:
                            HDassert(0 && "Too many datasets!");
                    } /* end switch */

                    /* Delete attributes from compact storage */
                    for(u = 0; u < (max_compact - 1); u++) {
                        /* Delete first attribute in appropriate order */
                        ret = H5Adelete_by_idx(my_dataset, ".", idx_type, order, (hsize_t)0, H5P_DEFAULT);
                        CHECK(ret, FAIL, "H5Adelete_by_idx");

                        /* Verify the attribute information for first attribute in appropriate order */
                        HDmemset(&ainfo, 0, sizeof(ainfo));
                        ret = H5Aget_info_by_idx(my_dataset, ".", idx_type, order, (hsize_t)0, &ainfo, H5P_DEFAULT);
                        if(new_format) {
                            if(order == H5_ITER_INC) {
                                VERIFY(ainfo.corder, (u + 1), "H5Aget_info_by_idx");
                            } /* end if */
                            else {
                                VERIFY(ainfo.corder, (max_compact - (u + 2)), "H5Aget_info_by_idx");
                            } /* end else */
                        } /* end if */

                        /* Verify the name for first attribute in appropriate order */
                        HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
                        ret = H5Aget_name_by_idx(my_dataset, ".", idx_type, order, (hsize_t)0, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT);
                        if(order == H5_ITER_INC)
                            sprintf(attrname, "attr %02u", (u + 1));
                        else
                            sprintf(attrname, "attr %02u", (max_compact - (u + 2)));
                        ret = HDstrcmp(attrname, tmpname);
                        VERIFY(ret, 0, "H5Aget_name_by_idx");
                    } /* end for */

                    /* Delete last attribute */
                    ret = H5Adelete_by_idx(my_dataset, ".", idx_type, order, (hsize_t)0, H5P_DEFAULT);
                    CHECK(ret, FAIL, "H5Adelete_by_idx");

                    /* Verify state of attribute storage (empty) */
                    is_empty = H5O_is_attr_empty_test(my_dataset);
                    VERIFY(is_empty, TRUE, "H5O_is_attr_empty_test");
                } /* end for */

                /* Work on all the datasets */
                for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
                    switch(curr_dset) {
                        case 0:
                            my_dataset = dset1;
                            break;

                        case 1:
                            my_dataset = dset2;
                            break;

                        case 2:
                            my_dataset = dset3;
                            break;

                        default:
                            HDassert(0 && "Too many datasets!");
                    } /* end switch */

                    /* Create more attributes, to push into dense form */
                    for(u = 0; u < (max_compact * 2); u++) {
                        /* Create attribute */
                        sprintf(attrname, "attr %02u", u);
                        attr = H5Acreate2(my_dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
                        CHECK(attr, FAIL, "H5Acreate2");

                        /* Write data into the attribute */
                        ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
                        CHECK(ret, FAIL, "H5Awrite");

                        /* Close attribute */
                        ret = H5Aclose(attr);
                        CHECK(ret, FAIL, "H5Aclose");

                        /* Verify state of object */
                        if(u >= max_compact) {
                            is_dense = H5O_is_attr_dense_test(my_dataset);
                            VERIFY(is_dense, (new_format ? TRUE : FALSE), "H5O_is_attr_dense_test");
                        } /* end if */

                        /* Verify information for new attribute */
                        ret = attr_info_by_idx_check(my_dataset, attrname, (hsize_t)u, use_index);
                        CHECK(ret, FAIL, "attr_info_by_idx_check");
                    } /* end for */

                    /* Verify state of object */
                    ret = H5O_num_attrs_test(my_dataset, &nattrs);
                    CHECK(ret, FAIL, "H5O_num_attrs_test");
                    VERIFY(nattrs, (max_compact * 2), "H5O_num_attrs_test");
                    is_empty = H5O_is_attr_empty_test(my_dataset);
                    VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
                    is_dense = H5O_is_attr_dense_test(my_dataset);
                    VERIFY(is_dense, (new_format ? TRUE : FALSE), "H5O_is_attr_dense_test");

                    if(new_format) {
                        /* Retrieve & verify # of records in the name & creation order indices */
                        ret = H5O_attr_dense_info_test(my_dataset, &name_count, &corder_count);
                        CHECK(ret, FAIL, "H5O_attr_dense_info_test");
                        if(use_index)
                            VERIFY(name_count, corder_count, "H5O_attr_dense_info_test");
                        VERIFY(name_count, (max_compact * 2), "H5O_attr_dense_info_test");
                    } /* end if */

                    /* Check for out of bound deletion */
                    ret = H5Adelete_by_idx(my_dataset, ".", idx_type, order, (hsize_t)u, H5P_DEFAULT);
                    VERIFY(ret, FAIL, "H5Adelete_by_idx");
                } /* end for */

                /* Work on all the datasets */
                for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
                    switch(curr_dset) {
                        case 0:
                            my_dataset = dset1;
                            break;

                        case 1:
                            my_dataset = dset2;
                            break;

                        case 2:
                            my_dataset = dset3;
                            break;

                        default:
                            HDassert(0 && "Too many datasets!");
                    } /* end switch */

                    /* Delete attributes from dense storage */
                    for(u = 0; u < ((max_compact * 2) - 1); u++) {
                        /* Delete first attribute in appropriate order */
                        ret = H5Adelete_by_idx(my_dataset, ".", idx_type, order, (hsize_t)0, H5P_DEFAULT);
                        CHECK(ret, FAIL, "H5Adelete_by_idx");

                        /* Verify the attribute information for first attribute in appropriate order */
                        HDmemset(&ainfo, 0, sizeof(ainfo));
                        ret = H5Aget_info_by_idx(my_dataset, ".", idx_type, order, (hsize_t)0, &ainfo, H5P_DEFAULT);
                        if(new_format) {
                            if(order == H5_ITER_INC) {
                                VERIFY(ainfo.corder, (u + 1), "H5Aget_info_by_idx");
                            } /* end if */
                            else {
                                VERIFY(ainfo.corder, ((max_compact * 2) - (u + 2)), "H5Aget_info_by_idx");
                            } /* end else */
                        } /* end if */

                        /* Verify the name for first attribute in appropriate order */
                        HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
                        ret = H5Aget_name_by_idx(my_dataset, ".", idx_type, order, (hsize_t)0, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT);
                        if(order == H5_ITER_INC)
                            sprintf(attrname, "attr %02u", (u + 1));
                        else
                            sprintf(attrname, "attr %02u", ((max_compact * 2) - (u + 2)));
                        ret = HDstrcmp(attrname, tmpname);
                        VERIFY(ret, 0, "H5Aget_name_by_idx");
                    } /* end for */

                    /* Delete last attribute */
                    ret = H5Adelete_by_idx(my_dataset, ".", idx_type, order, (hsize_t)0, H5P_DEFAULT);
                    CHECK(ret, FAIL, "H5Adelete_by_idx");

                    /* Verify state of attribute storage (empty) */
                    is_empty = H5O_is_attr_empty_test(my_dataset);
                    VERIFY(is_empty, TRUE, "H5O_is_attr_empty_test");

                    /* Check for deletion on empty attribute storage again */
                    ret = H5Adelete_by_idx(my_dataset, ".", idx_type, order, (hsize_t)0, H5P_DEFAULT);
                    VERIFY(ret, FAIL, "H5Adelete_by_idx");
                } /* end for */


                    /* Delete attributes in middle */


                /* Work on all the datasets */
                for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
                    switch(curr_dset) {
                        case 0:
                            my_dataset = dset1;
                            break;

                        case 1:
                            my_dataset = dset2;
                            break;

                        case 2:
                            my_dataset = dset3;
                            break;

                        default:
                            HDassert(0 && "Too many datasets!");
                    } /* end switch */

                    /* Create attributes, to push into dense form */
                    for(u = 0; u < (max_compact * 2); u++) {
                        /* Create attribute */
                        sprintf(attrname, "attr %02u", u);
                        attr = H5Acreate2(my_dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
                        CHECK(attr, FAIL, "H5Acreate2");

                        /* Write data into the attribute */
                        ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
                        CHECK(ret, FAIL, "H5Awrite");

                        /* Close attribute */
                        ret = H5Aclose(attr);
                        CHECK(ret, FAIL, "H5Aclose");

                        /* Verify state of object */
                        if(u >= max_compact) {
                            is_dense = H5O_is_attr_dense_test(my_dataset);
                            VERIFY(is_dense, (new_format ? TRUE : FALSE), "H5O_is_attr_dense_test");
                        } /* end if */

                        /* Verify information for new attribute */
                        ret = attr_info_by_idx_check(my_dataset, attrname, (hsize_t)u, use_index);
                        CHECK(ret, FAIL, "attr_info_by_idx_check");
                    } /* end for */
                } /* end for */

                /* Work on all the datasets */
                for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
                    switch(curr_dset) {
                        case 0:
                            my_dataset = dset1;
                            break;

                        case 1:
                            my_dataset = dset2;
                            break;

                        case 2:
                            my_dataset = dset3;
                            break;

                        default:
                            HDassert(0 && "Too many datasets!");
                    } /* end switch */

                    /* Delete every other attribute from dense storage, in appropriate order */
                    for(u = 0; u < max_compact; u++) {
                        /* Delete attribute */
                        ret = H5Adelete_by_idx(my_dataset, ".", idx_type, order, (hsize_t)u, H5P_DEFAULT);
                        CHECK(ret, FAIL, "H5Adelete_by_idx");

                        /* Verify the attribute information for first attribute in appropriate order */
                        HDmemset(&ainfo, 0, sizeof(ainfo));
                        ret = H5Aget_info_by_idx(my_dataset, ".", idx_type, order, (hsize_t)u, &ainfo, H5P_DEFAULT);
                        if(new_format) {
                            if(order == H5_ITER_INC) {
                                VERIFY(ainfo.corder, ((u * 2) + 1), "H5Aget_info_by_idx");
                            } /* end if */
                            else {
                                VERIFY(ainfo.corder, ((max_compact * 2) - ((u * 2) + 2)), "H5Aget_info_by_idx");
                            } /* end else */
                        } /* end if */

                        /* Verify the name for first attribute in appropriate order */
                        HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
                        ret = H5Aget_name_by_idx(my_dataset, ".", idx_type, order, (hsize_t)u, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT);
                        if(order == H5_ITER_INC)
                            sprintf(attrname, "attr %02u", ((u * 2) + 1));
                        else
                            sprintf(attrname, "attr %02u", ((max_compact * 2) - ((u * 2) + 2)));
                        ret = HDstrcmp(attrname, tmpname);
                        VERIFY(ret, 0, "H5Aget_name_by_idx");
                    } /* end for */
                } /* end for */

                /* Work on all the datasets */
                for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
                    switch(curr_dset) {
                        case 0:
                            my_dataset = dset1;
                            break;

                        case 1:
                            my_dataset = dset2;
                            break;

                        case 2:
                            my_dataset = dset3;
                            break;

                        default:
                            HDassert(0 && "Too many datasets!");
                    } /* end switch */

                    /* Delete remaining attributes from dense storage, in appropriate order */
                    for(u = 0; u < (max_compact - 1); u++) {
                        /* Delete attribute */
                        ret = H5Adelete_by_idx(my_dataset, ".", idx_type, order, (hsize_t)0, H5P_DEFAULT);
                        CHECK(ret, FAIL, "H5Adelete_by_idx");

                        /* Verify the attribute information for first attribute in appropriate order */
                        HDmemset(&ainfo, 0, sizeof(ainfo));
                        ret = H5Aget_info_by_idx(my_dataset, ".", idx_type, order, (hsize_t)0, &ainfo, H5P_DEFAULT);
                        if(new_format) {
                            if(order == H5_ITER_INC) {
                                VERIFY(ainfo.corder, ((u * 2) + 3), "H5Aget_info_by_idx");
                            } /* end if */
                            else {
                                VERIFY(ainfo.corder, ((max_compact * 2) - ((u * 2) + 4)), "H5Aget_info_by_idx");
                            } /* end else */
                        } /* end if */

                        /* Verify the name for first attribute in appropriate order */
                        HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
                        ret = H5Aget_name_by_idx(my_dataset, ".", idx_type, order, (hsize_t)0, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT);
                        if(order == H5_ITER_INC)
                            sprintf(attrname, "attr %02u", ((u * 2) + 3));
                        else
                            sprintf(attrname, "attr %02u", ((max_compact * 2) - ((u * 2) + 4)));
                        ret = HDstrcmp(attrname, tmpname);
                        VERIFY(ret, 0, "H5Aget_name_by_idx");
                    } /* end for */

                    /* Delete last attribute */
                    ret = H5Adelete_by_idx(my_dataset, ".", idx_type, order, (hsize_t)0, H5P_DEFAULT);
                    CHECK(ret, FAIL, "H5Adelete_by_idx");

                    /* Verify state of attribute storage (empty) */
                    is_empty = H5O_is_attr_empty_test(my_dataset);
                    VERIFY(is_empty, TRUE, "H5O_is_attr_empty_test");

                    /* Check for deletion on empty attribute storage again */
                    ret = H5Adelete_by_idx(my_dataset, ".", idx_type, order, (hsize_t)0, H5P_DEFAULT);
                    VERIFY(ret, FAIL, "H5Adelete_by_idx");
                } /* end for */

                /* Close Datasets */
                ret = H5Dclose(dset1);
                CHECK(ret, FAIL, "H5Dclose");
                ret = H5Dclose(dset2);
                CHECK(ret, FAIL, "H5Dclose");
                ret = H5Dclose(dset3);
                CHECK(ret, FAIL, "H5Dclose");

                /* Close file */
                ret = H5Fclose(fid);
                CHECK(ret, FAIL, "H5Fclose");
            } /* end for */
        } /* end for */
    } /* end for */

    /* Close property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
}   /* test_attr_delete_by_idx() */


/****************************************************************
**
**  attr_iterate2_cb(): Revised attribute operator
**
****************************************************************/
static herr_t
attr_iterate2_cb(hid_t loc_id, const char *attr_name, const H5A_info_t *info,
    void *_op_data)
{
    attr_iter_info_t *op_data = (attr_iter_info_t *)_op_data;   /* User data */
    char attrname[NAME_BUF_SIZE]; /* Object name */
    H5A_info_t my_info;         /* Local attribute info */

#ifdef QAK
HDfprintf(stderr, "attr_name = '%s'\n", attr_name);
if(info)
    HDfprintf(stderr, "info->corder = %u\n", (unsigned)info->corder);
HDfprintf(stderr, "op_data->curr = %Hd\n", op_data->curr);
#endif /* QAK */

    /* Increment # of times the callback was called */
    op_data->ncalled++;

    /* Get the attribute information directly to compare */
    if(H5Aget_info_by_name(loc_id, ".", attr_name, &my_info, H5P_DEFAULT) < 0)
        return(H5_ITER_ERROR);

    /* Check more things for revised attribute iteration (vs. older attribute iteration) */
    if(info) {
        /* Check for correct order of iteration */
        /* (if we are operating in increasing or decreasing order) */
        if(op_data->order != H5_ITER_NATIVE)
            if(info->corder != op_data->curr)
                return(H5_ITER_ERROR);

        /* Compare attribute info structs */
        if(info->corder_valid != my_info.corder_valid)
            return(H5_ITER_ERROR);
        if(info->corder != my_info.corder)
            return(H5_ITER_ERROR);
        if(info->cset != my_info.cset)
            return(H5_ITER_ERROR);
        if(info->data_size != my_info.data_size)
            return(H5_ITER_ERROR);
    } /* end if */

    /* Verify name of link */
    sprintf(attrname, "attr %02u", (unsigned)my_info.corder);
    if(HDstrcmp(attr_name, attrname))
        return(H5_ITER_ERROR);

    /* Check if we've visited this link before */
    if((size_t)op_data->curr >= op_data->max_visit)
        return(H5_ITER_ERROR);
    if(op_data->visited[op_data->curr])
        return(H5_ITER_ERROR);
    op_data->visited[op_data->curr] = TRUE;

    /* Advance to next value, in correct direction */
    if(op_data->order != H5_ITER_DEC)
        op_data->curr++;
    else
        op_data->curr--;

    /* Check for stopping in the middle of iterating */
    if(op_data->stop > 0)
        if(--op_data->stop == 0)
            return(CORDER_ITER_STOP);

    return(H5_ITER_CONT);
} /* end attr_iterate2_cb() */

#ifndef H5_NO_DEPRECATED_SYMBOLS

/****************************************************************
**
**  attr_iterate1_cb(): Attribute operator
**
****************************************************************/
static herr_t
attr_iterate1_cb(hid_t loc_id, const char *attr_name, void *_op_data)
{
    return(attr_iterate2_cb(loc_id, attr_name, NULL, _op_data));
} /* end attr_iterate1_cb() */
#endif /* H5_NO_DEPRECATED_SYMBOLS */


/*-------------------------------------------------------------------------
 * Function:    attr_iterate2_fail_cb
 *
 * Purpose:     Callback routine for iterating over attributes on object that
 *              always returns failure
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, February 20, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
attr_iterate2_fail_cb(hid_t UNUSED group_id, const char UNUSED *attr_name,
    const H5A_info_t UNUSED *info, void UNUSED *_op_data)
{
    return(H5_ITER_ERROR);
} /* end attr_iterate2_fail_cb() */


/*-------------------------------------------------------------------------
 * Function:    attr_iterate_check
 *
 * Purpose:     Check iteration over attributes on an object
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, February 20, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
attr_iterate_check(hid_t fid, const char *dsetname, hid_t obj_id,
    H5_index_t idx_type, H5_iter_order_t order, unsigned max_attrs,
    attr_iter_info_t *iter_info)
{
    unsigned    v;              /* Local index variable */
    hsize_t     skip;           /* # of attributes to skip on object */
#ifndef H5_NO_DEPRECATED_SYMBOLS
    unsigned    oskip;          /* # of attributes to skip on object, with H5Aiterate1 */
#endif /* H5_NO_DEPRECATED_SYMBOLS */
    int         old_nerrs;      /* Number of errors when entering this check */
    herr_t      ret;            /* Generic return value */

    /* Retrieve the current # of reported errors */
    old_nerrs = GetTestNumErrs();

    /* Iterate over attributes on object */
    iter_info->nskipped = (unsigned)(skip = 0);
    iter_info->order = order;
    iter_info->stop = -1;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? 0 : (max_attrs - 1);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    ret = H5Aiterate2(obj_id, idx_type, order, &skip, attr_iterate2_cb, iter_info);
    CHECK(ret, FAIL, "H5Aiterate2");

    /* Verify that we visited all the attributes */
    VERIFY(skip, max_attrs, "H5Aiterate2");
    for(v = 0; v < max_attrs; v++)
        VERIFY(iter_info->visited[v], TRUE, "H5Aiterate2");


    /* Iterate over attributes on object */
    iter_info->nskipped = (unsigned)(skip = 0);
    iter_info->order = order;
    iter_info->stop = -1;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? 0 : (max_attrs - 1);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    ret = H5Aiterate_by_name(fid, dsetname, idx_type, order, &skip, attr_iterate2_cb, iter_info, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Aiterate_by_name");

    /* Verify that we visited all the attributes */
    VERIFY(skip, max_attrs, "H5Aiterate_by_name");
    for(v = 0; v < max_attrs; v++)
        VERIFY(iter_info->visited[v], TRUE, "H5Aiterate_by_name");


    /* Iterate over attributes on object */
    iter_info->nskipped = (unsigned)(skip = 0);
    iter_info->order = order;
    iter_info->stop = -1;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? 0 : (max_attrs - 1);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    ret = H5Aiterate_by_name(obj_id, ".", idx_type, order, &skip, attr_iterate2_cb, iter_info, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Aiterate_by_name");

    /* Verify that we visited all the attributes */
    VERIFY(skip, max_attrs, "H5Aiterate_by_name");
    for(v = 0; v < max_attrs; v++)
        VERIFY(iter_info->visited[v], TRUE, "H5Aiterate_by_name");


#ifndef H5_NO_DEPRECATED_SYMBOLS
    /* Iterate over attributes on object, with H5Aiterate1 */
    iter_info->nskipped = oskip = 0;
    iter_info->order = order;
    iter_info->stop = -1;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? 0 : (max_attrs - 1);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    ret = H5Aiterate1(obj_id, &oskip, attr_iterate1_cb, iter_info);
    CHECK(ret, FAIL, "H5Aiterate1");

    /* Verify that we visited all the attributes */
    VERIFY(skip, max_attrs, "H5Aiterate1");
    for(v = 0; v < max_attrs; v++)
        VERIFY(iter_info->visited[v], TRUE, "H5Aiterate1");
#endif /* H5_NO_DEPRECATED_SYMBOLS */


    /* Skip over some attributes on object */
    iter_info->nskipped = (unsigned)(skip = max_attrs / 2);
    iter_info->order = order;
    iter_info->stop = -1;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? skip : ((max_attrs - 1) - skip);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    ret = H5Aiterate2(obj_id, idx_type, order, &skip, attr_iterate2_cb, iter_info);
    CHECK(ret, FAIL, "H5Aiterate2");

    /* Verify that we visited all the attributes */
    VERIFY(skip, max_attrs, "H5Aiterate2");
    if(order == H5_ITER_INC) {
        for(v = 0; v < (max_attrs / 2); v++)
            VERIFY(iter_info->visited[v + (max_attrs / 2)], TRUE, "H5Aiterate2");
    } /* end if */
    else if(order == H5_ITER_DEC) {
        for(v = 0; v < (max_attrs / 2); v++)
            VERIFY(iter_info->visited[v], TRUE, "H5Aiterate2");
    } /* end if */
    else {
        unsigned nvisit = 0;        /* # of links visited */

        HDassert(order == H5_ITER_NATIVE);
        for(v = 0; v < max_attrs; v++)
            if(iter_info->visited[v] == TRUE)
                nvisit++;

        VERIFY(skip, (max_attrs / 2), "H5Aiterate2");
    } /* end else */


    /* Skip over some attributes on object */
    iter_info->nskipped = (unsigned)(skip = max_attrs / 2);
    iter_info->order = order;
    iter_info->stop = -1;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? skip : ((max_attrs - 1) - skip);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    ret = H5Aiterate_by_name(fid, dsetname, idx_type, order, &skip, attr_iterate2_cb, iter_info, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Aiterate_by_name");

    /* Verify that we visited all the attributes */
    VERIFY(skip, max_attrs, "H5Aiterate_by_name");
    if(order == H5_ITER_INC) {
        for(v = 0; v < (max_attrs / 2); v++)
            VERIFY(iter_info->visited[v + (max_attrs / 2)], TRUE, "H5Aiterate_by_name");
    } /* end if */
    else if(order == H5_ITER_DEC) {
        for(v = 0; v < (max_attrs / 2); v++)
            VERIFY(iter_info->visited[v], TRUE, "H5Aiterate_by_name");
    } /* end if */
    else {
        unsigned nvisit = 0;        /* # of links visited */

        HDassert(order == H5_ITER_NATIVE);
        for(v = 0; v < max_attrs; v++)
            if(iter_info->visited[v] == TRUE)
                nvisit++;

        VERIFY(skip, (max_attrs / 2), "H5Aiterate_by_name");
    } /* end else */


    /* Skip over some attributes on object */
    iter_info->nskipped = (unsigned)(skip = max_attrs / 2);
    iter_info->order = order;
    iter_info->stop = -1;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? skip : ((max_attrs - 1) - skip);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    ret = H5Aiterate_by_name(obj_id, ".", idx_type, order, &skip, attr_iterate2_cb, iter_info, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Aiterate_by_name");

    /* Verify that we visited all the attributes */
    VERIFY(skip, max_attrs, "H5Aiterate_by_name");
    if(order == H5_ITER_INC) {
        for(v = 0; v < (max_attrs / 2); v++)
            VERIFY(iter_info->visited[v + (max_attrs / 2)], TRUE, "H5Aiterate_by_name");
    } /* end if */
    else if(order == H5_ITER_DEC) {
        for(v = 0; v < (max_attrs / 2); v++)
            VERIFY(iter_info->visited[v], TRUE, "H5Aiterate_by_name");
    } /* end if */
    else {
        unsigned nvisit = 0;        /* # of links visited */

        HDassert(order == H5_ITER_NATIVE);
        for(v = 0; v < max_attrs; v++)
            if(iter_info->visited[v] == TRUE)
                nvisit++;

        VERIFY(skip, (max_attrs / 2), "H5Aiterate_by_name");
    } /* end else */


#ifndef H5_NO_DEPRECATED_SYMBOLS
    /* Skip over some attributes on object, with H5Aiterate1 */
    iter_info->nskipped = oskip = max_attrs / 2;
    iter_info->order = order;
    iter_info->stop = -1;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? (unsigned)oskip : ((max_attrs - 1) - oskip);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    ret = H5Aiterate1(obj_id, &oskip, attr_iterate1_cb, iter_info);
    CHECK(ret, FAIL, "H5Aiterate1");

    /* Verify that we visited all the links */
    VERIFY(oskip, max_attrs, "H5Aiterate1");
    if(order == H5_ITER_INC) {
        for(v = 0; v < (max_attrs / 2); v++)
            VERIFY(iter_info->visited[v + (max_attrs / 2)], TRUE, "H5Aiterate1");
    } /* end if */
    else if(order == H5_ITER_DEC) {
        for(v = 0; v < (max_attrs / 2); v++)
            VERIFY(iter_info->visited[v], TRUE, "H5Aiterate1");
    } /* end if */
    else {
        unsigned nvisit = 0;        /* # of links visited */

        HDassert(order == H5_ITER_NATIVE);
        for(v = 0; v < max_attrs; v++)
            if(iter_info->visited[v] == TRUE)
                nvisit++;

        VERIFY(skip, (max_attrs / 2), "H5Aiterate1");
    } /* end else */
#endif /* H5_NO_DEPRECATED_SYMBOLS */


    /* Iterate over attributes on object, stopping in the middle */
    iter_info->nskipped = (unsigned)(skip = 0);
    iter_info->order = order;
    iter_info->stop = 3;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? 0 : (max_attrs - 1);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    ret = H5Aiterate2(obj_id, idx_type, order, &skip, attr_iterate2_cb, iter_info);
    CHECK(ret, FAIL, "H5Aiterate2");
    VERIFY(ret, CORDER_ITER_STOP, "H5Aiterate2");
    VERIFY(iter_info->ncalled, 3, "H5Aiterate2");


    /* Iterate over attributes on object, stopping in the middle */
    iter_info->nskipped = (unsigned)(skip = 0);
    iter_info->order = order;
    iter_info->stop = 3;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? 0 : (max_attrs - 1);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    ret = H5Aiterate_by_name(fid, dsetname, idx_type, order, &skip, attr_iterate2_cb, iter_info, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Aiterate_by_name");
    VERIFY(ret, CORDER_ITER_STOP, "H5Aiterate_by_name");
    VERIFY(iter_info->ncalled, 3, "H5Aiterate_by_name");


    /* Iterate over attributes on object, stopping in the middle */
    iter_info->nskipped = (unsigned)(skip = 0);
    iter_info->order = order;
    iter_info->stop = 3;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? 0 : (max_attrs - 1);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    ret = H5Aiterate_by_name(obj_id, ".", idx_type, order, &skip, attr_iterate2_cb, iter_info, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Aiterate_by_name");
    VERIFY(ret, CORDER_ITER_STOP, "H5Aiterate_by_name");
    VERIFY(iter_info->ncalled, 3, "H5Aiterate_by_name");


#ifndef H5_NO_DEPRECATED_SYMBOLS
    /* Iterate over attributes on object, stopping in the middle, with H5Aiterate1() */
    iter_info->nskipped = oskip = 0;
    iter_info->order = order;
    iter_info->stop = 3;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? 0 : (max_attrs - 1);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    ret = H5Aiterate1(obj_id, &oskip, attr_iterate1_cb, iter_info);
    CHECK(ret, FAIL, "H5Aiterate1");
    VERIFY(ret, CORDER_ITER_STOP, "H5Aiterate1");
    VERIFY(iter_info->ncalled, 3, "H5Aiterate1");
#endif /* H5_NO_DEPRECATED_SYMBOLS */


    /* Check for iteration routine indicating failure */
    skip = 0;
    ret = H5Aiterate2(obj_id, idx_type, order, &skip, attr_iterate2_fail_cb, NULL);
    VERIFY(ret, FAIL, "H5Aiterate2");

    skip = 0;
    ret = H5Aiterate_by_name(fid, dsetname, idx_type, order, &skip, attr_iterate2_fail_cb, NULL, H5P_DEFAULT);
    VERIFY(ret, FAIL, "H5Aiterate_by_name");

    skip = 0;
    ret = H5Aiterate_by_name(obj_id, ".", idx_type, order, &skip, attr_iterate2_fail_cb, NULL, H5P_DEFAULT);
    VERIFY(ret, FAIL, "H5Aiterate_by_name");

    /* Retrieve current # of errors */
    if(old_nerrs == GetTestNumErrs())
        return(0);
    else
        return(-1);
} /* end attr_iterate_check() */


/****************************************************************
**
**  test_attr_iterate2(): Test basic H5A (attribute) code.
**      Tests iterating over attributes by index
**
****************************************************************/
static void
test_attr_iterate2(hbool_t new_format, hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dset1, dset2, dset3;	/* Dataset IDs			*/
    hid_t	my_dataset;	/* Current dataset ID		*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    unsigned    max_compact;    /* Maximum # of links to store in group compactly */
    unsigned    min_dense;      /* Minimum # of links to store in group "densely" */
    htri_t	is_empty;	/* Are there any attributes? */
    htri_t	is_dense;	/* Are attributes stored densely? */
    hsize_t     nattrs;         /* Number of attributes on object */
    hsize_t     name_count;     /* # of records in name index */
    hsize_t     corder_count;   /* # of records in creation order index */
    H5_index_t idx_type;        /* Type of index to operate on */
    H5_iter_order_t order;      /* Order within in the index */
    attr_iter_info_t iter_info; /* Iterator info */
    hbool_t     *visited = NULL;        /* Array of flags for visiting links */
    hsize_t     idx;            /* Start index for iteration */
    hbool_t     use_index;      /* Use index on creation order values */
    const char *dsetname;       /* Name of dataset for attributes */
    char	attrname[NAME_BUF_SIZE];    /* Name of attribute */
    unsigned    curr_dset;      /* Current dataset to work on */
    unsigned    u;              /* Local index variable */
    herr_t	ret;		/* Generic return value		*/

    /* Create dataspace for dataset & attributes */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create dataset creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Query the attribute creation properties */
    ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");

    /* Allocate the "visited link" array */
    iter_info.max_visit = max_compact * 2;
    visited = HDmalloc(sizeof(hbool_t) * iter_info.max_visit);
    CHECK(visited, NULL, "HDmalloc");
    iter_info.visited = visited;

    /* Loop over operating on different indices on link fields */
    for(idx_type = H5_INDEX_NAME; idx_type <=H5_INDEX_CRT_ORDER; idx_type++) {
        /* Loop over operating in different orders */
        for(order = H5_ITER_INC; order <=H5_ITER_DEC; order++) {
            /* Loop over using index for creation order value */
            for(use_index = FALSE; use_index <= TRUE; use_index++) {
                /* Print appropriate test message */
                if(idx_type == H5_INDEX_CRT_ORDER) {
                    if(order == H5_ITER_INC) {
                        if(use_index)
                            MESSAGE(5, ("Testing Iterating over Attributes By Creation Order Index in Increasing Order w/Creation Order Index\n"))
                        else
                            MESSAGE(5, ("Testing Iterating over Attributes By Creation Order Index in Increasing Order w/o Creation Order Index\n"))
                    } /* end if */
                    else {
                        if(use_index)
                            MESSAGE(5, ("Testing Iterating over Attributes By Creation Order Index in Decreasing Order w/Creation Order Index\n"))
                        else
                            MESSAGE(5, ("Testing Iterating over Attributes By Creation Order Index in Decreasing Order w/o Creation Order Index\n"))
                    } /* end else */
                } /* end if */
                else {
                    if(order == H5_ITER_INC) {
                        if(use_index)
                            MESSAGE(5, ("Testing Iterating over Attributes By Name Index in Increasing Order w/Creation Order Index\n"))
                        else
                            MESSAGE(5, ("Testing Iterating over Attributes By Name Index in Increasing Order w/o Creation Order Index\n"))
                    } /* end if */
                    else {
                        if(use_index)
                            MESSAGE(5, ("Testing Iterating over Attributes By Name Index in Decreasing Order w/Creation Order Index\n"))
                        else
                            MESSAGE(5, ("Testing Iterating over Attributes By Name Index in Decreasing Order w/o Creation Order Index\n"))
                    } /* end else */
                } /* end else */

                /* Create file */
                fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
                CHECK(fid, FAIL, "H5Fcreate");

                /* Set attribute creation order tracking & indexing for object */
                if(new_format == TRUE) {
                    ret = H5Pset_attr_creation_order(dcpl, (H5P_CRT_ORDER_TRACKED | (use_index ? H5P_CRT_ORDER_INDEXED : (unsigned)0)));
                    CHECK(ret, FAIL, "H5Pset_attr_creation_order");
                } /* end if */

                /* Create datasets */
                dset1 = H5Dcreate2(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
                CHECK(dset1, FAIL, "H5Dcreate2");
                dset2 = H5Dcreate2(fid, DSET2_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
                CHECK(dset2, FAIL, "H5Dcreate2");
                dset3 = H5Dcreate2(fid, DSET3_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
                CHECK(dset3, FAIL, "H5Dcreate2");

                /* Work on all the datasets */
                for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
                    switch(curr_dset) {
                        case 0:
                            my_dataset = dset1;
                            dsetname = DSET1_NAME;
                            break;

                        case 1:
                            my_dataset = dset2;
                            dsetname = DSET2_NAME;
                            break;

                        case 2:
                            my_dataset = dset3;
                            dsetname = DSET3_NAME;
                            break;

                        default:
                            HDassert(0 && "Too many datasets!");
                    } /* end switch */

                    /* Check on dataset's attribute storage status */
                    is_empty = H5O_is_attr_empty_test(my_dataset);
                    VERIFY(is_empty, TRUE, "H5O_is_attr_empty_test");
                    is_dense = H5O_is_attr_dense_test(my_dataset);
                    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

                    /* Check for iterating over object with no attributes (should be OK) */
                    ret = H5Aiterate2(my_dataset, idx_type, order, NULL, attr_iterate2_cb, NULL);
                    CHECK(ret, FAIL, "H5Aiterate2");

                    ret = H5Aiterate_by_name(fid, dsetname, idx_type, order, NULL, attr_iterate2_cb, NULL, H5P_DEFAULT);
                    CHECK(ret, FAIL, "H5Aiterate_by_name");

                    ret = H5Aiterate_by_name(my_dataset, ".", idx_type, order, NULL, attr_iterate2_cb, NULL, H5P_DEFAULT);
                    CHECK(ret, FAIL, "H5Aiterate_by_name");

                    /* Create attributes, up to limit of compact form */
                    for(u = 0; u < max_compact; u++) {
                        /* Create attribute */
                        sprintf(attrname, "attr %02u", u);
                        attr = H5Acreate2(my_dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
                        CHECK(attr, FAIL, "H5Acreate2");

                        /* Write data into the attribute */
                        ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
                        CHECK(ret, FAIL, "H5Awrite");

                        /* Close attribute */
                        ret = H5Aclose(attr);
                        CHECK(ret, FAIL, "H5Aclose");

                        /* Verify information for new attribute */
                        ret = attr_info_by_idx_check(my_dataset, attrname, (hsize_t)u, use_index);
                        CHECK(ret, FAIL, "attr_info_by_idx_check");
                    } /* end for */

                    /* Verify state of object */
                    ret = H5O_num_attrs_test(my_dataset, &nattrs);
                    CHECK(ret, FAIL, "H5O_num_attrs_test");
                    VERIFY(nattrs, max_compact, "H5O_num_attrs_test");
                    is_empty = H5O_is_attr_empty_test(my_dataset);
                    VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
                    is_dense = H5O_is_attr_dense_test(my_dataset);
                    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

                    /* Check for out of bound iteration */
                    idx = u;
                    ret = H5Aiterate2(my_dataset, idx_type, order, &idx, attr_iterate2_cb, NULL);
                    VERIFY(ret, FAIL, "H5Aiterate2");

                    idx = u;
                    ret = H5Aiterate_by_name(fid, dsetname, idx_type, order, &idx, attr_iterate2_cb, NULL, H5P_DEFAULT);
                    VERIFY(ret, FAIL, "H5Aiterate_by_name");

                    idx = u;
                    ret = H5Aiterate_by_name(my_dataset, ".", idx_type, order, &idx, attr_iterate2_cb, NULL, H5P_DEFAULT);
                    VERIFY(ret, FAIL, "H5Aiterate_by_name");

                    /* Test iteration over attributes stored compactly */
                    ret = attr_iterate_check(fid, dsetname, my_dataset, idx_type, order, u, &iter_info);
                    CHECK(ret, FAIL, "attr_iterate_check");
                } /* end for */


                /* Work on all the datasets */
                for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
                    switch(curr_dset) {
                        case 0:
                            my_dataset = dset1;
                            dsetname = DSET1_NAME;
                            break;

                        case 1:
                            my_dataset = dset2;
                            dsetname = DSET2_NAME;
                            break;

                        case 2:
                            my_dataset = dset3;
                            dsetname = DSET3_NAME;
                            break;

                        default:
                            HDassert(0 && "Too many datasets!");
                    } /* end switch */

                    /* Create more attributes, to push into dense form */
                    for(u = max_compact; u < (max_compact * 2); u++) {
                        /* Create attribute */
                        sprintf(attrname, "attr %02u", u);
                        attr = H5Acreate2(my_dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
                        CHECK(attr, FAIL, "H5Acreate2");

                        /* Write data into the attribute */
                        ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
                        CHECK(ret, FAIL, "H5Awrite");

                        /* Close attribute */
                        ret = H5Aclose(attr);
                        CHECK(ret, FAIL, "H5Aclose");

                        /* Verify state of object */
                        if(u >= max_compact) {
                            is_dense = H5O_is_attr_dense_test(my_dataset);
                            VERIFY(is_dense, (new_format ? TRUE : FALSE), "H5O_is_attr_dense_test");
                        } /* end if */

                        /* Verify information for new attribute */
                        ret = attr_info_by_idx_check(my_dataset, attrname, (hsize_t)u, use_index);
                        CHECK(ret, FAIL, "attr_info_by_idx_check");
                    } /* end for */

                    /* Verify state of object */
                    ret = H5O_num_attrs_test(my_dataset, &nattrs);
                    CHECK(ret, FAIL, "H5O_num_attrs_test");
                    VERIFY(nattrs, (max_compact * 2), "H5O_num_attrs_test");
                    is_empty = H5O_is_attr_empty_test(my_dataset);
                    VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
                    is_dense = H5O_is_attr_dense_test(my_dataset);
                    VERIFY(is_dense, (new_format ? TRUE : FALSE), "H5O_is_attr_dense_test");

                    if(new_format) {
                        /* Retrieve & verify # of records in the name & creation order indices */
                        ret = H5O_attr_dense_info_test(my_dataset, &name_count, &corder_count);
                        CHECK(ret, FAIL, "H5O_attr_dense_info_test");
                        if(use_index)
                            VERIFY(name_count, corder_count, "H5O_attr_dense_info_test");
                        VERIFY(name_count, (max_compact * 2), "H5O_attr_dense_info_test");
                    } /* end if */

                    /* Check for out of bound iteration */
                    idx = u;
                    ret = H5Aiterate2(my_dataset, idx_type, order, &idx, attr_iterate2_cb, NULL);
                    VERIFY(ret, FAIL, "H5Aiterate2");

                    idx = u;
                    ret = H5Aiterate_by_name(fid, dsetname, idx_type, order, &idx, attr_iterate2_cb, NULL, H5P_DEFAULT);
                    VERIFY(ret, FAIL, "H5Aiterate_by_name");

                    idx = u;
                    ret = H5Aiterate_by_name(my_dataset, ".", idx_type, order, &idx, attr_iterate2_cb, NULL, H5P_DEFAULT);
                    VERIFY(ret, FAIL, "H5Aiterate_by_name");

                    /* Test iteration over attributes stored densely */
                    ret = attr_iterate_check(fid, dsetname, my_dataset, idx_type, order, u, &iter_info);
                    CHECK(ret, FAIL, "attr_iterate_check");
                } /* end for */

                /* Close Datasets */
                ret = H5Dclose(dset1);
                CHECK(ret, FAIL, "H5Dclose");
                ret = H5Dclose(dset2);
                CHECK(ret, FAIL, "H5Dclose");
                ret = H5Dclose(dset3);
                CHECK(ret, FAIL, "H5Dclose");

                /* Close file */
                ret = H5Fclose(fid);
                CHECK(ret, FAIL, "H5Fclose");
            } /* end for */
        } /* end for */
    } /* end for */

    /* Close property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Free the "visited link" array */
    HDfree(visited);
}   /* test_attr_iterate2() */


/*-------------------------------------------------------------------------
 * Function:    attr_open_by_idx_check
 *
 * Purpose:     Check opening attribute by index on an object
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, February 21, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
attr_open_by_idx_check(hid_t obj_id, H5_index_t idx_type, H5_iter_order_t order,
    unsigned max_attrs)
{
    hid_t       attr_id;        /* ID of attribute to test */
    H5A_info_t  ainfo;          /* Attribute info */
    int         old_nerrs;      /* Number of errors when entering this check */
    unsigned    u;              /* Local index variable */
    herr_t	ret;		/* Generic return value	*/

    /* Retrieve the current # of reported errors */
    old_nerrs = GetTestNumErrs();

    /* Open each attribute on object by index and check that it's the correct one */
    for(u = 0; u < max_attrs; u++) {
        /* Open the attribute */
        attr_id = H5Aopen_by_idx(obj_id, ".", idx_type, order, (hsize_t)u, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(attr_id, FAIL, "H5Aopen_by_idx");

        /* Get the attribute's information */
        ret = H5Aget_info(attr_id, &ainfo);
        CHECK(ret, FAIL, "H5Aget_info");

        /* Check that the object is the correct one */
        if(order == H5_ITER_INC) {
            VERIFY(ainfo.corder, u, "H5Aget_info");
        } /* end if */
        else if(order == H5_ITER_DEC) {
            VERIFY(ainfo.corder, (max_attrs - (u + 1)), "H5Aget_info");
        } /* end if */
        else {
            /* XXX: What to do about native order? */
        } /* end else */

        /* Close attribute */
        ret = H5Aclose(attr_id);
        CHECK(ret, FAIL, "H5Aclose");
    } /* end for */

    /* Retrieve current # of errors */
    if(old_nerrs == GetTestNumErrs())
        return(0);
    else
        return(-1);
} /* end attr_open_by_idx_check() */


/****************************************************************
**
**  test_attr_open_by_idx(): Test basic H5A (attribute) code.
**      Tests opening attributes by index
**
****************************************************************/
static void
test_attr_open_by_idx(hbool_t new_format, hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dset1, dset2, dset3;	/* Dataset IDs			*/
    hid_t	my_dataset;	/* Current dataset ID		*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    unsigned    max_compact;    /* Maximum # of links to store in group compactly */
    unsigned    min_dense;      /* Minimum # of links to store in group "densely" */
    htri_t	is_empty;	/* Are there any attributes? */
    htri_t	is_dense;	/* Are attributes stored densely? */
    hsize_t     nattrs;         /* Number of attributes on object */
    hsize_t     name_count;     /* # of records in name index */
    hsize_t     corder_count;   /* # of records in creation order index */
    H5_index_t idx_type;        /* Type of index to operate on */
    H5_iter_order_t order;      /* Order within in the index */
    hbool_t     use_index;      /* Use index on creation order values */
    char	attrname[NAME_BUF_SIZE];    /* Name of attribute */
    unsigned    curr_dset;      /* Current dataset to work on */
    unsigned    u;              /* Local index variable */
    herr_t	ret;		/* Generic return value		*/

    /* Create dataspace for dataset & attributes */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create dataset creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Query the attribute creation properties */
    ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");

    /* Loop over operating on different indices on link fields */
    for(idx_type = H5_INDEX_NAME; idx_type <=H5_INDEX_CRT_ORDER; idx_type++) {
        /* Loop over operating in different orders */
        for(order = H5_ITER_INC; order <=H5_ITER_DEC; order++) {
            /* Loop over using index for creation order value */
            for(use_index = FALSE; use_index <= TRUE; use_index++) {
                /* Print appropriate test message */
                if(idx_type == H5_INDEX_CRT_ORDER) {
                    if(order == H5_ITER_INC) {
                        if(use_index)
                            MESSAGE(5, ("Testing Opening Attributes By Creation Order Index in Increasing Order w/Creation Order Index\n"))
                        else
                            MESSAGE(5, ("Testing Opening Attributes By Creation Order Index in Increasing Order w/o Creation Order Index\n"))
                    } /* end if */
                    else {
                        if(use_index)
                            MESSAGE(5, ("Testing Opening Attributes By Creation Order Index in Decreasing Order w/Creation Order Index\n"))
                        else
                            MESSAGE(5, ("Testing Opening Attributes By Creation Order Index in Decreasing Order w/o Creation Order Index\n"))
                    } /* end else */
                } /* end if */
                else {
                    if(order == H5_ITER_INC) {
                        if(use_index)
                            MESSAGE(5, ("Testing Opening Attributes By Name Index in Increasing Order w/Creation Order Index\n"))
                        else
                            MESSAGE(5, ("Testing Opening Attributes By Name Index in Increasing Order w/o Creation Order Index\n"))
                    } /* end if */
                    else {
                        if(use_index)
                            MESSAGE(5, ("Testing Opening Attributes By Name Index in Decreasing Order w/Creation Order Index\n"))
                        else
                            MESSAGE(5, ("Testing Opening Attributes By Name Index in Decreasing Order w/o Creation Order Index\n"))
                    } /* end else */
                } /* end else */

                /* Create file */
                fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
                CHECK(fid, FAIL, "H5Fcreate");

                /* Set attribute creation order tracking & indexing for object */
                if(new_format == TRUE) {
                    ret = H5Pset_attr_creation_order(dcpl, (H5P_CRT_ORDER_TRACKED | (use_index ? H5P_CRT_ORDER_INDEXED : (unsigned)0)));
                    CHECK(ret, FAIL, "H5Pset_attr_creation_order");
                } /* end if */

                /* Create datasets */
                dset1 = H5Dcreate2(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
                CHECK(dset1, FAIL, "H5Dcreate2");
                dset2 = H5Dcreate2(fid, DSET2_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
                CHECK(dset2, FAIL, "H5Dcreate2");
                dset3 = H5Dcreate2(fid, DSET3_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
                CHECK(dset3, FAIL, "H5Dcreate2");

                /* Work on all the datasets */
                for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
                    switch(curr_dset) {
                        case 0:
                            my_dataset = dset1;
                            break;

                        case 1:
                            my_dataset = dset2;
                            break;

                        case 2:
                            my_dataset = dset3;
                            break;

                        default:
                            HDassert(0 && "Too many datasets!");
                    } /* end switch */

                    /* Check on dataset's attribute storage status */
                    is_empty = H5O_is_attr_empty_test(my_dataset);
                    VERIFY(is_empty, TRUE, "H5O_is_attr_empty_test");
                    is_dense = H5O_is_attr_dense_test(my_dataset);
                    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

                    /* Check for opening an attribute on an object with no attributes */
                    ret = H5Aopen_by_idx(my_dataset, ".", idx_type, order, (hsize_t)0, H5P_DEFAULT, H5P_DEFAULT);
                    VERIFY(ret, FAIL, "H5Aopen_by_idx");

                    /* Create attributes, up to limit of compact form */
                    for(u = 0; u < max_compact; u++) {
                        /* Create attribute */
                        sprintf(attrname, "attr %02u", u);
                        attr = H5Acreate2(my_dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
                        CHECK(attr, FAIL, "H5Acreate2");

                        /* Write data into the attribute */
                        ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
                        CHECK(ret, FAIL, "H5Awrite");

                        /* Close attribute */
                        ret = H5Aclose(attr);
                        CHECK(ret, FAIL, "H5Aclose");

                        /* Verify information for new attribute */
                        ret = attr_info_by_idx_check(my_dataset, attrname, (hsize_t)u, use_index);
                        CHECK(ret, FAIL, "attr_info_by_idx_check");
                    } /* end for */

                    /* Verify state of object */
                    ret = H5O_num_attrs_test(my_dataset, &nattrs);
                    CHECK(ret, FAIL, "H5O_num_attrs_test");
                    VERIFY(nattrs, max_compact, "H5O_num_attrs_test");
                    is_empty = H5O_is_attr_empty_test(my_dataset);
                    VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
                    is_dense = H5O_is_attr_dense_test(my_dataset);
                    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

                    /* Check for out of bound opening an attribute on an object */
                    ret = H5Aopen_by_idx(my_dataset, ".", idx_type, order, (hsize_t)u, H5P_DEFAULT, H5P_DEFAULT);
                    VERIFY(ret, FAIL, "H5Aopen_by_idx");

                    /* Test opening attributes by index stored compactly */
                    ret = attr_open_by_idx_check(my_dataset, idx_type, order, u);
                    CHECK(ret, FAIL, "attr_open_by_idx_check");
                } /* end for */


                /* Work on all the datasets */
                for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
                    switch(curr_dset) {
                        case 0:
                            my_dataset = dset1;
                            break;

                        case 1:
                            my_dataset = dset2;
                            break;

                        case 2:
                            my_dataset = dset3;
                            break;

                        default:
                            HDassert(0 && "Too many datasets!");
                    } /* end switch */

                    /* Create more attributes, to push into dense form */
                    for(u = max_compact; u < (max_compact * 2); u++) {
                        /* Create attribute */
                        sprintf(attrname, "attr %02u", u);
                        attr = H5Acreate2(my_dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
                        CHECK(attr, FAIL, "H5Acreate2");

                        /* Write data into the attribute */
                        ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
                        CHECK(ret, FAIL, "H5Awrite");

                        /* Close attribute */
                        ret = H5Aclose(attr);
                        CHECK(ret, FAIL, "H5Aclose");

                        /* Verify state of object */
                        if(u >= max_compact) {
                            is_dense = H5O_is_attr_dense_test(my_dataset);
                            VERIFY(is_dense, (new_format ? TRUE : FALSE), "H5O_is_attr_dense_test");
                        } /* end if */

                        /* Verify information for new attribute */
                        ret = attr_info_by_idx_check(my_dataset, attrname, (hsize_t)u, use_index);
                        CHECK(ret, FAIL, "attr_info_by_idx_check");
                    } /* end for */

                    /* Verify state of object */
                    ret = H5O_num_attrs_test(my_dataset, &nattrs);
                    CHECK(ret, FAIL, "H5O_num_attrs_test");
                    VERIFY(nattrs, (max_compact * 2), "H5O_num_attrs_test");
                    is_empty = H5O_is_attr_empty_test(my_dataset);
                    VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
                    is_dense = H5O_is_attr_dense_test(my_dataset);
                    VERIFY(is_dense, (new_format ? TRUE : FALSE), "H5O_is_attr_dense_test");

                    if(new_format) {
                        /* Retrieve & verify # of records in the name & creation order indices */
                        ret = H5O_attr_dense_info_test(my_dataset, &name_count, &corder_count);
                        CHECK(ret, FAIL, "H5O_attr_dense_info_test");
                        if(use_index)
                            VERIFY(name_count, corder_count, "H5O_attr_dense_info_test");
                        VERIFY(name_count, (max_compact * 2), "H5O_attr_dense_info_test");
                    } /* end if */

                    /* Check for out of bound opening an attribute on an object */
                    ret = H5Aopen_by_idx(my_dataset, ".", idx_type, order, (hsize_t)u, H5P_DEFAULT, H5P_DEFAULT);
                    VERIFY(ret, FAIL, "H5Aopen_by_idx");

                    /* Test opening attributes by index stored compactly */
                    ret = attr_open_by_idx_check(my_dataset, idx_type, order, u);
                    CHECK(ret, FAIL, "attr_open_by_idx_check");
                } /* end for */

                /* Close Datasets */
                ret = H5Dclose(dset1);
                CHECK(ret, FAIL, "H5Dclose");
                ret = H5Dclose(dset2);
                CHECK(ret, FAIL, "H5Dclose");
                ret = H5Dclose(dset3);
                CHECK(ret, FAIL, "H5Dclose");

                /* Close file */
                ret = H5Fclose(fid);
                CHECK(ret, FAIL, "H5Fclose");
            } /* end for */
        } /* end for */
    } /* end for */

    /* Close property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
}   /* test_attr_open_by_idx() */


/*-------------------------------------------------------------------------
 * Function:    attr_open_check
 *
 * Purpose:     Check opening attribute on an object
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, February 21, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
attr_open_check(hid_t fid, const char *dsetname, hid_t obj_id,
    unsigned max_attrs)
{
    hid_t       attr_id;        /* ID of attribute to test */
    H5A_info_t  ainfo;          /* Attribute info */
    char	attrname[NAME_BUF_SIZE];    /* Name of attribute */
    int         old_nerrs;      /* Number of errors when entering this check */
    unsigned    u;              /* Local index variable */
    herr_t	ret;		/* Generic return value	*/

    /* Retrieve the current # of reported errors */
    old_nerrs = GetTestNumErrs();

    /* Open each attribute on object by index and check that it's the correct one */
    for(u = 0; u < max_attrs; u++) {
        /* Open the attribute */
        sprintf(attrname, "attr %02u", u);
        attr_id = H5Aopen(obj_id, attrname, H5P_DEFAULT);
        CHECK(attr_id, FAIL, "H5Aopen");

        /* Get the attribute's information */
        ret = H5Aget_info(attr_id, &ainfo);
        CHECK(ret, FAIL, "H5Aget_info");

        /* Check that the object is the correct one */
        VERIFY(ainfo.corder, u, "H5Aget_info");

        /* Close attribute */
        ret = H5Aclose(attr_id);
        CHECK(ret, FAIL, "H5Aclose");


        /* Open the attribute */
        attr_id = H5Aopen_by_name(obj_id, ".", attrname, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(attr_id, FAIL, "H5Aopen_by_name");

        /* Get the attribute's information */
        ret = H5Aget_info(attr_id, &ainfo);
        CHECK(ret, FAIL, "H5Aget_info");

        /* Check that the object is the correct one */
        VERIFY(ainfo.corder, u, "H5Aget_info");

        /* Close attribute */
        ret = H5Aclose(attr_id);
        CHECK(ret, FAIL, "H5Aclose");


        /* Open the attribute */
        attr_id = H5Aopen_by_name(fid, dsetname, attrname, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(attr_id, FAIL, "H5Aopen_by_name");

        /* Get the attribute's information */
        ret = H5Aget_info(attr_id, &ainfo);
        CHECK(ret, FAIL, "H5Aget_info");

        /* Check that the object is the correct one */
        VERIFY(ainfo.corder, u, "H5Aget_info");

        /* Close attribute */
        ret = H5Aclose(attr_id);
        CHECK(ret, FAIL, "H5Aclose");
    } /* end for */

    /* Retrieve current # of errors */
    if(old_nerrs == GetTestNumErrs())
        return(0);
    else
        return(-1);
} /* end attr_open_check() */


/****************************************************************
**
**  test_attr_open_by_name(): Test basic H5A (attribute) code.
**      Tests opening attributes by name
**
****************************************************************/
static void
test_attr_open_by_name(hbool_t new_format, hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dset1, dset2, dset3;	/* Dataset IDs			*/
    hid_t	my_dataset;	/* Current dataset ID		*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    unsigned    max_compact;    /* Maximum # of links to store in group compactly */
    unsigned    min_dense;      /* Minimum # of links to store in group "densely" */
    htri_t	is_empty;	/* Are there any attributes? */
    htri_t	is_dense;	/* Are attributes stored densely? */
    hsize_t     nattrs;         /* Number of attributes on object */
    hsize_t     name_count;     /* # of records in name index */
    hsize_t     corder_count;   /* # of records in creation order index */
    hbool_t     use_index;      /* Use index on creation order values */
    const char *dsetname;       /* Name of dataset for attributes */
    char	attrname[NAME_BUF_SIZE];    /* Name of attribute */
    unsigned    curr_dset;      /* Current dataset to work on */
    unsigned    u;              /* Local index variable */
    herr_t	ret;		/* Generic return value		*/

    /* Create dataspace for dataset & attributes */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create dataset creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Query the attribute creation properties */
    ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");

    /* Loop over using index for creation order value */
    for(use_index = FALSE; use_index <= TRUE; use_index++) {
        /* Print appropriate test message */
        if(use_index)
            MESSAGE(5, ("Testing Opening Attributes By Name w/Creation Order Index\n"))
        else
            MESSAGE(5, ("Testing Opening Attributes By Name w/o Creation Order Index\n"))

        /* Create file */
        fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
        CHECK(fid, FAIL, "H5Fcreate");

        /* Set attribute creation order tracking & indexing for object */
        if(new_format == TRUE) {
            ret = H5Pset_attr_creation_order(dcpl, (H5P_CRT_ORDER_TRACKED | (use_index ? H5P_CRT_ORDER_INDEXED : (unsigned)0)));
            CHECK(ret, FAIL, "H5Pset_attr_creation_order");
        } /* end if */

        /* Create datasets */
        dset1 = H5Dcreate2(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        CHECK(dset1, FAIL, "H5Dcreate2");
        dset2 = H5Dcreate2(fid, DSET2_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        CHECK(dset2, FAIL, "H5Dcreate2");
        dset3 = H5Dcreate2(fid, DSET3_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        CHECK(dset3, FAIL, "H5Dcreate2");

        /* Work on all the datasets */
        for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
            switch(curr_dset) {
                case 0:
                    my_dataset = dset1;
                    dsetname = DSET1_NAME;
                    break;

                case 1:
                    my_dataset = dset2;
                    dsetname = DSET2_NAME;
                    break;

                case 2:
                    my_dataset = dset3;
                    dsetname = DSET3_NAME;
                    break;

                default:
                    HDassert(0 && "Too many datasets!");
            } /* end switch */

            /* Check on dataset's attribute storage status */
            is_empty = H5O_is_attr_empty_test(my_dataset);
            VERIFY(is_empty, TRUE, "H5O_is_attr_empty_test");
            is_dense = H5O_is_attr_dense_test(my_dataset);
            VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

            /* Check for opening a non-existant attribute on an object with no attributes */
            ret = H5Aopen(my_dataset, "foo", H5P_DEFAULT);
            VERIFY(ret, FAIL, "H5Aopen");

            ret = H5Aopen_by_name(my_dataset, ".", "foo", H5P_DEFAULT, H5P_DEFAULT);
            VERIFY(ret, FAIL, "H5Aopen_by_name");

            ret = H5Aopen_by_name(fid, dsetname, "foo", H5P_DEFAULT, H5P_DEFAULT);
            VERIFY(ret, FAIL, "H5Aopen_by_name");

            /* Create attributes, up to limit of compact form */
            for(u = 0; u < max_compact; u++) {
                /* Create attribute */
                sprintf(attrname, "attr %02u", u);
                attr = H5Acreate2(my_dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate2");

                /* Write data into the attribute */
                ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
                CHECK(ret, FAIL, "H5Awrite");

                /* Close attribute */
                ret = H5Aclose(attr);
                CHECK(ret, FAIL, "H5Aclose");

                /* Verify information for new attribute */
                ret = attr_info_by_idx_check(my_dataset, attrname, (hsize_t)u, use_index);
                CHECK(ret, FAIL, "attr_info_by_idx_check");
            } /* end for */

            /* Verify state of object */
            ret = H5O_num_attrs_test(my_dataset, &nattrs);
            CHECK(ret, FAIL, "H5O_num_attrs_test");
            VERIFY(nattrs, max_compact, "H5O_num_attrs_test");
            is_empty = H5O_is_attr_empty_test(my_dataset);
            VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
            is_dense = H5O_is_attr_dense_test(my_dataset);
            VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

            /* Check for opening a non-existant attribute on an object with compact attribute storage */
            ret = H5Aopen(my_dataset, "foo", H5P_DEFAULT);
            VERIFY(ret, FAIL, "H5Aopen");

            ret = H5Aopen_by_name(my_dataset, ".", "foo", H5P_DEFAULT, H5P_DEFAULT);
            VERIFY(ret, FAIL, "H5Aopen_by_name");

            ret = H5Aopen_by_name(fid, dsetname, "foo", H5P_DEFAULT, H5P_DEFAULT);
            VERIFY(ret, FAIL, "H5Aopen_by_name");

            /* Test opening attributes stored compactly */
            ret = attr_open_check(fid, dsetname, my_dataset, u);
            CHECK(ret, FAIL, "attr_open_check");
        } /* end for */


        /* Work on all the datasets */
        for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
            switch(curr_dset) {
                case 0:
                    my_dataset = dset1;
                    dsetname = DSET1_NAME;
                    break;

                case 1:
                    my_dataset = dset2;
                    dsetname = DSET2_NAME;
                    break;

                case 2:
                    my_dataset = dset3;
                    dsetname = DSET3_NAME;
                    break;

                default:
                    HDassert(0 && "Too many datasets!");
            } /* end switch */

            /* Create more attributes, to push into dense form */
            for(u = max_compact; u < (max_compact * 2); u++) {
                /* Create attribute */
                sprintf(attrname, "attr %02u", u);
                attr = H5Acreate2(my_dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate2");

                /* Write data into the attribute */
                ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
                CHECK(ret, FAIL, "H5Awrite");

                /* Close attribute */
                ret = H5Aclose(attr);
                CHECK(ret, FAIL, "H5Aclose");

                /* Verify state of object */
                if(u >= max_compact) {
                    is_dense = H5O_is_attr_dense_test(my_dataset);
                    VERIFY(is_dense, (new_format ? TRUE : FALSE), "H5O_is_attr_dense_test");
                } /* end if */

                /* Verify information for new attribute */
                ret = attr_info_by_idx_check(my_dataset, attrname, (hsize_t)u, use_index);
                CHECK(ret, FAIL, "attr_info_by_idx_check");
            } /* end for */

            /* Verify state of object */
            ret = H5O_num_attrs_test(my_dataset, &nattrs);
            CHECK(ret, FAIL, "H5O_num_attrs_test");
            VERIFY(nattrs, (max_compact * 2), "H5O_num_attrs_test");
            is_empty = H5O_is_attr_empty_test(my_dataset);
            VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
            is_dense = H5O_is_attr_dense_test(my_dataset);
            VERIFY(is_dense, (new_format ? TRUE : FALSE), "H5O_is_attr_dense_test");

            if(new_format) {
                /* Retrieve & verify # of records in the name & creation order indices */
                ret = H5O_attr_dense_info_test(my_dataset, &name_count, &corder_count);
                CHECK(ret, FAIL, "H5O_attr_dense_info_test");
                if(use_index)
                    VERIFY(name_count, corder_count, "H5O_attr_dense_info_test");
                VERIFY(name_count, (max_compact * 2), "H5O_attr_dense_info_test");
            } /* end if */

            /* Check for opening a non-existant attribute on an object with dense attribute storage */
            ret = H5Aopen(my_dataset, "foo", H5P_DEFAULT);
            VERIFY(ret, FAIL, "H5Aopen");

            ret = H5Aopen_by_name(my_dataset, ".", "foo", H5P_DEFAULT, H5P_DEFAULT);
            VERIFY(ret, FAIL, "H5Aopen_by_name");

            ret = H5Aopen_by_name(fid, dsetname, "foo", H5P_DEFAULT, H5P_DEFAULT);
            VERIFY(ret, FAIL, "H5Aopen_by_name");

            /* Test opening attributes stored compactly */
            ret = attr_open_check(fid, dsetname, my_dataset, u);
            CHECK(ret, FAIL, "attr_open_check");
        } /* end for */

        /* Close Datasets */
        ret = H5Dclose(dset1);
        CHECK(ret, FAIL, "H5Dclose");
        ret = H5Dclose(dset2);
        CHECK(ret, FAIL, "H5Dclose");
        ret = H5Dclose(dset3);
        CHECK(ret, FAIL, "H5Dclose");

        /* Close file */
        ret = H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");
    } /* end for */

    /* Close property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
}   /* test_attr_open_by_name() */


/****************************************************************
**
**  test_attr_create_by_name(): Test basic H5A (attribute) code.
**      Tests creating attributes by name
**
****************************************************************/
static void
test_attr_create_by_name(hbool_t new_format, hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dset1, dset2, dset3;	/* Dataset IDs			*/
    hid_t	my_dataset;	/* Current dataset ID		*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    unsigned    max_compact;    /* Maximum # of links to store in group compactly */
    unsigned    min_dense;      /* Minimum # of links to store in group "densely" */
    htri_t	is_empty;	/* Are there any attributes? */
    htri_t	is_dense;	/* Are attributes stored densely? */
    hsize_t     nattrs;         /* Number of attributes on object */
    hsize_t     name_count;     /* # of records in name index */
    hsize_t     corder_count;   /* # of records in creation order index */
    hbool_t     use_index;      /* Use index on creation order values */
    const char *dsetname;       /* Name of dataset for attributes */
    char	attrname[NAME_BUF_SIZE];    /* Name of attribute */
    unsigned    curr_dset;      /* Current dataset to work on */
    unsigned    u;              /* Local index variable */
    herr_t	ret;		/* Generic return value		*/

    /* Create dataspace for dataset & attributes */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create dataset creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Query the attribute creation properties */
    ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");

    /* Loop over using index for creation order value */
    for(use_index = FALSE; use_index <= TRUE; use_index++) {
        /* Print appropriate test message */
        if(use_index)
            MESSAGE(5, ("Testing Creating Attributes By Name w/Creation Order Index\n"))
        else
            MESSAGE(5, ("Testing Creating Attributes By Name w/o Creation Order Index\n"))

        /* Create file */
        fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
        CHECK(fid, FAIL, "H5Fcreate");

        /* Set attribute creation order tracking & indexing for object */
        if(new_format == TRUE) {
            ret = H5Pset_attr_creation_order(dcpl, (H5P_CRT_ORDER_TRACKED | (use_index ? H5P_CRT_ORDER_INDEXED : (unsigned)0)));
            CHECK(ret, FAIL, "H5Pset_attr_creation_order");
        } /* end if */

        /* Create datasets */
        dset1 = H5Dcreate2(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        CHECK(dset1, FAIL, "H5Dcreate2");
        dset2 = H5Dcreate2(fid, DSET2_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        CHECK(dset2, FAIL, "H5Dcreate2");
        dset3 = H5Dcreate2(fid, DSET3_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        CHECK(dset3, FAIL, "H5Dcreate2");

        /* Work on all the datasets */
        for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
            switch(curr_dset) {
                case 0:
                    my_dataset = dset1;
                    dsetname = DSET1_NAME;
                    break;

                case 1:
                    my_dataset = dset2;
                    dsetname = DSET2_NAME;
                    break;

                case 2:
                    my_dataset = dset3;
                    dsetname = DSET3_NAME;
                    break;

                default:
                    HDassert(0 && "Too many datasets!");
            } /* end switch */

            /* Check on dataset's attribute storage status */
            is_empty = H5O_is_attr_empty_test(my_dataset);
            VERIFY(is_empty, TRUE, "H5O_is_attr_empty_test");
            is_dense = H5O_is_attr_dense_test(my_dataset);
            VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

            /* Create attributes, up to limit of compact form */
            for(u = 0; u < max_compact; u++) {
                /* Create attribute */
                sprintf(attrname, "attr %02u", u);
                attr = H5Acreate_by_name(fid, dsetname, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate_by_name");

                /* Write data into the attribute */
                ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
                CHECK(ret, FAIL, "H5Awrite");

                /* Close attribute */
                ret = H5Aclose(attr);
                CHECK(ret, FAIL, "H5Aclose");

                /* Verify information for new attribute */
                ret = attr_info_by_idx_check(my_dataset, attrname, (hsize_t)u, use_index);
                CHECK(ret, FAIL, "attr_info_by_idx_check");
            } /* end for */

            /* Verify state of object */
            ret = H5O_num_attrs_test(my_dataset, &nattrs);
            CHECK(ret, FAIL, "H5O_num_attrs_test");
            VERIFY(nattrs, max_compact, "H5O_num_attrs_test");
            is_empty = H5O_is_attr_empty_test(my_dataset);
            VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
            is_dense = H5O_is_attr_dense_test(my_dataset);
            VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

            /* Test opening attributes stored compactly */
            ret = attr_open_check(fid, dsetname, my_dataset, u);
            CHECK(ret, FAIL, "attr_open_check");
        } /* end for */


        /* Work on all the datasets */
        for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
            switch(curr_dset) {
                case 0:
                    my_dataset = dset1;
                    dsetname = DSET1_NAME;
                    break;

                case 1:
                    my_dataset = dset2;
                    dsetname = DSET2_NAME;
                    break;

                case 2:
                    my_dataset = dset3;
                    dsetname = DSET3_NAME;
                    break;

                default:
                    HDassert(0 && "Too many datasets!");
            } /* end switch */

            /* Create more attributes, to push into dense form */
            for(u = max_compact; u < (max_compact * 2); u++) {
                /* Create attribute */
                sprintf(attrname, "attr %02u", u);
                attr = H5Acreate_by_name(fid, dsetname, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate_by_name");

                /* Write data into the attribute */
                ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
                CHECK(ret, FAIL, "H5Awrite");

                /* Close attribute */
                ret = H5Aclose(attr);
                CHECK(ret, FAIL, "H5Aclose");

                /* Verify state of object */
                if(u >= max_compact) {
                    is_dense = H5O_is_attr_dense_test(my_dataset);
                    VERIFY(is_dense, (new_format ? TRUE : FALSE), "H5O_is_attr_dense_test");
                } /* end if */

                /* Verify information for new attribute */
                ret = attr_info_by_idx_check(my_dataset, attrname, (hsize_t)u, use_index);
                CHECK(ret, FAIL, "attr_info_by_idx_check");
            } /* end for */

            /* Verify state of object */
            ret = H5O_num_attrs_test(my_dataset, &nattrs);
            CHECK(ret, FAIL, "H5O_num_attrs_test");
            VERIFY(nattrs, (max_compact * 2), "H5O_num_attrs_test");
            is_empty = H5O_is_attr_empty_test(my_dataset);
            VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
            is_dense = H5O_is_attr_dense_test(my_dataset);
            VERIFY(is_dense, (new_format ? TRUE : FALSE), "H5O_is_attr_dense_test");

            if(new_format) {
                /* Retrieve & verify # of records in the name & creation order indices */
                ret = H5O_attr_dense_info_test(my_dataset, &name_count, &corder_count);
                CHECK(ret, FAIL, "H5O_attr_dense_info_test");
                if(use_index)
                    VERIFY(name_count, corder_count, "H5O_attr_dense_info_test");
                VERIFY(name_count, (max_compact * 2), "H5O_attr_dense_info_test");
            } /* end if */

            /* Test opening attributes stored compactly */
            ret = attr_open_check(fid, dsetname, my_dataset, u);
            CHECK(ret, FAIL, "attr_open_check");
        } /* end for */

        /* Close Datasets */
        ret = H5Dclose(dset1);
        CHECK(ret, FAIL, "H5Dclose");
        ret = H5Dclose(dset2);
        CHECK(ret, FAIL, "H5Dclose");
        ret = H5Dclose(dset3);
        CHECK(ret, FAIL, "H5Dclose");

        /* Close file */
        ret = H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");
    } /* end for */

    /* Close property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
}   /* test_attr_create_by_name() */

/****************************************************************
**
**  test_attr_shared_write(): Test basic H5A (attribute) code.
**      Tests writing mix of shared & un-shared attributes in "compact" & "dense" storage
**
****************************************************************/
static void
test_attr_shared_write(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* File ID			*/
    hid_t	my_fcpl;	/* File creation property list ID */
    hid_t	dataset, dataset2;	/* Dataset IDs			*/
    hid_t	attr_tid;	/* Attribute's datatype ID	*/
    hid_t	sid, big_sid;	/* Dataspace IDs		*/
    hsize_t	big_dims[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};   /* Dimensions for "big" attribute */
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute */
    unsigned    max_compact;    /* Maximum # of attributes to store compactly */
    unsigned    min_dense;      /* Minimum # of attributes to store "densely" */
    htri_t	is_dense;	/* Are attributes stored densely? */
    htri_t	is_shared;	/* Is attributes shared? */
    hsize_t     shared_refcount;        /* Reference count of shared attribute */
    unsigned    attr_value;     /* Attribute value */
    unsigned	big_value[SPACE1_DIM1 * SPACE1_DIM2 * SPACE1_DIM3];   /* Data for "big" attribute */
    size_t      mesg_count;     /* # of shared messages */
    unsigned    test_shared;    /* Index over shared component type */
    unsigned    u;              /* Local index variable */
    h5_stat_size_t empty_filesize;       /* Size of empty file */
    h5_stat_size_t filesize;             /* Size of file after modifications */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Writing Shared & Unshared Attributes in Compact & Dense Storage\n"));

    /* Initialize "big" attribute data */
    HDmemset(big_value, 1, sizeof(big_value));

    /* Create dataspace for dataset */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create "big" dataspace for "large" attributes */
    big_sid = H5Screate_simple(SPACE1_RANK, big_dims, NULL);
    CHECK(big_sid, FAIL, "H5Screate_simple");

    /* Loop over type of shared components */
    for(test_shared = 0; test_shared < 3; test_shared++) {
        /* Make copy of file creation property list */
        my_fcpl = H5Pcopy(fcpl);
        CHECK(my_fcpl, FAIL, "H5Pcopy");

        /* Set up datatype for attributes */
        attr_tid = H5Tcopy(H5T_NATIVE_UINT);
        CHECK(attr_tid, FAIL, "H5Tcopy");

        /* Special setup for each type of shared components */
        if(test_shared == 0) {
            /* Make attributes > 500 bytes shared */
            ret = H5Pset_shared_mesg_nindexes(my_fcpl, (unsigned)1);
            CHECK_I(ret, "H5Pset_shared_mesg_nindexes");
            ret = H5Pset_shared_mesg_index(my_fcpl, (unsigned)0, H5O_SHMESG_ATTR_FLAG, (unsigned)500);
            CHECK_I(ret, "H5Pset_shared_mesg_index");
        } /* end if */
        else {
            /* Set up copy of file creation property list */

            ret = H5Pset_shared_mesg_nindexes(my_fcpl, (unsigned)3);
            CHECK_I(ret, "H5Pset_shared_mesg_nindexes");

            /* Make attributes > 500 bytes shared */
            ret = H5Pset_shared_mesg_index(my_fcpl, (unsigned)0, H5O_SHMESG_ATTR_FLAG, (unsigned)500);
            CHECK_I(ret, "H5Pset_shared_mesg_index");

            /* Make datatypes & dataspaces > 1 byte shared (i.e. all of them :-) */
            ret = H5Pset_shared_mesg_index(my_fcpl, (unsigned)1, H5O_SHMESG_DTYPE_FLAG, (unsigned)1);
            CHECK_I(ret, "H5Pset_shared_mesg_index");
            ret = H5Pset_shared_mesg_index(my_fcpl, (unsigned)2, H5O_SHMESG_SDSPACE_FLAG, (unsigned)1);
            CHECK_I(ret, "H5Pset_shared_mesg_index");
        } /* end else */

        /* Create file */
        fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, my_fcpl, fapl);
        CHECK(fid, FAIL, "H5Fcreate");

        /* Close FCPL copy */
        ret = H5Pclose(my_fcpl);
        CHECK(ret, FAIL, "H5Pclose");

        /* Close file */
        ret = H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");

        /* Get size of file */
        empty_filesize = h5_get_file_size(FILENAME, fapl);
        if(empty_filesize < 0)
            TestErrPrintf("Line %d: file size wrong!\n", __LINE__);


        /* Re-open file */
        fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
        CHECK(fid, FAIL, "H5Fopen");

        /* Commit datatype to file */
        if(test_shared == 2) {
            ret = H5Tcommit2(fid, TYPE1_NAME, attr_tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            CHECK(ret, FAIL, "H5Tcommit2");
        } /* end if */

        /* Set up to query the object creation properties */
        dcpl = H5Pcreate(H5P_DATASET_CREATE);
        CHECK(dcpl, FAIL, "H5Pcreate");

        /* Create datasets */
        dataset = H5Dcreate2(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        CHECK(dataset, FAIL, "H5Dcreate2");
        dataset2 = H5Dcreate2(fid, DSET2_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        CHECK(dataset2, FAIL, "H5Dcreate2");

        /* Check on dataset's message storage status */
        if(test_shared != 0) {
            /* Datasets' datatypes can be shared */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_DTYPE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 1, "H5F_get_sohm_mesg_count_test");

            /* Datasets' dataspace can be shared */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_SDSPACE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 1, "H5F_get_sohm_mesg_count_test");
        } /* end if */

        /* Retrieve limits for compact/dense attribute storage */
        ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
        CHECK(ret, FAIL, "H5Pget_attr_phase_change");

        /* Close property list */
        ret = H5Pclose(dcpl);
        CHECK(ret, FAIL, "H5Pclose");

        /* Check on datasets' attribute storage status */
        is_dense = H5O_is_attr_dense_test(dataset);
        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
        is_dense = H5O_is_attr_dense_test(dataset2);
        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

        /* Add attributes to each dataset, until after converting to dense storage */
        for(u = 0; u < max_compact * 2; u++) {
            /* Create attribute name */
            sprintf(attrname, "attr %02u", u);

            /* Alternate between creating "small" & "big" attributes */
            if(u % 2) {
                /* Create "small" attribute on first dataset */
                attr = H5Acreate2(dataset, attrname, attr_tid, sid, H5P_DEFAULT, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate2");

                /* Check that attribute is not shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, FALSE, "H5A_is_shared_test");

                /* Write data into the attribute */
                attr_value = u + 1;
                ret = H5Awrite(attr, attr_tid, &attr_value);
                CHECK(ret, FAIL, "H5Awrite");
            } /* end if */
            else {
                /* Create "big" attribute on first dataset */
                attr = H5Acreate2(dataset, attrname, attr_tid, big_sid, H5P_DEFAULT, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate2");

                /* Check that attribute is shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, TRUE, "H5A_is_shared_test");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");

                /* Write data into the attribute */
                big_value[0] = u + 1;
                ret = H5Awrite(attr, attr_tid, big_value);
                CHECK(ret, FAIL, "H5Awrite");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");
            } /* end else */

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");

            /* Check on dataset's attribute storage status */
            is_dense = H5O_is_attr_dense_test(dataset);
            if(u < max_compact)
                VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
            else
                VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");


            /* Alternate between creating "small" & "big" attributes */
            if(u % 2) {
                /* Create "small" attribute on second dataset */
                attr = H5Acreate2(dataset2, attrname, attr_tid, sid, H5P_DEFAULT, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate2");

                /* Check that attribute is not shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, FALSE, "H5A_is_shared_test");

                /* Write data into the attribute */
                attr_value = u + 1;
                ret = H5Awrite(attr, attr_tid, &attr_value);
                CHECK(ret, FAIL, "H5Awrite");
            } /* end if */
            else {
                /* Create "big" attribute on second dataset */
                attr = H5Acreate2(dataset2, attrname, attr_tid, big_sid, H5P_DEFAULT, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate2");

                /* Check that attribute is shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, TRUE, "H5A_is_shared_test");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");

                /* Write data into the attribute */
                big_value[0] = u + 1;
                ret = H5Awrite(attr, attr_tid, big_value);
                CHECK(ret, FAIL, "H5Awrite");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 2, "H5A_get_shared_rc_test");
            } /* end else */

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");

            /* Check on dataset's attribute storage status */
            is_dense = H5O_is_attr_dense_test(dataset2);
            if(u < max_compact)
                VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
            else
                VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");
        } /* end for */

        /* Close attribute's datatype */
        ret = H5Tclose(attr_tid);
        CHECK(ret, FAIL, "H5Tclose");

        /* Close Datasets */
        ret = H5Dclose(dataset);
        CHECK(ret, FAIL, "H5Dclose");
        ret = H5Dclose(dataset2);
        CHECK(ret, FAIL, "H5Dclose");

        /* Check on shared message status now */
        if(test_shared != 0) {
            if(test_shared == 1) {
                /* Check on datatype storage status */
                ret = H5F_get_sohm_mesg_count_test(fid, H5O_DTYPE_ID, &mesg_count);
                CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
                VERIFY(mesg_count, 2, "H5F_get_sohm_mesg_count_test");
            } /* end if */

            /* Check on dataspace storage status */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_SDSPACE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 2, "H5F_get_sohm_mesg_count_test");
        } /* end if */

        /* Unlink datasets with attributes */
        ret = H5Ldelete(fid, DSET1_NAME, H5P_DEFAULT);
        CHECK(ret, FAIL, "H5Ldelete");
        ret = H5Ldelete(fid, DSET2_NAME, H5P_DEFAULT);
        CHECK(ret, FAIL, "H5Ldelete");

        /* Unlink committed datatype */
        if(test_shared == 2) {
            ret = H5Ldelete(fid, TYPE1_NAME, H5P_DEFAULT);
            CHECK(ret, FAIL, "H5Ldelete");
        } /* end if */

        /* Check on attribute storage status */
        ret = H5F_get_sohm_mesg_count_test(fid, H5O_ATTR_ID, &mesg_count);
        CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
        VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");

        if(test_shared != 0) {
            /* Check on datatype storage status */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_DTYPE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");

            /* Check on dataspace storage status */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_SDSPACE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");
        } /* end if */

        /* Close file */
        ret = H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");

        /* Check size of file */
        filesize = h5_get_file_size(FILENAME, fapl);
        VERIFY(filesize, empty_filesize, "h5_get_file_size");
    } /* end for */

    /* Close dataspaces */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(big_sid);
    CHECK(ret, FAIL, "H5Sclose");
}   /* test_attr_shared_write() */

/****************************************************************
**
**  test_attr_shared_rename(): Test basic H5A (attribute) code.
**      Tests renaming shared attributes in "compact" & "dense" storage
**
****************************************************************/
static void
test_attr_shared_rename(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	my_fcpl;	/* File creation property list ID */
    hid_t	dataset, dataset2;	/* Dataset ID2			*/
    hid_t	attr_tid;	/* Attribute's datatype ID	*/
    hid_t	sid, big_sid;	/* Dataspace IDs		*/
    hsize_t	big_dims[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};   /* Dimensions for "big" attribute */
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute on first dataset */
    char	attrname2[NAME_BUF_SIZE];       /* Name of attribute on second dataset */
    unsigned    max_compact;    /* Maximum # of attributes to store compactly */
    unsigned    min_dense;      /* Minimum # of attributes to store "densely" */
    htri_t	is_dense;	/* Are attributes stored densely? */
    htri_t	is_shared;	/* Is attributes shared? */
    hsize_t     shared_refcount;        /* Reference count of shared attribute */
    unsigned    attr_value;     /* Attribute value */
    unsigned	big_value[SPACE1_DIM1 * SPACE1_DIM2 * SPACE1_DIM3];   /* Data for "big" attribute */
    size_t      mesg_count;     /* # of shared messages */
    unsigned    test_shared;    /* Index over shared component type */
    unsigned    u;              /* Local index variable */
    h5_stat_size_t empty_filesize;       /* Size of empty file */
    h5_stat_size_t filesize;             /* Size of file after modifications */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Renaming Shared & Unshared Attributes in Compact & Dense Storage\n"));

    /* Initialize "big" attribute data */
    HDmemset(big_value, 1, sizeof(big_value));

    /* Create dataspace for dataset */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create "big" dataspace for "large" attributes */
    big_sid = H5Screate_simple(SPACE1_RANK, big_dims, NULL);
    CHECK(big_sid, FAIL, "H5Screate_simple");

    /* Loop over type of shared components */
    for(test_shared = 0; test_shared < 3; test_shared++) {
        /* Make copy of file creation property list */
        my_fcpl = H5Pcopy(fcpl);
        CHECK(my_fcpl, FAIL, "H5Pcopy");

        /* Set up datatype for attributes */
        attr_tid = H5Tcopy(H5T_NATIVE_UINT);
        CHECK(attr_tid, FAIL, "H5Tcopy");

        /* Special setup for each type of shared components */
        if(test_shared == 0) {
            /* Make attributes > 500 bytes shared */
            ret = H5Pset_shared_mesg_nindexes(my_fcpl, (unsigned)1);
            CHECK_I(ret, "H5Pset_shared_mesg_nindexes");
            ret = H5Pset_shared_mesg_index(my_fcpl, (unsigned)0, H5O_SHMESG_ATTR_FLAG, (unsigned)500);
            CHECK_I(ret, "H5Pset_shared_mesg_index");
        } /* end if */
        else {
            /* Set up copy of file creation property list */

            ret = H5Pset_shared_mesg_nindexes(my_fcpl, (unsigned)3);
            CHECK_I(ret, "H5Pset_shared_mesg_nindexes");

            /* Make attributes > 500 bytes shared */
            ret = H5Pset_shared_mesg_index(my_fcpl, (unsigned)0, H5O_SHMESG_ATTR_FLAG, (unsigned)500);
            CHECK_I(ret, "H5Pset_shared_mesg_index");

            /* Make datatypes & dataspaces > 1 byte shared (i.e. all of them :-) */
            ret = H5Pset_shared_mesg_index(my_fcpl, (unsigned)1, H5O_SHMESG_DTYPE_FLAG, (unsigned)1);
            CHECK_I(ret, "H5Pset_shared_mesg_index");
            ret = H5Pset_shared_mesg_index(my_fcpl, (unsigned)2, H5O_SHMESG_SDSPACE_FLAG, (unsigned)1);
            CHECK_I(ret, "H5Pset_shared_mesg_index");
        } /* end else */

        /* Create file */
        fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, my_fcpl, fapl);
        CHECK(fid, FAIL, "H5Fcreate");

        /* Close FCPL copy */
        ret = H5Pclose(my_fcpl);
        CHECK(ret, FAIL, "H5Pclose");

        /* Close file */
        ret = H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");

        /* Get size of file */
        empty_filesize = h5_get_file_size(FILENAME, fapl);
        if(empty_filesize < 0)
            TestErrPrintf("Line %d: file size wrong!\n", __LINE__);


        /* Re-open file */
        fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
        CHECK(fid, FAIL, "H5Fopen");

        /* Commit datatype to file */
        if(test_shared == 2) {
            ret = H5Tcommit2(fid, TYPE1_NAME, attr_tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            CHECK(ret, FAIL, "H5Tcommit2");
        } /* end if */

        /* Set up to query the object creation properties */
        dcpl = H5Pcreate(H5P_DATASET_CREATE);
        CHECK(dcpl, FAIL, "H5Pcreate");

        /* Create datasets */
        dataset = H5Dcreate2(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        CHECK(dataset, FAIL, "H5Dcreate2");
        dataset2 = H5Dcreate2(fid, DSET2_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        CHECK(dataset2, FAIL, "H5Dcreate2");

        /* Check on dataset's message storage status */
        if(test_shared != 0) {
            /* Datasets' datatypes can be shared */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_DTYPE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 1, "H5F_get_sohm_mesg_count_test");

            /* Datasets' dataspace can be shared */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_SDSPACE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 1, "H5F_get_sohm_mesg_count_test");
        } /* end if */

        /* Retrieve limits for compact/dense attribute storage */
        ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
        CHECK(ret, FAIL, "H5Pget_attr_phase_change");

        /* Close property list */
        ret = H5Pclose(dcpl);
        CHECK(ret, FAIL, "H5Pclose");

        /* Check on datasets' attribute storage status */
        is_dense = H5O_is_attr_dense_test(dataset);
        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
        is_dense = H5O_is_attr_dense_test(dataset2);
        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

        /* Add attributes to each dataset, until after converting to dense storage */
        for(u = 0; u < max_compact * 2; u++) {
            /* Create attribute name */
            sprintf(attrname, "attr %02u", u);

            /* Alternate between creating "small" & "big" attributes */
            if(u % 2) {
                /* Create "small" attribute on first dataset */
                attr = H5Acreate2(dataset, attrname, attr_tid, sid, H5P_DEFAULT, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate2");

                /* Check that attribute is not shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, FALSE, "H5A_is_shared_test");

                /* Write data into the attribute */
                attr_value = u + 1;
                ret = H5Awrite(attr, attr_tid, &attr_value);
                CHECK(ret, FAIL, "H5Awrite");
            } /* end if */
            else {
                /* Create "big" attribute on first dataset */
                attr = H5Acreate2(dataset, attrname, attr_tid, big_sid, H5P_DEFAULT, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate2");

                /* Check that attribute is shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, TRUE, "H5A_is_shared_test");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");

                /* Write data into the attribute */
                big_value[0] = u + 1;
                ret = H5Awrite(attr, attr_tid, big_value);
                CHECK(ret, FAIL, "H5Awrite");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");
            } /* end else */

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");

            /* Check on dataset's attribute storage status */
            is_dense = H5O_is_attr_dense_test(dataset);
            if(u < max_compact)
                VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
            else
                VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");


            /* Alternate between creating "small" & "big" attributes */
            if(u % 2) {
                /* Create "small" attribute on second dataset */
                attr = H5Acreate2(dataset2, attrname, attr_tid, sid, H5P_DEFAULT, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate2");

                /* Check that attribute is not shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, FALSE, "H5A_is_shared_test");

                /* Write data into the attribute */
                attr_value = u + 1;
                ret = H5Awrite(attr, attr_tid, &attr_value);
                CHECK(ret, FAIL, "H5Awrite");
            } /* end if */
            else {
                /* Create "big" attribute on second dataset */
                attr = H5Acreate2(dataset2, attrname, attr_tid, big_sid, H5P_DEFAULT, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate2");

                /* Check that attribute is shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, TRUE, "H5A_is_shared_test");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");

                /* Write data into the attribute */
                big_value[0] = u + 1;
                ret = H5Awrite(attr, attr_tid, big_value);
                CHECK(ret, FAIL, "H5Awrite");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 2, "H5A_get_shared_rc_test");
            } /* end else */

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");

            /* Check on dataset's attribute storage status */
            is_dense = H5O_is_attr_dense_test(dataset2);
            if(u < max_compact)
                VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
            else
                VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");


            /* Create new attribute name */
            sprintf(attrname2, "new attr %02u", u);

            /* Change second dataset's attribute's name */
            ret = H5Arename_by_name(fid, DSET2_NAME, attrname, attrname2, H5P_DEFAULT);
            CHECK(ret, FAIL, "H5Arename_by_name");


            /* Check refcount on attributes now */

            /* Check refcount on renamed attribute */
            attr = H5Aopen(dataset2, attrname2, H5P_DEFAULT);
            CHECK(attr, FAIL, "H5Aopen");

            if(u % 2) {
                /* Check that attribute is not shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, FALSE, "H5A_is_shared_test");
            } /* end if */
            else {
                /* Check that attribute is shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, TRUE, "H5A_is_shared_test");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");
            } /* end else */

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");

            /* Check refcount on original attribute */
            attr = H5Aopen(dataset, attrname, H5P_DEFAULT);
            CHECK(attr, FAIL, "H5Aopen");

            if(u % 2) {
                /* Check that attribute is not shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, FALSE, "H5A_is_shared_test");
            } /* end if */
            else {
                /* Check that attribute is shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, TRUE, "H5A_is_shared_test");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");
            } /* end else */

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");


            /* Change second dataset's attribute's name back to original */
            ret = H5Arename_by_name(fid, DSET2_NAME, attrname2, attrname, H5P_DEFAULT);
            CHECK(ret, FAIL, "H5Arename_by_name");


            /* Check refcount on attributes now */

            /* Check refcount on renamed attribute */
            attr = H5Aopen(dataset2, attrname, H5P_DEFAULT);
            CHECK(attr, FAIL, "H5Aopen");

            if(u % 2) {
                /* Check that attribute is not shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, FALSE, "H5A_is_shared_test");
            } /* end if */
            else {
                /* Check that attribute is shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, TRUE, "H5A_is_shared_test");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 2, "H5A_get_shared_rc_test");
            } /* end else */

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");

            /* Check refcount on original attribute */
            attr = H5Aopen(dataset, attrname, H5P_DEFAULT);
            CHECK(attr, FAIL, "H5Aopen");

            if(u % 2) {
                /* Check that attribute is not shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, FALSE, "H5A_is_shared_test");
            } /* end if */
            else {
                /* Check that attribute is shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, TRUE, "H5A_is_shared_test");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 2, "H5A_get_shared_rc_test");
            } /* end else */

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");
        } /* end for */


        /* Close attribute's datatype */
        ret = H5Tclose(attr_tid);
        CHECK(ret, FAIL, "H5Tclose");

        /* Close Datasets */
        ret = H5Dclose(dataset);
        CHECK(ret, FAIL, "H5Dclose");
        ret = H5Dclose(dataset2);
        CHECK(ret, FAIL, "H5Dclose");

        /* Check on shared message status now */
        if(test_shared != 0) {
            if(test_shared == 1) {
                /* Check on datatype storage status */
                ret = H5F_get_sohm_mesg_count_test(fid, H5O_DTYPE_ID, &mesg_count);
                CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
                VERIFY(mesg_count, 2, "H5F_get_sohm_mesg_count_test");
            } /* end if */

            /* Check on dataspace storage status */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_SDSPACE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 2, "H5F_get_sohm_mesg_count_test");
        } /* end if */

        /* Unlink datasets with attributes */
        ret = H5Ldelete(fid, DSET1_NAME, H5P_DEFAULT);
        CHECK(ret, FAIL, "HLdelete");
        ret = H5Ldelete(fid, DSET2_NAME, H5P_DEFAULT);
        CHECK(ret, FAIL, "H5Ldelete");

        /* Unlink committed datatype */
        if(test_shared == 2) {
            ret = H5Ldelete(fid, TYPE1_NAME, H5P_DEFAULT);
            CHECK(ret, FAIL, "H5Ldelete");
        } /* end if */

        /* Check on attribute storage status */
        ret = H5F_get_sohm_mesg_count_test(fid, H5O_ATTR_ID, &mesg_count);
        CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
        VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");

        if(test_shared != 0) {
            /* Check on datatype storage status */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_DTYPE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");

            /* Check on dataspace storage status */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_SDSPACE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");
        } /* end if */

        /* Close file */
        ret = H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");

        /* Check size of file */
        filesize = h5_get_file_size(FILENAME, fapl);
        VERIFY(filesize, empty_filesize, "h5_get_file_size");
    } /* end for */

    /* Close dataspaces */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(big_sid);
    CHECK(ret, FAIL, "H5Sclose");
}   /* test_attr_shared_rename() */

/****************************************************************
**
**  test_attr_shared_delete(): Test basic H5A (attribute) code.
**      Tests deleting shared attributes in "compact" & "dense" storage
**
****************************************************************/
static void
test_attr_shared_delete(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* File ID			*/
    hid_t	my_fcpl;	/* File creation property list ID */
    hid_t	dataset, dataset2;	/* Dataset IDs			*/
    hid_t	attr_tid;	/* Attribute's datatype ID	*/
    hid_t	sid, big_sid;	/* Dataspace IDs		*/
    hsize_t	big_dims[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};   /* Dimensions for "big" attribute */
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute on first dataset */
    unsigned    max_compact;    /* Maximum # of attributes to store compactly */
    unsigned    min_dense;      /* Minimum # of attributes to store "densely" */
    htri_t	is_dense;	/* Are attributes stored densely? */
    htri_t	is_shared;	/* Is attributes shared? */
    hsize_t     shared_refcount;        /* Reference count of shared attribute */
    unsigned    attr_value;     /* Attribute value */
    unsigned	big_value[SPACE1_DIM1 * SPACE1_DIM2 * SPACE1_DIM3];   /* Data for "big" attribute */
    size_t      mesg_count;     /* # of shared messages */
    unsigned    test_shared;    /* Index over shared component type */
    unsigned    u;              /* Local index variable */
    h5_stat_size_t empty_filesize;       /* Size of empty file */
    h5_stat_size_t filesize;             /* Size of file after modifications */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Deleting Shared & Unshared Attributes in Compact & Dense Storage\n"));

    /* Initialize "big" attribute data */
    HDmemset(big_value, 1, sizeof(big_value));

    /* Create dataspace for dataset */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create "big" dataspace for "large" attributes */
    big_sid = H5Screate_simple(SPACE1_RANK, big_dims, NULL);
    CHECK(big_sid, FAIL, "H5Screate_simple");

    /* Loop over type of shared components */
    for(test_shared = 0; test_shared < 3; test_shared++) {
        /* Make copy of file creation property list */
        my_fcpl = H5Pcopy(fcpl);
        CHECK(my_fcpl, FAIL, "H5Pcopy");

        /* Set up datatype for attributes */
        attr_tid = H5Tcopy(H5T_NATIVE_UINT);
        CHECK(attr_tid, FAIL, "H5Tcopy");

        /* Special setup for each type of shared components */
        if(test_shared == 0) {
            /* Make attributes > 500 bytes shared */
            ret = H5Pset_shared_mesg_nindexes(my_fcpl, (unsigned)1);
            CHECK_I(ret, "H5Pset_shared_mesg_nindexes");
            ret = H5Pset_shared_mesg_index(my_fcpl, (unsigned)0, H5O_SHMESG_ATTR_FLAG, (unsigned)500);
            CHECK_I(ret, "H5Pset_shared_mesg_index");
        } /* end if */
        else {
            /* Set up copy of file creation property list */

            ret = H5Pset_shared_mesg_nindexes(my_fcpl, (unsigned)3);
            CHECK_I(ret, "H5Pset_shared_mesg_nindexes");

            /* Make attributes > 500 bytes shared */
            ret = H5Pset_shared_mesg_index(my_fcpl, (unsigned)0, H5O_SHMESG_ATTR_FLAG, (unsigned)500);
            CHECK_I(ret, "H5Pset_shared_mesg_index");

            /* Make datatypes & dataspaces > 1 byte shared (i.e. all of them :-) */
            ret = H5Pset_shared_mesg_index(my_fcpl, (unsigned)1, H5O_SHMESG_DTYPE_FLAG, (unsigned)1);
            CHECK_I(ret, "H5Pset_shared_mesg_index");
            ret = H5Pset_shared_mesg_index(my_fcpl, (unsigned)2, H5O_SHMESG_SDSPACE_FLAG, (unsigned)1);
            CHECK_I(ret, "H5Pset_shared_mesg_index");
        } /* end else */

        /* Create file */
        fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, my_fcpl, fapl);
        CHECK(fid, FAIL, "H5Fcreate");

        /* Close FCPL copy */
        ret = H5Pclose(my_fcpl);
        CHECK(ret, FAIL, "H5Pclose");

        /* Close file */
        ret = H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");

        /* Get size of file */
        empty_filesize = h5_get_file_size(FILENAME, fapl);
        if(empty_filesize < 0)
            TestErrPrintf("Line %d: file size wrong!\n", __LINE__);


        /* Re-open file */
        fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
        CHECK(fid, FAIL, "H5Fopen");

        /* Commit datatype to file */
        if(test_shared == 2) {
            ret = H5Tcommit2(fid, TYPE1_NAME, attr_tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            CHECK(ret, FAIL, "H5Tcommit2");
        } /* end if */

        /* Set up to query the object creation properties */
        dcpl = H5Pcreate(H5P_DATASET_CREATE);
        CHECK(dcpl, FAIL, "H5Pcreate");

        /* Create datasets */
        dataset = H5Dcreate2(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        CHECK(dataset, FAIL, "H5Dcreate2");
        dataset2 = H5Dcreate2(fid, DSET2_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        CHECK(dataset2, FAIL, "H5Dcreate2");

        /* Check on dataset's message storage status */
        if(test_shared != 0) {
            /* Datasets' datatypes can be shared */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_DTYPE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 1, "H5F_get_sohm_mesg_count_test");

            /* Datasets' dataspace can be shared */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_SDSPACE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 1, "H5F_get_sohm_mesg_count_test");
        } /* end if */

        /* Retrieve limits for compact/dense attribute storage */
        ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
        CHECK(ret, FAIL, "H5Pget_attr_phase_change");

        /* Close property list */
        ret = H5Pclose(dcpl);
        CHECK(ret, FAIL, "H5Pclose");

        /* Check on datasets' attribute storage status */
        is_dense = H5O_is_attr_dense_test(dataset);
        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
        is_dense = H5O_is_attr_dense_test(dataset2);
        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

        /* Add attributes to each dataset, until after converting to dense storage */
        for(u = 0; u < max_compact * 2; u++) {
            /* Create attribute name */
            sprintf(attrname, "attr %02u", u);

            /* Alternate between creating "small" & "big" attributes */
            if(u % 2) {
                /* Create "small" attribute on first dataset */
                attr = H5Acreate2(dataset, attrname, attr_tid, sid, H5P_DEFAULT, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate2");

                /* Check that attribute is not shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, FALSE, "H5A_is_shared_test");

                /* Write data into the attribute */
                attr_value = u + 1;
                ret = H5Awrite(attr, attr_tid, &attr_value);
                CHECK(ret, FAIL, "H5Awrite");
            } /* end if */
            else {
                /* Create "big" attribute on first dataset */
                attr = H5Acreate2(dataset, attrname, attr_tid, big_sid, H5P_DEFAULT, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate2");

                /* Check that attribute is shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, TRUE, "H5A_is_shared_test");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");

                /* Write data into the attribute */
                big_value[0] = u + 1;
                ret = H5Awrite(attr, attr_tid, big_value);
                CHECK(ret, FAIL, "H5Awrite");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");
            } /* end else */

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");

            /* Check on dataset's attribute storage status */
            is_dense = H5O_is_attr_dense_test(dataset);
            if(u < max_compact)
                VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
            else
                VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");


            /* Alternate between creating "small" & "big" attributes */
            if(u % 2) {
                /* Create "small" attribute on second dataset */
                attr = H5Acreate2(dataset2, attrname, attr_tid, sid, H5P_DEFAULT, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate2");

                /* Check that attribute is not shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, FALSE, "H5A_is_shared_test");

                /* Write data into the attribute */
                attr_value = u + 1;
                ret = H5Awrite(attr, attr_tid, &attr_value);
                CHECK(ret, FAIL, "H5Awrite");
            } /* end if */
            else {
                /* Create "big" attribute on second dataset */
                attr = H5Acreate2(dataset2, attrname, attr_tid, big_sid, H5P_DEFAULT, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate2");

                /* Check that attribute is shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, TRUE, "H5A_is_shared_test");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");

                /* Write data into the attribute */
                big_value[0] = u + 1;
                ret = H5Awrite(attr, attr_tid, big_value);
                CHECK(ret, FAIL, "H5Awrite");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 2, "H5A_get_shared_rc_test");
            } /* end else */

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");

            /* Check on dataset's attribute storage status */
            is_dense = H5O_is_attr_dense_test(dataset2);
            if(u < max_compact)
                VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
            else
                VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");
        } /* end for */


        /* Delete attributes from second dataset */
        for(u = 0; u < max_compact * 2; u++) {
            /* Create attribute name */
            sprintf(attrname, "attr %02u", u);

            /* Delete second dataset's attribute */
            ret = H5Adelete_by_name(fid, DSET2_NAME, attrname, H5P_DEFAULT);
            CHECK(ret, FAIL, "H5Adelete_by_name");


            /* Check refcount on attributes now */

            /* Check refcount on first dataset's attribute */
            attr = H5Aopen(dataset, attrname, H5P_DEFAULT);
            CHECK(attr, FAIL, "H5Aopen");

            if(u % 2) {
                /* Check that attribute is not shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, FALSE, "H5A_is_shared_test");
            } /* end if */
            else {
                /* Check that attribute is shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, TRUE, "H5A_is_shared_test");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");
            } /* end else */

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");
        } /* end for */


        /* Close attribute's datatype */
        ret = H5Tclose(attr_tid);
        CHECK(ret, FAIL, "H5Tclose");

        /* Close Datasets */
        ret = H5Dclose(dataset);
        CHECK(ret, FAIL, "H5Dclose");
        ret = H5Dclose(dataset2);
        CHECK(ret, FAIL, "H5Dclose");

        /* Check on shared message status now */
        if(test_shared != 0) {
            if(test_shared == 1) {
                /* Check on datatype storage status */
                ret = H5F_get_sohm_mesg_count_test(fid, H5O_DTYPE_ID, &mesg_count);
                CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
                VERIFY(mesg_count, 2, "H5F_get_sohm_mesg_count_test");
            } /* end if */

            /* Check on dataspace storage status */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_SDSPACE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 2, "H5F_get_sohm_mesg_count_test");
        } /* end if */

        /* Unlink datasets with attributes */
        ret = H5Ldelete(fid, DSET1_NAME, H5P_DEFAULT);
        CHECK(ret, FAIL, "H5Ldelete");
        ret = H5Ldelete(fid, DSET2_NAME, H5P_DEFAULT);
        CHECK(ret, FAIL, "H5Ldelete");

        /* Unlink committed datatype */
        if(test_shared == 2) {
            ret = H5Ldelete(fid, TYPE1_NAME, H5P_DEFAULT);
            CHECK(ret, FAIL, "H5Ldelete");
        } /* end if */

        /* Check on attribute storage status */
        ret = H5F_get_sohm_mesg_count_test(fid, H5O_ATTR_ID, &mesg_count);
        CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
        VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");

        if(test_shared != 0) {
            /* Check on datatype storage status */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_DTYPE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");

            /* Check on dataspace storage status */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_SDSPACE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");
        } /* end if */

        /* Close file */
        ret = H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");

        /* Check size of file */
        filesize = h5_get_file_size(FILENAME, fapl);
        VERIFY(filesize, empty_filesize, "h5_get_file_size");
    } /* end for */

    /* Close dataspaces */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(big_sid);
    CHECK(ret, FAIL, "H5Sclose");
}   /* test_attr_shared_delete() */

/****************************************************************
**
**  test_attr_shared_unlink(): Test basic H5A (attribute) code.
**      Tests unlinking object with  shared attributes in "compact" & "dense" storage
**
****************************************************************/
static void
test_attr_shared_unlink(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* File ID			*/
    hid_t	my_fcpl;	/* File creation property list ID */
    hid_t	dataset, dataset2;	/* Dataset IDs			*/
    hid_t	attr_tid;	/* Attribute's datatype ID	*/
    hid_t	sid, big_sid;	/* Dataspace IDs		*/
    hsize_t	big_dims[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};   /* Dimensions for "big" attribute */
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute on first dataset */
    unsigned    max_compact;    /* Maximum # of attributes to store compactly */
    unsigned    min_dense;      /* Minimum # of attributes to store "densely" */
    htri_t	is_dense;	/* Are attributes stored densely? */
    htri_t	is_shared;	/* Is attributes shared? */
    hsize_t     shared_refcount;        /* Reference count of shared attribute */
    unsigned    attr_value;     /* Attribute value */
    unsigned	big_value[SPACE1_DIM1 * SPACE1_DIM2 * SPACE1_DIM3];   /* Data for "big" attribute */
    size_t      mesg_count;     /* # of shared messages */
    unsigned    test_shared;    /* Index over shared component type */
    unsigned    u;              /* Local index variable */
    h5_stat_size_t empty_filesize;       /* Size of empty file */
    h5_stat_size_t filesize;             /* Size of file after modifications */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Unlinking Object with Shared Attributes in Compact & Dense Storage\n"));

    /* Initialize "big" attribute data */
    HDmemset(big_value, 1, sizeof(big_value));

    /* Create dataspace for dataset */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create "big" dataspace for "large" attributes */
    big_sid = H5Screate_simple(SPACE1_RANK, big_dims, NULL);
    CHECK(big_sid, FAIL, "H5Screate_simple");

    /* Loop over type of shared components */
    for(test_shared = 0; test_shared < 3; test_shared++) {
        /* Make copy of file creation property list */
        my_fcpl = H5Pcopy(fcpl);
        CHECK(my_fcpl, FAIL, "H5Pcopy");

        /* Set up datatype for attributes */
        attr_tid = H5Tcopy(H5T_NATIVE_UINT);
        CHECK(attr_tid, FAIL, "H5Tcopy");

        /* Special setup for each type of shared components */
        if(test_shared == 0) {
            /* Make attributes > 500 bytes shared */
            ret = H5Pset_shared_mesg_nindexes(my_fcpl, (unsigned)1);
            CHECK_I(ret, "H5Pset_shared_mesg_nindexes");
            ret = H5Pset_shared_mesg_index(my_fcpl, (unsigned)0, H5O_SHMESG_ATTR_FLAG, (unsigned)500);
            CHECK_I(ret, "H5Pset_shared_mesg_index");
        } /* end if */
        else {
            /* Set up copy of file creation property list */

            ret = H5Pset_shared_mesg_nindexes(my_fcpl, (unsigned)3);
            CHECK_I(ret, "H5Pset_shared_mesg_nindexes");

            /* Make attributes > 500 bytes shared */
            ret = H5Pset_shared_mesg_index(my_fcpl, (unsigned)0, H5O_SHMESG_ATTR_FLAG, (unsigned)500);
            CHECK_I(ret, "H5Pset_shared_mesg_index");

            /* Make datatypes & dataspaces > 1 byte shared (i.e. all of them :-) */
            ret = H5Pset_shared_mesg_index(my_fcpl, (unsigned)1, H5O_SHMESG_DTYPE_FLAG, (unsigned)1);
            CHECK_I(ret, "H5Pset_shared_mesg_index");
            ret = H5Pset_shared_mesg_index(my_fcpl, (unsigned)2, H5O_SHMESG_SDSPACE_FLAG, (unsigned)1);
            CHECK_I(ret, "H5Pset_shared_mesg_index");
        } /* end else */

        /* Create file */
        fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, my_fcpl, fapl);
        CHECK(fid, FAIL, "H5Fcreate");

        /* Close FCPL copy */
        ret = H5Pclose(my_fcpl);
        CHECK(ret, FAIL, "H5Pclose");

        /* Close file */
        ret = H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");

        /* Get size of file */
        empty_filesize = h5_get_file_size(FILENAME, fapl);
        if(empty_filesize < 0)
            TestErrPrintf("Line %d: file size wrong!\n", __LINE__);


        /* Re-open file */
        fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
        CHECK(fid, FAIL, "H5Fopen");

        /* Commit datatype to file */
        if(test_shared == 2) {
            ret = H5Tcommit2(fid, TYPE1_NAME, attr_tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            CHECK(ret, FAIL, "H5Tcommit2");
        } /* end if */

        /* Set up to query the object creation properties */
        dcpl = H5Pcreate(H5P_DATASET_CREATE);
        CHECK(dcpl, FAIL, "H5Pcreate");

        /* Create datasets */
        dataset = H5Dcreate2(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        CHECK(dataset, FAIL, "H5Dcreate2");
        dataset2 = H5Dcreate2(fid, DSET2_NAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        CHECK(dataset2, FAIL, "H5Dcreate2");

        /* Check on dataset's message storage status */
        if(test_shared != 0) {
            /* Datasets' datatypes can be shared */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_DTYPE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 1, "H5F_get_sohm_mesg_count_test");

            /* Datasets' dataspace can be shared */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_SDSPACE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 1, "H5F_get_sohm_mesg_count_test");
        } /* end if */

        /* Retrieve limits for compact/dense attribute storage */
        ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
        CHECK(ret, FAIL, "H5Pget_attr_phase_change");

        /* Close property list */
        ret = H5Pclose(dcpl);
        CHECK(ret, FAIL, "H5Pclose");

        /* Check on datasets' attribute storage status */
        is_dense = H5O_is_attr_dense_test(dataset);
        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
        is_dense = H5O_is_attr_dense_test(dataset2);
        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

        /* Add attributes to each dataset, until after converting to dense storage */
        for(u = 0; u < max_compact * 2; u++) {
            /* Create attribute name */
            sprintf(attrname, "attr %02u", u);

            /* Alternate between creating "small" & "big" attributes */
            if(u % 2) {
                /* Create "small" attribute on first dataset */
                attr = H5Acreate2(dataset, attrname, attr_tid, sid, H5P_DEFAULT, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate2");

                /* Check that attribute is not shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, FALSE, "H5A_is_shared_test");

                /* Write data into the attribute */
                attr_value = u + 1;
                ret = H5Awrite(attr, attr_tid, &attr_value);
                CHECK(ret, FAIL, "H5Awrite");
            } /* end if */
            else {
                /* Create "big" attribute on first dataset */
                attr = H5Acreate2(dataset, attrname, attr_tid, big_sid, H5P_DEFAULT, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate2");

                /* ChecFk that attribute is shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, TRUE, "H5A_is_shared_test");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");

                /* Write data into the attribute */
                big_value[0] = u + 1;
                ret = H5Awrite(attr, attr_tid, big_value);
                CHECK(ret, FAIL, "H5Awrite");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");
            } /* end else */

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");

            /* Check on dataset's attribute storage status */
            is_dense = H5O_is_attr_dense_test(dataset);
            if(u < max_compact)
                VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
            else
                VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");


            /* Alternate between creating "small" & "big" attributes */
            if(u % 2) {
                /* Create "small" attribute on second dataset */
                attr = H5Acreate2(dataset2, attrname, attr_tid, sid, H5P_DEFAULT, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate2");

                /* Check that attribute is not shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, FALSE, "H5A_is_shared_test");

                /* Write data into the attribute */
                attr_value = u + 1;
                ret = H5Awrite(attr, attr_tid, &attr_value);
                CHECK(ret, FAIL, "H5Awrite");
            } /* end if */
            else {
                /* Create "big" attribute on second dataset */
                attr = H5Acreate2(dataset2, attrname, attr_tid, big_sid, H5P_DEFAULT, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate2");

                /* Check that attribute is shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, TRUE, "H5A_is_shared_test");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");

                /* Write data into the attribute */
                big_value[0] = u + 1;
                ret = H5Awrite(attr, attr_tid, big_value);
                CHECK(ret, FAIL, "H5Awrite");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 2, "H5A_get_shared_rc_test");
            } /* end else */

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");

            /* Check on dataset's attribute storage status */
            is_dense = H5O_is_attr_dense_test(dataset2);
            if(u < max_compact)
                VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
            else
                VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");
        } /* end for */


        /* Close attribute's datatype */
        ret = H5Tclose(attr_tid);
        CHECK(ret, FAIL, "H5Tclose");

        /* Close second dataset */
        ret = H5Dclose(dataset2);
        CHECK(ret, FAIL, "H5Dclose");

        /* Unlink second dataset */
        ret = H5Ldelete(fid, DSET2_NAME, H5P_DEFAULT);
        CHECK(ret, FAIL, "H5Ldelete");


        /* Check on first dataset's attribute storage status */
        is_dense = H5O_is_attr_dense_test(dataset);
        VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

        /* Check ref count on  attributes of first dataset */
        for(u = 0; u < max_compact * 2; u++) {
            /* Create attribute name */
            sprintf(attrname, "attr %02u", u);

            /* Open attribute on first dataset */
            attr = H5Aopen(dataset, attrname, H5P_DEFAULT);
            CHECK(attr, FAIL, "H5Aopen");

            if(u % 2) {
                /* Check that attribute is not shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, FALSE, "H5A_is_shared_test");
            } /* end if */
            else {
                /* Check that attribute is shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, TRUE, "H5A_is_shared_test");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");
            } /* end else */

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");
        } /* end for */

        /* Close Datasets */
        ret = H5Dclose(dataset);
        CHECK(ret, FAIL, "H5Dclose");

        /* Unlink first dataset */
        ret = H5Ldelete(fid, DSET1_NAME, H5P_DEFAULT);
        CHECK(ret, FAIL, "H5Ldelete");

        /* Unlink committed datatype */
        if(test_shared == 2) {
            ret = H5Ldelete(fid, TYPE1_NAME, H5P_DEFAULT);
            CHECK(ret, FAIL, "H5Ldelete");
        } /* end if */

        /* Check on attribute storage status */
        ret = H5F_get_sohm_mesg_count_test(fid, H5O_ATTR_ID, &mesg_count);
        CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
        VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");

        if(test_shared != 0) {
            /* Check on datatype storage status */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_DTYPE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");

            /* Check on dataspace storage status */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_SDSPACE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");
        } /* end if */

        /* Close file */
        ret = H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");

        /* Check size of file */
        filesize = h5_get_file_size(FILENAME, fapl);
        VERIFY(filesize, empty_filesize, "h5_get_file_size");
    } /* end for */

    /* Close dataspaces */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(big_sid);
    CHECK(ret, FAIL, "H5Sclose");
}   /* test_attr_shared_unlink() */

/****************************************************************
**
**  test_attr_bug1(): Test basic H5A (attribute) code.
**      Tests odd sequence of allocating and deallocating space in the file.
**      The series of actions below constructs a file with an attribute
**      in each object header chunk, except the first.  Then, the attributes
**      are removed and re-created in a way that makes the object header
**      allocation code remove an object header chunk "in the middle" of
**      the sequence of the chunks.
**
****************************************************************/
static void
test_attr_bug1(hid_t fcpl, hid_t fapl)
{
    hid_t fid;          /* File ID */
    hid_t gid;          /* Group ID */
    hid_t aid;          /* Attribute ID */
    hid_t sid;          /* Dataspace ID */
    herr_t ret;         /* Generic return status */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Allocating and De-allocating Attributes in Unusual Way\n"));

    /* Create dataspace ID for attributes */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create main group to operate on */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    gid = H5Gcreate2(fid, GROUP1_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gcreate2");

    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    /* Re-open file and create another group, then attribute on first group */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Create second group */
    gid = H5Gcreate2(fid, GROUP2_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gcreate2");

    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    /* Re-open first group */
    gid = H5Gopen2(fid, GROUP1_NAME, H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gopen2");

    /* Create attribute on first group */
    aid = H5Acreate2(gid, ATTR7_NAME, H5T_NATIVE_DOUBLE, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(aid, FAIL, "H5Acreate2");

    ret = H5Aclose(aid);
    CHECK(ret, FAIL, "H5Aclose");

    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    /* Re-open file and create another group, then another attribute on first group */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Create third group */
    gid = H5Gcreate2(fid, GROUP3_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gcreate2");

    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    /* Unlink second group */
    ret = H5Ldelete(fid, GROUP2_NAME, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Ldelete");

    /* Re-open first group */
    gid = H5Gopen2(fid, GROUP1_NAME, H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gopen2");

    /* Create another attribute on first group */
    aid = H5Acreate2(gid, ATTR8_NAME, H5T_NATIVE_DOUBLE, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(aid, FAIL, "H5Acreate2");

    ret = H5Aclose(aid);
    CHECK(ret, FAIL, "H5Aclose");

    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    /* Re-open file and re-create attributes on first group */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Re-open first group */
    gid = H5Gopen2(fid, GROUP1_NAME, H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gopen2");

    /* Delete first attribute */
    ret = H5Adelete(gid, ATTR7_NAME);
    CHECK(ret, FAIL, "H5Adelete");

    /* Re-create first attribute */
    aid = H5Acreate2(gid, ATTR7_NAME, H5T_NATIVE_DOUBLE, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(aid, FAIL, "H5Acreate2");

    ret = H5Aclose(aid);
    CHECK(ret, FAIL, "H5Aclose");

    /* Delete second attribute */
    ret = H5Adelete(gid, ATTR8_NAME);
    CHECK(ret, FAIL, "H5Adelete");

    /* Re-create second attribute */
    aid = H5Acreate2(gid, ATTR8_NAME, H5T_NATIVE_DOUBLE, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(aid, FAIL, "H5Acreate2");

    ret = H5Aclose(aid);
    CHECK(ret, FAIL, "H5Aclose");

    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    /* Close dataspace ID */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Gclose");
}   /* test_attr_bug1() */

/****************************************************************
**
**  test_attr_bug2(): Test basic H5A (attribute) code.
**      Tests deleting a large number of attributes with the
**      intention of creating a null message with a size that
**      is too large.  This routine deletes every other
**      attribute, but the original bug could also be
**      reproduced by deleting every attribute except a few to
**      keep the chunk open.
**
****************************************************************/
static void
test_attr_bug2(hid_t fcpl, hid_t fapl)
{
    hid_t   fid;            /* File ID */
    hid_t   gid;            /* Group ID */
    hid_t   aid;            /* Attribute ID */
    hid_t   sid;            /* Dataspace ID */
    hid_t   tid;            /* Datatype ID */
    hid_t   gcpl;           /* Group creation property list */
    hsize_t dims[2] = {10, 100}; /* Attribute dimensions */
    char    aname[4];       /* Attribute name */
    unsigned i;             /* index */
    herr_t  ret;            /* Generic return status */
    htri_t  tri_ret;        /* htri_t return status */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Allocating and De-allocating Attributes in Unusual Way\n"));

    /* Create group creation property list */
    gcpl = H5Pcreate(H5P_GROUP_CREATE);
    CHECK(gcpl, FAIL, "H5Pcreate");

    /* Prevent the library from switching to dense attribute storage */
    /* Not doing this with the latest format actually triggers a different bug.
     * This will be tested here as soon as it is fixed.  -NAF
     */
    ret = H5Pset_attr_phase_change (gcpl, BUG2_NATTR+10, BUG2_NATTR+5);
    CHECK(ret, FAIL, "H5Pset_attr_phase_change");

    /* Create dataspace ID for attributes */
    sid = H5Screate_simple(2, dims, NULL);
    CHECK(sid, FAIL, "H5Screate_simple");

    /* Create main group to operate on */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    gid = H5Gcreate2(fid, GROUP1_NAME, H5P_DEFAULT, gcpl, H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gcreate2");

    /* Create attributes on group */
    for (i=0; i<BUG2_NATTR; i++) {
        sprintf(aname, "%03u", i);
        aid = H5Acreate2(gid, aname, H5T_STD_I32LE, sid, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(aid, FAIL, "H5Acreate2");

        ret = H5Aclose(aid);
        CHECK(ret, FAIL, "H5Aclose");
    }

    /* Delete every other attribute */
    for (i=1; i<BUG2_NATTR; i+=2) {
        sprintf(aname, "%03u", i);
        ret = H5Adelete(gid, aname);
        CHECK(ret, FAIL, "H5Adelete");
    }

    /* Close IDs */
    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Reopen file and group */
    fid = H5Fopen(FILENAME, H5F_ACC_RDONLY, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    gid = H5Gopen2(fid, GROUP1_NAME, H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gopen");

    /* Open an attribute in the middle */
    i = (BUG2_NATTR / 4) * 2;
    sprintf(aname, "%03u", i);
    aid = H5Aopen(gid, aname, H5P_DEFAULT);
    CHECK(aid, FAIL, "H5Aopen");

    /* Verify that the attribute has the correct datatype */
    tid = H5Aget_type(aid);
    CHECK(tid, FAIL, "H5Aget_type");

    tri_ret = H5Tequal(tid, H5T_STD_I32LE);
    VERIFY(tri_ret, TRUE, "H5Tequal");

    /* Close IDs */
    ret = H5Tclose(tid);
    CHECK(ret, FAIL, "H5Tclose");

    ret = H5Aclose(aid);
    CHECK(ret, FAIL, "H5Aclose");

    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Now test a variation on this bug - where either the size of chunk 0 goes
     * down a "notch" or two, or chunk 1 becomes completely null at the same
     * time that a null message that is too large is formed */
    dims[0] = 25;
    dims[1] = 41; /* 1025*4 byte attribute size */

    /* Create dataspace ID for attributes */
    sid = H5Screate_simple(2, dims, NULL);
    CHECK(sid, FAIL, "H5Screate_simple");

    /* Create main group to operate on */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    gid = H5Gcreate2(fid, GROUP1_NAME, H5P_DEFAULT, gcpl, H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gcreate2");

    /* Create attributes on group */
    for (i=0; i<BUG2_NATTR2; i++) {
        sprintf(aname, "%03u", i);
        aid = H5Acreate2(gid, aname, H5T_STD_I32LE, sid, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(aid, FAIL, "H5Acreate2");

        ret = H5Aclose(aid);
        CHECK(ret, FAIL, "H5Aclose");
    }

    /* Delete every other attribute */
    for (i=0; i<BUG2_NATTR2; i++) {
        sprintf(aname, "%03u", i);
        ret = H5Adelete(gid, aname);
        CHECK(ret, FAIL, "H5Adelete");
    }

    /* Close IDs */
    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Pclose(gcpl);
    CHECK(ret, FAIL, "H5Pclose");
}   /* test_attr_bug2() */

/****************************************************************
**
**  test_attr_bug3(): Test basic H5A (attribute) code.
**      Tests creating and deleting attributes which use a
**      datatype and/or dataspace stored in the same object
**      header.
**
****************************************************************/
static void
test_attr_bug3(hid_t fcpl, hid_t fapl)
{
    hid_t   fid;            /* File ID */
    hid_t   aid1, aid2;     /* Attribute IDs */
    hid_t   sid1, sid2;     /* Dataspace ID */
    hid_t   tid1, tid2;     /* Datatype IDs */
    hid_t   did;            /* Dataset ID */
    hsize_t dims1[2] = {2, 2},
            dims2[2] = {3, 3}; /* Dimensions */
    int     wdata1[2][2];
    unsigned wdata2[3][3];  /* Write buffers */
    unsigned u, v;          /* Local index variables */
    herr_t  ret;            /* Generic return status */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Attributes in the Same Header as their Datatypes\n"));

    /* Create dataspaces */
    sid1 = H5Screate_simple(2, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");
    sid2 = H5Screate_simple(2, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Create file to operate on */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create datatypes and commit tid1 */
    tid1 = H5Tcopy(H5T_STD_I16BE);
    CHECK(tid1, FAIL, "H5Tcopy");
    tid2 = H5Tcopy(H5T_STD_U64LE);
    CHECK(tid1, FAIL, "H5Tcopy");
    ret = H5Tcommit2(fid, "dtype", tid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");

    /* Create dataset */
    did = H5Dcreate2(fid, "dset", tid2, sid2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dcreate2");

    /* Create attribute on datatype, using that datatype as its datatype */
    aid1 = H5Acreate2(tid1, "attr", tid1, sid1, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(aid1, FAIL, "H5Acreate2");

    /* Create attribute on dataset, using its datatype and dataspace */
    aid2 = H5Acreate2(did, "attr", tid2, sid2, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(aid2, FAIL, "H5Acreate2");

    /* Close attributes */
    ret = H5Aclose(aid1);
    CHECK(ret, FAIL, "H5Aclose");
    ret = H5Aclose(aid2);
    CHECK(ret, FAIL, "H5Aclose");

    /* Reopen attributes */
    aid1 = H5Aopen(tid1, "attr", H5P_DEFAULT);
    CHECK(aid1, FAIL, "H5Aopen");
    aid2 = H5Aopen(did, "attr", H5P_DEFAULT);
    CHECK(aid2, FAIL, "H5Aopen");

    /* Initialize the write buffers */
    for(u = 0; u < dims1[0]; u++)
        for(v = 0; v < dims1[1]; v++)
            wdata1[u][v] = (int)((u * dims1[1]) + v);
    for(u = 0; u < dims2[0]; u++)
        for(v = 0; v < dims2[1]; v++)
            wdata2[u][v] = (int)((u * dims2[1]) + v);

    /* Write data to the attributes */
    ret = H5Awrite(aid1, H5T_NATIVE_INT, wdata1);
    CHECK(ret, FAIL, "H5Awrite");
    ret = H5Awrite(aid2, H5T_NATIVE_UINT, wdata2);
    CHECK(ret, FAIL, "H5Awrite");

    /* Close attributes */
    ret = H5Aclose(aid1);
    CHECK(ret, FAIL, "H5Aclose");
    ret = H5Aclose(aid2);
    CHECK(ret, FAIL, "H5Aclose");

    /* Delete attributes */
    ret = H5Adelete(tid1, "attr");
    CHECK(ret, FAIL, "H5Adelete");
    ret = H5Adelete(did, "attr");
    CHECK(ret, FAIL, "H5Adelete");

    /* Recreate attributes */
    aid1 = H5Acreate2(tid1, "attr", tid1, sid1, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(aid1, FAIL, "H5Acreate2");
    aid2 = H5Acreate2(did, "attr", tid2, sid2, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(aid2, FAIL, "H5Acreate2");

    /* Delete attributes (note they are still open) */
    ret = H5Adelete(tid1, "attr");
    CHECK(ret, FAIL, "H5Adelete");
    ret = H5Adelete(did, "attr");
    CHECK(ret, FAIL, "H5Adelete");

    /* Close dataspaces and transient datatype */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Tclose(tid2);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close dataset and committed datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    /* Delete dataset and committed datatype */
    ret = H5Ldelete(fid, "dtype", H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Ldelete(fid, "dset", H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close attributes */
    ret = H5Aclose(aid1);
    CHECK(ret, FAIL, "H5Aclose");
    ret = H5Aclose(aid2);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_bug3() */

/****************************************************************
**
**  test_attr_bug4(): Test basic H5A (attribute) code.
**      Attempts to trigger a bug which would result in being
**      unable to add an attribute to a named datatype.  This
**      happened when an object header chunk was too small to
**      hold a continuation message and could not be extended.
**
****************************************************************/
static void
test_attr_bug4(hid_t fcpl, hid_t fapl)
{
    hid_t   fid;            /* File ID */
    hid_t   gid;            /* Group ID */
    hid_t   aid1, aid2, aid3; /* Attribute IDs */
    hid_t   sid;            /* Dataspace ID */
    hid_t   tid;            /* Datatype ID */
    hid_t   did;            /* Dataset ID */
    hsize_t dims[1] = {5};  /* Attribute dimensions */
    herr_t  ret;            /* Generic return status */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing that attributes can always be added to named datatypes\n"));

    /* Create dataspace */
    sid = H5Screate_simple(1, dims, NULL);
    CHECK(sid, FAIL, "H5Screate_simple");

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Open root group */
    gid = H5Gopen2(fid, "/", H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gcreate2");

    /* Create committed datatype */
    tid = H5Tcopy(H5T_STD_I32LE);
    CHECK(tid, FAIL, "H5Tcopy");
    ret = H5Tcommit2(fid, "dtype", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");

    /* Create dataset */
    did = H5Dcreate2(fid, "dset", tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dcreate2");

    /* Create attributes on group and dataset */
    aid1 = H5Acreate2(gid, "attr", tid, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(aid1, FAIL, "H5Acreate2");
    aid2 = H5Acreate2(did, "attr", tid, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(aid2, FAIL, "H5Acreate2");

    /* Create attribute on datatype (this is the main test) */
    aid3 = H5Acreate2(tid, "attr", tid, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(aid3, FAIL, "H5Acreate2");

    /* Close IDs */
    ret = H5Aclose(aid3);
    CHECK(ret, FAIL, "H5Aclose");

    ret = H5Aclose(aid2);
    CHECK(ret, FAIL, "H5Aclose");

    ret = H5Aclose(aid1);
    CHECK(ret, FAIL, "H5Aclose");

    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    ret = H5Tclose(tid);
    CHECK(ret, FAIL, "H5Tclose");

    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
}   /* test_attr_bug4() */

/****************************************************************
**
**  test_attr_bug5(): Test basic H5A (attribute) code.
**      Tests opening an attribute multiple times through
**      objects opened through different file handles.
**
****************************************************************/
static void
test_attr_bug5(hid_t fcpl, hid_t fapl)
{
    hid_t   fid1, fid2;     /* File IDs */
    hid_t   gid1, gid2;     /* Group IDs */
    hid_t   did1, did2;     /* Dataset IDs */
    hid_t   tid1, tid2;     /* Datatype IDs */
    hid_t   aidg1, aidg2,
            aidd1, aidd2,
            aidt1, aidt2;   /* Attribute IDs */
    hid_t   sid;            /* Dataspace ID */
    hsize_t dims[1] = {5};  /* Attribute dimensions */
    herr_t  ret;            /* Generic return status */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Opening an Attribute Through Multiple Files Concurrently\n"));

    /* Create dataspace ID for attributes and datasets */
    sid = H5Screate_simple(1, dims, NULL);
    CHECK(sid, FAIL, "H5Screate_simple");

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Open root group */
    gid1 = H5Gopen2(fid1, "/", H5P_DEFAULT);
    CHECK(gid1, FAIL, "H5Gopen2");

    /* Create and commit datatype */
    tid1 = H5Tcopy(H5T_STD_I32LE);
    CHECK(tid1, FAIL, "H5Tcopy");
    ret = H5Tcommit2(fid1, BUG3_DT_NAME, tid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");

    /* Create dataset */
    did1 = H5Dcreate2(fid1, BUG3_DSET_NAME, tid1, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(did1, FAIL, "H5Dcreate2");

    /* Create attribute on root group */
    aidg1 = H5Acreate2(gid1, BUG3_ATTR_NAME, tid1, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(aidg1, FAIL, "H5Acreate2");

    /* Create attribute on dataset */
    aidd1 = H5Acreate2(did1, BUG3_ATTR_NAME, tid1, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(aidd1, FAIL, "H5Acreate2");

    /* Create attribute on datatype */
    aidt1 = H5Acreate2(tid1, BUG3_ATTR_NAME, tid1, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(aidt1, FAIL, "H5Acreate2");

    /* Close all IDs */
    ret = H5Aclose(aidt1);
    CHECK(ret, FAIL, "H5Aclose");
    ret = H5Aclose(aidd1);
    CHECK(ret, FAIL, "H5Aclose");
    ret = H5Aclose(aidg1);
    CHECK(ret, FAIL, "H5Aclose");
    ret = H5Dclose(did1);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Gclose(gid1);
    CHECK(ret, FAIL, "H5Gclose");
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Open file twice */
    fid1 = H5Fopen(FILENAME, H5F_ACC_RDONLY, fapl);
    CHECK(fid1, FAIL, "H5Fopen");
    fid2 = H5Fopen(FILENAME, H5F_ACC_RDONLY, fapl);
    CHECK(fid2, FAIL, "H5Fopen");

    /* Open the root group twice */
    gid1 = H5Gopen2(fid1, "/", H5P_DEFAULT);
    CHECK(gid1, FAIL, "H5Gopen2");
    gid2 = H5Gopen2(fid2, "/", H5P_DEFAULT);
    CHECK(gid2, FAIL, "H5Gopen2");

    /* Open the root group attribute twice */
    aidg1 = H5Aopen(gid1, BUG3_ATTR_NAME, H5P_DEFAULT);
    CHECK(aidg1, FAIL, "H5Aopen");
    aidg2 = H5Aopen(gid2, BUG3_ATTR_NAME, H5P_DEFAULT);
    CHECK(aidg1, FAIL, "H5Aopen");

    /* Open the dataset twice */
    did1 = H5Dopen2(fid1, BUG3_DSET_NAME, H5P_DEFAULT);
    CHECK(did1, FAIL, "H5Dopen2");
    did2 = H5Dopen2(fid2, BUG3_DSET_NAME, H5P_DEFAULT);
    CHECK(did2, FAIL, "H5Dopen2");

    /* Open the dataset attribute twice */
    aidd1 = H5Aopen(did1, BUG3_ATTR_NAME, H5P_DEFAULT);
    CHECK(aidd1, FAIL, "H5Aopen");
    aidd2 = H5Aopen(did2, BUG3_ATTR_NAME, H5P_DEFAULT);
    CHECK(aidd1, FAIL, "H5Aopen");

    /* Open the datatype twice */
    tid1 = H5Topen2(fid1, BUG3_DT_NAME, H5P_DEFAULT);
    CHECK(tid1, FAIL, "H5Topen2");
    tid2 = H5Topen2(fid2, BUG3_DT_NAME, H5P_DEFAULT);
    CHECK(tid2, FAIL, "H5Topen2");

    /* Open the datatype attribute twice */
    aidt1 = H5Aopen(tid1, BUG3_ATTR_NAME, H5P_DEFAULT);
    CHECK(aidt1, FAIL, "H5Aopen");
    aidt2 = H5Aopen(tid2, BUG3_ATTR_NAME, H5P_DEFAULT);
    CHECK(aidt2, FAIL, "H5Aopen");

    /* Close all attributes */
    ret = H5Aclose(aidg1);
    CHECK(ret, FAIL, "H5Aclose");
    ret = H5Aclose(aidg2);
    CHECK(ret, FAIL, "H5Aclose");
    ret = H5Aclose(aidd1);
    CHECK(ret, FAIL, "H5Aclose");
    ret = H5Aclose(aidd2);
    CHECK(ret, FAIL, "H5Aclose");
    ret = H5Aclose(aidt1);
    CHECK(ret, FAIL, "H5Aclose");
    ret = H5Aclose(aidt2);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close root groups */
    ret = H5Gclose(gid1);
    CHECK(ret, FAIL, "H5Gclose");
    ret = H5Gclose(gid2);
    CHECK(ret, FAIL, "H5Gclose");

    /* Close datasets */
    ret = H5Dclose(did1);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Dclose(did2);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close datatypes */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Tclose(tid2);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close files */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
    ret = H5Fclose(fid2);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_bug5() */

/****************************************************************
**
**  test_attr_bug6(): Test basic H5A (attribute) code.
**      Tests if reading an empty attribute is OK.
**
****************************************************************/
static void
test_attr_bug6(hid_t fcpl, hid_t fapl)
{
    hid_t   fid;            /* File ID */
    hid_t   gid;            /* Group ID */
    hid_t   aid1, aid2;     /* Attribute IDs */
    hid_t   sid;            /* Dataspace ID */
    hsize_t dims[ATTR1_RANK] = {ATTR1_DIM1};  /* Attribute dimensions */
    int     intar[ATTR1_DIM1];       /* Data reading buffer */
    herr_t  ret;            /* Generic return status */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing that empty attribute can be read\n"));

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Open root group */
    gid = H5Gopen2(fid, "/", H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gopen2");

    /* Create dataspace */
    sid = H5Screate_simple(1, dims, NULL);
    CHECK(sid, FAIL, "H5Screate_simple");

    /* Create attribute on group */
    aid1 = H5Acreate2(gid, ATTR1_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(aid1, FAIL, "H5Acreate2");

    ret = H5Aclose(aid1);
    CHECK(ret, FAIL, "H5Aclose");

    /* Open the attribute again */
    aid2 = H5Aopen(gid, ATTR1_NAME, H5P_DEFAULT);
    CHECK(aid2, FAIL, "H5Aopen");

    ret = H5Aread(aid2, H5T_NATIVE_INT, intar);
    CHECK(ret, FAIL, "H5Aread");

    /* Close IDs */
    ret = H5Aclose(aid2);
    CHECK(ret, FAIL, "H5Aclose");

    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
}

/****************************************************************
**
**  test_attr(): Main H5A (attribute) testing routine.
**
****************************************************************/
void
test_attr(void)
{
    hid_t	fapl = (-1), fapl2 = (-1);    /* File access property lists */
    hid_t	fcpl = (-1), fcpl2 = (-1);    /* File creation property lists */
    hbool_t new_format;         /* Whether to use the new format or not */
    hbool_t use_shared;         /* Whether to use shared attributes or not */
    herr_t ret;                 /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Attributes\n"));

    /* Create a default file access property list */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");

    /* Copy the file access property list */
    fapl2 = H5Pcopy(fapl);
    CHECK(fapl2, FAIL, "H5Pcopy");

    /* Set the "use the latest version of the format" bounds for creating objects in the file */
    ret = H5Pset_libver_bounds(fapl2, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST);
    CHECK(ret, FAIL, "H5Pset_libver_bounds");

    /* Create a default file creation property list */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    CHECK(fcpl, FAIL, "H5Pcreate");

    /* Copy the file creation property list */
    fcpl2 = H5Pcopy(fcpl);
    CHECK(fcpl2, FAIL, "H5Pcopy");

    /* Make attributes > 1 byte shared (i.e. all of them :-) */
    ret = H5Pset_shared_mesg_nindexes(fcpl2, (unsigned)1);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");
    ret = H5Pset_shared_mesg_index(fcpl2, (unsigned)0, H5O_SHMESG_ATTR_FLAG, (unsigned)1);
    CHECK_I(ret, "H5Pset_shared_mesg_index");

    /* Loop over using new group format */
    for(new_format = FALSE; new_format <= TRUE; new_format++) {
        hid_t my_fapl;

        /* Set the FAPL for the type of format */
        if(new_format) {
            MESSAGE(7, ("testing with new file format\n"));
            my_fapl = fapl2;
        } /* end if */
        else {
            MESSAGE(7, ("testing with old file format\n"));
            my_fapl = fapl;
        } /* end else */

        /* These next two tests use the same file information */
        test_attr_basic_write(my_fapl);    /* Test basic H5A writing code */
        test_attr_basic_read(my_fapl);     /* Test basic H5A reading code */

        /* These next two tests use their own file information */
        test_attr_flush(my_fapl);          /* Test H5A I/O in the presence of H5Fflush calls */
        test_attr_plist(my_fapl);          /* Test attribute property lists */

        /* These next two tests use the same file information */
        test_attr_compound_write(my_fapl);  /* Test complex datatype H5A writing code */
        test_attr_compound_read(my_fapl);   /* Test complex datatype H5A reading code */

        /* These next two tests use the same file information */
        test_attr_scalar_write(my_fapl);  /* Test scalar dataspace H5A writing code */
        test_attr_scalar_read(my_fapl);   /* Test scalar dataspace H5A reading code */

        /* These next four tests use the same file information */
        test_attr_mult_write(my_fapl);     /* Test H5A writing code for multiple attributes */
        test_attr_mult_read(my_fapl);      /* Test H5A reading code for multiple attributes */
        test_attr_iterate(my_fapl);        /* Test H5A iterator code */
        test_attr_delete(my_fapl);         /* Test H5A code for deleting attributes */

        /* This next test uses its own file information */
        test_attr_dtype_shared(my_fapl);   /* Test using shared dataypes in attributes */

        /* This next test uses its own file information */
        test_attr_duplicate_ids(my_fapl);

        /* Tests on "new format" attribute storage */
        if(new_format == TRUE) {
            /* Loop over using shared attributes */
            for(use_shared = FALSE; use_shared <= TRUE; use_shared++) {
                hid_t my_fcpl;

                /* Set the FCPL for shared or not */
                if(use_shared) {
                    MESSAGE(7, ("testing with shared attributes\n"));
                    my_fcpl = fcpl2;
                } /* end if */
                else {
                    MESSAGE(7, ("testing without shared attributes\n"));
                    my_fcpl = fcpl;
                } /* end else */

                /* General attribute tests */
                test_attr_dense_create(my_fcpl, my_fapl);       /* Test dense attribute storage creation */
                test_attr_dense_open(my_fcpl, my_fapl);         /* Test opening attributes in dense storage */
                test_attr_dense_delete(my_fcpl, my_fapl);       /* Test deleting attributes in dense storage */
                test_attr_dense_rename(my_fcpl, my_fapl);       /* Test renaming attributes in dense storage */
                test_attr_dense_unlink(my_fcpl, my_fapl);       /* Test unlinking object with attributes in dense storage */
                test_attr_dense_limits(my_fcpl, my_fapl);       /* Test dense attribute storage limits */
                test_attr_dense_dup_ids(my_fcpl, my_fapl);      /* Test duplicated IDs for dense attribute storage */

                test_attr_big(my_fcpl, my_fapl);                /* Test storing big attribute */
                test_attr_null_space(my_fcpl, my_fapl);         /* Test storing attribute with NULL dataspace */
                test_attr_deprec(fcpl, my_fapl);                /* Test deprecated API routines */
                test_attr_many(new_format, my_fcpl, my_fapl);               /* Test storing lots of attributes */

                /* Attribute creation order tests */
                test_attr_corder_create_basic(my_fcpl, my_fapl);/* Test creating an object w/attribute creation order info */
                test_attr_corder_create_compact(my_fcpl, my_fapl);  /* Test compact attribute storage on an object w/attribute creation order info */
                test_attr_corder_create_dense(my_fcpl, my_fapl);/* Test dense attribute storage on an object w/attribute creation order info */
                test_attr_corder_create_reopen(my_fcpl, my_fapl);/* Test creating attributes w/reopening file from using new format to using old format */
                test_attr_corder_transition(my_fcpl, my_fapl);  /* Test attribute storage transitions on an object w/attribute creation order info */
                test_attr_corder_delete(my_fcpl, my_fapl);      /* Test deleting object using dense storage w/attribute creation order info */

                /* New attribute API routine tests */
                test_attr_info_by_idx(new_format, my_fcpl, my_fapl);    /* Test querying attribute info by index */
                test_attr_delete_by_idx(new_format, my_fcpl, my_fapl);  /* Test deleting attribute by index */
                test_attr_iterate2(new_format, my_fcpl, my_fapl);       /* Test iterating over attributes by index */
                test_attr_open_by_idx(new_format, my_fcpl, my_fapl);    /* Test opening attributes by index */
                test_attr_open_by_name(new_format, my_fcpl, my_fapl);   /* Test opening attributes by name */
                test_attr_create_by_name(new_format, my_fcpl, my_fapl); /* Test creating attributes by name */

                /* More complex tests with both "new format" and "shared" attributes */
                if(use_shared == TRUE) {
                    test_attr_shared_write(my_fcpl, my_fapl);   /* Test writing to shared attributes in compact & dense storage */
                    test_attr_shared_rename(my_fcpl, my_fapl);  /* Test renaming shared attributes in compact & dense storage */
                    test_attr_shared_delete(my_fcpl, my_fapl);  /* Test deleting shared attributes in compact & dense storage */
                    test_attr_shared_unlink(my_fcpl, my_fapl);  /* Test unlinking object with shared attributes in compact & dense storage */
                } /* end if */

                /* Tests that address specific bugs */
                test_attr_bug1(my_fcpl, my_fapl);               /* Test odd allocation operations */
                test_attr_bug2(my_fcpl, my_fapl);               /* Test many deleted attributes */
                test_attr_bug3(my_fcpl, my_fapl);               /* Test "self referential" attributes */
                test_attr_bug4(my_fcpl, my_fapl);               /* Test attributes on named datatypes */
                test_attr_bug5(my_fcpl, my_fapl);               /* Test opening/closing attributes through different file handles */
                test_attr_bug6(my_fcpl, my_fapl);               /* Test reading empty attribute */
            } /* end for */
        } /* end if */
        else {
            /* General attribute tests */
            test_attr_big(fcpl, my_fapl);                       /* Test storing big attribute */
            test_attr_null_space(fcpl, my_fapl);                /* Test storing attribute with NULL dataspace */
            test_attr_deprec(fcpl, my_fapl);                    /* Test deprecated API routines */
            test_attr_many(new_format, fcpl, my_fapl);               /* Test storing lots of attributes */

            /* New attribute API routine tests, on old-format storage */
            test_attr_info_by_idx(new_format, fcpl, my_fapl);   /* Test querying attribute info by index */
            test_attr_delete_by_idx(new_format, fcpl, my_fapl); /* Test deleting attribute by index */
            test_attr_iterate2(new_format, fcpl, my_fapl);      /* Test iterating over attributes by index */
            test_attr_open_by_idx(new_format, fcpl, my_fapl);   /* Test opening attributes by index */
            test_attr_open_by_name(new_format, fcpl, my_fapl);  /* Test opening attributes by name */
            test_attr_create_by_name(new_format, fcpl, my_fapl); /* Test creating attributes by name */

            /* Tests that address specific bugs */
            test_attr_bug1(fcpl, my_fapl);                      /* Test odd allocation operations */
            test_attr_bug2(fcpl, my_fapl);                      /* Test many deleted attributes */
            test_attr_bug3(fcpl, my_fapl);                      /* Test "self referential" attributes */
            test_attr_bug4(fcpl, my_fapl);                      /* Test attributes on named datatypes */
            test_attr_bug5(fcpl, my_fapl);                      /* Test opening/closing attributes through different file handles */
            test_attr_bug6(fcpl, my_fapl);                      /* Test reading empty attribute */
        } /* end else */
    } /* end for */

    /* Close  FCPLs */
    ret = H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(fcpl2);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close  FAPLs */
    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(fapl2);
    CHECK(ret, FAIL, "H5Pclose");
}   /* test_attr() */


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
void
cleanup_attr(void)
{
    remove(FILENAME);
}

