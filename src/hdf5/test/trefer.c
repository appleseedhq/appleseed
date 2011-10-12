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
* Test program:	 trefer
*
* Test the Reference functionality
*
*************************************************************/

#include "testhdf5.h"

#include "hdf5.h"

#define FILE1   "trefer1.h5"
#define FILE2	"trefer2.h5"
#define FILE3	"trefer3.h5"

/* 1-D dataset with fixed dimensions */
#define SPACE1_RANK	1
#define SPACE1_DIM1	4

/* 2-D dataset with fixed dimensions */
#define SPACE2_RANK	2
#define SPACE2_DIM1	10
#define SPACE2_DIM2	10

/* Larger 1-D dataset with fixed dimensions */
#define SPACE3_RANK	1
#define SPACE3_DIM1	100

/* Element selection information */
#define POINT1_NPOINTS 10

/* Compound datatype */
typedef struct s1_t {
    unsigned int a;
    unsigned int b;
    float c;
} s1_t;

#define GROUPNAME       "/group"
#define GROUPNAME2      "group2"
#define GROUPNAME3      "group3"
#define DSETNAME        "/dset"
#define DSETNAME2       "dset2"
#define NAME_SIZE       16


/****************************************************************
**
**  test_reference_params(): Test basic H5R (reference) parameters
**                           for correct processing
**
****************************************************************/
static void
test_reference_params(void)
{
    hid_t       fid1;       /* HDF5 File IDs        */
    hid_t       dataset,    /* Dataset ID           */
                dset2;      /* Dereferenced dataset ID */
    hid_t       group;      /* Group ID             */
    hid_t       sid1;       /* Dataspace ID         */
    hid_t       tid1;       /* Datatype ID          */
    hsize_t     dims1[] = {SPACE1_DIM1};
    hobj_ref_t *wbuf,       /* buffer to write to disk */
               *rbuf,       /* buffer read from disk */
               *tbuf;       /* temp. buffer read from disk */
    unsigned   *tu32;       /* Temporary pointer to uint32 data */
    int         i;          /* counting variables */
    const char *write_comment = "Foo!"; /* Comments for group */
    herr_t      ret;        /* Generic return value     */
    size_t      name_size;  /* Size of reference name */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Reference Parameters\n"));

    /* Allocate write & read buffers */
    wbuf = (hobj_ref_t *)malloc(MAX(sizeof(unsigned), sizeof(hobj_ref_t)) * SPACE1_DIM1);
    rbuf = (hobj_ref_t *)malloc(MAX(sizeof(unsigned), sizeof(hobj_ref_t)) * SPACE1_DIM1);
    tbuf = (hobj_ref_t *)malloc(MAX(sizeof(unsigned), sizeof(hobj_ref_t)) * SPACE1_DIM1);

    /* Create file */
    fid1 = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a group */
    group = H5Gcreate2(fid1, "Group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(group, FAIL, "H5Gcreate2");

    /* Set group's comment */
    ret = H5Oset_comment(group, write_comment);
    CHECK(ret, FAIL, "H5Oset_comment");

    /* Create a dataset (inside Group1) */
    dataset = H5Dcreate2(group, "Dataset1", H5T_NATIVE_UINT, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    for(tu32 = (unsigned *)wbuf, i = 0; i < SPACE1_DIM1; i++)
        *tu32++=i*3;

    /* Write selection to disk */
    ret = H5Dwrite(dataset, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create another dataset (inside Group1) */
    dataset = H5Dcreate2(group, "Dataset2", H5T_NATIVE_UCHAR, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create a datatype to refer to */
    tid1 = H5Tcreate(H5T_COMPOUND, sizeof(s1_t));
    CHECK(tid1, FAIL, "H5Tcreate");

    /* Insert fields */
    ret = H5Tinsert(tid1, "a", HOFFSET(s1_t,a), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    ret = H5Tinsert(tid1, "b", HOFFSET(s1_t,b), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    ret = H5Tinsert(tid1, "c", HOFFSET(s1_t,c), H5T_NATIVE_FLOAT);
    CHECK(ret, FAIL, "H5Tinsert");

    /* Save datatype for later */
    ret = H5Tcommit2(group, "Datatype1", tid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");

    /* Close datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close group */
    ret = H5Gclose(group);
    CHECK(ret, FAIL, "H5Gclose");

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, "Dataset3", H5T_STD_REF_OBJ, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Dcreate2");

    /* Test parameters to H5Rcreate */
    ret = H5Rcreate(NULL, fid1, "/Group1/Dataset1", H5R_OBJECT, -1);
    VERIFY(ret, FAIL, "H5Rcreate ref");
    ret = H5Rcreate(&wbuf[0], -1, "/Group1/Dataset1", H5R_OBJECT, -1);
    VERIFY(ret, FAIL, "H5Rcreate loc_id");
    ret = H5Rcreate(&wbuf[0], fid1, NULL, H5R_OBJECT, -1);
    VERIFY(ret, FAIL, "H5Rcreate name");
    ret = H5Rcreate(&wbuf[0], fid1, "", H5R_OBJECT, -1);
    VERIFY(ret, FAIL, "H5Rcreate null name");
    ret = H5Rcreate(&wbuf[0], fid1, "/Group1/Dataset1", H5R_MAXTYPE, -1);
    VERIFY(ret, FAIL, "H5Rcreate type");
    ret = H5Rcreate(&wbuf[0], fid1, "/Group1/Dataset1", H5R_DATASET_REGION, -1);
    VERIFY(ret, FAIL, "H5Rcreate region space");
    ret = H5Rcreate(&wbuf[0], fid1, "/Group1/Dataset1", H5R_MAXTYPE, 0);
    VERIFY(ret, FAIL, "H5Rcreate space");

    /* Test parameters to H5Rdereference */
    dset2 = H5Rdereference(-1, H5R_OBJECT, &rbuf[0]);
    VERIFY(dset2, FAIL, "H5Rdereference loc_id");
    dset2 = H5Rdereference(dataset, H5R_OBJECT, NULL);
    VERIFY(dset2, FAIL, "H5Rdereference ref");
    dset2 = H5Rdereference(dataset, H5R_MAXTYPE, &rbuf[0]);
    VERIFY(dset2, FAIL, "H5Rdereference type");

    /* Test parameters to H5Rget_obj_type2 */
    ret = H5Rget_obj_type2(-1, H5R_OBJECT, &rbuf[0], NULL);
    VERIFY(ret, FAIL, "H5Rget_obj_type2 loc_id");
    ret = H5Rget_obj_type2(fid1, H5R_OBJECT, NULL, NULL);
    VERIFY(ret, FAIL, "H5Rget_obj_type2 ref");
    ret = H5Rget_obj_type2(fid1, H5R_MAXTYPE, &rbuf[0], NULL);
    VERIFY(ret, FAIL, "H5Rget_obj_type2 type");

    /* Test parameters to H5Rget_name */
    name_size = H5Rget_name(-1, H5R_DATASET_REGION, &rbuf[0], NULL, 0);
    VERIFY(name_size, FAIL, "H5Rget_name loc_id");
    name_size = H5Rget_name(fid1, H5R_DATASET_REGION, NULL, NULL, 0);
    VERIFY(ret, FAIL, "H5Rget_name ref");
    name_size = H5Rget_name(fid1, H5R_MAXTYPE, &rbuf[0], NULL, 0);
    VERIFY(ret, FAIL, "H5Rget_name type");

    /* Test parameters to H5Rget_region */
    ret = H5Rget_region(-1, H5R_OBJECT, &rbuf[0]);
    VERIFY(ret, FAIL, "H5Rget_region loc_id");
    ret = H5Rget_region(fid1, H5R_OBJECT, NULL);
    VERIFY(ret, FAIL, "H5Rget_region ref");
    ret = H5Rget_region(fid1, H5R_OBJECT, &rbuf[0]);
    VERIFY(ret, FAIL, "H5Rget_region type");

    /* Close disk dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Free memory buffers */
    free(wbuf);
    free(rbuf);
    free(tbuf);
}   /* test_reference_obj() */

/****************************************************************
**
**  test_reference_obj(): Test basic H5R (reference) object reference code.
**      Tests references to various kinds of objects
**
****************************************************************/
static void
test_reference_obj(void)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset,	/* Dataset ID			*/
                dset2;      /* Dereferenced dataset ID */
    hid_t		group;      /* Group ID             */
    hid_t		sid1;       /* Dataspace ID			*/
    hid_t		tid1;       /* Datatype ID			*/
    hsize_t		dims1[] = {SPACE1_DIM1};
    hobj_ref_t      *wbuf,      /* buffer to write to disk */
               *rbuf,       /* buffer read from disk */
               *tbuf;       /* temp. buffer read from disk */
    hobj_ref_t  nvrbuf[3]={0,101,1000000000}; /* buffer with non-valid refs */
    unsigned      *tu32;      /* Temporary pointer to uint32 data */
    int        i, j;          /* counting variables */
    const char *write_comment="Foo!"; /* Comments for group */
    char read_comment[10];
    H5O_type_t          obj_type;       /* Object type */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Object Reference Functions\n"));

    /* Allocate write & read buffers */
    wbuf = (hobj_ref_t *)malloc(MAX(sizeof(unsigned), sizeof(hobj_ref_t)) * SPACE1_DIM1);
    rbuf = (hobj_ref_t *)malloc(MAX(sizeof(unsigned), sizeof(hobj_ref_t)) * SPACE1_DIM1);
    tbuf = (hobj_ref_t *)malloc(MAX(sizeof(unsigned), sizeof(hobj_ref_t)) * SPACE1_DIM1);

    /* Create file */
    fid1 = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a group */
    group = H5Gcreate2(fid1, "Group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(group, FAIL, "H5Gcreate2");

    /* Set group's comment */
    ret = H5Oset_comment(group, write_comment);
    CHECK(ret, FAIL, "H5Oset_comment");

    /* Create a dataset (inside Group1) */
    dataset = H5Dcreate2(group, "Dataset1", H5T_NATIVE_UINT, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    for(tu32 = (unsigned *)wbuf, i = 0; i < SPACE1_DIM1; i++)
        *tu32++=i*3;

    /* Write selection to disk */
    ret = H5Dwrite(dataset, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create another dataset (inside Group1) */
    dataset = H5Dcreate2(group, "Dataset2", H5T_NATIVE_UCHAR, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create a datatype to refer to */
    tid1 = H5Tcreate(H5T_COMPOUND, sizeof(s1_t));
    CHECK(tid1, FAIL, "H5Tcreate");

    /* Insert fields */
    ret = H5Tinsert(tid1, "a", HOFFSET(s1_t,a), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    ret = H5Tinsert(tid1, "b", HOFFSET(s1_t,b), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    ret = H5Tinsert(tid1, "c", HOFFSET(s1_t,c), H5T_NATIVE_FLOAT);
    CHECK(ret, FAIL, "H5Tinsert");

    /* Save datatype for later */
    ret = H5Tcommit2(group, "Datatype1", tid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");

    /* Close datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close group */
    ret = H5Gclose(group);
    CHECK(ret, FAIL, "H5Gclose");

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, "Dataset3", H5T_STD_REF_OBJ, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Dcreate2");

    /* Create reference to dataset */
    ret = H5Rcreate(&wbuf[0], fid1, "/Group1/Dataset1", H5R_OBJECT, -1);
    CHECK(ret, FAIL, "H5Rcreate");
    ret = H5Rget_obj_type2(dataset, H5R_OBJECT, &wbuf[0], &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type2");
    VERIFY(obj_type, H5O_TYPE_DATASET, "H5Rget_obj_type2");

    /* Create reference to dataset */
    ret = H5Rcreate(&wbuf[1], fid1, "/Group1/Dataset2", H5R_OBJECT, -1);
    CHECK(ret, FAIL, "H5Rcreate");
    ret = H5Rget_obj_type2(dataset, H5R_OBJECT, &wbuf[1], &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type2");
    VERIFY(obj_type, H5O_TYPE_DATASET, "H5Rget_obj_type2");

    /* Create reference to group */
    ret = H5Rcreate(&wbuf[2], fid1, "/Group1", H5R_OBJECT, -1);
    CHECK(ret, FAIL, "H5Rcreate");
    ret = H5Rget_obj_type2(dataset, H5R_OBJECT, &wbuf[2], &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type2");
    VERIFY(obj_type, H5O_TYPE_GROUP, "H5Rget_obj_type2");

    /* Create reference to named datatype */
    ret = H5Rcreate(&wbuf[3], fid1, "/Group1/Datatype1", H5R_OBJECT, -1);
    CHECK(ret, FAIL, "H5Rcreate");
    ret = H5Rget_obj_type2(dataset, H5R_OBJECT, &wbuf[3], &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type2");
    VERIFY(obj_type, H5O_TYPE_NAMED_DATATYPE, "H5Rget_obj_type2");

    /* Write selection to disk */
    ret = H5Dwrite(dataset, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close disk dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open the file */
    fid1 = H5Fopen(FILE1, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Open the dataset */
    dataset = H5Dopen2(fid1, "/Dataset3", H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Dopen2");

    /* Read selection from disk */
    ret = H5Dread(dataset, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Open dataset object */
    dset2 = H5Rdereference(dataset, H5R_OBJECT, &rbuf[0]);
    CHECK(dset2, FAIL, "H5Rdereference");

    /* Check information in referenced dataset */
    sid1 = H5Dget_space(dset2);
    CHECK(sid1, FAIL, "H5Dget_space");

    ret = (int)H5Sget_simple_extent_npoints(sid1);
    VERIFY(ret, 4, "H5Sget_simple_extent_npoints");

    /* Read from disk */
    ret = H5Dread(dset2, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL, H5P_DEFAULT, tbuf);
    CHECK(ret, FAIL, "H5Dread");

    for(tu32 = (unsigned *)tbuf, i = 0; i < SPACE1_DIM1; i++, tu32++)
        VERIFY(*tu32, (uint32_t)(i*3), "Data");

    /* Close dereferenced Dataset */
    ret = H5Dclose(dset2);
    CHECK(ret, FAIL, "H5Dclose");

    /* Open group object */
    group = H5Rdereference(dataset, H5R_OBJECT, &rbuf[2]);
    CHECK(group, FAIL, "H5Rdereference");

    /* Get group's comment */
    ret = H5Oget_comment(group, read_comment, (size_t)10);
    CHECK(ret, FAIL, "H5Oget_comment");

    /* Check for correct comment value */
    if(HDstrcmp(write_comment, read_comment) != 0)
        TestErrPrintf("Error! Incorrect group comment, wanted: %s, got: %s\n",write_comment,read_comment);

    /* Close group */
    ret = H5Gclose(group);
    CHECK(ret, FAIL, "H5Gclose");

    /* Open datatype object */
    tid1 = H5Rdereference(dataset, H5R_OBJECT, &rbuf[3]);
    CHECK(tid1, FAIL, "H5Rdereference");

    /* Verify correct datatype */
    {
        H5T_class_t tclass;

        tclass = H5Tget_class(tid1);
        VERIFY(tclass, H5T_COMPOUND, "H5Tget_class");

        ret= H5Tget_nmembers(tid1);
        VERIFY(ret, 3, "H5Tget_nmembers");
    }

    /* Attempting to retrieve type of object using non-valid refs */
    for(j = 0; j < 3; j++) {
        H5E_BEGIN_TRY {
            ret = H5Rget_obj_type2(dataset, H5R_OBJECT, &nvrbuf[j], &obj_type);
        } H5E_END_TRY;
        VERIFY(ret, FAIL, "H5Rget_obj_type2");
    } /* end for */

    /* Close datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Free memory buffers */
    free(wbuf);
    free(rbuf);
    free(tbuf);
}   /* test_reference_obj() */

/****************************************************************
**
**  test_reference_region(): Test basic H5R (reference) object reference code.
**      Tests references to various kinds of objects
**
****************************************************************/
static void
test_reference_region(void)
{
    hid_t	fid1;		/* HDF5 File IDs		*/
    hid_t	dset1,		/* Dataset ID			*/
                dset2;		/* Dereferenced dataset ID */
    hid_t	sid1,		/* Dataspace ID	#1		*/
                sid2;		/* Dataspace ID	#2		*/
    hsize_t	dims1[] = {SPACE1_DIM1},
            	dims2[] = {SPACE2_DIM1, SPACE2_DIM2};
    hsize_t	start[SPACE2_RANK];     /* Starting location of hyperslab */
    hsize_t	stride[SPACE2_RANK];    /* Stride of hyperslab */
    hsize_t	count[SPACE2_RANK];     /* Element count of hyperslab */
    hsize_t	block[SPACE2_RANK];     /* Block size of hyperslab */
    hsize_t	coord1[POINT1_NPOINTS][SPACE2_RANK]; /* Coordinates for point selection */
    hsize_t *   coords;			/* Coordinate buffer */
    hsize_t	low[SPACE2_RANK];	/* Selection bounds */
    hsize_t	high[SPACE2_RANK];	/* Selection bounds */
    hdset_reg_ref_t      *wbuf,		/* buffer to write to disk */
               *rbuf;       /* buffer read from disk */
    hdset_reg_ref_t  nvrbuf[3]={0,101,1000000000}; /* buffer with non-valid refs */
    uint8_t    *dwbuf,      /* Buffer for writing numeric data to disk */
               *drbuf;      /* Buffer for reading numeric data from disk */
    uint8_t    *tu8;        /* Temporary pointer to uint8 data */
    H5O_type_t  obj_type;       /* Type of object */
    int        i, j;           /* counting variables */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Dataset Region Reference Functions\n"));

    /* Allocate write & read buffers */
    wbuf = (hdset_reg_ref_t *)HDcalloc(sizeof(hdset_reg_ref_t), (size_t)SPACE1_DIM1);
    rbuf = (hdset_reg_ref_t *)HDmalloc(sizeof(hdset_reg_ref_t) * SPACE1_DIM1);
    dwbuf = (uint8_t *)HDmalloc(sizeof(uint8_t) * SPACE2_DIM1 * SPACE2_DIM2);
    drbuf = (uint8_t *)HDcalloc(sizeof(uint8_t), (size_t)(SPACE2_DIM1 * SPACE2_DIM2));

    /* Create file */
    fid1 = H5Fcreate(FILE2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for datasets */
    sid2 = H5Screate_simple(SPACE2_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Create a dataset */
    dset2 = H5Dcreate2(fid1, "Dataset2", H5T_STD_U8LE, sid2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dset2, FAIL, "H5Dcreate2");

    for(tu8 = dwbuf, i = 0; i < (SPACE2_DIM1 * SPACE2_DIM2); i++)
        *tu8++ = i * 3;

    /* Write selection to disk */
    ret = H5Dwrite(dset2, H5T_STD_U8LE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dwbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close Dataset */
    ret = H5Dclose(dset2);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create dataspace for the reference dataset */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a dataset */
    dset1 = H5Dcreate2(fid1, "Dataset1", H5T_STD_REF_DSETREG, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Dcreate2");

    /* Create references */

    /* Select 6x6 hyperslab for first reference */
    start[0] = 2; start[1] = 2;
    stride[0] = 1; stride[1] = 1;
    count[0] = 1; count[1] = 1;
    block[0] = 6; block[1] = 6;
    ret = H5Sselect_hyperslab(sid2, H5S_SELECT_SET, start, stride, count, block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    ret = (int)H5Sget_select_npoints(sid2);
    VERIFY(ret, 36, "H5Sget_select_npoints");

    /* Store first dataset region */
    ret = H5Rcreate(&wbuf[0], fid1, "/Dataset2", H5R_DATASET_REGION, sid2);
    CHECK(ret, FAIL, "H5Rcreate");
    ret = H5Rget_obj_type2(dset1, H5R_DATASET_REGION, &wbuf[0], &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type2");
    VERIFY(obj_type, H5O_TYPE_DATASET, "H5Rget_obj_type2");

    /* Select sequence of ten points for second reference */
    coord1[0][0] = 6; coord1[0][1] = 9;
    coord1[1][0] = 2; coord1[1][1] = 2;
    coord1[2][0] = 8; coord1[2][1] = 4;
    coord1[3][0] = 1; coord1[3][1] = 6;
    coord1[4][0] = 2; coord1[4][1] = 8;
    coord1[5][0] = 3; coord1[5][1] = 2;
    coord1[6][0] = 0; coord1[6][1] = 4;
    coord1[7][0] = 9; coord1[7][1] = 0;
    coord1[8][0] = 7; coord1[8][1] = 1;
    coord1[9][0] = 3; coord1[9][1] = 3;
    ret = H5Sselect_elements(sid2, H5S_SELECT_SET, (size_t)POINT1_NPOINTS, (const hsize_t *)coord1);
    CHECK(ret, FAIL, "H5Sselect_elements");

    ret = (int)H5Sget_select_npoints(sid2);
    VERIFY(ret, 10, "H5Sget_select_npoints");

    /* Store second dataset region */
    ret = H5Rcreate(&wbuf[1], fid1, "/Dataset2", H5R_DATASET_REGION, sid2);
    CHECK(ret, FAIL, "H5Rcreate");

    /* Write selection to disk */
    ret = H5Dwrite(dset1, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close disk dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Dataset */
    ret = H5Dclose(dset1);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close uint8 dataset dataspace */
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open the file */
    fid1 = H5Fopen(FILE2, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Open the dataset */
    dset1 = H5Dopen2(fid1, "/Dataset1", H5P_DEFAULT);
    CHECK(dset1, FAIL, "H5Dopen2");

    /* Read selection from disk */
    ret = H5Dread(dset1, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Try to open objects */
    dset2 = H5Rdereference(dset1, H5R_DATASET_REGION, &rbuf[0]);
    CHECK(dset2, FAIL, "H5Rdereference");

    /* Check what H5Rget_obj_type2 function returns */
    ret = H5Rget_obj_type2(dset1, H5R_DATASET_REGION, &rbuf[0], &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type2");
    VERIFY(obj_type, H5O_TYPE_DATASET, "H5Rget_obj_type2");

    /* Check information in referenced dataset */
    sid1 = H5Dget_space(dset2);
    CHECK(sid1, FAIL, "H5Dget_space");

    ret = (int)H5Sget_simple_extent_npoints(sid1);
    VERIFY(ret, 100, "H5Sget_simple_extent_npoints");

    /* Read from disk */
    ret = H5Dread(dset2, H5T_STD_U8LE, H5S_ALL, H5S_ALL, H5P_DEFAULT, drbuf);
    CHECK(ret, FAIL, "H5Dread");

    for(tu8 = (uint8_t *)drbuf, i = 0; i < (SPACE2_DIM1 * SPACE2_DIM2); i++, tu8++)
        VERIFY(*tu8, (uint8_t)(i * 3), "Data");

    /* Get the hyperslab selection */
    sid2 = H5Rget_region(dset1, H5R_DATASET_REGION, &rbuf[0]);
    CHECK(sid2, FAIL, "H5Rget_region");

    /* Verify correct hyperslab selected */
    ret = (int)H5Sget_select_npoints(sid2);
    VERIFY(ret, 36, "H5Sget_select_npoints");
    ret = (int)H5Sget_select_hyper_nblocks(sid2);
    VERIFY(ret, 1, "H5Sget_select_hyper_nblocks");
    coords = HDmalloc(ret * SPACE2_RANK * sizeof(hsize_t) * 2); /* allocate space for the hyperslab blocks */
    ret = H5Sget_select_hyper_blocklist(sid2, (hsize_t)0, (hsize_t)ret, coords);
    CHECK(ret, FAIL, "H5Sget_select_hyper_blocklist");
    VERIFY(coords[0], 2, "Hyperslab Coordinates");
    VERIFY(coords[1], 2, "Hyperslab Coordinates");
    VERIFY(coords[2], 7, "Hyperslab Coordinates");
    VERIFY(coords[3], 7, "Hyperslab Coordinates");
    HDfree(coords);
    ret = H5Sget_select_bounds(sid2, low, high);
    CHECK(ret, FAIL, "H5Sget_select_bounds");
    VERIFY(low[0], 2, "Selection Bounds");
    VERIFY(low[1], 2, "Selection Bounds");
    VERIFY(high[0], 7, "Selection Bounds");
    VERIFY(high[1], 7, "Selection Bounds");

    /* Close region space */
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");

    /* Get the element selection */
    sid2 = H5Rget_region(dset1, H5R_DATASET_REGION, &rbuf[1]);
    CHECK(sid2, FAIL, "H5Rget_region");

    /* Verify correct elements selected */
    ret = (int)H5Sget_select_npoints(sid2);
    VERIFY(ret, 10, "H5Sget_select_npoints");
    ret = (int)H5Sget_select_elem_npoints(sid2);
    VERIFY(ret, 10, "H5Sget_select_elem_npoints");
    coords = HDmalloc(ret * SPACE2_RANK * sizeof(hsize_t)); /* allocate space for the element points */
    ret = H5Sget_select_elem_pointlist(sid2, (hsize_t)0, (hsize_t)ret, coords);
    CHECK(ret, FAIL, "H5Sget_select_elem_pointlist");
    VERIFY(coords[0], coord1[0][0], "Element Coordinates");
    VERIFY(coords[1], coord1[0][1], "Element Coordinates");
    VERIFY(coords[2], coord1[1][0], "Element Coordinates");
    VERIFY(coords[3], coord1[1][1], "Element Coordinates");
    VERIFY(coords[4], coord1[2][0], "Element Coordinates");
    VERIFY(coords[5], coord1[2][1], "Element Coordinates");
    VERIFY(coords[6], coord1[3][0], "Element Coordinates");
    VERIFY(coords[7], coord1[3][1], "Element Coordinates");
    VERIFY(coords[8], coord1[4][0], "Element Coordinates");
    VERIFY(coords[9], coord1[4][1], "Element Coordinates");
    VERIFY(coords[10], coord1[5][0], "Element Coordinates");
    VERIFY(coords[11], coord1[5][1], "Element Coordinates");
    VERIFY(coords[12], coord1[6][0], "Element Coordinates");
    VERIFY(coords[13], coord1[6][1], "Element Coordinates");
    VERIFY(coords[14], coord1[7][0], "Element Coordinates");
    VERIFY(coords[15], coord1[7][1], "Element Coordinates");
    VERIFY(coords[16], coord1[8][0], "Element Coordinates");
    VERIFY(coords[17], coord1[8][1], "Element Coordinates");
    VERIFY(coords[18], coord1[9][0], "Element Coordinates");
    VERIFY(coords[19], coord1[9][1], "Element Coordinates");
    HDfree(coords);
    ret = H5Sget_select_bounds(sid2, low, high);
    CHECK(ret, FAIL, "H5Sget_select_bounds");
    VERIFY(low[0], 0, "Selection Bounds");
    VERIFY(low[1], 0, "Selection Bounds");
    VERIFY(high[0], 9, "Selection Bounds");
    VERIFY(high[1], 9, "Selection Bounds");

    /* Close region space */
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close first space */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close dereferenced Dataset */
    ret = H5Dclose(dset2);
    CHECK(ret, FAIL, "H5Dclose");

    /* Attempting to retrieve type of object using non-valid refs */
    for(j = 0; j < 3; j++) {
        H5E_BEGIN_TRY {
            ret = H5Rget_obj_type2(dset1, H5R_DATASET_REGION, &nvrbuf[j], &obj_type);
        } H5E_END_TRY;
        VERIFY(ret, FAIL, "H5Rget_obj_type2");
    } /* end for */

    /* Close Dataset */
    ret = H5Dclose(dset1);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Free memory buffers */
    free(wbuf);
    free(rbuf);
    free(dwbuf);
    free(drbuf);
}   /* test_reference_region() */

/****************************************************************
**
**  test_reference_region_1D(): Test H5R (reference) object reference code.
**      Tests 1-D references to various kinds of objects
**
****************************************************************/
static void
test_reference_region_1D(void)
{
    hid_t	fid1;           /* HDF5 File IDs		*/
    hid_t	dset1,          /* Dataset ID			*/
                dset3;          /* Dereferenced dataset ID */
    hid_t	sid1,           /* Dataspace ID	#1		*/
                sid3;           /* Dataspace ID	#3		*/
    hsize_t	dims1[] = {SPACE1_DIM1},
            	dims3[] = {SPACE3_DIM1};
    hsize_t	start[SPACE3_RANK];     /* Starting location of hyperslab */
    hsize_t	stride[SPACE3_RANK];    /* Stride of hyperslab */
    hsize_t	count[SPACE3_RANK];     /* Element count of hyperslab */
    hsize_t	block[SPACE3_RANK];     /* Block size of hyperslab */
    hsize_t	coord1[POINT1_NPOINTS][SPACE3_RANK]; /* Coordinates for point selection */
    hsize_t *   coords;                 /* Coordinate buffer */
    hsize_t	low[SPACE3_RANK];       /* Selection bounds */
    hsize_t	high[SPACE3_RANK];      /* Selection bounds */
    hdset_reg_ref_t      *wbuf,         /* buffer to write to disk */
               *rbuf;   /* buffer read from disk */
    uint8_t    *dwbuf,  /* Buffer for writing numeric data to disk */
               *drbuf;  /* Buffer for reading numeric data from disk */
    uint8_t    *tu8;    /* Temporary pointer to uint8 data */
    H5O_type_t  obj_type;       /* Object type */
    int         i;      /* counting variables */
    herr_t	ret;    /* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing 1-D Dataset Region Reference Functions\n"));

    /* Allocate write & read buffers */
    wbuf = (hdset_reg_ref_t *)HDcalloc(sizeof(hdset_reg_ref_t), (size_t)SPACE1_DIM1);
    rbuf = (hdset_reg_ref_t *)HDmalloc(sizeof(hdset_reg_ref_t) * SPACE1_DIM1);
    dwbuf = (uint8_t *)HDmalloc(sizeof(uint8_t) * SPACE3_DIM1);
    drbuf = (uint8_t *)HDcalloc(sizeof(uint8_t), (size_t)SPACE3_DIM1);

    /* Create file */
    fid1 = H5Fcreate(FILE2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for datasets */
    sid3 = H5Screate_simple(SPACE3_RANK, dims3, NULL);
    CHECK(sid3, FAIL, "H5Screate_simple");

    /* Create a dataset */
    dset3 = H5Dcreate2(fid1, "Dataset2", H5T_STD_U8LE, sid3, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dset3, FAIL, "H5Dcreate2");

    for(tu8 = dwbuf, i = 0; i < SPACE3_DIM1; i++)
        *tu8++ = i * 3;

    /* Write selection to disk */
    ret = H5Dwrite(dset3, H5T_STD_U8LE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dwbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close Dataset */
    ret = H5Dclose(dset3);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create dataspace for the reference dataset */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a dataset */
    dset1 = H5Dcreate2(fid1, "Dataset1", H5T_STD_REF_DSETREG, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Dcreate2");

    /* Create references */

    /* Select 15 2x1 hyperslabs for first reference */
    start[0] = 2;
    stride[0] = 5;
    count[0] = 15;
    block[0] = 2;
    ret = H5Sselect_hyperslab(sid3, H5S_SELECT_SET, start, stride, count, block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    ret = (int)H5Sget_select_npoints(sid3);
    VERIFY(ret, 30, "H5Sget_select_npoints");

    /* Store first dataset region */
    ret = H5Rcreate(&wbuf[0], fid1, "/Dataset2", H5R_DATASET_REGION, sid3);
    CHECK(ret, FAIL, "H5Rcreate");
    ret = H5Rget_obj_type2(dset1, H5R_DATASET_REGION, &wbuf[0], &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type2");
    VERIFY(obj_type, H5O_TYPE_DATASET, "H5Rget_obj_type2");

    /* Select sequence of ten points for second reference */
    coord1[0][0] = 16;
    coord1[1][0] = 22;
    coord1[2][0] = 38;
    coord1[3][0] = 41;
    coord1[4][0] = 52;
    coord1[5][0] = 63;
    coord1[6][0] = 70;
    coord1[7][0] = 89;
    coord1[8][0] = 97;
    coord1[9][0] = 03;
    ret = H5Sselect_elements(sid3, H5S_SELECT_SET, (size_t)POINT1_NPOINTS, (const hsize_t *)coord1);
    CHECK(ret, FAIL, "H5Sselect_elements");

    ret = (int)H5Sget_select_npoints(sid3);
    VERIFY(ret, 10, "H5Sget_select_npoints");

    /* Store second dataset region */
    ret = H5Rcreate(&wbuf[1], fid1, "/Dataset2", H5R_DATASET_REGION, sid3);
    CHECK(ret, FAIL, "H5Rcreate");

    /* Write selection to disk */
    ret = H5Dwrite(dset1, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close disk dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Dataset */
    ret = H5Dclose(dset1);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close uint8 dataset dataspace */
    ret = H5Sclose(sid3);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open the file */
    fid1 = H5Fopen(FILE2, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Open the dataset */
    dset1 = H5Dopen2(fid1, "/Dataset1", H5P_DEFAULT);
    CHECK(dset1, FAIL, "H5Dopen2");

    /* Read selection from disk */
    ret = H5Dread(dset1, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Try to open objects */
    dset3 = H5Rdereference(dset1, H5R_DATASET_REGION, &rbuf[0]);
    CHECK(dset3, FAIL, "H5Rdereference");

    /* Check what H5Rget_obj_type2 function returns */
    ret = H5Rget_obj_type2(dset1, H5R_DATASET_REGION, &rbuf[0], &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type2");
    VERIFY(obj_type, H5O_TYPE_DATASET, "H5Rget_obj_type2");

    /* Check information in referenced dataset */
    sid1 = H5Dget_space(dset3);
    CHECK(sid1, FAIL, "H5Dget_space");

    ret = (int)H5Sget_simple_extent_npoints(sid1);
    VERIFY(ret, 100, "H5Sget_simple_extent_npoints");

    /* Read from disk */
    ret = H5Dread(dset3, H5T_STD_U8LE, H5S_ALL, H5S_ALL, H5P_DEFAULT, drbuf);
    CHECK(ret, FAIL, "H5Dread");

    for(tu8 = (uint8_t *)drbuf, i = 0; i < SPACE3_DIM1; i++, tu8++)
        VERIFY(*tu8, (uint8_t)(i * 3), "Data");

    /* Get the hyperslab selection */
    sid3 = H5Rget_region(dset1, H5R_DATASET_REGION, &rbuf[0]);
    CHECK(sid3, FAIL, "H5Rget_region");

    /* Verify correct hyperslab selected */
    ret = (int)H5Sget_select_npoints(sid3);
    VERIFY(ret, 30, "H5Sget_select_npoints");
    ret = (int)H5Sget_select_hyper_nblocks(sid3);
    VERIFY(ret, 15, "H5Sget_select_hyper_nblocks");
    coords = (hsize_t *)HDmalloc(ret * SPACE3_RANK * sizeof(hsize_t) * 2); /* allocate space for the hyperslab blocks */
    ret = H5Sget_select_hyper_blocklist(sid3, (hsize_t)0, (hsize_t)ret, coords);
    CHECK(ret, FAIL, "H5Sget_select_hyper_blocklist");
    VERIFY(coords[0],   2, "Hyperslab Coordinates");
    VERIFY(coords[1],   3, "Hyperslab Coordinates");
    VERIFY(coords[2],   7, "Hyperslab Coordinates");
    VERIFY(coords[3],   8, "Hyperslab Coordinates");
    VERIFY(coords[4],  12, "Hyperslab Coordinates");
    VERIFY(coords[5],  13, "Hyperslab Coordinates");
    VERIFY(coords[6],  17, "Hyperslab Coordinates");
    VERIFY(coords[7],  18, "Hyperslab Coordinates");
    VERIFY(coords[8],  22, "Hyperslab Coordinates");
    VERIFY(coords[9],  23, "Hyperslab Coordinates");
    VERIFY(coords[10], 27, "Hyperslab Coordinates");
    VERIFY(coords[11], 28, "Hyperslab Coordinates");
    VERIFY(coords[12], 32, "Hyperslab Coordinates");
    VERIFY(coords[13], 33, "Hyperslab Coordinates");
    VERIFY(coords[14], 37, "Hyperslab Coordinates");
    VERIFY(coords[15], 38, "Hyperslab Coordinates");
    VERIFY(coords[16], 42, "Hyperslab Coordinates");
    VERIFY(coords[17], 43, "Hyperslab Coordinates");
    VERIFY(coords[18], 47, "Hyperslab Coordinates");
    VERIFY(coords[19], 48, "Hyperslab Coordinates");
    VERIFY(coords[20], 52, "Hyperslab Coordinates");
    VERIFY(coords[21], 53, "Hyperslab Coordinates");
    VERIFY(coords[22], 57, "Hyperslab Coordinates");
    VERIFY(coords[23], 58, "Hyperslab Coordinates");
    VERIFY(coords[24], 62, "Hyperslab Coordinates");
    VERIFY(coords[25], 63, "Hyperslab Coordinates");
    VERIFY(coords[26], 67, "Hyperslab Coordinates");
    VERIFY(coords[27], 68, "Hyperslab Coordinates");
    VERIFY(coords[28], 72, "Hyperslab Coordinates");
    VERIFY(coords[29], 73, "Hyperslab Coordinates");
    HDfree(coords);
    ret = H5Sget_select_bounds(sid3, low, high);
    CHECK(ret, FAIL, "H5Sget_select_bounds");
    VERIFY(low[0], 2, "Selection Bounds");
    VERIFY(high[0], 73, "Selection Bounds");

    /* Close region space */
    ret = H5Sclose(sid3);
    CHECK(ret, FAIL, "H5Sclose");

    /* Get the element selection */
    sid3 = H5Rget_region(dset1, H5R_DATASET_REGION, &rbuf[1]);
    CHECK(sid3, FAIL, "H5Rget_region");

    /* Verify correct elements selected */
    ret = (int)H5Sget_select_npoints(sid3);
    VERIFY(ret, 10, "H5Sget_select_npoints");
    ret = (int)H5Sget_select_elem_npoints(sid3);
    VERIFY(ret, 10, "H5Sget_select_elem_npoints");
    coords = (hsize_t *)HDmalloc(ret * SPACE3_RANK * sizeof(hsize_t)); /* allocate space for the element points */
    ret = H5Sget_select_elem_pointlist(sid3, (hsize_t)0, (hsize_t)ret, coords);
    CHECK(ret, FAIL, "H5Sget_select_elem_pointlist");
    VERIFY(coords[0], coord1[0][0], "Element Coordinates");
    VERIFY(coords[1], coord1[1][0], "Element Coordinates");
    VERIFY(coords[2], coord1[2][0], "Element Coordinates");
    VERIFY(coords[3], coord1[3][0], "Element Coordinates");
    VERIFY(coords[4], coord1[4][0], "Element Coordinates");
    VERIFY(coords[5], coord1[5][0], "Element Coordinates");
    VERIFY(coords[6], coord1[6][0], "Element Coordinates");
    VERIFY(coords[7], coord1[7][0], "Element Coordinates");
    VERIFY(coords[8], coord1[8][0], "Element Coordinates");
    VERIFY(coords[9], coord1[9][0], "Element Coordinates");
    HDfree(coords);
    ret = H5Sget_select_bounds(sid3, low, high);
    CHECK(ret, FAIL, "H5Sget_select_bounds");
    VERIFY(low[0], 3, "Selection Bounds");
    VERIFY(high[0], 97, "Selection Bounds");

    /* Close region space */
    ret = H5Sclose(sid3);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close first space */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close dereferenced Dataset */
    ret = H5Dclose(dset3);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close Dataset */
    ret = H5Dclose(dset1);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Free memory buffers */
    free(wbuf);
    free(rbuf);
    free(dwbuf);
    free(drbuf);
}   /* test_reference_region_1D() */

/****************************************************************
**
**  test_reference_obj_deleted(): Test H5R (reference) object reference code.
**      Tests for correct failures for deleted and non-existent objects
**
****************************************************************/
static void
test_reference_obj_deleted(void)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset,	/* Dataset ID			*/
                dset2;      /* Dereferenced dataset ID */
    hid_t		sid1;       /* Dataspace ID			*/
    hobj_ref_t  oref;       /* Object Reference to test */
    H5O_type_t          obj_type;       /* Object type */
    herr_t		ret;		/* Generic return value		*/

    /* Create file */
    fid1 = H5Fcreate(FILE3, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create scalar dataspace for datasets */
    sid1 = H5Screate_simple(0, NULL, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a dataset to reference (deleted later) */
    dataset = H5Dcreate2(fid1, "Dataset1", H5T_NATIVE_INT, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, "Dataset2", H5T_STD_REF_OBJ, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Create reference to dataset */
    ret = H5Rcreate(&oref, fid1, "/Dataset1", H5R_OBJECT, -1);
    CHECK(ret, FAIL, "H5Rcreate");
    ret = H5Rget_obj_type2(dataset, H5R_OBJECT, &oref, &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type2");
    VERIFY(obj_type, H5O_TYPE_DATASET, "H5Rget_obj_type2");

    /* Write selection to disk */
    ret = H5Dwrite(dataset, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, H5P_DEFAULT, &oref);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Delete referenced dataset */
    ret = H5Ldelete(fid1, "/Dataset1", H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Ldelete");

    /* Close disk dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open the file */
    fid1 = H5Fopen(FILE3, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Open the dataset */
    dataset = H5Dopen2(fid1, "/Dataset2", H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Dopen2");

    /* Read selection from disk */
    HDmemset(&oref, 0, sizeof(hobj_ref_t));
    ret = H5Dread(dataset, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, H5P_DEFAULT, &oref);
    CHECK(ret, FAIL, "H5Dread");

    /* Open deleted dataset object */
    dset2 = H5Rdereference(dataset, H5R_OBJECT, &oref);
    VERIFY(dset2, FAIL, "H5Rdereference");

    /* Open nonsense reference */
    HDmemset(&oref, 0, sizeof(hobj_ref_t));
    dset2 = H5Rdereference(dataset, H5R_OBJECT, &oref);
    VERIFY(dset2, FAIL, "H5Rdereference");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_reference_obj_deleted() */

/****************************************************************
**
**  test_deref_iter_op(): Iterator callback for test_reference_group_iterate()
**      test.
**
****************************************************************/
static herr_t
test_deref_iter_op(hid_t UNUSED group, const char *name, const H5L_info_t UNUSED *info,
    void *op_data)
{
    int *count = (int *)op_data;        /* Pointer to name counter */
    herr_t ret_value;

    /* Simple check for correct names */
    if(*count == 0) {
        if(HDstrcmp(name, DSETNAME2) == 0)
            ret_value = 0;
        else
            ret_value = -1;
    } /* end if */
    else if(*count == 1) {
        if(HDstrcmp(name, GROUPNAME2) == 0)
            ret_value = 0;
        else
            ret_value = -1;
    } /* end if */
    else if(*count == 2) {
        if(HDstrcmp(name, GROUPNAME3) == 0)
            ret_value = 0;
        else
            ret_value = -1;
    } /* end if */
    else
        ret_value = -1;

    (*count)++;

    return(ret_value);
} /* end test_deref_iter_op() */

/****************************************************************
**
**  test_reference_group(): Test H5R (reference) object reference code.
**      Tests for correct behavior of various routines on dereferenced group
**
****************************************************************/
static void
test_reference_group(void)
{
    hid_t fid = -1;             /* File ID */
    hid_t gid = -1, gid2 = -1;  /* Group IDs */
    hid_t did;                  /* Dataset ID */
    hid_t sid;                  /* Dataspace ID */
    hobj_ref_t wref;            /* Reference to write */
    hobj_ref_t rref;            /* Reference to read */
    H5G_info_t ginfo;           /* Group info struct */
    char objname[NAME_SIZE];    /* Buffer to store name */
    H5O_info_t oinfo;           /* Object info struct */
    int count = 0;              /* Count within iterated group */
    herr_t ret;

    /* Create file with a group and a dataset containing an object reference to the group */
    fid = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create dataspace to use for dataset */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create group to refer to */
    gid = H5Gcreate2(fid, GROUPNAME,  H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gcreate2");

    /* Create nested groups */
    gid2 = H5Gcreate2(gid, GROUPNAME2,  H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(gid2, FAIL, "H5Gcreate2");
    ret = H5Gclose(gid2);
    CHECK(ret, FAIL, "H5Gclose");

    gid2 = H5Gcreate2(gid, GROUPNAME3,  H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(gid2, FAIL, "H5Gcreate2");
    ret = H5Gclose(gid2);
    CHECK(ret, FAIL, "H5Gclose");

    /* Create bottom dataset */
    did = H5Dcreate2(gid, DSETNAME2, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    assert(did > 0);
    ret = H5Dclose(did);
    assert(ret >= 0);

    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    /* Create dataset */
    did = H5Dcreate2(fid, DSETNAME, H5T_STD_REF_OBJ, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dcreate2");

    /* Create reference to group */
    ret = H5Rcreate(&wref, fid, GROUPNAME, H5R_OBJECT, -1);
    CHECK(ret, FAIL, "H5Rcreate");

    /* Write reference to disk */
    ret = H5Dwrite(did, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, H5P_DEFAULT, &wref);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close objects */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    /* Re-open file */
    fid = H5Fopen(FILE1, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /* Re-open dataset */
    did = H5Dopen2(fid, DSETNAME, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dopen2");

    /* Read in the reference */
    ret = H5Dread(did, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rref);
    CHECK(ret, FAIL, "H5Dread");

    /* Dereference to get the group */
    gid = H5Rdereference(did, H5R_OBJECT, &rref);
    CHECK(gid, FAIL, "H5Rdereference");

    /* Iterate through objects in dereferenced group */
    ret = H5Literate(gid, H5_INDEX_NAME, H5_ITER_INC, NULL, test_deref_iter_op, &count);
    CHECK(ret, FAIL, "H5Literate");

    /* Various queries on the group opened */
    ret = H5Gget_info(gid, &ginfo);
    CHECK(ret, FAIL, "H5Gget_info");
    VERIFY(ginfo.nlinks, 3, "H5Gget_info");

    ret = H5Lget_name_by_idx(gid, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)0, objname, (size_t)NAME_SIZE, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Lget_name_by_idx");
    VERIFY_STR(objname, DSETNAME2, "H5Lget_name_by_idx");

    ret = H5Oget_info_by_idx(gid, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)0, &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_idx");
    VERIFY(oinfo.type, H5O_TYPE_DATASET, "H5Oget_info_by_idx");

    /* Unlink one of the objects in the dereferenced group */
    ret = H5Ldelete(gid, GROUPNAME2, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Ldelete");

    /* Delete dataset object in dereferenced group (with other dataset still open) */
    ret = H5Ldelete(gid, DSETNAME2, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Ldelete");

    /* Close objects */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_reference_group() */

#ifndef H5_NO_DEPRECATED_SYMBOLS
/****************************************************************
**
**  test_reference_compat(): Test basic H5R (reference) object reference code.
**      Tests deprecated API routines
**
****************************************************************/
static void
test_reference_compat(void)
{
    hid_t	fid1;		/* HDF5 File IDs		*/
    hid_t	dataset;	/* Dataset ID			*/
    hid_t	group;          /* Group ID                     */
    hid_t	sid1,           /* Dataspace IDs		*/
                sid2;
    hid_t	tid1;           /* Datatype ID			*/
    hsize_t	dims1[] = {SPACE1_DIM1},
            	dims2[] = {SPACE2_DIM1, SPACE2_DIM2};
    hsize_t	start[SPACE2_RANK];     /* Starting location of hyperslab */
    hsize_t	stride[SPACE2_RANK];    /* Stride of hyperslab */
    hsize_t	count[SPACE2_RANK];     /* Element count of hyperslab */
    hsize_t	block[SPACE2_RANK];     /* Block size of hyperslab */
    hsize_t	coord1[POINT1_NPOINTS][SPACE2_RANK]; /* Coordinates for point selection */
    hobj_ref_t *wbuf_obj,       /* Buffer to write to disk */
               *rbuf_obj;       /* Buffer read from disk */
    hdset_reg_ref_t *wbuf_reg,  /* Buffer to write to disk */
               *rbuf_reg;       /* Buffer read from disk */
    H5G_obj_t   obj_type;       /* Object type */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Deprecated Object Reference Functions\n"));

    /* Allocate write & read buffers */
    wbuf_obj = (hobj_ref_t *)HDcalloc(sizeof(hobj_ref_t), SPACE1_DIM1);
    rbuf_obj = (hobj_ref_t *)HDmalloc(sizeof(hobj_ref_t) * SPACE1_DIM1);
    wbuf_reg = (hdset_reg_ref_t *)HDcalloc(sizeof(hdset_reg_ref_t), SPACE1_DIM1);
    rbuf_reg = (hdset_reg_ref_t *)HDmalloc(sizeof(hdset_reg_ref_t) * SPACE1_DIM1);

    /* Create file */
    fid1 = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create another dataspace for datasets */
    sid2 = H5Screate_simple(SPACE2_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Create a group */
    group = H5Gcreate2(fid1, "Group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(group, FAIL, "H5Gcreate2");

    /* Create a dataset (inside Group1) */
    dataset = H5Dcreate2(group, "Dataset1", H5T_NATIVE_UINT, sid2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create another dataset (inside Group1) */
    dataset = H5Dcreate2(group, "Dataset2", H5T_NATIVE_UCHAR, sid2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create a datatype to refer to */
    tid1 = H5Tcreate(H5T_COMPOUND, sizeof(s1_t));
    CHECK(tid1, FAIL, "H5Tcreate");

    /* Insert fields */
    ret = H5Tinsert(tid1, "a", HOFFSET(s1_t,a), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    ret = H5Tinsert(tid1, "b", HOFFSET(s1_t,b), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    ret = H5Tinsert(tid1, "c", HOFFSET(s1_t,c), H5T_NATIVE_FLOAT);
    CHECK(ret, FAIL, "H5Tinsert");

    /* Save datatype for later */
    ret = H5Tcommit2(group, "Datatype1", tid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");

    /* Close datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close group */
    ret = H5Gclose(group);
    CHECK(ret, FAIL, "H5Gclose");


    /* Create a dataset with object reference datatype */
    dataset = H5Dcreate2(fid1, "Dataset3", H5T_STD_REF_OBJ, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Dcreate2");

    /* Create reference to dataset */
    ret = H5Rcreate(&wbuf_obj[0], fid1, "/Group1/Dataset1", H5R_OBJECT, -1);
    CHECK(ret, FAIL, "H5Rcreate");

    /* Create reference to dataset */
    ret = H5Rcreate(&wbuf_obj[1], fid1, "/Group1/Dataset2", H5R_OBJECT, -1);
    CHECK(ret, FAIL, "H5Rcreate");

    /* Create reference to group */
    ret = H5Rcreate(&wbuf_obj[2], fid1, "/Group1", H5R_OBJECT, -1);
    CHECK(ret, FAIL, "H5Rcreate");

    /* Create reference to named datatype */
    ret = H5Rcreate(&wbuf_obj[3], fid1, "/Group1/Datatype1", H5R_OBJECT, -1);
    CHECK(ret, FAIL, "H5Rcreate");

    /* Write references to disk */
    ret = H5Dwrite(dataset, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf_obj);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");


    /* Create a dataset with region reference datatype */
    dataset = H5Dcreate2(fid1, "Dataset4", H5T_STD_REF_DSETREG, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Dcreate2");

    /* Select 6x6 hyperslab for first reference */
    start[0] = 2; start[1] = 2;
    stride[0] = 1; stride[1] = 1;
    count[0] = 1; count[1] = 1;
    block[0] = 6; block[1] = 6;
    ret = H5Sselect_hyperslab(sid2, H5S_SELECT_SET, start, stride, count, block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Create first dataset region */
    ret = H5Rcreate(&wbuf_reg[0], fid1, "/Group1/Dataset1", H5R_DATASET_REGION, sid2);
    CHECK(ret, FAIL, "H5Rcreate");

    /* Select sequence of ten points for second reference */
    coord1[0][0] = 6; coord1[0][1] = 9;
    coord1[1][0] = 2; coord1[1][1] = 2;
    coord1[2][0] = 8; coord1[2][1] = 4;
    coord1[3][0] = 1; coord1[3][1] = 6;
    coord1[4][0] = 2; coord1[4][1] = 8;
    coord1[5][0] = 3; coord1[5][1] = 2;
    coord1[6][0] = 0; coord1[6][1] = 4;
    coord1[7][0] = 9; coord1[7][1] = 0;
    coord1[8][0] = 7; coord1[8][1] = 1;
    coord1[9][0] = 3; coord1[9][1] = 3;
    ret = H5Sselect_elements(sid2, H5S_SELECT_SET, (size_t)POINT1_NPOINTS, (const hsize_t *)coord1);
    CHECK(ret, FAIL, "H5Sselect_elements");

    /* Create second dataset region */
    ret = H5Rcreate(&wbuf_reg[1], fid1, "/Group1/Dataset2", H5R_DATASET_REGION, sid2);
    CHECK(ret, FAIL, "H5Rcreate");

    /* Write selection to disk */
    ret = H5Dwrite(dataset, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf_reg);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");


    /* Close disk dataspaces */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");


    /* Re-open the file */
    fid1 = H5Fopen(FILE1, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Open the object reference dataset */
    dataset = H5Dopen2(fid1, "/Dataset3", H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Dopen2");

    /* Read selection from disk */
    ret = H5Dread(dataset, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf_obj);
    CHECK(ret, FAIL, "H5Dread");

    /* Verify type of objects pointed at */
    obj_type = H5Rget_obj_type1(dataset, H5R_OBJECT, &rbuf_obj[0]);
    CHECK(obj_type, H5G_UNKNOWN, "H5Rget_obj_type1");
    VERIFY(obj_type, H5G_DATASET, "H5Rget_obj_type1");

    obj_type = H5Rget_obj_type1(dataset, H5R_OBJECT, &rbuf_obj[1]);
    CHECK(obj_type, H5G_UNKNOWN, "H5Rget_obj_type1");
    VERIFY(obj_type, H5G_DATASET, "H5Rget_obj_type1");

    obj_type = H5Rget_obj_type1(dataset, H5R_OBJECT, &rbuf_obj[2]);
    CHECK(obj_type, H5G_UNKNOWN, "H5Rget_obj_type1");
    VERIFY(obj_type, H5G_GROUP, "H5Rget_obj_type1");

    obj_type = H5Rget_obj_type1(dataset, H5R_OBJECT, &rbuf_obj[3]);
    CHECK(obj_type, H5G_UNKNOWN, "H5Rget_obj_type1");
    VERIFY(obj_type, H5G_TYPE, "H5Rget_obj_type1");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");


    /* Open the dataset region reference dataset */
    dataset = H5Dopen2(fid1, "/Dataset4", H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Dopen2");

    /* Read selection from disk */
    ret = H5Dread(dataset, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf_reg);
    CHECK(ret, FAIL, "H5Dread");

    /* Verify type of objects pointed at */
    obj_type = H5Rget_obj_type1(dataset, H5R_DATASET_REGION, &rbuf_reg[0]);
    CHECK(obj_type, H5G_UNKNOWN, "H5Rget_obj_type1");
    VERIFY(obj_type, H5G_DATASET, "H5Rget_obj_type1");

    obj_type = H5Rget_obj_type1(dataset, H5R_DATASET_REGION, &rbuf_reg[1]);
    CHECK(obj_type, H5G_UNKNOWN, "H5Rget_obj_type1");
    VERIFY(obj_type, H5G_DATASET, "H5Rget_obj_type1");

    obj_type = H5Rget_obj_type1(dataset, H5R_DATASET_REGION, &rbuf_reg[2]);
    VERIFY(obj_type, H5G_UNKNOWN, "H5Rget_obj_type1");

    obj_type = H5Rget_obj_type1(dataset, H5R_DATASET_REGION, &rbuf_reg[3]);
    VERIFY(obj_type, H5G_UNKNOWN, "H5Rget_obj_type1");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");


    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Free memory buffers */
    HDfree(wbuf_obj);
    HDfree(rbuf_obj);
    HDfree(wbuf_reg);
    HDfree(rbuf_reg);
}   /* test_reference_compat() */
#endif /* H5_NO_DEPRECATED_SYMBOLS */

/****************************************************************
**
**  test_reference(): Main H5R reference testing routine.
**
****************************************************************/
void
test_reference(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing References\n"));

    test_reference_params();    /* Test for correct parameter checking */
    test_reference_obj();       /* Test basic H5R object reference code */
    test_reference_region();    /* Test basic H5R dataset region reference code */
    test_reference_region_1D(); /* Test H5R dataset region reference code for 1-D datasets */
    test_reference_obj_deleted(); /* Test H5R object reference code for deleted objects */
    test_reference_group();     /* Test operations on dereferenced groups */
#ifndef H5_NO_DEPRECATED_SYMBOLS
    test_reference_compat();    /* Test operations with old API routines */
#endif /* H5_NO_DEPRECATED_SYMBOLS */

}   /* test_reference() */


/*-------------------------------------------------------------------------
 * Function:	cleanup_reference
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	Quincey Koziol
 *              September 8, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_reference(void)
{
    remove(FILE1);
    remove(FILE2);
    remove(FILE3);
}

