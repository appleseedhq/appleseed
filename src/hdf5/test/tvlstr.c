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
* Test program:	 tvlstr
*
* Test the Variable-Length String functionality
*
*************************************************************/

#include "testhdf5.h"

#include "hdf5.h"

#define DATAFILE   "tvlstr.h5"
#define DATAFILE2  "tvlstr2.h5"

/* 1-D dataset with fixed dimensions */
#define SPACE1_RANK	1
#define SPACE1_DIM1	4

#define VLSTR_TYPE      "vl_string_type"

/* Definitions for the VL re-writing test */
#define REWRITE_NDATASETS       32

/* String for testing attributes */
static const char *string_att = "This is the string for the attribute";
static char *string_att_write=NULL;

void *test_vlstr_alloc_custom(size_t size, void *info);
void test_vlstr_free_custom(void *mem, void *info);

/****************************************************************
**
**  test_vlstr_alloc_custom(): Test VL datatype custom memory
**      allocation routines.  This routine just uses malloc to
**      allocate the memory and increments the amount of memory
**      allocated.
**
****************************************************************/
void *test_vlstr_alloc_custom(size_t size, void *info)
{
    void *ret_value=NULL;       /* Pointer to return */
    size_t *mem_used=(size_t *)info;  /* Get the pointer to the memory used */
    size_t extra;               /* Extra space needed */

    /*
     *  This weird contortion is required on the DEC Alpha to keep the
     *  alignment correct - QAK
     */
    extra=MAX(sizeof(void *),sizeof(size_t));

    if((ret_value=HDmalloc(extra+size))!=NULL) {
        *(size_t *)ret_value=size;
        *mem_used+=size;
    } /* end if */
    ret_value=((unsigned char *)ret_value)+extra;
    return(ret_value);
}

/****************************************************************
**
**  test_vlstr_free_custom(): Test VL datatype custom memory
**      allocation routines.  This routine just uses free to
**      release the memory and decrements the amount of memory
**      allocated.
**
****************************************************************/
void test_vlstr_free_custom(void *_mem, void *info)
{
    unsigned char *mem;
    size_t *mem_used=(size_t *)info;  /* Get the pointer to the memory used */
    size_t extra;               /* Extra space needed */

    /*
     *  This weird contortion is required on the DEC Alpha to keep the
     *  alignment correct - QAK
     */
    extra=MAX(sizeof(void *),sizeof(size_t));

    if(_mem!=NULL) {
        mem=((unsigned char *)_mem)-extra;
        *mem_used-=*(size_t *)mem;
        HDfree(mem);
    } /* end if */
}

/****************************************************************
**
**  test_vlstrings_basic(): Test basic VL string code.
**      Tests simple VL string I/O
**
****************************************************************/
static void
test_vlstrings_basic(void)
{
    const char *wdata[SPACE1_DIM1]= {
        "Four score and seven years ago our forefathers brought forth on this continent a new nation,",
        "conceived in liberty and dedicated to the proposition that all men are created equal.",
        "Now we are engaged in a great civil war,",
        "testing whether that nation or any nation so conceived and so dedicated can long endure."
        };   /* Information to write */
    char *rdata[SPACE1_DIM1];   /* Information read in */
    char *wdata2;
    hid_t dataspace, dataset2;
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1;       /* Dataspace ID			*/
    hid_t		tid1;       /* Datatype ID			*/
    hid_t       xfer_pid;   /* Dataset transfer property list ID */
    hsize_t		dims1[] = {SPACE1_DIM1};
    hsize_t     size;       /* Number of bytes which will be used */
    unsigned       i;          /* counting variable */
    size_t         str_used;   /* String data in memory */
    size_t         mem_used=0; /* Memory used during allocation */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic VL String Functionality\n"));

    /* Create file */
    fid1 = H5Fcreate(DATAFILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a datatype to refer to */
    tid1 = H5Tcopy(H5T_C_S1);
    CHECK(tid1, FAIL, "H5Tcopy");

    ret = H5Tset_size(tid1,H5T_VARIABLE);
    CHECK(ret, FAIL, "H5Tset_size");

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, "Dataset1", tid1, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Write dataset to disk */
    ret = H5Dwrite(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    CHECK(ret, FAIL, "H5Dwrite");

    dataspace = H5Screate(H5S_SCALAR);

    dataset2 = H5Dcreate2(fid1, "Dataset2", tid1, dataspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    wdata2 = (char*)HDcalloc((size_t)65534, sizeof(char));
    HDmemset(wdata2, 'A', (size_t)65533);

    ret = H5Dwrite(dataset2, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, &wdata2);
    CHECK(ret, FAIL, "H5Dwrite");

    H5Sclose(dataspace);
    H5Dclose(dataset2);
    HDfree(wdata2);

    /* Change to the custom memory allocation routines for reading VL string */
    xfer_pid = H5Pcreate(H5P_DATASET_XFER);
    CHECK(xfer_pid, FAIL, "H5Pcreate");

    ret=H5Pset_vlen_mem_manager(xfer_pid,test_vlstr_alloc_custom,&mem_used,test_vlstr_free_custom,&mem_used);
    CHECK(ret, FAIL, "H5Pset_vlen_mem_manager");

    /* Make certain the correct amount of memory will be used */
    ret=H5Dvlen_get_buf_size(dataset,tid1,sid1,&size);
    CHECK(ret, FAIL, "H5Dvlen_get_buf_size");

    /* Count the actual number of bytes used by the strings */
    for(i=0,str_used=0; i<SPACE1_DIM1; i++)
        str_used+=HDstrlen(wdata[i])+1;

    /* Compare against the strings actually written */
    VERIFY(size,(hsize_t)str_used,"H5Dvlen_get_buf_size");

    /* Read dataset from disk */
    ret = H5Dread(dataset, tid1, H5S_ALL, H5S_ALL, xfer_pid, rdata);
    CHECK(ret, FAIL, "H5Dread");

    /* Make certain the correct amount of memory has been used */
    VERIFY(mem_used,str_used,"H5Dread");

    /* Compare data read in */
    for(i = 0; i < SPACE1_DIM1; i++) {
        if(HDstrlen(wdata[i]) != HDstrlen(rdata[i])) {
            TestErrPrintf("VL data length don't match!, strlen(wdata[%d])=%d, strlen(rdata[%d])=%d\n",(int)i,(int)strlen(wdata[i]),(int)i,(int)strlen(rdata[i]));
            continue;
        } /* end if */
        if(HDstrcmp(wdata[i], rdata[i]) != 0 ) {
            TestErrPrintf("VL data values don't match!, wdata[%d]=%s, rdata[%d]=%s\n",(int)i,wdata[i],(int)i,rdata[i]);
            continue;
        } /* end if */
    } /* end for */

    /* Reclaim the read VL data */
    ret = H5Dvlen_reclaim(tid1,sid1,xfer_pid,rdata);
    CHECK(ret, FAIL, "H5Dvlen_reclaim");

    /* Make certain the VL memory has been freed */
    VERIFY(mem_used,0,"H5Dvlen_reclaim");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close disk dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close dataset transfer property list */
    ret = H5Pclose(xfer_pid);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

} /* end test_vlstrings_basic() */

/****************************************************************
**
**  test_vlstrings_special(): Test VL string code for special
**      string cases, nil and zero-sized.
**
****************************************************************/
static void
test_vlstrings_special(void)
{
    const char *wdata[SPACE1_DIM1] = {"", "two", "three", "\0"};
    const char *wdata2[SPACE1_DIM1] = {NULL, NULL, NULL, NULL};
    char *rdata[SPACE1_DIM1];   /* Information read in */
    char *fill;                 /* Fill value */
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1;       /* Dataspace ID			*/
    hid_t		tid1;       /* Datatype ID			*/
    hid_t		dcpl;       /* Dataset creation property list ID */
    hsize_t		dims1[] = {SPACE1_DIM1};
    unsigned       i;          /* counting variable */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Special VL Strings\n"));

    /* Create file */
    fid1 = H5Fcreate(DATAFILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a datatype to refer to */
    tid1 = H5Tcopy(H5T_C_S1);
    CHECK(tid1, FAIL, "H5Tcopy");

    ret = H5Tset_size(tid1,H5T_VARIABLE);
    CHECK(ret, FAIL, "H5Tset_size");

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, "Dataset3", tid1, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Read from dataset before writing data */
    ret = H5Dread(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);
    CHECK(ret, FAIL, "H5Dread");

    /* Check data read in */
    for(i = 0; i < SPACE1_DIM1; i++)
        if(rdata[i] != NULL)
            TestErrPrintf("VL doesn't match!, rdata[%d]=%p\n",(int)i,rdata[i]);

    /* Write dataset to disk */
    ret = H5Dwrite(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Read dataset from disk */
    ret = H5Dread(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);
    CHECK(ret, FAIL, "H5Dread");

    /* Compare data read in */
    for(i = 0; i < SPACE1_DIM1; i++) {
        if(HDstrlen(wdata[i]) != HDstrlen(rdata[i])) {
            TestErrPrintf("VL data length don't match!, strlen(wdata[%d])=%d, strlen(rdata[%d])=%d\n",(int)i,(int)strlen(wdata[i]),(int)i,(int)strlen(rdata[i]));
            continue;
        } /* end if */
        if((wdata[i] == NULL && rdata[i] != NULL) || (rdata[i] == NULL && wdata[i] != NULL)) {
            TestErrPrintf("VL data values don't match!\n");
            continue;
        } /* end if */
        if(HDstrcmp(wdata[i], rdata[i]) != 0 ) {
            TestErrPrintf("VL data values don't match!, wdata[%d]=%s, rdata[%d]=%s\n",(int)i,wdata[i],(int)i,rdata[i]);
            continue;
        } /* end if */
    } /* end for */

    /* Reclaim the read VL data */
    ret = H5Dvlen_reclaim(tid1, sid1, H5P_DEFAULT, rdata);
    CHECK(ret, FAIL, "H5Dvlen_reclaim");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create another dataset to test nil strings */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Set the fill value for the second dataset */
    fill = NULL;
    ret = H5Pset_fill_value(dcpl, tid1, &fill);
    CHECK(ret, FAIL, "H5Pset_fill_value");

    dataset = H5Dcreate2(fid1, "Dataset4", tid1, sid1, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Close dataset creation property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Read from dataset before writing data */
    ret = H5Dread(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);
    CHECK(ret, FAIL, "H5Dread");

    /* Check data read in */
    for(i = 0; i < SPACE1_DIM1; i++)
        if(rdata[i] != NULL)
            TestErrPrintf("VL doesn't match!, rdata[%d]=%p\n",(int)i,rdata[i]);

    /* Try to write nil strings to disk. */
    ret = H5Dwrite(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata2);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Read nil strings back from disk */
    ret = H5Dread(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);
    CHECK(ret, FAIL, "H5Dread");

    /* Check data read in */
    for(i = 0; i < SPACE1_DIM1; i++)
        if(rdata[i] != NULL)
            TestErrPrintf("VL doesn't match!, rdata[%d]=%p\n",(int)i,rdata[i]);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close disk dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
}

/****************************************************************
**
**  test_vlstring_type(): Test VL string type.
**      Tests if VL string is treated as string.
**
****************************************************************/
static void test_vlstring_type(void)
{
    hid_t               fid;           /* HDF5 File IDs                */
    hid_t               tid_vlstr;
    H5T_cset_t          cset;
    H5T_str_t           pad;
    htri_t              vl_str;         /* Whether string is VL */
    herr_t              ret;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing VL String type\n"));

    /* Open file */
    fid = H5Fopen(DATAFILE, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /* Create a datatype to refer to */
    tid_vlstr = H5Tcopy(H5T_C_S1);
    CHECK(tid_vlstr, FAIL, "H5Tcopy");

    /* Change padding and verify it */
    ret = H5Tset_strpad(tid_vlstr, H5T_STR_NULLPAD);
    CHECK(ret, FAIL, "H5Tset_strpad");
    pad = H5Tget_strpad(tid_vlstr);
    VERIFY(pad, H5T_STR_NULLPAD, "H5Tget_strpad");

    /* Convert to variable-length string */
    ret = H5Tset_size(tid_vlstr, H5T_VARIABLE);
    CHECK(ret, FAIL, "H5Tset_size");

    /* Check if datatype is VL string */
    ret = H5Tget_class(tid_vlstr);
    VERIFY(ret, H5T_STRING, "H5Tget_class");
    ret = H5Tis_variable_str(tid_vlstr);
    VERIFY(ret, TRUE, "H5Tis_variable_str");

    /* Verify that the class detects as a string */
    vl_str = H5Tdetect_class(tid_vlstr, H5T_STRING);
    CHECK(vl_str, FAIL, "H5Tdetect_class");
    VERIFY(vl_str, TRUE, "H5Tdetect_class");

    /* Check default character set and padding */
    cset = H5Tget_cset(tid_vlstr);
    VERIFY(cset, H5T_CSET_ASCII, "H5Tget_cset");
    pad = H5Tget_strpad(tid_vlstr);
    VERIFY(pad, H5T_STR_NULLPAD, "H5Tget_strpad");

    /* Commit variable-length string datatype to storage */
    ret = H5Tcommit2(fid, VLSTR_TYPE, tid_vlstr, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");

    /* Close datatype */
    ret = H5Tclose(tid_vlstr);
    CHECK(ret, FAIL, "H5Tclose");

    tid_vlstr = H5Topen2(fid, VLSTR_TYPE, H5P_DEFAULT);
    CHECK(tid_vlstr, FAIL, "H5Topen2");

    ret = H5Tclose(tid_vlstr);
    CHECK(ret, FAIL, "H5Tclose");

    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    fid = H5Fopen(DATAFILE, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /* Open the variable-length string datatype just created */
    tid_vlstr = H5Topen2(fid, VLSTR_TYPE, H5P_DEFAULT);
    CHECK(tid_vlstr, FAIL, "H5Topen2");

    /* Verify character set and padding */
    cset = H5Tget_cset(tid_vlstr);
    VERIFY(cset, H5T_CSET_ASCII, "H5Tget_cset");
    pad = H5Tget_strpad(tid_vlstr);
    VERIFY(pad, H5T_STR_NULLPAD, "H5Tget_strpad");

    /* Close datatype and file */
    ret = H5Tclose(tid_vlstr);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

} /* end test_vlstring_type() */

/****************************************************************
**
**  test_compact_vlstring(): Test code for storing VL strings in
**      compact datasets.
**
****************************************************************/
static void
test_compact_vlstring(void)
{
    const char *wdata[SPACE1_DIM1] = {"one", "two", "three", "four"};
    char *rdata[SPACE1_DIM1];   /* Information read in */
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1;       /* Dataspace ID			*/
    hid_t		tid1;       /* Datatype ID			*/
    hid_t		plist;      /* Dataset creation property list	*/
    hsize_t		dims1[] = {SPACE1_DIM1};
    unsigned       i;          /* counting variable */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing VL Strings in compact dataset\n"));

    /* Create file */
    fid1 = H5Fcreate(DATAFILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a datatype to refer to */
    tid1 = H5Tcopy (H5T_C_S1);
    CHECK(tid1, FAIL, "H5Tcopy");

    ret = H5Tset_size (tid1,H5T_VARIABLE);
    CHECK(ret, FAIL, "H5Tset_size");

    plist = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(plist, FAIL, "H5Pcreate");

    ret = H5Pset_layout(plist, H5D_COMPACT);
    CHECK(ret, FAIL, "H5Pset_layout");

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, "Dataset5", tid1, sid1, H5P_DEFAULT, plist, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Write dataset to disk */
    ret = H5Dwrite(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Read dataset from disk */
    ret = H5Dread(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);
    CHECK(ret, FAIL, "H5Dread");

    /* Compare data read in */
    for(i = 0; i < SPACE1_DIM1; i++) {
        if(HDstrlen(wdata[i]) != HDstrlen(rdata[i])) {
            TestErrPrintf("VL data length don't match!, strlen(wdata[%d])=%d, strlen(rdata[%d])=%d\n",(int)i,(int)strlen(wdata[i]),(int)i,(int)strlen(rdata[i]));
            continue;
        } /* end if */
        if(HDstrcmp(wdata[i], rdata[i]) != 0) {
            TestErrPrintf("VL data values don't match!, wdata[%d]=%s, rdata[%d]=%s\n",(int)i,wdata[i],(int)i,rdata[i]);
            continue;
        } /* end if */
    } /* end for */

    /* Reclaim the read VL data */
    ret = H5Dvlen_reclaim(tid1, sid1, H5P_DEFAULT, rdata);
    CHECK(ret, FAIL, "H5Dvlen_reclaim");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close disk dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close dataset create property list */
    ret = H5Pclose(plist);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
} /*test_compact_vlstrings*/

/****************************************************************
**
**  test_write_vl_string_attribute(): Test basic VL string code.
**      Tests writing VL strings as attributes
**
****************************************************************/
static void test_write_vl_string_attribute(void)
{
    hid_t file, root, dataspace, att;
    hid_t type;
    herr_t ret;
    char *string_att_check = NULL;

    /* Open the file */
    file = H5Fopen(DATAFILE, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fopen");

    /* Create a datatype to refer to. */
    type = H5Tcopy (H5T_C_S1);
    CHECK(type, FAIL, "H5Tcopy");

    ret = H5Tset_size (type, H5T_VARIABLE);
    CHECK(ret, FAIL, "H5Tset_size");

    root = H5Gopen2(file, "/", H5P_DEFAULT);
    CHECK(root, FAIL, "H5Gopen2");

    dataspace = H5Screate(H5S_SCALAR);
    CHECK(dataspace, FAIL, "H5Screate");

    /* Test creating a "normal" sized string attribute */
    att = H5Acreate2(root, "test_scalar", type, dataspace, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(att, FAIL, "H5Acreate2");

    ret = H5Awrite(att, type, &string_att);
    CHECK(ret, FAIL, "H5Awrite");

    ret = H5Aread(att, type, &string_att_check);
    CHECK(ret, FAIL, "H5Aread");

    if(HDstrcmp(string_att_check,string_att) != 0)
        TestErrPrintf("VL string attributes don't match!, string_att=%s, string_att_check=%s\n",string_att,string_att_check);

    HDfree(string_att_check);
    string_att_check = NULL;

    ret = H5Aclose(att);
    CHECK(ret, FAIL, "HAclose");

    /* Test creating a "large" sized string attribute */
    att = H5Acreate2(root, "test_scalar_large", type, dataspace, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(att, FAIL, "H5Acreate2");

    string_att_write = (char*)HDcalloc((size_t)8192, sizeof(char));
    HDmemset(string_att_write, 'A', (size_t)8191);

    ret = H5Awrite(att, type, &string_att_write);
    CHECK(ret, FAIL, "H5Awrite");

    ret = H5Aread(att, type, &string_att_check);
    CHECK(ret, FAIL, "H5Aread");

    if(HDstrcmp(string_att_check,string_att_write) != 0)
        TestErrPrintf("VL string attributes don't match!, string_att_write=%s, string_att_check=%s\n",string_att_write,string_att_check);

    HDfree(string_att_check);
    string_att_check = NULL;

    /* The attribute string written is freed below, in the test_read_vl_string_attribute() test */
    /* HDfree(string_att_write); */

    ret = H5Aclose(att);
    CHECK(ret, FAIL, "HAclose");

    ret = H5Gclose(root);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Tclose(type);
    CHECK(ret, FAIL, "H5Tclose");

    ret = H5Sclose(dataspace);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    return;
}

/****************************************************************
**
**  test_read_vl_string_attribute(): Test basic VL string code.
**      Tests reading VL strings from attributes
**
****************************************************************/
static void test_read_vl_string_attribute(void)
{
    hid_t file, root, att;
    hid_t type;
    herr_t ret;
    char *string_att_check = NULL;

    /* Open file */
    file = H5Fopen(DATAFILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fopen");

    /* Create a datatype to refer to. */
    type = H5Tcopy (H5T_C_S1);
    CHECK(type, FAIL, "H5Tcopy");

    ret = H5Tset_size (type, H5T_VARIABLE);
    CHECK(ret, FAIL, "H5Tset_size");

    root = H5Gopen2(file, "/", H5P_DEFAULT);
    CHECK(root, FAIL, "H5Gopen2");

    /* Test reading "normal" sized string attribute */
    att = H5Aopen(root, "test_scalar", H5P_DEFAULT);
    CHECK(att, FAIL, "H5Aopen");

    ret = H5Aread(att, type, &string_att_check);
    CHECK(ret, FAIL, "H5Aread");

    if(HDstrcmp(string_att_check,string_att) != 0)
        TestErrPrintf("VL string attributes don't match!, string_att=%s, string_att_check=%s\n",string_att,string_att_check);

    HDfree(string_att_check);
    string_att_check = NULL;

    ret = H5Aclose(att);
    CHECK(ret, FAIL, "HAclose");

    /* Test reading "large" sized string attribute */
    att = H5Aopen(root, "test_scalar_large", H5P_DEFAULT);
    CHECK(att, FAIL, "H5Aopen");

    if(string_att_write) {
        ret = H5Aread(att, type, &string_att_check);
        CHECK(ret, FAIL, "H5Aread");

        if(HDstrcmp(string_att_check,string_att_write) != 0)
            TestErrPrintf("VL string attributes don't match!, string_att_write=%s, string_att_check=%s\n",string_att_write,string_att_check);

        HDfree(string_att_check);
        string_att_check = NULL;
    }

    /* Free string allocated in test_write_vl_string_attribute */
    if(string_att_write)
        HDfree(string_att_write);

    ret = H5Aclose(att);
    CHECK(ret, FAIL, "HAclose");

    ret = H5Tclose(type);
    CHECK(ret, FAIL, "H5Tclose");

    ret = H5Gclose(root);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    return;
}

/* Helper routine for test_vl_rewrite() */
static void write_scalar_dset(hid_t file, hid_t type, hid_t space, char *name, char *data)
{
      hid_t dset;
      herr_t ret;

      dset = H5Dcreate2(file, name, type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
      CHECK(dset, FAIL, "H5Dcreate2");

      ret = H5Dwrite(dset, type, space, space, H5P_DEFAULT, &data);
      CHECK(ret, FAIL, "H5Dwrite");

      ret = H5Dclose(dset);
      CHECK(ret, FAIL, "H5Dclose");
}

/* Helper routine for test_vl_rewrite() */
static void read_scalar_dset(hid_t file, hid_t type, hid_t space, char *name, char *data)
{
    hid_t dset;
    herr_t ret;
    char *data_read;

    dset = H5Dopen2(file, name, H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Dopen2");

    ret = H5Dread(dset, type, space, space, H5P_DEFAULT, &data_read);
    CHECK(ret, FAIL, "H5Dread");

    ret = H5Dclose(dset);
    CHECK(ret, FAIL, "H5Dclose");

    if(HDstrcmp(data, data_read))
        TestErrPrintf("Expected %s for dataset %s but read %s\n", data, name, data_read);

    HDfree(data_read);
}

/****************************************************************
**
**  test_vl_rewrite(): Test basic VL string code.
**      Tests I/O on VL strings when lots of objects in the file
**      have been linked/unlinked.
**
****************************************************************/
static void test_vl_rewrite(void)
{
    hid_t file1, file2; /* File IDs */
    hid_t type;         /* VL string datatype ID */
    hid_t space;        /* Scalar dataspace */
    char name[256];     /* Buffer for names & data */
    int i;              /* Local index variable */
    herr_t ret;         /* Generic return value */

    /* Create the VL string datatype */
    type = H5Tcopy(H5T_C_S1);
    CHECK(type, FAIL, "H5Tcopy");

    ret = H5Tset_size(type, H5T_VARIABLE);
    CHECK(ret, FAIL, "H5Tset_size");

    /* Create the scalar dataspace */
    space = H5Screate(H5S_SCALAR);
    CHECK(space, FAIL, "H5Screate");

    /* Open the files */
    file1 = H5Fcreate(DATAFILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file1, FAIL, "H5Fcreate");

    file2 = H5Fcreate(DATAFILE2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file1, FAIL, "H5Fcreate");

    /* Create in file 1 */
    for(i=0; i<REWRITE_NDATASETS; i++) {
        sprintf(name, "/set_%d", i);
        write_scalar_dset(file1, type, space, name, name);
    }

    /* Effectively copy data from file 1 to 2 */
    for(i=0; i<REWRITE_NDATASETS; i++) {
        sprintf(name, "/set_%d", i);
        read_scalar_dset(file1, type, space, name, name);
        write_scalar_dset(file2, type, space, name, name);
    }

    /* Read back from file 2 */
    for(i = 0; i < REWRITE_NDATASETS; i++) {
        sprintf(name, "/set_%d", i);
        read_scalar_dset(file2, type, space, name, name);
    } /* end for */

    /* Remove from file 2. */
    for(i = 0; i < REWRITE_NDATASETS; i++) {
        sprintf(name, "/set_%d", i);
        ret = H5Ldelete(file2, name, H5P_DEFAULT);
        CHECK(ret, FAIL, "H5Ldelete");
    } /* end for */

    /* Effectively copy from file 1 to file 2 */
    for(i = 0; i < REWRITE_NDATASETS; i++) {
        sprintf(name, "/set_%d", i);
        read_scalar_dset(file1, type, space, name, name);
        write_scalar_dset(file2, type, space, name, name);
    } /* end for */

    /* Close everything */
    ret = H5Tclose(type);
    CHECK(ret, FAIL, "H5Tclose");

    ret = H5Sclose(space);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Fclose(file1);
    CHECK(ret, FAIL, "H5Fclose");

    ret = H5Fclose(file2);
    CHECK(ret, FAIL, "H5Fclose");

    return;
} /* end test_vl_rewrite() */

/****************************************************************
**
**  test_vlstrings(): Main VL string testing routine.
**
****************************************************************/
void
test_vlstrings(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing Variable-Length Strings\n"));

    /* These tests use the same file */
    /* Test basic VL string datatype */
    test_vlstrings_basic();
    test_vlstrings_special();
    test_vlstring_type();
    test_compact_vlstring();

    /* Test using VL strings in attributes */
    test_write_vl_string_attribute();
    test_read_vl_string_attribute();

    /* Test writing VL datasets in files with lots of unlinking */
    test_vl_rewrite();
}   /* test_vlstrings() */


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
void
cleanup_vlstrings(void)
{
    HDremove(DATAFILE);
    HDremove(DATAFILE2);
}

