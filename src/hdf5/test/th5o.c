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
* Test program:	 th5o
*
* Test public H5O functions for accessing
*
*************************************************************/

#include "testhdf5.h"

#define TEST_FILENAME "th5o_file"

#define RANK 2
#define DIM0 5
#define DIM1 10

#define TEST6_DIM1 100
#define TEST6_DIM2 100


/****************************************************************
**
**  test_h5o_open(): Test H5Oopen function.
**
****************************************************************/
static void
test_h5o_open(void)
{
    hid_t       fid;                        /* HDF5 File ID      */
    hid_t       grp, dset, dtype, dspace;   /* Object identifiers */
    hsize_t     dims[RANK];
    H5I_type_t  id_type;                    /* Type of IDs returned from H5Oopen */
    H5G_info_t  ginfo;                      /* Group info struct */
    H5T_class_t type_class;                 /* Class of the datatype */
    herr_t      ret;                        /* Value returned from API calls */

    /* Create a new HDF5 file */
    fid = H5Fcreate(TEST_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create a group, dataset, and committed datatype within the file */
    /* Create the group */
    grp = H5Gcreate2(fid, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(grp, FAIL, "H5Gcreate2");
    ret = H5Gclose(grp);
    CHECK(ret, FAIL, "H5Gclose");

    /* Commit the type inside the group */
    dtype = H5Tcopy(H5T_NATIVE_INT);
    CHECK(dtype, FAIL, "H5Tcopy");
    ret = H5Tcommit2(fid, "group/datatype", dtype, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");
    ret = H5Tclose(dtype);
    CHECK(ret, FAIL, "H5Tclose");

    /* Create the data space for the dataset. */
    dims[0] = DIM0;
    dims[1] = DIM1;
    dspace = H5Screate_simple(RANK, dims, NULL);
    CHECK(dspace, FAIL, "H5Screate_simple");

    /* Create the dataset. */
    dset = H5Dcreate2(fid, "dataset", H5T_NATIVE_INT, dspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Dcreate2");
    ret = H5Dclose(dset);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Sclose(dspace);
    CHECK(ret, FAIL, "H5Sclose");

    /* Now make sure that H5Oopen can open all three types of objects */
    grp = H5Oopen(fid, "group", H5P_DEFAULT);
    CHECK(grp, FAIL, "H5Oopen");
    dtype = H5Oopen(fid, "group/datatype", H5P_DEFAULT);
    CHECK(dtype, FAIL, "H5Oopen");
    /* Check that we can use the group as a valid location */
    dset = H5Oopen(grp, "/dataset", H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Oopen");

    /* Make sure that each is the right kind of ID */
    id_type = H5Iget_type(grp);
    VERIFY(id_type, H5I_GROUP, "H5Iget_type for group ID");
    id_type = H5Iget_type(dtype);
    VERIFY(id_type, H5I_DATATYPE, "H5Iget_type for datatype ID");
    id_type = H5Iget_type(dset);
    VERIFY(id_type, H5I_DATASET, "H5Iget_type for dataset ID");

    /* Do something more complex with each of the IDs to make sure they "work" */
    ret = H5Gget_info(grp, &ginfo);
    CHECK(ret, FAIL, "H5Gget_info");
    VERIFY(ginfo.nlinks, 1, "H5Gget_info"); /* There should be one object, the datatype */

    type_class = H5Tget_class(dtype);
    VERIFY(type_class, H5T_INTEGER, "H5Tget_class");

    dspace = H5Dget_space(dset);
    CHECK(dspace, FAIL, "H5Dget_space");

    /* Close the IDs */
    ret = H5Gclose(grp);
    CHECK(ret, FAIL, "H5Gclose");
    ret = H5Tclose(dtype);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Dclose(dset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Trying to open objects with bogus names should fail gracefully */
    H5E_BEGIN_TRY {
        grp = H5Oopen(fid, "bogus_group", H5P_DEFAULT);
        VERIFY(grp, FAIL, "H5Oopen");
        dtype = H5Oopen(fid, "group/bogus_datatype", H5P_DEFAULT);
        VERIFY(dtype, FAIL, "H5Oopen");
        dset = H5Oopen(fid, "/bogus_dataset", H5P_DEFAULT);
        VERIFY(dset, FAIL, "H5Oopen");
    } H5E_END_TRY

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Trying to open an object with a bogus file ID should fail */
    H5E_BEGIN_TRY {
        dset = H5Oopen(fid, "dataset", H5P_DEFAULT);
        VERIFY(dset, FAIL, "H5Oopen");
    } H5E_END_TRY
} /* test_h5o_open() */



/****************************************************************
**
**  test_h5o_close(): Test H5Oclose function.
**
****************************************************************/
static void
test_h5o_close(void)
{
    hid_t       fid;                        /* HDF5 File ID      */
    hid_t       grp, dset, dtype, dspace;   /* Object identifiers */
    hsize_t     dims[RANK];
    herr_t      ret;                        /* Value returned from API calls */

    /* Create a new HDF5 file */
    fid = H5Fcreate(TEST_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create a group, dataset, and committed datatype within the file */
    /* Create the group and close it with H5Oclose */
    grp = H5Gcreate2(fid, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(grp, FAIL, "H5Gcreate2");
    VERIFY(H5Iget_type(grp), H5I_GROUP, "H5Iget_type");
    ret = H5Oclose(grp);
    CHECK(ret, FAIL, "H5Oclose");

    /* Commit the type inside the group */
    dtype = H5Tcopy(H5T_NATIVE_INT);
    CHECK(dtype, FAIL, "H5Tcopy");
    ret = H5Tcommit2(fid, "group/datatype", dtype, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");
    ret = H5Oclose(dtype);
    CHECK(ret, FAIL, "H5Oclose");

    /* Create the data space for the dataset. */
    dims[0] = DIM0;
    dims[1] = DIM1;
    dspace = H5Screate_simple(RANK, dims, NULL);
    CHECK(dspace, FAIL, "H5Screate_simple");

    /* Create the dataset. */
    dset = H5Dcreate2(fid, "dataset", H5T_NATIVE_INT, dspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Dcreate2");
    ret = H5Oclose(dset);
    CHECK(ret, FAIL, "H5Oclose");

    /* Attempting to close the data space with H5Oclose should fail */
    H5E_BEGIN_TRY {
       ret = H5Oclose(dspace);
       VERIFY(ret, FAIL, "H5Oclose");
    } H5E_END_TRY
    /* Close the dataspace for real */
    ret = H5Sclose(dspace);
    CHECK(ret, FAIL, "H5Sclose");

    /* Make sure that H5Oclose can close objects opened with H5Oopen */
    grp = H5Oopen(fid, "group", H5P_DEFAULT);
    CHECK(grp, FAIL, "H5Oopen");
    dtype = H5Oopen(fid, "group/datatype", H5P_DEFAULT);
    CHECK(dtype, FAIL, "H5Oopen");
    dset = H5Oopen(fid, "dataset", H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Oopen");

    ret = H5Oclose(grp);
    CHECK(ret, FAIL, "H5Oclose");
    ret = H5Oclose(dtype);
    CHECK(ret, FAIL, "H5Oclose");
    ret = H5Oclose(dset);
    CHECK(ret, FAIL, "H5Oclose");

    /* Make sure H5Oclose can close objects opened with H5*open */
    grp = H5Gopen2(fid, "group", H5P_DEFAULT);
    CHECK(grp, FAIL, "H5Gopen2");
    dtype = H5Topen2(fid, "group/datatype", H5P_DEFAULT);
    CHECK(dtype, FAIL, "H5Topen2");
    dset = H5Dopen2(fid, "dataset", H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Dopen2");

    ret = H5Oclose(grp);
    CHECK(ret, FAIL, "H5Oclose");
    ret = H5Oclose(dtype);
    CHECK(ret, FAIL, "H5Oclose");
    ret = H5Oclose(dset);
    CHECK(ret, FAIL, "H5Oclose");

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
}


/****************************************************************
**
**  test_h5o_open_by_addr(): Test H5Oopen_by_addr function.
**
****************************************************************/
static void
test_h5o_open_by_addr(void)
{
    hid_t       fid;                        /* HDF5 File ID      */
    hid_t       grp, dset, dtype, dspace;   /* Object identifiers */
    H5L_info_t li;                      /* Buffer for H5Lget_info */
    haddr_t grp_addr;                       /* Addresses for objects */
    haddr_t dset_addr;
    haddr_t dtype_addr;
    hsize_t     dims[RANK];
    H5I_type_t  id_type;                    /* Type of IDs returned from H5Oopen */
    H5G_info_t  ginfo;                      /* Group info struct */
    H5T_class_t type_class;                 /* Class of the datatype */
    herr_t      ret;                        /* Value returned from API calls */

    /* Create a new HDF5 file */
    fid = H5Fcreate(TEST_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create a group, dataset, and committed datatype within the file */
    /* Create the group */
    grp = H5Gcreate2(fid, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(grp, FAIL, "H5Gcreate2");
    ret = H5Gclose(grp);
    CHECK(ret, FAIL, "H5Gclose");

    /* Commit the type inside the group */
    dtype = H5Tcopy(H5T_NATIVE_INT);
    CHECK(dtype, FAIL, "H5Tcopy");
    ret = H5Tcommit2(fid, "group/datatype", dtype, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");
    ret = H5Tclose(dtype);
    CHECK(ret, FAIL, "H5Tclose");

    /* Create the data space for the dataset. */
    dims[0] = DIM0;
    dims[1] = DIM1;
    dspace = H5Screate_simple(RANK, dims, NULL);
    CHECK(dspace, FAIL, "H5Screate_simple");

    /* Create the dataset. */
    dset = H5Dcreate2(fid, "dataset", H5T_NATIVE_INT, dspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Dcreate2");
    ret = H5Dclose(dset);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Sclose(dspace);
    CHECK(ret, FAIL, "H5Sclose");

    /* Get address for each object */
    ret = H5Lget_info(fid, "group", &li, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Lget_info");
    grp_addr = li.u.address;
    ret = H5Lget_info(fid, "group/datatype", &li, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Lget_info");
    dtype_addr = li.u.address;
    ret = H5Lget_info(fid, "dataset", &li, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Lget_info");
    dset_addr = li.u.address;

    /* Now make sure that H5Oopen_by_addr can open all three types of objects */
    grp = H5Oopen_by_addr(fid, grp_addr);
    CHECK(grp, FAIL, "H5Oopen_by_addr");
    dtype = H5Oopen_by_addr(fid, dtype_addr);
    CHECK(dtype, FAIL, "H5Oopen_by_addr");
    /* Check that we can use the group ID as a valid location */
    dset = H5Oopen_by_addr(grp, dset_addr);
    CHECK(dset, FAIL, "H5Oopen_by_addr");

    /* Make sure that each is the right kind of ID */
    id_type = H5Iget_type(grp);
    VERIFY(id_type, H5I_GROUP, "H5Iget_type for group ID");
    id_type = H5Iget_type(dtype);
    VERIFY(id_type, H5I_DATATYPE, "H5Iget_type for datatype ID");
    id_type = H5Iget_type(dset);
    VERIFY(id_type, H5I_DATASET, "H5Iget_type for dataset ID");

    /* Do something more complex with each of the IDs to make sure they "work" */
    ret = H5Gget_info(grp, &ginfo);
    CHECK(ret, FAIL, "H5Gget_info");
    VERIFY(ginfo.nlinks, 1, "H5Gget_info"); /* There should be one object, the datatype */

    type_class = H5Tget_class(dtype);
    VERIFY(type_class, H5T_INTEGER, "H5Tget_class");

    dspace = H5Dget_space(dset);
    CHECK(dspace, FAIL, "H5Dget_space");

    /* Close the IDs */
    ret = H5Gclose(grp);
    CHECK(ret, FAIL, "H5Gclose");
    ret = H5Tclose(dtype);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Dclose(dset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Try giving some bogus values to H5O_open_by_addr. */
    /* Try to open an object with a bad address */
    grp_addr += 20;
    H5E_BEGIN_TRY{
      grp = H5Oopen_by_addr(fid, grp_addr);
    }H5E_END_TRY
    VERIFY(grp, FAIL, "H5Oopen_by_addr");

    /* For instance, an objectno smaller than the end of the file's superblock should
     * trigger an error */
    grp_addr = 10;
    H5E_BEGIN_TRY{
      grp = H5Oopen_by_addr(fid, grp_addr);
    }H5E_END_TRY
    VERIFY(grp, FAIL, "H5Oopen_by_addr");

    /* Likewise, an objectno larger than the size of the file should fail */
    grp_addr = 0;
    grp_addr = 1000000000;
    H5E_BEGIN_TRY{
      grp = H5Oopen_by_addr(fid, grp_addr);
    }H5E_END_TRY
    VERIFY(grp, FAIL, "H5Oopen_by_addr");

    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Also, trying to open an object without a valid location should fail */
    H5E_BEGIN_TRY{
      dtype = H5Oopen_by_addr(fid, dtype_addr);
    }H5E_END_TRY
    VERIFY(dtype, FAIL, "H5Oopen_by_addr");
} /* test_h5o_open_by_addr() */


/****************************************************************
**
**  test_h5o_refcount(): Test H5O refcounting functions.
**
****************************************************************/
static void
test_h5o_refcount(void)
{
    hid_t       fid;                        /* HDF5 File ID      */
    hid_t       grp, dset, dtype, dspace;   /* Object identifiers */
    H5O_info_t	oinfo;                      /* Object info struct */
    hsize_t     dims[RANK];
    herr_t      ret;                        /* Value returned from API calls */

    /* Create a new HDF5 file */
    fid = H5Fcreate(TEST_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create a group, dataset, and committed datatype within the file */
    /* Create the group */
    grp = H5Gcreate2(fid, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(grp, FAIL, "H5Gcreate2");

    /* Commit the type inside the group */
    dtype = H5Tcopy(H5T_NATIVE_INT);
    CHECK(dtype, FAIL, "H5Tcopy");
    ret = H5Tcommit2(fid, "datatype", dtype, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");

    /* Create the data space for the dataset. */
    dims[0] = DIM0;
    dims[1] = DIM1;
    dspace = H5Screate_simple(RANK, dims, NULL);
    CHECK(dspace, FAIL, "H5Screate_simple");

    /* Create the dataset. */
    dset = H5Dcreate2(fid, "dataset", H5T_NATIVE_INT, dspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Dcreate2");
    ret = H5Sclose(dspace);
    CHECK(ret, FAIL, "H5Sclose");

    /* Get ref counts for each object.  They should all be 1, since each object has a hard link. */
    ret = H5Oget_info_by_name(fid, "group", &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.rc, 1, "reference count in H5Oget_info_by_name");
    ret = H5Oget_info_by_name(fid, "datatype", &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.rc, 1, "reference count in H5Oget_info_by_name");
    ret = H5Oget_info_by_name(fid, "dataset", &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.rc, 1, "reference count in H5Oget_info_by_name");

    /* Increment each object's reference count. */
    ret = H5Oincr_refcount(grp);
    CHECK(ret, FAIL, "H5Oincr_refcount");
    ret = H5Oincr_refcount(dtype);
    CHECK(ret, FAIL, "H5Oincr_refcount");
    ret = H5Oincr_refcount(dset);
    CHECK(ret, FAIL, "H5Oincr_refcount");

    /* Get ref counts for each object.  They should all be 2 now. */
    ret = H5Oget_info_by_name(fid, "group", &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.rc, 2, "reference count in H5Oget_info_by_name");
    ret = H5Oget_info_by_name(fid, "datatype", &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.rc, 2, "reference count in H5Oget_info_by_name");
    ret = H5Oget_info_by_name(fid, "dataset", &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.rc, 2, "reference count in H5Oget_info_by_name");

    /* Decrement the reference counts and check that they decrease back to 1. */
    ret = H5Odecr_refcount(grp);
    CHECK(ret, FAIL, "H5Odecr_refcount");
    ret = H5Odecr_refcount(dtype);
    CHECK(ret, FAIL, "H5Odecr_refcount");
    ret = H5Odecr_refcount(dset);
    CHECK(ret, FAIL, "H5Odecr_refcount");

    ret = H5Oget_info_by_name(fid, "group", &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.rc, 1, "reference count in H5Oget_info_by_name");
    ret = H5Oget_info_by_name(fid, "datatype", &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.rc, 1, "reference count in H5Oget_info_by_name");
    ret = H5Oget_info_by_name(fid, "dataset", &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.rc, 1, "reference count in H5Oget_info_by_name");

    /* Increment the reference counts and then close the file to make sure the increment is permanant */
    ret = H5Oincr_refcount(grp);
    CHECK(ret, FAIL, "H5Oincr_refcount");
    ret = H5Oincr_refcount(dtype);
    CHECK(ret, FAIL, "H5Oincr_refcount");
    ret = H5Oincr_refcount(dset);
    CHECK(ret, FAIL, "H5Oincr_refcount");

    ret = H5Gclose(grp);
    CHECK(ret, FAIL, "H5Gclose");
    ret = H5Tclose(dtype);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Dclose(dset);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open the file and check that the reference counts were really incremented */
    fid = H5Fopen(TEST_FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    grp = H5Gopen2(fid, "group", H5P_DEFAULT);
    CHECK(grp, FAIL, "H5Gopen2");
    dtype = H5Topen2(fid, "datatype", H5P_DEFAULT);
    CHECK(dtype, FAIL, "H5Topen2");
    dset = H5Dopen2(fid, "dataset", H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Dopen2");

    ret = H5Oget_info_by_name(fid, "group", &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.rc, 2, "reference count in H5Oget_info_by_name");
    ret = H5Oget_info_by_name(fid, "datatype", &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.rc, 2, "reference count in H5Oget_info_by_name");
    ret = H5Oget_info_by_name(fid, "dataset", &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.rc, 2, "reference count in H5Oget_info_by_name");

    /* Decrement the reference counts and close the file */
    ret = H5Odecr_refcount(grp);
    CHECK(ret, FAIL, "H5Odecr_refcount");
    ret = H5Odecr_refcount(dtype);
    CHECK(ret, FAIL, "H5Odecr_refcount");
    ret = H5Odecr_refcount(dset);
    CHECK(ret, FAIL, "H5Odecr_refcount");

    ret = H5Gclose(grp);
    CHECK(ret, FAIL, "H5Gclose");
    ret = H5Tclose(dtype);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Dclose(dset);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open the file and check that the reference counts were really decremented */
    fid = H5Fopen(TEST_FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    grp = H5Gopen2(fid, "group", H5P_DEFAULT);
    CHECK(grp, FAIL, "H5Gopen2");
    dtype = H5Topen2(fid, "datatype", H5P_DEFAULT);
    CHECK(dtype, FAIL, "H5Topen2");
    dset = H5Dopen2(fid, "dataset", H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Dopen2");

    ret = H5Oget_info_by_name(fid, "group", &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.rc, 1, "reference count in H5Oget_info_by_name");
    ret = H5Oget_info_by_name(fid, "datatype", &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.rc, 1, "reference count in H5Oget_info_by_name");
    ret = H5Oget_info_by_name(fid, "dataset", &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.rc, 1, "reference count in H5Oget_info_by_name");

    /* Close the IDs */
    ret = H5Gclose(grp);
    CHECK(ret, FAIL, "H5Gclose");
    ret = H5Tclose(dtype);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Dclose(dset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Make sure that bogus IDs return errors properly */
    H5E_BEGIN_TRY {
        ret = H5Oincr_refcount(grp);
        VERIFY(ret, FAIL, "H5Oincr_refcount");
        ret = H5Oincr_refcount(dtype);
        VERIFY(ret, FAIL, "H5Oincr_refcount");
        ret = H5Oincr_refcount(dset);
        VERIFY(ret, FAIL, "H5Oincr_refcount");
        ret = H5Odecr_refcount(grp);
        VERIFY(ret, FAIL, "H5Odecr_refcount");
        ret = H5Odecr_refcount(dtype);
        VERIFY(ret, FAIL, "H5Odecr_refcount");
        ret = H5Odecr_refcount(dset);
        VERIFY(ret, FAIL, "H5Odecr_refcount");
    } H5E_END_TRY

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
} /* test_h5o_refcount() */


/****************************************************************
**
**  test_h5o_plist(): Test object creation properties
**
****************************************************************/
static void
test_h5o_plist(void)
{
    hid_t       fid;                        /* HDF5 File ID      */
    hid_t       grp, dset, dtype, dspace;   /* Object identifiers */
    hid_t       fapl;                       /* File access property list */
    hid_t       gcpl, dcpl, tcpl;           /* Object creation properties */
    unsigned    def_max_compact, def_min_dense; /* Default phase change parameters */
    unsigned    max_compact, min_dense;         /* Actual phase change parameters */
    herr_t      ret;                        /* Value returned from API calls */

    /* Make a FAPL that uses the "use the latest version of the format" flag */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");

    /* Set the "use the latest version of the format" bounds for creating objects in the file */
    ret = H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST);
    CHECK(ret, FAIL, "H5Pset_libver_bounds");

    /* Create a new HDF5 file */
    fid = H5Fcreate(TEST_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create group, dataset & named datatype creation property lists */
    gcpl = H5Pcreate(H5P_GROUP_CREATE);
    CHECK(gcpl, FAIL, "H5Pcreate");
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");
    tcpl = H5Pcreate(H5P_DATATYPE_CREATE);
    CHECK(tcpl, FAIL, "H5Pcreate");

    /* Retrieve default attribute phase change values */
    ret = H5Pget_attr_phase_change(gcpl, &def_max_compact, &def_min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");

    /* Set non-default attribute phase change values on each creation property list */
    ret = H5Pset_attr_phase_change(gcpl, def_max_compact + 1, def_min_dense - 1);
    CHECK(ret, FAIL, "H5Pset_attr_phase_change");
    ret = H5Pset_attr_phase_change(dcpl, def_max_compact + 1, def_min_dense - 1);
    CHECK(ret, FAIL, "H5Pset_attr_phase_change");
    ret = H5Pset_attr_phase_change(tcpl, def_max_compact + 1, def_min_dense - 1);
    CHECK(ret, FAIL, "H5Pset_attr_phase_change");

    /* Retrieve attribute phase change values on each creation property list and verify */
    ret = H5Pget_attr_phase_change(gcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");
    VERIFY(max_compact, (def_max_compact + 1), "H5Pget_attr_phase_change");
    VERIFY(min_dense, (def_min_dense - 1), "H5Pget_attr_phase_change");
    ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");
    VERIFY(max_compact, (def_max_compact + 1), "H5Pget_attr_phase_change");
    VERIFY(min_dense, (def_min_dense - 1), "H5Pget_attr_phase_change");
    ret = H5Pget_attr_phase_change(tcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");
    VERIFY(max_compact, (def_max_compact + 1), "H5Pget_attr_phase_change");
    VERIFY(min_dense, (def_min_dense - 1), "H5Pget_attr_phase_change");

    /* Create a group, dataset, and committed datatype within the file,
     *  using the respective type of creation property lists.
     */

    /* Create the group anonymously and link it in */
    grp = H5Gcreate_anon(fid, gcpl, H5P_DEFAULT);
    CHECK(grp, FAIL, "H5Gcreate_anon");
    ret = H5Olink(grp, fid, "group", H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Olink");

    /* Commit the type inside the group anonymously and link it in */
    dtype = H5Tcopy(H5T_NATIVE_INT);
    CHECK(dtype, FAIL, "H5Tcopy");
    ret = H5Tcommit_anon(fid, dtype, tcpl, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit_anon");
    ret = H5Olink(dtype, fid, "datatype", H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Olink");

    /* Create the dataspace for the dataset. */
    dspace = H5Screate(H5S_SCALAR);
    CHECK(dspace, FAIL, "H5Screate");

    /* Create the dataset anonymously and link it in */
    dset = H5Dcreate_anon(fid, H5T_NATIVE_INT, dspace, dcpl, H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Dcreate_anon");
    ret = H5Olink(dset, fid, "dataset", H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Olink");
    ret = H5Sclose(dspace);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close current creation property lists */
    ret = H5Pclose(gcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(tcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Retrieve each object's creation property list */
    gcpl = H5Gget_create_plist(grp);
    CHECK(gcpl, FAIL, "H5Gget_create_plist");
    tcpl = H5Tget_create_plist(dtype);
    CHECK(dcpl, FAIL, "H5Tget_create_plist");
    dcpl = H5Dget_create_plist(dset);
    CHECK(dcpl, FAIL, "H5Dget_create_plist");

    /* Retrieve attribute phase change values on each creation property list and verify */
    ret = H5Pget_attr_phase_change(gcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");
    VERIFY(max_compact, (def_max_compact + 1), "H5Pget_attr_phase_change");
    VERIFY(min_dense, (def_min_dense - 1), "H5Pget_attr_phase_change");
    ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");
    VERIFY(max_compact, (def_max_compact + 1), "H5Pget_attr_phase_change");
    VERIFY(min_dense, (def_min_dense - 1), "H5Pget_attr_phase_change");
    ret = H5Pget_attr_phase_change(tcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");
    VERIFY(max_compact, (def_max_compact + 1), "H5Pget_attr_phase_change");
    VERIFY(min_dense, (def_min_dense - 1), "H5Pget_attr_phase_change");

    /* Close current objects */
    ret = H5Pclose(gcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(tcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Gclose(grp);
    CHECK(ret, FAIL, "H5Gclose");
    ret = H5Tclose(dtype);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Dclose(dset);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open the file and check that the object creation properties persist */
    fid = H5Fopen(TEST_FILENAME, H5F_ACC_RDONLY, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Re-open objects */
    grp = H5Gopen2(fid, "group", H5P_DEFAULT);
    CHECK(grp, FAIL, "H5Gopen2");
    dtype = H5Topen2(fid, "datatype", H5P_DEFAULT);
    CHECK(dtype, FAIL, "H5Topen2");
    dset = H5Dopen2(fid, "dataset", H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Dopen2");

    /* Retrieve each object's creation property list */
    gcpl = H5Gget_create_plist(grp);
    CHECK(gcpl, FAIL, "H5Gget_create_plist");
    tcpl = H5Tget_create_plist(dtype);
    CHECK(dcpl, FAIL, "H5Tget_create_plist");
    dcpl = H5Dget_create_plist(dset);
    CHECK(dcpl, FAIL, "H5Dget_create_plist");

    /* Retrieve attribute phase change values on each creation property list and verify */
    ret = H5Pget_attr_phase_change(gcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");
    VERIFY(max_compact, (def_max_compact + 1), "H5Pget_attr_phase_change");
    VERIFY(min_dense, (def_min_dense - 1), "H5Pget_attr_phase_change");
    ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");
    VERIFY(max_compact, (def_max_compact + 1), "H5Pget_attr_phase_change");
    VERIFY(min_dense, (def_min_dense - 1), "H5Pget_attr_phase_change");
    ret = H5Pget_attr_phase_change(tcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");
    VERIFY(max_compact, (def_max_compact + 1), "H5Pget_attr_phase_change");
    VERIFY(min_dense, (def_min_dense - 1), "H5Pget_attr_phase_change");

    /* Close current objects */
    ret = H5Pclose(gcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(tcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Gclose(grp);
    CHECK(ret, FAIL, "H5Gclose");
    ret = H5Tclose(dtype);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Dclose(dset);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Close the FAPL */
    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");
} /* test_h5o_plist() */


/****************************************************************
**
**  test_h5o_link(): Test creating link to object
**
****************************************************************/
static void
test_h5o_link(void)
{
    hid_t file_id=-1;
    hid_t group_id=-1;
    hid_t space_id=-1;
    hid_t dset_id=-1;
    hid_t type_id=-1;
    hid_t fapl_id=-1;
    hid_t lcpl_id=-1;
    hsize_t dims[2] = {TEST6_DIM1, TEST6_DIM2};
    htri_t committed;           /* Whether the named datatype is committed */
    hbool_t new_format;         /* Whether to use the new format or not */
    int wdata[TEST6_DIM1][TEST6_DIM2];
    int rdata[TEST6_DIM1][TEST6_DIM2];
    int i, n, j;
    herr_t ret;                 /* Value returned from API calls */

    /* Initialize the raw data */
    for(i = n = 0; i < TEST6_DIM1; i++)
        for(j = 0; j < TEST6_DIM2; j++)
          wdata[i][j] = n++;

    /* Create the dataspace */
    space_id = H5Screate_simple(2 ,dims, NULL);
    CHECK(space_id, FAIL, "H5Screate_simple");

    /* Create LCPL with intermediate group creation flag set */
    lcpl_id = H5Pcreate(H5P_LINK_CREATE);
    CHECK(lcpl_id, FAIL, "H5Pcreate");
    ret = H5Pset_create_intermediate_group(lcpl_id, TRUE);
    CHECK(ret, FAIL, "H5Pset_create_intermediate_group");

    /* Loop over using new group format */
    for(new_format = FALSE; new_format <= TRUE; new_format++) {

        /* Make a FAPL that uses the "use the latest version of the format" bounds */
        fapl_id = H5Pcreate(H5P_FILE_ACCESS);
        CHECK(fapl_id, FAIL, "H5Pcreate");

        /* Set the "use the latest version of the format" bounds for creating objects in the file */
        ret = H5Pset_libver_bounds(fapl_id, (new_format ? H5F_LIBVER_LATEST : H5F_LIBVER_EARLIEST), H5F_LIBVER_LATEST);
        CHECK(ret, FAIL, "H5Pset_libver_bounds");

        /* Create a new HDF5 file */
        file_id = H5Fcreate(TEST_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
        CHECK(file_id, FAIL, "H5Fcreate");

        /* Close the FAPL */
        ret = H5Pclose(fapl_id);
        CHECK(ret, FAIL, "H5Pclose");


        /* Create and commit a datatype with no name */
        type_id = H5Tcopy(H5T_NATIVE_INT);
        CHECK(type_id, FAIL, "H5Fcreate");
        ret = H5Tcommit_anon(file_id, type_id, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(ret, FAIL, "H5Tcommit_anon");
        committed = H5Tcommitted(type_id);
        VERIFY(committed, TRUE, "H5Tcommitted");

        /* Create a dataset with no name using the committed datatype*/
        dset_id = H5Dcreate_anon(file_id, type_id, space_id, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(dset_id, FAIL, "H5Dcreate_anon");

        /* Verify that we can write to and read from the dataset */

        /* Write the data to the dataset */
        ret = H5Dwrite(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
        CHECK(ret, FAIL, "H5Dwrite");

        /* Read the data back */
        ret = H5Dread(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);
        CHECK(ret, FAIL, "H5Dread");

        /* Verify the data */
        for(i = 0; i < TEST6_DIM1; i++)
            for(j = 0; j < TEST6_DIM2; j++)
                VERIFY(wdata[i][j], rdata[i][j], "H5Dread");

        /* Create a group with no name*/
        group_id = H5Gcreate_anon(file_id, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(group_id, FAIL, "H5Gcreate_anon");

        /* Link nameless datatype into nameless group */
        ret = H5Olink(type_id, group_id, "datatype", H5P_DEFAULT, H5P_DEFAULT);
        CHECK(ret, FAIL, "H5Olink");

        /* Link nameless dataset into nameless group with intermediate group */
        ret = H5Olink(dset_id, group_id, "inter_group/dataset", lcpl_id, H5P_DEFAULT);
        CHECK(ret, FAIL, "H5Olink");

        /* Close IDs for dataset and datatype */
        ret = H5Dclose(dset_id);
        CHECK(ret, FAIL, "H5Dclose");
        ret = H5Tclose(type_id);
        CHECK(ret, FAIL, "H5Tclose");

        /* Re-open datatype using new link */
        type_id = H5Topen2(group_id, "datatype", H5P_DEFAULT);
        CHECK(type_id, FAIL, "H5Topen2");

        /* Link nameless group to root group and close the group ID*/
        ret = H5Olink(group_id, file_id, "/group", H5P_DEFAULT, H5P_DEFAULT);
        CHECK(ret, FAIL, "H5Olink");
        ret = H5Gclose(group_id);
        CHECK(ret, FAIL, "H5Gclose");

        /* Open dataset through root group and verify its data */
        dset_id = H5Dopen2(file_id, "/group/inter_group/dataset", H5P_DEFAULT);
        CHECK(dset_id, FAIL, "H5Dopen2");

        /* Read data from dataset */
        ret = H5Dread(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);
        CHECK(ret, FAIL, "H5Dread");
        for(i = 0; i < TEST6_DIM1; i++)
            for(j = 0; j < TEST6_DIM2; j++)
                VERIFY(wdata[i][j], rdata[i][j], "H5Dread");

        /* Close open IDs */
        ret = H5Dclose(dset_id);
        CHECK(ret, FAIL, "H5Dclose");
        ret = H5Tclose(type_id);
        CHECK(ret, FAIL, "H5Tclose");
        ret = H5Fclose(file_id);
        CHECK(ret, FAIL, "H5Fclose");
    } /* end for */

    /* Close remaining IDs */
    ret = H5Sclose(space_id);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Pclose(lcpl_id);
    CHECK(ret, FAIL, "H5Pclose");
} /* end test_h5o_link() */


/****************************************************************
**
**  test_h5o_comment(): Test H5Oset(get)_comment functions.
**
****************************************************************/
static void
test_h5o_comment(void)
{
    hid_t       fid;                        /* HDF5 File ID      */
    hid_t       grp, dset, dtype, dspace;   /* Object identifiers */
    hid_t       attr_space, attr_id;
    hsize_t     dims[RANK];
    hsize_t     attr_dims = 1;
    int         attr_value = 5;
    const char  *file_comment = "file comment";
    const char  *grp_comment = "group comment";
    const char  *dset_comment = "dataset comment";
    const char  *dtype_comment = "datatype comment";
    char        check_comment[64];
    ssize_t     comment_len = 0;
    herr_t      ret;                        /* Value returned from API calls */
    int         ret_value;

    /* Create a new HDF5 file */
    fid = H5Fcreate(TEST_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create an attribute for the file */
    attr_space = H5Screate_simple(1, &attr_dims, NULL);
    CHECK(attr_space, FAIL, "H5Screate_simple");
    attr_id = H5Acreate2(fid, "file attribute", H5T_NATIVE_INT, attr_space, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr_id, FAIL, "H5Acreate2");
    ret = H5Awrite(attr_id, H5T_NATIVE_INT, &attr_value);
    CHECK(ret, FAIL, "H5Awrite");

    /* Putting a comment on the file through its attribute */
    ret = H5Oset_comment(attr_id, file_comment);
    CHECK(ret, FAIL, "H5Oset_comment");

    ret = H5Sclose(attr_space);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Aclose(attr_id);
    CHECK(ret, FAIL, "H5Aclose");

    /* Create a group, dataset, and committed datatype within the file */
    /* Create the group */
    grp = H5Gcreate2(fid, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(grp, FAIL, "H5Gcreate2");

    /* Putting a comment on the group */
    ret = H5Oset_comment(grp, grp_comment);
    CHECK(ret, FAIL, "H5Oset_comment");

    ret = H5Gclose(grp);
    CHECK(ret, FAIL, "H5Gclose");

    /* Commit the type inside the group */
    dtype = H5Tcopy(H5T_NATIVE_INT);
    CHECK(dtype, FAIL, "H5Tcopy");
    ret = H5Tcommit2(fid, "group/datatype", dtype, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");

    /* Putting a comment on the committed data type */
    ret = H5Oset_comment(dtype, dtype_comment);
    CHECK(ret, FAIL, "H5Oset_comment");

    ret = H5Tclose(dtype);
    CHECK(ret, FAIL, "H5Tclose");

    /* Create the data space for the dataset. */
    dims[0] = DIM0;
    dims[1] = DIM1;
    dspace = H5Screate_simple(RANK, dims, NULL);
    CHECK(dspace, FAIL, "H5Screate_simple");

    /* Create the dataset. */
    dset = H5Dcreate2(fid, "dataset", H5T_NATIVE_INT, dspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Dcreate2");

    /* Putting a comment on the dataset */
    ret = H5Oset_comment(dset, dset_comment);
    CHECK(ret, FAIL, "H5Oset_comment");

    /* Putting a comment on the dataspace.  It's supposed to fail. */
    H5E_BEGIN_TRY {
        ret = H5Oset_comment(dspace, "dataspace comment");
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Oset_comment");

    /* Close the file */
    ret = H5Dclose(dset);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Sclose(dspace);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    /* Now make sure that the comments are correct all 4 types of objects */
    /* Open file */
    fid = H5Fopen(TEST_FILENAME, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /* Getting the comment on the file and verify it */
    comment_len = H5Oget_comment(fid, NULL, (size_t)0);
    CHECK(comment_len, FAIL, "H5Oget_comment");

    ret = H5Oget_comment(fid, check_comment, (size_t)comment_len+1);
    CHECK(ret, FAIL, "H5Oget_comment");

    ret_value = HDstrcmp(file_comment, check_comment);
    VERIFY(ret_value, 0, "H5Oget_comment");

    /* Open the group */
    grp = H5Gopen2(fid, "group", H5P_DEFAULT);
    CHECK(grp, FAIL, "H5Gopen2");

    /* Getting the comment on the group and verify it */
    comment_len = H5Oget_comment(grp, NULL, (size_t)0);
    CHECK(comment_len, FAIL, "H5Oget_comment");

    ret = H5Oget_comment(grp, check_comment, (size_t)comment_len+1);
    CHECK(ret, FAIL, "H5Oget_comment");

    ret_value = HDstrcmp(grp_comment, check_comment);
    VERIFY(ret_value, 0, "H5Oget_comment");

    /* Open the datatype */
    dtype = H5Topen2(fid, "group/datatype", H5P_DEFAULT);
    CHECK(dtype, FAIL, "H5Topen2");

    /* Getting the comment on the datatype and verify it */
    comment_len = H5Oget_comment(dtype, NULL, (size_t)0);
    CHECK(comment_len, FAIL, "H5Oget_comment");

    ret = H5Oget_comment(dtype, check_comment, (size_t)comment_len+1);
    CHECK(ret, FAIL, "H5Oget_comment");

    ret_value = HDstrcmp(dtype_comment, check_comment);
    VERIFY(ret_value, 0, "H5Oget_comment");

    /* Open the dataset */
    dset = H5Dopen2(fid, "dataset", H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Dopen2");

    /* Getting the comment on the dataset and verify it */
    comment_len = H5Oget_comment(dset, NULL, (size_t)0);
    CHECK(comment_len, FAIL, "H5Oget_comment");

    ret = H5Oget_comment(dset, check_comment, (size_t)comment_len+1);
    CHECK(ret, FAIL, "H5Oget_comment");

    ret_value = HDstrcmp(dset_comment, check_comment);
    VERIFY(ret_value, 0, "H5Oget_comment");


    /* Close the IDs */
    ret = H5Gclose(grp);
    CHECK(ret, FAIL, "H5Gclose");
    ret = H5Tclose(dtype);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Dclose(dset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

} /* test_h5o_comment() */


/****************************************************************
**
**  test_h5o_comment_by_name(): Test H5Oset(get)_comment_by_name functions.
**
****************************************************************/
static void
test_h5o_comment_by_name(void)
{
    hid_t       fid;                        /* HDF5 File ID      */
    hid_t       grp, dset, dtype, dspace;   /* Object identifiers */
    hid_t       attr_space, attr_id;
    hsize_t     dims[RANK];
    hsize_t     attr_dims = 1;
    int         attr_value = 5;
    const char  *file_comment = "file comment by name";
    const char  *grp_comment = "group comment by name";
    const char  *dset_comment = "dataset comment by name";
    const char  *dtype_comment = "datatype comment by name";
    char        check_comment[64];
    ssize_t     comment_len = 0;
    herr_t      ret;                        /* Value returned from API calls */
    int         ret_value;

    /* Create a new HDF5 file */
    fid = H5Fcreate(TEST_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create an attribute for the file */
    attr_space = H5Screate_simple(1, &attr_dims, NULL);
    CHECK(attr_space, FAIL, "H5Screate_simple");
    attr_id = H5Acreate2(fid, "file attribute", H5T_NATIVE_INT, attr_space, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr_id, FAIL, "H5Acreate2");
    ret = H5Awrite(attr_id, H5T_NATIVE_INT, &attr_value);
    CHECK(ret, FAIL, "H5Awrite");

    /* Putting a comment on the file through its attribute */
    ret = H5Oset_comment_by_name(attr_id, ".", file_comment, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oset_comment_by_name");

    ret = H5Sclose(attr_space);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Aclose(attr_id);
    CHECK(ret, FAIL, "H5Aclose");

    /* Create a group, dataset, and committed datatype within the file */
    /* Create the group */
    grp = H5Gcreate2(fid, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(grp, FAIL, "H5Gcreate2");

    /* Putting a comment on the group */
    ret = H5Oset_comment_by_name(fid, "group", grp_comment, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oset_comment_by_name");

    /* Commit the type inside the group */
    dtype = H5Tcopy(H5T_NATIVE_INT);
    CHECK(dtype, FAIL, "H5Tcopy");
    ret = H5Tcommit2(fid, "group/datatype", dtype, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");

    /* Putting a comment on the committed data type */
    ret = H5Oset_comment_by_name(grp, "datatype", dtype_comment, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oset_comment_by_name");

    ret = H5Tclose(dtype);
    CHECK(ret, FAIL, "H5Tclose");

    ret = H5Gclose(grp);
    CHECK(ret, FAIL, "H5Gclose");

    /* Create the data space for the dataset. */
    dims[0] = DIM0;
    dims[1] = DIM1;
    dspace = H5Screate_simple(RANK, dims, NULL);
    CHECK(dspace, FAIL, "H5Screate_simple");

    /* Create the dataset. */
    dset = H5Dcreate2(fid, "dataset", H5T_NATIVE_INT, dspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Dcreate2");

    /* Putting a comment on the dataset */
    ret = H5Oset_comment_by_name(fid, "dataset", dset_comment, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oset_comment_by_name");

    /* Putting a comment on the dataspace.  It's supposed to fail. */
    H5E_BEGIN_TRY {
        ret = H5Oset_comment_by_name(dspace, ".", "dataspace comment", H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Oset_comment");

    /* Close the file */
    ret = H5Dclose(dset);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Sclose(dspace);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Now make sure that the comments are correct all 4 types of objects */
    /* Open file */
    fid = H5Fopen(TEST_FILENAME, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /* Getting the comment on the file and verify it */
    comment_len = H5Oget_comment_by_name(fid, ".", NULL, (size_t)0, H5P_DEFAULT);
    CHECK(comment_len, FAIL, "H5Oget_comment_by_name");

    ret = H5Oget_comment_by_name(fid, ".", check_comment, (size_t)comment_len+1, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_comment_by_name");

    ret_value = HDstrcmp(file_comment, check_comment);
    VERIFY(ret_value, 0, "H5Oget_comment_by_name");

    /* Open the group */
    grp = H5Gopen2(fid, "group", H5P_DEFAULT);
    CHECK(grp, FAIL, "H5Gopen2");

    /* Getting the comment on the group and verify it */
    comment_len = H5Oget_comment_by_name(fid, "group", NULL, (size_t)0, H5P_DEFAULT);
    CHECK(comment_len, FAIL, "H5Oget_comment_by_name");

    ret = H5Oget_comment_by_name(fid, "group", check_comment, (size_t)comment_len+1, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_comment_by_name");

    ret_value = HDstrcmp(grp_comment, check_comment);
    VERIFY(ret_value, 0, "H5Oget_comment_by_name");

    /* Getting the comment on the datatype and verify it */
    comment_len = H5Oget_comment_by_name(grp, "datatype", NULL, (size_t)0, H5P_DEFAULT);
    CHECK(comment_len, FAIL, "H5Oget_comment_by_name");

    ret = H5Oget_comment_by_name(grp, "datatype", check_comment, (size_t)comment_len+1, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_comment");

    ret_value = HDstrcmp(dtype_comment, check_comment);
    VERIFY(ret_value, 0, "H5Oget_comment_by_name");

    /* Getting the comment on the dataset and verify it */
    comment_len = H5Oget_comment_by_name(fid, "dataset", NULL, (size_t)0, H5P_DEFAULT);
    CHECK(comment_len, FAIL, "H5Oget_comment_by_name");

    ret = H5Oget_comment_by_name(fid, "dataset", check_comment, (size_t)comment_len+1, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_comment_by_name");

    ret_value = HDstrcmp(dset_comment, check_comment);
    VERIFY(ret_value, 0, "H5Oget_comment_by_name");

    /* Close the IDs */
    ret = H5Gclose(grp);
    CHECK(ret, FAIL, "H5Gclose");

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

} /* test_h5o_comment_by_name() */


/****************************************************************
**
**  test_h5o_getinfo_same_file():  Test that querying the object info for
**      objects in the same file will return the same file "number"
**
****************************************************************/
static void
test_h5o_getinfo_same_file(void)
{
    hid_t       fid1, fid2;             /* HDF5 File ID */
    hid_t       gid1, gid2;             /* Group IDs */
    H5O_info_t	oinfo1, oinfo2;         /* Object info structs */
    herr_t      ret;                    /* Value returned from API calls */

    /* Create a new HDF5 file */
    fid1 = H5Fcreate(TEST_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create two groups in the file */
    gid1 = H5Gcreate2(fid1, "group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(gid1, FAIL, "H5Gcreate2");
    gid2 = H5Gcreate2(fid1, "group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(gid2, FAIL, "H5Gcreate2");

    /* Reset object info */
    HDmemset(&oinfo1, 0, sizeof(oinfo1));
    HDmemset(&oinfo2, 0, sizeof(oinfo2));

    /* Query the object info for each object, through group IDs */
    ret = H5Oget_info(gid1, &oinfo1);
    CHECK(ret, FAIL, "H5Oget_info");
    ret = H5Oget_info(gid2, &oinfo2);
    CHECK(ret, FAIL, "H5Oget_info");

    VERIFY(oinfo1.fileno, oinfo2.fileno, "file number from H5Oget_info");

    /* Reset object info */
    HDmemset(&oinfo1, 0, sizeof(oinfo1));
    HDmemset(&oinfo2, 0, sizeof(oinfo2));

    /* Query the object info for each object, by name */
    ret = H5Oget_info_by_name(fid1, "group1", &oinfo1, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    ret = H5Oget_info_by_name(fid1, "group2", &oinfo2, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");

    VERIFY(oinfo1.fileno, oinfo2.fileno, "file number from H5Oget_info");

    /* Close everything */
    ret = H5Gclose(gid1);
    CHECK(ret, FAIL, "H5Gclose");
    ret = H5Gclose(gid2);
    CHECK(ret, FAIL, "H5Gclose");
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");


    /* Open file twice */
    fid1 = H5Fopen(TEST_FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fopen");
    fid2 = H5Fopen(TEST_FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid2, FAIL, "H5Fopen");

    /* Open the two groups in the file */
    gid1 = H5Gopen2(fid1, "group1", H5P_DEFAULT);
    CHECK(gid1, FAIL, "H5Gopen2");
    gid2 = H5Gopen2(fid2, "group2", H5P_DEFAULT);
    CHECK(gid2, FAIL, "H5Gopen2");

    /* Reset object info */
    HDmemset(&oinfo1, 0, sizeof(oinfo1));
    HDmemset(&oinfo2, 0, sizeof(oinfo2));

    /* Query the object info for each object, through group IDs */
    ret = H5Oget_info(gid1, &oinfo1);
    CHECK(ret, FAIL, "H5Oget_info");
    ret = H5Oget_info(gid2, &oinfo2);
    CHECK(ret, FAIL, "H5Oget_info");

    VERIFY(oinfo1.fileno, oinfo2.fileno, "file number from H5Oget_info");

    /* Reset object info */
    HDmemset(&oinfo1, 0, sizeof(oinfo1));
    HDmemset(&oinfo2, 0, sizeof(oinfo2));

    /* Query the object info for each object, by name */
    ret = H5Oget_info_by_name(fid1, "group1", &oinfo1, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    ret = H5Oget_info_by_name(fid1, "group2", &oinfo2, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");

    VERIFY(oinfo1.fileno, oinfo2.fileno, "file number from H5Oget_info");

    /* Close everything */
    ret = H5Gclose(gid1);
    CHECK(ret, FAIL, "H5Gclose");
    ret = H5Gclose(gid2);
    CHECK(ret, FAIL, "H5Gclose");
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
    ret = H5Fclose(fid2);
    CHECK(ret, FAIL, "H5Fclose");

} /* test_h5o_getinfo_same_file() */


/****************************************************************
**
**  test_h5o(): Main H5O (generic object) testing routine.
**
****************************************************************/
void
test_h5o(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing Objects\n"));

    test_h5o_open();		/* Test generic open function */
    test_h5o_open_by_addr();	/* Test opening objects by address */
    test_h5o_close();		/* Test generic close function */
    test_h5o_refcount();        /* Test incrementing and decrementing reference count */
    test_h5o_plist();           /* Test object creation properties */
    test_h5o_link();            /* Test object link routine */
    test_h5o_comment();         /* Test routines for comment */
    test_h5o_comment_by_name(); /* Test routines for comment by name */
#ifndef  H5_CANNOT_OPEN_TWICE   /* OpenVMS can't open a file twice */
    test_h5o_getinfo_same_file(); /* Test info for objects in the same file */
#endif /* H5_CANNOT_OPEN_TWICE */
} /* test_h5o() */


/*-------------------------------------------------------------------------
 * Function:	cleanup_h5o
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	James Laird
 *              June 3, 2006
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_h5o(void)
{
    remove(TEST_FILENAME);
}

