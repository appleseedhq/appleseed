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
 * Generate the binary hdf5 file for the h5copy tests
 */
#include <stdlib.h>
#include "hdf5.h"
#include "H5private.h"
#include "h5tools.h"

/* Name of tool */
#define PROGRAMNAME "h5copygentest"

/* HDF file names */
#define HDF_FILE1                "h5copytst.h5"
#define HDF_FILE2                "h5copy_ref.h5"
#define HDF_EXT_SRC_FILE         "h5copy_extlinks_src.h5"
#define HDF_EXT_TRG_FILE         "h5copy_extlinks_trg.h5"

/* objects in HDF_FILE1 */
#define DATASET_SIMPLE          "simple"
#define DATASET_CHUNK           "chunk"
#define DATASET_COMPACT         "compact"
#define DATASET_COMPOUND        "compound"
#define DATASET_COMPRESSED      "compressed"
#define DATASET_NAMED_VL        "named_vl"
#define DATASET_NESTED_VL       "nested_vl"
#define GROUP_EMPTY             "grp_empty"
#define GROUP_DATASETS          "grp_dsets"
#define GROUP_NESTED            "grp_nested"

/* Obj reference */
#define OBJ_REF_DS "Dset1"
#define OBJ_REF_GRP "Group"
/* Region reference */
#define REG_REF_DS1 "Dset_REGREF"
#define REG_REF_DS2 "Dset2"


/*-------------------------------------------------------------------------
 * Function:    gent_simple
 *
 * Purpose:     Generate a simple dataset in LOC_ID
 *
 *-------------------------------------------------------------------------
 */
static void gent_simple(hid_t loc_id)
{
    hid_t   sid, did;
    hsize_t dims[1] = {6};
    int     buf[6]  = {1,2,3,4,5,6};

    /* create dataspace */
    sid = H5Screate_simple(1, dims, NULL);

    /* create dataset */
    did = H5Dcreate2(loc_id, DATASET_SIMPLE, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* write */
    H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);

    /* close */
    H5Sclose(sid);
    H5Dclose(did);
}

/*-------------------------------------------------------------------------
 * Function:    gent_chunked
 *
 * Purpose:     Generate a chunked dataset in LOC_ID
 *
 *-------------------------------------------------------------------------
 */
static void gent_chunked(hid_t loc_id)
{
    hid_t   sid, did, pid;
    hsize_t dims[1] = {6};
    hsize_t chunk_dims[1] = {2};
    int     buf[6]  = {1,2,3,4,5,6};

    /* create dataspace */
    sid = H5Screate_simple(1, dims, NULL);

    /* create property plist */
    pid = H5Pcreate(H5P_DATASET_CREATE);
    H5Pset_chunk(pid, 1, chunk_dims);

    /* create dataset */
    did = H5Dcreate2(loc_id, DATASET_CHUNK, H5T_NATIVE_INT, sid, H5P_DEFAULT, pid, H5P_DEFAULT);

    /* write */
    H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);

    /* close */
    H5Sclose(sid);
    H5Dclose(did);
    H5Pclose(pid);
}


/*-------------------------------------------------------------------------
 * Function:    gent_compact
 *
 * Purpose:     Generate a compact dataset in LOC_ID
 *
 *-------------------------------------------------------------------------
 */
static void gent_compact(hid_t loc_id)
{
    hid_t   sid, did, pid;
    hsize_t dims[1] = {6};
    int     buf[6]  = {1,2,3,4,5,6};

    /* create dataspace */
    sid = H5Screate_simple(1, dims, NULL);

    /* create property plist  */
    pid = H5Pcreate(H5P_DATASET_CREATE);
    H5Pset_layout (pid,H5D_COMPACT);

    /* create dataset */
    did = H5Dcreate2(loc_id, DATASET_COMPACT, H5T_NATIVE_INT, sid, H5P_DEFAULT, pid, H5P_DEFAULT);

    /* write */
    H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);

    /* close */
    H5Sclose(sid);
    H5Dclose(did);
    H5Pclose(pid);
}


/*-------------------------------------------------------------------------
 * Function:    gent_compound
 *
 * Purpose:     Generate a compound dataset in LOC_ID
 *
 *-------------------------------------------------------------------------
 */
static void gent_compound(hid_t loc_id)
{
    typedef struct s_t
    {
        char str1[20];
        char str2[20];
    } s_t;
    hid_t   sid, did, tid_c, tid_s;
    hsize_t dims[1] = {2};
    s_t     buf[2]  = {{"str1", "str2"}, {"str3", "str4"}};

    /* create dataspace */
    sid = H5Screate_simple(1, dims, NULL);

    /* create a compound type */
    tid_c = H5Tcreate(H5T_COMPOUND, sizeof(s_t));
    tid_s = H5Tcopy(H5T_C_S1);
    H5Tset_size(tid_s, 20);

    H5Tinsert(tid_c, "str1", HOFFSET(s_t,str1), tid_s);
    H5Tinsert(tid_c, "str2", HOFFSET(s_t,str2), tid_s);

    /* create dataset */
    did = H5Dcreate2(loc_id, DATASET_COMPOUND, tid_c, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* write */
    H5Dwrite(did, tid_c, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);

    /* close */
    H5Sclose(sid);
    H5Dclose(did);
    H5Tclose(tid_c);
    H5Tclose(tid_s);
}

/*-------------------------------------------------------------------------
 * Function:    gent_compressed
 *
 * Purpose:     Generate a compressed dataset in LOC_ID
 *
 *-------------------------------------------------------------------------
 */
static void gent_compressed(hid_t loc_id)
{
    hid_t   sid, did, pid;
    hsize_t dims[1] = {6};
    hsize_t chunk_dims[1] = {2};
    int     buf[6]  = {1,2,3,4,5,6};

    /* create dataspace */
    sid = H5Screate_simple(1, dims, NULL);

    /* create property plist for chunk*/
    pid = H5Pcreate(H5P_DATASET_CREATE);
    H5Pset_chunk(pid, 1, chunk_dims);

    /* set the deflate filter */
#if defined (H5_HAVE_FILTER_DEFLATE)
    H5Pset_deflate(pid, 1);
#endif

    /* create dataset */
    did = H5Dcreate2(loc_id, DATASET_COMPRESSED, H5T_NATIVE_INT, sid, H5P_DEFAULT, pid, H5P_DEFAULT);

    /* write */
    H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);

    /* close */
    H5Sclose(sid);
    H5Dclose(did);
    H5Pclose(pid);
}


/*-------------------------------------------------------------------------
 * Function:    gent_named_vl
 *
 * Purpose:     Generate a variable lenght named datatype for a dataset in
                LOC_ID
 *
 *-------------------------------------------------------------------------
 */
static void gent_named_vl(hid_t loc_id)
{
    hid_t   sid, did, tid;
    hsize_t dims[1] = {2};
    hvl_t   buf[2];

    /* allocate and initialize VL dataset to write */
    buf[0].len = 1;
    buf[0].p = malloc( 1 * sizeof(int));
    ((int *)buf[0].p)[0]=1;
    buf[1].len = 2;
    buf[1].p = malloc( 2 * sizeof(int));
    ((int *)buf[1].p)[0]=2;
    ((int *)buf[1].p)[1]=3;

    /* create dataspace */
    sid = H5Screate_simple(1, dims, NULL);

    /* create datatype */
    tid = H5Tvlen_create(H5T_NATIVE_INT);

    /* create named datatype */
    H5Tcommit2(loc_id, "vl", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* create dataset */
    did = H5Dcreate2(loc_id, DATASET_NAMED_VL, tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* write */
    H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);

    /* close */
    H5Dvlen_reclaim(tid,sid,H5P_DEFAULT,buf);
    H5Sclose(sid);
    H5Dclose(did);
    H5Tclose(tid);
}


/*-------------------------------------------------------------------------
 * Function:    gent_nested_vl
 *
 * Purpose:     Generate a nested variable length dataset in LOC_ID
 *
 *-------------------------------------------------------------------------
 */
static void gent_nested_vl(hid_t loc_id)
{
    hid_t   sid, did, tid1, tid2;
    hsize_t dims[1] = {2};
    hvl_t   buf[2];
    hvl_t   *tvl;

    /* allocate and initialize VL dataset to write */
    buf[0].len = 1;
    buf[0].p = malloc( 1 * sizeof(hvl_t));
    tvl = buf[0].p;
    tvl->p = malloc( 1 * sizeof(int) );
    tvl->len = 1;
    ((int *)tvl->p)[0]=1;

    buf[1].len = 1;
    buf[1].p = malloc( 1 * sizeof(hvl_t));
    tvl = buf[1].p;
    tvl->p = malloc( 2 * sizeof(int) );
    tvl->len = 2;
    ((int *)tvl->p)[0]=2;
    ((int *)tvl->p)[1]=3;

    /* create dataspace */
    sid = H5Screate_simple(1, dims, NULL);

    /* create datatype */
    tid1 = H5Tvlen_create(H5T_NATIVE_INT);

    /* create nested VL datatype */
    tid2 = H5Tvlen_create(tid1);

    /* create dataset */
    did = H5Dcreate2(loc_id, DATASET_NESTED_VL, tid2, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* write */
    H5Dwrite(did, tid2, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);

    /* close */
    H5Dvlen_reclaim(tid2,sid,H5P_DEFAULT,buf);
    H5Sclose(sid);
    H5Dclose(did);
    H5Tclose(tid1);
    H5Tclose(tid2);
}


/*-------------------------------------------------------------------------
 * Function: gent_datasets
 *
 * Purpose: Generate all datasets in a particular location
 *
 *-------------------------------------------------------------------------
 */
static void gent_datasets(hid_t loc_id)
{
    gent_simple(loc_id);
    gent_chunked(loc_id);
    gent_compact(loc_id);
    gent_compound(loc_id);
    gent_compressed(loc_id);
    gent_named_vl(loc_id);
    gent_nested_vl(loc_id);
}

/*-------------------------------------------------------------------------
 * Function: gent_empty_group
 *
 * Purpose: Generate an empty group in a location
 *
 *-------------------------------------------------------------------------
 */
static void gent_empty_group(hid_t loc_id)
{
    hid_t   gid;

    /* Create group in location */
    gid = H5Gcreate2(loc_id, GROUP_EMPTY, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Release resources */
    H5Gclose(gid);
}

/*-------------------------------------------------------------------------
 * Function: gent_nested_datasets
 *
 * Purpose: Generate a group in a location and populate it with the "standard"
 *		datasets
 *
 *-------------------------------------------------------------------------
 */
static void gent_nested_datasets(hid_t loc_id)
{
    hid_t   gid;

    /* Create group in location */
    gid = H5Gcreate2(loc_id, GROUP_DATASETS, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Add datasets to group created */
    gent_datasets(gid);

    /* Release resources */
    H5Gclose(gid);
}

/*-------------------------------------------------------------------------
 * Function: gent_nested_group
 *
 * Purpose: Generate a group in a location and populate it with another group
 *		containing the "standard" datasets
 *
 *-------------------------------------------------------------------------
 */
static void gent_nested_group(hid_t loc_id)
{
    hid_t   gid;

    /* Create group in location */
    gid = H5Gcreate2(loc_id, GROUP_NESTED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Add datasets to group created */
    gent_nested_datasets(gid);

    /* Release resources */
    H5Gclose(gid);
}


/*-------------------------------------------------------------------------
 * Function: gen_obj_ref
 *
 * Purpose: Generate object references to dataset and group
 *
 * Programmer: Jonathan Kim (Feb 23, 2010)
 *------------------------------------------------------------------------*/
static herr_t gen_obj_ref(hid_t loc_id)
{
    hid_t sid=0, oid=0;
    hsize_t dims1[1]={3};
    hsize_t dims2[1]={2};
    int data[3] = {10,20,30};
    int status;

    /*---------------------
     * create obj references to the previously created objects.
     * Passing -1 as reference is an object.*/
    hobj_ref_t or_data[2];  /* write buffer */
    herr_t ret = SUCCEED;

    /*--------------
     * add dataset */
    sid = H5Screate_simple(1, dims1, NULL);
    if (sid < 0)
    {
        fprintf(stderr, "Error: %s %d> H5Screate_simple failed.\n", __FUNCTION__, __LINE__);
        ret = FAIL;
        goto out;
    }

    oid = H5Dcreate2 (loc_id, OBJ_REF_DS, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (oid < 0)
    {
        fprintf(stderr, "Error: %s %d> H5Dcreate2 failed.\n", __FUNCTION__, __LINE__);
        ret = FAIL;
        goto out;
    }

    status = H5Dwrite(oid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
    if (status < 0)
    {
        fprintf(stderr, "Error: %s %d> H5Dwrite failed.\n", __FUNCTION__, __LINE__);
        ret = FAIL;
        goto out;
    }

    H5Dclose(oid);
    H5Sclose(sid);

    /*--------------
     * add group  */
     oid = H5Gcreate2 (loc_id, OBJ_REF_GRP, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (oid < 0)
    {
        fprintf(stderr, "Error: %s %d> H5Gcreate2 failed.\n", __FUNCTION__, __LINE__);
        ret = FAIL;
        goto out;
    }
     H5Gclose(oid);

    status = H5Rcreate (&or_data[0], loc_id, OBJ_REF_DS, H5R_OBJECT, -1);
    if (status < 0)
    {
        fprintf(stderr, "Error: %s %d> H5Rcreate failed.\n", __FUNCTION__, __LINE__);
        ret = FAIL;
        goto out;
    }
    status = H5Rcreate (&or_data[1], loc_id, OBJ_REF_GRP, H5R_OBJECT, -1);
    if (status < 0)
    {
        fprintf(stderr, "Error: %s %d> H5Rcreate failed.\n", __FUNCTION__, __LINE__);
        ret = FAIL;
        goto out;
    }

    sid = H5Screate_simple (1, dims2, NULL);
    if (sid < 0)
    {
        fprintf(stderr, "Error: %s %d> H5Screate_simple failed.\n", __FUNCTION__, __LINE__);
        ret = FAIL;
        goto out;
    }

    oid = H5Dcreate2 (loc_id, "Dset_OBJREF", H5T_STD_REF_OBJ, sid, H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT);
    if (oid < 0)
    {
        fprintf(stderr, "Error: %s %d> H5Dcreate2 failed.\n", __FUNCTION__, __LINE__);
        ret = FAIL;
        goto out;
    }

    status = H5Dwrite(oid, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, H5P_DEFAULT, or_data);
    if (status < 0)
    {
        fprintf(stderr, "Error: %s %d> H5Dwrite failed.\n", __FUNCTION__, __LINE__);
        ret = FAIL;
        goto out;
    }

out:
    if(oid > 0)
        H5Dclose(oid);
    if(sid > 0)
        H5Sclose(sid);

    return ret;
}


/*-------------------------------------------------------------------------
 * Function: gen_region_ref
 *
 * Purpose: Generate dataset region references
 *
 * Programmer: Jonathan Kim (Feb 23, 2010)
 *------------------------------------------------------------------------*/
static herr_t gen_region_ref(hid_t loc_id)
{
    hid_t sid=0, oid1=0, oid2=0;
    int status;
    herr_t ret = SUCCEED;
    char  data[3][16] = {"The quick brown", "fox jumps over ", "the 5 lazy dogs"};
    hsize_t dims2[2] = {3,16};
    hsize_t coords[4][2] = { {0,1}, {2,11}, {1,0}, {2,4} };
    hdset_reg_ref_t  rr_data[2];
    hsize_t start[2] = {0,0};
    hsize_t stride[2] = {2,11};
    hsize_t count[2] = {2,2};
    hsize_t block[2] = {1,3};
    hsize_t dims1[1] = {2};

    sid = H5Screate_simple (2, dims2, NULL);
    if (sid < 0)
    {
        fprintf(stderr, "Error: %s %d> H5Screate_simple failed.\n", __FUNCTION__, __LINE__);
        ret = FAIL;
        goto out;
    }

    /* create normal dataset which is refered */
    oid2 = H5Dcreate2 (loc_id, REG_REF_DS2, H5T_STD_I8LE, sid, H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT);
    if (oid2 < 0)
    {
        fprintf(stderr, "Error: %s %d> H5Dcreate2 failed.\n", __FUNCTION__, __LINE__);
        ret = FAIL;
        goto out;
    }

    /* write values to dataset */
    status = H5Dwrite (oid2, H5T_NATIVE_CHAR, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
    if (status < 0)
    {
        fprintf(stderr, "Error: %s %d> H5Dwrite failed.\n", __FUNCTION__, __LINE__);
        ret = FAIL;
        goto out;
    }

    /* select elements space for reference */
    status = H5Sselect_elements (sid, H5S_SELECT_SET, 4, coords[0]);
    if (status < 0)
    {
        fprintf(stderr, "Error: %s %d> H5Sselect_elements failed.\n", __FUNCTION__, __LINE__);
        ret = FAIL;
        goto out;
    }

    /* create region reference from elements space */
    status = H5Rcreate (&rr_data[0], loc_id, REG_REF_DS2, H5R_DATASET_REGION, sid);
    if (status < 0)
    {
        fprintf(stderr, "Error: %s %d> H5Rcreate failed.\n", __FUNCTION__, __LINE__);
        ret = FAIL;
        goto out;
    }

    /* select hyperslab space for reference */
    status = H5Sselect_hyperslab (sid, H5S_SELECT_SET, start, stride, count, block);
    if (status < 0)
    {
        fprintf(stderr, "Error: %s %d> H5Sselect_hyperslab failed.\n", __FUNCTION__, __LINE__);
        ret = FAIL;
        goto out;
    }

    /* create region reference from hyperslab space */
    status = H5Rcreate (&rr_data[1], loc_id, REG_REF_DS2, H5R_DATASET_REGION, sid);
    if (status < 0)
    {
        fprintf(stderr, "Error: %s %d> H5Rcreate failed.\n", __FUNCTION__, __LINE__);
        ret = FAIL;
        goto out;
    }

    H5Sclose (sid);

    /* Create dataspace. */
    sid = H5Screate_simple (1, dims1, NULL);
    if (sid < 0)
    {
        fprintf(stderr, "Error: %s %d> H5Screate_simple failed.\n", __FUNCTION__, __LINE__);
        ret = FAIL;
        goto out;
    }

    /* create region reference dataset */
    oid1 = H5Dcreate2 (loc_id, REG_REF_DS1, H5T_STD_REF_DSETREG, sid, H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT);
    if (oid1 < 0)
    {
        fprintf(stderr, "Error: %s %d> H5Dcreate2 failed.\n", __FUNCTION__, __LINE__);
        ret = FAIL;
        goto out;
    }

    /* write data as region references */
    status = H5Dwrite (oid1, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT, rr_data);
    if (status < 0)
    {
        fprintf(stderr, "Error: %s %d> H5Dwrite failed.\n", __FUNCTION__, __LINE__);
        ret = FAIL;
        goto out;
    }

out:
    if (oid1 > 0)
        H5Dclose (oid1);
    if (oid2 > 0)
        H5Dclose (oid2);
    if (sid > 0)
        H5Sclose (sid);

    return ret;
}

/*-------------------------------------------------------------------------
 * Function: Test_Obj_Copy
 *
 * Purpose: Testing with various objects
 *
 *------------------------------------------------------------------------*/
static void Test_Obj_Copy()
{
    hid_t fid=0;

    /* Create source file */
    fid = H5Fcreate(HDF_FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (fid < 0)
    {
        fprintf(stderr, "Error: %s> H5Fcreate failed.\n", HDF_FILE1);
        goto out;
    }

    gent_datasets(fid);
    gent_empty_group(fid);
    gent_nested_datasets(fid);
    gent_nested_group(fid);

out:
    /*-----------------------------------------------------------------------
    * Close
    *------------------------------------------------------------------------*/
    if(fid > 0)
        H5Fclose(fid);
}

/*-------------------------------------------------------------------------
 * Function: Test_Ref_Copy
 *
 * Purpose: Testing with various references
 *
 *------------------------------------------------------------------------*/
static void Test_Ref_Copy()
{
    hid_t fid=0;
    herr_t status;

    fid = H5Fcreate (HDF_FILE2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (fid < 0)
    {
        fprintf(stderr, "Error: %s> H5Fcreate failed.\n", HDF_FILE2);
        goto out;
    }

    /* add object reference */
    status = gen_obj_ref(fid);
    if (status < 0)
        fprintf(stderr, "Failed to generate object reference.\n");

    /* add region reference */
    status = gen_region_ref(fid);
    if (status < 0)
        fprintf(stderr, "Failed to generate region reference.\n");

out:
    /*-----------------------------------------------------------------------
    * Close
    *------------------------------------------------------------------------*/
    if(fid > 0)
        H5Fclose(fid);
}

/*-------------------------------------------------------------------------
 * Function: gen_extlink_trg
 *
 * Purpose: generate target external link objs
 *
 * Programmer: Jonathan Kim (March 03, 2010)
 *------------------------------------------------------------------------*/
static herr_t gen_extlink_trg(hid_t loc_id)
{
    hid_t gid=0, tid=0;
    int status;
    herr_t ret = SUCCEED;

    /*-----------------------------------------------------------------------
    * Groups
    *------------------------------------------------------------------------*/
    /*--------------
     * target file */
    gid = H5Gcreate2(loc_id, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (gid < 0)
    {
        fprintf(stderr, "Error: %s %d> H5Gcreate2 failed.\n", __FUNCTION__, __LINE__);
        ret = FAIL;
        goto out;
    }

    /*--------------
     * add dataset */
     gent_simple(loc_id);

    /*--------------------
     * add named datatype
     */
     tid = H5Tcopy(H5T_NATIVE_INT);
     status = H5Tcommit2(loc_id, "datatype", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0)
    {
        fprintf(stderr, "Error: %s %d> H5Tcommit2 failed.\n", __FUNCTION__, __LINE__);
        ret = FAIL;
        goto out;
    }

out:
    if(gid > 0)
        H5Gclose(gid);
    if(tid > 0)
        H5Tclose(tid);

    return ret;
}

/*-------------------------------------------------------------------------
 * Function: gen_extlink_src
 *
 * Purpose: generate source external link objs
 *
 * Programmer: Jonathan Kim (March 03, 2010)
 *------------------------------------------------------------------------*/
static herr_t gen_extlink_src(hid_t loc_id)
{
    hid_t gid=0;
    int status;
    herr_t ret = SUCCEED;

    /*-----------------------------------------------------------------------
    * Groups
    *------------------------------------------------------------------------*/
    gid = H5Gcreate2(loc_id, "/group_ext", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (gid < 0)
    {
        fprintf(stderr, "Error: %s %d> H5Gcreate2 failed.\n", __FUNCTION__, __LINE__);
        ret = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
    * External links
    *------------------------------------------------------------------------*/
    /* link to dataset */
    status = H5Lcreate_external(HDF_EXT_TRG_FILE, "/simple", gid, "extlink_dset", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0)
    {
        fprintf(stderr, "Error: %s %d> H5Lcreate_external failed.\n", __FUNCTION__, __LINE__);
        ret = FAIL;
        goto out;
    }

    /* link to group */
    status = H5Lcreate_external(HDF_EXT_TRG_FILE, "/group", gid, "extlink_grp", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0)
    {
        fprintf(stderr, "Error: %s %d> H5Lcreate_external failed.\n", __FUNCTION__, __LINE__);
        ret = FAIL;
        goto out;
    }

    /* link to datatype */
    status = H5Lcreate_external(HDF_EXT_TRG_FILE, "/datatype", gid, "extlink_datatype", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0)
    {
        fprintf(stderr, "Error: %s %d> H5Lcreate_external failed.\n", __FUNCTION__, __LINE__);
        ret = FAIL;
        goto out;
    }

    /* dangling link - no obj*/
    status = H5Lcreate_external(HDF_EXT_TRG_FILE, "notyet", gid, "extlink_notyet1", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0)
    {
        fprintf(stderr, "Error: %s %d> H5Lcreate_external failed.\n", __FUNCTION__, __LINE__);
        ret = FAIL;
        goto out;
    }

    /* dangling link - no file */
    status = H5Lcreate_external("notyet_file.h5", "notyet", gid, "extlink_notyet2", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0)
    {
        fprintf(stderr, "Error: %s %d> H5Lcreate_external failed.\n", __FUNCTION__, __LINE__);
        ret = FAIL;
        goto out;
    }

out:
    if(gid > 0)
        H5Gclose(gid);

    return ret;
}

/*-------------------------------------------------------------------------
 * Function: Test_Extlink_Copy
 *
 * Purpose: gerenate external link files
 *
 *------------------------------------------------------------------------*/
static void Test_Extlink_Copy()
{
    hid_t fid1=0;
    hid_t fid2=0;
    herr_t status;

    fid1 = H5Fcreate (HDF_EXT_SRC_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (fid1 < 0)
    {
        fprintf(stderr, "Error: %s> H5Fcreate failed.\n", HDF_EXT_SRC_FILE);
        goto out;
    }

    fid2 = H5Fcreate (HDF_EXT_TRG_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (fid2 < 0)
    {
        fprintf(stderr, "Error: %s> H5Fcreate failed.\n", HDF_EXT_TRG_FILE);
        goto out;
    }

    /* add links to source external link file */
    status = gen_extlink_src(fid1);
    if (status < 0)
        fprintf(stderr, "Error: %s> gen_extlink_src failed.\n", HDF_EXT_SRC_FILE);

    /* add objs to target external link file */
    status = gen_extlink_trg(fid2);
    if (status < 0)
        fprintf(stderr, "Error: %s> gen_extlink_trg failed.\n", HDF_EXT_TRG_FILE);

out:
    /*-----------------------------------------------------------------------
    * Close
    *------------------------------------------------------------------------*/
    if(fid1 > 0)
        H5Fclose(fid1);
    if(fid2 > 0)
        H5Fclose(fid2);
}

/*-------------------------------------------------------------------------
 * Function: main
 *
 *-------------------------------------------------------------------------
 */

int main(void)
{
    h5tools_setprogname(PROGRAMNAME);
    h5tools_setstatus(EXIT_SUCCESS);

    Test_Obj_Copy();
    Test_Ref_Copy();
    Test_Extlink_Copy();

    return 0;
}

