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
 * Programmer: 	Peter X. Cao
 *             	May 01, 2005
 *
 * Purpose:	Test H5Ocopy().
 */

#include <time.h>
#include "h5test.h"
#include "H5srcdir.h"

/*
 * This file needs to access private information from the H5S package.
 * This file also needs to access the dataspace testing code.
 */
#define H5S_PACKAGE
#define H5S_TESTING
#include "H5Spkg.h"		/* Dataspaces 				*/

/*
 * This file needs to access private information from the H5P package.
 * This file also needs to access the property list testing code.
 */
#define H5P_PACKAGE
#define H5P_TESTING
#include "H5Ppkg.h"		/* Property Lists 			*/

#include "H5Dprivate.h"         /* Datasets (for EFL property name)     */


const char *FILENAME[] = {
    "objcopy_src",
    "objcopy_dst",
    "objcopy_ext",
    NULL
};

/* Configuration, really a series of bit flags.  Maximum value is all three
 * bit flags enabled.
 */
#define CONFIG_SHARE_SRC 1
#define CONFIG_SHARE_DST 2
#define CONFIG_NEW_FORMAT 4
#define CONFIG_DENSE 8
#define MAX_CONFIGURATION 15

#define FILE_EXT 		"objcopy_ext.dat"
/* The fill_old.h5 is generated from gen_old_fill.c in HDF5 'test' directory
 * for version 1.4(after 1.4.3).  To get this data file, simply compile
 * gen_old_fill.c with HDF5 library (before v1.5) and run it. */
#define FILE_OLD_LAYOUT         "fill_old.h5"


#define NAME_DATATYPE_SIMPLE 	"H5T_NATIVE_INT"
#define NAME_DATATYPE_SIMPLE2 	"H5T_NATIVE_INT-2"
#define NAME_DATATYPE_VL 	"vlen of int"
#define NAME_DATATYPE_VL_VL 	"vlen of vlen of int"
#define NAME_DATASET_SIMPLE 	"dataset_simple"
#define NAME_DATASET_SIMPLE2    "dataset_simple_copy"
#define NAME_DATASET_COMPOUND 	"dataset_compound"
#define NAME_DATASET_CHUNKED 	"dataset_chunked"
#define NAME_DATASET_CHUNKED2 	"dataset_chunked2"
#define NAME_DATASET_COMPACT 	"dataset_compact"
#define NAME_DATASET_EXTERNAL 	"dataset_ext"
#define NAME_DATASET_NAMED_DTYPE 	"dataset_named_dtype"
#define NAME_DATASET_NAMED_DTYPE2 	"dataset_named_dtype2"
#define NAME_DATASET_MULTI_OHDR 	"dataset_multi_ohdr"
#define NAME_DATASET_MULTI_OHDR2 	"dataset_multi_ohdr2"
#define NAME_DATASET_VL 	"dataset_vl"
#define NAME_DATASET_VL_VL 	"dataset_vl_vl"
#define NAME_DATASET_CMPD_VL 	"dataset_cmpd_vl"
#define NAME_DATASET_SUB_SUB 	"/g0/g00/g000/dataset_simple"
#define NAME_GROUP_UNCOPIED 	"/uncopied"
#define NAME_GROUP_EMPTY 	"/empty"
#define NAME_GROUP_TOP 		"/g0"
#define NAME_GROUP_SUB 		"/g0/g00"
#define NAME_GROUP_SUB_2	"/g0/g01"
#define NAME_GROUP_SUB_SUB 	"/g0/g00/g000"
#define NAME_GROUP_SUB_SUB2 	"g000"
#define NAME_GROUP_DATASET 	"/g0/dataset_simple"
#define NAME_GROUP_LINK		"/g_links"
#define NAME_GROUP_LINK2	"/g_links2"
#define NAME_GROUP_LOOP		"g_loop"
#define NAME_GROUP_LOOP2	"g_loop2"
#define NAME_GROUP_LOOP3	"g_loop3"
#define NAME_GROUP_REF	        "ref_grp"
#define NAME_LINK_DATASET	"/g_links/dataset_simple"
#define NAME_LINK_HARD		"/g_links/hard_link_to_dataset_simple"
#define NAME_LINK_SOFT		"/g_links/soft_link_to_dataset_simple"
#define NAME_LINK_SOFT2		"/g_links2/soft_link_to_dataset_simple"
#define NAME_LINK_EXTERN	"/g_links/external_link_to_dataset_simple"
#define NAME_LINK_EXTERN2       "/g_links2/external_link_to_dataset_simple"
#define NAME_LINK_SOFT_DANGLE	"/g_links/soft_link_to_nowhere"
#define NAME_LINK_SOFT_DANGLE2	"/g_links2/soft_link_to_nowhere"
#define NAME_LINK_EXTERN_DANGLE "/g_links/external_link_to_nowhere"
#define NAME_LINK_EXTERN_DANGLE2        "/g_links2/external_link_to_nowhere"
#define NAME_OLD_FORMAT		"/dset1"

#define NAME_BUF_SIZE   1024
#define ATTR_NAME_LEN 80
#define DIM_SIZE_1 12
#define DIM_SIZE_2  6
#define CHUNK_SIZE_1 5          /* Not an even fraction of dimension sizes, so we test copying partial chunks */
#define CHUNK_SIZE_2 5
#define NUM_SUB_GROUPS  20
#define NUM_WIDE_LOOP_GROUPS  10
#define NUM_DATASETS  10

char src_obj_full_name[215];  /* the full path + name of the object to be copied */

unsigned num_attributes_g;         /* Number of attributes created */

/* Table containing object id and object name */
/* (Used for detecting duplicate objects when comparing groups */
static struct {
    size_t  nalloc;             /* number of slots allocated */
    size_t  nobjs;              /* number of objects */
    haddr_t *obj;               /* Addresses of objects seen */
} idtab_g;

/* Local function prototypes */
static int
compare_data(hid_t parent1, hid_t parent2, hid_t pid, hid_t tid, size_t nelmts,
            const void *buf1, const void *buf2, hid_t obj_owner);
static int
compare_datasets(hid_t did, hid_t did2, hid_t pid, const void *wbuf);
static int
compare_groups(hid_t gid, hid_t gid2, hid_t pid, int depth, unsigned copy_flags);


/*-------------------------------------------------------------------------
 * Function: addr_insert
 *
 * Purpose: Add an address to the table.
 *
 * Return: void
 *
 * Programmer: Quincey Koziol
 *              Saturday, November  5, 2005
 *
 *-------------------------------------------------------------------------
 */
static void
addr_insert(H5O_info_t *oi)
{
    size_t  n;

    /* Don't add it if the link count is 1 because such an object can only
     * be encountered once. */
    if(oi->rc < 2)
        return;

    /* Extend the table */
    if(idtab_g.nobjs >= idtab_g.nalloc) {
        idtab_g.nalloc = MAX(256, 2*idtab_g.nalloc);
        idtab_g.obj = (haddr_t *)HDrealloc(idtab_g.obj, idtab_g.nalloc * sizeof(idtab_g.obj[0]));
    } /* end if */

    /* Insert the entry */
    n = idtab_g.nobjs++;
    idtab_g.obj[n] = oi->addr;
} /* end addr_insert() */


/*-------------------------------------------------------------------------
 * Function: addr_lookup
 *
 * Purpose: Check if address has already been encountered
 *
 * Return: Success: TRUE/FALSE
 *
 * Failure: (can't fail)
 *
 * Programmer: Quincey Koziol
 *              Saturday, November  5, 2005
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
addr_lookup(H5O_info_t *oi)
{
    size_t  n;

    if(oi->rc < 2) return FALSE; /*only one link possible*/

    for(n = 0; n < idtab_g.nobjs; n++)
        if(H5F_addr_eq(idtab_g.obj[n], oi->addr))
            return TRUE;

    return FALSE;
} /* end addr_lookup() */


/*-------------------------------------------------------------------------
 * Function: addr_reset
 *
 * Purpose: Reset the address tracking data structures
 *
 * Return: void
 *
 * Programmer: Quincey Koziol
 *              Saturday, November  5, 2005
 *
 *-------------------------------------------------------------------------
 */
static void
addr_reset(void)
{
    if(idtab_g.obj)
        HDfree(idtab_g.obj);
    idtab_g.obj = NULL;
    idtab_g.nalloc = idtab_g.nobjs = 0;
} /* end addr_reset() */


/*-------------------------------------------------------------------------
 * Function:    attach_ref_attr
 *
 * Purpose:     Create an attribute with object references
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Peter Cao
 *              Friday, August 4, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
attach_ref_attr(hid_t file_id, hid_t loc_id)
{
    char dsetname1[] = "dataset1_pointed_by_ref_attr";
    char dsetname2[] = "dataset2_pointed_by_ref_attr";
    hid_t did1 = (-1), did2 = (-1), aid = (-1), sid = (-1), sid_ref = (-1);
    hsize_t dims[2] =  {2,9};
    hsize_t dims_ref[1] = {2};
    hobj_ref_t ref[2];
    int data1[2][9] = {{1,1,1,1,1,1,1,1,1},{1,1,1,1,1,1,1,1,18}};
    int data2[2][9] = {{2,2,2,2,2,2,2,2,2},{2,2,2,2,2,2,2,2,18}};

    /* creates two simple datasets */
    if((sid = H5Screate_simple(2, dims, NULL)) < 0) TEST_ERROR
    if((sid_ref = H5Screate_simple(1, dims_ref, NULL)) < 0) TEST_ERROR
    if((did1 = H5Dcreate2(file_id, dsetname1, H5T_NATIVE_INT, sid,  H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Dwrite(did1, H5T_NATIVE_INT, H5S_ALL , H5S_ALL, H5P_DEFAULT,data1) < 0) TEST_ERROR
    if((did2 = H5Dcreate2(file_id, dsetname2, H5T_NATIVE_INT, sid,  H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Dwrite(did2, H5T_NATIVE_INT, H5S_ALL , H5S_ALL, H5P_DEFAULT,data2) < 0) TEST_ERROR

    /* create an attribute with two object references */
    if(H5Rcreate(&ref[0], file_id, dsetname1, H5R_OBJECT, -1) < 0) TEST_ERROR
    if(H5Rcreate(&ref[1], file_id, dsetname2, H5R_OBJECT, -1) < 0) TEST_ERROR
    if((aid = H5Acreate2(loc_id, "obj_ref_attr", H5T_STD_REF_OBJ, sid_ref, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Awrite(aid, H5T_STD_REF_OBJ, ref) < 0) TEST_ERROR

    if(H5Sclose(sid) < 0) TEST_ERROR
    if(H5Sclose(sid_ref) < 0) TEST_ERROR
    if(H5Dclose(did1) < 0) TEST_ERROR
    if(H5Dclose(did2) < 0) TEST_ERROR
    if(H5Aclose(aid) < 0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Sclose(sid);
        H5Sclose(sid_ref);
        H5Dclose(did1);
        H5Dclose(did2);
        H5Aclose(aid);
    } H5E_END_TRY;

    return(-1);
}


/*-------------------------------------------------------------------------
 * Function:    attach_reg_ref_attr
 *
 * Purpose:     Create an attribute with object references
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Peter Cao
 *              Monday, March 5, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
attach_reg_ref_attr(hid_t file_id, hid_t loc_id)
{
    const char dsetnamev[] = "dataset_pointed_by_reg_ref_attr";
    hid_t aid = (-1);
    hid_t space_id = (-1);       /* dataspace identifiers */
    hid_t spacer_id = (-1);       /* dataspace identifiers */
    hid_t dsetv_id = (-1);       /*dataset identifiers*/
    hsize_t dims[2] =  {2,9};
    hsize_t dimsr[1] =  {2};
    int rank = 2;
    int rankr =1;
    hdset_reg_ref_t ref[2];
    int data[2][9] = {{1,1,2,3,3,4,5,5,999},{1,2,2,3,4,4,5,6,999}};
    hsize_t start[2] = {0, 3};
    hsize_t count[2] = {2, 3};
    hsize_t coord[3][2] = {{0, 0}, {1, 6}, {0, 8}};
    size_t num_points = 3;

    /* create a 2D dataset */
    if((space_id = H5Screate_simple(rank, dims, NULL)) < 0) TEST_ERROR
    if((spacer_id = H5Screate_simple(rankr, dimsr, NULL)) < 0) TEST_ERROR
    if((dsetv_id = H5Dcreate2(file_id, dsetnamev, H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Dwrite(dsetv_id, H5T_NATIVE_INT, H5S_ALL , H5S_ALL, H5P_DEFAULT,data) < 0) TEST_ERROR

    /* create reg_ref of block selection */
    if(H5Sselect_hyperslab(space_id,H5S_SELECT_SET,start,NULL,count,NULL) < 0) TEST_ERROR
    if(H5Rcreate(&ref[0], file_id, dsetnamev, H5R_DATASET_REGION, space_id) < 0) TEST_ERROR

    /* create reg_ref of point selection */
    if(H5Sselect_none(space_id) < 0) TEST_ERROR
    if(H5Sselect_elements(space_id, H5S_SELECT_SET, num_points, (const hsize_t *)coord) < 0) TEST_ERROR
    if(H5Rcreate(&ref[1], file_id, dsetnamev, H5R_DATASET_REGION, space_id) < 0) TEST_ERROR

    /* create reg_ref attribute */
    if((aid = H5Acreate2(loc_id, "reg_ref_attr", H5T_STD_REF_DSETREG, spacer_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Awrite(aid, H5T_STD_REF_DSETREG, ref) < 0) TEST_ERROR

    /* attach the reg_ref attribute to the dataset itself */
    if(H5Aclose(aid) < 0) TEST_ERROR
    if((aid = H5Acreate2(dsetv_id, "reg_ref_attr", H5T_STD_REF_DSETREG, spacer_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Awrite(aid, H5T_STD_REF_DSETREG, ref) < 0) TEST_ERROR

    if(H5Sclose(spacer_id) < 0) TEST_ERROR
    if(H5Sclose(space_id) < 0) TEST_ERROR
    if(H5Dclose(dsetv_id) < 0) TEST_ERROR
    if(H5Aclose(aid) < 0) TEST_ERROR


    return 0;

error:
    H5E_BEGIN_TRY {
        H5Sclose(spacer_id);
        H5Sclose(space_id);
        H5Dclose(dsetv_id);
        H5Aclose(aid);
    } H5E_END_TRY;

    return(-1);
}


/*-------------------------------------------------------------------------
 * Function:    create_reg_ref_dataset
 *
 * Purpose:     Create a dataset with region references
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Peter Cao
 *              Friday, August 4, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
create_reg_ref_dataset(hid_t file_id, hid_t loc_id)
{
    const char dsetnamev[] = "dataset_pointed_by_ref_dset";
    const char dsetnamer[] = "dataset_with_reg_ref";
    const char dsetnamer1[] = "compact_dataset_with_reg_ref";
    const char dsetnamer2[] = "compressed_dataset_with_reg_ref";
    hid_t space_id = (-1);       /* dataspace identifiers */
    hid_t spacer_id = (-1);
    hid_t dsetv_id = (-1);       /*dataset identifiers*/
    hid_t dsetr_id = (-1);
    hsize_t dims[2] =  {2,9};
    hsize_t dimsr[1] =  {2};
    int rank = 2;
    int rankr =1;
    hsize_t chunk_size=1;
    hdset_reg_ref_t ref[2];
    int data[2][9] = {{1,1,2,3,3,4,5,5,6},{1,2,2,3,4,4,5,6,6}};
    hsize_t start[2];
    hsize_t count[2];
    hsize_t coord[3][2] = {{0, 0}, {1, 6}, {0, 8}};
    size_t num_points = 3;
    hid_t pid = (-1);

    if((space_id = H5Screate_simple(rank, dims, NULL)) < 0) TEST_ERROR
    if((spacer_id = H5Screate_simple(rankr, dimsr, NULL)) < 0) TEST_ERROR
    if((dsetv_id = H5Dcreate2(file_id, dsetnamev, H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Dwrite(dsetv_id, H5T_NATIVE_INT, H5S_ALL , H5S_ALL, H5P_DEFAULT,data) < 0) TEST_ERROR
    if((dsetr_id = H5Dcreate2(loc_id, dsetnamer, H5T_STD_REF_DSETREG, spacer_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    start[0] = 0;
    start[1] = 3;
    count[0] = 2;
    count[1] = 3;
    if(H5Sselect_hyperslab(space_id,H5S_SELECT_SET,start,NULL,count,NULL) < 0) TEST_ERROR
    if(H5Rcreate(&ref[0], file_id, dsetnamev, H5R_DATASET_REGION, space_id) < 0) TEST_ERROR
    if(H5Sselect_none(space_id) < 0) TEST_ERROR
    if(H5Sselect_elements(space_id, H5S_SELECT_SET, num_points, (const hsize_t *)coord) < 0) TEST_ERROR
    if(H5Rcreate(&ref[1], file_id, dsetnamev, H5R_DATASET_REGION, space_id) < 0) TEST_ERROR
    if(H5Dwrite(dsetr_id, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT,ref) < 0) TEST_ERROR
    if(H5Dclose(dsetr_id) < 0) TEST_ERROR

    /* create and set compact plist */
    if((pid = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR
    if(H5Pset_layout(pid, H5D_COMPACT) < 0) TEST_ERROR

    if((dsetr_id = H5Dcreate2(loc_id, dsetnamer1, H5T_STD_REF_DSETREG, spacer_id, H5P_DEFAULT, pid, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Pclose(pid) < 0) TEST_ERROR
    if(H5Dwrite(dsetr_id, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT, ref) < 0) TEST_ERROR
    if(H5Dclose(dsetr_id) < 0) TEST_ERROR

    /* create and set comp & chunk plist */
    if((pid = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR
    if(H5Pset_chunk(pid, 1, &chunk_size) < 0) TEST_ERROR
    if(H5Pset_deflate(pid, 9) < 0) TEST_ERROR

    if((dsetr_id = H5Dcreate2(loc_id, dsetnamer2, H5T_STD_REF_DSETREG, spacer_id, H5P_DEFAULT, pid, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Pclose(pid) < 0) TEST_ERROR
    if(H5Dwrite(dsetr_id, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT,ref) < 0) TEST_ERROR
    if(H5Dclose(dsetr_id) < 0) TEST_ERROR

    if(H5Sclose(space_id) < 0) TEST_ERROR
    if(H5Sclose(spacer_id) < 0) TEST_ERROR
    if(H5Dclose(dsetv_id) < 0) TEST_ERROR

    return 0;


error:
    H5E_BEGIN_TRY {
        H5Sclose(space_id);
        H5Sclose(spacer_id);
        H5Dclose(dsetr_id);
        H5Dclose(dsetv_id);
        H5Pclose(pid);
    } H5E_END_TRY;

    return(-1);
}


/*-------------------------------------------------------------------------
 * Function:    attach_attribute_vl
 *
 * Purpose:     Attach an vlen attribute to the object to be copied
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Peter Cao
 *              Saturday, December 17, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_attach_attribute_vl(hid_t loc_id)
{
    hid_t aid = -1, sid = -1, tid=-1;
    hvl_t buf[4];
    hsize_t dim1=4;
    unsigned int i, j;
    int ret_value = -1;

    if((sid = H5Screate_simple(1, &dim1, NULL)) < 0 )
        goto done;

    if((tid = H5Tvlen_create(H5T_NATIVE_INT)) < 0)
        goto done;

    for(i = 0; i < 4; i++) {
        buf[i].len = i*3+1;
        buf[i].p = (int *)HDmalloc(buf[i].len * sizeof(int));
        for(j = 0; j < buf[i].len; j++)
            ((int *)buf[i].p)[j] = (int)(j + 1);
    } /* end for */

    if((aid = H5Acreate2(loc_id, "vlen attribute", tid, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto done;

    if(H5Awrite(aid, tid, buf) < 0)
        goto done;

    ret_value = 0;

done:
    if(tid >0 && sid > 0)
        H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, buf);
    if(sid > 0)
        H5Sclose(sid);
    if(tid > 0)
        H5Tclose(tid);
    if(aid > 0)
        H5Aclose(aid);
    return ret_value;
} /* end of attach_attribute_vl */


/*-------------------------------------------------------------------------
 * Function:    test_copy_attach_attributes
 *
 * Purpose:     Attach NUM_ATTRIBUTES attributes to the object to be copied
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Peter Cao
 *              Friday, September 30, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_attach_attributes(hid_t loc_id, hid_t type_id)
{
    hid_t aid = -1, sid = -1;
    char attr_name[ATTR_NAME_LEN];
    int  attr_data[2];
    hsize_t dim1 = 2;
    unsigned  u;
    int ret_value = -1;

    if((sid = H5Screate_simple(1, &dim1, NULL)) < 0 )
        goto done;

    for(u = 0; u < num_attributes_g; u++) {
        sprintf(attr_name, "%u attr", u);

        /* Set attribute data */
        attr_data[0] = (int)(100 * u);
        attr_data[1] = (int)(200 * u);

        if((aid = H5Acreate2(loc_id, attr_name, type_id, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto done;

        if(H5Awrite(aid, H5T_NATIVE_INT, attr_data) < 0)
            goto done;

        if(aid > 0)
            H5Aclose(aid);

         aid = -1;
    }

    ret_value = 0;

done:
    if(sid > 0)
        H5Sclose(sid);
    if(aid > 0)
        H5Aclose(aid);

    return ret_value;
}


/*-------------------------------------------------------------------------
 * Function:    test_copy_attach_paired_attributes
 *
 * Purpose:     Attach NUM_ATTRIBUTES attributes to a pair of objects to be copied
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November 1, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_attach_paired_attributes(hid_t loc_id, hid_t loc_id2, hid_t type_id)
{
    hid_t aid = -1, sid = -1;
    char attr_name[ATTR_NAME_LEN];
    int  attr_data[2];
    unsigned  u;
    hsize_t dim1 = 2;

    if((sid = H5Screate_simple(1, &dim1, NULL)) < 0 ) goto done;

    for(u = 0; u < num_attributes_g; u++) {
        sprintf(attr_name, "%u attr", u);

        /* Set attribute data */
        attr_data[0] = (int)(100 * u);
        attr_data[1] = (int)(200 * u);

        /* Add attribute to first object */
        if((aid = H5Acreate2(loc_id, attr_name, type_id, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0) goto done;
        if(H5Awrite(aid, H5T_NATIVE_INT, attr_data) < 0) goto done;
        if(H5Aclose(aid) < 0) goto done;

        /* Add attribute to second object */
        if((aid = H5Acreate2(loc_id2, attr_name, type_id, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0) goto done;
        if(H5Awrite(aid, H5T_NATIVE_INT, attr_data) < 0) goto done;
        if(H5Aclose(aid) < 0) goto done;
    }

    if(H5Sclose(sid) < 0) goto done;

    return 0;

done:
    if(sid > 0)
        H5Sclose(sid);
    if(aid > 0)
        H5Aclose(aid);

    return -1;
} /* end test_copy_attach_paired_attributes() */


/*-------------------------------------------------------------------------
 * Function:    compare_attribute
 *
 * Purpose:     Compare two attributes to check that they are equal
 *
 * Return:      TRUE if attributes are equal/FALSE if they are different
 *
 * Programmer:  Peter Cao
 *              Saturday, December 17, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
compare_attribute(hid_t aid, hid_t aid2, hid_t pid, const void *wbuf, hid_t obj_owner)
{
    hid_t sid = -1, sid2 = -1;                  /* Dataspace IDs */
    hid_t tid = -1, tid2 = -1;                  /* Datatype IDs */
    size_t elmt_size;                           /* Size of datatype */
    htri_t is_committed;                        /* If the datatype is committed */
    htri_t is_committed2;                       /* If the datatype is committed */
    hssize_t nelmts;                            /* # of elements in dataspace */
    void *rbuf = NULL;                          /* Buffer for reading raw data */
    void *rbuf2 = NULL;                         /* Buffer for reading raw data */

    /* Check the datatypes are equal */

    /* Open the datatype for the source attribute */
    if((tid = H5Aget_type(aid)) < 0) TEST_ERROR

    /* Open the datatype for the destination attribute */
    if((tid2 = H5Aget_type(aid2)) < 0) TEST_ERROR

    /* Check that both datatypes are committed/not committed */
    if((is_committed = H5Tcommitted(tid)) < 0) TEST_ERROR
    if((is_committed2 = H5Tcommitted(tid2)) < 0) TEST_ERROR
    if(is_committed != is_committed2) TEST_ERROR

    /* Compare the datatypes */
    if(H5Tequal(tid, tid2) != TRUE) TEST_ERROR

    /* Determine the size of datatype (for later) */
    if((elmt_size = H5Tget_size(tid)) == 0) TEST_ERROR

    /* Check the dataspaces are equal */

    /* Open the dataspace for the source attribute */
    if((sid = H5Aget_space(aid)) < 0) TEST_ERROR

    /* Open the dataspace for the destination attribute */
    if((sid2 = H5Aget_space(aid2)) < 0) TEST_ERROR

    /* Compare the dataspaces */
    if(H5Sextent_equal(sid, sid2) != TRUE) TEST_ERROR

    /* Determine the number of elements in dataspace (for later) */
    if((nelmts = H5Sget_simple_extent_npoints(sid2)) < 0) TEST_ERROR

    /* Check the raw data is equal */

    /* Allocate & initialize space for the raw data buffers */
    if((rbuf = HDcalloc( elmt_size, (size_t)nelmts)) == NULL) TEST_ERROR
    if((rbuf2 = HDcalloc( elmt_size, (size_t)nelmts)) == NULL) TEST_ERROR

    /* Read data from the source attribute */
    if(H5Aread(aid, tid, rbuf) < 0) TEST_ERROR

    /* Read data from the destination attribute */
    if(H5Aread(aid2, tid2, rbuf2) < 0) TEST_ERROR

    /* Check raw data read in against data written out */
    if(wbuf) {
        if(!compare_data(aid, 0, pid, tid, (size_t)nelmts, wbuf, rbuf, obj_owner)) TEST_ERROR
        if(!compare_data(aid2, 0, pid, tid2, (size_t)nelmts, wbuf, rbuf2, obj_owner)) TEST_ERROR
    } /* end if */
    /* Don't have written data, just compare data between the two attributes */
    else
        if(!compare_data(aid, aid2, pid, tid, (size_t)nelmts, rbuf, rbuf2, obj_owner)) TEST_ERROR

    /* Reclaim vlen data, if necessary */
    if(H5Tdetect_class(tid, H5T_VLEN) == TRUE)
        if(H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, rbuf) < 0) TEST_ERROR
    if(H5Tdetect_class(tid2, H5T_VLEN) == TRUE)
        if(H5Dvlen_reclaim(tid2, sid2, H5P_DEFAULT, rbuf2) < 0) TEST_ERROR

    /* Release raw data buffers */
    HDfree(rbuf);
    rbuf = NULL;
    HDfree(rbuf2);
    rbuf2 = NULL;

    /* close the source dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    /* close the destination dataspace */
    if(H5Sclose(sid2) < 0) TEST_ERROR

    /* close the source datatype */
    if(H5Tclose(tid) < 0) TEST_ERROR

    /* close the destination datatype */
    if(H5Tclose(tid2) < 0) TEST_ERROR

    return TRUE;

error:
    if(rbuf)
        HDfree(rbuf);
    if(rbuf2)
        HDfree(rbuf2);
    H5E_BEGIN_TRY {
        H5Sclose(sid2);
        H5Sclose(sid);
        H5Tclose(tid2);
        H5Tclose(tid);
    } H5E_END_TRY;
    return FALSE;
} /* end compare_attribute() */


/*-------------------------------------------------------------------------
 * Function:    compare_std_attributes
 *
 * Purpose:     Compare "standard" attributes on two objects to check that they are equal
 *
 * Return:	TRUE if objects have same attributes/FALSE if they are different
 *
 * Programmer:  Quincey Koziol
 *              Monday, October 31, 2005
 *
 * Note:	This isn't very general, the attributes are assumed to be
 *              those written in test_copy_attach_attributes().
 *
 * Modifier:    Peter Cao
 *              Wednesday, March 21, 2007
 *              Change to compare any attributes of two objects
 *
 *-------------------------------------------------------------------------
 */
static int
compare_std_attributes(hid_t oid, hid_t oid2, hid_t pid)
{
    hid_t aid = -1, aid2 = -1;                  /* Attribute IDs */
    H5O_info_t oinfo1, oinfo2;                  /* Object info */
    unsigned cpy_flags;                         /* Object copy flags */

    /* Retrieve the object copy flags from the property list, if it's non-DEFAULT */
    if(pid != H5P_DEFAULT) {
        if(H5Pget_copy_object(pid, &cpy_flags) < 0) TEST_ERROR
    } /* end if */
    else
        cpy_flags = 0;

    /* Check the number of attributes on source dataset */
    if(H5Oget_info(oid, &oinfo1) < 0) TEST_ERROR

    /* Check the number of attributes on destination dataset */
    if(H5Oget_info(oid2, &oinfo2) < 0) TEST_ERROR

    if(cpy_flags & H5O_COPY_WITHOUT_ATTR_FLAG) {
        /* Check that the destination has no attributes */
        if(oinfo2.num_attrs != 0) TEST_ERROR
    } /* end if */
    else {
        char attr_name[ATTR_NAME_LEN];  /* Attribute name */
        unsigned i;             /* Local index variable */

        /* Compare the number of attributes */
        if(oinfo1.num_attrs != oinfo2.num_attrs) TEST_ERROR

        /* Check the attributes are equal */
        for(i = 0; i < (unsigned)oinfo1.num_attrs; i++) {
            if((aid = H5Aopen_by_idx(oid, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)i, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
            if(H5Aget_name(aid, (size_t)ATTR_NAME_LEN, attr_name) < 0) TEST_ERROR

            if((aid2 = H5Aopen(oid2, attr_name, H5P_DEFAULT)) < 0) TEST_ERROR

            /* Check the attributes are equal */
            if(!compare_attribute(aid, aid2, pid, NULL, oid)) TEST_ERROR

            /* Close the attributes */
            if(H5Aclose(aid) < 0) TEST_ERROR
            if(H5Aclose(aid2) < 0) TEST_ERROR
        } /* end for */
    } /* end if */

    /* Objects should be the same. :-) */
    return TRUE;

error:
    H5E_BEGIN_TRY {
    	H5Aclose(aid2);
    	H5Aclose(aid);
    } H5E_END_TRY;
    return FALSE;
} /* end compare_std_attributes() */


/*-------------------------------------------------------------------------
 * Function:    compare_data
 *
 * Purpose:     Compare two buffers of data to check that they are equal
 *
 * Return:	TRUE if buffer are equal/FALSE if they are different
 *
 * Programmer:  Quincey Koziol
 *              Monday, November 21, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
compare_data(hid_t parent1, hid_t parent2, hid_t pid, hid_t tid, size_t nelmts,
        const void *buf1, const void *buf2, hid_t obj_owner)
{
    size_t elmt_size;           /* Size of an element */

    /* Check size of each element */
    if((elmt_size = H5Tget_size(tid)) == 0) TEST_ERROR

    /* If the type is a compound containing a vlen, loop over all elements for
     * each compound member.  Compounds containing reference  are not supported
     * yet. */
    if((H5Tget_class(tid) == H5T_COMPOUND)
            && (H5Tdetect_class(tid, H5T_VLEN) == TRUE)) {
        hid_t           memb_id;    /* Member id */
        const uint8_t   *memb1;     /* Pointer to current member */
        const uint8_t   *memb2;     /* Pointer to current member */
        int             nmembs;     /* Number of members */
        size_t          memb_off;   /* Member offset */
        size_t          memb_size;  /* Member size */
        unsigned        memb_idx;   /* Member index */
        size_t          elmt;       /* Current element */

        /* Get number of members in compound */
        if((nmembs = H5Tget_nmembers(tid)) < 0) TEST_ERROR

        /* Loop over members */
        for(memb_idx=0; memb_idx<(unsigned)nmembs; memb_idx++) {
            /* Get member offset.  Note that we cannot check for an error here.
             */
            memb_off = H5Tget_member_offset(tid, memb_idx);

            /* Get member id */
            if((memb_id = H5Tget_member_type(tid, memb_idx)) < 0) TEST_ERROR

            /* Get member size */
            if((memb_size = H5Tget_size(memb_id)) == 0) TEST_ERROR

            /* Set up pointers to member in the first element */
            memb1 = (const uint8_t *)buf1 + memb_off;
            memb2 = (const uint8_t *)buf2 + memb_off;

            /* Check if this member contains (or is) a vlen */
            if(H5Tget_class(memb_id) == H5T_VLEN) {
                hid_t base_id;  /* vlen base type id */

                /* Get base type of vlen datatype */
                if((base_id = H5Tget_super(memb_id)) < 0) TEST_ERROR

                /* Iterate over all elements, recursively calling this function
                 * for each */
                for(elmt=0; elmt<nelmts; elmt++) {
                    /* Check vlen lengths */
                    if(((const hvl_t *)memb1)->len
                            != ((const hvl_t *)memb2)->len)
                        TEST_ERROR

                    /* Check vlen data */
                    if(!compare_data(parent1, parent2, pid, base_id,
                            ((const hvl_t *)memb1)->len,
                            ((const hvl_t *)memb1)->p,
                            ((const hvl_t *)memb2)->p, obj_owner))
                        TEST_ERROR

                    /* Update member pointers */
                    memb1 += elmt_size;
                    memb2 += elmt_size;
                } /* end for */
            } else {
                /* vlens cannot currently be nested below the top layer of a
                 * compound */
                HDassert(H5Tdetect_class(memb_id, H5T_VLEN) == FALSE);

                /* Iterate over all elements, calling memcmp() for each */
                for(elmt=0; elmt<nelmts; elmt++) {
                    if(HDmemcmp(memb1, memb2, memb_size))
                        TEST_ERROR

                    /* Update member pointers */
                    memb1 += elmt_size;
                    memb2 += elmt_size;
                } /* end for */
            } /* end else */
        } /* end for */
    } else if(H5Tdetect_class(tid, H5T_VLEN) == TRUE) {
        const hvl_t *vl_buf1, *vl_buf2; /* Aliases for buffers to compare */
        hid_t base_tid;                 /* Base type of vlen datatype */
        size_t u;                       /* Local index variable */

        /* Check for "simple" vlen datatype */
        if(H5Tget_class(tid) != H5T_VLEN) TEST_ERROR

        /* Get base type of vlen datatype */
        if((base_tid = H5Tget_super(tid)) < 0) TEST_ERROR

        /* Loop over elements in buffers */
        vl_buf1 = (const hvl_t *)buf1;
        vl_buf2 = (const hvl_t *)buf2;
        for(u = 0; u < nelmts; u++, vl_buf1++, vl_buf2++) {
            /* Check vlen lengths */
            if(vl_buf1->len != vl_buf2->len) TEST_ERROR

            /* Check vlen data */
            if(!compare_data(parent1, parent2, pid, base_tid, vl_buf1->len, vl_buf1->p, vl_buf2->p, obj_owner)) TEST_ERROR
        } /* end for */

        if(H5Tclose(base_tid) < 0) TEST_ERROR
    } /* end if */
    else if(H5Tdetect_class(tid, H5T_REFERENCE) == TRUE) {
        size_t u;                       /* Local index variable */

        /* Check for "simple" reference datatype */
        if(H5Tget_class(tid) != H5T_REFERENCE) TEST_ERROR

        /* Check for object or region reference */
        if(H5Tequal(tid, H5T_STD_REF_OBJ) > 0) {
            const hobj_ref_t *ref_buf1, *ref_buf2;      /* Aliases for buffers to compare */

            /* Loop over elements in buffers */
            ref_buf1 = (const hobj_ref_t *)buf1;
            ref_buf2 = (const hobj_ref_t *)buf2;
            for(u = 0; u < nelmts; u++, ref_buf1++, ref_buf2++) {
                hid_t obj1_id, obj2_id;         /* IDs for objects referenced */
                H5O_type_t obj1_type, obj2_type; /* Types of objects referenced */

                /* Check for types of objects handled */
                if(H5Rget_obj_type2(parent1, H5R_OBJECT, ref_buf1, &obj1_type) < 0) TEST_ERROR
                if(H5Rget_obj_type2(parent2, H5R_OBJECT, ref_buf2, &obj2_type) < 0) TEST_ERROR
                if(obj1_type != obj2_type) TEST_ERROR

                /* Open referenced objects */
                if((obj1_id = H5Rdereference(parent1, H5R_OBJECT, ref_buf1)) < 0) TEST_ERROR
                if((obj2_id = H5Rdereference(parent2, H5R_OBJECT, ref_buf2)) < 0) TEST_ERROR

                /* break the infinite loop when the ref_object points to itself */
                if(obj_owner > 0) {
                    H5O_info_t oinfo1, oinfo2;

                    if(H5Oget_info(obj_owner, &oinfo1) < 0) TEST_ERROR
                    if(H5Oget_info(obj1_id, &oinfo2) < 0) TEST_ERROR
                    if(H5F_addr_eq(oinfo1.addr, oinfo2.addr)) {
                        if(H5Oclose(obj1_id) < 0) TEST_ERROR
                        if(H5Oclose(obj2_id) < 0) TEST_ERROR
                        return TRUE;
                    }
                }

                /* Check for types of objects handled */
                switch(obj1_type) {
                    case H5O_TYPE_DATASET:
                        if(compare_datasets(obj1_id, obj2_id, pid, NULL) != TRUE) TEST_ERROR
                        break;

                    case H5O_TYPE_GROUP:
                        if(compare_groups(obj1_id, obj2_id, pid, -1, 0) != TRUE) TEST_ERROR
                        break;

                    case H5O_TYPE_NAMED_DATATYPE:
                        if(H5Tequal(obj1_id, obj2_id) != TRUE) TEST_ERROR
                        break;

                    case H5O_TYPE_UNKNOWN:
                    case H5O_TYPE_NTYPES:
                    default:
                        TEST_ERROR
                } /* end switch */

                /* Close objects */
                if(H5Oclose(obj1_id) < 0) TEST_ERROR
                if(H5Oclose(obj2_id) < 0) TEST_ERROR
            } /* end for */
        } /* end if */
        else if(H5Tequal(tid, H5T_STD_REF_DSETREG) > 0) {
            const hdset_reg_ref_t *ref_buf1, *ref_buf2;      /* Aliases for buffers to compare */

            /* Loop over elements in buffers */
            ref_buf1 = (const hdset_reg_ref_t *)buf1;
            ref_buf2 = (const hdset_reg_ref_t *)buf2;
            for(u = 0; u < nelmts; u++, ref_buf1++, ref_buf2++) {
                hid_t obj1_id, obj2_id;         /* IDs for objects referenced */
                hid_t obj1_sid, obj2_sid;       /* Dataspace IDs for objects referenced */
                H5O_type_t obj1_type, obj2_type; /* Types of objects referenced */

                /* Check for types of objects handled */
                if(H5Rget_obj_type2(parent1, H5R_DATASET_REGION, ref_buf1, &obj1_type) < 0) TEST_ERROR
                if(H5Rget_obj_type2(parent2, H5R_DATASET_REGION, ref_buf2, &obj2_type) < 0) TEST_ERROR
                if(obj1_type != obj2_type) TEST_ERROR

                /* Open referenced objects */
                if((obj1_id = H5Rdereference(parent1, H5R_DATASET_REGION, ref_buf1)) < 0) TEST_ERROR
                if((obj2_id = H5Rdereference(parent2, H5R_DATASET_REGION, ref_buf2)) < 0) TEST_ERROR

                /* break the infinite loop when the ref_object points to itself */
                if(obj_owner > 0) {
                    H5O_info_t oinfo1, oinfo2;

                    if(H5Oget_info(obj_owner, &oinfo1) < 0) TEST_ERROR
                    if(H5Oget_info(obj1_id, &oinfo2) < 0) TEST_ERROR
                    if(H5F_addr_eq(oinfo1.addr, oinfo2.addr)) {
                        if(H5Oclose(obj1_id) < 0) TEST_ERROR
                        if(H5Oclose(obj2_id) < 0) TEST_ERROR
                        return TRUE;
                    }
                }

                /* Check for types of objects handled */
                switch(obj1_type) {
                    case H5O_TYPE_DATASET:
                        if(compare_datasets(obj1_id, obj2_id, pid, NULL) != TRUE) TEST_ERROR
                        break;

                    case H5O_TYPE_GROUP:
                        if(compare_groups(obj1_id, obj2_id, pid, -1, 0) != TRUE) TEST_ERROR
                        break;

                    case H5O_TYPE_NAMED_DATATYPE:
                        if(H5Tequal(obj1_id, obj2_id) != TRUE) TEST_ERROR
                        break;

                    case H5O_TYPE_UNKNOWN:
                    case H5O_TYPE_NTYPES:
                    default:
                        TEST_ERROR
                } /* end switch */

                /* Close objects */
                if(H5Oclose(obj1_id) < 0) TEST_ERROR
                if(H5Oclose(obj2_id) < 0) TEST_ERROR

                /* Get regions for referenced datasets */
                if((obj1_sid = H5Rget_region(parent1, H5R_DATASET_REGION, ref_buf1)) < 0) TEST_ERROR
                if((obj2_sid = H5Rget_region(parent2, H5R_DATASET_REGION, ref_buf2)) < 0) TEST_ERROR

                /* Check if dataspaces are the same shape */
                if(H5S_select_shape_same_test(obj1_sid, obj2_sid) < 0) TEST_ERROR

                /* Close dataspaces */
                if(H5Sclose(obj1_sid) < 0) TEST_ERROR
                if(H5Sclose(obj2_sid) < 0) TEST_ERROR
            } /* end for */
        } /* end if */
        else
            TEST_ERROR
    } /* end else */
    else
        if(HDmemcmp(buf1, buf2, (elmt_size * nelmts))) TEST_ERROR

    /* Data should be the same. :-) */
    return TRUE;

error:
    return FALSE;
} /* end compare_data() */


/*-------------------------------------------------------------------------
 * Function:    compare_datasets
 *
 * Purpose:     Compare two datasets to check that they are equal
 *
 * Return:	TRUE if datasets are equal/FALSE if they are different
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, October 25, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
compare_datasets(hid_t did, hid_t did2, hid_t pid, const void *wbuf)
{
    hid_t sid = -1, sid2 = -1;                  /* Dataspace IDs */
    hid_t tid = -1, tid2 = -1;                  /* Datatype IDs */
    hid_t dcpl = -1, dcpl2 = -1;                /* Dataset creation property list IDs */
    size_t elmt_size;                           /* Size of datatype */
    htri_t is_committed;                        /* If the datatype is committed */
    htri_t is_committed2;                       /* If the datatype is committed */
    int ext_count;                              /* Number of external files in plist */
    int nfilters;                               /* Number of filters applied to dataset */
    hssize_t nelmts;                            /* # of elements in dataspace */
    void *rbuf = NULL;                          /* Buffer for reading raw data */
    void *rbuf2 = NULL;                         /* Buffer for reading raw data */
    H5D_space_status_t space_status;            /* Dataset's raw data space status */
    H5D_space_status_t space_status2;           /* Dataset's raw data space status */

    /* Check the datatypes are equal */

    /* Open the datatype for the source dataset */
    if((tid = H5Dget_type(did)) < 0) TEST_ERROR

    /* Open the datatype for the destination dataset */
    if((tid2 = H5Dget_type(did2)) < 0) TEST_ERROR

    /* Check that both datatypes are committed/not committed */
    if((is_committed = H5Tcommitted(tid)) < 0) TEST_ERROR
    if((is_committed2 = H5Tcommitted(tid2)) < 0) TEST_ERROR
    if(is_committed != is_committed2) TEST_ERROR

    /* Compare the datatypes */
    if(H5Tequal(tid, tid2) != TRUE) TEST_ERROR

    /* Determine the size of datatype (for later) */
    if((elmt_size = H5Tget_size(tid)) == 0) TEST_ERROR


    /* Check the dataspaces are equal */

    /* Open the dataspace for the source dataset */
    if((sid = H5Dget_space(did)) < 0) TEST_ERROR

    /* Open the dataspace for the destination dataset */
    if((sid2 = H5Dget_space(did2)) < 0) TEST_ERROR

    /* Compare the dataspaces */
    if(H5Sextent_equal(sid, sid2) != TRUE) TEST_ERROR

    /* Determine the number of elements in dataspace (for later) */
    if((nelmts = H5Sget_simple_extent_npoints(sid)) < 0) TEST_ERROR


    /* Check the dataset creation property lists are equal */

    /* Open the dataset creation property list for the source dataset */
    if((dcpl = H5Dget_create_plist(did)) < 0) TEST_ERROR

    /* Open the dataset creation property list for the destination dataset */
    if((dcpl2 = H5Dget_create_plist(did2)) < 0) TEST_ERROR

    /* If external file storage is being used, the value stored in the
     * dcpl will be a heap ID, which is not guaranteed to be the same in
     * source and destination files.
     * Instead, compare the actual external file values and then
     * delete this property from the dcpls before comparing them.
     */
    if((ext_count = H5Pget_external_count(dcpl)) < 0) TEST_ERROR

    if(ext_count > 0)
    {
        unsigned x;  /* Counter varaible */
        char name1[NAME_BUF_SIZE];
        char name2[NAME_BUF_SIZE];
        off_t offset1=0;
        off_t offset2=0;
        hsize_t size1=0;
        hsize_t size2=0;

        if(H5Pget_external_count(dcpl2) != ext_count) TEST_ERROR

        /* Ensure that all external file information is the same */
        for(x=0; x < (unsigned) ext_count; ++x)
        {
            if(H5Pget_external(dcpl, x, (size_t)NAME_BUF_SIZE, name1, &offset1, &size1) < 0) TEST_ERROR
            if(H5Pget_external(dcpl2, x, (size_t)NAME_BUF_SIZE, name2, &offset2, &size2) < 0) TEST_ERROR

            if(offset1 != offset2) TEST_ERROR
            if(size1 != size2) TEST_ERROR
            if(strcmp(name1, name2) != 0) TEST_ERROR
        }

        /* Remove external file information from the dcpls */
        /* Remove default property causes memory leak
        if(H5Premove(dcpl, H5D_CRT_EXT_FILE_LIST_NAME) < 0) TEST_ERROR
        if(H5Premove(dcpl2, H5D_CRT_EXT_FILE_LIST_NAME) < 0) TEST_ERROR
        */

        /* reset external file information from the dcpls */
        if (H5P_reset_external_file_test(dcpl) < 0) TEST_ERROR
        if (H5P_reset_external_file_test(dcpl2) < 0) TEST_ERROR
    }

    /* Compare the rest of the dataset creation property lists */
    if(H5Pequal(dcpl, dcpl2) != TRUE) TEST_ERROR

    /* Get the number of filters on dataset */
    if((nfilters = H5Pget_nfilters(dcpl)) < 0) TEST_ERROR

    /* close the source dataset creation property list */
    if(H5Pclose(dcpl) < 0) TEST_ERROR

    /* close the destination dataset creation property list */
    if(H5Pclose(dcpl2) < 0) TEST_ERROR


    /* Check the allocated storage is the same */

    /* Check that the space allocation status is the same */
    if(H5Dget_space_status(did, &space_status) < 0) TEST_ERROR
    if(H5Dget_space_status(did2, &space_status2) < 0) TEST_ERROR
    if(space_status != space_status2) TEST_ERROR

    /* Check that the space used is the same */
    /* (Don't check if the dataset is filtered (i.e. compressed, etc.) and
     *  the datatype is VLEN, since the addresses for the vlen
     *  data in each dataset will (probably) be different and the storage
     *  size will thus vary)
     */
    if(!(nfilters > 0 && H5Tdetect_class(tid, H5T_VLEN))) {
        hsize_t storage_size = H5Dget_storage_size(did);        /* Dataset's raw data storage size */
        hsize_t storage_size2 = H5Dget_storage_size(did2);      /* 2nd Dataset's raw data storage size */

        if(storage_size != storage_size2) TEST_ERROR
    } /* end if */

    /* Check the raw data is equal */

    /* Allocate & initialize space for the raw data buffers */
    if((rbuf = HDcalloc( elmt_size, (size_t)nelmts)) == NULL) TEST_ERROR
    if((rbuf2 = HDcalloc( elmt_size, (size_t)nelmts)) == NULL) TEST_ERROR

    /* Read data from datasets */
    if(H5Dread(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0) TEST_ERROR
    if(H5Dread(did2, tid2, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf2) < 0) TEST_ERROR

    /* Check raw data read in against data written out */
    if(wbuf) {
        if(!compare_data(did, 0, pid, tid, (size_t)nelmts, wbuf, rbuf, did)) TEST_ERROR
        if(!compare_data(did2, 0, pid, tid2, (size_t)nelmts, wbuf, rbuf2, did2)) TEST_ERROR
    } /* end if */
    /* Don't have written data, just compare data between the two datasets */
    else
        if(!compare_data(did, did2, pid, tid, (size_t)nelmts, rbuf, rbuf2, did)) TEST_ERROR

    /* Reclaim vlen data, if necessary */
    if(H5Tdetect_class(tid, H5T_VLEN) == TRUE)
        if(H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, rbuf) < 0) TEST_ERROR
    if(H5Tdetect_class(tid2, H5T_VLEN) == TRUE)
        if(H5Dvlen_reclaim(tid2, sid2, H5P_DEFAULT, rbuf2) < 0) TEST_ERROR

    /* Release raw data buffers */
    HDfree(rbuf);
    HDfree(rbuf2);

    /* close the source dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    /* close the destination dataspace */
    if(H5Sclose(sid2) < 0) TEST_ERROR

    /* close the source datatype */
    if(H5Tclose(tid) < 0) TEST_ERROR

    /* close the destination datatype */
    if(H5Tclose(tid2) < 0) TEST_ERROR


    /* Check if the attributes are equal */
    if(compare_std_attributes(did, did2, pid) != TRUE) TEST_ERROR


    /* Datasets should be the same. :-) */
    return TRUE;

error:
    H5E_BEGIN_TRY {
        if(rbuf)
            HDfree(rbuf);
        if(rbuf2)
            HDfree(rbuf2);
    	H5Pclose(dcpl2);
    	H5Pclose(dcpl);
    	H5Sclose(sid2);
    	H5Sclose(sid);
    	H5Tclose(tid2);
    	H5Tclose(tid);
    } H5E_END_TRY;
    return FALSE;
} /* end compare_datasets() */


/*-------------------------------------------------------------------------
 * Function:    compare_groups
 *
 * Purpose:     Compare two groups to check that they are "equal"
 *
 * Return:	TRUE if group are equal/FALSE if they are different
 *
 * Programmer:  Quincey Koziol
 *              Monday, October 31, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
compare_groups(hid_t gid, hid_t gid2, hid_t pid, int depth, unsigned copy_flags)
{
    H5G_info_t ginfo;           /* Group info struct */
    H5G_info_t ginfo2;          /* Group info struct */
    hsize_t idx;                /* Index over the objects in group */
    unsigned cpy_flags;         /* Object copy flags */

    /* Retrieve the object copy flags from the property list, if it's non-DEFAULT */
    if(pid != H5P_DEFAULT) {
        if(H5Pget_copy_object(pid, &cpy_flags) < 0) TEST_ERROR
    } /* end if */
    else
        cpy_flags = 0;

    /* Check if both groups have the same # of objects */
    if(H5Gget_info(gid, &ginfo) < 0) TEST_ERROR
    if(H5Gget_info(gid2, &ginfo2) < 0) TEST_ERROR
    if((cpy_flags & H5O_COPY_SHALLOW_HIERARCHY_FLAG) && depth == 0) {
        if(ginfo2.nlinks != 0) TEST_ERROR
    } /* end if */
    else {
        if(ginfo.nlinks != ginfo2.nlinks) TEST_ERROR
    } /* end if */

    /* Check contents of groups */
    if(ginfo2.nlinks > 0) {
        char objname[NAME_BUF_SIZE];            /* Name of object in group */
        char objname2[NAME_BUF_SIZE];           /* Name of object in group */
        H5L_info_t linfo;                       /* Link information */
        H5L_info_t linfo2;                      /* Link information */

        /* Loop over contents of groups */
        for(idx = 0; idx < ginfo.nlinks; idx++) {
            /* Check name of objects */
            if(H5Lget_name_by_idx(gid, ".", H5_INDEX_NAME, H5_ITER_INC, idx, objname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
            if(H5Lget_name_by_idx(gid2, ".", H5_INDEX_NAME, H5_ITER_INC, idx, objname2, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
            if(HDstrcmp(objname, objname2)) TEST_ERROR

            /* Get link info */
            if(H5Lget_info(gid, objname, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
            if(H5Lget_info(gid2, objname2, &linfo2, H5P_DEFAULT) < 0) TEST_ERROR
            if(linfo.type != linfo2.type) TEST_ERROR

            /* Extra checks for "real" objects */
            if(linfo.type == H5L_TYPE_HARD) {
                hid_t oid, oid2;                /* IDs of objects within group */
                H5O_info_t oinfo, oinfo2;       /* Object info */

                /* Compare some pieces of the object info */
                if(H5Oget_info_by_name(gid, objname, &oinfo, H5P_DEFAULT) < 0) TEST_ERROR
                if(H5Oget_info_by_name(gid2, objname2, &oinfo2, H5P_DEFAULT) < 0) TEST_ERROR

                if(oinfo.type != oinfo2.type) TEST_ERROR
                if(oinfo.rc != oinfo2.rc) TEST_ERROR

                /* If NULL messages are preserved, the number of messages
                 * should be the same in the destination.
                 * Otherwise, it should simply be true that the number
                 * of messages hasn't increased.
                 */
                 if(H5O_COPY_PRESERVE_NULL_FLAG & copy_flags) {
                    if(oinfo.hdr.nmesgs != oinfo2.hdr.nmesgs)
                        ;
                    else
                        if(oinfo.hdr.nmesgs < oinfo2.hdr.nmesgs) TEST_ERROR
                 }

                /* Check for object already having been compared */
                if(addr_lookup(&oinfo))
                    continue;
                else
                    addr_insert(&oinfo);

                /* Open objects */
                if((oid = H5Oopen(gid, objname, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
                if((oid2 = H5Oopen(gid2, objname2, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

                /* Compare objects within group */
                switch(oinfo.type) {
                    case H5O_TYPE_GROUP:
                        /* Compare groups */
                        if(compare_groups(oid, oid2, pid, depth - 1, copy_flags) != TRUE) TEST_ERROR
                        break;

                    case H5O_TYPE_DATASET:
                        /* Compare datasets */
                        if(compare_datasets(oid, oid2, pid, NULL) != TRUE) TEST_ERROR
                        break;

                    case H5O_TYPE_NAMED_DATATYPE:
                        /* Compare datatypes */
                        if(H5Tequal(oid, oid2) != TRUE) TEST_ERROR
                        break;

                    case H5O_TYPE_UNKNOWN:
                    case H5O_TYPE_NTYPES:
                    default:
HDassert(0 && "Unknown type of object");
                        break;
                } /* end switch */

                /* Close objects */
                if(H5Oclose(oid) < 0) TEST_ERROR
                if(H5Oclose(oid2) < 0) TEST_ERROR
            } /* end if */
            else {
                /* Check that both links are the same size */
                if(linfo.u.val_size != linfo2.u.val_size) TEST_ERROR

                /* Compare link values */
                if(linfo.type == H5L_TYPE_SOFT ||
                        (linfo.type >= H5L_TYPE_UD_MIN && linfo.type <= H5L_TYPE_MAX)) {
                    char linkval[NAME_BUF_SIZE];            /* Link value */
                    char linkval2[NAME_BUF_SIZE];           /* Link value */

                    /* Get link values */
                    HDassert(linfo.u.val_size <= NAME_BUF_SIZE);
                    if(H5Lget_val(gid, objname, linkval, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
                    if(H5Lget_val(gid2, objname2, linkval2, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Compare link data */
                    if(HDmemcmp(linkval, linkval2, linfo.u.val_size)) TEST_ERROR
                } /* end else-if */
                else {
HDassert(0 && "Unknown type of link");
                } /* end else */
            } /* end else */
        } /* end for */
    } /* end if */

    /* Check if the attributes are equal */
    if(compare_std_attributes(gid, gid2, pid) != TRUE) TEST_ERROR

    /* Groups should be the same. :-) */
    return TRUE;

error:
    H5E_BEGIN_TRY {
    } H5E_END_TRY;
    return FALSE;
} /* end compare_groups() */


/*-------------------------------------------------------------------------
 * Function:    test_copy_named_datatype
 *
 * Purpose:     Create name datatype in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao
 *              Friday, September 30, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_named_datatype(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;   /* File IDs */
    hid_t tid = -1, tid2 = -1;          /* Datatype IDs */
    char		src_filename[NAME_BUF_SIZE];
    char		dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): named datatype");

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* create datatype */
    if((tid = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR

    /* create named datatype */
    if((H5Tcommit2(fid_src, NAME_DATATYPE_SIMPLE, tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* close the datatype */
    if(H5Tclose(tid) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the datatype from SRC to DST */
    if(H5Ocopy(fid_src, NAME_DATATYPE_SIMPLE, fid_dst, NAME_DATATYPE_SIMPLE, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the datatype for copy */
    if((tid = H5Topen2(fid_src, NAME_DATATYPE_SIMPLE, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* open the copied datatype */
    if((tid2 = H5Topen2(fid_dst, NAME_DATATYPE_SIMPLE, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Compare the datatypes */
    if(H5Tequal(tid, tid2) != TRUE) TEST_ERROR

    /* close the destination datatype */
    if(H5Tclose(tid2) < 0) TEST_ERROR

    /* close the source datatype */
    if(H5Tclose(tid) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Tclose(tid2);
    	H5Tclose(tid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_named_datatype */


/*-------------------------------------------------------------------------
 * Function:    test_copy_named_datatype_vl
 *
 * Purpose:     Create name vlen datatype in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November 22, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_named_datatype_vl(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;   /* File IDs */
    hid_t tid = -1, tid2 = -1;          /* Datatype IDs */
    char		src_filename[NAME_BUF_SIZE];
    char		dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): named vlen datatype");

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* create datatype */
    if((tid = H5Tvlen_create(H5T_NATIVE_INT)) < 0) TEST_ERROR

    /* create named datatype */
    if((H5Tcommit2(fid_src, NAME_DATATYPE_VL, tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* close the datatype */
    if(H5Tclose(tid) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the datatype from SRC to DST */
    if(H5Ocopy(fid_src, NAME_DATATYPE_VL, fid_dst, NAME_DATATYPE_VL, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the datatype for copy */
    if((tid = H5Topen2(fid_src, NAME_DATATYPE_VL, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* open the copied datatype */
    if((tid2 = H5Topen2(fid_dst, NAME_DATATYPE_VL, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Compare the datatypes */
    if(H5Tequal(tid, tid2) != TRUE) TEST_ERROR

    /* close the destination datatype */
    if(H5Tclose(tid2) < 0) TEST_ERROR

    /* close the source datatype */
    if(H5Tclose(tid) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Tclose(tid2);
    	H5Tclose(tid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_named_datatype_vl */


/*-------------------------------------------------------------------------
 * Function:    test_copy_named_datatype_vl_vl
 *
 * Purpose:     Create named vlen of vlen datatype in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November 22, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_named_datatype_vl_vl(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;   /* File IDs */
    hid_t tid = -1, tid2 = -1;          /* Datatype IDs */
    char		src_filename[NAME_BUF_SIZE];
    char		dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): named nested vlen datatype");

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* create first vlen datatype */
    if((tid = H5Tvlen_create(H5T_NATIVE_INT)) < 0) TEST_ERROR

    /* create second (nested) vlen datatype */
    if((tid2 = H5Tvlen_create(tid)) < 0) TEST_ERROR

    /* create named datatype */
    if((H5Tcommit2(fid_src, NAME_DATATYPE_VL_VL, tid2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* close the first datatype */
    if(H5Tclose(tid) < 0) TEST_ERROR

    /* close the second datatype */
    if(H5Tclose(tid2) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the datatype from SRC to DST */
    if(H5Ocopy(fid_src, NAME_DATATYPE_VL_VL, fid_dst, NAME_DATATYPE_VL_VL, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the datatype for copy */
    if((tid = H5Topen2(fid_src, NAME_DATATYPE_VL_VL, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* open the copied datatype */
    if((tid2 = H5Topen2(fid_dst, NAME_DATATYPE_VL_VL, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Compare the datatypes */
    if(H5Tequal(tid, tid2) != TRUE) TEST_ERROR

    /* close the destination datatype */
    if(H5Tclose(tid2) < 0) TEST_ERROR

    /* close the source datatype */
    if(H5Tclose(tid) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Tclose(tid2);
    	H5Tclose(tid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_named_datatype_vl */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_simple
 *
 * Purpose:     Create a simple dataset in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao
 *              Friday, September 30, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_simple(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    int buf[DIM_SIZE_1][DIM_SIZE_2];            /* Buffer for writing data */
    hsize_t dim2d[2];                           /* Dataset dimensions */
    int i, j;                                   /* local index variables */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): simple dataset");

    /* Initialize write buffer */
    for (i=0; i<DIM_SIZE_1; i++)
        for (j=0; j<DIM_SIZE_2; j++)
            buf[i][j] = 10000 + 100*i+j;

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim2d[0] = DIM_SIZE_1;
    dim2d[1] = DIM_SIZE_2;

    /* create 2D dataspace */
    if((sid = H5Screate_simple(2, dim2d, NULL)) < 0) TEST_ERROR

    /* create 2D int dataset at SRC file */
    if((did = H5Dcreate2(fid_src, NAME_DATASET_SIMPLE, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    /* attach attributes to the dataset */
    if(test_copy_attach_attributes(did, H5T_NATIVE_INT) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST */
    if(H5Ocopy(fid_src, NAME_DATASET_SIMPLE, fid_dst, NAME_DATASET_SIMPLE, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the dataset for copy */
    if((did = H5Dopen2(fid_src, NAME_DATASET_SIMPLE, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_SIMPLE, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, buf) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_simple */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_simple_samefile
 *
 * Purpose:     Create a simple dataset in SRC file and copy it to SRC file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Neil Fortner
 *              Thursday, January 15, 2009
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_simple_samefile(hid_t fcpl, hid_t fapl)
{
    hid_t fid = -1;                             /* File ID */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    int buf[DIM_SIZE_1][DIM_SIZE_2];            /* Buffer for writing data */
    hsize_t dim2d[2];                           /* Dataset dimensions */
    int i, j;                                   /* local index variables */
    char filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): simple dataset within the same file");

    /* Initialize write buffer */
    for (i=0; i<DIM_SIZE_1; i++)
        for (j=0; j<DIM_SIZE_2; j++)
            buf[i][j] = 10000 + 100*i+j;

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim2d[0] = DIM_SIZE_1;
    dim2d[1] = DIM_SIZE_2;

    /* create 2D dataspace */
    if((sid = H5Screate_simple(2, dim2d, NULL)) < 0) TEST_ERROR

    /* create 2D int dataset at SRC file */
    if((did = H5Dcreate2(fid, NAME_DATASET_SIMPLE, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    /* attach attributes to the dataset */
    if(test_copy_attach_attributes(did, H5T_NATIVE_INT) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* open the source file with read-write */
    if((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST */
    if(H5Ocopy(fid, NAME_DATASET_SIMPLE, fid, NAME_DATASET_SIMPLE2, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the dataset for copy */
    if((did = H5Dopen2(fid, NAME_DATASET_SIMPLE, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid, NAME_DATASET_SIMPLE2, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, buf) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Sclose(sid);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_simple_samefile */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_simple_empty
 *
 * Purpose:     Create a simple dataset in SRC file and copy it to DST file
 *              (Note: dataset has no data)
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Monday, October 31, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_simple_empty(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    hsize_t dim2d[2];                           /* Dataset dimensions */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): empty contiguous dataset");

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim2d[0] = DIM_SIZE_1;
    dim2d[1] = DIM_SIZE_2;

    /* create 2D dataspace */
    if((sid = H5Screate_simple(2, dim2d, NULL)) < 0) TEST_ERROR

    /* create 2D int dataset at SRC file */
    if((did = H5Dcreate2(fid_src, NAME_DATASET_SIMPLE, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    /* attach attributes to the dataset */
    if(test_copy_attach_attributes(did, H5T_NATIVE_INT) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST */
    if(H5Ocopy(fid_src, NAME_DATASET_SIMPLE, fid_dst, NAME_DATASET_SIMPLE, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the dataset for copy */
    if((did = H5Dopen2(fid_src, NAME_DATASET_SIMPLE, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_SIMPLE, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, NULL) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_simple_empty */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_compound
 *
 * Purpose:     Create a compound dataset in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao
 *              Friday, September 30, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_compound(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t tid = -1;                             /* Datatype ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    hsize_t dim1d[1];                           /* Dataset dimensions */
    typedef struct comp_t {
        int a;
        double d;
    } comp_t;
    comp_t buf[DIM_SIZE_1];                     /* Buffer for writing data */
    int i;                                      /* Local index variable */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): compound dataset");

#ifdef H5_CLEAR_MEMORY
    HDmemset(buf, 0, sizeof(buf));
#endif /* H5_CLEAR_MEMORY */
    for(i = 0; i < DIM_SIZE_1; i++) {
        buf[i].a = i;
        buf[i].d = 1. / (i + 1);
    } /* end for */

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim1d[0] = DIM_SIZE_1;

    /* create dataspace */
    if((sid = H5Screate_simple(1, dim1d, NULL)) < 0) TEST_ERROR

    /* create datatype */
    if((tid = H5Tcreate(H5T_COMPOUND, sizeof(comp_t))) < 0) TEST_ERROR
    if(H5Tinsert(tid, "int_name", HOFFSET(comp_t, a), H5T_NATIVE_INT) < 0) TEST_ERROR
    if(H5Tinsert(tid, "double_name", HOFFSET(comp_t, d), H5T_NATIVE_DOUBLE) < 0) TEST_ERROR

    /* create dataset */
    if((did = H5Dcreate2(fid_src, NAME_DATASET_COMPOUND, tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    /* close datatype */
    if(H5Tclose(tid) < 0) TEST_ERROR

    /* attach attributes to the dataset */
    if(test_copy_attach_attributes(did, H5T_NATIVE_INT) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST */
    if(H5Ocopy(fid_src, NAME_DATASET_COMPOUND, fid_dst, NAME_DATASET_COMPOUND, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the dataset for copy */
    if((did = H5Dopen2(fid_src, NAME_DATASET_COMPOUND, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_COMPOUND, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, buf) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Tclose(tid);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_compound */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_chunked
 *
 * Purpose:     Create a chunked dataset in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao
 *              Friday, September 30, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_chunked(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t pid = -1;                             /* Dataset creation property list ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    hsize_t dim1d[1];                           /* Dataset dimensions */
    hsize_t max_dim1d[1];                       /* Dataset max. dimensions */
    hsize_t dim2d[2];                           /* Dataset dimensions */
    hsize_t chunk_dim1d[1] ={CHUNK_SIZE_1};     /* Chunk dimensions */
    hsize_t chunk_dim2d[2] ={CHUNK_SIZE_1, CHUNK_SIZE_2};             /* Chunk dimensions */
    float buf1d[DIM_SIZE_1];                    /* Buffer for writing data */
    float buf2d[DIM_SIZE_1][DIM_SIZE_2];        /* Buffer for writing data */
    int i, j;                                   /* Local index variables */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): chunked dataset");

    /* set initial data values */
    for(i = 0; i < DIM_SIZE_1; i++) {
        buf1d[i] = (float)(i / 2.0);
        for(j = 0; j < DIM_SIZE_2; j++)
            buf2d[i][j] = (float)(i + (j / 100.0));
    } /* end for */

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* Set 1-D dataspace dimensions */
    dim1d[0] = DIM_SIZE_1;
    max_dim1d[0] = H5S_UNLIMITED;

    /* create 1-D dataspace */
    if((sid = H5Screate_simple(1, dim1d, max_dim1d)) < 0) TEST_ERROR

    /* create and set chunk plist */
    if((pid = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR
    if(H5Pset_chunk(pid, 1, chunk_dim1d) < 0) TEST_ERROR

    /* create dataset */
    if((did = H5Dcreate2(fid_src, NAME_DATASET_CHUNKED, H5T_NATIVE_FLOAT, sid, H5P_DEFAULT, pid, H5P_DEFAULT)) < 0) TEST_ERROR

    /* close chunk plist */
    if(H5Pclose(pid) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf1d) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    /* attach attributes to the dataset */
    if(test_copy_attach_attributes(did, H5T_NATIVE_INT) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* Set 2-D dataspace dimensions */
    dim2d[0] = DIM_SIZE_1;
    dim2d[1] = DIM_SIZE_2;

    /* create 2-D dataspace */
    if((sid = H5Screate_simple(2, dim2d, NULL)) < 0) TEST_ERROR

    /* create and set chunk plist */
    if((pid = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR
    if(H5Pset_chunk(pid, 2, chunk_dim2d) < 0) TEST_ERROR

    /* create dataset */
    if((did = H5Dcreate2(fid_src, NAME_DATASET_CHUNKED2, H5T_NATIVE_FLOAT, sid, H5P_DEFAULT, pid, H5P_DEFAULT)) < 0) TEST_ERROR

    /* close chunk plist */
    if(H5Pclose(pid) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf2d) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    /* attach attributes to the dataset */
    if(test_copy_attach_attributes(did, H5T_NATIVE_INT) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the datasets from SRC to DST */
    if(H5Ocopy(fid_src, NAME_DATASET_CHUNKED, fid_dst, NAME_DATASET_CHUNKED, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Ocopy(fid_src, NAME_DATASET_CHUNKED2, fid_dst, NAME_DATASET_CHUNKED2, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the dataset for copy */
    if((did = H5Dopen2(fid_src, NAME_DATASET_CHUNKED, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the 1-D destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_CHUNKED, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, buf1d) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* open the 2-D dataset for copy */
    if((did = H5Dopen2(fid_src, NAME_DATASET_CHUNKED2, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_CHUNKED2, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, buf2d) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Pclose(pid);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_chunked */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_chunked_empty
 *
 * Purpose:     Create a chunked dataset in SRC file and copy it to DST file
 *              (Note: dataset has no data)
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Monday, October 31, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_chunked_empty(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t pid = -1;                             /* Dataset creation property list ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    hsize_t dim1d[1];                           /* Dataset dimensions */
    hsize_t max_dim1d[1];                       /* Dataset max. dimensions */
    hsize_t dim2d[2];                           /* Dataset dimensions */
    hsize_t chunk_dim1d[1] ={CHUNK_SIZE_1};     /* Chunk dimensions */
    hsize_t chunk_dim2d[2] ={CHUNK_SIZE_1, CHUNK_SIZE_2};             /* Chunk dimensions */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): empty chunked dataset");

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* Set 1-D dataspace dimensions */
    dim1d[0] = DIM_SIZE_1;
    max_dim1d[0] = H5S_UNLIMITED;

    /* create 1-D dataspace */
    if((sid = H5Screate_simple(1, dim1d, max_dim1d)) < 0) TEST_ERROR

    /* create and set chunk plist */
    if((pid = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR
    if(H5Pset_chunk(pid, 1, chunk_dim1d) < 0) TEST_ERROR

    /* create dataset */
    if((did = H5Dcreate2(fid_src, NAME_DATASET_CHUNKED, H5T_NATIVE_FLOAT, sid, H5P_DEFAULT, pid, H5P_DEFAULT)) < 0) TEST_ERROR

    /* close chunk plist */
    if(H5Pclose(pid) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    /* attach attributes to the dataset */
    if(test_copy_attach_attributes(did, H5T_NATIVE_INT) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* Set 2-D dataspace dimensions */
    dim2d[0] = DIM_SIZE_1;
    dim2d[1] = DIM_SIZE_2;

    /* create 2-D dataspace */
    if((sid = H5Screate_simple(2, dim2d, NULL)) < 0) TEST_ERROR

    /* create and set chunk plist */
    if((pid = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR
    if(H5Pset_chunk(pid, 2, chunk_dim2d) < 0) TEST_ERROR

    /* create dataset */
    if((did = H5Dcreate2(fid_src, NAME_DATASET_CHUNKED2, H5T_NATIVE_FLOAT, sid, H5P_DEFAULT, pid, H5P_DEFAULT)) < 0) TEST_ERROR

    /* close chunk plist */
    if(H5Pclose(pid) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    /* attach attributes to the dataset */
    if(test_copy_attach_attributes(did, H5T_NATIVE_INT) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the datasets from SRC to DST */
    if(H5Ocopy(fid_src, NAME_DATASET_CHUNKED, fid_dst, NAME_DATASET_CHUNKED, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Ocopy(fid_src, NAME_DATASET_CHUNKED2, fid_dst, NAME_DATASET_CHUNKED2, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the dataset for copy */
    if((did = H5Dopen2(fid_src, NAME_DATASET_CHUNKED, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_CHUNKED, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, NULL) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* open the dataset for copy */
    if((did = H5Dopen2(fid_src, NAME_DATASET_CHUNKED2, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_CHUNKED2, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, NULL) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Pclose(pid);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_chunked_empty */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_chunked_sparse
 *
 * Purpose:     Create a chunked dataset with unlimited dimensions and un-written
 *              chunks in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Monday, October 31, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_chunked_sparse(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t pid = -1;                             /* Dataset creation property list ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    hsize_t dim1d[1];                           /* Dataset dimensions */
    hsize_t new_dim1d[1];                       /* Dataset dimensions */
    hsize_t max_dim1d[1];                       /* Dataset max. dimensions */
    hsize_t dim2d[2];                           /* Dataset dimensions */
    hsize_t new_dim2d[2];                       /* Dataset dimensions */
    hsize_t max_dim2d[2];                       /* Dataset max. dimensions */
    hsize_t chunk_dim1d[1] = {CHUNK_SIZE_1};    /* Chunk dimensions */
    hsize_t chunk_dim2d[2] = {CHUNK_SIZE_1, CHUNK_SIZE_2};             /* Chunk dimensions */
    float buf1d[DIM_SIZE_1];                    /* Buffer for writing data */
    float buf2d[DIM_SIZE_1][DIM_SIZE_2];        /* Buffer for writing data */
    int i, j;                                   /* Local index variables */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): sparse dataset");

    /* set initial data values */
    for(i = 0; i < DIM_SIZE_1; i++) {
        buf1d[i] = (float)(i / 10.0);
        for(j = 0; j < DIM_SIZE_2; j++)
            buf2d[i][j] = (float)(i + (j / 100.0));
    } /* end for */

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* Set 1-D dataspace dimensions */
    dim1d[0]=DIM_SIZE_1;
    max_dim1d[0]=H5S_UNLIMITED;

    /* create 1-D dataspace */
    if((sid = H5Screate_simple(1, dim1d, max_dim1d)) < 0) TEST_ERROR

    /* create and set chunk plist */
    if((pid = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR
    if(H5Pset_chunk(pid, 1, chunk_dim1d) < 0) TEST_ERROR

    /* create dataset */
    if((did = H5Dcreate2(fid_src, NAME_DATASET_CHUNKED, H5T_NATIVE_FLOAT, sid, H5P_DEFAULT, pid, H5P_DEFAULT)) < 0) TEST_ERROR

    /* close chunk plist */
    if(H5Pclose(pid) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf1d) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    /* Set extended dataset dimensions */
    new_dim1d[0] = DIM_SIZE_1 * 2;

    /* Extend dataset's dimensions */
    if(H5Dset_extent(did, new_dim1d) < 0) TEST_ERROR

    /* attach attributes to the dataset */
    if(test_copy_attach_attributes(did, H5T_NATIVE_INT) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* Set 2-D dataspace dimensions */
    dim2d[0]=DIM_SIZE_1;
    dim2d[1]=DIM_SIZE_2;
    max_dim2d[0]=H5S_UNLIMITED;
    max_dim2d[1]=H5S_UNLIMITED;

    /* create 2-D dataspace */
    if((sid = H5Screate_simple(2, dim2d, max_dim2d)) < 0) TEST_ERROR

    /* create and set chunk plist */
    if((pid = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR
    if(H5Pset_chunk(pid, 2, chunk_dim2d) < 0) TEST_ERROR

    /* create dataset */
    if((did = H5Dcreate2(fid_src, NAME_DATASET_CHUNKED2, H5T_NATIVE_FLOAT, sid, H5P_DEFAULT, pid, H5P_DEFAULT)) < 0) TEST_ERROR

    /* close chunk plist */
    if(H5Pclose(pid) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf2d) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    /* Set extended dataset dimensions */
    new_dim2d[0] = DIM_SIZE_1 * 2;
    new_dim2d[1] = DIM_SIZE_2 * 2;

    /* Extend dataset's dimensions */
    if(H5Dset_extent(did, new_dim2d) < 0) TEST_ERROR

    /* attach attributes to the dataset */
    if(test_copy_attach_attributes(did, H5T_NATIVE_INT) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the datasets from SRC to DST */
    if(H5Ocopy(fid_src, NAME_DATASET_CHUNKED, fid_dst, NAME_DATASET_CHUNKED, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Ocopy(fid_src, NAME_DATASET_CHUNKED2, fid_dst, NAME_DATASET_CHUNKED2, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the dataset for copy */
    if((did = H5Dopen2(fid_src, NAME_DATASET_CHUNKED, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_CHUNKED, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, NULL) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* open the dataset for copy */
    if((did = H5Dopen2(fid_src, NAME_DATASET_CHUNKED2, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_CHUNKED2, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, NULL) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Pclose(pid);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_chunked_sparse */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_compressed
 *
 * Purpose:     Create a compressed, chunked dataset in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Monday, October 31, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_compressed(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
#ifdef H5_HAVE_FILTER_DEFLATE
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t pid = -1;                             /* Dataset creation property list ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    hsize_t dim2d[2];                           /* Dataset dimensions */
    hsize_t chunk_dim2d[2] ={CHUNK_SIZE_1, CHUNK_SIZE_2};             /* Chunk dimensions */
    float buf[DIM_SIZE_1][DIM_SIZE_2];          /* Buffer for writing data */
    int i, j;                                   /* Local index variables */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];
#endif /* H5_HAVE_FILTER_DEFLATE */

    TESTING("H5Ocopy(): compressed dataset");

#ifndef H5_HAVE_FILTER_DEFLATE
    SKIPPED();
    puts("    Deflation filter not available");
#else /* H5_HAVE_FILTER_DEFLATE */
    /* set initial data values */
    for (i=0; i<DIM_SIZE_1; i++)
        for (j=0; j<DIM_SIZE_2; j++)
            buf[i][j] = (float)(100.0);         /* Something easy to compress */

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim2d[0]=DIM_SIZE_1;
    dim2d[1]=DIM_SIZE_2;

    /* create dataspace */
    if((sid = H5Screate_simple(2, dim2d, NULL)) < 0) TEST_ERROR

    /* create and set comp & chunk plist */
    if((pid = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR
    if(H5Pset_chunk(pid, 2, chunk_dim2d) < 0) TEST_ERROR
    if(H5Pset_deflate(pid, 9) < 0) TEST_ERROR

    /* create dataset */
    if((did = H5Dcreate2(fid_src, NAME_DATASET_CHUNKED, H5T_NATIVE_FLOAT, sid, H5P_DEFAULT, pid, H5P_DEFAULT)) < 0) TEST_ERROR

    /* close chunk plist */
    if(H5Pclose(pid) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    /* attach attributes to the dataset */
    if(test_copy_attach_attributes(did, H5T_NATIVE_INT) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST */
    if(H5Ocopy(fid_src, NAME_DATASET_CHUNKED, fid_dst, NAME_DATASET_CHUNKED, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the dataset for copy */
    if((did = H5Dopen2(fid_src, NAME_DATASET_CHUNKED, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_CHUNKED, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, NULL) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    PASSED();
#endif /* H5_HAVE_FILTER_DEFLATE */
    return 0;

#ifdef H5_HAVE_FILTER_DEFLATE
error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Pclose(pid);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
#endif /* H5_HAVE_FILTER_DEFLATE */
} /* end test_copy_dataset_compressed */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_compact
 *
 * Purpose:     Create a compact dataset in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Monday, October 31, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_compact(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t pid = -1;                             /* Dataset creation property list ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    hsize_t dim2d[2];                           /* Dataset dimensions */
    float buf[DIM_SIZE_1][DIM_SIZE_2];          /* Buffer for writing data */
    int i, j;                                   /* Local index variables */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): compact dataset");

    /* set initial data values */
    for (i=0; i<DIM_SIZE_1; i++)
        for (j=0; j<DIM_SIZE_2; j++)
            buf[i][j] = (float)(i+j/100.0);

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim2d[0]=DIM_SIZE_1;
    dim2d[1]=DIM_SIZE_2;

    /* create dataspace */
    if((sid = H5Screate_simple(2, dim2d, NULL)) < 0) TEST_ERROR

    /* create and set compact plist */
    if((pid = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR
    if(H5Pset_layout(pid, H5D_COMPACT) < 0) TEST_ERROR

    /* create dataset */
    if((did = H5Dcreate2(fid_src, NAME_DATASET_COMPACT, H5T_NATIVE_FLOAT, sid, H5P_DEFAULT, pid, H5P_DEFAULT)) < 0) TEST_ERROR

    /* close chunk plist */
    if(H5Pclose(pid) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    /* attach attributes to the dataset */
    if(test_copy_attach_attributes(did, H5T_NATIVE_INT) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST */
    if(H5Ocopy(fid_src, NAME_DATASET_COMPACT, fid_dst, NAME_DATASET_COMPACT, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the dataset for copy */
    if((did = H5Dopen2(fid_src, NAME_DATASET_COMPACT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_COMPACT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, buf) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Pclose(pid);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_compact */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_external
 *
 * Purpose:     Create an external dataset in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao
 *              Friday, September 30, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_external(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t pid = -1;                             /* Dataset creation property list ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    int i;
    hsize_t size;
    hsize_t dim1d[1];
    int buf[DIM_SIZE_1];
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): external dataset");

    /* set initial data values */
    for (i=0; i<DIM_SIZE_1; i++)
        buf[i] = i;

    /* create an empty external file */
    HDfclose(HDfopen (FILE_EXT, "w"));

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim1d[0]=DIM_SIZE_1;

    /* create dataspace */
    if((sid = H5Screate_simple(1, dim1d, NULL)) < 0) TEST_ERROR

    /* set dataset creation plist */
    size = DIM_SIZE_1 * sizeof (int);
    if((pid = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR
    if(H5Pset_external(pid, FILE_EXT, (off_t)0, size) < 0) TEST_ERROR

    /* create dataset at SRC file */
    if((did = H5Dcreate2(fid_src, NAME_DATASET_EXTERNAL, H5T_NATIVE_INT, sid, H5P_DEFAULT, pid, H5P_DEFAULT)) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    /* close external plist */
    if(H5Pclose(pid) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST */
    if(H5Ocopy(fid_src, NAME_DATASET_EXTERNAL, fid_dst, NAME_DATASET_EXTERNAL, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the dataset for copy */
    if((did = H5Dopen2(fid_src, NAME_DATASET_EXTERNAL, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_EXTERNAL, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, buf) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Pclose(pid);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_external */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_named_dtype
 *
 * Purpose:     Create a dataset that uses a named datatype in SRC file and
 *              copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Monday, October 31, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_named_dtype(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t tid = -1;                             /* Datatype ID */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    int i;
    hsize_t dim1d[1];
    int buf[DIM_SIZE_1];
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): dataset that uses named datatype");

    /* set initial data values */
    for (i=0; i<DIM_SIZE_1; i++)
        buf[i] = i;

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim1d[0]=DIM_SIZE_1;

    /* create dataspace */
    if((sid = H5Screate_simple(1, dim1d, NULL)) < 0) TEST_ERROR

    /* create named datatype */
    if((tid = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR
    if((H5Tcommit2(fid_src, NAME_DATATYPE_SIMPLE, tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* create dataset at SRC file */
    if((did = H5Dcreate2(fid_src, NAME_DATASET_NAMED_DTYPE, tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* close the datatype */
    if(H5Tclose(tid) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST */
    if(H5Ocopy(fid_src, NAME_DATASET_NAMED_DTYPE, fid_dst, NAME_DATASET_NAMED_DTYPE, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the dataset for copy */
    if((did = H5Dopen2(fid_src, NAME_DATASET_NAMED_DTYPE, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_NAMED_DTYPE, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, buf) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Tclose(tid);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_named_dtype */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_named_dtype_hier
 *
 * Purpose:     Create a hierarchy of datasets that use a named datatype in
 *              SRC file and copy hierarchy to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, October 31, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_named_dtype_hier(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t tid = -1;                             /* Datatype ID */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1;                             /* Dataset ID */
    hid_t gid = -1, gid2 = -1;                  /* Group IDs */
    int i;
    hsize_t dim1d[1];
    int buf[DIM_SIZE_1];
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): hier. of datasets using named datatype inside hier.");

    /* set initial data values */
    for (i=0; i<DIM_SIZE_1; i++)
        buf[i] = i;

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* Create group to place all objects in */
    if((gid = H5Gcreate2(fid_src, NAME_GROUP_TOP, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim1d[0]=DIM_SIZE_1;

    /* create dataspace */
    if((sid = H5Screate_simple(1, dim1d, NULL)) < 0) TEST_ERROR

    /* create named datatype _inside_ hierarchy to copy */
    if((tid = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR
    if((H5Tcommit2(gid, NAME_DATATYPE_SIMPLE, tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* create first dataset at SRC file */
    if((did = H5Dcreate2(gid, NAME_DATASET_NAMED_DTYPE, tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* create second dataset at SRC file */
    if((did = H5Dcreate2(gid, NAME_DATASET_NAMED_DTYPE2, tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the datatype */
    if(H5Tclose(tid) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    /* close group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST */
    if(H5Ocopy(fid_src, NAME_GROUP_TOP, fid_dst, NAME_GROUP_TOP, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the group for copy */
    if((gid = H5Gopen2(fid_src, NAME_GROUP_TOP, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* open the destination group */
    if((gid2 = H5Gopen2(fid_dst, NAME_GROUP_TOP, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check if the groups are equal */
    if(compare_groups(gid, gid2, H5P_DEFAULT, -1, 0) != TRUE) TEST_ERROR

    /* close the destination group */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* close the source group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did);
    	H5Tclose(tid);
    	H5Sclose(sid);
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_named_dtype_hier */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_named_dtype_hier_outside
 *
 * Purpose:     Create a hierarchy of datasets that use a named datatype that
 *              is outside of hierarchy in SRC file and copy hierarchy to DST
 *              file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, October 31, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_named_dtype_hier_outside(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t tid = -1;                             /* Datatype ID */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1;                             /* Dataset ID */
    hid_t gid = -1, gid2 = -1;                  /* Group IDs */
    int i;
    hsize_t dim1d[1];
    int buf[DIM_SIZE_1];
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): hier. of datasets using named datatype outside hier.");

    /* set initial data values */
    for (i=0; i<DIM_SIZE_1; i++)
        buf[i] = i;

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* Create group to place all objects in */
    if((gid = H5Gcreate2(fid_src, NAME_GROUP_TOP, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim1d[0]=DIM_SIZE_1;

    /* create dataspace */
    if((sid = H5Screate_simple(1, dim1d, NULL)) < 0) TEST_ERROR

    /* create named datatype _outside_ hierarchy to copy */
    if((tid = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR
    if((H5Tcommit2(fid_src, NAME_DATATYPE_SIMPLE, tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* create first dataset at SRC file */
    if((did = H5Dcreate2(gid, NAME_DATASET_NAMED_DTYPE, tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* create second dataset at SRC file */
    if((did = H5Dcreate2(gid, NAME_DATASET_NAMED_DTYPE2, tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the datatype */
    if(H5Tclose(tid) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    /* close group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST */
    if(H5Ocopy(fid_src, NAME_GROUP_TOP, fid_dst, NAME_GROUP_TOP, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the group for copy */
    if((gid = H5Gopen2(fid_src, NAME_GROUP_TOP, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* open the destination group */
    if((gid2 = H5Gopen2(fid_dst, NAME_GROUP_TOP, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check if the groups are equal */
    if(compare_groups(gid, gid2, H5P_DEFAULT, -1, 0) != TRUE) TEST_ERROR

    /* close the destination group */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* close the source group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did);
    	H5Tclose(tid);
    	H5Sclose(sid);
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_named_dtype_hier_outside */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_multi_ohdr_chunks
 *
 * Purpose:     Create a pair of datasets that add attributes in a way that
 *              creates lots of object header chunks in SRC file and copy
 *              datasets to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, October 31, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_multi_ohdr_chunks(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    hid_t gid = -1, gid2 = -1;                  /* Group IDs */
    int i;
    hsize_t dim1d[1];
    int buf[DIM_SIZE_1];
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): datasets that have multiple ohdr chunks");

    /* set initial data values */
    for (i=0; i<DIM_SIZE_1; i++)
        buf[i] = i;

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* Create group to place all objects in */
    if((gid = H5Gcreate2(fid_src, NAME_GROUP_TOP, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim1d[0]=DIM_SIZE_1;

    /* create dataspace */
    if((sid = H5Screate_simple(1, dim1d, NULL)) < 0) TEST_ERROR

    /* create first dataset at SRC file */
    if((did = H5Dcreate2(gid, NAME_DATASET_MULTI_OHDR, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* create second dataset at SRC file */
    if((did2 = H5Dcreate2(gid, NAME_DATASET_MULTI_OHDR2, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* Add attributes to datasets in a way that creates lots of chunks */
    if(test_copy_attach_paired_attributes(did, did2, H5T_NATIVE_INT) < 0) TEST_ERROR

    /* close the first dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the second dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    /* close group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST */
    if(H5Ocopy(fid_src, NAME_GROUP_TOP, fid_dst, NAME_GROUP_TOP, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the group for copy */
    if((gid = H5Gopen2(fid_src, NAME_GROUP_TOP, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* open the destination group */
    if((gid2 = H5Gopen2(fid_dst, NAME_GROUP_TOP, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check if the groups are equal */
    if(compare_groups(gid, gid2, H5P_DEFAULT, -1, 0) != TRUE) TEST_ERROR

    /* close the destination group */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* close the source group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Sclose(sid);
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_multi_ohdr_chunks */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_attr_named_dtype
 *
 * Purpose:     Create a pair of datasets that add attributes that use
 *              named datatypes in SRC file and copy datasets to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, October 31, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_attr_named_dtype(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t tid = -1;                             /* Datatype ID */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    hid_t gid = -1, gid2 = -1;                  /* Group IDs */
    int i;
    hsize_t dim1d[1];
    int buf[DIM_SIZE_1];
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): objects with attributes using named datatypes");

    /* set initial data values */
    for (i=0; i<DIM_SIZE_1; i++)
        buf[i] = i;

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* Create group to place all objects in */
    if((gid = H5Gcreate2(fid_src, NAME_GROUP_TOP, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim1d[0]=DIM_SIZE_1;

    /* create dataspace */
    if((sid = H5Screate_simple(1, dim1d, NULL)) < 0) TEST_ERROR

    /* create named datatype _outside_ hierarchy to copy */
    if((tid = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR
    if((H5Tcommit2(fid_src, NAME_DATATYPE_SIMPLE, tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* create first dataset at SRC file */
    if((did = H5Dcreate2(gid, NAME_DATASET_MULTI_OHDR, tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* create second dataset at SRC file */
    if((did2 = H5Dcreate2(gid, NAME_DATASET_MULTI_OHDR2, tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* Add attributes to datasets in a way that creates lots of chunks */
    if(test_copy_attach_paired_attributes(did, did2, tid) < 0) TEST_ERROR

    /* close the datatype */
    if(H5Tclose(tid) < 0) TEST_ERROR

    /* close the first dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the second dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    /* close group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST */
    if(H5Ocopy(fid_src, NAME_GROUP_TOP, fid_dst, NAME_GROUP_TOP, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the group for copy */
    if((gid = H5Gopen2(fid_src, NAME_GROUP_TOP, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* open the destination group */
    if((gid2 = H5Gopen2(fid_dst, NAME_GROUP_TOP, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check if the groups are equal */
    if(compare_groups(gid, gid2, H5P_DEFAULT, -1, 0) != TRUE) TEST_ERROR

    /* close the destination group */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* close the source group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Tclose(tid);
    	H5Sclose(sid);
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_attr_named_dtype */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_contig_vl
 *
 * Purpose:     Create a contiguous dataset w/VLEN datatype in SRC
 *              file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao
 *              Friday, September 30, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_contig_vl(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t tid = -1;                             /* Datatype ID */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    unsigned int i, j;                          /* Local index variables */
    hsize_t dim1d[1];                           /* Dataset dimensions */
    hvl_t buf[DIM_SIZE_1];                      /* Buffer for writing data */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): contiguous dataset with VLEN datatype");

    /* set initial data values */
    for(i = 0; i < DIM_SIZE_1; i++) {
        buf[i].len = i+1;
        buf[i].p = (int *)HDmalloc(buf[i].len * sizeof(int));
        for(j = 0; j < buf[i].len; j++)
            ((int *)buf[i].p)[j] = (int)(i * 10 + j);
    } /* end for */

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim1d[0]=DIM_SIZE_1;

    /* create dataspace */
    if((sid = H5Screate_simple(1, dim1d, NULL)) < 0) TEST_ERROR

    /* create datatype */
    if((tid = H5Tvlen_create(H5T_NATIVE_INT)) < 0) TEST_ERROR

    /* create dataset at SRC file */
    if((did = H5Dcreate2(fid_src, NAME_DATASET_VL, tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST */
    if(H5Ocopy(fid_src, NAME_DATASET_VL, fid_dst, NAME_DATASET_VL, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the dataset for copy */
    if((did = H5Dopen2(fid_src, NAME_DATASET_VL, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_VL, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, buf) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR


    /* Reclaim vlen buffer */
    if(H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close datatype */
    if(H5Tclose(tid) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
        H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, buf);
    	H5Tclose(tid);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_contig_vl */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_chunked_vl
 *
 * Purpose:     Create a chunked dataset w/VLEN datatype in SRC
 *              file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao
 *              Saturday, December 10, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_chunked_vl(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t tid = -1;                             /* Datatype ID */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t pid = -1;                             /* Dataset creation property list ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    unsigned int i, j;                          /* Local index variables */
    hsize_t dim1d[1];                           /* Dataset dimensions */
    hsize_t chunk_dim1d[1] = {CHUNK_SIZE_1};    /* Chunk dimensions */
    hvl_t buf[DIM_SIZE_1];                      /* Buffer for writing data */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): chunked dataset with VLEN datatype");

    /* set initial data values */
    for(i = 0; i < DIM_SIZE_1; i++) {
        buf[i].len = i+1;
        buf[i].p = (int *)HDmalloc(buf[i].len * sizeof(int));
        for(j = 0; j < buf[i].len; j++)
            ((int *)buf[i].p)[j] = (int)(i * 10 + j);
    } /* end for */

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim1d[0]=DIM_SIZE_1;

    /* create dataspace */
    if((sid = H5Screate_simple(1, dim1d, NULL)) < 0) TEST_ERROR

    /* create datatype */
    if((tid = H5Tvlen_create(H5T_NATIVE_INT)) < 0) TEST_ERROR

    /* create and set chunk plist */
    if((pid = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR
    if(H5Pset_chunk(pid, 1, chunk_dim1d) < 0) TEST_ERROR

    /* create dataset at SRC file */
    if((did = H5Dcreate2(fid_src, NAME_DATASET_VL, tid, sid, H5P_DEFAULT, pid, H5P_DEFAULT)) < 0) TEST_ERROR

    /* close chunk plist */
    if(H5Pclose(pid) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST */
    if(H5Ocopy(fid_src, NAME_DATASET_VL, fid_dst, NAME_DATASET_VL, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the dataset for copy */
    if((did = H5Dopen2(fid_src, NAME_DATASET_VL, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_VL, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, buf) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR


    /* Reclaim vlen buffer */
    if(H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close datatype */
    if(H5Tclose(tid) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
        H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, buf);
    	H5Tclose(tid);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_chunked_vl */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_compact_vl
 *
 * Purpose:     Create a compact dataset w/VLEN datatype in SRC
 *              file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao
 *              Sunday, December 11, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_compact_vl(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t tid = -1;                             /* Datatype ID */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t pid = -1;                             /* Dataset creation property list ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    unsigned int i, j;                          /* Local index variables */
    hsize_t dim1d[1];                           /* Dataset dimensions */
    hvl_t buf[DIM_SIZE_1];                      /* Buffer for writing data */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): compact dataset with VLEN datatype");

    /* set initial data values */
    for(i = 0; i < DIM_SIZE_1; i++) {
        buf[i].len = i+1;
        buf[i].p = (int *)HDmalloc(buf[i].len * sizeof(int));
        for(j = 0; j < buf[i].len; j++)
            ((int *)buf[i].p)[j] = (int)(i * 10 + j);
    } /* end for */

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim1d[0]=DIM_SIZE_1;

    /* create dataspace */
    if((sid = H5Screate_simple(1, dim1d, NULL)) < 0) TEST_ERROR

    /* create datatype */
    if((tid = H5Tvlen_create(H5T_NATIVE_INT)) < 0) TEST_ERROR

    /* create and set compact plist */
    if((pid = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR
    if(H5Pset_layout(pid, H5D_COMPACT) < 0) TEST_ERROR

    /* create dataset at SRC file */
    if((did = H5Dcreate2(fid_src, NAME_DATASET_VL, tid, sid, H5P_DEFAULT, pid, H5P_DEFAULT)) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close compact plist */
    if(H5Pclose(pid) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST */
    if(H5Ocopy(fid_src, NAME_DATASET_VL, fid_dst, NAME_DATASET_VL, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the dataset for copy */
    if((did = H5Dopen2(fid_src, NAME_DATASET_VL, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_VL, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, buf) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR


    /* Reclaim vlen buffer */
    if(H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close datatype */
    if(H5Tclose(tid) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
        H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, buf);
    	H5Tclose(tid);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_compact_vl */


/*-------------------------------------------------------------------------
 * Function:    test_copy_attribute_vl
 *
 * Purpose:     Create a simple dataset with vlen attributes in SRC file
 *               and copy it to DST file  (Note: dataset has no data)
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao
 *              Saturday, December , 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_attribute_vl(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    hid_t aid = -1, aid2 = -1;                  /* Attribute IDs */
    hsize_t dim2d[2];                           /* Dataset dimensions */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): variable length attribute");

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim2d[0] = DIM_SIZE_1;
    dim2d[1] = DIM_SIZE_2;

    /* create 2D dataspace */
    if((sid = H5Screate_simple(2, dim2d, NULL)) < 0) TEST_ERROR

    /* create 2D int dataset at SRC file */
    if((did = H5Dcreate2(fid_src, NAME_DATASET_SIMPLE, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    /* attach VL attribute to the dataset */
    if(test_copy_attach_attribute_vl(did) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST */
    if(H5Ocopy(fid_src, NAME_DATASET_SIMPLE, fid_dst, NAME_DATASET_SIMPLE, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the dataset for copy */
    if((did = H5Dopen2(fid_src, NAME_DATASET_SIMPLE, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_SIMPLE, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the attributes are equal */

    if((aid = H5Aopen_by_idx(did, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)0, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if((aid2 = H5Aopen_by_idx(did2, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)0, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(compare_attribute(aid, aid2, H5P_DEFAULT, NULL, did) != TRUE) TEST_ERROR
    if(H5Aclose(aid) < 0) TEST_ERROR
    if(H5Aclose(aid2) < 0) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Aclose(aid2);
        H5Aclose(aid);
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_simple_empty */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_compressed_vl
 *
 * Purpose:     Create a compressed, chunked, VLEN dataset in SRC
 *              file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao
 *              Tuesday, December 27, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_compressed_vl(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
#ifdef H5_HAVE_FILTER_DEFLATE
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t tid = -1;                             /* Datatype ID */
    hid_t pid = -1;                             /* Dataset creation property list ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    hsize_t dim2d[2];                           /* Dataset dimensions */
    hsize_t chunk_dim2d[2] ={CHUNK_SIZE_1, CHUNK_SIZE_2};             /* Chunk dimensions */
    hvl_t buf[DIM_SIZE_1][DIM_SIZE_2];          /* Buffer for writing data */
    int i, j, k;                                /* Local index variables */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];
#endif /* H5_HAVE_FILTER_DEFLATE */

    TESTING("H5Ocopy(): compressed dataset with VLEN datatype");

#ifndef H5_HAVE_FILTER_DEFLATE
    SKIPPED();
    puts("    Deflation filter not available");
#else /* H5_HAVE_FILTER_DEFLATE */
    /* set initial data values */
    for (i = 0; i < DIM_SIZE_1; i++) {
        for (j = 0; j < DIM_SIZE_2; j++) {
            buf[i][j].len = (size_t)(j + 1);
            buf[i][j].p = (int *)HDmalloc(buf[i][j].len * sizeof(int));
            for (k = 0; k < (int)buf[i][j].len; k++)
                ((int *)buf[i][j].p)[k] = i * 10000 + j * 100 + k;
        }
    }

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim2d[0]=DIM_SIZE_1;
    dim2d[1]=DIM_SIZE_2;

    /* create dataspace */
    if((sid = H5Screate_simple(2, dim2d, NULL)) < 0) TEST_ERROR

    /* create datatype */
    if((tid = H5Tvlen_create(H5T_NATIVE_INT)) < 0) TEST_ERROR

    /* create and set comp & chunk plist */
    if((pid = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR
    if(H5Pset_chunk(pid, 2, chunk_dim2d) < 0) TEST_ERROR
    if(H5Pset_deflate(pid, 9) < 0) TEST_ERROR

    /* create dataset */
    if((did = H5Dcreate2(fid_src, NAME_DATASET_CHUNKED, tid, sid, H5P_DEFAULT, pid, H5P_DEFAULT)) < 0) TEST_ERROR

    /* close chunk plist */
    if(H5Pclose(pid) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST */
    if(H5Ocopy(fid_src, NAME_DATASET_CHUNKED, fid_dst, NAME_DATASET_CHUNKED, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the dataset for copy */
    if((did = H5Dopen2(fid_src, NAME_DATASET_CHUNKED, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_CHUNKED, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, NULL) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    /* Reclaim vlen buffer */
    if(H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close datatype */
    if(H5Tclose(tid) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    PASSED();
#endif /* H5_HAVE_FILTER_DEFLATE */
    return 0;

#ifdef H5_HAVE_FILTER_DEFLATE
error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Pclose(pid);
        H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, buf);
    	H5Tclose(tid);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
#endif /* H5_HAVE_FILTER_DEFLATE */
} /* end test_copy_dataset_compressed_vl */


/*-------------------------------------------------------------------------
 * Function:    test_copy_group_empty
 *
 * Purpose:     Create an empty group in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao
 *              Friday, September 30, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_group_empty(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t gid = -1, gid2 = -1;                  /* Group IDs */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): empty group");

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* create group at the SRC file */
    if((gid = H5Gcreate2(fid_src, NAME_GROUP_EMPTY, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* attach attributes to the group */
    if(test_copy_attach_attributes(gid, H5T_NATIVE_INT) < 0) TEST_ERROR

    /* close the group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the group from SRC to DST */
    if(H5Ocopy(fid_src, NAME_GROUP_EMPTY, fid_dst, NAME_GROUP_EMPTY, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the group for copy */
    if((gid = H5Gopen2(fid_src, NAME_GROUP_EMPTY, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* open the destination group */
    if((gid2 = H5Gopen2(fid_dst, NAME_GROUP_EMPTY, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check if the groups are equal */
    if(compare_groups(gid, gid2, H5P_DEFAULT, -1, 0) != TRUE) TEST_ERROR

    /* close the destination group */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* close the source group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_group_empty */


/*-------------------------------------------------------------------------
 * Function:    test_copy_group
 *
 * Purpose:     Create a group in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao
 *              Friday, September 30, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_group(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1;                             /* Dataset ID */
    hid_t gid = -1, gid2 = -1;                  /* Group IDs */
    hid_t gid_sub = -1;                         /* Sub-group ID */
    hsize_t dim2d[2];
    int buf[DIM_SIZE_1][DIM_SIZE_2];
    int i, j;
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): simple nested groups");

    /* set initial data values */
    for (i=0; i<DIM_SIZE_1; i++)
        for (j=0; j<DIM_SIZE_2; j++)
            buf[i][j] = 10000 + 100*i+j;

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* create group at the SRC file */
    if((gid = H5Gcreate2(fid_src, NAME_GROUP_TOP, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* attach attributes to the group */
    if(test_copy_attach_attributes(gid, H5T_NATIVE_INT) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim2d[0]=DIM_SIZE_1;
    dim2d[1]=DIM_SIZE_2;

    /* create dataspace */
    if((sid = H5Screate_simple(2, dim2d, NULL)) < 0) TEST_ERROR

    /* add a dataset to the group */
    if((did = H5Dcreate2(fid_src, NAME_GROUP_DATASET, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* create a sub-group */
    if((gid_sub = H5Gcreate2(fid_src, NAME_GROUP_SUB, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if( H5Gclose(gid_sub) < 0) TEST_ERROR

    /* create another  sub-group */
    if((gid_sub = H5Gcreate2(fid_src, NAME_GROUP_SUB_2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if( H5Gclose(gid_sub) < 0) TEST_ERROR

    /* close the group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the group from SRC to DST */
    if(H5Ocopy(fid_src, NAME_GROUP_TOP, fid_dst, NAME_GROUP_TOP, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the group for copy */
    if((gid = H5Gopen2(fid_src, NAME_GROUP_TOP, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* open the destination group */
    if((gid2 = H5Gopen2(fid_dst, NAME_GROUP_TOP, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check if the groups are equal */
    if(compare_groups(gid, gid2, H5P_DEFAULT, -1, 0) != TRUE) TEST_ERROR

    /* close the destination group */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* close the source group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Sclose(sid);
    	H5Dclose(did);
    	H5Gclose(gid_sub);
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_group */


/*-------------------------------------------------------------------------
 * Function:    test_copy_root_group
 *
 * Purpose:     Create a root group in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao
 *              August 8, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_root_group(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1;                             /* Dataset ID */
    hid_t gid = -1, gid2 = -1;                  /* Group IDs */
    hid_t gid_sub = -1;                         /* Sub-group ID */
    hsize_t dim2d[2];
    int buf[DIM_SIZE_1][DIM_SIZE_2];
    int i, j;
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): root group");

    /* set initial data values */
    for (i=0; i<DIM_SIZE_1; i++)
        for (j=0; j<DIM_SIZE_2; j++)
            buf[i][j] = 10000 + 100*i+j;

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* create group at the SRC file */
    if((gid = H5Gcreate2(fid_src, NAME_GROUP_TOP, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* attach attributes to the group */
    if(test_copy_attach_attributes(gid, H5T_NATIVE_INT) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim2d[0] = DIM_SIZE_1;
    dim2d[1] = DIM_SIZE_2;

    /* create dataspace */
    if((sid = H5Screate_simple(2, dim2d, NULL)) < 0) TEST_ERROR

    /* add a dataset to the group */
    if((did = H5Dcreate2(fid_src, NAME_GROUP_DATASET, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* create a sub-group */
    if((gid_sub = H5Gcreate2(fid_src, NAME_GROUP_SUB, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid_sub) < 0) TEST_ERROR

    /* create another  sub-group */
    if((gid_sub = H5Gcreate2(fid_src, NAME_GROUP_SUB_2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid_sub) < 0) TEST_ERROR

    /* close the group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the group from SRC to DST */
    if(H5Ocopy(fid_src, "/", fid_dst, "/root_from_src", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the group for copy */
    if((gid = H5Gopen2(fid_src, "/", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* open the destination group */
    if((gid2 = H5Gopen2(fid_dst, "/root_from_src", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check if the groups are equal */
    if(compare_groups(gid, gid2, H5P_DEFAULT, -1, 0) != TRUE) TEST_ERROR

    /* close the destination group */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* close the source group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Sclose(sid);
        H5Dclose(did);
        H5Gclose(gid_sub);
        H5Gclose(gid2);
        H5Gclose(gid);
        H5Fclose(fid_dst);
        H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_root_group */


/*-------------------------------------------------------------------------
 * Function:    test_copy_group_deep
 *
 * Purpose:     Create a deep group hier. in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November 1, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_group_deep(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1;                             /* Dataset ID */
    hid_t gid = -1, gid2 = -1;                  /* Group IDs */
    hid_t gid_sub = -1, gid_sub2;               /* Sub-group IDs */
    hsize_t dim2d[2];
    int buf[DIM_SIZE_1][DIM_SIZE_2];
    int i, j, k;                                /* Local index variables */
    char objname[NAME_BUF_SIZE];                /* Sub-group & dataset name buffer */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): deep nested groups");

    /* set initial data values */
    for (i=0; i<DIM_SIZE_1; i++)
        for (j=0; j<DIM_SIZE_2; j++)
            buf[i][j] = 10000 + 100*i+j;

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* create group at the SRC file */
    if((gid = H5Gcreate2(fid_src, NAME_GROUP_TOP, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* attach attributes to the group */
    if(test_copy_attach_attributes(gid, H5T_NATIVE_INT) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim2d[0]=DIM_SIZE_1;
    dim2d[1]=DIM_SIZE_2;

    /* create dataspace */
    if((sid = H5Screate_simple(2, dim2d, NULL)) < 0) TEST_ERROR

    /* create nested sub-groups & datasets */
    for(i = 0; i < NUM_SUB_GROUPS; i++) {
        sprintf(objname, "Group #%d", i);
        if((gid_sub = H5Gcreate2(gid, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

        for(j = 0; j < NUM_SUB_GROUPS; j++) {
            sprintf(objname, "Group #%d", j);
            if((gid_sub2 = H5Gcreate2(gid_sub, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

            for(k = 0; k < NUM_DATASETS; k++) {
                sprintf(objname, "Dataset #%d", k);

                /* add a dataset to the group */
                if((did = H5Dcreate2(gid_sub2, objname, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
                if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR
                if(H5Dclose(did) < 0) TEST_ERROR
            } /* end for */

            if(H5Gclose(gid_sub2) < 0) TEST_ERROR
        } /* end for */

        if(H5Gclose(gid_sub) < 0) TEST_ERROR
    } /* end for */

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    /* close the group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the group from SRC to DST */
    if(H5Ocopy(fid_src, NAME_GROUP_TOP, fid_dst, NAME_GROUP_TOP, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the group for copy */
    if((gid = H5Gopen2(fid_src, NAME_GROUP_TOP, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* open the destination group */
    if((gid2 = H5Gopen2(fid_dst, NAME_GROUP_TOP, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check if the groups are equal */
    if(compare_groups(gid, gid2, H5P_DEFAULT, -1, 0) != TRUE) TEST_ERROR

    /* close the destination group */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* close the source group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Sclose(sid);
    	H5Dclose(did);
    	H5Gclose(gid_sub);
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_group_deep */


/*-------------------------------------------------------------------------
 * Function:    test_copy_group_loop
 *
 * Purpose:     Create a group hier. with loops in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November 1, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_group_loop(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t gid = -1, gid2 = -1;                  /* Group IDs */
    hid_t gid_sub = -1, gid_sub2=-1;            /* Sub-group IDs */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): nested groups with loop");

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* create group at the SRC file */
    if((gid = H5Gcreate2(fid_src, NAME_GROUP_TOP, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* attach attributes to the group */
    if(test_copy_attach_attributes(gid, H5T_NATIVE_INT) < 0) TEST_ERROR

    /* create sub-groups */
    if((gid_sub = H5Gcreate2(gid, NAME_GROUP_SUB, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    if((gid_sub2 = H5Gcreate2(gid, NAME_GROUP_SUB_SUB, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Create link to top group */
    if(H5Lcreate_hard(gid, ".", gid_sub2, NAME_GROUP_LOOP, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* close sub sub group */
    if( H5Gclose(gid_sub2) < 0) TEST_ERROR

    /* close sub group */
    if( H5Gclose(gid_sub) < 0) TEST_ERROR

    /* close the group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the group from SRC to DST */
    if(H5Ocopy(fid_src, NAME_GROUP_TOP, fid_dst, NAME_GROUP_TOP, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the group for copy */
    if((gid = H5Gopen2(fid_src, NAME_GROUP_TOP, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* open the destination group */
    if((gid2 = H5Gopen2(fid_dst, NAME_GROUP_TOP, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check if the groups are equal */
    if(compare_groups(gid, gid2, H5P_DEFAULT, -1, 0) != TRUE) TEST_ERROR

    /* close the destination group */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* close the source group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Gclose(gid_sub2);
    	H5Gclose(gid_sub);
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_group_loop */


/*-------------------------------------------------------------------------
 * Function:    test_copy_group_wide_loop
 *
 * Purpose:     Create a group hier. with loops in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November 1, 2005
 *
 * Note:        Create groups w/lots of entries in each level, so that "dense"
 *              group form is used.
 *
 * Note:        Also tests multiple links to a locked group during copy.
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_group_wide_loop(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t gid = -1, gid2 = -1;                  /* Group IDs */
    hid_t gid_sub = -1, gid_sub2=-1;            /* Sub-group IDs */
    unsigned u, v;                              /* Local index variables */
    char objname[NAME_BUF_SIZE];                /* Object name buffer */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): wide nested groups with loop");

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* create group at the SRC file */
    if((gid = H5Gcreate2(fid_src, NAME_GROUP_TOP, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* attach attributes to the group */
    if(test_copy_attach_attributes(gid, H5T_NATIVE_INT) < 0) TEST_ERROR

    /* create wide sub-group hierarchy, with multiple links to higher groups */
    for(u = 0; u < NUM_WIDE_LOOP_GROUPS; u++) {
        sprintf(objname, "%s-%u", NAME_GROUP_SUB, u);
        if((gid_sub = H5Gcreate2(gid, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

        for(v = 0; v < NUM_WIDE_LOOP_GROUPS; v++) {
            sprintf(objname, "%s-%u", NAME_GROUP_SUB_SUB2, v);
            if((gid_sub2 = H5Gcreate2(gid_sub, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

            /* Create link to top group */
            if(H5Lcreate_hard(gid, ".", gid_sub2, NAME_GROUP_LOOP, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

            /* Create link to sub-group */
            if(H5Lcreate_hard(gid_sub, ".", gid_sub2, NAME_GROUP_LOOP2, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

            /* Create link to self :-) */
            if(H5Lcreate_hard(gid_sub2, ".", gid_sub2, NAME_GROUP_LOOP3, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

            /* close sub sub group */
            if(H5Gclose(gid_sub2) < 0) FAIL_STACK_ERROR
        } /* end for */

        /* close sub group */
        if( H5Gclose(gid_sub) < 0) TEST_ERROR
    } /* end for */

    /* close the group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the group from SRC to DST */
    if(H5Ocopy(fid_src, NAME_GROUP_TOP, fid_dst, NAME_GROUP_TOP, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the group for copy */
    if((gid = H5Gopen2(fid_src, NAME_GROUP_TOP, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* open the destination group */
    if((gid2 = H5Gopen2(fid_dst, NAME_GROUP_TOP, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check if the groups are equal */
    if(compare_groups(gid, gid2, H5P_DEFAULT, -1, 0) != TRUE) TEST_ERROR

    /* close the destination group */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* close the source group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Gclose(gid_sub2);
    	H5Gclose(gid_sub);
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_group_wide_loop */


/*-------------------------------------------------------------------------
 * Function:    test_copy_group_links
 *
 * Purpose:     Create a group and links in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao
 *              Friday, September 30, 2005
 *
 * Modifications:
 *              Neil Fortner
 *              Tuesday, February 16, 2010
 *              Modified test to test flags for expanding soft and external
 *              links.
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_group_links(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1, fid_ext = -1; /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1, did2 = -1;                  /* Dataset ID */
    hid_t gid = -1, gid2 = -1;                  /* Group IDs */
    hid_t plid = -1;                            /* Object copy plist ID */
    hsize_t dim2d[2];
    hsize_t dim1d[1];
    H5L_info_t linfo;
    int buf[DIM_SIZE_1][DIM_SIZE_2];
    int i, j;
    unsigned expand_soft;
    unsigned expand_ext;
    unsigned copy_options;
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];
    char ext_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): group with links");

    /* set initial data values */
    for (i=0; i<DIM_SIZE_1; i++)
        for (j=0; j<DIM_SIZE_2; j++)
            buf[i][j] = 10000 + 100*i+j;

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);
    h5_fixname(FILENAME[2], fapl, ext_filename, sizeof ext_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* create file to hold external dataset */
    if((fid_ext = H5Fcreate(ext_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* create groups at the SRC file.  Group 2 will hold dangling links. */
    if((gid = H5Gcreate2(fid_src, NAME_GROUP_LINK, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if((gid2 = H5Gcreate2(fid_src, NAME_GROUP_LINK2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* attach attributes to the groups */
    if(test_copy_attach_attributes(gid, H5T_NATIVE_INT) < 0) TEST_ERROR
    if(test_copy_attach_attributes(gid2, H5T_NATIVE_INT) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim2d[0]=DIM_SIZE_1;
    dim2d[1]=DIM_SIZE_2;

    /* create dataspace */
    if((sid = H5Screate_simple(2, dim2d, NULL)) < 0) TEST_ERROR

    /* add a dataset to the group */
    if((did = H5Dcreate2(fid_src, NAME_LINK_DATASET, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* Now create a 1-D dataset in an external file */
    /* Set dataspace dimensions */
    dim1d[0]=DIM_SIZE_2;

    /* create dataspace */
    if((sid = H5Screate_simple(1, dim1d, NULL)) < 0) TEST_ERROR

    /* add a dataset to the external file */
    if((did = H5Dcreate2(fid_ext, NAME_DATASET_SIMPLE, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* make a hard link to the dataset */
    if(H5Lcreate_hard(fid_src, NAME_LINK_DATASET, H5L_SAME_LOC, NAME_LINK_HARD, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* make a soft link to the dataset */
    if(H5Lcreate_soft(NAME_LINK_DATASET, fid_src, NAME_LINK_SOFT, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* make an external link to the external dataset */
    if(H5Lcreate_external(ext_filename, NAME_DATASET_SIMPLE, fid_src, NAME_LINK_EXTERN, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* make a dangling soft link */
    if(H5Lcreate_soft("nowhere", fid_src, NAME_LINK_SOFT_DANGLE2, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* make a dangling external link */
    if(H5Lcreate_external("no_file", "no_object", fid_src, NAME_LINK_EXTERN_DANGLE2, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* close the groups */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* close the SRC and EXT files */
    if(H5Fclose(fid_src) < 0) TEST_ERROR
    if(H5Fclose(fid_ext) < 0) TEST_ERROR


    /* Create the object copy plist */
    if((plid = H5Pcreate(H5P_OBJECT_COPY)) < 0) TEST_ERROR

    /* Loop over all configurations (expand soft/external links) */
    for(expand_soft=0; expand_soft<=1; expand_soft++) {
        for(expand_ext=0; expand_ext<=1; expand_ext++) {
            /* Set the correct copy options on the obj copy plist */
            copy_options = 0;
            if(expand_soft)
                copy_options |= H5O_COPY_EXPAND_SOFT_LINK_FLAG;
            if(expand_ext)
                copy_options |= H5O_COPY_EXPAND_EXT_LINK_FLAG;
            if(H5Pset_copy_object(plid, copy_options) < 0) TEST_ERROR

            /* open the source file with read-only */
            if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

            /* create destination file */
            if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

            /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
            if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

            /* copy the group from SRC to DST */
            if(H5Ocopy(fid_src, NAME_GROUP_LINK, fid_dst, NAME_GROUP_LINK, plid, H5P_DEFAULT) < 0) TEST_ERROR

            /* open the group for copy */
            if((gid = H5Gopen2(fid_src, NAME_GROUP_LINK, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

            /* open the destination group */
            if((gid2 = H5Gopen2(fid_dst, NAME_GROUP_LINK, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

            /* If expand_soft is set to true, verify that the soft link is now a
             * hard link, and compare the expanded dataset, then delete it and
             * re-add it as a soft link so compare_groups() works */
            if(expand_soft) {
                /* Check link type */
                if(H5Lget_info(fid_dst, NAME_LINK_SOFT, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
                if(linfo.type != H5L_TYPE_HARD)
                    FAIL_PUTS_ERROR("Soft link was not expanded to a hard link")

                /* Compare datasets */
                if((did = H5Dopen2(fid_src, NAME_LINK_DATASET, H5P_DEFAULT)) < 0) TEST_ERROR
                if((did2 = H5Dopen2(fid_dst, NAME_LINK_SOFT, H5P_DEFAULT)) < 0) TEST_ERROR
                if(compare_datasets(did, did2, H5P_DEFAULT, NULL) != TRUE) TEST_ERROR

                /* Delete expanded dataset, add soft link */
                if(H5Ldelete(fid_dst, NAME_LINK_SOFT, H5P_DEFAULT) < 0) TEST_ERROR
                if(H5Lcreate_soft(NAME_LINK_DATASET, fid_dst, NAME_LINK_SOFT, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

                /* Close datasets */
                if(H5Dclose(did) < 0) TEST_ERROR
                if(H5Dclose(did2) < 0) TEST_ERROR
            } /* end if */

            /* If expand_ext is set to true, verify that the external link is
             * now a hard link, and compare the expanded dataset, then delete it
             * and re-add it as an external link so compare_groups() works */
            if(expand_ext) {
                /* Check link type */
                if(H5Lget_info(fid_dst, NAME_LINK_EXTERN, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
                if(linfo.type != H5L_TYPE_HARD)
                    FAIL_PUTS_ERROR("External link was not expanded to a hard link")

                /* Compare datasets */
                if((fid_ext = H5Fopen(ext_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR
                if((did = H5Dopen2(fid_ext, NAME_DATASET_SIMPLE, H5P_DEFAULT)) < 0) TEST_ERROR
                if((did2 = H5Dopen2(fid_dst, NAME_LINK_EXTERN, H5P_DEFAULT)) < 0) TEST_ERROR
                if(compare_datasets(did, did2, H5P_DEFAULT, NULL) != TRUE) TEST_ERROR

                /* Delete expanded dataset, add external link */
                if(H5Ldelete(fid_dst, NAME_LINK_EXTERN, H5P_DEFAULT) < 0) TEST_ERROR
                if(H5Lcreate_external(ext_filename, NAME_DATASET_SIMPLE, fid_dst, NAME_LINK_EXTERN, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

                /* Close datasets and external file */
                if(H5Dclose(did) < 0) TEST_ERROR
                if(H5Dclose(did2) < 0) TEST_ERROR
                if(H5Fclose(fid_ext) < 0) TEST_ERROR
            } /* end if */

            /* Check if the groups are equal */
            if(compare_groups(gid, gid2, H5P_DEFAULT, -1, 0) != TRUE) TEST_ERROR

            /* close the destination group */
            if(H5Gclose(gid2) < 0) TEST_ERROR

            /* close the source group */
            if(H5Gclose(gid) < 0) TEST_ERROR

            /* Now try to copy the group containing the dangling link.  They
             * should always be copied as the same type of link, never expanded
             * to hard links. */
            if(H5Ocopy(fid_src, NAME_GROUP_LINK2, fid_dst, NAME_GROUP_LINK2, plid, H5P_DEFAULT) < 0) TEST_ERROR

            /* Open the original and copied groups */
            if((gid = H5Gopen2(fid_src, NAME_GROUP_LINK2, H5P_DEFAULT)) < 0) TEST_ERROR
            if((gid2 = H5Gopen2(fid_dst, NAME_GROUP_LINK2, H5P_DEFAULT)) < 0) TEST_ERROR

            /* Compare the groups */
            if(compare_groups(gid, gid2, H5P_DEFAULT, -1, 0) != TRUE) TEST_ERROR

            /* Close groups */
            if(H5Gclose(gid2) < 0) TEST_ERROR
            if(H5Gclose(gid) < 0) TEST_ERROR

            /* close the SRC file */
            if(H5Fclose(fid_src) < 0) TEST_ERROR

            /* close the DST file */
            if(H5Fclose(fid_dst) < 0) TEST_ERROR
        } /* end for */
    } /* end for */

    /* Close the object copy plist */
    if(H5Pclose(plid) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Sclose(sid);
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid_ext);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    	H5Pclose(plid);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_group_links */


/*-------------------------------------------------------------------------
 * Function:    test_copy_soft_link
 *
 * Purpose:     Create a soft link in SRC file and copy it to DST file
 *              copy a datast pointed by a soft link to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, September 30, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_soft_link(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    hid_t gid = -1;                             /* Group ID */
    hsize_t dim2d[2];
    int buf[DIM_SIZE_1][DIM_SIZE_2];
    int i, j;
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): object through soft link");

    /* set initial data values */
    for (i=0; i<DIM_SIZE_1; i++)
        for (j=0; j<DIM_SIZE_2; j++)
            buf[i][j] = 10000 + 100*i+j;

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* create group at the SRC file */
    if((gid = H5Gcreate2(fid_src, NAME_GROUP_LINK, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* attach attributes to the group */
    if(test_copy_attach_attributes(gid, H5T_NATIVE_INT) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim2d[0]=DIM_SIZE_1;
    dim2d[1]=DIM_SIZE_2;

    /* create dataspace */
    if((sid = H5Screate_simple(2, dim2d, NULL)) < 0) TEST_ERROR

    /* add a dataset to the group */
    if((did = H5Dcreate2(fid_src, NAME_LINK_DATASET, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) FAIL_STACK_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) FAIL_STACK_ERROR

    /* make a soft link to the dataset */
    if(H5Lcreate_soft(NAME_LINK_DATASET, fid_src, NAME_LINK_SOFT, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* close the group */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) FAIL_STACK_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST */
    if(H5Ocopy(fid_src, NAME_LINK_SOFT, fid_dst, NAME_DATASET_SIMPLE, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the dataset through the soft link for copy */
    if((did = H5Dopen2(fid_src, NAME_LINK_SOFT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_SIMPLE, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, buf) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Sclose(sid);
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Gclose(gid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_soft_link */


/*-------------------------------------------------------------------------
 * Function:    test_copy_ext_link
 *
 * Purpose:     Create an external link in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  James Laird
 *              Friday, June 16, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_ext_link(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1, fid_ext = -1; /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    hid_t gid = -1;                             /* Group ID */
    hsize_t dim2d[2];
    int buf[DIM_SIZE_1][DIM_SIZE_2];
    int i, j;
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];
    char ext_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): object through external link");

    /* set initial data values */
    for (i=0; i<DIM_SIZE_1; i++)
        for (j=0; j<DIM_SIZE_2; j++)
            buf[i][j] = 10000 + 100*i+j;

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);
    h5_fixname(FILENAME[2], fapl, ext_filename, sizeof ext_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* create group at the SRC file */
    if((gid = H5Gcreate2(fid_src, NAME_GROUP_LINK, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* attach attributes to the group */
    if(test_copy_attach_attributes(gid, H5T_NATIVE_INT) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim2d[0]=DIM_SIZE_1;
    dim2d[1]=DIM_SIZE_2;

    /* create dataspace */
    if((sid = H5Screate_simple(2, dim2d, NULL)) < 0) TEST_ERROR

    /* add a dataset to the group */
    if((did = H5Dcreate2(fid_src, NAME_LINK_DATASET, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR
    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR
    /* close the group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* create file to hold external links to the src file */
    if((fid_ext = H5Fcreate(ext_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* create group in the file that will hold the external link */
    if((gid = H5Gcreate2(fid_ext, NAME_GROUP_LINK, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Create an external link to the dataset in the source file */
    if(H5Lcreate_external(src_filename, NAME_LINK_DATASET, fid_ext, NAME_LINK_EXTERN, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* close the group and file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid_ext) < 0) TEST_ERROR

    /* open the "extern" file with read-only */
    if((fid_ext = H5Fopen(ext_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST */
    if(H5Ocopy(fid_ext, NAME_LINK_EXTERN, fid_dst, NAME_DATASET_SIMPLE, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the dataset through the external link */
    if((did = H5Dopen2(fid_ext, NAME_LINK_EXTERN, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_SIMPLE, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, buf) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the EXT file */
    if(H5Fclose(fid_ext) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Sclose(sid);
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Gclose(gid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_ext_link */


/*-------------------------------------------------------------------------
 * Function:    test_copy_exist
 *
 * Purpose:     Create a simple dataset in SRC file and copy it onto an
 *              existing object in DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November  8, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_exist(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1;                             /* Dataset IDs */
    int buf[DIM_SIZE_1][DIM_SIZE_2];            /* Buffer for writing data */
    hsize_t dim2d[2];                           /* Dataset dimensions */
    int i, j;                                   /* local index variables */
    herr_t ret;                                 /* Generic return value */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): existing object");

    /* Initialize write buffer */
    for (i=0; i<DIM_SIZE_1; i++)
        for (j=0; j<DIM_SIZE_2; j++)
            buf[i][j] = 10000 + 100*i+j;

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim2d[0] = DIM_SIZE_1;
    dim2d[1] = DIM_SIZE_2;

    /* create 2D dataspace */
    if((sid = H5Screate_simple(2, dim2d, NULL)) < 0) TEST_ERROR

    /* create 2D int dataset at SRC file */
    if((did = H5Dcreate2(fid_src, NAME_DATASET_SIMPLE, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    /* attach attributes to the dataset */
    if(test_copy_attach_attributes(did, H5T_NATIVE_INT) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST */
    if(H5Ocopy(fid_src, NAME_DATASET_SIMPLE,fid_dst, NAME_DATASET_SIMPLE, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* try to copy the dataset from SRC to DST again (should fail) */
    H5E_BEGIN_TRY {
        ret = H5Ocopy(fid_src, NAME_DATASET_SIMPLE, fid_dst, NAME_DATASET_SIMPLE, H5P_DEFAULT, H5P_DEFAULT);
    } H5E_END_TRY;
    if( ret >= 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_exist */


/*-------------------------------------------------------------------------
 * Function:    test_copy_path
 *
 * Purpose:     Create a simple dataset in SRC file and copy it to DST file
 *              using a full path name
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November  8, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_path(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    hid_t gid = -1;                             /* Group ID */
    int buf[DIM_SIZE_1][DIM_SIZE_2];            /* Buffer for writing data */
    hsize_t dim2d[2];                           /* Dataset dimensions */
    int i, j;                                   /* local index variables */
    herr_t ret;                                 /* Generic return value */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): full path");

    /* Initialize write buffer */
    for (i=0; i<DIM_SIZE_1; i++)
        for (j=0; j<DIM_SIZE_2; j++)
            buf[i][j] = 10000 + 100*i+j;

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim2d[0] = DIM_SIZE_1;
    dim2d[1] = DIM_SIZE_2;

    /* create 2D dataspace */
    if((sid = H5Screate_simple(2, dim2d, NULL)) < 0) TEST_ERROR

    /* create 2D int dataset at SRC file */
    if((did = H5Dcreate2(fid_src, NAME_DATASET_SIMPLE, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    /* attach attributes to the dataset */
    if(test_copy_attach_attributes(did, H5T_NATIVE_INT) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST (should fail - intermediate groups not there) */
    H5E_BEGIN_TRY {
        ret = H5Ocopy(fid_src, NAME_DATASET_SUB_SUB, fid_dst, NAME_DATASET_SUB_SUB, H5P_DEFAULT, H5P_DEFAULT);
    } H5E_END_TRY;
    if( ret >= 0) TEST_ERROR

    /* Create the intermediate groups in destination file */
    if((gid = H5Gcreate2(fid_dst, NAME_GROUP_TOP, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    if((gid = H5Gcreate2(fid_dst, NAME_GROUP_SUB, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    if((gid = H5Gcreate2(fid_dst, NAME_GROUP_SUB_SUB, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST, using full path */
    if(H5Ocopy(fid_src, NAME_DATASET_SIMPLE, fid_dst, NAME_DATASET_SUB_SUB, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the dataset for copy */
    if((did = H5Dopen2(fid_src, NAME_DATASET_SIMPLE, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_SUB_SUB, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, buf) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Sclose(sid);
    	H5Gclose(gid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_path */


/*-------------------------------------------------------------------------
 * Function:    test_copy_same_file_named_datatype
 *
 * Purpose:     Create name datatype in SRC file and copy it to same file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November  8, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_same_file_named_datatype(hid_t fcpl_src, hid_t fapl)
{
    hid_t fid = -1;                     /* File ID */
    hid_t tid = -1, tid2 = -1;          /* Datatype IDs */
    char filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): named datatype in same file");

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* create datatype */
    if((tid = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR

    /* create named datatype */
    if((H5Tcommit2(fid, NAME_DATATYPE_SIMPLE, tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR


    /* copy the datatype from SRC to DST */
    if(H5Ocopy(fid, NAME_DATATYPE_SIMPLE, fid, NAME_DATATYPE_SIMPLE2, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the copied datatype */
    if((tid2 = H5Topen2(fid, NAME_DATATYPE_SIMPLE2, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Compare the datatypes */
    if(H5Tequal(tid, tid2) != TRUE) TEST_ERROR

    /* close the destination datatype */
    if(H5Tclose(tid2) < 0) TEST_ERROR

    /* close the source datatype */
    if(H5Tclose(tid) < 0) TEST_ERROR

    /* close the file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Tclose(tid2);
    	H5Tclose(tid);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_same_file_named_datatype */


/*-------------------------------------------------------------------------
 * Function:    test_copy_old_layout
 *
 * Purpose:     Copy dataset that uses the "old" layout version (pre version 3)
 *              format.
 *
 * Note:	This test uses the "fill_old.h5" file for convenience, since it
 *              has a dataset with the old layout format.
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Thursday, November 30, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_old_layout(hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    char *srcdir = HDgetenv("srcdir");  /* Where the src code is located */
    char src_filename[NAME_BUF_SIZE] = "";
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): dataset with old layout format");

    /* Generate correct name for source file by prepending the source path */
    if(srcdir && ((HDstrlen(srcdir) + HDstrlen(FILE_OLD_LAYOUT) + 1) < sizeof(src_filename))) {
        HDstrcpy(src_filename, srcdir);
        HDstrcat(src_filename, "/");
    } /* end if */
    HDstrcat(src_filename, FILE_OLD_LAYOUT);

    /* Initialize the destination filename */
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* open source file (read-only) */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST */
    if(H5Ocopy(fid_src, NAME_OLD_FORMAT, fid_dst, NAME_DATASET_SIMPLE, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the source dataset */
    if((did = H5Dopen2(fid_src, NAME_OLD_FORMAT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_SIMPLE, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, NULL) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_old_layout */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_compact_named_vl
 *
 * Purpose:     Create a dataset that uses a named variable length datatype
 *              in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao
 *              Saturday, February 4, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_compact_named_vl(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t tid = -1, tid_copy=-1;                /* Datatype ID */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t pid = -1;                             /* Dataset creation property list ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    unsigned int i, j;                          /* Local index variables */
    hsize_t dim1d[1];                           /* Dataset dimensions */
    hvl_t buf[DIM_SIZE_1];                      /* Buffer for writing data */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): compact dataset with named VLEN datatype");

    /* set initial data values */
    for(i = 0; i < DIM_SIZE_1; i++) {
        buf[i].len = i+1;
        buf[i].p = (int *)HDmalloc(buf[i].len * sizeof(int));
        for(j = 0; j < buf[i].len; j++)
            ((int *)buf[i].p)[j] = (int)(i * 10 + j);
    } /* end for */

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim1d[0]=DIM_SIZE_1;

    /* create dataspace */
    if((sid = H5Screate_simple(1, dim1d, NULL)) < 0) TEST_ERROR

    /* create datatype */
    if((tid = H5Tvlen_create(H5T_NATIVE_INT)) < 0) TEST_ERROR

    /* make a copy of the datatype for later use */
    if((tid_copy = H5Tcopy(tid)) < 0)TEST_ERROR

    /* named data type */
    if((H5Tcommit2(fid_src, NAME_DATATYPE_VL, tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* create and set compact plist */
    if((pid = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR
    if(H5Pset_layout(pid, H5D_COMPACT) < 0) TEST_ERROR

    /* create dataset at SRC file */
    if((did = H5Dcreate2(fid_src, NAME_DATASET_VL, tid, sid, H5P_DEFAULT, pid, H5P_DEFAULT)) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close compact plist */
    if(H5Pclose(pid) < 0) TEST_ERROR

    /* close the datatype */
    if(H5Tclose(tid) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST */
    if(H5Ocopy(fid_src, NAME_DATASET_VL, fid_dst, NAME_DATASET_VL, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the dataset for copy */
    if((did = H5Dopen2(fid_src, NAME_DATASET_VL, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_VL, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, buf) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR


    /* Reclaim vlen buffer */
    if(H5Dvlen_reclaim(tid_copy, sid, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close datatype */
    if(H5Tclose(tid_copy) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Pclose(pid);
    	H5Dclose(did2);
    	H5Dclose(did);
        H5Dvlen_reclaim(tid_copy, sid, H5P_DEFAULT, buf);
    	H5Tclose(tid);
    	H5Tclose(tid_copy);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_compact_named_vl */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_contig_named_vl
 *
 * Purpose:     Create a dataset that uses a named variable length datatype
 *              in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao
 *              Saturday, February 11, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_contig_named_vl(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t tid = -1, tid_copy=-1;                /* Datatype ID */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    unsigned int i, j;                          /* Local index variables */
    hsize_t dim1d[1];                           /* Dataset dimensions */
    hvl_t buf[DIM_SIZE_1];                      /* Buffer for writing data */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): contigous dataset with named VLEN datatype");

    /* set initial data values */
    for(i = 0; i < DIM_SIZE_1; i++) {
        buf[i].len = i+1;
        buf[i].p = (int *)HDmalloc(buf[i].len * sizeof(int));
        for(j = 0; j < buf[i].len; j++)
            ((int *)buf[i].p)[j] = (int)(i * 10 + j);
    } /* end for */

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim1d[0]=DIM_SIZE_1;

    /* create dataspace */
    if((sid = H5Screate_simple(1, dim1d, NULL)) < 0) TEST_ERROR

    /* create datatype */
    if((tid = H5Tvlen_create(H5T_NATIVE_INT)) < 0) TEST_ERROR

    /* make a copy of the datatype for later use */
    if((tid_copy = H5Tcopy(tid)) < 0)TEST_ERROR

    /* named data type */
    if((H5Tcommit2(fid_src, NAME_DATATYPE_VL, tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* create dataset at SRC file */
    if((did = H5Dcreate2(fid_src, NAME_DATASET_VL, tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close the datatype */
    if(H5Tclose(tid) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST */
    if(H5Ocopy(fid_src, NAME_DATASET_VL, fid_dst, NAME_DATASET_VL, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the dataset for copy */
    if((did = H5Dopen2(fid_src, NAME_DATASET_VL, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_VL, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, buf) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR


    /* Reclaim vlen buffer */
    if(H5Dvlen_reclaim(tid_copy, sid, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close datatype */
    if(H5Tclose(tid_copy) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
        H5Dvlen_reclaim(tid_copy, sid, H5P_DEFAULT, buf);
    	H5Tclose(tid);
    	H5Tclose(tid_copy);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_contig_named_vl */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_chunked_named_vl
 *
 * Purpose:     Create a dataset that uses a named variable length datatype
 *              in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao
 *              Saturday, February 11, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_chunked_named_vl(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t tid = -1, tid_copy=-1;                /* Datatype ID */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t pid = -1;                             /* Dataset creation property list ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    unsigned int i, j;                          /* Local index variables */
    hsize_t dim1d[1];                           /* Dataset dimensions */
    hvl_t buf[DIM_SIZE_1];                      /* Buffer for writing data */
    hsize_t chunk_dim1d[1] = {CHUNK_SIZE_1};    /* Chunk dimensions */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): chunked dataset with named VLEN datatype");

    /* set initial data values */
    for(i = 0; i < DIM_SIZE_1; i++) {
        buf[i].len = i+1;
        buf[i].p = (int *)HDmalloc(buf[i].len * sizeof(int));
        for(j = 0; j < buf[i].len; j++)
            ((int *)buf[i].p)[j] = (int)(i * 10 + j);
    } /* end for */

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim1d[0]=DIM_SIZE_1;

    /* create dataspace */
    if((sid = H5Screate_simple(1, dim1d, NULL)) < 0) TEST_ERROR

    /* create datatype */
    if((tid = H5Tvlen_create(H5T_NATIVE_INT)) < 0) TEST_ERROR

    /* make a copy of the datatype for later use */
    if((tid_copy = H5Tcopy(tid)) < 0)TEST_ERROR

    /* named data type */
    if((H5Tcommit2(fid_src, NAME_DATATYPE_VL, tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

     /* create and set chunk plist */
    if((pid = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR
    if(H5Pset_chunk(pid, 1, chunk_dim1d) < 0) TEST_ERROR

    /* create dataset at SRC file */
    if((did = H5Dcreate2(fid_src, NAME_DATASET_VL, tid, sid, H5P_DEFAULT, pid, H5P_DEFAULT)) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close compact plist */
    if(H5Pclose(pid) < 0) TEST_ERROR

    /* close the datatype */
    if(H5Tclose(tid) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST */
    if(H5Ocopy(fid_src, NAME_DATASET_VL, fid_dst, NAME_DATASET_VL, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the dataset for copy */
    if((did = H5Dopen2(fid_src, NAME_DATASET_VL, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_VL, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, buf) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR


    /* Reclaim vlen buffer */
    if(H5Dvlen_reclaim(tid_copy, sid, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close datatype */
    if(H5Tclose(tid_copy) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Pclose(pid);
    	H5Dclose(did2);
    	H5Dclose(did);
        H5Dvlen_reclaim(tid_copy, sid, H5P_DEFAULT, buf);
    	H5Tclose(tid);
    	H5Tclose(tid_copy);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_chunked_named_vl */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_compressed_named_vl
 *
 * Purpose:     Create a dataset that uses a named variable length datatype
 *              in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao
 *              Saturday, February 11, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_compressed_named_vl(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t tid = -1, tid_copy=-1;                /* Datatype ID */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t pid = -1;                             /* Dataset creation property list ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    unsigned int i, j;                          /* Local index variables */
    hsize_t dim1d[1];                           /* Dataset dimensions */
    hvl_t buf[DIM_SIZE_1];                      /* Buffer for writing data */
    hsize_t chunk_dim1d[1] = {CHUNK_SIZE_1};    /* Chunk dimensions */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): compressed dataset with named VLEN datatype");

    /* set initial data values */
    for(i = 0; i < DIM_SIZE_1; i++) {
        buf[i].len = i+1;
        buf[i].p = (int *)HDmalloc(buf[i].len * sizeof(int));
        for(j = 0; j < buf[i].len; j++)
            ((int *)buf[i].p)[j] = (int)(i * 10 + j);
    } /* end for */

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim1d[0]=DIM_SIZE_1;

    /* create dataspace */
    if((sid = H5Screate_simple(1, dim1d, NULL)) < 0) TEST_ERROR

    /* create datatype */
    if((tid = H5Tvlen_create(H5T_NATIVE_INT)) < 0) TEST_ERROR

    /* make a copy of the datatype for later use */
    if((tid_copy = H5Tcopy(tid)) < 0)TEST_ERROR

    /* named data type */
    if((H5Tcommit2(fid_src, NAME_DATATYPE_VL, tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

     /* create and set chunk plist */
    if((pid = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR
    if(H5Pset_chunk(pid, 1, chunk_dim1d) < 0) TEST_ERROR
    if(H5Pset_deflate(pid, 9) < 0) TEST_ERROR

    /* create dataset at SRC file */
    if((did = H5Dcreate2(fid_src, NAME_DATASET_VL, tid, sid, H5P_DEFAULT, pid, H5P_DEFAULT)) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close compact plist */
    if(H5Pclose(pid) < 0) TEST_ERROR

    /* close the datatype */
    if(H5Tclose(tid) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST */
    if(H5Ocopy(fid_src, NAME_DATASET_VL, fid_dst, NAME_DATASET_VL, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the dataset for copy */
    if((did = H5Dopen2(fid_src, NAME_DATASET_VL, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_VL, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, buf) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR


    /* Reclaim vlen buffer */
    if(H5Dvlen_reclaim(tid_copy, sid, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close datatype */
    if(H5Tclose(tid_copy) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Pclose(pid);
    	H5Dclose(did2);
    	H5Dclose(did);
        H5Dvlen_reclaim(tid_copy, sid, H5P_DEFAULT, buf);
    	H5Tclose(tid);
    	H5Tclose(tid_copy);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_compressed_named_vl */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_compact_vl_vl
 *
 * Purpose:     Create a compact dataset w/nested VLEN datatype
 *              in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao
 *              Saturday, February 11, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_compact_vl_vl(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t tid=-1, tid2=-1;                      /* Datatype ID */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t pid = -1;                             /* Dataset creation property list ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    unsigned int i, j, k;                       /* Local index variables */
    hsize_t dim1d[1];                           /* Dataset dimensions */
    hvl_t buf[DIM_SIZE_1];                      /* Buffer for writing data */
    hvl_t *tvl;                                 /* Temporary pointer to VL information */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): compact dataset with nested VLEN datatype");

    /* set initial data values */
    for(i=0; i<DIM_SIZE_1; i++) {
        buf[i].p=HDmalloc((i+1)*sizeof(hvl_t));
        if(buf[i].p==NULL) {
            TestErrPrintf("Cannot allocate memory for VL data! i=%u\n",i);
            return 1;
        } /* end if */
        buf[i].len=i+1;
        for(tvl = (hvl_t *)buf[i].p,j=0; j<(i+1); j++, tvl++) {
            tvl->p=HDmalloc((j+1)*sizeof(unsigned int));
            if(tvl->p==NULL) {
                TestErrPrintf("Cannot allocate memory for VL data! i=%u, j=%u\n",i,j);
                return 1;
            } /* end if */
            tvl->len=j+1;
            for(k=0; k<(j+1); k++)
                ((unsigned int *)tvl->p)[k]=i*100+j*10+k;
        } /* end for */
    } /* end for */


    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim1d[0]=DIM_SIZE_1;

    /* create dataspace */
    if((sid = H5Screate_simple(1, dim1d, NULL)) < 0) TEST_ERROR

    /* create datatype */
    if((tid = H5Tvlen_create(H5T_NATIVE_INT)) < 0) TEST_ERROR

    /* create nested VL datatype */
    if((tid2 = H5Tvlen_create(tid)) < 0) TEST_ERROR

    /* create and set compact plist */
    if((pid = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR
    if(H5Pset_layout(pid, H5D_COMPACT) < 0) TEST_ERROR

    /* create dataset at SRC file */
    if((did = H5Dcreate2(fid_src, NAME_DATASET_VL_VL, tid2, sid, H5P_DEFAULT, pid, H5P_DEFAULT)) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, tid2, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close compact plist */
    if(H5Pclose(pid) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST */
    if(H5Ocopy(fid_src, NAME_DATASET_VL_VL, fid_dst, NAME_DATASET_VL_VL, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the dataset for copy */
    if((did = H5Dopen2(fid_src, NAME_DATASET_VL_VL, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_VL_VL, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, buf) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    /* Reclaim vlen buffer */
    if(H5Dvlen_reclaim(tid2, sid, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close datatype */
    if(H5Tclose(tid) < 0) TEST_ERROR
    if(H5Tclose(tid2) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
        H5Dvlen_reclaim(tid2, sid, H5P_DEFAULT, buf);
    	H5Tclose(tid);
    	H5Tclose(tid2);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_compact_vl_vl */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_contig_vl_vl
 *
 * Purpose:     Create a compact dataset w/nested VLEN datatype
 *              in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao
 *              Saturday, February 11, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_contig_vl_vl(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t tid=-1, tid2=-1;                      /* Datatype ID */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t pid = -1;                             /* Dataset creation property list ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    unsigned int i, j, k;                       /* Local index variables */
    hsize_t dim1d[1];                           /* Dataset dimensions */
    hvl_t buf[DIM_SIZE_1];                      /* Buffer for writing data */
    hvl_t *tvl;                                 /* Temporary pointer to VL information */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): contigous dataset with nested VLEN datatype");

    /* set initial data values */
    for(i=0; i<DIM_SIZE_1; i++) {
        buf[i].p=HDmalloc((i+1)*sizeof(hvl_t));
        if(buf[i].p==NULL) {
            TestErrPrintf("Cannot allocate memory for VL data! i=%u\n",i);
            TEST_ERROR
        } /* end if */
        buf[i].len=i+1;
        for(tvl = (hvl_t *)buf[i].p,j=0; j<(i+1); j++, tvl++) {
            tvl->p=HDmalloc((j+1)*sizeof(unsigned int));
            if(tvl->p==NULL) {
                TestErrPrintf("Cannot allocate memory for VL data! i=%u, j=%u\n",i,j);
                TEST_ERROR
            } /* end if */
            tvl->len=j+1;
            for(k=0; k<(j+1); k++)
                ((unsigned int *)tvl->p)[k]=i*100+j*10+k;
        } /* end for */
    } /* end for */


    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim1d[0]=DIM_SIZE_1;

    /* create dataspace */
    if((sid = H5Screate_simple(1, dim1d, NULL)) < 0) TEST_ERROR

    /* create datatype */
    if((tid = H5Tvlen_create(H5T_NATIVE_INT)) < 0) TEST_ERROR

    /* create nested VL datatype */
    if((tid2 = H5Tvlen_create(tid)) < 0) TEST_ERROR

    /* create and set compact plist */
    if((pid = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR

    /* create dataset at SRC file */
    if((did = H5Dcreate2(fid_src, NAME_DATASET_VL_VL, tid2, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, tid2, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close compact plist */
    if(H5Pclose(pid) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST */
    if(H5Ocopy(fid_src, NAME_DATASET_VL_VL, fid_dst, NAME_DATASET_VL_VL, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the dataset for copy */
    if((did = H5Dopen2(fid_src, NAME_DATASET_VL_VL, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_VL_VL, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, buf) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    /* Reclaim vlen buffer */
    if(H5Dvlen_reclaim(tid2, sid, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close datatype */
    if(H5Tclose(tid) < 0) TEST_ERROR
    if(H5Tclose(tid2) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
        H5Dvlen_reclaim(tid2, sid, H5P_DEFAULT, buf);
    	H5Tclose(tid);
    	H5Tclose(tid2);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_contig_vl_vl */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_chunked_vl_vl
 *
 * Purpose:     Create a dataset that uses a named variable length datatype
 *              in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao
 *              Saturday, March 11, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_chunked_vl_vl(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t tid = -1, tid2=-1;       /* Datatype ID */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t pid = -1;                             /* Dataset creation property list ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    unsigned int i, j, k;                       /* Local index variables */
    hsize_t dim1d[1];                           /* Dataset dimensions */
    hvl_t buf[DIM_SIZE_1];                      /* Buffer for writing data */
    hvl_t *tvl;                                 /* Temporary pointer to VL information */
    hsize_t chunk_dim1d[1] = {CHUNK_SIZE_1};    /* Chunk dimensions */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): chunked dataset with nested VLEN datatype");

    /* set initial data values */
    for(i=0; i<DIM_SIZE_1; i++) {
        buf[i].p=HDmalloc((i+1)*sizeof(hvl_t));
        if(buf[i].p==NULL) {
            TestErrPrintf("Cannot allocate memory for VL data! i=%u\n",i);
            TEST_ERROR
        } /* end if */
        buf[i].len=i+1;
        for(tvl = (hvl_t *)buf[i].p,j=0; j<(i+1); j++, tvl++) {
            tvl->p=HDmalloc((j+1)*sizeof(unsigned int));
            if(tvl->p==NULL) {
                TestErrPrintf("Cannot allocate memory for VL data! i=%u, j=%u\n",i,j);
                TEST_ERROR
            } /* end if */
            tvl->len=j+1;
            for(k=0; k<(j+1); k++)
                ((unsigned int *)tvl->p)[k]=i*100+j*10+k;
        } /* end for */
    } /* end for */

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim1d[0]=DIM_SIZE_1;

    /* create dataspace */
    if((sid = H5Screate_simple(1, dim1d, NULL)) < 0) TEST_ERROR

    /* create datatype */
    if((tid = H5Tvlen_create(H5T_NATIVE_INT)) < 0) TEST_ERROR

    /* create nested VL datatype */
    if((tid2 = H5Tvlen_create(tid)) < 0) TEST_ERROR

     /* create and set chunk plist */
    if((pid = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR
    if(H5Pset_chunk(pid, 1, chunk_dim1d) < 0) TEST_ERROR

    /* create dataset at SRC file */
    if((did = H5Dcreate2(fid_src, NAME_DATASET_VL_VL, tid2, sid, H5P_DEFAULT, pid, H5P_DEFAULT)) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, tid2, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close compact plist */
    if(H5Pclose(pid) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST */
    if(H5Ocopy(fid_src, NAME_DATASET_VL_VL, fid_dst, NAME_DATASET_VL_VL, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the dataset for copy */
    if((did = H5Dopen2(fid_src, NAME_DATASET_VL_VL, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_VL_VL, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, buf) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR


    /* Reclaim vlen buffer */
    if(H5Dvlen_reclaim(tid2, sid, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close datatype */
    if(H5Tclose(tid) < 0) TEST_ERROR
    if(H5Tclose(tid2) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Pclose(pid);
    	H5Dclose(did2);
    	H5Dclose(did);
        H5Dvlen_reclaim(tid2, sid, H5P_DEFAULT, buf);
    	H5Tclose(tid);
    	H5Tclose(tid2);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_chunked_vl_vl */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_compressed_vl_vl
 *
 * Purpose:     Create a dataset that uses a named variable length datatype
 *              in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao
 *              Saturday, March 11, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_compressed_vl_vl(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t tid = -1, tid2=-1;       /* Datatype ID */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t pid = -1;                             /* Dataset creation property list ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    unsigned int i, j, k;                       /* Local index variables */
    hsize_t dim1d[1];                           /* Dataset dimensions */
    hvl_t buf[DIM_SIZE_1];                      /* Buffer for writing data */
    hvl_t *tvl;                                 /* Temporary pointer to VL information */
    hsize_t chunk_dim1d[1] = {CHUNK_SIZE_1};    /* Chunk dimensions */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): compressed dataset with nested VLEN datatype");

    /* set initial data values */
    for(i=0; i<DIM_SIZE_1; i++) {
        buf[i].p=HDmalloc((i+1)*sizeof(hvl_t));
        if(buf[i].p==NULL) {
            TestErrPrintf("Cannot allocate memory for VL data! i=%u\n",i);
            TEST_ERROR
        } /* end if */
        buf[i].len=i+1;
        for(tvl = (hvl_t *)buf[i].p,j=0; j<(i+1); j++, tvl++) {
            tvl->p=HDmalloc((j+1)*sizeof(unsigned int));
            if(tvl->p==NULL) {
                TestErrPrintf("Cannot allocate memory for VL data! i=%u, j=%u\n",i,j);
                TEST_ERROR
            } /* end if */
            tvl->len=j+1;
            for(k=0; k<(j+1); k++)
                ((unsigned int *)tvl->p)[k]=i*100+j*10+k;
        } /* end for */
    } /* end for */

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim1d[0]=DIM_SIZE_1;

    /* create dataspace */
    if((sid = H5Screate_simple(1, dim1d, NULL)) < 0) TEST_ERROR

    /* create datatype */
    if((tid = H5Tvlen_create(H5T_NATIVE_INT)) < 0) TEST_ERROR

    /* create nested VL datatype */
    if((tid2 = H5Tvlen_create(tid)) < 0) TEST_ERROR

     /* create and set chunk plist */
    if((pid = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR
    if(H5Pset_chunk(pid, 1, chunk_dim1d) < 0) TEST_ERROR
    if(H5Pset_deflate(pid, 9) < 0) TEST_ERROR

    /* create dataset at SRC file */
    if((did = H5Dcreate2(fid_src, NAME_DATASET_VL_VL, tid2, sid, H5P_DEFAULT, pid, H5P_DEFAULT)) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, tid2, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close compact plist */
    if(H5Pclose(pid) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST */
    if(H5Ocopy(fid_src, NAME_DATASET_VL_VL, fid_dst, NAME_DATASET_VL_VL, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the dataset for copy */
    if((did = H5Dopen2(fid_src, NAME_DATASET_VL_VL, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_VL_VL, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, buf) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR


    /* Reclaim vlen buffer */
    if(H5Dvlen_reclaim(tid2, sid, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close datatype */
    if(H5Tclose(tid) < 0) TEST_ERROR
    if(H5Tclose(tid2) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Pclose(pid);
    	H5Dclose(did2);
    	H5Dclose(did);
        H5Dvlen_reclaim(tid2, sid, H5P_DEFAULT, buf);
    	H5Tclose(tid);
    	H5Tclose(tid2);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_compressed_vl_vl */

/*
 * Common data structure for the copy_dataset_*_cmpd_vl tests.
 */
typedef struct cmpd_vl_t {
    int a;
    hvl_t b;
    double c;
} cmpd_vl_t;


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_contig_cmpd_vl
 *
 * Purpose:     Create a contiguous dataset w/VLEN datatype contained in
 *              a compound in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Neil Fortner
 *              Tuseday, September 29, 2009
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_contig_cmpd_vl(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t tid = -1;                             /* Datatype ID */
    hid_t tid2 = -1;                            /* Datatype ID */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1;                             /* Dataset ID */
    hid_t did2 = -1;                            /* Dataset ID */
    unsigned int i, j;                          /* Local index variables */
    hsize_t dim1d[1];                           /* Dataset dimensions */
    cmpd_vl_t buf[DIM_SIZE_1];                  /* Buffer for writing data */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): contiguous dataset with compound VLEN datatype");

    /* set initial data values */
    for(i = 0; i < DIM_SIZE_1; i++) {
        buf[i].a = i * (i - 1);
        buf[i].b.len = i+1;
        buf[i].b.p = (int *)HDmalloc(buf[i].b.len * sizeof(int));
        for(j = 0; j < buf[i].b.len; j++)
            ((int *)buf[i].b.p)[j] = (int)(i * 10 + j);
        buf[i].c = 1. / (i + 1.);
    } /* end for */

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim1d[0]=DIM_SIZE_1;

    /* create dataspace */
    if((sid = H5Screate_simple(1, dim1d, NULL)) < 0) TEST_ERROR

    /* create datatype */
    if((tid2 = H5Tvlen_create(H5T_NATIVE_INT)) < 0) TEST_ERROR
    if((tid = H5Tcreate(H5T_COMPOUND, sizeof(cmpd_vl_t))) < 0) TEST_ERROR
    if(H5Tinsert(tid, "a", HOFFSET(cmpd_vl_t, a), H5T_NATIVE_INT) < 0) TEST_ERROR
    if(H5Tinsert(tid, "b", HOFFSET(cmpd_vl_t, b), tid2) < 0) TEST_ERROR
    if(H5Tinsert(tid, "c", HOFFSET(cmpd_vl_t, c), H5T_NATIVE_DOUBLE) < 0) TEST_ERROR

    /* create dataset at SRC file */
    if((did = H5Dcreate2(fid_src, NAME_DATASET_CMPD_VL, tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST */
    if(H5Ocopy(fid_src, NAME_DATASET_CMPD_VL, fid_dst, NAME_DATASET_CMPD_VL, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the dataset for copy */
    if((did = H5Dopen2(fid_src, NAME_DATASET_CMPD_VL, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_CMPD_VL, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, buf) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR


    /* Reclaim vlen buffer */
    if(H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close datatype */
    if(H5Tclose(tid2) < 0) TEST_ERROR
    if(H5Tclose(tid) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
        H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, buf);
        H5Tclose(tid2);
    	H5Tclose(tid);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_contig_cmpd_vl */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_chunked_cmpd_vl
 *
 * Purpose:     Create a chunked dataset w/VLEN datatype contained in a
 *              compound in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Neil Fortner
 *              Wednesdat, September 30 , 2009
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_chunked_cmpd_vl(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t tid = -1, tid2 = -1;                  /* Datatype IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t pid = -1;                             /* Dataset creation property list ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    unsigned int i, j;                          /* Local index variables */
    hsize_t dim1d[1];                           /* Dataset dimensions */
    hsize_t chunk_dim1d[1] = {CHUNK_SIZE_1};    /* Chunk dimensions */
    cmpd_vl_t buf[DIM_SIZE_1];                      /* Buffer for writing data */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): chunked dataset with compound VLEN datatype");

    /* set initial data values */
    for(i = 0; i < DIM_SIZE_1; i++) {
        buf[i].a = i * (i - 1);
        buf[i].b.len = i+1;
        buf[i].b.p = (int *)HDmalloc(buf[i].b.len * sizeof(int));
        for(j = 0; j < buf[i].b.len; j++)
            ((int *)buf[i].b.p)[j] = (int)(i * 10 + j);
        buf[i].c = 1. / (i + 1.);
    } /* end for */

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim1d[0]=DIM_SIZE_1;

    /* create dataspace */
    if((sid = H5Screate_simple(1, dim1d, NULL)) < 0) TEST_ERROR

    /* create datatype */
    if((tid2 = H5Tvlen_create(H5T_NATIVE_INT)) < 0) TEST_ERROR
    if((tid = H5Tcreate(H5T_COMPOUND, sizeof(cmpd_vl_t))) < 0) TEST_ERROR
    if(H5Tinsert(tid, "a", HOFFSET(cmpd_vl_t, a), H5T_NATIVE_INT) < 0) TEST_ERROR
    if(H5Tinsert(tid, "b", HOFFSET(cmpd_vl_t, b), tid2) < 0) TEST_ERROR
    if(H5Tinsert(tid, "c", HOFFSET(cmpd_vl_t, c), H5T_NATIVE_DOUBLE) < 0) TEST_ERROR

    /* create and set chunk plist */
    if((pid = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR
    if(H5Pset_chunk(pid, 1, chunk_dim1d) < 0) TEST_ERROR

    /* create dataset at SRC file */
    if((did = H5Dcreate2(fid_src, NAME_DATASET_CMPD_VL, tid, sid, H5P_DEFAULT, pid, H5P_DEFAULT)) < 0) TEST_ERROR

    /* close chunk plist */
    if(H5Pclose(pid) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST */
    if(H5Ocopy(fid_src, NAME_DATASET_CMPD_VL, fid_dst, NAME_DATASET_CMPD_VL, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the dataset for copy */
    if((did = H5Dopen2(fid_src, NAME_DATASET_CMPD_VL, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_CMPD_VL, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, buf) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR


    /* Reclaim vlen buffer */
    if(H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close datatype */
    if(H5Tclose(tid2) < 0) TEST_ERROR
    if(H5Tclose(tid) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
        H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, buf);
        H5Tclose(tid2);
    	H5Tclose(tid);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_chunked_cmpd_vl */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_compact_cmpd_vl
 *
 * Purpose:     Create a compact dataset w/VLEN datatype contained in a
 *              compound in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao
 *              Sunday, December 11, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_compact_cmpd_vl(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t tid = -1, tid2 = -1;                  /* Datatype IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t pid = -1;                             /* Dataset creation property list ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    unsigned int i, j;                          /* Local index variables */
    hsize_t dim1d[1];                           /* Dataset dimensions */
    cmpd_vl_t buf[DIM_SIZE_1];                      /* Buffer for writing data */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): compact dataset with compound VLEN datatype");

    /* set initial data values */
    for(i = 0; i < DIM_SIZE_1; i++) {
        buf[i].a = i * (i - 1);
        buf[i].b.len = i+1;
        buf[i].b.p = (int *)HDmalloc(buf[i].b.len * sizeof(int));
        for(j = 0; j < buf[i].b.len; j++)
            ((int *)buf[i].b.p)[j] = (int)(i * 10 + j);
        buf[i].c = 1. / (i + 1.);
    } /* end for */

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim1d[0]=DIM_SIZE_1;

    /* create dataspace */
    if((sid = H5Screate_simple(1, dim1d, NULL)) < 0) TEST_ERROR

    /* create datatype */
    if((tid2 = H5Tvlen_create(H5T_NATIVE_INT)) < 0) TEST_ERROR
    if((tid = H5Tcreate(H5T_COMPOUND, sizeof(cmpd_vl_t))) < 0) TEST_ERROR
    if(H5Tinsert(tid, "a", HOFFSET(cmpd_vl_t, a), H5T_NATIVE_INT) < 0) TEST_ERROR
    if(H5Tinsert(tid, "b", HOFFSET(cmpd_vl_t, b), tid2) < 0) TEST_ERROR
    if(H5Tinsert(tid, "c", HOFFSET(cmpd_vl_t, c), H5T_NATIVE_DOUBLE) < 0) TEST_ERROR

    /* create and set compact plist */
    if((pid = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR
    if(H5Pset_layout(pid, H5D_COMPACT) < 0) TEST_ERROR

    /* create dataset at SRC file */
    if((did = H5Dcreate2(fid_src, NAME_DATASET_CMPD_VL, tid, sid, H5P_DEFAULT, pid, H5P_DEFAULT)) < 0) TEST_ERROR

    /* write data into file */
    if(H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close compact plist */
    if(H5Pclose(pid) < 0) TEST_ERROR

    /* close the dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR


    /* open the source file with read-only */
    if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* copy the dataset from SRC to DST */
    if(H5Ocopy(fid_src, NAME_DATASET_CMPD_VL, fid_dst, NAME_DATASET_CMPD_VL, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* open the dataset for copy */
    if((did = H5Dopen2(fid_src, NAME_DATASET_CMPD_VL, H5P_DEFAULT)) < 0) TEST_ERROR

    /* open the destination dataset */
    if((did2 = H5Dopen2(fid_dst, NAME_DATASET_CMPD_VL, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check if the datasets are equal */
    if(compare_datasets(did, did2, H5P_DEFAULT, buf) != TRUE) TEST_ERROR

    /* close the destination dataset */
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* close the source dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR


    /* Reclaim vlen buffer */
    if(H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close datatype */
    if(H5Tclose(tid2) < 0) TEST_ERROR
    if(H5Tclose(tid) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
        H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, buf);
        H5Tclose(tid2);
    	H5Tclose(tid);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_compact_cmpd_vl */


/*-------------------------------------------------------------------------
 * Function:    test_copy_null_ref
 *
 * Purpose:     Creates 2 datasets with references, one with object and
 *              the other with region references.  Copies these datasets
 *              to a new file without expanding references, causing them
 *              to become NULL.  Next, copies these references to a third
 *              file with expanding references, to verify that NULL
 *              references are handled correctly.
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Neil Fortner
 *              Wednesday, March 31, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_null_ref(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl)
{
    hid_t fid1 = -1, fid2 = -1;                 /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t pid = -1;                             /* Object copy property list ID */
    hid_t did1 = -1, did2 = -1;                 /* Dataset IDs */
    hsize_t dim1d[1] = {2};                     /* Dataset dimensions */
    hobj_ref_t obj_buf[2];                      /* Buffer for object refs */
    hdset_reg_ref_t reg_buf[2];                 /* Buffer for region refs */
    char zeros[MAX(sizeof(obj_buf),sizeof(reg_buf))]; /* Array of zeros, for memcmp */
    char src_filename[NAME_BUF_SIZE];
    char mid_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Ocopy(): NULL references");

    /* Initialize "zeros" array */
    HDmemset(zeros, 0, sizeof(zeros));

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, mid_filename, sizeof mid_filename);
    h5_fixname(FILENAME[2], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* Create source file */
    if((fid1 = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0)
        TEST_ERROR

    /* Create dataspace */
    if((sid = H5Screate_simple(1, dim1d, NULL)) < 0) TEST_ERROR

    /* Create object reference dataset at SRC file */
    if((did1 = H5Dcreate2(fid1, "obj_ref_dset", H5T_STD_REF_OBJ, sid,
            H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Create region reference dataset at SRC file */
    if((did2 = H5Dcreate2(fid1, "reg_ref_dset", H5T_STD_REF_DSETREG,
            sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Create references */
    if(H5Rcreate(&obj_buf[0], did1, ".", H5R_OBJECT, -1) < 0) TEST_ERROR
    if(H5Rcreate(&obj_buf[1], did2, ".", H5R_OBJECT, -1) < 0) TEST_ERROR
    if(H5Rcreate(&reg_buf[0], did1, ".", H5R_DATASET_REGION, sid) < 0)
        TEST_ERROR
    if(H5Rcreate(&reg_buf[1], did2, ".", H5R_DATASET_REGION, sid) < 0)
        TEST_ERROR

    /* Write data into file */
    if(H5Dwrite(did1, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, H5P_DEFAULT, obj_buf)
            < 0) TEST_ERROR
    if(H5Dwrite(did2, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT,
            reg_buf) < 0) TEST_ERROR

    /* Close datasets */
    if(H5Dclose(did1) < 0) TEST_ERROR
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* Create middle file */
    if((fid2 = H5Fcreate(mid_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0)
        TEST_ERROR

    /* Copy the source file to the middle file.  Note the expand references
     * flag is not set. */
    if(H5Ocopy(fid1, "/", fid2, "/A", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close source file */
    if(H5Fclose(fid1) < 0) TEST_ERROR

    /* Open copied datasets */
    if((did1 = H5Dopen2(fid2, "/A/obj_ref_dset", H5P_DEFAULT)) < 0) TEST_ERROR
    if((did2 = H5Dopen2(fid2, "/A/reg_ref_dset", H5P_DEFAULT)) < 0) TEST_ERROR

    /* Read copied datasets */
    if(H5Dread(did1, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, H5P_DEFAULT, obj_buf)
            < 0) TEST_ERROR
    if(H5Dread(did2, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT,
            reg_buf) < 0) TEST_ERROR

    /* Verify that the references contain only "0" bytes */
    if(HDmemcmp(obj_buf, zeros, sizeof(obj_buf))) TEST_ERROR
    if(HDmemcmp(reg_buf, zeros, sizeof(reg_buf))) TEST_ERROR

    /* Close datasets */
    if(H5Dclose(did1) < 0) TEST_ERROR
    if(H5Dclose(did2) < 0) TEST_ERROR

    /* Create destination file */
    if((fid1 = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0)
        TEST_ERROR

    /* Create object copy property list */
    if((pid = H5Pcreate(H5P_OBJECT_COPY)) < 0) TEST_ERROR

    /* Set the "expand references" flag */
    if(H5Pset_copy_object(pid, H5O_COPY_EXPAND_REFERENCE_FLAG) < 0) TEST_ERROR

    /* Copy the middle file to the destination file.  Note the expand references
     * flag *is* set, even though the references are now NULL. */
    if(H5Ocopy(fid2, "/", fid1, "/AA", pid, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close source file */
    if(H5Fclose(fid2) < 0) TEST_ERROR

    /* Open copied datasets */
    if((did1 = H5Dopen2(fid1, "/AA/A/obj_ref_dset", H5P_DEFAULT)) < 0) TEST_ERROR
    if((did2 = H5Dopen2(fid1, "/AA/A/reg_ref_dset", H5P_DEFAULT)) < 0) TEST_ERROR

    /* Read copied datasets */
    if(H5Dread(did1, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, H5P_DEFAULT, obj_buf)
            < 0) TEST_ERROR
    if(H5Dread(did2, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT,
            reg_buf) < 0) TEST_ERROR

    /* Verify that the references contain only "0" bytes */
    if(HDmemcmp(obj_buf, zeros, sizeof(obj_buf))) TEST_ERROR
    if(HDmemcmp(reg_buf, zeros, sizeof(reg_buf))) TEST_ERROR

    /* Close */
    if(H5Pclose(pid) < 0) TEST_ERROR
    if(H5Dclose(did1) < 0) TEST_ERROR
    if(H5Dclose(did2) < 0) TEST_ERROR
    if(H5Fclose(fid1) < 0) TEST_ERROR
    if(H5Sclose(sid) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(pid);
        H5Dclose(did1);
        H5Dclose(did2);
        H5Fclose(fid1);
        H5Fclose(fid2);
        H5Sclose(sid);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_null_ref */


/*-------------------------------------------------------------------------
 * Function:    test_copy_option
 *
 * Purpose:     Create a group in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao
 *               March 11, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_option(hid_t fcpl_src, hid_t fcpl_dst, hid_t fapl, unsigned flag, hbool_t crt_intermediate_grp, const char* test_desciption)
{
    hid_t fid_src = -1, fid_dst = -1, fid_ext = -1; /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1;                             /* Dataset ID */
    hid_t gid=-1, gid2=-1, gid_ref=-1;          /* Group IDs */
    hid_t gid_sub=-1, gid_sub_sub=-1;           /* Sub-group ID */
    hid_t pid=-1, lcpl_id=-1;			/* Property IDs */
    unsigned cpy_flags;                         /* Object copy flags */
    int depth = -1;                             /* Copy depth */
    hsize_t dim2d[2];
    int buf[DIM_SIZE_1][DIM_SIZE_2];
    int i, j;
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING(test_desciption);

    /* set initial data values */
    for (i=0; i<DIM_SIZE_1; i++)
        for (j=0; j<DIM_SIZE_2; j++)
            buf[i][j] = 10000 + 100*i+j;

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR

    /* create group at the SRC file */
    if((gid = H5Gcreate2(fid_src, NAME_GROUP_TOP, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* attach attributes to the group */
    if(test_copy_attach_attributes(gid, H5T_NATIVE_INT) < 0) TEST_ERROR

    /* Set dataspace dimensions */
    dim2d[0]=DIM_SIZE_1;
    dim2d[1]=DIM_SIZE_2;

    /* create dataspace */
    if((sid = H5Screate_simple(2, dim2d, NULL)) < 0) TEST_ERROR

    /* add a dataset to the top group */
    if((did = H5Dcreate2(gid, NAME_DATASET_SIMPLE, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR
    if(H5Dclose(did) < 0) TEST_ERROR

    /* create a sub-group */
    if((gid_sub = H5Gcreate2(fid_src, NAME_GROUP_SUB, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* add a dataset to the sub group */
    if((did = H5Dcreate2(gid_sub, NAME_DATASET_SIMPLE, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR
    if(H5Dclose(did) < 0) TEST_ERROR

    /* create sub-sub-group */
    if((gid_sub_sub = H5Gcreate2(gid_sub, NAME_GROUP_SUB_SUB2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* add a dataset to the sub sub group */
    if((did = H5Dcreate2(gid_sub_sub, NAME_DATASET_SIMPLE, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* close dataset */
    if(H5Dclose(did) < 0) TEST_ERROR

    /* close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    if(H5Gclose(gid_sub_sub) < 0) TEST_ERROR

    if(H5Gclose(gid_sub) < 0) TEST_ERROR

    /* close the group */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    if((flag & H5O_COPY_EXPAND_SOFT_LINK_FLAG) > 0) {
        /* Create group to copy */
        if((gid = H5Gcreate2(fid_src, NAME_GROUP_LINK, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
        if(H5Lcreate_soft(NAME_DATASET_SUB_SUB, fid_src, NAME_LINK_SOFT, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
        if(H5Lcreate_soft("nowhere", fid_src, NAME_LINK_SOFT_DANGLE, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
        if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

        /* Create group to compare with */
        if((gid = H5Gcreate2(fid_src, NAME_GROUP_LINK2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
        if(H5Lcreate_hard(fid_src, NAME_DATASET_SUB_SUB, H5L_SAME_LOC, NAME_LINK_SOFT2, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
        if(H5Lcreate_soft("nowhere", fid_src, NAME_LINK_SOFT_DANGLE2, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
        if(H5Gclose(gid) < 0) FAIL_STACK_ERROR
    } /* end if */

    if((flag & H5O_COPY_EXPAND_EXT_LINK_FLAG) > 0) {
        char    ext_filename[NAME_BUF_SIZE];

        h5_fixname(FILENAME[2], fapl, ext_filename, sizeof ext_filename);

        /* Create the external file and dataset */
        if((fid_ext = H5Fcreate(ext_filename, H5F_ACC_TRUNC, fcpl_src, fapl)) < 0) TEST_ERROR
        if((sid = H5Screate_simple(2, dim2d, NULL)) < 0) TEST_ERROR
        if((did = H5Dcreate2(fid_ext, NAME_DATASET_SIMPLE, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
        if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR
        if(H5Dclose(did) < 0) TEST_ERROR
        if(H5Fclose(fid_ext) < 0) TEST_ERROR

        /* Create group to copy */
        if(!(flag & H5O_COPY_EXPAND_SOFT_LINK_FLAG)) {
            if((gid = H5Gcreate2(fid_src, NAME_GROUP_LINK, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
        } /* end if */
        else
            if((gid = H5Gopen2(fid_src, NAME_GROUP_LINK, H5P_DEFAULT)) < 0) TEST_ERROR
        if(H5Lcreate_external(ext_filename, NAME_DATASET_SIMPLE, fid_src, NAME_LINK_EXTERN, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
        if(H5Lcreate_external("no_file", "no_object", fid_src, NAME_LINK_EXTERN_DANGLE, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
        if(H5Gclose(gid) < 0) TEST_ERROR

        /* Create group to compare with */
        if(!(flag & H5O_COPY_EXPAND_SOFT_LINK_FLAG)) {
            if((gid = H5Gcreate2(fid_src, NAME_GROUP_LINK2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
        } /* end if */
        else
            if((gid = H5Gopen2(fid_src, NAME_GROUP_LINK2, H5P_DEFAULT)) < 0) TEST_ERROR
        if((did = H5Dcreate2(fid_src, NAME_LINK_EXTERN2, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
        if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR
        if(H5Lcreate_external("no_file", "no_object", fid_src, NAME_LINK_EXTERN_DANGLE2, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
        if(H5Dclose(did) < 0) TEST_ERROR
        if(H5Gclose(gid) < 0) TEST_ERROR

        /* Close dataspace */
        if(H5Sclose(sid) < 0) TEST_ERROR
    } /* end if */

    if((flag & H5O_COPY_EXPAND_REFERENCE_FLAG) > 0) {
        if((gid_ref = H5Gcreate2(fid_src, NAME_GROUP_REF, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

        /* create an attribute of object references */
        if(attach_ref_attr(fid_src, gid_ref) < 0) TEST_ERROR

        /* create an attribute of region references */
        if(attach_reg_ref_attr(fid_src, gid_ref) < 0) TEST_ERROR

        /* create a dataset of region references */
        if(create_reg_ref_dataset(fid_src, gid_ref) < 0) TEST_ERROR

        /* Close group holding reference objects */
        if(H5Gclose(gid_ref) < 0) TEST_ERROR
    } /* end if */

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* open the source file with read-only */
    /* (except when expanding soft links */
    if((flag & H5O_COPY_EXPAND_SOFT_LINK_FLAG) > 0) {
        if((fid_src = H5Fopen(src_filename, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR
    } /* end if */
    else
        if((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* create destination file */
    if((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, fapl)) < 0) TEST_ERROR

    /* Create an uncopied object in destination file so that addresses in source and destination
       files aren't the same */
    if(H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* create property to pass copy options */
    if((pid = H5Pcreate(H5P_OBJECT_COPY)) < 0) TEST_ERROR

    /* set options for object copy */
    if(H5Pset_copy_object(pid, flag) < 0) TEST_ERROR

    /* Verify object copy flags */
    if(H5Pget_copy_object(pid, &cpy_flags) < 0) TEST_ERROR
    if(cpy_flags != flag) TEST_ERROR

    /* copy the group from SRC to DST */
    if(crt_intermediate_grp) {
        /* Create link creation plist to pass in intermediate group creation */
        if((lcpl_id = H5Pcreate(H5P_LINK_CREATE)) < 0) TEST_ERROR
        if(H5Pset_create_intermediate_group(lcpl_id, TRUE) < 0) TEST_ERROR

        if(H5Ocopy(fid_src, NAME_GROUP_TOP, fid_dst, "/new_g0/new_g00", pid, lcpl_id) < 0) TEST_ERROR

        if(H5Pclose(lcpl_id) < 0) TEST_ERROR

        /* open the group for copy */
        if((gid = H5Gopen2(fid_src, NAME_GROUP_TOP, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

        /* open the destination group */
        if((gid2 = H5Gopen2(fid_dst, "/new_g0/new_g00", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    } else if(((flag & H5O_COPY_EXPAND_SOFT_LINK_FLAG) > 0)
            || ((flag & H5O_COPY_EXPAND_EXT_LINK_FLAG) > 0)) {
        if(H5Ocopy(fid_src, NAME_GROUP_LINK, fid_dst, NAME_GROUP_LINK, pid, H5P_DEFAULT) < 0) TEST_ERROR

        if((flag & H5O_COPY_EXPAND_SOFT_LINK_FLAG) > 0)
            /* Unlink dataset to copy from original location */
            /* (So group comparison works properly) */
            if(H5Ldelete(fid_src, NAME_DATASET_SUB_SUB, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

        /* open the group for copy */
        if((gid = H5Gopen2(fid_src, NAME_GROUP_LINK2, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

        /* open the destination group */
        if((gid2 = H5Gopen2(fid_dst, NAME_GROUP_LINK, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    } else if(flag & (H5O_COPY_WITHOUT_ATTR_FLAG | H5O_COPY_PRESERVE_NULL_FLAG)) {
        if(H5Ocopy(fid_src, NAME_GROUP_TOP, fid_dst, NAME_GROUP_TOP, pid, H5P_DEFAULT) < 0) TEST_ERROR

        /* open the group for copy */
        if((gid = H5Gopen2(fid_src, NAME_GROUP_TOP, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

        /* open the destination group */
        if((gid2 = H5Gopen2(fid_dst, NAME_GROUP_TOP, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    } else if(flag & H5O_COPY_SHALLOW_HIERARCHY_FLAG) {
        if(H5Ocopy(fid_src, NAME_GROUP_TOP, fid_dst, NAME_GROUP_TOP, pid, H5P_DEFAULT) < 0) TEST_ERROR

        /* open the group for copy */
        if((gid = H5Gopen2(fid_src, NAME_GROUP_TOP, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

        /* open the destination group */
        if((gid2 = H5Gopen2(fid_dst, NAME_GROUP_TOP, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

        /* Set the copy depth */
        depth = 1;
    } else if((flag & H5O_COPY_EXPAND_REFERENCE_FLAG) > 0) {
        if(H5Ocopy(fid_src, NAME_GROUP_REF, fid_dst, NAME_GROUP_REF, pid, H5P_DEFAULT) < 0) TEST_ERROR

        /* open the group for copy */
        if((gid = H5Gopen2(fid_src, NAME_GROUP_REF, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

        /* open the destination group */
        if((gid2 = H5Gopen2(fid_dst, NAME_GROUP_REF, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    } else {
        /* Unknown flag */
        TEST_ERROR
    } /* end else */

    /* Check if the groups are equal */
    if(compare_groups(gid, gid2, pid, depth, flag) != TRUE) TEST_ERROR
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* close the SRC file */
    if(H5Fclose(fid_src) < 0) TEST_ERROR

    /* close the DST file */
    if(H5Fclose(fid_dst) < 0) TEST_ERROR

    /* close properties */
    if(H5Pclose(pid) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(lcpl_id);
        H5Pclose(pid);
    	H5Sclose(sid);
    	H5Dclose(did);
    	H5Gclose(gid_ref);
    	H5Gclose(gid_sub);
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    	H5Fclose(fid_ext);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_option */


/*-------------------------------------------------------------------------
 * Function:   	main
 *
 * Purpose:     Test H5Ocopy()
 *
 *              Tests a number of cases: messages can be stored in the
 *              new or old format, messages can be shared in either,
 *              both, or neither of the source and destination files.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Peter Cao
 *              Friday, September 30, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    int     nerrors = 0;
    hid_t	fapl, fapl2;
    hid_t   fcpl_shared, ocpl;
    unsigned    max_compact, min_dense;
    int     configuration;  /* Configuration of tests. */
    int	ExpressMode;

    /* Setup */
    h5_reset();
    fapl = h5_fileaccess();

    ExpressMode = GetTestExpress();
    if (ExpressMode > 1)
        printf("***Express test mode on.  Some tests may be skipped\n");

    /* Copy the file access property list */
    if((fapl2 = H5Pcopy(fapl)) < 0) TEST_ERROR

    /* Set the "use the latest version of the format" bounds for creating objects in the file */
    if(H5Pset_libver_bounds(fapl2, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0) TEST_ERROR

    /* Create an FCPL with sharing enabled */
    if((fcpl_shared = H5Pcreate(H5P_FILE_CREATE)) < 0) TEST_ERROR
    if(H5Pset_shared_mesg_nindexes(fcpl_shared, 1) < 0) TEST_ERROR
    if(H5Pset_shared_mesg_index(fcpl_shared, 0, H5O_SHMESG_ALL_FLAG, 10) < 0) TEST_ERROR

    /* Obtain the default attribute storage phase change values */
    if((ocpl = H5Pcreate(H5P_OBJECT_CREATE)) < 0) TEST_ERROR
    if(H5Pget_attr_phase_change(ocpl, &max_compact, &min_dense) < 0) TEST_ERROR
    if(H5Pclose(ocpl) < 0) TEST_ERROR

    /* Test in all configurations */
    for(configuration = 0; configuration <= MAX_CONFIGURATION; configuration++) {
        hid_t my_fapl;
        hid_t fcpl_src;
        hid_t fcpl_dst;

        /* No need to test dense attributes with old format */
        if(!(configuration & CONFIG_NEW_FORMAT) && (configuration & CONFIG_DENSE))
            continue;

        /* Test with and without shared messages */
        if(configuration & CONFIG_SHARE_SRC) {
            puts("\nTesting with shared src messages:");
            fcpl_src = fcpl_shared;
        }
        else {
            puts("\nTesting without shared src messages:");
            fcpl_src = H5P_DEFAULT;
        }
        if(configuration & CONFIG_SHARE_DST) {
            puts("Testing with shared dst messages:");
            fcpl_dst = fcpl_shared;
        }
        else {
            puts("Testing without shared dst messages:");
            fcpl_dst = H5P_DEFAULT;
        }

        /* Set the FAPL for the type of format */
        if(configuration & CONFIG_NEW_FORMAT) {
            puts("Testing with new group format:");
            my_fapl = fapl2;

                /* Test with and without dense attributes */
            if(configuration & CONFIG_DENSE) {
                puts("Testing with dense attributes:");
                num_attributes_g = max_compact + 1;
            }
            else {
                puts("Testing without dense attributes:");
                num_attributes_g = MAX(min_dense, 2) - 1;
            }
        } /* end if */
        else {
            puts("Testing with old group format:");
            my_fapl = fapl;
            num_attributes_g = 4;
        } /* end else */

        /* The tests... */
        nerrors += test_copy_dataset_simple(fcpl_src, fcpl_dst, my_fapl);
        nerrors += test_copy_dataset_simple_samefile(fcpl_src, my_fapl);
        nerrors += test_copy_dataset_simple_empty(fcpl_src, fcpl_dst, my_fapl);
        nerrors += test_copy_dataset_compound(fcpl_src, fcpl_dst, my_fapl);
        nerrors += test_copy_dataset_chunked(fcpl_src, fcpl_dst, my_fapl);
        nerrors += test_copy_dataset_chunked_empty(fcpl_src, fcpl_dst, my_fapl);
        nerrors += test_copy_dataset_chunked_sparse(fcpl_src, fcpl_dst, my_fapl);
        nerrors += test_copy_dataset_compressed(fcpl_src, fcpl_dst, my_fapl);
        nerrors += test_copy_dataset_compact(fcpl_src, fcpl_dst, my_fapl);
        nerrors += test_copy_dataset_multi_ohdr_chunks(fcpl_src, fcpl_dst, my_fapl);
        nerrors += test_copy_dataset_attr_named_dtype(fcpl_src, fcpl_dst, my_fapl);

        nerrors += test_copy_group_empty(fcpl_src, fcpl_dst, my_fapl);
        nerrors += test_copy_root_group(fcpl_src, fcpl_dst, my_fapl);
        nerrors += test_copy_group(fcpl_src, fcpl_dst, my_fapl);
        nerrors += test_copy_group_deep(fcpl_src, fcpl_dst, my_fapl);
        nerrors += test_copy_group_loop(fcpl_src, fcpl_dst, my_fapl);
        nerrors += test_copy_group_wide_loop(fcpl_src, fcpl_dst, my_fapl);
        nerrors += test_copy_group_links(fcpl_src, fcpl_dst, my_fapl);
        nerrors += test_copy_soft_link(fcpl_src, fcpl_dst, my_fapl);
#ifndef H5_CANNOT_OPEN_TWICE
        nerrors += test_copy_ext_link(fcpl_src, fcpl_dst, my_fapl);
#endif /* H5_CANNOT_OPEN_TWICE */
        nerrors += test_copy_exist(fcpl_src, fcpl_dst, my_fapl);
        nerrors += test_copy_path(fcpl_src, fcpl_dst, my_fapl);

        nerrors += test_copy_option(fcpl_src, fcpl_dst, my_fapl, H5O_COPY_WITHOUT_ATTR_FLAG,
                   FALSE, "H5Ocopy(): without attributes");
        nerrors += test_copy_option(fcpl_src, fcpl_dst, my_fapl, 0, TRUE,
                   "H5Ocopy(): with missing groups");
        nerrors += test_copy_option(fcpl_src, fcpl_dst, my_fapl, H5O_COPY_EXPAND_SOFT_LINK_FLAG,
                   FALSE, "H5Ocopy(): expand soft link");
        nerrors += test_copy_option(fcpl_src, fcpl_dst, my_fapl, H5O_COPY_EXPAND_EXT_LINK_FLAG,
                    FALSE, "H5Ocopy: expand external link");
        nerrors += test_copy_option(fcpl_src, fcpl_dst, my_fapl,
                    H5O_COPY_EXPAND_SOFT_LINK_FLAG | H5O_COPY_EXPAND_EXT_LINK_FLAG,
                    FALSE, "H5Ocopy: expand soft and external links");
        nerrors += test_copy_option(fcpl_src, fcpl_dst, my_fapl, H5O_COPY_SHALLOW_HIERARCHY_FLAG,
                   FALSE, "H5Ocopy(): shallow group copy");
        nerrors += test_copy_option(fcpl_src, fcpl_dst, my_fapl, H5O_COPY_EXPAND_REFERENCE_FLAG,
                   FALSE, "H5Ocopy(): expand object reference");
        nerrors += test_copy_option(fcpl_src, fcpl_dst, my_fapl, H5O_COPY_PRESERVE_NULL_FLAG,
                   FALSE, "H5Ocopy(): preserve NULL messages");
        nerrors += test_copy_option(fcpl_src, fcpl_dst, my_fapl, H5O_COPY_WITHOUT_ATTR_FLAG |
                   H5O_COPY_PRESERVE_NULL_FLAG, TRUE, "H5Ocopy(): preserve NULL messages");

        /* Tests that do not use attributes and do not need to be tested
         * multiple times for different attribute configurations */
        if(configuration < CONFIG_DENSE) {
            nerrors += test_copy_named_datatype(fcpl_src, fcpl_dst, my_fapl);
            nerrors += test_copy_named_datatype_vl(fcpl_src, fcpl_dst, my_fapl);
            nerrors += test_copy_named_datatype_vl_vl(fcpl_src, fcpl_dst, my_fapl);

            nerrors += test_copy_dataset_external(fcpl_src, fcpl_dst, my_fapl);
            nerrors += test_copy_dataset_named_dtype(fcpl_src, fcpl_dst, my_fapl);
            nerrors += test_copy_dataset_named_dtype_hier(fcpl_src, fcpl_dst, my_fapl);
            nerrors += test_copy_dataset_named_dtype_hier_outside(fcpl_src, fcpl_dst, my_fapl);

            nerrors += test_copy_dataset_contig_vl(fcpl_src, fcpl_dst, my_fapl);
            nerrors += test_copy_dataset_chunked_vl(fcpl_src, fcpl_dst, my_fapl);
            nerrors += test_copy_dataset_compact_vl(fcpl_src, fcpl_dst, my_fapl);
            nerrors += test_copy_dataset_compressed_vl(fcpl_src, fcpl_dst, my_fapl);
            nerrors += test_copy_attribute_vl(fcpl_src, fcpl_dst, my_fapl);
            nerrors += test_copy_dataset_compact_named_vl(fcpl_src, fcpl_dst, my_fapl);
            nerrors += test_copy_dataset_contig_named_vl(fcpl_src, fcpl_dst, my_fapl);
            nerrors += test_copy_dataset_chunked_named_vl(fcpl_src, fcpl_dst, my_fapl);
            nerrors += test_copy_dataset_compressed_named_vl(fcpl_src, fcpl_dst, my_fapl);
            nerrors += test_copy_dataset_compact_vl_vl(fcpl_src, fcpl_dst, my_fapl);
            nerrors += test_copy_dataset_contig_vl_vl(fcpl_src, fcpl_dst, my_fapl);
            nerrors += test_copy_dataset_chunked_vl_vl(fcpl_src, fcpl_dst, my_fapl);
            nerrors += test_copy_dataset_compressed_vl_vl(fcpl_src, fcpl_dst, my_fapl);
            nerrors += test_copy_dataset_contig_cmpd_vl(fcpl_src, fcpl_dst, my_fapl);
            nerrors += test_copy_dataset_chunked_cmpd_vl(fcpl_src, fcpl_dst, my_fapl);
            nerrors += test_copy_dataset_compact_cmpd_vl(fcpl_src, fcpl_dst, my_fapl);

            nerrors += test_copy_same_file_named_datatype(fcpl_src, my_fapl);
            nerrors += test_copy_old_layout(fcpl_dst, my_fapl);
            nerrors += test_copy_null_ref(fcpl_src, fcpl_dst, my_fapl);
        }

/* TODO: not implemented
        nerrors += test_copy_mount(my_fapl);
*/
    } /* end for */

    /* Reset file address checking info */
    addr_reset();

    /* Verify symbol table messages are cached */
    nerrors += (h5_verify_cached_stabs(FILENAME, fapl) < 0 ? 1 : 0);

    /* Results */
    if(nerrors) {
        printf("***** %d OBJECT COPY TEST%s FAILED! *****\n",
                nerrors, (1 == nerrors ? "" : "S"));
        exit(1);
    } /* end if */

    puts ("All object copying tests passed.");

    h5_cleanup(FILENAME, fapl);

    return 0;

error:
    return 1;
} /* main */

