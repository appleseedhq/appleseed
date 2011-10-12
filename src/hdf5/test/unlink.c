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
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Friday, September 25, 1998
 *
 * Purpose:	Test unlinking operations.
 */

#define H5G_PACKAGE		/*suppress error about including H5Gpkg	  */

/* Define this macro to indicate that the testing APIs should be available */
#define H5G_TESTING

#include <time.h>
#include "h5test.h"
#include "H5Gpkg.h"		/* Groups				*/

const char *FILENAME[] = {
    "unlink",
    "new_move_a",
    "new_move_b",
    "lunlink",
    "filespace",
    "slashes",
    "resurrect_set",
    "resurrect_type",
    "resurrect_group",
    "unlink_chunked",
    "full_group",
    NULL
};

/* Macros for test_create_unlink() & test_filespace */
#define GROUPNAME       "group"
#define GROUP2NAME      "group2"
#define UNLINK_NGROUPS         1000
#define DATASETNAME     "dataset"
#define DATASET2NAME    "dataset2"
#define ATTRNAME        "attribute"
#define TYPENAME        "datatype"
#define TYPE2NAME       "datatype2"
#define FILESPACE_NDIMS 3
#define FILESPACE_DIM0  20
#define FILESPACE_DIM1  20
#define FILESPACE_DIM2  20
#define FILESPACE_CHUNK0  10
#define FILESPACE_CHUNK1  10
#define FILESPACE_CHUNK2  10
#define FILESPACE_DEFLATE_LEVEL 6
#define FILESPACE_REWRITE       10
#define FILESPACE_NATTR 100
#define FILESPACE_ATTR_NDIMS    2
#define FILESPACE_ATTR_DIM0     5
#define FILESPACE_ATTR_DIM1     5
#define FILESPACE_TOP_GROUPS    10
#define FILESPACE_NESTED_GROUPS 50
#define FILESPACE_NDATASETS     50
#define SLASHES_GROUP_NAME      "Group///"
#define SLASHES_HARDLINK_NAME   "Hard///"
#define SLASHES_SOFTLINK_NAME   "Soft///"
#define SLASHES_SOFTLINK2_NAME  "Soft2///"
#define SLASHES_ROOTLINK_NAME   "Root///"
#define FULL_GROUP_NUM_KEEP     2
#define FULL_GROUP_NUM_DELETE_COMPACT   2
#define FULL_GROUP_NUM_DELETE_DENSE     16
#define FULL_GROUP_EST_NUM_ENTRIES      8
#define FULL_GROUP_EST_ENTRY_LEN        9


/*-------------------------------------------------------------------------
 * Function:	test_one
 *
 * Purpose:	Creates a group that has just one entry and then unlinks that
 *		entry.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Friday, September 25, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_one(hid_t file)
{
    hid_t	work = -1, grp = -1;
    herr_t	status;

    /* Create a test group */
    if((work = H5Gcreate2(file, "/test_one", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Delete by absolute name */
    TESTING("unlink by absolute name");
    if((grp = H5Gcreate2(work, "foo", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Gclose(grp) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(file, "/test_one/foo", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    PASSED();

    /* Delete by local name */
    TESTING("unlink by local name");
    if((grp = H5Gcreate2(work, "foo", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Gclose(grp) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(work, "foo", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    PASSED();

    /* Delete directly - should fail */
    TESTING("unlink without a name");
    if((grp = H5Gcreate2(work, "foo", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    H5E_BEGIN_TRY {
	status = H5Ldelete(grp, ".", H5P_DEFAULT);
    } H5E_END_TRY;
    if(status>=0)
	FAIL_PUTS_ERROR("    Unlinking object w/o a name should have failed.")
    if(H5Gclose(grp) < 0) FAIL_STACK_ERROR

    /* Cleanup */
    if(H5Gclose(work) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Gclose(work);
	H5Gclose(grp);
    } H5E_END_TRY;
    return 1;
} /* end test_one() */


/*-------------------------------------------------------------------------
 * Function:	test_many
 *
 * Purpose:	Tests many unlinks in a single directory.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Friday, September 25, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_many(hid_t file)
{
    hid_t	work=-1, grp=-1;
    int		i;
    const int	how_many=500;
    char	name[32];

    /* Create a test group */
    if((work = H5Gcreate2(file, "/test_many", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) goto error;
    if((grp = H5Gcreate2(work, "/test_many_foo", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) goto error;
    if(H5Gclose(grp) < 0) goto error;

    /* Create a bunch of names and unlink them in order */
    TESTING("forward unlink");
    for(i = 0; i < how_many; i++) {
	sprintf(name, "obj_%05d", i);
	if(H5Lcreate_hard(work, "/test_many_foo", H5L_SAME_LOC, name, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    } /* end for */
    for(i = 0; i < how_many; i++) {
	sprintf(name, "obj_%05d", i);
	if(H5Ldelete(work, name, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    } /* end for */
    PASSED();

    /* Create a bunch of names and unlink them in reverse order */
    TESTING("backward unlink");
    for(i = 0; i < how_many; i++) {
	sprintf(name, "obj_%05d", i);
	if(H5Lcreate_hard(work, "/test_many_foo", H5L_SAME_LOC, name, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    } /* end for */
    for(i = (how_many - 1); i >= 0; --i) {
	sprintf(name, "obj_%05d", i);
	if(H5Ldelete(work, name, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    } /* end for */
    PASSED();

    /* Create a bunch of names and unlink them from both directions */
    TESTING("inward unlink");
    for(i = 0; i < how_many; i++) {
	sprintf(name, "obj_%05d", i);
	if(H5Lcreate_hard(work, "/test_many_foo", H5L_SAME_LOC, name, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    } /* end for */
    for(i = 0; i < how_many; i++) {
	if(i % 2)
	    sprintf(name, "obj_%05d", how_many - (1 + i / 2));
	else
	    sprintf(name, "obj_%05d", i / 2);
	if(H5Ldelete(work, name, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    } /* end for */
    PASSED();

    /* Create a bunch of names and unlink them from the midle */
    TESTING("outward unlink");
    for(i = 0; i < how_many; i++) {
	sprintf(name, "obj_%05d", i);
	if(H5Lcreate_hard(work, "/test_many_foo", H5L_SAME_LOC, name, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    } /* end for */
    for(i = (how_many - 1); i >= 0; --i) {
	if(i % 2)
	    sprintf(name, "obj_%05d", how_many - (1 + i / 2));
	else
	    sprintf(name, "obj_%05d", i / 2);
	if(H5Ldelete(work, name, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    } /* end for */
    PASSED();


    /* Cleanup */
    if (H5Gclose(work) < 0) goto error;
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Gclose(work);
	H5Gclose(grp);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_symlink
 *
 * Purpose:	Tests removal of symbolic links.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Friday, September 25, 1998
 *
 *-------------------------------------------------------------------------
 */
static int
test_symlink(hid_t file)
{
    hid_t	work=-1;

    TESTING("symlink removal");

    /* Create a test group and symlink */
    if((work = H5Gcreate2(file, "/test_symlink", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("link_value", work, "link", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(work, "link", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Cleanup */
    if(H5Gclose(work) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Gclose(work);
    } H5E_END_TRY;
    return 1;
} /* end test_symlink() */


/*-------------------------------------------------------------------------
 * Function:	test_rename
 *
 * Purpose:	Tests H5Lmove()
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Friday, September 25, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_rename(hid_t file)
{
    hid_t	work=-1, foo=-1, inner=-1;

    /* Create a test group and rename something */
    TESTING("object renaming");
    if((work = H5Gcreate2(file, "/test_rename", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((foo = H5Gcreate2(work, "foo", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Lmove(work, "foo", H5L_SAME_LOC, "bar", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if((inner = H5Gcreate2(foo, "inner", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Gclose(inner) < 0) FAIL_STACK_ERROR
    if(H5Gclose(foo) < 0) FAIL_STACK_ERROR
    if((inner = H5Gopen2(work, "bar/inner", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Gclose(inner) < 0) FAIL_STACK_ERROR
    PASSED();

    /* Try renaming a symlink */
    TESTING("symlink renaming");
    if(H5Lcreate_soft("link_value", work, "link_one", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lmove(work, "link_one", H5L_SAME_LOC, "link_two", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Cleanup */
    if (H5Gclose(work) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Gclose(work);
	H5Gclose(foo);
	H5Gclose(inner);
    } H5E_END_TRY;
    return 1;
} /* end test_rename() */


/*-------------------------------------------------------------------------
 * Function:    test_new_move
 *
 * Purpose:     Tests H5Lmove() with different locations
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 * Programmer:  Raymond Lu
 *              Thursday, April 25, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_new_move(hid_t fapl)
{
    hid_t 	file_a, file_b=(-1);
    hid_t	grp_1=(-1), grp_2=(-1), grp_move=(-1), moved_grp=(-1);
    char 	filename[1024];

    TESTING("new move");

    /* Create a second file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if((file_a = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR
    h5_fixname(FILENAME[2], fapl, filename, sizeof filename);
    if((file_b = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create groups in first file */
    if((grp_1 = H5Gcreate2(file_a, "group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((grp_2 = H5Gcreate2(file_a, "group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((grp_move = H5Gcreate2(grp_1, "group_move", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create hard and soft links. */
    if(H5Lcreate_hard(grp_1, "group_move", H5L_SAME_LOC, "hard", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("/group1/group_move", grp_2, "soft", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Move a group within the file.  Both of source and destination use
     * H5L_SAME_LOC.  Should fail. */
    H5E_BEGIN_TRY {
        if(H5Lmove(H5L_SAME_LOC, "group_move", H5L_SAME_LOC, "group_new_name", H5P_DEFAULT, H5P_DEFAULT) != FAIL) TEST_ERROR
    } H5E_END_TRY;

    /* Move a group across files.  Should fail. */
    H5E_BEGIN_TRY {
        if(H5Lmove(grp_1, "group_move", file_b, "group_new_name", H5P_DEFAULT, H5P_DEFAULT) != FAIL) TEST_ERROR
    } H5E_END_TRY;

    /* Move a group across groups in the same file. */
    if(H5Lmove(grp_1, "group_move", grp_2, "group_new_name", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Open the group just moved to the new location. */
    if((moved_grp = H5Gopen2(grp_2, "group_new_name", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    if(H5Gclose(grp_1) < 0) FAIL_STACK_ERROR
    if(H5Gclose(grp_2) < 0) FAIL_STACK_ERROR
    if(H5Gclose(grp_move) < 0) FAIL_STACK_ERROR
    if(H5Gclose(moved_grp) < 0) FAIL_STACK_ERROR
    if(H5Fclose(file_a) < 0) FAIL_STACK_ERROR
    if(H5Fclose(file_b) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

  error:
    H5E_BEGIN_TRY {
 	H5Gclose(grp_1);
	H5Gclose(grp_2);
	H5Gclose(grp_move);
        H5Gclose(moved_grp);
	H5Fclose(file_a);
	H5Fclose(file_b);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:    check_new_move
 *
 * Purpose:     Checks result of H5Lmove() with different locations
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 * Programmer:  Raymond Lu
 *              Thursday, April 25, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
check_new_move(hid_t fapl)
{
    hid_t 	file;
    H5O_info_t	oi_hard1, oi_hard2;
    char 	filename[1024];
    char 	linkval[1024];

    TESTING("check new move function");

    /* Open file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get hard link info */
    if(H5Oget_info_by_name(file, "/group2/group_new_name", &oi_hard1, H5P_DEFAULT) < 0)
        FAIL_STACK_ERROR
    if(H5Oget_info_by_name(file, "/group1/hard", &oi_hard2, H5P_DEFAULT) < 0)
        FAIL_STACK_ERROR

    /* Check hard links */
    if(H5O_TYPE_GROUP != oi_hard1.type || H5O_TYPE_GROUP != oi_hard2.type)
        FAIL_PUTS_ERROR("    Unexpected object type, should have been a group")
    if(H5F_addr_ne(oi_hard1.addr, oi_hard2.addr))
        FAIL_PUTS_ERROR("    Hard link test failed.  Link seems not to point to the expected file location.")

    /* Check soft links */
    if(H5Lget_val(file, "group2/soft", linkval, sizeof linkval, H5P_DEFAULT) < 0)
        FAIL_STACK_ERROR
    if(HDstrcmp(linkval, "/group1/group_move"))
        FAIL_PUTS_ERROR("    Soft link test failed. Wrong link value")

    /* Cleanup */
    if(H5Fclose(file) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    return 1;
} /* end check_new_move() */


/*-------------------------------------------------------------------------
 * Function:    test_filespace
 *
 * Purpose:     Test proper reuse of space in the file when objects are unlinked
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Saturday, March 22, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_filespace(hid_t fapl)
{
    hid_t 	fapl_nocache;   /* File access property list with raw data cache turned off */
    hid_t 	contig_dcpl;    /* Dataset creation property list for contiguous dataset */
    hid_t 	early_chunk_dcpl; /* Dataset creation property list for chunked dataset & early allocation */
    hid_t 	late_chunk_dcpl; /* Dataset creation property list for chunked dataset & late allocation */
    hid_t 	comp_dcpl;      /* Dataset creation property list for compressed, chunked dataset */
    hid_t 	compact_dcpl;   /* Dataset creation property list for compact dataset */
    hid_t 	file;           /* File ID */
    hid_t 	group, group2;  /* Group IDs */
    hid_t 	dataset, dataset2;      /* Dataset IDs */
    hid_t 	space;          /* Dataspace ID */
    hid_t 	type;           /* Datatype ID */
    hid_t 	attr_space;     /* Dataspace ID for attributes */
    hid_t 	attr;           /* Attribute ID */
    char 	filename[1024]; /* Name of file to create */
    char 	objname[128];   /* Name of object to create */
    hsize_t     dims[FILESPACE_NDIMS]= {FILESPACE_DIM0, FILESPACE_DIM1, FILESPACE_DIM2};        /* Dataset dimensions */
    hsize_t     chunk_dims[FILESPACE_NDIMS]= {FILESPACE_CHUNK0, FILESPACE_CHUNK1, FILESPACE_CHUNK2};        /* Chunk dimensions */
    hsize_t     attr_dims[FILESPACE_ATTR_NDIMS]= {FILESPACE_ATTR_DIM0, FILESPACE_ATTR_DIM1};        /* Attribute dimensions */
    int        *data = NULL;    /* Pointer to dataset buffer */
    int        *tmp_data;       /* Temporary pointer to dataset buffer */
    h5_stat_size_t       empty_size;     /* Size of an empty file */
    h5_stat_size_t       file_size;      /* Size of each file created */
    herr_t	status;         /* Function status return value */
    unsigned u,v,w;             /* Local index variables */

    /* Metadata cache parameters */
    int mdc_nelmts;
    size_t rdcc_nelmts;
    size_t rdcc_nbytes;
    double rdcc_w0;

    puts("Testing file space gets reused:");

    /* Open file */
    h5_fixname(FILENAME[4], fapl, filename, sizeof filename);

/* Create FAPL with raw data cache disabled */
    /* Create file access property list with raw data cache disabled */
    if ((fapl_nocache=H5Pcopy(fapl)) < 0) TEST_ERROR

    /* Get the cache settings */
    if(H5Pget_cache(fapl_nocache,&mdc_nelmts,&rdcc_nelmts,&rdcc_nbytes,&rdcc_w0) < 0) TEST_ERROR

    /* Disable the raw data cache */
    rdcc_nelmts=0;
    rdcc_nbytes=0;
    if(H5Pset_cache(fapl_nocache,mdc_nelmts,rdcc_nelmts,rdcc_nbytes,rdcc_w0) < 0) TEST_ERROR

/* Create empty file for size comparisons later */

    /* Create file */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(file) < 0) TEST_ERROR

    /* Get the size of an empty file */
    if((empty_size = h5_get_file_size(filename, fapl)) < 0) TEST_ERROR

/* Create common objects for datasets */

    /* Create dataset creation property list for contigous storage */
    if ((contig_dcpl=H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR

    /* Make certain that space is allocated early */
    if(H5Pset_alloc_time(contig_dcpl, H5D_ALLOC_TIME_EARLY) < 0) TEST_ERROR

    /* Create dataset creation property list for chunked storage & early allocation */
    if ((early_chunk_dcpl=H5Pcopy(contig_dcpl)) < 0) TEST_ERROR

    /* Set chunk dimensions */
    if(H5Pset_chunk(early_chunk_dcpl, FILESPACE_NDIMS, chunk_dims) < 0) TEST_ERROR

    /* Create dataset creation property list for chunked storage & late allocation */
    if ((late_chunk_dcpl=H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR

    /* Set chunk dimensions */
    if(H5Pset_chunk(late_chunk_dcpl, FILESPACE_NDIMS, chunk_dims) < 0) TEST_ERROR

    /* Create dataset creation property list for compressed, chunked storage & early allocation */
    if ((comp_dcpl=H5Pcopy(early_chunk_dcpl)) < 0) TEST_ERROR

    /* Enable compression & set level */
    if(H5Pset_deflate(comp_dcpl, FILESPACE_DEFLATE_LEVEL) < 0) TEST_ERROR

    /* Create dataset creation property list for compact storage */
    if ((compact_dcpl=H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR

    /* Set to compact storage */
    if(H5Pset_layout(compact_dcpl, H5D_COMPACT) < 0) TEST_ERROR

    /* Create dataspace for datasets */
    if((space = H5Screate_simple(FILESPACE_NDIMS, dims, NULL)) < 0) TEST_ERROR

    /* Create buffer for writing dataset */
    if(NULL == (data = (int *)HDmalloc(sizeof(int) * FILESPACE_DIM0 * FILESPACE_DIM1 * FILESPACE_DIM2))) TEST_ERROR


/* Create single dataset (with contiguous storage & late allocation), remove it & verify file size */
    TESTING("    contiguous dataset with late allocation");

    /* Create file */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create a single dataset to remove */
    if((dataset = H5Dcreate2(file, DATASETNAME, H5T_NATIVE_INT, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Dclose(dataset) < 0) FAIL_STACK_ERROR

    /* Remove the dataset */
    if(H5Ldelete(file, DATASETNAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0) FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0) TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != empty_size) TEST_ERROR

    PASSED();


/* Create single dataset (with contiguous storage & early allocation), remove it & verify file size */
    TESTING("    contiguous dataset with early allocation");

    /* Create file */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create a single dataset to remove */
    if((dataset = H5Dcreate2(file, DATASETNAME, H5T_NATIVE_INT, space, H5P_DEFAULT, contig_dcpl, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Dclose(dataset) < 0) FAIL_STACK_ERROR

    /* Remove the dataset */
    if(H5Ldelete(file, DATASETNAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0) FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0) TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != empty_size) TEST_ERROR

    PASSED();


/* Create single dataset (with chunked storage & late allocation), remove it & verify file size */
    TESTING("    chunked dataset with late allocation");

    /* Create file */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create a single dataset to remove */
    if((dataset = H5Dcreate2(file, DATASETNAME, H5T_NATIVE_INT, space, H5P_DEFAULT, late_chunk_dcpl, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Dclose(dataset) < 0) FAIL_STACK_ERROR

    /* Remove the dataset */
    if(H5Ldelete(file, DATASETNAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0) FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0) TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != empty_size) TEST_ERROR

    PASSED();


/* Create single dataset (with chunked storage & early allocation), remove it & verify file size */
    TESTING("    chunked dataset with early allocation");

    /* Create file */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create a single dataset to remove */
    if((dataset = H5Dcreate2(file, DATASETNAME, H5T_NATIVE_INT, space, H5P_DEFAULT, early_chunk_dcpl, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Dclose(dataset) < 0) FAIL_STACK_ERROR

    /* Remove the dataset */
    if(H5Ldelete(file, DATASETNAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0) FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0) TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != empty_size) TEST_ERROR

    PASSED();


/* Create single dataset (with compressed storage & early allocation), remove it & verify file size */
    TESTING("    compressed, chunked dataset");

    /* Create file */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create a single dataset to remove */
    if((dataset = H5Dcreate2(file, DATASETNAME, H5T_NATIVE_INT, space, H5P_DEFAULT, comp_dcpl, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Dclose(dataset) < 0) FAIL_STACK_ERROR

    /* Remove the dataset */
    if(H5Ldelete(file, DATASETNAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0) FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0) TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != empty_size) TEST_ERROR

    PASSED();


/* Create single dataset (with compressed storage & early allocation), re-write it a bunch of
 * times (which should re-allocate blocks many times) and remove it & verify
 * file size.
 */
    TESTING("    re-writing compressed, chunked dataset");

    /* Create file (using FAPL with disabled raw data cache) */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_nocache)) < 0) FAIL_STACK_ERROR

    /* Create a single dataset to remove */
    if((dataset = H5Dcreate2(file, DATASETNAME, H5T_NATIVE_INT, space, H5P_DEFAULT, comp_dcpl, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Alternate re-writing dataset with compressible & random data */
    for(u = 0; u < FILESPACE_REWRITE; u++) {
        /* Set buffer to some compressible values */
        for(v = 0, tmp_data = data; v < (FILESPACE_DIM0 * FILESPACE_DIM1 * FILESPACE_DIM2); v++)
            *tmp_data++ = (int)(v * u);

        /* Write the buffer to the dataset */
        if(H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0) FAIL_STACK_ERROR

        /* Set buffer to different random numbers each time */
        for(v = 0, tmp_data = data; v < (FILESPACE_DIM0 * FILESPACE_DIM1 * FILESPACE_DIM2); v++)
            *tmp_data++ = (int)HDrandom();

        /* Write the buffer to the dataset */
        if(H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0) FAIL_STACK_ERROR
    } /* end for */

    /* Close dataset */
    if(H5Dclose(dataset) < 0) FAIL_STACK_ERROR

    /* Remove the dataset */
    if(H5Ldelete(file, DATASETNAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0) FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl_nocache)) < 0) TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != empty_size) TEST_ERROR

    PASSED();


/* Create single dataset (with compact storage), remove it & verify file size */
    TESTING("    compact dataset");

    /* Create file */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create a single dataset to remove */
    if((dataset = H5Dcreate2(file, DATASETNAME, H5T_NATIVE_INT, space, H5P_DEFAULT, compact_dcpl, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Dclose(dataset) < 0) FAIL_STACK_ERROR

    /* Remove the dataset */
    if(H5Ldelete(file, DATASETNAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0) FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0) TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != empty_size) TEST_ERROR

    PASSED();


/* Create two datasets (with contiguous storage), alternate adding attributes
 * to each one (which creates many object header continuations),
 * remove both & verify file size.
 */
    TESTING("    object header continuations");

    /* Create file */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create datasets to remove */
    if((dataset = H5Dcreate2(file, DATASETNAME, H5T_NATIVE_INT, space, H5P_DEFAULT, contig_dcpl, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((dataset2 = H5Dcreate2(file, DATASET2NAME, H5T_NATIVE_INT, space, H5P_DEFAULT, contig_dcpl, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create a dataspace for the attributes */
    if((attr_space = H5Screate_simple(FILESPACE_ATTR_NDIMS, attr_dims, NULL)) < 0) FAIL_STACK_ERROR

    /* Alternate adding attributes to each one */
    for(u = 0; u < FILESPACE_NATTR; u++) {
        /* Set the name of the attribute to create */
        sprintf(objname,"%s %u",ATTRNAME,u);

        /* Create an attribute on the first dataset */
        if((attr = H5Acreate2(dataset, objname, H5T_NATIVE_INT, attr_space, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

        /* Don't worry about writing the attribute - it will have a fill value */

        /* Close the attribute on the first dataset */
        if(H5Aclose(attr) < 0) FAIL_STACK_ERROR

        /* Create an attribute on the second dataset */
        if((attr = H5Acreate2(dataset2, objname, H5T_NATIVE_INT, attr_space, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

        /* Don't worry about writing the attribute - it will have a fill value */

        /* Close the attribute on the second dataset */
        if(H5Aclose(attr) < 0) FAIL_STACK_ERROR

        /* Flush the file (to fix the sizes of object header buffers, etc) */
        if(H5Fflush(file,H5F_SCOPE_GLOBAL) < 0) FAIL_STACK_ERROR
    } /* end for */

    /* Close the dataspace for the attributes */
    if(H5Sclose(attr_space) < 0) FAIL_STACK_ERROR

    /* Close datasets */
    if(H5Dclose(dataset) < 0) FAIL_STACK_ERROR
    if(H5Dclose(dataset2) < 0) FAIL_STACK_ERROR

    /* Remove the datasets */
    if(H5Ldelete(file, DATASETNAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(file, DATASET2NAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0) FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0) TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != empty_size) TEST_ERROR

    PASSED();


/* Create single named datatype, remove it & verify file size */
    TESTING("    named datatype");

    /* Create file */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create datatype to commit */
    if((type = H5Tcopy(H5T_NATIVE_INT)) < 0) FAIL_STACK_ERROR

    /* Create a single named datatype to remove */
    if(H5Tcommit2(file, TYPENAME, type, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Tclose(type) < 0) FAIL_STACK_ERROR

    /* Remove the named datatype */
    if(H5Ldelete(file, TYPENAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0) FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0) TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != empty_size) TEST_ERROR

    PASSED();


/* Create single group, remove it & verify file size */
    TESTING("    single group");

    /* Create file */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create a single group to remove */
    if((group = H5Gcreate2(file, GROUPNAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group) < 0) FAIL_STACK_ERROR

    /* Remove the group */
    if(H5Ldelete(file, GROUPNAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0) FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0) TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != empty_size) TEST_ERROR

    PASSED();


/* Create many groups, remove them & verify file size */
    TESTING("    multiple groups");

    /* Create file */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create a many groups to remove */
    for(u = 0; u < UNLINK_NGROUPS; u++) {
        sprintf(objname, "%s %u", GROUPNAME, u);
        if((group = H5Gcreate2(file, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
        if(H5Gclose(group) < 0) FAIL_STACK_ERROR
    } /* end for */

    /* Remove the all the groups */
    /* (Remove them in reverse order just to make file size calculation easier -QAK) */
    for(u = UNLINK_NGROUPS; u > 0; u--) {
        sprintf(objname, "%s %u", GROUPNAME, (u - 1));
        if(H5Ldelete(file, objname, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    } /* end for */

    /* Close file */
    if(H5Fclose(file) < 0) FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0) TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != empty_size) TEST_ERROR

    PASSED();


/* Create simple group hiearchy, remove it & verify file size */
    TESTING("    simple group hierarchy");

    /* Create file */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create a small group hierarchy to remove */
    if((group = H5Gcreate2(file, GROUPNAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2 = H5Gcreate2(group, GROUP2NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group) < 0) FAIL_STACK_ERROR

    /* Remove the second group */
    if(H5Ldelete(file, GROUPNAME "/" GROUP2NAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Remove the first group */
    if(H5Ldelete(file, GROUPNAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0) FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0) TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != empty_size) TEST_ERROR

    PASSED();


/* Create complex group hiearchy, remove it & verify file size */
    TESTING("    complex group hierarchy");

    /* Create file */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create a complex group hierarchy to remove */
    for(u = 0; u < FILESPACE_TOP_GROUPS; u++) {
        /* Create group */
        sprintf(objname,"%s %u",GROUPNAME,u);
        if((group = H5Gcreate2(file, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

        /* Create nested groups inside top groups */
        for(v = 0; v < FILESPACE_NESTED_GROUPS; v++) {
            /* Create group */
            sprintf(objname, "%s %u", GROUP2NAME, v);
            if((group2 = H5Gcreate2(group, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

            /* Create datasets inside nested groups */
            for(w = 0; w < FILESPACE_NDATASETS; w++) {
                /* Create & close a dataset */
                sprintf(objname, "%s %u", DATASETNAME, w);
                if((dataset = H5Dcreate2(group2, objname, H5T_NATIVE_INT, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
                if(H5Dclose(dataset) < 0) FAIL_STACK_ERROR
            } /* end for */

            /* Close nested group */
            if(H5Gclose(group2) < 0) FAIL_STACK_ERROR
        } /* end for */

        /* Close top group */
        if(H5Gclose(group) < 0) FAIL_STACK_ERROR
    } /* end for */

    /* Remove complex group hierarchy */
    /* (Remove them in reverse order just to make file size calculation easier -QAK) */
    for(u = FILESPACE_TOP_GROUPS; u > 0; u--) {
        /* Open group */
        sprintf(objname, "%s %u", GROUPNAME, (u - 1));
        if((group = H5Gopen2(file, objname, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

        /* Open nested groups inside top groups */
        for(v = 0; v < FILESPACE_NESTED_GROUPS; v++) {
            /* Create group */
            sprintf(objname, "%s %u", GROUP2NAME, v);
            if((group2 = H5Gopen2(group, objname, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

            /* Remove datasets inside nested groups */
            for(w = 0; w < FILESPACE_NDATASETS; w++) {
                /* Remove dataset */
                sprintf(objname, "%s %u", DATASETNAME, w);
                if(H5Ldelete(group2, objname, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
            } /* end for */

            /* Close nested group */
            if(H5Gclose(group2) < 0) FAIL_STACK_ERROR

            /* Remove nested group */
            sprintf(objname, "%s %u",GROUP2NAME, v);
            if(H5Ldelete(group, objname, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
        } /* end for */

        /* Close top group */
        if(H5Gclose(group) < 0) FAIL_STACK_ERROR

        /* Remove top group */
        sprintf(objname, "%s %u", GROUPNAME, (u - 1));
        if(H5Ldelete(file, objname, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    } /* end for */

    /* Close file */
    if(H5Fclose(file) < 0) FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0) TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != empty_size) TEST_ERROR

    PASSED();


/* Create dataset and duplicate dataset, remove original & verify file size */
    TESTING("    duplicate dataset");

    /* Create file */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create a single dataset to remove */
    if((dataset = H5Dcreate2(file, DATASETNAME, H5T_NATIVE_INT, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Dclose(dataset) < 0) FAIL_STACK_ERROR

    /* Create another dataset with same name */
    H5E_BEGIN_TRY {
        dataset = H5Dcreate2(file, DATASETNAME, H5T_NATIVE_INT, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    } H5E_END_TRY;
    if(dataset >= 0) {
        H5Dclose(dataset);
        TEST_ERROR
    } /* end if */

    /* Remove the dataset */
    if(H5Ldelete(file, DATASETNAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0) FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0) TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != empty_size) TEST_ERROR

    PASSED();


/* Create group and duplicate group, remove original & verify file size */
    TESTING("    duplicate group");

    /* Create file */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create a single group to remove */
    if((group = H5Gcreate2(file, GROUPNAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group) < 0) FAIL_STACK_ERROR

    /* Create another group with same name */
    H5E_BEGIN_TRY {
        group = H5Gcreate2(file, GROUPNAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    } H5E_END_TRY;
    if(group >= 0) {
        H5Gclose(group);
        TEST_ERROR
    } /* end if */

    /* Remove the group */
    if(H5Ldelete(file, GROUPNAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0) FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0) TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != empty_size) TEST_ERROR

    PASSED();


/* Create named datatype and duplicate named datatype, remove original & verify file size */
    TESTING("    duplicate named datatype");

    /* Create file */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create datatype to commit */
    if((type = H5Tcopy(H5T_NATIVE_INT)) < 0) FAIL_STACK_ERROR

    /* Create a single named datatype to remove */
    if(H5Tcommit2(file, TYPENAME, type, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Tclose(type) < 0) FAIL_STACK_ERROR

    /* Create datatype to commit */
    if((type = H5Tcopy(H5T_NATIVE_INT)) < 0) FAIL_STACK_ERROR

    /* Create another named datatype with same name */
    H5E_BEGIN_TRY {
        status = H5Tcommit2(file, TYPENAME, type, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    } H5E_END_TRY;
    if(status >= 0) TEST_ERROR
    if(H5Tclose(type) < 0) FAIL_STACK_ERROR

    /* Remove the named datatype */
    if(H5Ldelete(file, TYPENAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0) FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0) TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != empty_size) TEST_ERROR

    PASSED();


/* Create named datatype and duplicate named datatype, remove original & verify file size */
    TESTING("    duplicate attribute");

    /* Create file */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create datasets to remove */
    if((dataset = H5Dcreate2(file, DATASETNAME, H5T_NATIVE_INT, space, H5P_DEFAULT, contig_dcpl, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create a dataspace for the attributes */
    if((attr_space = H5Screate_simple(FILESPACE_ATTR_NDIMS, attr_dims, NULL)) < 0) FAIL_STACK_ERROR

    /* Create an attribute on the dataset */
    if((attr = H5Acreate2(dataset, ATTRNAME, H5T_NATIVE_INT, attr_space, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Don't worry about writing the attribute - it will have a fill value */

    /* Close the attribute on the dataset */
    if(H5Aclose(attr) < 0) FAIL_STACK_ERROR

    /* Create another attribute with same name */
    H5E_BEGIN_TRY {
        attr = H5Acreate2(dataset, ATTRNAME, H5T_NATIVE_INT, attr_space, H5P_DEFAULT, H5P_DEFAULT);
    } H5E_END_TRY;
    if(attr >= 0) {
        H5Aclose(attr);
        TEST_ERROR
    } /* end if */

    /* Close the dataspace for the attributes */
    if(H5Sclose(attr_space) < 0) FAIL_STACK_ERROR

    /* Close dataset */
    if(H5Dclose(dataset) < 0) FAIL_STACK_ERROR

    /* Remove the dataset */
    if(H5Ldelete(file, DATASETNAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0) FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0) TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != empty_size) TEST_ERROR

    PASSED();


/* Cleanup common objects */

    /* Release dataset buffer */
    HDfree(data);

    /* Close property lists */
    if(H5Pclose(fapl_nocache) < 0) FAIL_STACK_ERROR
    if(H5Pclose(contig_dcpl) < 0) FAIL_STACK_ERROR
    if(H5Pclose(early_chunk_dcpl) < 0) FAIL_STACK_ERROR
    if(H5Pclose(late_chunk_dcpl) < 0) FAIL_STACK_ERROR
    if(H5Pclose(comp_dcpl) < 0) FAIL_STACK_ERROR
    if(H5Pclose(compact_dcpl) < 0) FAIL_STACK_ERROR

    /* Close dataspace */
    if(H5Sclose(space) < 0) FAIL_STACK_ERROR

    /* Indicate success */
    /* Don't print final "PASSED", since we aren't on the correct line anymore */
    return 0;

error:
    /* Release dataset buffer */
    if(data)
        HDfree(data);

    return 1;
} /* end test_filespace() */


/*-------------------------------------------------------------------------
 * Function:    test_create_unlink
 *
 * Purpose:     Creates and then unlinks a large number of objects
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Friday, April 11, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_create_unlink(const char *msg, hid_t fapl)
{
    hid_t 	file, group;
    unsigned u;
    char 	groupname[1024];
    char	filename[1024];

    TESTING(msg);

    /* Create file */
    h5_fixname(FILENAME[3], fapl, filename, sizeof filename);
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_PUTS_ERROR("    Creating file failed")

    /* Create a many groups to remove */
    for(u = 0; u < UNLINK_NGROUPS; u++) {
        sprintf(groupname, "%s %u", GROUPNAME, u);
        if((group = H5Gcreate2(file, groupname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("group %s creation failed\n", groupname);
            goto error;
        } /* end if */
        if(H5Gclose (group) < 0) {
            H5_FAILED();
            printf("closing group %s failed\n", groupname);
            goto error;
        } /* end if */
    } /* end for */

    /* Remove the all the groups */
    for(u = 0; u < UNLINK_NGROUPS; u++) {
        sprintf(groupname, "%s %u", GROUPNAME, u);
        if(H5Ldelete(file, groupname, H5P_DEFAULT) < 0) {
            H5_FAILED();
            printf("Unlinking group %s failed\n", groupname);
            goto error;
        } /* end if */
    } /* end for */

    /* Close file */
    if(H5Fclose(file) < 0)
        FAIL_PUTS_ERROR("Closing file failed")

    PASSED();
    return 0;

error:
    return 1;
} /* end test_create_unlink() */


/*-------------------------------------------------------------------------
 * Function:    test_link_slashes
 *
 * Purpose:     Tests creating links with various multiple & trailing slashes
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Saturday, August 16, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_link_slashes(hid_t fapl)
{
    hid_t fid;          /* File ID */
    hid_t gid, gid2;     /* Group ID */
    char	filename[1024];

    TESTING("creating links with multiple slashes");

    /* Create file */
    h5_fixname(FILENAME[5], fapl, filename, sizeof filename);

    /* Create a file */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create a group in the root group */
    if((gid = H5Gcreate2(fid, SLASHES_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create a nested group in the root group */
    if((gid2 = H5Gcreate2(gid, SLASHES_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close the nested group */
    if(H5Gclose(gid2) < 0) FAIL_STACK_ERROR

    /* Create a hard link to the nested group */
    if(H5Lcreate_hard(gid, SLASHES_GROUP_NAME, H5L_SAME_LOC, SLASHES_HARDLINK_NAME, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Create a soft link with a relative path to the nested group */
    if(H5Lcreate_soft(SLASHES_GROUP_NAME, gid, SLASHES_SOFTLINK_NAME, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Create a soft link with the full path to the nested group */
    if(H5Lcreate_soft("////"SLASHES_GROUP_NAME""SLASHES_GROUP_NAME, gid, SLASHES_SOFTLINK2_NAME, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Create a soft link to the root group */
    if(H5Lcreate_soft("////", gid, SLASHES_ROOTLINK_NAME, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close the group */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Create a hard link to the existing group */
    if(H5Lcreate_hard(fid, SLASHES_GROUP_NAME, H5L_SAME_LOC, SLASHES_HARDLINK_NAME, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close the file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    return 1;
} /* end test_link_slashes() */


/*-------------------------------------------------------------------------
 * Function:    test_unlink_slashes
 *
 * Purpose:     Tests deleting links with various multiple & trailing slashes
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Saturday, August 16, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_unlink_slashes(hid_t fapl)
{
    hid_t fid;          /* File ID */
    hid_t gid;          /* Group ID */
    char	filename[1024];

    TESTING("deleting links with multiple slashes");

    /* Create file */
    h5_fixname(FILENAME[5], fapl, filename, sizeof filename);

    /* Open the file */
    if((fid = H5Fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Open the top level group */
    if((gid = H5Gopen2(fid, SLASHES_GROUP_NAME, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Delete the root link */
    if(H5Ldelete(gid,SLASHES_ROOTLINK_NAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Delete the soft link with the full path */
    if(H5Ldelete(gid,SLASHES_SOFTLINK2_NAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Delete the soft link with the relative path */
    if(H5Ldelete(gid,SLASHES_SOFTLINK_NAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Delete the hard link */
    if(H5Ldelete(gid,SLASHES_HARDLINK_NAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Delete the group itself */
    if(H5Ldelete(gid,SLASHES_GROUP_NAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close the group */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Delete the hard link */
    if(H5Ldelete(fid,SLASHES_HARDLINK_NAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Delete the group itself */
    if(H5Ldelete(fid,SLASHES_GROUP_NAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close the file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    return 1;
} /* end test_unlink_slashes() */

/*
 * Helper routine for test_unlink_rightleaf()
 */
static int
delete_node(hid_t pid, hid_t id)
{
    char name[256];     /* Name of object to close */

    /* Get the name of the object to delete */
    if(H5Iget_name(id, name, sizeof(name)) < 0) return (-1);

    /* Close the object */
    if(H5Gclose(id) < 0) return(-1);

    /* Unlink the object */
    if(H5Ldelete(pid, name, H5P_DEFAULT) < 0) return(-1);

    /* If this object is the right-most child, try opening the previous object */
    if(HDstrcmp(name,"/Zone81") == 0) {
        hid_t gid;

        if((gid = H5Gopen2(pid, "/Zone80", H5P_DEFAULT)) < 0) return(-1);
        if(H5Gclose(gid) < 0) return(-1);
    } /* end if */

    /* Indicate success */
    return(0);
}


/*-------------------------------------------------------------------------
 * Function:    test_unlink_rightleaf
 *
 * Purpose:     Tests deleting objects in a way that triggers deletion of the
 *              right child in the leaf of a non-leaf B-tree node
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Monday, January 19, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_unlink_rightleaf(hid_t fid)
{
    hid_t rootid = -1,  /* Group ID for root group */
        *gids = NULL;   /* Array of IDs for groups created */
    int n,              /* Local index variable */
        ngroups = 150;  /* Number of groups to create */
    char name[256];     /* Name of object to create */

    TESTING("deleting right-most child in non-leaf B-tree node");

    /* Allocate space for the group IDs */
    if(NULL == (gids = (hid_t *)HDcalloc((size_t)ngroups, sizeof(hid_t)))) TEST_ERROR

    if((rootid = H5Gopen2(fid, "/", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create all the groups */
    for (n = 0; n < ngroups; n++) {
        sprintf(name, "Zone%d", n + 1);
        if((gids[n] = H5Gcreate2(rootid, name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    } /* end for */

    /* Unlink & re-create each group */
    for (n = 0; n < ngroups; n++) {
        if(delete_node (rootid, gids[n]) < 0) TEST_ERROR
        sprintf(name, "Zone%d", n + 1);
        if((gids[n] = H5Gcreate2(rootid, name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    } /* end for */

    /* Close all the groups */
    for (n = 0; n < ngroups; n++) {
        if(H5Gclose(gids[n]) < 0) TEST_ERROR
        gids[n] = 0;
    } /* end for */

    /* Close root group ID */
    if(H5Gclose(rootid) < 0) TEST_ERROR

    /* Free memory */
    HDfree(gids);

    PASSED();
    return 0;

error:
    if(gids) {
        /* Close any open groups */
        for (n = 0; n < ngroups; n++)
            if(gids[n]) {
                H5E_BEGIN_TRY {
                    H5Gclose(gids[n]);
                } H5E_END_TRY;
            } /* end if */
        HDfree(gids);
    } /* end if */
    H5E_BEGIN_TRY {
        H5Gclose(rootid);
    } H5E_END_TRY;

    return 1;
} /* end test_unlink_rightleaf() */


/*-------------------------------------------------------------------------
 * Function:    test_unlink_rightnode
 *
 * Purpose:     Tests deleting objects in a way that triggers deletion of the
 *              entire right child leaf of a non-leaf B-tree node
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Monday, January 19, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_unlink_rightnode(hid_t fid)
{
    hid_t rootid = -1,       /* Group ID for root group */
        *gids = NULL;   /* Array of IDs for groups created */
    int n,              /* Local index variable */
        ngroups = 150;  /* Number of groups to create */
    char name[256];     /* Name of object to create */

    TESTING("deleting right-most child in non-leaf B-tree node");

    /* Allocate space for the group IDs */
    if(NULL == (gids = (hid_t *)HDcalloc((size_t)ngroups, sizeof(hid_t)))) TEST_ERROR

    if((rootid = H5Gopen2(fid, "/", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create all the groups */
    for (n = 0; n < ngroups; n++) {
        sprintf(name, "ZoneB%d", n + 1);
        if((gids[n] = H5Gcreate2(rootid, name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    } /* end for */

    /* Close all the groups */
    for (n = 0; n < ngroups; n++) {
        if(H5Gclose(gids[n]) < 0) FAIL_STACK_ERROR
        gids[n] = 0;
    } /* end for */

    /* Unlink specific objects to trigger deletion of right leaf in non-leaf node */
    if(H5Ldelete(fid, "/ZoneB77", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneB78", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneB79", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneB8", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneB80", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close root group ID */
    if(H5Gclose(rootid) < 0) FAIL_STACK_ERROR

    /* Free memory */
    HDfree(gids);

    PASSED();
    return 0;

error:
    if(gids) {
        /* Close any open groups */
        for (n = 0; n < ngroups; n++)
            if(gids[n]) {
                H5E_BEGIN_TRY {
                    H5Gclose(gids[n]);
                } H5E_END_TRY;
            } /* end if */
        HDfree(gids);
    } /* end if */
    H5E_BEGIN_TRY {
        H5Gclose(rootid);
    } H5E_END_TRY;

    return 1;
} /* end test_unlink_rightnode() */


/*-------------------------------------------------------------------------
 * Function:    test_unlink_middlenode
 *
 * Purpose:     Tests deleting objects in a way that triggers deletion of all
 *              the leafs of a "middle" non-leaf B-tree node
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Monday, January 19, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_unlink_middlenode(hid_t fid)
{
    hid_t rootid = -1,  /* Group ID for root group */
        *gids = NULL;   /* Array of IDs for groups created */
    int n,              /* Local index variable */
        ngroups = 250;  /* Number of groups to create */
    char name[256];     /* Name of object to create */

    TESTING("deleting right-most child in non-leaf B-tree node");

    /* Allocate space for the group IDs */
    if(NULL == (gids = (hid_t *)HDcalloc((size_t)ngroups, sizeof(hid_t)))) TEST_ERROR

    if((rootid = H5Gopen2(fid, "/", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create all the groups */
    for (n = 0; n < ngroups; n++) {
        sprintf(name, "ZoneC%d", n + 1);
        if((gids[n] = H5Gcreate2(rootid, name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    } /* end for */

    /* Close all the groups */
    for (n = 0; n < ngroups; n++) {
        if(H5Gclose(gids[n]) < 0) FAIL_STACK_ERROR
        gids[n] = 0;
    } /* end for */

    /* Unlink specific objects to trigger deletion of all leafs in "interior" non-leaf node */
    if(H5Ldelete(fid, "/ZoneC11", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC110", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC111", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC112", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC113", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC114", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC115", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC116", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC117", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC118", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC119", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC12", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC120", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC121", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC122", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC123", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC124", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC125", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC126", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC127", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC128", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC129", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC13", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC130", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC131", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC132", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC133", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC134", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC135", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC136", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC137", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC138", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC139", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC14", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC140", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC141", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC142", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC143", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC144", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC145", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC146", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC147", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC148", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC149", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC15", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC150", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC151", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC152", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC153", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC154", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC155", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC156", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC157", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC158", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC159", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC16", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC160", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC161", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC162", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC163", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC164", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC165", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC166", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC167", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC168", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC169", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC17", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC170", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC171", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC172", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC173", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC174", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC175", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC176", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC177", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC178", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC179", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC18", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC180", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC19", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC2", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC20", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC21", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC22", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC23", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC24", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC25", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC26", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC27", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC28", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC29", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC3", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC30", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC31", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC32", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC33", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC34", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC35", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC36", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC37", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC38", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC39", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC4", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC40", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC41", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC42", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC43", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC44", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC45", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC46", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC47", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC48", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC49", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC5", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC50", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC51", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC52", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC53", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC54", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC55", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC56", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC57", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC58", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC59", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC6", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC60", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC61", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC62", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC63", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC64", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC65", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC66", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC67", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC68", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC69", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC7", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC70", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC71", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC72", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC73", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC74", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC75", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC76", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC77", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC78", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC79", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC8", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, "/ZoneC80", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close root group ID */
    if(H5Gclose(rootid) < 0) FAIL_STACK_ERROR

    /* Free memory */
    HDfree(gids);

    PASSED();
    return 0;

error:
    if(gids) {
        /* Close any open groups */
        for (n = 0; n < ngroups; n++)
            if(gids[n]) {
                H5E_BEGIN_TRY {
                    H5Gclose(gids[n]);
                } H5E_END_TRY;
            } /* end if */
        HDfree(gids);
    } /* end if */
    H5E_BEGIN_TRY {
        H5Gclose(rootid);
    } H5E_END_TRY;

    return 1;
} /* end test_unlink_middlenode() */


/*-------------------------------------------------------------------------
 * Function:    test_resurrect_dataset
 *
 * Purpose:     Tests deleting a dataset while its still open and then
 *              "resurrecting" it by creating a link to it again.
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, July 14, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_resurrect_dataset(hid_t fapl)
{
    hid_t       f = -1, s = -1, d = -1;
    char	filename[1024];

    TESTING("resurrecting dataset after deletion");

    /* Create file */
    h5_fixname(FILENAME[6], fapl, filename, sizeof filename);

    /* Create the file */
    if((f = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create a dataset in the file */
    if((s = H5Screate(H5S_SCALAR)) < 0) FAIL_STACK_ERROR
    if((d = H5Dcreate2(f, DATASETNAME, H5T_NATIVE_INT, s, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Sclose(s) < 0) FAIL_STACK_ERROR

    /* Unlink the dataset while it's open (will mark it for deletion when closed) */
    if(H5Ldelete(f, DATASETNAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Check that dataset name is NULL */
    if(H5Iget_name(d, NULL, (size_t)0) != 0) FAIL_STACK_ERROR

    /* Re-link the dataset to the group hierarchy (shouldn't get deleted now) */
    if(H5Lcreate_hard(d, ".", f, DATASET2NAME, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close things */
    if(H5Dclose(d) < 0) FAIL_STACK_ERROR
    if(H5Fclose(f) < 0) FAIL_STACK_ERROR

    /* Re-open the file */
    if((f = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0) FAIL_STACK_ERROR

    /* Attempt to open the dataset under the new name */
    if((d = H5Dopen2(f, DATASET2NAME, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close things */
    if(H5Dclose(d) < 0) FAIL_STACK_ERROR
    if(H5Fclose(f) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Sclose(s);
	H5Dclose(d);
	H5Fclose(f);
    } H5E_END_TRY;
    return 1;
} /* end test_resurrect_dataset() */


/*-------------------------------------------------------------------------
 * Function:    test_resurrect_datatype
 *
 * Purpose:     Tests deleting a datatype while it's still open and then
 *              "resurrecting" it by creating a link to it again.
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  James Laird
 *              Wednesday, July 28, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_resurrect_datatype(hid_t fapl)
{
    hid_t       file = -1, type = -1;
    char        filename[1024];

    TESTING("resurrecting datatype after deletion");

    /* Create file */
    h5_fixname(FILENAME[7], fapl, filename, sizeof filename);

    /* Create the file */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create a named datatype in the file */
    if((type = H5Tcopy (H5T_NATIVE_INT)) < 0) FAIL_STACK_ERROR
    if(H5Tcommit2(file, TYPENAME, type, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Unlink the datatype while it's open (will mark it for deletion when closed) */
    if(H5Ldelete(file, TYPENAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Check that datatype name is NULL */
    if(H5Iget_name(type, NULL, (size_t)0) != 0) FAIL_STACK_ERROR

    /* Re-link the datatype to the group hierarchy (shouldn't get deleted now) */
    if(H5Lcreate_hard(type, ".", file, TYPE2NAME, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close things */
    if(H5Tclose(type) < 0) FAIL_STACK_ERROR
    if(H5Fclose(file) < 0) FAIL_STACK_ERROR

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0) FAIL_STACK_ERROR

    /* Attempt to open the datatype under the new name */
    if((type = H5Topen2(file,TYPE2NAME, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close things */
    if(H5Tclose(type) < 0) FAIL_STACK_ERROR
    if(H5Fclose(file) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Tclose(type);
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* end test_resurrect_datatype() */


/*-------------------------------------------------------------------------
 * Function:    test_resurrect_group
 *
 * Purpose:     Tests deleting a group while it's still open and then
 *              "resurrecting" it by creating a link to it again.
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  James Laird
 *              Wednesday, July 28, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_resurrect_group(hid_t fapl)
{
    hid_t       file = -1, group = -1;
    char        filename[1024];

    TESTING("resurrecting group after deletion");

    /* Create file */
    h5_fixname(FILENAME[8], fapl, filename, sizeof filename);

    /* Create the file */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create a group in the file */
    if((group = H5Gcreate2(file, GROUPNAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Unlink the group while it's open (will mark it for deletion when closed) */
    if(H5Ldelete(file, GROUPNAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Check that group's name is NULL */
    if(H5Iget_name(group, NULL, (size_t)0) != 0) FAIL_STACK_ERROR

    /* Re-link the group into the group hierarchy (shouldn't get deleted now) */
    if(H5Lcreate_hard(group, ".", file, GROUP2NAME, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close things */
    if(H5Gclose(group) < 0) FAIL_STACK_ERROR
    if(H5Fclose(file) < 0) FAIL_STACK_ERROR

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0) FAIL_STACK_ERROR

    /* Attempt to open the datatype under the new name */
    if((group = H5Gopen2(file, GROUP2NAME, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close things */
    if(H5Gclose(group) < 0) FAIL_STACK_ERROR
    if(H5Fclose(file) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Gclose(group);
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* end test_resurrect_group() */


/*-------------------------------------------------------------------------
 * Function:    test_unlink_chunked_dataset
 *
 * Purpose:     Tests deleting a chunked dataset
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Monday, September 27, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_unlink_chunked_dataset(hid_t fapl)
{
    hid_t file_id = -1;
    hid_t dset_id = -1;
    hid_t space_id = -1;
    hid_t dcpl_id = -1;
    hsize_t dims[FILESPACE_NDIMS] = {FILESPACE_DIM0, FILESPACE_DIM1, FILESPACE_DIM2};
    hsize_t max_dims[FILESPACE_NDIMS] = {H5S_UNLIMITED, H5S_UNLIMITED, H5S_UNLIMITED};
    hsize_t chunk_dims[FILESPACE_NDIMS] = {FILESPACE_CHUNK0, FILESPACE_CHUNK1, FILESPACE_CHUNK2};
    char filename[1024];

    TESTING("unlinking chunked dataset");

    /* Create file */
    h5_fixname(FILENAME[9], fapl, filename, sizeof filename);

    /* Create the file */
    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create the dataspace */
    if((space_id = H5Screate_simple(FILESPACE_NDIMS, dims, max_dims)) < 0) FAIL_STACK_ERROR

    /* Create the dataset creation filter */
    if((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0) FAIL_STACK_ERROR

    /* Set to chunked storage */
    if(H5Pset_chunk(dcpl_id, FILESPACE_NDIMS, chunk_dims) < 0) FAIL_STACK_ERROR

    /* Set to early space allocation */
    if(H5Pset_alloc_time(dcpl_id, H5D_ALLOC_TIME_EARLY) < 0) FAIL_STACK_ERROR

    /* Create the dataset */
    if((dset_id = H5Dcreate2(file_id, DATASETNAME, H5T_NATIVE_INT, space_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close the dataspace */
    if(H5Sclose(space_id) < 0) FAIL_STACK_ERROR

    /* Close the dataset creation property list */
    if(H5Pclose(dcpl_id) < 0) FAIL_STACK_ERROR

    /* Close the dataset */
    if(H5Dclose(dset_id) < 0) FAIL_STACK_ERROR

    /* Close the file */
    if(H5Fclose(file_id) < 0) FAIL_STACK_ERROR

    /* Re-open the file */
    if((file_id = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) FAIL_STACK_ERROR

    /* Delete the dataset */
    if(H5Ldelete(file_id, DATASETNAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close the file */
    if(H5Fclose(file_id) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Pclose(dcpl_id);
	H5Sclose(space_id);
	H5Dclose(dset_id);
	H5Fclose(file_id);
    } H5E_END_TRY;
    return 1;
} /* end test_unlink_chunked_dataset() */


/*-------------------------------------------------------------------------
 * Function:    test_full_group_compact
 *
 * Purpose:     Test deleting a compact group which still has valid objects in it
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, January 18, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_full_group_compact(hid_t fapl)
{
    hid_t file_id = -1;
    hid_t gid = -1, gid2 = -1;  /* Group IDs */
    H5O_info_t	oi;             /* Stat buffer for object */
    char objname[128];          /* Buffer for name of objects to create */
    char objname2[128];         /* Buffer for name of objects to create */
    char filename[1024];        /* Buffer for filename */
    h5_stat_size_t       keep_size;      /* Size of the file with objects to keep */
    h5_stat_size_t       file_size;      /* Size of each file created */
    unsigned u;                 /* Local index variable */

    TESTING("unlinking non-empty compact group");

    /* Create filename */
    h5_fixname(FILENAME[10], fapl, filename, sizeof filename);

    /* Create the file */
    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create group to link objects to */
    if((gid = H5Gcreate2(file_id, "/keep", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create several objects to link to */
    for(u = 0; u < FULL_GROUP_NUM_KEEP; u++) {
        sprintf(objname, "keep %u\n", u);
        if((gid2 = H5Gcreate2(gid, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
        if(H5Gclose(gid2) < 0) FAIL_STACK_ERROR
    } /* end for */

    /* Close group with objects to keep */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Close the file */
    if(H5Fclose(file_id) < 0) FAIL_STACK_ERROR

    /* Get the size of the file with only the objects to keep */
    if((keep_size = h5_get_file_size(filename, fapl)) < 0) TEST_ERROR

    /* Re-open the file */
    if((file_id = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) FAIL_STACK_ERROR

    /* Create group to delete */
    if((gid = H5Gcreate2(file_id, "/delete", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create external link (doesn't matter if it dangles) */
    if(H5Lcreate_external("foo.h5", "/dst", gid, "external", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Create soft link (doesn't matter if it dangles) */
    if(H5Lcreate_soft("/foo", gid, "soft", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Create hard links to objects in group to keep */
    for(u = 0; u < FULL_GROUP_NUM_KEEP; u++) {
        sprintf(objname, "/keep/keep %u\n", u);
        sprintf(objname2, "keep %u\n", u);
        if(H5Lcreate_hard(file_id, objname, gid, objname2, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    } /* end for */

    /* Create several objects to delete */
    for(u = 0; u < FULL_GROUP_NUM_DELETE_COMPACT; u++) {
        sprintf(objname, "delete %u\n", u);
        if((gid2 = H5Gcreate2(gid, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
        if(H5Gclose(gid2) < 0) FAIL_STACK_ERROR
    } /* end for */

    /* Check on group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR
    if(H5G_has_links_test(gid, NULL) != TRUE) TEST_ERROR
    if(H5G_has_stab_test(gid) == TRUE) TEST_ERROR

    /* Close group with objects to delete */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Check reference count on objects to keep */
    for(u = 0; u < FULL_GROUP_NUM_KEEP; u++) {
        sprintf(objname, "/keep/keep %u\n", u);
        if(H5Oget_info_by_name(file_id, objname, &oi, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
        if(oi.rc != 2) TEST_ERROR
    } /* end for */

    /* Close the file */
    if(H5Fclose(file_id) < 0) FAIL_STACK_ERROR


    /* Re-open the file */
    if((file_id = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) FAIL_STACK_ERROR

    /* Delete the full group */
    if(H5Ldelete(file_id, "/delete", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Check reference count on objects to keep */
    for(u = 0; u < FULL_GROUP_NUM_KEEP; u++) {
        sprintf(objname, "/keep/keep %u\n", u);
        if(H5Oget_info_by_name(file_id, objname, &oi, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
        if(oi.rc != 1) TEST_ERROR
    } /* end for */

    /* Close the file */
    if(H5Fclose(file_id) < 0) FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0) TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != keep_size) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Gclose(gid2);
	H5Gclose(gid);
	H5Fclose(file_id);
    } H5E_END_TRY;
    return 1;
} /* end test_full_group_compact() */


/*-------------------------------------------------------------------------
 * Function:    test_full_group_dense
 *
 * Purpose:     Test deleting a dense group which still has valid objects in it
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, January 18, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_full_group_dense(hid_t fapl)
{
    hid_t file_id = -1;
    hid_t gcpl = (-1);          /* Group creation property list ID */
    hid_t gid = -1, gid2 = -1;  /* Group IDs */
    H5O_info_t	oi;             /* Stat buffer for object */
    char objname[128];          /* Buffer for name of objects to create */
    char objname2[128];         /* Buffer for name of objects to create */
    char filename[1024];        /* Buffer for filename */
    h5_stat_size_t       keep_size;      /* Size of the file with objects to keep */
    h5_stat_size_t       file_size;      /* Size of each file created */
    unsigned u;                 /* Local index variable */

    TESTING("unlinking non-empty dense group");

    /* Create filename */
    h5_fixname(FILENAME[10], fapl, filename, sizeof filename);

    /* Create the file */
    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create group to link objects to */
    if((gid = H5Gcreate2(file_id, "/keep", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create several objects to link to */
    for(u = 0; u < FULL_GROUP_NUM_KEEP; u++) {
        sprintf(objname, "keep %u\n", u);
        if((gid2 = H5Gcreate2(gid, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
        if(H5Gclose(gid2) < 0) FAIL_STACK_ERROR
    } /* end for */

    /* Close group with objects to keep */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Close the file */
    if(H5Fclose(file_id) < 0) FAIL_STACK_ERROR

    /* Get the size of the file with only the objects to keep */
    if((keep_size = h5_get_file_size(filename, fapl)) < 0) TEST_ERROR

    /* Re-open the file */
    if((file_id = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) FAIL_STACK_ERROR

    /* Set group creation "est. link info" closer to what will actually occur */
    if((gcpl = H5Pcreate(H5P_GROUP_CREATE)) < 0) FAIL_STACK_ERROR
    if(H5Pset_est_link_info(gcpl, FULL_GROUP_EST_NUM_ENTRIES, FULL_GROUP_EST_ENTRY_LEN) < 0) FAIL_STACK_ERROR

    /* Create group to delete */
    /* (use non-default GCPL, in order to make certain that the group's object
     *  header is large enough to hold the links inserted without allocating
     *  another object header message chunk - in order to make the file size
     *  computation below easier/correct - QAK)
     */
    if((gid = H5Gcreate2(file_id, "/delete", H5P_DEFAULT, gcpl, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close GCPL */
    if(H5Pclose(gcpl) < 0) FAIL_STACK_ERROR

    /* Create external link (doesn't matter if it dangles) */
    if(H5Lcreate_external("foo.h5", "/dst", gid, "external", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Create soft link (doesn't matter if it dangles) */
    if(H5Lcreate_soft("/foo", gid, "soft", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Create hard links to objects in group to keep */
    for(u = 0; u < FULL_GROUP_NUM_KEEP; u++) {
        sprintf(objname, "/keep/keep %u\n", u);
        sprintf(objname2, "keep %u\n", u);
        if(H5Lcreate_hard(file_id, objname, gid, objname2, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    } /* end for */

    /* Create several objects to delete */
    for(u = 0; u < FULL_GROUP_NUM_DELETE_DENSE; u++) {
        sprintf(objname, "delete %u\n", u);
        if((gid2 = H5Gcreate2(gid, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
        if(H5Gclose(gid2) < 0) FAIL_STACK_ERROR
    } /* end for */

    /* Check on group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR
    if(H5G_has_links_test(gid, NULL) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(gid) != TRUE) TEST_ERROR

    /* Close group with objects to delete */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Check reference count on objects to keep */
    for(u = 0; u < FULL_GROUP_NUM_KEEP; u++) {
        sprintf(objname, "/keep/keep %u\n", u);
        if(H5Oget_info_by_name(file_id, objname, &oi, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
        if(oi.rc != 2) TEST_ERROR
    } /* end for */

    /* Close the file */
    if(H5Fclose(file_id) < 0) FAIL_STACK_ERROR


    /* Re-open the file */
    if((file_id = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) FAIL_STACK_ERROR

    /* Delete the full group */
    if(H5Ldelete(file_id, "/delete", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Check reference count on objects to keep */
    for(u = 0; u < FULL_GROUP_NUM_KEEP; u++) {
        sprintf(objname, "/keep/keep %u\n", u);
        if(H5Oget_info_by_name(file_id, objname, &oi, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
        if(oi.rc != 1) TEST_ERROR
    } /* end for */

    /* Close the file */
    if(H5Fclose(file_id) < 0) FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0) TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != keep_size) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Gclose(gid2);
	H5Gclose(gid);
    	H5Pclose(gcpl);
	H5Fclose(file_id);
    } H5E_END_TRY;
    return 1;
} /* end test_full_group_dense() */


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Test unlinking operations
 *
 * Return:	Success:	zero
 *
 *		Failure:	non-zero
 *
 * Programmer:	Robb Matzke
 *              Friday, September 25, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t	fapl, fapl2, file;
    int	nerrors = 0;
    char	filename[1024];
    hbool_t new_format;

    /* Metadata cache parameters */
    int mdc_nelmts;
    size_t rdcc_nelmts;
    size_t rdcc_nbytes;
    double rdcc_w0;

    /* Set the random # seed */
    HDsrandom((unsigned)HDtime(NULL));

    /* Open */
    h5_reset();
    fapl = h5_fileaccess();

    /* Copy the file access property list */
    if((fapl2 = H5Pcopy(fapl)) < 0) TEST_ERROR

    /* Set the "use the latest version of the format" bounds for creating objects in the file */
    if(H5Pset_libver_bounds(fapl2, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0) TEST_ERROR

    /* Test with old & new format groups */
    for(new_format = FALSE; new_format <= TRUE; new_format++) {
        hid_t my_fapl;

        /* Set the FAPL for the type of format */
        if(new_format) {
            puts("\nTesting with new group format:");
            my_fapl = fapl2;
        } /* end if */
        else {
            puts("Testing with old group format:");
            my_fapl = fapl;
        } /* end else */

        h5_fixname(FILENAME[0], my_fapl, filename, sizeof filename);
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0) TEST_ERROR

        /* Tests */
        nerrors += test_one(file);
        nerrors += test_many(file);
        nerrors += test_symlink(file);
        nerrors += test_rename(file);

        nerrors += test_new_move(my_fapl);
        nerrors += check_new_move(my_fapl);
        nerrors += test_filespace(my_fapl);

        /* Test creating & unlinking lots of objects with default FAPL */
        nerrors += test_create_unlink("create and unlink large number of objects", my_fapl);

        {
            hid_t fapl_small_mdc;

            /* Make copy of regular fapl, to turn down the elements in the metadata cache */
            if((fapl_small_mdc = H5Pcopy(my_fapl)) < 0)
                goto error;

            /* Get FAPL cache settings */
            if(H5Pget_cache(fapl_small_mdc, &mdc_nelmts, &rdcc_nelmts, &rdcc_nbytes, &rdcc_w0) < 0)
                printf("H5Pget_cache failed\n");

            /* Change FAPL cache settings */
            mdc_nelmts=1;
            if(H5Pset_cache(fapl_small_mdc, mdc_nelmts, rdcc_nelmts, rdcc_nbytes, rdcc_w0) < 0)
                printf("H5Pset_cache failed\n");

            /* Test creating & unlinking lots of objects with a 1-element metadata cache FAPL */
            nerrors += test_create_unlink("create and unlink large number of objects with small cache", fapl_small_mdc);

            if(H5Pclose(fapl_small_mdc) < 0) TEST_ERROR
        } /* end block */

        nerrors += test_link_slashes(my_fapl);
        nerrors += test_unlink_slashes(my_fapl);

        /* Test specific B-tree removal issues */
        /* (only for old format groups) */
        if(!new_format) {
            nerrors += test_unlink_rightleaf(file);
            nerrors += test_unlink_rightnode(file);
            nerrors += test_unlink_middlenode(file);
        } /* end if */

        /* Test "resurrecting" objects */
        nerrors += test_resurrect_dataset(my_fapl);
        nerrors += test_resurrect_datatype(my_fapl);
        nerrors += test_resurrect_group(my_fapl);

        /* Test unlinking chunked datasets */
        nerrors += test_unlink_chunked_dataset(my_fapl);

        /* Test unlinked groups which still have objects in them */
        /* (only for new format groups) */
        if(new_format) {
            nerrors += test_full_group_compact(my_fapl);
            nerrors += test_full_group_dense(my_fapl);
        } /* end if */

        /* Close */
        if(H5Fclose(file) < 0) TEST_ERROR
    } /* end for */

    /* Close 2nd FAPL */
    H5Pclose(fapl2);

    /* Verify symbol table messages are cached */
    nerrors += (h5_verify_cached_stabs(FILENAME, fapl) < 0 ? 1 : 0);

    if (nerrors) {
        printf("***** %d FAILURE%s! *****\n", nerrors, 1==nerrors?"":"S");
        exit(1);
    }

    puts("All unlink tests passed.");

    h5_cleanup(FILENAME, fapl);

    return 0;

error:
    return 1;
}

