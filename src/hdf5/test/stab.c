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
 *              Tuesday, November 24, 1998
 */

#define H5G_PACKAGE		/*suppress error about including H5Gpkg	  */

/* Define this macro to indicate that the testing APIs should be available */
#define H5G_TESTING

#include "h5test.h"
#include "H5Gpkg.h"		/* Groups				*/

const char *FILENAME[] = {
    "stab",
    NULL
};

#define NAME_BUF_SIZE   1024

/* Definitions for 'long' test */
#define LONG_NAME_LEN           40960

/* Definitions for 'large' test */
#define LARGE_NOBJS             5000

/* Definitions for 'lifecycle' test */
#define LIFECYCLE_TOP_GROUP     "top"
#define LIFECYCLE_BOTTOM_GROUP  "bottom %u"
#define LIFECYCLE_LOCAL_HEAP_SIZE_HINT   256
#define LIFECYCLE_MAX_COMPACT   4
#define LIFECYCLE_MIN_DENSE     3
#define LIFECYCLE_EST_NUM_ENTRIES       4
#define LIFECYCLE_EST_NAME_LEN  8

/* Definitions for 'long_compact' test */
#define LONG_COMPACT_LENGTH     ((64 * 1024) + 1024)

/* Definitions for 'read_old' test */
#define READ_OLD_NGROUPS        100

/* The group_old.h5 is generated from gen_old_fill.c in HDF5 'test' directory
 * for version 1.6.  To get this data file, simply compile gen_old_group.c with
 * the HDF5 library in that branch and run it. */
/* I changed the name "group_old.h5.copy" to "group_old_copy.h5" because OpenVMS 
 * doesn't like any file name with more than one ".". SLU 2010/12/13 */ 
#define FILE_OLD_GROUPS "group_old.h5"
#define FILE_OLD_GROUPS_COPY "group_old_copy.h5"

/* Definitions for 'no_compact' test */
#define NO_COMPACT_TOP_GROUP     "top"
#define NO_COMPACT_BOTTOM_GROUP  "bottom %u"
#define NO_COMPACT_MAX_COMPACT   0
#define NO_COMPACT_MIN_DENSE     0

/* Definitions for 'gcpl_on_root' test */
#define GCPL_ON_ROOT_MIDDLE_GROUP  "/middle"
#define GCPL_ON_ROOT_BOTTOM_GROUP  "/middle/bottom"
#define GCPL_ON_ROOT_MAX_COMPACT   4
#define GCPL_ON_ROOT_MIN_DENSE     2

/* Definitions for 'old_api' test */
#define OLD_API_GROUP           "/old_api"

/* Definitions for 'corrupt_stab_msg' test */
#define CORRUPT_STAB_FILE           "corrupt_stab_msg.h5"
#define CORRUPT_STAB_TMP_FILE       "corrupt_stab_msg_tmp.h5"
#define CORRUPT_STAB_DSET           "DS1"


/*-------------------------------------------------------------------------
 * Function:	test_misc
 *
 * Purpose:	Test miscellaneous group stuff.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Tuesday, November 24, 1998
 *
 *-------------------------------------------------------------------------
 */
static int
test_misc(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);             /* File ID */
    hid_t	g1 = (-1), g2 = (-1), g3 = (-1);
    char	filename[NAME_BUF_SIZE];
    char	comment[64];

    if(new_format)
        TESTING("miscellaneous group tests (w/new group format)")
    else
        TESTING("miscellaneous group tests")

    /* Create file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create initial groups for testing, then close */
    if((g1 = H5Gcreate2(fid, "test_1a", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if((g2 = H5Gcreate2(g1, "sub_1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if((g3 = H5Gcreate2(fid, "test_1b", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Oset_comment(g3, "hello world") < 0) TEST_ERROR
    if(H5Gclose(g1) < 0) TEST_ERROR
    if(H5Gclose(g2) < 0) TEST_ERROR
    if(H5Gclose(g3) < 0) TEST_ERROR

    /* Open all groups with absolute names to check for exsistence */
    if((g1 = H5Gopen2(fid, "/test_1a", H5P_DEFAULT)) < 0) TEST_ERROR
    if((g2 = H5Gopen2(fid, "/test_1a/sub_1", H5P_DEFAULT)) < 0) TEST_ERROR
    if((g3 = H5Gopen2(fid, "/test_1b", H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Oget_comment_by_name(g3, "././.", comment, sizeof comment, H5P_DEFAULT) < 0) TEST_ERROR
    if(HDstrcmp(comment, "hello world")) {
	H5_FAILED();
	puts("    Read the wrong comment string from the group.");
	printf("    got: \"%s\"\n    ans: \"hello world\"\n", comment);
	TEST_ERROR
    }
    if(H5Gclose(g1) < 0) TEST_ERROR
    if(H5Gclose(g2) < 0) TEST_ERROR
    if(H5Gclose(g3) < 0) TEST_ERROR

    /* Check that creating groups with no-op names isn't allowed */
    H5E_BEGIN_TRY {
        g1 = H5Gcreate2(fid, "/", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    } H5E_END_TRY
    if(g1 >= 0) TEST_ERROR

    H5E_BEGIN_TRY {
        g1 = H5Gcreate2(fid, "./././", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    } H5E_END_TRY
    if(g1 >= 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Gclose(g1);
	H5Gclose(g2);
	H5Gclose(g3);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Purpose:     Creates a group with a very long name
 *
 * Return:      Success:	0
 *
 * 		Failure:	number of errors
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov> 2002-03-28
 *
 * Modifications:
 *-------------------------------------------------------------------------
 */
static int
test_long(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);             /* File ID */
    hid_t       g1 = (-1), g2 = (-1);
    char        *name1 = NULL, *name2 = NULL;
    char	filename[NAME_BUF_SIZE];
    size_t      i;

    if(new_format)
        TESTING("long names (w/new group format)")
    else
        TESTING("long names")

    /* Create file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Group names */
    name1 = (char *)HDmalloc((size_t)LONG_NAME_LEN);
    for(i = 0; i < LONG_NAME_LEN; i++)
        name1[i] = (char)('A' + i%26);
    name1[LONG_NAME_LEN - 1] = '\0';
    name2 = (char *)HDmalloc((size_t)((2 * LONG_NAME_LEN) + 2));
    sprintf(name2, "%s/%s", name1, name1);

    /* Create groups */
    if((g1 = H5Gcreate2(fid, name1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if((g2 = H5Gcreate2(g1, name1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(g1) < 0) TEST_ERROR
    if(H5Gclose(g2) < 0) TEST_ERROR

    /* Open groups */
    if((g1 = H5Gopen2(fid, name1, H5P_DEFAULT)) < 0) TEST_ERROR
    if((g2 = H5Gopen2(fid, name2, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(g1) < 0) TEST_ERROR
    if(H5Gclose(g2) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Release name buffers */
    HDfree(name2);
    HDfree(name1);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Gclose(g1);
        H5Gclose(g2);
    	H5Fclose(fid);
        HDfree(name2);
        HDfree(name1);
    } H5E_END_TRY;
    return 1;
} /* end test_long() */


/*-------------------------------------------------------------------------
 * Function:    test_large
 *
 * Purpose:     Creates a really large directory.
 *
 * Return:      Success:	0
 *
 * 		Failure:	number of errors
 *
 * Programmer:  Robb Matzke
 *              robb@maya.nuance.com
 *              Aug 29 1997
 *
 *-------------------------------------------------------------------------
 */
static int
test_large(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);             /* File ID */
    hid_t       cwg = (-1), dir = (-1); /* Group IDs */
    char	filename[NAME_BUF_SIZE];
    char        name[NAME_BUF_SIZE];
    int         i;

    if(new_format)
        TESTING("large directories (w/new group format)")
    else
        TESTING("large directories")

    /* Create file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /*
     * Create a directory that has so many entries that the root
     * of the B-tree ends up splitting.
     */
    if((cwg = H5Gcreate2(fid, "/big", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(new_format)
        if(H5G_has_stab_test(cwg) != FALSE) TEST_ERROR
    for(i = 0; i < LARGE_NOBJS; i++) {
        sprintf(name, "%05d%05d", (HDrandom() % 100000), i);
	if((dir = H5Gcreate2(cwg, name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
        if(H5Gclose(dir) < 0) TEST_ERROR
    }
    if(new_format)
        if(H5G_is_new_dense_test(cwg) != TRUE) TEST_ERROR
    if(H5Gclose(cwg) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Gclose(dir);
	H5Gclose(cwg);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return 1;
} /* end test_large() */


/*-------------------------------------------------------------------------
 * Function:    lifecycle
 *
 * Purpose:     Test that adding links to a group follow proper "lifecycle"
 *              of empty->compact->symbol table->compact->empty.  (As group
 *              is created, links are added, then links removed)
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, October 17, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
lifecycle(hid_t fapl2)
{
    hid_t	fid = (-1);             /* File ID */
    hid_t	gid = (-1);             /* Group ID */
    hid_t	gid2 = (-1);            /* Datatype ID */
    hid_t       gcpl = (-1);            /* Group creation property list ID */
    size_t      lheap_size_hint;        /* Local heap size hint */
    unsigned    max_compact;            /* Maximum # of links to store in group compactly */
    unsigned    min_dense;              /* Minimum # of links to store in group "densely" */
    unsigned	est_num_entries;	/* Estimated # of entries in group */
    unsigned	est_name_len;		/* Estimated length of entry name */
    unsigned	nmsgs;		        /* Number of messages in group's header */
    H5O_info_t  oinfo;                  /* Object info */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    char	filename[NAME_BUF_SIZE];
    h5_stat_size_t       empty_size;             /* Size of an empty file */
    unsigned    u;                      /* Local index variable */
    h5_stat_size_t       file_size;              /* Size of each file created */

    TESTING("group lifecycle");

    /* Create file */
    h5_fixname(FILENAME[0], fapl2, filename, sizeof(filename));
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl2)) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Get size of file as empty */
    if((empty_size = h5_get_file_size(filename, fapl2)) < 0) TEST_ERROR

    /* Re-open file */
    if((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl2)) < 0) TEST_ERROR

    /* Set up group creation property list */
    if((gcpl = H5Pcreate(H5P_GROUP_CREATE)) < 0) TEST_ERROR

    /* Query default group creation property settings */
    if(H5Pget_local_heap_size_hint(gcpl, &lheap_size_hint) < 0) TEST_ERROR
    if(lheap_size_hint != H5G_CRT_GINFO_LHEAP_SIZE_HINT) TEST_ERROR
    if(H5Pget_link_phase_change(gcpl, &max_compact, &min_dense) < 0) TEST_ERROR
    if(max_compact != H5G_CRT_GINFO_MAX_COMPACT) TEST_ERROR
    if(min_dense != H5G_CRT_GINFO_MIN_DENSE) TEST_ERROR
    if(H5Pget_est_link_info(gcpl, &est_num_entries, &est_name_len) < 0) TEST_ERROR
    if(est_num_entries != H5G_CRT_GINFO_EST_NUM_ENTRIES) TEST_ERROR
    if(est_name_len != H5G_CRT_GINFO_EST_NAME_LEN) TEST_ERROR

    /* Set GCPL parameters */
    if(H5Pset_local_heap_size_hint(gcpl, (size_t)LIFECYCLE_LOCAL_HEAP_SIZE_HINT) < 0) TEST_ERROR
    if(H5Pset_link_phase_change(gcpl, LIFECYCLE_MAX_COMPACT, LIFECYCLE_MIN_DENSE) < 0) TEST_ERROR
    if(H5Pset_est_link_info(gcpl, LIFECYCLE_EST_NUM_ENTRIES, LIFECYCLE_EST_NAME_LEN) < 0) TEST_ERROR

    /* Create group for testing lifecycle */
    if((gid = H5Gcreate2(fid, LIFECYCLE_TOP_GROUP, H5P_DEFAULT, gcpl, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Query group creation property settings */
    if(H5Pget_local_heap_size_hint(gcpl, &lheap_size_hint) < 0) TEST_ERROR
    if(lheap_size_hint != LIFECYCLE_LOCAL_HEAP_SIZE_HINT) TEST_ERROR
    if(H5Pget_link_phase_change(gcpl, &max_compact, &min_dense) < 0) TEST_ERROR
    if(max_compact != LIFECYCLE_MAX_COMPACT) TEST_ERROR
    if(min_dense != LIFECYCLE_MIN_DENSE) TEST_ERROR
    if(H5Pget_est_link_info(gcpl, &est_num_entries, &est_name_len) < 0) TEST_ERROR
    if(est_num_entries != LIFECYCLE_EST_NUM_ENTRIES) TEST_ERROR
    if(est_name_len != LIFECYCLE_EST_NAME_LEN) TEST_ERROR

    /* Use internal testing routine to check that the group has no links or symbol table */
    if(H5G_is_empty_test(gid) != TRUE) TEST_ERROR

    /* Create first "bottom" group */
    sprintf(objname, LIFECYCLE_BOTTOM_GROUP, (unsigned)0);
    if((gid2 = H5Gcreate2(gid, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check on bottom group's status */
    if(H5G_is_empty_test(gid2) != TRUE) TEST_ERROR

    /* Close bottom group */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Check on top group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR
    if(H5G_has_links_test(gid, &nmsgs) != TRUE) TEST_ERROR
    if(nmsgs != 1) TEST_ERROR

    /* Create several more bottom groups, to push the top group almost to a symbol table */
    /* (Start counting at '1', since we've already created one bottom group */
    for(u = 1; u < LIFECYCLE_MAX_COMPACT; u++) {
        sprintf(objname, LIFECYCLE_BOTTOM_GROUP, u);
        if((gid2 = H5Gcreate2(gid, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

        /* Check on bottom group's status */
        if(H5G_is_empty_test(gid2) != TRUE) TEST_ERROR

        /* Close bottom group */
        if(H5Gclose(gid2) < 0) TEST_ERROR
    } /* end for */

    /* Check on top group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR
    if(H5G_has_links_test(gid, &nmsgs) != TRUE) TEST_ERROR
    if(nmsgs != LIFECYCLE_MAX_COMPACT) TEST_ERROR
    if(H5G_is_new_dense_test(gid) != FALSE) TEST_ERROR

    /* Check that the object header is only one chunk and the space has been allocated correctly */
    if(H5Oget_info(gid, &oinfo) < 0) TEST_ERROR
    if(oinfo.hdr.space.total != 151) TEST_ERROR
    if(oinfo.hdr.space.free != 0) TEST_ERROR
    if(oinfo.hdr.nmesgs != 6) TEST_ERROR
    if(oinfo.hdr.nchunks != 1) TEST_ERROR

    /* Create one more "bottom" group, which should push top group into using a symbol table */
    sprintf(objname, LIFECYCLE_BOTTOM_GROUP, u);
    if((gid2 = H5Gcreate2(gid, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check on bottom group's status */
    if(H5G_is_empty_test(gid2) != TRUE) TEST_ERROR

    /* Close bottom group */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Check on top group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR
    if(H5G_has_links_test(gid, NULL) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(gid) != TRUE) TEST_ERROR

    /* Check that the object header is still one chunk and the space has been allocated correctly */
    if(H5Oget_info(gid, &oinfo) < 0) TEST_ERROR
    if(oinfo.hdr.space.total != 151) TEST_ERROR
    if(oinfo.hdr.space.free != 92) TEST_ERROR
    if(oinfo.hdr.nmesgs != 3) TEST_ERROR
    if(oinfo.hdr.nchunks != 1) TEST_ERROR

    /* Unlink objects from top group */
    while(u >= LIFECYCLE_MIN_DENSE) {
        sprintf(objname, LIFECYCLE_BOTTOM_GROUP, u);

        if(H5Ldelete(gid, objname, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

        u--;
    } /* end while */

    /* Check on top group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR
    if(H5G_has_links_test(gid, NULL) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(gid) != TRUE) TEST_ERROR

    /* Unlink one more object from the group, which should transform back to using links */
    sprintf(objname, LIFECYCLE_BOTTOM_GROUP, u);
    if(H5Ldelete(gid, objname, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    u--;

    /* Check on top group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR
    if(H5G_has_links_test(gid, &nmsgs) != TRUE) TEST_ERROR
    if(nmsgs != (LIFECYCLE_MIN_DENSE - 1)) TEST_ERROR

    /* Unlink last two objects from top group */
    sprintf(objname, LIFECYCLE_BOTTOM_GROUP, u);
    if(H5Ldelete(gid, objname, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    u--;
    sprintf(objname, LIFECYCLE_BOTTOM_GROUP, u);
    if(H5Ldelete(gid, objname, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Check on top group's status */
    if(H5G_is_empty_test(gid) != TRUE) TEST_ERROR

    /* Close top group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Unlink top group */
    if(H5Ldelete(fid, LIFECYCLE_TOP_GROUP, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close GCPL */
    if(H5Pclose(gcpl) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Get size of file as empty */
    if((file_size = h5_get_file_size(filename, fapl2)) < 0) TEST_ERROR

    /* Verify that file is correct size */
    if(file_size != empty_size) TEST_ERROR


    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Gclose(gcpl);
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return 1;
} /* end lifecycle() */


/*-------------------------------------------------------------------------
 * Function:    long_compact
 *
 * Purpose:     Test that long links are correctly _not_ put into compact
 *              form.
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, October 18, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
long_compact(hid_t fapl2)
{
    hid_t	fid = (-1);             /* File ID */
    hid_t	gid = (-1);             /* Group ID */
    hid_t	gid2 = (-1);            /* Group ID */
    char        *objname;               /* Object name */
    char	filename[NAME_BUF_SIZE];
    h5_stat_size_t       empty_size;             /* Size of an empty file */
    h5_stat_size_t       file_size;              /* Size of each file created */

    TESTING("long link names in compact groups");

    /* Create file */
    h5_fixname(FILENAME[0], fapl2, filename, sizeof(filename));
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl2)) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Get size of file as empty */
    if((empty_size = h5_get_file_size(filename, fapl2)) < 0) TEST_ERROR

    /* Construct very long object name template */
    if((objname = (char *)HDmalloc((size_t)(LONG_COMPACT_LENGTH + 1))) == NULL) TEST_ERROR
    HDmemset(objname, 'a', (size_t)LONG_COMPACT_LENGTH);
    objname[LONG_COMPACT_LENGTH] = '\0';

    /* Re-open file */
    if((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl2)) < 0) TEST_ERROR

    /* Create top group */
    if((gid = H5Gcreate2(fid, "top", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Use internal testing routine to check that the group has no links or dense storage */
    if(H5G_is_empty_test(gid) != TRUE) TEST_ERROR

    /* Create first group with "long" name */
    if((gid2 = H5Gcreate2(gid, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check on bottom group's status */
    if(H5G_is_empty_test(gid2) != TRUE) TEST_ERROR

    /* Close bottom group */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Check on top group's status */
    /* (Should have dense storage to hold links, since name is too long for object header message) */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR
    if(H5G_has_links_test(gid, NULL) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(gid) != TRUE) TEST_ERROR

    /* Create second group with "long" name */
    objname[0] = 'b';
    if((gid2 = H5Gcreate2(gid, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check on bottom group's status */
    if(H5G_is_empty_test(gid2) != TRUE) TEST_ERROR

    /* Close bottom group */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Check on top group's status */
    /* (Should have dense storage to hold links, since name is too long for object header message) */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR
    if(H5G_has_links_test(gid, NULL) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(gid) != TRUE) TEST_ERROR

    /* Unlink second object from top group */
    if(H5Ldelete(gid, objname, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Check on top group's status */
    /* (Should still be dense storage to hold links, since name is too long for object header message) */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR
    if(H5G_has_links_test(gid, NULL) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(gid) != TRUE) TEST_ERROR

    /* Unlink first object from top group */
    objname[0] = 'a';
    if(H5Ldelete(gid, objname, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Check on top group's status */
    /* (Should have deleted the dense storage now) */
    if(H5G_is_empty_test(gid) != TRUE) TEST_ERROR

    /* Free object name */
    HDfree(objname);

    /* Close top group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Unlink top group */
    if(H5Ldelete(fid, "top", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Get size of file as empty */
    if((file_size = h5_get_file_size(filename, fapl2)) < 0) TEST_ERROR

    /* Verify that file is correct size */
    if(file_size != empty_size) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return 1;
} /* end long_compact() */


/*-------------------------------------------------------------------------
 * Function:    read_old
 *
 * Purpose:     Test reading a file with "old style" (symbol table) groups
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, October 24, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
read_old(void)
{
    hid_t fid = (-1);                   /* File ID */
    hid_t gid = (-1);                   /* Group ID */
    hid_t gid2 = (-1);                  /* Group ID */
    char  objname[NAME_BUF_SIZE];       /* Object name */
    unsigned    u;                      /* Local index variable */

    TESTING("reading old groups");

    /* Make a copy of the data file from svn. */
    if(h5_make_local_copy(FILE_OLD_GROUPS, FILE_OLD_GROUPS_COPY) < 0) TEST_ERROR

    /* Open copied file */
    if((fid = H5Fopen(FILE_OLD_GROUPS_COPY, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Attempt to open "old" group */
    if((gid = H5Gopen2(fid, "old", H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check on old group's status */
    if(H5G_is_empty_test(gid) == FALSE) TEST_ERROR
    if(H5G_has_links_test(gid, NULL) == TRUE) TEST_ERROR
    if(H5G_has_stab_test(gid) != TRUE) TEST_ERROR

    /* Create a bunch of objects in the group */
    for(u = 0; u < READ_OLD_NGROUPS; u++) {
        sprintf(objname, "Group %u", u);
        if((gid2 = H5Gcreate2(gid, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

        /* Check on bottom group's status */
        if(H5G_is_empty_test(gid2) != TRUE) TEST_ERROR

        /* Close bottom group */
        if(H5Gclose(gid2) < 0) TEST_ERROR
    } /* end for */

    /* Check on old group's status */
    /* (Should stay in old "symbol table" form) */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR
    if(H5G_has_links_test(gid, NULL) == TRUE) TEST_ERROR
    if(H5G_has_stab_test(gid) != TRUE) TEST_ERROR

    /* Delete new objects from old group */
    for(u = 0; u < READ_OLD_NGROUPS; u++) {
        sprintf(objname, "Group %u", u);
        if(H5Ldelete(gid, objname, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    } /* end for */

    /* Check on old group's status */
    /* (Should stay in old "symbol table" form, but have no links) */
    if(H5G_is_empty_test(gid) == FALSE) TEST_ERROR
    if(H5G_has_links_test(gid, NULL) == TRUE) TEST_ERROR
    if(H5G_has_stab_test(gid) != TRUE) TEST_ERROR

    /* Close old group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close first file */
    if(H5Fclose(fid)<0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Gclose(gid);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return 1;
} /* end read_old() */


/*-------------------------------------------------------------------------
 * Function:    no_compact
 *
 * Purpose:     Test that its possible to create groups that don't use the
 *              compact form directly (and don't use link messages).
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, October 25, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
no_compact(hid_t fapl2)
{
    hid_t	fid = (-1);             /* File ID */
    hid_t	gid = (-1);             /* Group ID */
    hid_t	gid2 = (-1);            /* Datatype ID */
    hid_t       gcpl = (-1);            /* Group creation property list ID */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    char	filename[NAME_BUF_SIZE];
    h5_stat_size_t       empty_size;             /* Size of an empty file */
    h5_stat_size_t       file_size;              /* Size of each file created */
    unsigned	est_num_entries;	/* Estimated # of entries in group */
    unsigned	est_name_len;		/* Estimated length of entry name */

    TESTING("group without compact form");

    /* Create file */
    h5_fixname(FILENAME[0], fapl2, filename, sizeof(filename));
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl2)) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Get size of file as empty */
    if((empty_size = h5_get_file_size(filename, fapl2)) < 0) TEST_ERROR

    /* Re-open file */
    if((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl2)) < 0) TEST_ERROR

    /* Set up group creation property list */
    if((gcpl = H5Pcreate(H5P_GROUP_CREATE)) < 0) TEST_ERROR

    /* Set GCPL parameters */
    if(H5Pset_link_phase_change(gcpl, NO_COMPACT_MAX_COMPACT, NO_COMPACT_MIN_DENSE) < 0) TEST_ERROR

    /* Check information for default group creation */
    if(H5Pget_est_link_info(gcpl, &est_num_entries, &est_name_len) < 0) TEST_ERROR
    if(est_num_entries != H5G_CRT_GINFO_EST_NUM_ENTRIES) TEST_ERROR
    if(est_name_len != H5G_CRT_GINFO_EST_NAME_LEN) TEST_ERROR

    /* Create group for testing no compact form */
    if((gid = H5Gcreate2(fid, NO_COMPACT_TOP_GROUP, H5P_DEFAULT, gcpl, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close GCPL */
    if(H5Pclose(gcpl) < 0) TEST_ERROR

    /* Use internal testing routine to check that the group has no links or dense storage */
    if(H5G_is_empty_test(gid) != TRUE) TEST_ERROR

    /* Create first "bottom" group */
    sprintf(objname, NO_COMPACT_BOTTOM_GROUP, (unsigned)0);
    if((gid2 = H5Gcreate2(gid, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check on bottom group's status */
    if(H5G_is_empty_test(gid2) != TRUE) TEST_ERROR

    /* Close bottom group */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Check on top group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR
    if(H5G_has_links_test(gid, NULL) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(gid) != TRUE) TEST_ERROR

    /* Unlink object from top group */
    sprintf(objname, NO_COMPACT_BOTTOM_GROUP, (unsigned)0);
    if(H5Ldelete(gid, objname, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Check on top group's status */
    if(H5G_is_empty_test(gid) != TRUE) TEST_ERROR

    /* Close top group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Unlink top group */
    if(H5Ldelete(fid, NO_COMPACT_TOP_GROUP, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Get size of file as empty */
    if((file_size = h5_get_file_size(filename, fapl2)) < 0) TEST_ERROR

    /* Verify that file is correct size */
    if(file_size != empty_size) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Gclose(gcpl);
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return 1;
} /* end no_compact() */


/*-------------------------------------------------------------------------
 * Function:    gcpl_on_root
 *
 * Purpose:     Test setting group creation properties for root group.
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, October 25, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
gcpl_on_root(hid_t fapl2)
{
    hid_t	fid = (-1);             /* File ID */
    hid_t	gid = (-1);             /* Group ID */
    hid_t	gid2 = (-1);            /* Datatype ID */
    hid_t       fcpl = (-1);            /* File creation property list ID */
    hid_t       gcpl = (-1);            /* Group creation property list ID */
    hid_t       lcpl = (-1);            /* Link creation property list ID */
    unsigned    max_compact;            /* Maximum # of links to store in group compactly */
    unsigned    min_dense;              /* Minimum # of links to store in group "densely" */
    char	filename[NAME_BUF_SIZE];

    TESTING("setting root group creation properties");

    /* Create file */
    h5_fixname(FILENAME[0], fapl2, filename, sizeof(filename));

    /* Set up file creation property list */
    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0) TEST_ERROR

    /* Set GCPL parameters */
    if(H5Pset_link_phase_change(fcpl, GCPL_ON_ROOT_MAX_COMPACT, GCPL_ON_ROOT_MIN_DENSE) < 0) TEST_ERROR

    /* Query the group creation properties from the FCPL */
    if(H5Pget_link_phase_change(fcpl, &max_compact, &min_dense) < 0) TEST_ERROR
    if(max_compact != GCPL_ON_ROOT_MAX_COMPACT) TEST_ERROR
    if(min_dense != GCPL_ON_ROOT_MIN_DENSE) TEST_ERROR

    /* Create file with modified root group creation properties */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl2)) < 0) TEST_ERROR

    /* Close FCPL */
    if(H5Pclose(fcpl) < 0) TEST_ERROR

    /* Open the root group */
    if((gid = H5Gopen2(fid, "/", H5P_DEFAULT)) < 0) TEST_ERROR

    /* Query the group creation properties */
    if((gcpl = H5Gget_create_plist(gid)) < 0) TEST_ERROR
    if(H5Pget_link_phase_change(gcpl, &max_compact, &min_dense) < 0) TEST_ERROR
    if(max_compact != GCPL_ON_ROOT_MAX_COMPACT) TEST_ERROR
    if(min_dense != GCPL_ON_ROOT_MIN_DENSE) TEST_ERROR

    /* Close GCPL */
    if(H5Pclose(gcpl) < 0) TEST_ERROR

    /* Create a link creation property list, with intermediate group creation set */
    if((lcpl = H5Pcreate(H5P_LINK_CREATE)) < 0) TEST_ERROR
    if(H5Pset_create_intermediate_group(lcpl, TRUE) < 0) TEST_ERROR

    /* Create a group and intermediate groups, to check if root group settings are inherited */
    if((gid2 = H5Gcreate2(gid, GCPL_ON_ROOT_BOTTOM_GROUP, lcpl, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close LCPL */
    if(H5Pclose(lcpl) < 0) TEST_ERROR

    /* Query the group creation properties */
    if((gcpl = H5Gget_create_plist(gid2)) < 0) TEST_ERROR
    if(H5Pget_link_phase_change(gcpl, &max_compact, &min_dense) < 0) TEST_ERROR
    if(max_compact != H5G_CRT_GINFO_MAX_COMPACT) TEST_ERROR
    if(min_dense != H5G_CRT_GINFO_MIN_DENSE) TEST_ERROR

    /* Close GCPL */
    if(H5Pclose(gcpl) < 0) TEST_ERROR

    /* Close bottom group */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Open the middle group */
    if((gid2 = H5Gopen2(fid, GCPL_ON_ROOT_MIDDLE_GROUP, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Query the group creation properties */
    if((gcpl = H5Gget_create_plist(gid2)) < 0) TEST_ERROR
    if(H5Pget_link_phase_change(gcpl, &max_compact, &min_dense) < 0) TEST_ERROR
    if(max_compact != GCPL_ON_ROOT_MAX_COMPACT) TEST_ERROR
    if(min_dense != GCPL_ON_ROOT_MIN_DENSE) TEST_ERROR

    /* Close GCPL */
    if(H5Pclose(gcpl) < 0) TEST_ERROR

    /* Close bottom group */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close root group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Gclose(lcpl);
    	H5Gclose(gcpl);
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Gclose(fcpl);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return 1;
} /* end gcpl_on_root() */


/*-------------------------------------------------------------------------
 * Function:    old_api
 *
 * Purpose:     Test old API routines
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Thursday, August 23, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
old_api(hid_t fapl)
{
#ifndef H5_NO_DEPRECATED_SYMBOLS
    hid_t	fid = (-1);             /* File ID */
    hid_t	gid = (-1);             /* Group ID */
    h5_stat_size_t small_file_size;     /* Size of small group file */
    h5_stat_size_t large_file_size;     /* Size of large group file */
    char	filename[NAME_BUF_SIZE];
#endif /* H5_NO_DEPRECATED_SYMBOLS */

    TESTING("old API routines");

#ifndef H5_NO_DEPRECATED_SYMBOLS
    /* Create file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create file */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create a group, with the old API routine and a "small" heap */
    if((gid = H5Gcreate1(fid, OLD_API_GROUP, (size_t)0)) < 0) FAIL_STACK_ERROR

    /* Close group */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    /* Get the size of the file with a "small" heap for group */
    if((small_file_size = h5_get_file_size(filename, fapl)) < 0) TEST_ERROR


    /* Create file */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create a group, with the old API routine and a "large" heap */
    if((gid = H5Gcreate1(fid, OLD_API_GROUP, (size_t)10000)) < 0) FAIL_STACK_ERROR

    /* Close group */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Re-open group */
    if((gid = H5Gopen1(fid, OLD_API_GROUP)) < 0) FAIL_STACK_ERROR

    /* Close group */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    /* Get the size of the file with a "large" heap for group */
    if((large_file_size = h5_get_file_size(filename, fapl)) < 0) TEST_ERROR

    /* Check that the file with a "large" group heap is actually bigger */
    if(large_file_size <= small_file_size) TEST_ERROR

    PASSED();
#else /* H5_NO_DEPRECATED_SYMBOLS */
    /* Shut compiler up */
    fapl = fapl;

    SKIPPED();
    puts("    Deprecated API symbols not enabled");
#endif /* H5_NO_DEPRECATED_SYMBOLS */

    return 0;

#ifndef H5_NO_DEPRECATED_SYMBOLS
error:
    H5E_BEGIN_TRY {
    	H5Gclose(gid);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return 1;
#endif /* H5_NO_DEPRECATED_SYMBOLS */
} /* end old_api() */


/*-------------------------------------------------------------------------
 * Function:    corrupt_stab_msg
 *
 * Purpose:     Test that a corrupt symbol table message can be fixed
 *              using the cached symbol table information.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Neil Fortner
 *              Wednesday, March 18, 2009
 *
 *-------------------------------------------------------------------------
 */
static int
corrupt_stab_msg(void)
{
    hid_t       fid = (-1);             /* File ID */
    hid_t       did = (-1);             /* Dataset ID */

    TESTING("corrupt symbol table message");

    /* Make a copy of the data file from svn. */
    if(h5_make_local_copy(CORRUPT_STAB_FILE, CORRUPT_STAB_TMP_FILE) < 0) TEST_ERROR

#ifndef H5_STRICT_FORMAT_CHECKS
    /* Open temp file through HDF5 library */
    if((fid = H5Fopen(CORRUPT_STAB_TMP_FILE, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Open dataset */
    if((did = H5Dopen2(fid, CORRUPT_STAB_DSET, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close dataset and file */
    if(H5Dclose(did) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Now reopen with read only access.  This verifies that the issue has been
     * corrected, as the symbol table message is not patched in read only mode.
     */

    /* Open file */
    if((fid = H5Fopen(CORRUPT_STAB_TMP_FILE, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Open dataset */
    if((did = H5Dopen2(fid, CORRUPT_STAB_DSET, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close dataset and file */
    if(H5Dclose(did) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

#else /* H5_STRICT_FORMAT_CHECKS */
    /* Open file */
    if((fid = H5Fopen(CORRUPT_STAB_TMP_FILE, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Verify that an error is thrown when we try to access the dataset */
    H5E_BEGIN_TRY {
        did = H5Dopen2(fid, CORRUPT_STAB_DSET, H5P_DEFAULT);
    } H5E_END_TRY
    if(did >= 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

#endif /* H5_STRICT_FORMAT_CHECKS */

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did);
    	H5Fclose(fid);
    } H5E_END_TRY;

    return 1;
} /* end old_api() */


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Test groups
 *
 * Return:	Success:	zero
 *
 *		Failure:	non-zero
 *
 * Programmer:	Robb Matzke
 *              Tuesday, November 24, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t	fapl, fapl2;    /* File access property list IDs */
    hbool_t new_format;     /* Whether to use the new format or not */
    int	nerrors = 0;

    /* Reset library */
    h5_reset();
    fapl = h5_fileaccess();

    /* Copy the file access property list */
    if((fapl2 = H5Pcopy(fapl)) < 0) TEST_ERROR

    /* Set the "use the latest version of the format" bounds for creating objects in the file */
    if(H5Pset_libver_bounds(fapl2, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0) TEST_ERROR

    /* Loop over using new group format */
    for(new_format = FALSE; new_format <= TRUE; new_format++) {
        /* Perform basic tests, with old & new style groups */
        nerrors += test_misc((new_format ? fapl2 : fapl), new_format);
        nerrors += test_long((new_format ? fapl2 : fapl), new_format);
        nerrors += test_large((new_format ? fapl2 : fapl), new_format);
    } /* end for */

    /* New format group specific tests (require new format features) */
    nerrors += lifecycle(fapl2);
    nerrors += long_compact(fapl2);
    nerrors += read_old();
    nerrors += no_compact(fapl2);
    nerrors += gcpl_on_root(fapl2);

    /* Old group API specific tests */
    nerrors += old_api(fapl);
    nerrors += corrupt_stab_msg();

    /* Close 2nd FAPL */
    H5Pclose(fapl2);

    /* Verify symbol table messages are cached */
    nerrors += (h5_verify_cached_stabs(FILENAME, fapl) < 0 ? 1 : 0);

    /* Check for test errors */
    if(nerrors)
        goto error;

    puts("All symbol table tests passed.");

    /* Cleanup */
    if (GetTestCleanup()) {
        HDremove(FILE_OLD_GROUPS_COPY);
        HDremove(CORRUPT_STAB_TMP_FILE);
    }
    h5_cleanup(FILENAME, fapl);

    return 0;

error:
    puts("*** TESTS FAILED ***");
    return 1;
}

