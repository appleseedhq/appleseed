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
 * Programmer:  Pedro Vicente <pvn@ncsa.uiuc.edu>
 *              April 12, 2002
 *
 * Purpose:     Tests the "ID to name" functionality
 */

#define H5G_PACKAGE		/*suppress error about including H5Gpkg	  */
#define H5I_PACKAGE		/*suppress error about including H5Ipkg	  */

/* Define these macros to indicate that the testing APIs should be available */
#define H5G_TESTING
#define H5I_TESTING

#include "h5test.h"
#include "H5Gpkg.h"		/* Groups				*/
#include "H5Ipkg.h"		/* IDs					*/


/* Compound datatype */
typedef struct s1_t {
    unsigned int a;
    unsigned int b;
    float c;
} s1_t;

const char *FILENAME[] = {
    "getname",
    "getname1",
    "getname2",
    "getname3",
    NULL
};

#define NAME_BUF_SIZE   64
#define SMALL_NAME_BUF_SIZE   2

/* Object reference macros */
#define SPACE1_RANK	1
#define SPACE1_DIM1	8

/* Dataset region reference macros */
#define REFREG_DSETNAMEV "MATRIX"
#define REFREG_DSETNAMER "REGION_REFERENCES"

static int
check_name(hid_t id, const char *chk_name, const char *chk_user_path)
{
    char name[NAME_BUF_SIZE];           /* Buffer to hold name and its size */
    char user_path[NAME_BUF_SIZE];      /* Buffer to hold user path */
    size_t user_path_len;               /* Length of user path */
    unsigned user_path_hidden;          /* Whether the user path is hidden */

    /* Get name */
    *name = '\0';
    if(H5Iget_name(id, name, NAME_BUF_SIZE) < 0) TEST_ERROR

    /* Get user path */
    *user_path = '\0';
    if(H5G_user_path_test(id, user_path, &user_path_len, &user_path_hidden) < 0) TEST_ERROR

    /* Check on name from H5Iget_name() */
    if(HDstrcmp(name, chk_name)) goto error;

    /* Check on user path */
    if(HDstrcmp(user_path, chk_user_path)) goto error;

    /* Check that if user path is hidden, the name from H5Iget_name() and the user path should be different */
    if(user_path_hidden && !HDstrcmp(chk_name, chk_user_path)) TEST_ERROR

    /* Everything matches */
    return 0;

error:
    /* Something doesn't match or something bad happened */
    return -1;
}

static int
test_main(hid_t file_id, hid_t fapl)
{
    char filename1[1024];
    char filename2[1024];
    char filename3[1024];
    hid_t   file1_id, file2_id, file3_id;
    hid_t   group_id, group2_id, group3_id, group4_id, group5_id, group6_id, group7_id;
    hid_t   dataset_id, dataset2_id;
    hid_t   space_id;
    hid_t   type_id, type2_id;
    hsize_t dims[1] = { 5 };
    size_t  name_len; /* Name length */

    /* Initialize the file names */
    h5_fixname(FILENAME[1], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[2], fapl, filename2, sizeof filename2);
    h5_fixname(FILENAME[3], fapl, filename3, sizeof filename3);

    /*-------------------------------------------------------------------------
    * Test H5Iget_name with one group
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with one group");

    /* Create group "g0" in the root group using absolute name */
    if((group_id = H5Gcreate2(file_id, "/g0", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group_id, "/g0", "/g0") < 0) TEST_ERROR

    /* Close */
    H5Gclose(group_id);

    PASSED();



    /*-------------------------------------------------------------------------
    * Test H5Iget_name with more than one group
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with more than one group");
    /* Create group "g1" in the root group using absolute name */
    if((group_id = H5Gcreate2(file_id, "/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create group "g2" in group "g1" using absolute name */
    if((group2_id = H5Gcreate2(file_id, "/g1/g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group_id, "/g1", "/g1") < 0) TEST_ERROR

    /* Verify */
    if(check_name(group2_id, "/g1/g2", "/g1/g2") < 0) TEST_ERROR

    /* Close */
    H5Gclose(group_id);
    H5Gclose(group2_id);

    PASSED();


    /*-------------------------------------------------------------------------
    * Test H5Iget_name with H5Gopen2
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with H5Gopen2");

    /* Reopen the group */
    if((group_id = H5Gopen2(file_id, "/g1", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Reopen the group */
    if((group2_id = H5Gopen2(file_id, "/g1/g2", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group_id, "/g1", "/g1") < 0) TEST_ERROR

    /* Verify */
    if(check_name(group2_id, "/g1/g2", "/g1/g2") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR

    PASSED();




    /*-------------------------------------------------------------------------
    * Test H5Iget_name with H5Dcreate2
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with H5Dcreate2");

    /* Create the dataspace  */
    if((space_id = H5Screate_simple(1, dims, NULL)) < 0) TEST_ERROR

    /* Create a new dataset */
    if((dataset_id = H5Dcreate2(file_id , "d1", H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Verify */
    if(check_name(dataset_id, "/d1", "/d1") < 0) TEST_ERROR

    /* Close */
    if(H5Dclose(dataset_id) < 0) FAIL_STACK_ERROR

    /* Reopen the group */
    if((group_id = H5Gopen2(file_id, "g1", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create a new dataset inside "g1" */
    if((dataset_id = H5Dcreate2(group_id , "d1", H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(dataset_id, "/g1/d1", "/g1/d1") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Dclose(dataset_id) < 0) FAIL_STACK_ERROR
    if(H5Sclose(space_id) < 0) FAIL_STACK_ERROR

    PASSED();



    /*-------------------------------------------------------------------------
    * Test H5Iget_name with H5Dopen2
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with H5Dopen2");

    /* Reopen the dataset */
    if((dataset_id = H5Dopen2(file_id, "d1", H5P_DEFAULT)) < 0) TEST_ERROR

    /* Verify */
    if(check_name(dataset_id, "/d1", "/d1") < 0) TEST_ERROR

    /* Close */
    if(H5Dclose(dataset_id) < 0) FAIL_STACK_ERROR


    /* Reopen the group */
    if((group_id = H5Gopen2(file_id, "g1", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Reopen the dataset */
    if((dataset_id = H5Dopen2(group_id, "d1", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(dataset_id, "/g1/d1", "/g1/d1") < 0) TEST_ERROR

    /* Close */
    if(H5Dclose(dataset_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR

    PASSED();



    /*-------------------------------------------------------------------------
     * Test H5Iget_name with a long path
     *-------------------------------------------------------------------------
     */

    TESTING("H5Iget_name with a long path");

    /* Create group "g2/bar/baz" */
    if((group_id = H5Gcreate2(file_id, "g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file_id, "g2/bar", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group3_id = H5Gcreate2(file_id, "g2/bar/baz", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create a dataset */
    if((space_id = H5Screate_simple(1, dims, NULL)) < 0) TEST_ERROR
    if((dataset_id = H5Dcreate2(group3_id , "d1", H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close */
    if(H5Dclose(dataset_id) < 0) FAIL_STACK_ERROR
    if(H5Sclose(space_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR

    /* Reopen the dataset */
    if((dataset_id = H5Dopen2(file_id, "/g2/bar/baz/d1", H5P_DEFAULT)) < 0) TEST_ERROR

    /* Verify */
    if(check_name(dataset_id, "/g2/bar/baz/d1", "/g2/bar/baz/d1") < 0) TEST_ERROR

    /* Close */
    if(H5Dclose(dataset_id) < 0) FAIL_STACK_ERROR

    PASSED();


    /*-------------------------------------------------------------------------
     * Test H5Iget_name with H5Tcommit2
     *-------------------------------------------------------------------------
     */

    TESTING("H5Iget_name with H5Tcommit2");

    /* Create a datatype */
    if((type_id = H5Tcreate(H5T_COMPOUND, sizeof(s1_t))) < 0) TEST_ERROR

    /* Insert fields */
    if(H5Tinsert(type_id, "a", HOFFSET(s1_t, a), H5T_NATIVE_INT) < 0) TEST_ERROR
    if(H5Tinsert(type_id, "b", HOFFSET(s1_t, b), H5T_NATIVE_INT) < 0) TEST_ERROR
    if(H5Tinsert(type_id, "c", HOFFSET(s1_t, c), H5T_NATIVE_FLOAT) < 0) TEST_ERROR

    /* Save datatype for later */
    if(H5Tcommit2(file_id, "t1", type_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Verify */
    if(check_name(type_id, "/t1", "/t1") < 0) TEST_ERROR

    /* Close datatype */
    H5Tclose(type_id);

    PASSED();

    /*-------------------------------------------------------------------------
     * Test H5Iget_name with H5Topen2
     *-------------------------------------------------------------------------
     */

    TESTING("H5Iget_name with H5Topen2");

    /* Open the named datatype */
    if((type_id = H5Topen2(file_id, "t1", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(type_id, "/t1", "/t1") < 0) TEST_ERROR

    /* Close datatype */
    if(H5Tclose(type_id) < 0) FAIL_STACK_ERROR

    PASSED();



   /*-------------------------------------------------------------------------
    * Test H5Iget_name with H5Lmove and H5Gopen2
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with H5Lmove and H5Gopen2");

    /* Reopen the group */
    if((group_id = H5Gopen2(file_id, "/g1", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Rename group */
    if(H5Lmove(file_id, "/g1", H5L_SAME_LOC, "/g1a", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group_id, "/g1a", "/g1a") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR

    PASSED();



   /*-------------------------------------------------------------------------
    * Test H5Iget_name with H5Lmove and H5Dopen2
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with H5Lmove and H5Dopen2");

    /* Reopen the dataset */
    if((dataset_id = H5Dopen2(file_id, "/d1", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Rename dataset */
    if(H5Lmove(file_id, "/d1", H5L_SAME_LOC, "/d1a", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(dataset_id, "/d1a", "/d1a") < 0) FAIL_STACK_ERROR

    /* Close */
    if(H5Dclose(dataset_id) < 0) FAIL_STACK_ERROR

    PASSED();



   /*-------------------------------------------------------------------------
    * Test H5Iget_name with H5Lmove and H5Topen2
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with H5Lmove and H5Topen2");

    /* Open the named datatype */
    if((type_id = H5Topen2(file_id, "/t1", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Rename datatype */
    if(H5Lmove(file_id, "/t1", H5L_SAME_LOC, "/t1a", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(type_id, "/t1a", "/t1a") < 0) FAIL_STACK_ERROR

    /* Close datatype */
    if(H5Tclose(type_id) < 0) FAIL_STACK_ERROR

    PASSED();


    /*-------------------------------------------------------------------------
    * Test H5Iget_name with H5Lmove and relative names
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with H5Lmove and relative names");

    /* Create group "/g3" */
    if((group_id = H5Gcreate2(file_id, "/g3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create group "/g3/foo" using absolute name */
    if((group2_id = H5Gcreate2(file_id, "/g3/foo1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Open group "/g3/foo" again */
    if((group3_id = H5Gopen2(file_id, "/g3/foo1", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Rename group */
    if(H5Lmove(group_id, "foo1", H5L_SAME_LOC, "foo2", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group_id, "/g3", "/g3") < 0) TEST_ERROR

    /* Verify */
    if(check_name(group2_id, "/g3/foo2", "/g3/foo2") < 0) TEST_ERROR

    /* Verify */
    if(check_name(group3_id, "/g3/foo2", "/g3/foo2") < 0) TEST_ERROR

    /* Rename group again */
    if(H5Lmove(file_id, "g3/foo2", H5L_SAME_LOC, "g3/foo1", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group_id, "/g3", "/g3") < 0) TEST_ERROR

    /* Verify */
    if(check_name(group2_id, "/g3/foo1", "/g3/foo1") < 0) TEST_ERROR

    /* Verify */
    if(check_name(group3_id, "/g3/foo1", "/g3/foo1") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR

    PASSED();



   /*-------------------------------------------------------------------------
    * Test H5Iget_name with H5Lmove and a long path
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with H5Lmove and a long path");

    /* Create group "g4/A/B" */
    if((group_id = H5Gcreate2(file_id, "g4", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file_id, "g4/A", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group3_id = H5Gcreate2(file_id, "g4/A/B", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create group "g5/C" */
    if((group4_id = H5Gcreate2(file_id, "g5", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group5_id = H5Gcreate2(file_id, "g5/C", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group3_id, "/g4/A/B", "/g4/A/B") < 0) TEST_ERROR

    /* Move group "B" to "D"*/
    if(H5Lmove(file_id, "/g4/A/B", H5L_SAME_LOC, "/g5/C/D", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group3_id, "/g5/C/D", "/g5/C/D") < 0) TEST_ERROR

    /* Move group "/g5/C/D" back to "/g4/A/B" using relative name */
    if(H5Lmove(group5_id, "D", group2_id, "B", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group3_id, "/g4/A/B", "/g4/A/B") < 0) TEST_ERROR

    /* Move group "/g4/A/B" to "/g4/F/B" using relative name */
    if(H5Lmove(group_id, "A", group_id, "F", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group3_id, "/g4/F/B", "/g4/F/B") < 0) TEST_ERROR

    /* Verify */
    if(check_name(group2_id, "/g4/F", "/g4/F") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group4_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group5_id) < 0) FAIL_STACK_ERROR

    PASSED();


   /*-------------------------------------------------------------------------
    * Test H5Iget_name with H5Lmove and a long path #2
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with H5Lmove and a long path #2");

    /* Create group "g6/A/B" and "g7" */
    if((group_id = H5Gcreate2(file_id, "g6", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file_id, "g6/A", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group3_id = H5Gcreate2(file_id, "g6/A/B", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group4_id = H5Gcreate2(file_id, "g7", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group3_id, "/g6/A/B", "/g6/A/B") < 0) TEST_ERROR

    /* Move group "A" to "C"*/
    if(H5Lmove(file_id, "/g6/A", H5L_SAME_LOC, "/g7/C", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group2_id, "/g7/C", "/g7/C") < 0) TEST_ERROR

    /* Verify */
    if(check_name(group3_id, "/g7/C/B", "/g7/C/B") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group4_id) < 0) FAIL_STACK_ERROR

    PASSED();


   /*-------------------------------------------------------------------------
    * Test H5Iget_name with H5Ldelete
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with H5Ldelete");

    /* Create a new group. */
    if((group_id = H5Gcreate2(file_id, "/g8", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Delete */
    if(H5Ldelete(file_id, "/g8", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group_id, "", "") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR

    PASSED();


   /*-------------------------------------------------------------------------
    * Test H5Iget_name with H5Ldelete and a long path
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with H5Ldelete and a long path");

    /* Create group "g9/a/b" */
    if((group_id = H5Gcreate2(file_id, "g9", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file_id, "g9/a", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group3_id = H5Gcreate2(file_id, "g9/a/b", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Delete */
    if(H5Ldelete(file_id, "/g9/a", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group2_id, "", "") < 0) TEST_ERROR

    /* Verify */
    if(check_name(group3_id, "", "") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR

    /* Recreate groups */
    if((group2_id = H5Gcreate2(group_id, "a", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group3_id = H5Gcreate2(group_id, "a/b", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Delete, using relative path */
    if(H5Ldelete(group_id, "a", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group2_id, "", "") < 0) TEST_ERROR

    /* Verify */
    if(check_name(group3_id, "", "") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR

    /* Create group "g10/a/b" */
    if((group_id = H5Gcreate2(file_id, "g10", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file_id, "g10/a", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group3_id = H5Gcreate2(file_id, "g10/a/b", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Delete */
    if(H5Ldelete(file_id, "/g10/a/b", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group3_id, "", "") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR

    /* Recreate group */
    if((group3_id = H5Gcreate2(group_id, "a/b", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Delete, using relative path */
    if(H5Ldelete(group_id, "a/b", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group3_id, "", "") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR

    PASSED();


   /*-------------------------------------------------------------------------
    * Test H5Iget_name with H5Ldelete, same names
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with H5Ldelete, same names");

    /* Create group "g11/g" */
    if((group_id = H5Gcreate2(file_id, "g11", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file_id, "g11/g", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create two datasets "g11/d" and "g11/g/d"*/
    if((space_id = H5Screate_simple(1, dims, NULL)) < 0) FAIL_STACK_ERROR
    if((dataset_id = H5Dcreate2(group_id , "d", H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((dataset2_id = H5Dcreate2(group2_id , "d", H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Delete */
    if(H5Ldelete(file_id, "/g11/d", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(dataset_id, "", "") < 0) TEST_ERROR

    /* Verify */
    if(check_name(dataset2_id, "/g11/g/d", "/g11/g/d") < 0) TEST_ERROR

    /* Close */
    if(H5Dclose(dataset_id) < 0) FAIL_STACK_ERROR
    if(H5Dclose(dataset2_id) < 0) FAIL_STACK_ERROR
    if(H5Sclose(space_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR

    PASSED();


   /*-------------------------------------------------------------------------
    * Test H5Iget_name with H5Fmount; with IDs on the list
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with H5Fmount; with IDs on the list");

    /* Create a group "g12" in the first file */
    if((group_id = H5Gcreate2(file_id, "/g12", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR

    /* Create second file and dataset "d" in it */
    if((file1_id = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create a dataspace  */
    if((space_id = H5Screate_simple(1, dims, NULL)) < 0) TEST_ERROR

    /* Create the dataset */
    if((dataset_id = H5Dcreate2(file1_id , "d", H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close */
    if(H5Dclose(dataset_id) < 0) FAIL_STACK_ERROR

    /* Mount second file under "g12" in the first file */
    if(H5Fmount(file_id, "/g12", file1_id, H5P_DEFAULT) < 0) TEST_ERROR

    /* Access dataset D in the first file under "/G/D" name */
    if((dataset_id = H5Dopen2(file_id, "/g12/d", H5P_DEFAULT)) < 0) TEST_ERROR

    /* Verify */
    if(check_name(dataset_id, "/g12/d", "/g12/d") < 0) TEST_ERROR

    if(H5Funmount(file_id, "/g12") < 0) TEST_ERROR

    /* Close */
    if(H5Dclose(dataset_id) < 0) FAIL_STACK_ERROR
    if(H5Fclose(file1_id) < 0) FAIL_STACK_ERROR
    if(H5Sclose(space_id) < 0) FAIL_STACK_ERROR


    PASSED();


    /*-------------------------------------------------------------------------
     * Test H5Iget_name with H5Fmount; long name
     *-------------------------------------------------------------------------
     */

    TESTING("H5Iget_name with H5Fmount; long name");

    /* Create a group "g13/g1/g2" in the first file */
    if((group_id = H5Gcreate2(file_id, "/g13", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file_id, "/g13/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group3_id = H5Gcreate2(file_id, "/g13/g1/g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR

    /* Create second file and group "g" in it */
    file1_id = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

    if((group_id = H5Gcreate2(file1_id, "/g14", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file1_id, "/g14/g3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group3_id = H5Gcreate2(file1_id, "/g14/g3/g4", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR

    /* Mount second file under "/g13/g1" in the first file */
    if(H5Fmount(file_id, "/g13/g1", file1_id, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Access group in the first file */
    if((group_id = H5Gopen2(file_id, "/g13/g1/g14/g3/g4", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group_id, "/g13/g1/g14/g3/g4", "/g13/g1/g14/g3/g4") < 0) TEST_ERROR

    if(H5Funmount(file_id, "/g13/g1") < 0) FAIL_STACK_ERROR


    /* Verify */
    if(check_name(group_id, "/g14/g3/g4", "") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR


    /* Access group in the file to mount */
    if((group3_id = H5Gopen2(file1_id, "/g14/g3/g4", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Mount second file under "/g13/g1" in the first file(again) */
    if(H5Fmount(file_id, "/g13/g1", file1_id, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Get a group ID for the parent of the newly mounted group */
    if((group2_id = H5Gopen2(file_id, "/g13", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Access group in the first file */
    if((group_id = H5Gopen2(file_id, "/g13/g1/g14/g3/g4", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group_id, "/g13/g1/g14/g3/g4", "/g13/g1/g14/g3/g4") < 0) TEST_ERROR
    if(check_name(group3_id, "/g14/g3/g4", "/g14/g3/g4") < 0) TEST_ERROR

    if(H5Funmount(group2_id, "g1") < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group_id, "/g14/g3/g4", "") < 0) TEST_ERROR
    if(check_name(group3_id, "/g14/g3/g4", "/g14/g3/g4") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR

    /* Mount second file under "/g13/g1" in the first file(again) */
    if(H5Fmount(file_id, "/g13/g1", file1_id, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Get a group ID for the newly mounted group */
    if((group2_id = H5Gopen2(file_id, "/g13/g1", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Access group in the first file */
    if((group_id = H5Gopen2(file_id, "/g13/g1/g14/g3/g4", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group_id, "/g13/g1/g14/g3/g4", "/g13/g1/g14/g3/g4") < 0) TEST_ERROR
    if(check_name(group2_id, "/g13/g1", "/g13/g1") < 0) TEST_ERROR

    if(H5Funmount(group2_id, ".") < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group_id, "/g14/g3/g4", "") < 0) TEST_ERROR
    if(check_name(group2_id, "/", "") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR

    /* Mount second file under "/g13/g1" in the first file, using relative path */

    if((group3_id = H5Gopen2(file_id, "/g13", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group3_id, "/g13", "/g13") < 0) TEST_ERROR

    if(H5Fmount(group3_id, "g1", file1_id, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Get a group ID for the newly mounted group */
    if((group2_id = H5Gopen2(file_id, "/g13/g1", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group2_id, "/g13/g1", "/g13/g1") < 0) TEST_ERROR

    /* Access group in the first file */
    if((group_id = H5Gopen2(file_id, "/g13/g1/g14/g3/g4", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group_id, "/g13/g1/g14/g3/g4", "/g13/g1/g14/g3/g4") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR

    /* Access group in the first file, with relative path */
    if((group_id = H5Gopen2(group2_id, "g14/g3/g4", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group_id, "/g13/g1/g14/g3/g4", "/g13/g1/g14/g3/g4") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR

    if(H5Funmount(group2_id, ".") < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group2_id, "/", "") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR

    /* Mount second file under "/g13/g1" in the first file, using relative path */

    if((group3_id = H5Gopen2(file_id, "/g13/g1", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group3_id, "/g13/g1", "/g13/g1") < 0) TEST_ERROR

    if(H5Fmount(group3_id, ".", file1_id, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Get a group ID for the newly mounted group */
    if((group2_id = H5Gopen2(file_id, "/g13/g1", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group2_id, "/g13/g1", "/g13/g1") < 0) TEST_ERROR

    /* Access group in the first file */
    if((group_id = H5Gopen2(file_id, "/g13/g1/g14/g3/g4", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group_id, "/g13/g1/g14/g3/g4", "/g13/g1/g14/g3/g4") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR

    /* Access group in the first file, with relative path */
    if((group_id = H5Gopen2(group2_id, "g14/g3/g4", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group_id, "/g13/g1/g14/g3/g4", "/g13/g1/g14/g3/g4") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR

    if(H5Funmount(group2_id, ".") < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group2_id, "/", "") < 0) TEST_ERROR
    if(check_name(group3_id, "/g13/g1", "/g13/g1") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR

    if(H5Fclose(file1_id) < 0) FAIL_STACK_ERROR


    PASSED();


/*-------------------------------------------------------------------------
    * Test H5Iget_name with H5Funmount
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with H5Funmount");

    /* Create a group "g15/g1/g2" in the first file */
    if((group_id = H5Gcreate2(file_id, "/g15", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file_id, "/g15/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group3_id = H5Gcreate2(file_id, "/g15/g1/g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group4_id = H5Gcreate2(file_id, "/g15/g1/g2/g3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close */
    H5Gclose(group_id);
    H5Gclose(group2_id);
    H5Gclose(group3_id);
    H5Gclose(group4_id);

    /* Create second file and group "g" in it */
    file1_id = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

    if((group_id = H5Gcreate2(file1_id, "/g16", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file1_id, "/g16/g4", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group3_id = H5Gcreate2(file1_id, "/g16/g4/g5", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR

    /* Access group in the first file */
    if((group_id = H5Gopen2(file_id, "/g15/g1/g2/g3", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Mount second file under "/g13/g1" in the first file */
    if(H5Fmount(file_id, "/g15/g1", file1_id, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Access group in the second file */
    if((group2_id = H5Gopen2(file_id, "/g15/g1/g16/g4/g5", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group_id, "", "/g15/g1/g2/g3") < 0) TEST_ERROR

    /* Verify */
    if(check_name(group2_id, "/g15/g1/g16/g4/g5", "/g15/g1/g16/g4/g5") < 0) TEST_ERROR

    if(H5Funmount(file_id, "/g15/g1") < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group_id, "/g15/g1/g2/g3", "/g15/g1/g2/g3") < 0) TEST_ERROR

    /* Verify */
    if(check_name(group2_id, "/g16/g4/g5", "") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Fclose(file1_id) < 0) FAIL_STACK_ERROR


    PASSED();


    /*-------------------------------------------------------------------------
     * Test H5Iget_name with a defined type dataset
     *-------------------------------------------------------------------------
     */

    TESTING("H5Iget_name with a defined type dataset");

    /* Create a datatype */
    if((type_id = H5Tcreate(H5T_COMPOUND, sizeof(s1_t))) < 0) FAIL_STACK_ERROR

    /* Insert fields */
    if(H5Tinsert(type_id, "a", HOFFSET(s1_t, a), H5T_NATIVE_INT) < 0) FAIL_STACK_ERROR
    if(H5Tinsert(type_id, "b", HOFFSET(s1_t, b), H5T_NATIVE_INT) < 0) FAIL_STACK_ERROR
    if(H5Tinsert(type_id, "c", HOFFSET(s1_t, c), H5T_NATIVE_FLOAT) < 0) FAIL_STACK_ERROR

    /* Create group "g17" */
    if((group_id = H5Gcreate2(file_id, "g17", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Save datatype for later */
    if(H5Tcommit2(group_id, "t", type_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Create a dataspace  */
    if((space_id = H5Screate_simple(1, dims, NULL)) < 0) FAIL_STACK_ERROR

    /* Create a new dataset */
    if((dataset_id = H5Dcreate2(group_id , "d", type_id, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close */
    if(H5Dclose(dataset_id) < 0) FAIL_STACK_ERROR
    if(H5Tclose(type_id) < 0) FAIL_STACK_ERROR
    if(H5Sclose(space_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR

    /* Open the named datatype */
    if((type_id = H5Topen2(file_id, "/g17/t", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(type_id, "/g17/t", "/g17/t") < 0) TEST_ERROR

    /* Close datatype */
    if(H5Tclose(type_id) < 0) FAIL_STACK_ERROR

    /* Reopen the dataset */
    if((dataset_id = H5Dopen2(file_id, "/g17/d", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Get datatype*/
    if((type_id = H5Dget_type(dataset_id)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(type_id, "/g17/t", "/g17/t") < 0) TEST_ERROR

    /* Close */
    if(H5Dclose(dataset_id) < 0) FAIL_STACK_ERROR
    if(H5Tclose(type_id) < 0) FAIL_STACK_ERROR

    PASSED();


   /*-------------------------------------------------------------------------
    * Test H5Iget_name with objects that have two names
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with datasets that have two names");

    /* Open dataset named "d"*/
    if((dataset_id = H5Dopen2(file_id, "/g17/d", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create link to dataset named "link" */
    if(H5Lcreate_hard(dataset_id, ".", file_id, "/g17/link", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if((dataset2_id = H5Dopen2(file_id, "/g17/link", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Make sure that the two IDs use two different names */
    if(check_name(dataset_id, "/g17/d", "/g17/d") < 0) TEST_ERROR
    if(check_name(dataset2_id, "/g17/link", "/g17/link") < 0) TEST_ERROR

    if(H5Dclose(dataset_id) < 0) FAIL_STACK_ERROR
    if(H5Dclose(dataset2_id) < 0) FAIL_STACK_ERROR

    PASSED();


   /*-------------------------------------------------------------------------
    * Test H5Iget_name with different files, test1
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with different files");

    /* Create a new file using default properties. */
    if((file2_id = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create a new file using default properties. */
    if((file3_id = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create the dataspace  */
    if((space_id = H5Screate_simple(1, dims, NULL)) < 0) FAIL_STACK_ERROR

    /* Create a new dataset */
    if((dataset_id = H5Dcreate2(file2_id , "d", H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create a new dataset */
    if((dataset2_id = H5Dcreate2(file3_id , "d", H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Delete */
    if(H5Ldelete(file2_id, "/d", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(dataset_id, "", "") < 0) TEST_ERROR

    /* Verify */
    if(check_name(dataset2_id, "/d", "/d") < 0) TEST_ERROR

    /* Close */
    if(H5Dclose(dataset_id) < 0) FAIL_STACK_ERROR
    if(H5Dclose(dataset2_id) < 0) FAIL_STACK_ERROR
    if(H5Sclose(space_id) < 0) FAIL_STACK_ERROR
    if(H5Fclose(file2_id) < 0) FAIL_STACK_ERROR
    if(H5Fclose(file3_id) < 0) FAIL_STACK_ERROR

    PASSED();


   /*-------------------------------------------------------------------------
    * Test H5Iget_name with different files, test2
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with different files #2");

    /* Create a new file using default properties. */
    if((file2_id = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create a new file using default properties. */
    if((file3_id = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create the dataspace  */
    if((space_id = H5Screate_simple(1, dims, NULL)) < 0) FAIL_STACK_ERROR

    /* Create a new dataset */
    if((dataset_id = H5Dcreate2(file2_id , "d", H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create a new dataset */
    if((dataset2_id = H5Dcreate2(file3_id , "d", H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Delete */
    if(H5Ldelete(file3_id, "/d", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(dataset_id, "/d", "/d") < 0) TEST_ERROR

    /* Verify */
    if(check_name(dataset2_id, "", "") < 0) TEST_ERROR

    /* Close */
    if(H5Dclose(dataset_id) < 0) FAIL_STACK_ERROR
    if(H5Dclose(dataset2_id) < 0) FAIL_STACK_ERROR
    if(H5Sclose(space_id) < 0) FAIL_STACK_ERROR
    if(H5Fclose(file2_id) < 0) FAIL_STACK_ERROR
    if(H5Fclose(file3_id) < 0) FAIL_STACK_ERROR

    PASSED();


   /*-------------------------------------------------------------------------
    * Test H5Iget_name with a small buffer for name
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with a small buffer for name");

    /* Reopen the group */
    if((group_id = H5Gopen2(file_id, "/g17", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

{
    /*small buffer to hold name and its size */
    char    name2[SMALL_NAME_BUF_SIZE];

    /* Get name */
    *name2 = '\0';
    name_len=(size_t)H5Iget_name(group_id, name2, SMALL_NAME_BUF_SIZE);

    /* Check that name is longer */
    if(name_len <= SMALL_NAME_BUF_SIZE) TEST_ERROR
    if(HDstrcmp(name2, "/")) TEST_ERROR
}

    /* Verify */
    if(check_name(group_id, "/g17", "/g17") < 0) TEST_ERROR

    /* Close */
    H5Gclose(group_id);

    PASSED();


    /*-------------------------------------------------------------------------
    * Test H5Iget_name with a dynamic buffer for name
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with a dynamic buffer for name");

    /* Reopen the group */
    if((group_id = H5Gopen2(file_id, "/g17", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Get name */
    name_len = (size_t)H5Iget_name(group_id, NULL, NAME_BUF_SIZE);

{
    /* dynamic buffer to hold name */
    char    *name3;

    /* Include the extra null character */
    name3 = (char *)HDmalloc(name_len + 1);
    if(!name3) TEST_ERROR

    /* Get name with dynamic buffer */
    *name3 = '\0';
    if(H5Iget_name(group_id, name3, name_len + 1) < 0) TEST_ERROR

    /* Verify */
    if(HDstrcmp(name3, "/g17")) TEST_ERROR
    *name3 = '\0';

    /* Get name with smaller buffer */
    *name3 = '\0';
    if(H5Iget_name(group_id, name3, 3) < 0) TEST_ERROR

    /* Verify */
    if(HDstrcmp(name3, "/g")) TEST_ERROR

    HDfree(name3);
}

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR

    PASSED();


/*-------------------------------------------------------------------------
    * Test H5Iget_name with invalid IDs
    *-------------------------------------------------------------------------
    */


    TESTING("H5Iget_name with invalid IDs");

    /* Create a dataspace  */
    if((space_id = H5Screate_simple(1, dims, NULL)) < 0) TEST_ERROR

    /* Define a datatype */
    if((type_id = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR

    /* Create a new dataset */
    if((dataset_id = H5Dcreate2(file_id , "d2", type_id, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

{
    char name[NAME_BUF_SIZE];   /* Buffer to hold name and its size */

    /* Get name for non commited datatype, it should fail */
    H5E_BEGIN_TRY {
        if(H5Iget_name(type_id, name, NAME_BUF_SIZE) > 0) TEST_ERROR
    } H5E_END_TRY;

    /* Get name for dataspace, it should fail */
    H5E_BEGIN_TRY {
        if(H5Iget_name(space_id, name, NAME_BUF_SIZE) > 0) TEST_ERROR
    } H5E_END_TRY;
}

    /* Close */
    if(H5Dclose(dataset_id) < 0) FAIL_STACK_ERROR
    if(H5Sclose(space_id) < 0) FAIL_STACK_ERROR
    if(H5Tclose(type_id) < 0) FAIL_STACK_ERROR

    PASSED();


    /*-------------------------------------------------------------------------
     * Test H5Iget_name with added names with mounting
     *-------------------------------------------------------------------------
     */

    TESTING("H5Iget_name with added names with mounting");

    /* Create a group "g18/g2" in the first file */
    if((group_id = H5Gcreate2(file_id, "/g18", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file_id, "/g18/g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Also create a dataset and a datatype */
    if((space_id = H5Screate_simple(1, dims, NULL)) < 0) FAIL_STACK_ERROR
    if((type_id = H5Tcopy(H5T_NATIVE_INT)) < 0) FAIL_STACK_ERROR
    if((dataset_id = H5Dcreate2(file_id, "g18/d2", type_id, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    if(H5Tcommit2(file_id, "g18/t2", type_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Create second file and group "/g3/g4/g5" in it */
    if((file1_id = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR
    if((group3_id = H5Gcreate2(file1_id, "/g3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group4_id = H5Gcreate2(file1_id, "/g3/g4", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group5_id = H5Gcreate2(file1_id, "/g3/g4/g5", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Mount first file at "g3/g4" in the second file */
    if(H5Fmount(file1_id, "/g3/g4", file_id, H5P_DEFAULT) < 0) TEST_ERROR

    /* Get name for the group ID in the first file, should be "/g18/g2" still */
    if(check_name(group2_id, "/g18/g2", "/g18/g2") < 0) TEST_ERROR

    /* Get name for the dataset ID in the first file, should be "/g18/g2/d2" still */
    if(check_name(dataset_id, "/g18/d2", "/g18/d2") < 0) TEST_ERROR

    /* Get name for the datatype ID in the first file, should be "/g18/g2/t2" still */
    if(check_name(type_id, "/g18/t2", "/g18/t2") < 0) TEST_ERROR

    /* Open the mounted group, dataset, and datatype through their new names */
    if((group6_id = H5Gopen2(file1_id, "/g3/g4/g18/g2", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((dataset2_id = H5Dopen2(file1_id, "/g3/g4/g18/d2", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((type2_id = H5Topen2(file1_id, "/g3/g4/g18/t2", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify names */
    if(check_name(group6_id, "/g3/g4/g18/g2", "/g3/g4/g18/g2") < 0) TEST_ERROR
    if(check_name(dataset2_id, "/g3/g4/g18/d2", "/g3/g4/g18/d2") < 0) TEST_ERROR
    if(check_name(type2_id, "/g3/g4/g18/t2", "/g3/g4/g18/t2") < 0) TEST_ERROR

    /* Verify that old IDs still refer to objects by their old names */
    if(check_name(group2_id, "/g18/g2", "/g18/g2") < 0) TEST_ERROR
    if(check_name(dataset_id, "/g18/d2", "/g18/d2") < 0) TEST_ERROR
    if(check_name(type_id, "/g18/t2", "/g18/t2") < 0) TEST_ERROR

    /* Unmount */
    if(H5Funmount(file1_id, "/g3/g4") < 0) FAIL_STACK_ERROR

    /* Get name for the IDs of the first file, should be unchanged */
    if(check_name(group2_id, "/g18/g2", "/g18/g2") < 0) TEST_ERROR
    if(check_name(dataset_id, "/g18/d2", "/g18/d2") < 0) TEST_ERROR
    if(check_name(type_id, "/g18/t2", "/g18/t2") < 0) TEST_ERROR

    /* Get name for the IDs of the second file, should be local names now */
    if(check_name(group6_id, "/g18/g2", "") < 0) TEST_ERROR
    if(check_name(dataset2_id, "/g18/d2", "") < 0) TEST_ERROR
    if(check_name(type2_id, "/g18/t2", "") < 0) TEST_ERROR

    if(H5Tclose(type_id) < 0) FAIL_STACK_ERROR
    if(H5Tclose(type2_id) < 0) FAIL_STACK_ERROR
    if(H5Dclose(dataset_id) < 0) FAIL_STACK_ERROR
    if(H5Dclose(dataset2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group4_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group5_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group6_id) < 0) FAIL_STACK_ERROR
    if(H5Fclose(file1_id) < 0) FAIL_STACK_ERROR

    PASSED();


   /*-------------------------------------------------------------------------
    * Test H5Iget_name with H5Fclose
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with H5Fclose");

    /* Create a file and group "/g1/g2" in it */
    if((file1_id = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR
    if((group_id = H5Gcreate2(file1_id, "/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file1_id, "/g1/g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group2_id, "/g1/g2", "/g1/g2") < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(file1_id) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group2_id, "/g1/g2", "/g1/g2") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR

    PASSED();


   /*-------------------------------------------------------------------------
    * Test H5Iget_name with H5Fmount and H5Ldelete
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with H5Fmount and H5Ldelete");

    /* Create a file and group "/g1/g2" in it */
    if((file1_id = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR
    if((group_id = H5Gcreate2(file1_id, "/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file1_id, "/g1/g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create a new file and group "/g3/g4" in it */
    if((file2_id = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR
    if((group3_id = H5Gcreate2(file2_id, "/g3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group4_id = H5Gcreate2(file2_id, "/g3/g4", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Mount first file at "/g3/g4" in the second file */
    if(H5Fmount(file2_id, "/g3/g4", file1_id, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Open the mounted group */
    if((group5_id = H5Gopen2(file2_id, "/g3/g4/g1/g2", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group5_id, "/g3/g4/g1/g2", "/g3/g4/g1/g2") < 0) TEST_ERROR

    /* Delete */
    if(H5Ldelete(file1_id, "/g3/g4/g1/g2", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group5_id, "", "") < 0) TEST_ERROR

    /* Verify */
    if(check_name(group2_id, "", "") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group4_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group5_id) < 0) FAIL_STACK_ERROR
    if(H5Fclose(file1_id) < 0) FAIL_STACK_ERROR
    if(H5Fclose(file2_id) < 0) FAIL_STACK_ERROR

    PASSED();


   /*-------------------------------------------------------------------------
    * Test H5Iget_name with H5Fmount and H5Lmove
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with H5Fmount and H5Lmove");

    /* Create a file and group "/g1/g2" in it */
    if((file1_id = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR
    if((group_id = H5Gcreate2(file1_id, "/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file1_id, "/g1/g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create a new file and group "/g3/g4" in it */
    if((file2_id = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR
    if((group3_id = H5Gcreate2(file2_id, "/g3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group4_id = H5Gcreate2(file2_id, "/g3/g4", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Mount first file at "g3/g4" in the second file */
    if(H5Fmount(file2_id, "/g3/g4", file1_id, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group4_id, "/g3/g4", "/g3/g4") < 0) TEST_ERROR

    /* Open the mounted group */
    if((group5_id = H5Gopen2(file2_id, "/g3/g4/g1/g2", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group5_id, "/g3/g4/g1/g2", "/g3/g4/g1/g2") < 0) TEST_ERROR

    /* Open another mounted group, in the middle of the path */
    if((group6_id = H5Gopen2(file2_id, "/g3/g4/g1", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group6_id, "/g3/g4/g1", "/g3/g4/g1") < 0) TEST_ERROR

    /* Rename group */
    if(H5Lmove(file2_id, "/g3/g4/g1/g2", H5L_SAME_LOC, "/g3/g4/g1/g5", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group5_id, "/g3/g4/g1/g5", "/g3/g4/g1/g5") < 0) TEST_ERROR
    if(check_name(group2_id, "/g1/g5", "/g1/g5") < 0) TEST_ERROR

    /* Rename group */
    if(H5Lmove(file2_id, "/g3/g4/g1", H5L_SAME_LOC, "/g3/g4/g1a", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group5_id, "/g3/g4/g1a/g5", "/g3/g4/g1a/g5") < 0) TEST_ERROR
    if(check_name(group2_id, "/g1a/g5", "/g1a/g5") < 0) TEST_ERROR

    /* Verify */
    if(check_name(group6_id, "/g3/g4/g1a", "/g3/g4/g1a") < 0) TEST_ERROR
    if(check_name(group_id, "/g1a", "/g1a") < 0) TEST_ERROR

    /* Rename middle group back, using relative path */
    if(H5Lmove(group3_id, "g4/g1a", H5L_SAME_LOC, "g4/g1", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group5_id, "/g3/g4/g1/g5", "/g3/g4/g1/g5") < 0) TEST_ERROR
    if(check_name(group2_id, "/g1/g5", "/g1/g5") < 0) TEST_ERROR
    if(check_name(group6_id, "/g3/g4/g1", "/g3/g4/g1") < 0) TEST_ERROR
    if(check_name(group_id, "/g1", "/g1") < 0) TEST_ERROR

    /* Rename end group back, using relative path */
    if(H5Lmove(group3_id, "g4/g1/g5", H5L_SAME_LOC, "g4/g1/g2", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group5_id, "/g3/g4/g1/g2", "/g3/g4/g1/g2") < 0) TEST_ERROR
    if(check_name(group2_id, "/g1/g2", "/g1/g2") < 0) TEST_ERROR
    if(check_name(group6_id, "/g3/g4/g1", "/g3/g4/g1") < 0) TEST_ERROR
    if(check_name(group_id, "/g1", "/g1") < 0) TEST_ERROR

    /* Rename mount point */
    if(H5Lmove(file2_id, "/g3/g4", H5L_SAME_LOC, "/g3/g4a", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group4_id, "/g3/g4a", "/g3/g4a") < 0) TEST_ERROR
    if(check_name(group5_id, "/g3/g4a/g1/g2", "/g3/g4a/g1/g2") < 0) TEST_ERROR
    if(check_name(group6_id, "/g3/g4a/g1", "/g3/g4a/g1") < 0) TEST_ERROR

    /* Rename mount point back, using relative path*/
    if(H5Lmove(group3_id, "g4a", H5L_SAME_LOC, "g4", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group4_id, "/g3/g4", "/g3/g4") < 0) TEST_ERROR
    if(check_name(group5_id, "/g3/g4/g1/g2", "/g3/g4/g1/g2") < 0) TEST_ERROR
    if(check_name(group6_id, "/g3/g4/g1", "/g3/g4/g1") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group4_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group5_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group6_id) < 0) FAIL_STACK_ERROR
    if(H5Fclose(file1_id) < 0) FAIL_STACK_ERROR
    if(H5Fclose(file2_id) < 0) FAIL_STACK_ERROR

    PASSED();


   /*-------------------------------------------------------------------------
    * Test H5Iget_name with H5Lcreate_hard
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with H5Lcreate_hard");

    /* Create group "g19/g1" */
    if((group_id = H5Gcreate2(file_id, "/g19", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file_id, "/g19/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create hard link to "g19/g1/ group */
    if(H5Lcreate_hard(file_id, "/g19/g1", H5L_SAME_LOC, "/g19/g2", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group2_id, "/g19/g1", "/g19/g1") < 0) TEST_ERROR

    /* Open the group */
    if((group3_id = H5Gopen2(file_id, "/g19/g2", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group3_id, "/g19/g2", "/g19/g2") < 0) TEST_ERROR

    /* Rename original group */
    if(H5Lmove(file_id, "/g19/g1", H5L_SAME_LOC, "/g19/g3", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group2_id, "/g19/g3", "/g19/g3") < 0) TEST_ERROR
    if(check_name(group3_id, "/g19/g2", "/g19/g2") < 0) TEST_ERROR

    /* Rename original group back, using relative path */
    if(H5Lmove(group_id, "g3", H5L_SAME_LOC, "g1", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group2_id, "/g19/g1", "/g19/g1") < 0) TEST_ERROR
    if(check_name(group3_id, "/g19/g2", "/g19/g2") < 0) TEST_ERROR

    /* Create another hard link to "/g19/g1" group */
    if(H5Lcreate_hard(file_id, "/g19/g1", H5L_SAME_LOC, "/g19/g3", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Open the group */
    if((group4_id = H5Gopen2(file_id, "/g19/g3", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group4_id, "/g19/g3", "/g19/g3") < 0) TEST_ERROR

    /* Delete group */
    if(H5Ldelete(file_id, "/g19/g3", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group4_id, "/g19/g1", "") < 0) TEST_ERROR
    if(check_name(group2_id, "/g19/g1", "/g19/g1") < 0) TEST_ERROR
    if(check_name(group3_id, "/g19/g2", "/g19/g2") < 0) TEST_ERROR

    /* Close the unlinked group */
    if(H5Gclose(group4_id) < 0) FAIL_STACK_ERROR

    /* Create another hard link to "/g19/g1" group */
    if(H5Lcreate_hard(file_id, "/g19/g1", H5L_SAME_LOC, "/g19/g3", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Open the group */
    if((group4_id = H5Gopen2(file_id, "/g19/g3", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group4_id, "/g19/g3", "/g19/g3") < 0) TEST_ERROR

    /* Delete group, using relative path */
    if(H5Ldelete(group_id, "g3", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group4_id, "/g19/g1", "") < 0) TEST_ERROR
    if(check_name(group2_id, "/g19/g1", "/g19/g1") < 0) TEST_ERROR
    if(check_name(group3_id, "/g19/g2", "/g19/g2") < 0) TEST_ERROR

    /* Close the unlinked group */
    if(H5Gclose(group4_id) < 0) FAIL_STACK_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR

    PASSED();


   /*-------------------------------------------------------------------------
    * Test H5Iget_name with H5Lcreate_soft
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with H5Lcreate_soft");

    /* Create group "g20/g1" */
    if((group_id = H5Gcreate2(file_id, "/g20", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file_id, "/g20/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create symbolic link to "g20/g1/ group */
    if(H5Lcreate_soft("/g20/g1", file_id, "/g20/g2", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group2_id, "/g20/g1", "/g20/g1") < 0) TEST_ERROR

    /* Open the group */
    if((group3_id = H5Gopen2(file_id, "/g20/g2", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group3_id, "/g20/g2", "/g20/g2") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR

    PASSED();


   /*-------------------------------------------------------------------------
    * Test H5Iget_name with H5Lcreate_soft and move target
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with H5Lcreate_soft and move target");

    /* Create group "g21/g1" */
    if((group_id = H5Gcreate2(file_id, "/g21", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file_id, "/g21/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create symbolic link to "g21/g1/ group */
    if(H5Lcreate_soft("/g21/g1", file_id, "/g21/g2", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group2_id, "/g21/g1", "/g21/g1") < 0) TEST_ERROR

    /* Open the group */
    if((group3_id = H5Gopen2(file_id, "/g21/g2", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Rename group */
    if(H5Lmove(file_id, "/g21/g1", H5L_SAME_LOC, "/g21/g3", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group2_id, "/g21/g3", "/g21/g3") < 0) TEST_ERROR
    if(check_name(group3_id, "/g21/g2", "/g21/g2") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR

    PASSED();


   /*-------------------------------------------------------------------------
    * Test H5Iget_name with H5Lcreate_soft and move source
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with H5Lcreate_soft and move source");

    /* Create group "g22/g1" */
    if((group_id = H5Gcreate2(file_id, "/g22", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file_id, "/g22/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create symbolic link to "g22/g1/ group */
    if(H5Lcreate_soft("/g22/g1", file_id, "/g22/g2", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group2_id, "/g22/g1", "/g22/g1") < 0) TEST_ERROR

    /* Open the group */
    if((group3_id = H5Gopen2(file_id, "/g22/g2", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Rename soft link */
    if(H5Lmove(file_id, "/g22/g2", H5L_SAME_LOC, "/g22/g3", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group2_id, "/g22/g1", "/g22/g1") < 0) TEST_ERROR
    if(check_name(group3_id, "/g22/g3", "/g22/g3") < 0) TEST_ERROR

    /* Rename soft link, using relative paths */
    if(H5Lmove(group_id, "g3", H5L_SAME_LOC, "g2", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group2_id, "/g22/g1", "/g22/g1") < 0) TEST_ERROR
    if(check_name(group3_id, "/g22/g2", "/g22/g2") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR

    PASSED();



   /*-------------------------------------------------------------------------
    * Test H5Iget_name with H5Lcreate_soft and unlink target
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with H5Lcreate_soft and unlink target");

    /* Create group "g23/g1" */
    if((group_id = H5Gcreate2(file_id, "/g23", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file_id, "/g23/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create symbolic link to "g23/g1/ group */
    if(H5Lcreate_soft("/g23/g1", file_id, "/g23/g2", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group2_id, "/g23/g1", "/g23/g1") < 0) TEST_ERROR

    /* Open the group */
    if((group3_id = H5Gopen2(file_id, "/g23/g2", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Delete group */
    if(H5Ldelete(file_id, "/g23/g1", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group3_id, "/g23/g2", "/g23/g2") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR

    PASSED();


   /*-------------------------------------------------------------------------
    * Test H5Iget_name with H5Lcreate_soft and unlink source
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with H5Lcreate_soft and unlink source");

    /* Create group "g24/g1" */
    if((group_id = H5Gcreate2(file_id, "/g24", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file_id, "/g24/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create symbolic link to "g24/g1/ group */
    if(H5Lcreate_soft("/g24/g1", file_id, "/g24/g2", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group2_id, "/g24/g1", "/g24/g1") < 0) TEST_ERROR

    /* Open the group */
    if((group3_id = H5Gopen2(file_id, "/g24/g2", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Delete symbolic link */
    if(H5Ldelete(file_id, "/g24/g2", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group3_id, "/g24/g1", "") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR

    PASSED();


   /*-------------------------------------------------------------------------
    * Test H5Iget_name with several nested mounted files
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with several nested mounted files");

    /* Create a group "g25/g1/g2" in the first file */
    if((group_id = H5Gcreate2(file_id, "/g25", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file_id, "/g25/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group3_id = H5Gcreate2(file_id, "/g25/g1/g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR

    /* Create second file and group "/g26/g3/g4" in it */
    if((file1_id = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    if((group_id = H5Gcreate2(file1_id, "/g26", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file1_id, "/g26/g3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group3_id = H5Gcreate2(file1_id, "/g26/g3/g4", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR

    /* Create third file and group "/g27/g5/g6" in it */
    if((file2_id = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    if((group_id = H5Gcreate2(file2_id, "/g27", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file2_id, "/g27/g5", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group3_id = H5Gcreate2(file2_id, "/g27/g5/g6", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR

    /* Create fourth file and group "/g28/g5/g6" in it */
    if((file3_id = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    if((group_id = H5Gcreate2(file3_id, "/g28", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file3_id, "/g28/g7", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group3_id = H5Gcreate2(file3_id, "/g28/g7/g8", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR

    /* Access group which will be hidden in the first file */
    if((group_id = H5Gopen2(file_id, "/g25/g1/g2", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group_id, "/g25/g1/g2", "/g25/g1/g2") < 0) TEST_ERROR

    /* Mount second file under "/g25/g1" in the first file */
    if(H5Fmount(file_id, "/g25/g1", file1_id, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group_id, "", "/g25/g1/g2") < 0) TEST_ERROR

    /* Access group which will be hidden in the second file */
    if((group2_id = H5Gopen2(file_id, "/g25/g1/g26/g3/g4", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group2_id, "/g25/g1/g26/g3/g4", "/g25/g1/g26/g3/g4") < 0) TEST_ERROR

    /* Mount third file under "/g25/g1/g26/g3" in the first file */
    if(H5Fmount(file_id, "/g25/g1/g26/g3", file2_id, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group2_id, "", "/g25/g1/g26/g3/g4") < 0) TEST_ERROR

    /* Access group in the third file */
    if((group3_id = H5Gopen2(file_id, "/g25/g1/g26/g3/g27/g5/g6", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group3_id, "/g25/g1/g26/g3/g27/g5/g6", "/g25/g1/g26/g3/g27/g5/g6") < 0) TEST_ERROR

    /* Mount fourth file under "/g25/g1/g26/g3/g27/g5" in the first file */
    if(H5Fmount(file_id, "/g25/g1/g26/g3/g27/g5", file3_id, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group3_id, "", "/g25/g1/g26/g3/g27/g5/g6") < 0) TEST_ERROR

    /* Access group in the fourth file */
    if((group4_id = H5Gopen2(file_id, "/g25/g1/g26/g3/g27/g5/g28/g7/g8", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group4_id, "/g25/g1/g26/g3/g27/g5/g28/g7/g8", "/g25/g1/g26/g3/g27/g5/g28/g7/g8") < 0) TEST_ERROR

    if(H5Funmount(file_id, "/g25/g1/g26/g3/g27/g5") < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group4_id, "/g28/g7/g8", "") < 0) TEST_ERROR
    if(check_name(group3_id, "/g25/g1/g26/g3/g27/g5/g6", "/g25/g1/g26/g3/g27/g5/g6") < 0) TEST_ERROR
    if(check_name(group2_id, "", "/g25/g1/g26/g3/g4") < 0) TEST_ERROR
    if(check_name(group_id, "", "/g25/g1/g2") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group4_id) < 0) FAIL_STACK_ERROR
    if(H5Fclose(file3_id) < 0) FAIL_STACK_ERROR

    if(H5Funmount(file_id, "/g25/g1/g26/g3") < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group3_id, "/g27/g5/g6", "") < 0) TEST_ERROR
    if(check_name(group2_id, "/g25/g1/g26/g3/g4", "/g25/g1/g26/g3/g4") < 0) TEST_ERROR
    if(check_name(group_id, "", "/g25/g1/g2") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR
    if(H5Fclose(file2_id) < 0) FAIL_STACK_ERROR

    if(H5Funmount(file_id, "/g25/g1") < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group2_id, "/g26/g3/g4", "") < 0) TEST_ERROR
    if(check_name(group_id, "/g25/g1/g2", "/g25/g1/g2") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Fclose(file1_id) < 0) FAIL_STACK_ERROR

    PASSED();


   /*-------------------------------------------------------------------------
    * Test H5Iget_name and H5Lmove with repeated path components
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name and H5Lmove with repeated path components");

    /* Create a group "g29/g1/g2/g1/g2" in a file */
    if((group_id = H5Gcreate2(file_id, "/g29", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file_id, "/g29/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group3_id = H5Gcreate2(file_id, "/g29/g1/g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group4_id = H5Gcreate2(file_id, "/g29/g1/g2/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group5_id = H5Gcreate2(file_id, "/g29/g1/g2/g1/g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Rename group */
    if(H5Lmove(file_id, "/g29/g1/g2/g1/g2", H5L_SAME_LOC, "/g29/g1/g2/g1/g3", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group5_id, "/g29/g1/g2/g1/g3", "/g29/g1/g2/g1/g3") < 0) TEST_ERROR

    /* Rename group in middle of path, keeping within the same group */
    if(H5Lmove(file_id, "/g29/g1/g2/g1", H5L_SAME_LOC, "/g29/g1/g2/g3", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group4_id, "/g29/g1/g2/g3", "/g29/g1/g2/g3") < 0) TEST_ERROR
    if(check_name(group5_id, "/g29/g1/g2/g3/g3", "/g29/g1/g2/g3/g3") < 0) TEST_ERROR

    /* Rename group in middle of path, moving to another group in file */
    if(H5Lmove(file_id, "/g29/g1/g2/g3", H5L_SAME_LOC, "/g29/g3", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group4_id, "/g29/g3", "/g29/g3") < 0) TEST_ERROR
    if(check_name(group5_id, "/g29/g3/g3", "/g29/g3/g3") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group4_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group5_id) < 0) FAIL_STACK_ERROR

    PASSED();


/*-------------------------------------------------------------------------
    * Test H5Iget_name with higher mounted file
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with higher mounted file");

    /* Create a group "/g30/g1/g2" in the first file */
    if((group_id = H5Gcreate2(file_id, "/g30", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file_id, "/g30/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group3_id = H5Gcreate2(file_id, "/g30/g1/g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close */
    H5Gclose(group_id);
    H5Gclose(group2_id);
    H5Gclose(group3_id);

    /* Create second file and group "/g31/g3/g4" in it */
    file1_id = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

    if((group_id = H5Gcreate2(file1_id, "/g31", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file1_id, "/g31/g3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group3_id = H5Gcreate2(file1_id, "/g31/g3/g4", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close */
    H5Gclose(group_id);
    H5Gclose(group2_id);
    H5Gclose(group3_id);

    /* Create third file and group "/g32/g5/g6" in it */
    file2_id = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

    if((group_id = H5Gcreate2(file2_id, "/g32", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file2_id, "/g32/g5", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group3_id = H5Gcreate2(file2_id, "/g32/g5/g6", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close */
    H5Gclose(group_id);
    H5Gclose(group2_id);
    H5Gclose(group3_id);

    /* Create fourth file and group "/g33/g5/g6" in it */
    file3_id = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

    if((group_id = H5Gcreate2(file3_id, "/g33", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file3_id, "/g33/g7", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group3_id = H5Gcreate2(file3_id, "/g33/g7/g8", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close */
    H5Gclose(group_id);
    H5Gclose(group2_id);
    H5Gclose(group3_id);

    /* Access group which will be hidden in the first file */
    if((group_id = H5Gopen2(file_id, "/g30/g1/g2", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group_id, "/g30/g1/g2", "/g30/g1/g2") < 0) TEST_ERROR

    /* Mount second file under "/g30/g1" in the first file */
    if(H5Fmount(file_id, "/g30/g1", file1_id, H5P_DEFAULT) < 0) TEST_ERROR

    /* Verify */
    if(check_name(group_id, "", "/g30/g1/g2") < 0) TEST_ERROR

    /* Access group which will be hidden in the second file */
    if((group2_id = H5Gopen2(file_id, "/g30/g1/g31/g3/g4", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group2_id, "/g30/g1/g31/g3/g4", "/g30/g1/g31/g3/g4") < 0) TEST_ERROR

    /* Mount third file under "/g30/g1/g31/g3" in the first file */
    if(H5Fmount(file_id, "/g30/g1/g31/g3", file2_id, H5P_DEFAULT) < 0) TEST_ERROR

    /* Verify */
    if(check_name(group2_id, "", "/g30/g1/g31/g3/g4") < 0) TEST_ERROR

    /* Access group which will be hidden in the third file */
    if((group3_id = H5Gopen2(file_id, "/g30/g1/g31/g3/g32/g5/g6", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group3_id, "/g30/g1/g31/g3/g32/g5/g6", "/g30/g1/g31/g3/g32/g5/g6") < 0) TEST_ERROR

    /* Mount fourth file under "/g30" in the first file, hiding the files below it */
    if(H5Fmount(file_id, "/g30", file3_id, H5P_DEFAULT) < 0) TEST_ERROR

    /* Verify */
    if(check_name(group3_id, "", "/g30/g1/g31/g3/g32/g5/g6") < 0) TEST_ERROR

    /* Access group which will be in the fourth file */
    if((group4_id = H5Gopen2(file_id, "/g30/g33/g7/g8", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group4_id, "/g30/g33/g7/g8", "/g30/g33/g7/g8") < 0) TEST_ERROR

    /* Unmount fourth file */
    if(H5Funmount(file_id, "/g30") < 0) TEST_ERROR

    /* Verify */
    if(check_name(group4_id, "/g33/g7/g8", "") < 0) TEST_ERROR
    if(check_name(group3_id, "/g30/g1/g31/g3/g32/g5/g6", "/g30/g1/g31/g3/g32/g5/g6") < 0) TEST_ERROR
    if(check_name(group2_id, "", "/g30/g1/g31/g3/g4") < 0) TEST_ERROR
    if(check_name(group_id, "", "/g30/g1/g2") < 0) TEST_ERROR

    /* Unmount third file */
    if(H5Funmount(file_id, "/g30/g1/g31/g3") < 0) TEST_ERROR

    /* Verify */
    if(check_name(group4_id, "/g33/g7/g8", "") < 0) TEST_ERROR
    if(check_name(group3_id, "/g32/g5/g6", "") < 0) TEST_ERROR
    if(check_name(group2_id, "/g30/g1/g31/g3/g4", "/g30/g1/g31/g3/g4") < 0) TEST_ERROR
    if(check_name(group_id, "", "/g30/g1/g2") < 0) TEST_ERROR

    /* Unmount second file */
    if(H5Funmount(file_id, "/g30/g1") < 0) TEST_ERROR

    /* Verify */
    if(check_name(group4_id, "/g33/g7/g8", "") < 0) TEST_ERROR
    if(check_name(group3_id, "/g32/g5/g6", "") < 0) TEST_ERROR
    if(check_name(group2_id, "/g31/g3/g4", "") < 0) TEST_ERROR
    if(check_name(group_id, "/g30/g1/g2", "/g30/g1/g2") < 0) TEST_ERROR

    /* Close groups */
    H5Gclose(group_id);
    H5Gclose(group2_id);
    H5Gclose(group3_id);
    H5Gclose(group4_id);

    /* Close files */
    H5Fclose(file1_id);
    H5Fclose(file2_id);
    H5Fclose(file3_id);

    PASSED();


/*-------------------------------------------------------------------------
    * Test H5Iget_name with multiple hard links and mounted files
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with multiple hard links and mounted files");

    /* Create second file and group "/g35/g3/g4" in it */
    if((file1_id = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    if((group_id = H5Gcreate2(file1_id, "/g35", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file1_id, "/g35/g3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group3_id = H5Gcreate2(file1_id, "/g35/g3/g4", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR

    /* Create group "/g34/g1/g2" in first file */
    if((group_id = H5Gcreate2(file_id, "/g34", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file_id, "/g34/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group3_id = H5Gcreate2(file_id, "/g34/g1/g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create hard link to "/g34/g1/g2 group */
    if(H5Lcreate_hard(file_id, "/g34/g1/g2", H5L_SAME_LOC, "/g34/g2a", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group3_id, "/g34/g1/g2", "/g34/g1/g2") < 0) TEST_ERROR

    /* Open the link to the group */
    if((group4_id = H5Gopen2(file_id, "/g34/g2a", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group4_id, "/g34/g2a", "/g34/g2a") < 0) TEST_ERROR

    /* Mount second file under "/g34/g1" in the first file */
    if(H5Fmount(file_id, "/g34/g1", file1_id, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group3_id, "", "/g34/g1/g2") < 0) TEST_ERROR
    if(check_name(group4_id, "/g34/g2a", "/g34/g2a") < 0) TEST_ERROR

    /* Unmount second file */
    if(H5Funmount(file_id, "/g34/g1") < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group3_id, "/g34/g1/g2", "/g34/g1/g2") < 0) TEST_ERROR
    if(check_name(group4_id, "/g34/g2a", "/g34/g2a") < 0) TEST_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group4_id) < 0) FAIL_STACK_ERROR
    if(H5Fclose(file1_id) < 0) FAIL_STACK_ERROR

    PASSED();


   /*-------------------------------------------------------------------------
    * Test H5Iget_name with mounted files and unlinking
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with mounted files and unlinking");

    /* Create group "/g36/g1/g2" in first file */
    if((group_id = H5Gcreate2(file_id, "/g36", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file_id, "/g36/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group3_id = H5Gcreate2(file_id, "/g36/g1/g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR

    /* Create second file and group "/g37/g4" in it */
    if((file1_id = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    if((group_id = H5Gcreate2(file1_id, "/g37", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file1_id, "/g37/g4", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group3_id = H5Gcreate2(file1_id, "/g37/g4/g5a", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group4_id = H5Gcreate2(file1_id, "/g37/g4/g5b", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Mount second file under "/g36/g1" in the first file */
    if(H5Fmount(file_id, "/g36/g1", file1_id, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Open group in mounted file */
    if((group5_id = H5Gopen2(file_id, "/g36/g1/g37/", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group5_id, "/g36/g1/g37", "/g36/g1/g37") < 0) TEST_ERROR

    /* Open group to delete in mounted file */
    if((group6_id = H5Gopen2(file_id, "/g36/g1/g37/g4/g5a", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group6_id, "/g36/g1/g37/g4/g5a", "/g36/g1/g37/g4/g5a") < 0) TEST_ERROR

    /* Delete end group in mounted file, using relative paths */
    if(H5Ldelete(group5_id, "g4/g5a", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group6_id, "", "") < 0) TEST_ERROR
    if(check_name(group3_id, "", "") < 0) TEST_ERROR

    /* Close deleted group */
    if(H5Gclose(group6_id) < 0) FAIL_STACK_ERROR

    /* Open groups to delete in mounted file */
    if((group6_id = H5Gopen2(file_id, "/g36/g1/g37/g4", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group7_id = H5Gopen2(file_id, "/g36/g1/g37/g4/g5b", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group6_id, "/g36/g1/g37/g4", "/g36/g1/g37/g4") < 0) TEST_ERROR
    if(check_name(group7_id, "/g36/g1/g37/g4/g5b", "/g36/g1/g37/g4/g5b") < 0) TEST_ERROR

    /* Delete middle group in mounted file, using relative paths */
    if(H5Ldelete(group5_id, "g4", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group6_id, "", "") < 0) TEST_ERROR
    if(check_name(group2_id, "", "") < 0) TEST_ERROR
    if(check_name(group7_id, "", "") < 0) TEST_ERROR
    if(check_name(group4_id, "", "") < 0) TEST_ERROR

    /* Close deleted groups */
    if(H5Gclose(group6_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group7_id) < 0) FAIL_STACK_ERROR

    /* Close group in mounted file */
    if(H5Gclose(group5_id) < 0) FAIL_STACK_ERROR

    if(H5Funmount(file_id, "/g36/g1") < 0) FAIL_STACK_ERROR

    /* Close */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group3_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group4_id) < 0) FAIL_STACK_ERROR
    if(H5Fclose(file1_id) < 0) FAIL_STACK_ERROR

    PASSED();


/*-------------------------------------------------------------------------
    * Test H5Iget_name with mounting already mounted files
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with mounting already mounted files");

    /* Create file and group "/g38/g1/g2" in it */
    if((file1_id = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    if((group_id = H5Gcreate2(file1_id, "/g38", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file1_id, "/g38/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group3_id = H5Gcreate2(file1_id, "/g38/g1/g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close */
    H5Gclose(group_id);
    H5Gclose(group2_id);
    H5Gclose(group3_id);

    /* Create second file and group "/g39/g1/g2" in it */
    if((file2_id = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    if((group_id = H5Gcreate2(file2_id, "/g39", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file2_id, "/g39/g3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group3_id = H5Gcreate2(file2_id, "/g39/g3/g4", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close */
    H5Gclose(group_id);
    H5Gclose(group2_id);
    H5Gclose(group3_id);

    /* Create third file and group "/g40/g5/g6" in it */
    if((file3_id = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    if((group_id = H5Gcreate2(file3_id, "/g40", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file3_id, "/g40/g5", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group3_id = H5Gcreate2(file3_id, "/g40/g5/g6", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close */
    H5Gclose(group_id);
    H5Gclose(group2_id);
    H5Gclose(group3_id);

    /* Mount second file under "/g38/g1" in the first file */
    if(H5Fmount(file1_id, "/g38/g1", file2_id, H5P_DEFAULT) < 0) TEST_ERROR

    if((group_id = H5Gopen2(file1_id, "/g38/g1/g39/g3/g4", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group_id, "/g38/g1/g39/g3/g4", "/g38/g1/g39/g3/g4") < 0) TEST_ERROR

    /* Mount first file under "/g40/g5" in the third file */
    if(H5Fmount(file3_id, "/g40/g5", file1_id, H5P_DEFAULT) < 0) TEST_ERROR

    if((group2_id = H5Gopen2(file3_id, "/g40/g5/g38/g1/g39/g3/g4", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group2_id, "/g40/g5/g38/g1/g39/g3/g4", "/g40/g5/g38/g1/g39/g3/g4") < 0) TEST_ERROR
    if(check_name(group_id, "/g38/g1/g39/g3/g4", "/g38/g1/g39/g3/g4") < 0) TEST_ERROR

    /* Unmount first file */
    if(H5Funmount(file3_id, "/g40/g5") < 0) TEST_ERROR

    /* Verify */
    if(check_name(group2_id, "/g38/g1/g39/g3/g4", "") < 0) TEST_ERROR
    if(check_name(group_id, "/g38/g1/g39/g3/g4", "/g38/g1/g39/g3/g4") < 0) TEST_ERROR

    /* Unmount second file */
    if(H5Funmount(file1_id, "/g38/g1") < 0) TEST_ERROR

    /* Verify */
    if(check_name(group_id, "/g39/g3/g4", "") < 0) TEST_ERROR

    /* Close */
    H5Gclose(group_id);
    H5Gclose(group2_id);
    H5Fclose(file1_id);
    H5Fclose(file2_id);
    H5Fclose(file3_id);

    PASSED();

/*-------------------------------------------------------------------------
    * Test H5Iget_name with opening object in unmounted file
    *-------------------------------------------------------------------------
    */

    TESTING("H5Iget_name with opening object in unmounted file");

    /* Create file and group "/g39/g1/g2" in it */
    file1_id = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

    if((group_id = H5Gcreate2(file1_id, "/g41", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file1_id, "/g41/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group3_id = H5Gcreate2(file1_id, "/g41/g1/g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close */
    H5Gclose(group_id);
    H5Gclose(group2_id);
    H5Gclose(group3_id);

    /* Create second file and group "/g42/g1/g2" in it */
    file2_id = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

    if((group_id = H5Gcreate2(file2_id, "/g42", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file2_id, "/g42/g3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group3_id = H5Gcreate2(file2_id, "/g42/g3/g4", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close */
    H5Gclose(group_id);
    H5Gclose(group2_id);
    H5Gclose(group3_id);

    /* Mount second file under "/g41/g1" in the first file */
    if(H5Fmount(file1_id, "/g41/g1", file2_id, H5P_DEFAULT) < 0) TEST_ERROR

    if((group_id = H5Gopen2(file1_id, "/g41/g1/g42/g3", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group_id, "/g41/g1/g42/g3", "/g41/g1/g42/g3") < 0) TEST_ERROR

    /* Unmount file */
    if(H5Funmount(file1_id, "/g41/g1") < 0) TEST_ERROR

    if((group2_id = H5Gopen2(group_id, "g4", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify */
    if(check_name(group2_id, "/g42/g3/g4", "") < 0) TEST_ERROR

    /* Close */
    H5Gclose(group_id);
    H5Gclose(group2_id);
    H5Fclose(file1_id);
    H5Fclose(file2_id);

    PASSED();

    return(0);

error:
    return(1);
}

static int
test_obj_ref(hid_t fapl)
{
    char filename1[1024];
    char filename2[1024];
    hid_t	fid1, fid2;		/* HDF5 File IDs		*/
    hid_t	dataset, dataset2;	/* Dataset ID			*/
    hid_t	group, group2;          /* Group ID                     */
    hid_t	sid1;                   /* Dataspace ID			*/
    hid_t	tid1;                   /* Datatype ID			*/
    hsize_t	dims1[] = {SPACE1_DIM1};
    hobj_ref_t  wbuf[SPACE1_DIM1];      /* Buffer to write to disk */
    int         tu32[SPACE1_DIM1];      /* Int data */
    int         i;                      /* counting variables */
    char buf[100];

    /* Initialize the file names */
    h5_fixname(FILENAME[1], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[2], fapl, filename2, sizeof filename2);

    /* Create files */
    if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Create dataspace for datasets */
    if((sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL)) < 0)
        FAIL_STACK_ERROR

    /* Create a group */
    if((group = H5Gcreate2(fid1, "Group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create a single dataset inside the second file, which will be mounted
     * and used to mask objects in the first file */
    if((dataset = H5Dcreate2(fid2, "Dataset1", H5T_STD_U32LE, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR
    if(H5Dclose(dataset) < 0)
        FAIL_STACK_ERROR

    /* Create a dataset(inside Group1) */
    if((dataset = H5Dcreate2(group, "Dataset1", H5T_STD_U32LE, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Initialize data buffer */
    for(i = 0; i < SPACE1_DIM1; i++)
        tu32[i] = i * 3;

    /* Write selection to disk */
    if(H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, tu32) < 0)
        FAIL_STACK_ERROR

    /* Close Dataset */
    if(H5Dclose(dataset) < 0)
        FAIL_STACK_ERROR

    /* Create another dataset(inside Group1) */
    if((dataset = H5Dcreate2(group, "Dataset2", H5T_NATIVE_UCHAR, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Close Dataset */
    if(H5Dclose(dataset) < 0)
        FAIL_STACK_ERROR

    /* Create a datatype to refer to */
    if((tid1 = H5Tcreate(H5T_COMPOUND, sizeof(s1_t))) < 0)
        FAIL_STACK_ERROR

    /* Insert fields */
    if(H5Tinsert(tid1, "a", HOFFSET(s1_t, a), H5T_NATIVE_INT) < 0)
        FAIL_STACK_ERROR
    if(H5Tinsert(tid1, "b", HOFFSET(s1_t, b), H5T_NATIVE_INT) < 0)
        FAIL_STACK_ERROR
    if(H5Tinsert(tid1, "c", HOFFSET(s1_t, c), H5T_NATIVE_FLOAT) < 0)
        FAIL_STACK_ERROR

    /* Save datatype for later */
    if(H5Tcommit2(group, "Datatype1", tid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        FAIL_STACK_ERROR

    /* Close datatype */
    if(H5Tclose(tid1) < 0)
        FAIL_STACK_ERROR

    /* Create a new group in group1 */
    if((group2 = H5Gcreate2(group, "Group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create a hard link to group1 in group2 */
    if(H5Lcreate_hard(fid1, "/Group1", H5L_SAME_LOC, "/Group1/Group2/Link", H5P_DEFAULT, H5P_DEFAULT) < 0)
        FAIL_STACK_ERROR

    /* Create dataset in that group */
    if((dataset = H5Dcreate2(group2, "Dataset4", H5T_NATIVE_UCHAR, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Close Dataset */
    if(H5Dclose(dataset) < 0)
        FAIL_STACK_ERROR

    /* Close group */
    if(H5Gclose(group) < 0)
        FAIL_STACK_ERROR
    if(H5Gclose(group2) < 0)
        FAIL_STACK_ERROR

    /* Open up that hard link and make a new dataset there */
    if((group = H5Gopen2(fid1, "/Group1/Group2/Link", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR
    if((dataset = H5Dcreate2(group, "Dataset5", H5T_NATIVE_UCHAR, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    if(H5Dclose(dataset) < 0)
        FAIL_STACK_ERROR
    if(H5Gclose(group) < 0)
        FAIL_STACK_ERROR


    /* Create a dataset to store references */
    if((dataset = H5Dcreate2(fid1, "Dataset3", H5T_STD_REF_OBJ, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Create reference to dataset */
    if(H5Rcreate(&wbuf[0], fid1, "/Dataset3", H5R_OBJECT, -1) < 0)
        FAIL_STACK_ERROR

    /* Create reference to dataset */
    if(H5Rcreate(&wbuf[1], fid1, "/Group1/Dataset2", H5R_OBJECT, -1) < 0)
        FAIL_STACK_ERROR

    /* Create reference to group */
    if(H5Rcreate(&wbuf[2], fid1, "/Group1", H5R_OBJECT, -1) < 0)
        FAIL_STACK_ERROR

    /* Create reference to named datatype */
    if(H5Rcreate(&wbuf[3], fid1, "/Group1/Datatype1", H5R_OBJECT, -1) < 0)
        FAIL_STACK_ERROR

    if(H5Rcreate(&wbuf[4], fid1, "/Group1/Group2/Dataset4", H5R_OBJECT, -1) < 0)
        FAIL_STACK_ERROR
    if(H5Rcreate(&wbuf[5], fid1, "/Group1/Group2", H5R_OBJECT, -1) < 0)
        FAIL_STACK_ERROR
    if(H5Rcreate(&wbuf[6], fid1, "/Group1/Group2/Link/Dataset5", H5R_OBJECT, -1) < 0)
        FAIL_STACK_ERROR

    /* Create reference to root group */
    if(H5Rcreate(&wbuf[7], fid1, "/", H5R_OBJECT, -1) < 0)
        FAIL_STACK_ERROR

    /* Write selection to disk */
    if(H5Dwrite(dataset, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0)
        FAIL_STACK_ERROR

    TESTING("getting path to normal dataset in root group");
    if((dataset2 = H5Rdereference(dataset, H5R_OBJECT, &wbuf[0])) < 0) FAIL_STACK_ERROR
    *buf = '\0';
    i = H5Iget_name(dataset2, (char*)buf, sizeof(buf));
    if(H5Dclose(dataset2) < 0) FAIL_STACK_ERROR
    if(!((HDstrcmp(buf, "/Dataset3") == 0) &&(i == 9))) TEST_ERROR
    *buf = '\0';
    i = H5Rget_name(dataset, H5R_OBJECT, &wbuf[0], (char*)buf, sizeof(buf));
    if(!((HDstrcmp(buf, "/Dataset3") == 0) &&(i == 9))) TEST_ERROR
    PASSED()

    HDmemset(buf, 0, sizeof(buf));
    TESTING("getting path to dataset in /Group1");
    if((dataset2 = H5Rdereference(dataset, H5R_OBJECT, &wbuf[1])) < 0) FAIL_STACK_ERROR
    *buf = '\0';
    i = H5Iget_name(dataset2, (char*)buf, sizeof(buf));
    if(H5Dclose(dataset2) < 0) FAIL_STACK_ERROR
    if(!((HDstrcmp(buf, "/Group1/Dataset2") == 0) &&(i == 16))) TEST_ERROR
    *buf = '\0';
    i = H5Rget_name(dataset, H5R_OBJECT, &wbuf[1], (char*)buf, sizeof(buf));
    if(!((HDstrcmp(buf, "/Group1/Dataset2") == 0) &&(i == 16))) TEST_ERROR
    PASSED()

    HDmemset(buf, 0, sizeof(buf));
    TESTING("getting path to /Group1");
    if((group = H5Rdereference(dataset, H5R_OBJECT, &wbuf[2])) < 0) FAIL_STACK_ERROR
    *buf = '\0';
    i = H5Iget_name(group, (char*)buf, sizeof(buf));
    if(H5Gclose(group) < 0) FAIL_STACK_ERROR
    if(!((HDstrcmp(buf, "/Group1") == 0) &&(i == 7))) TEST_ERROR
    *buf = '\0';
    i = H5Rget_name(dataset, H5R_OBJECT, &wbuf[2], (char*)buf, sizeof(buf));
    if(!((HDstrcmp(buf, "/Group1") == 0) &&(i == 7))) TEST_ERROR
    PASSED()

    HDmemset(buf, 0, sizeof(buf));
    TESTING("getting path to datatype in /Group1");
    if((tid1 = H5Rdereference(dataset, H5R_OBJECT, &wbuf[3])) < 0) FAIL_STACK_ERROR
    *buf = '\0';
    i = H5Iget_name(tid1, (char*)buf, sizeof(buf));
    if(H5Tclose(tid1) < 0) FAIL_STACK_ERROR
    if(!((HDstrcmp(buf, "/Group1/Datatype1") == 0) &&(i == 17))) TEST_ERROR
    *buf = '\0';
    i = H5Rget_name(dataset, H5R_OBJECT, &wbuf[3], (char*)buf, sizeof(buf));
    if(!((HDstrcmp(buf, "/Group1/Datatype1") == 0) &&(i == 17))) TEST_ERROR
    PASSED()

    HDmemset(buf, 0, sizeof(buf));
    TESTING("getting path to dataset in nested group");
    if((dataset2 = H5Rdereference(dataset, H5R_OBJECT, &wbuf[4])) < 0) FAIL_STACK_ERROR
    *buf = '\0';
    i = H5Iget_name(dataset2, (char*)buf, sizeof(buf));
    if(H5Dclose(dataset2) < 0) FAIL_STACK_ERROR
    if(!((HDstrcmp(buf, "/Group1/Group2/Dataset4") == 0) &&(i == 23))) TEST_ERROR
    *buf = '\0';
    i = H5Rget_name(dataset, H5R_OBJECT, &wbuf[4], (char*)buf, sizeof(buf));
    if(!((HDstrcmp(buf, "/Group1/Group2/Dataset4") == 0) &&(i == 23))) TEST_ERROR
    PASSED()

    HDmemset(buf, 0, sizeof(buf));
    TESTING("getting path to nested group");
    if((group = H5Rdereference(dataset, H5R_OBJECT, &wbuf[5])) < 0) FAIL_STACK_ERROR
    *buf = '\0';
    i = H5Iget_name(group, (char*)buf, sizeof(buf));
    if(H5Gclose(group) < 0) FAIL_STACK_ERROR
    if(!((HDstrcmp(buf, "/Group1/Group2") == 0) &&(i == 14))) TEST_ERROR
    *buf = '\0';
    i = H5Rget_name(dataset, H5R_OBJECT, &wbuf[5], (char*)buf, sizeof(buf));
    if(!((HDstrcmp(buf, "/Group1/Group2") == 0) &&(i == 14))) TEST_ERROR
    PASSED()

    HDmemset(buf, 0, sizeof(buf));
    TESTING("getting path to dataset created via hard link");
    if((dataset2 = H5Rdereference(dataset, H5R_OBJECT, &wbuf[6])) < 0) FAIL_STACK_ERROR
    *buf = '\0';
    i = H5Iget_name(dataset2, (char*)buf, sizeof(buf));
    if(H5Dclose(dataset2) < 0) FAIL_STACK_ERROR
    if(!((HDstrcmp(buf, "/Group1/Dataset5") == 0) &&(i == 16))) TEST_ERROR
    *buf = '\0';
    i = H5Rget_name(dataset, H5R_OBJECT, &wbuf[6], (char*)buf, sizeof(buf));
    if(!((HDstrcmp(buf, "/Group1/Dataset5") == 0) &&(i == 16))) TEST_ERROR
    PASSED()

    HDmemset(buf, 0, sizeof(buf));
    TESTING("getting path to root group");
    if((group = H5Rdereference(dataset, H5R_OBJECT, &wbuf[7])) < 0) FAIL_STACK_ERROR
    *buf = '\0';
    i = H5Iget_name(group, (char*)buf, sizeof(buf));
    if(H5Gclose(group) < 0) FAIL_STACK_ERROR
    if(!((HDstrcmp(buf, "/") == 0) &&(i == 1))) TEST_ERROR
    *buf = '\0';
    i = H5Rget_name(dataset, H5R_OBJECT, &wbuf[7], (char*)buf, sizeof(buf));
    if(!((HDstrcmp(buf, "/") == 0) &&(i == 1))) TEST_ERROR
    PASSED()

    /* Now we mount fid2 at /Group2 and look for dataset4.  It shouldn't be found */
    if(H5Fmount(fid1, "/Group1/Group2", fid2, H5P_DEFAULT) < 0)
        FAIL_STACK_ERROR

    TESTING("getting path to dataset hidden by a mounted file");
    if((dataset2 = H5Rdereference(dataset, H5R_OBJECT, &wbuf[4])) < 0) FAIL_STACK_ERROR
    *buf = '\0';
    i = H5Iget_name(dataset2, (char*)buf, sizeof(buf));
    if(H5Dclose(dataset2) < 0) FAIL_STACK_ERROR
    if(i != 0) TEST_ERROR
    *buf = '\0';
    i = H5Rget_name(dataset, H5R_OBJECT, &wbuf[4], (char*)buf, sizeof(buf));
    if(i != 0) TEST_ERROR
    PASSED()

    /* Now we try unlinking dataset2 from the file and searching for it.  It shouldn't be found */
    if((dataset2 = H5Rdereference(dataset, H5R_OBJECT, &wbuf[1])) < 0)
        FAIL_STACK_ERROR
    if(H5Ldelete(fid1, "/Group1/Dataset2", H5P_DEFAULT) < 0)
        FAIL_STACK_ERROR

    TESTING("getting path to dataset that has been unlinked");
    *buf = '\0';
    i = H5Iget_name(dataset2, (char*)buf, sizeof(buf));
    if(H5Dclose(dataset2) < 0) FAIL_STACK_ERROR
    if(i != 0) TEST_ERROR
    *buf = '\0';
    i = H5Rget_name(dataset, H5R_OBJECT, &wbuf[1], (char*)buf, sizeof(buf));
    if(i != 0) TEST_ERROR
    PASSED()

    /* Close disk dataspace */
    if(H5Sclose(sid1) < 0)
        FAIL_STACK_ERROR

    /* Close Dataset */
    if(H5Dclose(dataset) < 0)
        FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(fid1) < 0)
        FAIL_STACK_ERROR
    if(H5Fclose(fid2) < 0)
        FAIL_STACK_ERROR

    return 0;

error:
    return 1;
}

static int
test_reg_ref(hid_t fapl)
{
    char filename1[1024];
    hid_t	file_id;        /* file identifier */
    hid_t	dsetv_id;       /*dataset identifiers*/
    hid_t	dsetr_id;
    hid_t	space_id, spacer_id;
    hsize_t	dims[2] = {2,9};
    hsize_t	dimsr[1] = {2};
    int		rank = 2;
    int		rankr = 1;
    hdset_reg_ref_t ref[2];
    hdset_reg_ref_t ref_out[2];
    int		data[2][9] = {{1,1,2,3,3,4,5,5,6},{1,2,2,3,4,4,5,6,6}};
    hsize_t 	start[2];
    hsize_t 	count[2];
    hsize_t 	coord[2][3] = {{0, 0, 1}, {6, 0, 8}};
    unsigned 	num_points = 3;
    ssize_t 	name_size1, name_size2;
    char 	buf1[NAME_BUF_SIZE], buf2[NAME_BUF_SIZE];

    /* Initialize the file name */
    h5_fixname(FILENAME[1], fapl, filename1, sizeof filename1);

    /* Create file with default file create property but vfd access property. */
    if((file_id = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
	TEST_ERROR

    /* Create dataspace for datasets */
    if((space_id = H5Screate_simple(rank, dims, NULL)) < 0)
	TEST_ERROR
    if((spacer_id = H5Screate_simple(rankr, dimsr, NULL)) < 0)
	TEST_ERROR

    /* Create integer dataset */
    if((dsetv_id = H5Dcreate2(file_id, REFREG_DSETNAMEV, H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
	TEST_ERROR

     /* Write data to the dataset */
    if(H5Dwrite(dsetv_id, H5T_NATIVE_INT, H5S_ALL , H5S_ALL, H5P_DEFAULT, data) < 0)
	TEST_ERROR
    if(H5Dclose(dsetv_id) < 0)
	TEST_ERROR

    /* Dataset with references */
    if((dsetr_id = H5Dcreate2(file_id, REFREG_DSETNAMER, H5T_STD_REF_DSETREG, spacer_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
	TEST_ERROR

    /*
     * Create a reference to the hyperslab.
     */
    start[0] = 0;
    start[1] = 3;
    count[0] = 2;
    count[1] = 3;
    if(H5Sselect_hyperslab(space_id, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
	TEST_ERROR
    if(H5Rcreate(&ref[0], file_id, REFREG_DSETNAMEV, H5R_DATASET_REGION, space_id) < 0)
	TEST_ERROR

    /* Create a reference to elements selection */
    if(H5Sselect_none(space_id) < 0)
	TEST_ERROR
    if(H5Sselect_elements(space_id, H5S_SELECT_SET, num_points, (const hsize_t *)coord) < 0)
	TEST_ERROR
    if(H5Rcreate(&ref[1], file_id, REFREG_DSETNAMEV, H5R_DATASET_REGION, space_id) < 0)
	TEST_ERROR

    /* Write dataset with the references */
    if(H5Dwrite(dsetr_id, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT, ref) < 0)
	TEST_ERROR

    /* Close all objects */
    if(H5Sclose(space_id) < 0)
	TEST_ERROR
    if(H5Sclose(spacer_id) < 0)
	TEST_ERROR
    if(H5Dclose(dsetr_id) < 0)
	TEST_ERROR
    if(H5Fclose(file_id) < 0)
	TEST_ERROR


    /* Reopen the file to read selections back */
    if((file_id = H5Fopen(filename1, H5F_ACC_RDWR, fapl)) < 0)
	TEST_ERROR

    /* Reopen the dataset with object references and read references to the buffer */
    if((dsetr_id = H5Dopen2(file_id, REFREG_DSETNAMER, H5P_DEFAULT)) < 0)
	TEST_ERROR

    if(H5Dread(dsetr_id, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT, ref_out) < 0)
	TEST_ERROR

    /* Get name of the dataset the first region reference points to using H5Rget_name */
    TESTING("H5Rget_name to get name from region reference(hyperslab)");
    *buf1 = '\0';
    name_size1 = H5Rget_name(dsetr_id, H5R_DATASET_REGION, &ref_out[0], (char*)buf1, NAME_BUF_SIZE);
    if(!((HDstrcmp(buf1, "/MATRIX") == 0) &&(name_size1 == 7))) TEST_ERROR
    PASSED()

    TESTING("H5Iget_name to get name from region reference(hyperslab)");

    /* Dereference the first reference */
    dsetv_id = H5Rdereference(dsetr_id, H5R_DATASET_REGION, &ref_out[0]);

    /* Get name of the dataset the first region reference points using H5Iget_name */
    *buf2 = '\0';
    name_size2 = H5Iget_name(dsetv_id, (char*)buf2, NAME_BUF_SIZE);
    if(!((HDstrcmp(buf2, "/MATRIX") == 0) &&(name_size2 == 7))) TEST_ERROR

    if(H5Dclose(dsetv_id) < 0) TEST_ERROR

    PASSED()

    /* Get name of the dataset the second region reference points to using H5Rget_name */
    TESTING("H5Rget_name to get name from region reference(pnt selec)");
    *buf1 = '\0';
    name_size1 = H5Rget_name(dsetr_id, H5R_DATASET_REGION, &ref_out[1], (char*)buf1, NAME_BUF_SIZE);
    if(!((HDstrcmp(buf1, "/MATRIX") == 0) &&(name_size1 == 7))) TEST_ERROR
    PASSED()

    TESTING("H5Iget_name to get name from region reference(pnt selec)");

    /* Dereference the second reference */
    if((dsetv_id = H5Rdereference(dsetr_id, H5R_DATASET_REGION, &ref_out[1])) < 0) TEST_ERROR

    /* Get name of the dataset the first region reference points using H5Iget_name */
    *buf2 = '\0';
    name_size2 = H5Iget_name(dsetv_id, (char*)buf2, NAME_BUF_SIZE);
    if(!((HDstrcmp(buf2, "/MATRIX") == 0) &&(name_size2 == 7))) TEST_ERROR

    if(H5Dclose(dsetv_id) < 0) TEST_ERROR

    PASSED()

    if(H5Dclose(dsetr_id) < 0)
	TEST_ERROR
    if(H5Fclose(file_id) < 0)
	TEST_ERROR

    return 0;

error:
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:    test_elinks
 *
 * Purpose:     Verify that querying names of objects reached via external
 *              links uses cached path/name information for object and doesn't
 *              search the file.
 *
 * Return:      Success:        0
 *              Failure:        1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, July 27, 2010
 *
 *-------------------------------------------------------------------------
 */
static int
test_elinks(hid_t fapl)
{
    char filename1[1024], filename2[1024]; /* Filenames                 */
    hid_t	fid1, fid2;		/* HDF5 File IDs		*/
    hid_t	group, group2;          /* Group IDs                    */
    char        name[NAME_BUF_SIZE];    /* Buffer for storing object's name */
    ssize_t     namelen;                /* Length of object's name */
    hbool_t     name_cached;            /* Indicate if name is cached */

    /* Initialize the file names */
    h5_fixname(FILENAME[1], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[2], fapl, filename2, sizeof filename2);

    /* Create files */
    if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Create a group in the second file */
    if((group2 = H5Gcreate2(fid2, "Group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Close Group */
    if(H5Gclose(group2) < 0)
        FAIL_STACK_ERROR

    /* Create an external link in first file to the group in the second file */
    if(H5Lcreate_external(filename2, "Group2", fid1, "Link_to_Group2", H5P_DEFAULT, H5P_DEFAULT) < 0)
        FAIL_STACK_ERROR

    /* Create an external link in second file to the external link in the first file */
    if(H5Lcreate_external(filename1, "Link_to_Group2", fid2, "Link_to_Link_to_Group2", H5P_DEFAULT, H5P_DEFAULT) < 0)
        FAIL_STACK_ERROR

    /* Open the group in thesecond file through the external link */
    if((group = H5Gopen2(fid1, "Link_to_Group2", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Query the external link object's name */
    *name = '\0';
    name_cached = FALSE;
    namelen = H5I_get_name_test(group, (char*)name, sizeof(name), &name_cached);
    if(!((HDstrcmp(name, "/Group2") == 0) && (namelen == 7) && name_cached))
        TEST_ERROR

    /* Close Group */
    if(H5Gclose(group) < 0)
        FAIL_STACK_ERROR

    /* Open the group in the second file through the external link to the external link */
    if((group = H5Gopen2(fid2, "Link_to_Link_to_Group2", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Query the external link to external link object's name */
    *name = '\0';
    name_cached = FALSE;
    namelen = H5I_get_name_test(group, (char*)name, sizeof(name), &name_cached);
    if(!((HDstrcmp(name, "/Group2") == 0) && (namelen == 7) && name_cached))
        TEST_ERROR

    /* Close Group */
    if(H5Gclose(group) < 0)
        FAIL_STACK_ERROR

    /* Close files */
    if(H5Fclose(fid1) < 0)
        FAIL_STACK_ERROR
    if(H5Fclose(fid2) < 0)
        FAIL_STACK_ERROR

    return 0;

error:
    return 1;
}

int
main(void)
{
    hid_t   file_id =(-1);
    int nerrors = 0;
    hid_t fapl;
    char filename0[1024];

    /* Reset the library and get the file access property list */
    h5_reset();
    fapl = h5_fileaccess();
    h5_fixname(FILENAME[0], fapl, filename0, sizeof filename0);

    /* Create a new file_id using default create property but vfd access
     * property.
     */
    if((file_id = H5Fcreate(filename0,H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Call "main" test routine */
    nerrors += test_main(file_id, fapl);
    nerrors += test_obj_ref(fapl);
    nerrors += test_reg_ref(fapl);
#ifndef H5_CANNOT_OPEN_TWICE
    nerrors += test_elinks(fapl);
#endif /*H5_CANNOT_OPEN_TWICE*/

    /* Close file */
    H5Fclose(file_id);

    /* Verify symbol table messages are cached */
    nerrors += (h5_verify_cached_stabs(FILENAME, fapl) < 0 ? 1 : 0);

    if(nerrors)
        goto error;
    puts("All getname tests passed.");

    h5_cleanup(FILENAME, fapl);

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Fclose(file_id);
    } H5E_END_TRY;

    puts("***** GET NAME TESTS FAILED *****");

    return 1;
}

