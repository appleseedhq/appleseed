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
* Test program:	 tsohm
*
* Test Shared Object Header Messages
*
*************************************************************/

#include "testhdf5.h"

/*
 * This file needs to access private information from the H5F package.
 * This file also needs to access the file testing code.
 */
#define H5F_PACKAGE
#define H5F_TESTING
#include "H5Fpkg.h"		/* File access	 			*/

/* Default SOHM values */
#define DEF_NUM_INDEXES 0
const unsigned def_type_flags[H5O_SHMESG_MAX_NINDEXES] = {0,0,0,0,0,0};
const unsigned def_minsizes[H5O_SHMESG_MAX_NINDEXES] = {250,250,250,250,250,250};
#define DEF_L2B 50
#define DEF_B2L 40

/* Non-default SOHM values for testing */
#define TEST_NUM_INDEXES 4
const unsigned test_type_flags[H5O_SHMESG_MAX_NINDEXES] =
                {H5O_SHMESG_FILL_FLAG,
                 H5O_SHMESG_DTYPE_FLAG | H5O_SHMESG_ATTR_FLAG,
                 H5O_SHMESG_SDSPACE_FLAG,
                 H5O_SHMESG_PLINE_FLAG,
                 0, 0};
const unsigned test_minsizes[H5O_SHMESG_MAX_NINDEXES] = {0, 2, 40, 100, 3, 1000};
#define TEST_L2B 65
#define TEST_B2L 64

#define FILENAME   "tsohm.h5"
#define FILENAME_SRC   "tsohm_src.h5"
#define FILENAME_DST   "tsohm_dst.h5"

#define NAME_BUF_SIZE 512

/* How much overhead counts as "not much" when converting B-trees, etc. */
#define OVERHEAD_ALLOWED 1.15

#define NUM_DATASETS 10
#define NUM_ATTRIBUTES 100

typedef struct dtype1_struct {
    int    i1;
    char   str[10];
    int    i2;
    int    i3;
    int    i4;
    int    i5;
    int    i6;
    int    i7;
    int    i8;
    float  f1;
} dtype1_struct;

#define DTYPE2_SIZE 1024
const char *DSETNAME[] = {
    "dataset0",    "dataset1",
    "dataset2",    "dataset3",
    "dataset4",    "dataset5",
    "dataset6",    "dataset7",
    "dataset8",    "dataset9",
    "dataset10",    "dataset11",
    NULL
};
const char *EXTRA_DSETNAME[] = {
    "ex_dataset0",    "ex_dataset1",
    "ex_dataset2",    "ex_dataset3",
    "ex_dataset4",    "ex_dataset5",
    "ex_dataset6",    "ex_dataset7",
    "ex_dataset8",    "ex_dataset9",
    "ex_dataset10",   "ex_dataset11",
    "ex_dataset12",   "ex_dataset13",
    "ex_dataset14",   "ex_dataset15",
    "ex_dataset16",   "ex_dataset17",
    "ex_dataset18",   "ex_dataset19",
    NULL
};
#define SOHM_HELPER_NUM_EX_DSETS 20
typedef struct complex_t {
    double                  re;
    double                  im;
} complex_t;
#define ENUM_NUM_MEMBS 20
const char *ENUM_NAME[] = {
    "enum_member0",     "enum_member1",
    "enum_member2",     "enum_member3",
    "enum_member4",     "enum_member5",
    "enum_member6",     "enum_member7",
    "enum_member8",     "enum_member9",
    "enum_member10",    "enum_member11",
    "enum_member12",    "enum_member13",
    "enum_member14",    "enum_member15",
    "enum_member16",    "enum_member17",
    "enum_member18",    "enum_member19",
    NULL
};
const int ENUM_VAL[] = {
    0,     13,
    -500,  63,
    64,    -64,
    65,    2048,
    1,     2,
    -1,    7,
    130,    -5000,
    630,    640,
    -640,   650,
    20480,  10,
    -1001,    -10
};
#define SIZE2_RANK1 6
#define SIZE2_RANK2 10
#define SIZE2_DIMS {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}

#define LONG_STRING "00 index.  A long string used for testing. To create new strings, set the first two characters to be some ASCII number other than 00, such as 01."

/* Struct returned from size2_helper function */
typedef struct size2_helper_struct {
    h5_stat_size_t empty_size;
    h5_stat_size_t first_dset;
    h5_stat_size_t second_dset;
    h5_stat_size_t dsets1;
    h5_stat_size_t dsets2;
    h5_stat_size_t interleaved;
    h5_stat_size_t attrs1;
    h5_stat_size_t attrs2;
} size2_helper_struct;

/* Number of distinct messages for the sohm_delete test */
#define DELETE_NUM_MESGS 7
#define HALF_DELETE_NUM_MESGS 3
#define DELETE_DIMS {1,1,1,1,1,1,1}
#define DELETE_MIN_MESG_SIZE 10
#define DELETE_MAX_MESG_SIZE 60


/* Number of dimensions in extend_dset test */
#define EXTEND_NDIMS 2

/* Dimensions for external_dtype test */
#define NX     10
#define NY     10

/* Helper function prototypes */
static hid_t make_dtype_1(void);
static hid_t make_dtype_2(void);
static hid_t close_reopen_file(hid_t file, const char* filename, hid_t fapl_id);
static void test_sohm_attrs(void);
#ifdef NOT_NOW
static void size2_dump_struct(const char *name, size2_helper_struct *sizes);
#endif /* NOT_NOW */
static void size2_verify(void);
static void test_sohm_delete(void);
static void test_sohm_delete_revert(void);
static void test_sohm_extlink(void);



/****************************************************************
**
**  check_fcpl_values(): Helper function for test_sohm_fcpl.
**         Verifies that the *_in and *_out parameters are equal.
**
****************************************************************/
static void check_fcpl_values(hid_t fcpl_id, const unsigned nindexes_in,
                const unsigned *flags_in, const unsigned *minsizes_in,
                unsigned l2b, unsigned b2l)
{
    unsigned    num_indexes;
    unsigned    index_flags, min_mesg_size;
    unsigned    list_size, btree_size;
    unsigned     x;
    herr_t      ret;

    /* Verify number of indexes is set to default */
    ret = H5Pget_shared_mesg_nindexes(fcpl_id, &num_indexes);
    CHECK_I(ret, "H5Pget_shared_mesg_nindexes");
    VERIFY(num_indexes, nindexes_in, "H5Pget_shared_mesg_nindexes");

    /* Verify index flags and minsizes are set */
    for(x=0; x<num_indexes; ++x)
    {
        ret = H5Pget_shared_mesg_index(fcpl_id, x, &index_flags, &min_mesg_size);
        CHECK_I(ret, "H5Pget_shared_mesg_index");
        VERIFY(index_flags, flags_in[x], "H5Pget_shared_mesg_index");
        VERIFY(min_mesg_size, minsizes_in[x], "H5Pget_shared_mesg_index");
    }

    /* Check list-to-btree and btree-to-list values */
    ret = H5Pget_shared_mesg_phase_change(fcpl_id, &list_size, &btree_size);
    CHECK_I(ret, "H5Pset_shared_mesg_phase_change");
    VERIFY(list_size, l2b, "H5Pset_shared_mesg_phase_change");
    VERIFY(btree_size, b2l, "H5Pset_shared_mesg_phase_change");
}


/****************************************************************
**
**  test_sohm_fcpl(): Test File Creation Property Lists.
**
****************************************************************/
static void test_sohm_fcpl(void)
{
    hid_t       fid = -1;
    hid_t       fcpl_id = -1;
    hid_t       fcpl2_id = -1;
    unsigned    x;
    herr_t	ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing File Creation Properties for Shared Messages\n"));

    fcpl_id = H5Pcreate(H5P_FILE_CREATE);
    CHECK_I(fcpl_id, "H5Pcreate");

    /* Verify fcpl values */
    check_fcpl_values(fcpl_id, DEF_NUM_INDEXES, def_type_flags, def_minsizes, DEF_L2B, DEF_B2L);

    /* Create a file with this fcpl and make sure that all the values can be
     * retrieved.
     */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl_id, H5P_DEFAULT);
    CHECK_I(fid, "H5Fcreate");

    fcpl2_id = H5Fget_create_plist(fid);
    CHECK_I(fcpl2_id, "H5Fcreate");

    /* Verify fcpl values */
    check_fcpl_values(fcpl2_id, DEF_NUM_INDEXES, def_type_flags, def_minsizes, DEF_L2B, DEF_B2L);

    ret = H5Pclose(fcpl2_id);
    CHECK_I(ret, "H5Pclose");

    /* Close and re-open the file.  Make sure that fcpl values are still
     * correct.
     */
    ret = H5Fclose(fid);
    CHECK_I(ret, "H5Fclose");
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK_I(fid, "H5Fopen");

    fcpl2_id = H5Fget_create_plist(fid);
    CHECK_I(ret, "H5Fcreate");

    /* Verify fcpl values */
    check_fcpl_values(fcpl2_id, DEF_NUM_INDEXES, def_type_flags, def_minsizes, DEF_L2B, DEF_B2L);

    /* Clean up */
    ret = H5Pclose(fcpl2_id);
    CHECK_I(ret, "H5Pclose");
    ret = H5Pclose(fcpl_id);
    CHECK_I(ret, "H5Pclose");
    ret = H5Fclose(fid);
    CHECK_I(ret, "H5Fclose");


    /* Start over with a non-default fcpl */
    fcpl_id = H5Pcreate(H5P_FILE_CREATE);
    CHECK_I(fcpl_id, "H5Pcreate");

    /* Set up index values */
    ret = H5Pset_shared_mesg_nindexes(fcpl_id, TEST_NUM_INDEXES);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");
    for(x = 0; x < TEST_NUM_INDEXES; ++x) {
        ret = H5Pset_shared_mesg_index(fcpl_id, x, test_type_flags[x], test_minsizes[x]);
        CHECK_I(ret, "H5Pset_shared_mesg_index");
    } /* end for */

    ret = H5Pset_shared_mesg_phase_change(fcpl_id, TEST_L2B, TEST_B2L);
    CHECK_I(ret, "H5Pset_shared_mesg_phase_change");

    check_fcpl_values(fcpl_id, TEST_NUM_INDEXES, test_type_flags, test_minsizes, TEST_L2B, TEST_B2L);

    /* Use the fcpl to create a file and get it back again */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl_id, H5P_DEFAULT);
    CHECK_I(fid, "H5Fcreate");
    fcpl2_id = H5Fget_create_plist(fid);
    CHECK_I(fcpl2_id, "H5Fcreate");

    /* Verify fcpl values */
    check_fcpl_values(fcpl2_id, TEST_NUM_INDEXES, test_type_flags, test_minsizes, TEST_L2B, TEST_B2L);

    ret = H5Pclose(fcpl2_id);
    CHECK_I(ret, "H5Pclose");

    /* Close and re-open the file.  Make sure that fcpl values are still
     * correct.
     */
    ret = H5Fclose(fid);
    CHECK_I(ret, "H5Fclose");
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK_I(fid, "H5Fopen");

    fcpl2_id = H5Fget_create_plist(fid);
    CHECK_I(ret, "H5Fcreate");

    /* Verify fcpl values */
    check_fcpl_values(fcpl2_id, TEST_NUM_INDEXES, test_type_flags, test_minsizes, TEST_L2B, TEST_B2L);

    /* Clean up */
    ret = H5Pclose(fcpl2_id);
    CHECK_I(ret, "H5Pclose");
    ret = H5Fclose(fid);
    CHECK_I(ret, "H5Fclose");

    /* Test giving bogus values to H5P* functions */
    H5E_BEGIN_TRY {
        /* Trying to create too many indexes should fail */
        ret = H5Pset_shared_mesg_nindexes(fcpl_id, H5O_SHMESG_MAX_NINDEXES + 1);
        VERIFY(ret, -1, "H5Pset_shared_mesg_nindexes");

        /* Trying to set index to an index higher than the current number
         * of indexes should fail.
         */
        ret = H5Pset_shared_mesg_index(fcpl_id, H5O_SHMESG_MAX_NINDEXES, 0, 15);
        VERIFY(ret, -1, "H5Pset_shared_mesg_index");
        ret = H5Pset_shared_mesg_index(fcpl_id, TEST_NUM_INDEXES, 0, 15);
        VERIFY(ret, -1, "H5Pset_shared_mesg_index");

        /* Setting an unknown flag (all flags + 1) should fail */
        ret = H5Pset_shared_mesg_index(fcpl_id, 1, H5O_SHMESG_ALL_FLAG + 1, 15);
        VERIFY(ret, -1, "H5Pset_shared_mesg_index");

        /* Try setting two different indexes to hold fill messages.  They
         * should hold even very small messages for testing, even though we
         * wouldn't really want to share such tiny messages in the real world.
         */
        ret = H5Pset_shared_mesg_index(fcpl_id, 0, H5O_SHMESG_FILL_FLAG, 15);
        CHECK_I(ret, "H5Pset_shared_mesg_index");
        ret = H5Pset_shared_mesg_index(fcpl_id, 1, H5O_SHMESG_FILL_FLAG, 15);
        CHECK_I(ret, "H5Pset_shared_mesg_index");
        fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl_id, H5P_DEFAULT);
        VERIFY(fid, -1, "H5Fcreate");
        ret = H5Pset_shared_mesg_index(fcpl_id, 1, H5O_SHMESG_DTYPE_FLAG | H5O_SHMESG_FILL_FLAG, 15);
        CHECK_I(ret, "H5Pset_shared_mesg_index");
        fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl_id, H5P_DEFAULT);
        VERIFY(fid, -1, "H5Fcreate");

        /* Test list/btree cutoffs.  We can set these to any positive value,
         * but if the list max is less than the btree min we'll get an error
         * when the file is created.
         */
        ret = H5Pset_shared_mesg_phase_change(fcpl_id, 10, 12);
        VERIFY(ret, -1, "H5Pset_shared_mesg_phase_change");
        /* Setting them to extremely large values should also fail */
        ret = H5Pset_shared_mesg_phase_change(fcpl_id, H5O_SHMESG_MAX_LIST_SIZE + 1, 0);
        VERIFY(ret, -1, "H5Pset_shared_mesg_phase_change");
        ret = H5Pset_shared_mesg_phase_change(fcpl_id, 10, H5O_SHMESG_MAX_LIST_SIZE + 10);
        VERIFY(ret, -1, "H5Pset_shared_mesg_phase_change");
        ret = H5Pset_shared_mesg_phase_change(fcpl_id, H5O_SHMESG_MAX_LIST_SIZE, H5O_SHMESG_MAX_LIST_SIZE+1);
        VERIFY(ret, -1, "H5Pset_shared_mesg_phase_change");
    } H5E_END_TRY


    /* Actually, the list max can be exactly 1 greater than the
     * btree min, but no more.  Also, the errors above shouldn't
     * have corrupted the fcpl, although we do need to reset the
     * second index that we changed above.
     */
    ret = H5Pset_shared_mesg_index(fcpl_id, 1, test_type_flags[1], 15);
    CHECK_I(ret, "H5Pset_shared_mesg_index");
    ret = H5Pset_shared_mesg_phase_change(fcpl_id, 10, 11);
    CHECK_I(ret, "H5Pset_shared_mesg_phase_change");
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl_id, H5P_DEFAULT);
    CHECK_I(fid, "H5Fcreate");
    ret = H5Fclose(fid);
    CHECK_I(ret, "H5Fclose");

    /* Test edge cases; H5O_SHMESG_MAX_NINDEXES and H5O_SHMESG_MAX_LIST_SIZE should be
     * valid values.  Also, creating a file with uninitialized indexes
     * (indexes 3-5) should work.
     */
    ret = H5Pset_shared_mesg_nindexes(fcpl_id, H5O_SHMESG_MAX_NINDEXES);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");
    ret = H5Pset_shared_mesg_phase_change(fcpl_id, H5O_SHMESG_MAX_LIST_SIZE, H5O_SHMESG_MAX_LIST_SIZE);
    CHECK_I(ret, "H5Pset_shared_mesg_phase_change");
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl_id, H5P_DEFAULT);
    CHECK_I(fid, "H5Fcreate");


    /* Clean up */
    ret = H5Pclose(fcpl_id);
    CHECK_I(ret, "H5Pclose");
    ret = H5Fclose(fid);
    CHECK_I(ret, "H5Fclose");
}


/*-------------------------------------------------------------------------
 * Function:    make_dtype_1
 *
 * Purpose:     Creates a complicated datatype for use in testing
 *              shared object header messages. The important thing is that
 *              the datatypes must take a lot of space to store on disk.
 *
 * Return:      Success:        datatype ID (should be closed by calling function)
 *              Failure:        negative
 *
 * Programmer:  James Laird
 *              Saturday, August 26, 2006
 *
 *-------------------------------------------------------------------------
 */
static hid_t
make_dtype_1(void)
{
    hid_t dtype1_id = -1;
    hid_t str_id = -1;

    /* Create compound datatype. */
    if((dtype1_id = H5Tcreate(H5T_COMPOUND, sizeof(struct dtype1_struct))) < 0) TEST_ERROR

    if(H5Tinsert(dtype1_id, "i1", HOFFSET(dtype1_struct, i1), H5T_NATIVE_INT) < 0) TEST_ERROR

    str_id = H5Tcopy(H5T_C_S1);
    if(H5Tset_size(str_id, (size_t)10) < 0) TEST_ERROR

    if(H5Tinsert(dtype1_id, "string", HOFFSET(dtype1_struct, str), str_id) < 0) TEST_ERROR
    if(H5Tinsert(dtype1_id, "i2", HOFFSET(dtype1_struct, i2), H5T_NATIVE_INT) < 0) TEST_ERROR
    if(H5Tinsert(dtype1_id, "i3", HOFFSET(dtype1_struct, i3), H5T_NATIVE_INT) < 0) TEST_ERROR
    if(H5Tinsert(dtype1_id, "i4", HOFFSET(dtype1_struct, i4), H5T_NATIVE_INT) < 0) TEST_ERROR
    if(H5Tinsert(dtype1_id, "i5", HOFFSET(dtype1_struct, i5), H5T_NATIVE_INT) < 0) TEST_ERROR
    if(H5Tinsert(dtype1_id, "i6", HOFFSET(dtype1_struct, i6), H5T_NATIVE_INT) < 0) TEST_ERROR
    if(H5Tinsert(dtype1_id, "i7", HOFFSET(dtype1_struct, i7), H5T_NATIVE_INT) < 0) TEST_ERROR
    if(H5Tinsert(dtype1_id, "i8", HOFFSET(dtype1_struct, i8), H5T_NATIVE_INT) < 0) TEST_ERROR
    if(H5Tinsert(dtype1_id, "f1", HOFFSET(dtype1_struct, f1), H5T_NATIVE_FLOAT) < 0) TEST_ERROR

    if(H5Tclose(str_id) < 0) TEST_ERROR

    return dtype1_id;

error:
    H5E_BEGIN_TRY {
      H5Tclose(str_id);
      H5Tclose(dtype1_id);
    } H5E_END_TRY
    return -1;
}

/*-------------------------------------------------------------------------
 * Function:    make_dtype_2
 *
 * Purpose:     Creates complicated datatypes for use in testing
 *              shared object header messages. The important thing is that
 *              the datatypes must take a lot of space to store on disk.
 *
 * Return:      Success:        datatype ID (should be closed by calling function)
 *              Failure:        negative
 *
 * Programmer:  James Laird
 *              Saturday, August 26, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hid_t
make_dtype_2(void)
{
    hid_t dtype2_id = -1;
    hid_t enum_id= -1;
    hid_t int_id=-1;
    int x;
    hsize_t dims[] = {2, 1, 2, 4};
    size_t size;

    /* Create an int with a strange precision */
    if((int_id = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR
    if(H5Tset_precision(int_id, (size_t)24) < 0) TEST_ERROR

    /* Create an enumeration using that int */
    if((enum_id = H5Tenum_create(int_id)) < 0) TEST_ERROR

    for(x = 0; x < ENUM_NUM_MEMBS; x++)
      if(H5Tenum_insert(enum_id, ENUM_NAME[x], &ENUM_VAL[x]) < 0) TEST_ERROR

    /* Create arrays of arrays of arrays of enums */
    if((dtype2_id = H5Tarray_create2(enum_id, 3, dims)) < 0) TEST_ERROR
    if((dtype2_id = H5Tarray_create2(dtype2_id, 4, dims)) < 0) TEST_ERROR
    if((dtype2_id = H5Tarray_create2(dtype2_id, 2, dims)) < 0) TEST_ERROR
    if((dtype2_id = H5Tarray_create2(dtype2_id, 1, dims)) < 0) TEST_ERROR

    if(H5Tclose(enum_id) < 0) TEST_ERROR
    if(H5Tclose(int_id) < 0) TEST_ERROR

    /* Check the datatype size.  If this is different than the #defined
     * size then the fills values will have the wrong size.
     */
    size = H5Tget_size(dtype2_id);
    if(size != DTYPE2_SIZE) TEST_ERROR

    return dtype2_id;

error:
    H5E_BEGIN_TRY {
      H5Tclose(dtype2_id);
      H5Tclose(enum_id);
      H5Tclose(int_id);
    } H5E_END_TRY
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    close_reopen_file
 *
 * Purpose:     Closes a file and then reopens it.  Used to ensure that
 *              SOHMs are written to and read from disk
 *
 * Return:      Success:        new hid_t for the file
 *              Failure:        Negative
 *
 * Programmer:  James Laird
 *              Wednesday, October 4, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hid_t
close_reopen_file(hid_t file, const char* filename, hid_t fapl_id)
{
    hid_t fid;

    if(H5Fclose(file) < 0) FAIL_STACK_ERROR
    if((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl_id)) < 0) FAIL_STACK_ERROR

    return(fid);

error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    size1_helper
 *
 * Purpose:     Creates object headers that use a large datatype message.
 *
 *              Used in test_sohm_basic.  Should close the file ID passed in.
 *              Set test_file_closing to 1 to add file closing and reopening
 *              whenever possible (to test that SOHMs are written correctly
 *              on disk and not just in memory).
 *
 * Return:      Success:        file ID (may not be the same one passed in)
 *              Failure:        Negative
 *
 * Programmer:  James Laird
 *              Monday, April 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static hid_t
size1_helper(hid_t file, const char* filename, hid_t fapl_id, int test_file_closing)
{
    dtype1_struct wdata;
    dtype1_struct rdata;
    hid_t       dtype1_id = -1;
    hid_t       space_id = -1;
    hid_t       dset_id = -1;
    hsize_t     dim1[1];
    int         x;

    /* Closing and re-opening the file takes a long time on systems without
     * local disks.  Don't close and reopen if express testing is enabled.
     */
    if(GetTestExpress() > 1)
      test_file_closing = 0;

    /* Intialize wdata */
    HDmemset(&wdata, 0, sizeof(wdata));
    wdata.i1 = 11;
    HDstrcpy(wdata.str, "string");
    wdata.i2 = 22;
    wdata.i3 = 33;
    wdata.i4 = 44;
    wdata.i5 = 55;
    wdata.i6 = 66;
    wdata.i7 = 77;
    wdata.i8 = 88;
    wdata.f1 = 0.0;

    /* Intialize rdata */
    HDmemset(&rdata, 0, sizeof(rdata));

    if((dtype1_id = make_dtype_1()) < 0) TEST_ERROR

    /* Create the dataspace and dataset */
    dim1[0] = 1;
    if((space_id = H5Screate_simple(1, dim1, NULL)) < 0) TEST_ERROR

    if((dset_id = H5Dcreate2(file, DSETNAME[0], dtype1_id, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Test writing and reading */
    if(H5Dwrite(dset_id, dtype1_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, &wdata) < 0) FAIL_STACK_ERROR

    if(H5Dread(dset_id, dtype1_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdata) < 0) FAIL_STACK_ERROR

    if(rdata.i1 != wdata.i1 || rdata.i2 != wdata.i2 || HDstrcmp(rdata.str, wdata.str)) {
        H5_FAILED(); AT();
        printf("incorrect read data\n");
        goto error;
    } /* end if */
    if(H5Dclose(dset_id) < 0) FAIL_STACK_ERROR

    /* Close and re-open the file if requested*/
    if(test_file_closing)
        if((file = close_reopen_file(file, filename, fapl_id)) < 0) TEST_ERROR

    /* Create more datasets with the same datatype */
    if((dset_id = H5Dcreate2(file, DSETNAME[1], dtype1_id, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Dclose(dset_id) < 0) FAIL_STACK_ERROR

    /* Close and re-open the file if requested*/
    if(test_file_closing)
        if((file = close_reopen_file(file, filename, fapl_id)) < 0) TEST_ERROR

    if((dset_id = H5Dcreate2(file, DSETNAME[2], dtype1_id, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Dclose(dset_id) < 0) TEST_ERROR

    /* Close and re-open the file if requested*/
    if(test_file_closing)
        if((file = close_reopen_file(file, filename, fapl_id)) < 0) TEST_ERROR

    if((dset_id = H5Dcreate2(file,DSETNAME[3],dtype1_id,space_id,H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Write data to dataset 3 for later */
    if(H5Dwrite(dset_id, dtype1_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, &wdata) < 0) TEST_ERROR

    if(H5Dclose(dset_id) < 0) TEST_ERROR
    if(H5Tclose(dtype1_id) < 0) TEST_ERROR

    /* Close and re-open the file if requested*/
    if(test_file_closing)
        if((file = close_reopen_file(file, filename, fapl_id)) < 0) TEST_ERROR

    /* Make sure the data has been written successfully */
    if((dset_id = H5Dopen2(file, DSETNAME[0], H5P_DEFAULT)) < 0) TEST_ERROR
    if((dtype1_id = H5Dget_type(dset_id)) < 0) TEST_ERROR

    /* Read data back again */
    HDmemset(&rdata, 0, sizeof(rdata));
    if(H5Dread(dset_id, dtype1_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdata) < 0) {
        H5_FAILED(); AT();
        printf("Can't read data\n");
        goto error;
    } /* end if */

    if(rdata.i1 != wdata.i1 || rdata.i2 != wdata.i2 || HDstrcmp(rdata.str, wdata.str)) {
        H5_FAILED(); AT();
        printf("incorrect read data\n");
        goto error;
    } /* end if */

    if(H5Dclose(dset_id) < 0) TEST_ERROR

    /* Create several copies of the dataset (this increases the amount of space saved by sharing the datatype message) */
    for(x = 0; x < SOHM_HELPER_NUM_EX_DSETS; x++) {
        if((dset_id = H5Dcreate2(file, EXTRA_DSETNAME[x], dtype1_id, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
        if(H5Dclose(dset_id) < 0) TEST_ERROR

        /* Close and re-open the file if requested*/
        if(test_file_closing)
            if((file = close_reopen_file(file, filename, fapl_id)) < 0) TEST_ERROR
    } /* end for */

    if(H5Tclose(dtype1_id) < 0) TEST_ERROR
    if(H5Sclose(space_id) < 0) TEST_ERROR

    /* Ensure that we can still read data back from dataset 3 */
    if((dset_id = H5Dopen2(file, DSETNAME[3], H5P_DEFAULT)) < 0) TEST_ERROR
    if((dtype1_id = H5Dget_type(dset_id)) < 0) TEST_ERROR

    /* Read data back again */
    HDmemset(&rdata, 0, sizeof(rdata));
    if(H5Dread(dset_id, dtype1_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdata) < 0) {
        H5_FAILED(); AT();
        printf("Can't read data\n");
        goto error;
    } /* end if */

    if(rdata.i1 != wdata.i1 || rdata.i2 != wdata.i2 || HDstrcmp(rdata.str, wdata.str)) {
        H5_FAILED(); AT();
        printf("incorrect read data\n");
        goto error;
    } /* end if */

    if(H5Dclose(dset_id) < 0) TEST_ERROR
    if(H5Tclose(dtype1_id) < 0) TEST_ERROR
    return file;

 error:
    H5E_BEGIN_TRY {
        H5Sclose(space_id);
        H5Tclose(dtype1_id);
        H5Dclose(dset_id);
        H5Fclose(file);
    } H5E_END_TRY
    return -1;
}

/*-------------------------------------------------------------------------
 * Function:    test_sohm_size1
 *
 * Purpose:     Tests shared object header messages with a large datatype
 *
 * Programmer:  James Laird
 *              Monday, April 10, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void test_sohm_size1(void)
{
    hid_t       file = -1;
    hid_t       fcpl_id = -1;
    hid_t       fapl_id = -1;
    hsize_t     sohm_oh_size;
    hsize_t     sohm_btree_oh_size;
    h5_stat_size_t norm_empty_filesize;
    h5_stat_size_t sohm_empty_filesize;
    h5_stat_size_t sohm_btree_empty_filesize;
    h5_stat_size_t norm_final_filesize;
    h5_stat_size_t sohm_final_filesize;
    h5_stat_size_t sohm_btree_final_filesize;
    h5_stat_size_t norm_final_filesize2;
    h5_stat_size_t sohm_final_filesize2;
    h5_stat_size_t sohm_btree_final_filesize2;
    H5O_info_t  oinfo;
    unsigned    num_indexes = 1;
    unsigned    index_flags = H5O_SHMESG_DTYPE_FLAG;
    unsigned    min_mesg_size = 50;
    unsigned    list_max = 11;
    unsigned    btree_min = 10;
    herr_t      ret;

    MESSAGE(5, ("Testing that shared datatypes save space\n"));


    /* Create a FAPL with "semi" close degree, to detect dangling IDs */
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    CHECK_I(fapl_id, "H5Pcreate");

    ret = H5Pset_fclose_degree(fapl_id, H5F_CLOSE_SEMI);
    CHECK_I(ret, "H5Pset_fclose_degree");

    /* Create a file with SOHMs disabled and get its size */
    fcpl_id = H5Pcreate(H5P_FILE_CREATE);
    CHECK_I(fcpl_id, "H5Pcreate");

    ret = H5Pset_shared_mesg_nindexes(fcpl_id, 0);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");

    file = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl_id, fapl_id);
    CHECK_I(file, "H5Fcreate");

    ret = H5Fclose(file);
    CHECK_I(ret, "H5Fclose");

    /* Get the file size */
    norm_empty_filesize = h5_get_file_size(FILENAME, fapl_id);

    /* Add a bunch of large datatypes to the file */
    file = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl_id);
    CHECK_I(file, "H5Fopen");
    file = size1_helper(file, FILENAME, fapl_id, 0);
    CHECK_I(file, "size1_helper");
    ret = H5Fclose(file);
    CHECK_I(ret, "H5Fclose");

    /* Get the new file size */
    norm_final_filesize = h5_get_file_size(FILENAME, fapl_id);

    /* Use the same property list to create a new file. */
    file = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl_id, fapl_id);
    CHECK_I(file, "H5Fcreate");

    ret = H5Pclose(fcpl_id);
    CHECK_I(ret, "H5Pclose");

    /* Add the same large datatypes, but keep closing and re-opening the file */
    file = size1_helper(file, FILENAME, fapl_id, 1);
    CHECK_I(file, "size1_helper");
    ret = H5Fclose(file);
    CHECK_I(ret, "H5Fclose");

    /* Get the file size */
    norm_final_filesize2 = h5_get_file_size(FILENAME, fapl_id);



    /* Now do the same thing for a file with SOHMs enabled */
    /* Create FCPL with SOHMs enabled */
    fcpl_id = H5Pcreate(H5P_FILE_CREATE);
    CHECK_I(fcpl_id, "H5Pcreate");

    /* Tests one index holding only datatype messages */
    ret = H5Pset_shared_mesg_nindexes(fcpl_id, num_indexes);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");
    ret = H5Pset_shared_mesg_index(fcpl_id, 0, index_flags, min_mesg_size);
    CHECK_I(ret, "H5Pset_shared_mesg_index");
    ret = H5Pset_shared_mesg_phase_change(fcpl_id, list_max, btree_min);
    CHECK_I(ret, "H5Pset_shared_mesg_phase_change");

    /* Create a file */
    file = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl_id, fapl_id);
    CHECK_I(file, "H5Fcreate");

    ret = H5Fclose(file);
    CHECK_I(ret, "H5Fclose");

    sohm_empty_filesize = h5_get_file_size(FILENAME, fapl_id);

    /* Add a bunch of datatypes to this file */
    file = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl_id);
    CHECK_I(file, "H5Fopen");
    file = size1_helper(file, FILENAME, fapl_id, 0);
    CHECK_I(file, "size1_helper");

    /* Get the size of a dataset object header */
    ret = H5Oget_info_by_name(file, DSETNAME[0], &oinfo, H5P_DEFAULT);
    CHECK_I(ret, "H5Oget_info_by_name");
    ret = H5Fclose(file);
    CHECK_I(ret, "H5Fclose");
    sohm_oh_size = oinfo.hdr.space.total;

    /* Get the new file size */
    sohm_final_filesize = h5_get_file_size(FILENAME, fapl_id);

    /* Use the same property list to create a new file. */
    file = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl_id, fapl_id);
    CHECK_I(file, "H5Fcreate");

    ret = H5Pclose(fcpl_id);
    CHECK_I(ret, "H5Pclose");

    /* Add the same large datatypes, but keep closing and re-opening the file */
    file = size1_helper(file, FILENAME, fapl_id, 1);
    CHECK_I(file, "size1_helper");
    ret = H5Fclose(file);
    CHECK_I(ret, "H5Fclose");

    /* Get the file size */
    sohm_final_filesize2 = h5_get_file_size(FILENAME, fapl_id);



    /* Create FCPL with SOHMs enabled that uses a B-tree index */
    fcpl_id = H5Pcreate(H5P_FILE_CREATE);
    CHECK_I(fcpl_id, "H5Pcreate");

    /* Tests one index holding only datatype messages */
    ret = H5Pset_shared_mesg_nindexes(fcpl_id, num_indexes);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");
    ret = H5Pset_shared_mesg_index(fcpl_id, 0, index_flags, min_mesg_size);
    CHECK_I(ret, "H5Pset_shared_mesg_index");
    ret = H5Pset_shared_mesg_phase_change(fcpl_id, 0, 0);
    CHECK_I(ret, "H5Pset_shared_mesg_phase_change");

    /* Create a file */
    file = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl_id, fapl_id);
    CHECK_I(file, "H5Fcreate");

    ret = H5Fclose(file);
    CHECK_I(ret, "H5Fclose");

    sohm_btree_empty_filesize = h5_get_file_size(FILENAME, fapl_id);

    /* Add a bunch of datatypes to this file */
    file = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl_id);
    CHECK_I(file, "H5Fopen");
    file = size1_helper(file, FILENAME, fapl_id, 0);
    CHECK_I(file, "size1_helper");

    /* Get the size of a dataset object header */
    ret = H5Oget_info_by_name(file, DSETNAME[0], &oinfo, H5P_DEFAULT);
    CHECK_I(ret, "H5Oget_info_by_name");
    ret = H5Fclose(file);
    CHECK_I(ret, "H5Fclose");
    sohm_btree_oh_size = oinfo.hdr.space.total;

    /* Get the new file size */
    sohm_btree_final_filesize = h5_get_file_size(FILENAME, fapl_id);

    /* Use the same property list to create a new file. */
    file = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl_id, fapl_id);
    CHECK_I(file, "H5Fcreate");

    ret = H5Pclose(fcpl_id);
    CHECK_I(ret, "H5Pclose");

    /* Add the same large datatypes, but keep closing and re-opening the file */
    file = size1_helper(file, FILENAME, fapl_id, 1);
    CHECK_I(file, "size1_helper");
    ret = H5Fclose(file);
    CHECK_I(ret, "H5Fclose");

    /* Get the file size */
    sohm_btree_final_filesize2 = h5_get_file_size(FILENAME, fapl_id);



    /* Check that all sizes make sense */
    /* Object headers in SOHM files should be smaller than normal object
     * headers.  How the SOHM messages are stored shouldn't affect the
     * size of the object header.
     */
    if(sohm_oh_size != sohm_btree_oh_size)
        VERIFY(sohm_btree_oh_size, 1, "H5Oget_info_by_name");

    /* Both sohm files should be bigger than a normal file when empty.
     * It's hard to say whether a B-tree with no nodes allocated should be
     * smaller than a list with SOHM_HELPER_NUM_DTYPES elements.
     * The sizes here shouldn't really be 1; it's just used to ensure that the
     * error code triggers.
     */
    if(sohm_empty_filesize <= norm_empty_filesize)
        VERIFY(sohm_empty_filesize, 1, "h5_get_file_size");

    if(sohm_btree_empty_filesize <= norm_empty_filesize)
        VERIFY(sohm_btree_empty_filesize, 1, "h5_get_file_size");

    /* When full, the sohm btree file should be smaller than the normal file.
     * The sohm list file should be at least as small, since it doesn't need the
     * overhead of a B-tree.
     */
    if(sohm_btree_final_filesize >= norm_final_filesize)
        VERIFY(sohm_btree_final_filesize, 1, "h5_get_file_size");
    if(sohm_final_filesize > sohm_btree_final_filesize)
        VERIFY(sohm_final_filesize, 1, "h5_get_file_size");

    /* This shouldn't change even if we open and close the file */
    if(sohm_btree_final_filesize2 >= norm_final_filesize2)
        VERIFY(sohm_btree_final_filesize2, 1, "h5_get_file_size");
    if(sohm_final_filesize2 > sohm_btree_final_filesize2)
        VERIFY(sohm_final_filesize2, 1, "h5_get_file_size");

    ret = H5Pclose(fapl_id);
    CHECK_I(ret, "H5Pclose");
}


/*-------------------------------------------------------------------------
 * Function:    sohm_attr_helper
 *
 * Purpose:     Given an fcpl, tests creating attributes with and without
 *              committed datatypes.
 *
 * Programmer:  James Laird
 *              Thursday, November 30, 2006
 *
 *-------------------------------------------------------------------------
 */
static void sohm_attr_helper(hid_t fcpl_id)
{
    hid_t file_id;
    hid_t type_id;
    hid_t space_id;
    hid_t group_id;
    hid_t attr_id, attr_id2;
    hsize_t dims = 2;
    int wdata[2] = {7, 42};
    int rdata[2];
    herr_t ret;
    size_t x;

    /*----------------------------------------------------------------------------
     *    Test attribute with transient datatype
     */
    /* Create a file using the fcpl */
    file_id = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl_id, H5P_DEFAULT);
    CHECK_I(file_id, "H5Fcreate");

    /* Create a normal datatype and dataset */
    type_id = H5Tcopy(H5T_NATIVE_INT);
    CHECK_I(type_id, "H5Tcopy");
    space_id = H5Screate_simple(1, &dims, &dims);
    CHECK_I(space_id, "H5Screate_simple");

    /* Create and verify an attribute on a group */
    group_id = H5Gcreate2(file_id, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK_I(group_id, "H5Gcreate2");
    attr_id = H5Acreate2(group_id, "attribute", type_id, space_id, H5P_DEFAULT, H5P_DEFAULT);
    CHECK_I(attr_id, "H5Acreate2");
    ret = H5Awrite(attr_id, H5T_NATIVE_INT, wdata);
    CHECK_I(ret, "H5Awrite");

    /* Close the datatype and group */
    ret = H5Tclose(type_id);
    CHECK_I(ret, "H5Tclose");
    ret = H5Gclose(group_id);
    CHECK_I(ret, "H5Gclose");

    /* Flush the file to force data to be written */
    ret = H5Fflush(file_id, H5F_SCOPE_GLOBAL);
    CHECK_I(ret, "H5Fflush");

    /* Verify */
    HDmemset(rdata, 0, sizeof(rdata));
    ret = H5Aread(attr_id, H5T_NATIVE_INT, rdata);
    CHECK_I(ret, "H5Aread");
    for(x = 0; x < (size_t)dims; ++x)
        VERIFY(rdata[x], wdata[x], "H5Aread");

    /* Cleanup */
    ret = H5Aclose(attr_id);
    CHECK_I(ret, "H5Aclose");

    /*----------------------------------------------------------------------------
     *    Test attribute with committed datatype
     */
    /* Repeat with a committed datatype */
    type_id = H5Tcopy(H5T_NATIVE_INT);
    CHECK_I(type_id, "H5Tcopy");
    ret = H5Tcommit2(file_id, "datatype", type_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK_I(ret, "H5Tcommit2");

    /* Create and verify an attribute */
    group_id = H5Gcreate2(file_id, "another_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK_I(group_id, "H5Gcreate2");
    attr_id = H5Acreate2(group_id, "attribute", type_id, space_id, H5P_DEFAULT, H5P_DEFAULT);
    CHECK_I(attr_id, "H5Acreate2");
    ret = H5Awrite(attr_id, H5T_NATIVE_INT, wdata);
    CHECK_I(ret, "H5Awrite");

    /* Close the datatype and group */
    ret = H5Tclose(type_id);
    CHECK_I(ret, "H5Tclose");
    ret = H5Gclose(group_id);
    CHECK_I(ret, "H5Gclose");

    /* Flush the file to force data to be written */
    ret = H5Fflush(file_id, H5F_SCOPE_GLOBAL);
    CHECK_I(ret, "H5Fflush");

    /* Verify */
    HDmemset(rdata, 0, sizeof(rdata));
    ret = H5Aread(attr_id, H5T_NATIVE_INT, rdata);
    CHECK_I(ret, "H5Aread");
    for(x = 0; x < (size_t)dims; ++x)
        VERIFY(rdata[x], wdata[x], "H5Aread");

    /* Cleanup */
    ret = H5Aclose(attr_id);
    CHECK_I(ret, "H5Aclose");

    /*----------------------------------------------------------------------------
     *    Test attribute operation with two ID handles
     */
    /* Create and verify an attribute */
    group_id = H5Gcreate2(file_id, "yet_another_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK_I(group_id, "H5Gcreate2");

    attr_id = H5Acreate2(group_id, "attribute", H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT);
    CHECK_I(attr_id, "H5Acreate2");

    /* Open the attribute to get another handle */
    attr_id2 = H5Aopen(group_id, "attribute", H5P_DEFAULT);
    CHECK_I(attr_id2, "H5Aopen");

    ret = H5Awrite(attr_id, H5T_NATIVE_INT, wdata);
    CHECK_I(ret, "H5Awrite");

    /* Close the group */
    ret = H5Gclose(group_id);
    CHECK_I(ret, "H5Gclose");

    /* Flush the file to force data to be written */
    ret = H5Fflush(file_id, H5F_SCOPE_GLOBAL);
    CHECK_I(ret, "H5Fflush");

    /* Verify the data with another ID handle */
    HDmemset(rdata, 0, sizeof(rdata));
    ret = H5Aread(attr_id2, H5T_NATIVE_INT, rdata);
    CHECK_I(ret, "H5Aread");

    for(x = 0; x < (size_t)dims; ++x)
        VERIFY(rdata[x], wdata[x], "H5Aread");

    /* Cleanup */
    ret = H5Aclose(attr_id);
    CHECK_I(ret, "H5Aclose");
    ret = H5Aclose(attr_id2);
    CHECK_I(ret, "H5Aclose");

    ret = H5Sclose(space_id);
    CHECK_I(ret, "H5Sclose");
    ret = H5Fclose(file_id);
    CHECK_I(ret, "H5Fclose");
}


/*-------------------------------------------------------------------------
 * Function:    test_sohm_attrs
 *
 * Purpose:     Attributes can be shared and can also contain shared
 *              datatype and dataspace messages.  Committed datatypes
 *              shouldn't be shared.
 *
 *              Test permutations of this.
 *
 * Programmer:  James Laird
 *              Thursday, November 30, 2006
 *
 *-------------------------------------------------------------------------
 */
static void test_sohm_attrs(void)
{
    hid_t fcpl_id;
    herr_t ret;

    MESSAGE(5, ("Testing that shared messages work with attributes\n"));

    /* Create an fcpl with no shared messages */
    fcpl_id = H5Pcreate(H5P_FILE_CREATE);
    CHECK_I(fcpl_id, "H5Pcreate");
    ret = H5Pset_shared_mesg_nindexes(fcpl_id, 0);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");

    /* Make sure attributes can be read with these settings (they'd better!) */
    sohm_attr_helper(fcpl_id);


    /* Run tests with only one kind of message to be shared */
    ret = H5Pset_shared_mesg_nindexes(fcpl_id, 1);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");
    ret = H5Pset_shared_mesg_index(fcpl_id, 0, H5O_SHMESG_ATTR_FLAG, 2);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");

    /* Verify */
    sohm_attr_helper(fcpl_id);

    ret = H5Pset_shared_mesg_index(fcpl_id, 0, H5O_SHMESG_SDSPACE_FLAG, 2);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");

    sohm_attr_helper(fcpl_id);

    ret = H5Pset_shared_mesg_index(fcpl_id, 0, H5O_SHMESG_DTYPE_FLAG, 2);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");

    sohm_attr_helper(fcpl_id);


    /* Run with any two types shared */
    ret = H5Pset_shared_mesg_index(fcpl_id, 0, H5O_SHMESG_SDSPACE_FLAG | H5O_SHMESG_DTYPE_FLAG, 2);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");

    sohm_attr_helper(fcpl_id);

    ret = H5Pset_shared_mesg_index(fcpl_id, 0, H5O_SHMESG_ATTR_FLAG | H5O_SHMESG_DTYPE_FLAG, 2);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");

    sohm_attr_helper(fcpl_id);

    ret = H5Pset_shared_mesg_index(fcpl_id, 0, H5O_SHMESG_SDSPACE_FLAG | H5O_SHMESG_ATTR_FLAG, 2);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");

    sohm_attr_helper(fcpl_id);


    /* Run test with all three kinds of message shared */
    ret = H5Pset_shared_mesg_index(fcpl_id, 0, H5O_SHMESG_SDSPACE_FLAG | H5O_SHMESG_DTYPE_FLAG | H5O_SHMESG_ATTR_FLAG, 2);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");

    sohm_attr_helper(fcpl_id);


    /* Try using two indexes */
    ret = H5Pset_shared_mesg_nindexes(fcpl_id, 2);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");
    ret = H5Pset_shared_mesg_index(fcpl_id, 0, H5O_SHMESG_ATTR_FLAG | H5O_SHMESG_DTYPE_FLAG, 2);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");
    ret = H5Pset_shared_mesg_index(fcpl_id, 1, H5O_SHMESG_SDSPACE_FLAG, 2);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");

    sohm_attr_helper(fcpl_id);

    ret = H5Pset_shared_mesg_index(fcpl_id, 0, H5O_SHMESG_DTYPE_FLAG, 2);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");

    sohm_attr_helper(fcpl_id);

    ret = H5Pset_shared_mesg_index(fcpl_id, 1, H5O_SHMESG_ATTR_FLAG, 2);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");

    sohm_attr_helper(fcpl_id);


    /* One index for each kind of message */
    ret = H5Pset_shared_mesg_nindexes(fcpl_id, 3);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");
    ret = H5Pset_shared_mesg_index(fcpl_id, 2, H5O_SHMESG_SDSPACE_FLAG, 2);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");

    sohm_attr_helper(fcpl_id);


    /* Close the FCPL */
    ret = H5Pclose(fcpl_id);
    CHECK_I(ret, "H5Pclose");
}

/*-------------------------------------------------------------------------
 * Function:    size2_verify_plist1
 *
 * Purpose:     Verify that the property list passed in is in fact the
 *              same property list used as dcpl1_id in the size2 helper
 *              function.  This ensures that the filters can be read.
 *
 * Programmer:  James Laird
 *              Wednesday, November 22, 2006
 *
 *-------------------------------------------------------------------------
 */
static void size2_verify_plist1(hid_t plist)
{
    size_t cd_nelmts;
    unsigned int cd_value;
    char name[NAME_BUF_SIZE];
    H5Z_filter_t filter;
    hid_t dtype1_id;
    dtype1_struct fill1;
    dtype1_struct fill1_correct;
    herr_t ret;

    /* Hardcoded to correspond to dcpl1_id created in size2_helper */
    /* Check filters */
    cd_nelmts = 1;
    filter = H5Pget_filter2(plist, 0, NULL, &cd_nelmts, &cd_value, (size_t)NAME_BUF_SIZE, name, NULL);
    CHECK_I(filter, "H5Pget_filter2");
    VERIFY(filter, H5Z_FILTER_SHUFFLE, "H5Pget_filter2");

    cd_nelmts = 1;
    filter = H5Pget_filter2(plist, 1, NULL, &cd_nelmts, &cd_value, (size_t)NAME_BUF_SIZE, name, NULL);
    CHECK_I(filter, "H5Pget_filter2");
    VERIFY(filter, H5Z_FILTER_DEFLATE, "H5Pget_filter2");
    VERIFY(cd_value, 1, "H5Pget_filter2");

    cd_nelmts = 1;
    filter = H5Pget_filter2(plist, 2, NULL, &cd_nelmts, &cd_value, (size_t)NAME_BUF_SIZE, name, NULL);
    CHECK_I(filter, "H5Pget_filter2");
    VERIFY(filter, H5Z_FILTER_SHUFFLE, "H5Pget_filter2");

    cd_nelmts = 1;
    filter = H5Pget_filter2(plist, 3, NULL, &cd_nelmts, &cd_value, (size_t)NAME_BUF_SIZE, name, NULL);
    CHECK_I(filter, "H5Pget_filter2");
    VERIFY(filter, H5Z_FILTER_FLETCHER32, "H5Pget_filter2");


    /* Check fill value */
    dtype1_id=make_dtype_1();
    CHECK_I(dtype1_id, "make_dtype_1");
    HDmemset(&fill1_correct, '1', sizeof(fill1_correct));

    ret = H5Pget_fill_value(plist, dtype1_id, &fill1);
    CHECK_I(ret, "H5Pget_fill_value");

    ret = memcmp(&fill1, &fill1_correct, sizeof(fill1_correct));
    VERIFY(ret, 0, memcmp);
}

/*-------------------------------------------------------------------------
 * Function:    size2_verify_plist2
 *
 * Purpose:     Verify that the property list passed in is in fact the
 *              same property list used as dcpl2_id in the size2 helper
 *              function.  This ensures that the filters can be read.
 *
 * Programmer:  James Laird
 *              Wednesday, November 22, 2006
 *
 *-------------------------------------------------------------------------
 */
static void size2_verify_plist2(hid_t plist)
{
    size_t cd_nelmts;
    unsigned int cd_value;
    char name[NAME_BUF_SIZE];
    H5Z_filter_t filter;
    hid_t dtype2_id;
    char fill2[DTYPE2_SIZE];
    char fill2_correct[DTYPE2_SIZE];
    herr_t ret;

    /* Hardcoded to correspond to dcpl1_id created in size2_helper */
    /* Check filters */
    cd_nelmts = 1;
    filter = H5Pget_filter2(plist, 0, NULL, &cd_nelmts, &cd_value, (size_t)NAME_BUF_SIZE, name, NULL);
    CHECK_I(filter, "H5Pget_filter2");
    VERIFY(filter, H5Z_FILTER_DEFLATE, "H5Pget_filter2");
    VERIFY(cd_value, 1, "H5Pget_filter2");

    cd_nelmts = 1;
    filter = H5Pget_filter2(plist, 1, NULL, &cd_nelmts, &cd_value, (size_t)NAME_BUF_SIZE, name, NULL);
    CHECK_I(filter, "H5Pget_filter2");
    VERIFY(filter, H5Z_FILTER_DEFLATE, "H5Pget_filter2");
    VERIFY(cd_value, 2, "H5Pget_filter2");

    cd_nelmts = 1;
    filter = H5Pget_filter2(plist, 2, NULL, &cd_nelmts, &cd_value, (size_t)NAME_BUF_SIZE, name, NULL);
    CHECK_I(filter, "H5Pget_filter2");
    VERIFY(filter, H5Z_FILTER_DEFLATE, "H5Pget_filter2");
    VERIFY(cd_value, 2, "H5Pget_filter2");

    cd_nelmts = 1;
    filter = H5Pget_filter2(plist, 3, NULL, &cd_nelmts, &cd_value, (size_t)NAME_BUF_SIZE, name, NULL);
    CHECK_I(filter, "H5Pget_filter2");
    VERIFY(filter, H5Z_FILTER_DEFLATE, "H5Pget_filter2");
    VERIFY(cd_value, 1, "H5Pget_filter2");

    cd_nelmts = 1;
    filter = H5Pget_filter2(plist, 4, NULL, &cd_nelmts, &cd_value, (size_t)NAME_BUF_SIZE, name, NULL);
    CHECK_I(filter, "H5Pget_filter2");
    VERIFY(filter, H5Z_FILTER_DEFLATE, "H5Pget_filter2");
    VERIFY(cd_value, 5, "H5Pget_filter2");


    /* Check fill value */
    dtype2_id = make_dtype_2();
    CHECK_I(dtype2_id, "make_dtype_2");
    HDmemset(&fill2_correct, '2', (size_t)DTYPE2_SIZE);

    ret = H5Pget_fill_value(plist, dtype2_id, &fill2);
    CHECK_I(ret, "H5Pget_fill_value");

    ret = HDmemcmp(&fill2, &fill2_correct, (size_t)DTYPE2_SIZE);
    VERIFY(ret, 0, memcmp);
}

#ifdef NOT_NOW

/*-------------------------------------------------------------------------
 * Function:    size2_dump_struct
 *
 * Purpose:     A debugging function to print the contents of a
 *              size2_helper_struct (which holds the various sizes for a
 *              given file during the size2_helper function).
 *
 * Programmer:  James Laird
 *              Friday, January 26, 2007
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
size2_dump_struct(const char *name, size2_helper_struct *sizes)
{
  puts(name);
  printf("    empty size: %llu\n", (unsigned long long)sizes->empty_size);
  printf(" first dataset: %llu \tdelta: %llu\n", (unsigned long long)sizes->first_dset, (unsigned long long)(sizes->first_dset - sizes->empty_size));
  printf("second dataset: %llu \tdelta: %llu\n", (unsigned long long)sizes->second_dset, (unsigned long long)(sizes->second_dset - sizes->first_dset));
  printf("       dsets 1: %llu \tdelta: %llu\n", (unsigned long long)sizes->dsets1, (unsigned long long)(sizes->dsets1 - sizes->second_dset));
  printf("       dsets 2: %llu \tdelta: %llu\n", (unsigned long long)sizes->dsets2, (unsigned long long)(sizes->dsets2 - sizes->dsets1));
  printf("   interleaved: %llu \tdelta: %llu\n", (unsigned long long)sizes->interleaved, (unsigned long long)(sizes->interleaved - sizes->dsets2));
  printf("    attributes: %llu \tdelta: %llu\n", (unsigned long long)sizes->attrs1, (unsigned long long)(sizes->attrs1 - sizes->interleaved));
  printf("  attributes 2: %llu \tdelta: %llu\n", (unsigned long long)sizes->attrs2, (unsigned long long)(sizes->attrs2 - sizes->attrs1));
}
#endif /* NOT_NOW */


/*-------------------------------------------------------------------------
 * Function:    size2_helper
 *
 * Purpose:     A helper functon for test_sohm_size2.
 *
 *              Creates a file using the given fcpl, then creates lots
 *              of different kinds of messages within the file and
 *              returns the size of the file for comparison.
 *
 *              If test_file_closing is not zero, closes and re-opens
 *              the file after every write.
 *
 *              Doesn't close the property list.  Prints an error message
 *              if there's a failure, but doesn't alter its return value.
 *
 * Programmer:  James Laird
 *              Friday, November 17, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
size2_helper(hid_t fcpl_id, int test_file_closing, size2_helper_struct *ret_sizes)
{
    hid_t file_id = -1;
    hid_t dtype1_id = -1;
    hid_t dtype2_id = -1;
    hid_t dspace1_id = -1;
    hid_t dspace2_id = -1;
    hid_t dcpl1_id = -1;
    hid_t dcpl2_id = -1;
    hid_t dset_id = -1;
    hid_t attr_type_id = -1;
    hid_t attr_space_id = -1;
    hid_t attr_id = -1;
    hid_t group_id = -1;
    char attr_string1[NAME_BUF_SIZE];
    char attr_string2[NAME_BUF_SIZE];
    char attr_name[NAME_BUF_SIZE];
    int x;
    herr_t ret;

    /* Constants used in this function */
    const int rank1 = SIZE2_RANK1;
    const int rank2 = SIZE2_RANK2;
    const hsize_t dims[SIZE2_RANK2] = SIZE2_DIMS;
    dtype1_struct fill1;
    char fill2[DTYPE2_SIZE];

    /* Closing and re-opening the file takes a long time on systems without
     * local disks.  Don't close and reopen if express testing is enabled.
     */
    if(GetTestExpress() > 1)
        test_file_closing = 0;

    /* Create a file and get its size */
    file_id = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl_id, H5P_DEFAULT);
    CHECK_I(file_id, "H5Fcreate");

    ret = H5Fclose(file_id);
    CHECK_I(ret, "H5Fclose");

    /* Get the file size */
    ret_sizes->empty_size = h5_get_file_size(FILENAME, H5P_DEFAULT);

    /* Re-open the file and set up messages to write */
    file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK_I(file_id, "H5Fopen");

    /* Create two large datatype messages */
    dtype1_id = make_dtype_1();
    CHECK_I(dtype1_id, "make_dtype_1");
    dtype2_id = make_dtype_2();
    CHECK_I(dtype2_id, "make_dtype_2");

    /* Create some large dataspaces */
    dspace1_id = H5Screate_simple(rank1, dims, dims);
    CHECK_I(dspace1_id, "H5Screate_simple");
    dspace2_id = H5Screate_simple(rank2, dims, dims);
    CHECK_I(dspace2_id, "H5Screate_simple");

    /* fill1 and fill2 are fill values for the two datatypes.
     * We'll set them in the DCPL.
     */
    HDmemset(&fill1, '1', sizeof(dtype1_struct));
    HDmemset(&fill2, '2', (size_t)DTYPE2_SIZE);

    dcpl1_id = H5Pcreate(H5P_DATASET_CREATE);
    CHECK_I(dcpl1_id, "H5Pcreate");
    H5Pset_fill_value(dcpl1_id, dtype1_id, &fill1);

    dcpl2_id = H5Pcreate(H5P_DATASET_CREATE);
    CHECK_I(dcpl2_id, "H5Pcreate");
    H5Pset_fill_value(dcpl2_id, dtype2_id, &fill2);

    /* Filter messages we'll create by setting them in a DCPL. These
     * values don't need to make sense, they just need to take up space.
     */
    ret = H5Pset_chunk(dcpl1_id, rank1, dims);
    CHECK_I(ret, "H5Pset_chunk");
    ret = H5Pset_shuffle(dcpl1_id);
    CHECK_I(ret, "H5Pset_shuffle");
    ret = H5Pset_deflate(dcpl1_id, 1);
    CHECK_I(ret, "H5Pset_deflate");
    ret = H5Pset_shuffle(dcpl1_id);
    CHECK_I(ret, "H5Pset_shuffle");
    ret = H5Pset_fletcher32(dcpl1_id);
    CHECK_I(ret, "H5Pset_fletcher32");
    /* Make sure that this property list is what it should be */
    size2_verify_plist1(dcpl1_id);

    /* Second dcpl */
    ret = H5Pset_chunk(dcpl2_id, rank2, dims);
    CHECK_I(ret, "H5Pset_chunk");
    ret = H5Pset_deflate(dcpl2_id, 1);
    CHECK_I(ret, "H5Pset_deflate");
    ret = H5Pset_deflate(dcpl2_id, 2);
    CHECK_I(ret, "H5Pset_deflate");
    ret = H5Pset_deflate(dcpl2_id, 2);
    CHECK_I(ret, "H5Pset_deflate");
    ret = H5Pset_deflate(dcpl2_id, 1);
    CHECK_I(ret, "H5Pset_deflate");
    ret = H5Pset_deflate(dcpl2_id, 5);
    CHECK_I(ret, "H5Pset_deflate");
    /* Make sure that this property list is what it should be */
    size2_verify_plist2(dcpl2_id);

    /* Set up attribute data */
    HDmemset(attr_string1, 0, (size_t)NAME_BUF_SIZE);
    HDmemset(attr_string2, 0, (size_t)NAME_BUF_SIZE);
    HDstrcpy(attr_string1, LONG_STRING);
    HDstrcpy(attr_string2, LONG_STRING);
    attr_string2[1] = '1';        /* The second string starts "01 index..." */

    /* Set up attribute metadata */
    attr_type_id = H5Tcopy(H5T_C_S1);
    CHECK_I(attr_type_id, "H5Tcopy");
    ret = H5Tset_size(attr_type_id, (size_t)NAME_BUF_SIZE);
    CHECK_I(ret, "H5Tset_size");
    attr_space_id = H5Screate_simple(1, dims, dims);
    CHECK_I(attr_space_id, "H5Screate_simple");

    /* Create datasets with a big datatype, dataspace, fill value,
     * and filter pipeline.
     */
    for(x = 0; x < NUM_DATASETS; ++x) {
        dset_id = H5Dcreate2(file_id, DSETNAME[x], dtype1_id, dspace1_id, H5P_DEFAULT, dcpl1_id, H5P_DEFAULT);
        CHECK_I(dset_id, "H5Dcreate2");

        attr_id = H5Acreate2(dset_id, "attr_name", attr_type_id, attr_space_id, H5P_DEFAULT, H5P_DEFAULT);
        CHECK_I(attr_id, "H5Acreate2");
        ret = H5Awrite(attr_id, attr_type_id, attr_string1);
        CHECK_I(ret, "H5Awrite");

        ret = H5Aclose(attr_id);
        CHECK_I(ret, "H5Aclose");
        ret = H5Dclose(dset_id);
        CHECK_I(ret, "H5Dclose");

        /* Gather extra statistics on first two datasets in file */
        if(x < 2) {
            ret = H5Fclose(file_id);
            CHECK_I(ret, "H5Fclose");

            /* Get the file's size now */
            if(x == 0)
                ret_sizes->first_dset = h5_get_file_size(FILENAME, H5P_DEFAULT);
            else
                ret_sizes->second_dset = h5_get_file_size(FILENAME, H5P_DEFAULT);

            file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
            CHECK_I(file_id, "H5Fopen");
        } /* end if */
        /* Close & reopen file if requested */
        else if(test_file_closing) {
            file_id = close_reopen_file(file_id, FILENAME, H5P_DEFAULT);
            CHECK_I(file_id, "H5Fopen");
        } /* end if */
    } /* end for */

    /* Close file and get its size now */
    ret = H5Fclose(file_id);
    CHECK_I(ret, "H5Fclose");
    ret_sizes->dsets1 = h5_get_file_size(FILENAME, H5P_DEFAULT);


    /* Now create a new group filled with datasets that use all different messages */
    file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK_I(file_id, "H5Fopen");
    group_id = H5Gcreate2(file_id, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK_I(group_id, "H5Gcreate2");

    /* Create NUM_DATASETS datasets in the new group */
    for(x=0; x<NUM_DATASETS; ++x)
    {
        dset_id = H5Dcreate2(group_id, DSETNAME[x], dtype2_id, dspace2_id, H5P_DEFAULT, dcpl2_id, H5P_DEFAULT);
        CHECK_I(dset_id, "H5Dcreate2");

        attr_id = H5Acreate2(dset_id, "attr_name", attr_type_id, attr_space_id, H5P_DEFAULT, H5P_DEFAULT);
        CHECK_I(attr_id, "H5Acreate2");
        ret = H5Awrite(attr_id, attr_type_id, attr_string2);
        CHECK_I(ret, "H5Awrite");

        ret = H5Dclose(dset_id);
        CHECK_I(ret, "H5Dclose");
        ret = H5Aclose(attr_id);
        CHECK_I(ret, "H5Aclose");

        /* Close everything & reopen file if requested */
        if(test_file_closing) {
            ret = H5Gclose(group_id);
            CHECK_I(ret, "H5Gclose");
            file_id = close_reopen_file(file_id, FILENAME, H5P_DEFAULT);
            CHECK_I(file_id, "H5Fopen");
            group_id = H5Gopen2(file_id, "group", H5P_DEFAULT);
            CHECK_I(group_id, "H5Gopen2");
        }
    }

    /* Close file and get its size now */
    ret = H5Gclose(group_id);
    CHECK_I(ret, "H5Gclose");
    ret = H5Fclose(file_id);
    CHECK_I(ret, "H5Fclose");
    ret_sizes->dsets2 = h5_get_file_size(FILENAME, H5P_DEFAULT);


    /* Create a new group and interleave writes of datasets types 1 and 2. */
    file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK_I(file_id, "H5Fopen");
    group_id = H5Gcreate2(file_id, "interleaved group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK_I(group_id, "H5Gcreate2");

    /* Create NUM_DATASETS datasets in the new group */
    for(x=0; x<NUM_DATASETS; x+=2)
    {
        dset_id = H5Dcreate2(group_id, DSETNAME[x], dtype1_id, dspace1_id, H5P_DEFAULT, dcpl1_id, H5P_DEFAULT);
        CHECK_I(dset_id, "H5Dcreate2");

        attr_id = H5Acreate2(dset_id, "attr_name", attr_type_id, attr_space_id, H5P_DEFAULT, H5P_DEFAULT);
        CHECK_I(attr_id, "H5Acreate2");
        ret = H5Awrite(attr_id, attr_type_id, attr_string1);
        CHECK_I(ret, "H5Awrite");

        ret = H5Dclose(dset_id);
        CHECK_I(ret, "H5Dclose");
        ret = H5Aclose(attr_id);
        CHECK_I(ret, "H5Aclose");

        dset_id = H5Dcreate2(group_id, DSETNAME[x+1], dtype2_id, dspace2_id, H5P_DEFAULT, dcpl2_id, H5P_DEFAULT);
        CHECK_I(dset_id, "H5Dcreate2");

        attr_id = H5Acreate2(dset_id, "attr_name", attr_type_id, attr_space_id, H5P_DEFAULT, H5P_DEFAULT);
        CHECK_I(attr_id, "H5Acreate2");
        ret = H5Awrite(attr_id, attr_type_id, attr_string2);
        CHECK_I(ret, "H5Awrite");

        ret = H5Dclose(dset_id);
        CHECK_I(ret, "H5Dclose");
        ret = H5Aclose(attr_id);
        CHECK_I(ret, "H5Aclose");

        /* Close everything & reopen file if requested */
        if(test_file_closing) {
            ret = H5Gclose(group_id);
            CHECK_I(ret, "H5Gclose");
            file_id = close_reopen_file(file_id, FILENAME, H5P_DEFAULT);
            CHECK_I(file_id, "H5Fopen");
            group_id = H5Gopen2(file_id, "interleaved group", H5P_DEFAULT);
            CHECK_I(group_id, "H5Gopen2");
        }
    }

    /* Close file and get its size now */
    ret = H5Gclose(group_id);
    CHECK_I(ret, "H5Gclose");
    ret = H5Fclose(file_id);
    CHECK_I(ret, "H5Fclose");
    ret_sizes->interleaved = h5_get_file_size(FILENAME, H5P_DEFAULT);

    /* Create lots of new attribute messages on the group
     * (using different strings for the attribute)
     */
    file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK_I(file_id, "H5Fopen");
    group_id = H5Gopen2(file_id, "group", H5P_DEFAULT);
    CHECK_I(group_id, "H5Gopen2");

    HDstrcpy(attr_name, "00 index");

    for(x = 0; x < NUM_ATTRIBUTES; ++x) {
        /* Create a unique name and value for each attribute */
        attr_string1[0] = attr_name[0] = (x / 10) + '0';
        attr_string1[1] = attr_name[1] = (x % 10) + '0';

        /* Create an attribute on the group */
        attr_id = H5Acreate2(group_id, attr_name, attr_type_id, attr_space_id, H5P_DEFAULT, H5P_DEFAULT);
        CHECK_I(attr_id, "H5Acreate2");
        ret = H5Awrite(attr_id, attr_type_id, attr_string1);
        CHECK_I(ret, "H5Awrite");

        ret = H5Aclose(attr_id);
        CHECK_I(ret, "H5Aclose");

        /* Close everything & reopen file if requested */
        if(test_file_closing) {
            ret = H5Gclose(group_id);
            CHECK_I(ret, "H5Gclose");
            file_id = close_reopen_file(file_id, FILENAME, H5P_DEFAULT);
            CHECK_I(file_id, "H5Fopen");
            group_id = H5Gopen2(file_id, "group", H5P_DEFAULT);
            CHECK_I(group_id, "H5Gopen2");
        }
    }

    /* Close file and get its size now */
    ret = H5Gclose(group_id);
    CHECK_I(ret, "H5Gclose");
    ret = H5Fclose(file_id);
    CHECK_I(ret, "H5Fclose");
    ret_sizes->attrs1 = h5_get_file_size(FILENAME, H5P_DEFAULT);


    /* Create all of the attributes again on the other group */
    file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK_I(file_id, "H5Fopen");
    group_id = H5Gopen2(file_id, "interleaved group", H5P_DEFAULT);
    CHECK_I(group_id, "H5Gopen2");

    for(x=0; x<NUM_ATTRIBUTES; ++x)
    {
        /* Create the same name and value for each attribute as before */
        attr_string1[0] = attr_name[0] = (x / 10) + '0';
        attr_string1[1] = attr_name[1] = (x % 10) + '0';

        /* Create an attribute on the group */
        attr_id = H5Acreate2(group_id, attr_name, attr_type_id, attr_space_id, H5P_DEFAULT, H5P_DEFAULT);
        CHECK_I(attr_id, "H5Acreate2");
        ret = H5Awrite(attr_id, attr_type_id, attr_string1);
        CHECK_I(ret, "H5Awrite");

        ret = H5Aclose(attr_id);
        CHECK_I(ret, "H5Aclose");

        /* Close everything & reopen file if requested */
        if(test_file_closing) {
            ret = H5Gclose(group_id);
            CHECK_I(ret, "H5Gclose");
            file_id = close_reopen_file(file_id, FILENAME, H5P_DEFAULT);
            CHECK_I(file_id, "H5Fopen");
            group_id = H5Gopen2(file_id, "interleaved group", H5P_DEFAULT);
            CHECK_I(group_id, "H5Gopen2");
        }
    }
    /* Close file and get its size now */
    ret = H5Gclose(group_id);
    CHECK_I(ret, "H5Gclose");
    ret = H5Fclose(file_id);
    CHECK_I(ret, "H5Fclose");
    ret_sizes->attrs2 = h5_get_file_size(FILENAME, H5P_DEFAULT);


    /* Close everything */
    ret = H5Sclose(attr_space_id);
    CHECK_I(ret, "H5Sclose");
    ret = H5Tclose(attr_type_id);
    CHECK_I(ret, "H5Sclose");
    ret = H5Tclose(dtype1_id);
    CHECK_I(ret, "H5Tclose");
    ret = H5Tclose(dtype2_id);
    CHECK_I(ret, "H5Tclose");
    ret = H5Sclose(dspace1_id);
    CHECK_I(ret, "H5Sclose");
    ret = H5Sclose(dspace2_id);
    CHECK_I(ret, "H5Sclose");
    ret = H5Pclose(dcpl1_id);
    CHECK_I(ret, "H5Pclose");
    ret = H5Pclose(dcpl2_id);
    CHECK_I(ret, "H5Pclose");

    return 0;
}


/*-------------------------------------------------------------------------
 * Function:    size2_verify
 *
 * Purpose:     A helper functon to verify the file created by size2_helper.
 *
 *              Runs various tests (not exhaustive) to ensure that the
 *              file FILENAME actually has the structure that size2_helper
 *              should have created.
 *
 * Programmer:  James Laird
 *              Friday, November 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static void size2_verify(void)
{
    hid_t file_id = -1;
    hid_t dset_id=-1;
    hid_t plist_id=-1;
    hid_t space_id=-1;
    hid_t group1_id, group2_id;
    hid_t attr1_id, attr2_id;
    hid_t attr_type_id;
    int x, y;
    herr_t ret;
    char attr_string[NAME_BUF_SIZE];
    char attr_correct_string[NAME_BUF_SIZE];
    char attr_name[NAME_BUF_SIZE];
    int ndims;
    hsize_t dims[SIZE2_RANK2];
    hsize_t correct_dims[SIZE2_RANK2] = SIZE2_DIMS;

    file_id = H5Fopen(FILENAME, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK_I(file_id, "H5Fopen");


    /* Verify property lists and dataspaces */

    /* Get property lists from first batch of datasets */
    for(x = 0; x < NUM_DATASETS; ++x) {
        dset_id = H5Dopen2(file_id, DSETNAME[x], H5P_DEFAULT);
        CHECK_I(dset_id, "H5Dopen2");
        plist_id = H5Dget_create_plist(dset_id);
        CHECK_I(plist_id, "H5Dget_create_plist");
        size2_verify_plist1(plist_id);
        ret = H5Pclose(plist_id);
        CHECK_I(ret, "H5Pclose");

        space_id = H5Dget_space(dset_id);
        CHECK_I(space_id, "H5Dget_space");
        ndims = H5Sget_simple_extent_dims(space_id, dims, NULL);
        CHECK_I(ndims, "H5Sget_simple_extent_dims");
        VERIFY(ndims, SIZE2_RANK1, "H5Sget_simple_extent_dims");
        for(y = 0; y < ndims; ++y)
            VERIFY(dims[y], correct_dims[y], "H5Sget_simple_extent_dims");

        ret = H5Sclose(space_id);
        CHECK_I(ret, "H5Sclose");

        ret = H5Dclose(dset_id);
        CHECK_I(ret, "H5Dclose");
    }

    /* Get property lists from second batch of datasets */
    group1_id = H5Gopen2(file_id, "group", H5P_DEFAULT);
    CHECK_I(group1_id, "H5Gopen2");
    for(x = 0; x < NUM_DATASETS; ++x) {
        dset_id = H5Dopen2(group1_id, DSETNAME[x], H5P_DEFAULT);
        CHECK_I(dset_id, "H5Dopen2");
        plist_id = H5Dget_create_plist(dset_id);
        CHECK_I(plist_id, "H5Dget_create_plist");
        size2_verify_plist2(plist_id);
        ret = H5Pclose(plist_id);
        CHECK_I(ret, "H5Pclose");

        space_id = H5Dget_space(dset_id);
        CHECK_I(space_id, "H5Dget_space");
        ndims = H5Sget_simple_extent_dims(space_id, dims, NULL);
        CHECK_I(ndims, "H5Sget_simple_extent_dims");
        VERIFY(ndims, SIZE2_RANK2, "H5Sget_simple_extent_dims");
        for(y = 0; y < ndims; ++y)
            VERIFY(dims[y], correct_dims[y], "H5Sget_simple_extent_dims");

        ret = H5Sclose(space_id);
        CHECK_I(ret, "H5Sclose");

        ret = H5Dclose(dset_id);
        CHECK_I(ret, "H5Dclose");
    } /* end for */
    ret = H5Gclose(group1_id);
    CHECK_I(ret, "H5Gclose");

    /* Get property lists from interleaved group of datasets */
    group1_id = H5Gopen2(file_id, "interleaved group", H5P_DEFAULT);
    CHECK_I(group1_id, "H5Gopen2");
    for(x = 0; x < NUM_DATASETS; x += 2) {
        /* First "type 1" dataset */
        dset_id = H5Dopen2(group1_id, DSETNAME[x], H5P_DEFAULT);
        CHECK_I(dset_id, "H5Dopen2");
        plist_id = H5Dget_create_plist(dset_id);
        CHECK_I(plist_id, "H5Dget_create_plist");
        size2_verify_plist1(plist_id);
        ret = H5Pclose(plist_id);
        CHECK_I(ret, "H5Pclose");

        space_id = H5Dget_space(dset_id);
        CHECK_I(space_id, "H5Dget_space");
        ndims = H5Sget_simple_extent_dims(space_id, dims, NULL);
        CHECK_I(ndims, "H5Sget_simple_extent_dims");
        VERIFY(ndims, SIZE2_RANK1, "H5Sget_simple_extent_dims");
        for(y = 0; y < ndims; ++y)
            VERIFY(dims[y], correct_dims[y], "H5Sget_simple_extent_dims");

        ret = H5Sclose(space_id);
        CHECK_I(ret, "H5Sclose");

        ret = H5Dclose(dset_id);
        CHECK_I(ret, "H5Dclose");

        /* Second "type 2" dataset */
        dset_id = H5Dopen2(group1_id, DSETNAME[x + 1], H5P_DEFAULT);
        CHECK_I(dset_id, "H5Dopen2");
        plist_id = H5Dget_create_plist(dset_id);
        CHECK_I(plist_id, "H5Dget_create_plist");
        size2_verify_plist2(plist_id);
        ret = H5Pclose(plist_id);
        CHECK_I(ret, "H5Pclose");

        space_id = H5Dget_space(dset_id);
        CHECK_I(space_id, "H5Dget_space");
        ndims = H5Sget_simple_extent_dims(space_id, dims, NULL);
        CHECK_I(ndims, "H5Sget_simple_extent_dims");
        VERIFY(ndims, SIZE2_RANK2, "H5Sget_simple_extent_dims");
        for(y = 0; y < ndims; ++y)
            VERIFY(dims[y], correct_dims[y], "H5Sget_simple_extent_dims");

        ret = H5Sclose(space_id);
        CHECK_I(ret, "H5Sclose");

        ret = H5Dclose(dset_id);
        CHECK_I(ret, "H5Dclose");
    } /* end for */
    ret = H5Gclose(group1_id);
    CHECK_I(ret, "H5Gclose");


    /* Verify attributes */

    /* Create attribute data type */
    attr_type_id = H5Tcopy(H5T_C_S1);
    CHECK_I(attr_type_id, "H5Tcopy");
    ret = H5Tset_size(attr_type_id, (size_t)NAME_BUF_SIZE);
    CHECK_I(ret, "H5Tset_size");

    /* Read attributes on both groups and verify that they are correct */
    group1_id = H5Gopen2(file_id, "group", H5P_DEFAULT);
    CHECK_I(group1_id, "H5Gopen2");
    group2_id = H5Gopen2(file_id, "interleaved group", H5P_DEFAULT);
    CHECK_I(group2_id, "H5Gopen2");

    HDmemset(attr_string, 0, (size_t)NAME_BUF_SIZE);
    HDmemset(attr_correct_string, 0, (size_t)NAME_BUF_SIZE);
    HDstrcpy(attr_correct_string, LONG_STRING);
    HDstrcpy(attr_name, "00 index");

    for(x = 0; x < NUM_ATTRIBUTES; ++x) {
        /* Create the name and correct value for each attribute */
        attr_correct_string[0] = attr_name[0] = (x / 10) + '0';
        attr_correct_string[1] = attr_name[1] = (x % 10) + '0';

        attr1_id = H5Aopen(group1_id, attr_name, H5P_DEFAULT);
        CHECK_I(attr1_id, "H5Aopen");
        attr2_id = H5Aopen(group2_id, attr_name, H5P_DEFAULT);
        CHECK_I(attr2_id, "H5Aopen");

        ret = H5Aread(attr1_id, attr_type_id, attr_string);
        CHECK_I(ret, "H5Aread");
        VERIFY_STR(attr_string, attr_correct_string, "H5Aread");
        ret = H5Aread(attr2_id, attr_type_id, attr_string);
        CHECK_I(ret, "H5Aread");
        VERIFY_STR(attr_string, attr_correct_string, "H5Aread");

        ret = H5Aclose(attr1_id);
        CHECK_I(attr1_id, "H5Aclose");
        ret = H5Aclose(attr2_id);
        CHECK_I(attr2_id, "H5Aclose");
    }

    /* Close everything */
    ret = H5Tclose(attr_type_id);
    CHECK_I(ret, "H5Tclose");
    ret = H5Gclose(group1_id);
    CHECK_I(ret, "H5Gclose");
    ret = H5Gclose(group2_id);
    CHECK_I(ret, "H5Gclose");
    ret = H5Fclose(file_id);
    CHECK_I(ret, "H5Fclose");
}


/*-------------------------------------------------------------------------
 * Function:    test_sohm_size2
 *
 * Purpose:     Tests shared object header messages using size2_helper to
 *              create different kinds of big messages.
 *
 *              If close_reopen is set, closes and reopens the HDF5 file
 *              repeatedly while writing.
 *
 *              This test works by first creating FCPLs with various
 *              parameters, then creating a standard file that includes
 *              every kind of message that can be shared using the helper
 *              function size2_helper.  The test measures the size of the
 *              file at various points.  Once all of the files have been
 *              generated, the test compares the measured sizes of the files.
 *
 *
 * Programmer:  James Laird
 *              Friday, November 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static void test_sohm_size2(int close_reopen)
{
    hid_t       fcpl_id = -1;
    /* Sizes for file with no shared messages at all */
    size2_helper_struct norm_sizes;
    /* Sizes for files with all messages in one index */
    size2_helper_struct list_index_med, list_index_big;
    size2_helper_struct btree_index, list_index_small;
    /* Sizes for files with messages in three different indexes */
    size2_helper_struct mult_index_med, mult_index_btree;
    /* Sizes for files that don't share all kinds of messages */
    size2_helper_struct share_some_med, share_some_btree;
    /* Sizes for files that share different sizes of messages */
    size2_helper_struct share_some_toobig_index, share_tiny_index, type_space_index;
    herr_t      ret;

    if(close_reopen == 0)
        MESSAGE(5, ("Testing that shared object header messages save space\n"))
    else
        MESSAGE(5, ("Testing that shared messages save space when file is closed and reopened\n"))

    /* Create an fcpl with SOHMs disabled */
    fcpl_id = H5Pcreate(H5P_FILE_CREATE);
    CHECK_I(fcpl_id, "H5Pcreate");

    ret = H5Pset_shared_mesg_nindexes(fcpl_id, 0);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");

    /* Get the file size & verify its contents */
    size2_helper(fcpl_id, close_reopen, &norm_sizes);
    size2_verify();

    ret = H5Pclose(fcpl_id);
    CHECK_I(ret, "H5Pclose");


    /* Create an fcpl with one big index */
    fcpl_id = H5Pcreate(H5P_FILE_CREATE);
    CHECK_I(fcpl_id, "H5Pcreate");

    ret = H5Pset_shared_mesg_nindexes(fcpl_id, 1);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");
    ret = H5Pset_shared_mesg_index(fcpl_id, 0, H5O_SHMESG_ALL_FLAG, 20);
    CHECK_I(ret, "H5Pset_shared_mesg_index");

    /* Set the indexes to use a medium-sized list */
    ret = H5Pset_shared_mesg_phase_change(fcpl_id, 30, 25);
    CHECK_I(ret, "H5Pset_shared_mesg_phase_change");

    /* Get the file size & verify its contents */
    size2_helper(fcpl_id, close_reopen, &list_index_med);
    size2_verify();


    /* Try making the list really big */
    ret = H5Pset_shared_mesg_phase_change(fcpl_id, 1000, 900);
    CHECK_I(ret, "H5Pset_shared_mesg_phase_change");

    /* Get the file size & verify its contents */
    size2_helper(fcpl_id, close_reopen, &list_index_big);
    size2_verify();


    /* Use a B-tree instead of a list */
    ret = H5Pset_shared_mesg_phase_change(fcpl_id, 0, 0);
    CHECK_I(ret, "H5Pset_shared_mesg_phase_change");

    /* Get the file size & verify its contents */
    size2_helper(fcpl_id, close_reopen, &btree_index);
    size2_verify();


    /* Use such a small list that it'll become a B-tree */
    ret = H5Pset_shared_mesg_phase_change(fcpl_id, 10, 0);
    CHECK_I(ret, "H5Pset_shared_mesg_phase_change");

    /* Get the file size & verify its contents */
    size2_helper(fcpl_id, close_reopen, &list_index_small);
    size2_verify();

    ret = H5Pclose(fcpl_id);
    CHECK_I(ret, "H5Pclose");


    /* Create a new property list that puts messages in different indexes. */
    fcpl_id = H5Pcreate(H5P_FILE_CREATE);
    CHECK_I(fcpl_id, "H5Pcreate");

    ret = H5Pset_shared_mesg_nindexes(fcpl_id, 3);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");
    ret = H5Pset_shared_mesg_index(fcpl_id, 0, H5O_SHMESG_SDSPACE_FLAG | H5O_SHMESG_DTYPE_FLAG, 20);
    CHECK_I(ret, "H5Pset_shared_mesg_index");
    ret = H5Pset_shared_mesg_index(fcpl_id, 1, H5O_SHMESG_FILL_FLAG | H5O_SHMESG_PLINE_FLAG, 20);
    CHECK_I(ret, "H5Pset_shared_mesg_index");
    ret = H5Pset_shared_mesg_index(fcpl_id, 2, H5O_SHMESG_ATTR_FLAG, 20);
    CHECK_I(ret, "H5Pset_shared_mesg_index");

    /* Use lists that are the same size as the "medium" list on the previous
     * run.
     */
    ret = H5Pset_shared_mesg_phase_change(fcpl_id, 30, 25);
    CHECK_I(ret, "H5Pset_shared_mesg_phase_change");

    /* Get the file size & verify its contents */
    size2_helper(fcpl_id, close_reopen, &mult_index_med);
    size2_verify();


    /* Use all B-trees */
    ret = H5Pset_shared_mesg_phase_change(fcpl_id, 0, 0);
    CHECK_I(ret, "H5Pset_shared_mesg_phase_change");

    /* Get the file size & verify its contents */
    size2_helper(fcpl_id, close_reopen, &mult_index_btree);
    size2_verify();


    /* Edit the same property list (this should work) and don't share all messages.
     */
    ret = H5Pset_shared_mesg_index(fcpl_id, 0, H5O_SHMESG_PLINE_FLAG, 20);
    CHECK_I(ret, "H5Pset_shared_mesg_index");
    ret = H5Pset_shared_mesg_index(fcpl_id, 1, H5O_SHMESG_DTYPE_FLAG | H5O_SHMESG_FILL_FLAG, 100000);
    CHECK_I(ret, "H5Pset_shared_mesg_index");
    ret = H5Pset_shared_mesg_index(fcpl_id, 2, H5O_SHMESG_ATTR_FLAG | H5O_SHMESG_SDSPACE_FLAG, 20);
    CHECK_I(ret, "H5Pset_shared_mesg_index");

    /* Use "normal-sized" lists. */
    ret = H5Pset_shared_mesg_phase_change(fcpl_id, 30, 25);
    CHECK_I(ret, "H5Pset_shared_mesg_phase_change");

    /* Get the file size & verify its contents */
    size2_helper(fcpl_id, close_reopen, &share_some_med);
    size2_verify();

    /* Use btrees. */
    ret = H5Pset_shared_mesg_phase_change(fcpl_id, 0, 0);
    CHECK_I(ret, "H5Pset_shared_mesg_phase_change");

    /* Get the file size & verify its contents */
    size2_helper(fcpl_id, close_reopen, &share_some_btree);
    size2_verify();


    /* Change the second index to hold only gigantic messages.  Result should
     * be the same as the previous file.
     */
    ret = H5Pset_shared_mesg_index(fcpl_id, 1, H5O_SHMESG_DTYPE_FLAG | H5O_SHMESG_FILL_FLAG, 100000);
    CHECK_I(ret, "H5Pset_shared_mesg_index");

    /* Get the file size & verify its contents */
    size2_helper(fcpl_id, close_reopen, &share_some_toobig_index);
    size2_verify();


    /* Share even tiny dataspace and datatype messages.  This should result in
     * attribute datatypes being shared.  Make this one use "really big" lists.
     * It turns out that attribute dataspaces are just big enough that it saves
     * some space to share them, while sharing datatypes creates as much overhead
     * as one gains from sharing them.
     */
    ret = H5Pset_shared_mesg_nindexes(fcpl_id, 1);
    ret = H5Pset_shared_mesg_index(fcpl_id, 0, H5O_SHMESG_DTYPE_FLAG | H5O_SHMESG_SDSPACE_FLAG, 1);
    ret = H5Pset_shared_mesg_phase_change(fcpl_id, 1000, 900);

    /* Get the file size & verify its contents */
    size2_helper(fcpl_id, close_reopen, &share_tiny_index);
    size2_verify();

    /* Create the same file but don't share the really tiny messages */
    ret = H5Pset_shared_mesg_index(fcpl_id, 0, H5O_SHMESG_DTYPE_FLAG | H5O_SHMESG_SDSPACE_FLAG, 100);
    ret = H5Pset_shared_mesg_phase_change(fcpl_id, 1000, 900);

    /* Get the file size & verify its contents */
    size2_helper(fcpl_id, close_reopen, &type_space_index);
    size2_verify();

    ret = H5Pclose(fcpl_id);
    CHECK_I(ret, "H5Pclose");



    /* Check that all sizes make sense.  There is lots of room for inexact
     * results here since so many different factors contribute to file size.
     */


    /* Check sizes of all files created using a single index first */

    /* The empty size of each file with shared messages enabled should be the
     * same and should be bigger than a normal file.
     */
    if(norm_sizes.empty_size > list_index_med.empty_size)
        VERIFY(norm_sizes.empty_size, 1, "h5_get_file_size");
    if(list_index_med.empty_size != list_index_big.empty_size)
        VERIFY(list_index_med.empty_size, list_index_big.empty_size, "h5_get_file_size");
    if(list_index_med.empty_size != btree_index.empty_size)
        VERIFY(list_index_med.empty_size, btree_index.empty_size, "h5_get_file_size");
    if(list_index_med.empty_size != list_index_small.empty_size)
        VERIFY(list_index_med.empty_size, list_index_small.empty_size, "h5_get_file_size");
    /* The files with indexes shouldn't be that much bigger than an
     * empty file.
     */
    if(list_index_med.empty_size > norm_sizes.empty_size * OVERHEAD_ALLOWED)
        VERIFY(0, 1, "h5_get_file_size");


    /* Once one dataset has been created (with one of every kind of message),
     * the normal file should still be smallest.  The very small list
     * btree_convert should be smaller than the B-tree since it has no
     * extra overhead.  The small list should also be smaller than the B-tree.
     * The very large list should be much larger than anything else.
     */
    if(norm_sizes.first_dset >= list_index_small.first_dset)
        VERIFY(norm_sizes.first_dset, 1, "h5_get_file_size");
    if(list_index_small.first_dset >= btree_index.first_dset)
        VERIFY(list_index_small.first_dset, 1, "h5_get_file_size");
    if(list_index_med.first_dset >= btree_index.first_dset)
        VERIFY(btree_index.first_dset, 1, "h5_get_file_size");
    if(btree_index.first_dset >= list_index_big.first_dset)
        VERIFY(list_index_med.first_dset, 1, "h5_get_file_size");


    /* Once a few copies of the same dataset have been created, the
     * very small list shouldn't have become a B-tree yet, so it should
     * be the smallest file.  A larger list should be next, followed
     * by a B-tree, followed by a normal file, followed by a
     * list that is too large.
     */
    if(list_index_small.dsets1 >= list_index_med.dsets1)
        VERIFY(btree_index.dsets1, 1, "h5_get_file_size");
    if(list_index_med.dsets1 >= btree_index.dsets1)
        VERIFY(list_index_med.dsets1, 1, "h5_get_file_size");
    if(btree_index.dsets1 >= norm_sizes.dsets1)
        VERIFY(btree_index.dsets1, 1, "h5_get_file_size");
    if(norm_sizes.dsets1 >= list_index_big.dsets1)
        VERIFY(list_index_big.dsets1, 1, "h5_get_file_size");

    /* The size gain should have been the same for each of the lists;
     * their overhead is fixed.  The B-tree should have gained at least
     * as much, and the normal file more than that.
     */
    if((list_index_small.dsets1 - list_index_small.first_dset) !=
            (list_index_med.dsets1 - list_index_med.first_dset))
        VERIFY(0, 1, "h5_get_file_size");
    if((list_index_med.dsets1 - list_index_med.first_dset) !=
            (list_index_big.dsets1 - list_index_big.first_dset))
        VERIFY(0, 1, "h5_get_file_size");
    if((list_index_big.dsets1 - list_index_big.first_dset) >
            (btree_index.dsets1 - btree_index.first_dset))
        VERIFY(0, 1, "h5_get_file_size");
    if((btree_index.dsets1 - btree_index.first_dset) >=
            (norm_sizes.dsets1 - norm_sizes.first_dset))
        VERIFY(0, 1, "h5_get_file_size");


    /* Once another kind of each message has been written, the very small list
     * should convert into a B-tree.  Now the list should be smallest, then
     * the B-trees (although the converted B-tree file may be a little bigger),
     * then the normal file.  The largest list may or may not be bigger than
     * the normal file.
     */
    if(list_index_med.dsets2 >= btree_index.dsets2)
        VERIFY(list_index_med.dsets2, 1, "h5_get_file_size");
    if(btree_index.dsets2 > list_index_small.dsets2 * OVERHEAD_ALLOWED)
        VERIFY(btree_index.dsets2, list_index_small.dsets2, "h5_get_file_size");
    if(list_index_small.dsets2 >= norm_sizes.dsets2)
        VERIFY(btree_index.dsets2, 1, "h5_get_file_size");
    /* If the small list (now a B-tree) is bigger than the existing B-tree,
     * it shouldn't be much bigger.
     * It seems that the small lists tends to be pretty big anyway.  Allow
     * for it to have twice as much overhead.
     */
    if(list_index_small.dsets2 > btree_index.dsets2 * OVERHEAD_ALLOWED * OVERHEAD_ALLOWED)
        VERIFY(0, 1, "h5_get_file_size");
    /* The lists should have grown the least since they share messages and
     * have no extra overhead.  The normal file should have grown more than
     * either the lists or the B-tree.  The B-tree may not have grown more
     * than the lists, depending on whether it needed to split nodes or not.
     */
    if((list_index_med.dsets2 - list_index_med.dsets1) !=
            (list_index_big.dsets2 - list_index_big.dsets1))
        VERIFY(0, 1, "h5_get_file_size");
    if((list_index_big.dsets2 - list_index_big.dsets1) >
            (btree_index.dsets2 - btree_index.dsets1))
        VERIFY(0, 1, "h5_get_file_size");
    if((btree_index.dsets2 - btree_index.dsets1) >=
            (norm_sizes.dsets2 - norm_sizes.dsets1))
        VERIFY(0, 1, "h5_get_file_size");


    /* Interleaving the writes should have no effect on how the messages are
     * shared.  No new messages should be written to the indexes, so the
     * sohm files will only get a little bit bigger.
     */
    if(list_index_med.interleaved >= btree_index.interleaved)
        VERIFY(0, 1, "h5_get_file_size");
    if(btree_index.interleaved > list_index_small.interleaved * OVERHEAD_ALLOWED)
        VERIFY(btree_index.interleaved, list_index_small.interleaved, "h5_get_file_size");
    if(list_index_small.interleaved >= norm_sizes.interleaved)
        VERIFY(0, 1, "h5_get_file_size");
    /* The lists should still have grown the same amount.  The converted
     * B-tree shouldn't have grown more than the index that was originally
     * a B-tree (although it might have grown less if there was extra free
     * space within the file).
     */
    if((list_index_med.interleaved - list_index_med.dsets2) !=
            (list_index_big.interleaved - list_index_big.dsets2))
        VERIFY(0, 1, "h5_get_file_size");
    if((list_index_big.interleaved - list_index_big.dsets2) >
            (btree_index.interleaved - btree_index.dsets2))
        VERIFY(0, 1, "h5_get_file_size");
    if((list_index_small.interleaved - list_index_small.dsets2) >
            (btree_index.interleaved - btree_index.dsets2))
        VERIFY(0, 1, "h5_get_file_size");
    if((btree_index.interleaved - btree_index.dsets2) >=
            (norm_sizes.interleaved - norm_sizes.dsets2))
        VERIFY(0, 1, "h5_get_file_size");

    /* After many attributes have been written, both the small and medium lists
     * should have become B-trees and be about the same size as the index
     * that started as a B-tree.
     * Add in OVERHEAD_ALLOWED as a fudge factor here, since the allocation
     * of file space can be hard to predict.

     */
    if(btree_index.attrs1 > list_index_small.attrs1 * OVERHEAD_ALLOWED)
        VERIFY(btree_index.attrs1, list_index_small.attrs1, "h5_get_file_size");
    if(btree_index.attrs1 > list_index_med.attrs1 * OVERHEAD_ALLOWED)
        VERIFY(0, 1, "h5_get_file_size");
    if(list_index_med.attrs1 > btree_index.attrs1 * OVERHEAD_ALLOWED)
        VERIFY(0, 1, "h5_get_file_size");
    if(list_index_small.attrs1 > btree_index.attrs1 * OVERHEAD_ALLOWED)
        VERIFY(0, 1, "h5_get_file_size");
    /* Neither of the converted lists should be too much bigger than
     * the index that was originally a B-tree.
     */
    if(list_index_small.attrs1 > btree_index.attrs1 * OVERHEAD_ALLOWED)
        VERIFY(0, 1, "h5_get_file_size");
    if(list_index_med.attrs1 > btree_index.attrs1 * OVERHEAD_ALLOWED)
        VERIFY(0, 1, "h5_get_file_size");
    /* The "normal" file should have had less overhead, so should gain less
     * size than any of the other indexes since none of these attribute
     * messages could be shared.  The large list should have gained
     * less overhead than the B-tree indexes.
     */
    if((norm_sizes.attrs1 - norm_sizes.interleaved) >=
            (list_index_big.attrs1 - list_index_big.interleaved))
        VERIFY(0, 1, "h5_get_file_size");
    if((list_index_big.attrs1 - list_index_big.interleaved) >=
            (list_index_small.attrs1 - list_index_small.interleaved))
        VERIFY(0, 1, "h5_get_file_size");

    /* Give it some overhead (for checkin to move messages into continuation message) */
    if((list_index_small.attrs1 - list_index_small.interleaved) >
            ((btree_index.attrs1 - btree_index.interleaved) * OVERHEAD_ALLOWED))
        VERIFY(0, 1, "h5_get_file_size");


    /* Writing another copy of each attribute shouldn't change the ordering
     * of sizes.  The big list index is still too big to be smaller than a
     * normal file.  The B-tree indexes should all be about the same size.
     */
    if(btree_index.attrs2 > list_index_small.attrs2 * OVERHEAD_ALLOWED)
        VERIFY(btree_index.attrs2, list_index_small.attrs2, "h5_get_file_size");
    if(list_index_small.attrs2 > btree_index.attrs2 * OVERHEAD_ALLOWED)
        VERIFY(0, 1, "h5_get_file_size");
    if(btree_index.attrs2 > list_index_med.attrs2 * OVERHEAD_ALLOWED)
        VERIFY(0, 1, "h5_get_file_size");
    if(list_index_med.attrs2 > btree_index.attrs2 * OVERHEAD_ALLOWED)
        VERIFY(0, 1, "h5_get_file_size");
    if(list_index_med.attrs2 >= norm_sizes.attrs2)
        VERIFY(0, 1, "h5_get_file_size");
    if(list_index_big.attrs2 >= norm_sizes.attrs2)
        VERIFY(0, 1, "h5_get_file_size");
    /* All of the B-tree indexes should have gained about the same amount
     * of space; at least as much as the list index and less than a normal
     * file.
     */
    if((list_index_small.attrs2 - list_index_small.attrs1) >
            (btree_index.attrs2 - btree_index.attrs1))
        VERIFY(0, 1, "h5_get_file_size");
    if((list_index_med.attrs2 - list_index_med.attrs1) >
            (btree_index.attrs2 - btree_index.attrs1))
        VERIFY(0, 1, "h5_get_file_size");
    if((list_index_big.attrs2 - list_index_big.attrs1) >
            (list_index_med.attrs2 - list_index_med.attrs1))
        VERIFY(0, 1, "h5_get_file_size");
    if((btree_index.attrs2 - btree_index.attrs1) >=
            (norm_sizes.attrs2 - norm_sizes.attrs1))
        VERIFY(0, 1, "h5_get_file_size");

    /* Done checking the first few files that use a single index. */


    /* Start comparing other kinds of files with these "standard"
     * one-index files
     */

    /* Check files with multiple indexes. */
    /* These files should be larger when first created than one-index
     * files.
     */
    if(mult_index_med.empty_size <= list_index_med.empty_size)
        VERIFY(0, 1, "h5_get_file_size");
    if(mult_index_btree.empty_size != mult_index_med.empty_size)
        VERIFY(0, 1, "h5_get_file_size");

    /* When the first dataset is written, they should grow quite a bit as
     * many different indexes must be created.
     */
    if((mult_index_med.first_dset - mult_index_med.empty_size) <=
            (list_index_med.first_dset - list_index_med.empty_size))
        VERIFY(0, 1, "h5_get_file_size");
    if((mult_index_btree.first_dset - mult_index_btree.empty_size) <=
            (btree_index.first_dset - btree_index.empty_size))
        VERIFY(0, 1, "h5_get_file_size");

    /* When the second dataset is written, they should grow less as
     * some extra heap space is allocated, but no more indices.
     */
    if((mult_index_med.second_dset - mult_index_med.first_dset) >
            (mult_index_med.first_dset - mult_index_med.empty_size))
        VERIFY(0, 1, "h5_get_file_size");
    if((list_index_med.second_dset - list_index_med.first_dset) >
            (list_index_med.first_dset - list_index_med.empty_size))
        VERIFY(0, 1, "h5_get_file_size");
    if((mult_index_btree.second_dset - mult_index_btree.first_dset) >
            (mult_index_btree.first_dset - mult_index_btree.empty_size))
        VERIFY(0, 1, "h5_get_file_size");
    if((btree_index.second_dset - btree_index.first_dset) >
            (btree_index.first_dset - btree_index.empty_size))
        VERIFY(0, 1, "h5_get_file_size");

    /* And the size delta for the second dataset is less in files with only
     *  one index.
     */
    if((mult_index_med.second_dset - mult_index_med.first_dset) <=
            (list_index_med.second_dset - list_index_med.first_dset))
        VERIFY(0, 1, "h5_get_file_size");
    if((mult_index_btree.first_dset - mult_index_btree.empty_size) <=
            (btree_index.first_dset - btree_index.empty_size))
        VERIFY(0, 1, "h5_get_file_size");

    /* Once that initial overhead is out of the way and the lists/btrees
     * have been created, files with more than one index should grow at
     * the same rate or slightly faster than files with just one index
     * and one heap.
     */
     if((mult_index_med.dsets1 - mult_index_med.second_dset) !=
            (list_index_med.dsets1 - list_index_med.second_dset))
        VERIFY((mult_index_med.dsets1 - mult_index_med.second_dset), (list_index_med.dsets1 - list_index_med.second_dset), "h5_get_file_size");
     if((mult_index_btree.dsets1 - mult_index_btree.second_dset) !=
            (btree_index.dsets1 - btree_index.second_dset))
        VERIFY((mult_index_btree.dsets1 - mult_index_btree.second_dset), (btree_index.dsets1 - btree_index.second_dset), "h5_get_file_size");

     if((mult_index_med.dsets2 - mult_index_med.dsets1) >
            (list_index_med.dsets2 - list_index_med.dsets1) * OVERHEAD_ALLOWED)
        VERIFY(0, 1, "h5_get_file_size");
     if((mult_index_btree.dsets2 - mult_index_btree.dsets1) >
            (btree_index.dsets2 - btree_index.dsets1) * OVERHEAD_ALLOWED)
        VERIFY(0, 1, "h5_get_file_size");

     if((mult_index_med.interleaved - mult_index_med.dsets2) >
            (list_index_med.interleaved - list_index_med.dsets2) * OVERHEAD_ALLOWED)
        VERIFY(0, 1, "h5_get_file_size");
     if((mult_index_btree.interleaved - mult_index_btree.dsets2) >
            (btree_index.interleaved - btree_index.dsets2) * OVERHEAD_ALLOWED)
        VERIFY(0, 1, "h5_get_file_size");

    /* When all the attributes are added, only the index holding attributes
     * will become a B-tree.  Skip the interleaved to attrs1 interval when
     * this happens because it's hard to predict exactly how much space this
     * will take.
     */
     if((mult_index_med.attrs2 - mult_index_med.attrs1) !=
            (list_index_med.attrs2 - list_index_med.attrs1))
        VERIFY(0, 1, "h5_get_file_size");
     if((mult_index_btree.attrs2 - mult_index_btree.attrs1) !=
            (btree_index.attrs2 - btree_index.attrs1))
        VERIFY(0, 1, "h5_get_file_size");

    /* The final file size for both of the multiple index files should be
     * smaller than a normal file but bigger than any of the one-index files.
     */
     if(mult_index_med.attrs2 >= norm_sizes.attrs2)
        VERIFY(0, 1, "h5_get_file_size");
     if(mult_index_btree.attrs2 >= norm_sizes.attrs2)
        VERIFY(0, 1, "h5_get_file_size");
     if(mult_index_med.attrs2 * OVERHEAD_ALLOWED < btree_index.attrs2)
        VERIFY(0, 1, "h5_get_file_size");
     if(mult_index_btree.attrs2 * OVERHEAD_ALLOWED < btree_index.attrs2)
        VERIFY(0, 1, "h5_get_file_size");


    /* Check files that don't share all messages. */
    /* These files have three indexes like the files above, so they should be
     * the same size when created.
     */
    if(share_some_med.empty_size != mult_index_med.empty_size)
        VERIFY(0, 1, "h5_get_file_size");
    if(share_some_med.empty_size != share_some_btree.empty_size)
        VERIFY(0, 1, "h5_get_file_size");

    /* When the first dataset is created, they should be not quite as big
     * as equivalent files that share all messages (since shared messages
     * have a little bit of overhead).
     */
    if(share_some_med.first_dset >= mult_index_med.first_dset)
        VERIFY(0, 1, "h5_get_file_size");
    if(share_some_btree.first_dset >= mult_index_btree.first_dset)
        VERIFY(0, 1, "h5_get_file_size");

    /* The files that share some should have a growth rate in between
     * files that share all messages and normal files
     */
    if((share_some_med.interleaved - share_some_med.first_dset) <=
            (mult_index_med.interleaved - mult_index_med.first_dset))
        VERIFY(0, 1, "h5_get_file_size");
    if((share_some_med.interleaved - share_some_med.first_dset) >=
            (norm_sizes.interleaved - norm_sizes.first_dset))
        VERIFY(0, 1, "h5_get_file_size");
    if((share_some_btree.interleaved - share_some_btree.first_dset) <=
            (mult_index_btree.interleaved - mult_index_btree.first_dset))
        VERIFY(0, 1, "h5_get_file_size");
    if((share_some_btree.interleaved - share_some_btree.first_dset) >=
            (norm_sizes.interleaved - norm_sizes.first_dset))
        VERIFY(0, 1, "h5_get_file_size");


    /* Check the file that only stored gigantic messages in its second
     * index.  Since no messages were that big, it should be identical
     * to the file with an empty index.
     */
    if(share_some_btree.empty_size != share_some_toobig_index.empty_size)
        VERIFY(0, 1, "h5_get_file_size");
    if(share_some_btree.first_dset != share_some_toobig_index.first_dset)
        VERIFY(0, 1, "h5_get_file_size");
    if(share_some_btree.dsets1 != share_some_toobig_index.dsets1)
        VERIFY(0, 1, "h5_get_file_size");
    if(share_some_btree.dsets2 != share_some_toobig_index.dsets2)
        VERIFY(0, 1, "h5_get_file_size");
    if(share_some_btree.interleaved != share_some_toobig_index.interleaved)
        VERIFY(0, 1, "h5_get_file_size");
    if(share_some_btree.attrs1 != share_some_toobig_index.attrs1)
        VERIFY(0, 1, "h5_get_file_size");
    if(share_some_btree.attrs2 != share_some_toobig_index.attrs2)
        VERIFY(0, 1, "h5_get_file_size");


    /* Check the file that shares even very tiny messages.  Once messages
     * are written to it, it should gain a little space from sharing the
     * messages and lose a little space to overhead so that it's just slightly
     * smaller than a file that doesn't share tiny messages.
     * If the overhead increases or the size of messages decreases, these
     * numbers may be off.
     */
    if(share_tiny_index.empty_size != type_space_index.empty_size)
        VERIFY(share_tiny_index.empty_size, type_space_index.empty_size, "h5_get_file_size");

    if(share_tiny_index.first_dset >= type_space_index.first_dset * OVERHEAD_ALLOWED)
        VERIFY(share_tiny_index.first_dset, type_space_index.first_dset, "h5_get_file_size");
    if(share_tiny_index.first_dset < type_space_index.first_dset)
        VERIFY(0, 1, "h5_get_file_size");

    if(share_tiny_index.second_dset >= type_space_index.second_dset)
        VERIFY(share_tiny_index.second_dset, type_space_index.second_dset, "h5_get_file_size");
    if(share_tiny_index.second_dset * OVERHEAD_ALLOWED < type_space_index.second_dset)
        VERIFY(0, 1, "h5_get_file_size");

    if(share_tiny_index.dsets1 >= type_space_index.dsets1)
        VERIFY(0, 1, "h5_get_file_size");
    if(share_tiny_index.dsets1 * OVERHEAD_ALLOWED < type_space_index.dsets1)
        VERIFY(0, 1, "h5_get_file_size");

    if(share_tiny_index.dsets2 >= type_space_index.dsets2)
        VERIFY(0, 1, "h5_get_file_size");
    if(share_tiny_index.dsets2 * OVERHEAD_ALLOWED < type_space_index.dsets2)
        VERIFY(0, 1, "h5_get_file_size");

    if(share_tiny_index.interleaved >= type_space_index.interleaved)
        VERIFY(0, 1, "h5_get_file_size");
    if(share_tiny_index.interleaved * OVERHEAD_ALLOWED < type_space_index.interleaved)
        VERIFY(0, 1, "h5_get_file_size");

    if(share_tiny_index.attrs1 >= type_space_index.attrs1)
        VERIFY(0, 1, "h5_get_file_size");
    if(share_tiny_index.attrs1 * OVERHEAD_ALLOWED < type_space_index.attrs1)
        VERIFY(0, 1, "h5_get_file_size");

    if(share_tiny_index.attrs2 >= type_space_index.attrs2)
        VERIFY(0, 1, "h5_get_file_size");
    if(share_tiny_index.attrs2 * OVERHEAD_ALLOWED < type_space_index.attrs2)
        VERIFY(0, 1, "h5_get_file_size");
} /* end test_sohm_size2() */


/*-------------------------------------------------------------------------
 * Function:    delete_helper_write
 *
 * Purpose:     Creates a dataset and attribute in file FILE_ID using value X
 *              in the DSPACE_ID and DCPL_ID arrays.
 *
 * Programmer:  James Laird
 *              Tuesday, December 19, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void delete_helper_write(hid_t file_id, hid_t *dspace_id, hid_t *dcpl_id, int x)
{
    hid_t dset_id = -1;
    hid_t attr_id = -1;
    char wdata;
    herr_t ret;

    /* Create dataset */
    dset_id = H5Dcreate2(file_id, DSETNAME[x], H5T_NATIVE_CHAR, dspace_id[x], H5P_DEFAULT, dcpl_id[x], H5P_DEFAULT);
    CHECK_I(dset_id, "H5Dcreate2");

    /* Write data to dataset */
    wdata = x + 'a';
    ret = H5Dwrite(dset_id, H5T_NATIVE_CHAR, dspace_id[x], dspace_id[x], H5P_DEFAULT, &wdata);
    CHECK_I(ret, "H5Dwrite");

    /* Create an attribute on the dataset. */
    attr_id = H5Acreate2(dset_id, "attr_name", H5T_NATIVE_CHAR, dspace_id[x], H5P_DEFAULT, H5P_DEFAULT);
    CHECK_I(attr_id, "H5Acreate2");

    /* Write to attribute */
    ret = H5Awrite(attr_id, H5T_NATIVE_CHAR, &wdata);
    CHECK_I(ret, "H5Awrite");

    ret = H5Aclose(attr_id);
    CHECK_I(ret, "H5Aclose");
    ret = H5Dclose(dset_id);
    CHECK_I(ret, "H5Dclose");
}


/*-------------------------------------------------------------------------
 * Function:    delete_helper_read
 *
 * Purpose:     Checks the value of the dataset and attribute created by
 *              delete_helper_write.
 *
 * Programmer:  James Laird
 *              Tuesday, December 19, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void delete_helper_read(hid_t file_id, hid_t *dspace_id, int x)
{
    hid_t dset_id = -1;
    hid_t attr_id = -1;
    char rdata;
    herr_t ret;

    /* Open dataset */
    dset_id = H5Dopen2(file_id, DSETNAME[x], H5P_DEFAULT);
    CHECK_I(dset_id, "H5Dopen2");

    /* Read */
    rdata = '\0';
    ret = H5Dread(dset_id, H5T_NATIVE_CHAR, dspace_id[x], dspace_id[x], H5P_DEFAULT, &rdata);
    CHECK_I(ret, "H5Dread");
    VERIFY(rdata, (x + 'a'), "H5Dread");

    /* Open attribute */
    attr_id = H5Aopen(dset_id, "attr_name", H5P_DEFAULT);
    CHECK_I(attr_id, "H5Aopen");

    /* Read */
    rdata = '\0';
    ret = H5Aread(attr_id, H5T_NATIVE_CHAR, &rdata);
    CHECK_I(ret, "H5Dread");
    VERIFY(rdata, (x + 'a'), "H5Dread");

    /* Cleanup */
    ret = H5Aclose(attr_id);
    CHECK_I(ret, "H5Aclose");
    ret = H5Dclose(dset_id);
    CHECK_I(ret, "H5Dclose");
}


/*-------------------------------------------------------------------------
 * Function:    delete_helper
 *
 * Purpose:     Creates some shared messages, deletes them, and creates some
 *              more messages.  The second batch of messages should use the
 *              space freed by the first batch, so should be about the same
 *              size as a file that never had the first batch of messages
 *              created.
 *
 *              FCPL_ID is the file creation property list to use.
 *              DSPACE_ID and DCPL_ID are arrays of different dataspaces
 *              and property lists with filter pipelines used to create the
 *              messages.
 *
 * Programmer:  James Laird
 *              Tuesday, December 19, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void delete_helper(hid_t fcpl_id, hid_t *dspace_id, hid_t *dcpl_id)
{
    hid_t file_id=-1;
    int x;
    h5_stat_size_t norm_filesize;
    h5_stat_size_t deleted_filesize;
    herr_t ret;

    /* Get the size of a "normal" file with no deleted messages */
    file_id = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl_id, H5P_DEFAULT);
    CHECK_I(file_id, "H5Fcreate");

    /* Create batch of messages in the file starting at message 2 */
    for(x=HALF_DELETE_NUM_MESGS; x<DELETE_NUM_MESGS; ++x) {
        delete_helper_write(file_id, dspace_id, dcpl_id, x);
    }

    /* Check that messages can be read */
    for(x=HALF_DELETE_NUM_MESGS; x<DELETE_NUM_MESGS; ++x) {
        delete_helper_read(file_id, dspace_id, x);
    }

    /* Close file and get filesize */
    ret = H5Fclose(file_id);
    CHECK_I(ret, "H5Fclose");
    norm_filesize = h5_get_file_size(FILENAME, H5P_DEFAULT);

    /* Create a new file with messages 0 to (HALF_DELETE_NUM_MESGS - 1) */
    file_id = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl_id, H5P_DEFAULT);
    CHECK_I(file_id, "H5Fcreate");

    for(x=0; x<HALF_DELETE_NUM_MESGS; ++x) {
        delete_helper_write(file_id, dspace_id, dcpl_id, x);
    }

    /* Verify each dataset, then delete it (which should delete
     * its shared messages as well
     */
    for(x=0; x<HALF_DELETE_NUM_MESGS; ++x) {
        delete_helper_read(file_id, dspace_id, x);
        ret = H5Ldelete(file_id, DSETNAME[x], H5P_DEFAULT);
        CHECK_I(ret, "H5Ldelete");
    }

    /* The file is now empty.  Write and verify the second batch of messages
     * again.
     */
    for(x=HALF_DELETE_NUM_MESGS; x<DELETE_NUM_MESGS; ++x) {
        delete_helper_write(file_id, dspace_id, dcpl_id, x);
    }
    for(x=HALF_DELETE_NUM_MESGS; x<DELETE_NUM_MESGS; ++x) {
        delete_helper_read(file_id, dspace_id, x);
    }

    /* Close file and get filesize */
    ret = H5Fclose(file_id);
    CHECK_I(ret, "H5Fclose");
    deleted_filesize = h5_get_file_size(FILENAME, H5P_DEFAULT);

    /* The two filesizes should be almost the same */
    if(norm_filesize > deleted_filesize * OVERHEAD_ALLOWED)
        VERIFY(norm_filesize, deleted_filesize, "h5_get_file_size");
    if(deleted_filesize > norm_filesize * OVERHEAD_ALLOWED)
        VERIFY(deleted_filesize, norm_filesize, "h5_get_file_size");
}


/*-------------------------------------------------------------------------
 * Function:    test_sohm_delete
 *
 * Purpose:     Tests shared object header message deletion.
 *
 *              Creates lots of shared messages, then ensures that they
 *              can be deleted without corrupting the remaining messages.
 *              Also checks that indexes convert from B-trees back into
 *              lists.
 *
 * Programmer:  James Laird
 *              Tuesday, December 19, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
test_sohm_delete(void)
{
    hid_t fcpl_id;
    /* We'll use dataspaces and filter pipelines for this test.
     *  Create a number of distinct messages of each type.
     */
    hid_t dspace_id[DELETE_NUM_MESGS] = {0};
    hid_t dcpl_id[DELETE_NUM_MESGS] = {0};
    unsigned u;
    int x;
    hsize_t dims[] = DELETE_DIMS;
    herr_t ret;

    /* Create a number of different dataspaces.
     * For simplicity, each dataspace has only one element.
     */
   for(u = 0; u < DELETE_NUM_MESGS; ++u) {
        dspace_id[u] = H5Screate_simple((int)(u + 1), dims, dims);
        CHECK_I(dspace_id[u], "H5Screate_simple");
    } /* end for */

    /* Create a number of different filter pipelines. */
    dcpl_id[0] = H5Pcreate(H5P_DATASET_CREATE);
    CHECK_I(dcpl_id[0], "H5Pcreate");

    ret = H5Pset_chunk(dcpl_id[0], 1, dims);
    CHECK_I(ret, "H5Pset_chunk");
    ret = H5Pset_shuffle(dcpl_id[0]);
    CHECK_I(ret, "H5Pset_shuffle");

    for(u = 1; u < DELETE_NUM_MESGS; u += 2) {
        dcpl_id[u] = H5Pcopy(dcpl_id[u - 1]);
        CHECK_I(dcpl_id[u], "H5Pcopy");
        ret = H5Pset_chunk(dcpl_id[u], (int)(u + 1), dims);
        CHECK_I(ret, "H5Pset_chunk");
        ret = H5Pset_deflate(dcpl_id[u], 1);
        CHECK_I(ret, "H5Pset_deflate");

        dcpl_id[u + 1] = H5Pcopy(dcpl_id[u]);
        CHECK_I(dcpl_id[u + 1], "H5Pcopy");
        ret = H5Pset_chunk(dcpl_id[u + 1], (int)(u + 2), dims);
        CHECK_I(ret, "H5Pset_chunk");
        ret = H5Pset_shuffle(dcpl_id[u + 1]);
        CHECK_I(ret, "H5Pset_shuffle");
    } /* end for */

    /* Create an fcpl where all messages are shared in the same index */
    fcpl_id = H5Pcreate(H5P_FILE_CREATE);
    CHECK_I(fcpl_id, "H5Pcreate");
    ret = H5Pset_shared_mesg_nindexes(fcpl_id, 1);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");
    ret = H5Pset_shared_mesg_index(fcpl_id, 0, H5O_SHMESG_ALL_FLAG, 16);
    CHECK_I(ret, "H5Pset_shared_mesg_index");


    /* Use big list indexes */
    ret = H5Pset_shared_mesg_phase_change(fcpl_id, 4 * DELETE_NUM_MESGS, 0);
    CHECK_I(ret, "H5Pset_shared_mesg_phase_change");

    /* Test that messages can be created and deleted properly */
    delete_helper(fcpl_id, dspace_id, dcpl_id);

    /* Use B-tree indexes */
    ret = H5Pset_shared_mesg_phase_change(fcpl_id, 0, 0);
    CHECK_I(ret, "H5Pset_shared_mesg_phase_change");

    delete_helper(fcpl_id, dspace_id, dcpl_id);


    /* Use small list indexes that will convert from lists to B-trees and back */
    ret = H5Pset_shared_mesg_phase_change(fcpl_id, HALF_DELETE_NUM_MESGS, HALF_DELETE_NUM_MESGS - 1);
    CHECK_I(ret, "H5Pset_shared_mesg_phase_change");

    delete_helper(fcpl_id, dspace_id, dcpl_id);


    /* Use two indexes */
    ret = H5Pset_shared_mesg_nindexes(fcpl_id, 2);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");
    ret = H5Pset_shared_mesg_index(fcpl_id, 0, H5O_SHMESG_SDSPACE_FLAG | H5O_SHMESG_ATTR_FLAG, 16);
    CHECK_I(ret, "H5Pset_shared_mesg_index");
    ret = H5Pset_shared_mesg_index(fcpl_id, 1, H5O_SHMESG_DTYPE_FLAG, 16);
    CHECK_I(ret, "H5Pset_shared_mesg_index");

    /* Use big list indexes */
    ret = H5Pset_shared_mesg_phase_change(fcpl_id, 5000, 0);
    CHECK_I(ret, "H5Pset_shared_mesg_phase_change");


    /* Use B-tree indexes */
    ret = H5Pset_shared_mesg_phase_change(fcpl_id, 0, 0);
    CHECK_I(ret, "H5Pset_shared_mesg_phase_change");

    delete_helper(fcpl_id, dspace_id, dcpl_id);


    /* Set phase change values so that one index converts to a B-tree and one doesn't */
    ret = H5Pset_shared_mesg_phase_change(fcpl_id, HALF_DELETE_NUM_MESGS + 1, 0);
    CHECK_I(ret, "H5Pset_shared_mesg_phase_change");

    delete_helper(fcpl_id, dspace_id, dcpl_id);


    /* Test with varying message sizes (ideally, so some messages are too
     * small to be written but some are big enough that they are still written
     */
    ret = H5Pset_shared_mesg_nindexes(fcpl_id, 1);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");
    for(u = DELETE_MIN_MESG_SIZE; u <= DELETE_MAX_MESG_SIZE; u += 10) {
        ret = H5Pset_shared_mesg_index(fcpl_id, 0, H5O_SHMESG_ALL_FLAG, u);
        CHECK_I(ret, "H5Pset_shared_mesg_phase_change");
        delete_helper(fcpl_id, dspace_id, dcpl_id);
    } /* end for */

    /* Cleanup */
    ret = H5Pclose(fcpl_id);
    CHECK_I(ret, "H5Pclose");

    for(x = DELETE_NUM_MESGS - 1; x >= 0; --x) {
        ret = H5Sclose(dspace_id[x]);
        CHECK_I(ret, "H5Sclose");
        ret = H5Pclose(dcpl_id[x]);
        CHECK_I(ret, "H5Pclose");
    } /* end for */
} /* end test_sohm_delete() */


/*-------------------------------------------------------------------------
 * Function:    test_sohm_delete_revert_helper
 *
 * Purpose:     Tests that shared object header message deletion returns
 *              the file to its previous state using the supplied FCPL.
 *
 *              Creates shared messages and then deletes them.  Ensures
 *              that the file has not grown in size.
 *
 * Programmer:  James Laird
 *              Wednesday, January 3, 2007
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_sohm_delete_revert_helper(hid_t fcpl_id)
{
    hid_t file_id;
    hid_t dspace_id;
    hid_t dset_id;
    hsize_t dims[1] = {1};
    h5_stat_size_t initial_filesize, deleted_filesize;
    int old_nerrs;              /* Number of errors when entering this routine */
    herr_t ret;

    /* Retrieve the current # of reported errors */
    old_nerrs = GetTestNumErrs();

    /* Create a dataspace for later */
    dspace_id = H5Screate_simple(1, dims, dims);
    CHECK_I(dspace_id, "H5Screate_simple");

    /* Create a file using the FCPL supplied*/
    file_id = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl_id, H5P_DEFAULT);
    CHECK_I(file_id, "H5Fcreate");

    /* Close the file and get its size */
    ret = H5Fclose(file_id);
    CHECK_I(ret, "H5Fclose");
    initial_filesize = h5_get_file_size(FILENAME, H5P_DEFAULT);


    /* Re-create the file and create a dataset in it */
    file_id = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl_id, H5P_DEFAULT);
    CHECK_I(file_id, "H5Fcreate");

    dset_id = H5Dcreate2(file_id, "dset", H5T_NATIVE_SHORT, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK_I(dset_id, "H5Dcreate2");

    /* Close the dataset and delete it */
    ret = H5Dclose(dset_id);
    CHECK_I(ret, "H5Dclose");
    ret = H5Ldelete(file_id, "dset", H5P_DEFAULT);
    CHECK_I(ret, "H5Ldelete");

    /* Close the file and get its size */
    ret = H5Fclose(file_id);
    CHECK_I(ret, "H5Fclose");
    deleted_filesize = h5_get_file_size(FILENAME, H5P_DEFAULT);

    VERIFY(deleted_filesize, initial_filesize, "h5_get_file_size");


    /* Repeat, creating two datasets in the file */
    file_id = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl_id, H5P_DEFAULT);
    CHECK_I(file_id, "H5Fcreate");

    /* Create and close the first dataset */
    dset_id = H5Dcreate2(file_id, "dset", H5T_NATIVE_SHORT, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK_I(dset_id, "H5Dcreate2");
    ret = H5Dclose(dset_id);
    CHECK_I(ret, "H5Dclose");

    /* Create and close the second.  These messages should be shared */
    dset_id = H5Dcreate2(file_id, "dset2", H5T_NATIVE_SHORT, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK_I(dset_id, "H5Dcreate2");
    ret = H5Dclose(dset_id);
    CHECK_I(ret, "H5Dclose");

    /* Delete both datasets */
    ret = H5Ldelete(file_id, "dset", H5P_DEFAULT);
    CHECK_I(ret, "H5Ldelete");
    ret = H5Ldelete(file_id, "dset2", H5P_DEFAULT);
    CHECK_I(ret, "H5Ldelete");

    /* Close the file and get its size */
    ret = H5Fclose(file_id);
    CHECK_I(ret, "H5Fclose");
    deleted_filesize = h5_get_file_size(FILENAME, H5P_DEFAULT);

    VERIFY(deleted_filesize, initial_filesize, "h5_get_file_size");


    /* Cleanup */
    ret = H5Sclose(dspace_id);
    CHECK_I(ret, "H5Sclose");

    /* Retrieve current # of errors */
    if(old_nerrs == GetTestNumErrs())
        return(0);
    else
        return(-1);
} /* test_sohm_delete_revert_helper() */


/*-------------------------------------------------------------------------
 * Function:    test_sohm_delete_revert
 *
 * Purpose:     Calls test_sohm_delete_revert_helper with different FCPLs.
 *
 * Programmer:  James Laird
 *              Wednesday, January 3, 2007
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
test_sohm_delete_revert(void)
{
    hid_t fcpl_id;
    herr_t ret;

    /* Create an fcpl with messages in two indexes */
    fcpl_id = H5Pcreate(H5P_FILE_CREATE);
    CHECK_I(fcpl_id, "H5Pcreate");
    ret = H5Pset_shared_mesg_nindexes(fcpl_id, 2);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");
    ret = H5Pset_shared_mesg_index(fcpl_id, 0, H5O_SHMESG_DTYPE_FLAG, 10);
    CHECK_I(ret, "H5Pset_shared_mesg_index");
    ret = H5Pset_shared_mesg_index(fcpl_id, 1, H5O_SHMESG_SDSPACE_FLAG, 10);
    CHECK_I(ret, "H5Pset_shared_mesg_index");

    /* Call the helper function to test this FCPL. */
    ret = test_sohm_delete_revert_helper(fcpl_id);
    CHECK_I(ret, "test_sohm_delete_revert_helper");

    /* Try using B-trees */
    ret = H5Pset_shared_mesg_phase_change(fcpl_id, 0, 0);
    CHECK_I(ret, "H5Pset_shared_mesg_phase_change");
    ret = test_sohm_delete_revert_helper(fcpl_id);
    CHECK_I(ret, "test_sohm_delete_revert_helper");


    /* Try sharing all messages */
    ret = H5Pset_shared_mesg_nindexes(fcpl_id, 1);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");
    ret = H5Pset_shared_mesg_index(fcpl_id, 0, H5O_SHMESG_ALL_FLAG, 10);
    CHECK_I(ret, "H5Pset_shared_mesg_index");
    ret = H5Pset_shared_mesg_phase_change(fcpl_id, 10, 5);
    CHECK_I(ret, "H5Pset_shared_mesg_phase_change");

    ret = test_sohm_delete_revert_helper(fcpl_id);
    CHECK_I(ret, "test_sohm_delete_revert_helper");

    /* Try using B-trees */
    ret = H5Pset_shared_mesg_phase_change(fcpl_id, 0, 0);
    CHECK_I(ret, "H5Pset_shared_mesg_phase_change");
    ret = test_sohm_delete_revert_helper(fcpl_id);
    CHECK_I(ret, "test_sohm_delete_revert_helper");

    /* There should be at least two messages in the test (datatype and
     * dataspace).  Use an index that will transition from a list to
     * a B-tree and back.
     */
    ret = H5Pset_shared_mesg_phase_change(fcpl_id, 1, 2);
    CHECK_I(ret, "H5Pset_shared_mesg_phase_change");
    ret = test_sohm_delete_revert_helper(fcpl_id);
    CHECK_I(ret, "test_sohm_delete_revert_helper");


    /* Try with shared messages enabled, but when messages are too big
     * to be shared.
     */
    ret = H5Pset_shared_mesg_index(fcpl_id, 0, H5O_SHMESG_ALL_FLAG, 35);
    CHECK_I(ret, "H5Pset_shared_mesg_phase_change");
    ret = test_sohm_delete_revert_helper(fcpl_id);
    CHECK_I(ret, "test_sohm_delete_revert_helper");

    ret = H5Pclose(fcpl_id);
    CHECK_I(ret, "H5Pclose");
}


/*-------------------------------------------------------------------------
 * Function:    test_sohm_extlink_helper
 *
 * Purpose:     Tests that a dataset created through an external link can
 *              be opened (that shared messages were created or not and
 *              were shared in the right file).
 *
 * Programmer:  James Laird
 *              Friday, December 22, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void test_sohm_extlink_helper(hid_t src_fcpl_id, hid_t dst_fcpl_id)
{
    hid_t src_file_id = -1;
    hid_t dst_file_id = -1;
    hid_t space_id = -1;
    hid_t dset_id = -1;
    hsize_t dims[] = {1, 1};
    herr_t ret;

    /* Create files */
    src_file_id = H5Fcreate(FILENAME_SRC, H5F_ACC_TRUNC, src_fcpl_id, H5P_DEFAULT);
    CHECK_I(src_file_id, "H5Fcreate");
    dst_file_id = H5Fcreate(FILENAME_DST, H5F_ACC_TRUNC, dst_fcpl_id, H5P_DEFAULT);
    CHECK_I(dst_file_id, "H5Fcreate");

    /* Create an external link from the source file to the destination file */
    ret = H5Lcreate_external(FILENAME_DST, "/", src_file_id, "ext_link", H5P_DEFAULT, H5P_DEFAULT);
    CHECK_I(ret, "H5Lcreate_external");

    /* Create a dataset through the external link */
    space_id = H5Screate_simple(2, dims, dims);
    CHECK_I(space_id, "H5Screate_simple");
    dset_id = H5Dcreate2(src_file_id, "ext_link/dataset", H5T_NATIVE_FLOAT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK_I(dset_id, "H5Dcreate2");

    /* Close the dataset and both files to make sure everything gets flushed
     * out of memory
     */
    ret = H5Dclose(dset_id);
    CHECK_I(ret, "H5Dclose");
    ret = H5Fclose(src_file_id);
    CHECK_I(ret, "H5Fclose");
    ret = H5Fclose(dst_file_id);
    CHECK_I(ret, "H5Fclose");

    /* Ensure that the dataset can be opened.  If the messages were written in
     * the wrong file, it'll be impossible to read the dataset's object
     * header.
     */
    dst_file_id = H5Fopen(FILENAME_DST, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK_I(dst_file_id, "H5Fopen");
    dset_id = H5Dopen2(dst_file_id, "dataset", H5P_DEFAULT);
    CHECK_I(dset_id, "H5Dopen2");

    /* Cleanup */
    ret = H5Dclose(dset_id);
    CHECK_I(ret, "H5Dclose");
    ret = H5Fclose(dst_file_id);
    CHECK_I(ret, "H5Fclose");
}


/*-------------------------------------------------------------------------
 * Function:    test_sohm_extlink
 *
 * Purpose:     Test creating SOHMs through external links (to make sure that
 *              they're created in the correct file).
 *
 * Programmer:  James Laird
 *              Friday, December 22, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
test_sohm_extlink(void)
{
    hid_t fcpl_id = -1;
    herr_t ret;

    /* Create fcpl */
    fcpl_id = H5Pcreate(H5P_FILE_CREATE);
    CHECK_I(fcpl_id, "H5Pcreate");
    ret = H5Pset_shared_mesg_nindexes(fcpl_id, 1);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");
    ret = H5Pset_shared_mesg_index(fcpl_id, 0, H5O_SHMESG_ALL_FLAG, 16);
    CHECK_I(ret, "H5Pset_shared_mesg_index");

    /* Test using external links when the source or destination file uses
     * shared messages
     */
    test_sohm_extlink_helper(fcpl_id, H5P_DEFAULT);
    test_sohm_extlink_helper(H5P_DEFAULT, fcpl_id);
    test_sohm_extlink_helper(fcpl_id, fcpl_id);
}


/*-------------------------------------------------------------------------
 * Function:    test_sohm_extend_dset_helper
 *
 * Purpose:     Tests extending a dataset's dataspace when sharing is
 *              enabled.
 *
 *              If close_reopen is TRUE, closes and reopens the file to
 *              ensure that data is correctly written to disk.
 *
 * Programmer:  James Laird
 *              Wednesday, January 10, 2007
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_sohm_extend_dset_helper(hid_t fcpl_id, hbool_t close_reopen)
{
    hid_t file_id = -1;
    hid_t orig_space_id = -1;
    hid_t space1_id, space2_id, space3_id;
    hid_t dcpl_id = -1;
    hid_t dset1_id, dset2_id, dset3_id;
    hsize_t dims1[] = {1, 2};
    hsize_t max_dims[] = {H5S_UNLIMITED, 2};
    hsize_t dims2[] = {5, 2};
    hsize_t out_dims[2];
    hsize_t out_maxdims[2];
    int x;
    int old_nerrs;              /* Number of errors when entering this routine */
    herr_t ret;

    /* Retrieve the current # of reported errors */
    old_nerrs = GetTestNumErrs();

    /* Create file */
    file_id = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl_id, H5P_DEFAULT);
    CHECK_I(file_id, "H5Fcreate");

    /* Create property list with chunking */
    dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
    CHECK_I(dcpl_id, "H5Pcreate");
    ret = H5Pset_chunk(dcpl_id, 2, dims1);
    CHECK_I(ret, "H5Pset_chunk");

    /* Create a dataspace and a dataset*/
    orig_space_id = H5Screate_simple(EXTEND_NDIMS, dims1, max_dims);
    CHECK_I(orig_space_id, "H5Screate_simple");
    dset1_id = H5Dcreate2(file_id, "dataset", H5T_NATIVE_LONG, orig_space_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
    CHECK_I(dset1_id, "H5Dcreate2");

    if(close_reopen) {
        /* If requested, close all open IDs and reopen them */
        ret = H5Dclose(dset1_id);
        CHECK_I(ret, "H5Dclose");
        ret = H5Fclose(file_id);
        CHECK_I(ret, "H5Fclose");

        file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
        CHECK_I(file_id, "H5Fopen");
        dset1_id = H5Dopen2(file_id, "dataset", H5P_DEFAULT);
        CHECK_I(dset1_id, "H5Dopen2");
    }

    /* Create another dataset starting with the same dataspace */
    dset2_id = H5Dcreate2(file_id, "dataset2", H5T_NATIVE_LONG, orig_space_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
    CHECK_I(dset2_id, "H5Dcreate2");

    if(close_reopen) {
        /* If requested, close all open IDs and reopen them */
        ret = H5Dclose(dset1_id);
        CHECK_I(ret, "H5Dclose");
        ret = H5Dclose(dset2_id);
        CHECK_I(ret, "H5Dclose");
        ret = H5Fclose(file_id);
        CHECK_I(ret, "H5Fclose");

        file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
        CHECK_I(file_id, "H5Fopen");
        dset1_id = H5Dopen2(file_id, "dataset", H5P_DEFAULT);
        CHECK_I(dset1_id, "H5Dopen2");
        dset2_id = H5Dopen2(file_id, "dataset2", H5P_DEFAULT);
        CHECK_I(dset2_id, "H5Dopen2");
    }

    /* Create a third dataset with the same dataspace */
    dset3_id = H5Dcreate2(file_id, "dataset3", H5T_NATIVE_LONG, orig_space_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
    CHECK_I(dset3_id, "H5Dcreate2");

    if(close_reopen) {
        /* If requested, close all open IDs and reopen them */
        ret = H5Dclose(dset1_id);
        CHECK_I(ret, "H5Dclose");
        ret = H5Dclose(dset2_id);
        CHECK_I(ret, "H5Dclose");
        ret = H5Dclose(dset3_id);
        CHECK_I(ret, "H5Dclose");
        ret = H5Fclose(file_id);
        CHECK_I(ret, "H5Fclose");

        file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
        CHECK_I(file_id, "H5Fopen");
        dset1_id = H5Dopen2(file_id, "dataset", H5P_DEFAULT);
        CHECK_I(dset1_id, "H5Dopen2");
        dset2_id = H5Dopen2(file_id, "dataset2", H5P_DEFAULT);
        CHECK_I(dset2_id, "H5Dopen2");
        dset3_id = H5Dopen2(file_id, "dataset3", H5P_DEFAULT);
        CHECK_I(dset3_id, "H5Dopen2");
    }

    /* Extend the first dataset */
    ret = H5Dset_extent(dset1_id, dims2);
    CHECK_I(ret, "H5Dset_extent");
    if(close_reopen) {
        /* If requested, close all open IDs and reopen them */
        ret = H5Dclose(dset1_id);
        CHECK_I(ret, "H5Dclose");
        ret = H5Dclose(dset2_id);
        CHECK_I(ret, "H5Dclose");
        ret = H5Dclose(dset3_id);
        CHECK_I(ret, "H5Dclose");
        ret = H5Fclose(file_id);
        CHECK_I(ret, "H5Fclose");

        file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
        CHECK_I(file_id, "H5Fopen");
        dset1_id = H5Dopen2(file_id, "dataset", H5P_DEFAULT);
        CHECK_I(dset1_id, "H5Dopen2");
        dset2_id = H5Dopen2(file_id, "dataset2", H5P_DEFAULT);
        CHECK_I(dset2_id, "H5Dopen2");
        dset3_id = H5Dopen2(file_id, "dataset3", H5P_DEFAULT);
        CHECK_I(dset3_id, "H5Dopen2");
    }

    /* Get the dataspaces from the datasets */
    space1_id = H5Dget_space(dset1_id);
    CHECK_I(space1_id, "H5Dget_space");
    space2_id = H5Dget_space(dset2_id);
    CHECK_I(space2_id, "H5Dget_space");
    space3_id = H5Dget_space(dset3_id);
    CHECK_I(space3_id, "H5Dget_space");

    /* Verify the dataspaces */
    ret = H5Sget_simple_extent_dims(space1_id, out_dims, out_maxdims);
    CHECK_I(ret, "H5Sget_simple_extent_dims");
    for(x=0; x<EXTEND_NDIMS; ++x) {
        VERIFY(out_dims[x], dims2[x], "H5Sget_simple_extent_dims");
        VERIFY(out_maxdims[x], max_dims[x], "H5Sget_simple_extent_dims");
    }

    ret = H5Sget_simple_extent_dims(space2_id, out_dims, out_maxdims);
    CHECK_I(ret, "H5Sget_simple_extent_dims");
    for(x=0; x<EXTEND_NDIMS; ++x) {
        VERIFY(out_dims[x], dims1[x], "H5Sget_simple_extent_dims");
        VERIFY(out_maxdims[x], max_dims[x], "H5Sget_simple_extent_dims");
    }

    ret = H5Sget_simple_extent_dims(space3_id, out_dims, out_maxdims);
    CHECK_I(ret, "H5Sget_simple_extent_dims");
    for(x=0; x<EXTEND_NDIMS; ++x) {
        VERIFY(out_dims[x], dims1[x], "H5Sget_simple_extent_dims");
        VERIFY(out_maxdims[x], max_dims[x], "H5Sget_simple_extent_dims");
    }

    /* Close all three dataspaces */
    ret = H5Sclose(space1_id);
    CHECK_I(ret, "H5Sclose");
    ret = H5Sclose(space2_id);
    CHECK_I(ret, "H5Sclose");
    ret = H5Sclose(space3_id);
    CHECK_I(ret, "H5Sclose");

    /* Extend the second dataset */
    ret = H5Dset_extent(dset2_id, dims2);
    CHECK_I(ret, "H5Dset_extent");
    if(close_reopen) {
        /* If requested, close all open IDs and reopen them */
        ret = H5Dclose(dset1_id);
        CHECK_I(ret, "H5Dclose");
        ret = H5Dclose(dset2_id);
        CHECK_I(ret, "H5Dclose");
        ret = H5Dclose(dset3_id);
        CHECK_I(ret, "H5Dclose");
        ret = H5Fclose(file_id);
        CHECK_I(ret, "H5Fclose");

        file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
        CHECK_I(file_id, "H5Fopen");
        dset1_id = H5Dopen2(file_id, "dataset", H5P_DEFAULT);
        CHECK_I(dset1_id, "H5Dopen2");
        dset2_id = H5Dopen2(file_id, "dataset2", H5P_DEFAULT);
        CHECK_I(dset2_id, "H5Dopen2");
        dset3_id = H5Dopen2(file_id, "dataset3", H5P_DEFAULT);
        CHECK_I(dset3_id, "H5Dopen2");
    }

    /* Get the dataspaces from the datasets */
    space1_id = H5Dget_space(dset1_id);
    CHECK_I(space1_id, "H5Dget_space");
    space2_id = H5Dget_space(dset2_id);
    CHECK_I(space2_id, "H5Dget_space");
    space3_id = H5Dget_space(dset3_id);
    CHECK_I(space3_id, "H5Dget_space");

    /* Verify the dataspaces */
    ret = H5Sget_simple_extent_dims(space1_id, out_dims, out_maxdims);
    CHECK_I(ret, "H5Sget_simple_extent_dims");
    for(x=0; x<EXTEND_NDIMS; ++x) {
        VERIFY(out_dims[x], dims2[x], "H5Sget_simple_extent_dims");
        VERIFY(out_maxdims[x], max_dims[x], "H5Sget_simple_extent_dims");
    }

    ret = H5Sget_simple_extent_dims(space2_id, out_dims, out_maxdims);
    CHECK_I(ret, "H5Sget_simple_extent_dims");
    for(x=0; x<EXTEND_NDIMS; ++x) {
        VERIFY(out_dims[x], dims2[x], "H5Sget_simple_extent_dims");
        VERIFY(out_maxdims[x], max_dims[x], "H5Sget_simple_extent_dims");
    }

    ret = H5Sget_simple_extent_dims(space3_id, out_dims, out_maxdims);
    CHECK_I(ret, "H5Sget_simple_extent_dims");
    for(x=0; x<EXTEND_NDIMS; ++x) {
        VERIFY(out_dims[x], dims1[x], "H5Sget_simple_extent_dims");
        VERIFY(out_maxdims[x], max_dims[x], "H5Sget_simple_extent_dims");
    }

    /* Close all three dataspaces */
    ret = H5Sclose(space1_id);
    CHECK_I(ret, "H5Sclose");
    ret = H5Sclose(space2_id);
    CHECK_I(ret, "H5Sclose");
    ret = H5Sclose(space3_id);
    CHECK_I(ret, "H5Sclose");

    /* Extend the third dataset */
    ret = H5Dset_extent(dset3_id, dims2);
    CHECK_I(ret, "H5Dset_extent");
    if(close_reopen) {
        /* If requested, close all open IDs and reopen them */
        ret = H5Dclose(dset1_id);
        CHECK_I(ret, "H5Dclose");
        ret = H5Dclose(dset2_id);
        CHECK_I(ret, "H5Dclose");
        ret = H5Dclose(dset3_id);
        CHECK_I(ret, "H5Dclose");
        ret = H5Fclose(file_id);
        CHECK_I(ret, "H5Fclose");

        file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
        CHECK_I(file_id, "H5Fopen");
        dset1_id = H5Dopen2(file_id, "dataset", H5P_DEFAULT);
        CHECK_I(dset1_id, "H5Dopen2");
        dset2_id = H5Dopen2(file_id, "dataset2", H5P_DEFAULT);
        CHECK_I(dset2_id, "H5Dopen2");
        dset3_id = H5Dopen2(file_id, "dataset3", H5P_DEFAULT);
        CHECK_I(dset3_id, "H5Dopen2");
    }

    /* Get the dataspaces from the datasets */
    space1_id = H5Dget_space(dset1_id);
    CHECK_I(space1_id, "H5Dget_space");
    space2_id = H5Dget_space(dset2_id);
    CHECK_I(space2_id, "H5Dget_space");
    space3_id = H5Dget_space(dset3_id);
    CHECK_I(space3_id, "H5Dget_space");

    /* Verify the dataspaces */
    ret = H5Sget_simple_extent_dims(space1_id, out_dims, out_maxdims);
    CHECK_I(ret, "H5Sget_simple_extent_dims");
    for(x=0; x<EXTEND_NDIMS; ++x) {
        VERIFY(out_dims[x], dims2[x], "H5Sget_simple_extent_dims");
        VERIFY(out_maxdims[x], max_dims[x], "H5Sget_simple_extent_dims");
    }

    ret = H5Sget_simple_extent_dims(space2_id, out_dims, out_maxdims);
    CHECK_I(ret, "H5Sget_simple_extent_dims");
    for(x=0; x<EXTEND_NDIMS; ++x) {
        VERIFY(out_dims[x], dims2[x], "H5Sget_simple_extent_dims");
        VERIFY(out_maxdims[x], max_dims[x], "H5Sget_simple_extent_dims");
    }

    ret = H5Sget_simple_extent_dims(space3_id, out_dims, out_maxdims);
    CHECK_I(ret, "H5Sget_simple_extent_dims");
    for(x=0; x<EXTEND_NDIMS; ++x) {
        VERIFY(out_dims[x], dims2[x], "H5Sget_simple_extent_dims");
        VERIFY(out_maxdims[x], max_dims[x], "H5Sget_simple_extent_dims");
    }

    /* Close all three dataspaces */
    ret = H5Sclose(space1_id);
    CHECK_I(ret, "H5Sclose");
    ret = H5Sclose(space2_id);
    CHECK_I(ret, "H5Sclose");
    ret = H5Sclose(space3_id);
    CHECK_I(ret, "H5Sclose");

    /* Close the datasets and file */
    ret = H5Dclose(dset1_id);
    CHECK_I(ret, "H5Dclose");
    ret = H5Dclose(dset2_id);
    CHECK_I(ret, "H5Dclose");
    ret = H5Dclose(dset3_id);
    CHECK_I(ret, "H5Dclose");
    ret = H5Fclose(file_id);
    CHECK_I(ret, "H5Fclose");



    /* Change the order in which datasets are extended to ensure that there
     * are no problems if a dataspace goes from being shared to not being
     * shared or vice versa.
     */
    file_id = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl_id, H5P_DEFAULT);
    CHECK_I(file_id, "H5Fcreate");
    dset1_id = H5Dcreate2(file_id, "dataset", H5T_NATIVE_LONG, orig_space_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
    CHECK_I(dset1_id, "H5Dcreate2");
    if(close_reopen) {
        /* If requested, close all open IDs and reopen them */
        ret = H5Dclose(dset1_id);
        CHECK_I(ret, "H5Dclose");
        ret = H5Fclose(file_id);
        CHECK_I(ret, "H5Fclose");

        file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
        CHECK_I(file_id, "H5Fopen");
        dset1_id = H5Dopen2(file_id, "dataset", H5P_DEFAULT);
        CHECK_I(dset1_id, "H5Dopen2");
    }

    /* Extend the first dataset */
    ret = H5Dset_extent(dset1_id, dims2);
    CHECK_I(ret, "H5Dset_extent");
    if(close_reopen) {
        /* If requested, close all open IDs and reopen them */
        ret = H5Dclose(dset1_id);
        CHECK_I(ret, "H5Dclose");
        ret = H5Fclose(file_id);
        CHECK_I(ret, "H5Fclose");

        file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
        CHECK_I(file_id, "H5Fopen");
        dset1_id = H5Dopen2(file_id, "dataset", H5P_DEFAULT);
        CHECK_I(dset1_id, "H5Dopen2");
    }

    /* Create the second dataset.  Its dataspace will be unshared and then
     * become shared when extended.
     */
    dset2_id = H5Dcreate2(file_id, "dataset2", H5T_NATIVE_LONG, orig_space_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
    CHECK_I(dset2_id, "H5Dcreate2");
    if(close_reopen) {
        /* If requested, close all open IDs and reopen them */
        ret = H5Dclose(dset1_id);
        CHECK_I(ret, "H5Dclose");
        ret = H5Dclose(dset2_id);
        CHECK_I(ret, "H5Dclose");
        ret = H5Fclose(file_id);
        CHECK_I(ret, "H5Fclose");

        file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
        CHECK_I(file_id, "H5Fopen");
        dset1_id = H5Dopen2(file_id, "dataset", H5P_DEFAULT);
        CHECK_I(dset1_id, "H5Dopen2");
        dset2_id = H5Dopen2(file_id, "dataset2", H5P_DEFAULT);
        CHECK_I(dset2_id, "H5Dopen2");
    }

    /* Extend the second dataset */
    ret = H5Dset_extent(dset2_id, dims2);
    CHECK_I(ret, "H5Dset_extent");
    if(close_reopen) {
        /* If requested, close all open IDs and reopen them */
        ret = H5Dclose(dset1_id);
        CHECK_I(ret, "H5Dclose");
        ret = H5Dclose(dset2_id);
        CHECK_I(ret, "H5Dclose");
        ret = H5Fclose(file_id);
        CHECK_I(ret, "H5Fclose");

        file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
        CHECK_I(file_id, "H5Fopen");
        dset1_id = H5Dopen2(file_id, "dataset", H5P_DEFAULT);
        CHECK_I(dset1_id, "H5Dopen2");
        dset2_id = H5Dopen2(file_id, "dataset2", H5P_DEFAULT);
        CHECK_I(dset2_id, "H5Dopen2");
    }

    /* Create the third dataset.  Its dataspace will be unshared and then
     * become shared when extended.
     */
    dset3_id = H5Dcreate2(file_id, "dataset3", H5T_NATIVE_LONG, orig_space_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
    CHECK_I(dset3_id, "H5Dcreate2");
    if(close_reopen) {
        /* If requested, close all open IDs and reopen them */
        ret = H5Dclose(dset1_id);
        CHECK_I(ret, "H5Dclose");
        ret = H5Dclose(dset2_id);
        CHECK_I(ret, "H5Dclose");
        ret = H5Dclose(dset3_id);
        CHECK_I(ret, "H5Dclose");
        ret = H5Fclose(file_id);
        CHECK_I(ret, "H5Fclose");

        file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
        CHECK_I(file_id, "H5Fopen");
        dset1_id = H5Dopen2(file_id, "dataset", H5P_DEFAULT);
        CHECK_I(dset1_id, "H5Dopen2");
        dset2_id = H5Dopen2(file_id, "dataset2", H5P_DEFAULT);
        CHECK_I(dset2_id, "H5Dopen2");
        dset3_id = H5Dopen2(file_id, "dataset3", H5P_DEFAULT);
        CHECK_I(dset3_id, "H5Dopen2");
    }

    /* Extend the third dataset */
    ret = H5Dset_extent(dset3_id, dims2);
    CHECK_I(ret, "H5Dset_extent");
    if(close_reopen) {
        /* If requested, close all open IDs and reopen them */
        ret = H5Dclose(dset1_id);
        CHECK_I(ret, "H5Dclose");
        ret = H5Dclose(dset2_id);
        CHECK_I(ret, "H5Dclose");
        ret = H5Dclose(dset3_id);
        CHECK_I(ret, "H5Dclose");
        ret = H5Fclose(file_id);
        CHECK_I(ret, "H5Fclose");

        file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
        CHECK_I(file_id, "H5Fopen");
        dset1_id = H5Dopen2(file_id, "dataset", H5P_DEFAULT);
        CHECK_I(dset1_id, "H5Dopen2");
        dset2_id = H5Dopen2(file_id, "dataset2", H5P_DEFAULT);
        CHECK_I(dset2_id, "H5Dopen2");
        dset3_id = H5Dopen2(file_id, "dataset3", H5P_DEFAULT);
        CHECK_I(dset3_id, "H5Dopen2");
    }

    /* Get the dataspaces from the datasets */
    space1_id = H5Dget_space(dset1_id);
    CHECK_I(space1_id, "H5Dget_space");
    space2_id = H5Dget_space(dset2_id);
    CHECK_I(space2_id, "H5Dget_space");
    space3_id = H5Dget_space(dset3_id);
    CHECK_I(space3_id, "H5Dget_space");

    /* Verify the dataspaces */
    ret = H5Sget_simple_extent_dims(space1_id, out_dims, out_maxdims);
    CHECK_I(ret, "H5Sget_simple_extent_dims");
    for(x=0; x<EXTEND_NDIMS; ++x) {
        VERIFY(out_dims[x], dims2[x], "H5Sget_simple_extent_dims");
        VERIFY(out_maxdims[x], max_dims[x], "H5Sget_simple_extent_dims");
    }

    ret = H5Sget_simple_extent_dims(space2_id, out_dims, out_maxdims);
    CHECK_I(ret, "H5Sget_simple_extent_dims");
    for(x=0; x<EXTEND_NDIMS; ++x) {
        VERIFY(out_dims[x], dims2[x], "H5Sget_simple_extent_dims");
        VERIFY(out_maxdims[x], max_dims[x], "H5Sget_simple_extent_dims");
    }

    ret = H5Sget_simple_extent_dims(space3_id, out_dims, out_maxdims);
    CHECK_I(ret, "H5Sget_simple_extent_dims");
    for(x=0; x<EXTEND_NDIMS; ++x) {
        VERIFY(out_dims[x], dims2[x], "H5Sget_simple_extent_dims");
        VERIFY(out_maxdims[x], max_dims[x], "H5Sget_simple_extent_dims");
    }

    /* Close all three dataspaces */
    ret = H5Sclose(space1_id);
    CHECK_I(ret, "H5Sclose");
    ret = H5Sclose(space2_id);
    CHECK_I(ret, "H5Sclose");
    ret = H5Sclose(space3_id);
    CHECK_I(ret, "H5Sclose");

    /* Close the datasets and file */
    ret = H5Dclose(dset1_id);
    CHECK_I(ret, "H5Dclose");
    ret = H5Dclose(dset2_id);
    CHECK_I(ret, "H5Dclose");
    ret = H5Dclose(dset3_id);
    CHECK_I(ret, "H5Dclose");
    ret = H5Fclose(file_id);
    CHECK_I(ret, "H5Fclose");

    /* Cleanup */
    ret = H5Sclose(orig_space_id);
    CHECK_I(ret, "H5Sclose");
    ret = H5Pclose(dcpl_id);
    CHECK_I(ret, "H5Pclose");

    /* Retrieve current # of errors */
    if(old_nerrs == GetTestNumErrs())
        return(0);
    else
        return(-1);
} /* end test_sohm_extend_dset_helper() */


/*-------------------------------------------------------------------------
 * Function:    test_sohm_extend_dset
 *
 * Purpose:     Test extended shared dataspaces.  An extended dataset's
 *              dataspace will change, possibly confusing the shared message
 *              code.
 *
 * Programmer:  James Laird
 *              Wednesday, January 10, 2007
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
test_sohm_extend_dset(void)
{
    hid_t fcpl_id = -1;
    herr_t ret;

    /* Create fcpl */
    fcpl_id = H5Pcreate(H5P_FILE_CREATE);
    CHECK_I(fcpl_id, "H5Pcreate");

    /* Test extending datasets with different FCPLs */
    ret = H5Pset_shared_mesg_nindexes(fcpl_id, 1);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");

    /* No shared messages */
    ret = test_sohm_extend_dset_helper(fcpl_id, FALSE);
    CHECK_I(ret, "test_sohm_extend_dset_helper");
    ret = test_sohm_extend_dset_helper(fcpl_id, TRUE);
    CHECK_I(ret, "test_sohm_extend_dset_helper");


    /* Only dataspaces */
    ret = H5Pset_shared_mesg_index(fcpl_id, 0, H5O_SHMESG_SDSPACE_FLAG, 16);
    CHECK_I(ret, "H5Pset_shared_mesg_index");

    ret = test_sohm_extend_dset_helper(fcpl_id, FALSE);
    CHECK_I(ret, "test_sohm_extend_dset_helper");
    ret = test_sohm_extend_dset_helper(fcpl_id, TRUE);
    CHECK_I(ret, "test_sohm_extend_dset_helper");

    /* All messages */
    ret = H5Pset_shared_mesg_nindexes(fcpl_id, 1);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");
    ret = H5Pset_shared_mesg_index(fcpl_id, 0, H5O_SHMESG_ALL_FLAG, 16);
    CHECK_I(ret, "H5Pset_shared_mesg_index");

    ret = test_sohm_extend_dset_helper(fcpl_id, FALSE);
    CHECK_I(ret, "test_sohm_extend_dset_helper");
    ret = test_sohm_extend_dset_helper(fcpl_id, TRUE);
    CHECK_I(ret, "test_sohm_extend_dset_helper");


    /* All messages in lists */
    ret = H5Pset_shared_mesg_phase_change(fcpl_id, 100, 50);
    CHECK_I(ret, "H5Pset_shared_mesg_phase_change");

    ret = test_sohm_extend_dset_helper(fcpl_id, FALSE);
    CHECK_I(ret, "test_sohm_extend_dset_helper");
    ret = test_sohm_extend_dset_helper(fcpl_id, TRUE);
    CHECK_I(ret, "test_sohm_extend_dset_helper");


    /* All messages in lists converted to B-trees */
    ret = H5Pset_shared_mesg_phase_change(fcpl_id, 1, 0);
    CHECK_I(ret, "H5Pset_shared_mesg_phase_change");

    ret = test_sohm_extend_dset_helper(fcpl_id, FALSE);
    CHECK_I(ret, "test_sohm_extend_dset_helper");
    ret = test_sohm_extend_dset_helper(fcpl_id, TRUE);
    CHECK_I(ret, "test_sohm_extend_dset_helper");


    /* All messages in B-trees */
    ret = H5Pset_shared_mesg_phase_change(fcpl_id, 0, 0);
    CHECK_I(ret, "H5Pset_shared_mesg_phase_change");

    ret = test_sohm_extend_dset_helper(fcpl_id, FALSE);
    CHECK_I(ret, "test_sohm_extend_dset_helper");
    ret = test_sohm_extend_dset_helper(fcpl_id, TRUE);
    CHECK_I(ret, "test_sohm_extend_dset_helper");
}


/*-------------------------------------------------------------------------
 * Function:    test_sohm_external_dtype
 *
 * Purpose:     When a datatype is a SOHM type in one file, test that the
 *              second file using the same datatype actually save it in
 *              the file, too.
 *
 * Programmer:  Raymond Lu
 *              13 October, 2008
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
test_sohm_external_dtype(void)
{
    typedef struct s1_t {
        int a;
        int b;
    } s1_t;
    s1_t  *s_ptr, *orig;
    hid_t fcpl, file1, file2;
    hid_t dataset1, dataset2;
    hid_t s1_tid, dset1_tid, dset2_tid, space;
    hsize_t dims[2] = {NX, NY};
    H5T_class_t dtype_class;
    size_t dmsg_count;
    unsigned x, i;
    herr_t ret;

    fcpl = H5Pcreate(H5P_FILE_CREATE);
    CHECK_I(fcpl, "H5Pcreate");

    /* Set up index values for sohm */
    ret = H5Pset_shared_mesg_nindexes(fcpl, TEST_NUM_INDEXES);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");

    for(x=0; x<TEST_NUM_INDEXES; ++x)
    {
        ret = H5Pset_shared_mesg_index(fcpl, x, test_type_flags[x], test_minsizes[x]);
        CHECK_I(ret, "H5Pset_shared_mesg_index");
    }

    ret = H5Pset_shared_mesg_phase_change(fcpl, TEST_L2B, TEST_B2L);
    CHECK_I(ret, "H5Pset_shared_mesg_phase_change");

    /* Create the data space */
    space = H5Screate_simple(2, dims, NULL);
    CHECK_I(space, "H5Screate_simple");

    /* Create a data type for s1_t */
    s1_tid = H5Tcreate(H5T_COMPOUND, sizeof(s1_t));
    CHECK_I(s1_tid, "H5Tcreate");

    ret = H5Tinsert(s1_tid, "a", HOFFSET(s1_t,a), H5T_NATIVE_INT);
    CHECK_I(ret, "H5Tinsert");

    ret = H5Tinsert (s1_tid, "b", HOFFSET(s1_t,b), H5T_NATIVE_INT);
    CHECK_I(ret, "H5Tinsert");

    /* Create the first file for this test */
    file1 = H5Fcreate(FILENAME_SRC, H5F_ACC_TRUNC, fcpl, H5P_DEFAULT);
    CHECK_I(file1, "H5Fcreate");

    /* Check on datatype storage status. It should be zero now. */
    ret = H5F_get_sohm_mesg_count_test(file1, H5O_DTYPE_ID, &dmsg_count);
    CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
    VERIFY(dmsg_count, 0, "H5F_get_sohm_mesg_count_test");

    /* Create data set */
    dataset1 = H5Dcreate2(file1, "dataset_1", s1_tid, space, H5P_DEFAULT, H5P_DEFAULT,
        H5P_DEFAULT);
    CHECK_I(dataset1, "H5Dcreate2");

    /* Check on datatype storage status.  It should be 1 now. */
    ret = H5F_get_sohm_mesg_count_test(file1, H5O_DTYPE_ID, &dmsg_count);
    CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
    VERIFY(dmsg_count, 1, "H5F_get_sohm_mesg_count_test");

    /* Retieve the dataset's datatype */
    dset1_tid = H5Dget_type(dataset1);
    CHECK_I(dset1_tid, "H5Dget_type");

    /* Allocate space and initialize data */
    orig = (s1_t*)malloc(NX * NY * sizeof(s1_t));
    for(i=0; i<NX*NY; i++) {
        s_ptr = (s1_t*)orig + i;
        s_ptr->a = i*3 + 1;
        s_ptr->b = i*3 + 2;
    }

    /* Write the data to the dataset1 */
    ret = H5Dwrite(dataset1, s1_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, orig);
    CHECK_I(ret, "H5Dwrite");

    ret = H5Dclose(dataset1);
    CHECK_I(ret, "H5Dclose");

    /* Create the second file for this test */
    file2 = H5Fcreate(FILENAME_DST, H5F_ACC_TRUNC, fcpl, H5P_DEFAULT);
    CHECK_I(file2, "H5Fcreate");

    /* Check on datatype storage status. It should be zero now. */
    ret = H5F_get_sohm_mesg_count_test(file2, H5O_DTYPE_ID, &dmsg_count);
    CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
    VERIFY(dmsg_count, 0, "H5F_get_sohm_mesg_count_test");

    /* Create a data set using the datatype of the dataset in the first file. */
    dataset2 = H5Dcreate2(file2, "dataset_2", dset1_tid, space, H5P_DEFAULT, H5P_DEFAULT,
        H5P_DEFAULT);
    CHECK_I(dataset2, "H5Dcreate2");

    /* Check on datatype storage status.  It should be 1 now. */
    ret = H5F_get_sohm_mesg_count_test(file2, H5O_DTYPE_ID, &dmsg_count);
    CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
    VERIFY(dmsg_count, 1, "H5F_get_sohm_mesg_count_test");

    /* Write the data to the dataset2 */
    ret = H5Dwrite(dataset2, s1_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, orig);
    CHECK_I(ret, "H5Dwrite");

    ret = H5Dclose(dataset2);
    CHECK_I(ret, "H5Dclose");

    /* Close file 1 and the dataset's datatype in file 1.  Verify that the datatype in
     * file 2 is still accessible. */
    ret = H5Tclose(dset1_tid);
    CHECK_I(ret, "H5Tclose");

    ret = H5Fclose(file1);
    CHECK_I(ret, "H5Fclose");

    /* Open the dataset in file 2 */
    dataset2 = H5Dopen2(file2, "dataset_2", H5P_DEFAULT);
    CHECK_I(dataset2, "H5Dopen2");

    /* Retieve the dataset's datatype */
    dset2_tid = H5Dget_type(dataset2);
    CHECK_I(dset2_tid, "H5Dget_type");

    /* Verify the datatype is compound */
    dtype_class = H5Tget_class(dset2_tid);
    VERIFY(dtype_class, H5T_COMPOUND, "H5Tget_class");

    ret = H5Tclose(dset2_tid);
    CHECK_I(ret, "H5Tclose");

    ret = H5Dclose(dataset2);
    CHECK_I(ret, "H5Dclose");

    /* Finishing test and release resources */
    ret = H5Sclose(space);
    CHECK_I(ret, "H5Sclose");

    ret = H5Tclose(s1_tid);
    CHECK_I(ret, "H5Tclose");

    ret = H5Pclose(fcpl);
    CHECK_I(ret, "H5Pclose");

    ret = H5Fclose(file2);
    CHECK_I(ret, "H5Fclose");

    free(orig);
}


/****************************************************************
**
**  test_sohm(): Main Shared Object Header Message testing routine.
**
****************************************************************/
void
test_sohm(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing Shared Object Header Messages\n"));

    test_sohm_fcpl();		/* Test SOHMs and file creation plists */
    test_sohm_size1();          /* Tests the sizes of files with one SOHM */
    test_sohm_attrs();          /* Tests shared messages in attributes */
    test_sohm_size2(0);         /* Tests the sizes of files with multiple SOHMs */
    test_sohm_size2(1);         /* Tests the sizes of files with multiple
                                 * SOHMs, closing and reopening file after
                                 * each write. */
    test_sohm_delete();         /* Test deleting shared messages */
    test_sohm_delete_revert();  /* Test that a file with SOHMs becomes an
                                 * empty file again when they are deleted. */
#ifndef  H5_CANNOT_OPEN_TWICE   /* On VMS this test fails since it tries to
                                   open target file the second time */
    test_sohm_extlink();        /* Test SOHMs when external links are used */
#endif /* H5_CANNOT_OPEN_TWICE */

    test_sohm_extend_dset();    /* Test extending shared datasets */
    test_sohm_external_dtype(); /* Test using datatype in another file */
} /* test_sohm() */


/*-------------------------------------------------------------------------
 * Function:	cleanup_sohm
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	James Laird
 *              October 9, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_sohm(void)
{
    remove(FILENAME);
    remove(FILENAME_SRC);
    remove(FILENAME_DST);
}

