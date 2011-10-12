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
 * Programmer:  Robb Matzke <robb@arborea.spizella.com>
 *              Thursday, October  1, 1998
 *
 * Purpose:	Tests dataset fill values.
 */
#include "h5test.h"
#include "H5srcdir.h"

/*
 * Define NO_FILLING if you want to compare how this test works when there is
 * no fill value (that is, when the fill value is zero).
 */
/* #define NO_FILLING */

const char *FILENAME[] = {
    "fillval_1",
    "fillval_2",
    "fillval_3",
    "fillval_4",
    "fillval_5",
    "fillval_6",
    "fillval_7",
    "fillval_8",
    NULL
};

/* Common type for compound datatype operations */
typedef struct {
    float  a;
    int    x;
    double y;
    char   z;
} comp_datatype;

/* Common type for compound+vl datatype operations */
typedef struct {
    int    x;
    char   *a;
    char   *b;
    int    y;
} comp_vl_datatype;

/* The fill_old.h5 is generated from gen_old_fill.c in HDF5 'test' directory
 * for version 1.4(after 1.4.3).  To get this data file, simply compile
 * gen_old_fill.c with HDF5 library (before v1.5) and run it. */
#define FILE_COMPATIBLE "fill_old.h5"
#define FILE_NAME_RAW	"fillval.raw"


/*-------------------------------------------------------------------------
 * Function:    create_compound_type
 *
 * Purpose:     create a compound datatype
 *
 * Return:      Success:        datatype ID
 *
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              Monday, Jan 26, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hid_t create_compound_type(void)
{
    hid_t ret_value=-1;

    if((ret_value = H5Tcreate(H5T_COMPOUND, sizeof(comp_datatype))) < 0)
        goto error;
    if(H5Tinsert(ret_value, "a", HOFFSET(comp_datatype, a), H5T_NATIVE_FLOAT) < 0)
        goto error;
    if(H5Tinsert(ret_value, "x", HOFFSET(comp_datatype, x), H5T_NATIVE_INT) < 0)
        goto error;
    if(H5Tinsert(ret_value, "y", HOFFSET(comp_datatype, y), H5T_NATIVE_DOUBLE) < 0)
        goto error;
    if(H5Tinsert(ret_value, "z", HOFFSET(comp_datatype, z), H5T_NATIVE_CHAR) < 0)
	goto error;

    return ret_value;

error:
    H5E_BEGIN_TRY {
        H5Tclose(ret_value);
    } H5E_END_TRY;
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    create_compound_vl_type
 *
 * Purpose:     create a compound+vl datatype
 *
 * Return:      Success:        datatype ID
 *
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, July 3, 2007
 *
 *-------------------------------------------------------------------------
 */
static hid_t
create_compound_vl_type(void)
{
    hid_t str_id = -1;          /* Datatype for VL-string fields */
    hid_t ret_value = -1;

    /* Create a string datatype */
    if((str_id = H5Tcopy(H5T_C_S1)) < 0) TEST_ERROR
    if(H5Tset_size(str_id, H5T_VARIABLE) < 0) TEST_ERROR

    if((ret_value = H5Tcreate(H5T_COMPOUND, sizeof(comp_vl_datatype))) < 0) TEST_ERROR
    if(H5Tinsert(ret_value, "x", HOFFSET(comp_vl_datatype, x), H5T_NATIVE_INT) < 0) TEST_ERROR
    if(H5Tinsert(ret_value, "a", HOFFSET(comp_vl_datatype, a), str_id) < 0) TEST_ERROR
    if(H5Tinsert(ret_value, "b", HOFFSET(comp_vl_datatype, b), str_id) < 0) TEST_ERROR
    if(H5Tinsert(ret_value, "y", HOFFSET(comp_vl_datatype, y), H5T_NATIVE_INT) < 0) TEST_ERROR

    /* Close string datatype */
    if(H5Tclose(str_id) < 0) TEST_ERROR

    return ret_value;

error:
    H5E_BEGIN_TRY {
        H5Tclose(str_id);
        H5Tclose(ret_value);
    } H5E_END_TRY;
    return -1;
} /* end create_compound_vl_type() */


/*-------------------------------------------------------------------------
 * Function:	test_getset
 *
 * Purpose:	Tests the H5Pget_fill_value() and H5Pset_fill_value()
 *		functions.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Thursday, October  1, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_getset(void)
{
    hid_t	dcpl=-1;
    int		fill_i;
    hid_t	type_ss=-1, type_si=-1;
    struct fill_si {
	int 	v1, v2;
    }		fill_si;
    struct fill_ss {
	short	v1, v2;
    }		fill_ss, fill_ss_rd;

    TESTING("property lists");

    /*
     * Create the dataset creation property list and the data types that will
     * be used during this test.
     */
    if((dcpl=H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if((type_ss=H5Tcreate(H5T_COMPOUND, sizeof fill_ss)) < 0 ||
	H5Tinsert(type_ss, "v1", HOFFSET(struct fill_ss, v1),
		  H5T_NATIVE_SHORT) < 0 ||
	H5Tinsert(type_ss, "v2", HOFFSET(struct fill_ss, v2),
		  H5T_NATIVE_SHORT) < 0) {
	goto error;
    }
    if((type_si=H5Tcreate(H5T_COMPOUND, sizeof fill_si)) < 0 ||
	H5Tinsert(type_si, "v1", HOFFSET(struct fill_si, v1),
		  H5T_NATIVE_INT) < 0 ||
	H5Tinsert(type_si, "v2", HOFFSET(struct fill_si, v2),
		  H5T_NATIVE_INT) < 0) {
	goto error;
    }

    /*
     * Reading the fill value from a dataset creation property list that has
     * no fill value should result in a failure.
     */
    H5E_BEGIN_TRY {
	H5Pget_fill_value(dcpl, H5T_NATIVE_INT, &fill_i);
    } H5E_END_TRY;
    if(fill_i != 0) {
	H5_FAILED();
	puts("    H5Pget_fill_value() should return default 0");
	goto error;
    }

    /*
     * Set the fill value using a struct as the data type.
     */
    fill_ss.v1 = 1111;
    fill_ss.v2 = 2222;
    if(H5Pset_fill_value(dcpl, type_ss, &fill_ss) < 0) goto error;

    /*
     * Get the fill value using the same data type that was used to set it.
     */
    if(H5Pget_fill_value(dcpl, type_ss, &fill_ss_rd) < 0) goto error;
    if(fill_ss.v1!=fill_ss_rd.v1 || fill_ss.v2!=fill_ss_rd.v2) {
	H5_FAILED();
	puts("    Failed to get fill value using same data type that was ");
	puts("    used to set the fill value.");
	goto error;
    }

    /*
     * Get the fill value using some other data type.
     */
    if(H5Pget_fill_value(dcpl, type_si, &fill_si) < 0) goto error;
    if(fill_ss.v1!=fill_si.v1 || fill_ss.v2!=fill_si.v2) {
	H5_FAILED();
	puts("    Failed to get fill value using a data type other than what");
	puts("    was used to set the fill value.");
	goto error;
    }

    /*
     * Reset the fill value
     */
    if(H5Pset_fill_value(dcpl, type_si, &fill_si) < 0) goto error;
    if(H5Pget_fill_value(dcpl, type_ss, &fill_ss) < 0) goto error;
    if(fill_si.v1!=fill_ss.v1 || fill_si.v2!=fill_ss.v2) {
	H5_FAILED();
	puts("    Resetting the fill value was unsuccessful.");
	goto error;
    }

    /* Success */
    if(H5Pclose(dcpl) < 0) goto error;
    if(H5Tclose(type_si) < 0) goto error;
    if(H5Tclose(type_ss) < 0) goto error;
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Pclose(dcpl);
	H5Tclose(type_si);
	H5Tclose(type_ss);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_getset_vl
 *
 * Purpose:	Tests the H5Pget_fill_value() and H5Pset_fill_value()
 *		functions, using variable-length datatype.
 *
 * Return:	Success:	0
 *		Failure:	number of errors
 *
 * Programmer:	Quincey Koziol
 *              Thursday, May 31, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
test_getset_vl(hid_t fapl)
{
    hsize_t dims[1] = {2};
    hid_t fileid = (-1), spaceid = (-1), typeid = (-1), datasetid = (-1), plistid = (-1);
    char fill_value[] = "aaaa";
    char orig_fill_value[] = "aaaa";
    char *f1 = fill_value;
    char *f2;
    char filename[1024];

    TESTING("property lists, with variable-length datatype");

    /* Create string type. */
    if((typeid =  H5Tcopy(H5T_C_S1)) < 0) TEST_ERROR
    if(H5Tset_size(typeid, H5T_VARIABLE) < 0) TEST_ERROR

    /* Set up dataset creation property list, with fill value */
    if((plistid = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR
    if(H5Pset_fill_value(plistid, typeid, &f1) < 0) TEST_ERROR

    /* Modify original fill value string */
    fill_value[0] = 'b';

    /* Retrieve fill value from property */
    if(H5Pget_fill_value(plistid, typeid, &f2) < 0) TEST_ERROR

    /* Verify that the fill value is the original value */
    if(HDstrcmp(f2, orig_fill_value)) TEST_ERROR

    /* Release the fill value retrieved */
    HDfree(f2);

    /* Open file. */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if((fileid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Write an dataset of this type. */
    if((spaceid = H5Screate_simple(1, dims, NULL)) < 0) TEST_ERROR
    if((datasetid = H5Dcreate2(fileid, "Dataset", typeid, spaceid, H5P_DEFAULT, plistid, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close IDs (except datatype) */
    if(H5Dclose(datasetid) < 0) TEST_ERROR
    if(H5Pclose(plistid) < 0) TEST_ERROR
    if(H5Sclose(spaceid) < 0) TEST_ERROR
    if(H5Fclose(fileid) < 0) TEST_ERROR


    /* Re-open file, group & dataset */
    if((fileid = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0) TEST_ERROR
    if((datasetid = H5Dopen2(fileid, "Dataset", H5P_DEFAULT)) < 0) TEST_ERROR

    /* Get dataset's creation property list */
    if((plistid = H5Dget_create_plist(datasetid)) < 0) TEST_ERROR

    /* Query fill value */
    if(H5Pget_fill_value(plistid, typeid, &f2) < 0) TEST_ERROR

    /* Verify that the fill value is the original value */
    if(HDstrcmp(f2, orig_fill_value)) TEST_ERROR

    /* Release the fill value retrieved */
    HDfree(f2);

    /* Close IDs */
    if(H5Dclose(datasetid) < 0) TEST_ERROR
    if(H5Fclose(fileid) < 0) TEST_ERROR
    if(H5Pclose(plistid) < 0) TEST_ERROR
    if(H5Tclose(typeid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    } H5E_END_TRY;
    return 1;
} /* end test_getset_vl() */


/*-------------------------------------------------------------------------
 * Function:	test_create
 *
 * Purpose:	Tests creating datasets that have fill values.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Thursday, October  1, 1998
 *
 * Modifications:
 *		Many new cases have been added to this test since
 *		the fill value design has been modified.
 *
 *-------------------------------------------------------------------------
 */
static int
test_create(hid_t fapl, const char *base_name, H5D_layout_t layout)
{
    hid_t	file=-1, space=-1, dcpl=-1, comp_type_id=-1;
    hid_t	dset1=-1, dset2=-1, dset3=-1, dset4=-1, dset5=-1,
		dset6=-1, /* dset7=-1, */ dset8=-1, dset9=-1;
    hsize_t     cur_size[5] = {2, 8, 8, 4, 2};
    hsize_t	ch_size[5] = {1, 1, 1, 4, 1};
    short	rd_s, fill_s = 0x1234;
    long	rd_l, fill_l = 0x4321;
    char	filename[1024];
    H5D_space_status_t	allocation;
    H5D_alloc_time_t    alloc_time;
    H5D_fill_time_t	fill_time;
    comp_datatype       rd_c, fill_ctype;

    if(H5D_CHUNKED==layout) {
	TESTING("chunked dataset creation");
    } else if(H5D_COMPACT==layout) {
        TESTING("compact dataset creation");
    } else {
	TESTING("contiguous dataset creation");
    }

    /*
     * Create a file.
     */
    h5_fixname(base_name, fapl, filename, sizeof filename);
    if((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
	goto error;
    if((space=H5Screate_simple(5, cur_size, cur_size)) < 0) goto error;
    if((dcpl=H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5D_CHUNKED==layout) {
	if(H5Pset_chunk(dcpl, 5, ch_size) < 0) goto error;
    } else if(H5D_COMPACT==layout) {
        if(H5Pset_layout(dcpl, H5D_COMPACT) < 0) goto error;
    }

    /* Create a compound datatype */
    if((comp_type_id = create_compound_type()) < 0) goto error;

    /* I. Test cases for late space allocation except compact dataset */

    if(H5D_COMPACT!=layout) {
        if(H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_LATE) < 0) goto error;
        if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC) < 0) goto error;

        /* 1. Compound datatype test */
        if(H5Pget_fill_value(dcpl, comp_type_id, &fill_ctype) < 0) goto error;
        fill_ctype.y = 4444;
        if(H5Pset_fill_value(dcpl, comp_type_id, &fill_ctype) < 0) goto error;
        if((dset9 = H5Dcreate2(file, "dset9", comp_type_id, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            goto error;

        /* The three datasets test three fill
         * conversion paths: small to large, large to small, and no conversion.
         * They depend on `short' being smaller than `long'.
         */
        /* 2. Small to large fill conversion */
#ifndef NO_FILLING
        if(H5Pset_fill_value(dcpl, H5T_NATIVE_SHORT, &fill_s) < 0) goto error;
#endif
        if((dset1=H5Dcreate2(file, "dset1", H5T_NATIVE_LONG, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
	   goto error;

        /* 3. Large to small fill conversion */
#ifndef NO_FILLING
        if(H5Pset_fill_value(dcpl, H5T_NATIVE_LONG, &fill_l) < 0) goto error;
#endif
        if((dset2=H5Dcreate2(file, "dset2", H5T_NATIVE_SHORT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
	   goto error;

        /* 4. No conversion */
#ifndef NO_FILLING
        if(H5Pset_fill_value(dcpl, H5T_NATIVE_LONG, &fill_l) < 0) goto error;
#endif
        if((dset3=H5Dcreate2(file, "dset3", H5T_NATIVE_LONG, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
	   goto error;

        /* 5. late space allocation and never write fill value */
        if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_NEVER) < 0) goto error;
        if((dset4=H5Dcreate2(file, "dset4", H5T_NATIVE_LONG, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            goto error;

        /* 6. fill value is undefined while fill write time is H5D_FILL_TIME_ALLOC.
         * Supposed to fail. */
        if(H5Pset_fill_value(dcpl, -1, NULL) < 0) goto error;
        if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC) < 0) goto error;
        H5E_BEGIN_TRY {
            if(H5Dcreate2(file, "dset7", H5T_NATIVE_LONG, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)!=FAIL)
                goto error;
        } H5E_END_TRY;
    }

    /* II. Test early space allocation cases */

    if(H5Pclose(dcpl) < 0)  goto error;
    if((dcpl=H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5D_CHUNKED==layout) {
        if(H5Pset_chunk(dcpl, 5, ch_size) < 0) goto error;
    } else if(H5D_COMPACT==layout) {
        if(H5Pset_layout(dcpl, H5D_COMPACT) < 0) goto error;
    }
    if(H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0) goto error;

    /* 1. Compound datatype test */
    if(H5Pget_fill_value(dcpl, comp_type_id, &fill_ctype) < 0) goto error;
    fill_ctype.y = 4444;
    if(H5Pset_fill_value(dcpl, comp_type_id, &fill_ctype) < 0) goto error;
    if((dset8 = H5Dcreate2(file, "dset8", comp_type_id, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;


    if(H5Pset_fill_value(dcpl, H5T_NATIVE_LONG, &fill_l) < 0) goto error;

    /* 2. Never write fill value */
    if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_NEVER) < 0) goto error;
    if((dset5 = H5Dcreate2(file, "dset5", H5T_NATIVE_INT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    /* 3. Write fill value at space allocation time */
    if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC) < 0) goto error;
    if((dset6 = H5Dcreate2(file, "dset6", H5T_NATIVE_LONG, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
	goto error;

    /* 4. fill value is undefined while fill write time is H5D_FILL_TIME_ALLOC.
     * Supposed to fail. */
    if(H5Pset_fill_value(dcpl, -1, NULL) < 0) goto error;
    H5E_BEGIN_TRY {
        if(H5Dcreate2(file, "dset7", H5T_NATIVE_LONG, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)!=FAIL)
            goto error;
    } H5E_END_TRY;

    /* Close everything */
    if(H5D_COMPACT != layout) {
        if(H5Dclose(dset1) < 0) goto error;
        if(H5Dclose(dset2) < 0) goto error;
        if(H5Dclose(dset3) < 0) goto error;
        if(H5Dclose(dset4) < 0) goto error;
        if(H5Dclose(dset9) < 0) goto error;
    }
    if(H5Dclose(dset5) < 0) goto error;
    if(H5Dclose(dset6) < 0) goto error;
    if(H5Dclose(dset8) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Pclose(dcpl) < 0)  goto error;
    if(H5Fclose(file) < 0)  goto error;

    /* Open the file and get the dataset fill value from each dataset */
    if((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
	goto error;

    /* I. Check cases for late space allocation except compact dataset */
    if(H5D_COMPACT != layout) {
        /* 1. Large to small conversion */
        if((dset1 = H5Dopen2(file, "dset1", H5P_DEFAULT)) < 0) goto error;
        if((dcpl = H5Dget_create_plist(dset1)) < 0) goto error;
#ifndef NO_FILLING
        if(H5Pget_fill_value(dcpl, H5T_NATIVE_SHORT, &rd_s) < 0) goto error;
        if(rd_s != fill_s) {
	   H5_FAILED();
	   printf("    %d: Got a different fill value than what was set.",__LINE__);
	   printf("    Got %d, set %d\n", rd_s, fill_s);
	   goto error;
        }
#endif
        if(H5Dclose(dset1) < 0) goto error;
        if(H5Pclose(dcpl) < 0) goto error;

        /* 2. Small to large conversion */
        if((dset2 = H5Dopen2(file, "dset2", H5P_DEFAULT)) < 0) goto error;
        if((dcpl = H5Dget_create_plist(dset2)) < 0) goto error;
#ifndef NO_FILLING
        if(H5Pget_fill_value(dcpl, H5T_NATIVE_LONG, &rd_l) < 0) goto error;
        if(rd_l!=fill_l) {
	   H5_FAILED();
	   printf("    %d: Got a different fill value than what was set.",__LINE__);
	   printf("    Got %ld, set %ld\n", rd_l, fill_l);
	   goto error;
        }
#endif
        if(H5Dclose(dset2) < 0) goto error;
        if(H5Pclose(dcpl) < 0) goto error;

        /* 3. No conversion */
        if((dset3 = H5Dopen2(file, "dset3", H5P_DEFAULT)) < 0) goto error;
        if((dcpl = H5Dget_create_plist(dset3)) < 0) goto error;
#ifndef NO_FILLING
        if(H5Pget_fill_value(dcpl, H5T_NATIVE_LONG, &rd_l) < 0) goto error;
        if(rd_l != fill_l) {
	   H5_FAILED();
	   printf("    %d: Got a different fill value than what was set.",__LINE__);
	   printf("    Got %ld, set %ld\n", rd_l, fill_l);
	   goto error;
        }
#endif
        if(H5Pget_alloc_time(dcpl, &alloc_time) < 0) goto error;
        if(H5Pget_fill_time(dcpl, &fill_time) < 0) goto error;
        if(alloc_time != H5D_ALLOC_TIME_LATE) {
            H5_FAILED();
            puts("    Got non-H5D_ALLOC_TIME_LATE space allocation time.");
            printf("    Got %d\n", alloc_time);
        }
        if(fill_time != H5D_FILL_TIME_ALLOC) {
            H5_FAILED();
            puts("    Got non-H5D_FILL_TIME_ALLOC fill value write time.");
            printf("    Got %d\n", fill_time);
        }
        if(H5Dclose(dset3) < 0) goto error;
        if(H5Pclose(dcpl) < 0) goto error;

        /* 4. late space allocation and never write fill value */
        if((dset4 = H5Dopen2(file, "dset4", H5P_DEFAULT)) < 0) goto error;
        if(H5Dget_space_status(dset4, &allocation) < 0) goto error;
        if(layout == H5D_CONTIGUOUS && allocation != H5D_SPACE_STATUS_NOT_ALLOCATED) {
            H5_FAILED();
            puts("    Got allocated space instead of unallocated.");
            printf("    Got %d\n", allocation);
            goto error;
        }
        if((dcpl = H5Dget_create_plist(dset4)) < 0) goto error;
        if(H5Pget_alloc_time(dcpl, &alloc_time) < 0) goto error;
        if(H5Pget_fill_time(dcpl, &fill_time) < 0) goto error;
        if(alloc_time != H5D_ALLOC_TIME_LATE) {
	   H5_FAILED();
	   puts("    Got non-H5D_ALLOC_TIME_LATE space allocation time.");
	   printf("    Got %d\n", alloc_time);
        }
        if(fill_time != H5D_FILL_TIME_NEVER) {
	   H5_FAILED();
	   puts("    Got non-H5D_FILL_TIME_NEVER fill value write time.");
    	   printf("    Got %d\n", fill_time);
        }
        if(H5Dclose(dset4) < 0) goto error;
        if(H5Pclose(dcpl) < 0) goto error;

        /* 5. Compound datatype test */
        if((dset9 = H5Dopen2(file, "dset9", H5P_DEFAULT)) < 0) goto error;
        if((dcpl = H5Dget_create_plist(dset9)) < 0) goto error;
        if(H5Pget_fill_value(dcpl, comp_type_id, &rd_c) < 0) goto error;
        if( rd_c.a!=0 || rd_c.y != fill_ctype.y || rd_c.x != 0 || rd_c.z != '\0') {
           H5_FAILED();
           puts("    Got wrong fill value");
           printf("    Got rd_c.a=%f, rd_c.y=%f and rd_c.x=%d, rd_c.z=%c\n",
                  rd_c.a, rd_c.y, rd_c.x, rd_c.z);
        }
        if(H5Dclose(dset9) < 0) goto error;
        if(H5Pclose(dcpl) < 0) goto error;
    }

    /* II. Check early space allocation cases */

    /* 1. Never write fill value */
    if((dset5 = H5Dopen2(file, "dset5", H5P_DEFAULT)) < 0) goto error;
    if((dcpl = H5Dget_create_plist(dset5)) < 0) goto error;
    if(H5Dget_space_status(dset5, &allocation) < 0) goto error;
    if(layout == H5D_CONTIGUOUS && allocation != H5D_SPACE_STATUS_ALLOCATED) {
        H5_FAILED();
        printf("    %d: Got unallocated space instead of allocated.\n",__LINE__);
        printf("    Got %d\n", allocation);
        goto error;
    }
    if(H5Pget_alloc_time(dcpl, &alloc_time) < 0) goto error;
    if(alloc_time != H5D_ALLOC_TIME_EARLY) {
        H5_FAILED();
        puts("    Got non-H5D_ALLOC_TIME_EARLY space allocation time.");
        printf("    Got %d\n", alloc_time);
    }
    if(H5Pget_fill_time(dcpl, &fill_time) < 0) goto error;
    if(fill_time != H5D_FILL_TIME_NEVER) {
        H5_FAILED();
        puts("    Got non-H5D_FILL_TIME_NEVER fill value write time.");
        printf("    Got %d\n", fill_time);
    }
    if(H5Dclose(dset5) < 0) goto error;
    if(H5Pclose(dcpl) < 0) goto error;

    /* 2. test writing fill value at space allocation time */
    if((dset6 = H5Dopen2(file, "dset6", H5P_DEFAULT)) < 0) goto error;
    if((dcpl = H5Dget_create_plist(dset6)) < 0) goto error;
    if(H5Dget_space_status(dset6, &allocation) < 0) goto error;
    if(layout == H5D_CONTIGUOUS && allocation != H5D_SPACE_STATUS_ALLOCATED) {
        H5_FAILED();
        printf("    %d: Got unallocated space instead of allocated.\n",__LINE__);
        printf("    Got %d\n", allocation);
        goto error;
    }
    if(H5Pget_fill_value(dcpl, H5T_NATIVE_LONG, &rd_l) < 0) goto error;
    if(rd_l != fill_l) {
        H5_FAILED();
	printf("    %d: Got a different fill value than what was set.",__LINE__);
        printf("    Got %ld, set %ld\n", rd_l, fill_l);
        goto error;
    }
    if(H5Pget_alloc_time(dcpl, &alloc_time) < 0) goto error;
    if(alloc_time != H5D_ALLOC_TIME_EARLY) {
        H5_FAILED();
        puts("    Got non-H5D_ALLOC_TIME_EARLY space allocation time.");
        printf("    Got %d\n", alloc_time);
    }
    if(H5Pget_fill_time(dcpl, &fill_time) < 0) goto error;
    if(fill_time != H5D_FILL_TIME_ALLOC) {
        H5_FAILED();
        puts("    Got non-H5D_FILL_TIME_ALLOC fill value write time.");
        printf("    Got %d\n", fill_time);
    }
    if(H5Dclose(dset6) < 0) goto error;
    if(H5Pclose(dcpl) < 0) goto error;

    /* 3. Compound datatype test */
    if((dset8 = H5Dopen2(file, "dset8", H5P_DEFAULT)) < 0) goto error;
    if((dcpl = H5Dget_create_plist(dset8)) < 0) goto error;
    if(H5Pget_fill_value(dcpl, comp_type_id, &rd_c) < 0) goto error;
    if(rd_c.a != 0 || rd_c.y != fill_ctype.y || rd_c.x != 0 || rd_c.z!='\0') {
        H5_FAILED();
        puts("    Got wrong fill value");
        printf("    Got rd_c.a=%f, rd_c.y=%f and rd_c.x=%d, rd_c.z=%c\n",
		rd_c.a, rd_c.y, rd_c.x, rd_c.z);
    }
    if(H5Dclose(dset8) < 0) goto error;
    if(H5Pclose(dcpl) < 0) goto error;

    if(H5Fclose(file) < 0) goto error;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Pclose(dcpl);
	H5Sclose(space);
        if(H5D_COMPACT != layout) {
	   H5Dclose(dset1);
	   H5Dclose(dset2);
	   H5Dclose(dset3);
           H5Dclose(dset4);
           H5Dclose(dset9);
        }
        H5Dclose(dset5);
        H5Dclose(dset6);
	H5Dclose(dset8);
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:	test_rdwr_cases
 *
 * Purpose:	Tests fill values read and write for datasets.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Robb Matzke
 *              Thursday, October  1, 1998
 *
 * Modifications:
 * 		This function is called by test_rdwr to write and read
 *		dataset for different cases.
 *
 *-------------------------------------------------------------------------
 */
static int
test_rdwr_cases(hid_t file, hid_t dcpl, const char *dname, void *_fillval,
		H5D_fill_time_t fill_time, H5D_layout_t layout,
		H5T_class_t datatype, hid_t ctype_id)
{
    hid_t	fspace=-1, mspace=-1, dset1=-1, dset2=-1;
    hsize_t	cur_size[5] = {2, 8, 8, 4, 2};
    hsize_t	one[5] = {1, 1, 1, 1, 1};
    hsize_t	hs_size[5], hs_stride[5];
    hsize_t	hs_offset[5], nelmts;
    int		fillval=(-1), val_rd, should_be;
    int		i, j, *buf=NULL, odd;
    unsigned    u;
    comp_datatype       rd_c, fill_c, should_be_c;
    comp_datatype	*buf_c=NULL;
    H5D_space_status_t  allocation;

    if(datatype==H5T_INTEGER)
        fillval = *(int*)_fillval;
    else if(datatype==H5T_COMPOUND) {
	fill_c.a=((comp_datatype*)_fillval)->a;
        fill_c.x=((comp_datatype*)_fillval)->x;
        fill_c.y=((comp_datatype*)_fillval)->y;
        fill_c.z=((comp_datatype*)_fillval)->z;
    } else {
        puts("Invalid type for test");
        goto error;
    }

    /* Create dataset */
    if((fspace = H5Screate_simple(5, cur_size, cur_size)) < 0)
        goto error;
    if(datatype == H5T_INTEGER && (dset1 = H5Dcreate2(file, dname, H5T_NATIVE_INT, fspace, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;
    if(datatype == H5T_COMPOUND && (dset2 = H5Dcreate2(file, dname, ctype_id, fspace, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    /* Read some data and make sure it's the fill value */
    if((mspace = H5Screate_simple(5, one, NULL)) < 0)
        goto error;
    for (i=0; i<1000; i++) {
	for (j=0; j<5; j++)
	    hs_offset[j] = rand() % cur_size[j];
	if(H5Sselect_hyperslab(fspace, H5S_SELECT_SET, hs_offset, NULL,
				one, NULL) < 0) goto error;

   	/* case for atomic datatype */
	if(datatype==H5T_INTEGER) {
            if(H5Dread(dset1, H5T_NATIVE_INT, mspace, fspace, H5P_DEFAULT,
		&val_rd) < 0) goto error;
	    if(fill_time!=H5D_FILL_TIME_NEVER && val_rd!=fillval) {
	        H5_FAILED();
                HDfprintf(stdout, "%u: Value read was not a fill value.\n", (unsigned)__LINE__);
	        HDfprintf(stdout,"    Elmt={%Hu,%Hu,%Hu,%Hu,%Hu}, read: %u, "
		       "Fill value: %u\n",
		       hs_offset[0], hs_offset[1],
		       hs_offset[2], hs_offset[3],
		       hs_offset[4], val_rd, fillval);
	        goto error;
	    }
	/* case for compound datatype */
	} else if(datatype==H5T_COMPOUND) {
            if(H5Dread(dset2, ctype_id, mspace, fspace, H5P_DEFAULT,
                &rd_c) < 0) goto error;
            if(fill_time!=H5D_FILL_TIME_NEVER && (rd_c.a!=fill_c.a ||
		rd_c.x!=fill_c.x || rd_c.y!=fill_c.y ||
		rd_c.z!=fill_c.z)) {
                H5_FAILED();
                HDfprintf(stdout, "%u: Value read was not a fill value.\n", (unsigned)__LINE__);
                HDfprintf(stdout,"    Elmt={%Hu,%Hu,%Hu,%Hu,%Hu}, read: %f, %d, %f, %c"
                       "Fill value: %f, %d, %f, %c\n",
                       hs_offset[0], hs_offset[1],
                       hs_offset[2], hs_offset[3],
                       hs_offset[4], rd_c.a, rd_c.x, rd_c.y, rd_c.z,
			fill_c.a, fill_c.x, fill_c.y, fill_c.z);
                goto error;
            }
        }
    }
    if(H5Sclose(mspace) < 0) goto error;

    /* Select all odd data locations in the file dataset */
    for (i=0, nelmts=1; i<5; i++) {
	hs_size[i] = cur_size[i]/2;
	hs_offset[i] = 0;
	hs_stride[i] = 2;
	nelmts *= hs_size[i];
    }
    if((mspace=H5Screate_simple(5, hs_size, hs_size)) < 0) goto error;
    if(H5Sselect_hyperslab(fspace, H5S_SELECT_SET, hs_offset, hs_stride,
                            hs_size, NULL) < 0) goto error;

    /* Read non-contiguous selection from empty dataset */

    /* case for atomic datatype */
    if(datatype==H5T_INTEGER) {
        /*check for overflow*/
        HDassert((nelmts * sizeof(int)) == (hsize_t)((size_t)(nelmts * sizeof(int))));
        buf = HDmalloc((size_t)(nelmts * sizeof(int)));

        if(H5Dread(dset1, H5T_NATIVE_INT, mspace, fspace, H5P_DEFAULT, buf) < 0)
            goto error;

        /* Verify values, except if no fill value written */
        if(fill_time != H5D_FILL_TIME_NEVER) {
            for(u = 0; u < nelmts; u++) {
                if(buf[u] != fillval) {
                    H5_FAILED();
                    HDfprintf(stdout, "%u: Value read was not a fill value.\n", (unsigned)__LINE__);
                    HDfprintf(stdout,"    Elmt={%Hu, %Hu, %Hu, %Hu, %Hu}, read: %u, "
                           "Fill value: %u\n",
                           hs_offset[0], hs_offset[1],
                           hs_offset[2], hs_offset[3],
                           hs_offset[4], buf[u], fillval);
                    goto error;
                } /* end if */
            } /* end for */
        } /* end if */
    }
    /* case for compound datatype */
    else if(datatype == H5T_COMPOUND) {
        /*check for overflow*/
        HDassert((nelmts * sizeof(comp_datatype)) ==
	    (hsize_t)((size_t)(nelmts * sizeof(comp_datatype))));
	buf_c = (comp_datatype *)HDmalloc((size_t)nelmts * sizeof(comp_datatype));

        if(H5Dread(dset2, ctype_id, mspace, fspace, H5P_DEFAULT, buf_c) < 0)
            goto error;

        /* Verify values, except if no fill value written */
        if(fill_time != H5D_FILL_TIME_NEVER) {
            for(u = 0; u < nelmts; u++) {
                if(buf_c[u].a != fill_c.a || buf_c[u].x != fill_c.x ||
                        buf_c[u].y != fill_c.y || buf_c[u].z != fill_c.z) {
                    H5_FAILED();
                    HDfprintf(stdout, "%u: Value read was not a fill value.\n", (unsigned)__LINE__);
                    HDfprintf(stdout,"    Elmt={%Hu, %Hu, %Hu, %Hu, %Hu}, read: %f, %d, %f, %c"
                            "Fill value: %f, %d, %f, %c\n",
                            hs_offset[0], hs_offset[1],
                            hs_offset[2], hs_offset[3],
                            hs_offset[4],
                            buf_c[u].a, buf_c[u].x, buf_c[u].y, buf_c[u].z,
                            fill_c.a, fill_c.x, fill_c.y, fill_c.z);
                    goto error;
                } /* end if */
            } /* end for */
        } /* end if */
    }

    /* Write to all odd data locations */

    /* case for atomic datatype */
    if(datatype == H5T_INTEGER) {
        for(u = 0; u < nelmts; u++)
            buf[u] = 9999;
        if(H5Dwrite(dset1, H5T_NATIVE_INT, mspace, fspace, H5P_DEFAULT, buf) < 0)
            goto error;
    }
    /* case for compound datatype */
    else if(datatype == H5T_COMPOUND) {
        HDmemset(buf_c, 0, ((size_t)nelmts * sizeof(comp_datatype)));
        for(u = 0; u < nelmts; u++) {
	    buf_c[u].a = (float)1111.11;
 	    buf_c[u].x = 2222;
	    buf_c[u].y = 3333.3333;
	    buf_c[u].z = 'd';
	}
        if(H5Dwrite(dset2, ctype_id, mspace, fspace, H5P_DEFAULT, buf_c) < 0)
            goto error;
    }

    /* Check if space is allocated */
    if(datatype==H5T_INTEGER && H5Dget_space_status(dset1, &allocation) < 0)
	goto error;
    if(datatype==H5T_COMPOUND && H5Dget_space_status(dset2, &allocation) < 0)
        goto error;
    if(layout == H5D_CONTIGUOUS && allocation != H5D_SPACE_STATUS_ALLOCATED) {
        H5_FAILED();
        printf("    %d: Got unallocated space instead of allocated.\n",__LINE__);
        printf("    Got %d\n", allocation);
        goto error;
    }
    HDfree(buf);
    buf = NULL;
    H5Sclose(mspace);

    /* Read some data and make sure it's the right value */
    if((mspace = H5Screate_simple(5, one, NULL)) < 0)
        goto error;
    for(i = 0; i < 1000; i++) {
	for(j = 0, odd = 0; j < 5; j++) {
	    hs_offset[j] = rand() % cur_size[j];
	    odd += (int)(hs_offset[j]%2);
	} /* end for */
	if(H5Sselect_hyperslab(fspace, H5S_SELECT_SET, hs_offset, NULL, one, NULL) < 0)
            goto error;

	/* case for atomic datatype */
        if(datatype==H5T_INTEGER) {
	    if(H5Dread(dset1, H5T_NATIVE_INT, mspace, fspace, H5P_DEFAULT, &val_rd) < 0)
                goto error;
            if(fill_time == H5D_FILL_TIME_ALLOC) {
                should_be = odd ? fillval : 9999;
                if(val_rd!=should_be) {
                    H5_FAILED();
                    HDfprintf(stdout, "%u: Value read was not correct.\n", (unsigned)__LINE__);
                    printf("    Elmt={%ld,%ld,%ld,%ld,%ld}, read: %u, "
                           "should be: %u\n",
                           (long)hs_offset[0], (long)hs_offset[1],
                           (long)hs_offset[2], (long)hs_offset[3],
                           (long)hs_offset[4], val_rd, should_be);
                    goto error;
                }
	    }
	    else if(fill_time == H5D_FILL_TIME_NEVER && !odd) {
	        should_be = 9999;
	        if(val_rd!=should_be) {
	            H5_FAILED();
                    HDfprintf(stdout, "%u: Value read was not correct.\n", (unsigned)__LINE__);
	            printf("    Elmt={%ld,%ld,%ld,%ld,%ld}, read: %u, "
		           "should be: %u\n",
		           (long)hs_offset[0], (long)hs_offset[1],
		           (long)hs_offset[2], (long)hs_offset[3],
		           (long)hs_offset[4], val_rd, should_be);
	            goto error;
	        }
	    } else if(fill_time == H5D_FILL_TIME_NEVER && odd) {
 	        /*Trash data. Don't compare*/
	    }
	} /* end for datatype==H5T_INTEGER */
	/* case for compound datatype */
	else if(datatype==H5T_COMPOUND) {
            if(H5Dread(dset2, ctype_id, mspace, fspace, H5P_DEFAULT, &rd_c) < 0)
                goto error;
            if(fill_time == H5D_FILL_TIME_ALLOC) {
		if(odd) {
		    should_be_c.a=fill_c.a;
		    should_be_c.x=fill_c.x;
		    should_be_c.y=fill_c.y;
		    should_be_c.z=fill_c.z;
		} else {
		    should_be_c.a=buf_c[0].a;
		    should_be_c.x=buf_c[0].x;
		    should_be_c.y=buf_c[0].y;
		    should_be_c.z=buf_c[0].z;
		}
		if( rd_c.a!=should_be_c.a || rd_c.x!=should_be_c.x ||
		    rd_c.y!=should_be_c.y || rd_c.z!=should_be_c.z)  {
                    H5_FAILED();
                    HDfprintf(stdout, "%u: Value read was not correct.\n", (unsigned)__LINE__);
                    printf("    Elmt={%ld,%ld,%ld,%ld,%ld}, read: %f,%d,%f,%c "
                           "should be: %f,%d,%f,%c\n",
                           (long)hs_offset[0], (long)hs_offset[1],
                           (long)hs_offset[2], (long)hs_offset[3],
                           (long)hs_offset[4],
			   rd_c.a, rd_c.x, rd_c.y, rd_c.z, should_be_c.a,
		           should_be_c.x,should_be_c.y,should_be_c.z);
                    goto error;
 		}
	    } /* end for fill_time == H5D_FILL_TIME_ALLOC */
	    else if(fill_time == H5D_FILL_TIME_NEVER && !odd) {
                should_be_c.a=buf_c[0].a;
                should_be_c.x=buf_c[0].x;
                should_be_c.y=buf_c[0].y;
                should_be_c.z=buf_c[0].z;
                if( rd_c.a!=should_be_c.a || rd_c.x!=should_be_c.x ||
                    rd_c.y!=should_be_c.y || rd_c.z!=should_be_c.z)  {
                    H5_FAILED();
                    HDfprintf(stdout, "%u: Value read was not correct.\n", (unsigned)__LINE__);
                    printf("    Elmt={%ld,%ld,%ld,%ld,%ld}, read: %f,%d,%f,%c "
                           "should be: %f,%d,%f,%c\n",
                           (long)hs_offset[0], (long)hs_offset[1],
                           (long)hs_offset[2], (long)hs_offset[3],
                           (long)hs_offset[4],
                           rd_c.a, rd_c.x, rd_c.y, rd_c.z, should_be_c.a,
                           should_be_c.x,should_be_c.y,should_be_c.z);
                    goto error;
                }
	    } /* end for fill_time == H5D_FILL_TIME_NEVER */
            else if(fill_time == H5D_FILL_TIME_NEVER && odd) {
                /*Trash data. Don't compare*/
            }
	} /* end for datatype==H5T_COMPOUND */
    }
    if(datatype == H5T_COMPOUND) {
        HDfree(buf_c);
        buf_c = NULL;
    } /* end if */

    if(H5Sclose(mspace) < 0) goto error;
    if(datatype==H5T_INTEGER && H5Dclose(dset1) < 0) goto error;
    if(datatype==H5T_COMPOUND && H5Dclose(dset2) < 0) goto error;
    if(H5Sclose(fspace) < 0) goto error;
    return 0;

 error:
    H5E_BEGIN_TRY {
	if(datatype==H5T_INTEGER) H5Dclose(dset1);
	if(datatype==H5T_COMPOUND) H5Dclose(dset2);
	H5Sclose(fspace);
	H5Sclose(mspace);
    } H5E_END_TRY;

    return 1;
}


/*-------------------------------------------------------------------------
 * Function:    test_rdwr
 *
 * Purpose:     Tests fill values for datasets.
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 * Programmer:  Robb Matzke
 *              Thursday, October  1, 1998
 *
 * Modifications:
 *		Many new cases have been added to this test since the
 *		fill value design is modified.
 *
 *-------------------------------------------------------------------------
 */
static int
test_rdwr(hid_t fapl, const char *base_name, H5D_layout_t layout)
{
    char        filename[1024];
    hid_t 	file=-1, dcpl=-1, ctype_id=-1;
    hsize_t     ch_size[5] = {2, 8, 8, 4, 2};
    int		nerrors=0;
    int         fillval = 0x4c70f1cd;
    comp_datatype       fill_ctype={0,0,0,0};

    if(H5D_CHUNKED==layout) {
        TESTING("chunked dataset I/O");
    } else if(H5D_COMPACT==layout) {
        TESTING("compact dataset I/O");
    } else {
        TESTING("contiguous dataset I/O");
    }

    h5_fixname(base_name, fapl, filename, sizeof filename);
    if((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        goto error;

    if((dcpl=H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5D_CHUNKED==layout) {
        if(H5Pset_chunk(dcpl, 5, ch_size) < 0) goto error;
    } else if(H5D_COMPACT==layout) {
        if(H5Pset_layout(dcpl, H5D_COMPACT) < 0) goto error;
    }
    if((ctype_id=create_compound_type()) < 0) goto error;


    /* I. Test H5D_ALLOC_TIME_LATE space allocation cases */
    if(H5D_COMPACT != layout) {
        if(H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_LATE) < 0) goto error;

        /* case for H5D_FILL_TIME_ALLOC as fill write time and fill value to be default */
        if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC) < 0) goto error;
        fillval = 0;
        nerrors += test_rdwr_cases(file, dcpl, "dset1", &fillval, H5D_FILL_TIME_ALLOC,
	           			layout, H5T_INTEGER, -1);

        /* case for H5D_FILL_TIME_NEVER as fill write time and fill value to be default */
        if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_NEVER) < 0) goto error;
        nerrors += test_rdwr_cases(file, dcpl, "dset2", &fillval, H5D_FILL_TIME_NEVER,
				layout, H5T_INTEGER, -1);

        /* case for H5D_FILL_TIME_ALLOC as fill write time and fill value is user-defined */
        if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC) < 0) goto error;
        fillval = 0x4c70f1cd;
        if(H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval) < 0) goto error;
        nerrors += test_rdwr_cases(file, dcpl, "dset3", &fillval, H5D_FILL_TIME_ALLOC,
				layout, H5T_INTEGER, -1);

        /* case for H5D_FILL_TIME_NEVER as fill write time and fill value is user-defined */
        if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_NEVER) < 0) goto error;
        if(H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval) < 0) goto error;
        nerrors += test_rdwr_cases(file, dcpl, "dset4", &fillval, H5D_FILL_TIME_NEVER,
				layout, H5T_INTEGER, -1);

        /* case for H5D_FILL_TIME_ALLOC as fill write time and fill value is undefined */
        /* This case has been tested in test_create() function */

        /* case for H5D_FILL_TIME_NEVER as fill write time and fill value is undefined */
        if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_NEVER) < 0) goto error;
        if(H5Pset_fill_value(dcpl, -1, NULL) < 0) goto error;
        nerrors += test_rdwr_cases(file, dcpl, "dset5", &fillval, H5D_FILL_TIME_NEVER,
				layout, H5T_INTEGER, -1);

        /* case for H5D_FILL_TIME_ALLOC as fill write time and fill value is user-defined
         * as compound type */
        if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC) < 0) goto error;
        HDmemset(&fill_ctype, 0, sizeof(fill_ctype));
        fill_ctype.y = 4444.4444;
        if(H5Pset_fill_value(dcpl, ctype_id, &fill_ctype) < 0) goto error;
        nerrors += test_rdwr_cases(file, dcpl, "dset11", &fill_ctype, H5D_FILL_TIME_ALLOC,
				layout, H5T_COMPOUND, ctype_id);

        if(H5Pclose(dcpl) < 0) goto error;
        if((dcpl=H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
        if(H5D_CHUNKED==layout) {
            if(H5Pset_chunk(dcpl, 5, ch_size) < 0) goto error;
        }
    }


    /* II.  Test H5D_ALLOC_TIME_EARLY space allocation cases */
    if(H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0) goto error;

    /* case for H5D_FILL_TIME_ALLOC as fill write time and fill value to be default */
    if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC) < 0) goto error;
    fillval = 0;
    nerrors += test_rdwr_cases(file, dcpl, "dset6", &fillval, H5D_FILL_TIME_ALLOC,
				layout, H5T_INTEGER, -1);

    /* case for H5D_FILL_TIME_NEVER as fill write time and fill value to be default */
    if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_NEVER) < 0) goto error;
    nerrors += test_rdwr_cases(file, dcpl, "dset7", &fillval, H5D_FILL_TIME_NEVER, layout,
        			H5T_INTEGER, -1);

    /* case for H5D_FILL_TIME_ALLOC as fill write time and fill value is user-defined */
    if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC) < 0) goto error;
    fillval = 0x4c70f1cd;
    if(H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval) < 0) goto error;
    nerrors += test_rdwr_cases(file, dcpl, "dset8", &fillval, H5D_FILL_TIME_ALLOC,
				layout, H5T_INTEGER, -1);

    /* case for H5D_FILL_TIME_NEVER as fill write time and fill value is user-defined */
    if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_NEVER) < 0) goto error;
    if(H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval) < 0) goto error;
    nerrors += test_rdwr_cases(file, dcpl, "dset9", &fillval, H5D_FILL_TIME_NEVER,
				layout, H5T_INTEGER, -1);

    /* case for H5D_FILL_TIME_ALLOC as fill write time and fill value is undefined */
    /* This case has been tested in test_create() function */

    /* case for H5D_FILL_TIME_NEVER as fill write time and fill value is undefined */
    if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_NEVER) < 0) goto error;
    if(H5Pset_fill_value(dcpl, -1, NULL) < 0) goto error;
    nerrors += test_rdwr_cases(file, dcpl, "dset10", &fillval, H5D_FILL_TIME_NEVER,
				layout, H5T_INTEGER, -1);

    /* case for H5D_FILL_TIME_ALLOC as fill write time and fill value is user-defined
     * as compound type */
    if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC) < 0) goto error;
    HDmemset(&fill_ctype, 0, sizeof(fill_ctype));
    fill_ctype.y = 4444.4444;
    if(H5Pset_fill_value(dcpl, ctype_id, &fill_ctype) < 0) goto error;
    nerrors += test_rdwr_cases(file, dcpl, "dset12", &fill_ctype, H5D_FILL_TIME_ALLOC,
                                layout, H5T_COMPOUND, ctype_id);


    if(nerrors)
	goto error;
    if(H5Pclose(dcpl) < 0) goto error;
    if(H5Tclose(ctype_id) < 0) goto error;
    if(H5Fclose(file) < 0) goto error;
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
	H5Tclose(ctype_id);
        H5Fclose(file);
    } H5E_END_TRY;
    return nerrors;
}


/*-------------------------------------------------------------------------
 * Function:	test_extend_init_integer
 *
 * Purpose:	Initializes integer values
 *
 * Return:	Success:	0
 *		Failure:	< 0
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, July  3, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
test_extend_init_integer(void *_buf, size_t nelmts, const void *_val)
{
    int *buf = (int *)_buf;                     /* Buffer to initialize */
    const int   *val = (const int *)_val;       /* Value to use */

    while(nelmts) {
        *buf++ = *val;
        nelmts--;
    } /* end while */

    return 0;
} /* end test_extend_init_integer() */


/*-------------------------------------------------------------------------
 * Function:	test_extend_verify_integer
 *
 * Purpose:	Verifies integer values
 *
 * Return:	Success:	0
 *		Failure:	< 0
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, July  3, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
test_extend_verify_integer(unsigned lineno, const hsize_t *offset,
    const void *_test_val, const void *_compare_val)
{
    const int   *test_val = (const int *)_test_val;             /* Value to test */
    const int   *compare_val = (const int *)_compare_val;       /* Value to compare against */

    /* Verify value */
    if(*test_val != *compare_val) {
        HDfprintf(stdout, "%u: Value read was not expected.\n", lineno);
        HDfprintf(stdout,"    Elmt = {%Hu, %Hu, %Hu, %Hu, %Hu}, read: %d, "
                "expected: %d\n",
                offset[0], offset[1],
                offset[2], offset[3],
                offset[4], *test_val, *compare_val);
        goto error;
    } /* end if */

    return 0;

error:
    return -1;
} /* end test_extend_verify_integer() */


/*-------------------------------------------------------------------------
 * Function:	test_extend_release_integer
 *
 * Purpose:	Release element of integer value
 *
 * Return:	Success:	0
 *		Failure:	< 0
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, July  3, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
test_extend_release_integer(void UNUSED *_elmt)
{
    return 0;
} /* end test_extend_release_integer() */


/*-------------------------------------------------------------------------
 * Function:	test_extend_init_cmpd_vl
 *
 * Purpose:	Initializes compound+vl values
 *
 * Return:	Success:	0
 *		Failure:	< 0
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, July  3, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
test_extend_init_cmpd_vl(void *_buf, size_t nelmts, const void *_val)
{
    comp_vl_datatype *buf = (comp_vl_datatype *)_buf;                     /* Buffer to initialize */
    const comp_vl_datatype   *val = (const comp_vl_datatype *)_val;       /* Value to use */

    while(nelmts) {
        /* Shallow copy all fields */
        *buf = *val;

        /* Deep copy string fields */
        buf->a = HDstrdup(val->a);
        buf->b = HDstrdup(val->b);

        buf++;
        nelmts--;
    } /* end while */

    return 0;
} /* end test_extend_init_cmpd_vl() */


/*-------------------------------------------------------------------------
 * Function:	test_extend_verify_cmpd_vl
 *
 * Purpose:	Verifies compound+vl values
 *
 * Return:	Success:	0
 *		Failure:	< 0
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, July  3, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
test_extend_verify_cmpd_vl(unsigned lineno, const hsize_t *offset,
    const void *_test_val, const void *_compare_val)
{
    const comp_vl_datatype   *test_val = (const comp_vl_datatype *)_test_val;             /* Value to test */
    const comp_vl_datatype   *compare_val = (const comp_vl_datatype *)_compare_val;       /* Value to compare against */

    /* Verify value */
    if((test_val->x != compare_val->x) ||
            HDstrcmp(test_val->a, compare_val->a) ||
            HDstrcmp(test_val->b, compare_val->b) ||
            (test_val->y != compare_val->y)) {
        HDfprintf(stdout, "%u: Value read was not expected.\n", lineno);
        HDfprintf(stdout,"    Elmt = {%Hu, %Hu, %Hu, %Hu, %Hu}, read: {%d, '%s', '%s', %d} "
                "expected: {%d, '%s', '%s', %d}\n",
                offset[0], offset[1], offset[2], offset[3], offset[4],
                test_val->x, test_val->a, test_val->b, test_val->y,
                compare_val->x, compare_val->a, compare_val->b, compare_val->y);
        goto error;
    } /* end if */

    return 0;

error:
    return -1;
} /* end test_extend_verify_cmpd_vl() */


/*-------------------------------------------------------------------------
 * Function:	test_extend_release_cmpd_vl
 *
 * Purpose:	Release element of compound+vl value
 *
 * Return:	Success:	0
 *		Failure:	< 0
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, July  3, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
test_extend_release_cmpd_vl(void *_elmt)
{
    comp_vl_datatype   *elmt = (comp_vl_datatype *)_elmt;             /* Element to free */

    /* Free memory for string fields */
    HDfree(elmt->a);
    HDfree(elmt->b);

    return 0;
} /* end test_extend_release_integer() */


/*-------------------------------------------------------------------------
 * Function:	test_extend_cases
 *
 * Purpose:	Called to test fill values with various different values
 *
 * Return:	Success:	0
 *		Failure:	number of errors
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, July  3, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
test_extend_cases(hid_t file, hid_t _dcpl, const char *dset_name,
    hsize_t *ch_size, hsize_t *start_size, hsize_t *max_size, hid_t dtype, void *fillval)
{
    hid_t	fspace = -1, mspace = -1;       /* File & memory dataspaces */
    hid_t	dset = -1;                      /* Dataset ID */
    hid_t       dcpl = -1;                      /* Dataset creation property list */
    hsize_t	extend_size[5];                 /* Dimensions to extend to */
    hsize_t	one[5] = {1, 1, 1, 1, 1};       /* Dimensions of single element dataspace */
    hsize_t	hs_size[5], hs_stride[5], hs_offset[5];
    size_t	nelmts;
    H5T_class_t dtype_class;            /* Class of datatype */
    int         (*init_rtn)(void *, size_t, const void *);
    int         (*verify_rtn)(unsigned, const hsize_t *, const void *, const void *);
    int         (*release_rtn)(void *);
    size_t      val_size;               /* Size of element */
    void        *val_rd, *should_be, *init_val, *odd_val, *even_val;
    int		val_rd_i, init_val_i = 9999;
    comp_vl_datatype	val_rd_c, init_val_c = {87, "baz", "mumble", 129};
    void	*buf = NULL;
    unsigned	odd;                    /* Whether an odd or even coord. was read */
    unsigned	i, j;                   /* Local index variables */

    /* Make copy of dataset creation property list */
    if((dcpl = H5Pcopy(_dcpl)) < 0) TEST_ERROR

#ifndef NO_FILLING
    if(H5Pset_fill_value(dcpl, dtype, fillval) < 0) TEST_ERROR
#endif

    /* Get class of datatype */
    if((dtype_class = H5Tget_class(dtype)) < 0) TEST_ERROR

    /* Create a dataspace */
    if((fspace = H5Screate_simple(5, start_size, max_size)) < 0) TEST_ERROR

    /* Create dataset */
    if((dset = H5Dcreate2(file, dset_name, dtype, fspace, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) TEST_ERROR


    /* Set up pointers to elements */
    if(dtype_class == H5T_INTEGER) {
        /* Initialize specific values for this datatype */
        val_size = sizeof(int);
        init_val = &init_val_i;
        init_rtn = test_extend_init_integer;
        verify_rtn = test_extend_verify_integer;
        release_rtn = test_extend_release_integer;
        val_rd = &val_rd_i;
        odd_val = fillval;
        even_val = &init_val_i;
    } /* end if */
    else {
        /* Sanity check */
        assert(dtype_class == H5T_COMPOUND);

        /* Initialize specific values for this datatype */
        val_size = sizeof(comp_vl_datatype);
        init_val = &init_val_c;
        init_rtn = test_extend_init_cmpd_vl;
        verify_rtn = test_extend_verify_cmpd_vl;
        release_rtn = test_extend_release_cmpd_vl;
        val_rd = &val_rd_c;
        odd_val = fillval;
        even_val = &init_val_c;
    } /* end else */


    /* Read some data and make sure it's the fill value */
    if((mspace = H5Screate_simple(5, one, NULL)) < 0) TEST_ERROR
    for(i = 0; i < 1000; i++) {
        /* Set offset for random element */
	for(j = 0; j < 5; j++)
	    hs_offset[j] = rand() % start_size[j];

        /* Select the random element */
	if(H5Sselect_hyperslab(fspace, H5S_SELECT_SET, hs_offset, NULL, one, NULL) < 0) TEST_ERROR

        /* Read the random element */
	if(H5Dread(dset, dtype, mspace, fspace, H5P_DEFAULT, val_rd) < 0) TEST_ERROR

        /* Verify the element read in */
        if(verify_rtn((unsigned)__LINE__, hs_offset, val_rd, fillval) < 0) TEST_ERROR

        /* Release any VL components */
        if(H5Dvlen_reclaim(dtype, mspace, H5P_DEFAULT, val_rd) < 0) TEST_ERROR

        /* Clear the read buffer */
        HDmemset(val_rd, 0, val_size);
    } /* end for */
    if(H5Sclose(mspace) < 0) TEST_ERROR


    /* Initialize dataspace & hyperslab info */
    for(i = 0, nelmts = 1; i < 5; i++) {
	hs_size[i] = (start_size[i] + 1) / 2;
	hs_offset[i] = 0;
	hs_stride[i] = 2;
	nelmts *= hs_size[i];
    } /* end for */

    /* Check for overflow */
    assert((nelmts * val_size) == (hsize_t)((size_t)(nelmts * val_size)));

    /* Allocate & initialize buffer */
    buf = HDmalloc((size_t)(nelmts * val_size));
    init_rtn(buf, nelmts, init_val);

    /* Create dataspace describing memory buffer */
    if((mspace = H5Screate_simple(5, hs_size, hs_size)) < 0) TEST_ERROR

    /* Select elements within file dataspace */
    if(H5Sselect_hyperslab(fspace, H5S_SELECT_SET, hs_offset, hs_stride, hs_size, NULL) < 0) TEST_ERROR

    /* Write to all even data locations */
    if(H5Dwrite(dset, dtype, mspace, fspace, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* Close memory dataspace */
    if(H5Sclose(mspace) < 0) TEST_ERROR


    /* Read some data and make sure it's the right value */
    if((mspace = H5Screate_simple(5, one, NULL)) < 0) TEST_ERROR
    for(i = 0; i < 1000; i++) {
        /* Set offset for random element */
	for(j = 0, odd = 0; j < 5; j++) {
	    hs_offset[j] = rand() % start_size[j];
	    odd += (unsigned)(hs_offset[j] % 2);
	} /* end for */

        /* Select the random element */
	if(H5Sselect_hyperslab(fspace, H5S_SELECT_SET, hs_offset, NULL, one, NULL) < 0) TEST_ERROR

        /* Read the random element */
	if(H5Dread(dset, dtype, mspace, fspace, H5P_DEFAULT, val_rd) < 0) TEST_ERROR

        /* Verify the element read in */
	should_be = odd ? odd_val : even_val;
        if(verify_rtn((unsigned)__LINE__, hs_offset, val_rd, should_be) < 0) TEST_ERROR

        /* Release any VL components */
        if(H5Dvlen_reclaim(dtype, mspace, H5P_DEFAULT, val_rd) < 0) TEST_ERROR

        /* Clear the read buffer */
        HDmemset(val_rd, 0, val_size);
    } /* end for */
    if(H5Sclose(mspace) < 0) TEST_ERROR


    /* Extend the dataset one element in each dimension */
    for(i = 0; i < 5; i++)
        extend_size[i] = start_size[i] + 1;
    if(H5Dset_extent(dset, extend_size) < 0) TEST_ERROR

    /* Re-open file dataspace */
    if(H5Sclose(fspace) < 0) TEST_ERROR
    if((fspace = H5Dget_space(dset)) < 0) TEST_ERROR


    /* Read some data and make sure it's the right value */
    if((mspace = H5Screate_simple(5, one, NULL)) < 0) TEST_ERROR
    for(i = 0; i < 1000; i++) {
        /* Set offset for random element */
	for(j = 0, odd = 0; j < 5; j++) {
	    hs_offset[j] = rand() % extend_size[j];
	    if(hs_offset[j] >= start_size[j])
		odd = 1;
	    else
  		odd += (unsigned)(hs_offset[j] % 2);
	} /* end for */

        /* Select the random element */
	if(H5Sselect_hyperslab(fspace, H5S_SELECT_SET, hs_offset, NULL, one, NULL) < 0) TEST_ERROR

        /* Read the random element */
	if(H5Dread(dset, dtype, mspace, fspace, H5P_DEFAULT, val_rd) < 0) TEST_ERROR

        /* Verify the element read in */
	should_be = odd ? odd_val : even_val;
        if(verify_rtn((unsigned)__LINE__, hs_offset, val_rd, should_be) < 0) TEST_ERROR

        /* Release any VL components */
        if(H5Dvlen_reclaim(dtype, mspace, H5P_DEFAULT, val_rd) < 0) TEST_ERROR

        /* Clear the read buffer */
        HDmemset(val_rd, 0, val_size);
    } /* end for */
    if(H5Sclose(mspace) < 0) TEST_ERROR


    /* Extend the dataset to the maximum dimension sizes */
    if(H5Dset_extent(dset, max_size) < 0) TEST_ERROR

    /* Re-open file dataspace */
    if(H5Sclose(fspace) < 0) TEST_ERROR
    if((fspace = H5Dget_space(dset)) < 0) TEST_ERROR


    /* Read some data and make sure it's the right value */
    if((mspace = H5Screate_simple(5, one, NULL)) < 0) TEST_ERROR
    for(i = 0; i < 1000; i++) {
        /* Set offset for random element */
	for(j = 0, odd = 0; j < 5; j++) {
	    hs_offset[j] = rand() % max_size[j];
	    if(hs_offset[j] >= start_size[j])
		odd = 1;
	    else
  		odd += (unsigned)(hs_offset[j] % 2);
	} /* end for */

        /* Select the random element */
	if(H5Sselect_hyperslab(fspace, H5S_SELECT_SET, hs_offset, NULL, one, NULL) < 0) TEST_ERROR

        /* Read the random element */
	if(H5Dread(dset, dtype, mspace, fspace, H5P_DEFAULT, val_rd) < 0) TEST_ERROR

        /* Verify the element read in */
	should_be = odd ? odd_val : even_val;
        if(verify_rtn((unsigned)__LINE__, hs_offset, val_rd, should_be) < 0) TEST_ERROR

        /* Release any VL components */
        if(H5Dvlen_reclaim(dtype, mspace, H5P_DEFAULT, val_rd) < 0) TEST_ERROR

        /* Clear the read buffer */
        HDmemset(val_rd, 0, val_size);
    } /* end for */
    if(H5Sclose(mspace) < 0) TEST_ERROR


    /* Shrink the dataset to half of it's maximum size, plus 1/2 of a chunk */
    for(i = 0; i < 5; i++)
        extend_size[i] = (max_size[i] / 2) + (ch_size[i] / 2);
    if(H5Dset_extent(dset, extend_size) < 0) TEST_ERROR

    /* Re-open file dataspace */
    if(H5Sclose(fspace) < 0) TEST_ERROR
    if((fspace = H5Dget_space(dset)) < 0) TEST_ERROR


    /* Read some data and make sure it's the right value */
    if((mspace = H5Screate_simple(5, one, NULL)) < 0) TEST_ERROR
    for(i = 0; i < 1000; i++) {
        /* Set offset for random element */
	for(j = 0, odd = 0; j < 5; j++) {
	    hs_offset[j] = rand() % extend_size[j];
	    if(hs_offset[j] >= start_size[j])
		odd = 1;
	    else
  		odd += (unsigned)(hs_offset[j] % 2);
	} /* end for */

        /* Select the random element */
	if(H5Sselect_hyperslab(fspace, H5S_SELECT_SET, hs_offset, NULL, one, NULL) < 0) TEST_ERROR

        /* Read the random element */
	if(H5Dread(dset, dtype, mspace, fspace, H5P_DEFAULT, val_rd) < 0) TEST_ERROR

        /* Verify the element read in */
	should_be = odd ? odd_val : even_val;
        if(verify_rtn((unsigned)__LINE__, hs_offset, val_rd, should_be) < 0) TEST_ERROR

        /* Release any VL components */
        if(H5Dvlen_reclaim(dtype, mspace, H5P_DEFAULT, val_rd) < 0) TEST_ERROR

        /* Clear the read buffer */
        HDmemset(val_rd, 0, val_size);
    } /* end for */
    if(H5Sclose(mspace) < 0) TEST_ERROR


    /* Extend the dataset's size by one element, in a dimension that won't
     *  cause additional chunks to be needed */
    extend_size[2] += 1;
    if(H5Dset_extent(dset, extend_size) < 0) TEST_ERROR

    /* Re-open file dataspace */
    if(H5Sclose(fspace) < 0) TEST_ERROR
    if((fspace = H5Dget_space(dset)) < 0) TEST_ERROR


    /* Create dataspace for single element sized bufer */
    if((mspace = H5Screate_simple(5, one, NULL)) < 0) TEST_ERROR

    /* Set location for "top-most" element in dataset to write */
    for(i = 0; i < 5; i++)
        hs_offset[i] = extend_size[i] - 1;

    /* Select the element */
    if(H5Sselect_hyperslab(fspace, H5S_SELECT_SET, hs_offset, NULL, one, NULL) < 0) TEST_ERROR

    /* Write one element in a chunk that's 'partial' and overwrite a fill
     *  value that was initialized in the H5Dset_extent() routine.  This will
     *  overwrite a fill-value element, which must be de-allocated properly or
     *  next read of another fill-value initialized element in this chunk will
     *  fail.
     */
    if(H5Dwrite(dset, dtype, mspace, fspace, H5P_DEFAULT, buf) < 0) TEST_ERROR

    /* Read value back in */
    if(H5Dread(dset, dtype, mspace, fspace, H5P_DEFAULT, val_rd) < 0) TEST_ERROR

    /* Verify the element read in is the value written out */
    if(verify_rtn((unsigned)__LINE__, hs_offset, val_rd, buf) < 0) TEST_ERROR

    /* Set the element back to fillval */
    if(H5Dwrite(dset, dtype, mspace, fspace, H5P_DEFAULT, fillval) < 0) TEST_ERROR

    /* Release any VL components */
    if(H5Dvlen_reclaim(dtype, mspace, H5P_DEFAULT, val_rd) < 0) TEST_ERROR

    /* Clear the read buffer */
    HDmemset(val_rd, 0, val_size);


    /* Set location for another element initialized by H5Dset_extent() */
    hs_offset[3] -= 1;

    /* Select the element */
    if(H5Sselect_hyperslab(fspace, H5S_SELECT_SET, hs_offset, NULL, one, NULL) < 0) TEST_ERROR

    /* Read value back in */
    if(H5Dread(dset, dtype, mspace, fspace, H5P_DEFAULT, val_rd) < 0) TEST_ERROR

    /* Verify the element read in is the fill-value */
    if(verify_rtn((unsigned)__LINE__, hs_offset, val_rd, fillval) < 0) TEST_ERROR

    /* Release any VL components */
    if(H5Dvlen_reclaim(dtype, mspace, H5P_DEFAULT, val_rd) < 0) TEST_ERROR

    /* Clear the read buffer */
    HDmemset(val_rd, 0, val_size);


    /* Read some data and make sure it's the right value */
    for(i = 0; i < 1000; i++) {
        /* Set offset for random element */
	for(j = 0, odd = 0; j < 5; j++) {
	    hs_offset[j] = rand() % extend_size[j];
	    if(hs_offset[j] >= start_size[j])
		odd = 1;
	    else
  		odd += (unsigned)(hs_offset[j] % 2);
	} /* end for */

        /* Select the random element */
	if(H5Sselect_hyperslab(fspace, H5S_SELECT_SET, hs_offset, NULL, one, NULL) < 0) TEST_ERROR

        /* Read the random element */
	if(H5Dread(dset, dtype, mspace, fspace, H5P_DEFAULT, val_rd) < 0) TEST_ERROR

        /* Verify the element read in */
	should_be = odd ? odd_val : even_val;
        if(verify_rtn((unsigned)__LINE__, hs_offset, val_rd, should_be) < 0) TEST_ERROR

        /* Release any VL components */
        if(H5Dvlen_reclaim(dtype, mspace, H5P_DEFAULT, val_rd) < 0) TEST_ERROR

        /* Clear the read buffer */
        HDmemset(val_rd, 0, val_size);
    } /* end for */
    if(H5Sclose(mspace) < 0) TEST_ERROR


    /* Release elements & memory buffer */
    for(i = 0; i < nelmts; i++)
        release_rtn((void *)((char *)buf + (val_size * i)));
    HDfree(buf);
    buf = NULL;

    /* Cleanup IDs */
    if(H5Pclose(dcpl) < 0) TEST_ERROR
    if(H5Dclose(dset) < 0) TEST_ERROR
    if(H5Sclose(fspace) < 0) TEST_ERROR

    return 0;

error:
    if(buf)
        HDfree(buf);
    H5E_BEGIN_TRY {
	H5Pclose(dcpl);
	H5Dclose(dset);
	H5Sclose(fspace);
	H5Sclose(mspace);
    } H5E_END_TRY;

    return -1;
} /* end test_extend_cases() */


/*-------------------------------------------------------------------------
 * Function:	test_extend
 *
 * Purpose:	Test that filling works okay when a dataset is extended.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Monday, October  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_extend(hid_t fapl, const char *base_name, H5D_layout_t layout)
{
    hid_t	file = -1;              /* File ID */
    hid_t	dcpl = -1;              /* Dataset creation property list ID */
    hid_t	cmpd_vl_tid = -1;       /* Compound+vl datatype ID */
    hsize_t	start_size[5] = {8, 8, 8, 4, 2};
    hsize_t	max_size[5] = {32, 32, 32, 16, 8};
    hsize_t	ch_size[5] = {1, 8, 8, 4, 2};
#ifdef NO_FILLING
    int		fillval_i = 0;
#else
    int		fillval_i = 0x4c70f1cd;
#endif
    comp_vl_datatype fillval_c = {32, "foo", "bar", 64};         /* Fill value for compound+vl datatype tests */
    char	filename[1024];

    /* Print testing message */
    if(H5D_CHUNKED == layout)
	TESTING("chunked dataset extend")
    else
	TESTING("contiguous dataset extend")

    /* Create dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR
    if(H5D_CHUNKED == layout)
	if(H5Pset_chunk(dcpl, 5, ch_size) < 0) TEST_ERROR

#if 1
    /*
     * Remove this once contiguous datasets can support extensions in other
     * than the slowest varying dimension.  The purpose of this block is to
     * make only the slowest varying dimension extendible and yet have the
     * same total number of elements as originally.
     *
     * If this is removed prematurely then you will get an error `only the
     * first dimension can be extendible' as long as the test isn't skipped
     * below.
     */
    if(H5D_CONTIGUOUS==layout) {
	max_size[0] = (max_size[0] * max_size[1] * max_size[2] *
		       max_size[3] * max_size[4]) /
		      (start_size[1] * start_size[2] * start_size[3] * start_size[4]);
	max_size[1] = start_size[1];
	max_size[2] = start_size[2];
	max_size[3] = start_size[3];
	max_size[4] = start_size[4];
    }
#endif

#if 1
    /*
     * Remove this once internal contiguous datasets can support
     * extending. If it's removed prematurely you will get an error
     * `extendible contiguous non-external dataset' as long as the test isn't
     * skipped below.
     */
    if(H5D_CONTIGUOUS==layout) {
        int fd;
        hsize_t	nelmts;

	nelmts = max_size[0]*max_size[1]*max_size[2]*max_size[3]*max_size[4];
	if((fd=HDopen(FILE_NAME_RAW, O_RDWR|O_CREAT|O_TRUNC, 0666)) < 0 ||
	    HDclose(fd) < 0) goto error;
	if(H5Pset_external(dcpl, FILE_NAME_RAW, (off_t)0, (hsize_t)nelmts*sizeof(int)) < 0)
	    goto error;
    }
#endif

#if 1
    /*
     * Remove this when contiguous datasets can be exended to some
     * predetermined fininte size, even if it's just in the slowest varying
     * dimension.  If it's removed prematurely then you'll get one of the
     * errors described above or `unable to select fill value region'.
     */
    if(H5D_CONTIGUOUS==layout) {
	SKIPPED();
	puts("    Not implemented yet -- needs H5S_SELECT_DIFF operator");
	goto skip;
    }
#endif

    /* Create a file and dataset */
    h5_fixname(base_name, fapl, filename, sizeof filename);
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Get the compound+vl datatype */
    if((cmpd_vl_tid = create_compound_vl_type()) < 0) TEST_ERROR

    /* Test integer datatype case */
    if(test_extend_cases(file, dcpl, "dset1", ch_size, start_size, max_size,
            H5T_NATIVE_INT, &fillval_i) < 0) TEST_ERROR

    /* Test compound+vl datatype datatype case */
    if(test_extend_cases(file, dcpl, "dset2", ch_size, start_size, max_size,
            cmpd_vl_tid, &fillval_c) < 0) TEST_ERROR

    /* Cleanup */
    if(H5Tclose(cmpd_vl_tid) < 0) TEST_ERROR
    if(H5Pclose(dcpl) < 0) TEST_ERROR
    if(H5Fclose(file) < 0) TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Tclose(cmpd_vl_tid);
	H5Pclose(dcpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;

skip:
    H5E_BEGIN_TRY {
	H5Pclose(dcpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return 0;
} /* end test_extend() */


/*-------------------------------------------------------------------------
 * Function:    test_compatible
 *
 * Purpose:     Tests fill value and dataspace for datasets created by v1.4
 *              library.
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 * Programmer:  Raymond Lu
 *              Feb 27, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_compatible(void)
{
    hid_t      file=-1, dset1=-1, dset2=-1;
    hid_t      dcpl1=-1, dcpl2=-1, fspace=-1, mspace=-1;
    int        rd_fill=0, fill_val=4444, val_rd=0;
    hsize_t    dims[2], one[2]={1,1};
    hsize_t   hs_offset[2]={3,4};
    H5D_fill_value_t status;
    char       *srcdir = getenv("srcdir"); /*where the src code is located*/
    char       testfile[512]="";  /* test file name */

    TESTING("contiguous dataset compatibility with v. 1.4");

  /* Generate correct name for test file by prepending the source path */
  if(srcdir && ((strlen(srcdir) + strlen(FILE_COMPATIBLE) + 1) <
     sizeof(testfile))) {
     HDstrcpy(testfile, srcdir);
     HDstrcat(testfile, "/");
  }
  HDstrcat(testfile, FILE_COMPATIBLE);

  if((file = H5Fopen(testfile, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0) {
      printf("    Could not open file %s. Try set $srcdir to point at the "
              "source directory of test\n", testfile);
      goto error;
  }

  if((dset1 = H5Dopen2(file, "dset1", H5P_DEFAULT)) < 0) goto error;
  if((dcpl1 = H5Dget_create_plist(dset1)) < 0) goto error;
  if(H5Pfill_value_defined(dcpl1, &status) < 0) goto error;
  if(status != H5D_FILL_VALUE_UNDEFINED) {
      H5_FAILED();
      printf("    %d: Got a different fill value than what was set.",__LINE__);
      printf("    Got status=%ld, suppose to be H5D_FILL_VALUE_UNDEFINED\n",
            (long)status);
      goto error;
  }
  if((fspace = H5Dget_space(dset1)) < 0) goto error;
  if(H5Sget_simple_extent_dims(fspace, dims, NULL) < 0) goto error;
  if(dims[0] != 8 || dims[1] != 8) {
      H5_FAILED();
      puts("    Got a different dimension size than what was set.");
      printf("    Got dims[0]=%ld, dims[1]=%ld, set 8x8\n", (long)dims[0], (long)dims[1]);
      goto error;
  }
  if((mspace = H5Screate_simple(2, one, NULL)) < 0) goto error;
  if(H5Sselect_hyperslab(fspace, H5S_SELECT_SET, hs_offset, NULL, one, NULL) < 0)
      goto error;
  if(H5Dread(dset1, H5T_NATIVE_INT, mspace, fspace, H5P_DEFAULT, &val_rd) < 0)
      goto error;
  if(val_rd != 0) {
      H5_FAILED();
      puts("    Got a different value than what was set.");
      printf("    Got %ld, set 0\n", (long)val_rd);
      goto error;
  }
  if(H5Pclose(dcpl1) < 0) goto error;
  if(H5Sclose(fspace) < 0) goto error;
  if(H5Sclose(mspace) < 0) goto error;
  if(H5Dclose(dset1) < 0) goto error;


  if((dset2 = H5Dopen2(file, "dset2", H5P_DEFAULT)) < 0) goto error;
  if((dcpl2 = H5Dget_create_plist(dset2)) < 0) goto error;
  if(H5Pfill_value_defined(dcpl2, &status) < 0) goto error;
  if(status != H5D_FILL_VALUE_USER_DEFINED) {
      H5_FAILED();
      printf("    %d: Got a different fill value than what was set.",__LINE__);
      printf("    Got status=%ld, suppose to be H5D_FILL_VALUE_USER_DEFINED\n",
            (long)status);
      goto error;
  }
  if(H5Pget_fill_value(dcpl2, H5T_NATIVE_INT, &rd_fill) < 0) goto error;
  if(rd_fill != fill_val) {
      H5_FAILED();
      printf("    %d: Got a different fill value than what was set.",__LINE__);
      printf("    Got %ld, set %ld\n", (long)rd_fill, (long)fill_val);
      goto error;
  }
  fspace = -1;
  if((fspace = H5Dget_space(dset2)) < 0) goto error;
  dims[0] = dims[1] = (hsize_t)-1;
  if(H5Sget_simple_extent_dims(fspace, dims, NULL) < 0) goto error;
  if(dims[0] != 8 || dims[1] != 8) {
      H5_FAILED();
      puts("    Got a different dimension size than what was set.");
      printf("    Got dims[0]=%ld, dims[1]=%ld, set 8x8\n", (long)dims[0], (long)dims[1]);
      goto error;
  }
  if((mspace=H5Screate_simple(2, one, NULL)) < 0) goto error;
  if(H5Sselect_hyperslab(fspace, H5S_SELECT_SET, hs_offset, NULL, one, NULL) < 0)
      goto error;
  if(H5Dread(dset2, H5T_NATIVE_INT, mspace, fspace, H5P_DEFAULT, &val_rd) < 0)
      goto error;
  if(val_rd != fill_val) {
      H5_FAILED();
      puts("    Got a different value than what was set.");
      printf("    Got %ld, set %ld\n", (long)val_rd, (long)fill_val);
      goto error;
  }
  if(H5Pclose(dcpl2) < 0) goto error;
  if(H5Sclose(fspace) < 0) goto error;
  if(H5Sclose(mspace) < 0) goto error;
  if(H5Dclose(dset2) < 0) goto error;

  if(H5Fclose(file) < 0) goto error;

  PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl1);
        H5Sclose(fspace);
        H5Sclose(mspace);
        H5Dclose(dset1);
        H5Pclose(dcpl2);
        H5Sclose(fspace);
        H5Dclose(dset2);
        H5Fclose(file);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Tests fill values
 *
 * Return:	Success:
 *
 *		Failure:
 *
 * Programmer:	Robb Matzke
 *              Thursday, October  1, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, char *argv[])
{
    int	nerrors=0, argno, test_contig=1, test_chunk=1, test_compact=1;
    hid_t	fapl = (-1), fapl2 = (-1);    /* File access property lists */
    hbool_t new_format;     /* Whether to use the new format or not */

    if(argc >= 2) {
        test_contig = test_chunk = test_compact = 0;
        for(argno = 1; argno < argc; argno++) {
            if(!strcmp(argv[argno], "contiguous"))
                test_contig = 1;
            else if(!strcmp(argv[argno], "chunked"))
                test_chunk = 1;
            else if(!strcmp(argv[argno], "compact"))
                test_compact =1;
            else {
                fprintf(stderr, "usage: %s [contiguous] [chunked] [compact]\n", argv[0]);
                exit(1);
            }
        } /* end for */
    } /* end if */

    h5_reset();
    fapl = h5_fileaccess();

    /* Property list tests */
    nerrors += test_getset();
    nerrors += test_getset_vl(fapl);

    /* Copy the file access property list */
    if((fapl2 = H5Pcopy(fapl)) < 0) TEST_ERROR

    /* Set the "use the latest version of the format" bounds for creating objects in the file */
    if(H5Pset_libver_bounds(fapl2, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0) TEST_ERROR

    /* Loop over using new group format */
    for(new_format = FALSE; new_format <= TRUE; new_format++) {
        hid_t my_fapl;

        /* Set the FAPL for the type of format */
        if(new_format) {
            puts("\nTesting with new file format:");
            my_fapl = fapl2;
        } /* end if */
        else {
            puts("Testing with old file format:");
            my_fapl = fapl;
        } /* end else */

        /* Chunked storage layout tests */
        if(test_chunk) {
            nerrors += test_create(my_fapl, FILENAME[0], H5D_CHUNKED);
            nerrors += test_rdwr  (my_fapl, FILENAME[2], H5D_CHUNKED);
            nerrors += test_extend(my_fapl, FILENAME[4], H5D_CHUNKED);
        } /* end if */

        /* Contiguous storage layout tests */
        if(test_contig) {
            nerrors += test_create(my_fapl, FILENAME[1], H5D_CONTIGUOUS);
            nerrors += test_rdwr  (my_fapl, FILENAME[3], H5D_CONTIGUOUS);
            nerrors += test_extend(my_fapl, FILENAME[5], H5D_CONTIGUOUS);
            nerrors += test_compatible();
        } /* end if */

        /* Compact dataset storage tests */
        if(test_compact) {
            nerrors += test_create(my_fapl, FILENAME[6], H5D_COMPACT);
            nerrors += test_rdwr  (my_fapl, FILENAME[7], H5D_COMPACT);
        } /* end if */
    } /* end for */

    /* Close 2nd FAPL */
    H5Pclose(fapl2);

    /* Verify symbol table messages are cached */
    nerrors += (h5_verify_cached_stabs(FILENAME, fapl) < 0 ? 1 : 0);

    if(nerrors)
        goto error;
    puts("All fill value tests passed.");

    if(h5_cleanup(FILENAME, fapl))
        HDremove(FILE_NAME_RAW);

    return 0;

error:
    puts("***** FILL VALUE TESTS FAILED *****");
    return 1;
}

