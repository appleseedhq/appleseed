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
 *              Friday, April 10, 1998
 *
 * Purpose:	Tests hard, soft (symbolic) & external links.
 */

/*
 * This file needs to access private information from the H5G package.
 * This file also needs to access the group testing code.
 */
#define H5G_PACKAGE
#define H5G_TESTING

#include "h5test.h"
#include "H5srcdir.h"
#include "H5Gpkg.h"		/* Groups 				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Lprivate.h"         /* Links                                */

/* File for external link test.  Created with gen_udlinks.c */
#define LINKED_FILE  "be_extlink2.h5"

#ifdef H5_VMS
#define TMPDIR          "[.tmp]"
#define TMPDIR2         "[.tmp2]"
#else /* H5_VMS */
#define TMPDIR          "tmp/"
#define TMPDIR2         "tmp2/"
#endif /* H5_VMS */

/* Symlinks for external link symlink test */
#define SYMLINK1  TMPDIR "sym1.h5"
#define SYMLINK2  TMPDIR2 "sym2.h5"

const char *FILENAME[] = {
    "links0",
    "links1",
    "links2",
    "links3",
    "links4a", /* 4 */
    "links4b", /* 5 */
    "links4c", /* 6 */
    "links4d", /* 7 */
    "links5",  /* 8 */
    "links6",  /* 9 */
    "links7",  /* 10 */
    "links8",  /* 11 */
    "extlinks0",	/* 12: main files */
    TMPDIR "extlinks0",	/* 13: */
    "extlinks1",	/* 14: target files */
    TMPDIR "extlinks1",	/* 15: */
    "extlinks2",	/* 16: */
    TMPDIR "extlinks2",	/* 17: */
    "extlinks3",	/* 18: */
    TMPDIR "extlinks3",	/* 19: */
    "extlinks4",	/* 20: */
    TMPDIR "extlinks4",	/* 21: */
    "extlinks5",	/* 22: */
    TMPDIR "extlinks6",	/* 23: */
    "extlinks7",	/* 24: */
    TMPDIR "extlinks7",	/* 25: */
    TMPDIR "extlinks8",	/* 26: */
    "extlinks9",	/* 27: */
    TMPDIR "extlinks9",	/* 28: */
    "extlinks10",	/* 29: */ /* TESTS for windows */
    TMPDIR "extlinks10",/* 30: */
    TMPDIR "extlinks11",/* 31: */
    TMPDIR "extlinks12",/* 32: */
    "extlinks13",	/* 33: */
    TMPDIR "extlinks13",/* 34: */
    TMPDIR "extlinks14",/* 35: */
    TMPDIR "extlinks15",/* 36: */
    "extlinks16A",	/* 37: */ /* TESTS for H5P_set_elink_fapl */
    "extlinks16B",	/* 38: */
    "extlinks17",	/* 39: */
    "extlinks18A",	/* 40: */
    "extlinks18B",	/* 41: */
    "extlinks19A",	/* 42: */
    "extlinks19B",	/* 43: */
    "extlinks20",	/* 44: */
    "extlinks21A",	/* 45: Files for symlink() tests*/
    TMPDIR2 "extlinks21B",/* 46: */
    TMPDIR2 "extlinks21C",/* 47: */
    "extlinks21C",	/* 48: (same as #47, only without the TMPDIR2 prefix) */
    TMPDIR "extlinks21D",/* 49: */
    TMPDIR "extlinks21E",/* 50: */
    "extlinks21E",	/* 51: (same as #50, only without the TMPDIR prefix) */
    NULL
};

#define FAMILY_SIZE	1024
#define CORE_INCREMENT  1024
#define NUM40		40

/* do not do check_all_closed() for "ext*" files and "tmp/ext*" */
#define EXTSTOP		12

#define LINK_BUF_SIZE   1024
#define NAME_BUF_SIZE   1024
#define MAX_NAME_LEN    ((64*1024)+1024)

/* Link type IDs */
#define UD_HARD_TYPE 201
#define UD_CB_TYPE H5L_TYPE_MAX
#define UD_PLIST_TYPE 128
#define UD_CBFAIL_TYPE UD_PLIST_TYPE
#define UD_ERROR_TYPE 189
#define UD_BAD_TYPE1 H5L_TYPE_HARD
#define UD_BAD_TYPE2 (H5L_TYPE_UD_MIN - 5)
#define UD_BAD_VERS (H5L_LINK_CLASS_T_VERS + 1)

#define DEST_PROP_NAME "destination_group"
#define REREG_TARGET_NAME "rereg_target"

#define UD_CB_LINK_NAME "ud_callback_link"
#define NEW_UD_CB_LINK_NAME "ud_callback_link2"
#define UD_CB_TARGET "ud_target"
#define UD_CB_TARGET_LEN 10

#define LE_FILENAME "le_extlink1.h5"
#define BE_FILENAME "be_extlink1.h5"

#define ELINK_CB_FAM_SIZE (hsize_t) 100

#define H5L_DIM1 100
#define H5L_DIM2 100

#define FILTER_FILESIZE_MAX_FRACTION .9

/* Creation order macros */
#define CORDER_GROUP_NAME       "corder_group"
#define CORDER_SOFT_GROUP_NAME  "corder_soft_group"
#define CORDER_NLINKS               18
#define CORDER_ITER_STOP            3
#define CORDER_EST_ENTRY_LEN        9

/* Timestamp macros */
#define TIMESTAMP_GROUP_1       "timestamp1"
#define TIMESTAMP_GROUP_2       "timestamp2"

/* Link iteration struct */
typedef struct {
    H5_iter_order_t order;      /* Direction of iteration */
    unsigned ncalled;           /* # of times callback is entered */
    unsigned nskipped;          /* # of links skipped */
    int stop;                   /* # of iterations to stop after */
    int64_t curr;               /* Current creation order value */
    size_t max_visit;           /* Size of "visited link" flag array */
    hbool_t *visited;           /* Pointer to array of "visited link" flags */
} link_iter_info_t;

/* Link visit structs */
typedef struct {
    const char *path;           /* Path to link */
    H5L_type_t type;            /* Type of link */
} link_visit_t;
static const link_visit_t lvisit0[] = {
    {"Dataset_zero", H5L_TYPE_HARD},
    {"Group1", H5L_TYPE_HARD},
    {"Group1/Dataset_one", H5L_TYPE_HARD},
    {"Group1/Group2", H5L_TYPE_HARD},
    {"Group1/Group2/Dataset_two", H5L_TYPE_HARD},
    {"Group1/Group2/Type_two", H5L_TYPE_HARD},
    {"Group1/Group2/hard_zero", H5L_TYPE_HARD},
    {"Group1/Type_one", H5L_TYPE_HARD},
    {"Group1/hard_one", H5L_TYPE_HARD},
    {"Type_zero", H5L_TYPE_HARD},
    {"ext_dangle", H5L_TYPE_EXTERNAL},
    {"ext_one", H5L_TYPE_EXTERNAL},
    {"hard_one", H5L_TYPE_HARD},
    {"hard_two", H5L_TYPE_HARD},
    {"hard_zero", H5L_TYPE_HARD},
    {"soft_dangle", H5L_TYPE_SOFT},
    {"soft_one", H5L_TYPE_SOFT},
    {"soft_two", H5L_TYPE_SOFT}
};
static const link_visit_t lvisit1[] = {
    {"Dataset_one", H5L_TYPE_HARD},
    {"Group2", H5L_TYPE_HARD},
    {"Group2/Dataset_two", H5L_TYPE_HARD},
    {"Group2/Type_two", H5L_TYPE_HARD},
    {"Group2/hard_zero", H5L_TYPE_HARD},
    {"Group2/hard_zero/Dataset_zero", H5L_TYPE_HARD},
    {"Group2/hard_zero/Group1", H5L_TYPE_HARD},
    {"Group2/hard_zero/Type_zero", H5L_TYPE_HARD},
    {"Group2/hard_zero/ext_dangle", H5L_TYPE_EXTERNAL},
    {"Group2/hard_zero/ext_one", H5L_TYPE_EXTERNAL},
    {"Group2/hard_zero/hard_one", H5L_TYPE_HARD},
    {"Group2/hard_zero/hard_two", H5L_TYPE_HARD},
    {"Group2/hard_zero/hard_zero", H5L_TYPE_HARD},
    {"Group2/hard_zero/soft_dangle", H5L_TYPE_SOFT},
    {"Group2/hard_zero/soft_one", H5L_TYPE_SOFT},
    {"Group2/hard_zero/soft_two", H5L_TYPE_SOFT},
    {"Type_one", H5L_TYPE_HARD},
    {"hard_one", H5L_TYPE_HARD}
};
static const link_visit_t lvisit2[] = {
    {"Dataset_two", H5L_TYPE_HARD},
    {"Type_two", H5L_TYPE_HARD},
    {"hard_zero", H5L_TYPE_HARD},
    {"hard_zero/Dataset_zero", H5L_TYPE_HARD},
    {"hard_zero/Group1", H5L_TYPE_HARD},
    {"hard_zero/Group1/Dataset_one", H5L_TYPE_HARD},
    {"hard_zero/Group1/Group2", H5L_TYPE_HARD},
    {"hard_zero/Group1/Type_one", H5L_TYPE_HARD},
    {"hard_zero/Group1/hard_one", H5L_TYPE_HARD},
    {"hard_zero/Type_zero", H5L_TYPE_HARD},
    {"hard_zero/ext_dangle", H5L_TYPE_EXTERNAL},
    {"hard_zero/ext_one", H5L_TYPE_EXTERNAL},
    {"hard_zero/hard_one", H5L_TYPE_HARD},
    {"hard_zero/hard_two", H5L_TYPE_HARD},
    {"hard_zero/hard_zero", H5L_TYPE_HARD},
    {"hard_zero/soft_dangle", H5L_TYPE_SOFT},
    {"hard_zero/soft_one", H5L_TYPE_SOFT},
    {"hard_zero/soft_two", H5L_TYPE_SOFT}
};

typedef struct {
    unsigned idx;               /* Index in link visit structure */
    const link_visit_t *info;   /* Pointer to the link visit structure to use */
} lvisit_ud_t;


/* Object visit structs */
typedef struct {
    const char *path;           /* Path to object */
    H5O_type_t type;            /* Type of object */
} obj_visit_t;
static const obj_visit_t ovisit0_old[] = {
    {".", H5O_TYPE_GROUP},
    {"Dataset_zero", H5O_TYPE_DATASET},
    {"Group1", H5O_TYPE_GROUP},
    {"Group1/Dataset_one", H5O_TYPE_DATASET},
    {"Group1/Group2", H5O_TYPE_GROUP},
    {"Group1/Group2/Dataset_two", H5O_TYPE_DATASET},
    {"Group1/Group2/Type_two", H5O_TYPE_NAMED_DATATYPE},
    {"Group1/Type_one", H5O_TYPE_NAMED_DATATYPE},
    {"Type_zero", H5O_TYPE_NAMED_DATATYPE}
};
static const obj_visit_t ovisit0_new[] = {
    {".", H5O_TYPE_GROUP},
    {"Dataset_zero", H5O_TYPE_DATASET},
    {"Group1", H5O_TYPE_GROUP},
    {"Group1/Dataset_one", H5O_TYPE_DATASET},
    {"Group1/Group2", H5O_TYPE_GROUP},
    {"Group1/Group2/Dataset_two", H5O_TYPE_DATASET},
    {"Group1/Group2/Type_two", H5O_TYPE_NAMED_DATATYPE},
    {"Group1/Type_one", H5O_TYPE_NAMED_DATATYPE},
    {"Type_zero", H5O_TYPE_NAMED_DATATYPE}
};
static const obj_visit_t ovisit1_old[] = {
    {".", H5O_TYPE_GROUP},
    {"Dataset_one", H5O_TYPE_DATASET},
    {"Group2", H5O_TYPE_GROUP},
    {"Group2/Dataset_two", H5O_TYPE_DATASET},
    {"Group2/Type_two", H5O_TYPE_NAMED_DATATYPE},
    {"Group2/hard_zero", H5O_TYPE_GROUP},
    {"Group2/hard_zero/Dataset_zero", H5O_TYPE_DATASET},
    {"Group2/hard_zero/Type_zero", H5O_TYPE_NAMED_DATATYPE},
    {"Type_one", H5O_TYPE_NAMED_DATATYPE}
};
static const obj_visit_t ovisit1_new[] = {
    {".", H5O_TYPE_GROUP},
    {"Dataset_one", H5O_TYPE_DATASET},
    {"Group2", H5O_TYPE_GROUP},
    {"Group2/Dataset_two", H5O_TYPE_DATASET},
    {"Group2/Type_two", H5O_TYPE_NAMED_DATATYPE},
    {"Group2/hard_zero", H5O_TYPE_GROUP},
    {"Group2/hard_zero/Dataset_zero", H5O_TYPE_DATASET},
    {"Group2/hard_zero/Type_zero", H5O_TYPE_NAMED_DATATYPE},
    {"Type_one", H5O_TYPE_NAMED_DATATYPE}
};
static const obj_visit_t ovisit2_old[] = {
    {".", H5O_TYPE_GROUP},
    {"Dataset_two", H5O_TYPE_DATASET},
    {"Type_two", H5O_TYPE_NAMED_DATATYPE},
    {"hard_zero", H5O_TYPE_GROUP},
    {"hard_zero/Dataset_zero", H5O_TYPE_DATASET},
    {"hard_zero/Group1", H5O_TYPE_GROUP},
    {"hard_zero/Group1/Dataset_one", H5O_TYPE_DATASET},
    {"hard_zero/Group1/Type_one", H5O_TYPE_NAMED_DATATYPE},
    {"hard_zero/Type_zero", H5O_TYPE_NAMED_DATATYPE}
};
static const obj_visit_t ovisit2_new[] = {
    {".", H5O_TYPE_GROUP},
    {"Dataset_two", H5O_TYPE_DATASET},
    {"Type_two", H5O_TYPE_NAMED_DATATYPE},
    {"hard_zero", H5O_TYPE_GROUP},
    {"hard_zero/Dataset_zero", H5O_TYPE_DATASET},
    {"hard_zero/Group1", H5O_TYPE_GROUP},
    {"hard_zero/Group1/Dataset_one", H5O_TYPE_DATASET},
    {"hard_zero/Group1/Type_one", H5O_TYPE_NAMED_DATATYPE},
    {"hard_zero/Type_zero", H5O_TYPE_NAMED_DATATYPE}
};

typedef struct {
    unsigned idx;               /* Index in object visit structure */
    const obj_visit_t *info;    /* Pointer to the object visit structure to use */
} ovisit_ud_t;



/*-------------------------------------------------------------------------
 * Function:    fix_ext_filename
 *
 * Purpose:     Internal function to append path to file name.  It handles
 *              path name of Unix, Windows, and OpenVMS.
 *
 * Return:      void
 *
 * Programmer:  Raymond Lu
 *              14 Jan. 2009
 *-------------------------------------------------------------------------
 */
static void
fix_ext_filename(char *path_name, char *cwd, const char *file_name)
{
    HDstrcpy(path_name, cwd);

#ifdef H5_VMS
    if(file_name[0] == '[') {
        char *tmp = file_name;
        path_name[strlen(cwd)-1] = '\0';
        HDstrcat(path_name, ++tmp);
    } else
        HDstrcat(path_name, file_name);
#else
    HDstrcat(path_name, "/");
    HDstrcat(path_name, file_name);
#endif
}


/*-------------------------------------------------------------------------
 * Function:	mklinks
 *
 * Purpose:	Build a file with assorted links.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Friday, August 14, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
mklinks(hid_t fapl, hbool_t new_format)
{
    hid_t		file, scalar, grp, d1;
    hsize_t	        size[1] = {1};
    char		filename[NAME_BUF_SIZE];

    if(new_format)
        TESTING("link creation (w/new group format)")
    else
        TESTING("link creation")

    /* Create a file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((scalar = H5Screate_simple(1, size, size)) < 0) TEST_ERROR

    /* Create a group */
    if((grp = H5Gcreate2(file, "grp1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(grp) < 0) TEST_ERROR

    /* Create a dataset */
    if((d1 = H5Dcreate2(file, "d1", H5T_NATIVE_INT, scalar, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Dclose(d1) < 0) TEST_ERROR

    /* Create a hard link */
    if(H5Lcreate_hard(file, "d1", H5L_SAME_LOC, "grp1/hard", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Create a symbolic link */
    if(H5Lcreate_soft("/d1", file, "grp1/soft", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Create a symbolic link to something that doesn't exist */
    if(H5Lcreate_soft("foobar", file, "grp1/dangle", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Create a recursive symbolic link */
    if(H5Lcreate_soft("/grp1/recursive", file, "/grp1/recursive", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close */
    if(H5Sclose(scalar) < 0) TEST_ERROR
    if(H5Fclose(file) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    new_links
 *
 * Purpose:     Build a file with assorted links for different locations.
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              Friday, April 19, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
new_links(hid_t fapl, hbool_t new_format)
{
    hid_t		file_a, file_b=(-1);
    hid_t		grp1_a=(-1), grp1_b=(-1), grp2_a=(-1), grp2_b=(-1);
    hid_t		scalar=(-1);
    hid_t		dset1=(-1), dset2=(-1);
    char		filename[NAME_BUF_SIZE];
    hsize_t             size[1] = {1};

    if(new_format)
        TESTING("H5Lcreate functions (w/new group format)")
    else
        TESTING("H5Lcreate functions")

    /* Create two files */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if((file_a = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    h5_fixname(FILENAME[2], fapl, filename, sizeof filename);
    if((file_b = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    if((scalar = H5Screate_simple (1, size, size)) < 0) TEST_ERROR

    /* Create two groups in each file */
    if((grp1_a = H5Gcreate2(file_a, "grp1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if((grp2_a = H5Gcreate2(file_a, "grp2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if((grp1_b = H5Gcreate2(file_b, "grp1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if((grp2_b = H5Gcreate2(file_b, "grp2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Create datasets */
    if((dset1 = H5Dcreate2(file_a, "dataset1", H5T_NATIVE_INT, scalar, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if((dset2 = H5Dcreate2(grp1_a, "dataset2", H5T_NATIVE_INT, scalar, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Create links within a file.  Both of source and destination use
     * H5L_SAME_LOC.  Both hard and soft links should fail. */
    H5E_BEGIN_TRY {
        if(H5Lcreate_hard(H5L_SAME_LOC, "dataset1", H5L_SAME_LOC, "hard", H5P_DEFAULT, H5P_DEFAULT)!=FAIL) TEST_ERROR
    } H5E_END_TRY;
    H5E_BEGIN_TRY {
        if(H5Lcreate_soft("dataset1", H5L_SAME_LOC, "soft", H5P_DEFAULT, H5P_DEFAULT)!=FAIL) TEST_ERROR
    } H5E_END_TRY;

    /* Create links across files with hard link.  Should fail. */
    H5E_BEGIN_TRY {
        if(H5Lcreate_hard(file_a, "dataset1", file_b, "hard", H5P_DEFAULT, H5P_DEFAULT)!=FAIL) TEST_ERROR
    } H5E_END_TRY;

    /* Create hard link to test H5L_SAME_LOC */
    if(H5Lcreate_hard(grp1_a, "dataset2", H5L_SAME_LOC, "hard1", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Create links to test hard links across different locations */
    if(H5Lcreate_hard(grp1_a, "dataset2", grp2_a, "hard2", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close dataspace and files */
    if(H5Sclose(scalar) < 0) TEST_ERROR
    if(H5Dclose(dset1) < 0) TEST_ERROR
    if(H5Dclose(dset2) < 0) TEST_ERROR
    if(H5Gclose(grp1_a) < 0) TEST_ERROR
    if(H5Gclose(grp2_a) < 0) TEST_ERROR
    if(H5Gclose(grp1_b) < 0) TEST_ERROR
    if(H5Gclose(grp2_b) < 0) TEST_ERROR
    if(H5Fclose(file_a) < 0) TEST_ERROR
    if(H5Fclose(file_b) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Sclose(scalar);
    	H5Dclose(dset1);
    	H5Dclose(dset2);
    	H5Gclose(grp1_a);
    	H5Gclose(grp2_a);
    	H5Gclose(grp1_b);
    	H5Gclose(grp2_b);
    	H5Fclose(file_a);
    	H5Fclose(file_b);
    } H5E_END_TRY;
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	cklinks
 *
 * Purpose:	Open the file created in the first step and check that the
 *		links look correct.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Friday, August 14, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
cklinks(hid_t fapl, hbool_t new_format)
{
    hid_t		file;
    H5O_info_t		oinfo1, oinfo2;
    H5L_info_t		linfo2;
    char		linkval[LINK_BUF_SIZE];
    char		filename[NAME_BUF_SIZE];
    herr_t		status;

    if(new_format)
        TESTING("link queries (w/new group format)")
    else
        TESTING("link queries")

    /* Open the file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0) FAIL_STACK_ERROR

    /* Hard link */
    if(H5Oget_info_by_name(file, "d1", &oinfo1, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Oget_info_by_name(file, "grp1/hard", &oinfo2, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5O_TYPE_DATASET != oinfo2.type) {
	H5_FAILED();
	printf("    %d: Unexpected object type should have been a dataset\n", __LINE__);
	TEST_ERROR
    } /* end if */
    if(H5F_addr_ne(oinfo1.addr, oinfo2.addr)) {
	H5_FAILED();
	puts("    Hard link test failed. Link seems not to point to the ");
	puts("    expected file location.");
	TEST_ERROR
    } /* end if */
    if(H5Lexists(file, "d1", H5P_DEFAULT) != TRUE) FAIL_STACK_ERROR
    if(H5Lexists(file, "grp1/hard", H5P_DEFAULT) != TRUE) FAIL_STACK_ERROR

    /* Symbolic link */
    if(H5Oget_info_by_name(file, "grp1/soft", &oinfo2, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5O_TYPE_DATASET != oinfo2.type) {
	H5_FAILED();
	printf("    %d: Unexpected object type should have been a dataset\n", __LINE__);
	TEST_ERROR
    } /* end if */
    if(H5F_addr_ne(oinfo1.addr, oinfo2.addr)) {
	H5_FAILED();
	puts("    Soft link test failed. Link seems not to point to the ");
	puts("    expected file location.");
	TEST_ERROR
    } /* end if */
    if(H5Lget_val(file, "grp1/soft", linkval, sizeof linkval, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(HDstrcmp(linkval, "/d1")) {
	H5_FAILED();
	puts("    Soft link test failed. Wrong link value");
	TEST_ERROR
    } /* end if */
    if(H5Lexists(file, "grp1/soft", H5P_DEFAULT) != TRUE) FAIL_STACK_ERROR

    /* Dangling link */
    H5E_BEGIN_TRY {
	status = H5Oget_info_by_name(file, "grp1/dangle", &oinfo2, H5P_DEFAULT);
    } H5E_END_TRY;
    if(status >= 0) {
	H5_FAILED();
	puts("    H5Oget_info_by_name() should have failed for a dangling link.");
	TEST_ERROR
    } /* end if */
    if(H5Lget_info(file, "grp1/dangle", &linfo2, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5L_TYPE_SOFT != linfo2.type) {
	H5_FAILED();
	printf("    %d: Unexpected object type should have been a symbolic link\n", __LINE__);
	TEST_ERROR
    } /* end if */
    if(H5Lget_val(file, "grp1/dangle", linkval, sizeof linkval, H5P_DEFAULT) < 0) {
	H5_FAILED();
	printf("    %d: Can't retrieve link value\n", __LINE__);
	TEST_ERROR
    } /* end if */
    if(HDstrcmp(linkval, "foobar")) {
	H5_FAILED();
	puts("    Dangling link test failed. Wrong link value");
	TEST_ERROR
    } /* end if */
    if(H5Lexists(file, "grp1/dangle", H5P_DEFAULT) != TRUE) FAIL_STACK_ERROR

    /* Recursive link */
    H5E_BEGIN_TRY {
	status = H5Oget_info_by_name(file, "grp1/recursive", &oinfo2, H5P_DEFAULT);
    } H5E_END_TRY;
    if(status >= 0) {
	H5_FAILED();
	puts("    H5Oget_info_by_name() should have failed for a recursive link.");
	TEST_ERROR
    } /* end if */
    if(H5Lget_info(file, "grp1/recursive", &linfo2, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5L_TYPE_SOFT != linfo2.type) {
	H5_FAILED();
	printf("    %d: Unexpected object type should have been a symbolic link\n", __LINE__);
	TEST_ERROR
    } /* end if */
    if(H5Lget_val(file, "grp1/recursive", linkval, sizeof linkval, H5P_DEFAULT) < 0) {
	H5_FAILED();
	printf("    %d: Can't retrieve link value\n", __LINE__);
	TEST_ERROR
    } /* end if */
    if(HDstrcmp(linkval, "/grp1/recursive")) {
	H5_FAILED();
	puts("   Recursive link test failed. Wrong link value");
	TEST_ERROR
    } /* end if */

    /* Non-existent link */
    if(H5Lexists(file, "foobar", H5P_DEFAULT) == TRUE) FAIL_STACK_ERROR

    /* Cleanup */
    if(H5Fclose(file) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    ck_new_links
 *
 * Purpose:     Open the file created in the first step and check that the
 *              links look correct.
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              Thursday, April 25, 2002
 *
 *-------------------------------------------------------------------------
 */
static int
ck_new_links(hid_t fapl, hbool_t new_format)
{
    hid_t 		file;
    H5O_info_t		oi_dset, oi_hard1, oi_hard2;
    char 		filename[NAME_BUF_SIZE];

    if(new_format)
        TESTING("new link queries (w/new group format)")
    else
        TESTING("new link queries")

    /* Open the file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* Get hard link info */
    if(H5Oget_info_by_name(file, "/grp1/dataset2", &oi_dset, H5P_DEFAULT) < 0)
	TEST_ERROR
    if(H5Oget_info_by_name(file, "/grp1/hard1", &oi_hard1, H5P_DEFAULT) < 0)
	TEST_ERROR
    if(H5Oget_info_by_name(file, "/grp2/hard2", &oi_hard2, H5P_DEFAULT) < 0)
	TEST_ERROR

    /* Check hard links */
    if(H5O_TYPE_DATASET != oi_hard1.type || H5O_TYPE_DATASET != oi_hard2.type) {
	H5_FAILED();
	printf("    %d: Unexpected object type should have been a dataset\n", __LINE__);
	TEST_ERROR
    }
    if(H5F_addr_ne(oi_dset.addr, oi_hard1.addr) || H5F_addr_ne(oi_dset.addr, oi_hard2.addr)) {
	H5_FAILED();
	puts("    Hard link test failed.  Link seems not to point to the ");
	puts("    expected file location.");
	TEST_ERROR
    }

    /* Cleanup */
    if(H5Fclose(file) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    long_links
 *
 * Purpose:     Build a file with long names
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Saturday, April 16, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
long_links(hid_t fapl, hbool_t new_format)
{
    hid_t		fid = (-1);     /* File ID */
    hid_t		gid = (-1);     /* Group ID */
    hid_t		gid2 = (-1);    /* Datatype ID */
    char               *objname = NULL; /* Name of object [Long] */
    size_t              u;              /* Local index variable */
    char		filename[NAME_BUF_SIZE];

    if(new_format)
        TESTING("long names for objects & links (w/new group format)")
    else
        TESTING("long names for objects & links")

    /* Create files */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create group with short name in file (used as target for hard links) */
    if((gid = H5Gcreate2(fid, "grp1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Construct very long file name */
    if((objname = (char *)HDmalloc((size_t)(MAX_NAME_LEN + 1))) == NULL) TEST_ERROR
    for(u = 0; u < MAX_NAME_LEN; u++)
        objname[u] = 'a';
    objname[MAX_NAME_LEN] = '\0';

    /* Create hard link to existing object */
    if(H5Lcreate_hard(fid, "grp1", fid, objname, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Create soft link to existing object */
    objname[0] = 'b';
    if(H5Lcreate_soft("grp1", fid, objname, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Create group with long name in existing group */
    if((gid2 = H5Gcreate2(gid, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close objects */
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Release memory */
    HDfree(objname);

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Gclose (gid2);
    	H5Gclose (gid);
    	H5Fclose (fid);
    } H5E_END_TRY;
    HDfree(objname);
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    toomany
 *
 * Purpose:     Build a file with too many symbolic links
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, August 9, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
toomany(hid_t fapl, hbool_t new_format)
{
    hid_t		fid = (-1);     /* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    char                objname[NAME_BUF_SIZE];         /* Object name */
    char		filename[NAME_BUF_SIZE];

    if(new_format)
        TESTING("too many links (w/new group format)")
    else
        TESTING("too many links")

    /* Make certain test is valid */
    /* XXX: should probably make a "generic" test that creates the proper
     *          # of links based on this value - QAK
     */
    HDassert(H5L_NUM_LINKS == 16);

    /* Create file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create group with short name in file (used as target for hard links) */
    if((gid = H5Gcreate2(fid, "final", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Create chain of hard links to existing object (no limit on #) */
    if(H5Lcreate_hard(fid, "final", fid, "hard1", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard1", fid, "hard2", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard2", fid, "hard3", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard3", fid, "hard4", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard4", fid, "hard5", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard5", fid, "hard6", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard6", fid, "hard7", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard7", fid, "hard8", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard8", fid, "hard9", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard9", fid, "hard10", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard10", fid, "hard11", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard11", fid, "hard12", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard12", fid, "hard13", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard13", fid, "hard14", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard14", fid, "hard15", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard15", fid, "hard16", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard16", fid, "hard17", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard17", fid, "hard18", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard18", fid, "hard19", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard19", fid, "hard20", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard20", fid, "hard21", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Create chain of soft links to existing object (limited) */
    if(H5Lcreate_soft("final", fid, "soft1", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("soft1", fid, "soft2", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("soft2", fid, "soft3", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("soft3", fid, "soft4", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("soft4", fid, "soft5", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("soft5", fid, "soft6", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("soft6", fid, "soft7", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("soft7", fid, "soft8", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("soft8", fid, "soft9", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("soft9", fid, "soft10", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("soft10", fid, "soft11", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("soft11", fid, "soft12", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("soft12", fid, "soft13", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("soft13", fid, "soft14", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("soft14", fid, "soft15", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("soft15", fid, "soft16", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("soft16", fid, "soft17", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close objects */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Open file */
    if((fid=H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR

    /* Open object through last hard link */
    if((gid = H5Gopen2(fid, "hard21", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if(H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/hard21")) TEST_ERROR

    /* Create object in hard-linked group */
    if((gid2 = H5Gcreate2(gid, "new_hard", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close group in hard-linked group */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close hard-linked object */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Open object through too deep soft link */
    H5E_BEGIN_TRY {
        gid = H5Gopen2(fid, "soft17", H5P_DEFAULT);
    } H5E_END_TRY;
    if(gid >= 0) {
	H5_FAILED();
	puts("    Should have failed for sequence of too many nested links.");
	TEST_ERROR
    } /* end if */

    /* Open object through lesser soft link */
    if((gid = H5Gopen2(fid, "soft16", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if(H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/soft16")) TEST_ERROR

    /* Create object using soft links */
    if((gid2 = H5Gcreate2(gid, "new_soft", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close groups */
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end toomany() */


/*-------------------------------------------------------------------------
 * Function:    test_lcpl
 *
 * Purpose:     Tests Link Creation Property Lists
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  James Laird
 *              Monday, January 30, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_lcpl(hid_t fapl, hbool_t new_format)
{
    hid_t file_id=-1;
    hid_t group_id=-1;
    hid_t space_id=-1;
    hid_t dset_id=-1;
    hid_t type_id=-1;
    hid_t lcpl_id=-1;
    H5L_info_t linfo;
    char filename[1024];
    hsize_t dims[2];

    if(new_format)
        TESTING("link creation property lists (w/new group format)")
    else
        TESTING("link creation property lists")

    /* Actually, intermediate group creation is tested elsewhere (tmisc).
     * Here we only need to test the character encoding property */

    /* Create file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create and link a group with the default LCPL */
    if((group_id = H5Gcreate2(file_id, "/group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(group_id) < 0) TEST_ERROR

    /* Check that its character encoding is the default */
    if(H5Lget_info(file_id, "group", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.cset != H5F_DEFAULT_CSET) TEST_ERROR

    /* Create and commit a datatype with the default LCPL */
    if((type_id = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR
    if(H5Tcommit2(file_id, "/type", type_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Tclose(type_id) < 0) TEST_ERROR

    /* Check that its character encoding is the default */
    if(H5Lget_info(file_id, "type", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.cset != H5F_DEFAULT_CSET) TEST_ERROR

    /* Create a dataspace */
    dims[0] = H5L_DIM1;
    dims[1] = H5L_DIM2;
    if((space_id=H5Screate_simple(2 ,dims, NULL)) < 0) TEST_ERROR

    /* Create a dataset using the default LCPL */
    if((dset_id = H5Dcreate2(file_id, "/dataset", H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Dclose(dset_id) < 0) TEST_ERROR

    /* Check that its character encoding is the default */
    if(H5Lget_info(file_id, "dataset", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.cset != H5F_DEFAULT_CSET) TEST_ERROR

    /* Create a link creation property list with the UTF-8 character encoding */
    if((lcpl_id = H5Pcreate(H5P_LINK_CREATE)) < 0) TEST_ERROR
    if(H5Pset_char_encoding(lcpl_id, H5T_CSET_UTF8) < 0) TEST_ERROR

    /* Create and link a group with the new LCPL */
    if((group_id = H5Gcreate2(file_id, "/group2", lcpl_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(group_id) < 0) TEST_ERROR

    /* Check that its character encoding is UTF-8 */
    if(H5Lget_info(file_id, "group2", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR

    /* Create and commit a datatype with the new LCPL */
    if((type_id = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR
    if(H5Tcommit2(file_id, "/type2", type_id, lcpl_id, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Tclose(type_id) < 0) TEST_ERROR

    /* Check that its character encoding is UTF-8 */
    if(H5Lget_info(file_id, "type2", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR

    /* Create a dataset using the new LCPL */
    if((dset_id = H5Dcreate2(file_id, "/dataset2", H5T_NATIVE_INT, space_id, lcpl_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Dclose(dset_id) < 0) TEST_ERROR

    /* Check that its character encoding is UTF-8 */
    if(H5Lget_info(file_id, "dataset2", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR

    /* Create a new link to the dataset with a different character encoding. */
    if(H5Pclose(lcpl_id) < 0) TEST_ERROR
    if((lcpl_id = H5Pcreate(H5P_LINK_CREATE)) < 0) TEST_ERROR
    if(H5Pset_char_encoding(lcpl_id, H5T_CSET_ASCII) < 0) TEST_ERROR

    if(H5Lcreate_hard(file_id, "/dataset2", file_id, "/dataset2_link", lcpl_id, H5P_DEFAULT) < 0) TEST_ERROR

    /* Check that its character encoding is ASCII */
    if(H5Lget_info(file_id, "/dataset2_link", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.cset != H5T_CSET_ASCII) TEST_ERROR

    /* Check that the first link's encoding hasn't changed */
    if(H5Lget_info(file_id, "/dataset2", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR

    /* Make sure that LCPLs work properly for other API calls: */
    /* H5Lcreate_soft */
    if(H5Pset_char_encoding(lcpl_id, H5T_CSET_UTF8) < 0) TEST_ERROR
    if(H5Lcreate_soft("dataset2", file_id, "slink_to_dset2", lcpl_id, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lget_info(file_id, "slink_to_dset2", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR

    /* H5Lmove */
    if(H5Pset_char_encoding(lcpl_id, H5T_CSET_ASCII) < 0) TEST_ERROR
    if(H5Lmove(file_id, "slink_to_dset2", file_id, "moved_slink", lcpl_id, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lget_info(file_id, "moved_slink", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.cset != H5T_CSET_ASCII) TEST_ERROR

    /* H5Lcopy */
    if(H5Pset_char_encoding(lcpl_id, H5T_CSET_UTF8) < 0) TEST_ERROR
    if(H5Lcopy(file_id, "moved_slink", file_id, "copied_slink", lcpl_id, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lget_info(file_id, "copied_slink", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR

    /* H5Lcreate_external */
    if(H5Lcreate_external("filename", "path", file_id, "extlink", lcpl_id, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lget_info(file_id, "extlink", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR

    /* Close open IDs */
    if(H5Pclose(lcpl_id) < 0) TEST_ERROR
    if(H5Sclose(space_id) < 0) TEST_ERROR
    if(H5Fclose(file_id) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Gclose(group_id);
        H5Dclose(dset_id);
        H5Tclose(type_id);
        H5Pclose(lcpl_id);
        H5Sclose(space_id);
        H5Fclose(file_id);
    } H5E_END_TRY;
    return 1;
} /* end test_lcpl() */


/*-------------------------------------------------------------------------
 * Function:    test_move
 *
 * Purpose:     Tests H5Lmove()
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 * Programmer:  James Laird
 *              Friday, March 30, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_move(hid_t fapl, hbool_t new_format)
{
    hid_t 	file_a, file_b=(-1);
    hid_t	grp_1=(-1), grp_2=(-1), grp_move=(-1), moved_grp=(-1);
    char 	filename[1024];

    if(new_format)
        TESTING("H5Lmove (w/new group format)")
    else
        TESTING("H5Lmove")

    /* Create two new files */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file_a=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if ((file_b=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Create groups in first file */
    if((grp_1 = H5Gcreate2(file_a, "group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if((grp_2 = H5Gcreate2(file_a, "group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if((grp_move = H5Gcreate2(grp_1, "group_move", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Create hard, soft and external links. */
    if(H5Lcreate_hard(grp_1, "group_move", H5L_SAME_LOC, "hard", H5P_DEFAULT, H5P_DEFAULT) < 0)
	TEST_ERROR
    if(H5Lcreate_soft("/group1/group_move", grp_2, "soft", H5P_DEFAULT, H5P_DEFAULT) < 0)
	TEST_ERROR
    if(H5Lcreate_external("filename", "pathname", grp_2, "ext", H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Move a group within the file.  Both of source and destination use
     * H5L_SAME_LOC.  Should fail. */
    H5E_BEGIN_TRY {
        if(H5Lmove(H5L_SAME_LOC, "group_move", H5L_SAME_LOC, "group_new_name", H5P_DEFAULT, H5P_DEFAULT)
                !=FAIL) TEST_ERROR
    } H5E_END_TRY;

    /* Move a group across files.  Should fail. */
    H5E_BEGIN_TRY {
        if(H5Lmove(grp_1, "group_move", file_b, "group_new_name", H5P_DEFAULT, H5P_DEFAULT)
                !=FAIL) TEST_ERROR
    } H5E_END_TRY;

    /* Move a soft link across files.  Should succeed. */
    if(H5Lmove(grp_2, "soft", file_b, "soft_new_name", H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR
    if(H5Lexists(file_b, "soft_new_name", H5P_DEFAULT) != TRUE)
        TEST_ERROR

    /* Move an external link across files.  Should succeed. */
    if(H5Lmove(grp_2, "ext", file_b, "ext_new_name", H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR
    if(H5Lexists(file_b, "ext_new_name", H5P_DEFAULT) != TRUE)
        TEST_ERROR

    /* Move a group across groups in the same file while renaming it. */
    if(H5Lmove(grp_1, "group_move", grp_2, "group_new_name", H5P_DEFAULT, H5P_DEFAULT) < 0)
	TEST_ERROR

    /* Open the group just moved to the new location. */
    if((moved_grp = H5Gopen2(grp_2, "group_new_name", H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR
    if( H5Gclose(moved_grp) < 0)
        TEST_ERROR

    /* Verify that the group is no longer in the original location */
    H5E_BEGIN_TRY {
        moved_grp = H5Gopen2(grp_1, "group_move", H5P_DEFAULT);
    } H5E_END_TRY;
    if(moved_grp >= 0) {
	H5_FAILED();
	puts("    Group still in original location?");
	TEST_ERROR
    } /* end if */

    /* Use H5Lmove to rename a group without moving it. */
    if(H5Lmove(grp_2, "group_new_name", H5L_SAME_LOC, "group_newer_name", H5P_DEFAULT, H5P_DEFAULT) < 0)
	TEST_ERROR

    /* Open the group. */
    if((moved_grp = H5Gopen2(grp_2, "group_newer_name", H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR
    if(H5Gclose(moved_grp) < 0)
        TEST_ERROR

    /* Use H5Lmove to move a group without renaming it. */
    if(H5Lmove(grp_2, "group_newer_name", grp_1, "group_newer_name", H5P_DEFAULT, H5P_DEFAULT) < 0)
	TEST_ERROR

    /* Open the group . */
    if((moved_grp = H5Gopen2(grp_1, "group_newer_name", H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR
    if(H5Gclose(moved_grp) < 0)
        TEST_ERROR

    /* Move the group while giving long paths. */
    if(H5Lmove(file_a, "/group1/group_newer_name", grp_2, "/group2/group_newest_name", H5P_DEFAULT, H5P_DEFAULT) < 0)
	TEST_ERROR

    /* Open the group just moved to the new location. */
    if((moved_grp = H5Gopen2(grp_2, "group_newest_name", H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR

    if(H5Gclose(moved_grp) < 0)
        TEST_ERROR

    /* Verify that the group is in no previous locations */
    H5E_BEGIN_TRY {
        if((moved_grp = H5Gopen2(grp_1, "group_newer_name", H5P_DEFAULT)) >= 0)
            FAIL_STACK_ERROR
        if((moved_grp = H5Gopen2(grp_2, "group_newer_name", H5P_DEFAULT)) >= 0)
            FAIL_STACK_ERROR
        if((moved_grp = H5Gopen2(grp_2, "group_new_name", H5P_DEFAULT)) >= 0)
            FAIL_STACK_ERROR
        if((moved_grp = H5Gopen2(grp_1, "group_copy", H5P_DEFAULT)) >= 0)
            FAIL_STACK_ERROR
    } H5E_END_TRY;

    H5Gclose(grp_1);
    H5Gclose(grp_2);
    H5Gclose(grp_move);
    H5Fclose(file_a);
    H5Fclose(file_b);

    PASSED();
    return 0;

  error:
    H5_FAILED();
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
 * Function:    test_copy
 *
 * Purpose:     Tests H5Lcopy()
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 * Programmer:  James Laird
 *              Friday, March 30, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy(hid_t fapl, hbool_t new_format)
{
    hid_t 	file_a, file_b=(-1);
    hid_t	grp_1=(-1), grp_2=(-1), grp_move=(-1), moved_grp=(-1);
    char 	filename[1024];

    if(new_format)
        TESTING("H5Lcopy (w/new group format)")
    else
        TESTING("H5Lcopy")

    /* Create two new files */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file_a=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if ((file_b=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Create groups in first file */
    if((grp_1 = H5Gcreate2(file_a, "group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if((grp_2 = H5Gcreate2(file_a, "group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if((grp_move = H5Gcreate2(grp_1, "group_copy", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Create hard, soft and external links. */
    if(H5Lcreate_hard(grp_1, "group_copy", H5L_SAME_LOC, "hard", H5P_DEFAULT, H5P_DEFAULT) < 0)
	TEST_ERROR
    if(H5Lcreate_soft("/group1/group_copy", grp_2, "soft", H5P_DEFAULT, H5P_DEFAULT) < 0)
	TEST_ERROR
    if(H5Lcreate_external("filename", "pathname", grp_2, "ext", H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Copy a group within the file.  Both of source and destination use
     * H5L_SAME_LOC.  Should fail. */
    H5E_BEGIN_TRY {
        if(H5Lcopy(H5L_SAME_LOC, "group_copy", H5L_SAME_LOC, "group_new_name", H5P_DEFAULT, H5P_DEFAULT)
                !=FAIL) TEST_ERROR
    } H5E_END_TRY;

    /* Copy a group across files.  Should fail. */
    H5E_BEGIN_TRY {
        if(H5Lcopy(grp_1, "group_copy", file_b, "group_new_name", H5P_DEFAULT, H5P_DEFAULT)
                !=FAIL) TEST_ERROR
    } H5E_END_TRY;

    /* Copy a soft link across files.  Should succeed. */
    if(H5Lcopy(grp_2, "soft", file_b, "soft_new_name", H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR
    if(H5Lexists(file_b, "soft_new_name", H5P_DEFAULT) != TRUE)
        TEST_ERROR

    /* Copy an external link across files.  Should succeed. */
    if(H5Lcopy(grp_2, "ext", file_b, "ext_new_name", H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR
    if(H5Lexists(file_b, "ext_new_name", H5P_DEFAULT) != TRUE)
        TEST_ERROR

    /* Move a group across groups in the same file while renaming it. */
    if(H5Lcopy(grp_1, "group_copy", grp_2, "group_new_name", H5P_DEFAULT, H5P_DEFAULT) < 0)
	TEST_ERROR

    /* Open the group just moved to the new location. */
    if((moved_grp = H5Gopen2(grp_2, "group_new_name", H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR
    if(H5Gclose(moved_grp) < 0)
        TEST_ERROR

    /* Verify that the group is also in the original location */
    if((moved_grp = H5Gopen2(grp_1, "group_copy", H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR
    if(H5Gclose(moved_grp) < 0)
        TEST_ERROR

    /* Use H5Lcopy to create a group in the same location with a different name. */
    if(H5Lcopy(grp_2, "group_new_name", H5L_SAME_LOC, "group_newer_name", H5P_DEFAULT, H5P_DEFAULT) < 0)
	TEST_ERROR

    /* Open the group. */
    if((moved_grp = H5Gopen2(grp_2, "group_newer_name", H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR
    if(H5Gclose(moved_grp) < 0)
        TEST_ERROR
    /* Verify that the group is also in the original location */
    if((moved_grp = H5Gopen2(grp_2, "group_new_name", H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR
    if(H5Gclose(moved_grp) < 0)
        TEST_ERROR

    /* Use H5Lcopy to copy to a different location with the same name. */
    if(H5Lcopy(grp_2, "group_newer_name", grp_1, "group_newer_name", H5P_DEFAULT, H5P_DEFAULT) < 0)
	TEST_ERROR

    /* Open the group . */
    if((moved_grp = H5Gopen2(grp_1, "group_newer_name", H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR
    if(H5Gclose(moved_grp) < 0)
        TEST_ERROR
    /* Verify that the group is still in the previous location */
    if((moved_grp = H5Gopen2(grp_2, "group_new_name", H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR
    if(H5Gclose(moved_grp) < 0)
        TEST_ERROR

    /* Copy the group while giving long paths. */
    if(H5Lcopy(file_a, "/group1/group_newer_name", grp_2, "/group2/group_newest_name", H5P_DEFAULT, H5P_DEFAULT) < 0)
	TEST_ERROR

    /* Open the group just moved to the new location. */
    if((moved_grp = H5Gopen2(grp_2, "group_newest_name", H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR
    if(H5Gclose(moved_grp) < 0)
        TEST_ERROR

    /* Verify that the group is still in all previous original locations */
    if((moved_grp = H5Gopen2(grp_1, "group_newer_name", H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR
    if(H5Gclose(moved_grp) < 0)
        TEST_ERROR
    if((moved_grp = H5Gopen2(grp_2, "group_newer_name", H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR
    if(H5Gclose(moved_grp) < 0)
        TEST_ERROR
    if((moved_grp = H5Gopen2(grp_2, "group_new_name", H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR
    if(H5Gclose(moved_grp) < 0)
        TEST_ERROR
    if((moved_grp = H5Gopen2(grp_1, "group_copy", H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR
    if(H5Gclose(moved_grp) < 0)
        TEST_ERROR

    H5Gclose(grp_1);
    H5Gclose(grp_2);
    H5Gclose(grp_move);
    H5Fclose(file_a);
    H5Fclose(file_b);

    PASSED();
    return 0;

  error:
    H5_FAILED();
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
 * Function:    test_move_preserves
 *
 * Purpose:     Tests that moving and renaming links preserves their
 *              properties.
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  James Laird
 *              Monday, January 30, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_move_preserves(hid_t fapl_id, hbool_t new_format)
{
    hid_t file_id=-1;
    hid_t group_id=-1;
    hid_t fcpl_id=-1;           /* Group creation property list ID */
    hid_t lcpl_id=-1;
    hid_t lcpl2_id=-1;
    H5O_info_t oinfo;
    H5L_info_t linfo;
    H5T_cset_t old_cset;
    int64_t old_corder;         /* Creation order value of link */
    time_t old_modification_time;
    time_t curr_time;
    unsigned crt_order_flags;   /* Status of creation order info for GCPL */
    char filename[1024];

    if(new_format)
        TESTING("moving and copying links preserves their properties (w/new group format)")
    else
        TESTING("moving and copying links preserves their properties")

    /* Create a file creation property list with creation order stored for links
     * in the root group
     */
    if((fcpl_id = H5Pcreate(H5P_FILE_CREATE)) < 0) TEST_ERROR
    if(H5Pget_link_creation_order(fcpl_id, &crt_order_flags) < 0) TEST_ERROR
    if(crt_order_flags != 0) TEST_ERROR
    if(H5Pset_link_creation_order(fcpl_id, H5P_CRT_ORDER_TRACKED) < 0) TEST_ERROR
    if(H5Pget_link_creation_order(fcpl_id, &crt_order_flags) < 0) TEST_ERROR
    if(crt_order_flags != H5P_CRT_ORDER_TRACKED) TEST_ERROR

    /* Create file */
    /* (with creation order tracking for the root group) */
    h5_fixname(FILENAME[0], fapl_id, filename, sizeof filename);
    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl_id, fapl_id)) < 0) TEST_ERROR

    /* Create a link creation property list with the UTF-8 character encoding */
    if((lcpl_id = H5Pcreate(H5P_LINK_CREATE)) < 0) TEST_ERROR
    if(H5Pset_char_encoding(lcpl_id, H5T_CSET_UTF8) < 0) TEST_ERROR

    /* Create a group with that lcpl */
    if((group_id = H5Gcreate2(file_id, "group", lcpl_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(group_id) < 0) TEST_ERROR

    /* Get the group's link's information */
    if(H5Lget_info(file_id, "group", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Oget_info_by_name(file_id, "group", &oinfo, H5P_DEFAULT) < 0) TEST_ERROR
    old_cset = linfo.cset;
    if(old_cset != H5T_CSET_UTF8) TEST_ERROR
    if(linfo.corder_valid != TRUE) TEST_ERROR
    old_corder = linfo.corder;
    if(old_corder != 0) TEST_ERROR
    old_modification_time = oinfo.mtime;

    /* If this test happens too quickly, the times will all be the same.  Make sure the time changes. */
    curr_time = HDtime(NULL);
    while(HDtime(NULL) <= curr_time)
        ;

    /* Close the file and reopen it */
    if(H5Fclose(file_id) < 0) TEST_ERROR
    if((file_id = H5Fopen(filename, H5F_ACC_RDWR, fapl_id)) < 0) TEST_ERROR

    /* Get the link's character set & modification time .  They should be unchanged */
    if(H5Lget_info(file_id, "group", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Oget_info_by_name(file_id, "group", &oinfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(old_modification_time != oinfo.mtime) TEST_ERROR
    if(old_cset != linfo.cset) TEST_ERROR
    if(linfo.corder_valid != TRUE) TEST_ERROR
    if(old_corder != linfo.corder) TEST_ERROR

    /* Create a new link to the group.  It should have a different creation order value but the same modification time */
    if(H5Lcreate_hard(file_id, "group", file_id, "group2", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Oget_info_by_name(file_id, "group2", &oinfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(old_modification_time != oinfo.mtime) TEST_ERROR
    if(H5Lget_info(file_id, "group2", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(old_corder == linfo.corder) TEST_ERROR
    if(linfo.corder_valid != TRUE) TEST_ERROR
    if(linfo.corder != 1) TEST_ERROR
    if(linfo.cset != H5T_CSET_ASCII) TEST_ERROR

    /* Copy the first link to a UTF-8 name.
     *  Its creation order value should be different, but modification time
     * should not change.
     */
    if(H5Lcopy(file_id, "group", file_id, "group_copied", lcpl_id, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Oget_info_by_name(file_id, "group_copied", &oinfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(old_modification_time != oinfo.mtime) TEST_ERROR
    if(H5Lget_info(file_id, "group_copied", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.corder_valid != TRUE) TEST_ERROR
    if(linfo.corder != 2) TEST_ERROR

    /* Check that its character encoding is UTF-8 */
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR

    /* Move the link with the default property list. */
    if(H5Lmove(file_id, "group_copied", file_id, "group_copied2", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Oget_info_by_name(file_id, "group_copied2", &oinfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(old_modification_time != oinfo.mtime) TEST_ERROR
    if(H5Lget_info(file_id, "group_copied2", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.corder_valid != TRUE) TEST_ERROR
    if(linfo.corder != 3) TEST_ERROR

    /* Check that its character encoding is not UTF-8 */
    if(linfo.cset == H5T_CSET_UTF8) TEST_ERROR

    /* Check that the original link is unchanged */
    if(H5Oget_info_by_name(file_id, "group", &oinfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(old_modification_time != oinfo.mtime) TEST_ERROR
    if(H5Lget_info(file_id, "group", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.corder_valid != TRUE) TEST_ERROR
    if(old_corder != linfo.corder) TEST_ERROR
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR

    /* Move the first link to a UTF-8 name.
     *  Its creation order value will change, but modification time should not
     *  change. */
    if(H5Lmove(file_id, "group", file_id, "group_moved", lcpl_id, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Oget_info_by_name(file_id, "group_moved", &oinfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(old_modification_time != oinfo.mtime) TEST_ERROR
    if(H5Lget_info(file_id, "group_moved", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.corder_valid != TRUE) TEST_ERROR
    if(linfo.corder != 4) TEST_ERROR

    /* Check that its character encoding is UTF-8 */
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR

    /* Move the link again using the default property list. */
    if(H5Lmove(file_id, "group_moved", file_id, "group_moved_again", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Oget_info_by_name(file_id, "group_moved_again", &oinfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(old_modification_time != oinfo.mtime) TEST_ERROR
    if(H5Lget_info(file_id, "group_moved_again", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.corder_valid != TRUE) TEST_ERROR
    if(linfo.corder != 5) TEST_ERROR

    /* Check that its character encoding is not UTF-8 */
    if(linfo.cset == H5T_CSET_UTF8) TEST_ERROR

    /* Close open IDs */
    if(H5Pclose(fcpl_id) < 0) TEST_ERROR
    if(H5Pclose(lcpl_id) < 0) TEST_ERROR
    if(H5Fclose(file_id) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Pclose(fcpl_id);
	H5Pclose(lcpl_id);
	H5Pclose(lcpl2_id);
        H5Gclose(group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;
    return 1;
} /* end test_move_preserves() */


/*-------------------------------------------------------------------------
 * Function:    test_deprec
 *
 * Purpose:     Tests deprecated functions for backward compatibility.
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  James Laird
 *              Wednesday, April 26 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
#ifndef H5_NO_DEPRECATED_SYMBOLS
static int
test_deprec(hid_t fapl, hbool_t new_format)
{
    hid_t file_id = -1;
    hid_t group1_id = -1;
    hid_t group2_id = -1;
    H5G_stat_t	sb_hard1, sb_hard2, sb_soft1, sb_soft2;
    H5G_obj_t obj_type;         /* Object type */
    hsize_t num_objs;           /* Number of objects in a group */
    char filename[1024];
    char tmpstr[1024];

    if(new_format)
        TESTING("backwards compatibility (w/new group format)")
    else
        TESTING("backwards compatibility")

    /* Create file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create two groups in the file */
    if((group1_id = H5Gcreate2(file_id, "group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file_id, "group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Test H5Gset and get comment */
    if(H5Gset_comment(file_id, "group1", "comment") < 0) FAIL_STACK_ERROR
    if(H5Gget_comment(file_id, "group1", sizeof(tmpstr), tmpstr) < 0) FAIL_STACK_ERROR
    if(HDstrcmp(tmpstr, "comment")) TEST_ERROR

    /* Create links using H5Glink and H5Glink2 */
    if(H5Glink(file_id, H5G_LINK_HARD, "group2", "group1/link_to_group2") < 0) FAIL_STACK_ERROR
    if(H5Glink2(file_id, "group1", H5G_LINK_HARD, group2_id, "link_to_group1") < 0) FAIL_STACK_ERROR
    if(H5Glink2(file_id, "link_to_group1", H5G_LINK_SOFT, H5G_SAME_LOC, "group2/soft_link_to_group1") < 0) FAIL_STACK_ERROR
    if(H5Glink2(file_id, "dangle", H5G_LINK_SOFT, H5G_SAME_LOC, "group2/dangle_soft_link") < 0) FAIL_STACK_ERROR

    /* Test getting the names for objects */
    if(H5Gget_objname_by_idx(group1_id, (hsize_t)0, tmpstr, sizeof(tmpstr)) < 0) FAIL_STACK_ERROR
    if(HDstrcmp(tmpstr, "link_to_group2")) TEST_ERROR
    H5E_BEGIN_TRY {
        if(H5Gget_objname_by_idx(group1_id, (hsize_t)1, tmpstr, sizeof(tmpstr)) >= 0) TEST_ERROR
    } H5E_END_TRY;

    /* Test getting the type for objects */
    if((obj_type = H5Gget_objtype_by_idx(group1_id, (hsize_t)0)) < 0) FAIL_STACK_ERROR
    if(obj_type != H5G_GROUP) TEST_ERROR
    H5E_BEGIN_TRY {
        if(H5Gget_objtype_by_idx(group1_id, (hsize_t)1) >= 0) TEST_ERROR
    } H5E_END_TRY;

    /* Test getting the number of objects in a group */
    if(H5Gget_num_objs(file_id, &num_objs) < 0) FAIL_STACK_ERROR
    if(num_objs != 2) TEST_ERROR
    if(H5Gget_num_objs(group1_id, &num_objs) < 0) FAIL_STACK_ERROR
    if(num_objs != 1) TEST_ERROR

    /* Test that H5Glink created hard links properly */
    if(H5Gget_objinfo(file_id, "/group2", TRUE, &sb_hard1) < 0) FAIL_STACK_ERROR
    if(H5Gget_objinfo(file_id, "/group1/link_to_group2", TRUE, &sb_hard2) < 0) FAIL_STACK_ERROR

    if(HDmemcmp(&sb_hard1.objno, sb_hard2.objno, sizeof(sb_hard1.objno))) {
        H5_FAILED();
        puts("    Hard link test failed.  Link seems not to point to the ");
        puts("    expected file location.");
        TEST_ERROR
    } /* end if */

    /* Test for the other hard link created */
    if(H5Gget_objinfo(file_id, "/group1", TRUE, &sb_hard1) < 0) FAIL_STACK_ERROR
    if(H5Gget_objinfo(file_id, "/group2/link_to_group1", TRUE, &sb_hard2) < 0) FAIL_STACK_ERROR

    if(HDmemcmp(&sb_hard1.objno, sb_hard2.objno, sizeof(sb_hard1.objno))) {
        H5_FAILED();
        puts("    Hard link test failed.  Link seems not to point to the ");
        puts("    expected file location.");
        TEST_ERROR
    } /* end if */

    /* Test the soft link */
    if(H5Gget_objinfo(file_id, "/group2/soft_link_to_group1", FALSE, &sb_soft1) < 0) FAIL_STACK_ERROR
    if(sb_soft1.type != H5G_LINK) TEST_ERROR
    if(sb_soft1.linklen != HDstrlen("link_to_group1") + 1) TEST_ERROR

    if(H5Gget_linkval(group2_id, "soft_link_to_group1", sb_soft1.linklen, tmpstr) < 0) FAIL_STACK_ERROR
    if(HDstrcmp("link_to_group1", tmpstr)) TEST_ERROR


    /* Test the dangling soft link */
    if(H5Gget_objinfo(file_id, "/group2/dangle_soft_link", FALSE, &sb_soft2) < 0) FAIL_STACK_ERROR
    if(sb_soft2.type != H5G_LINK) TEST_ERROR
    if(sb_soft2.linklen != HDstrlen("dangle") + 1) TEST_ERROR

    if(H5Gget_linkval(group2_id, "dangle_soft_link", sb_soft2.linklen, tmpstr) < 0) FAIL_STACK_ERROR
    if(HDstrcmp("dangle", tmpstr)) TEST_ERROR


    /* Test H5Gmove and H5Gmove2 */
    if(H5Gmove(file_id, "group1", "moved_group1") < 0) FAIL_STACK_ERROR
    if(H5Gmove2(file_id, "group2", group1_id, "moved_group2") < 0) FAIL_STACK_ERROR

    /* Ensure that both groups can be opened */
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group1_id) < 0) FAIL_STACK_ERROR

    if((group1_id = H5Gopen2(file_id, "moved_group1", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gopen2(file_id, "moved_group1/moved_group2", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close open IDs */
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group1_id) < 0) FAIL_STACK_ERROR

    /* Test H5Gunlink */
    if(H5Gunlink(file_id, "moved_group1/moved_group2") < 0) FAIL_STACK_ERROR

    H5E_BEGIN_TRY {
        if(H5Gopen2(file_id, "moved_group1/moved_group2", H5P_DEFAULT) >=0) TEST_ERROR
    } H5E_END_TRY;

    if(H5Fclose(file_id) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Gclose(group2_id);
        H5Gclose(group1_id);
        H5Fclose(file_id);
    } H5E_END_TRY;
    return 1;
} /* end test_deprec() */
#endif /* H5_NO_DEPRECATED_SYMBOLS */


/*-------------------------------------------------------------------------
 * Function:    external_link_root
 *
 * Purpose:     Build a file with external link to root group in external file
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, May 25, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_root(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    H5L_info_t	linfo;                          /* Link information */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    char	filename1[NAME_BUF_SIZE];
    char	filename2[NAME_BUF_SIZE];
    const char  *file;				/* File from external link */
    const char  *path;				/* Path from external link */

    if(new_format)
        TESTING("external link to root (w/new group format)")
    else
        TESTING("external link to root")

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);

    /* Create file to point to */
    if((fid = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Check that external links are registered with the library */
    if(H5Lis_registered(H5L_TYPE_EXTERNAL) != TRUE) TEST_ERROR

    /* Create file with link to first file */
    if((fid = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link to object in first file */
    if(H5Lcreate_external(filename1, "/", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Check information for external link */
    if(H5Lget_info(fid, "ext_link", &linfo, H5P_DEFAULT) < 0) goto error;
    if(H5L_TYPE_EXTERNAL != linfo.type) {
	H5_FAILED();
	puts("    Unexpected object type - should have been an external link");
	goto error;
    }
    if(H5Lget_val(fid, "ext_link", objname, sizeof(objname), H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lunpack_elink_val(objname, linfo.u.val_size, NULL, &file, &path) < 0) TEST_ERROR
    if(HDstrcmp(file, filename1)) {
	H5_FAILED();
	puts("    External link file name incorrect");
	goto error;
    }
    if(HDstrcmp(path, "/")) {
	H5_FAILED();
	puts("    External link path incorrect");
	goto error;
    }

    /* Create external link to object in first file */
    /* (add a few extra '/'s to make certain library normalizes external link object names) */
    if(H5Lcreate_external(filename1, "///", fid, "ext_link2", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Check information for external link */
    if(H5Lget_info(fid, "ext_link", &linfo, H5P_DEFAULT) < 0) goto error;
    if(H5L_TYPE_EXTERNAL != linfo.type) {
	H5_FAILED();
	puts("    Unexpected object type - should have been an external link");
	goto error;
    }
    if(H5Lget_val(fid, "ext_link", objname, sizeof(objname), H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lunpack_elink_val(objname, linfo.u.val_size, NULL, &file, &path) < 0) TEST_ERROR
    if(HDstrcmp(file, filename1)) {
	H5_FAILED();
	puts("    External link file name incorrect");
	goto error;
    }
    if(HDstrcmp(path, "/")) {
	H5_FAILED();
	puts("    External link path incorrect");
	goto error;
    }

    /* Close and re-open file to ensure that data is written to disk */
    if(H5Fclose(fid) < 0) TEST_ERROR
    if((fid = H5Fopen(filename2, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR


    /* Open object through external link */
    if((gid = H5Gopen2(fid, "ext_link", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if(H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/")) TEST_ERROR

    /* Create object in external file */
    if((gid2 = H5Gcreate2(gid, "new_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close group in external file */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close external object (lets first file close) */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Create a new object using H5Gcreate2 through the external link
     * directly
     */
    if((gid = H5Gcreate2(fid, "ext_link/newer_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close file and group */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Check that all file IDs have been closed */
    if(H5I_nmembers(H5I_FILE) != 0) TEST_ERROR
    if(H5F_sfile_assert_num(0) != 0) TEST_ERROR

    /* Open first file again with read-only access and check on objects created */
    if((fid = H5Fopen(filename1, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* Open objects created through external link */
    if((gid = H5Gopen2(fid, "new_group", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((gid2 = H5Gopen2(fid, "newer_group", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check names */
    if(H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/new_group")) TEST_ERROR
    if(H5Iget_name(gid2, objname, (size_t)NAME_BUF_SIZE) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/newer_group")) TEST_ERROR

    /* Close opened objects */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Check that all file IDs have been closed */
    if(H5I_nmembers(H5I_FILE) != 0) TEST_ERROR
    if(H5F_sfile_assert_num(0) != 0) TEST_ERROR

    /* Verify that new objects can't be created through a read-only external
     * link.
     */
    if((fid = H5Fopen(filename2, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    H5E_BEGIN_TRY {
        gid = H5Gcreate2(fid, "ext_link/readonly_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    } H5E_END_TRY
    if(gid >= 0) TEST_ERROR

    /* Close second file again */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Check that all file IDs have been closed */
    if(H5I_nmembers(H5I_FILE) != 0) TEST_ERROR
    if(H5F_sfile_assert_num(0) != 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Gclose (gid2);
    	H5Gclose (gid);
    	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_root() */


/*-------------------------------------------------------------------------
 * Function:    external_link_path
 *
 * Purpose:     Build a file with external link to object down a path in the
 *              external file
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, July 26, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_path(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    char	filename1[NAME_BUF_SIZE];
    char	filename2[NAME_BUF_SIZE];

    if(new_format)
        TESTING("external link to object on path (w/new group format)")
    else
        TESTING("external link to object on path")

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);

    /* Create file to point to */
    if((fid=H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create object down a path */
    if((gid = H5Gcreate2(fid, "A", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    if((gid = H5Gcreate2(fid, "A/B", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    if((gid = H5Gcreate2(fid, "A/B/C", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Create file with link to first file */
    if((fid=H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link to object in first file */
    if(H5Lcreate_external(filename1, "/A/B/C", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Open object through external link */
    if((gid = H5Gopen2(fid, "ext_link", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if(H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/A/B/C")) TEST_ERROR

    /* Create object in external file */
    if((gid2 = H5Gcreate2(gid, "new_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close group in external file */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close external object (lets first file close) */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close second file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Open first file again and check on object created */
    if((fid = H5Fopen(filename1, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* Open object created through external link */
    if((gid = H5Gopen2(fid, "/A/B/C/new_group", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if(H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/A/B/C/new_group")) TEST_ERROR

    /* Close opened object */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Gclose (gid2);
    	H5Gclose (gid);
    	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_path() */


/*-------------------------------------------------------------------------
 * Function:    external_link_mult
 *
 * Purpose:     Build a file with external link to object that crossed several
 *              external file links
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, July 26, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_mult(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1), fid2 = (-1); 	/* File IDs */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE],
    		filename3[NAME_BUF_SIZE],
    		filename4[NAME_BUF_SIZE];       /* Names of files to externally link across */

    if(new_format)
        TESTING("external links across multiple files (w/new group format)")
    else
        TESTING("external links across multiple files")

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);
    h5_fixname(FILENAME[5], fapl, filename3, sizeof filename3);
    h5_fixname(FILENAME[6], fapl, filename4, sizeof filename4);

    /* Create first file to point to */
    if((fid=H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create object down a path */
    if((gid = H5Gcreate2(fid, "A", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    if((gid = H5Gcreate2(fid, "A/B", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    if((gid = H5Gcreate2(fid, "A/B/C", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Create second file to point to */
    if((fid=H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link down a path */
    if((gid = H5Gcreate2(fid, "D", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    if((gid = H5Gcreate2(fid, "D/E", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Create external link to object in first file */
    if(H5Lcreate_external(filename1, "/A/B/C", gid, "F", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Create third file to point to */
    if((fid=H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link down a path */
    if((gid = H5Gcreate2(fid, "G", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    if((gid = H5Gcreate2(fid, "G/H", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Create external link to object in second file */
    if(H5Lcreate_external(filename2, "/D/E/F", gid, "I", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Create file with link to third file */
    if((fid=H5Fcreate(filename4, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link to object in first file */
    if(H5Lcreate_external(filename3, "/G/H/I", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Open object through external link */
    if((gid = H5Gopen2(fid, "ext_link", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if(H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/A/B/C")) TEST_ERROR

    /* Create object in external file */
    if((gid2 = H5Gcreate2(gid, "new_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close group in external file */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close external object (lets first file close) */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close second file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Open first file again and check on object created */
    if((fid = H5Fopen(filename1, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* Open object created through external link */
    if((gid = H5Gopen2(fid, "/A/B/C/new_group", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if(H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/A/B/C/new_group")) TEST_ERROR

    /* Close opened object */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Open an object through external links */
    if((fid = H5Fopen(filename4, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR
    if((gid = H5Gopen2(fid, "ext_link", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* The intermediate files should not stay open. Replace one of them with a new file. */
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if(H5Fclose(fid2) < 0) TEST_ERROR

    /* Open the other with write access and delete the external link in it */
    if((fid2 = H5Fopen(filename3, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR
    if(H5Ldelete(fid2, "G/H/I", H5P_DEFAULT) < 0) TEST_ERROR

    if(H5Fclose(fid2) < 0) TEST_ERROR

    /* Cleanup */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Gclose (gid2);
    	H5Gclose (gid);
    	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_mult() */


/*-------------------------------------------------------------------------
 * Function:    external_link_self
 *
 * Purpose:     Build a file with external link to itself
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  James Laird
 *              Wednesday, July 12, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_self(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    hid_t	lcpl_id = (-1);     		/* Link Creation Property List ID */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    char	filename1[NAME_BUF_SIZE];
    char	filename2[NAME_BUF_SIZE];
    char	filename3[NAME_BUF_SIZE];

    if(new_format)
        TESTING("external link to self (w/new group format)")
    else
        TESTING("external link to self")

    /* Set up filename */
    h5_fixname(FILENAME[1], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[2], fapl, filename2, sizeof filename1);
    h5_fixname(FILENAME[3], fapl, filename3, sizeof filename1);

    /* Create file */
    if((fid = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create an lcpl with intermediate group creation set */
    if((lcpl_id = H5Pcreate(H5P_LINK_CREATE)) < 0) TEST_ERROR
    if(H5Pset_create_intermediate_group(lcpl_id, TRUE) < 0) TEST_ERROR

    /* Create a series of groups within the file: /A/B and /X/Y/Z */
    if((gid = H5Gcreate2(fid, "A/B", lcpl_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR
    if((gid = H5Gcreate2(fid, "X/Y", lcpl_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    if(H5Pclose (lcpl_id) < 0) TEST_ERROR

    /* Create external link to own root group*/
    if(H5Lcreate_external(filename1, "/X", fid, "A/B/C", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Open object through external link */
    if((gid = H5Gopen2(fid, "A/B/C/", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if(H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/X")) TEST_ERROR

    /* Create object through external link */
    if((gid2 = H5Gcreate2(gid, "new_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close created group */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close object opened through external link */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Check on object created */
    if((gid = H5Gopen2(fid, "X/new_group", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if(H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/X/new_group")) TEST_ERROR

    /* Close opened object */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Complicate things. Use this file as an intermediate file in a chain
        * of external links that will go: file2 -> file1 -> file1 -> file3
        */

    /* Create file2 with an external link to file1  */
    if((fid=H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    if(H5Lcreate_external(filename1, "/A", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close file2 */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Create file3 as a target */
    if((fid=H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((gid = H5Gcreate2(fid, "end", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Open file1 and create an extlink pointing to file3 */
    if((fid=H5Fopen(filename1, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR

    if(H5Lcreate_external(filename3, "/", fid, "/X/Y/Z", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close file1 */
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Re-open file2 and traverse through file1 (with its recursive extlink) to file3 */
    if((fid=H5Fopen(filename2, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR

    if((gid = H5Gopen2(fid, "ext_link/B/C/Y/Z/end", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create object through external link */
    if((gid2 = H5Gcreate2(gid, "newer_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Cleanup */
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Open up file3 and make sure the object was created successfully */
    if((fid = H5Fopen(filename3, H5F_ACC_RDWR, fapl)) < 0) FAIL_STACK_ERROR

    if((gid = H5Gopen2(fid, "end/newer_group", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Cleanup */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();

    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Pclose(lcpl_id);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_self() */


/*-------------------------------------------------------------------------
 * Function:    external_link_pingpong
 *
 * Purpose:     Build a file with external link to object that goes back and
 *              force between two files a couple of times:
 *
 *                      file1:/link1    -> file2: /link2
 *                      file2:/link2    -> file1: /link3
 *                      file1:/link3    -> file2: /link4
 *                      file2:/link4    -> file1: /link5
 *                      file1:/link5    -> file2: /link6
 *                      file2:/link6    -> file1: /final
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, July 26, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_pingpong(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE];       /* Names of files to externally link across */

    if(new_format)
        TESTING("external links back and forth (w/new group format)")
    else
        TESTING("external links back and forth")

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);

    /* Create first file */
    if((fid=H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external links for chain */
    if(H5Lcreate_external(filename2, "/link2", fid, "link1", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename2, "/link4", fid, "link3", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename2, "/link6", fid, "link5", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Create final object */
    if((gid = H5Gcreate2(fid, "final", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Create second file */
    if((fid=H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external links for chain */
    if(H5Lcreate_external(filename1, "/link3", fid, "link2", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename1, "/link5", fid, "link4", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename1, "/final", fid, "link6", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Open first file */
    if((fid=H5Fopen(filename1, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR

    /* Open object through external link */
    if((gid = H5Gopen2(fid, "link1", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if(H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/final")) TEST_ERROR

    /* Create object in external file */
    if((gid2 = H5Gcreate2(gid, "new_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close group in external file */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close external object (lets first file close) */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Open first file again and check on object created */
    if((fid = H5Fopen(filename1, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* Open object created through external link */
    if((gid = H5Gopen2(fid, "/final/new_group", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if(H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/final/new_group")) TEST_ERROR

    /* Close opened object */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();

    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_pingpong() */


/*-------------------------------------------------------------------------
 * Function:    external_link_toomany
 *
 * Purpose:     Build a file with too many external links to objects (i.e.
 *              more than H5L_NLINKS_DEF.  Use a "back & forth" style of
 *              linking (like the "ping pong" test above) to minimize the
 *              number of files involved:
 *
 *                      file1:/link1    -> file2: /link2
 *                      file2:/link2    -> file1: /link3
 *                      file1:/link3    -> file2: /link4
 *                      file2:/link4    -> file1: /link5
 *                      file1:/link5    -> file2: /link6
 *                      file2:/link6    -> file1: /link7
 *                      file1:/link7    -> file2: /link8
 *                      file2:/link8    -> file1: /link9
 *                      file1:/link9    -> file2: /link10
 *                      file2:/link10   -> file1: /link11
 *                      file1:/link11   -> file2: /link12
 *                      file2:/link12   -> file1: /link13
 *                      file1:/link13   -> file2: /link14
 *                      file2:/link14   -> file1: /link15
 *                      file1:/link15   -> file2: /link16
 *                      file2:/link16   -> file1: /link17
 *                      file1:/link17   -> file2: /final
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, August 8, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_toomany(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE];       /* Names of files to externally link across */

    if(new_format)
        TESTING("too many external links (w/new group format)")
    else
        TESTING("too many external links")

    /* Make certain test is valid */
    /* XXX: should probably make a "generic" test that creates the proper
     *          # of links based on this value - QAK
     */
    HDassert(H5L_NUM_LINKS == 16);

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);

    /* Create first file */
    if((fid=H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external links for chain */
    if(H5Lcreate_external(filename2, "/link2", fid, "link1", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename2, "/link4", fid, "link3", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename2, "/link6", fid, "link5", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename2, "/link8", fid, "link7", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename2, "/link10", fid, "link9", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename2, "/link12", fid, "link11", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename2, "/link14", fid, "link13", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename2, "/link16", fid, "link15", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename2, "/final", fid, "link17", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Create second file */
    if((fid=H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external links for chain */
    if(H5Lcreate_external(filename1, "/link3", fid, "link2", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename1, "/link5", fid, "link4", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename1, "/link7", fid, "link6", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename1, "/link9", fid, "link8", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename1, "/link11", fid, "link10", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename1, "/link13", fid, "link12", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename1, "/link15", fid, "link14", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename1, "/link17", fid, "link16", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Create final object */
    if((gid = H5Gcreate2(fid, "final", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Open first file */
    if((fid=H5Fopen(filename1, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR

    /* Open object through external link */
    H5E_BEGIN_TRY {
        gid = H5Gopen2(fid, "link1", H5P_DEFAULT);
    } H5E_END_TRY;
    if (gid >= 0) {
	H5_FAILED();
	printf("%d:    Should have failed for sequence of too many nested links.", __LINE__);
	goto error;
    }

    /* Open object through external link */
    if((gid = H5Gopen2(fid, "link3", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if(H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/final")) TEST_ERROR

    /* Create object in external file */
    if((gid2 = H5Gcreate2(gid, "new_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close group in external file */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close external object */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Gclose (gid2);
    	H5Gclose (gid);
    	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_toomany() */


/*-------------------------------------------------------------------------
 * Function:    external_link_dangling
 *
 * Purpose:     Build a file with "dangling" external links: with both
 *              missing files and missing objects.
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, August 9, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_dangling(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);	                /* Group IDs */
    hid_t   rid = (-1);             /* Root Group ID */
    hid_t   status = (-1);          /* Status */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE];       /* Names of files to externally link across */

    if(new_format)
        TESTING("dangling external links (w/new group format)")
    else
        TESTING("dangling external links")

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);

    /* Create first file */
    if((fid=H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create dangling external links */
    if(H5Lcreate_external("missing", "/missing", fid, "no_file", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename2, "/missing", fid, "no_object", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Create second file (for dangling object test) */
    if((fid=H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Open first file */
    if((fid=H5Fopen(filename1, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR

    /* Get root group ID */
    if((rid=H5Gopen2(fid, "/", H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Open object through dangling file external link */
    H5E_BEGIN_TRY {
        gid = H5Gopen2(fid, "no_file", H5P_DEFAULT);
    } H5E_END_TRY;
    if (gid >= 0) {
	H5_FAILED();
	puts("    Should have failed for sequence of too many nested links.");
	goto error;
    }

    /* Open object through dangling object external link */
    H5E_BEGIN_TRY {
        gid = H5Gopen2(fid, "no_object", H5P_DEFAULT);
    } H5E_END_TRY;
    if (gid >= 0) {
	H5_FAILED();
	puts("    Should have failed for sequence of too many nested links.");
	goto error;
    }

    /* Try to get name of object by index through dangling file external link */
    H5E_BEGIN_TRY {
        status = H5Lget_name_by_idx(rid, "no_file", H5_INDEX_NAME, H5_ITER_INC, 0, NULL, 0, H5P_DEFAULT);
    } H5E_END_TRY;
    if (status >= 0) {
        H5_FAILED();
        puts("    Retreiving name of object by index through dangling file external link should have failed.");
    } /* end if */

    /* Close root group */
    if(H5Gclose(rid) < 0) TEST_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Gclose (gid);
    	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_dangling() */


/*-------------------------------------------------------------------------
 * Function:    external_link_prefix
 *
 * Purpose:     1. target link: "extlinks2"
 *		2. main file: "extlinks0"
 *		3. target file: "tmp/extlinks2"
 * 		4. Set up external link prefix via H5Pset_elink_prefix() to be "tmp"
 *		Should be able to access the target file in tmp directory via the prefix set
 *		by H5Pset_elink_prefix()
 *
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Vailin Choi
 *              Feb 19, 2008
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_prefix(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);	                /* Group IDs */
    hid_t	gapl_id = (-1);
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE],
    		filename3[NAME_BUF_SIZE];

    if(new_format)
        TESTING("external links via H5Pset_elink_prefix()(w/new group format)")
    else
        TESTING("external links via H5Pset_elink_prefix()")

    /* set up name for main file: "extlinks0" */
    h5_fixname(FILENAME[12], fapl, filename1, sizeof filename1);
    /* set up name for external linked target file: "extlinks2" */
    h5_fixname(FILENAME[16], fapl, filename2, sizeof filename2);

    /* create tmp directory and get current working directory path */
    if (HDmkdir(TMPDIR, (mode_t)0755) < 0 && errno != EEXIST)
	TEST_ERROR

    /* set up name for target file: "tmp/extlinks2" */
    h5_fixname(FILENAME[17], fapl, filename3, sizeof filename3);

    /* Create the target file */
    if((fid=H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((gid=H5Gcreate2(fid, "A", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* closing for target file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Create the main file */
    if((fid=H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link to target file (without the absolute path) */
    if(H5Lcreate_external(filename2, "/A", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* set up prefix for external link */
    if((gapl_id = H5Pcreate(H5P_GROUP_ACCESS)) < 0) TEST_ERROR
    if(H5Pset_elink_prefix(gapl_id, TMPDIR) < 0) TEST_ERROR

    /* Open object through external link */
    H5E_BEGIN_TRY {
        gid = H5Gopen2(fid, "ext_link", gapl_id);
    } H5E_END_TRY;

    /* should be able to find the target file from pathnames set via H5Pset_elink_prefix() */
    if (gid < 0) {
	H5_FAILED();
	puts("    Should have found the file in tmp directory.");
	goto error;
    }

    /* closing for main file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Gclose (gid);
	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_prefix() */


/*-------------------------------------------------------------------------
 * Function:    external_link_abs_mainpath: test 3
 *
 * Purpose:     1. target link: "extlinks3"
 *		2. main file: Linux:"/CWD/tmp/extlinks0"; Windows: "<cur drive>:/CWD/tmp/extlinks0"
 *		3. target file: "tmp/extlinks3"
 *		Should be able to access the target file via the main file's absolute path
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Vailin Choi
 *              Feb 19, 2008
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_abs_mainpath(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);	                /* Group IDs */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE],
    		filename3[NAME_BUF_SIZE],
    		tmpname[NAME_BUF_SIZE],
    		cwdpath[NAME_BUF_SIZE];

    if(new_format)
        TESTING("external links via main file's absolute path (w/new group format)")
    else
        TESTING("external links via main file's absolute path")

    /* set up name for external linked target file: "extlinks3" */
    h5_fixname(FILENAME[18], fapl, filename2, sizeof filename2);
    /* set up name for target file: "tmp/extlinks3" */
    h5_fixname(FILENAME[19], fapl, filename3, sizeof filename3);

    /* create tmp directory and get current working directory path */
    if((HDmkdir(TMPDIR, (mode_t)0755) < 0 && errno != EEXIST) || (NULL == HDgetcwd(cwdpath, (size_t)NAME_BUF_SIZE)))
        TEST_ERROR

    /*
     * set up name for main file:
     *	Linux: "/CWD/tmp/extlinks0"
     *  Window: "<cur drive>:/CWD/tmp/extlinks0"
     *  OpenVMS: "<cur disk>$<partition>:[CWD.tmp]extlinks0"
     */
    fix_ext_filename(tmpname, cwdpath, FILENAME[13]);
    h5_fixname(tmpname, fapl, filename1, sizeof filename1);

    /* Create the target file */
    if((fid = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((gid = H5Gcreate2(fid, "A", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* closing for target file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Create the main file */
    if((fid = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link to target file */
    if(H5Lcreate_external(filename2, "/A", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Open object through external link */
    H5E_BEGIN_TRY {
        gid = H5Gopen2(fid, "ext_link", H5P_DEFAULT);
    } H5E_END_TRY;

    /* should be able to find the target file from absolute path set for main file */
    if(gid < 0) {
	H5_FAILED();
	puts("    Should have found the file in tmp directory.");
	goto error;
    }

    /* closing for main file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Gclose (gid);
	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_abs_mainpath() */


/*-------------------------------------------------------------------------
 * Function:    external_link_rel_mainpath: test 4
 *
 * Purpose: 	1. target link: "extlinks4"
 *		2. main file: "tmp/extlinks0"
 *		3. target file: "tmp/extlinks4"
 *		Should be able to access the target file via the main file's CWD+relative path
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Vailin Choi
 *              Feb 19, 2008
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_rel_mainpath(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);	                /* Group IDs */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE],
    		filename3[NAME_BUF_SIZE];

    if(new_format)
        TESTING("external links via main file's CWD + relative path(w/new group format)")
    else
        TESTING("external links via main file's CWD + relative path")

    /* set up name for external linked target file: "extlinks4" */
    h5_fixname(FILENAME[20], fapl, filename2, sizeof filename2);

    if (HDmkdir(TMPDIR, (mode_t)0755) < 0 && errno != EEXIST)
        TEST_ERROR

     /* set up name for main file: "tmp/extlinks0" */
    h5_fixname(FILENAME[13], fapl, filename1, sizeof filename1);
    /* set up name for target file: "tmp/extlinks4" */
    h5_fixname(FILENAME[21], fapl, filename3, sizeof filename3);

    /* Create the target file */
    if((fid=H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((gid=H5Gcreate2(fid, "A", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* closing for target file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Create the main file */
    if((fid=H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link to target file */
    if(H5Lcreate_external(filename2, "/A", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Open object through external link */
    H5E_BEGIN_TRY {
        gid = H5Gopen2(fid, "ext_link", H5P_DEFAULT);
    } H5E_END_TRY;

    /* should be able to find the target file from the main file's relative pathname */
    if (gid < 0) {
	H5_FAILED();
	puts("    Should have found the file in current working directory");
	goto error;
    }

    /* closing for main file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Gclose (gid);
	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_rel_mainpath() */

/*-------------------------------------------------------------------------
 * Function:    external_link_cwd: test 5
 *
 * Purpose:     1. target link: "extlinks5"
 *		2. main file: Linux:"/CWD/tmp/extlinks0"; Window: "<cur drive>:/CWD/tmp/extlinks0"
 * 		2. target file: "extlinks5"
 *		Should be able to access the target file in the current working directory
 *
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Vailin Choi
 *              Feb 19, 2008
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_cwd(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);	                /* Group IDs */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE],
		tmpname[NAME_BUF_SIZE],
                cwdpath[NAME_BUF_SIZE];


    if(new_format)
        TESTING("external links via current working directory(w/new group format)")
    else
        TESTING("external links via current working directory")

    /* set up name for external linked target file: "extlinks5"  */
    /* set up name for target file: "extlinks5" */
    h5_fixname(FILENAME[22], fapl, filename2, sizeof filename2);

    if((HDmkdir(TMPDIR, (mode_t)0755) < 0 && errno != EEXIST) || (NULL == HDgetcwd(cwdpath, (size_t)NAME_BUF_SIZE)))
        TEST_ERROR

    /*
     * set up name for main file:
     *	 Linux: "/CWD/tmp/extlinks0"
     *   Windows: "<cur drive>:/CWD/tmp/extlinks0"
     */
    fix_ext_filename(tmpname, cwdpath, FILENAME[13]);
    h5_fixname(tmpname, fapl, filename1, sizeof filename1);

    /* Create the target file */
    if((fid = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((gid = H5Gcreate2(fid, "A", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* closing for target file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Create the main file */
    if((fid = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link to target file */
    if(H5Lcreate_external(filename2, "/A", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Open object through external link */
    H5E_BEGIN_TRY {
        gid = H5Gopen2(fid, "ext_link", H5P_DEFAULT);
    } H5E_END_TRY;

    /* should be able to find the target file from the current working directory */
    if(gid < 0) {
	H5_FAILED();
	puts("    Should have found the file in current working directory");
	goto error;
    }

    /* closing for main file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Gclose (gid);
	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_cwd() */



/*-------------------------------------------------------------------------
 * Function:    external_link_abstar: test 6
 *
 * Purpose:     1. target link: Linux:"/CWD/tmp/extlinks6"; Windows:"<cur drive>:/CWD/tmp/extlinks6"
 *		2. main file: "extlinks0"
 *		3. target file: "tmp/extlinks6"
 *		Should be able to access the target file's absolute path
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Vailin Choi
 *              Feb. 20, 2008
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_abstar(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);	                /* Group IDs */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE],
    		filename3[NAME_BUF_SIZE],
		tmpname[NAME_BUF_SIZE],
                cwdpath[NAME_BUF_SIZE];

    if(new_format)
        TESTING("external links via target's absolute path (w/new group format)")
    else
        TESTING("external links via target's absolute path")

    /* set up name for main file: "extlinks0" */
    h5_fixname(FILENAME[12], fapl, filename1, sizeof filename1);

     /* create tmp directory and get current working directory path */
    if((HDmkdir(TMPDIR, (mode_t)0755) < 0 && errno != EEXIST) || (NULL == HDgetcwd(cwdpath, (size_t)NAME_BUF_SIZE)))
        TEST_ERROR

    /*
     * set up name for external linked target file:
     *   Linux: "/CWD/tmp/extlinks6"
     *	 Windows: "<cur drive>:/CWD/tmp/extlinks6"
     */
    fix_ext_filename(tmpname, cwdpath, FILENAME[23]);
    h5_fixname(tmpname, fapl, filename2, sizeof filename2);

    /* set up name for target file: "tmp/extlinks6" */
    h5_fixname(FILENAME[23], fapl, filename3, sizeof filename3);

    /* Create the target file */
    if((fid = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((gid = H5Gcreate2(fid, "A", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* closing for target file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Create the main file */
    if((fid = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link to target file */
    if(H5Lcreate_external(filename2, "/A", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Open object through external link */
    H5E_BEGIN_TRY {
        gid = H5Gopen2(fid, "ext_link", H5P_DEFAULT);
    } H5E_END_TRY;

    /* should be able to find the target file with abolute path */
    if(gid < 0) {
	H5_FAILED();
	puts("    Should have found the file in tmp directory.");
	goto error;
    }

    /* closing for main file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Gclose (gid);
	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_abstar() */

/*-------------------------------------------------------------------------
 * Function:    external_link_abstar_cur: test 7
 *
 * Purpose:     1. target link: Linux: "/CWD/tmp/extlinks7"; Windows: "<cur drive>:/CWD/tmp/extlinks7"
 *		2. main file: "extlinks0"
 *		3. target file: "extlinks7"
 *		Should be able to access the target file via the main file's CWD.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Vailin Choi
 *              Feb. 20, 2008
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_abstar_cur(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);	                /* Group IDs */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE],
    		filename3[NAME_BUF_SIZE],
    		tmpname[NAME_BUF_SIZE],
    		cwdpath[NAME_BUF_SIZE];

    if(new_format)
        TESTING("external links via main file's CWD (w/new group format)")
    else
        TESTING("external links via main file's CWD")

    /* set up name for main file: "extlinks0" */
    h5_fixname(FILENAME[12], fapl, filename1, sizeof filename1);

    /* set up name for target file name: "extlinks7" */
    h5_fixname(FILENAME[24], fapl, filename3, sizeof filename3);

    /* create tmp directory and get current working directory path */
    if((HDmkdir(TMPDIR, (mode_t)0755) < 0 && errno != EEXIST) || (NULL == HDgetcwd(cwdpath, (size_t)NAME_BUF_SIZE)))
        TEST_ERROR

     /*
      * set up name for external linked target file:
      *   Linux: "/CWD/tmp/extlinks7"
      *	  Windows: "<cur drive>:/CWD/tmp/extlinks7"
      */
    fix_ext_filename(tmpname, cwdpath, FILENAME[25]);
    h5_fixname(tmpname, fapl, filename2, sizeof filename2);

    /* Create the target file */
    if((fid = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((gid = H5Gcreate2(fid, "A", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* closing for target file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Create the main file */
    if((fid = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link to target file */
    if(H5Lcreate_external(filename2, "/A", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Open object through external link */
    H5E_BEGIN_TRY {
        gid = H5Gopen2(fid, "ext_link", H5P_DEFAULT);
    } H5E_END_TRY;

    /* should be able to find the target file from main file's current working directory */
    if (gid < 0) {
	H5_FAILED();
	puts("    Should have found the file in current working directory.");
	goto error;
    }

    /* closing for main file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Gclose (gid);
	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_abstar_cur() */


/*-------------------------------------------------------------------------
 * Function:    external_link_reltar: test 8
 *
 * Purpose:     1. target link: Linux:"tmp/extlinks8"
 *		2. main file: "extlinks0"
 *		3. target file: "tmp/extlinks8"
 *		Should be able to access the target file via the main file's CWD+ target's relative path
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Vailin Choi
 *              Feb. 20, 2008
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_reltar(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);	                /* Group IDs */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE];

    if(new_format)
        TESTING("external links via main file's CWD + target's relative path(w/new group format)")
    else
        TESTING("external links via main file's CWD + target's relative path")

    /* set up name for main file: "extlinks0" */
    h5_fixname(FILENAME[12], fapl, filename1, sizeof filename1);

    /* create tmp directory */
    if (HDmkdir(TMPDIR, (mode_t)0755) < 0 && errno != EEXIST)
        TEST_ERROR

    /* set up name for target file name: "tmp/extlinks8" */
    /* set up name for external linked target file: "tmp/extlinks8" */
    h5_fixname(FILENAME[26], fapl, filename2, sizeof filename2);

    /* Create the target file */
    if((fid = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((gid = H5Gcreate2(fid, "A", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* closing for target file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Create the main file */
    if((fid = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link to target file */
    if(H5Lcreate_external(filename2, "///A", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Open object through external link */
    if((gid = H5Gopen2(fid, "ext_link", H5P_DEFAULT)) < 0) {
	H5_FAILED();
	puts("    Should have found the file in tmp directory.");
	goto error;
    } /* end if */

    /* closing for main file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Gclose (gid);
	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_reltar() */


/*-------------------------------------------------------------------------
 * Function:    external_link_chdir: test 9
 *
 * Purpose:
 *		1. target link: "extlinks9"
 *		2. main file: "extlinks0"
 *		3. target file" "tmp/extlinks9"
 *		3. chdir "tmp"
 *		Should be able to access the target file in current working directory
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Vailin Choi
 *              Feb. 20, 2008
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_chdir(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);	                /* Group IDs */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE],
    		filename3[NAME_BUF_SIZE];

    if(new_format)
        TESTING("external links via chdir and found in current working directory (w/new group format)")
    else
        TESTING("external links via chdir and found in current working directory")

    /* set up name for main file: "extlinks0" */
    h5_fixname(FILENAME[12], fapl, filename1, sizeof filename1);
    /* set up name for external linked target file ("extlinks9") */
    h5_fixname(FILENAME[27], fapl, filename2, sizeof filename2);

    /* create tmp directory */
    if (HDmkdir(TMPDIR, (mode_t)0755) < 0 && errno != EEXIST)
        TEST_ERROR

    /* set up name for target file name ("tmp/extlinks9") */
    h5_fixname(FILENAME[28], fapl, filename3, sizeof filename3);

    /* Create the target file */
    if((fid=H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((gid=H5Gcreate2(fid, "A", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* closing for target file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Create the main file */
    if((fid=H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link to target file */
    if(H5Lcreate_external(filename2, "/A", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR


    if (HDchdir(TMPDIR) < 0) TEST_ERROR

    /* Open object through external link */
    H5E_BEGIN_TRY {
        gid = H5Gopen2(fid, "ext_link", H5P_DEFAULT);
    } H5E_END_TRY;

    if (HDchdir("..") < 0) TEST_ERROR

    /*
     * Should be able to find the target file from:
     * main file's current working directory + pathname of external linked targetfile
     */
    if (gid < 0) {
	H5_FAILED();
	puts("    Should have found the file in tmp directory.");
	goto error;
    }

    /* closing for main file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Gclose (gid);
	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_chdir() */


/*-------------------------------------------------------------------------
 * Function:    external_set_elink_fapl1: test 10
 *
 * Purpose:     To verify that the external linked target file with physical layout
 *		different from the parent can be successfully opened.
 *
 *		1. target link: "extlinks16"
 * 		2. target file: "extlinks16"
 *		3. main file: Linux:"/CWD/tmp/extlinks0"; Window: "<cur drive>:/CWD/tmp/extlinks0"
 *		4. Create target file A to be a "family" file: extlinks16A
 *		4. Create target file B to be a "multi" file: extlinks16B
 *		5. Create external link from main file to target file A: ext_linkA->extlinks16A:/A
 *		5. Create external link from main file to target file B: ext_linkB->extlinks16B:/B
 *		6. Should succeed in opening the target object: ext_extA
 *		6. Should succeed in opening the target object: ext_extB
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Vailin Choi
 *              Sept. 12, 2008
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_set_elink_fapl1(hid_t fapl, hbool_t new_format)
{
    hid_t	fid=(-1);
    hid_t	fidA=(-1), fidB=(-1);
    hid_t	gidA=(-1), gidB=(-1);
    hid_t	oidA=(-1), oidB=(-1);
    char	filename1[NAME_BUF_SIZE],
    		filename2A[NAME_BUF_SIZE],
    		filename2B[NAME_BUF_SIZE],
		tmpname[NAME_BUF_SIZE],
                cwdpath[NAME_BUF_SIZE];
    hid_t 	fam_fapl=-1, multi_fapl=-1;
    hid_t	lapl_idA=-1, lapl_idB=-1;
    H5FD_mem_t	mt, memb_map[H5FD_MEM_NTYPES];
    hid_t	memb_fapl[H5FD_MEM_NTYPES];
    char        sv[H5FD_MEM_NTYPES][500];
    const	char *memb_name[H5FD_MEM_NTYPES];
    haddr_t	memb_addr[H5FD_MEM_NTYPES];

    if(new_format)
        TESTING("H5Pset/get_elink_fapl() with different physical layouts (w/new group format)")
    else
        TESTING("H5Pset/get_elink_fapl() with different physical layouts")

    if((HDmkdir(TMPDIR, (mode_t)0755) < 0 && errno != EEXIST) || (NULL == HDgetcwd(cwdpath, (size_t)NAME_BUF_SIZE)))
	TEST_ERROR

    /*
     * set up name for main file:
     *	 Linux: "/CWD/tmp/extlinks0"
     *   Windows: "<cur drive>:/CWD/tmp/extlinks0"
     */
    fix_ext_filename(tmpname, cwdpath, FILENAME[13]);
    h5_fixname(tmpname, fapl, filename1, sizeof filename1);

    /* create "family" fapl */
    fam_fapl = h5_fileaccess();
    if(H5Pset_fapl_family(fam_fapl, (hsize_t)FAMILY_SIZE, H5P_DEFAULT) < 0)
        TEST_ERROR;

    /* set up name for external linked target file A: "extlinks16A"  */
    /* set up name for target file A: "extlinks16A" */
    h5_fixname(FILENAME[37], fam_fapl, filename2A, sizeof filename2A);

    /* settings for multi file */
    HDmemset(memb_map, 0,  sizeof memb_map);
    HDmemset(memb_fapl, 0, sizeof memb_fapl);
    HDmemset(memb_name, 0, sizeof memb_name);
    HDmemset(memb_addr, 0, sizeof memb_addr);
    HDmemset(sv, 0, sizeof sv);

    for(mt = H5FD_MEM_DEFAULT; mt < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, mt)) {
	memb_map[mt] = H5FD_MEM_SUPER;
	memb_fapl[mt] = H5P_DEFAULT;
    } /* end for */

    memb_map[H5FD_MEM_DRAW] = H5FD_MEM_DRAW;
    memb_map[H5FD_MEM_BTREE] = H5FD_MEM_BTREE;
    memb_map[H5FD_MEM_GHEAP] = H5FD_MEM_GHEAP;
    memb_map[H5FD_MEM_LHEAP] = H5FD_MEM_LHEAP;

    sprintf(sv[H5FD_MEM_SUPER], "%%s-%c.h5", 's');
    memb_name[H5FD_MEM_SUPER] = sv[H5FD_MEM_SUPER];
    memb_addr[H5FD_MEM_SUPER] = 0;

    sprintf(sv[H5FD_MEM_BTREE], "%%s-%c.h5", 'b');
    memb_name[H5FD_MEM_BTREE] = sv[H5FD_MEM_BTREE];
    memb_addr[H5FD_MEM_BTREE] = HADDR_MAX/6;

    sprintf(sv[H5FD_MEM_DRAW], "%%s-%c.h5", 'r');
    memb_name[H5FD_MEM_DRAW] = sv[H5FD_MEM_DRAW];
    memb_addr[H5FD_MEM_DRAW] = HADDR_MAX/3;

    sprintf(sv[H5FD_MEM_GHEAP], "%%s-%c.h5", 'g');
    memb_name[H5FD_MEM_GHEAP] = sv[H5FD_MEM_GHEAP];
    memb_addr[H5FD_MEM_GHEAP] = HADDR_MAX/2;

    sprintf(sv[H5FD_MEM_LHEAP], "%%s-%c.h5", 'l');
    memb_name[H5FD_MEM_LHEAP] = sv[H5FD_MEM_LHEAP];
    memb_addr[H5FD_MEM_LHEAP] = (HADDR_MAX/3)*2;

    sprintf(sv[H5FD_MEM_OHDR], "%%s-%c.h5", 'o');
    memb_name[H5FD_MEM_OHDR] = sv[H5FD_MEM_OHDR];
    memb_addr[H5FD_MEM_OHDR] = (HADDR_MAX/6)*5;

    /* create "multi" fapl */
    multi_fapl = h5_fileaccess();
    if(H5Pset_fapl_multi(multi_fapl, memb_map, memb_fapl, memb_name, memb_addr, TRUE) < 0)
        TEST_ERROR;

    /* set up name for external linked target file B: "extlinks16B"  */
    /* set up name for target file B: "extlinks16B" */
    h5_fixname(FILENAME[38], multi_fapl, filename2B, sizeof filename2B);

    /* Create target file A to be a "family" file */
    if((fidA=H5Fcreate(filename2A, H5F_ACC_TRUNC, H5P_DEFAULT, fam_fapl)) < 0) TEST_ERROR
    if((gidA=H5Gcreate2(fidA, "A", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Create target file B to be a "multi" file */
    if((fidB=H5Fcreate(filename2B, H5F_ACC_TRUNC, H5P_DEFAULT, multi_fapl)) < 0) TEST_ERROR
    if((gidB=H5Gcreate2(fidB, "B", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* closing for target files */
    if(H5Gclose(gidA) < 0) TEST_ERROR
    if(H5Gclose(gidB) < 0) TEST_ERROR
    if(H5Fclose(fidA) < 0) TEST_ERROR
    if(H5Fclose(fidB) < 0) TEST_ERROR

    /* Create the main file */
    if((fid=H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link to target file A:/A */
    if(H5Lcreate_external(filename2A, "/A", fid, "ext_linkA", H5P_DEFAULT, H5P_DEFAULT) < 0)
	TEST_ERROR
    /* Create external link to target file B:/B */
    if(H5Lcreate_external(filename2B, "/B", fid, "ext_linkB", H5P_DEFAULT, H5P_DEFAULT) < 0)
	TEST_ERROR

    /* Set file access property list for link access to use the family driver */
    if((lapl_idA = H5Pcreate(H5P_LINK_ACCESS)) < 0) TEST_ERROR
    if(H5Pset_elink_fapl(lapl_idA, fam_fapl) < 0) TEST_ERROR

    /* open target object A */
    oidA = H5Oopen(fid, "ext_linkA", lapl_idA);

    /* should succeed in opening the target object A in the current working directory */
    if (oidA < 0) {
	H5_FAILED();
	puts("    Should succeed in opening family target file A in current working directory");
	goto error;
    }

    /* Set file access property list for link access to use the multi driver */
    if((lapl_idB = H5Pcreate(H5P_LINK_ACCESS)) < 0) TEST_ERROR
    if(H5Pset_elink_fapl(lapl_idB, multi_fapl) < 0) TEST_ERROR

    /* open target object B */
    oidB = H5Oopen(fid, "ext_linkB", lapl_idB);

    /* should succeed in opening the target object B in the current working directory */
    if (oidB < 0) {
	H5_FAILED();
	puts("    Should succeed in opening multi target file B in current working directory");
	goto error;
    }

    /* closing */
    if(H5Pclose(lapl_idA) < 0) TEST_ERROR
    if(H5Pclose(lapl_idB) < 0) TEST_ERROR
    if(H5Pclose(fam_fapl) < 0) TEST_ERROR
    if(H5Pclose(multi_fapl) < 0) TEST_ERROR
    if(H5Oclose(oidA) < 0) TEST_ERROR
    if(H5Oclose(oidB) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Pclose (lapl_idA);
	H5Pclose (lapl_idB);
	H5Pclose (fam_fapl);
	H5Pclose (multi_fapl);
	H5Gclose (gidA);
	H5Gclose (gidB);
	H5Oclose (oidA);
	H5Oclose (oidB);
	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end external_set_elink_fapl1() */


/*-------------------------------------------------------------------------
 * Function:    external_set_elink_fapl2: test 11
 *
 * Purpose:     To verify that processing done to the external linked target object is
 *		correctly handled when the parent and target files have the same
 *		physical layout but different access methods.
 *
 * 		1. target link: "extlinks17"
 * 		2. target file: "extlinks17"
 *		3. main file: Linux:"/CWD/tmp/extlinks0"; Window: "<cur drive>:/CWD/tmp/extlinks0"
 *		4. Create target file to be a "core" file:/A/Dataset
 *		5. Create external link from main file to target file:ext_link->target file:/A/Dataset
 *		6. Set the file access property list of the link access to use "core" file without
 *		   backing store
 *		6. Should succeed in opening the target dataset: ext_link
 *		7. Write data to the target dataset
 *		8. On closing, the file size of target should be the same as before since
 *		   it is opened without backing store.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Vailin Choi
 *              Sept. 12, 2008
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_set_elink_fapl2(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);	                /* Group IDs */
    hid_t       core_fapl = -1, space = -1, dset = -1, did = -1, dapl_id = -1, dcpl = -1;
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE],
		tmpname[NAME_BUF_SIZE],
                cwdpath[NAME_BUF_SIZE];
    hsize_t     dims[2];
    int		points[NUM40][NUM40];
    h5_stat_size_t	filesize, new_filesize;
    int		i, j, n;

    if(new_format)
        TESTING("H5Pset/get_elink_fapl() with same physical layout (w/new group format)")
    else
        TESTING("H5Pset/get_elink_fapl() with same physical layout")

    if((HDmkdir(TMPDIR, (mode_t)0755) < 0 && errno != EEXIST) || (NULL == HDgetcwd(cwdpath, (size_t)NAME_BUF_SIZE)))
	TEST_ERROR

    /*
     * set up name for main file:
     *	 Linux: "/CWD/tmp/extlinks0"
     *   Windows: "<cur drive>:/CWD/tmp/extlinks0"
     */
    fix_ext_filename(tmpname, cwdpath, FILENAME[13]);
    h5_fixname(tmpname, fapl, filename1, sizeof filename1);

    /* create fapl for the target file to be a "core" file */
    core_fapl = h5_fileaccess();
    if(H5Pset_fapl_core(core_fapl, (size_t)CORE_INCREMENT, TRUE) < 0)
        TEST_ERROR

    /* set up name for external linked target file: "extlinks17"  */
    /* set up name for target file: "extlinks17" */
    h5_fixname(FILENAME[39], core_fapl, filename2, sizeof filename2);

    /* Create the target file to be a "core" file */
    if((fid = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, core_fapl)) < 0) TEST_ERROR
    if((gid = H5Gcreate2(fid, "A", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    dims[0] = NUM40;
    dims[1] = NUM40;
    if((space = H5Screate_simple(2, dims, NULL)) < 0) TEST_ERROR

    /* Create dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR;
    if(H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_LATE) < 0) TEST_ERROR;

    /* create "Dataset" in group "A" of target file */
    if((dset = H5Dcreate2(gid, "Dataset", H5T_NATIVE_INT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
	TEST_ERROR

    /* closing for target file */
    if(H5Pclose(dcpl) < 0) TEST_ERROR
    if(H5Sclose(space) < 0) TEST_ERROR
    if(H5Dclose(dset) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* get size of target file */
    filesize = h5_get_file_size(filename2, core_fapl);

    /* Create the main file */
    if((fid=H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link to target file: ext_link->extlinks17:/A/Dataset */
    if(H5Lcreate_external(filename2, "/A/Dataset", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0)
	TEST_ERROR

    /* create fapl to be a "core" file without backing store */
    if(H5Pset_fapl_core(core_fapl, (size_t)CORE_INCREMENT, FALSE) < 0)
        TEST_ERROR

    /* Set file access property list for link access to use the "core" driver */
    if((dapl_id = H5Pcreate(H5P_DATASET_ACCESS)) < 0) TEST_ERROR
    if(H5Pset_elink_fapl(dapl_id, core_fapl) < 0) TEST_ERROR

    /* try to open the external linked target dataset */
    did = H5Dopen2(fid, "ext_link", dapl_id);
    if(did < 0) {
	H5_FAILED();
	puts("    Should succeed in opening the target dataset");
	goto error;
    }

    /* Initialize the dataset */
    for(i = n = 0; i < NUM40; i++)
        for(j = 0; j < NUM40; j++)
            points[i][j] = n++;

    /* Write the data to the dataset */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, points) < 0)
	TEST_ERROR

    if(H5Pclose(dapl_id) < 0) TEST_ERROR
    if(H5Dclose(did) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    new_filesize = h5_get_file_size(filename2, core_fapl);

    /* the file size should remain the same since there is no backing store */
    if(new_filesize != filesize) TEST_ERROR

    if(H5Pclose(core_fapl) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Pclose(dcpl);
	H5Sclose(space);
	H5Dclose(dset);
	H5Pclose(core_fapl);
	H5Pclose(dapl_id);
	H5Dclose(did);
	H5Gclose(gid);
	H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end external_set_elink_fapl2() */


/*-------------------------------------------------------------------------
 * Function:    external_set_elink_fapl3: test 12
 *
 * Purpose:     To verify that the file access property list for link access is
 *		set and closed correctly.
 *
 *		1. Create fapl for core driver
 *		2. Create fapl for stdio driver
 *		3. Set link access's fapl to use stdio driver
 *		4. Verify that link access's fapl is the stdio driver
 *		5. Reset the link access' fapl to use core driver
 *		6. H5Pcopy() the link access
 *		7. Get the fapl property value of the original link access
 *		8. Close the original link access
 *		9. H5Pclose() fapl should fail since closing in step #8 should also close its fapl
 *	       10. Verify that the copied link access's fapl is the core driver
 *	       11. Get the fapl property value of the copied link access
 *	       12. H5Premove() the fapl property from the copied link access
 *	       13. H5Pclose() fapl set in the copied link access should fail since the
 *		   removal in #12 should also close its fapl
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Vailin Choi
 *              Sept. 12, 2008
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_set_elink_fapl3(hbool_t new_format)
{
    hid_t       core_fapl = -1, stdio_fapl = -1;
    hid_t	lapl_id = -1, new_lapl_id = -1, l_fapl = -1, out_fapl;
    int   	ret;

    if(new_format)
        TESTING("H5Pset/get_fapl() (w/new group format)")
    else
        TESTING("H5Pset/get_fapl()")

    /* create fapl for the target file to be a "core" file */
    core_fapl = h5_fileaccess();
    if(H5Pset_fapl_core(core_fapl, (size_t)CORE_INCREMENT, TRUE) < 0)
        TEST_ERROR

    stdio_fapl = h5_fileaccess();
    if(H5Pset_fapl_stdio(stdio_fapl) < 0)
        TEST_ERROR

    /* Set file access property list for link access to use the "stdio" driver */
    if((lapl_id = H5Pcreate(H5P_LINK_ACCESS)) < 0) TEST_ERROR
    if(H5Pset_elink_fapl(lapl_id, stdio_fapl) < 0) TEST_ERROR

    /* Verify that the driver for the link's fapl is the "stdio" driver */
    if((l_fapl = H5Pget_elink_fapl(lapl_id)) < 0) TEST_ERROR
    if(H5Pget_driver(l_fapl) != H5FD_STDIO) TEST_ERROR
    if(H5Pclose(l_fapl) < 0) TEST_ERROR

    /* Set file access property list for link access to use the "core" driver */
    if(H5Pset_elink_fapl(lapl_id, core_fapl) < 0) TEST_ERROR

    /* Make a copy of the link access property */
    if((new_lapl_id = H5Pcopy(lapl_id)) < 0) TEST_ERROR

    /* get the fapl set in lapl_id */
    if(H5Pget(lapl_id, "external link fapl", &out_fapl) < 0) TEST_ERROR
    if(H5Pclose(lapl_id) < 0) TEST_ERROR

    /* Try closing out_fapl should fail since H5Pclose(lapl_id) should also close its fapl */
    H5E_BEGIN_TRY {
        ret = H5Pclose(out_fapl);
    } H5E_END_TRY;
    if(ret != FAIL) TEST_ERROR

    /* Verify that the driver for the copied link's fapl is the "core" driver */
    if((l_fapl = H5Pget_elink_fapl(new_lapl_id)) < 0) TEST_ERROR
    if(H5Pget_driver(l_fapl) != H5FD_CORE) TEST_ERROR

    /* get the fapl set in new_lapl_id */
    if(H5Pget(new_lapl_id, "external link fapl", &out_fapl) < 0) TEST_ERROR
    if(H5Premove(new_lapl_id, "external link fapl") < 0) TEST_ERROR

    /* Try closing out_fapl should fail since the property is removed from new_lapl_id */
    H5E_BEGIN_TRY {
        ret = H5Pclose(out_fapl);
    } H5E_END_TRY;
    if(ret != FAIL) TEST_ERROR

    if(H5Pclose(l_fapl) < 0) TEST_ERROR
    if(H5Pclose(new_lapl_id) < 0) TEST_ERROR
    if(H5Pclose(core_fapl) < 0) TEST_ERROR
    if(H5Pclose(stdio_fapl) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Pclose(l_fapl);
	H5Pclose(lapl_id);
	H5Pclose(new_lapl_id);
	H5Pclose(core_fapl);
	H5Pclose(stdio_fapl);
    } H5E_END_TRY;
    return -1;
} /* end external_set_elink_fapl3() */


/*-------------------------------------------------------------------------
 * Function:    external_set_elink_acc_flags
 *
 * Purpose:     Verify functionality of H5P_set/get_elink_acc_flags
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Neil Fortner
 *              Jan. 5, 2009
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_set_elink_acc_flags(hid_t fapl, hbool_t new_format)
{
    hid_t       file1 = -1, file2 = -1, group = -1, subgroup = -1, gapl = -1;
    char        filename1[NAME_BUF_SIZE],
                filename2[NAME_BUF_SIZE];
    herr_t      ret;
    unsigned    flags;

    if(new_format)
        TESTING("H5Pset/get_elink_acc_flags() (w/new group format)")
    else
        TESTING("H5Pset/get_elink_acc_flags()")

    /* Create parent and target files, and external link */
    h5_fixname(FILENAME[40], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[41], fapl, filename2, sizeof filename2);
    if((file1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((file2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if(H5Lcreate_external(filename2, "/", file1, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close file2, leave file1 open (should be read-write) */
    if(H5Fclose(file2) < 0) TEST_ERROR

    /* Create new gapl, and set elink access flags to be H5F_ACC_RDONLY */
    if((gapl = H5Pcreate(H5P_GROUP_ACCESS)) < 0) TEST_ERROR
    if(H5Pset_elink_acc_flags(gapl, H5F_ACC_RDONLY) < 0) TEST_ERROR

    /* Verify "get" routine functionality */
    if(H5Pget_elink_acc_flags(gapl, &flags) < 0) TEST_ERROR
    if(flags != H5F_ACC_RDONLY) TEST_ERROR

    /* Attempt to create a group through the external link using gapl (should fail) */
    H5E_BEGIN_TRY {
        group = H5Gcreate2(file1, "/ext_link/group", H5P_DEFAULT, H5P_DEFAULT, gapl);
    } H5E_END_TRY;
    if(group != FAIL) TEST_ERROR

    /* Close file1 and reopen with read only access */
    if(H5Fclose(file1) < 0) TEST_ERROR
    if((file1 = H5Fopen(filename1, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* Set elink access flags on gapl to be H5F_ACC_RDWR */
    if(H5Pset_elink_acc_flags(gapl, H5F_ACC_RDWR) < 0) TEST_ERROR

    /* Create a group through the external link using gapl (should succeed) */
    if((group = H5Gcreate2(file1, "/ext_link/group", H5P_DEFAULT, H5P_DEFAULT, gapl)) < 0) TEST_ERROR

    /* Unset elink access flags on gapl */
    if(H5Pset_elink_acc_flags(gapl, H5F_ACC_DEFAULT) < 0) TEST_ERROR

    /* Attempt to create a group through the external link using gapl (should fail) */
    H5E_BEGIN_TRY {
        subgroup = H5Gcreate2(file1, "/ext_link/group/subgroup", H5P_DEFAULT, H5P_DEFAULT, gapl);
    } H5E_END_TRY;
    if(subgroup != FAIL) TEST_ERROR

    /* Attempt to set invalid flags on gapl */
    H5E_BEGIN_TRY {
        ret = H5Pset_elink_acc_flags(gapl, H5F_ACC_TRUNC);
    } H5E_END_TRY;
    if(ret != FAIL) TEST_ERROR
    H5E_BEGIN_TRY {
        ret = H5Pset_elink_acc_flags(gapl, H5F_ACC_EXCL);
    } H5E_END_TRY;
    if(ret != FAIL) TEST_ERROR
    H5E_BEGIN_TRY {
        ret = H5Pset_elink_acc_flags(gapl, H5F_ACC_DEBUG);
    } H5E_END_TRY;
    if(ret != FAIL) TEST_ERROR
    H5E_BEGIN_TRY {
        ret = H5Pset_elink_acc_flags(gapl, H5F_ACC_CREAT);
    } H5E_END_TRY;
    if(ret != FAIL) TEST_ERROR

    /* Close file1 and group */
    if(H5Gclose(group) < 0) TEST_ERROR
    if(H5Fclose(file1) < 0) TEST_ERROR

    /* Verify that H5Fcreate and H5Fopen reject H5F_ACC_DEFAULT */
    H5E_BEGIN_TRY {
        file1 = H5Fcreate(filename1, H5F_ACC_DEFAULT, H5P_DEFAULT, fapl);
    } H5E_END_TRY;
    if(file1 != FAIL) TEST_ERROR
    H5E_BEGIN_TRY {
        file1 = H5Fcreate(filename1, H5F_ACC_TRUNC | H5F_ACC_DEFAULT, H5P_DEFAULT, fapl);
    } H5E_END_TRY;
    if(file1 != FAIL) TEST_ERROR
    H5E_BEGIN_TRY {
        file1 = H5Fopen(filename1, H5F_ACC_DEFAULT, fapl);
    } H5E_END_TRY;
    if(file1 != FAIL) TEST_ERROR
    H5E_BEGIN_TRY {
        file1 = H5Fopen(filename1, H5F_ACC_RDWR | H5F_ACC_DEFAULT, fapl);
    } H5E_END_TRY;
    if(file1 != FAIL) TEST_ERROR

    /* Close gapl */
    if(H5Pclose(gapl) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
        H5Gclose(group);
        H5Gclose(subgroup);
        H5Fclose(file1);
        H5Fclose(file2);
        H5Pclose(gapl);
    } H5E_END_TRY;
    return -1;
} /* end external_set_elink_acc_flags() */


/*-------------------------------------------------------------------------
 * Function:    external_set_elink_cb
 *
 * Purpose:     Verify functionality of H5P_set/get_elink_cb
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Neil Fortner
 *              Jan. 5, 2009
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
 /* User data structure for callback function */
typedef struct {
    const char  *parent_file;   /* Expected parent file name */
    const char  *target_file;   /* Expected target file name */
    hid_t       base_fapl;      /* Base fapl for family driver */
    hsize_t     fam_size;       /* Size of family files */
    int         code;           /* Code to control the actions taken by the callback */
} set_elink_cb_t;

/* Callback function */
static herr_t
external_set_elink_cb_cb(const char *parent_file, const char *parent_group,
    const char *target_file, const char *target_obj, unsigned *flags,
    hid_t fapl, void *_op_data)
{
    set_elink_cb_t  *op_data = (set_elink_cb_t *)_op_data;

    /* Verify file and object names are correct */
    if (HDstrcmp(parent_file, op_data->parent_file)) return FAIL;
    if (HDstrcmp(parent_group, "/group1")) return FAIL;
    if (HDstrcmp(target_file, op_data->target_file)) return FAIL;
    if (HDstrcmp(target_obj, "/")) return FAIL;

    /* Set flags to be read-write */
    *flags = (*flags & ~H5F_ACC_RDONLY) | H5F_ACC_RDWR;

    /* Set family file driver on fapl */
    if (H5Pset_fapl_family(fapl, op_data->fam_size, op_data->base_fapl) < 0) return FAIL;

    /* Codes to cause an invalid condition (and verify that an error is issued */
    if (op_data->code == 1)
        return FAIL;
    if (op_data->code == 2)
        *flags = H5F_ACC_DEFAULT;

    return 0;
}

/* Main test function */
static int
external_set_elink_cb(hid_t fapl, hbool_t new_format)
{
    hid_t       file1 = -1, file2 = -1, group = -1, gapl = -1, fam_fapl = -1, ret_fapl = -1, base_driver;
    set_elink_cb_t op_data,
                *op_data_p;
    H5L_elink_traverse_t cb;
    char        filename1[NAME_BUF_SIZE],
                filename2[NAME_BUF_SIZE];
    unsigned    flags;

    if(new_format)
        TESTING("H5Pset/get_elink_cb() (w/new group format)")
    else
        TESTING("H5Pset/get_elink_cb()")

    /* Build user data for callback */
    op_data.parent_file = filename1;
    op_data.target_file = filename2;
    /* Core file driver has issues when used as the member file driver for a family file */
    /* Family file driver cannot be used with family or multi drivers for member files */
    /* Also disable parellel member drivers, because IS_H5FD_MPI whould report FALSE, causing problems */
    base_driver = H5Pget_driver(fapl);
    op_data.base_fapl = (base_driver == H5FD_FAMILY || base_driver ==  H5FD_MULTI
            || base_driver == H5FD_MPIO || base_driver == H5FD_MPIPOSIX
            || base_driver == H5FD_CORE) ? H5P_DEFAULT : fapl;
    op_data.fam_size = ELINK_CB_FAM_SIZE;
    op_data.code = 0;

    /* Create family fapl */
    if ((fam_fapl = H5Pcopy(fapl)) < 0) TEST_ERROR
    if (H5Pset_fapl_family(fam_fapl, op_data.fam_size, op_data.base_fapl) < 0) TEST_ERROR

    /* Create parent and target files, group, and external link */
    h5_fixname(FILENAME[42], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[43], fam_fapl, filename2, sizeof filename2);
    if((file1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((file2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fam_fapl)) < 0) TEST_ERROR
    if((group = H5Gcreate2(file1, "group1",H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Lcreate_external(filename2, "/", group, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close files and group */
    if(H5Fclose(file1) < 0) TEST_ERROR
    if(H5Fclose(file2) < 0) TEST_ERROR
    if(H5Gclose(group) < 0) TEST_ERROR

    /* Create new gapl, and set elink callback */
    if((gapl = H5Pcreate(H5P_GROUP_ACCESS)) < 0) TEST_ERROR
    if(H5Pset_elink_cb(gapl, external_set_elink_cb_cb, &op_data) < 0) TEST_ERROR

    /* Verify "get" routine functionality */
    if(H5Pget_elink_cb(gapl, &cb, (void **) &op_data_p) < 0) TEST_ERROR
    if(cb != external_set_elink_cb_cb) TEST_ERROR
    if(op_data_p != &op_data) TEST_ERROR

    /* Open file1 with read only access */
    if((file1 = H5Fopen(filename1, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* Create a group through the external link using gapl */
    if((group = H5Gcreate2(file1, "/group1/ext_link/group2", H5P_DEFAULT, H5P_DEFAULT, gapl)) < 0) TEST_ERROR

    /* Verify that the correct parameters have been set on file2 (somewhat
     * redundant as the library would be unable to create the group otherwise)
     */
    if((file2 = H5Iget_file_id(group)) < 0) TEST_ERROR
    if(H5Fget_intent(file2, &flags) < 0) TEST_ERROR
    if(!(flags & H5F_ACC_RDWR)) TEST_ERROR
    if((ret_fapl = H5Fget_access_plist(file2)) < 0) TEST_ERROR
    if(H5FD_FAMILY != H5Pget_driver(ret_fapl)) TEST_ERROR

    if(H5Gclose(group) < 0) TEST_ERROR
    if(H5Fclose(file2) < 0) TEST_ERROR
    if(H5Pclose(ret_fapl) < 0) TEST_ERROR
    if(H5Pclose(fam_fapl) < 0) TEST_ERROR

    /* Modify the user data structure to cause the callback to fail next time */
    op_data.code = 1;

    /* Attempt to reopen group2 (should fail) */
    H5E_BEGIN_TRY {
        group = H5Gopen2(file1, "/group1/ext_link/group2", gapl);
    } H5E_END_TRY;
    if(group != FAIL) TEST_ERROR

    /* Modify the user data structure to cause the callback to return invalid flags */
    op_data.code = 2;

    /* Attempt to reopen group2 (should fail) */
    H5E_BEGIN_TRY {
        group = H5Gopen2(file1, "/group1/ext_link/group2", gapl);
    } H5E_END_TRY;
    if(group != FAIL) TEST_ERROR

    /* Close */
    if(H5Fclose(file1) < 0) TEST_ERROR
    if(H5Pclose(gapl) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
        H5Gclose(group);
        H5Fclose(file1);
        H5Fclose(file2);
        H5Pclose(gapl);
        H5Pclose(ret_fapl);
        H5Pclose(fam_fapl);
    } H5E_END_TRY;
    return -1;
} /* end external_set_elink_cb() */


/*-------------------------------------------------------------------------
 * Function:    external_reset_register
 *
 * Purpose:     Check that external links are registered after the library
 *              is reset.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Neil Fortner
 *              Apr. 9, 2009
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_reset_register(void)
{
    hid_t       file;
    char        filename[NAME_BUF_SIZE];

    TESTING("external links are registered after reset")

    /* Create and close file */
    h5_fixname(FILENAME[44], H5P_DEFAULT, filename, sizeof filename);
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if (H5Fclose(file) < 0) TEST_ERROR

    /* Reset the library */
    H5close();

    /* Re open file */
    if ((file = H5Fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Create an external link */
    if (H5Lcreate_external("some_file", "some_obj", file, "ext_link1", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close file */
    if (H5Fclose(file) < 0) TEST_ERROR

    /* Try again to make sure the previous H5Lcreate_external call does not
     * affect the ability to reset */
    H5close();
    if ((file = H5Fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) TEST_ERROR
    if (H5Lcreate_external("another_file", "another_obj", file, "ext_link2", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if (H5Fclose(file) < 0) TEST_ERROR

    if(HDremove(filename) != 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
    } H5E_END_TRY;
    return -1;
} /* end external_reset_register() */


#ifdef H5_HAVE_WINDOW_PATH

/*-------------------------------------------------------------------------
 * Function:    external_link_win1
 *
 * Purpose:
 *   		1. target link: "/CWD/tmp/extlinks10"
 *		2. main file: "extlinks0"
 *		3. target file: "extlinks10"
 *		Should be able to find the target file via main file's current drive/rel path
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Vailin Choi
 *              April 15, 2008
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_win1(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);	                /* Group IDs */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE],
    		filename3[NAME_BUF_SIZE],
		tmpname[NAME_BUF_SIZE],
                cwdpath[NAME_BUF_SIZE];

    if(new_format)
        TESTING("external links via main file's current drive/rel path(windows)(w/new group format)")
    else
        TESTING("external links via main file's current drive/rel path(windows)")

    /* set up name for main file: "extlinks0" */
    h5_fixname(FILENAME[12], fapl, filename1, sizeof filename1);

    if(NULL == HDgetcwd(cwdpath, (size_t)NAME_BUF_SIZE))
        TEST_ERROR

    /* set up name for target link: "/CWD/tmp/extlinks10" */
    HDstrcpy(tmpname, &cwdpath[2]); /* stripped the drive letter to make it rel drive but absolute path */
    HDstrcat(tmpname, "/");
    HDstrcat(tmpname, FILENAME[30]);
    h5_fixname(tmpname, fapl, filename2, sizeof filename2);

    /* set up name for target file: "extlinks10" */
    h5_fixname(FILENAME[29], fapl, filename3, sizeof filename3);

    /* Create the target file */
    if((fid = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((gid = H5Gcreate2(fid, "A", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* closing for target file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Create the main file */
    if((fid = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link to target file */
    if(H5Lcreate_external(filename2, "/A", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Open object through external link */
    H5E_BEGIN_TRY {
        gid = H5Gopen2(fid, "ext_link", H5P_DEFAULT);
    } H5E_END_TRY;

    /* should be able to find the target file via main file's CWD*/
    if(gid < 0) {
	H5_FAILED();
	puts("    Should have found the file in CWD.");
	goto error;
    }

    /* closing for main file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Gclose (gid);
	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_win1() */


/*-------------------------------------------------------------------------
 * Function:    external_link_win2
 *
 * Purpose:
 *   		1. target link: "/CWD/tmp/extlinks11"
 *		2. main file: "extlinks0"
 *		3. target file: "tmp/extlinks11"
 *		Should be able to access the target file directly (rel drive/abs path)
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Vailin Choi
 *              April 15, 2008
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_win2(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);	                /* Group IDs */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE],
    		filename3[NAME_BUF_SIZE],
		tmpname[NAME_BUF_SIZE],
                cwdpath[NAME_BUF_SIZE];

    if(new_format)
        TESTING("external links via target's rel drive/abs path directly(windows)(w/new group format)")
    else
        TESTING("external links via target's rel drive/abs path directly(windows)")

    /* set up name for main file: "extlinks0" */
    h5_fixname(FILENAME[12], fapl, filename1, sizeof filename1);

     /* create tmp directory and get current working directory path */
    if((HDmkdir(TMPDIR, (mode_t)0755) < 0 && errno != EEXIST) || (NULL == HDgetcwd(cwdpath, (size_t)NAME_BUF_SIZE)))
        TEST_ERROR

    /* set up name for target link: "/CWD/tmp/extlinks11" */
    HDstrcpy(tmpname, &cwdpath[2]); /* stripped the drive letter to make it relative drive but absolute path */
    HDstrcat(tmpname, "/");
    HDstrcat(tmpname, FILENAME[31]);
    h5_fixname(tmpname, fapl, filename2, sizeof filename2);

    /* set up name for target file: "tmp/extlinks11" */
    h5_fixname(FILENAME[31], fapl, filename3, sizeof filename3);

    /* Create the target file */
    if((fid = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((gid = H5Gcreate2(fid, "A", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* closing for target file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Create the main file */
    if((fid = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link to target file */
    if(H5Lcreate_external(filename2, "/A", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Open object through external link */
    H5E_BEGIN_TRY {
        gid = H5Gopen2(fid, "ext_link", H5P_DEFAULT);
    } H5E_END_TRY;

    /* should be able to find the target file directly */
    if(gid < 0) {
	H5_FAILED();
	puts("    Should have found the file in tmp.");
	goto error;
    }

    /* closing for main file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Gclose (gid);
	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_win2() */


/*-------------------------------------------------------------------------
 * Function:    external_link_win3
 *
 * Purpose:
 *   		1. target link: "<cur drive>:tmp/extlinks12"
 *		2. main file: "extlinks0"
 *		3. target file: "tmp/extlinks12"
 *		Should be able to access the target file directly (abs drive/rel path)
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Vailin Choi
 *              April i15 2008
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_win3(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);	                /* Group IDs */
    int		drive=0;
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE],
    		filename3[NAME_BUF_SIZE],
		tmpname[NAME_BUF_SIZE];

    if(new_format)
        TESTING("external links via target's abs drive/rel path directly (windows)(w/new group format)")
    else
        TESTING("external links via target's abs drive/rel path directly (windows)")

    /* set up name for main file: "extlinks0" */
    h5_fixname(FILENAME[12], fapl, filename1, sizeof filename1);

    /* create tmp directory */
    if (HDmkdir(TMPDIR, (mode_t)0755) < 0 && errno != EEXIST)
        TEST_ERROR

    /* set up name for target link: "<drive-letter>:tmp/extlinks12" */
    drive = HDgetdrive();
    sprintf(tmpname, "%c:%s", (drive+'A'-1), FILENAME[32]);
    h5_fixname(tmpname, fapl, filename2, sizeof filename2);

    /* set up name for target file: "tmp/extlinks12" */
    h5_fixname(FILENAME[32], fapl, filename3, sizeof filename3);

    /* Create the target file */
    if((fid=H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((gid=H5Gcreate2(fid, "A", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* closing for target file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Create the main file */
    if((fid=H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link to target file */
    if(H5Lcreate_external(filename2, "/A", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Open object through external link */
    H5E_BEGIN_TRY {
        gid = H5Gopen2(fid, "ext_link", H5P_DEFAULT);
    } H5E_END_TRY;

    /* should be able to find the target file directly */
    if (gid < 0) {
	H5_FAILED();
	puts("    Should have found the file in tmp.");
	goto error;
    }

    /* closing for main file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Gclose (gid);
	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_win3() */

/*-------------------------------------------------------------------------
 * Function:    external_link_win4
 *
 * Purpose:
 *   		1. target link: "<cur drive>:extlinks13"
 *		2. main file: "<cur-drive>:tmp/extlinks0"
 *		3. target file: tmp/extlinks13
 *		Should be able to access the target file via main file's abs drive/rel path
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Vailin Choi
 *              April 15, 2008
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_win4(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);	                /* Group IDs */
    int		drive=0;
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE],
    		filename3[NAME_BUF_SIZE],
		tmpname[NAME_BUF_SIZE];

    if(new_format)
        TESTING("external links via main file's abs drive/rel path (windows)(w/new group format)")
    else
        TESTING("external links via main file's abs drive/rel path (windows)")

    /* set up name for main file: "<drive-letter>:tmp/extlinks0" */
    drive = HDgetdrive();
    sprintf(tmpname, "%c:%s", (drive+'A'-1), FILENAME[13]);
    h5_fixname(tmpname, fapl, filename1, sizeof filename1);

    /* set up name for target link: "<drive-letter>:extlinks13" */
    sprintf(tmpname, "%c:%s", (drive+'A'-1), FILENAME[33]);
    h5_fixname(tmpname, fapl, filename2, sizeof filename2);

    /* set up name for target file: "tmp/extlinks13" */
    h5_fixname(FILENAME[34], fapl, filename3, sizeof filename3);

    /* Create the target file */
    if((fid=H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((gid=H5Gcreate2(fid, "A", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* closing for target file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Create the main file */
    if((fid=H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link to target file */
    if(H5Lcreate_external(filename2, "/A", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Open object through external link */
    H5E_BEGIN_TRY {
        gid = H5Gopen2(fid, "ext_link", H5P_DEFAULT);
    } H5E_END_TRY;

    /* should be able to find the target file via main file's absolute drive/relative path */
    if (gid < 0) {
	H5_FAILED();
	puts("    Should have found the file in CWD.");
	goto error;
    }

    /* closing for main file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Gclose (gid);
	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_win4() */


/*-------------------------------------------------------------------------
 * Function:    external_link_win5
 *
 * Purpose:
 *   		1. target link: "<cur drive+1>:tmp/extlinks14"
 *		2. main file: "/CWD/extlinks0"
 *		3. target file: "tmp/extlinks14"
 *		Should be able to access the target file via main file's relative drive/absolute path
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Vailin Choi
 *              April 15, 2008
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_win5(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);	                /* Group IDs */
    int		drive=0;
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE],
    		filename3[NAME_BUF_SIZE],
		tmpname[NAME_BUF_SIZE],
                cwdpath[NAME_BUF_SIZE];

    if(new_format)
        TESTING("external links via main file's rel drive/abs path (windows)(w/new group format)")
    else
        TESTING("external links via main file's rel drive/abs path (windows)")

    if(NULL == HDgetcwd(cwdpath, (size_t)NAME_BUF_SIZE))
        TEST_ERROR
    drive = HDgetdrive();

    /* set up name for main file: "/CWD/extlinks0" */
    HDstrcpy(tmpname, &cwdpath[2]); /* stripped the drive letter to make it rel drive but absolute path */
    HDstrcat(tmpname, "/");
    HDstrcat(tmpname, FILENAME[12]);
    h5_fixname(tmpname, fapl, filename1, sizeof filename1);

    /* set up name for target link: "<drive-letter+1>:tmp/extlinks14" */
    sprintf(tmpname, "%c:%s", ((drive+1)+'A'-1), FILENAME[35]);
    h5_fixname(tmpname, fapl, filename2, sizeof filename2);

    /* set up name for target file: "tmp/extlinks14" */
    h5_fixname(FILENAME[35], fapl, filename3, sizeof filename3);

    /* Create the target file */
    if((fid = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((gid = H5Gcreate2(fid, "A", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* closing for target file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Create the main file */
    if((fid = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link to target file */
    if(H5Lcreate_external(filename2, "/A", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Open object through external link */
    H5E_BEGIN_TRY {
        gid = H5Gopen2(fid, "ext_link", H5P_DEFAULT);
    } H5E_END_TRY;

    /* should be able to find the target file via main file's rel drive/abs path */
    if(gid < 0) {
	H5_FAILED();
	puts("    Should have found the file in CWD.");
	goto error;
    }

    /* closing for main file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Gclose (gid);
	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_win5() */


/*-------------------------------------------------------------------------
 * Function:    external_link_win6
 *
 * Purpose:
 *   		1. target link: "<cur drive+1>:tmp/extlinks15"
 *		2. main file: "extlinks0"
 *		3. target file: "tmp/extlinks15"
 *		Should be able to access the target file via target's current drive/rel path
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Vailin Choi
 *              April 15, 2008
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_win6(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);	                /* Group IDs */
    int		drive=0;
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE],
    		filename3[NAME_BUF_SIZE],
		tmpname[NAME_BUF_SIZE];

    if(new_format)
        TESTING("external links via target's current drive/rel path (windows)(w/new group format)")
    else
        TESTING("external links via target's current drive/rel path (windows)")

    /* create tmp directory */
    if (HDmkdir(TMPDIR, (mode_t)0755) < 0 && errno != EEXIST)
        TEST_ERROR
    drive = HDgetdrive();

    /* set up name for main file: "extlinks0" */
    h5_fixname(FILENAME[12], fapl, filename1, sizeof filename1);

    /* set up name for target link: "<drive-letter+1>:tmp/extlinks15" */
    sprintf(tmpname, "%c:%s", ((drive+1)+'A'-1), FILENAME[36]);
    h5_fixname(tmpname, fapl, filename2, sizeof filename2);

    /* set up name for target file: "tmp/extlinks15" */
    h5_fixname(FILENAME[36], fapl, filename3, sizeof filename3);

    /* Create the target file */
    if((fid=H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((gid=H5Gcreate2(fid, "A", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* closing for target file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Create the main file */
    if((fid=H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link to target file */
    if(H5Lcreate_external(filename2, "/A", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Open object through external link */
    H5E_BEGIN_TRY {
        gid = H5Gopen2(fid, "ext_link", H5P_DEFAULT);
    } H5E_END_TRY;

    /* should be able to find the target file via target file's rel path in current drive */
    if (gid < 0) {
	H5_FAILED();
	puts("    Should have found the file in tmp.");
	goto error;
    }

    /* closing for main file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Gclose (gid);
	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_win6() */


/*-------------------------------------------------------------------------
 * Function:    external_link_win7
 *
 * Purpose:
 *      1. UNC target link: "\\127.0.01\c$/tmp/extlinks10"
 *      2. main file: "extlinks0"
 *      3. target file: "extlinks15"
 *      Should be able to find the target file via main file's local host/main drive/rel path
 *
 * Return:      Success:        0
 *              Failure:        -1
 *-------------------------------------------------------------------------
 */
static int
external_link_win7(hid_t fapl, hbool_t new_format)
{
    hid_t   fid = (-1);             /* File ID */
    hid_t   gid = (-1);                 /* Group IDs */
    char    filename1[NAME_BUF_SIZE],
            filename2[NAME_BUF_SIZE],
            filename3[NAME_BUF_SIZE],
            tmpname[NAME_BUF_SIZE],
            cwdpath[NAME_BUF_SIZE];

    if(new_format)
        TESTING("external links via main file's UNC local host/main drive/rel path(windows)(w/new group format)")
    else
        TESTING("external links via main file's UNC local host/main drive/rel path(windows)")

    /* set up name for main file: "extlinks0" */
    h5_fixname(FILENAME[12], fapl, filename1, sizeof filename1);

    if(NULL == HDgetcwd(cwdpath, (size_t)NAME_BUF_SIZE))
        TEST_ERROR

    /* set up name for target link: "\\127.0.0.1\c$/tmp/extlinks10" */
    HDstrcpy(tmpname, "\\\\127.0.0.1\\c$"); /* absolute path */
    HDstrcat(tmpname, "/");
    HDstrcat(tmpname, FILENAME[30]);
    h5_fixname(tmpname, fapl, filename2, sizeof filename2);

    /* set up name for target file: "extlinks15" */
    h5_fixname(FILENAME[29], fapl, filename3, sizeof filename3);

    /* Create the target file */
    if((fid = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((gid = H5Gcreate2(fid, "A", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* closing for target file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Create the main file */
    if((fid = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link to target file */
    if(H5Lcreate_external(filename2, "/A", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Open object through external link */
    H5E_BEGIN_TRY {
        gid = H5Gopen2(fid, "ext_link", H5P_DEFAULT);
    } H5E_END_TRY;

    /* should be able to find the target file via main file's local host/main drive*/
    if(gid < 0) {
        H5_FAILED();
        puts("    Should have found the file in local host/main drive.");
        goto error;
    }

    /* closing for main file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    H5Gclose (gid);
    H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_win7() */


/*-------------------------------------------------------------------------
 * Function:    external_link_win8
 *
 * Purpose:
 *      1. Long UNC target link: "\\?\<cur drive>:\CWD\extlinks10"
 *      2. main file: "extlinks0"
 *      3. target file: "extlinks10"
 *      Should be able to access the target file directly (abs drive/abs path)
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_win8(hid_t fapl, hbool_t new_format)
{
    hid_t   fid = (-1);             /* File ID */
    hid_t   gid = (-1);                 /* Group IDs */
    int     drive=0;
    char    filename1[NAME_BUF_SIZE],
            filename2[NAME_BUF_SIZE],
            filename3[NAME_BUF_SIZE],
            tmpname[NAME_BUF_SIZE],
            cwdpath[NAME_BUF_SIZE];

    if(new_format)
        TESTING("external links via target's Long UNC abs drive/abs path directly (windows)(w/new group format)")
    else
        TESTING("external links via target's Long UNC abs drive/abs path directly (windows)")

    /* set up name for main file: "extlinks0" */
    h5_fixname(FILENAME[12], fapl, filename1, sizeof filename1);

    if(NULL == HDgetcwd(cwdpath, (size_t)NAME_BUF_SIZE))
        TEST_ERROR

    /* create tmp directory */
    if(HDmkdir(TMPDIR, (mode_t)0755) < 0 && errno != EEXIST)
        TEST_ERROR

    /* set up name for target link: "<drive-letter>:\CWD\extlinks10" */
    drive = HDgetdrive();
    sprintf(tmpname, "\\\\?\\%c:%s\\%s", (drive+'A'-1), &cwdpath[2], FILENAME[30]);
    h5_fixname(tmpname, fapl, filename2, sizeof filename2);

    /* set up name for target file: "extlinks10" */
    h5_fixname(FILENAME[30], fapl, filename3, sizeof filename3);

    /* Create the target file */
    if((fid = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((gid = H5Gcreate2(fid, "A", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* closing for target file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Create the main file */
    if((fid = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link to target file */
    if(H5Lcreate_external(filename2, "/A", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Open object through external link */
    H5E_BEGIN_TRY {
        gid = H5Gopen2(fid, "ext_link", H5P_DEFAULT);
    } H5E_END_TRY;

    /* should be able to find the target file directly */
    if(gid < 0) {
        H5_FAILED();
        puts("    Should have found the file in tmp.");
        goto error;
    }

    /* closing for main file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    H5Gclose (gid);
    H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_win8() */


/*-------------------------------------------------------------------------
 * Function:    external_link_win9
 *
 * Purpose:
 *      1. Long UNC target link: "\\?\UNC\127.0.01\c$/tmp/extlinks10"
 *      2. main file: "extlinks0"
 *      3. target file: "extlinks15"
 *      Should be able to find the target file via main file's local host/main drive/rel path
 *
 * Return:      Success:        0
 *              Failure:        -1
 *-------------------------------------------------------------------------
 */
static int
external_link_win9(hid_t fapl, hbool_t new_format)
{
    hid_t   fid = (-1);             /* File ID */
    hid_t   gid = (-1);                 /* Group IDs */
    char    filename1[NAME_BUF_SIZE],
            filename2[NAME_BUF_SIZE],
            filename3[NAME_BUF_SIZE],
        tmpname[NAME_BUF_SIZE],
                cwdpath[NAME_BUF_SIZE];

    if(new_format)
        TESTING("external links via main file's Long UNC local host/main drive/rel path(windows)(w/new group format)")
    else
        TESTING("external links via main file's Long UNC local host/main drive/rel path(windows)")

    /* set up name for main file: "extlinks0" */
    h5_fixname(FILENAME[12], fapl, filename1, sizeof filename1);

    if(NULL == HDgetcwd(cwdpath, (size_t)NAME_BUF_SIZE))
        TEST_ERROR

    /* set up name for target link: "\\?\UNC\127.0.0.1\c$/tmp/extlinks10" */
    HDstrcpy(tmpname, "\\\\?\\UNC\127.0.0.1\\c$"); /* absolute path */
    HDstrcat(tmpname, "/");
    HDstrcat(tmpname, FILENAME[30]);
    h5_fixname(tmpname, fapl, filename2, sizeof filename2);

    /* set up name for target file: "extlinks15" */
    h5_fixname(FILENAME[29], fapl, filename3, sizeof filename3);

    /* Create the target file */
    if((fid = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((gid = H5Gcreate2(fid, "A", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* closing for target file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Create the main file */
    if((fid = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link to target file */
    if(H5Lcreate_external(filename2, "/A", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Open object through external link */
    H5E_BEGIN_TRY {
        gid = H5Gopen2(fid, "ext_link", H5P_DEFAULT);
    } H5E_END_TRY;

    /* should be able to find the target file via main file's local host/main drive*/
    if(gid < 0) {
        H5_FAILED();
        puts("    Should have found the file in local host/main drive.");
        goto error;
    }

    /* closing for main file */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    H5Gclose (gid);
    H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_win9() */

#endif /* H5_HAVE_WINDOW_PATH */


/*-------------------------------------------------------------------------
 * Function:    external_link_recursive
 *
 * Purpose:     Build a file with "recursive" external link
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, August 15, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_recursive(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);	                /* Group IDs */
    char	filename1[NAME_BUF_SIZE];       /* Names of files to externally link across */

    if(new_format)
        TESTING("recursive external links (w/new group format)")
    else
        TESTING("recursive external links")

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);

    /* Create first file */
    if((fid=H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create recursive external links */
    if(H5Lcreate_external(filename1, "/recursive", fid, "recursive", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Open file */
    if((fid=H5Fopen(filename1, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* Open object through dangling file external link */
    H5E_BEGIN_TRY {
        gid = H5Gopen2(fid, "recursive", H5P_DEFAULT);
    } H5E_END_TRY;
    if (gid >= 0) {
	H5_FAILED();
	puts("    Should have failed for recursive external links.");
	goto error;
    }

    /* Close first file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Gclose (gid);
    	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_recursive() */


/*-------------------------------------------------------------------------
 * Function:    external_link_query
 *
 * Purpose:     Query file & object names for external links, as well as
 *              information from H5Gget_obj_info
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, August 15, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_query(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);	                /* Group IDs */
    const char *file_name;                      /* Name of the file the external link points to */
    const char *object_name;                    /* Name of the object the external link points to */
    H5O_info_t	oi;                             /* Object information */
    H5L_info_t  li;                             /* Link information */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE],       /* Names of files to externally link across */
                query_buf[NAME_BUF_SIZE];       /* Buffer to hold query result */

    if(new_format)
        TESTING("query aspects of external link (w/new group format)")
    else
        TESTING("query aspects of external link")

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);

    /* Create first file, with external link to object in second file */
    if((fid=H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link */
    /* (add a few extra '/'s to make certain library normalizes external link object names) */
    if(H5Lcreate_external(filename2, "///dst//", fid, "src", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Get size of buffer for external link */
    if(H5Lget_info(fid, "src", &li, H5P_DEFAULT) < 0) TEST_ERROR
    if(li.u.val_size != (1 + (HDstrlen(filename2) + 1) + (HDstrlen("/dst") + 1))) TEST_ERROR
    if (H5L_TYPE_EXTERNAL != li.type) {
	H5_FAILED();
	puts("    Unexpected link class - should have been an external link");
	goto error;
    }

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Create second file to point to */
    if((fid=H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create object to link to */
    if((gid = H5Gcreate2(fid, "dst", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Open first file */
    if((fid = H5Fopen(filename1, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* Get size of buffer for external link */
    if(H5Lget_info(fid, "src", &li, H5P_DEFAULT) < 0) TEST_ERROR
    if(li.u.val_size != (1 + (HDstrlen(filename2) + 1) + (HDstrlen("/dst") + 1))) TEST_ERROR
    if(H5L_TYPE_EXTERNAL != li.type) {
	H5_FAILED();
	puts("    Unexpected link class - should have been an external link");
	goto error;
    }

    /* Get information for external link.  It should be two strings right after each other */
    if(H5Lget_val(fid, "src", query_buf, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR

    /* Extract the file and object names from the buffer */
    if(H5Lunpack_elink_val(query_buf, li.u.val_size, NULL, &file_name, &object_name) < 0) TEST_ERROR

    /* Compare the file and object names */
    if(HDstrcmp(file_name, filename2)) TEST_ERROR
    if(HDstrcmp(object_name, "/dst")) TEST_ERROR

    /* Query information about object that external link points to */
    if(H5Oget_info_by_name(fid, "src", &oi, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5O_TYPE_GROUP != oi.type) {
	H5_FAILED();
	puts("    Unexpected object type - should have been a group");
	goto error;
    }

    /* Close first file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Make sure that passing in NULLs to H5Lunpack_elink_val works */
    if(H5Lunpack_elink_val(query_buf, li.u.val_size, NULL, NULL, NULL) < 0) TEST_ERROR

    /* Make sure that bogus cases trigger errors in H5Lunpack_elink_val */
    H5E_BEGIN_TRY {
      if(H5Lunpack_elink_val(query_buf, li.u.val_size - 1, NULL, NULL, NULL) >= 0) TEST_ERROR
    } H5E_END_TRY
    H5E_BEGIN_TRY {
      if(H5Lunpack_elink_val(query_buf, (size_t)0, NULL, NULL, NULL) >= 0) TEST_ERROR
    } H5E_END_TRY
    H5E_BEGIN_TRY {
      if(H5Lunpack_elink_val(NULL, (size_t)0, NULL, NULL, NULL) >= 0) TEST_ERROR
    } H5E_END_TRY
    H5E_BEGIN_TRY {
      if(H5Lunpack_elink_val(NULL, (size_t)1000, NULL, NULL, NULL) >= 0) TEST_ERROR
    } H5E_END_TRY

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Gclose(gid);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_query() */


/*-------------------------------------------------------------------------
 * Function:    external_link_unlink_compact
 *
 * Purpose:     Remove an external link (from a compact group)
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, January 18, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_unlink_compact(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE];       /* Names of files to externally link across */

    if(new_format)
        TESTING("unlinking external link in compact group (w/new group format)")
    else
        TESTING("unlinking external link in compact group")

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);

    /* Create first file, with external link to object in second file */
    if((fid = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link */
    if(H5Lcreate_external(filename2, "/dst", fid, "src", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Create second file to point to */
    if((fid = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create object to link to */
    if((gid = H5Gcreate2(fid, "dst", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR


/* Unlink external link */

    /* Open first file */
    if((fid = H5Fopen(filename1, H5F_ACC_RDWR, fapl)) < 0) FAIL_STACK_ERROR

    /* Unlink external link */
    if(H5Ldelete(fid, "src", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    /* Open second file */
    if((fid = H5Fopen(filename2, H5F_ACC_RDONLY, fapl)) < 0) FAIL_STACK_ERROR

    /* Open group for external link */
    if((gid = H5Gopen2(fid, "dst", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close group */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_unlink_compact() */


/*-------------------------------------------------------------------------
 * Function:    external_link_unlink_dense
 *
 * Purpose:     Remove an external link (from a dense group)
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, January 18, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_unlink_dense(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t       gcpl = (-1);                    /* Group creation property list ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE];       /* Names of files to externally link across */
    unsigned	nmsgs;		                /* Number of messages in group's header */
    unsigned    max_compact;                    /* Maximum # of links to store in group compactly */
    unsigned    min_dense;                      /* Minimum # of links to store in group "densely" */
    unsigned    u;                              /* Local index variable */

    if(new_format)
        TESTING("unlinking external link in dense group (w/new group format)")
    else
        TESTING("unlinking external link in dense group")

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);

    /* Create first file, with external link to object in second file */
    if((fid = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Open root group */
    if((gid = H5Gopen2(fid, "/", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check on root group's status */
    if(H5G_is_empty_test(gid) != TRUE) TEST_ERROR

    /* Query the group creation properties */
    if((gcpl = H5Gget_create_plist(gid)) < 0) TEST_ERROR
    if(H5Pget_link_phase_change(gcpl, &max_compact, &min_dense) < 0) TEST_ERROR

    /* Create external link */
    /* (This also covers the case of having an external link in a compact group that's converted to a dense group) */
    if(H5Lcreate_external(filename2, "/dst", gid, "src", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Check on root group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR
    if(H5G_has_links_test(gid, &nmsgs) != TRUE) TEST_ERROR
    if(nmsgs != 1) TEST_ERROR
    if(H5G_has_stab_test(gid) == TRUE) TEST_ERROR

    /* Create enough objects in the root group to change it into a "dense" group */
    for(u = 0; u < max_compact; u++) {
        sprintf(objname, "filler %u", u);
        if((gid2 = H5Gcreate2(gid, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
        if(H5Gclose(gid2) < 0) TEST_ERROR
    } /* end for */

    /* Check on root group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR
    if(H5G_has_links_test(gid, NULL) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(gid) != TRUE) TEST_ERROR

    /* Close group creation property list */
    if(H5Pclose(gcpl) < 0) TEST_ERROR

    /* Close root group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Create second file to point to */
    if((fid = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create object to link to */
    if((gid = H5Gcreate2(fid, "dst", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR


/* Unlink external link */

    /* Open first file */
    if((fid = H5Fopen(filename1, H5F_ACC_RDWR, fapl)) < 0) FAIL_STACK_ERROR

    /* Open root group */
    if((gid = H5Gopen2(fid, "/", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Unlink external link */
    if(H5Ldelete(fid, "src", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Remove enough objects in the root group to change it into a "compact" group */
    for(u = 0; u < ((max_compact - min_dense) + 1); u++) {
        sprintf(objname, "filler %u", u);
        if(H5Ldelete(gid, objname, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    } /* end for */

    /* Check on root group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR
    if(H5G_has_links_test(gid, &nmsgs) != TRUE) TEST_ERROR
    if(nmsgs != (min_dense - 1)) TEST_ERROR
    if(H5G_is_new_dense_test(gid) == TRUE) TEST_ERROR
    if(H5G_has_stab_test(gid) == TRUE) TEST_ERROR

    /* Close root group */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    /* Open second file */
    if((fid = H5Fopen(filename2, H5F_ACC_RDONLY, fapl)) < 0) FAIL_STACK_ERROR

    /* Open group for external link (should be unaffected) */
    if((gid = H5Gopen2(fid, "dst", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close group */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_unlink_dense() */


/*-------------------------------------------------------------------------
 * Function:    external_link_move
 *
 * Purpose:     Move/rename external link
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, December  5, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_move(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE];       /* Names of files to externally link across */

    if(new_format)
        TESTING("move external link (w/new group format)")
    else
        TESTING("move external link")

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);

    /* Create first file, with external link to object in second file */
    if((fid = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link */
    if(H5Lcreate_external(filename2, "/dst", fid, "src", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Create second file to point to */
    if((fid = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create object to link to */
    if((gid = H5Gcreate2(fid, "dst", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR


/* Move external link to different name within same group */

    /* Open first file */
    if((fid = H5Fopen(filename1, H5F_ACC_RDWR, fapl)) < 0) FAIL_STACK_ERROR

    /* Move external link within same group */
    if(H5Lmove(fid, "src", H5L_SAME_LOC, "src2", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Open object through external link */
    if((gid = H5Gopen2(fid, "src2", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if(H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE) < 0) FAIL_STACK_ERROR
    if(HDstrcmp(objname, "/dst")) TEST_ERROR

    /* Create object in external file */
    if((gid2 = H5Gcreate2(gid, "new_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close group in external file */
    if(H5Gclose(gid2) < 0) FAIL_STACK_ERROR

    /* Close external object */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    /* Open second file */
    if((fid = H5Fopen(filename2, H5F_ACC_RDONLY, fapl)) < 0) FAIL_STACK_ERROR

    /* Open group created through external link */
    if((gid = H5Gopen2(fid, "dst/new_group", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close group */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

/* Move external link to different group */

    /* Open first file */
    if((fid = H5Fopen(filename1, H5F_ACC_RDWR, fapl)) < 0) FAIL_STACK_ERROR

    /* Create another group, to move the external link into */
    if((gid = H5Gcreate2(fid, "group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Move external link to different group */
    if(H5Lmove(fid, "src2", gid, "src3", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close new group */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Open object through external link */
    if((gid = H5Gopen2(fid, "/group2/src3", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if(H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE) < 0) FAIL_STACK_ERROR
    if(HDstrcmp(objname, "/dst")) TEST_ERROR

    /* Create object in external file */
    if((gid2 = H5Gcreate2(gid, "new_group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close group in external file */
    if(H5Gclose(gid2) < 0) FAIL_STACK_ERROR

    /* Close external object */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    /* Open second file */
    if((fid = H5Fopen(filename2, H5F_ACC_RDONLY, fapl)) < 0) FAIL_STACK_ERROR

    /* Open group created through external link */
    if((gid = H5Gopen2(fid, "dst/new_group2", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close group */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

/* Move external link back to original group */

    /* Open first file */
    if((fid = H5Fopen(filename1, H5F_ACC_RDWR, fapl)) < 0) FAIL_STACK_ERROR

    /* Open object through external link */
    if((gid = H5Gopen2(fid, "/group2/src3", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if(H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE) < 0) FAIL_STACK_ERROR
    if(HDstrcmp(objname, "/dst")) TEST_ERROR

    /* Move external link back to original location */
    if(H5Lmove(fid, "/group2/src3", H5L_SAME_LOC, "/src", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Check name */
    if(H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE) < 0) FAIL_STACK_ERROR
    if(HDstrcmp(objname, "/dst")) TEST_ERROR

    /* Create object in external file */
    if((gid2 = H5Gcreate2(gid, "new_group3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close group in external file */
    if(H5Gclose(gid2) < 0) FAIL_STACK_ERROR

    /* Close external object */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    /* Open second file */
    if((fid = H5Fopen(filename2, H5F_ACC_RDONLY, fapl)) < 0) FAIL_STACK_ERROR

    /* Open group created through external link */
    if((gid = H5Gopen2(fid, "dst/new_group3", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close group */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_move() */


/*-------------------------------------------------------------------------
 * Function:    external_link_ride
 *
 * Purpose:     Let an external link "come along for the ride" when a group is
 *              converted between compact & dense forms.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, January 18, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_ride(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t       gcpl = (-1);                    /* Group creation property list ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE];       /* Names of files to externally link across */
    unsigned	nmsgs;		                /* Number of messages in group's header */
    unsigned    max_compact;                    /* Maximum # of links to store in group compactly */
    unsigned    min_dense;                      /* Minimum # of links to store in group "densely" */
    unsigned    u;                              /* Local index variable */

    if(new_format)
        TESTING("external link along for the ride (w/new group format)")
    else
        TESTING("external link along for the ride")

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);

    /* Create first file, with external link to object in second file */
    if((fid = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Open root group */
    if((gid = H5Gopen2(fid, "/", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check on root group's status */
    if(H5G_is_empty_test(gid) != TRUE) TEST_ERROR

    /* Query the group creation properties */
    if((gcpl = H5Gget_create_plist(gid)) < 0) TEST_ERROR
    if(H5Pget_link_phase_change(gcpl, &max_compact, &min_dense) < 0) TEST_ERROR

    /* Create enough objects in the root group to change it into a "dense" group */
    for(u = 0; u < (max_compact + 1); u++) {
        sprintf(objname, "filler %u", u);
        if((gid2 = H5Gcreate2(gid, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
        if(H5Gclose(gid2) < 0) TEST_ERROR
    } /* end for */

    /* Check on root group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR
    if(H5G_has_links_test(gid, NULL) == TRUE) TEST_ERROR
    if(new_format) {
        if(H5G_is_new_dense_test(gid) != TRUE) TEST_ERROR
    } /* end if */
    else {
        if(H5G_has_stab_test(gid) != TRUE) TEST_ERROR
    } /* end else */

    /* Create external link */
    /* (This also covers the case of adding an external link to a dense group) */
    if(H5Lcreate_external(filename2, "/dst", gid, "src", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Check on root group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR
    if(H5G_has_links_test(gid, NULL) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(gid) != TRUE) TEST_ERROR

    /* Close group creation property list */
    if(H5Pclose(gcpl) < 0) TEST_ERROR

    /* Close root group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Create second file to point to */
    if((fid = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create object to link to */
    if((gid = H5Gcreate2(fid, "dst", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

/* Remove enough objects to convert group containing external link back into compact form */

    /* Open first file */
    if((fid = H5Fopen(filename1, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR

    /* Open object through external link */
    if((gid = H5Gopen2(fid, "src", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if(H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/dst")) TEST_ERROR

    /* Create object in external file */
    if((gid2 = H5Gcreate2(gid, "new_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close group in external file */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close external object */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Open root group */
    if((gid = H5Gopen2(fid, "/", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Remove enough objects in the root group to change it into a "compact" group */
    for(u = 0; u < ((max_compact - min_dense) + 3); u++) {
        sprintf(objname, "filler %u", u);
        if(H5Ldelete(gid, objname, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    } /* end for */

    /* Check on root group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR
    if(H5G_has_links_test(gid, &nmsgs) != TRUE) TEST_ERROR
    if(nmsgs != (min_dense - 1)) TEST_ERROR
    if(H5G_has_stab_test(gid) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(gid) == TRUE) TEST_ERROR

    /* Close root group */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Open object through external link */
    if((gid = H5Gopen2(fid, "src", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if(H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/dst")) TEST_ERROR

    /* Create object in external file */
    if((gid2 = H5Gcreate2(gid, "new_group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close group in external file */
    if(H5Gclose(gid2) < 0) FAIL_STACK_ERROR

    /* Close external object */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    /* Open second file */
    if((fid = H5Fopen(filename2, H5F_ACC_RDONLY, fapl)) < 0) FAIL_STACK_ERROR

    /* Open group created through external link */
    if((gid = H5Gopen2(fid, "dst/new_group", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close group */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Open group created through external link */
    if((gid = H5Gopen2(fid, "dst/new_group2", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close group */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(gcpl);
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_ride() */


/*-------------------------------------------------------------------------
 * Function:    external_link_closing
 *
 * Purpose:     Test that files are closed correctly when traversing
 *              external links.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  James Laird
 *              Wednesday, August 16, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_closing(hid_t fapl, hbool_t new_format)
{
    hid_t       fid1 = (-1), fid2 = (-1), fid3 = (-1), fid4=(-1);
    hid_t       gid=(-1), tid=(-1), tid2=(-1), sid=(-1), did=(-1);
    hid_t       lcpl_id=(-1);
    hsize_t     dims[2];
    char	filename1[NAME_BUF_SIZE],
                filename2[NAME_BUF_SIZE],
    		filename3[NAME_BUF_SIZE],
    		filename4[NAME_BUF_SIZE],       /* Names of files to externally link across */
    		buf[NAME_BUF_SIZE];             /* misc. buffer */
    H5L_info_t  li;
    H5O_info_t  oi;
    hobj_ref_t  obj_ref;

    if(new_format)
        TESTING("that external files are closed during traversal (w/new group format)")
    else
        TESTING("that external files are closed during traversal")

    /* In this test, external links will go from file1 to file2 and from
     * file2 to file3.
     * Test that all functions that can traverse external files close
     * the files they open.
     * Test that providing unusual paths containing external links can't
     * make HDF5 forget to close a file it opened.
     */

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);
    h5_fixname(FILENAME[5], fapl, filename3, sizeof filename3);
    h5_fixname(FILENAME[6], fapl, filename4, sizeof filename4);

    /* Create four files */
    if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((fid3 = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((fid4 = H5Fcreate(filename4, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create a dataspace and a datatype so we can create/commit a dataset/datatype in the files */
    dims[0] = 2;
    dims[1] = 2;
    if((sid = H5Screate_simple(2, dims, NULL)) < 0) TEST_ERROR
    if((tid = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR
    if((tid2 = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR

    /* Create external links from each file to the next */
    if(H5Lcreate_external(filename2, "/", fid1, "elink", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename3, "/", fid2, "elink", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename4, "/", fid3, "elink", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close all files but the first */
    if(H5Fclose(fid4) < 0) TEST_ERROR
    if(H5Fclose(fid3) < 0) TEST_ERROR
    if(H5Fclose(fid2) < 0) TEST_ERROR

    /* Test creating each kind of object */
    if((gid = H5Gcreate2(fid1, "elink/elink/elink/group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Tcommit2(fid1, "elink/elink/elink/type1", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if((did = H5Dcreate2(fid1, "elink/elink/elink/dataset1", tid2, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close objects */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Tclose(tid) < 0) TEST_ERROR
    if(H5Dclose(did) < 0) TEST_ERROR

    /* Test that getting info works */
    if(H5Lget_info(fid1, "elink/elink/elink/type1", &li, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lget_info(fid1, "elink/elink/elink", &li, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Oget_info_by_name(fid1, "elink/elink/elink/type1", &oi, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Oget_info_by_name(fid1, "elink/elink/elink", &oi, H5P_DEFAULT) < 0) TEST_ERROR

    /* Test move */
    if(H5Lmove(fid1, "elink/elink/elink/group1", fid1,
        "elink/elink/elink/group1_moved", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Open file 4 so we can do some fancy things */
    if((fid4 = H5Fopen(filename4, H5F_ACC_RDWR, fapl)) < 0) FAIL_STACK_ERROR
    if(H5Lmove(fid1, "elink/elink/elink/type1", fid4,
        "type1_moved", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lmove(fid4, "dataset1", fid1,
        "elink/elink/elink/dataset1_moved", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close file 4 again */
    if(H5Fclose(fid4) < 0) FAIL_STACK_ERROR

    /* Test copy (as of this test, it uses the same code as move) */
    if(H5Lcopy(fid1, "elink/elink/elink", fid1,
        "elink/elink/elink_copied", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcopy(fid1, "elink/elink/elink", fid1,
        "elink/elink/elink/elink_copied2", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Test H5Gset and get comment */
    if(H5Oset_comment_by_name(fid1, "elink/elink/elink/group1_moved", "comment", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Oget_comment_by_name(fid1, "elink/elink/elink/group1_moved", buf, sizeof(buf), H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(HDstrcmp(buf, "comment")) TEST_ERROR

    /* Test H5*open */
    if((gid = H5Gopen2(fid1, "elink/elink/elink/group1_moved", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((tid = H5Topen2(fid1, "elink/elink/elink/type1_moved", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((did = H5Dopen2(fid1, "elink/elink/elink/dataset1_moved", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    /* Close objects */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR
    if(H5Tclose(tid) < 0) FAIL_STACK_ERROR
    if(H5Dclose(did) < 0) FAIL_STACK_ERROR

    /* Test H5*open2 */
    if((gid = H5Gopen2(fid1, "elink/elink/elink/group1_moved", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((tid = H5Topen2(fid1, "elink/elink/elink/type1_moved", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((did = H5Dopen2(fid1, "elink/elink/elink/dataset1_moved", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    /* Close objects */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR
    if(H5Tclose(tid) < 0) FAIL_STACK_ERROR
    if(H5Dclose(did) < 0) FAIL_STACK_ERROR

    /* Test H5Oopen */
    if((did = H5Oopen(fid1, "elink/elink/elink/dataset1_moved", H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Dclose(did) < 0) TEST_ERROR

    /* Test H5Fmount */
    if((gid = H5Gcreate2(fid1, "elink/elink/elink/mnt", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR
    H5E_BEGIN_TRY {
        if(H5Fmount(fid1, "elink/elink/elink/mnt", fid1, H5P_DEFAULT) >= 0) TEST_ERROR
        if(H5Funmount(fid1, "elink/elink/elink/mnt") >= 0) TEST_ERROR
    } H5E_END_TRY

    /* Test H5Rcreate */
    if(H5Rcreate(&obj_ref, fid1, "elink/elink/elink/type1_moved", H5R_OBJECT, (-1)) < 0) TEST_ERROR

    /* Test unlink */
    if(H5Ldelete(fid1, "elink/elink/elink/group1_moved", H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Ldelete(fid1, "elink/elink/elink/type1_moved", H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Ldelete(fid1, "elink/elink/elink/dataset1_moved", H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Ldelete(fid1, "elink/elink/elink_copied", H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Ldelete(fid1, "elink/elink/elink/elink_copied2", H5P_DEFAULT) < 0) TEST_ERROR

    /* We've tested that the various functions above don't leave files open.
        * Now test that we can't confuse HDF5 by giving unusual paths with external links
        */
    /* Create an external link that points to another external link */
    if((fid2 = H5Fopen(filename2, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR
    if(H5Lcreate_external(filename3, "/elink", fid2, "elink2",
            H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Fclose(fid2) < 0) TEST_ERROR

    /* Do an external link traversal that recursively calls another external link. */
    if((gid = H5Gcreate2(fid1, "elink/elink2/group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Create two more groups so that the last three elements in the path are
        * all within the same external file
        */
    if((gid = H5Gcreate2(fid1, "elink/elink2/group2/group3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR
    if((gid = H5Gcreate2(fid1, "elink/elink2/group2/group3/group4", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Oget_info_by_name(fid1, "elink/elink2/group2/group3/group4", &oi, H5P_DEFAULT) < 0) TEST_ERROR

    /* Add a few regular groups and a soft link in file2 using intermediate group creation */
    if((lcpl_id = H5Pcreate(H5P_LINK_CREATE)) < 0) TEST_ERROR
    if(H5Pset_create_intermediate_group(lcpl_id, TRUE) < 0) TEST_ERROR
    if(H5Lcreate_soft("/elink2", fid1, "elink/file2group1/file2group2/slink",
                lcpl_id, H5P_DEFAULT) < 0) TEST_ERROR

    /* Try to traverse this path.  There are three soft traversals in a row;
     * slink points to (file2)/elink2, which points to (file3)/elink, which
     * points to file 4.
     */
    if((gid = H5Gcreate2(fid1, "elink/file2group1/file2group2/slink/group3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Lget_info(fid1, "elink/file2group1/file2group2/slink/group3", &li, H5P_DEFAULT) < 0) TEST_ERROR

    /* Some simpler tests */
    if((gid = H5Gcreate2(fid1, "elink/file2group3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Lget_info(fid1, "elink/file2group3", &li, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lget_info(fid1, "elink/elink", &li, H5P_DEFAULT) < 0) TEST_ERROR


    /* Close file1, the only file that should still be open */
    if(H5Fclose(fid1) < 0) TEST_ERROR

    /* Re-create each file. If they are hanging open, these creates will fail */
    if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((fid3 = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((fid4 = H5Fcreate(filename4, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Cleanup */
    if(H5Sclose(sid) < 0) TEST_ERROR
    if(H5Tclose(tid2) < 0) TEST_ERROR
    if(H5Fclose(fid4) < 0) TEST_ERROR
    if(H5Fclose(fid3) < 0) TEST_ERROR
    if(H5Fclose(fid2) < 0) TEST_ERROR
    if(H5Fclose(fid1) < 0) TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Gclose(gid);
        H5Tclose(tid);
        H5Dclose(did);
        H5Sclose(sid);
        H5Tclose(tid2);
        H5Fclose(fid4);
        H5Fclose(fid3);
        H5Fclose(fid2);
        H5Fclose(fid1);
    } H5E_END_TRY;
    return -1;
} /* external_link_closing() */


/*-------------------------------------------------------------------------
 * Function:    ext_link_endian
 *
 * Purpose:     Check that external links work properly when they are
 *              moved from big-endian to little-endian systems and
 *              vice versa.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  James Laird
 *              Tuesday, June 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_endian(hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    hid_t       lapl_id = (-1);                 /* Prop List ID */
    char      * srcdir = getenv("srcdir");      /* The source directory */
    char        pathbuf[NAME_BUF_SIZE];         /* Path to the files */
    char        namebuf[NAME_BUF_SIZE];

    if(new_format)
        TESTING("endianness of external links (w/new group format)")
    else
        TESTING("endianness of external links")

    /*
     * Create the name of the file to open (in case we are using the --srcdir
     * option and the file is in a different directory from this test).
     */
    if (srcdir && ((HDstrlen(srcdir) + 2) < sizeof(pathbuf)) )
    {
        HDstrcpy(pathbuf, srcdir);
        HDstrcat(pathbuf, "/");
    }
    else
        HDstrcpy(pathbuf, "");

    /* Create a link access property list with the path to the srcdir */
    if((lapl_id = H5Pcreate(H5P_LINK_ACCESS)) < 0) TEST_ERROR
    if(H5Pset_elink_prefix(lapl_id, pathbuf) < 0) TEST_ERROR

    if(HDstrlen(pathbuf) + HDstrlen(LE_FILENAME) >= sizeof(namebuf)) TEST_ERROR
    HDstrcpy(namebuf, pathbuf);
    HDstrcat(namebuf, LE_FILENAME);

    /* Test LE file; try to open a group through the external link */
    if((fid = H5Fopen(namebuf, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0) TEST_ERROR
    if((gid = H5Oopen(fid, "ext_link", lapl_id)) < 0) TEST_ERROR

    /* Open a group in the external file using that group ID */
    if((gid2 = H5Gopen2(gid, "subgroup", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close the IDs */
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    if(HDstrlen(pathbuf) + HDstrlen(BE_FILENAME) >= sizeof(namebuf)) TEST_ERROR
    HDstrcpy(namebuf, pathbuf);
    HDstrcat(namebuf, BE_FILENAME);

    /* Test BE file; try to open a group through the external link */
    if((fid = H5Fopen(namebuf, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0) TEST_ERROR
    if((gid = H5Oopen(fid, "ext_link", lapl_id)) < 0) TEST_ERROR

    /* Open a group in the external file using that group ID */
    if((gid2 = H5Gopen2(gid, "subgroup", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close the IDs */
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    external_link_strong
 *
 * Purpose:     Check that external links work properly when they opened in
 *              a file with "strong" file close degree.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, March 5, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_strong(hid_t fapl, hbool_t new_format)
{
    hid_t       my_fapl;                        /* File access property list */
    hid_t       fid1 = (-1), fid2 = (-1);       /* File ID */
    hid_t       gid1 = (-1), gid2 = (-1);       /* Group IDs */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    char	filename1[NAME_BUF_SIZE],
                filename2[NAME_BUF_SIZE];

    if(new_format)
        TESTING("that external files work with strong file close degree (w/new group format)")
    else
        TESTING("that external files work with strong file close degree")

    /* Set up filenames */
    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);

    /* Copy file access property list */
    if((my_fapl = H5Pcopy(fapl)) < 0) TEST_ERROR

    /* Set strong file close degree */
    if(H5Pset_fclose_degree(my_fapl, H5F_CLOSE_STRONG) < 0) TEST_ERROR

    /* Create a group at /A/B/C in first file */
    if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0) TEST_ERROR
    if((gid1 = H5Gcreate2(fid1, "A", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid1) < 0) TEST_ERROR
    if((gid1 = H5Gcreate2(fid1, "A/B", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid1) < 0) TEST_ERROR
    if((gid1 = H5Gcreate2(fid1, "A/B/C", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid1) < 0) TEST_ERROR
    if(H5Fclose(fid1) < 0) TEST_ERROR

    /* Create an external link /W/X/DLINK in second file to <filename1>:/A/B/C */
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((gid2 = H5Gcreate2(fid2, "/W", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if((gid2 = H5Gcreate2(fid2, "/W/X", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Lcreate_external(filename1, "/A/B/C", gid2, "DLINK", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if(H5Fclose(fid2) < 0) TEST_ERROR

    /* Access external link from file #1 */
    if((fid2 = H5Fopen(filename2, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR
    if((gid2 = H5Gopen2(fid2, "/W/X/DLINK", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Iget_name(gid2, objname, (size_t)NAME_BUF_SIZE) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/A/B/C")) TEST_ERROR
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if(H5Fclose(fid2) < 0) TEST_ERROR


    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Gclose(fapl);
        H5Gclose(gid2);
        H5Gclose(gid1);
        H5Fclose(fid2);
        H5Fclose(fid1);
    } H5E_END_TRY;
    return -1;
} /* end external_link_strong() */


/*-------------------------------------------------------------------------
 * Function:    external_symlink
 *
 * Purpose:     Verify functionality of external links when symlinks are
 *              used for parent/child files
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Nov. 23, 2009
 *
 *-------------------------------------------------------------------------
 */
static int
external_symlink(const char *env_h5_drvr, hid_t fapl, hbool_t new_format)
{
#ifdef H5_HAVE_SYMLINK
    hid_t       file1 = -1, file2 = -1, file3 = -1, file4 = -1, file5 = -1;
    hid_t       group2 = -1, group3 = -1, group4 = -1, group5 = -1;
    char	filename1[NAME_BUF_SIZE],
    		filename2a[NAME_BUF_SIZE],
    		filename2b[NAME_BUF_SIZE],
    		filename3a[NAME_BUF_SIZE],
    		filename3b[NAME_BUF_SIZE],
    		filename4a[NAME_BUF_SIZE],
    		filename4b[NAME_BUF_SIZE],
    		filename5a[NAME_BUF_SIZE],
    		filename5b[NAME_BUF_SIZE],
    		tmpname[NAME_BUF_SIZE],
    		cwdpath[NAME_BUF_SIZE];
    hbool_t     have_posix_compat_vfd;   /* Whether VFD used is compatible w/POSIX I/O calls */
#endif /* H5_HAVE_SYMLINK */

    if(new_format)
        TESTING("external links w/symlink files (w/new group format)")
    else
        TESTING("external links w/symlink files")

#ifdef H5_HAVE_SYMLINK
    /* Skip test when using VFDs that can't provide a POSIX compatible file
     *  descriptor.
     */
    have_posix_compat_vfd = (hbool_t)(!HDstrcmp(env_h5_drvr, "sec2")
            || !HDstrcmp(env_h5_drvr, "core")
            || !HDstrcmp(env_h5_drvr, "nomatch"));
    if(have_posix_compat_vfd) {
        /* set up name for main file: "extlinks21A" */
        h5_fixname(FILENAME[45], fapl, filename1, sizeof(filename1));

        /* create tmp directory and get current working directory path */
        if(HDmkdir(TMPDIR, (mode_t)0755) < 0 && errno != EEXIST)
            TEST_ERROR
        if(HDmkdir(TMPDIR2, (mode_t)0755) < 0 && errno != EEXIST)
            TEST_ERROR
        if(NULL == HDgetcwd(cwdpath, (size_t)NAME_BUF_SIZE))
            TEST_ERROR

        /* Set up names for files in the subdirectories */

        /* set up names for file #2 in temporary directory #2: "tmp2/extlinks21B" */
        h5_fixname(FILENAME[46], fapl, filename2a, sizeof(filename2a));
        fix_ext_filename(tmpname, cwdpath, FILENAME[46]);
        h5_fixname(tmpname, fapl, filename2b, sizeof(filename2b));

        /* Create symbolic link #1 in temporary directory #1 to file #2 in temporary directory #2 */
        /* (i.e. tmp/sym1.h5 -> <full path to>/tmp2/extlinks21B.h5) */
        if(HDsymlink(filename2b, SYMLINK1) < 0 && errno != EEXIST) TEST_ERROR

        /* set up name for file #3 in temporary directory #2: "tmp2/extlinks21C" */
        h5_fixname(FILENAME[47], fapl, filename3a, sizeof(filename3a));
        h5_fixname(FILENAME[48], fapl, filename3b, sizeof(filename3b));

        /* set up name for file #4 in temporary directory #1: "tmp/extlinks21D" */
        h5_fixname(FILENAME[49], fapl, filename4a, sizeof(filename4a));
        fix_ext_filename(tmpname, cwdpath, FILENAME[49]);
        h5_fixname(tmpname, fapl, filename4b, sizeof(filename4b));

        /* Create symbolic link #2 in temporary directory #2 to file #4 in temporary directory #1 */
        /* (i.e. tmp2/sym2.h5 -> <full path to>/tmp/extlinks21D.h5) */
        if(HDsymlink(filename4b, SYMLINK2) < 0 && errno != EEXIST) TEST_ERROR

        /* set up name for file #5 in temporary directory #1: "tmp/extlinks21E" */
        h5_fixname(FILENAME[50], fapl, filename5a, sizeof(filename5a));
        h5_fixname(FILENAME[51], fapl, filename5b, sizeof(filename5b));


        /* Create file #1 in current directory */
        if((file1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

        /* Create external link to file & object in temporary directory #2, using symlink #1 name */
        if(H5Lcreate_external(SYMLINK1, "group2", file1, "extlink2-sym", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

        /* Close file #1 */
        if(H5Fclose(file1) < 0) TEST_ERROR


        /* Create file #2 in tmp directory #2 */
        if((file2 = H5Fcreate(filename2a, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
        if(H5Fclose(file2) < 0) TEST_ERROR

        /* Re-open file #2 in tmp directory through symlink */
        if((file2 = H5Fopen(SYMLINK1, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR

        /* Create group in file #2 in temporary directory */
        if((group2 = H5Gcreate2(file2, "group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

        /* Create external link to file #3 & object in temporary directory #2 */
        if(H5Lcreate_external(filename3b, "group3", group2, "extlink3", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

        /* Close group in file #2 */
        if(H5Gclose(group2) < 0) TEST_ERROR

        /* Close file #2 */
        if(H5Fclose(file2) < 0) TEST_ERROR


        /* Create file #3 in temp. directory #2 */
        if((file3 = H5Fcreate(filename3a, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

        /* Create group in file #3 */
        if((group3 = H5Gcreate2(file3, "group3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

        /* Create external link to file & object in temporary directory #1, using symlink #2 name */
        if(H5Lcreate_external(SYMLINK2, "group4", group3, "extlink4-sym", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

        /* Close group in file #3 */
        if(H5Gclose(group3) < 0) TEST_ERROR

        /* Close file #3 */
        if(H5Fclose(file3) < 0) TEST_ERROR


        /* Create file #4 in temporary directory #1 */
        if((file4 = H5Fcreate(filename4b, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

        /* Create group in file #4 in 'temporary' directory */
        if((group4 = H5Gcreate2(file4, "group4", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

        /* Create external link to file #5 & object in temporary directory #1 */
        if(H5Lcreate_external(filename5b, "group5", group4, "extlink5", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

        /* Close group in file #4 */
        if(H5Gclose(group4) < 0) TEST_ERROR

        /* Close file #4 */
        if(H5Fclose(file4) < 0) TEST_ERROR


        /* Create file #5 in temporary directory #1 */
        if((file5 = H5Fcreate(filename5a, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

        /* Create group in file #5 in 'temporary' directory #1 */
        if((group5 = H5Gcreate2(file5, "group5", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
        if(H5Gclose(group5) < 0) TEST_ERROR

        /* Close file #5 */
        if(H5Fclose(file5) < 0) TEST_ERROR


        /* Actual tests... */

        /* Reopen file #1 */
        if((file1 = H5Fopen(filename1, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR

        /* Open group in file #2, through external link w/symlink */
        if((group2 = H5Gopen2(file1, "extlink2-sym", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
        if(H5Gclose(group2) < 0) TEST_ERROR

        /* Open group in file #3, through external link w/symlink to external link */
        if((group3 = H5Gopen2(file1, "extlink2-sym/extlink3", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
        if(H5Gclose(group3) < 0) TEST_ERROR

        /* Open group in file #4, through external link w/symlink to external link w/symlink */
        if((group4 = H5Gopen2(file1, "extlink2-sym/extlink3/extlink4-sym", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
        if(H5Gclose(group4) < 0) TEST_ERROR

        /* Open group in file #5, through external link w/symlink to external link w/symlink to external link */
        if((group5 = H5Gopen2(file1, "extlink2-sym/extlink3/extlink4-sym/extlink5", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
        if(H5Gclose(group5) < 0) TEST_ERROR

        /* Close file #1 */
        if(H5Fclose(file1) < 0) TEST_ERROR


        PASSED();
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support POSIX I/O calls");
    } /* end else */

    return 0;

 error:
    H5E_BEGIN_TRY {
        H5Gclose(group5);
        H5Gclose(group4);
        H5Gclose(group3);
        H5Gclose(group2);
        H5Fclose(file5);
        H5Fclose(file4);
        H5Fclose(file3);
        H5Fclose(file2);
        H5Fclose(file1);
    } H5E_END_TRY;
    return -1;
#else /* H5_HAVE_SYMLINK */
    SKIPPED();
    puts("    Current file system or operating system doesn't support symbolic links");

    return 0;
#endif /* H5_HAVE_SYMLINK */
} /* end external_symlink() */


/*-------------------------------------------------------------------------
 * Function:    external_copy_invalid_object
 *
 * Purpose:     Check that attempting to copy an object through an
 *              external link to the source file but with an invalid
 *              object name fails gracefully, and does not leave lingering
 *              file ids.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Neil Fortner
 *              Wednesday, March 3, 2010
 *
 *-------------------------------------------------------------------------
 */
static int
external_copy_invalid_object(hid_t fapl, hbool_t new_format)
{
    hid_t       fid = (-1);                     /* File ID */
    hid_t       gid = (-1);                     /* Group ID */
    hid_t       ocpyplid = (-1);                /* Object copy plist ID */
    char        filename[NAME_BUF_SIZE];

    if(new_format)
        TESTING("copying invalid external links to the source file (w/new group format)")
    else
        TESTING("copying invalid external links to the source file")

    /* Set up filename */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create object copy plist, set expand external flag */
    if((ocpyplid = H5Pcreate(H5P_OBJECT_COPY)) < 0) TEST_ERROR
    if(H5Pset_copy_object(ocpyplid, H5O_COPY_EXPAND_EXT_LINK_FLAG) < 0) TEST_ERROR

    /* Create file and group */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((gid = H5Gcreate2(fid, "group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Create an external link in the group to the source file with an invalid
     * object name */
    if(H5Lcreate_external(filename, "no_object", fid, "/group1/link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Copy the group containing the external link */
    if(H5Ocopy(fid, "group1", fid, "group2", ocpyplid, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Attempt to truncate the file again.  If there is a lingering id for this
     * file this will fail */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Close */
    if(H5Fclose(fid) < 0) TEST_ERROR
    if(H5Pclose(ocpyplid) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Gclose(gid);
        H5Fclose(fid);
        H5Pclose(ocpyplid);
    } H5E_END_TRY

    return -1;
} /* end external_copy_invalid_object */


/*-------------------------------------------------------------------------
 * Function:    external_dont_fail_to_source
 *
 * Purpose:     Check that external links with invalid target file names
 *              work properly and do not attempt to (re)open the source
 *              file.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Neil Fortner
 *              Wednesday, March 3, 2010
 *
 *-------------------------------------------------------------------------
 */
static int
external_dont_fail_to_source(hid_t fapl, hbool_t new_format)
{
    hid_t       fid = (-1);                     /* File ID */
    hid_t       gid = (-1);                     /* Group ID */
    hid_t       oid = (-1);                     /* Object ID */
    char        filename[NAME_BUF_SIZE];

    if(new_format)
        TESTING("that invalid external links don't open the source file (w/new group format)")
    else
        TESTING("that invalid external links don't open the source file")

    /* Set up filename */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create file and group */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((gid = H5Gcreate2(fid, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Create an external link with an invalid file name, but the same object
     * name as the group.  This way, if the external link is interpreted to
     * refer to the source file, it will link to the group */
    if(H5Lcreate_external("no_file", "/group", fid, "link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Attempt to open the object the link points to.  This should fail */
    H5E_BEGIN_TRY {
        oid = H5Oopen(fid, "link", H5P_DEFAULT);
    } H5E_END_TRY
    if(oid >= 0) FAIL_PUTS_ERROR("Succeeded in opening target of invalid external link")

    /* Close */
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Gclose(gid);
        H5Oclose(oid);
        H5Fclose(fid);
    } H5E_END_TRY

    return -1;
} /* end external_dont_fail_to_source */


/*-------------------------------------------------------------------------
 * Function:    external_file_cache
 *
 * Purpose:     Tests that the external file cache works with external
 *              links in a few basic cases.  More complicated cases are
 *              tested by interfacing directly with the cache in efc.c.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Neil Fortner
 *              Thursday, January 13, 2011
 *
 *-------------------------------------------------------------------------
 */
static int
external_file_cache(hid_t fapl, hbool_t new_format)
{
    hid_t       my_fapl = (-1);                 /* Temporary FAPL */
    hid_t       fid1 = (-1);                    /* File ID */
    hid_t       fid2 = (-1);                    /* File ID */
    hid_t       fid3 = (-1);                    /* File ID */
    hid_t       fid4 = (-1);                    /* File ID */
    hid_t       oid = (-1);                     /* Object ID */
    unsigned    efc_size;
    char        filename1[NAME_BUF_SIZE];
    char        filename2[NAME_BUF_SIZE];
    char        filename3[NAME_BUF_SIZE];
    char        filename4[NAME_BUF_SIZE];

    if(new_format)
        TESTING("external file cache with external links (w/new group format)")
    else
        TESTING("external file cache with external links")

    /* Set up filenames */
    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);
    h5_fixname(FILENAME[2], fapl, filename3, sizeof filename3);
    h5_fixname(FILENAME[3], fapl, filename4, sizeof filename4);

    /* Verify that the default EFC size is 0 */
    if(H5Pget_elink_file_cache_size(fapl, &efc_size) < 0)
        TEST_ERROR
    if(efc_size != 0)
        FAIL_PUTS_ERROR("default external file cache size is not 0")

    /* Copy FAPL and enable external file caching */
    if((my_fapl = H5Pcopy(fapl)) < 0)
        TEST_ERROR
    if(H5Pset_elink_file_cache_size(my_fapl, 8) < 0)
        TEST_ERROR

    /* Verify that the external file cache size has been set */
    if(H5Pget_elink_file_cache_size(my_fapl, &efc_size) < 0)
        TEST_ERROR
    if(efc_size != 8)
        FAIL_PUTS_ERROR("external file cache size different from expected")


    /*
     * Test 1: One file caches another
     */
    /* Create files */
    if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0)
        TEST_ERROR
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0)
        TEST_ERROR

    /* Create link */
    if(H5Lcreate_external(filename2, "/", fid1, "link_to_2", H5P_DEFAULT,
            H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Close file 2 */
    if(H5Fclose(fid2) < 0)
        TEST_ERROR

    /* Verify that only 1 file is open */
    if(H5F_sfile_assert_num(1) < 0)
        TEST_ERROR

    /* Open and close the target of the external link */
    if((oid = H5Oopen(fid1, "link_to_2", H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Oclose(oid) < 0)
        TEST_ERROR

    /* Verify that both files are now open */
    if(H5F_sfile_assert_num(2) < 0)
        TEST_ERROR

    /* Close file 1 */
    if(H5Fclose(fid1) < 0)
        TEST_ERROR

    /* Verify that both files are now closed */
    if(H5F_sfile_assert_num(0) < 0)
        TEST_ERROR


    /*
     * Test 2: One file caches another, release parent's EFC
     */
    /* Create files */
    if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0)
        TEST_ERROR
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0)
        TEST_ERROR

    /* Create link */
    if(H5Lcreate_external(filename2, "/", fid1, "link_to_2", H5P_DEFAULT,
            H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Close file 2 */
    if(H5Fclose(fid2) < 0)
        TEST_ERROR

    /* Verify that only 1 file is open */
    if(H5F_sfile_assert_num(1) < 0)
        TEST_ERROR

    /* Open and close the target of the external link */
    if((oid = H5Oopen(fid1, "link_to_2", H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Oclose(oid) < 0)
        TEST_ERROR

    /* Verify that both files are now open */
    if(H5F_sfile_assert_num(2) < 0)
        TEST_ERROR

    /* Release file 1's EFC */
    if(H5Fclear_elink_file_cache(fid1) < 0)
        TEST_ERROR

    /* Verify that only the parent file is now open */
    if(H5F_sfile_assert_num(1) < 0)
        TEST_ERROR

    /* Close file 1 */
    if(H5Fclose(fid1) < 0)
        TEST_ERROR

    /* Verify that both files are now closed */
    if(H5F_sfile_assert_num(0) < 0)
        TEST_ERROR


    /*
     * Test 3: "Y" shaped tree
     */
    /* Create files */
    if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0)
        TEST_ERROR
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0)
        TEST_ERROR
    if((fid3 = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0)
        TEST_ERROR
    if((fid4 = H5Fcreate(filename4, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0)
        TEST_ERROR

    /* Create links */
    if(H5Lcreate_external(filename2, "/", fid1, "link_to_2", H5P_DEFAULT,
            H5P_DEFAULT) < 0)
        TEST_ERROR
    if(H5Lcreate_external(filename3, "/", fid2, "link_to_3", H5P_DEFAULT,
            H5P_DEFAULT) < 0)
        TEST_ERROR
    if(H5Lcreate_external(filename4, "/", fid2, "link_to_4", H5P_DEFAULT,
            H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Close files 2-4 */
    if(H5Fclose(fid2) < 0)
        TEST_ERROR
    if(H5Fclose(fid3) < 0)
        TEST_ERROR
    if(H5Fclose(fid4) < 0)
        TEST_ERROR

    /* Verify that only 1 file is open */
    if(H5F_sfile_assert_num(1) < 0)
        TEST_ERROR

    /* Open and close one branch of the tree */
    if((oid = H5Oopen(fid1, "link_to_2/link_to_3", H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Oclose(oid) < 0)
        TEST_ERROR

    /* Verify that files 2 and 3 are now open */
    if(H5F_sfile_assert_num(3) < 0)
        TEST_ERROR

    /* Open and close the other branch of the tree */
    if((oid = H5Oopen(fid1, "link_to_2/link_to_4", H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Oclose(oid) < 0)
        TEST_ERROR

    /* Verify that all files are now open */
    if(H5F_sfile_assert_num(4) < 0)
        TEST_ERROR

    /* Close file 1 */
    if(H5Fclose(fid1) < 0)
        TEST_ERROR

    /* Verify that all files are now closed */
    if(H5F_sfile_assert_num(0) < 0)
        TEST_ERROR


    /*
     * Test 4: "Y" shaped tree, release parent's EFC
     */
    /* Create files */
    if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0)
        TEST_ERROR
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0)
        TEST_ERROR
    if((fid3 = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0)
        TEST_ERROR
    if((fid4 = H5Fcreate(filename4, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0)
        TEST_ERROR

    /* Create links */
    if(H5Lcreate_external(filename2, "/", fid1, "link_to_2", H5P_DEFAULT,
            H5P_DEFAULT) < 0)
        TEST_ERROR
    if(H5Lcreate_external(filename3, "/", fid2, "link_to_3", H5P_DEFAULT,
            H5P_DEFAULT) < 0)
        TEST_ERROR
    if(H5Lcreate_external(filename4, "/", fid2, "link_to_4", H5P_DEFAULT,
            H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Close files 2-4 */
    if(H5Fclose(fid2) < 0)
        TEST_ERROR
    if(H5Fclose(fid3) < 0)
        TEST_ERROR
    if(H5Fclose(fid4) < 0)
        TEST_ERROR

    /* Verify that only 1 file is open */
    if(H5F_sfile_assert_num(1) < 0)
        TEST_ERROR

    /* Open and close one branch of the tree */
    if((oid = H5Oopen(fid1, "link_to_2/link_to_3", H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Oclose(oid) < 0)
        TEST_ERROR

    /* Verify that files 2 and 3 are now open */
    if(H5F_sfile_assert_num(3) < 0)
        TEST_ERROR

    /* Open and close the other branch of the tree */
    if((oid = H5Oopen(fid1, "link_to_2/link_to_4", H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Oclose(oid) < 0)
        TEST_ERROR

    /* Verify that all files are now open */
    if(H5F_sfile_assert_num(4) < 0)
        TEST_ERROR

    /* Release file 1's EFC */
    if(H5Fclear_elink_file_cache(fid1) < 0)
        TEST_ERROR

    /* Verify that only file 1 is now open */
    if(H5F_sfile_assert_num(1) < 0)
        TEST_ERROR

    /* Close file 1 */
    if(H5Fclose(fid1) < 0)
        TEST_ERROR

    /* Verify that all files are now closed */
    if(H5F_sfile_assert_num(0) < 0)
        TEST_ERROR

#ifndef H5_CANNOT_OPEN_TWICE
    /*
     * Test 5: 3 file cycle
     */
    /* Create files */
    if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0)
        TEST_ERROR
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0)
        TEST_ERROR
    if((fid3 = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0)
        TEST_ERROR

    /* Create links */
    if(H5Lcreate_external(filename2, "/", fid1, "link_to_2", H5P_DEFAULT,
            H5P_DEFAULT) < 0)
        TEST_ERROR
    if(H5Lcreate_external(filename3, "/", fid2, "link_to_3", H5P_DEFAULT,
            H5P_DEFAULT) < 0)
        TEST_ERROR
    if(H5Lcreate_external(filename1, "/", fid3, "link_to_1", H5P_DEFAULT,
            H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Close files 2-3 */
    if(H5Fclose(fid2) < 0)
        TEST_ERROR
    if(H5Fclose(fid3) < 0)
        TEST_ERROR

    /* Verify that only 1 file is open */
    if(H5F_sfile_assert_num(1) < 0)
        TEST_ERROR

    /* Open and close one complete cycle */
    if((oid = H5Oopen(fid1, "link_to_2/link_to_3/link_to_1", H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Oclose(oid) < 0)
        TEST_ERROR

    /* Verify that all files are now open */
    if(H5F_sfile_assert_num(3) < 0)
        TEST_ERROR

    /* Close file 1 */
    if(H5Fclose(fid1) < 0)
        TEST_ERROR

    /* Verify that all files are now closed */
    if(H5F_sfile_assert_num(0) < 0)
        TEST_ERROR


    /*
     * Test 6: 3 file cycle, release parent's EFC
     */
    /* Create files */
    if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0)
        TEST_ERROR
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0)
        TEST_ERROR
    if((fid3 = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0)
        TEST_ERROR

    /* Create links */
    if(H5Lcreate_external(filename2, "/", fid1, "link_to_2", H5P_DEFAULT,
            H5P_DEFAULT) < 0)
        TEST_ERROR
    if(H5Lcreate_external(filename3, "/", fid2, "link_to_3", H5P_DEFAULT,
            H5P_DEFAULT) < 0)
        TEST_ERROR
    if(H5Lcreate_external(filename1, "/", fid3, "link_to_1", H5P_DEFAULT,
            H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Close files 2-3 */
    if(H5Fclose(fid2) < 0)
        TEST_ERROR
    if(H5Fclose(fid3) < 0)
        TEST_ERROR

    /* Verify that only 1 file is open */
    if(H5F_sfile_assert_num(1) < 0)
        TEST_ERROR

    /* Open and close one complete cycle */
    if((oid = H5Oopen(fid1, "link_to_2/link_to_3/link_to_1", H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Oclose(oid) < 0)
        TEST_ERROR

    /* Verify that all files are now open */
    if(H5F_sfile_assert_num(3) < 0)
        TEST_ERROR

    /* Release file 1's EFC */
    if(H5Fclear_elink_file_cache(fid1) < 0)
        TEST_ERROR

    /* Verify that only file 1 is now open */
    if(H5F_sfile_assert_num(1) < 0)
        TEST_ERROR

    /* Close file 1 */
    if(H5Fclose(fid1) < 0)
        TEST_ERROR

    /* Verify that all files are now closed */
    if(H5F_sfile_assert_num(0) < 0)
        TEST_ERROR
#endif /* H5_CANNOT_OPEN_TWICE */

    /* Close fapl */
    H5Pclose(my_fapl);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Oclose(oid);
        H5Fclose(fid1);
        H5Fclose(fid2);
        H5Fclose(fid3);
        H5Fclose(fid4);
        H5Pclose(my_fapl);
    } H5E_END_TRY

    return -1;
} /* end external_file_cache */


/*-------------------------------------------------------------------------
 * Function:    external_open_twice
 *
 * Purpose:     Test opening the same object twice, both times through an
 *              external links.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Neil Fortner
 *              Saturday, April 30, 2011
 *
 *-------------------------------------------------------------------------
 */
static int
external_open_twice(hid_t fapl, hbool_t new_format)
{
    hid_t       fid1 = (-1);                    /* File ID */
    hid_t       fid2 = (-1);                    /* File ID */
    hid_t       oid1 = (-1);                    /* Object ID */
    hid_t       oid2 = (-1);                    /* Object ID */
    hid_t       type = (-1);                    /* Datatype ID */
    hid_t       space = (-1);                   /* Dataspace ID */
    char        filename1[NAME_BUF_SIZE];
    char        filename2[NAME_BUF_SIZE];

    if(new_format)
        TESTING("opening object twice through elink (w/new group format)")
    else
        TESTING("opening object twice through elink")

    /* Set up filenames */
    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);


    /*
     * Test 1: Open root group twice
     */
    /* Create files */
    if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Create link */
    if(H5Lcreate_external(filename2, "/", fid1, "link_to_2", H5P_DEFAULT,
            H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Close file 2 */
    if(H5Fclose(fid2) < 0)
        TEST_ERROR

    /* Open the target of the external link twice */
    if((oid1 = H5Oopen(fid1, "link_to_2", H5P_DEFAULT)) < 0)
        TEST_ERROR
    if((oid2 = H5Oopen(fid1, "link_to_2", H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Close both objects, in the reverse opening order (necessary to duplicate
     * bug */
    if(H5Oclose(oid2) < 0)
        TEST_ERROR
    if(H5Oclose(oid1) < 0)
        TEST_ERROR

    /* Close file 1 */
    if(H5Fclose(fid1) < 0)
        TEST_ERROR

    /* Verify that both files are now closed */
    if(H5F_sfile_assert_num(0) < 0)
        TEST_ERROR


    /*
     * Test 2: Open group twice
     */
    /* Create files */
    if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Create target group */
    if((oid1 = H5Gcreate2(fid2, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT))
            < 0)
        TEST_ERROR
    if(H5Gclose(oid1) < 0)
        TEST_ERROR

    /* Create link */
    if(H5Lcreate_external(filename2, "/group", fid1, "link_to_2", H5P_DEFAULT,
            H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Close file 2 */
    if(H5Fclose(fid2) < 0)
        TEST_ERROR

    /* Open the target of the external link twice */
    if((oid1 = H5Oopen(fid1, "link_to_2", H5P_DEFAULT)) < 0)
        TEST_ERROR
    if((oid2 = H5Oopen(fid1, "link_to_2", H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Close both objects, in the reverse opening order (necessary to duplicate
     * bug */
    if(H5Oclose(oid2) < 0)
        TEST_ERROR
    if(H5Oclose(oid1) < 0)
        TEST_ERROR

    /* Close file 1 */
    if(H5Fclose(fid1) < 0)
        TEST_ERROR

    /* Verify that both files are now closed */
    if(H5F_sfile_assert_num(0) < 0)
        TEST_ERROR


    /*
     * Test 3: Open dataset twice
     */
    /* Create files */
    if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Create target dataset */
    if((space = H5Screate(H5S_SCALAR)) < 0)
        TEST_ERROR
    if((oid1 = H5Dcreate2(fid2, "dset", H5T_NATIVE_INT, space, H5P_DEFAULT,
            H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Dclose(oid1) < 0)
        TEST_ERROR
    if(H5Sclose(space) < 0)
        TEST_ERROR

    /* Create link */
    if(H5Lcreate_external(filename2, "/dset", fid1, "link_to_2", H5P_DEFAULT,
            H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Close file 2 */
    if(H5Fclose(fid2) < 0)
        TEST_ERROR

    /* Open the target of the external link twice */
    if((oid1 = H5Oopen(fid1, "link_to_2", H5P_DEFAULT)) < 0)
        TEST_ERROR
    if((oid2 = H5Oopen(fid1, "link_to_2", H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Close both objects, in the reverse opening order (necessary to duplicate
     * bug */
    if(H5Oclose(oid2) < 0)
        TEST_ERROR
    if(H5Oclose(oid1) < 0)
        TEST_ERROR

    /* Close file 1 */
    if(H5Fclose(fid1) < 0)
        TEST_ERROR

    /* Verify that both files are now closed */
    if(H5F_sfile_assert_num(0) < 0)
        TEST_ERROR


    /*
     * Test 4: Open datatype twice
     */
    /* Create files */
    if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Create target datatype */
    if((type = H5Tcopy(H5T_NATIVE_INT)) < 0)
        TEST_ERROR
    if(H5Tcommit2(fid2, "dtype", type, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)
            < 0)
        TEST_ERROR
    if(H5Tclose(type) < 0)
        TEST_ERROR

    /* Create link */
    if(H5Lcreate_external(filename2, "/dtype", fid1, "link_to_2", H5P_DEFAULT,
            H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Close file 2 */
    if(H5Fclose(fid2) < 0)
        TEST_ERROR

    /* Open the target of the external link twice */
    if((oid1 = H5Oopen(fid1, "link_to_2", H5P_DEFAULT)) < 0)
        TEST_ERROR
    if((oid2 = H5Oopen(fid1, "link_to_2", H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Close both objects, in the reverse opening order (necessary to duplicate
     * bug */
    if(H5Oclose(oid2) < 0)
        TEST_ERROR
    if(H5Oclose(oid1) < 0)
        TEST_ERROR

    /* Close file 1 */
    if(H5Fclose(fid1) < 0)
        TEST_ERROR

    /* Verify that both files are now closed */
    if(H5F_sfile_assert_num(0) < 0)
        TEST_ERROR


    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Oclose(oid1);
        H5Oclose(oid2);
        H5Tclose(type);
        H5Fclose(fid1);
        H5Fclose(fid2);
        H5Sclose(space);
    } H5E_END_TRY

    return -1;
} /* end efc_open_twice */


/*-------------------------------------------------------------------------
 * Function:    ud_hard_links
 *
 * Purpose:     Check that the functionality of hard links can be duplicated
 *              with user-defined links.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  James Laird
 *              Tuesday, June 6, 2006
 *
 *-------------------------------------------------------------------------
 */
/* Callback functions for UD hard links. */
/* UD_hard_create increments the object's reference count */
static herr_t
UD_hard_create(const char UNUSED * link_name, hid_t loc_group, const void *udata,
    size_t udata_size, hid_t UNUSED lcpl_id)
{
    haddr_t addr;
    hid_t target_obj = -1;
    herr_t ret_value = 0;

    if(udata_size != sizeof(haddr_t)) {
        ret_value = -1;
        goto done;
    } /* end if */

    addr = *((const haddr_t *)udata);

    /* Open the object this link points to */
    target_obj= H5Oopen_by_addr(loc_group, addr);
    if(target_obj < 0) {
        ret_value = -1;
        goto done;
    } /* end if */

    /* Increment the reference count of the target object */
    if(H5Oincr_refcount(target_obj) < 0) {
        ret_value = -1;
        goto done;
    } /* end if */

done:
    /* Close the target object if we opened it */
    if(target_obj >= 0) {
        switch(H5Iget_type(target_obj)) {
            case H5I_GROUP:
                if(H5Gclose(target_obj) < 0)
                    ret_value = -1;
                break;

            case H5I_DATASET:
                if(H5Dclose(target_obj) < 0)
                    ret_value = -1;
                break;

            case H5I_DATATYPE:
                if(H5Tclose(target_obj) < 0)
                    ret_value = -1;
                break;

            case H5I_UNINIT:
            case H5I_BADID:
            case H5I_FILE:
            case H5I_DATASPACE:
            case H5I_ATTR:
            case H5I_REFERENCE:
            case H5I_VFL:
            case H5I_GENPROP_CLS:
            case H5I_GENPROP_LST:
            case H5I_ERROR_CLASS:
            case H5I_ERROR_MSG:
            case H5I_ERROR_STACK:
            case H5I_NTYPES:
            default:
              return -1;
        } /* end switch */
    } /* end if */

    return ret_value;
} /* end UD_hard_create() */

/* Traverse a hard link by opening the object */
static hid_t
UD_hard_traverse(const char UNUSED *link_name, hid_t cur_group,
    const void *udata, size_t udata_size, hid_t UNUSED lapl_id)
{
    haddr_t addr;
    hid_t ret_value = -1;

    if(udata_size != sizeof(haddr_t))
        return -1;

    addr = *((const haddr_t *) udata);

    ret_value = H5Oopen_by_addr(cur_group, addr); /* If this fails, our return value will be negative. */

    return ret_value;
} /* end UD_hard_traverse() */

/* UD_hard_delete decrements the object's reference count */
static herr_t
UD_hard_delete(const char UNUSED * link_name, hid_t file, const void *udata,
    size_t udata_size)
{
    haddr_t addr;
    hid_t target_obj = -1;
    herr_t ret_value = 0;

    if(udata_size != sizeof(haddr_t)) {
        ret_value = -1;
        goto done;
    } /* end if */

    addr = *((const haddr_t *) udata);

    /* Open the object this link points to */
    target_obj= H5Oopen_by_addr(file, addr);
    if(target_obj < 0) {
        ret_value = -1;
        goto done;
    } /* end if */

    /* Decrement the reference count of the target object */
    if(H5Odecr_refcount(target_obj) < 0) {
        ret_value = -1;
        goto done;
    } /* end if */

done:
    /* Close the target object if we opened it */
    if(target_obj >= 0) {
        switch(H5Iget_type(target_obj)) {
            case H5I_GROUP:
                if(H5Gclose(target_obj) < 0)
                    ret_value = -1;
                break;

            case H5I_DATASET:
                if(H5Dclose(target_obj) < 0)
                    ret_value = -1;
                break;

            case H5I_DATATYPE:
                if(H5Tclose(target_obj) < 0)
                    ret_value = -1;
                break;

            case H5I_UNINIT:
            case H5I_BADID:
            case H5I_FILE:
            case H5I_DATASPACE:
            case H5I_ATTR:
            case H5I_REFERENCE:
            case H5I_VFL:
            case H5I_GENPROP_CLS:
            case H5I_GENPROP_LST:
            case H5I_ERROR_CLASS:
            case H5I_ERROR_MSG:
            case H5I_ERROR_STACK:
            case H5I_NTYPES:
            default:
                return -1;
        } /* end switch */
    } /* end if */

    return ret_value;
} /* end UD_hard_delete() */

const H5L_class_t UD_hard_class[1] = {{
    H5L_LINK_CLASS_T_VERS,      /* H5L_class_t version       */
    (H5L_type_t)UD_HARD_TYPE,   /* Link type id number            */
    "UD_hard_link",             /* Link class name for debugging  */
    UD_hard_create,             /* Creation callback              */
    NULL,                       /* Move/rename callback           */
    NULL,                       /* Copy callback                  */
    UD_hard_traverse,           /* The actual traversal function  */
    UD_hard_delete,             /* Deletion callback              */
    NULL                        /* Query callback                 */
}};

static int
ud_hard_links(hid_t fapl)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    H5L_info_t li;                          /* Link information */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    h5_stat_size_t empty_size;                  /* Size of an empty file */
    char	filename[NAME_BUF_SIZE];

    TESTING("user-defined hard link (w/new group format)")

    /* Set up filename and create file*/
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Get the size of the empty file for reference */
    if(H5Fclose(fid) < 0) TEST_ERROR
    if((empty_size = h5_get_file_size(filename, fapl))<0) TEST_ERROR

    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Check that external links are registered and UD hard links are not */
    if(H5Lis_registered(H5L_TYPE_EXTERNAL) != TRUE) TEST_ERROR
    if(H5Lis_registered((H5L_type_t)UD_HARD_TYPE) != FALSE) TEST_ERROR

    /* Register "user-defined hard links" with the library */
    if(H5Lregister(UD_hard_class) < 0) TEST_ERROR

    /* Check that UD hard links are now registered */
    if(H5Lis_registered(H5L_TYPE_EXTERNAL) != TRUE) TEST_ERROR
    if(H5Lis_registered((H5L_type_t)UD_HARD_TYPE) != TRUE) TEST_ERROR

    /* Create a group for the UD hard link to point to */
    if((gid = H5Gcreate2(fid, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Get address for the group to give to the hard link */
    if(H5Lget_info(fid, "group", &li, H5P_DEFAULT) < 0) TEST_ERROR

    if(H5Gclose(gid) < 0) TEST_ERROR


    /* Create a user-defined "hard link" to the group using the address we got
     * from H5Lget_info */
    if(H5Lcreate_ud(fid, "ud_link", (H5L_type_t)UD_HARD_TYPE, &(li.u.address), (size_t)sizeof(haddr_t), H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close and re-open file to ensure that data is written to disk */
    if(H5Fclose(fid) < 0) TEST_ERROR
    if((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR

    /* Open group through UD link */
    if((gid = H5Gopen2(fid, "ud_link", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if(H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/group")) TEST_ERROR

    /* Create object in group */
    if((gid2 = H5Gcreate2(gid, "new_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close groups*/
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Re-open group without using ud link to check that it was created properly */
    if((gid = H5Gopen2(fid, "group/new_group", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if(H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/group/new_group")) TEST_ERROR

    /* Close opened object */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Check that H5Lget_objinfo works on the hard link */
    if(H5Lget_info(fid, "ud_link", &li, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    /* UD hard links have no query function, thus return a "link length" of 0 */
    if(li.u.val_size != 0) TEST_ERROR
    if(UD_HARD_TYPE != li.type) {
	H5_FAILED();
	puts("    Unexpected link class - should have been a UD hard link");
	goto error;
    } /* end if */

    /* Unlink the group pointed to by the UD link.  It shouldn't be
     * deleted because of the UD link. */
    if(H5Ldelete(fid, "/group", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Ensure we can open the group through the UD link */
    if((gid = H5Gopen2(fid, "ud_link", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Unlink the group contained within it. */
    if(H5Ldelete(gid, "new_group", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Now delete the UD link.  This should cause the group to be
     * deleted, too. */
    if(H5Ldelete(fid, "ud_link", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    /* The file should be empty again. */
    if(empty_size != h5_get_file_size(filename, fapl)) TEST_ERROR

    if(H5Lunregister((H5L_type_t)UD_HARD_TYPE) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end ud_hard_links() */


/*-------------------------------------------------------------------------
 * Function:    UD_rereg_traverse
 *
 * Purpose:     Check that user defined link types can be unregistered and
 *              reregistered properly.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  James Laird
 *              Tuesday, June 6, 2006
 *
 *-------------------------------------------------------------------------
 */
 /* A traversal function that ignores any udata and simply opens an object
 * in the current group named REREG_TARGET_NAME
 */
static hid_t
UD_rereg_traverse(const char UNUSED * link_name, hid_t cur_group,
    const void UNUSED *udata, size_t UNUSED udata_size, hid_t lapl_id)
{
    hid_t ret_value;

    if((ret_value = H5Oopen(cur_group, REREG_TARGET_NAME, lapl_id)) < 0) TEST_ERROR

    return ret_value;

error:
    return -1;
} /* end UD_rereg_traverse() */

/* This link class has the same ID number as the UD hard links but
 * has a very different traversal function */
const H5L_class_t UD_rereg_class[1] = {{
    H5L_LINK_CLASS_T_VERS,      /* H5L_class_t version       */
    (H5L_type_t)UD_HARD_TYPE,   /* Link type id number            */
    "UD_reregistered_type",     /* Link class name for debugging  */
    NULL,                       /* Creation callback              */
    NULL,                       /* Move/rename callback           */
    NULL,                       /* Copy callback                  */
    UD_rereg_traverse,          /* The actual traversal function  */
    NULL,                       /* Deletion callback              */
    NULL                        /* Query callback                 */
}};

static int
ud_link_reregister(hid_t fapl)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    H5L_info_t	li;                     /* Link information */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    char	filename[NAME_BUF_SIZE];
    h5_stat_size_t empty_size;                  /* Size of an empty file */

    TESTING("registering a new class for existing UD links (w/new group format)")

    /* Set up filename and create file*/
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Get the size of the empty file for reference */
    if(H5Fclose(fid) < 0) TEST_ERROR
    if((empty_size=h5_get_file_size(filename, fapl))<0) TEST_ERROR

    if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Check that UD hard links are not registered */
    if(H5Lis_registered((H5L_type_t)UD_HARD_TYPE) != FALSE) TEST_ERROR

    /* Register "user-defined hard links" with the library */
    if(H5Lregister(UD_hard_class) < 0) TEST_ERROR

    /* Check that UD hard links are registered */
    if(H5Lis_registered((H5L_type_t)UD_HARD_TYPE) != TRUE) TEST_ERROR

    /* Point a UD defined hard link to a group in the same way as the previous test */
    if((gid = H5Gcreate2(fid, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if (H5Lget_info(fid, "group", &li, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    if(H5Lcreate_ud(fid, "ud_link", (H5L_type_t)UD_HARD_TYPE, &(li.u.address),
                    sizeof(li.u.address), H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Create a group named REREG_TARGET_NAME in the same group as the ud link */
    if((gid = H5Gcreate2(fid, REREG_TARGET_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Now unregister UD hard links */
    if(H5Lunregister((H5L_type_t)UD_HARD_TYPE) < 0) TEST_ERROR

    /* Check that UD hard links are no longer registered */
    if(H5Lis_registered((H5L_type_t)UD_HARD_TYPE) != FALSE) TEST_ERROR

    /* Verify that we can't traverse the ud link anymore */
    H5E_BEGIN_TRY {
        if((gid = H5Gopen2(fid, "ud_link", H5P_DEFAULT)) >= 0) TEST_ERROR
    } H5E_END_TRY

    /* Verify that we can't create any new links of this type */
    H5E_BEGIN_TRY {
      if(H5Lcreate_ud(fid, "ud_link2", (H5L_type_t)UD_HARD_TYPE, &(li.u.address),
                      sizeof(li.u.address), H5P_DEFAULT, H5P_DEFAULT) >= 0)
          TEST_ERROR
    } H5E_END_TRY

    /* Register a new kind of link with the same ID number */
    if(H5Lregister(UD_rereg_class) < 0) TEST_ERROR

    /* Check that UD hard links are registered again */
    if(H5Lis_registered((H5L_type_t)UD_HARD_TYPE) != TRUE) TEST_ERROR

    /* Open a group through the ud link (now a different class of link).
     * It should be a different group
     * than the UD hard link pointed to */
    if((gid = H5Gopen2(fid, "ud_link", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if(H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/" REREG_TARGET_NAME)) TEST_ERROR

    /* Create object in group */
    if((gid2 = H5Gcreate2(gid, "new_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close groups*/
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Re-open group without using ud link to check that it was created properly */
    if((gid = H5Gopen2(fid, "rereg_target/new_group", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if(H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/rereg_target/new_group")) TEST_ERROR

    /* Close opened object */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Unlink the group pointed to by the UD hard link.  It shouldn't be
     * deleted because the UD link incremented its reference count. */
    if(H5Ldelete(fid, "/group", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* What a mess! Re-register user-defined links to clean up the
     * reference counts.  We shouldn't actually need to unregister the
     * other link type */
    if(H5Lregister(UD_hard_class) < 0) FAIL_STACK_ERROR
    if(H5Lis_registered((H5L_type_t)UD_HARD_TYPE) != TRUE) FAIL_STACK_ERROR

    /* Ensure we can open the group through the UD link (now that UD hard
     * links have been registered) */
    if((gid = H5Gopen2(fid, "ud_link", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Delete the UD hard link.  This should cause the group to be
     * deleted, too. */
    if(H5Ldelete(fid, "ud_link", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Unlink the other two groups so that we can make sure the file is empty */
    if(H5Ldelete(fid, "/rereg_target/new_group", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, REREG_TARGET_NAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    /* The file should be empty again. */
    if(empty_size != h5_get_file_size(filename, fapl)) TEST_ERROR

    if(H5Lunregister((H5L_type_t)UD_HARD_TYPE) < 0) FAIL_STACK_ERROR
    if(H5Lis_registered((H5L_type_t)UD_HARD_TYPE) != FALSE) FAIL_STACK_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end ud_link_reregister() */


/*-------------------------------------------------------------------------
 * Function:    ud_callbacks
 *
 * Purpose:     Check that all callbacks are called and are given the correct
 *              information.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  James Laird
 *              Tuesday, June 6, 2006
 *
 *-------------------------------------------------------------------------
 */
/* Callback functions for UD "callback" links. */
/* Creation callback.  Called during move as well. */
static herr_t
UD_cb_create(const char * link_name, hid_t loc_group, const void *udata,
    size_t udata_size, hid_t lcpl_id)
{
    if(!link_name) TEST_ERROR
    if(loc_group < 0) TEST_ERROR
    if(udata_size > 0 && !udata) TEST_ERROR
    if(lcpl_id < 0) TEST_ERROR

    if(HDstrcmp(link_name, UD_CB_LINK_NAME) && strcmp(link_name, NEW_UD_CB_LINK_NAME)) TEST_ERROR
    if(HDstrcmp((const char *)udata, UD_CB_TARGET)) TEST_ERROR
    if(udata_size != UD_CB_TARGET_LEN) TEST_ERROR

    return 0;

error:
    return -1;
} /* end UD_cb_create() */

static hid_t
UD_cb_traverse(const char * link_name, hid_t cur_group, const void *udata,
    size_t udata_size, hid_t lapl_id)
{
    const char *target = (const char *)udata;
    hid_t ret_value;

    if(!link_name) TEST_ERROR
    if(cur_group < 0) TEST_ERROR
    if(udata_size > 0 && !udata) TEST_ERROR

    if(HDstrcmp(link_name, UD_CB_LINK_NAME) && strcmp(link_name, NEW_UD_CB_LINK_NAME)) TEST_ERROR
    if(HDstrcmp((const char *)udata, UD_CB_TARGET)) TEST_ERROR
    if(udata_size != UD_CB_TARGET_LEN) TEST_ERROR

    if((ret_value = H5Oopen(cur_group, target, lapl_id)) < 0)
        TEST_ERROR

    return ret_value;

error:
    return -1;
} /* end UD_cb_traverse() */

/* Callback for when the link is moved or renamed */
static herr_t
UD_cb_move(const char *new_name, hid_t new_loc, const void *udata,
    size_t udata_size)
{
    if(!new_name) TEST_ERROR
    if(new_loc < 0) TEST_ERROR
    if(udata_size > 0 && !udata) TEST_ERROR

    if(HDstrcmp(new_name, NEW_UD_CB_LINK_NAME)) TEST_ERROR
    if(HDstrcmp((const char *)udata, UD_CB_TARGET)) TEST_ERROR
    if(udata_size != UD_CB_TARGET_LEN) TEST_ERROR

    return 0;

error:
    return -1;
} /* end UD_cb_move() */

/* Callback for when the link is deleted.  Also called during move */
static herr_t
UD_cb_delete(const char *link_name, hid_t file, const void *udata,
    size_t udata_size)
{
    if(!link_name) TEST_ERROR
    if(file < 0) TEST_ERROR
    if(udata_size > 0 && !udata) TEST_ERROR

    if(HDstrcmp(link_name, UD_CB_LINK_NAME) && HDstrcmp(link_name, NEW_UD_CB_LINK_NAME)) TEST_ERROR
    if(HDstrcmp((const char *)udata, UD_CB_TARGET)) TEST_ERROR
    if(udata_size != UD_CB_TARGET_LEN) TEST_ERROR

    return 0;

error:
    return -1;
} /* end UD_cb_delete() */

/* Callback for when the link is queried */
static ssize_t
UD_cb_query(const char * link_name, const void *udata, size_t udata_size,
    void *buf, size_t buf_size)
{
    if(!link_name) TEST_ERROR
    if(udata_size > 0 && !udata) TEST_ERROR

    if(HDstrcmp(link_name, UD_CB_LINK_NAME)) TEST_ERROR
    if(HDstrcmp((const char *)udata, UD_CB_TARGET)) TEST_ERROR
    if(udata_size != UD_CB_TARGET_LEN) TEST_ERROR

    if(buf) {
      if(buf_size < 16) TEST_ERROR
      HDstrcpy((char *)buf, "query succeeded");
    } /* end if */

    /* There are 15 characters and a NULL in "query succeeded" */
    return 16;

error:
    return -1;
} /* end UD_cb_query() */

const H5L_class_t UD_cb_class[1] = {{
    H5L_LINK_CLASS_T_VERS,    /* H5L_class_t version       */
    (H5L_type_t)UD_CB_TYPE,   /* Link type id number            */
    NULL,                     /* NULL name (to make sure this doesn't break anything */
    UD_cb_create,             /* Creation callback              */
    UD_cb_move,               /* Move/rename callback           */
    UD_cb_move,               /* Copy callback                  */
    UD_cb_traverse,           /* The actual traversal function  */
    UD_cb_delete,             /* Deletion callback              */
    UD_cb_query               /* Query callback                 */
}};

static int
ud_callbacks(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);	                /* Group ID */
    hid_t       lcpl = (-1);                    /* Link Creation PL */
    H5L_info_t  li;                             /* Link information */
    char        ud_target_name[] = UD_CB_TARGET; /* Link target name */
    char	filename[NAME_BUF_SIZE];
    char        query_buf[NAME_BUF_SIZE];

    if(new_format)
        TESTING("user-defined link callbacks (w/new group format)")
    else
        TESTING("user-defined link callbacks")

    /* Set up filename and create file*/
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Check that registered link classes are, and unregistered ones aren't */
    if(H5Lis_registered(H5L_TYPE_EXTERNAL) != TRUE) TEST_ERROR
    if(H5Lis_registered((H5L_type_t)UD_HARD_TYPE) != FALSE) TEST_ERROR
    if(H5Lis_registered((H5L_type_t)UD_CB_TYPE) != FALSE) TEST_ERROR

    /* Hit two birds with one stone: register UD hard links from previous
     * test to check that having two UD links registered at once presents
     * no problems. */
    if(H5Lregister(UD_hard_class) < 0) TEST_ERROR

    /* Register user-defined link class.  This is the one we'll actually
     * be using. */
    if(H5Lregister(UD_cb_class) < 0) TEST_ERROR

    /* Check that registered link classes are, and unregistered ones aren't */
    if(H5Lis_registered(H5L_TYPE_EXTERNAL) != TRUE) TEST_ERROR
    if(H5Lis_registered((H5L_type_t)UD_HARD_TYPE) != TRUE) TEST_ERROR
    if(H5Lis_registered((H5L_type_t)UD_CB_TYPE) != TRUE) TEST_ERROR

    /* Create a group for the UD link to point to */
    if((gid = H5Gcreate2(fid, UD_CB_TARGET, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Create a user-defined link to the group.  These UD links behave like soft links. */
    if(H5Lcreate_ud(fid, UD_CB_LINK_NAME, (H5L_type_t)UD_CB_TYPE, ud_target_name, (size_t)UD_CB_TARGET_LEN, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Try opening group through UD link */
    if((gid = H5Gopen2(fid, UD_CB_LINK_NAME, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Query the link to test its query callback */
    if (H5Lget_info(fid, UD_CB_LINK_NAME, &li, H5P_DEFAULT) < 0) TEST_ERROR
    if(li.u.val_size != 16) TEST_ERROR
    if (UD_CB_TYPE != li.type) {
	H5_FAILED();
	puts("    Unexpected link class - should have been a UD hard link");
	goto error;
    }

    /* Fill the query buffer */
    if(H5Lget_val(fid, UD_CB_LINK_NAME, query_buf, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(HDstrcmp(query_buf, "query succeeded") != 0) TEST_ERROR

    /* Move the link */
    if(H5Lmove(fid, UD_CB_LINK_NAME, H5L_SAME_LOC, NEW_UD_CB_LINK_NAME, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Re-open group to ensure that move worked */
    if((gid = H5Gopen2(fid, NEW_UD_CB_LINK_NAME, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Remove UD link */
    if(H5Ldelete(fid, NEW_UD_CB_LINK_NAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR


    /* Test that the callbacks don't work if the link class is not registered */

    /* Create a new link. Just for fun, give it a non-default character
     * encoding (to test that LAPLs work) */
    if((lcpl = H5Pcreate(H5P_LINK_CREATE)) < 0) FAIL_STACK_ERROR
    if(H5Pset_char_encoding(lcpl, H5T_CSET_UTF8) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_ud(fid, UD_CB_LINK_NAME, (H5L_type_t)UD_CB_TYPE, ud_target_name, (size_t)UD_CB_TARGET_LEN, lcpl, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Pclose(lcpl) < 0) FAIL_STACK_ERROR

    /* Check its character encoding */
    if(H5Lget_info(fid, UD_CB_LINK_NAME, &li, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(li.cset != H5T_CSET_UTF8) TEST_ERROR

    /* Unregister the link class so the library forgets what its callbacks do */
    if(H5Lunregister((H5L_type_t)UD_CB_TYPE) < 0) FAIL_STACK_ERROR

    /* Now test that each of the callbacks fails */
    H5E_BEGIN_TRY {
        if(H5Lcreate_ud(fid, NEW_UD_CB_LINK_NAME, (H5L_type_t)UD_CB_TYPE, ud_target_name, (size_t)UD_CB_TARGET_LEN, H5P_DEFAULT, H5P_DEFAULT) >= 0) FAIL_STACK_ERROR
        if(H5Lmove(fid, UD_CB_LINK_NAME, H5L_SAME_LOC, NEW_UD_CB_LINK_NAME, H5P_DEFAULT, H5P_DEFAULT) >= 0) FAIL_STACK_ERROR
        if(H5Ldelete(fid, UD_CB_LINK_NAME, H5P_DEFAULT) >= 0) FAIL_STACK_ERROR
        if((gid = H5Gopen2(gid, UD_CB_LINK_NAME, H5P_DEFAULT)) >= 0) FAIL_STACK_ERROR
        if(H5Ldelete(fid, UD_CB_LINK_NAME, H5P_DEFAULT) >= 0) FAIL_STACK_ERROR
    } H5E_END_TRY

    /* The query callback should NOT fail, but should be unable to give a linklen */
    if(H5Lget_info(fid, UD_CB_LINK_NAME, &li, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(li.u.val_size != 0) TEST_ERROR
    if(li.type != UD_CB_TYPE) TEST_ERROR

    /* Unregister the UD hard links */
    if(H5Lunregister((H5L_type_t)UD_HARD_TYPE) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Pclose (lcpl);
    	H5Gclose (gid);
    	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end ud_callbacks() */


/*-------------------------------------------------------------------------
 * Function:    lapl_udata
 *
 * Purpose:     Check that information can be passed to UD links using the
 *              Link Access Property List.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  James Laird
 *              Tuesday, June 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static hid_t
UD_plist_traverse(const char UNUSED * link_name, hid_t cur_group,
    const void UNUSED *udata, size_t udata_size, hid_t lapl_id)
{
    char target[NAME_BUF_SIZE];
    hid_t ret_value;

    if(udata_size != 0) TEST_ERROR

    /* Get the name of the target from the property list. */
    if(H5Pget(lapl_id, DEST_PROP_NAME, target) < 0) TEST_ERROR

    if((ret_value = H5Oopen(cur_group, target, lapl_id)) < 0)
        TEST_ERROR

    return ret_value;

error:
    return -1;
} /* end UD_plist_traverse() */

const H5L_class_t UD_plist_class[1] = {{
    H5L_LINK_CLASS_T_VERS,    /* H5L_class_t version       */
    (H5L_type_t)UD_PLIST_TYPE, /* Link type id number            */
    "UD_plist_link",          /* Link class name for debugging  */
    NULL,                     /* Creation callback              */
    NULL,                     /* Move/rename callback           */
    NULL,                     /* Copy callback                  */
    UD_plist_traverse,        /* The actual traversal function  */
    NULL,                     /* Deletion callback              */
    NULL                      /* Query callback                 */
}};

static int
lapl_udata(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    hid_t       plist_id = (-1);                /* Property List ID */
    char	group_a_name[NAME_BUF_SIZE];
    char	group_b_name[NAME_BUF_SIZE];
    char	filename[NAME_BUF_SIZE];

    if(new_format)
        TESTING("user data passed through lapl (w/new group format)")
    else
        TESTING("user data passed through lapl")

    /* Set up filename and create file*/
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Register UD link types from previous tests to check that having
     * multiple types registered at once presents no problems. */
    if(H5Lregister(UD_cb_class) < 0) TEST_ERROR

    /* Register the link class.  We'll actually be using for this test. */
    if(H5Lregister(UD_plist_class) < 0) TEST_ERROR

    /* Another link class from a previous test */
    if(H5Lregister(UD_hard_class) < 0) TEST_ERROR

    /* Unregister the first link type registered to make sure this doesn't
     * break anything. */
    if(H5Lunregister((H5L_type_t)UD_CB_TYPE) < 0) TEST_ERROR

    /* Create two groups for the UD link to point to */
    if((gid = H5Gcreate2(fid, "group_a", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR
    if((gid = H5Gcreate2(fid, "group_b", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Create a user-defined link to the group.  These UD links have no udata. */
    if(H5Lcreate_ud(fid, "ud_link", (H5L_type_t)UD_PLIST_TYPE, NULL, (size_t)0, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Create a non-default lapl with a new property pointing to group a*/
    if((plist_id = H5Pcreate(H5P_LINK_ACCESS)) < 0) TEST_ERROR
    HDstrcpy(group_a_name, "group_a");
    if(H5Pinsert2(plist_id, DEST_PROP_NAME, (size_t)NAME_BUF_SIZE, group_a_name, NULL, NULL, NULL, NULL, NULL, NULL) < 0) TEST_ERROR

    /* Try opening group through UD link */
    if((gid = H5Oopen(fid, "ud_link", plist_id)) < 0) TEST_ERROR
    if((gid2 = H5Gcreate2(gid, "subgroup_a", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Verify that we can open the new group without using the ud link */
    if((gid2 = H5Gopen2(fid, "/group_a/subgroup_a", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Now use the same ud link to access group_b */
    strcpy(group_b_name, "group_b");
    if(H5Pset(plist_id, DEST_PROP_NAME, group_b_name) < 0) TEST_ERROR

    /* Create a subgroup */
    if((gid = H5Oopen(fid, "ud_link", plist_id)) < 0) TEST_ERROR
    if((gid2 = H5Gcreate2(gid, "subgroup_b", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Verify that we can open the new group without using the ud link */
    if((gid2 = H5Gopen2(fid, "/group_b/subgroup_b", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close property list */
    if(H5Pclose(plist_id) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Pclose (plist_id);
    	H5Gclose (gid);
    	H5Gclose (gid2);
    	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end lapl_udata() */


/*-------------------------------------------------------------------------
 * Function:    ud_link_errors
 *
 * Purpose:     Create error conditions in callbacks and ensure that the
 *              errors propagate correctly.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  James Laird
 *              Tuesday, June 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
UD_cbsucc_create(const char UNUSED * link_name, hid_t UNUSED loc_group,
    const void *udata, size_t udata_size, hid_t UNUSED lcpl_id)
{
    /* Check to make sure that this "soft link" has a target */
    if(udata_size < 1 || !udata)
       return -1;

    return 0;
} /* end UD_cbsucc_create() */

static hid_t
UD_cbsucc_traverse(const char UNUSED *link_name, hid_t cur_group,
    const void *udata, size_t UNUSED udata_size, hid_t lapl_id)
{
    const char *target = (const char *)udata;
    hid_t ret_value;

    if(!target) goto error;

    if((ret_value = H5Oopen(cur_group, target, lapl_id)) < 0) goto error;

    return ret_value;

error:
    return -1;
} /* end UD_cbsucc_traverse() */

/* Failure callback for when the link is moved or renamed */
static herr_t
UD_cbfail_move(const char UNUSED *new_name, hid_t UNUSED new_loc,
    const void UNUSED *udata, size_t UNUSED udata_size)
{
    /* This traversal function will always fail. */
    return -1;
} /* end UD_cbfail_move() */

/* SuccessCallback for when the link is moved or renamed */
static herr_t
UD_cbsucc_move(const char UNUSED *new_name, hid_t UNUSED new_loc,
    const void UNUSED *udata, size_t UNUSED udata_size)
{
    /* This traversal function will always succeed. */
    return 0;
} /* end UD_cbsucc_move() */

/* Callback for when the link is deleted.  Also called during move */
static herr_t
UD_cbsucc_delete(const char UNUSED *link_name, hid_t UNUSED file,
    const void UNUSED *udata, size_t UNUSED udata_size)
{
    /* This callback will always succeed */
    return 0;
} /* end UD_cbsucc_delete() */

/* Callback for when the link is deleted.  Also called during move */
static herr_t
UD_cbfail_delete(const char UNUSED *link_name, hid_t UNUSED file,
    const void UNUSED *udata, size_t UNUSED udata_size)
{
    /* This traversal function will always fail. */
    /* Note: un-deletable links are in general a very bad idea! */
    return -1;
} /* end UD_cbfail_delete() */

/* Callback for when the link is queried */
static ssize_t
UD_cbfail_query(const char UNUSED *link_name, const void UNUSED *udata,
    size_t UNUSED udata_size, void UNUSED *buf, size_t UNUSED buf_size)
{
    /* This traversal function will always fail. */
    return -1;
} /* end UD_cbfail_query() */

/* Callback for when the link is queried */
static ssize_t
UD_cbfail_on_write_query(const char UNUSED *link_name, const void UNUSED *udata,
    size_t UNUSED udata_size, void *buf, size_t UNUSED buf_size)
{
    /* This traversal function will return a buffer size,
     * but will fail when a buffer is passed in ("writing to the buffer"
     * fails
     */

    if(buf != NULL)
        return -1;

    return 0;
} /* end UD_cbfail_on_write_query() */

/* Callback for when the link is queried */
static ssize_t
UD_cbsucc_query(const char UNUSED *link_name, const void UNUSED *udata,
    size_t UNUSED udata_size, void *buf, size_t buf_size)
{
    /* This traversal function will return a buffer size,
     * but will fail when a buffer is passed in ("writing to the buffer"
     * fails
     */

    if(buf != NULL && buf_size >= 8)
        HDstrcpy((char *)buf, "succeed");

    return 8;
} /* end UD_cbsucc_query() */

/* This class is full of failing callbacks */
const H5L_class_t UD_cbfail_class1[1] = {{
    H5L_LINK_CLASS_T_VERS,    /* H5L_class_t version       */
    (H5L_type_t)UD_CBFAIL_TYPE,   /* Link type id number            */
    "UD_cbfail_link1",            /* Link class name for debugging  */
    UD_cbsucc_create,             /* Creation callback              */
    UD_cbfail_move,               /* Move/rename callback           */
    UD_cbfail_move,               /* Copy callback                  */
    UD_cbsucc_traverse,           /* The actual traversal function  */
    UD_cbfail_delete,             /* Deletion callback              */
    UD_cbfail_query               /* Query callback                 */
}};

/* This class is has two failing callbacks, move and query */
const H5L_class_t UD_cbfail_class2[1] = {{
    H5L_LINK_CLASS_T_VERS,    /* H5L_class_t version       */
    (H5L_type_t)UD_CBFAIL_TYPE,   /* Link type id number            */
    "UD_cbfail_link2",            /* Link class name for debugging  */
    UD_cbsucc_create,             /* Creation callback              */
    UD_cbfail_move,               /* Move/rename callback           */
    UD_cbsucc_move,               /* Copy callback                  */
    UD_cbsucc_traverse,           /* The actual traversal function  */
    UD_cbsucc_delete,             /* Deletion callback              */
    UD_cbfail_on_write_query      /* Query callback                 */
}};

/* All of these callbacks will succeed */
const H5L_class_t UD_cbfail_class3[1] = {{
    H5L_LINK_CLASS_T_VERS,    /* H5L_class_t version       */
    (H5L_type_t)UD_CBFAIL_TYPE,   /* Link type id number            */
    "UD_cbfail_link3",            /* Link class name for debugging  */
    UD_cbsucc_create,             /* Creation callback              */
    UD_cbsucc_move,               /* Move/rename callback           */
    UD_cbsucc_move,               /* Copy callback                  */
    UD_cbsucc_traverse,           /* The actual traversal function  */
    UD_cbsucc_delete,             /* Deletion callback              */
    UD_cbsucc_query               /* Query callback                 */
}};

/* Link classes that are invalid for various reasons */
const H5L_class_t UD_error1_class[1] = {{
    H5L_LINK_CLASS_T_VERS,    /* H5L_class_t version       */
    (H5L_type_t)UD_ERROR_TYPE, /* Link type id number            */
    "UD_error_link",          /* Link class name for debugging  */
    NULL,                     /* Creation callback              */
    NULL,                     /* Move/rename callback           */
    NULL,                     /* Copy callback                  */
    NULL,                     /* This class has no traversal function */
    NULL,                     /* Deletion callback              */
    NULL                      /* Query callback                 */
}};
const H5L_class_t UD_error2_class[1] = {{
    UD_BAD_VERS,              /* Invalid H5L_class_t version */
    (H5L_type_t)UD_ERROR_TYPE, /* Link type id number            */
    "UD_error_link",          /* Link class name for debugging  */
    NULL,                     /* Creation callback              */
    NULL,                     /* Move/rename callback           */
    NULL,                     /* Copy callback                  */
    UD_cbsucc_traverse,       /* Traversal function             */
    NULL,                     /* Deletion callback              */
    NULL                      /* Query callback                 */
}};
const H5L_class_t UD_error3_class[1] = {{
    H5L_LINK_CLASS_T_VERS,    /* H5L_class_t version */
    (H5L_type_t)UD_BAD_TYPE1, /* Invalid Link type id number            */
    "UD_error_link",          /* Link class name for debugging  */
    NULL,                     /* Creation callback              */
    NULL,                     /* Move/rename callback           */
    NULL,                     /* Copy callback                  */
    UD_cbsucc_traverse,       /* Traversal function             */
    NULL,                     /* Deletion callback              */
    NULL                      /* Query callback                 */
}};
const H5L_class_t UD_error4_class[1] = {{
    H5L_LINK_CLASS_T_VERS,    /* H5L_class_t version */
    (H5L_type_t)UD_BAD_TYPE2, /* Invalid Link type id number            */
    "UD_error_link",          /* Link class name for debugging  */
    NULL,                     /* Creation callback              */
    NULL,                     /* Move/rename callback           */
    NULL,                     /* Copy callback                  */
    UD_cbsucc_traverse,       /* Traversal function             */
    NULL,                     /* Deletion callback              */
    NULL                      /* Query callback                 */
}};

static int
ud_link_errors(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);                     /* Group IDs */
    char	group_name[NAME_BUF_SIZE];
    char	filename[NAME_BUF_SIZE];
    char        query_buf[NAME_BUF_SIZE];
    H5L_info_t li;                         /* Link information */

    if(new_format)
        TESTING("user-defined link error conditions (w/new group format)")
    else
        TESTING("user-defined link error conditions")

    /* Set up filename and create file*/
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Try to register some invalid link classes */
    H5E_BEGIN_TRY {
      if(H5Lregister(UD_error1_class) >= 0) TEST_ERROR
      if(H5Lregister(UD_error2_class) >= 0) TEST_ERROR
      if(H5Lregister(UD_error3_class) >= 0) TEST_ERROR
      if(H5Lregister(UD_error4_class) >= 0) TEST_ERROR
    } H5E_END_TRY

    /* Register the UD plist class. */
    if(H5Lregister(UD_plist_class) < 0) TEST_ERROR
    /* Now register the first class we'll be using.
     * It has the same ID as the plist class, and should replace it. */
    if(H5Lregister(UD_cbfail_class1) < 0) FAIL_STACK_ERROR

    /* Create a group for the UD link to point to */
    if((gid = H5Gcreate2(fid, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Try to create internally defined links with H5Lcreate_ud */
    H5E_BEGIN_TRY {
        if(H5Lcreate_ud(fid, "/ud_link", H5L_TYPE_HARD, NULL, (size_t)0, H5P_DEFAULT, H5P_DEFAULT) >= 0)
            TEST_ERROR
        if(H5Lcreate_ud(fid, "/ud_link", H5L_TYPE_SOFT, "str", (size_t)4, H5P_DEFAULT, H5P_DEFAULT) >= 0)
            TEST_ERROR
    } H5E_END_TRY

    /* Create a user-defined link to the group. */
    strcpy(group_name, "/group");
    if(H5Lcreate_ud(fid, "/ud_link", (H5L_type_t)UD_CBFAIL_TYPE, &group_name, HDstrlen(group_name) + 1, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Open the group through the ud link */
    if((gid = H5Gopen2(fid, "ud_link", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Now test that each of the callbacks will cause a failure if it returns -1 */
    H5E_BEGIN_TRY {
        /* The create callback will fail if we pass in no udata */
        if(H5Lcreate_ud(fid, "fail", (H5L_type_t)UD_CBFAIL_TYPE, NULL, (size_t)0, H5P_DEFAULT, H5P_DEFAULT) >= 0) TEST_ERROR

        /* The move and copy callbacks will fail */
        if(H5Lmove(fid, "ud_link", H5L_SAME_LOC, "move_fail", H5P_DEFAULT, H5P_DEFAULT) >= 0) TEST_ERROR
        if(H5Lcopy(fid, "ud_link", fid, "copy_fail", H5P_DEFAULT, H5P_DEFAULT) >= 0) TEST_ERROR

        /* The traversal callback will fail if we remove its target */
        if(H5Ldelete(fid, "group", H5P_DEFAULT) < 0) TEST_ERROR
        if((gid = H5Gopen2(gid, "ud_link", H5P_DEFAULT)) >= 0) TEST_ERROR

        /* The deletion callback will always fail */
        if(H5Ldelete(fid, "ud_link", H5P_DEFAULT) >= 0) TEST_ERROR

        /* The query callback will fail */
        if(H5Lget_info(fid, "ud_link", &li, H5P_DEFAULT) >=0) TEST_ERROR
    } H5E_END_TRY

    /* Now use a class with different callback functions */
    if(H5Lregister(UD_cbfail_class2) < 0) FAIL_STACK_ERROR

    /* Moving should still fail, but copying will succeed */
    H5E_BEGIN_TRY {
        if(H5Lmove(fid, "ud_link", H5L_SAME_LOC, "move_fail", H5P_DEFAULT, H5P_DEFAULT) >= 0) TEST_ERROR
    } H5E_END_TRY
    if(H5Lcopy(fid, "ud_link", fid, "copy_succ", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* The query callback will succeed when we only want to get the size of the buffer... */
    if(H5Lget_info(fid, "ud_link", &li, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(li.u.val_size != 0) TEST_ERROR
    /* ...but fail when we try to write data to the buffer itself*/
    H5E_BEGIN_TRY {
        if(H5Lget_val(fid, "ud_link", query_buf, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) >=0) TEST_ERROR
    } H5E_END_TRY

    /* Register a new class */
    if(H5Lregister(UD_cbfail_class3) < 0) FAIL_STACK_ERROR

    /* Now querying should succeed */
    if(H5Lget_info(fid, "ud_link", &li, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(li.u.val_size != 8) TEST_ERROR
    if(H5Lget_val(fid, "ud_link", query_buf, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(HDstrcmp(query_buf, "succeed") != 0) TEST_ERROR

    /* Moving and copying should both succeed */
    if(H5Lmove(fid, "copy_succ", H5L_SAME_LOC, "move_succ", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcopy(fid, "ud_link", fid, "copy_succ2", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Delete link (this callback should work now) */
    if(H5Ldelete(fid, "ud_link", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Gclose (gid);
    	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end ud_link_errors() */


/*-------------------------------------------------------------------------
 * Function:    lapl_nlinks
 *
 * Purpose:     Check that the maximum number of soft links can be adjusted
 *              by the user using the Link Access Property List.
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  James Laird
 *              Tuesday, June 6, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
lapl_nlinks(hid_t fapl, hbool_t new_format)
{
    hid_t		fid = (-1);               /* File ID */
    hid_t	gid = (-1), gid2 = (-1);          /* Group IDs */
    hid_t		plist = (-1);             /* lapl ID */
    hid_t       tid = (-1), sid = (-1), did = (-1); /* Other IDs */
    hid_t gapl = (-1), dapl = (-1), tapl = (-1);   /* Other property lists */
    char                objname[NAME_BUF_SIZE];   /* Object name */
    char		filename[NAME_BUF_SIZE];
    size_t              nlinks;               /* nlinks for H5Pset_nlinks */
    hsize_t     	dims[2];

    if(new_format)
        TESTING("adjusting nlinks with LAPL (w/new group format)")
    else
        TESTING("adjusting nlinks with LAPL")

    /* Make certain test is valid */
    /* XXX: should probably make a "generic" test that creates the proper
     *          # of links based on this value - QAK
     */
    HDassert(H5L_NUM_LINKS == 16);

    /* Create file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create group with short name in file (used as target for links) */
    if((gid = H5Gcreate2(fid, "final", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Create chain of soft links to existing object (limited) */
    if(H5Lcreate_soft("final", fid, "soft1", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("soft1", fid, "soft2", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("soft2", fid, "soft3", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("soft3", fid, "soft4", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("soft4", fid, "soft5", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("soft5", fid, "soft6", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("soft6", fid, "soft7", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("soft7", fid, "soft8", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("soft8", fid, "soft9", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("soft9", fid, "soft10", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("soft10", fid, "soft11", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("soft11", fid, "soft12", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("soft12", fid, "soft13", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("soft13", fid, "soft14", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("soft14", fid, "soft15", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("soft15", fid, "soft16", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("soft16", fid, "soft17", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close objects */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Open file */
    if((fid=H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR

    /* Create LAPL with higher-than-usual nlinks value */
    /* Create a non-default lapl with udata set to point to the first group */
    if((plist = H5Pcreate(H5P_LINK_ACCESS)) < 0) TEST_ERROR
    nlinks = 20;
    if(H5Pset_nlinks(plist, nlinks) < 0) TEST_ERROR

    /* Ensure that nlinks was set successfully */
    nlinks = 0;
    if(H5Pget_nlinks(plist, &nlinks) < 0) TEST_ERROR
    if(nlinks != 20) TEST_ERROR

    /* Open object through what is normally too many soft links using
     * new property list */
    if((gid = H5Oopen(fid, "soft17", plist)) < 0) TEST_ERROR

    /* Check name */
    if(H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/soft17")) TEST_ERROR

    /* Create group using soft link */
    if((gid2 = H5Gcreate2(gid, "new_soft", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close groups */
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Set nlinks to a smaller number */
    nlinks = 4;
    if(H5Pset_nlinks(plist, nlinks) < 0) TEST_ERROR

    /* Ensure that nlinks was set successfully */
    nlinks = 0;
    if(H5Pget_nlinks(plist, &nlinks) < 0) TEST_ERROR
    if(nlinks != 4) TEST_ERROR

    /* Try opening through what is now too many soft links */
    H5E_BEGIN_TRY {
        gid = H5Oopen(fid, "soft5", plist);
    } H5E_END_TRY;
    if (gid >= 0) {
	H5_FAILED();
	puts("    Should have failed for sequence of too many nested links.");
	goto error;
    }

    /* Open object through lesser soft link */
    if((gid = H5Oopen(fid, "soft4", plist)) < 0) TEST_ERROR

    /* Check name */
    if(H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/soft4")) TEST_ERROR


    /* Test other functions that should use a LAPL */
    nlinks = 20;
    if(H5Pset_nlinks(plist, nlinks) < 0) TEST_ERROR

    /* Try copying and moving when both src and dst contain many soft links
     * using a non-default LAPL
     */
    if(H5Lcopy(fid, "soft17", fid, "soft17/newer_soft", H5P_DEFAULT, plist) < 0) TEST_ERROR
    if(H5Lmove(fid, "soft17/newer_soft", fid, "soft17/newest_soft", H5P_DEFAULT, plist) < 0) TEST_ERROR

    /* H5Olink */
    if(H5Olink(gid, fid, "soft17/link_to_group", H5P_DEFAULT, plist) < 0) TEST_ERROR

    /* H5Lcreate_hard  and H5Lcreate_soft */
    if(H5Lcreate_hard(fid, "soft17", fid, "soft17/link2_to_group", H5P_DEFAULT, plist) < 0) TEST_ERROR
    if(H5Lcreate_soft("/soft4", fid, "soft17/soft_link", H5P_DEFAULT, plist) < 0) TEST_ERROR

    /* H5Ldelete */
    if(H5Ldelete(fid, "soft17/soft_link", plist) < 0) TEST_ERROR

    /* H5Lget_val and H5Lget_info */
    if(H5Lget_val(fid, "soft17", NULL, (size_t)0, plist) < 0) TEST_ERROR
    if(H5Lget_info(fid, "soft17", NULL, plist) < 0) TEST_ERROR

    /* H5Lcreate_external and H5Lcreate_ud */
    if(H5Lcreate_external("filename", "path", fid, "soft17/extlink", H5P_DEFAULT, plist) < 0) TEST_ERROR
    if(H5Lregister(UD_rereg_class) < 0) TEST_ERROR
    if(H5Lcreate_ud(fid, "soft17/udlink", (H5L_type_t)UD_HARD_TYPE, NULL, (size_t)0, H5P_DEFAULT, plist) < 0) TEST_ERROR

    /* Close plist */
    if(H5Pclose(plist) < 0) TEST_ERROR


    /* Create a datatype and dataset as targets inside the group */
    if((tid = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR
    if(H5Tcommit2(gid, "datatype", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Tclose(tid) < 0) TEST_ERROR

    dims[0] = 2;
    dims[1] = 2;
    if((sid = H5Screate_simple(2, dims, NULL)) < 0) TEST_ERROR
    if((did = H5Dcreate2(gid, "dataset", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Dclose(did) < 0) TEST_ERROR

    /* Close group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Try to open the objects using too many symlinks with default *APLs */
    H5E_BEGIN_TRY {
        if((gid = H5Gopen2(fid, "soft17", H5P_DEFAULT)) >= 0)
            FAIL_PUTS_ERROR("    Should have failed for too many nested links.")
        if((tid = H5Topen2(fid, "soft17/datatype", H5P_DEFAULT)) >= 0)
            FAIL_PUTS_ERROR("    Should have failed for too many nested links.")
        if((did = H5Dopen2(fid, "soft17/dataset", H5P_DEFAULT)) >= 0)
            FAIL_PUTS_ERROR("    Should have failed for too many nested links.")
    } H5E_END_TRY

    /* Create property lists with nlinks set */
    if((gapl = H5Pcreate(H5P_GROUP_ACCESS)) < 0) TEST_ERROR
    if((tapl = H5Pcreate(H5P_DATATYPE_ACCESS)) < 0) TEST_ERROR
    if((dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0) TEST_ERROR

    nlinks = 20;
    if(H5Pset_nlinks(gapl, nlinks) < 0) TEST_ERROR
    if(H5Pset_nlinks(tapl, nlinks) < 0) TEST_ERROR
    if(H5Pset_nlinks(dapl, nlinks) < 0) TEST_ERROR

    /* We should now be able to use these property lists to open each kind
     * of object.
     */
    if((gid = H5Gopen2(fid, "soft17", gapl)) < 0) FAIL_STACK_ERROR
    if((tid = H5Topen2(fid, "soft17/datatype", tapl)) < 0) TEST_ERROR
    if((did = H5Dopen2(fid, "soft17/dataset", dapl)) < 0) TEST_ERROR

    /* Close objects */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Tclose(tid) < 0) TEST_ERROR
    if(H5Dclose(did) < 0) TEST_ERROR

    /* Close plists */
    if(H5Pclose(gapl) < 0) TEST_ERROR
    if(H5Pclose(tapl) < 0) TEST_ERROR
    if(H5Pclose(dapl) < 0) TEST_ERROR

    /* Unregister UD hard link class */
    if(H5Lunregister((H5L_type_t)UD_HARD_TYPE) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Pclose(gapl);
    	H5Pclose(dapl);
    	H5Pclose(tapl);
    	H5Dclose(did);
    	H5Sclose(sid);
    	H5Tclose(tid);
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Pclose(plist);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end lapl_nlinks() */


/*-------------------------------------------------------------------------
 * Function:    linkinfo
 *
 * Purpose:     Check that the link class is returned correctly when queried.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  James Laird
 *              Tuesday, June 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
linkinfo(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);	                /* Group ID */
    hid_t       tid = (-1);                     /* Type ID */
    hid_t       sid = (-1), did = -(1);         /* Dataspace and dataset IDs */
    H5L_info_t li;                          /* Link information */
    char	filename[NAME_BUF_SIZE];

    if(new_format)
        TESTING("link type field in H5Lget_info (w/new group format)")
    else
        TESTING("link type field in H5Lget_info")

    /* Set up filename and create file*/
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Register a couple of user-defined link classes with the library */
    if(H5Lregister(UD_plist_class) < 0) TEST_ERROR

    /* Create an object of each type */
    if((tid = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR
    if(H5Tcommit2(fid, "datatype", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if((gid = H5Gcreate2(fid, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("group", fid, "softlink", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    if((sid = H5Screate(H5S_SCALAR)) < 0) TEST_ERROR
    if((did = H5Dcreate2(fid, "dataset", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    if(H5Lcreate_ud(fid, "ud_link", (H5L_type_t)UD_PLIST_TYPE, NULL, (size_t)0, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external("file_name", "obj_path", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close all objects */
    if(H5Tclose(tid) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Dclose(did) < 0) TEST_ERROR

    /* Make sure that link type is correct when objects are queried */
    if(H5Lget_info(fid, "datatype", &li, H5P_DEFAULT) < 0) TEST_ERROR
    if(li.type != H5L_TYPE_HARD) TEST_ERROR
    if(H5Lget_info(fid, "group", &li, H5P_DEFAULT) < 0) TEST_ERROR
    if(li.type != H5L_TYPE_HARD) TEST_ERROR
    if(H5Lget_info(fid, "dataset", &li, H5P_DEFAULT) < 0) TEST_ERROR
    if(li.type != H5L_TYPE_HARD) TEST_ERROR

    if(H5Lget_info(fid, "ext_link", &li, H5P_DEFAULT) < 0) TEST_ERROR
    if(li.type != H5L_TYPE_EXTERNAL) TEST_ERROR
    if(H5Lget_info(fid, "softlink", &li, H5P_DEFAULT) < 0) TEST_ERROR
    if(li.type != H5L_TYPE_SOFT) TEST_ERROR
    if(H5Lget_info(fid, "ud_link", &li, H5P_DEFAULT) < 0) TEST_ERROR
    if(li.type != UD_PLIST_TYPE) TEST_ERROR

    /* Ensure that passing a NULL pointer doesn't cause an error */
    if(H5Lget_info(fid, "group", NULL, H5P_DEFAULT) < 0) TEST_ERROR

    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Tclose (tid);
    	H5Dclose (did);
    	H5Gclose (gid);
    	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end linkinfo() */


/*-------------------------------------------------------------------------
 * Function:    check_all_closed
 *
 * Purpose:     External links and some UD links open files.  To make sure
 *              that all such files got closed correctly, try to create
 *              each of them.
 *
 *              If the files are still open, this will fail (indicating that
 *              some other test made a mistake).
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  James Laird
 *              Thursday, August 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
check_all_closed(hid_t fapl, hbool_t new_format, int stopat)
{
    hid_t fid=-1;
    char filename[NAME_BUF_SIZE];
    int x;

    if(new_format)
        TESTING("that all files were closed correctly (w/new group format)")
    else
        TESTING("that all files were closed correctly")

    /* Some of the external or UD link tests may have failed to close
     * an external file properly.
     * To check this, try to create every file used in this test.  If
     * a file is already open, creating it will fail.
     */
    for(x=0; FILENAME[x] != NULL && x < stopat; x++)
    {
        h5_fixname(FILENAME[x], fapl, filename, sizeof filename);

        if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
        if(H5Fclose(fid) < 0) TEST_ERROR
    }

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end check_all_closed() */



/*-------------------------------------------------------------------------
 * Function:    build_visit_file
 *
 * Purpose:     Build an "interesting" file to use for visiting links & objects
 *
 * Return:      Success:        >0, File ID for file built
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Saturday, November 24, 2007
 *
 *-------------------------------------------------------------------------
 */
static hid_t
build_visit_file(hid_t fapl)
{
    hid_t fid = -1;                     /* File ID */
    hid_t gid = -1, gid2 = -1;          /* Group IDs */
    hid_t sid = (-1);                   /* Dataspace ID */
    hid_t did = (-1);                   /* Dataset ID */
    hid_t tid = (-1);                   /* Datatype ID */
    char filename[NAME_BUF_SIZE];
    char pathname[1024];                /* Path of external link file */
    char *srcdir = getenv("srcdir");    /* where the src code is located */

    h5_fixname(FILENAME[9], fapl, filename, sizeof filename);

    /* Create file for visiting */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create group */
    if((gid = H5Gcreate2(fid, "/Group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Create nested group */
    if((gid2 = H5Gcreate2(gid, "Group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close groups */
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR


    /* Create soft links to groups created */
    if(H5Lcreate_soft("/Group1", fid, "/soft_one", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("/Group1/Group2", fid, "/soft_two", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Create dangling soft link */
    if(H5Lcreate_soft("nowhere", fid, "/soft_dangle", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR


    /* Create hard links to all groups */
    if(H5Lcreate_hard(fid, "/", fid, "hard_zero", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "/Group1", fid, "hard_one", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "/Group1/Group2", fid, "hard_two", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Create loops w/hard links */
    if(H5Lcreate_hard(fid, "/Group1", fid, "/Group1/hard_one", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "/", fid, "/Group1/Group2/hard_zero", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Create external link to existing file */
    pathname[0] = '\0';
    /* Generate correct name for test file by prepending the source path */
    if(srcdir && ((HDstrlen(srcdir) + HDstrlen(LINKED_FILE) + 1) < sizeof(pathname))) {
        HDstrcpy(pathname, srcdir);
        HDstrcat(pathname, "/");
    }
    HDstrcat(pathname, LINKED_FILE);

    if(H5Lcreate_external(pathname, "/group", fid, "/ext_one", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Create dangling external link to non-existent file */
    if(H5Lcreate_external("foo.h5", "/group", fid, "/ext_dangle", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Create dataset in each group */
    if((sid = H5Screate(H5S_SCALAR)) < 0) TEST_ERROR

    if((did = H5Dcreate2(fid, "/Dataset_zero", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Dclose(did) < 0) TEST_ERROR

    if((did = H5Dcreate2(fid, "/Group1/Dataset_one", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Dclose(did) < 0) TEST_ERROR

    if((did = H5Dcreate2(fid, "/Group1/Group2/Dataset_two", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Dclose(did) < 0) TEST_ERROR

    if(H5Sclose(sid) < 0) TEST_ERROR

    /* Create named datatype in each group */
    if((tid = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR
    if(H5Tcommit2(fid, "/Type_zero", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Tclose(tid) < 0) TEST_ERROR

    if((tid = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR
    if(H5Tcommit2(fid, "/Group1/Type_one", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Tclose(tid) < 0) TEST_ERROR

    if((tid = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR
    if(H5Tcommit2(fid, "/Group1/Group2/Type_two", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Tclose(tid) < 0) TEST_ERROR

    return(fid);

error:
    H5E_BEGIN_TRY {
        H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end build_visit_file() */


/*-------------------------------------------------------------------------
 * Function:    visit_link_cb
 *
 * Purpose:     Callback routine for visiting links in a file
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Saturday, November 24, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
visit_link_cb(hid_t UNUSED group_id, const char *name, const H5L_info_t *linfo,
    void *_op_data)
{
    lvisit_ud_t *op_data = (lvisit_ud_t *)_op_data;

    /* Check for correct link information */
    if(HDstrcmp(op_data->info[op_data->idx].path, name)) return(H5_ITER_ERROR);
    if(op_data->info[op_data->idx].type != linfo->type) return(H5_ITER_ERROR);

    /* Advance to next location in expected output */
    op_data->idx++;

    return(H5_ITER_CONT);
} /* end visit_link_cb() */


/*-------------------------------------------------------------------------
 * Function:    link_visit
 *
 * Purpose:     Test the link visiting routine
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Saturday, November 24, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
link_visit(hid_t fapl, hbool_t new_format)
{
    lvisit_ud_t udata;          /* User-data for visiting */
    hid_t fid = -1;
    hid_t gid = -1;             /* Group ID */

    if(new_format)
        TESTING("link visiting (w/new group format)")
    else
        TESTING("link visiting")

    /* Construct "interesting" file to visit */
    if((fid = build_visit_file(fapl)) < 0) TEST_ERROR

    /* Visit all the links reachable from the root group (with file ID) */
    udata.idx = 0;
    udata.info = lvisit0;
    if(H5Lvisit(fid, H5_INDEX_NAME, H5_ITER_INC, visit_link_cb, &udata) < 0) FAIL_STACK_ERROR

    /* Visit all the links reachable from the root group (with group ID) */
    if((gid = H5Gopen2(fid, "/", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    udata.idx = 0;
    udata.info = lvisit0;
    if(H5Lvisit(gid, H5_INDEX_NAME, H5_ITER_INC, visit_link_cb, &udata) < 0) FAIL_STACK_ERROR
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR


    /* Visit all the links reachable from each internal group */
    if((gid = H5Gopen2(fid, "/Group1", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    udata.idx = 0;
    udata.info = lvisit1;
    if(H5Lvisit(gid, H5_INDEX_NAME, H5_ITER_INC, visit_link_cb, &udata) < 0) FAIL_STACK_ERROR
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    if((gid = H5Gopen2(fid, "/Group1/Group2", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    udata.idx = 0;
    udata.info = lvisit2;
    if(H5Lvisit(gid, H5_INDEX_NAME, H5_ITER_INC, visit_link_cb, &udata) < 0) FAIL_STACK_ERROR
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR


    /* Close file created */
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Gclose(gid);
        H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end link_visit() */


/*-------------------------------------------------------------------------
 * Function:    link_visit_by_name
 *
 * Purpose:     Test the link visiting "by name" routine
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Saturday, November 24, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
link_visit_by_name(hid_t fapl, hbool_t new_format)
{
    lvisit_ud_t udata;          /* User-data for visiting */
    hid_t fid = -1;
    hid_t gid = -1;             /* Group ID */

    if(new_format)
        TESTING("link visiting by name (w/new group format)")
    else
        TESTING("link visiting by name")

    /* Construct "interesting" file to visit */
    if((fid = build_visit_file(fapl)) < 0) TEST_ERROR

    /* Visit all the links reachable from the root group (with file ID) */
    udata.idx = 0;
    udata.info = lvisit0;
    if(H5Lvisit_by_name(fid, "/", H5_INDEX_NAME, H5_ITER_INC, visit_link_cb, &udata, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Visit all the links reachable from the root group (with group ID) */
    if((gid = H5Gopen2(fid, "/", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    udata.idx = 0;
    udata.info = lvisit0;
    if(H5Lvisit_by_name(gid, ".", H5_INDEX_NAME, H5_ITER_INC, visit_link_cb, &udata, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR


    /* Visit all the links reachable from each internal group */
    udata.idx = 0;
    udata.info = lvisit1;
    if(H5Lvisit_by_name(fid, "/Group1", H5_INDEX_NAME, H5_ITER_INC, visit_link_cb, &udata, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    if((gid = H5Gopen2(fid, "/Group1", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    udata.idx = 0;
    udata.info = lvisit1;
    if(H5Lvisit_by_name(gid, ".", H5_INDEX_NAME, H5_ITER_INC, visit_link_cb, &udata, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    udata.idx = 0;
    udata.info = lvisit2;
    if(H5Lvisit_by_name(fid, "/Group1/Group2", H5_INDEX_NAME, H5_ITER_INC, visit_link_cb, &udata, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    if((gid = H5Gopen2(fid, "/Group1/Group2", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    udata.idx = 0;
    udata.info = lvisit2;
    if(H5Lvisit_by_name(gid, ".", H5_INDEX_NAME, H5_ITER_INC, visit_link_cb, &udata, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR


    /* Close file created */
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Gclose(gid);
        H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end link_visit_by_name() */


/*-------------------------------------------------------------------------
 * Function:    visit_obj_cb
 *
 * Purpose:     Callback routine for visiting objects in a file
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Sunday, November 25, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
visit_obj_cb(hid_t UNUSED group_id, const char *name, const H5O_info_t *oinfo,
    void *_op_data)
{
    ovisit_ud_t *op_data = (ovisit_ud_t *)_op_data;

    /* Check for correct object information */
    if(HDstrcmp(op_data->info[op_data->idx].path, name)) return(H5_ITER_ERROR);
    if(op_data->info[op_data->idx].type != oinfo->type) return(H5_ITER_ERROR);

    /* Advance to next location in expected output */
    op_data->idx++;

    return(H5_ITER_CONT);
} /* end visit_obj_cb() */


/*-------------------------------------------------------------------------
 * Function:    obj_visit
 *
 * Purpose:     Test the object visiting routine
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Sunday, November 25, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
obj_visit(hid_t fapl, hbool_t new_format)
{
    ovisit_ud_t udata;          /* User-data for visiting */
    hid_t fid = -1;
    hid_t gid = -1;             /* Group ID */

    if(new_format)
        TESTING("object visiting (w/new group format)")
    else
        TESTING("object visiting")

    /* Construct "interesting" file to visit */
    if((fid = build_visit_file(fapl)) < 0) TEST_ERROR

    /* Visit all the objects reachable from the root group (with file ID) */
    udata.idx = 0;
    udata.info = new_format ? ovisit0_new : ovisit0_old;
    if(H5Ovisit(fid, H5_INDEX_NAME, H5_ITER_INC, visit_obj_cb, &udata) < 0) FAIL_STACK_ERROR

    /* Visit all the objects reachable from the root group (with group ID) */
    if((gid = H5Gopen2(fid, "/", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    udata.idx = 0;
    udata.info = new_format ? ovisit0_new : ovisit0_old;
    if(H5Ovisit(gid, H5_INDEX_NAME, H5_ITER_INC, visit_obj_cb, &udata) < 0) FAIL_STACK_ERROR
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR


    /* Visit all the objects reachable from each internal group */
    if((gid = H5Gopen2(fid, "/Group1", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    udata.idx = 0;
    udata.info = new_format ? ovisit1_new : ovisit1_old;
    if(H5Ovisit(gid, H5_INDEX_NAME, H5_ITER_INC, visit_obj_cb, &udata) < 0) FAIL_STACK_ERROR
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    if((gid = H5Gopen2(fid, "/Group1/Group2", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    udata.idx = 0;
    udata.info = new_format ? ovisit2_new : ovisit2_old;
    if(H5Ovisit(gid, H5_INDEX_NAME, H5_ITER_INC, visit_obj_cb, &udata) < 0) FAIL_STACK_ERROR
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR


    /* Close file created */
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Gclose(gid);
        H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end obj_visit() */


/*-------------------------------------------------------------------------
 * Function:    obj_visit_by_name
 *
 * Purpose:     Test the object visiting "by name" routine
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Sunday, November 25, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
obj_visit_by_name(hid_t fapl, hbool_t new_format)
{
    ovisit_ud_t udata;          /* User-data for visiting */
    hid_t fid = -1;
    hid_t gid = -1;             /* Group ID */

    if(new_format)
        TESTING("object visiting by name (w/new group format)")
    else
        TESTING("object visiting by name")

    /* Construct "interesting" file to visit */
    if((fid = build_visit_file(fapl)) < 0) TEST_ERROR

    /* Visit all the objects reachable from the root group (with file ID) */
    udata.idx = 0;
    udata.info = new_format ? ovisit0_new : ovisit0_old;
    if(H5Ovisit_by_name(fid, "/", H5_INDEX_NAME, H5_ITER_INC, visit_obj_cb, &udata, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Visit all the objects reachable from the root group (with group ID) */
    if((gid = H5Gopen2(fid, "/", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    udata.idx = 0;
    udata.info = new_format ? ovisit0_new : ovisit0_old;
    if(H5Ovisit_by_name(gid, ".", H5_INDEX_NAME, H5_ITER_INC, visit_obj_cb, &udata, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR


    /* Visit all the objects reachable from each internal group */
    udata.idx = 0;
    udata.info = new_format ? ovisit1_new : ovisit1_old;
    if(H5Ovisit_by_name(fid, "/Group1", H5_INDEX_NAME, H5_ITER_INC, visit_obj_cb, &udata, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    if((gid = H5Gopen2(fid, "/Group1", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    udata.idx = 0;
    udata.info = new_format ? ovisit1_new : ovisit1_old;
    if(H5Ovisit_by_name(gid, ".", H5_INDEX_NAME, H5_ITER_INC, visit_obj_cb, &udata, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR


    udata.idx = 0;
    udata.info = new_format ? ovisit2_new : ovisit2_old;
    if(H5Ovisit_by_name(fid, "/Group1/Group2", H5_INDEX_NAME, H5_ITER_INC, visit_obj_cb, &udata, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    if((gid = H5Gopen2(fid, "/Group1/Group2", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    udata.idx = 0;
    udata.info = new_format ? ovisit2_new : ovisit2_old;
    if(H5Ovisit_by_name(gid, ".", H5_INDEX_NAME, H5_ITER_INC, visit_obj_cb, &udata, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR


    /* Close file created */
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Gclose(gid);
        H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end obj_visit_by_name() */


/*-------------------------------------------------------------------------
 * Function:    visit_obj_stop_cb
 *
 * Purpose:     Callback routine for visiting objects in a file
 *
 * Return:      1 (H5_ITER_STOP)
 *
 * Programmer:  Neil Fortner
 *              Sunday, November 2, 2008
 *
 *-------------------------------------------------------------------------
 */
static int
visit_obj_stop_cb(hid_t UNUSED group_id, const char UNUSED *name, const H5O_info_t UNUSED *oinfo,
    void *_op_data)
{
    unsigned *op_data = (unsigned *)_op_data;

    /* Increment the number of visited objects */
    (*op_data)++;

    return(H5_ITER_STOP);
} /* end visit_obj_stop_cb() */


/*-------------------------------------------------------------------------
 * Function:    obj_visit_stop
 *
 * Purpose:     Test that the object visiting routine stops iteration
 *              properly on the starting object.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Neil Fortner
 *              Sunday, November 2, 2008
 *
 *-------------------------------------------------------------------------
 */
static int
obj_visit_stop(hid_t fapl, hbool_t new_format)
{
    unsigned    nvisited;           /* User-data for visiting */
    hid_t       fid = -1;
    herr_t      ret;                /* Return value */

    if(new_format)
        TESTING("stopping object iteration (w/new group format)")
    else
        TESTING("stopping object iteration")

    /* Construct "interesting" file to visit */
    if((fid = build_visit_file(fapl)) < 0) TEST_ERROR

    /* Start iteration.  The callback should only be called once because it
     * returns H5_ITER_STOP
     */
    nvisited = 0;
    if((ret = H5Ovisit(fid, H5_INDEX_NAME, H5_ITER_INC, visit_obj_stop_cb, &nvisited)) < 0)
        FAIL_STACK_ERROR
    if(ret != H5_ITER_STOP) TEST_ERROR
    if(nvisited != 1) TEST_ERROR

    /* Same test with H5Ovisit_by_name */
    nvisited = 0;
    if((ret = H5Ovisit_by_name(fid, "/", H5_INDEX_NAME, H5_ITER_INC, visit_obj_stop_cb,
        &nvisited, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(ret != H5_ITER_STOP) TEST_ERROR
    if(nvisited != 1) TEST_ERROR

    /* Close file created */
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end obj_visit_stop() */


/*-------------------------------------------------------------------------
 * Function:    link_filters
 *
 * Purpose:     Tests adding filters to group link storage.  Also tests
 *              copying these groups.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Neil Fortner
 *              Tuesday, June 16, 2009
 *
 *-------------------------------------------------------------------------
 */
static enum {
    LFS_INIT,
    LFS_CAN_APPLY_CALLED,
    LFS_SET_LOCAL_CALLED,
    LFS_ENCODED,
    LFS_DECODED
} link_filter_state;

static htri_t link_filter_can_apply(hid_t dcpl_id, hid_t type_id, hid_t space_id)
{
    if(dcpl_id >= 0 || type_id >= 0 || space_id >= 0)
        return -1;

    if(link_filter_state >= LFS_ENCODED)
        return 1;

    if(link_filter_state != LFS_INIT)
        return -1;

    link_filter_state = LFS_CAN_APPLY_CALLED;

    return 1;
} /* end link_fitler_can_apply */

static herr_t link_filter_set_local(hid_t dcpl_id, hid_t type_id, hid_t space_id)
{
    if(dcpl_id >= 0 || type_id >= 0 || space_id >= 0)
        return -1;

    if(link_filter_state >= LFS_ENCODED)
        return 0;

    if(link_filter_state != LFS_CAN_APPLY_CALLED)
        return -1;

    link_filter_state = LFS_SET_LOCAL_CALLED;

    return 0;
} /* end link_filter_set_local */

static size_t link_filter_filter(unsigned int flags, size_t cd_nelmts,
    const unsigned int cd_values[], size_t nbytes, size_t UNUSED *buf_size,
    void UNUSED **buf)
{
    if(flags & H5Z_FLAG_OPTIONAL || cd_nelmts != 1 || cd_values[0] != 2112)
        return 0;

    if(link_filter_state == LFS_DECODED)
        return nbytes;

    if(flags & H5Z_FLAG_REVERSE) {
        if(link_filter_state != LFS_ENCODED)
            return 0;
        link_filter_state = LFS_DECODED;
    } else {
        if(link_filter_state < LFS_SET_LOCAL_CALLED)
            return 0;
        link_filter_state = LFS_ENCODED;
    } /* end else */

    return nbytes;
} /* end link_filter_filter */

static int
link_filters(hid_t fapl, hbool_t new_format)
{
    hid_t       fid = -1, fcpl = -1;
    hid_t       gid1 = -1, gid2 = -1, gcpl1 = -1, gcpl2 = -1;
    hid_t       lcpl = -1;
    size_t      cd_nelmts = 1;
    unsigned    cd_value = 2112;
    unsigned    cd_value_out;
    unsigned    flags_out;
    unsigned    filter_config_out;
    int         nfilters = 0;
    H5Z_class2_t filter_class;
    char        name_out[24];
    char	filename[NAME_BUF_SIZE];
    htri_t      tri_ret;
    herr_t      status;

    /* This test actually always uses the new group format for the main group.
     * The new format flag affects the version of object header messages,
     * etc., which are important for this test. */
    if(new_format)
        TESTING("group link filters (w/new group format)")
    else
        TESTING("group link filters")

    /* Initialize link filter state global */
    link_filter_state = LFS_INIT;

    /* Set up filename and create file*/
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Create gcpl, force use of dense storage */
    if((gcpl1 = H5Pcreate(H5P_GROUP_CREATE)) < 0) TEST_ERROR
    if(H5Pset_link_phase_change(gcpl1, 2, 2) < 0) TEST_ERROR

    /* Add deflate and checksum filters, if available */
    if((tri_ret = H5Zfilter_avail(H5Z_FILTER_DEFLATE)) < 0) TEST_ERROR
    if(tri_ret) {
        if(H5Pset_deflate(gcpl1, 6) < 0) TEST_ERROR
        nfilters++;
    } /* end if */
    if((tri_ret = H5Zfilter_avail(H5Z_FILTER_FLETCHER32)) < 0) TEST_ERROR
    if(tri_ret) {
        if(H5Pset_fletcher32(gcpl1) < 0) TEST_ERROR
        nfilters++;
    } /* end if */

    /* Register and add custom filter */
    filter_class.version = H5Z_CLASS_T_VERS;
    filter_class.id = H5Z_FILTER_RESERVED + 42;
    filter_class.encoder_present = TRUE;
    filter_class.decoder_present = TRUE;
    filter_class.name = "custom_link_filter";
    filter_class.can_apply = link_filter_can_apply;
    filter_class.set_local = link_filter_set_local;
    filter_class.filter = link_filter_filter;
    if(H5Zregister(&filter_class) < 0) TEST_ERROR
    if(H5Pset_filter(gcpl1, H5Z_FILTER_RESERVED + 42, 0, (size_t)1, &cd_value) < 0)
        TEST_ERROR
    nfilters++;

    /* Test various other filter functions for use on gcpl's */
    if(H5Pget_nfilters(gcpl1) != nfilters) TEST_ERROR
    if(H5Pall_filters_avail(gcpl1) != TRUE) TEST_ERROR

    /* Create a group using this filter, add some soft links to it */
    if((gid1 = H5Gcreate2(fid, "group1", H5P_DEFAULT, gcpl1, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Lcreate_soft("/", gid1, "link1", H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR
    if(H5Lcreate_soft("/", gid1, "link2", H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR
    if(H5Lcreate_soft("/", gid1, "link3", H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Close file and group */
    if(H5Gclose(gid1) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Verify the filter has been applied */
    if(link_filter_state != LFS_ENCODED) TEST_ERROR

    /* Reopen file and group */
    if((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR
    if((gid1 = H5Gopen2(fid, "group1", H5P_DEFAULT)) < 0) TEST_ERROR

    /* Retrieve gcpl, verify number of filters */
    if((gcpl2 = H5Gget_create_plist(gid1)) < 0) TEST_ERROR
    if(H5Pget_nfilters(gcpl2) != nfilters) TEST_ERROR
    if(H5Pclose(gcpl2) < 0) TEST_ERROR

    /* Now try copying gcpl1, and verify number of filters */
    if((gcpl2 = H5Pcopy(gcpl1)) < 0) TEST_ERROR
    if(H5Pget_nfilters(gcpl2) != nfilters) TEST_ERROR
    if(H5Pclose(gcpl2) < 0) TEST_ERROR

    /* Add another soft link */
    if(H5Lcreate_soft("/", gid1, "link4", H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Copy the group */
    if(H5Ocopy(fid, "group1", fid, "group2", H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR
    if((gid2 = H5Gopen2(fid, "group2", H5P_DEFAULT)) <0) TEST_ERROR

    /* Verify that all links have been copied */
    if(H5Lexists(gid2, "link1", H5P_DEFAULT) != TRUE) TEST_ERROR
    if(H5Lexists(gid2, "link2", H5P_DEFAULT) != TRUE) TEST_ERROR
    if(H5Lexists(gid2, "link3", H5P_DEFAULT) != TRUE) TEST_ERROR
    if(H5Lexists(gid2, "link4", H5P_DEFAULT) != TRUE) TEST_ERROR

    /* Retrieve gcpl, verify number of filters */
    if((gcpl2 = H5Gget_create_plist(gid2)) < 0) TEST_ERROR
    if(H5Pget_nfilters(gcpl2) != nfilters) TEST_ERROR

    /* Delete 3 links to force the group back into compact mode */
    if(H5Ldelete(gid1, "link2", H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Ldelete(gid1, "link3", H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Ldelete(gid1, "link4", H5P_DEFAULT) < 0) TEST_ERROR

    /* Close file and groups */
    if(H5Gclose(gid1) < 0) TEST_ERROR
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Reset link filter state */
    link_filter_state = LFS_INIT;

    /* Reopen file and group, add 2 links */
    if((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR
    if((gid1 = H5Gopen2(fid, "group1", H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Lcreate_soft("/", gid1, "link2", H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR
    if(H5Lcreate_soft("/", gid1, "link3", H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Close file and group */
    if(H5Gclose(gid1) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Verify that the filter was reapplied */
    if(link_filter_state != LFS_ENCODED) TEST_ERROR

    /* Test H5Pget_filter_by_id2 and H5Pget_filter2 */
    if(H5Pget_filter_by_id2(gcpl2, H5Z_FILTER_RESERVED + 42, &flags_out,
            &cd_nelmts, &cd_value_out, (size_t)24, name_out, &filter_config_out) < 0)
        TEST_ERROR
    if(flags_out != 0 || cd_value_out != cd_value
            || HDstrcmp(filter_class.name, name_out)
            || filter_config_out != (H5Z_FILTER_CONFIG_ENCODE_ENABLED
            | H5Z_FILTER_CONFIG_DECODE_ENABLED))
        TEST_ERROR
    if(H5Pget_filter2(gcpl2, (unsigned)(nfilters - 1), &flags_out, &cd_nelmts,
            &cd_value_out, (size_t)24, name_out, &filter_config_out) < 0)
        TEST_ERROR
    if(flags_out != 0 || cd_value_out != cd_value
            || HDstrcmp(filter_class.name, name_out)
            || filter_config_out != (H5Z_FILTER_CONFIG_ENCODE_ENABLED
            | H5Z_FILTER_CONFIG_DECODE_ENABLED))
        TEST_ERROR

    /* Test H5Pmodify_filter */
    cd_value++;
    if(H5Pmodify_filter(gcpl2, H5Z_FILTER_RESERVED + 42, 0, (size_t)1, &cd_value) < 0)
        TEST_ERROR
    if(H5Pget_filter_by_id2(gcpl2, H5Z_FILTER_RESERVED + 42, &flags_out,
            &cd_nelmts, &cd_value_out, (size_t)24, name_out, &filter_config_out) < 0)
        TEST_ERROR
    if(flags_out != 0 || cd_value_out != cd_value
            || HDstrcmp(filter_class.name, name_out)
            || filter_config_out != (H5Z_FILTER_CONFIG_ENCODE_ENABLED
            | H5Z_FILTER_CONFIG_DECODE_ENABLED))
        TEST_ERROR

    /* Test H5Premove_filter */
    if(H5Premove_filter(gcpl2, H5Z_FILTER_RESERVED + 42) < 0) TEST_ERROR
    H5E_BEGIN_TRY {
        status = H5Pget_filter_by_id2(gcpl2, H5Z_FILTER_RESERVED + 42,
                &flags_out, &cd_nelmts, &cd_value_out, (size_t)24, name_out,
                &filter_config_out);
    } H5E_END_TRY
    if(status >= 0) TEST_ERROR

    /* Close remaining ids */
    if(H5Pclose(gcpl1) < 0) TEST_ERROR
    if(H5Pclose(gcpl2) < 0) TEST_ERROR

    /* Now create an object in the compressed group, creating intermediate
     * groups, to verify that the filter pipeline is inherited for the groups
     * that are created along the way */
    /* Reopen file */
    if((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR

    /* Create lcpl, setting the "create intermediate groups" flag */
    if((lcpl = H5Pcreate(H5P_LINK_CREATE)) < 0) TEST_ERROR
    if(H5Pset_create_intermediate_group(lcpl, (unsigned)TRUE) < 0) TEST_ERROR

    /* Create new group, with missing intermediate groups, in compressed group */
    if((gid1 = H5Gcreate2(fid, "group1/group2/group3/group4", lcpl, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Close LCPL ID */
    if(H5Pclose(lcpl) < 0) TEST_ERROR

    /* Verify that new group doesn't have filters */
    if((gcpl1 = H5Gget_create_plist(gid1)) < 0) TEST_ERROR
    if(H5Pget_nfilters(gcpl1) != 0) TEST_ERROR

    /* Close group & GCPL IDs */
    if(H5Pclose(gcpl1) < 0) TEST_ERROR
    if(H5Gclose(gid1) < 0) TEST_ERROR

    /* Open intermediate groups that were created and verify that they have filters */
    if((gid1 = H5Gopen2(fid, "group1/group2", H5P_DEFAULT)) < 0) TEST_ERROR
    if((gcpl1 = H5Gget_create_plist(gid1)) < 0) TEST_ERROR
    if(H5Pget_nfilters(gcpl1) != nfilters) TEST_ERROR
    if(H5Pclose(gcpl1) < 0) TEST_ERROR
    if(H5Gclose(gid1) < 0) TEST_ERROR
    if((gid1 = H5Gopen2(fid, "group1/group2/group3", H5P_DEFAULT)) < 0) TEST_ERROR
    if((gcpl1 = H5Gget_create_plist(gid1)) < 0) TEST_ERROR
    if(H5Pget_nfilters(gcpl1) != nfilters) TEST_ERROR
    if(H5Pclose(gcpl1) < 0) TEST_ERROR
    if(H5Gclose(gid1) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Now create the same file with and without deflate, and verify that the
     * file size is smaller with deflate */
    /* But only if the deflate filter is available */
    if((tri_ret = H5Zfilter_avail(H5Z_FILTER_DEFLATE)) < 0) TEST_ERROR
    if(tri_ret) {
        h5_stat_size_t filesize_filtered;
        h5_stat_size_t filesize_unfiltered;

        /* Create gcpl, force use of dense storage */
        if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0) TEST_ERROR
        if(H5Pset_link_phase_change(fcpl, 2, 2) < 0) TEST_ERROR

        /* Create file */
        if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
            TEST_ERROR

        /* Create links in file */
        if(H5Lcreate_soft("/", fid, "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", H5P_DEFAULT, H5P_DEFAULT) < 0)
            TEST_ERROR
        if(H5Lcreate_soft("/", fid, "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb", H5P_DEFAULT, H5P_DEFAULT) < 0)
            TEST_ERROR
        if(H5Lcreate_soft("/", fid, "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc", H5P_DEFAULT, H5P_DEFAULT) < 0)
            TEST_ERROR

        /* Close file, get file size */
        if(H5Fclose(fid) < 0) TEST_ERROR
        filesize_unfiltered = h5_get_file_size(filename, fapl);

        /* Set deflate fitler */
        if(H5Pset_deflate(fcpl, 6) < 0) TEST_ERROR

        /* Recreate the same file with the deflate filter */
        if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
            TEST_ERROR
        if(H5Lcreate_soft("/", fid, "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", H5P_DEFAULT, H5P_DEFAULT) < 0)
            TEST_ERROR
        if(H5Lcreate_soft("/", fid, "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb", H5P_DEFAULT, H5P_DEFAULT) < 0)
            TEST_ERROR
        if(H5Lcreate_soft("/", fid, "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc", H5P_DEFAULT, H5P_DEFAULT) < 0)
            TEST_ERROR

        /* Close file, get file size */
        if(H5Fclose(fid) < 0) TEST_ERROR
        filesize_filtered = h5_get_file_size(filename, fapl);

        /* Check that the file size is smaller with the filter */
        if((double)filesize_filtered
                > ((double)filesize_unfiltered * FILTER_FILESIZE_MAX_FRACTION))
            TEST_ERROR

        /* Close */
        if(H5Pclose(fcpl) < 0) TEST_ERROR
    } /* end if */

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Gclose(gid1);
        H5Gclose(gid2);
        H5Fclose(fid);
        H5Pclose(lcpl);
        H5Pclose(gcpl1);
        H5Pclose(gcpl2);
        H5Pclose(fcpl);
    } H5E_END_TRY;
    return -1;
} /* end link_filters() */


/*-------------------------------------------------------------------------
 * Function:    obj_exists
 *
 * Purpose:     Test the 'object exists' routine
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, February 2, 2010
 *
 *-------------------------------------------------------------------------
 */
static int
obj_exists(hid_t fapl, hbool_t new_format)
{
    char filename[NAME_BUF_SIZE];       /* Buffer for file name */
    hid_t fid = -1;     /* File ID */
    hid_t gid = -1;     /* Group ID */
    herr_t status;      /* Generic return value */

    if(new_format)
        TESTING("object exists (w/new group format)")
    else
        TESTING("object exists")

    /* Set up filename and create file*/
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

/* Hard links */
    /* Verify that H5Oexists_by_name() fails for non-existent link in root group */
    H5E_BEGIN_TRY {
        status = H5Oexists_by_name(fid, "foo", H5P_DEFAULT);
    } H5E_END_TRY
    if(status >= 0) TEST_ERROR

    /* Create a group, as a destination for testing */
    if((gid = H5Gcreate2(fid, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Verify that H5Oexists_by_name() succeeds for hard linked object */
    if(TRUE != H5Oexists_by_name(fid, "group", H5P_DEFAULT))
        TEST_ERROR

    /* Verify that H5Oexists_by_name() fails for non-existent link in non-root group */
    H5E_BEGIN_TRY {
        status = H5Oexists_by_name(fid, "group/foo", H5P_DEFAULT);
    } H5E_END_TRY
    if(status >= 0) TEST_ERROR


/* Soft links */
    /* Create dangling soft-link in root group */
    if(H5Lcreate_soft("dangle", fid, "soft1", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify that H5Oexists_by_name() returns FALSE for dangling soft-link in root group */
    if(FALSE != H5Oexists_by_name(fid, "soft1", H5P_DEFAULT))
        TEST_ERROR

    /* Create soft-link in root group that points to object */
    if(H5Lcreate_soft("/group", fid, "soft2", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify that H5Oexists_by_name() returns TRUE for soft-link in root group that points to object */
    if(TRUE != H5Oexists_by_name(fid, "soft2", H5P_DEFAULT))
        TEST_ERROR

    /* Create dangling soft-link in non-root group */
    if(H5Lcreate_soft("dangle", fid, "group/soft1", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify that H5Oexists_by_name() returns FALSE for dangling soft-link in non-root group */
    if(FALSE != H5Oexists_by_name(fid, "group/soft1", H5P_DEFAULT))
        TEST_ERROR

    /* Create soft-link in non-root group that points to object */
    if(H5Lcreate_soft("/group", fid, "group/soft2", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify that H5Oexists_by_name() returns TRUE for soft-link in non-root group that points to object */
    if(TRUE != H5Oexists_by_name(fid, "group/soft2", H5P_DEFAULT))
        TEST_ERROR


/* External links */
    /* Create dangling (file doesn't exist) external link in root group */
    if(H5Lcreate_external("nofile", "dangle", fid, "external1", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Verify that H5Oexists_by_name() returns FALSE for dangling (file doesn't exist) external link in root group */
    if(FALSE != H5Oexists_by_name(fid, "external1", H5P_DEFAULT))
        TEST_ERROR

    /* Create dangling (object doesn't exist) external link in root group */
    if(H5Lcreate_external(filename, "dangle", fid, "external2", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Verify that H5Oexists_by_name() returns FALSE for dangling (object doesn't exist) external link in root group */
    if(FALSE != H5Oexists_by_name(fid, "external2", H5P_DEFAULT))
        TEST_ERROR

    /* Create external link in root group that points to object */
    if(H5Lcreate_external(filename, "group", fid, "external3", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Verify that H5Oexists_by_name() returns TRUE for external link in root group that points to object */
    if(TRUE != H5Oexists_by_name(fid, "external3", H5P_DEFAULT))
        TEST_ERROR

    /* Create dangling (file doesn't exist) external link in non-root group */
    if(H5Lcreate_external("nofile", "dangle", fid, "group/external1", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Verify that H5Oexists_by_name() returns FALSE for dangling (file doesn't exist) external link in non-root group */
    if(FALSE != H5Oexists_by_name(fid, "group/external1", H5P_DEFAULT))
        TEST_ERROR

    /* Create dangling (object doesn't exist) external link in non-root group */
    if(H5Lcreate_external(filename, "dangle", fid, "group/external2", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Verify that H5Oexists_by_name() returns FALSE for dangling (object doesn't exist) external link in non-root group */
    if(FALSE != H5Oexists_by_name(fid, "group/external2", H5P_DEFAULT))
        TEST_ERROR

    /* Create external link in non-root group that points to object */
    if(H5Lcreate_external(filename, "group", fid, "group/external3", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Verify that H5Oexists_by_name() returns TRUE for external link in non-root group that points to object */
    if(TRUE != H5Oexists_by_name(fid, "group/external3", H5P_DEFAULT))
        TEST_ERROR


/* Soft->External links */
    /* Create soft-link in root group that points to dangling (file doesn't exist) external link */
    if(H5Lcreate_soft("external1", fid, "soft-elink1", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify that H5Oexists_by_name() returns FALSE */
    if(FALSE != H5Oexists_by_name(fid, "soft-elink1", H5P_DEFAULT))
        TEST_ERROR

    /* Create soft-link in root group that points to dangling (object doesn't exist) external link */
    if(H5Lcreate_soft("external2", fid, "soft-elink2", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify that H5Oexists_by_name() returns FALSE */
    if(FALSE != H5Oexists_by_name(fid, "soft-elink2", H5P_DEFAULT))
        TEST_ERROR

    /* Create soft-link in root group that points to external link that points to object */
    if(H5Lcreate_soft("external3", fid, "soft-elink3", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify that H5Oexists_by_name() returns TRUE */
    if(TRUE != H5Oexists_by_name(fid, "soft-elink3", H5P_DEFAULT))
        TEST_ERROR

    /* Create soft-link in root group that points to dangling (file doesn't exist) external link in non-root group */
    if(H5Lcreate_soft("group/external1", fid, "soft-elink4", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify that H5Oexists_by_name() returns FALSE */
    if(FALSE != H5Oexists_by_name(fid, "soft-elink4", H5P_DEFAULT))
        TEST_ERROR

    /* Create soft-link in root group that points to dangling (object doesn't exist) external link in non-root group */
    if(H5Lcreate_soft("group/external2", fid, "soft-elink5", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify that H5Oexists_by_name() returns FALSE */
    if(FALSE != H5Oexists_by_name(fid, "soft-elink5", H5P_DEFAULT))
        TEST_ERROR

    /* Create soft-link in root group that points to external link in non-root group that points to object */
    if(H5Lcreate_soft("group/external3", fid, "soft-elink6", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify that H5Oexists_by_name() returns TRUE */
    if(TRUE != H5Oexists_by_name(fid, "soft-elink6", H5P_DEFAULT))
        TEST_ERROR

    /* Create soft-link in non-root group that points to dangling (file doesn't exist) external link */
    if(H5Lcreate_soft("/external1", fid, "group/soft-elink1", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify that H5Oexists_by_name() returns FALSE */
    if(FALSE != H5Oexists_by_name(fid, "group/soft-elink1", H5P_DEFAULT))
        TEST_ERROR

    /* Create soft-link in non-root group that points to dangling (object doesn't exist) external link */
    if(H5Lcreate_soft("/external2", fid, "group/soft-elink2", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify that H5Oexists_by_name() returns FALSE */
    if(FALSE != H5Oexists_by_name(fid, "group/soft-elink2", H5P_DEFAULT))
        TEST_ERROR

    /* Create soft-link in non-root group that points to external link that points to object */
    if(H5Lcreate_soft("/external3", fid, "group/soft-elink3", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify that H5Oexists_by_name() returns TRUE */
    if(TRUE != H5Oexists_by_name(fid, "group/soft-elink3", H5P_DEFAULT))
        TEST_ERROR

    /* Create soft-link in non-root group that points to dangling (file doesn't exist) external link in non-root group */
    if(H5Lcreate_soft("/group/external1", fid, "group/soft-elink4", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify that H5Oexists_by_name() returns FALSE */
    if(FALSE != H5Oexists_by_name(fid, "group/soft-elink4", H5P_DEFAULT))
        TEST_ERROR

    /* Create soft-link in non-root group that points to dangling (object doesn't exist) external link in non-root group */
    if(H5Lcreate_soft("/group/external2", fid, "group/soft-elink5", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify that H5Oexists_by_name() returns FALSE */
    if(FALSE != H5Oexists_by_name(fid, "group/soft-elink5", H5P_DEFAULT))
        TEST_ERROR

    /* Create soft-link in non-root group that points to external link in non-root group that points to object */
    if(H5Lcreate_soft("/group/external3", fid, "group/soft-elink6", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Verify that H5Oexists_by_name() returns TRUE */
    if(TRUE != H5Oexists_by_name(fid, "group/soft-elink6", H5P_DEFAULT))
        TEST_ERROR


/* External->Soft links */
    /* Create external link in root group that points to dangling soft link in root group */
    if(H5Lcreate_external(filename, "soft1", fid, "elink-soft1", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Verify that H5Oexists_by_name() returns FALSE */
    if(FALSE != H5Oexists_by_name(fid, "elink-soft1", H5P_DEFAULT))
        TEST_ERROR

    /* Create external link in root group that points to soft link in root group that points to object */
    if(H5Lcreate_external(filename, "soft2", fid, "elink-soft2", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Verify that H5Oexists_by_name() returns TRUE */
    if(TRUE != H5Oexists_by_name(fid, "elink-soft2", H5P_DEFAULT))
        TEST_ERROR

    /* Create external link in root group that points to dangling soft link in non-root group */
    if(H5Lcreate_external(filename, "group/soft1", fid, "elink-soft3", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Verify that H5Oexists_by_name() returns FALSE */
    if(FALSE != H5Oexists_by_name(fid, "elink-soft3", H5P_DEFAULT))
        TEST_ERROR

    /* Create external link in root group that points to soft link in root group that points to object */
    if(H5Lcreate_external(filename, "group/soft2", fid, "elink-soft4", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Verify that H5Oexists_by_name() returns TRUE */
    if(TRUE != H5Oexists_by_name(fid, "elink-soft4", H5P_DEFAULT))
        TEST_ERROR

    /* Create external link in non-root group that points to dangling soft link in root group */
    if(H5Lcreate_external(filename, "soft1", fid, "group/elink-soft1", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Verify that H5Oexists_by_name() returns FALSE */
    if(FALSE != H5Oexists_by_name(fid, "group/elink-soft1", H5P_DEFAULT))
        TEST_ERROR

    /* Create external link in non-root group that points to soft link in root group that points to object */
    if(H5Lcreate_external(filename, "soft2", fid, "group/elink-soft2", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Verify that H5Oexists_by_name() returns TRUE */
    if(TRUE != H5Oexists_by_name(fid, "group/elink-soft2", H5P_DEFAULT))
        TEST_ERROR

    /* Create external link in non-root group that points to dangling soft link in non-root group */
    if(H5Lcreate_external(filename, "group/soft1", fid, "group/elink-soft3", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Verify that H5Oexists_by_name() returns FALSE */
    if(FALSE != H5Oexists_by_name(fid, "group/elink-soft3", H5P_DEFAULT))
        TEST_ERROR

    /* Create external link in non-root group that points to soft link in non-root group that points to object */
    if(H5Lcreate_external(filename, "group/soft2", fid, "group/elink-soft4", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Verify that H5Oexists_by_name() returns TRUE */
    if(TRUE != H5Oexists_by_name(fid, "group/elink-soft4", H5P_DEFAULT))
        TEST_ERROR


    /* Close file created */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Gclose(gid);
        H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end obj_exists() */


/*-------------------------------------------------------------------------
 * Function:    corder_create_empty
 *
 * Purpose:     Create an empty group with creation order indices
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, October 30, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
corder_create_empty(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	group_id = (-1);	/* Group ID */
    hid_t       gcpl_id = (-1); 	/* Group creation property list ID */
    unsigned    crt_order_flags;   	/* Status of creation order info for GCPL */
    herr_t      ret;                    /* Generic return value */
    char        filename[NAME_BUF_SIZE];/* File name */

    TESTING("creating empty group with creation order indexing")

    /* Create file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create group creation property list */
    if((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0) TEST_ERROR

    /* Set creation order indexing on group */
    if(H5Pget_link_creation_order(gcpl_id, &crt_order_flags) < 0) TEST_ERROR
    if(crt_order_flags != 0) TEST_ERROR

    /* Setting invalid combination of a group order creation order indexing on should fail */
    H5E_BEGIN_TRY {
        ret = H5Pset_link_creation_order(gcpl_id, H5P_CRT_ORDER_INDEXED);
    } H5E_END_TRY;
    if(ret > 0) {
	H5_FAILED();
	puts("    H5Pset_link_create_order() should have failed for a creation order index with no tracking.");
	TEST_ERROR
    } /* end if */

    /* Set creation order tracking & indexing on group */
    if(H5Pget_link_creation_order(gcpl_id, &crt_order_flags) < 0) TEST_ERROR
    if(crt_order_flags != 0) TEST_ERROR
    if(H5Pset_link_creation_order(gcpl_id, (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED)) < 0) TEST_ERROR
    if(H5Pget_link_creation_order(gcpl_id, &crt_order_flags) < 0) TEST_ERROR
    if(crt_order_flags != (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED)) TEST_ERROR

    /* Create group with creation order indexing & tracking on */
    if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check on group's status */
    if(H5G_is_empty_test(group_id) != TRUE) TEST_ERROR

    /* Close the group */
    if(H5Gclose(group_id) < 0) TEST_ERROR

    /* Close the group creation property list */
    if(H5Pclose(gcpl_id) < 0) TEST_ERROR

    /* Close the file */
    if(H5Fclose(file_id) < 0) TEST_ERROR


    /* Re-open the file */
    if((file_id = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* Open group created */
    if((group_id = H5Gopen2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check on group's status */
    if(H5G_is_empty_test(group_id) != TRUE) TEST_ERROR

    /* Retrieve group creation property list for group */
    if((gcpl_id = H5Gget_create_plist(group_id)) < 0) TEST_ERROR

    /* Query the group creation properties */
    if(H5Pget_link_creation_order(gcpl_id, &crt_order_flags) < 0) TEST_ERROR
    if(crt_order_flags != (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED)) TEST_ERROR

    /* Close the group creation property list */
    if(H5Pclose(gcpl_id) < 0) TEST_ERROR

    /* Close the group */
    if(H5Gclose(group_id) < 0) TEST_ERROR

    /* Close the file */
    if(H5Fclose(file_id) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(gcpl_id);
        H5Gclose(group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;
    return -1;
} /* end corder_create_empty() */


/*-------------------------------------------------------------------------
 * Function:    corder_create_compact
 *
 * Purpose:     Create a group with creation order indices and insert links
 *              in it when in compact form
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, October 30, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
corder_create_compact(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	group_id = (-1), group_id2 = (-1);	/* Group IDs */
    hid_t       gcpl_id = (-1); 	/* Group creation property list ID */
    unsigned    max_compact;            /* Maximum # of links to store in group compactly */
    unsigned    min_dense;              /* Minimum # of links to store in group "densely" */
    unsigned	nlinks;		        /* Number of link messages in group's header */
    char        objname[NAME_BUF_SIZE]; /* Object name */
    char        filename[NAME_BUF_SIZE];/* File name */
    unsigned    u;                      /* Local index variable */

    TESTING("creating compact group with creation order indexing")

    /* Create file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create group creation property list */
    if((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0) TEST_ERROR

    /* Set creation order tracking & indexing on group */
    if(H5Pset_link_creation_order(gcpl_id, (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED)) < 0) TEST_ERROR

    /* Create group with creation order indexing & tracking on */
    if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check on group's initial status */
    if(H5G_is_empty_test(group_id) != TRUE) TEST_ERROR
    if(H5G_has_stab_test(group_id) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(group_id) == TRUE) TEST_ERROR

    /* Query the group creation properties */
    if(H5Pget_link_phase_change(gcpl_id, &max_compact, &min_dense) < 0) TEST_ERROR

    /* Create several links, but keep group in compact form */
    for(u = 0; u < max_compact; u++) {
        sprintf(objname, "filler %u", u);
        if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
        if(H5Gclose(group_id2) < 0) TEST_ERROR

        /* Verify state of group */
        if(H5G_has_links_test(group_id, &nlinks) != TRUE) TEST_ERROR
        if(nlinks != (u + 1)) TEST_ERROR
        if(H5G_has_stab_test(group_id) == TRUE) TEST_ERROR
        if(H5G_is_new_dense_test(group_id) == TRUE) TEST_ERROR
    } /* end for */

    /* Close the group */
    if(H5Gclose(group_id) < 0) TEST_ERROR

    /* Close the group creation property list */
    if(H5Pclose(gcpl_id) < 0) TEST_ERROR

    /* Close the file */
    if(H5Fclose(file_id) < 0) TEST_ERROR


    /* Re-open the file */
    if((file_id = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* Open group created */
    if((group_id = H5Gopen2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify state of group */
    if(H5G_has_links_test(group_id, &nlinks) != TRUE) TEST_ERROR
    if(nlinks != max_compact) TEST_ERROR
    if(H5G_has_stab_test(group_id) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(group_id) == TRUE) TEST_ERROR

    /* Loop through links, checking their creation order values */
    /* (the name index is used, but the creation order value is in the same order) */
    for(u = 0; u < max_compact; u++) {
        H5L_info_t linfo;           /* Link information */

        /* Retrieve information for link */
        sprintf(objname, "filler %u", u);
        if(H5Lget_info(group_id, objname, &linfo, H5P_DEFAULT) < 0) TEST_ERROR

        /* Verify creation order of link */
        if(linfo.corder_valid != TRUE) TEST_ERROR
        if(linfo.corder != u) TEST_ERROR
    } /* end for */

    /* Close the group */
    if(H5Gclose(group_id) < 0) TEST_ERROR

    /* Close the file */
    if(H5Fclose(file_id) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(gcpl_id);
        H5Gclose(group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;
    return -1;
} /* end corder_create_compact() */


/*-------------------------------------------------------------------------
 * Function:    corder_create_dense
 *
 * Purpose:     Create a group with creation order indices and insert links
 *              in it until it's in dense form
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, October 30, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
corder_create_dense(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	group_id = (-1), group_id2 = (-1);	/* Group IDs */
    hid_t       gcpl_id = (-1); 	/* Group creation property list ID */
    unsigned    max_compact;            /* Maximum # of links to store in group compactly */
    unsigned    min_dense;              /* Minimum # of links to store in group "densely" */
    unsigned	nlinks;		        /* Number of link messages in group's header */
    hsize_t     name_count;             /* # of records in name index */
    hsize_t     corder_count;           /* # of records in creation order index */
    char        objname[NAME_BUF_SIZE]; /* Object name */
    char        filename[NAME_BUF_SIZE];/* File name */
    unsigned    u;                      /* Local index variable */

    TESTING("creating dense group with creation order indexing")

    /* Create file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create group creation property list */
    if((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0) TEST_ERROR

    /* Set creation order tracking & indexing on group */
    if(H5Pset_link_creation_order(gcpl_id, (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED)) < 0) TEST_ERROR

    /* Create group with creation order indexing & tracking on */
    if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check on group's initial status */
    if(H5G_is_empty_test(group_id) != TRUE) TEST_ERROR
    if(H5G_has_stab_test(group_id) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(group_id) == TRUE) TEST_ERROR

    /* Query the group creation properties */
    if(H5Pget_link_phase_change(gcpl_id, &max_compact, &min_dense) < 0) TEST_ERROR

    /* Create several links, up to limit of compact form */
    for(u = 0; u < max_compact; u++) {
        sprintf(objname, "filler %u", u);
        if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
        if(H5Gclose(group_id2) < 0) TEST_ERROR

        /* Verify state of group */
        if(H5G_has_links_test(group_id, &nlinks) != TRUE) TEST_ERROR
        if(nlinks != (u + 1)) TEST_ERROR
        if(H5G_has_stab_test(group_id) == TRUE) TEST_ERROR
        if(H5G_is_new_dense_test(group_id) == TRUE) TEST_ERROR
    } /* end for */

    /* Create another link, to push group into dense form */
    sprintf(objname, "filler %u", max_compact);
    if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(group_id2) < 0) TEST_ERROR

    /* Verify state of group */
    if(H5G_has_links_test(group_id, NULL) == TRUE) TEST_ERROR
    if(H5G_has_stab_test(group_id) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(group_id) != TRUE) TEST_ERROR

    /* Retrieve & verify # of records in the name & creation order indices */
    if(H5G_new_dense_info_test(group_id, &name_count, &corder_count) < 0) TEST_ERROR
    if(name_count != corder_count) TEST_ERROR

    /* Close the group */
    if(H5Gclose(group_id) < 0) TEST_ERROR

    /* Close the group creation property list */
    if(H5Pclose(gcpl_id) < 0) TEST_ERROR

    /* Close the file */
    if(H5Fclose(file_id) < 0) TEST_ERROR


    /* Re-open the file */
    if((file_id = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* Open group created */
    if((group_id = H5Gopen2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify state of group */
    if(H5G_has_links_test(group_id, NULL) == TRUE) TEST_ERROR
    if(H5G_has_stab_test(group_id) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(group_id) != TRUE) TEST_ERROR

    /* Loop through links, checking their creation order values */
    /* (the name index is used, but the creation order value is in the same order) */
    for(u = 0; u < (max_compact + 1); u++) {
        H5L_info_t linfo;           /* Link information */

        /* Retrieve information for link */
        sprintf(objname, "filler %u", u);
        if(H5Lget_info(group_id, objname, &linfo, H5P_DEFAULT) < 0) TEST_ERROR

        /* Verify creation order of link */
        if(linfo.corder_valid != TRUE) TEST_ERROR
        if(linfo.corder != u) TEST_ERROR
    } /* end for */

    /* Close the group */
    if(H5Gclose(group_id) < 0) TEST_ERROR

    /* Close the file */
    if(H5Fclose(file_id) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(gcpl_id);
        H5Gclose(group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;
    return -1;
} /* end corder_create_dense() */


/*-------------------------------------------------------------------------
 * Function:    corder_transition
 *
 * Purpose:     Create a group with creation order indices and verify correct
 *              transitions between compact & dense forms
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, October 30, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
corder_transition(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	group_id = (-1), group_id2 = (-1);	/* Group IDs */
    hid_t       gcpl_id = (-1); 	/* Group creation property list ID */
    unsigned    max_compact;            /* Maximum # of links to store in group compactly */
    unsigned    min_dense;              /* Minimum # of links to store in group "densely" */
    unsigned	nlinks;		        /* Number of link messages in group's header */
    hsize_t     name_count;             /* # of records in name index */
    hsize_t     corder_count;           /* # of records in creation order index */
    h5_stat_size_t       empty_size;             /* Size of empty file */
    h5_stat_size_t       file_size;              /* Size of file after operating on it */
    char        objname[NAME_BUF_SIZE]; /* Object name */
    char        filename[NAME_BUF_SIZE];/* File name */
    unsigned    u;                      /* Local index variable */

    TESTING("transitioning group with creation order indexing between dense & compact forms")

    /* Create file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create group creation property list */
    if((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0) FAIL_STACK_ERROR

    /* Set creation order tracking & indexing on group */
    if(H5Pset_link_creation_order(gcpl_id, (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED)) < 0) TEST_ERROR

    /* Query the group creation properties */
    if(H5Pget_link_phase_change(gcpl_id, &max_compact, &min_dense) < 0) FAIL_STACK_ERROR

    /* Increase estimated link info, so the group's object header is large
     *      enough to hold all the link messages in one chunk
     */
    if(H5Pset_est_link_info(gcpl_id, max_compact, CORDER_EST_ENTRY_LEN) < 0) TEST_ERROR

    /* Create group with creation order indexing & tracking on */
    if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close the group creation property list */
    if(H5Pclose(gcpl_id) < 0) FAIL_STACK_ERROR

    /* Close the group */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR

    /* Close the file */
    if(H5Fclose(file_id) < 0) FAIL_STACK_ERROR

    /* Get the size of the file with an empty group */
    if((empty_size = h5_get_file_size(filename, fapl)) < 0) TEST_ERROR


    /* Re-open the file */
    if((file_id = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) FAIL_STACK_ERROR

    /* Open group created */
    if((group_id = H5Gopen2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create several links, up to limit of compact form */
    for(u = 0; u < max_compact; u++) {
        sprintf(objname, "filler %u", u);
        if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
        if(H5Gclose(group_id2) < 0) TEST_ERROR
    } /* end for */

    /* Create another link, to push group into dense form */
    sprintf(objname, "filler %u", max_compact);
    if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(group_id2) < 0) TEST_ERROR

    /* Verify state of group */
    if(H5G_has_links_test(group_id, NULL) == TRUE) TEST_ERROR
    if(H5G_has_stab_test(group_id) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(group_id) != TRUE) TEST_ERROR

    /* Retrieve & verify # of records in the name & creation order indices */
    if(H5G_new_dense_info_test(group_id, &name_count, &corder_count) < 0) TEST_ERROR
    if(name_count != corder_count) TEST_ERROR

    /* Delete several links from group, until it resumes compact form */
    for(u = max_compact; u >= min_dense; u--) {
        sprintf(objname, "filler %u", u);
        if(H5Ldelete(group_id, objname, H5P_DEFAULT) < 0) TEST_ERROR

        /* Verify state of group */
        if(H5G_has_links_test(group_id, NULL) == TRUE) TEST_ERROR
        if(H5G_has_stab_test(group_id) == TRUE) TEST_ERROR
        if(H5G_is_new_dense_test(group_id) != TRUE) TEST_ERROR

        /* Retrieve & verify # of records in the name & creation order indices */
        if(H5G_new_dense_info_test(group_id, &name_count, &corder_count) < 0) TEST_ERROR
        if(name_count != corder_count) TEST_ERROR
    } /* end for */

    /* Delete another link, to push group into compact form */
    sprintf(objname, "filler %u", (min_dense - 1));
    if(H5Ldelete(group_id, objname, H5P_DEFAULT) < 0) TEST_ERROR

    /* Verify state of group */
    if(H5G_has_links_test(group_id, &nlinks) != TRUE) TEST_ERROR
    if(nlinks != (min_dense - 1)) TEST_ERROR
    if(H5G_has_stab_test(group_id) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(group_id) == TRUE) TEST_ERROR

    /* Re-add links to get back into dense form */
    for(u = (min_dense - 1); u < (max_compact + 1); u++) {
        sprintf(objname, "filler %u", u);
        if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
        if(H5Gclose(group_id2) < 0) TEST_ERROR
    } /* end for */

    /* Verify state of group */
    if(H5G_has_links_test(group_id, NULL) == TRUE) TEST_ERROR
    if(H5G_has_stab_test(group_id) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(group_id) != TRUE) TEST_ERROR

    /* Retrieve & verify # of records in the name & creation order indices */
    if(H5G_new_dense_info_test(group_id, &name_count, &corder_count) < 0) TEST_ERROR
    if(name_count != corder_count) TEST_ERROR

    /* Close the group */
    if(H5Gclose(group_id) < 0) TEST_ERROR

    /* Close the file */
    if(H5Fclose(file_id) < 0) TEST_ERROR


    /* Re-open the file */
    if((file_id = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR

    /* Open group created */
    if((group_id = H5Gopen2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify state of group */
    if(H5G_has_links_test(group_id, NULL) == TRUE) TEST_ERROR
    if(H5G_has_stab_test(group_id) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(group_id) != TRUE) TEST_ERROR

    /* Retrieve & verify # of records in the name & creation order indices */
    if(H5G_new_dense_info_test(group_id, &name_count, &corder_count) < 0) TEST_ERROR
    if(name_count != corder_count) TEST_ERROR

    /* Delete several links from group, until it resumes compact form */
    for(u = max_compact; u >= min_dense; u--) {
        sprintf(objname, "filler %u", u);
        if(H5Ldelete(group_id, objname, H5P_DEFAULT) < 0) TEST_ERROR

        /* Verify state of group */
        if(H5G_has_links_test(group_id, NULL) == TRUE) TEST_ERROR
        if(H5G_has_stab_test(group_id) == TRUE) TEST_ERROR
        if(H5G_is_new_dense_test(group_id) != TRUE) TEST_ERROR

        /* Retrieve & verify # of records in the name & creation order indices */
        if(H5G_new_dense_info_test(group_id, &name_count, &corder_count) < 0) TEST_ERROR
        if(name_count != corder_count) TEST_ERROR
    } /* end for */

    /* Delete another link, to push group into compact form */
    sprintf(objname, "filler %u", (min_dense - 1));
    if(H5Ldelete(group_id, objname, H5P_DEFAULT) < 0) TEST_ERROR

    /* Verify state of group */
    if(H5G_has_links_test(group_id, &nlinks) != TRUE) TEST_ERROR
    if(nlinks != (min_dense - 1)) TEST_ERROR
    if(H5G_has_stab_test(group_id) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(group_id) == TRUE) TEST_ERROR

    /* Re-add links to get back into dense form */
    for(u = (min_dense - 1); u < (max_compact + 1); u++) {
        sprintf(objname, "filler %u", u);
        if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
        if(H5Gclose(group_id2) < 0) TEST_ERROR
    } /* end for */

    /* Verify state of group */
    if(H5G_has_links_test(group_id, NULL) == TRUE) TEST_ERROR
    if(H5G_has_stab_test(group_id) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(group_id) != TRUE) TEST_ERROR

    /* Retrieve & verify # of records in the name & creation order indices */
    if(H5G_new_dense_info_test(group_id, &name_count, &corder_count) < 0) TEST_ERROR
    if(name_count != corder_count) TEST_ERROR

    /* Delete all the links */
    for(u = max_compact; u > 0; u--) {
        sprintf(objname, "filler %u", u);
        if(H5Ldelete(group_id, objname, H5P_DEFAULT) < 0) TEST_ERROR
    } /* end for */
    sprintf(objname, "filler %u", (unsigned)0);
    if(H5Ldelete(group_id, objname, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close the group */
    if(H5Gclose(group_id) < 0) TEST_ERROR

    /* Close the file */
    if(H5Fclose(file_id) < 0) TEST_ERROR

    /* Get the size of the file now */
    if((file_size = h5_get_file_size(filename, fapl)) < 0) TEST_ERROR
    if(file_size != empty_size) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(gcpl_id);
        H5Gclose(group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;
    return -1;
} /* end corder_transition() */


/*-------------------------------------------------------------------------
 * Function:    corder_delete
 *
 * Purpose:     Create a group with creation order indices and verify correct
 *              deletion of creation order index when the group is in dense
 *              storage form and the group is unlinked
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, October 30, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
corder_delete(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	group_id = (-1), group_id2 = (-1);	/* Group IDs */
    hid_t       gcpl_id = (-1); 	/* Group creation property list ID */
    unsigned    max_compact;            /* Maximum # of links to store in group compactly */
    unsigned    min_dense;              /* Minimum # of links to store in group "densely" */
    hsize_t     name_count;             /* # of records in name index */
    hsize_t     corder_count;           /* # of records in creation order index */
    hbool_t     reopen_file;            /* Whether to re-open the file before deleting group */
    h5_stat_size_t       empty_size;             /* Size of empty file */
    h5_stat_size_t       file_size;              /* Size of file after operating on it */
    char        objname[NAME_BUF_SIZE]; /* Object name */
    char        filename[NAME_BUF_SIZE];/* File name */
    unsigned    u;                      /* Local index variable */

    TESTING("deleting group with creation order indexing in dense form")

    /* Loop to leave file open when deleting group, or to close & re-open file
     *  before deleting group */
    for(reopen_file = FALSE; reopen_file <= TRUE; reopen_file++) {
        /* Create file */
        h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
        if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file_id) < 0) FAIL_STACK_ERROR

        /* Get the size of an empty file */
        if((empty_size = h5_get_file_size(filename, fapl)) < 0) TEST_ERROR

        /* Re-open the file */
        if((file_id = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) FAIL_STACK_ERROR

        /* Create group creation property list */
        if((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0) FAIL_STACK_ERROR

        /* Set creation order tracking & indexing on group */
        if(H5Pset_link_creation_order(gcpl_id, (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED)) < 0) TEST_ERROR

        /* Query the group creation properties */
        if(H5Pget_link_phase_change(gcpl_id, &max_compact, &min_dense) < 0) FAIL_STACK_ERROR

        /* Increase estimated link info, so the group's object header is large
         *      enough to hold all the link messages in one chunk
         */
        if(H5Pset_est_link_info(gcpl_id, max_compact, CORDER_EST_ENTRY_LEN) < 0) TEST_ERROR

        /* Create group with creation order indexing & tracking on */
        if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

        /* Close the group creation property list */
        if(H5Pclose(gcpl_id) < 0) FAIL_STACK_ERROR

        /* Create links until the group is in dense form */
        for(u = 0; u < max_compact * 2; u++) {
            sprintf(objname, "filler %u", u);
            if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
            if(H5Gclose(group_id2) < 0) FAIL_STACK_ERROR
        } /* end for */

        /* Verify state of group */
        if(H5G_has_links_test(group_id, NULL) == TRUE) TEST_ERROR
        if(H5G_has_stab_test(group_id) == TRUE) TEST_ERROR
        if(H5G_is_new_dense_test(group_id) != TRUE) TEST_ERROR

        /* Retrieve & verify # of records in the name & creation order indices */
        if(H5G_new_dense_info_test(group_id, &name_count, &corder_count) < 0) TEST_ERROR
        if(name_count != corder_count) TEST_ERROR

        /* Close the group */
        if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR

        /* Check for deleting group without re-opening file */
        if(!reopen_file)
            /* Delete the group with the creation order index */
            if(H5Ldelete(file_id, CORDER_GROUP_NAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

        /* Close the file */
        if(H5Fclose(file_id) < 0) FAIL_STACK_ERROR

        /* Check for deleting group after re-opening file */
        if(reopen_file) {
            /* Re-open the file */
            if((file_id = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) FAIL_STACK_ERROR

            /* Delete the group with the creation order index */
            if(H5Ldelete(file_id, CORDER_GROUP_NAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

            /* Close the file */
            if(H5Fclose(file_id) < 0) FAIL_STACK_ERROR
        } /* end if */

        /* Get the size of the file now */
        if((file_size = h5_get_file_size(filename, fapl)) < 0) TEST_ERROR
        if(file_size != empty_size) TEST_ERROR
    } /* end for */

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(gcpl_id);
        H5Gclose(group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;
    return -1;
} /* end corder_delete() */


/*-------------------------------------------------------------------------
 * Function:    link_info_by_idx_check
 *
 * Purpose:     Support routine for link_info_by_idx, to verify the link
 *              info is correct for a link
 *
 * Note:	This routine assumes that the links have been inserted in the
 *              group in alphabetical order.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
link_info_by_idx_check(hid_t group_id, const char *linkname, hsize_t n,
    hbool_t hard_link, hbool_t use_index)
{
    char tmpname[NAME_BUF_SIZE];        /* Temporary link name */
    char valname[NAME_BUF_SIZE];        /* Link value name */
    char tmpval[NAME_BUF_SIZE];         /* Temporary link value */
    H5L_info_t  linfo;                  /* Link info struct */

    /* Make link value for increasing/native order queries */
    sprintf(valname, "value %02u", (unsigned)n);

    /* Verify the link information for first link, in increasing creation order */
    HDmemset(&linfo, 0, sizeof(linfo));
    if(H5Lget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)0, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.corder != 0) TEST_ERROR

    /* Verify the link information for new link, in increasing creation order */
    HDmemset(&linfo, 0, sizeof(linfo));
    if(H5Lget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, n, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.corder != (int64_t)n) TEST_ERROR

    /* Verify value for new soft link, in increasing creation order */
    if(!hard_link) {
        HDmemset(tmpval, 0, (size_t)NAME_BUF_SIZE);
        if(H5Lget_val_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, n, tmpval, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
        if(HDstrcmp(valname, tmpval)) TEST_ERROR
    } /* end if */

    /* Verify the name for new link, in increasing creation order */
    HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
    if(H5Lget_name_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, n, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
    if(HDstrcmp(linkname, tmpname)) TEST_ERROR

    /* Don't test "native" order if there is no creation order index, since
     *  there's not a good way to easily predict the link's order in the name
     *  index.
     */
    if(use_index) {
        /* Verify the link information for first link, in native creation order (which is increasing) */
        HDmemset(&linfo, 0, sizeof(linfo));
        if(H5Lget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_NATIVE, (hsize_t)0, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
        if(linfo.corder != 0) TEST_ERROR

        /* Verify the link information for new link, in native creation order (which is increasing) */
        HDmemset(&linfo, 0, sizeof(linfo));
        if(H5Lget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_NATIVE, n, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
        if(linfo.corder != (int64_t)n) TEST_ERROR

        /* Verify value for new soft link, in native creation order (which is increasing) */
        if(!hard_link) {
            HDmemset(tmpval, 0, (size_t)NAME_BUF_SIZE);
            if(H5Lget_val_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_NATIVE, n, tmpval, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
            if(HDstrcmp(valname, tmpval)) TEST_ERROR
        } /* end if */

        /* Verify the name for new link, in native creation order (which is increasing) */
        HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
        if(H5Lget_name_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_NATIVE, n, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
        if(HDstrcmp(linkname, tmpname)) TEST_ERROR
    } /* end if */

    /* Verify the link information for first link, in decreasing creation order */
    HDmemset(&linfo, 0, sizeof(linfo));
    if(H5Lget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_DEC, n, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.corder != 0) TEST_ERROR

    /* Verify the link information for new link, in decreasing creation order */
    HDmemset(&linfo, 0, sizeof(linfo));
    if(H5Lget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_DEC, (hsize_t)0, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.corder != (int64_t)n) TEST_ERROR

    /* Verify value for new soft link, in decreasing creation order */
    if(!hard_link) {
        HDmemset(tmpval, 0, (size_t)NAME_BUF_SIZE);
        if(H5Lget_val_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_DEC, (hsize_t)0, tmpval, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
        if(HDstrcmp(valname, tmpval)) TEST_ERROR
    } /* end if */

    /* Verify the name for new link, in decreasing creation order */
    HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
    if(H5Lget_name_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_DEC, (hsize_t)0, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
    if(HDstrcmp(linkname, tmpname)) TEST_ERROR


    /* Verify the link information for first link, in increasing link name order */
    HDmemset(&linfo, 0, sizeof(linfo));
    if(H5Lget_info_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)0, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.corder != 0) TEST_ERROR

    /* Verify the link information for new link, in increasing link name order */
    HDmemset(&linfo, 0, sizeof(linfo));
    if(H5Lget_info_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_INC, n, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.corder != (int64_t)n) TEST_ERROR

    /* Verify value for new soft link, in increasing link name order */
    if(!hard_link) {
        HDmemset(tmpval, 0, (size_t)NAME_BUF_SIZE);
        if(H5Lget_val_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_INC, n, tmpval, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
        if(HDstrcmp(valname, tmpval)) TEST_ERROR
    } /* end if */

    /* Verify the name for new link, in increasing link name order */
    HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
    if(H5Lget_name_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_INC, n, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
    if(HDstrcmp(linkname, tmpname)) TEST_ERROR

    /* Don't test "native" order queries on link name order, since there's not
     *  a good way to easily predict the order of the links in the name index.
     */

    /* Verify the link information for first link, in decreasing link name order */
    HDmemset(&linfo, 0, sizeof(linfo));
    if(H5Lget_info_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_DEC, n, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.corder != 0) TEST_ERROR

    /* Verify the link information for new link, in decreasing link name order */
    HDmemset(&linfo, 0, sizeof(linfo));
    if(H5Lget_info_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_DEC, (hsize_t)0, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.corder != (int64_t)n) TEST_ERROR

    /* Verify value for new soft link, in decreasing link name order */
    if(!hard_link) {
        HDmemset(tmpval, 0, (size_t)NAME_BUF_SIZE);
        if(H5Lget_val_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_DEC, (hsize_t)0, tmpval, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
        if(HDstrcmp(valname, tmpval)) TEST_ERROR
    } /* end if */

    /* Verify the name for new link, in decreasing link name order */
    HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
    if(H5Lget_name_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_DEC, (hsize_t)0, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
    if(HDstrcmp(linkname, tmpname)) TEST_ERROR

    /* Success */
    return(0);

error:
    /* Failure */
    return(-1);
} /* end link_info_by_idx_check() */


/*-------------------------------------------------------------------------
 * Function:    link_info_by_idx
 *
 * Purpose:     Create a group with creation order indices and test querying
 *              info by index.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, November  6, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
link_info_by_idx(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	group_id = (-1);	/* Group ID */
    hid_t       gcpl_id = (-1); 	/* Group creation property list ID */
    hbool_t     hard_link;              /* Create hard or soft link? */
    hbool_t     use_index;              /* Use index on creation order values */
    unsigned    max_compact;            /* Maximum # of links to store in group compactly */
    unsigned    min_dense;              /* Minimum # of links to store in group "densely" */
    H5L_info_t  linfo;                  /* Link info struct */
    char        objname[NAME_BUF_SIZE]; /* Object name */
    char        valname[NAME_BUF_SIZE]; /* Link value name */
    char        filename[NAME_BUF_SIZE];/* File name */
    char        tmpname[NAME_BUF_SIZE]; /* Temporary link name */
    unsigned    u;                      /* Local index variable */
    herr_t      ret;                    /* Generic return value */

    /* Loop over creating hard or soft links */
    for(hard_link = FALSE; hard_link <= TRUE; hard_link++) {
        /* Loop over using index for creation order value */
        for(use_index = FALSE; use_index <= TRUE; use_index++) {
            if(hard_link) {
                if(use_index)
                    TESTING("querying info by index w/creation order index, using hard links")
                else
                    TESTING("querying info by index w/o creation order index, using hard links")
            } /* end if */
            else {
                if(use_index)
                    TESTING("querying info by index w/creation order index, using soft links")
                else
                    TESTING("querying info by index w/o creation order index, using soft links")
            } /* end else */

            /* Create file */
            h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
            if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

            /* Create group creation property list */
            if((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0) TEST_ERROR

            /* Set creation order tracking & indexing on group */
            if(H5Pset_link_creation_order(gcpl_id, (H5P_CRT_ORDER_TRACKED | (use_index ? H5P_CRT_ORDER_INDEXED : (unsigned)0))) < 0) TEST_ERROR

            /* Create group with creation order indexing & tracking on */
            if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) TEST_ERROR

            /* Query the group creation properties */
            if(H5Pget_link_phase_change(gcpl_id, &max_compact, &min_dense) < 0) TEST_ERROR

            /* Check for query on empty group */
            H5E_BEGIN_TRY {
                ret = H5Lget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)0, &linfo, H5P_DEFAULT);
            } H5E_END_TRY;
            if(ret >= 0) TEST_ERROR
            H5E_BEGIN_TRY {
                ret = H5Lget_name_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)0, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT);
            } H5E_END_TRY;
            if(ret >= 0) TEST_ERROR

            /* Create several links, up to limit of compact form */
            for(u = 0; u < max_compact; u++) {
                /* Make name for link */
                sprintf(objname, "filler %02u", u);

                /* Check for creating hard or soft link */
                if(hard_link) {
                    hid_t group_id2;	        /* Group ID */

                    /* Create hard link, with group object */
                    if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
                    if(H5Gclose(group_id2) < 0) TEST_ERROR
                } /* end if */
                else {
                    /* Make value for link */
                    sprintf(valname, "value %02u", u);

                    /* Create soft link */
                    if(H5Lcreate_soft(valname, group_id, objname, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
                } /* end else */

                /* Verify link information for new link */
                if(link_info_by_idx_check(group_id, objname, (hsize_t)u, hard_link, use_index) < 0) TEST_ERROR
            } /* end for */

            /* Verify state of group */
            if(H5G_has_links_test(group_id, NULL) != TRUE) TEST_ERROR

            /* Check for out of bound offset queries */
            H5E_BEGIN_TRY {
                ret = H5Lget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)u, &linfo, H5P_DEFAULT);
            } H5E_END_TRY;
            if(ret >= 0) TEST_ERROR
            H5E_BEGIN_TRY {
                ret = H5Lget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_DEC, (hsize_t)u, &linfo, H5P_DEFAULT);
            } H5E_END_TRY;
            if(ret >= 0) TEST_ERROR
            H5E_BEGIN_TRY {
                ret = H5Lget_name_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)u, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT);
            } H5E_END_TRY;
            if(ret >= 0) TEST_ERROR

            /* Create more links, to push group into dense form */
            for(; u < (max_compact * 2); u++) {
                /* Make name for link */
                sprintf(objname, "filler %02u", u);

                /* Check for creating hard or soft link */
                if(hard_link) {
                    hid_t group_id2;	        /* Group ID */

                    /* Create hard link, with group object */
                    if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
                    if(H5Gclose(group_id2) < 0) TEST_ERROR
                } /* end if */
                else {
                    /* Make value for link */
                    sprintf(valname, "value %02u", u);

                    /* Create soft link */
                    if(H5Lcreate_soft(valname, group_id, objname, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
                } /* end else */

                /* Verify state of group */
                if(H5G_is_new_dense_test(group_id) != TRUE) TEST_ERROR

                /* Verify link information for new link */
                if(link_info_by_idx_check(group_id, objname, (hsize_t)u, hard_link, use_index) < 0) TEST_ERROR
            } /* end for */

            /* Check for out of bound offset queries */
            H5E_BEGIN_TRY {
                ret = H5Lget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)u, &linfo, H5P_DEFAULT);
            } H5E_END_TRY;
            if(ret >= 0) TEST_ERROR
            H5E_BEGIN_TRY {
                ret = H5Lget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_DEC, (hsize_t)u, &linfo, H5P_DEFAULT);
            } H5E_END_TRY;
            if(ret >= 0) TEST_ERROR
            H5E_BEGIN_TRY {
                ret = H5Lget_name_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)u, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT);
            } H5E_END_TRY;
            if(ret >= 0) TEST_ERROR

            /* Close the group */
            if(H5Gclose(group_id) < 0) TEST_ERROR

            /* Close the group creation property list */
            if(H5Pclose(gcpl_id) < 0) TEST_ERROR

            /* Close the file */
            if(H5Fclose(file_id) < 0) TEST_ERROR

            PASSED();
        } /* end for */
    } /* end for */

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(gcpl_id);
        H5Gclose(group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;
    return -1;
} /* end link_info_by_idx() */


/*-------------------------------------------------------------------------
 * Function:    link_info_by_idx_old
 *
 * Purpose:     Create a old-format group and test querying
 *              info by index.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
link_info_by_idx_old(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	group_id = (-1), group_id2 = (-1);	/* Group IDs */
    hbool_t     hard_link;              /* Create hard or soft link? */
    H5L_info_t  linfo;                  /* Link info struct */
    char        objname[NAME_BUF_SIZE]; /* Object name */
    char        valname[NAME_BUF_SIZE]; /* Link value name */
    char        filename[NAME_BUF_SIZE];/* File name */
    haddr_t     objno[CORDER_NLINKS];   /* Addresses of the objects created */
    char        tmpname[NAME_BUF_SIZE]; /* Temporary link name */
    char        tmpval[NAME_BUF_SIZE];  /* Temporary link value */
    unsigned    u;                      /* Local index variable */
    herr_t      ret;                    /* Generic return value */

    /* Loop over creating hard or soft links */
    for(hard_link = FALSE; hard_link <= TRUE; hard_link++) {
        if(hard_link)
            TESTING("querying info by index in old-style group, using hard links")
        else
            TESTING("querying info by index in old-style group, using soft links")

        /* Create file */
        h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
        if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

        /* Create group to operate on */
        if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

        /* Create several links */
        for(u = 0; u < CORDER_NLINKS; u++) {
            /* Make name for link */
            sprintf(objname, "filler %02u", u);

            /* Check for creating hard or soft link */
            if(hard_link) {
                H5O_info_t oi;                  /* Buffer for querying object's info */

                /* Create group */
                if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

                /* Retrieve group's address on disk */
                if(H5Oget_info(group_id2, &oi) < 0) TEST_ERROR
                objno[u] = oi.addr;

                /* Close group */
                if(H5Gclose(group_id2) < 0) TEST_ERROR
            } /* end if */
            else {
                /* Make value for link */
                sprintf(valname, "value %02u", u);

                /* Create soft link */
                if(H5Lcreate_soft(valname, group_id, objname, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
            } /* end else */
        } /* end for */

        /* Verify link information for created links */
        for(u = 0; u < CORDER_NLINKS; u++) {
            unsigned dec_u = CORDER_NLINKS - (u + 1);       /* Decreasing mapped index */

            /* Make link name for increasing/native order queries */
            sprintf(objname, "filler %02u", u);

            /* Make link value for increasing/native order queries */
            sprintf(valname, "value %02u", u);

            /* Verify link information (in increasing order) */
            if(hard_link) {
                if(H5Lget_info_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)u, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
                if(H5F_addr_ne(linfo.u.address, objno[u])) TEST_ERROR
            } /* end if */
            else {
                if(H5Lget_val_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)u, tmpval, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
                if(HDstrcmp(valname, tmpval)) TEST_ERROR
            } /* end else */

            /* Verify link name (in increasing order) */
            if(H5Lget_name_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)u, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
            if(HDstrcmp(objname, tmpname)) TEST_ERROR


            /* Verify link information (in native order - native is increasing) */
            if(hard_link) {
                if(H5Lget_info_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_NATIVE, (hsize_t)u, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
                if(H5F_addr_ne(linfo.u.address, objno[u])) TEST_ERROR
            } /* end if */
            else {
                if(H5Lget_val_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_NATIVE, (hsize_t)u, tmpval, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
                if(HDstrcmp(valname, tmpval)) TEST_ERROR
            } /* end else */

            /* Verify link name (in native order - native is increasing) */
            if(H5Lget_name_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_NATIVE, (hsize_t)u, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
            if(HDstrcmp(objname, tmpname)) TEST_ERROR


            /* Make link name for decreasing order queries */
            sprintf(objname, "filler %02u", dec_u);

            /* Make link value for decreasing order queries */
            sprintf(valname, "value %02u", dec_u);

            /* Verify link information (in decreasing order) */
            if(hard_link) {
                if(H5Lget_info_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_DEC, (hsize_t)u, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
                if(H5F_addr_ne(linfo.u.address, objno[dec_u])) TEST_ERROR
            } /* end if */
            else {
                if(H5Lget_val_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_DEC, (hsize_t)u, tmpval, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
                if(HDstrcmp(valname, tmpval)) TEST_ERROR
            } /* end else */

            /* Verify link name (in decreasing order) */
            if(H5Lget_name_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_DEC, (hsize_t)u, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
            if(HDstrcmp(objname, tmpname)) TEST_ERROR
        } /* end for */

        /* Check for creation order index queries */
        H5E_BEGIN_TRY {
            ret = H5Lget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)u, &linfo, H5P_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR
        H5E_BEGIN_TRY {
            ret = H5Lget_name_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)u, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR

        /* Verify state of group */
        if(H5G_has_stab_test(group_id) != TRUE) TEST_ERROR

        /* Close the group */
        if(H5Gclose(group_id) < 0) TEST_ERROR

        /* Close the file */
        if(H5Fclose(file_id) < 0) TEST_ERROR

        PASSED();
    } /* end for */

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Gclose(group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;
    return -1;
} /* end link_info_by_idx_old() */


/*-------------------------------------------------------------------------
 * Function:    delete_by_idx
 *
 * Purpose:     Create a group with creation order indices and test deleting
 *              links by index.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November 14, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
delete_by_idx(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	group_id = (-1);	/* Group ID */
    hid_t       gcpl_id = (-1); 	/* Group creation property list ID */
    H5_index_t idx_type;               /* Type of index to operate on */
    H5_iter_order_t order;              /* Order within in the index */
    hbool_t     use_index;              /* Use index on creation order values */
    unsigned    max_compact;            /* Maximum # of links to store in group compactly */
    unsigned    min_dense;              /* Minimum # of links to store in group "densely" */
    H5L_info_t  linfo;                  /* Link info struct */
    char        objname[NAME_BUF_SIZE]; /* Object name */
    char        filename[NAME_BUF_SIZE];/* File name */
    char        tmpname[NAME_BUF_SIZE]; /* Temporary link name */
    unsigned    u;                      /* Local index variable */
    herr_t      ret;                    /* Generic return value */

    /* Loop over operating on different indices on link fields */
    for(idx_type = H5_INDEX_NAME; idx_type <= H5_INDEX_CRT_ORDER; H5_INC_ENUM(H5_index_t, idx_type)) {
        /* Loop over operating in different orders */
        for(order = H5_ITER_INC; order <=H5_ITER_DEC; H5_INC_ENUM(H5_iter_order_t, order)) {
            /* Loop over using index for creation order value */
            for(use_index = FALSE; use_index <= TRUE; use_index++) {
                /* Print appropriate test message */
                if(idx_type == H5_INDEX_CRT_ORDER) {
                    if(order == H5_ITER_INC) {
                        if(use_index)
                            TESTING("deleting links by creation order index in increasing order w/creation order index")
                        else
                            TESTING("deleting links by creation order index in increasing order w/o creation order index")
                    } /* end if */
                    else {
                        if(use_index)
                            TESTING("deleting links by creation order index in decreasing order w/creation order index")
                        else
                            TESTING("deleting links by creation order index in decreasing order w/o creation order index")
                    } /* end else */
                } /* end if */
                else {
                    if(order == H5_ITER_INC) {
                        if(use_index)
                            TESTING("deleting links by name index in increasing order w/creation order index")
                        else
                            TESTING("deleting links by name index in increasing order w/o creation order index")
                    } /* end if */
                    else {
                        if(use_index)
                            TESTING("deleting links by name index in decreasing order w/creation order index")
                        else
                            TESTING("deleting links by name index in decreasing order w/o creation order index")
                    } /* end else */
                } /* end else */

                /* Create file */
                h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
                if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

                /* Create group creation property list */
                if((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0) TEST_ERROR

                /* Set creation order tracking & indexing on group */
                if(H5Pset_link_creation_order(gcpl_id, (H5P_CRT_ORDER_TRACKED | (use_index ? H5P_CRT_ORDER_INDEXED : (unsigned)0))) < 0) TEST_ERROR

                /* Create group with creation order tracking on */
                if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) TEST_ERROR

                /* Query the group creation properties */
                if(H5Pget_link_phase_change(gcpl_id, &max_compact, &min_dense) < 0) TEST_ERROR


                /* Delete links from one end */


                /* Check for deletion on empty group */
                H5E_BEGIN_TRY {
                    ret = H5Ldelete_by_idx(group_id, ".", idx_type, order, (hsize_t)0, H5P_DEFAULT);
                } H5E_END_TRY;
                if(ret >= 0) TEST_ERROR

                /* Create several links, up to limit of compact form */
                for(u = 0; u < max_compact; u++) {
                    hid_t group_id2;	        /* Group ID */

                    /* Make name for link */
                    sprintf(objname, "filler %02u", u);

                    /* Create hard link, with group object */
                    if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
                    if(H5Gclose(group_id2) < 0) TEST_ERROR

                    /* Verify link information for new link */
                    if(link_info_by_idx_check(group_id, objname, (hsize_t)u, TRUE, use_index) < 0) TEST_ERROR
                } /* end for */

                /* Verify state of group (compact) */
                if(H5G_has_links_test(group_id, NULL) != TRUE) TEST_ERROR

                /* Check for out of bound deletion */
                H5E_BEGIN_TRY {
                    ret = H5Ldelete_by_idx(group_id, ".", idx_type, order, (hsize_t)u, H5P_DEFAULT);
                } H5E_END_TRY;
                if(ret >= 0) TEST_ERROR

                /* Delete links from compact group */
                for(u = 0; u < (max_compact - 1); u++) {
                    /* Delete first link in appropriate order */
                    if(H5Ldelete_by_idx(group_id, ".", idx_type, order, (hsize_t)0, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Verify the link information for first link in appropriate order */
                    HDmemset(&linfo, 0, sizeof(linfo));
                    if(H5Lget_info_by_idx(group_id, ".", idx_type, order, (hsize_t)0, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
                    if(order == H5_ITER_INC) {
                        if(linfo.corder != (u + 1)) TEST_ERROR
                    } /* end if */
                    else {
                        if(linfo.corder != (max_compact - (u + 2))) TEST_ERROR
                    } /* end else */

                    /* Verify the name for first link in appropriate order */
                    HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
                    if(H5Lget_name_by_idx(group_id, ".", idx_type, order, (hsize_t)0, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
                    if(order == H5_ITER_INC)
                        sprintf(objname, "filler %02u", (u + 1));
                    else
                        sprintf(objname, "filler %02u", (max_compact - (u + 2)));
                    if(HDstrcmp(objname, tmpname)) TEST_ERROR
                } /* end for */

                /* Delete last link */
                if(H5Ldelete_by_idx(group_id, ".", idx_type, order, (hsize_t)0, H5P_DEFAULT) < 0) TEST_ERROR

                /* Verify state of group (empty) */
                if(H5G_has_links_test(group_id, NULL) == TRUE) TEST_ERROR

                /* Create more links, to push group into dense form */
                for(u = 0; u < (max_compact * 2); u++) {
                    hid_t group_id2;	        /* Group ID */

                    /* Make name for link */
                    sprintf(objname, "filler %02u", u);

                    /* Create hard link, with group object */
                    if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
                    if(H5Gclose(group_id2) < 0) TEST_ERROR

                    /* Verify state of group (dense) */
                    if(u >= max_compact)
                        if(H5G_is_new_dense_test(group_id) != TRUE) TEST_ERROR

                    /* Verify link information for new link */
                    if(link_info_by_idx_check(group_id, objname, (hsize_t)u, TRUE, use_index) < 0) TEST_ERROR
                } /* end for */

                /* Check for out of bound deletion again */
                H5E_BEGIN_TRY {
                    ret = H5Ldelete_by_idx(group_id, ".", idx_type, order, (hsize_t)u, H5P_DEFAULT);
                } H5E_END_TRY;
                if(ret >= 0) TEST_ERROR

                /* Delete links from dense group, in appropriate order */
                for(u = 0; u < ((max_compact * 2) - 1); u++) {
                    /* Delete first link in appropriate order */
                    if(H5Ldelete_by_idx(group_id, ".", idx_type, order, (hsize_t)0, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Verify the link information for first link in appropriate order */
                    HDmemset(&linfo, 0, sizeof(linfo));
                    if(H5Lget_info_by_idx(group_id, ".", idx_type, order, (hsize_t)0, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
                    if(order == H5_ITER_INC) {
                        if(linfo.corder != (u + 1)) TEST_ERROR
                    } /* end if */
                    else {
                        if(linfo.corder != ((max_compact * 2) - (u + 2))) TEST_ERROR
                    } /* end else */

                    /* Verify the name for first link in appropriate order */
                    HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
                    if(H5Lget_name_by_idx(group_id, ".", idx_type, order, (hsize_t)0, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
                    if(order == H5_ITER_INC)
                        sprintf(objname, "filler %02u", (u + 1));
                    else
                        sprintf(objname, "filler %02u", ((max_compact * 2) - (u + 2)));
                    if(HDstrcmp(objname, tmpname)) TEST_ERROR
                } /* end for */

                /* Delete last link */
                if(H5Ldelete_by_idx(group_id, ".", idx_type, order, (hsize_t)0, H5P_DEFAULT) < 0) TEST_ERROR

                /* Verify state of group (empty) */
                if(H5G_has_links_test(group_id, NULL) == TRUE) TEST_ERROR
                if(H5G_is_new_dense_test(group_id) == TRUE) TEST_ERROR

                /* Check for deletion on empty group again */
                H5E_BEGIN_TRY {
                    ret = H5Ldelete_by_idx(group_id, ".", idx_type, order, (hsize_t)0, H5P_DEFAULT);
                } H5E_END_TRY;
                if(ret >= 0) TEST_ERROR


                /* Delete links in middle */


                /* Create more links, to push group into dense form */
                for(u = 0; u < (max_compact * 2); u++) {
                    hid_t group_id2;	        /* Group ID */

                    /* Make name for link */
                    sprintf(objname, "filler %02u", u);

                    /* Create hard link, with group object */
                    if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
                    if(H5Gclose(group_id2) < 0) TEST_ERROR

                    /* Verify state of group (dense) */
                    if(u >= max_compact)
                        if(H5G_is_new_dense_test(group_id) != TRUE) TEST_ERROR

                    /* Verify link information for new link */
                    if(link_info_by_idx_check(group_id, objname, (hsize_t)u, TRUE, use_index) < 0) TEST_ERROR
                } /* end for */

                /* Delete every other link from dense group, in appropriate order */
                for(u = 0; u < max_compact; u++) {
                    /* Delete link */
                    if(H5Ldelete_by_idx(group_id, ".", idx_type, order, (hsize_t)u, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Verify the link information for current link in appropriate order */
                    HDmemset(&linfo, 0, sizeof(linfo));
                    if(H5Lget_info_by_idx(group_id, ".", idx_type, order, (hsize_t)u, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
                    if(order == H5_ITER_INC) {
                        if(linfo.corder != ((u * 2) + 1)) TEST_ERROR
                    } /* end if */
                    else {
                        if(linfo.corder != ((max_compact * 2) - ((u * 2) + 2))) TEST_ERROR
                    } /* end else */

                    /* Verify the name for current link in appropriate order */
                    HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
                    if(H5Lget_name_by_idx(group_id, ".", idx_type, order, (hsize_t)u, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
                    if(order == H5_ITER_INC)
                        sprintf(objname, "filler %02u", ((u * 2) + 1));
                    else
                        sprintf(objname, "filler %02u", ((max_compact * 2) - ((u * 2) + 2)));
                    if(HDstrcmp(objname, tmpname)) TEST_ERROR
                } /* end for */

                /* Delete remaining links from dense group, in appropriate order */
                for(u = 0; u < (max_compact - 1); u++) {
                    /* Delete link */
                    if(H5Ldelete_by_idx(group_id, ".", idx_type, order, (hsize_t)0, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Verify the link information for first link in appropriate order */
                    HDmemset(&linfo, 0, sizeof(linfo));
                    if(H5Lget_info_by_idx(group_id, ".", idx_type, order, (hsize_t)0, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
                    if(order == H5_ITER_INC) {
                        if(linfo.corder != ((u * 2) + 3)) TEST_ERROR
                    } /* end if */
                    else {
                        if(linfo.corder != ((max_compact * 2) - ((u * 2) + 4))) TEST_ERROR
                    } /* end else */

                    /* Verify the name for first link in appropriate order */
                    HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
                    if(H5Lget_name_by_idx(group_id, ".", idx_type, order, (hsize_t)0, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
                    if(order == H5_ITER_INC)
                        sprintf(objname, "filler %02u", ((u * 2) + 3));
                    else
                        sprintf(objname, "filler %02u", ((max_compact * 2) - ((u * 2) + 4)));
                    if(HDstrcmp(objname, tmpname)) TEST_ERROR
                } /* end for */

                /* Delete last link */
                if(H5Ldelete_by_idx(group_id, ".", idx_type, order, (hsize_t)0, H5P_DEFAULT) < 0) TEST_ERROR

                /* Verify state of group (empty) */
                if(H5G_has_links_test(group_id, NULL) == TRUE) TEST_ERROR
                if(H5G_is_new_dense_test(group_id) == TRUE) TEST_ERROR



                /* Close the group */
                if(H5Gclose(group_id) < 0) TEST_ERROR

                /* Close the group creation property list */
                if(H5Pclose(gcpl_id) < 0) TEST_ERROR

                /* Close the file */
                if(H5Fclose(file_id) < 0) TEST_ERROR

                PASSED();
            } /* end for */
        } /* end for */
    } /* end for */

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(gcpl_id);
        H5Gclose(group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;
    return -1;
} /* end delete_by_idx() */


/*-------------------------------------------------------------------------
 * Function:    delete_by_idx_old
 *
 * Purpose:     Create a old-format group and test deleting
 *              links by index.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, November 15, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
delete_by_idx_old(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	group_id = (-1), group_id2 = (-1);	/* Group IDs */
    H5L_info_t  linfo;                  /* Link info struct */
    H5_iter_order_t order;              /* Order within in the index */
    char        objname[NAME_BUF_SIZE]; /* Object name */
    char        filename[NAME_BUF_SIZE];/* File name */
    haddr_t     objno[CORDER_NLINKS];   /* Addresses of the objects created */
    char        tmpname[NAME_BUF_SIZE]; /* Temporary link name */
    unsigned    u;                      /* Local index variable */
    herr_t      ret;                    /* Generic return value */

    /* Loop over operating in different orders */
    for(order = H5_ITER_INC; order <=H5_ITER_DEC; H5_INC_ENUM(H5_iter_order_t, order)) {
        /* Print test banner */
        if(order == H5_ITER_INC)
            TESTING("deleting links by index in increasing order in old-style group")
        else
            TESTING("deleting links by index in decreasing order in old-style group")

        /* Create file */
        h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
        if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

        /* Create group to operate on */
        if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR


        /* Delete links from one end */


        /* Check for deletion in empty group */
        H5E_BEGIN_TRY {
            ret = H5Ldelete_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)0, H5P_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR

        /* Create several links */
        for(u = 0; u < CORDER_NLINKS; u++) {
            H5O_info_t oi;                  /* Buffer for querying object's info */

            /* Make name for link */
            sprintf(objname, "filler %02u", u);

            /* Create group */
            if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

            /* Retrieve group's address on disk */
            if(H5Oget_info(group_id2, &oi) < 0) TEST_ERROR
            objno[u] = oi.addr;

            /* Close group */
            if(H5Gclose(group_id2) < 0) TEST_ERROR
        } /* end for */

        /* Check for bad index type deletion */
        H5E_BEGIN_TRY {
            ret = H5Ldelete_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, order, (hsize_t)0, H5P_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR

        /* Check for out of bounds deletion */
        H5E_BEGIN_TRY {
            ret = H5Ldelete_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)u, H5P_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR

        /* Delete links, in appropriate order */
        for(u = 0; u < (CORDER_NLINKS - 1); u++) {
            unsigned dec_u = CORDER_NLINKS - (u + 2);       /* Decreasing mapped index */

            /* Delete first link in appropriate order */
            if(H5Ldelete_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)0, H5P_DEFAULT) < 0) TEST_ERROR

            /* Verify the link information for first link in appropriate order */
            HDmemset(&linfo, 0, sizeof(linfo));
            if(H5Lget_info_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)0, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
            if(order == H5_ITER_INC) {
                if(H5F_addr_ne(linfo.u.address, objno[u + 1])) TEST_ERROR
            } /* end if */
            else {
                if(H5F_addr_ne(linfo.u.address, objno[dec_u])) TEST_ERROR
            } /* end else */

            /* Verify the name for first link in appropriate order */
            HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
            if(H5Lget_name_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)0, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
            if(order == H5_ITER_INC)
                sprintf(objname, "filler %02u", (u + 1));
            else
                sprintf(objname, "filler %02u", dec_u);
            if(HDstrcmp(objname, tmpname)) TEST_ERROR
        } /* end for */

        /* Delete last link */
        if(H5Ldelete_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)0, H5P_DEFAULT) < 0) TEST_ERROR

        /* Check for deletion in empty group (again) */
        H5E_BEGIN_TRY {
            ret = H5Ldelete_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)0, H5P_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR

        /* Verify state of group */
        if(H5G_has_stab_test(group_id) != TRUE) TEST_ERROR


        /* Delete links in middle */


        /* Create several links */
        for(u = 0; u < CORDER_NLINKS; u++) {
            H5O_info_t oi;                  /* Buffer for querying object's info */

            /* Make name for link */
            sprintf(objname, "filler %02u", u);

            /* Create group */
            if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

            /* Retrieve group's address on disk */
            if(H5Oget_info(group_id2, &oi) < 0) TEST_ERROR
            objno[u] = oi.addr;

            /* Close group */
            if(H5Gclose(group_id2) < 0) TEST_ERROR
        } /* end for */

        /* Delete every other link from group, in appropriate order */
        for(u = 0; u < (CORDER_NLINKS / 2); u++) {
            unsigned dec_u = CORDER_NLINKS - ((u * 2) + 2);       /* Decreasing mapped index */

            /* Delete link */
            if(H5Ldelete_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)u, H5P_DEFAULT) < 0) TEST_ERROR

            /* Verify the link information for current link in appropriate order */
            HDmemset(&linfo, 0, sizeof(linfo));
            if(H5Lget_info_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)u, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
            if(order == H5_ITER_INC) {
                if(H5F_addr_ne(linfo.u.address, objno[(u * 2) + 1])) TEST_ERROR
            } /* end if */
            else {
                if(H5F_addr_ne(linfo.u.address, objno[dec_u])) TEST_ERROR
            } /* end else */

            /* Verify the name for current link in appropriate order */
            HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
            if(H5Lget_name_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)u, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
            if(order == H5_ITER_INC)
                sprintf(objname, "filler %02u", ((u * 2) + 1));
            else
                sprintf(objname, "filler %02u", dec_u);
            if(HDstrcmp(objname, tmpname)) TEST_ERROR
        } /* end for */

        /* Delete remaining links from group, in appropriate order */
        for(u = 0; u < ((CORDER_NLINKS / 2) - 1); u++) {
            unsigned dec_u = CORDER_NLINKS - ((u * 2) + 4);       /* Decreasing mapped index */

            /* Delete link */
            if(H5Ldelete_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)0, H5P_DEFAULT) < 0) TEST_ERROR

            /* Verify the link information for first link in appropriate order */
            HDmemset(&linfo, 0, sizeof(linfo));
            if(H5Lget_info_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)0, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
            if(order == H5_ITER_INC) {
                if(H5F_addr_ne(linfo.u.address, objno[(u * 2) + 3])) TEST_ERROR
            } /* end if */
            else {
                if(H5F_addr_ne(linfo.u.address, objno[dec_u])) TEST_ERROR
            } /* end else */

            /* Verify the name for first link in appropriate order */
            HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
            if(H5Lget_name_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)0, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
            if(order == H5_ITER_INC)
                sprintf(objname, "filler %02u", ((u * 2) + 3));
            else
                sprintf(objname, "filler %02u", dec_u);
            if(HDstrcmp(objname, tmpname)) TEST_ERROR
        } /* end for */

        /* Delete last link */
        if(H5Ldelete_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)0, H5P_DEFAULT) < 0) TEST_ERROR

        /* Verify state of group */
        if(H5G_has_stab_test(group_id) != TRUE) TEST_ERROR

        /* Close the group */
        if(H5Gclose(group_id) < 0) TEST_ERROR

        /* Close the file */
        if(H5Fclose(file_id) < 0) TEST_ERROR

        PASSED();
    } /* end for */

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Gclose(group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;

    return -1;
} /* end delete_by_idx_old() */


/*-------------------------------------------------------------------------
 * Function:    link_iterate_cb
 *
 * Purpose:     Callback routine for iterating over links in group
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, November 20, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
link_iterate_cb(hid_t group_id, const char *link_name, const H5L_info_t *info,
    void *_op_data)
{
    link_iter_info_t *op_data = (link_iter_info_t *)_op_data;   /* User data */
    char objname[NAME_BUF_SIZE]; /* Object name */
    H5L_info_t my_info;         /* Local link info */

    /* Increment # of times the callback was called */
    op_data->ncalled++;

    /* Get the link information directly to compare */
    if(H5Lget_info(group_id, link_name, &my_info, H5P_DEFAULT) < 0)
        return(H5_ITER_ERROR);

    /* Check more things for link iteration (vs. group iteration) */
    if(info) {
        /* Check for correct order of iteration */
        /* (if we are operating in increasing or decreasing order) */
        if(op_data->order != H5_ITER_NATIVE)
            if(info->corder != op_data->curr)
                return(H5_ITER_ERROR);

        /* Compare link info structs */
        if(info->type != my_info.type)
            return(H5_ITER_ERROR);
        if(info->corder_valid != my_info.corder_valid)
            return(H5_ITER_ERROR);
        if(info->corder != my_info.corder)
            return(H5_ITER_ERROR);
        if(info->cset != my_info.cset)
            return(H5_ITER_ERROR);
        if(H5F_addr_ne(info->u.address, my_info.u.address))
            return(H5_ITER_ERROR);
    } /* end if */

    /* Verify name of link */
    sprintf(objname, "filler %02u", (unsigned)my_info.corder);
    if(HDstrcmp(link_name, objname))
        return(H5_ITER_ERROR);

    /* Check if we've visited this link before */
    if((size_t)op_data->curr >= op_data->max_visit)
        return(H5_ITER_ERROR);
    if(op_data->visited[op_data->curr])
        return(H5_ITER_ERROR);
    op_data->visited[op_data->curr] = TRUE;

    /* Advance to next value, in correct direction */
    if(op_data->order != H5_ITER_DEC)
        op_data->curr++;
    else
        op_data->curr--;

    /* Check for stopping in the middle of iterating */
    if(op_data->stop > 0)
        if(--op_data->stop == 0)
            return(CORDER_ITER_STOP);

    return(H5_ITER_CONT);
} /* end link_iterate_cb() */

#ifndef H5_NO_DEPRECATED_SYMBOLS

/*-------------------------------------------------------------------------
 * Function:    group_iterate_cb
 *
 * Purpose:     Callback routine for iterating over links in group with
 *              H5Giterate()
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, November 20, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
group_iterate_cb(hid_t group_id, const char *link_name, void *_op_data)
{
    return(link_iterate_cb(group_id, link_name, NULL, _op_data));
} /* end group_iterate_cb() */
#endif /* H5_NO_DEPRECATED_SYMBOLS */


/*-------------------------------------------------------------------------
 * Function:    link_iterate_fail_cb
 *
 * Purpose:     Callback routine for iterating over links in group that
 *              always returns failure
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, November 20, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
link_iterate_fail_cb(hid_t UNUSED group_id, const char UNUSED *link_name,
    const H5L_info_t UNUSED *info, void UNUSED *_op_data)
{
    return(H5_ITER_ERROR);
} /* end link_iterate_fail_cb() */


/*-------------------------------------------------------------------------
 * Function:    link_iterate_check
 *
 * Purpose:     Check iteration over links in a group
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, November 20, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
link_iterate_check(hid_t group_id, H5_index_t idx_type, H5_iter_order_t order,
    unsigned max_links, link_iter_info_t *iter_info)
{
    unsigned    v;                      /* Local index variable */
    hsize_t     skip;                   /* # of links to skip in group */
#ifndef H5_NO_DEPRECATED_SYMBOLS
    int         gskip;                  /* # of links to skip in group, with H5Giterate */
#endif /* H5_NO_DEPRECATED_SYMBOLS */
    herr_t      ret;                    /* Generic return value */

    /* Iterate over links in group */
    iter_info->nskipped = (unsigned)(skip = 0);
    iter_info->order = order;
    iter_info->stop = -1;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? 0 : (max_links - 1);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    if(H5Literate(group_id, idx_type, order, &skip, link_iterate_cb, iter_info) < 0) TEST_ERROR

    /* Verify that we visited all the links */
    if(skip != max_links) TEST_ERROR
    for(v = 0; v < max_links; v++)
        if(iter_info->visited[v] == FALSE) TEST_ERROR


#ifndef H5_NO_DEPRECATED_SYMBOLS
    /* Iterate over links in group, with H5Giterate */
    iter_info->nskipped = (unsigned)(gskip = 0);
    iter_info->order = order;
    iter_info->stop = -1;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? 0 : (max_links - 1);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    if(H5Giterate(group_id, ".", &gskip, group_iterate_cb, iter_info) < 0) TEST_ERROR

    /* Verify that we visited all the links */
    if(gskip != (int)max_links) TEST_ERROR
    for(v = 0; v < max_links; v++)
        if(iter_info->visited[v] == FALSE) TEST_ERROR
#endif /* H5_NO_DEPRECATED_SYMBOLS */


    /* Skip over some links in group */
    iter_info->nskipped = (unsigned)(skip = max_links / 2);
    iter_info->order = order;
    iter_info->stop = -1;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? skip : ((max_links - 1) - skip);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    if(H5Literate(group_id, idx_type, order, &skip, link_iterate_cb, iter_info) < 0) TEST_ERROR

    /* Verify that we visited all the links */
    if(skip != max_links) TEST_ERROR
    if(order == H5_ITER_INC) {
        for(v = 0; v < (max_links / 2); v++)
            if(iter_info->visited[v + (max_links / 2)] == FALSE) TEST_ERROR
    } /* end if */
    else if(order == H5_ITER_DEC) {
        for(v = 0; v < (max_links / 2); v++)
            if(iter_info->visited[v] == FALSE) TEST_ERROR
    } /* end if */
    else {
        unsigned nvisit = 0;        /* # of links visited */

        HDassert(order == H5_ITER_NATIVE);
        for(v = 0; v < max_links; v++)
            if(iter_info->visited[v] == TRUE)
                nvisit++;

        if(nvisit != (max_links / 2)) TEST_ERROR
    } /* end else */


#ifndef H5_NO_DEPRECATED_SYMBOLS
    /* Skip over some links in group, with H5Giterate */
    iter_info->nskipped = gskip = max_links / 2;
    iter_info->order = order;
    iter_info->stop = -1;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? (unsigned)gskip : ((max_links - 1) - gskip);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    if(H5Giterate(group_id, ".", &gskip, group_iterate_cb, iter_info) < 0) TEST_ERROR

    /* Verify that we visited all the links */
    if(gskip != (int)max_links) TEST_ERROR
    if(order == H5_ITER_INC) {
        for(v = 0; v < (max_links / 2); v++)
            if(iter_info->visited[v + (max_links / 2)] == FALSE) TEST_ERROR
    } /* end if */
    else if(order == H5_ITER_DEC) {
        for(v = 0; v < (max_links / 2); v++)
            if(iter_info->visited[v] == FALSE) TEST_ERROR
    } /* end if */
    else {
        unsigned nvisit = 0;        /* # of links visited */

        HDassert(order == H5_ITER_NATIVE);
        for(v = 0; v < max_links; v++)
            if(iter_info->visited[v] == TRUE)
                nvisit++;

        if(nvisit != (max_links / 2)) TEST_ERROR
    } /* end else */
#endif /* H5_NO_DEPRECATED_SYMBOLS */


    /* Iterate over links in group, stopping in the middle */
    iter_info->nskipped = (unsigned)(skip = 0);
    iter_info->order = order;
    iter_info->stop = 3;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? 0 : (max_links - 1);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    if((ret = H5Literate(group_id, idx_type, order, &skip, link_iterate_cb, iter_info)) < 0) TEST_ERROR
    if(ret != CORDER_ITER_STOP) TEST_ERROR
    if(iter_info->ncalled != 3) TEST_ERROR


#ifndef H5_NO_DEPRECATED_SYMBOLS
    /* Iterate over links in group, stopping in the middle, with H5Giterate() */
    iter_info->nskipped = gskip = 0;
    iter_info->order = order;
    iter_info->stop = 3;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? 0 : (max_links - 1);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    if((ret = H5Giterate(group_id, ".", &gskip, group_iterate_cb, iter_info)) < 0) TEST_ERROR
    if(ret != CORDER_ITER_STOP) TEST_ERROR
    if(iter_info->ncalled != 3) TEST_ERROR
#endif /* H5_NO_DEPRECATED_SYMBOLS */


    /* Check for iteration routine indicating failure */
    skip = 0;
    H5E_BEGIN_TRY {
        ret = H5Literate(group_id, idx_type, order, &skip, link_iterate_fail_cb, NULL);
    } H5E_END_TRY;
    if(ret >= 0) TEST_ERROR

    /* Success */
    return(0);

error:
    return(-1);
} /* end link_iterate_check() */


/*-------------------------------------------------------------------------
 * Function:    link_iterate
 *
 * Purpose:     Create a group with creation order indices and test iterating over
 *              links by index.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November 14, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
link_iterate(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	group_id = (-1);	/* Group ID */
    hid_t       gcpl_id = (-1); 	/* Group creation property list ID */
    H5_index_t idx_type;               /* Type of index to operate on */
    H5_iter_order_t order;              /* Order within in the index */
    hbool_t     use_index;              /* Use index on creation order values */
    unsigned    max_compact;            /* Maximum # of links to store in group compactly */
    unsigned    min_dense;              /* Minimum # of links to store in group "densely" */
    char        objname[NAME_BUF_SIZE]; /* Object name */
    char        filename[NAME_BUF_SIZE];/* File name */
    link_iter_info_t iter_info;         /* Iterator info */
    hbool_t     *visited = NULL;        /* Array of flags for visiting links */
    hsize_t     skip;                   /* # of links to skip in group */
    unsigned    u;                      /* Local index variable */
    herr_t      ret;                    /* Generic return value */

    /* Create group creation property list */
    if((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0) TEST_ERROR

    /* Query the group creation properties */
    if(H5Pget_link_phase_change(gcpl_id, &max_compact, &min_dense) < 0) TEST_ERROR

    /* Allocate the "visited link" array */
    iter_info.max_visit = max_compact * 2;
    if(NULL == (visited = (hbool_t *)HDmalloc(sizeof(hbool_t) * iter_info.max_visit))) TEST_ERROR
    iter_info.visited = visited;

    /* Loop over operating on different indices on link fields */
    for(idx_type = H5_INDEX_NAME; idx_type <= H5_INDEX_CRT_ORDER; H5_INC_ENUM(H5_index_t, idx_type)) {
        /* Loop over operating in different orders */
        for(order = H5_ITER_INC; order <=H5_ITER_NATIVE; H5_INC_ENUM(H5_iter_order_t, order)) {
            /* Loop over using index for creation order value */
            for(use_index = FALSE; use_index <= TRUE; use_index++) {
                /* Print appropriate test message */
                if(idx_type == H5_INDEX_CRT_ORDER) {
                    if(order == H5_ITER_INC) {
                        if(use_index)
                            TESTING("iterating over links by creation order index in increasing order w/creation order index")
                        else
                            TESTING("iterating over links by creation order index in increasing order w/o creation order index")
                    } /* end if */
                    else if(order == H5_ITER_DEC) {
                        if(use_index)
                            TESTING("iterating over links by creation order index in decreasing order w/creation order index")
                        else
                            TESTING("iterating over links by creation order index in decreasing order w/o creation order index")
                    } /* end else */
                    else {
                        HDassert(order == H5_ITER_NATIVE);
                        if(use_index)
                            TESTING("iterating over links by creation order index in native order w/creation order index")
                        else
                            TESTING("iterating over links by creation order index in native order w/o creation order index")
                    } /* end else */
                } /* end if */
                else {
                    if(order == H5_ITER_INC) {
                        if(use_index)
                            TESTING("iterating over links by name index in increasing order w/creation order index")
                        else
                            TESTING("iterating over links by name index in increasing order w/o creation order index")
                    } /* end if */
                    else if(order == H5_ITER_DEC) {
                        if(use_index)
                            TESTING("iterating over links by name index in decreasing order w/creation order index")
                        else
                            TESTING("iterating over links by name index in decreasing order w/o creation order index")
                    } /* end else */
                    else {
                        HDassert(order == H5_ITER_NATIVE);
                        if(use_index)
                            TESTING("iterating over links by name index in native order w/creation order index")
                        else
                            TESTING("iterating over links by name index in native order w/o creation order index")
                    } /* end else */
                } /* end else */

                /* Create file */
                h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
                if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

                /* Set creation order tracking & indexing on group */
                if(H5Pset_link_creation_order(gcpl_id, (H5P_CRT_ORDER_TRACKED | (use_index ? H5P_CRT_ORDER_INDEXED : (unsigned)0))) < 0) TEST_ERROR

                /* Create group with creation order tracking on */
                if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) TEST_ERROR


                /* Check for iteration on empty group */
                /* (should be OK) */
                if(H5Literate(group_id, idx_type, order, NULL, link_iterate_cb, NULL) < 0) TEST_ERROR

                /* Create several links, up to limit of compact form */
                for(u = 0; u < max_compact; u++) {
                    hid_t group_id2;	        /* Group ID */

                    /* Make name for link */
                    sprintf(objname, "filler %02u", u);

                    /* Create hard link, with group object */
                    if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
                    if(H5Gclose(group_id2) < 0) TEST_ERROR
                } /* end for */

                /* Verify state of group (compact) */
                if(H5G_has_links_test(group_id, NULL) != TRUE) TEST_ERROR

                /* Check for out of bound iteration on compact group */
                skip = (hsize_t)u;
                H5E_BEGIN_TRY {
                    ret = H5Literate(group_id, idx_type, order, &skip, link_iterate_cb, NULL);
                } H5E_END_TRY;
                if(ret >= 0) TEST_ERROR

                /* Test iteration over links in compact group */
                if(link_iterate_check(group_id, idx_type, order, u, &iter_info) < 0) TEST_ERROR


                /* Create more links, to push group into dense form */
                for(; u < (max_compact * 2); u++) {
                    hid_t group_id2;	        /* Group ID */

                    /* Make name for link */
                    sprintf(objname, "filler %02u", u);

                    /* Create hard link, with group object */
                    if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
                    if(H5Gclose(group_id2) < 0) TEST_ERROR
                } /* end for */

                /* Verify state of group (dense) */
                if(H5G_is_new_dense_test(group_id) != TRUE) TEST_ERROR

                /* Check for out of bound iteration on dense group */
                skip = (hsize_t)u;
                H5E_BEGIN_TRY {
                    ret = H5Literate(group_id, idx_type, order, &skip, link_iterate_cb, NULL);
                } H5E_END_TRY;
                if(ret >= 0) TEST_ERROR

                /* Test iteration over links in dense group */
                if(link_iterate_check(group_id, idx_type, order, u, &iter_info) < 0) TEST_ERROR


                /* Close the group */
                if(H5Gclose(group_id) < 0) TEST_ERROR

                /* Close the file */
                if(H5Fclose(file_id) < 0) TEST_ERROR

                PASSED();
            } /* end for */
        } /* end for */
    } /* end for */

    /* Close the group creation property list */
    if(H5Pclose(gcpl_id) < 0) TEST_ERROR

    /* Free resources */
    if(visited)
        HDfree(visited);

    return 0;

error:
    /* Free resources */
    H5E_BEGIN_TRY {
        H5Pclose(gcpl_id);
        H5Gclose(group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;

    if(visited)
        HDfree(visited);

    return -1;
} /* end link_iterate() */


/*-------------------------------------------------------------------------
 * Function:    link_iterate_old_cb
 *
 * Purpose:     Callback routine for iterating over [old] links in group
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, November 20, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
link_iterate_old_cb(hid_t group_id, const char *link_name, const H5L_info_t *info,
    void *_op_data)
{
    link_iter_info_t *op_data = (link_iter_info_t *)_op_data;   /* User data */
    char objname[NAME_BUF_SIZE]; /* Object name */
    H5L_info_t my_info;         /* Local link info */

    /* Increment # of times the callback was called */
    op_data->ncalled++;

    /* Get the link information directly to compare */
    if(H5Lget_info(group_id, link_name, &my_info, H5P_DEFAULT) < 0)
        return(H5_ITER_ERROR);

    /* Check more things for link iteration (vs. group iteration) */
    if(info) {
        /* Compare link info structs */
        if(info->type != my_info.type)
            return(H5_ITER_ERROR);
        if(info->corder_valid != my_info.corder_valid)
            return(H5_ITER_ERROR);
        if(info->corder != my_info.corder)
            return(H5_ITER_ERROR);
        if(info->cset != my_info.cset)
            return(H5_ITER_ERROR);
        if(H5F_addr_ne(info->u.address, my_info.u.address))
            return(H5_ITER_ERROR);
    } /* end if */

    /* Verify name of link */
    sprintf(objname, "filler %02u", (info ? (unsigned)op_data->curr : (unsigned)((op_data->ncalled - 1) + op_data->nskipped)));
    if(HDstrcmp(link_name, objname))
        return(H5_ITER_ERROR);

    /* Check if we've visited this link before */
    if((size_t)op_data->curr >= op_data->max_visit)
        return(H5_ITER_ERROR);
    if(op_data->visited[op_data->curr])
        return(H5_ITER_ERROR);
    op_data->visited[op_data->curr] = TRUE;

    /* Advance to next value, in correct direction */
    if(op_data->order != H5_ITER_DEC)
        op_data->curr++;
    else
        op_data->curr--;

    /* Check for stopping in the middle of iterating */
    if(op_data->stop > 0)
        if(--op_data->stop == 0)
            return(CORDER_ITER_STOP);

    return(H5_ITER_CONT);
} /* end link_iterate_old_cb() */

#ifndef H5_NO_DEPRECATED_SYMBOLS

/*-------------------------------------------------------------------------
 * Function:    group_iterate_old_cb
 *
 * Purpose:     Callback routine for iterating over links in group with
 *              H5Giterate()
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, November 20, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
group_iterate_old_cb(hid_t group_id, const char *link_name, void *_op_data)
{
    return(link_iterate_old_cb(group_id, link_name, NULL, _op_data));
} /* end group_iterate_old_cb() */
#endif /* H5_NO_DEPRECATED_SYMBOLS */


/*-------------------------------------------------------------------------
 * Function:    link_iterate_old_check
 *
 * Purpose:     Check iteration over [old] links in a group
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, November 20, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
link_iterate_old_check(hid_t group_id, H5_iter_order_t order,
    unsigned max_links, link_iter_info_t *iter_info)
{
    unsigned    v;                      /* Local index variable */
    hsize_t     skip;                   /* # of links to skip in group */
#ifndef H5_NO_DEPRECATED_SYMBOLS
    int         gskip;                  /* # of links to skip in group, with H5Giterate */
#endif /* H5_NO_DEPRECATED_SYMBOLS */
    herr_t      ret;                    /* Generic return value */

    /* Iterate over links in group */
    iter_info->nskipped = (unsigned)(skip = 0);
    iter_info->order = order;
    iter_info->stop = -1;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? 0 : (max_links - 1);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    if(H5Literate(group_id, H5_INDEX_NAME, order, &skip, link_iterate_old_cb, iter_info) < 0) TEST_ERROR

    /* Verify that we visited all the links */
    if(skip != max_links) TEST_ERROR
    for(v = 0; v < max_links; v++)
        if(iter_info->visited[v] == FALSE) TEST_ERROR


#ifndef H5_NO_DEPRECATED_SYMBOLS
    /* Iterate over links in group, with H5Giterate */
    iter_info->nskipped = gskip = 0;
    iter_info->order = order;
    iter_info->stop = -1;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? 0 : (max_links - 1);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    if(H5Giterate(group_id, ".", &gskip, group_iterate_old_cb, iter_info) < 0) TEST_ERROR

    /* Verify that we visited all the links */
    if(gskip != (int)max_links) TEST_ERROR
    for(v = 0; v < max_links; v++)
        if(iter_info->visited[v] == FALSE) TEST_ERROR
#endif /* H5_NO_DEPRECATED_SYMBOLS */


    /* Skip over some links in group */
    iter_info->nskipped = (unsigned)(skip = max_links / 2);
    iter_info->order = order;
    iter_info->stop = -1;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? skip : ((max_links - 1) - skip);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    if(H5Literate(group_id, H5_INDEX_NAME, order, &skip, link_iterate_old_cb, iter_info) < 0) TEST_ERROR

    /* Verify that we visited all the links */
    if(skip != max_links) TEST_ERROR
    if(order == H5_ITER_INC) {
        for(v = 0; v < (max_links / 2); v++)
            if(iter_info->visited[v + (max_links / 2)] == FALSE) TEST_ERROR
    } /* end if */
    else if(order == H5_ITER_DEC) {
        for(v = 0; v < (max_links / 2); v++)
            if(iter_info->visited[v] == FALSE) TEST_ERROR
    } /* end if */
    else {
        unsigned nvisit = 0;        /* # of links visited */

        HDassert(order == H5_ITER_NATIVE);
        for(v = 0; v < max_links; v++)
            if(iter_info->visited[v] == TRUE)
                nvisit++;

        if(nvisit != (max_links / 2)) TEST_ERROR
    } /* end else */


#ifndef H5_NO_DEPRECATED_SYMBOLS
    /* Skip over some links in group, with H5Giterate */
    iter_info->nskipped = gskip = max_links / 2;
    iter_info->order = order;
    iter_info->stop = -1;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? (unsigned)gskip : ((max_links - 1) - gskip);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    if(H5Giterate(group_id, ".", &gskip, group_iterate_old_cb, iter_info) < 0) TEST_ERROR

    /* Verify that we visited all the links */
    if(gskip != (int)max_links) TEST_ERROR
    if(order == H5_ITER_INC) {
        for(v = 0; v < (max_links / 2); v++)
            if(iter_info->visited[v + (max_links / 2)] == FALSE) TEST_ERROR
    } /* end if */
    else if(order == H5_ITER_DEC) {
        for(v = 0; v < (max_links / 2); v++)
            if(iter_info->visited[v] == FALSE) TEST_ERROR
    } /* end if */
    else {
        unsigned nvisit = 0;        /* # of links visited */

        HDassert(order == H5_ITER_NATIVE);
        for(v = 0; v < max_links; v++)
            if(iter_info->visited[v] == TRUE)
                nvisit++;

        if(nvisit != (max_links / 2)) TEST_ERROR
    } /* end else */
#endif /* H5_NO_DEPRECATED_SYMBOLS */


    /* Iterate over links in group, stopping in the middle */
    iter_info->nskipped = (unsigned)(skip = 0);
    iter_info->order = order;
    iter_info->stop = 3;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? 0 : (max_links - 1);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    if((ret = H5Literate(group_id, H5_INDEX_NAME, order, &skip, link_iterate_old_cb, iter_info)) < 0) TEST_ERROR
    if(ret != CORDER_ITER_STOP) TEST_ERROR
    if(iter_info->ncalled != 3) TEST_ERROR


#ifndef H5_NO_DEPRECATED_SYMBOLS
    /* Iterate over links in group, stopping in the middle, with H5Giterate() */
    iter_info->nskipped = gskip = 0;
    iter_info->order = order;
    iter_info->stop = 3;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? 0 : (max_links - 1);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    if((ret = H5Giterate(group_id, ".", &gskip, group_iterate_old_cb, iter_info)) < 0) TEST_ERROR
    if(ret != CORDER_ITER_STOP) TEST_ERROR
    if(iter_info->ncalled != 3) TEST_ERROR
#endif /* H5_NO_DEPRECATED_SYMBOLS */


    /* Check for iteration routine indicating failure */
    skip = 0;
    H5E_BEGIN_TRY {
        ret = H5Literate(group_id, H5_INDEX_NAME, order, &skip, link_iterate_fail_cb, NULL);
    } H5E_END_TRY;
    if(ret >= 0) TEST_ERROR

    /* Check for iteration w/bad location ID */
    skip = 0;
    H5E_BEGIN_TRY {
        ret = H5Literate((-1), H5_INDEX_NAME, order, &skip, link_iterate_fail_cb, NULL);
    } H5E_END_TRY;
    if(ret >= 0) TEST_ERROR

#ifndef H5_NO_DEPRECATED_SYMBOLS
    H5E_BEGIN_TRY {
        ret = H5Giterate((-1), ".", &gskip, group_iterate_old_cb, iter_info);
    } H5E_END_TRY;
    if(ret >= 0) TEST_ERROR
#endif /* H5_NO_DEPRECATED_SYMBOLS */

    /* Success */
    return(0);

error:
    return(-1);
} /* end link_iterate_old_check() */


/*-------------------------------------------------------------------------
 * Function:    link_iterate_old
 *
 * Purpose:     Create a "old-style" group and test iterating over links by index.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November 14, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
link_iterate_old(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	group_id = (-1);	/* Group ID */
    H5_iter_order_t order;              /* Order within in the index */
    char        objname[NAME_BUF_SIZE]; /* Object name */
    char        filename[NAME_BUF_SIZE];/* File name */
    link_iter_info_t iter_info;         /* Iterator info */
    hbool_t     *visited = NULL;        /* Array of flags for visiting links */
    hsize_t     skip;                   /* # of links to skip in group */
    unsigned    u;                      /* Local index variable */
    herr_t      ret;                    /* Generic return value */

    /* Allocate the "visited link" array */
    iter_info.max_visit = CORDER_NLINKS;
    if(NULL == (visited = (hbool_t *)HDmalloc(sizeof(hbool_t) * iter_info.max_visit))) TEST_ERROR
    iter_info.visited = visited;

    /* Loop over operating in different orders */
    for(order = H5_ITER_INC; order <=H5_ITER_NATIVE; H5_INC_ENUM(H5_iter_order_t, order)) {
        /* Print appropriate test message */
        if(order == H5_ITER_INC) {
            TESTING("iterating over links by name index in increasing order in old-style group")
        } /* end if */
        else if(order == H5_ITER_DEC) {
            TESTING("iterating over links by name index in decreasing order in old-style group")
        } /* end else */
        else {
            HDassert(order == H5_ITER_NATIVE);
            TESTING("iterating over links by name index in native order in old-style group")
        } /* end else */

        /* Create file */
        h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
        if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

        /* Create group with creation order tracking on */
        if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR


        /* Check for iteration on empty group */
        /* (should be OK) */
        if(H5Literate(group_id, H5_INDEX_NAME, order, NULL, link_iterate_old_cb, NULL) < 0) TEST_ERROR

        /* Create several links */
        for(u = 0; u < CORDER_NLINKS; u++) {
            hid_t group_id2;	        /* Group ID */

            /* Make name for link */
            sprintf(objname, "filler %02u", u);

            /* Create hard link, with group object */
            if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
            if(H5Gclose(group_id2) < 0) TEST_ERROR
        } /* end for */

        /* Verify state of group (symbol table) */
        if(H5G_has_stab_test(group_id) != TRUE) TEST_ERROR

        /* Check for out of bound iteration on old-style group */
        skip = (hsize_t)u;
        H5E_BEGIN_TRY {
            ret = H5Literate(group_id, H5_INDEX_NAME, order, &skip, link_iterate_old_cb, NULL);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR

        /* Check for iteration on creation order */
        /* (should fail) */
        skip = (hsize_t)0;
        H5E_BEGIN_TRY {
            ret = H5Literate(group_id, H5_INDEX_CRT_ORDER, order, &skip, link_iterate_old_cb, NULL);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR

        /* Test iteration over links in group */
        if(link_iterate_old_check(group_id, order, u, &iter_info) < 0) TEST_ERROR


        /* Close the group */
        if(H5Gclose(group_id) < 0) TEST_ERROR

        /* Close the file */
        if(H5Fclose(file_id) < 0) TEST_ERROR

        PASSED();
    } /* end for */

    /* Free resources */
    if(visited)
        HDfree(visited);

    return 0;

error:
    /* Free resources */
    H5E_BEGIN_TRY {
        H5Gclose(group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;

    if(visited)
        HDfree(visited);

    return -1;
} /* end link_iterate_old() */


/*-------------------------------------------------------------------------
 * Function:    open_by_idx_check
 *
 * Purpose:     Check opening by index in a group
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November 21, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
open_by_idx_check(hid_t main_group_id, hid_t soft_group_id, hid_t mount_file_id,
    H5_index_t idx_type, H5_iter_order_t order, unsigned max_links,
    haddr_t *objno)
{
    char        mntname[NAME_BUF_SIZE]; /* Link value */
    hid_t       group_id = (-1); /* ID of group to test */
    H5O_info_t  oi;             /* Buffer for querying object's info */
    haddr_t     mnt_root_addr;  /* Address of root group in file to mount */
    hid_t       obj_id;         /* ID of object opened */
    unsigned    mnt_idx;        /* Index to mount group on */
    unsigned    u, v;           /* Local index variables */

    /* Work through main & soft link groups */
    for(v = 0; v < 2; v++) {
        /* Choose appropriate group to open links within */
        if(0 == v)
            group_id = main_group_id;
        else
            group_id = soft_group_id;

        /* Open each object in main group by index and check that it's the correct one */
        for(u = 0; u < max_links; u++) {
            /* Open the object */
            if((obj_id = H5Oopen_by_idx(group_id, ".", idx_type, order, (hsize_t)u, H5P_DEFAULT)) < 0) TEST_ERROR

            /* Get the object's information */
            if(H5Oget_info(obj_id, &oi) < 0) TEST_ERROR

            /* Check that the object is the correct one */
            if(order == H5_ITER_INC) {
                if(H5F_addr_ne(oi.addr, objno[u])) TEST_ERROR
            } /* end if */
            else if(order == H5_ITER_DEC) {
                unsigned dec_u = max_links - (u + 1);       /* Decreasing mapped index */

                if(H5F_addr_ne(oi.addr, objno[dec_u])) TEST_ERROR
            } /* end if */
            else {
                /* XXX: What to do about native order? */
            } /* end else */

            /* Close object */
            if(H5Oclose(obj_id) < 0) TEST_ERROR
        } /* end for */
    } /* end for */


    /*
     * Verify opening correct object by index when file mounting is present
     */

    /* Get the address of the root group in the file to mount */
    if(H5Oget_info(mount_file_id, &oi) < 0) TEST_ERROR
    mnt_root_addr = oi.addr;

    /* Mount a file over a group in main group */
    mnt_idx = 2;
    sprintf(mntname, "/%s/filler %02u", CORDER_GROUP_NAME, mnt_idx);
    if(H5Fmount(main_group_id, mntname, mount_file_id, H5P_DEFAULT) < 0) TEST_ERROR

    /* Open the object that the file is mounted on */
    if((obj_id = H5Oopen_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)mnt_idx, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Get the object's information */
    if(H5Oget_info(obj_id, &oi) < 0) TEST_ERROR

    /* Check that the object is the root of the mounted file and not in the previous file */
    if(H5F_addr_ne(oi.addr, mnt_root_addr)) TEST_ERROR
    if(H5F_addr_eq(oi.addr, objno[mnt_idx])) TEST_ERROR

    /* Close object */
    if(H5Oclose(obj_id) < 0) TEST_ERROR

    /* Unmount the file */
    if(H5Funmount(main_group_id, mntname) < 0) TEST_ERROR


    /* Success */
    return(0);

error:
    return(-1);
} /* end open_by_idx_check() */


/*-------------------------------------------------------------------------
 * Function:    open_by_idx
 *
 * Purpose:     Create a group with creation order indices and test opening
 *              objects by index.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November 21, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
open_by_idx(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	mount_file_id = (-1); 	/* File ID for file to mount */
    hid_t	group_id = (-1);	/* Group ID */
    hid_t	soft_group_id = (-1);	/* Group ID for soft links */
    hid_t       gcpl_id = (-1); 	/* Group creation property list ID */
    H5_index_t idx_type;               /* Type of index to operate on */
    H5_iter_order_t order;              /* Order within in the index */
    hbool_t     use_index;              /* Use index on creation order values */
    unsigned    max_compact;            /* Maximum # of links to store in group compactly */
    unsigned    min_dense;              /* Minimum # of links to store in group "densely" */
    H5O_info_t  oi;                     /* Buffer for querying object's info */
    char        filename[NAME_BUF_SIZE];/* File name */
    char        objname[NAME_BUF_SIZE]; /* Object name */
    char        valname[NAME_BUF_SIZE]; /* Link value */
    haddr_t     *objno = NULL;          /* Addresses of the objects created */
    unsigned    u;                      /* Local index variable */
    herr_t      ret;                    /* Generic return value */

    /* Create group creation property list */
    if((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0) TEST_ERROR

    /* Query the group creation properties */
    if(H5Pget_link_phase_change(gcpl_id, &max_compact, &min_dense) < 0) TEST_ERROR

    /* Allocate object address array */
    if(NULL == (objno = (haddr_t *)HDmalloc(sizeof(haddr_t) * (max_compact * 2)))) TEST_ERROR

    /* Create file to mount */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if((mount_file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Loop over operating on different indices on link fields */
    for(idx_type = H5_INDEX_NAME; idx_type <= H5_INDEX_CRT_ORDER; H5_INC_ENUM(H5_index_t, idx_type)) {
        /* Loop over operating in different orders */
        for(order = H5_ITER_INC; order <= H5_ITER_NATIVE; H5_INC_ENUM(H5_iter_order_t, order)) {
            /* Loop over using index for creation order value */
            for(use_index = FALSE; use_index <= TRUE; use_index++) {
                /* Print appropriate test message */
                if(idx_type == H5_INDEX_CRT_ORDER) {
                    if(order == H5_ITER_INC) {
                        if(use_index)
                            TESTING("open object by creation order index in increasing order w/creation order index")
                        else
                            TESTING("open object by creation order index in increasing order w/o creation order index")
                    } /* end if */
                    else if(order == H5_ITER_DEC) {
                        if(use_index)
                            TESTING("open object by creation order index in decreasing order w/creation order index")
                        else
                            TESTING("open object by creation order index in decreasing order w/o creation order index")
                    } /* end else */
                    else {
                        HDassert(order == H5_ITER_NATIVE);
                        if(use_index)
                            TESTING("open object by creation order index in native order w/creation order index")
                        else
                            TESTING("open object by creation order index in native order w/o creation order index")
                    } /* end else */
                } /* end if */
                else {
                    if(order == H5_ITER_INC) {
                        if(use_index)
                            TESTING("open object by name index in increasing order w/creation order index")
                        else
                            TESTING("open object by name index in increasing order w/o creation order index")
                    } /* end if */
                    else if(order == H5_ITER_DEC) {
                        if(use_index)
                            TESTING("open object by name index in decreasing order w/creation order index")
                        else
                            TESTING("open object by name index in decreasing order w/o creation order index")
                    } /* end else */
                    else {
                        HDassert(order == H5_ITER_NATIVE);
                        if(use_index)
                            TESTING("open object by name index in native order w/creation order index")
                        else
                            TESTING("open object by name index in native order w/o creation order index")
                    } /* end else */
                } /* end else */

                /* Create file */
                h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
                if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

                /* Set creation order tracking & indexing on group */
                if(H5Pset_link_creation_order(gcpl_id, (H5P_CRT_ORDER_TRACKED | (use_index ? H5P_CRT_ORDER_INDEXED : (unsigned)0))) < 0) TEST_ERROR

                /* Create group with creation order tracking on */
                if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) TEST_ERROR

                /* Create group with creation order tracking on for soft links */
                if((soft_group_id = H5Gcreate2(file_id, CORDER_SOFT_GROUP_NAME, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) TEST_ERROR


                /* Try to open on object in an empty group */
                H5E_BEGIN_TRY {
                    ret = H5Oopen_by_idx(group_id, ".", idx_type, order, (hsize_t)0, H5P_DEFAULT);
                } H5E_END_TRY;
                if(ret >= 0) TEST_ERROR


                /* Create several links, up to limit of compact form */
                for(u = 0; u < max_compact; u++) {
                    hid_t group_id2;	        /* Group ID */

                    /* Make name for link */
                    sprintf(objname, "filler %02u", u);

                    /* Create hard link, with group object */
                    if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

                    /* Retrieve group's address on disk */
                    if(H5Oget_info(group_id2, &oi) < 0) TEST_ERROR
                    objno[u] = oi.addr;

                    /* Close group created */
                    if(H5Gclose(group_id2) < 0) TEST_ERROR

                    /* Create soft link in another group, to objects in main group */
                    sprintf(valname, "/%s/%s", CORDER_GROUP_NAME, objname);
                    if(H5Lcreate_soft(valname, soft_group_id, objname, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
                } /* end for */

                /* Verify state of group (compact) */
                if(H5G_has_links_test(group_id, NULL) != TRUE) TEST_ERROR

                /* Check for out of bound open by index on compact group */
                H5E_BEGIN_TRY {
                    ret = H5Oopen_by_idx(group_id, ".", idx_type, order, (hsize_t)u, H5P_DEFAULT);
                } H5E_END_TRY;
                if(ret >= 0) TEST_ERROR

                /* Verify opening objects by index */
                if(open_by_idx_check(group_id, soft_group_id, mount_file_id, idx_type, order, u, objno) < 0) TEST_ERROR


                /* Create more links, to push group into dense form */
                for(; u < (max_compact * 2); u++) {
                    hid_t group_id2;	        /* Group ID */

                    /* Make name for link */
                    sprintf(objname, "filler %02u", u);

                    /* Create hard link, with group object */
                    if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

                    /* Retrieve group's address on disk */
                    if(H5Oget_info(group_id2, &oi) < 0) TEST_ERROR
                    objno[u] = oi.addr;

                    /* Close group created */
                    if(H5Gclose(group_id2) < 0) TEST_ERROR

                    /* Create soft link in another group, to objects in main group */
                    sprintf(valname, "/%s/%s", CORDER_GROUP_NAME, objname);
                    if(H5Lcreate_soft(valname, soft_group_id, objname, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
                } /* end for */

                /* Verify state of group (dense) */
                if(H5G_is_new_dense_test(group_id) != TRUE) TEST_ERROR

                /* Check for out of bound open by index on compact group */
                H5E_BEGIN_TRY {
                    ret = H5Oopen_by_idx(group_id, ".", idx_type, order, (hsize_t)u, H5P_DEFAULT);
                } H5E_END_TRY;
                if(ret >= 0) TEST_ERROR

                /* Verify opening objects by index */
                if(open_by_idx_check(group_id, soft_group_id, mount_file_id, idx_type, order, u, objno) < 0) TEST_ERROR


                /* Close the groups */
                if(H5Gclose(group_id) < 0) TEST_ERROR
                if(H5Gclose(soft_group_id) < 0) TEST_ERROR

                /* Close the file */
                if(H5Fclose(file_id) < 0) TEST_ERROR

                PASSED();
            } /* end for */
        } /* end for */
    } /* end for */

    /* Close the file for mounting */
    if(H5Fclose(mount_file_id) < 0) TEST_ERROR

    /* Close the group creation property list */
    if(H5Pclose(gcpl_id) < 0) TEST_ERROR

    /* Free resources */
    if(objno)
        HDfree(objno);

    return 0;

error:
    /* Free resources */
    H5E_BEGIN_TRY {
        H5Pclose(gcpl_id);
        H5Gclose(group_id);
        H5Gclose(soft_group_id);
        H5Fclose(file_id);
        H5Fclose(mount_file_id);
    } H5E_END_TRY;

    if(objno)
        HDfree(objno);

    return -1;
} /* end open_by_idx() */


/*-------------------------------------------------------------------------
 * Function:    open_by_idx_old
 *
 * Purpose:     Create an old-style group and test opening
 *              objects by index.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November 21, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
open_by_idx_old(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	mount_file_id = (-1); 	/* File ID for file to mount */
    hid_t	group_id = (-1);	/* Group ID */
    hid_t	soft_group_id = (-1);	/* Group ID for soft links */
    H5_iter_order_t order;              /* Order within in the index */
    H5O_info_t  oi;                     /* Buffer for querying object's info */
    char        filename[NAME_BUF_SIZE];/* File name */
    char        objname[NAME_BUF_SIZE]; /* Object name */
    char        valname[NAME_BUF_SIZE]; /* Link value */
    haddr_t     objno[CORDER_NLINKS];   /* Addresses of the objects created */
    unsigned    u;                      /* Local index variable */
    herr_t      ret;                    /* Generic return value */

    /* Create file to mount */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if((mount_file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Loop over operating in different orders */
    for(order = H5_ITER_INC; order <=H5_ITER_NATIVE; H5_INC_ENUM(H5_iter_order_t, order)) {
        /* Print appropriate test message */
        if(order == H5_ITER_INC) {
            TESTING("open object by name index in increasing order in old-style group")
        } /* end if */
        else if(order == H5_ITER_DEC) {
            TESTING("open object by name index in decreasing order in old-style group")
        } /* end else */
        else {
            HDassert(order == H5_ITER_NATIVE);
            TESTING("open object by name index in native order in old-style group")
        } /* end else */

        /* Create file */
        h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
        if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

        /* Create old-style group */
        if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

        /* Create old-style group for soft links */
        if((soft_group_id = H5Gcreate2(file_id, CORDER_SOFT_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR


        /* Try to open on object in an empty group */
        H5E_BEGIN_TRY {
            ret = H5Oopen_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)0, H5P_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR


        /* Create several links */
        for(u = 0; u < CORDER_NLINKS; u++) {
            hid_t group_id2;	        /* Group ID */

            /* Make name for link */
            sprintf(objname, "filler %02u", u);

            /* Create hard link, with group object */
            if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

            /* Retrieve group's address on disk */
            if(H5Oget_info(group_id2, &oi) < 0) TEST_ERROR
            objno[u] = oi.addr;

            /* Close group created */
            if(H5Gclose(group_id2) < 0) TEST_ERROR

            /* Create soft link in another group, to objects in main group */
            sprintf(valname, "/%s/%s", CORDER_GROUP_NAME, objname);
            if(H5Lcreate_soft(valname, soft_group_id, objname, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
        } /* end for */

        /* Verify state of group (symbol table) */
        if(H5G_has_stab_test(group_id) != TRUE) TEST_ERROR

        /* Check for out of bound open by index */
        H5E_BEGIN_TRY {
            ret = H5Oopen_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)u, H5P_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR

        /* Check for creation order index open */
        H5E_BEGIN_TRY {
            ret = H5Oopen_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, order, (hsize_t)(u - 1), H5P_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR

        /* Verify opening objects by index */
        if(open_by_idx_check(group_id, soft_group_id, mount_file_id, H5_INDEX_NAME, order, u, objno) < 0) TEST_ERROR


        /* Close the groups */
        if(H5Gclose(group_id) < 0) TEST_ERROR
        if(H5Gclose(soft_group_id) < 0) TEST_ERROR

        /* Close the file */
        if(H5Fclose(file_id) < 0) TEST_ERROR

        PASSED();
    } /* end for */

    /* Close the file for mounting */
    if(H5Fclose(mount_file_id) < 0) TEST_ERROR

    return 0;

error:
    /* Free resources */
    H5E_BEGIN_TRY {
        H5Gclose(group_id);
        H5Gclose(soft_group_id);
        H5Fclose(file_id);
        H5Fclose(mount_file_id);
    } H5E_END_TRY;

    return -1;
} /* end open_by_idx_old() */


/*-------------------------------------------------------------------------
 * Function:    object_info_check
 *
 * Purpose:     Check querying object info in a group
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Sunday, November 26, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
object_info_check(hid_t main_group_id, hid_t soft_group_id, H5_index_t idx_type,
    H5_iter_order_t order, unsigned max_links, haddr_t *objno)
{
    char        objname[NAME_BUF_SIZE]; /* Object name */
    hid_t       group_id = (-1); /* ID of group to test */
    H5O_info_t  oinfo;          /* Buffer for querying object's info */
    unsigned    u, v;           /* Local index variables */

    /* Work through main & soft link groups */
    for(v = 0; v < 2; v++) {
        /* Choose appropriate group to open links within */
        if(0 == v)
            group_id = main_group_id;
        else
            group_id = soft_group_id;

        /* Open each object in group by name and check that it's the correct one */
        for(u = 0; u < max_links; u++) {
            /* Make name for link */
            sprintf(objname, "filler %02u", u);

            /* Query the object's information, by name */
            if(H5Oget_info_by_name(group_id, objname, &oinfo, H5P_DEFAULT) < 0) TEST_ERROR

            /* Check that the object is the correct one */
            if(H5F_addr_ne(oinfo.addr, objno[u])) TEST_ERROR
            if(H5F_addr_ne(oinfo.num_attrs, u)) TEST_ERROR

            /* Query the object's information, by index */
            if(H5Oget_info_by_idx(group_id, ".", idx_type, order, (hsize_t)u, &oinfo, H5P_DEFAULT) < 0) TEST_ERROR

            /* Check that the object is the correct one */
            if(order == H5_ITER_INC) {
                if(H5F_addr_ne(oinfo.addr, objno[u])) TEST_ERROR
                if(H5F_addr_ne(oinfo.num_attrs, u)) TEST_ERROR
            } /* end if */
            else if(order == H5_ITER_DEC) {
                unsigned dec_u = max_links - (u + 1);       /* Decreasing mapped index */

                if(H5F_addr_ne(oinfo.addr, objno[dec_u])) TEST_ERROR
                if(H5F_addr_ne(oinfo.num_attrs, dec_u)) TEST_ERROR
            } /* end if */
            else {
                /* XXX: What to do about native order? */
            } /* end else */

        } /* end for */
    } /* end for */

    /* Success */
    return(0);

error:
    return(-1);
} /* end object_info_check() */


/*-------------------------------------------------------------------------
 * Function:    object_info
 *
 * Purpose:     Create a group with creation order indices and test querying
 *              object info.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Sunday, November 26, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
object_info(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	group_id = (-1);	/* Group ID */
    hid_t	soft_group_id = (-1);	/* Group ID for soft links */
    hid_t       gcpl_id = (-1); 	/* Group creation property list ID */
    hid_t       space_id = (-1);        /* Dataspace ID (for attributes) */
    H5_index_t idx_type;               /* Type of index to operate on */
    H5_iter_order_t order;              /* Order within in the index */
    hbool_t     use_index;              /* Use index on creation order values */
    unsigned    max_compact;            /* Maximum # of links to store in group compactly */
    unsigned    min_dense;              /* Minimum # of links to store in group "densely" */
    H5O_info_t  oinfo;                  /* Buffer for querying object's info */
    char        filename[NAME_BUF_SIZE];/* File name */
    char        objname[NAME_BUF_SIZE]; /* Object name */
    char        valname[NAME_BUF_SIZE]; /* Link value */
    char        attrname[NAME_BUF_SIZE]; /* Attribute name */
    haddr_t     *objno = NULL;          /* Addresses of the objects created */
    herr_t      ret;                    /* Generic return value */
    unsigned    u, v;                   /* Local index variables */

    /* Create group creation property list */
    if((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0) TEST_ERROR

    /* Query the group creation properties */
    if(H5Pget_link_phase_change(gcpl_id, &max_compact, &min_dense) < 0) TEST_ERROR

    /* Allocate object address array */
    if(NULL == (objno = (haddr_t *)HDmalloc(sizeof(haddr_t) * (max_compact * 2)))) TEST_ERROR

    /* Create dataspace for attributes */
    if((space_id = H5Screate(H5S_SCALAR)) < 0) TEST_ERROR

    /* Loop over operating on different indices on link fields */
    for(idx_type = H5_INDEX_NAME; idx_type <= H5_INDEX_CRT_ORDER; H5_INC_ENUM(H5_index_t, idx_type)) {
        /* Loop over operating in different orders */
        for(order = H5_ITER_INC; order <=H5_ITER_NATIVE; H5_INC_ENUM(H5_iter_order_t, order)) {
            /* Loop over using index for creation order value */
            for(use_index = FALSE; use_index <= TRUE; use_index++) {
                /* Print appropriate test message */
                if(idx_type == H5_INDEX_CRT_ORDER) {
                    if(order == H5_ITER_INC) {
                        if(use_index)
                            TESTING("query object info by creation order index in increasing order w/creation order index")
                        else
                            TESTING("query object info by creation order index in increasing order w/o creation order index")
                    } /* end if */
                    else if(order == H5_ITER_DEC) {
                        if(use_index)
                            TESTING("query object info by creation order index in decreasing order w/creation order index")
                        else
                            TESTING("query object info by creation order index in decreasing order w/o creation order index")
                    } /* end else */
                    else {
                        HDassert(order == H5_ITER_NATIVE);
                        if(use_index)
                            TESTING("query object info by creation order index in native order w/creation order index")
                        else
                            TESTING("query object info by creation order index in native order w/o creation order index")
                    } /* end else */
                } /* end if */
                else {
                    if(order == H5_ITER_INC) {
                        if(use_index)
                            TESTING("query object info by name index in increasing order w/creation order index")
                        else
                            TESTING("query object info by name index in increasing order w/o creation order index")
                    } /* end if */
                    else if(order == H5_ITER_DEC) {
                        if(use_index)
                            TESTING("query object info by name index in decreasing order w/creation order index")
                        else
                            TESTING("query object info by name index in decreasing order w/o creation order index")
                    } /* end else */
                    else {
                        HDassert(order == H5_ITER_NATIVE);
                        if(use_index)
                            TESTING("query object info by name index in native order w/creation order index")
                        else
                            TESTING("query object info by name index in native order w/o creation order index")
                    } /* end else */
                } /* end else */

                /* Create file */
                h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
                if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

                /* Set creation order tracking & indexing on group */
                if(H5Pset_link_creation_order(gcpl_id, (H5P_CRT_ORDER_TRACKED | (use_index ? H5P_CRT_ORDER_INDEXED : (unsigned)0))) < 0) TEST_ERROR

                /* Create group with creation order tracking on */
                if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) TEST_ERROR

                /* Create group with creation order tracking on for soft links */
                if((soft_group_id = H5Gcreate2(file_id, CORDER_SOFT_GROUP_NAME, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) TEST_ERROR


                /* Check for out of bound query by index on empty group */
                H5E_BEGIN_TRY {
                    ret = H5Oget_info_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)0, &oinfo, H5P_DEFAULT);
                } H5E_END_TRY;
                if(ret >= 0) TEST_ERROR

                /* Create several links, up to limit of compact form */
                for(u = 0; u < max_compact; u++) {
                    hid_t group_id2;	        /* Group ID */
                    hid_t attr_id;              /* Attribute ID */

                    /* Make name for link */
                    sprintf(objname, "filler %02u", u);

                    /* Create hard link, with group object */
                    if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

                    /* Retrieve group's address on disk */
                    if(H5Oget_info(group_id2, &oinfo) < 0) TEST_ERROR
                    objno[u] = oinfo.addr;

                    /* Create attributes on new object */
                    for(v = 0; v < u; v++) {
                        /* Make name for attribute */
                        sprintf(attrname, "attr %02u", v);

                        /* Create attribute */
                        if((attr_id = H5Acreate2(group_id2, attrname, H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

                        /* Close attribute */
                        if(H5Aclose(attr_id) < 0) TEST_ERROR
                    } /* end for */

                    /* Close group created */
                    if(H5Gclose(group_id2) < 0) TEST_ERROR

                    /* Create soft link in another group, to objects in main group */
                    sprintf(valname, "/%s/%s", CORDER_GROUP_NAME, objname);
                    if(H5Lcreate_soft(valname, soft_group_id, objname, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
                } /* end for */

                /* Verify state of group (compact) */
                if(H5G_has_links_test(group_id, NULL) != TRUE) TEST_ERROR

                /* Check for out of bound query by index */
                H5E_BEGIN_TRY {
                    ret = H5Oget_info_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)u, &oinfo, H5P_DEFAULT);
                } H5E_END_TRY;
                if(ret >= 0) TEST_ERROR

                /* Verify querying objects by name */
                if(object_info_check(group_id, soft_group_id, idx_type, order, u, objno) < 0) TEST_ERROR


                /* Create more links, to push group into dense form */
                for(; u < (max_compact * 2); u++) {
                    hid_t group_id2;	        /* Group ID */
                    hid_t attr_id;              /* Attribute ID */

                    /* Make name for link */
                    sprintf(objname, "filler %02u", u);

                    /* Create hard link, with group object */
                    if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

                    /* Retrieve group's address on disk */
                    if(H5Oget_info(group_id2, &oinfo) < 0) TEST_ERROR
                    objno[u] = oinfo.addr;

                    /* Create attributes on new object */
                    for(v = 0; v < u; v++) {
                        /* Make name for attribute */
                        sprintf(attrname, "attr %02u", v);

                        /* Create attribute */
                        if((attr_id = H5Acreate2(group_id2, attrname, H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

                        /* Close attribute */
                        if(H5Aclose(attr_id) < 0) TEST_ERROR
                    } /* end for */

                    /* Close group created */
                    if(H5Gclose(group_id2) < 0) TEST_ERROR

                    /* Create soft link in another group, to objects in main group */
                    sprintf(valname, "/%s/%s", CORDER_GROUP_NAME, objname);
                    if(H5Lcreate_soft(valname, soft_group_id, objname, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
                } /* end for */

                /* Verify state of group (dense) */
                if(H5G_is_new_dense_test(group_id) != TRUE) TEST_ERROR

                /* Check for out of bound query by index */
                H5E_BEGIN_TRY {
                    ret = H5Oget_info_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)u, &oinfo, H5P_DEFAULT);
                } H5E_END_TRY;
                if(ret >= 0) TEST_ERROR

                /* Verify querying objects by name */
                if(object_info_check(group_id, soft_group_id, idx_type, order, u, objno) < 0) TEST_ERROR


                /* Close the groups */
                if(H5Gclose(group_id) < 0) TEST_ERROR
                if(H5Gclose(soft_group_id) < 0) TEST_ERROR

                /* Close the file */
                if(H5Fclose(file_id) < 0) TEST_ERROR

                PASSED();
            } /* end for */
        } /* end for */
    } /* end for */

    /* Free resources */
    if(H5Pclose(gcpl_id) < 0) TEST_ERROR
    if(H5Sclose(space_id) < 0) TEST_ERROR
    if(objno)
        HDfree(objno);

    return 0;

error:
    /* Free resources */
    H5E_BEGIN_TRY {
        H5Sclose(space_id);
        H5Pclose(gcpl_id);
        H5Gclose(group_id);
        H5Gclose(soft_group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;

    if(objno)
        HDfree(objno);

    return -1;
} /* end object_info() */


/*-------------------------------------------------------------------------
 * Function:    object_info_old
 *
 * Purpose:     Create an old-style group test querying object info.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Sunday, November 26, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
object_info_old(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	group_id = (-1);	/* Group ID */
    hid_t	soft_group_id = (-1);	/* Group ID for soft links */
    hid_t       space_id = (-1);        /* Dataspace ID (for attributes) */
    H5_iter_order_t order;              /* Order within in the index */
    H5O_info_t  oinfo;                  /* Buffer for querying object's info */
    char        filename[NAME_BUF_SIZE];/* File name */
    char        objname[NAME_BUF_SIZE]; /* Object name */
    char        valname[NAME_BUF_SIZE]; /* Link value */
    char        attrname[NAME_BUF_SIZE]; /* Attribute name */
    haddr_t     objno[CORDER_NLINKS];   /* Addresses of the objects created */
    herr_t      ret;                    /* Generic return value */
    unsigned    u, v;                   /* Local index variables */

    /* Create dataspace for attributes */
    if((space_id = H5Screate(H5S_SCALAR)) < 0) TEST_ERROR

    /* Loop over operating in different orders */
    for(order = H5_ITER_INC; order <=H5_ITER_NATIVE; H5_INC_ENUM(H5_iter_order_t, order)) {
        /* Print appropriate test message */
        if(order == H5_ITER_INC) {
            TESTING("query object info by name index in increasing order in old-style group")
        } /* end if */
        else if(order == H5_ITER_DEC) {
            TESTING("query object info by name index in decreasing order in old-style group")
        } /* end else */
        else {
            HDassert(order == H5_ITER_NATIVE);
            TESTING("query object info by name index in native order in old-style group")
        } /* end else */

        /* Create file */
        h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
        if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

        /* Create old-style group */
        if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

        /* Create old-style group for soft links */
        if((soft_group_id = H5Gcreate2(file_id, CORDER_SOFT_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR


        /* Check for out of bound query by index on empty group */
        H5E_BEGIN_TRY {
            ret = H5Oget_info_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)0, &oinfo, H5P_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR

        /* Create several links */
        for(u = 0; u < CORDER_NLINKS; u++) {
            hid_t group_id2;	        /* Group ID */
            hid_t attr_id;              /* Attribute ID */

            /* Make name for link */
            sprintf(objname, "filler %02u", u);

            /* Create hard link, with group object */
            if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

            /* Retrieve group's address on disk */
            if(H5Oget_info(group_id2, &oinfo) < 0) TEST_ERROR
            objno[u] = oinfo.addr;

            /* Create attributes on new object */
            for(v = 0; v < u; v++) {
                /* Make name for attribute */
                sprintf(attrname, "attr %02u", v);

                /* Create attribute */
                if((attr_id = H5Acreate2(group_id2, attrname, H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

                /* Close attribute */
                if(H5Aclose(attr_id) < 0) TEST_ERROR
            } /* end for */

            /* Close group created */
            if(H5Gclose(group_id2) < 0) TEST_ERROR

            /* Create soft link in another group, to objects in main group */
            sprintf(valname, "/%s/%s", CORDER_GROUP_NAME, objname);
            if(H5Lcreate_soft(valname, soft_group_id, objname, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
        } /* end for */

        /* Verify state of group (symbol table) */
        if(H5G_has_stab_test(group_id) != TRUE) TEST_ERROR

        /* Check for out of bound query by index */
        H5E_BEGIN_TRY {
            ret = H5Oget_info_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)u, &oinfo, H5P_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR

        /* Check for creation order index query */
        H5E_BEGIN_TRY {
            ret = H5Oget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, order, (hsize_t)(u - 1), &oinfo, H5P_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR

        /* Verify querying objects by name */
        if(object_info_check(group_id, soft_group_id, H5_INDEX_NAME, order, u, objno) < 0) TEST_ERROR


        /* Close the groups */
        if(H5Gclose(group_id) < 0) TEST_ERROR
        if(H5Gclose(soft_group_id) < 0) TEST_ERROR

        /* Close the file */
        if(H5Fclose(file_id) < 0) TEST_ERROR

        PASSED();
    } /* end for */

    /* Free resources */
    if(H5Sclose(space_id) < 0) TEST_ERROR

    return 0;

error:
    /* Free resources */
    H5E_BEGIN_TRY {
        H5Sclose(space_id);
        H5Gclose(group_id);
        H5Gclose(soft_group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;

    return -1;
} /* end object_info_old() */


/*-------------------------------------------------------------------------
 * Function:    group_info
 *
 * Purpose:     Create a group with creation order indices and test querying
 *              group info.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, November 27, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
group_info(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	group_id = (-1);	/* Group ID */
    hid_t	soft_group_id = (-1);	/* Group ID for soft links */
    hid_t       gcpl_id = (-1); 	/* Group creation property list ID */
    H5_index_t idx_type;               /* Type of index to operate on */
    H5_iter_order_t order;              /* Order within in the index */
    hbool_t     use_index;              /* Use index on creation order values */
    unsigned    max_compact;            /* Maximum # of links to store in group compactly */
    unsigned    min_dense;              /* Minimum # of links to store in group "densely" */
    H5G_info_t  grp_info;               /* Buffer for querying object's info */
    char        filename[NAME_BUF_SIZE];/* File name */
    char        objname[NAME_BUF_SIZE]; /* Object name */
    char        objname2[NAME_BUF_SIZE]; /* Object name */
    char        valname[NAME_BUF_SIZE]; /* Link value */
    herr_t      ret;                    /* Generic return value */
    unsigned    u, v;                   /* Local index variables */

    /* Create group creation property list */
    if((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0) TEST_ERROR

    /* Query the group creation properties */
    if(H5Pget_link_phase_change(gcpl_id, &max_compact, &min_dense) < 0) TEST_ERROR

    /* Loop over operating on different indices on link fields */
    for(idx_type = H5_INDEX_NAME; idx_type <= H5_INDEX_CRT_ORDER; H5_INC_ENUM(H5_index_t, idx_type)) {
        /* Loop over operating in different orders */
        for(order = H5_ITER_INC; order <=H5_ITER_NATIVE; H5_INC_ENUM(H5_iter_order_t, order)) {
            /* Loop over using index for creation order value */
            for(use_index = FALSE; use_index <= TRUE; use_index++) {
                /* Print appropriate test message */
                if(idx_type == H5_INDEX_CRT_ORDER) {
                    if(order == H5_ITER_INC) {
                        if(use_index)
                            TESTING("query group info by creation order index in increasing order w/creation order index")
                        else
                            TESTING("query group info by creation order index in increasing order w/o creation order index")
                    } /* end if */
                    else if(order == H5_ITER_DEC) {
                        if(use_index)
                            TESTING("query group info by creation order index in decreasing order w/creation order index")
                        else
                            TESTING("query group info by creation order index in decreasing order w/o creation order index")
                    } /* end else */
                    else {
                        HDassert(order == H5_ITER_NATIVE);
                        if(use_index)
                            TESTING("query group info by creation order index in native order w/creation order index")
                        else
                            TESTING("query group info by creation order index in native order w/o creation order index")
                    } /* end else */
                } /* end if */
                else {
                    if(order == H5_ITER_INC) {
                        if(use_index)
                            TESTING("query group info by name index in increasing order w/creation order index")
                        else
                            TESTING("query group info by name index in increasing order w/o creation order index")
                    } /* end if */
                    else if(order == H5_ITER_DEC) {
                        if(use_index)
                            TESTING("query group info by name index in decreasing order w/creation order index")
                        else
                            TESTING("query group info by name index in decreasing order w/o creation order index")
                    } /* end else */
                    else {
                        HDassert(order == H5_ITER_NATIVE);
                        if(use_index)
                            TESTING("query group info by name index in native order w/creation order index")
                        else
                            TESTING("query group info by name index in native order w/o creation order index")
                    } /* end else */
                } /* end else */

                /* Create file */
                h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
                if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

                /* Set creation order tracking & indexing on group */
                if(H5Pset_link_creation_order(gcpl_id, (H5P_CRT_ORDER_TRACKED | (use_index ? H5P_CRT_ORDER_INDEXED : (unsigned)0))) < 0) TEST_ERROR

                /* Create group with creation order tracking on */
                if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) TEST_ERROR

                /* Create group with creation order tracking on for soft links */
                if((soft_group_id = H5Gcreate2(file_id, CORDER_SOFT_GROUP_NAME, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) TEST_ERROR


                /* Check for out of bound query by index on empty group */
                H5E_BEGIN_TRY {
                    ret = H5Gget_info_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)0, &grp_info, H5P_DEFAULT);
                } H5E_END_TRY;
                if(ret >= 0) TEST_ERROR

                /* Create several links, up to limit of compact form */
                for(u = 0; u < max_compact; u++) {
                    hid_t group_id2, group_id3;	        /* Group IDs */

                    /* Make name for link */
                    sprintf(objname, "filler %02u", u);

                    /* Create hard link, with group object */
                    if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) TEST_ERROR


                    /* Retrieve group's information */
                    if(H5Gget_info(group_id2, &grp_info) < 0) TEST_ERROR

                    /* Check (new/empty) group's information */
                    if(grp_info.storage_type != H5G_STORAGE_TYPE_COMPACT) TEST_ERROR
                    if(grp_info.max_corder != 0) TEST_ERROR
                    if(grp_info.nlinks != 0) TEST_ERROR

                    /* Retrieve group's information */
                    if(H5Gget_info_by_name(group_id, objname, &grp_info, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Check (new/empty) group's information */
                    if(grp_info.storage_type != H5G_STORAGE_TYPE_COMPACT) TEST_ERROR
                    if(grp_info.max_corder != 0) TEST_ERROR
                    if(grp_info.nlinks != 0) TEST_ERROR

                    /* Retrieve group's information */
                    if(H5Gget_info_by_name(group_id2, ".", &grp_info, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Check (new/empty) group's information */
                    if(grp_info.storage_type != H5G_STORAGE_TYPE_COMPACT) TEST_ERROR
                    if(grp_info.max_corder != 0) TEST_ERROR
                    if(grp_info.nlinks != 0) TEST_ERROR


                    /* Create objects in new group created */
                    for(v = 0; v <= u; v++) {
                        /* Make name for link */
                        sprintf(objname2, "filler %02u", v);

                        /* Create hard link, with group object */
                        if((group_id3 = H5Gcreate2(group_id2, objname2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

                        /* Close group created */
                        if(H5Gclose(group_id3) < 0) TEST_ERROR
                    } /* end for */


                    /* Retrieve group's information */
                    if(H5Gget_info(group_id2, &grp_info) < 0) TEST_ERROR

                    /* Check (new) group's information */
                    if(grp_info.storage_type != H5G_STORAGE_TYPE_COMPACT) TEST_ERROR
                    if(grp_info.max_corder != (int64_t)(u + 1)) TEST_ERROR
                    if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR

                    /* Retrieve group's information */
                    if(H5Gget_info_by_name(group_id, objname, &grp_info, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Check (new) group's information */
                    if(grp_info.storage_type != H5G_STORAGE_TYPE_COMPACT) TEST_ERROR
                    if(grp_info.max_corder != (int64_t)(u + 1)) TEST_ERROR
                    if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR

                    /* Retrieve group's information */
                    if(H5Gget_info_by_name(group_id2, ".", &grp_info, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Check (new) group's information */
                    if(grp_info.storage_type != H5G_STORAGE_TYPE_COMPACT) TEST_ERROR
                    if(grp_info.max_corder != (int64_t)(u + 1)) TEST_ERROR
                    if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR


                    /* Retrieve group's information */
                    if(order != H5_ITER_NATIVE) {
                        if(order == H5_ITER_INC) {
                            if(H5Gget_info_by_idx(group_id, ".", idx_type, order, (hsize_t)u, &grp_info, H5P_DEFAULT) < 0) TEST_ERROR
                        } /* end if */
                        else {
                            if(H5Gget_info_by_idx(group_id, ".", idx_type, order, (hsize_t)0, &grp_info, H5P_DEFAULT) < 0) TEST_ERROR
                        } /* end else */

                        /* Check (new) group's information */
                        if(grp_info.storage_type != H5G_STORAGE_TYPE_COMPACT) TEST_ERROR
                        if(grp_info.max_corder != (int64_t)(u + 1)) TEST_ERROR
                        if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR
                    } /* end if */

                    /* Close group created */
                    if(H5Gclose(group_id2) < 0) TEST_ERROR


                    /* Retrieve main group's information */
                    if(H5Gget_info(group_id, &grp_info) < 0) TEST_ERROR

                    /* Check main group's information */
                    if(grp_info.storage_type != H5G_STORAGE_TYPE_COMPACT) TEST_ERROR
                    if(grp_info.max_corder != (int64_t)(u + 1)) TEST_ERROR
                    if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR

                    /* Retrieve main group's information, by name */
                    if(H5Gget_info_by_name(file_id, CORDER_GROUP_NAME, &grp_info, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Check main group's information */
                    if(grp_info.storage_type != H5G_STORAGE_TYPE_COMPACT) TEST_ERROR
                    if(grp_info.max_corder != (int64_t)(u + 1)) TEST_ERROR
                    if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR

                    /* Retrieve main group's information, by name */
                    if(H5Gget_info_by_name(group_id, ".", &grp_info, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Check main group's information */
                    if(grp_info.storage_type != H5G_STORAGE_TYPE_COMPACT) TEST_ERROR
                    if(grp_info.max_corder != (int64_t)(u + 1)) TEST_ERROR
                    if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR


                    /* Create soft link in another group, to objects in main group */
                    sprintf(valname, "/%s/%s", CORDER_GROUP_NAME, objname);
                    if(H5Lcreate_soft(valname, soft_group_id, objname, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Retrieve soft link group's information, by name */
                    if(H5Gget_info(soft_group_id, &grp_info) < 0) TEST_ERROR

                    /* Check soft link group's information */
                    if(grp_info.storage_type != H5G_STORAGE_TYPE_COMPACT) TEST_ERROR
                    if(grp_info.max_corder != (int64_t)(u + 1)) TEST_ERROR
                    if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR
                } /* end for */

                /* Verify state of group (compact) */
                if(H5G_has_links_test(group_id, NULL) != TRUE) TEST_ERROR

                /* Check for out of bound query by index */
                H5E_BEGIN_TRY {
                    ret = H5Gget_info_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)u, &grp_info, H5P_DEFAULT);
                } H5E_END_TRY;
                if(ret >= 0) TEST_ERROR


                /* Create more links, to push group into dense form */
                for(; u < (max_compact * 2); u++) {
                    hid_t group_id2, group_id3;	        /* Group IDs */

                    /* Make name for link */
                    sprintf(objname, "filler %02u", u);

                    /* Create hard link, with group object */
                    if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) TEST_ERROR


                    /* Retrieve group's information */
                    if(H5Gget_info(group_id2, &grp_info) < 0) TEST_ERROR

                    /* Check (new/empty) group's information */
                    if(grp_info.storage_type != H5G_STORAGE_TYPE_COMPACT) TEST_ERROR
                    if(grp_info.max_corder != 0) TEST_ERROR
                    if(grp_info.nlinks != 0) TEST_ERROR

                    /* Retrieve group's information, by name */
                    if(H5Gget_info_by_name(group_id, objname, &grp_info, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Check (new/empty) group's information */
                    if(grp_info.storage_type != H5G_STORAGE_TYPE_COMPACT) TEST_ERROR
                    if(grp_info.max_corder != 0) TEST_ERROR
                    if(grp_info.nlinks != 0) TEST_ERROR

                    /* Retrieve group's information, by name */
                    if(H5Gget_info_by_name(group_id2, ".", &grp_info, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Check (new/empty) group's information */
                    if(grp_info.storage_type != H5G_STORAGE_TYPE_COMPACT) TEST_ERROR
                    if(grp_info.max_corder != 0) TEST_ERROR
                    if(grp_info.nlinks != 0) TEST_ERROR


                    /* Create objects in new group created */
                    for(v = 0; v <= u; v++) {
                        /* Make name for link */
                        sprintf(objname2, "filler %02u", v);

                        /* Create hard link, with group object */
                        if((group_id3 = H5Gcreate2(group_id2, objname2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

                        /* Close group created */
                        if(H5Gclose(group_id3) < 0) TEST_ERROR
                    } /* end for */


                    /* Retrieve group's information */
                    if(H5Gget_info(group_id2, &grp_info) < 0) TEST_ERROR

                    /* Check (new) group's information */
                    if(grp_info.storage_type != H5G_STORAGE_TYPE_DENSE) TEST_ERROR
                    if(grp_info.max_corder != (int64_t)(u + 1)) TEST_ERROR
                    if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR

                    /* Retrieve group's information, by name */
                    if(H5Gget_info_by_name(group_id, objname, &grp_info, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Check (new) group's information */
                    if(grp_info.storage_type != H5G_STORAGE_TYPE_DENSE) TEST_ERROR
                    if(grp_info.max_corder != (int64_t)(u + 1)) TEST_ERROR
                    if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR

                    /* Retrieve group's information, by name */
                    if(H5Gget_info_by_name(group_id2, ".", &grp_info, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Check (new) group's information */
                    if(grp_info.storage_type != H5G_STORAGE_TYPE_DENSE) TEST_ERROR
                    if(grp_info.max_corder != (int64_t)(u + 1)) TEST_ERROR
                    if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR


                    /* Retrieve group's information */
                    if(order != H5_ITER_NATIVE) {
                        if(order == H5_ITER_INC) {
                            if(H5Gget_info_by_idx(group_id, ".", idx_type, order, (hsize_t)u, &grp_info, H5P_DEFAULT) < 0) TEST_ERROR
                        } /* end if */
                        else {
                            if(H5Gget_info_by_idx(group_id, ".", idx_type, order, (hsize_t)0, &grp_info, H5P_DEFAULT) < 0) TEST_ERROR
                        } /* end else */

                        /* Check (new) group's information */
                        if(grp_info.storage_type != H5G_STORAGE_TYPE_DENSE) TEST_ERROR
                        if(grp_info.max_corder != (int64_t)(u + 1)) TEST_ERROR
                        if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR
                    } /* end if */

                    /* Close group created */
                    if(H5Gclose(group_id2) < 0) TEST_ERROR


                    /* Retrieve main group's information */
                    if(H5Gget_info(group_id, &grp_info) < 0) TEST_ERROR

                    /* Check main group's information */
                    if(grp_info.storage_type != H5G_STORAGE_TYPE_DENSE) TEST_ERROR
                    if(grp_info.max_corder != (int64_t)(u + 1)) TEST_ERROR
                    if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR

                    /* Retrieve main group's information, by name */
                    if(H5Gget_info_by_name(file_id, CORDER_GROUP_NAME, &grp_info, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Check main group's information */
                    if(grp_info.storage_type != H5G_STORAGE_TYPE_DENSE) TEST_ERROR
                    if(grp_info.max_corder != (int64_t)(u + 1)) TEST_ERROR
                    if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR

                    /* Retrieve main group's information, by name */
                    if(H5Gget_info_by_name(group_id, ".", &grp_info, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Check main group's information */
                    if(grp_info.storage_type != H5G_STORAGE_TYPE_DENSE) TEST_ERROR
                    if(grp_info.max_corder != (int64_t)(u + 1)) TEST_ERROR
                    if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR


                    /* Create soft link in another group, to objects in main group */
                    sprintf(valname, "/%s/%s", CORDER_GROUP_NAME, objname);
                    if(H5Lcreate_soft(valname, soft_group_id, objname, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Retrieve soft link group's information, by name */
                    if(H5Gget_info(soft_group_id, &grp_info) < 0) TEST_ERROR

                    /* Check soft link group's information */
                    if(grp_info.storage_type != H5G_STORAGE_TYPE_DENSE) TEST_ERROR
                    if(grp_info.max_corder != (int64_t)(u + 1)) TEST_ERROR
                    if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR
                } /* end for */

                /* Verify state of group (dense) */
                if(H5G_is_new_dense_test(group_id) != TRUE) TEST_ERROR

                /* Check for out of bound query by index */
                H5E_BEGIN_TRY {
                    ret = H5Gget_info_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)u, &grp_info, H5P_DEFAULT);
                } H5E_END_TRY;
                if(ret >= 0) TEST_ERROR


                /* Close the groups */
                if(H5Gclose(group_id) < 0) TEST_ERROR
                if(H5Gclose(soft_group_id) < 0) TEST_ERROR

                /* Close the file */
                if(H5Fclose(file_id) < 0) TEST_ERROR

                PASSED();
            } /* end for */
        } /* end for */
    } /* end for */

    /* Free resources */
    if(H5Pclose(gcpl_id) < 0) TEST_ERROR

    return 0;

error:
    /* Free resources */
    H5E_BEGIN_TRY {
        H5Pclose(gcpl_id);
        H5Gclose(group_id);
        H5Gclose(soft_group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;

    return -1;
} /* end group_info() */


/*-------------------------------------------------------------------------
 * Function:    group_info_old
 *
 * Purpose:     Create an old-style group and test querying
 *              group info.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, November 27, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
group_info_old(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	group_id = (-1);	/* Group ID */
    hid_t	soft_group_id = (-1);	/* Group ID for soft links */
    H5_iter_order_t order;              /* Order within in the index */
    H5G_info_t  grp_info;               /* Buffer for querying object's info */
    char        filename[NAME_BUF_SIZE];/* File name */
    char        objname[NAME_BUF_SIZE]; /* Object name */
    char        objname2[NAME_BUF_SIZE]; /* Object name */
    char        valname[NAME_BUF_SIZE]; /* Link value */
    herr_t      ret;                    /* Generic return value */
    unsigned    u, v;                   /* Local index variables */

    /* Loop over operating in different orders */
    for(order = H5_ITER_INC; order <=H5_ITER_NATIVE; H5_INC_ENUM(H5_iter_order_t, order)) {
        if(order == H5_ITER_INC) {
            TESTING("query group info by name index in increasing order in old-style group")
        } /* end if */
        else if(order == H5_ITER_DEC) {
            TESTING("query group info by name index in decreasing order in old-style group")
        } /* end else */
        else {
            HDassert(order == H5_ITER_NATIVE);
            TESTING("query group info by name index in native order in old-style group")
        } /* end else */

        /* Create file */
        h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
        if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

        /* Create old-style group */
        if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

        /* Create old-style group for soft links */
        if((soft_group_id = H5Gcreate2(file_id, CORDER_SOFT_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR


        /* Check for out of bound query by index on empty group */
        H5E_BEGIN_TRY {
            ret = H5Gget_info_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)0, &grp_info, H5P_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR

        /* Create several links */
        for(u = 0; u < CORDER_NLINKS; u++) {
            hid_t group_id2, group_id3;	        /* Group IDs */

            /* Make name for link */
            sprintf(objname, "filler %02u", u);

            /* Create hard link, with group object */
            if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR


            /* Retrieve group's information */
            if(H5Gget_info(group_id2, &grp_info) < 0) TEST_ERROR

            /* Check (new/empty) group's information */
            if(grp_info.storage_type != H5G_STORAGE_TYPE_SYMBOL_TABLE) TEST_ERROR
            if(grp_info.max_corder != 0) TEST_ERROR
            if(grp_info.nlinks != 0) TEST_ERROR

            /* Retrieve group's information, by name */
            if(H5Gget_info_by_name(group_id, objname, &grp_info, H5P_DEFAULT) < 0) TEST_ERROR

            /* Check (new/empty) group's information */
            if(grp_info.storage_type != H5G_STORAGE_TYPE_SYMBOL_TABLE) TEST_ERROR
            if(grp_info.max_corder != 0) TEST_ERROR
            if(grp_info.nlinks != 0) TEST_ERROR

            /* Retrieve group's information, by name */
            if(H5Gget_info_by_name(group_id2, ".", &grp_info, H5P_DEFAULT) < 0) TEST_ERROR

            /* Check (new/empty) group's information */
            if(grp_info.storage_type != H5G_STORAGE_TYPE_SYMBOL_TABLE) TEST_ERROR
            if(grp_info.max_corder != 0) TEST_ERROR
            if(grp_info.nlinks != 0) TEST_ERROR


            /* Create objects in new group created */
            for(v = 0; v <= u; v++) {
                /* Make name for link */
                sprintf(objname2, "filler %02u", v);

                /* Create hard link, with group object */
                if((group_id3 = H5Gcreate2(group_id2, objname2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

                /* Close group created */
                if(H5Gclose(group_id3) < 0) TEST_ERROR
            } /* end for */


            /* Retrieve group's information */
            if(H5Gget_info(group_id2, &grp_info) < 0) TEST_ERROR

            /* Check (new) group's information */
            if(grp_info.storage_type != H5G_STORAGE_TYPE_SYMBOL_TABLE) TEST_ERROR
            if(grp_info.max_corder != 0) TEST_ERROR
            if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR

            /* Retrieve group's information, by name */
            if(H5Gget_info_by_name(group_id, objname, &grp_info, H5P_DEFAULT) < 0) TEST_ERROR

            /* Check (new) group's information */
            if(grp_info.storage_type != H5G_STORAGE_TYPE_SYMBOL_TABLE) TEST_ERROR
            if(grp_info.max_corder != 0) TEST_ERROR
            if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR

            /* Retrieve group's information, by name */
            if(H5Gget_info_by_name(group_id2, ".", &grp_info, H5P_DEFAULT) < 0) TEST_ERROR

            /* Check (new) group's information */
            if(grp_info.storage_type != H5G_STORAGE_TYPE_SYMBOL_TABLE) TEST_ERROR
            if(grp_info.max_corder != 0) TEST_ERROR
            if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR


            /* Retrieve group's information */
            if(order != H5_ITER_NATIVE) {
                if(order == H5_ITER_INC) {
                    if(H5Gget_info_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)u, &grp_info, H5P_DEFAULT) < 0) TEST_ERROR
                } /* end if */
                else {
                    if(H5Gget_info_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)0, &grp_info, H5P_DEFAULT) < 0) TEST_ERROR
                } /* end else */

                /* Check (new) group's information */
                if(grp_info.storage_type != H5G_STORAGE_TYPE_SYMBOL_TABLE) TEST_ERROR
                if(grp_info.max_corder != 0) TEST_ERROR
                if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR
            } /* end if */

            /* Close group created */
            if(H5Gclose(group_id2) < 0) TEST_ERROR


            /* Retrieve main group's information */
            if(H5Gget_info(group_id, &grp_info) < 0) TEST_ERROR

            /* Check main group's information */
            if(grp_info.storage_type != H5G_STORAGE_TYPE_SYMBOL_TABLE) TEST_ERROR
            if(grp_info.max_corder != 0) TEST_ERROR
            if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR

            /* Retrieve main group's information, by name */
            if(H5Gget_info_by_name(file_id, CORDER_GROUP_NAME, &grp_info, H5P_DEFAULT) < 0) TEST_ERROR

            /* Check main group's information */
            if(grp_info.storage_type != H5G_STORAGE_TYPE_SYMBOL_TABLE) TEST_ERROR
            if(grp_info.max_corder != 0) TEST_ERROR
            if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR

            /* Retrieve main group's information, by name */
            if(H5Gget_info_by_name(group_id, ".", &grp_info, H5P_DEFAULT) < 0) TEST_ERROR

            /* Check main group's information */
            if(grp_info.storage_type != H5G_STORAGE_TYPE_SYMBOL_TABLE) TEST_ERROR
            if(grp_info.max_corder != 0) TEST_ERROR
            if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR


            /* Create soft link in another group, to objects in main group */
            sprintf(valname, "/%s/%s", CORDER_GROUP_NAME, objname);
            if(H5Lcreate_soft(valname, soft_group_id, objname, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

            /* Retrieve soft link group's information, by name */
            if(H5Gget_info(soft_group_id, &grp_info) < 0) TEST_ERROR

            /* Check soft link group's information */
            if(grp_info.storage_type != H5G_STORAGE_TYPE_SYMBOL_TABLE) TEST_ERROR
            if(grp_info.max_corder != 0) TEST_ERROR
            if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR
        } /* end for */

        /* Verify state of group (old-style) */
        if(H5G_has_stab_test(group_id) != TRUE) TEST_ERROR

        /* Check for out of bound query by index */
        H5E_BEGIN_TRY {
            ret = H5Gget_info_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)u, &grp_info, H5P_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR

        /* Check for bad index query by index group */
        H5E_BEGIN_TRY {
            ret = H5Gget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, order, (hsize_t)0, &grp_info, H5P_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR


        /* Close the groups */
        if(H5Gclose(group_id) < 0) TEST_ERROR
        if(H5Gclose(soft_group_id) < 0) TEST_ERROR

        /* Close the file */
        if(H5Fclose(file_id) < 0) TEST_ERROR

        PASSED();
    } /* end for */

    return 0;

error:
    /* Free resources */
    H5E_BEGIN_TRY {
        H5Gclose(group_id);
        H5Gclose(soft_group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;

    return -1;
} /* end group_info_old() */


/*-------------------------------------------------------------------------
 * Function:    timestamps
 *
 * Purpose:     Verify that disabling tracking timestamps for an object
 *              works correctly
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Saturday, March 3, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
timestamps(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	group_id = (-1);	/* Group ID */
    hid_t	group_id2 = (-1);	/* Group ID */
    hid_t       gcpl_id = (-1); 	/* Group creation property list ID */
    hid_t       gcpl_id2 = (-1); 	/* Group creation property list ID */
    H5O_info_t  oinfo, oinfo2;          /* Object info for groups created */
    char        filename[NAME_BUF_SIZE];/* File name */
    hbool_t     track_times;            /* The object timestamp setting */

    /* Print test message */
    TESTING("timestamps on objects")

    /* Create group creation property list */
    if((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0) TEST_ERROR

    /* Query the object timestamp setting */
    if(H5Pget_obj_track_times(gcpl_id, &track_times) < 0) TEST_ERROR

    /* Check default timestamp information */
    if(track_times != TRUE) TEST_ERROR

    /* Set a non-default object timestamp setting */
    if(H5Pset_obj_track_times(gcpl_id, FALSE) < 0) TEST_ERROR

    /* Query the object timestamp setting */
    if(H5Pget_obj_track_times(gcpl_id, &track_times) < 0) TEST_ERROR

    /* Check default timestamp information */
    if(track_times != FALSE) TEST_ERROR


    /* Create file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create group with non-default object timestamp setting */
    if((group_id = H5Gcreate2(file_id, TIMESTAMP_GROUP_1, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close the group creation property list */
    if(H5Pclose(gcpl_id) < 0) TEST_ERROR

    /* Create group with default object timestamp setting */
    if((group_id2 = H5Gcreate2(file_id, TIMESTAMP_GROUP_2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Retrieve the new groups' creation properties */
    if((gcpl_id = H5Gget_create_plist(group_id)) < 0) TEST_ERROR
    if((gcpl_id2 = H5Gget_create_plist(group_id2)) < 0) TEST_ERROR

    /* Query & verify the object timestamp settings */
    if(H5Pget_obj_track_times(gcpl_id, &track_times) < 0) TEST_ERROR
    if(track_times != FALSE) TEST_ERROR
    if(H5Pget_obj_track_times(gcpl_id2, &track_times) < 0) TEST_ERROR
    if(track_times != TRUE) TEST_ERROR

    /* Query the object information for each group */
    if(H5Oget_info(group_id, &oinfo) < 0) TEST_ERROR
    if(H5Oget_info(group_id2, &oinfo2) < 0) TEST_ERROR

    /* Sanity check object information for each group */
    if(oinfo.atime != 0) TEST_ERROR
    if(oinfo.mtime != 0) TEST_ERROR
    if(oinfo.ctime != 0) TEST_ERROR
    if(oinfo.btime != 0) TEST_ERROR
    if(oinfo.atime == oinfo2.atime) TEST_ERROR
    if(oinfo.mtime == oinfo2.mtime) TEST_ERROR
    if(oinfo.ctime == oinfo2.ctime) TEST_ERROR
    if(oinfo.btime == oinfo2.btime) TEST_ERROR
    if((oinfo.hdr.flags & H5O_HDR_STORE_TIMES) != 0) TEST_ERROR
    if((oinfo2.hdr.flags & H5O_HDR_STORE_TIMES) == 0) TEST_ERROR
    if(oinfo.hdr.space.total >= oinfo2.hdr.space.total) TEST_ERROR
    if(oinfo.hdr.space.meta >= oinfo2.hdr.space.meta) TEST_ERROR

    /* Close the property lists */
    if(H5Pclose(gcpl_id) < 0) TEST_ERROR
    if(H5Pclose(gcpl_id2) < 0) TEST_ERROR

    /* Close the groups */
    if(H5Gclose(group_id) < 0) TEST_ERROR
    if(H5Gclose(group_id2) < 0) TEST_ERROR

    /* Close the file */
    if(H5Fclose(file_id) < 0) TEST_ERROR


    /* Re-open the file */
    if((file_id = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* Open groups */
    if((group_id = H5Gopen2(file_id, TIMESTAMP_GROUP_1, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group_id2 = H5Gopen2(file_id, TIMESTAMP_GROUP_2, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Retrieve the groups' creation properties */
    if((gcpl_id = H5Gget_create_plist(group_id)) < 0) TEST_ERROR
    if((gcpl_id2 = H5Gget_create_plist(group_id2)) < 0) TEST_ERROR

    /* Query & verify the object timestamp settings */
    if(H5Pget_obj_track_times(gcpl_id, &track_times) < 0) TEST_ERROR
    if(track_times != FALSE) TEST_ERROR
    if(H5Pget_obj_track_times(gcpl_id2, &track_times) < 0) TEST_ERROR
    if(track_times != TRUE) TEST_ERROR

    /* Query the object information for each group */
    if(H5Oget_info(group_id, &oinfo) < 0) TEST_ERROR
    if(H5Oget_info(group_id2, &oinfo2) < 0) TEST_ERROR

    /* Sanity check object information for each group */
    if(oinfo.atime != 0) TEST_ERROR
    if(oinfo.mtime != 0) TEST_ERROR
    if(oinfo.ctime != 0) TEST_ERROR
    if(oinfo.btime != 0) TEST_ERROR
    if(oinfo.atime == oinfo2.atime) TEST_ERROR
    if(oinfo.mtime == oinfo2.mtime) TEST_ERROR
    if(oinfo.ctime == oinfo2.ctime) TEST_ERROR
    if(oinfo.btime == oinfo2.btime) TEST_ERROR
    if((oinfo.hdr.flags & H5O_HDR_STORE_TIMES) != 0) TEST_ERROR
    if((oinfo2.hdr.flags & H5O_HDR_STORE_TIMES) == 0) TEST_ERROR
    if(oinfo.hdr.space.total >= oinfo2.hdr.space.total) TEST_ERROR
    if(oinfo.hdr.space.meta >= oinfo2.hdr.space.meta) TEST_ERROR

    /* Close the property lists */
    if(H5Pclose(gcpl_id) < 0) TEST_ERROR
    if(H5Pclose(gcpl_id2) < 0) TEST_ERROR

    /* Close the groups */
    if(H5Gclose(group_id) < 0) TEST_ERROR
    if(H5Gclose(group_id2) < 0) TEST_ERROR

    /* Close the file */
    if(H5Fclose(file_id) < 0) TEST_ERROR

    PASSED();

    return 0;

error:
    /* Free resources */
    H5E_BEGIN_TRY {
        H5Pclose(gcpl_id);
        H5Gclose(group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;

    return -1;
} /* end timestamps() */


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Test links
 *
 * Return:	Success:	exit(0)
 *
 *		Failure:	exit(non-zero)
 *
 * Programmer:	Robb Matzke
 *              Friday, August 14, 1998
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t	fapl, fapl2;    /* File access property lists */
    int	nerrors = 0;
    hbool_t new_format;     /* Whether to use the new format or not */
    hbool_t efc;            /* Whether to use the external file cache */
    const char  *env_h5_drvr;      /* File Driver value from environment */

    env_h5_drvr = HDgetenv("HDF5_DRIVER");
    if(env_h5_drvr == NULL)
        env_h5_drvr = "nomatch";

    h5_reset();
    fapl = h5_fileaccess();

    /* Copy the file access property list */
    if((fapl2 = H5Pcopy(fapl)) < 0) TEST_ERROR

    /* Set the "use the latest version of the format" bounds for creating objects in the file */
    if(H5Pset_libver_bounds(fapl2, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0) TEST_ERROR

    /* Loop over using new group format */
    for(new_format = FALSE; new_format <= TRUE; new_format++) {
        hid_t my_fapl;

        /* Check for FAPL to use */
        if(new_format)
            my_fapl = fapl2;
        else
            my_fapl = fapl;

        /* General tests... (on both old & new format groups */
        nerrors += mklinks(my_fapl, new_format) < 0 ? 1 : 0;
        nerrors += cklinks(my_fapl, new_format) < 0 ? 1 : 0;
        nerrors += new_links(my_fapl, new_format) < 0 ? 1 : 0;
        nerrors += ck_new_links(my_fapl, new_format) < 0 ? 1 : 0;
        nerrors += long_links(my_fapl, new_format) < 0 ? 1 : 0;
        nerrors += toomany(my_fapl, new_format) < 0 ? 1 : 0;

        /* Test new H5L link creation routine */
        nerrors += test_lcpl(my_fapl, new_format);
        nerrors += test_move(my_fapl, new_format);
        nerrors += test_copy(my_fapl, new_format);
        nerrors += test_move_preserves(my_fapl, new_format);
#ifndef H5_NO_DEPRECATED_SYMBOLS
        nerrors += test_deprec(my_fapl, new_format);
#endif /* H5_NO_DEPRECATED_SYMBOLS */

        /* tests for external link */
        /* Test external file cache first, so it sees the default efc setting on
         * the fapl */
        nerrors += external_file_cache(my_fapl, new_format) < 0 ? 1 : 0;

        /* This test cannot run with the EFC because it assumes that an
         * intermediate file is not held open */
        nerrors += external_link_mult(my_fapl, new_format) < 0 ? 1 : 0;

        /* This test cannot run with the EFC because the EFC cannot currently
         * reopen a cached file with a different intent */
        nerrors += external_set_elink_acc_flags(my_fapl, new_format) < 0 ? 1 : 0;

        /* Try external link tests both with and without the external file cache
         */
        for(efc = FALSE; efc <= TRUE; efc++) {
            if(efc) {
                if(H5Pset_elink_file_cache_size(my_fapl, 8) < 0)
                    TEST_ERROR
                printf("\n---Testing with external file cache---\n");
            } /* end if */
            else {
                if(H5Pset_elink_file_cache_size(my_fapl, 0) < 0)
                    TEST_ERROR
                printf("\n---Testing without external file cache---\n");
            } /* end else */

#ifndef H5_CANNOT_OPEN_TWICE
            nerrors += external_link_root(my_fapl, new_format) < 0 ? 1 : 0;
#endif /* H5_CANNOT_OPEN_TWICE */
            nerrors += external_link_path(my_fapl, new_format) < 0 ? 1 : 0;
#ifndef H5_CANNOT_OPEN_TWICE
            nerrors += external_link_self(my_fapl, new_format) < 0 ? 1 : 0;
            nerrors += external_link_pingpong(my_fapl, new_format) < 0 ? 1 : 0;
            nerrors += external_link_toomany(my_fapl, new_format) < 0 ? 1 : 0;
#endif /* H5_CANNOT_OPEN_TWICE */
            nerrors += external_link_dangling(my_fapl, new_format) < 0 ? 1 : 0;
            nerrors += external_link_recursive(my_fapl, new_format) < 0 ? 1 : 0;
            nerrors += external_link_query(my_fapl, new_format) < 0 ? 1 : 0;
            nerrors += external_link_unlink_compact(my_fapl, new_format) < 0 ? 1 : 0;
            nerrors += external_link_unlink_dense(my_fapl, new_format) < 0 ? 1 : 0;
            nerrors += external_link_move(my_fapl, new_format) < 0 ? 1 : 0;
            nerrors += external_link_ride(my_fapl, new_format) < 0 ? 1 : 0;
#ifndef H5_CANNOT_OPEN_TWICE
            nerrors += external_link_closing(my_fapl, new_format) < 0 ? 1 : 0;
#endif /* H5_CANNOT_OPEN_TWICE */
            nerrors += external_link_endian(new_format) < 0 ? 1 : 0;
            nerrors += external_link_strong(my_fapl, new_format) < 0 ? 1 : 0;

            nerrors += external_link_prefix(my_fapl, new_format) < 0 ? 1 : 0;
            nerrors += external_link_abs_mainpath(my_fapl, new_format) < 0 ? 1 : 0;
            nerrors += external_link_rel_mainpath(my_fapl, new_format) < 0 ? 1 : 0;
            nerrors += external_link_cwd(my_fapl, new_format) < 0 ? 1 : 0;
            nerrors += external_link_abstar(my_fapl, new_format) < 0 ? 1 : 0;
            nerrors += external_link_abstar_cur(my_fapl, new_format) < 0 ? 1 : 0;
            nerrors += external_link_reltar(my_fapl, new_format) < 0 ? 1 : 0;
            nerrors += external_link_chdir(my_fapl, new_format) < 0 ? 1 : 0;
            nerrors += external_set_elink_fapl1(my_fapl, new_format) < 0 ? 1 : 0;
            nerrors += external_set_elink_fapl2(my_fapl, new_format) < 0 ? 1 : 0;
            nerrors += external_set_elink_fapl3(new_format) < 0 ? 1 : 0;
            nerrors += external_set_elink_cb(my_fapl, new_format) < 0 ? 1 : 0;
#ifdef H5_HAVE_WINDOW_PATH
            nerrors += external_link_win1(my_fapl, new_format) < 0 ? 1 : 0;
            nerrors += external_link_win2(my_fapl, new_format) < 0 ? 1 : 0;
            nerrors += external_link_win3(my_fapl, new_format) < 0 ? 1 : 0;
            nerrors += external_link_win4(my_fapl, new_format) < 0 ? 1 : 0;
            nerrors += external_link_win5(my_fapl, new_format) < 0 ? 1 : 0;
            nerrors += external_link_win6(my_fapl, new_format) < 0 ? 1 : 0;
            nerrors += external_link_win7(my_fapl, new_format) < 0 ? 1 : 0;
            nerrors += external_link_win8(my_fapl, new_format) < 0 ? 1 : 0;
            nerrors += external_link_win9(my_fapl, new_format) < 0 ? 1 : 0;
#endif
            nerrors += external_symlink(env_h5_drvr, my_fapl, new_format) < 0 ? 1 : 0;
            nerrors += external_copy_invalid_object(my_fapl, new_format) < 0 ? 1 : 0;
            nerrors += external_dont_fail_to_source(my_fapl, new_format) < 0 ? 1 : 0;
#ifndef H5_CANNOT_OPEN_TWICE
            nerrors += external_open_twice(my_fapl, new_format) < 0 ? 1 : 0;
#endif /* H5_CANNOT_OPEN_TWICE */
        } /* end for */

        /* These tests assume that external links are a form of UD links,
         * so assume that everything that passed for external links
         * above has already been tested for UD links.
         */
        if(new_format == TRUE) {
            nerrors += ud_hard_links(fapl2) < 0 ? 1 : 0;     /* requires new format groups */
            nerrors += ud_link_reregister(fapl2) < 0 ? 1 : 0;        /* requires new format groups */
        } /* end if */
        nerrors += ud_callbacks(my_fapl, new_format) < 0 ? 1 : 0;
        nerrors += ud_link_errors(my_fapl, new_format) < 0 ? 1 : 0;
        nerrors += lapl_udata(my_fapl, new_format) < 0 ? 1 : 0;
        nerrors += lapl_nlinks(my_fapl, new_format) < 0 ? 1 : 0;
        nerrors += linkinfo(my_fapl, new_format) < 0 ? 1 : 0;

        /* Misc. extra tests, useful for both new & old format files */
        nerrors += link_visit(my_fapl, new_format) < 0 ? 1 : 0;
        nerrors += link_visit_by_name(my_fapl, new_format) < 0 ? 1 : 0;
        nerrors += obj_visit(my_fapl, new_format) < 0 ? 1 : 0;
        nerrors += obj_visit_by_name(my_fapl, new_format) < 0 ? 1 : 0;
        nerrors += obj_visit_stop(my_fapl, new_format) < 0 ? 1 : 0;
        nerrors += link_filters(my_fapl, new_format) < 0 ? 1 : 0;
#ifndef H5_CANNOT_OPEN_TWICE
        nerrors += obj_exists(my_fapl, new_format) < 0 ? 1 : 0;
#endif /* H5_CANNOT_OPEN_TWICE */

        /* Keep this test last, it's testing files that are used above */
        /* do not do this for files used by external link tests */
        nerrors += check_all_closed(my_fapl, new_format, EXTSTOP) < 0 ? 1 : 0;
    } /* end for */

    /* New group revision feature tests */
    nerrors += corder_create_empty(fapl2) < 0 ? 1 : 0;
/* XXX: when creation order indexing is fully working, go back and add checks
*      to these tests to make certain that the creation order values are
*      correct.
*/
    nerrors += corder_create_compact(fapl2) < 0 ? 1 : 0;
    nerrors += corder_create_dense(fapl2) < 0 ? 1 : 0;
    nerrors += corder_transition(fapl2) < 0 ? 1 : 0;
    nerrors += corder_delete(fapl2) < 0 ? 1 : 0;
    nerrors += link_info_by_idx(fapl2) < 0 ? 1 : 0;
    nerrors += delete_by_idx(fapl2) < 0 ? 1 : 0;
    nerrors += link_iterate(fapl2) < 0 ? 1 : 0;
    nerrors += open_by_idx(fapl2) < 0 ? 1 : 0;
    nerrors += object_info(fapl2) < 0 ? 1 : 0;
    nerrors += group_info(fapl2) < 0 ? 1 : 0;
    nerrors += timestamps(fapl2) < 0 ? 1 : 0;

    /* Test new API calls on old-style groups */
    nerrors += link_info_by_idx_old(fapl) < 0 ? 1 : 0;
    nerrors += delete_by_idx_old(fapl) < 0 ? 1 : 0;
    nerrors += link_iterate_old(fapl) < 0 ? 1 : 0;
    nerrors += open_by_idx_old(fapl) < 0 ? 1 : 0;
    nerrors += object_info_old(fapl) < 0 ? 1 : 0;
    nerrors += group_info_old(fapl) < 0 ? 1 : 0;

    /* Close 2nd FAPL */
    H5Pclose(fapl2);

    /* Verify symbol table messages are cached */
    nerrors += (h5_verify_cached_stabs(FILENAME, fapl) < 0 ? 1 : 0);

    h5_cleanup(FILENAME, fapl);

    /* Test that external links can be used after a library reset.  MUST be
     * called last so the reset doesn't interfere with the property lists.  This
     * routine will delete its own file. */
    nerrors += external_reset_register() < 0 ? 1 : 0;

    /* Results */
    if(nerrors) {
        printf("***** %d LINK TEST%s FAILED! *****\n",
                nerrors, 1 == nerrors ? "" : "S");
        exit(1);
    }
    printf("All link tests passed.\n");

    /* clean up symlink created by external link tests */
    HDremove(SYMLINK1);
    HDremove(SYMLINK2);

    /* clean up tmp directory created by external link tests */
    HDrmdir(TMPDIR);
    HDrmdir(TMPDIR2);

    return 0;

error:
    puts("*** TESTS FAILED ***");
    return 1;
}

