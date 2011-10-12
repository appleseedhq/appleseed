/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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

/*****************************************************************************
   FILE
   tlinks.cpp - HDF5 C++ testing functionalities associated with the
        C attribute interface (H5L)

 ***************************************************************************/

#ifdef OLD_HEADER_FILENAME
#include <iostream.h>
#else
#include <iostream>
#endif
#include <string>

#ifndef H5_NO_NAMESPACE
#ifndef H5_NO_STD
    using std::cerr;
    using std::endl;
#endif  // H5_NO_STD
#endif

#include "H5Cpp.h"      // C++ API header file

#ifndef H5_NO_NAMESPACE
    using namespace H5;
#endif

#include "h5cpputil.h"  // C++ test utilility header file

// A lot of the definition inherited from C test links.c is left here until
// the H5L API is implemented and tests are completed - BMR 10/19/2009
/*
 * This file needs to access private information from the H5G package.
 * This file also needs to access the group testing code.
 */
//#define H5G_PACKAGE
//#define H5G_TESTING

//#include "h5test.h"
//#include "H5Gpkg.h"		/* Groups 				*/
//#include "H5Iprivate.h"		/* IDs			  		*/
//#include "H5Lprivate.h"         /* Links                                */

/* File for external link test.  Created with gen_udlinks.c */
#define LINKED_FILE  "be_extlink2.h5"

#ifdef H5_VMS
#if 0
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
    "[.tmp]extlinks0",	/* 13: */
    "extlinks1",	/* 14: target files */
    "[.tmp]extlinks1",	/* 15: */
    "extlinks2",	/* 16: */
    "[.tmp]extlinks2",	/* 17: */
    "extlinks3",	/* 18: */
    "[.tmp]extlinks3",	/* 19: */
    "extlinks4",	/* 20: */
    "[.tmp]extlinks4",	/* 21: */
    "extlinks5",	/* 22: */
    "[.tmp]extlinks6",	/* 23: */
    "extlinks7",	/* 24: */
    "[.tmp]extlinks7",	/* 25: */
    "[.tmp]extlinks8",	/* 26: */
    "extlinks9",	/* 27: */
    "[.tmp]extlinks9",	/* 28: */
    "extlinks10",	/* 29: */ /* TESTS for windows */
    "[.tmp]extlinks10",	/* 30: */
    "[.tmp]extlinks11",	/* 31: */
    "[.tmp]extlinks12",	/* 32: */
    "extlinks13",	/* 33: */
    "[.tmp]extlinks13",	/* 34: */
    "[.tmp]extlinks14",	/* 35: */
    "[.tmp]extlinks15",	/* 36: */
    "extlinks16A",	/* 37: */ /* TESTS for H5P_set_elink_fapl */
    "extlinks16B",	/* 38: */
    "extlinks17",	/* 39: */
    "extlinks18A",	/* 40: */
    "extlinks18B",	/* 41: */
    "extlinks19A",	/* 42: */
    "extlinks19B",	/* 43: */
    "extlinks20",	/* 44: */
    NULL
};
#endif // 0

#define TMPDIR          "[.tmp]"
#else
#if 0
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
    "tmp/extlinks0",	/* 13: */
    "extlinks1",	/* 14: target files */
    "tmp/extlinks1",	/* 15: */
    "extlinks2",	/* 16: */
    "tmp/extlinks2",	/* 17: */
    "extlinks3",	/* 18: */
    "tmp/extlinks3",	/* 19: */
    "extlinks4",	/* 20: */
    "tmp/extlinks4",	/* 21: */
    "extlinks5",	/* 22: */
    "tmp/extlinks6",	/* 23: */
    "extlinks7",	/* 24: */
    "tmp/extlinks7",	/* 25: */
    "tmp/extlinks8",	/* 26: */
    "extlinks9",	/* 27: */
    "tmp/extlinks9",	/* 28: */
    "extlinks10",	/* 29: */ /* TESTS for windows */
    "tmp/extlinks10",	/* 30: */
    "tmp/extlinks11",	/* 31: */
    "tmp/extlinks12",	/* 32: */
    "extlinks13",	/* 33: */
    "tmp/extlinks13",	/* 34: */
    "tmp/extlinks14",	/* 35: */
    "tmp/extlinks15",	/* 36: */
    "extlinks16A",	/* 37: */ /* TESTS for H5P_set_elink_fapl */
    "extlinks16B",	/* 38: */
    "extlinks17",	/* 39: */
    "extlinks18A",	/* 40: */
    "extlinks18B",	/* 41: */
    "extlinks19A",	/* 42: */
    "extlinks19B",	/* 43: */
    "extlinks20",	/* 44: */
    NULL
};

#endif // 0

#define TMPDIR          "tmp"
#endif

#define FAMILY_SIZE	1024
#define CORE_INCREMENT  1024
#define NUM400		400

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

static const char *FILENAME[] = {
    "link0",
    "link1.h5",
    "link2.h5",
    NULL
};


/*-------------------------------------------------------------------------
 * Function:	test_basic_links
 *
 * Purpose:	Test building a file with assorted links.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Binh-Minh Ribler
 *		October 16, 2009
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void test_basic_links(hid_t fapl_id, hbool_t new_format)
{
    hsize_t	        size[1] = {1};
    char		filename[NAME_BUF_SIZE];

    // Use the file access template id to create a file access prop. list.
    FileAccPropList fapl(fapl_id);

    try
    {
	if(new_format)
	    SUBTEST("Link creation (w/new group format)")
	else
	    SUBTEST("Link creation")

	h5_fixname(FILENAME[0], fapl_id, filename, sizeof filename);
	H5File file(filename, H5F_ACC_TRUNC, FileCreatPropList::DEFAULT, fapl);

	// Create simple dataspace
	DataSpace scalar (1, size, size);

	// Create a group then close it by letting the object go out of scope
	{
	    Group group(file.createGroup("grp1", 0));
	}

	// Create a dataset then close it by letting the object go out of scope
	{
	    DataSet dset1(file.createDataSet("dset1", PredType::NATIVE_INT, scalar));
	}

	hid_t file_id = file.getId();

	// Because these are not implemented in the C++ API yet, they are
	// used so CommonFG::getLinkval can be tested.
	// Create a hard link
	if(H5Lcreate_hard(
		file_id, "dset1", H5L_SAME_LOC, "grp1/hard1",
		H5P_DEFAULT, H5P_DEFAULT) < 0)
	    throw Exception("test_basic_links", "H5Lcreate_hard failed");

	// Create a symbolic link
	if(H5Lcreate_soft(
		"/dset1", file_id, "grp1/soft", H5P_DEFAULT, H5P_DEFAULT) < 0)
	    throw Exception("test_basic_links", "H5Lcreate_soft failed");

	// Create a symbolic link to something that doesn't exist
	if(H5Lcreate_soft(
		"foobar", file_id, "grp1/dangle", H5P_DEFAULT, H5P_DEFAULT) < 0)
	    throw Exception("test_basic_links", "H5Lcreate_soft failed");

	// Create a recursive symbolic link
	if(H5Lcreate_soft(
		"/grp1/recursive", file_id, "/grp1/recursive",
		H5P_DEFAULT, H5P_DEFAULT) < 0)
	    throw Exception("test_basic_links", "H5Lcreate_soft failed");

	// Verify link values before closing the file

	H5std_string softlink_val = file.getLinkval("grp1/soft");
	verify_val(softlink_val, "/dset1", "H5File::getLinkval grp1/soft", __LINE__, __FILE__);

	H5std_string dngllink_val = file.getLinkval("grp1/dangle");
	verify_val(dngllink_val, "foobar", "H5File::getLinkval grp1/dangle", __LINE__, __FILE__);

	H5std_string reclink_val = file.getLinkval("grp1/recursive");
	verify_val(reclink_val, "/grp1/recursive", "H5File::getLinkval grp1/recursive", __LINE__, __FILE__);

    } // end of try block
    catch (Exception E)
    {
	issue_fail_msg("test_basic_links()", __LINE__, __FILE__, E.getCDetailMsg());
    }

    // Open the file and check on the links in it
    try
    {
	// Open the file above
	H5File file(filename, H5F_ACC_RDWR, FileCreatPropList::DEFAULT, fapl);

	// Verify link existence
	if(H5Lexists(file.getId(), "dset1", H5P_DEFAULT) != TRUE)
	    throw InvalidActionException("H5Lexists", "dset1 doesn't exist");
	if(H5Lexists(file.getId(), "grp1/soft", H5P_DEFAULT) != TRUE)
	    throw InvalidActionException("H5Lexists", "grp1/soft doesn't exist");

	// Verify link values
	H5std_string softlink_val = file.getLinkval("grp1/soft");
	verify_val(softlink_val, "/dset1", "H5File::getLinkval grp1/soft", __LINE__, __FILE__);

	H5std_string reclink_val = file.getLinkval("grp1/recursive");
	verify_val(reclink_val, "/grp1/recursive", "H5File::getLinkval grp1/recursive", __LINE__, __FILE__);

	PASSED();
    } // end of try block
    catch (Exception E)
    {
	issue_fail_msg("test_basic_links()", __LINE__, __FILE__, E.getCDetailMsg());
    }
}


/*-------------------------------------------------------------------------
 * Function:	test_links
 *
 * Purpose:	Test links
 *
 * Return:	None
 *
 * Programmer:	Binh-Minh Ribler
 *              October 16, 2009
 *
 *-------------------------------------------------------------------------
 */
#ifdef __cplusplus
extern "C"
#endif
void test_links()
{
    hid_t	fapl_id, fapl2_id;    /* File access property lists */
    hbool_t new_format;     /* Whether to use the new format or not */
    const char  *envval;

    envval = HDgetenv("HDF5_DRIVER");
    if(envval == NULL)
        envval = "nomatch";

    fapl_id = h5_fileaccess();

    // Output message about test being performed
    //MESSAGE("Testing Various Links\n");
    MESSAGE(5, ("Testing Various Links\n"));
    try
    {
	/* Copy the file access property list */
	if((fapl2_id = H5Pcopy(fapl_id)) < 0)
	    throw Exception("test_links", "H5Pcopy failed");

	/* Set the "use the latest version of the format" bounds for creating objects in the file */
	if(H5Pset_libver_bounds(fapl2_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
	throw Exception("test_links", "H5Pset_libver_bounds failed");

	/* Loop over using new group format */
	for(new_format = FALSE; new_format <= TRUE; new_format++)
	{
	    hid_t my_fapl_id;

	    /* Check for FAPL to use */
	    if(new_format)
		my_fapl_id = fapl2_id;
	    else
		my_fapl_id = fapl_id;

	    /* General tests... (on both old & new format groups */
	    // FileAccPropList may be passed in instead of fapl id
	    test_basic_links(my_fapl_id, new_format);
#if 0
// these tests are from the C test links.c and left here for future
// implementation of H5L API
	nerrors += test_basic_links(fapl_id, new_format) < 0 ? 1 : 0;
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
#ifndef H5_CANNOT_OPEN_TWICE
        nerrors += external_link_root(my_fapl, new_format) < 0 ? 1 : 0;
#endif /* H5_CANNOT_OPEN_TWICE */
        nerrors += external_link_path(my_fapl, new_format) < 0 ? 1 : 0;
        nerrors += external_link_mult(my_fapl, new_format) < 0 ? 1 : 0;
#ifndef H5_CANNOT_OPEN_TWICE
        nerrors += external_link_self(envval, my_fapl, new_format) < 0 ? 1 : 0;
        nerrors += external_link_pingpong(envval, my_fapl, new_format) < 0 ? 1 : 0;
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
        nerrors += external_link_closing(envval, my_fapl, new_format) < 0 ? 1 : 0;
#endif /* H5_CANNOT_OPEN_TWICE */
        nerrors += external_link_endian(new_format) < 0 ? 1 : 0;
        nerrors += external_link_strong(my_fapl, new_format) < 0 ? 1 : 0;

        /* tests for external link */
        nerrors += external_link_env(my_fapl, new_format) < 0 ? 1 : 0;
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
        nerrors += external_set_elink_acc_flags(my_fapl, new_format) < 0 ? 1 : 0;
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

        /* Keep this test last, it's testing files that are used above */
        /* do not do this for files used by external link tests */
        nerrors += check_all_closed(my_fapl, new_format, EXTSTOP) < 0 ? 1 : 0;
#endif // 0
	} /* end for */

#if 0
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

#endif
	/* Close 2nd FAPL */
	H5Pclose(fapl2_id);

	h5_cleanup(FILENAME, fapl_id);

	/* Test that external links can be used after a library reset.  MUST be
	* called last so the reset doesn't interfere with the property lists.  This
	* routine will delete its own file. */
	/* nerrors += external_reset_register() < 0 ? 1 : 0;
 */
    }
    catch (Exception E)
    {
	issue_fail_msg("test_links()", __LINE__, __FILE__, E.getCDetailMsg());
    }

}

/*-------------------------------------------------------------------------
 * Function:	cleanup_links
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	Binh-Minh Ribler
 *		October 16, 2009
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
#ifdef __cplusplus
extern "C"
#endif
void cleanup_links()
{
    HDremove(FILENAME[0]);
}

