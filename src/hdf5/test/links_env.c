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
 * Purpose:	Tests hard, soft (symbolic) & external links.
 */

#define H5G_PACKAGE
#define H5G_TESTING

#include "h5test.h"
#include "H5srcdir.h"
#include "H5Gpkg.h"		/* Groups 				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Lprivate.h"         /* Links                                */

#ifdef H5_VMS
#define TMPDIR          "[.tmp]"
#else /* H5_VMS */
#define TMPDIR          "tmp/"
#endif /* H5_VMS */
#define NAME_BUF_SIZE   1024

const char *FILENAME[] = {
    "extlinks_env0",	/* 0: main file */
    "extlinks_env1",	/* 1: target file */
    TMPDIR "extlinks_env1", /* 2 */
    NULL
};

static int external_link_env(hid_t fapl, hbool_t new_format);


/*-------------------------------------------------------------------------
 * Function:    external_link_env (moved from links.c)
 *
 * Purpose: 	Verify that the target file is found successfully in "tmp" directory
 *		via searching the pathnames set in the environment variable HDF5_EXT_PREFIX.
 *		1. Target link: "extlinks_env1"
 *		2. Main file: "extlinks_env0"
 *		3. Target file is created in: "tmp/extlinks_env1"
 * 		4. The environment variable "HDF5_EXT_PREFIX" is set to ".:tmp"
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
external_link_env(hid_t fapl, hbool_t new_format)
{
    hid_t fid = (-1);     	/* File ID */
    hid_t gid = (-1);	        /* Group IDs */
    const char *envval = NULL;	/* Pointer to environment variable */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE],
    		filename3[NAME_BUF_SIZE];	/* Holders for filename */

    if(new_format)
        TESTING("external links via environment variable (w/new group format)")
    else
        TESTING("external links via environment variable")

    if ((envval = HDgetenv("HDF5_EXT_PREFIX")) == NULL)
        envval = "nomatch";
    if (HDstrcmp(envval, ".:tmp")) TEST_ERROR

    /* Set up name for main file:"extlinks_env0" */
    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);

    /* Set up name for external linked target file: "extlinks_env1" */
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);

    /* Create "tmp" directory */
    if(HDmkdir(TMPDIR, (mode_t)0755) < 0 && errno != EEXIST)
	TEST_ERROR

    /* Set up name (location) for the target file: "tmp/extlinks1" */
    h5_fixname(FILENAME[2], fapl, filename3, sizeof filename3);

    /* Create the target file in "tmp" directory */
    if((fid=H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((gid=H5Gcreate2(fid, "A", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Closing for target file */
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

    /* Should be able to find the target file from pathnames set via HDF5_EXT_PREFIX */
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
} /* end external_link_env() */


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Test external link with environment variable HDF5_EXT_PREFIX
 *
 * Return:	Success:	exit(0)
 *		Failure:	exit(non-zero)
 *
 * Programmer:	Vailin Choi; Nov 2010
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t fapl; 	/* File access property lists */
    int	nerrors = 0;	/* Error from tests */
    const char  *env_h5_drvr;      /* File Driver value from environment */

    env_h5_drvr = HDgetenv("HDF5_DRIVER");
    if(env_h5_drvr == NULL)
        env_h5_drvr = "nomatch";

    h5_reset();
    fapl = h5_fileaccess();

    nerrors += external_link_env(fapl, FALSE) < 0 ? 1 : 0;

    /* Set the "use the latest version of the format" bounds for creating objects in the file */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0) TEST_ERROR

    nerrors += external_link_env(fapl, TRUE) < 0 ? 1 : 0;

    /* Verify symbol table messages are cached */
    nerrors += (h5_verify_cached_stabs(FILENAME, fapl) < 0 ? 1 : 0);

    h5_cleanup(FILENAME, fapl);

    /* Results */
    if(nerrors) {
        printf("***** %d External Link (HDF5_EXT_PREFIX) test%s FAILED! *****\n",
                nerrors, 1 == nerrors ? "" : "s");
        exit(1);
    }
    printf("All external Link (HDF5_EXT_PREFIX) tests passed.\n");

    /* clean up tmp directory created by external link tests */
    HDrmdir(TMPDIR);

    return 0;

error:
    puts("*** TESTS FAILED ***");
    return 1;
}
