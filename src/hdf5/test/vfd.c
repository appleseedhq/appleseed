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
 * Programmer:  Raymond Lu<slu@ncsa.uiuc.edu>
 *              Tuesday, Sept 24, 2002
 *
 * Purpose:     Tests the basic features of Virtual File Drivers
 */

#include "h5test.h"

#define KB              1024U
#define FAMILY_NUMBER   4
#define FAMILY_SIZE     (1*KB)
#define FAMILY_SIZE2    (5*KB)
#define MULTI_SIZE      128
#define CORE_INCREMENT  (4*KB)

/*Macros for Direct VFD*/
#define MBOUNDARY    512
#define FBSIZE       (4*KB)
#define CBSIZE       (8*KB)
#define THRESHOLD    1
#define DSET1_NAME   "dset1"
#define DSET1_DIM1   1024
#define DSET1_DIM2   32
#define DSET2_NAME   "dset2"
#define DSET2_DIM    4

const char *FILENAME[] = {
    "sec2_file",         /*0*/
    "core_file",         /*1*/
    "family_file",       /*2*/
    "new_family_v16_",   /*3*/
    "multi_file",        /*4*/
    "direct_file",       /*5*/
    "log_file",          /*6*/
    "stdio_file",        /*7*/
    "windows_file",      /*8*/
    NULL
};

#define LOG_FILENAME "log_vfd_out.log"

#define COMPAT_BASENAME "family_v16_"



/*-------------------------------------------------------------------------
 * Function:    test_sec2
 *
 * Purpose:     Tests the file handle interface for SEC2 driver
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              Tuesday, Sept 24, 2002
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_sec2(void)
{
	hid_t        file            = -1;
	hid_t        fapl            = -1;
	hid_t        access_fapl     = -1;
    char         filename[1024];
    int          *fhandle        = NULL;
    hsize_t      file_size       = 0;

    TESTING("SEC2 file driver");

    /* Set property list and file name for SEC2 driver. */
    fapl = h5_fileaccess();
    if(H5Pset_fapl_sec2(fapl) < 0)
        TEST_ERROR;
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    /* Retrieve the access property list... */
    if((access_fapl = H5Fget_access_plist(file)) < 0)
        TEST_ERROR;

    /* Check that the driver is correct */
    if(H5FD_SEC2 != H5Pget_driver(access_fapl))
        TEST_ERROR;

    /* ...and close the property list */
    if(H5Pclose(access_fapl) < 0)
        TEST_ERROR;

    /* Check file handle API */
    if(H5Fget_vfd_handle(file, H5P_DEFAULT, (void **)&fhandle) < 0)
        TEST_ERROR;
    if(*fhandle < 0)
        TEST_ERROR;

    /* Check file size API */
    if(H5Fget_filesize(file, &file_size) < 0)
        TEST_ERROR;

    /* There is no guarantee the size of metadata in file is constant.
     * Just try to check if it's reasonable.  It's 2KB right now.
     */
    if(file_size < 1 * KB || file_size > 4 * KB)
        TEST_ERROR;

    if(H5Fclose(file) < 0)
        TEST_ERROR;

    h5_cleanup(FILENAME, fapl);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl);
        H5Fclose(file);
    } H5E_END_TRY;
    return -1;
}



/*-------------------------------------------------------------------------
 * Function:    test_direct
 *
 * Purpose:     Tests the file handle interface for DIRECT I/O driver
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              Wednesday, 20 September 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_direct(void)
{
#ifdef H5_HAVE_DIRECT
    hid_t       file=(-1), fapl, access_fapl = -1;
    hid_t	dset1=-1, dset2=-1, space1=-1, space2=-1;
    char        filename[1024];
    int         *fhandle=NULL;
    hsize_t     file_size;
    hsize_t	dims1[2], dims2[1];
    size_t	mbound;
    size_t	fbsize;
    size_t	cbsize;
    int		*points, *check, *p1, *p2;
    int		wdata2[DSET2_DIM] = {11,12,13,14};
    int		rdata2[DSET2_DIM];
    int		i, j, n;
#endif /*H5_HAVE_DIRECT*/

    TESTING("DIRECT I/O file driver");

#ifndef H5_HAVE_DIRECT
    SKIPPED();
    return 0;
#else /*H5_HAVE_DIRECT*/

    /* Set property list and file name for Direct driver.  Set memory alignment boundary
     * and file block size to 512 which is the minimum for Linux 2.6. */
    fapl = h5_fileaccess();
    if(H5Pset_fapl_direct(fapl, MBOUNDARY, FBSIZE, CBSIZE) < 0)
        TEST_ERROR;
    h5_fixname(FILENAME[5], fapl, filename, sizeof filename);

    /* Verify the file access properties */
    if(H5Pget_fapl_direct(fapl, &mbound, &fbsize, &cbsize) < 0)
        TEST_ERROR;
    if(mbound != MBOUNDARY || fbsize != FBSIZE || cbsize != CBSIZE)
	TEST_ERROR;

    if(H5Pset_alignment(fapl, (hsize_t)THRESHOLD, (hsize_t)FBSIZE) < 0)
	TEST_ERROR;

    H5E_BEGIN_TRY {
        file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    } H5E_END_TRY;
    if(file<0) {
        H5Pclose (fapl);
        SKIPPED();
        printf("	Probably the file system doesn't support Direct I/O\n");
        return 0;
    }

    /* Retrieve the access property list... */
    if ((access_fapl = H5Fget_access_plist(file)) < 0)
        TEST_ERROR;

    /* Check that the driver is correct */
    if(H5FD_DIRECT != H5Pget_driver(access_fapl))
        TEST_ERROR;

    /* ...and close the property list */
    if (H5Pclose(access_fapl) < 0)
        TEST_ERROR;

    /* Check file handle API */
    if(H5Fget_vfd_handle(file, H5P_DEFAULT, (void **)&fhandle) < 0)
        TEST_ERROR;
    if(*fhandle<0)
        TEST_ERROR;

    /* Check file size API */
    if(H5Fget_filesize(file, &file_size) < 0)
        TEST_ERROR;

    /* There is no guarantee of the number of metadata allocations, but it's
     * 4 currently and the size of the file should be between 3 & 4 file buffer
     * sizes..
     */
    if(file_size < (FBSIZE * 3) || file_size >= (FBSIZE * 4))
        TEST_ERROR;

    /* Allocate aligned memory for data set 1. For data set 1, everything is aligned including
     * memory address, size of data, and file address. */
    if(posix_memalign(&points, (size_t)FBSIZE, (size_t)(DSET1_DIM1*DSET1_DIM2*sizeof(int)))!=0)
        TEST_ERROR;

    if(posix_memalign(&check, (size_t)FBSIZE, (size_t)(DSET1_DIM1*DSET1_DIM2*sizeof(int)))!=0)
        TEST_ERROR;

    /* Initialize the dset1 */
    p1 = points;
    for(i = n = 0; i < DSET1_DIM1; i++)
	for(j = 0; j < DSET1_DIM2; j++)
	    *p1++ = n++;

    /* Create the data space1 */
    dims1[0] = DSET1_DIM1;
    dims1[1] = DSET1_DIM2;
    if((space1 = H5Screate_simple(2, dims1, NULL)) < 0)
        TEST_ERROR;

    /* Create the dset1 */
    if((dset1 = H5Dcreate2(file, DSET1_NAME, H5T_NATIVE_INT, space1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Write the data to the dset1 */
    if(H5Dwrite(dset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, points) < 0)
        TEST_ERROR;

    if(H5Dclose(dset1) < 0)
        TEST_ERROR;

    if((dset1 = H5Dopen2(file, DSET1_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Read the data back from dset1 */
    if(H5Dread(dset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, check) < 0)
        TEST_ERROR;

    /* Check that the values read are the same as the values written */
    p1 = points;
    p2 = check;
    for(i = 0; i < DSET1_DIM1; i++)
	for(j = 0; j < DSET1_DIM2; j++)
	    if(*p1++ != *p2++) {
		H5_FAILED();
		printf("    Read different values than written in data set 1.\n");
		printf("    At index %d,%d\n", i, j);
        	TEST_ERROR;
	    } /* end if */

    /* Create the data space2. For data set 2, memory address and data size are not aligned. */
    dims2[0] = DSET2_DIM;
    if((space2 = H5Screate_simple(1, dims2, NULL)) < 0)
        TEST_ERROR;

    /* Create the dset2 */
    if((dset2 = H5Dcreate2(file, DSET2_NAME, H5T_NATIVE_INT, space2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Write the data to the dset1 */
    if(H5Dwrite(dset2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata2) < 0)
        TEST_ERROR;

    if(H5Dclose(dset2) < 0)
        TEST_ERROR;

    if((dset2 = H5Dopen2(file, DSET2_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Read the data back from dset1 */
    if(H5Dread(dset2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata2) < 0)
        TEST_ERROR;

    /* Check that the values read are the same as the values written */
    for(i = 0; i < DSET2_DIM; i++)
	if(wdata2[i] != rdata2[i]) {
	    H5_FAILED();
	    printf("    Read different values than written in data set 2.\n");
	    printf("    At index %d\n", i);
            TEST_ERROR;
	} /* end if */

    if(H5Sclose(space1) < 0)
        TEST_ERROR;
    if(H5Dclose(dset1) < 0)
        TEST_ERROR;
    if(H5Sclose(space2) < 0)
        TEST_ERROR;
    if(H5Dclose(dset2) < 0)
        TEST_ERROR;
    if(H5Fclose(file) < 0)
        TEST_ERROR;
    if(points)
	free(points);
    if(check)
	free(check);

    h5_cleanup(FILENAME, fapl);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose (fapl);
        H5Sclose(space1);
        H5Dclose(dset1);
        H5Sclose(space2);
        H5Dclose(dset2);
        H5Fclose(file);
    } H5E_END_TRY;
    return -1;
#endif /*H5_HAVE_DIRECT*/
}


/*-------------------------------------------------------------------------
 * Function:    test_core
 *
 * Purpose:     Tests the file handle interface for CORE driver
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              Tuesday, Sept 24, 2002
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_core(void)
{
    hid_t       file=(-1), fapl, access_fapl = -1;
    char        filename[1024];
    void        *fhandle=NULL;
    hsize_t     file_size;
    int		*points, *check, *p1, *p2;
    hid_t	dset1=-1, space1=-1;
    hsize_t	dims1[2];
    int		i, j, n;

    TESTING("CORE file driver");

    /* Set property list and file name for CORE driver */
    fapl = h5_fileaccess();
    if(H5Pset_fapl_core(fapl, (size_t)CORE_INCREMENT, TRUE) < 0)
        TEST_ERROR;
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);

    if((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    /* Retrieve the access property list... */
    if ((access_fapl = H5Fget_access_plist(file)) < 0)
        TEST_ERROR;

    /* Check that the driver is correct */
    if(H5FD_CORE != H5Pget_driver(access_fapl))
        TEST_ERROR;

    /* ...and close the property list */
    if (H5Pclose(access_fapl) < 0)
        TEST_ERROR;

    if(H5Fget_vfd_handle(file, H5P_DEFAULT, &fhandle) < 0)
        TEST_ERROR;
    if(fhandle==NULL)
    {
        printf("fhandle==NULL\n");
               TEST_ERROR;
    }

    /* Check file size API */
    if(H5Fget_filesize(file, &file_size) < 0)
        TEST_ERROR;

    /* There is no garantee the size of metadata in file is constant.
     * Just try to check if it's reasonable.  Why is this 4KB?
     */
    if(file_size<2*KB || file_size>6*KB)
        TEST_ERROR;

    if(H5Fclose(file) < 0)
        TEST_ERROR;


    /* Open the file with backing store off for read and write.
     * Changes won't be saved in file. */
    if(H5Pset_fapl_core(fapl, (size_t)CORE_INCREMENT, FALSE) < 0)
        TEST_ERROR;

    if((file=H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    /* Allocate memory for data set. */
    points=(int*)malloc(DSET1_DIM1*DSET1_DIM2*sizeof(int));
    check=(int*)malloc(DSET1_DIM1*DSET1_DIM2*sizeof(int));

    /* Initialize the dset1 */
    p1 = points;
    for(i = n = 0; i < DSET1_DIM1; i++)
	for(j = 0; j < DSET1_DIM2; j++)
	    *p1++ = n++;

    /* Create the data space1 */
    dims1[0] = DSET1_DIM1;
    dims1[1] = DSET1_DIM2;
    if((space1 = H5Screate_simple(2, dims1, NULL)) < 0)
        TEST_ERROR;

    /* Create the dset1 */
    if((dset1 = H5Dcreate2(file, DSET1_NAME, H5T_NATIVE_INT, space1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Write the data to the dset1 */
    if(H5Dwrite(dset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, points) < 0)
        TEST_ERROR;

    if(H5Dclose(dset1) < 0)
        TEST_ERROR;

    if((dset1 = H5Dopen2(file, DSET1_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Read the data back from dset1 */
    if(H5Dread(dset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, check) < 0)
        TEST_ERROR;

    /* Check that the values read are the same as the values written */
    p1 = points;
    p2 = check;
    for(i = 0; i < DSET1_DIM1; i++)
	for(j = 0; j < DSET1_DIM2; j++)
	    if(*p1++ != *p2++) {
		H5_FAILED();
		printf("    Read different values than written in data set 1.\n");
		printf("    At index %d,%d\n", i, j);
        	TEST_ERROR;
	    } /* end if */

    if(H5Dclose(dset1) < 0)
        TEST_ERROR;

    if(H5Fclose(file) < 0)
        TEST_ERROR;

    /* Open the file with backing store on for read and write.
     * Changes will be saved in file. */
    if(H5Pset_fapl_core(fapl, (size_t)CORE_INCREMENT, TRUE) < 0)
        TEST_ERROR;

    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    /* Create the dset1 */
    if((dset1 = H5Dcreate2(file, DSET1_NAME, H5T_NATIVE_INT, space1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Write the data to the dset1 */
    if(H5Dwrite(dset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, points) < 0)
        TEST_ERROR;

    if(H5Dclose(dset1) < 0)
        TEST_ERROR;

    if((dset1 = H5Dopen2(file, DSET1_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Reallocate memory for reading buffer. */
    if(check)
	free(check);

    check = (int*)malloc(DSET1_DIM1 * DSET1_DIM2 * sizeof(int));

    /* Read the data back from dset1 */
    if(H5Dread(dset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, check) < 0)
        TEST_ERROR;

    /* Check that the values read are the same as the values written */
    p1 = points;
    p2 = check;
    for(i = 0; i < DSET1_DIM1; i++)
	for(j = 0; j < DSET1_DIM2; j++)
	    if(*p1++ != *p2++) {
		H5_FAILED();
		printf("    Read different values than written in data set 1.\n");
		printf("    At index %d,%d\n", i, j);
        	TEST_ERROR;
	    } /* end if */

    /* Check file size API */
    if(H5Fget_filesize(file, &file_size) < 0)
        TEST_ERROR;

    /* There is no garantee the size of metadata in file is constant.
     * Just try to check if it's reasonable. */
    if(file_size<64*KB || file_size>256*KB)
        TEST_ERROR;

    if(H5Sclose(space1) < 0)
        TEST_ERROR;
    if(H5Dclose(dset1) < 0)
        TEST_ERROR;
    if(H5Fclose(file) < 0)
        TEST_ERROR;
    if(points)
	free(points);
    if(check)
	free(check);

    h5_cleanup(FILENAME, fapl);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose (fapl);
        H5Fclose(file);
    } H5E_END_TRY;
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    test_family_opens
 *
 * Purpose:     Private function for test_family() to tests wrong ways of
 *              reopening family file.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              Thursday, May 19, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_family_opens(char *fname, hid_t fa_pl)
{
    hid_t file;
    char first_name[1024];
    char wrong_name[1024];
    int i;

    /* Case 1: reopen file with 1st member file name and default property list */
    sprintf(first_name, fname, 0);

    H5E_BEGIN_TRY {
        file=H5Fopen(first_name, H5F_ACC_RDWR, H5P_DEFAULT);
    } H5E_END_TRY;
    if(file >= 0) TEST_ERROR

    /* Case 2: reopen file with correct name template but default property list */
    H5E_BEGIN_TRY {
        file=H5Fopen(fname, H5F_ACC_RDWR, H5P_DEFAULT);
    } H5E_END_TRY;
    if(file >= 0) TEST_ERROR

    /* Case 3: reopen file with wrong member size */
    if(H5Pset_fapl_family(fa_pl, (hsize_t)128, H5P_DEFAULT) < 0)
        TEST_ERROR;

    H5E_BEGIN_TRY {
        file=H5Fopen(fname, H5F_ACC_RDWR, fa_pl);
    } H5E_END_TRY;
    if(file >= 0) TEST_ERROR

    /* Case 4: reopen file with wrong name template */
    HDstrcpy(wrong_name, fname);
    for(i = 0; i < 1024; i++)
        if(wrong_name[i] == '5') {
            wrong_name[i] = '4';
            break;
        }

    if(H5Pset_fapl_family(fa_pl, (hsize_t)FAMILY_SIZE, H5P_DEFAULT) < 0)
        TEST_ERROR;

    H5E_BEGIN_TRY {
        file=H5Fopen(wrong_name, H5F_ACC_RDWR, fa_pl);
    } H5E_END_TRY;
    if(file >= 0) TEST_ERROR

    return 0;

error:
    return -1;
} /* end test_family_opens() */


/*-------------------------------------------------------------------------
 * Function:    test_family
 *
 * Purpose:     Tests the file handle interface for FAMILY driver
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              Tuesday, Sept 24, 2002
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_family(void)
{
    hid_t       file=(-1), fapl, fapl2=(-1), space=(-1), dset=(-1);
    hid_t       access_fapl = -1;
    char        filename[1024];
    char        dname[]="dataset";
    unsigned int i, j;
    int         *fhandle=NULL, *fhandle2=NULL;
    int         buf[FAMILY_NUMBER][FAMILY_SIZE];
    hsize_t     dims[2]={FAMILY_NUMBER, FAMILY_SIZE};
    hsize_t     file_size;

    TESTING("FAMILY file driver");

    /* Set property list and file name for FAMILY driver */
    fapl = h5_fileaccess();

    if(H5Pset_fapl_family(fapl, (hsize_t)FAMILY_SIZE, H5P_DEFAULT) < 0)
        TEST_ERROR;
    h5_fixname(FILENAME[2], fapl, filename, sizeof filename);

    if((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    if(H5Fclose(file) < 0)
        TEST_ERROR;

    /* Test different wrong ways to reopen family files where there's only
     * one member file existing. */
    if(test_family_opens(filename, fapl) < 0)
        TEST_ERROR;

    /* Reopen the file with default member file size */
    if(H5Pset_fapl_family(fapl, (hsize_t)H5F_FAMILY_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR;

    if((file=H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    /* Check file size API */
    if(H5Fget_filesize(file, &file_size) < 0)
        TEST_ERROR;

    /* The file size is supposed to be about 800 bytes right now. */
    if(file_size < (KB / 2) || file_size > KB)
        TEST_ERROR;

    /* Create and write dataset */
    if((space=H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Retrieve the access property list... */
    if ((access_fapl = H5Fget_access_plist(file)) < 0)
        TEST_ERROR;

    /* Check that the driver is correct */
    if(H5FD_FAMILY != H5Pget_driver(access_fapl))
        TEST_ERROR;

    /* ...and close the property list */
    if (H5Pclose(access_fapl) < 0)
        TEST_ERROR;

    if((dset=H5Dcreate2(file, dname, H5T_NATIVE_INT, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    for(i=0; i<FAMILY_NUMBER; i++)
        for(j=0; j<FAMILY_SIZE; j++)
            buf[i][j] = i*10000+j;

    if(H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        TEST_ERROR;

    /* check file handle API */
    if((fapl2=H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if(H5Pset_family_offset(fapl2, (hsize_t)0) < 0)
        TEST_ERROR;

    if(H5Fget_vfd_handle(file, fapl2, (void **)&fhandle) < 0)
        TEST_ERROR;
    if(*fhandle<0)
        TEST_ERROR;

    if(H5Pset_family_offset(fapl2, (hsize_t)(FAMILY_SIZE*2)) < 0)
        TEST_ERROR;
    if(H5Fget_vfd_handle(file, fapl2, (void **)&fhandle2) < 0)
        TEST_ERROR;
    if(*fhandle2<0)
        TEST_ERROR;

    /* Check file size API */
    if(H5Fget_filesize(file, &file_size) < 0)
        TEST_ERROR;

    /* Some data has been written.  The file size should be bigger(18KB+976
     * bytes if int size is 4 bytes) now. */
    if(sizeof(int)<=4) {
        if(file_size<18*KB || file_size>20*KB)
            TEST_ERROR;
    } else if(sizeof(int)>=8) {
        if(file_size<32*KB || file_size>40*KB)
            TEST_ERROR;
    }

    if(H5Sclose(space) < 0)
        TEST_ERROR;
    if(H5Dclose(dset) < 0)
        TEST_ERROR;
    if(H5Pclose(fapl2) < 0)
        TEST_ERROR;
    if(H5Fclose(file) < 0)
        TEST_ERROR;

    /* Test different wrong ways to reopen family files when there're multiple
     * member files existing. */
    if(test_family_opens(filename, fapl) < 0)
        TEST_ERROR;

    /* Reopen the file with correct member file size. */
    if(H5Pset_fapl_family(fapl, (hsize_t)FAMILY_SIZE, H5P_DEFAULT) < 0)
        TEST_ERROR;

    if((file=H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    if(H5Fclose(file) < 0)
        TEST_ERROR;

    h5_cleanup(FILENAME, fapl);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Sclose(space);
        H5Dclose(dset);
        H5Pclose (fapl2);
        H5Fclose(file);
    } H5E_END_TRY;
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    test_family_compat
 *
 * Purpose:     Tests the backward compatibility for FAMILY driver.
 *              See if we can open files created with v1.6 library.
 *              The source file was created by the test/file_handle.c
 *              of the v1.6 library.  Then tools/misc/h5repart.c was
 *              used to concantenated.  The command was "h5repart -m 5k
 *              family_file%05d.h5 family_v16_%05d.h5".
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              June 3, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_family_compat(void)
{
    hid_t       file = (-1), fapl;
    hid_t       dset;
    char        dname[]="dataset";
    char        filename[1024];
    char        pathname[1024], pathname_individual[1024];
    char        newname[1024], newname_individual[1024];
    int         counter = 0;

    TESTING("FAMILY file driver backward compatibility");

    /* Set property list and file name for FAMILY driver */
    fapl = h5_fileaccess();

    if(H5Pset_fapl_family(fapl, (hsize_t)FAMILY_SIZE2, H5P_DEFAULT) < 0)
        TEST_ERROR;

    h5_fixname(COMPAT_BASENAME, fapl, filename, sizeof filename);
    h5_fixname(FILENAME[3], fapl, newname, sizeof newname);

    pathname[0] = '\0';
    HDstrcat(pathname, filename);

    /* The following code makes the copies of the family files in the source directory.
     * Since we're going to open the files with write mode, this protects the original
     * files.
     */
    sprintf(newname_individual, newname, counter);
    sprintf(pathname_individual, pathname, counter);

    while (h5_make_local_copy(pathname_individual, newname_individual) >= 0) {
        counter++;
        sprintf(newname_individual, newname, counter);
        sprintf(pathname_individual, pathname, counter);
    }

    /* Make sure we can open the file.  Use the read and write mode to flush the
     * superblock. */
    if((file = H5Fopen(newname, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    if((dset = H5Dopen2(file, dname, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if(H5Dclose(dset) < 0)
        TEST_ERROR;

    if(H5Fclose(file) < 0)
        TEST_ERROR;

    /* Open the file again to make sure it isn't corrupted. */
    if((file = H5Fopen(newname, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    if((dset = H5Dopen2(file, dname, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if(H5Dclose(dset) < 0)
        TEST_ERROR;

    if(H5Fclose(file) < 0)
        TEST_ERROR;

    h5_cleanup(FILENAME, fapl);

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
        H5Pclose(fapl);
    } H5E_END_TRY;

    return -1;
} /* end test_family_compat() */


/*-------------------------------------------------------------------------
 * Function:    test_multi_opens
 *
 * Purpose:     Private function for test_multi() to tests wrong ways of
 *              reopening multi file.
 *
 * Return:      Success:        0
 *              Failure:        1
 *
 * Programmer:  Raymond Lu
 *              Thursday, May 19, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_multi_opens(char *fname)
{
    hid_t file;
    char  super_name[1024];     /*name string "%%s-s.h5"*/
    char  sf_name[1024];        /*name string "multi_file-s.h5"*/

    /* Case: reopen with the name of super file and default property list */
    sprintf(super_name, "%%s-%c.h5", 's');
    sprintf(sf_name, super_name, fname);

    H5E_BEGIN_TRY {
        file=H5Fopen(sf_name, H5F_ACC_RDWR, H5P_DEFAULT);
    } H5E_END_TRY;

    return 0;
}


/*-------------------------------------------------------------------------
 * Function:    test_multi
 *
 * Purpose:     Tests the file handle interface for MUTLI driver
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              Tuesday, Sept 24, 2002
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_multi(void)
{
    hid_t       file=(-1), fapl, fapl2=(-1), dset=(-1), space=(-1);
    hid_t       root, attr, aspace, atype;
    hid_t       access_fapl = -1;
    char        filename[1024];
    int         *fhandle2=NULL, *fhandle=NULL;
    hsize_t     file_size;
    H5FD_mem_t  mt, memb_map[H5FD_MEM_NTYPES];
    hid_t       memb_fapl[H5FD_MEM_NTYPES];
    haddr_t     memb_addr[H5FD_MEM_NTYPES];
    const char  *memb_name[H5FD_MEM_NTYPES];
    char        sv[H5FD_MEM_NTYPES][32];
    hsize_t     dims[2]={MULTI_SIZE, MULTI_SIZE};
    hsize_t     adims[1]={1};
    char        dname[]="dataset";
    char        meta[] = "this is some metadata on this file";
    int         i, j;
    int         buf[MULTI_SIZE][MULTI_SIZE];

    TESTING("MULTI file driver");
    /* Set file access property list for MULTI driver */
    fapl = h5_fileaccess();

    HDmemset(memb_map, 0,  sizeof memb_map);
    HDmemset(memb_fapl, 0, sizeof memb_fapl);
    HDmemset(memb_name, 0, sizeof memb_name);
    HDmemset(memb_addr, 0, sizeof memb_addr);
    HDmemset(sv, 0, sizeof sv);

    for(mt=H5FD_MEM_DEFAULT; mt<H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t,mt)) {
        memb_fapl[mt] = H5P_DEFAULT;
        memb_map[mt] = H5FD_MEM_SUPER;
    }
    memb_map[H5FD_MEM_DRAW] = H5FD_MEM_DRAW;
    memb_map[H5FD_MEM_BTREE] = H5FD_MEM_BTREE;
    memb_map[H5FD_MEM_GHEAP] = H5FD_MEM_GHEAP;

    sprintf(sv[H5FD_MEM_SUPER], "%%s-%c.h5", 's');
    memb_name[H5FD_MEM_SUPER] = sv[H5FD_MEM_SUPER];
    memb_addr[H5FD_MEM_SUPER] = 0;

    sprintf(sv[H5FD_MEM_BTREE],  "%%s-%c.h5", 'b');
    memb_name[H5FD_MEM_BTREE] = sv[H5FD_MEM_BTREE];
    memb_addr[H5FD_MEM_BTREE] = HADDR_MAX/4;

    sprintf(sv[H5FD_MEM_DRAW], "%%s-%c.h5", 'r');
    memb_name[H5FD_MEM_DRAW] = sv[H5FD_MEM_DRAW];
    memb_addr[H5FD_MEM_DRAW] = HADDR_MAX/2;

    sprintf(sv[H5FD_MEM_GHEAP], "%%s-%c.h5", 'g');
    memb_name[H5FD_MEM_GHEAP] = sv[H5FD_MEM_GHEAP];
    memb_addr[H5FD_MEM_GHEAP] = (HADDR_MAX/4)*3;


    if(H5Pset_fapl_multi(fapl, memb_map, memb_fapl, memb_name, memb_addr, TRUE) < 0)
        TEST_ERROR;
    h5_fixname(FILENAME[4], fapl, filename, sizeof filename);

    if((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    if(H5Fclose(file) < 0)
        TEST_ERROR;


    /* Test wrong ways to reopen multi files */
    if(test_multi_opens(filename) < 0)
        TEST_ERROR;

    /* Reopen the file */
    if((file=H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    /* Create and write data set */
    if((space=H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Retrieve the access property list... */
    if ((access_fapl = H5Fget_access_plist(file)) < 0)
        TEST_ERROR;

    /* Check that the driver is correct */
    if(H5FD_MULTI != H5Pget_driver(access_fapl))
        TEST_ERROR;

    /* ...and close the property list */
    if (H5Pclose(access_fapl) < 0)
        TEST_ERROR;

    /* Check file size API */
    if(H5Fget_filesize(file, &file_size) < 0)
        TEST_ERROR;

    /* Before any data is written, the raw data file is empty.  So
     * the file size is only the size of b-tree + HADDR_MAX/4.
     */
    if(file_size < HADDR_MAX/4 || file_size > HADDR_MAX/2)
        TEST_ERROR;

    if((dset=H5Dcreate2(file, dname, H5T_NATIVE_INT, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    for(i=0; i<MULTI_SIZE; i++)
        for(j=0; j<MULTI_SIZE; j++)
            buf[i][j] = i*10000+j;
    if(H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        TEST_ERROR;

    if((fapl2=H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if(H5Pset_multi_type(fapl2, H5FD_MEM_SUPER) < 0)
        TEST_ERROR;
    if(H5Fget_vfd_handle(file, fapl2, (void **)&fhandle) < 0)
        TEST_ERROR;
    if(*fhandle<0)
        TEST_ERROR;

    if(H5Pset_multi_type(fapl2, H5FD_MEM_DRAW) < 0)
        TEST_ERROR;
    if(H5Fget_vfd_handle(file, fapl2, (void **)&fhandle2) < 0)
        TEST_ERROR;
    if(*fhandle2<0)
        TEST_ERROR;

    /* Check file size API */
    if(H5Fget_filesize(file, &file_size) < 0)
        TEST_ERROR;

    /* After the data is written, the file size is huge because the
     * beginning of raw data file is set at HADDR_MAX/2.  It's supposed
     * to be (HADDR_MAX/2 + 128*128*4)
     */
    if(file_size < HADDR_MAX/2 || file_size > HADDR_MAX)
        TEST_ERROR;

    if(H5Sclose(space) < 0)
        TEST_ERROR;
    if(H5Dclose(dset) < 0)
        TEST_ERROR;
    if(H5Pclose(fapl2) < 0)
        TEST_ERROR;

    /* Create and write attribute for the root group. */
    if((root = H5Gopen2(file, "/", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Attribute string. */
    if((atype = H5Tcopy(H5T_C_S1)) < 0)
        TEST_ERROR;

    if(H5Tset_size(atype, strlen(meta) + 1) < 0)
        TEST_ERROR;

    if(H5Tset_strpad(atype, H5T_STR_NULLTERM) < 0)
        TEST_ERROR;

    /* Create and write attribute */
    if((aspace = H5Screate_simple(1, adims, NULL)) < 0)
        TEST_ERROR;

    if((attr = H5Acreate2(root, "Metadata", atype, aspace, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if(H5Awrite(attr, atype, meta) < 0)
        TEST_ERROR;

    /* Close IDs */
    if(H5Tclose(atype) < 0)
        TEST_ERROR;
    if(H5Sclose(aspace) < 0)
        TEST_ERROR;
    if(H5Aclose(attr) < 0)
        TEST_ERROR;

    if(H5Fclose(file) < 0)
        TEST_ERROR;

    h5_cleanup(FILENAME, fapl);
    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Sclose(space);
        H5Dclose(dset);
        H5Pclose(fapl);
        H5Pclose(fapl2);
        H5Fclose(file);
    } H5E_END_TRY;
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    test_log
 *
 * Purpose:     Tests the file handle interface for log driver
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Dana Robinson
 *              Tuesday, March 22, 2011
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_log(void)
{
    hid_t        file            = -1;
    hid_t        fapl            = -1;
    hid_t        access_fapl     = -1;
    char         filename[1024];
    int          *fhandle        = NULL;
    hsize_t      file_size       = 0;
    unsigned int flags           = H5FD_LOG_ALL;
    size_t       buf_size        = 0;

    TESTING("LOG file driver");

    /* Set property list and file name for log driver. */
    fapl = h5_fileaccess();
    if(H5Pset_fapl_log(fapl, LOG_FILENAME, 0, buf_size) < 0)
        TEST_ERROR;
    h5_fixname(FILENAME[6], fapl, filename, sizeof filename);

    /* Create the test file */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    /* Retrieve the access property list... */
    if((access_fapl = H5Fget_access_plist(file)) < 0)
        TEST_ERROR;

    /* Check that the driver is correct */
    if(H5FD_LOG != H5Pget_driver(access_fapl))
        TEST_ERROR;

    /* ...and close the property list */
    if(H5Pclose(access_fapl) < 0)
        TEST_ERROR;

    /* Check file handle API */
    if(H5Fget_vfd_handle(file, H5P_DEFAULT, (void **)&fhandle) < 0)
        TEST_ERROR;
    if(*fhandle < 0)
        TEST_ERROR;

    /* Check file size API */
    if(H5Fget_filesize(file, &file_size) < 0)
        TEST_ERROR;

    /* There is no guarantee the size of metadata in file is constant.
     * Just try to check if it's reasonable.  It's 2KB right now.
     */
    if(file_size < 1 * KB || file_size > 4 * KB)
        TEST_ERROR;

    if(H5Fclose(file) < 0)
        TEST_ERROR;

    h5_cleanup(FILENAME, fapl);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl);
        H5Fclose(file);
    } H5E_END_TRY;
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    test_stdio
 *
 * Purpose:     Tests the file handle interface for STDIO driver
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Dana Robinson
 *              Tuesday, March 22, 2011
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_stdio(void)
{
    hid_t        file            = -1;
    hid_t        fapl            = -1;
    hid_t        access_fapl     = -1;
    char         filename[1024];
    FILE         *fhandle        = NULL;
    hsize_t      file_size       = 0;

    TESTING("STDIO file driver");

    /* Set property list and file name for STDIO driver. */
    fapl = h5_fileaccess();
    if(H5Pset_fapl_stdio(fapl) < 0)
        TEST_ERROR;
    h5_fixname(FILENAME[7], fapl, filename, sizeof filename);

    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    /* Retrieve the access property list... */
    if((access_fapl = H5Fget_access_plist(file)) < 0)
        TEST_ERROR;

    /* Check that the driver is correct */
    if(H5FD_STDIO != H5Pget_driver(access_fapl))
        TEST_ERROR;

    /* ...and close the property list */
    if(H5Pclose(access_fapl) < 0)
        TEST_ERROR;

    /* Check file handle API */
    if(H5Fget_vfd_handle(file, H5P_DEFAULT, (void **)&fhandle) < 0)
        TEST_ERROR;
    if(NULL == fhandle)
        TEST_ERROR;

    /* Check file size API */
    if(H5Fget_filesize(file, &file_size) < 0)
        TEST_ERROR;

    /* There is no guarantee the size of metadata in file is constant.
     * Just try to check if it's reasonable.  It's 2KB right now.
     */
    if(file_size < 1 * KB || file_size > 4 * KB)
        TEST_ERROR;

    if(H5Fclose(file) < 0)
        TEST_ERROR;

    h5_cleanup(FILENAME, fapl);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl);
        H5Fclose(file);
    } H5E_END_TRY;
    return -1;
}



/*-------------------------------------------------------------------------
 * Function:    test_windows
 *
 * Purpose:     Tests the file handle interface for WINDOWS driver
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Dana Robinson
 *              Tuesday, March 22, 2011
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_windows(void)
{
#ifdef _WIN32

    hid_t        file            = -1;
    hid_t        fapl            = -1;
    hid_t        access_fapl     = -1;
    char         filename[1024];
    int          *fhandle        = NULL;
    hsize_t      file_size       = 0;

#endif /*_WIN32*/

    TESTING("WINDOWS file driver");

#ifndef _WIN32

    SKIPPED();
    return 0;

#else /*_WIN32*/

    /* Set property list and file name for WINDOWS driver. */
    fapl = h5_fileaccess();
    if(H5Pset_fapl_windows(fapl) < 0)
        TEST_ERROR;
    h5_fixname(FILENAME[8], fapl, filename, sizeof filename);

    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    /* Retrieve the access property list... */
    if((access_fapl = H5Fget_access_plist(file)) < 0)
        TEST_ERROR;

    /* Check that the driver is correct */
    if(H5FD_WINDOWS!= H5Pget_driver(access_fapl))
        TEST_ERROR;

    /* ...and close the property list */
    if(H5Pclose(access_fapl) < 0)
        TEST_ERROR;

    /* Check file handle API */
    if(H5Fget_vfd_handle(file, H5P_DEFAULT, (void **)&fhandle) < 0)
        TEST_ERROR;
    if(*fhandle < 0)
        TEST_ERROR;

    /* Check file size API */
    if(H5Fget_filesize(file, &file_size) < 0)
        TEST_ERROR;

    /* There is no guarantee the size of metadata in file is constant.
     * Just try to check if it's reasonable.  It's 2KB right now.
     */
    if(file_size < 1 * KB || file_size > 4 * KB)
        TEST_ERROR;

    if(H5Fclose(file) < 0)
        TEST_ERROR;

    h5_cleanup(FILENAME, fapl);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl);
        H5Fclose(file);
    } H5E_END_TRY;
    return -1;

#endif /*_WIN32*/
}



/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests the basic features of Virtual File Drivers
 *
 * Return:      Success:        0
 *              Failure:        1
 *
 * Programmer:  Raymond Lu
 *              Tuesday, Sept 24, 2002
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    int nerrors = 0;

    h5_reset();

    printf("Testing basic Virtual File Driver functionality.\n");

    nerrors += test_sec2() < 0           ? 1 : 0;
    nerrors += test_core() < 0           ? 1 : 0;
    nerrors += test_family() < 0         ? 1 : 0;
    nerrors += test_family_compat() < 0  ? 1 : 0;
    nerrors += test_multi() < 0          ? 1 : 0;
    nerrors += test_direct() < 0         ? 1 : 0;
    nerrors += test_log() < 0            ? 1 : 0;
    nerrors += test_stdio() < 0          ? 1 : 0;
    nerrors += test_windows() < 0        ? 1 : 0;

    if(nerrors) {
	printf("***** %d Virtual File Driver TEST%s FAILED! *****\n",
		nerrors, nerrors > 1 ? "S" : "");
	return 1;
    }

    printf("All Virtual File Driver tests passed.\n");
    return 0;
}

