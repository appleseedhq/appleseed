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
 * Programmer:  Raymond Lu <slu@ncsa.uiuc.edu>
 *              June 1, 2005
 *
 * Purpose:	This program tests family files after being repartitioned
 *              by h5repart.  It simply tries to reopen the files with
 *              correct family driver and member size.
 */
#include "hdf5.h"

#define KB                      1024
#define FAMILY_H5REPART_SIZE1   20000
#define FAMILY_H5REPART_SIZE2   (5*KB)
#define MAX(a,b) (a>b ? a:b)

const char *FILENAME[] = {
    "fst_family%05d.h5",
    "scd_family%05d.h5",
    "family_to_sec2.h5",
    NULL
};

herr_t test_family_h5repart_opens(void);
herr_t test_sec2_h5repart_opens(void);


/*-------------------------------------------------------------------------
 * Function:    test_family_h5repart_opens
 *
 * Purpose:     Tries to reopen family files.
 *
 * Return:      Success:        exit(0)
 *
 *              Failure:        exit(1)
 *
 * Programmer:  Raymond Lu
 *              June 1, 2005
 *
 * Modifications:
 *-------------------------------------------------------------------------
 */
herr_t
test_family_h5repart_opens(void)
{
    hid_t       file=(-1), fapl=(-1);

    /* open 1st file(single member file) with correct family size(20000 byte) */
    if ((fapl=H5Pcreate(H5P_FILE_ACCESS))<0)
        goto error;

    if(H5Pset_fapl_family(fapl, (hsize_t)FAMILY_H5REPART_SIZE1, H5P_DEFAULT)<0)
        goto error;

    if((file=H5Fopen(FILENAME[0], H5F_ACC_RDWR, fapl))<0)
        goto error;

    if(H5Fclose(file)<0)
        goto error;

    /* open 2nd file(multiple member files) with correct family size(5KB) */
    if(H5Pset_fapl_family(fapl, (hsize_t)FAMILY_H5REPART_SIZE2, H5P_DEFAULT)<0)
        goto error;

    if((file=H5Fopen(FILENAME[1], H5F_ACC_RDWR, fapl))<0)
        goto error;

    if(H5Fclose(file)<0)
        goto error;

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
    } H5E_END_TRY;
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    test_sec2_h5repart_opens
 *
 * Purpose:     Tries to reopen a sec2 file.
 *
 * Return:      Success:        exit(0)
 *
 *              Failure:        exit(1)
 *
 * Programmer:  Raymond Lu
 *              June 21, 2005
 *
 * Modifications:
 *-------------------------------------------------------------------------
 */
herr_t
test_sec2_h5repart_opens(void)
{
    hid_t       file=(-1);

    /* open the sec2 file */
    if((file=H5Fopen(FILENAME[2], H5F_ACC_RDWR, H5P_DEFAULT))<0)
        goto error;

    if(H5Fclose(file)<0)
        goto error;

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
    } H5E_END_TRY;
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests h5repart-ed family files
 *
 * Return:      Success:        exit(0)
 *
 *              Failure:        exit(1)
 *
 * Programmer:  Raymond Lu
 *              June 1, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    int                 nerrors=0;

    nerrors += test_family_h5repart_opens()<0   ?1:0;
    nerrors += test_sec2_h5repart_opens()<0     ?1:0;

    if (nerrors) goto error;

    return 0;

error:
    nerrors = MAX(1, nerrors);
    printf("***** %d FAMILY FILE TEST%s FAILED! *****\n",
            nerrors, 1 == nerrors ? "" : "S");
    return 1;
}
