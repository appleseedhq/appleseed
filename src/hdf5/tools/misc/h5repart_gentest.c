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
 *              June 1, 2005
 *
 * Purpose:     Generate a family file of 1024 bytes for each member
 *              for h5repart test.
 */
#include "hdf5.h"
#include "H5private.h"

#define KB              1024
#define FAMILY_NUMBER   4
#define FAMILY_SIZE     1024
#define FILENAME        "family_file%05d.h5"

int main(void)
{
    hid_t       file=(-1), fapl, space=(-1), dset=(-1);
    char        dname[]="dataset";
    int         i, j;
    int         buf[FAMILY_NUMBER][FAMILY_SIZE];
    hsize_t     dims[2]={FAMILY_NUMBER, FAMILY_SIZE};

    /* Set property list and file name for FAMILY driver */
    if ((fapl=H5Pcreate(H5P_FILE_ACCESS)) < 0) {
        perror ("H5Pcreate");
        exit (EXIT_FAILURE);
    }

    if(H5Pset_fapl_family(fapl, (hsize_t)FAMILY_SIZE, H5P_DEFAULT) < 0) {
        perror ("H5Pset_fapl_family");
        exit (EXIT_FAILURE);
    }

    if((file = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) {
        perror("H5Fcreate");
        exit(EXIT_FAILURE);
    }

    /* Create and write dataset */
    if((space = H5Screate_simple(2, dims, NULL)) < 0) {
        perror("H5Screate_simple");
        exit(EXIT_FAILURE);
    }


    if((dset = H5Dcreate2(file, dname, H5T_NATIVE_INT, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        perror("H5Dcreate2");
        exit(EXIT_FAILURE);
    }


    for(i = 0; i<FAMILY_NUMBER; i++)
        for(j = 0; j<FAMILY_SIZE; j++)
            buf[i][j] = i * 10000 + j;

    if(H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) {
        perror("H5Dwrite");
        exit(EXIT_FAILURE);
    }


    if(H5Sclose(space) < 0) {
        perror ("H5Sclose");
        exit (EXIT_FAILURE);
    }

    if(H5Dclose(dset) < 0) {
        perror ("H5Dclose");
        exit (EXIT_FAILURE);
    }

    if(H5Pclose(fapl) < 0) {
        perror ("H5Pclose");
        exit (EXIT_FAILURE);
    }

    if(H5Fclose(file) < 0) {
        perror ("H5Fclose");
        exit (EXIT_FAILURE);
    }

    puts(" PASSED"); fflush(stdout);

    return 0;
}
