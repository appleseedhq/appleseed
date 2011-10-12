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
 *              Thursday, March 23, 2006
 *
 * Purpose:     Check if floating-point data created on OpenVMS, big-endian, and
 *              little-endian machines can be read on the machine running this test.
 */

#include "h5test.h"
#include "H5srcdir.h"

const char *FILENAME[] = {
    "vms_data",
    "le_data",
    "be_data",
    NULL
};

#define DATASETNAME        "Array"
#define DATASETNAME2       "Scale_offset_float_data_le"
#define DATASETNAME3       "Scale_offset_float_data_be"
#define DATASETNAME4       "Scale_offset_double_data_le"
#define DATASETNAME5       "Scale_offset_double_data_be"
#define DATASETNAME6       "Scale_offset_char_data_le"
#define DATASETNAME7       "Scale_offset_char_data_be"
#define DATASETNAME8       "Scale_offset_short_data_le"
#define DATASETNAME9       "Scale_offset_short_data_be"
#define DATASETNAME10      "Scale_offset_int_data_le"
#define DATASETNAME11      "Scale_offset_int_data_be"
#define DATASETNAME12      "Scale_offset_long_long_data_le"
#define DATASETNAME13      "Scale_offset_long_long_data_be"
#define NX 		6
#define NY 		6


/*-------------------------------------------------------------------------
 * Function:    read_data
 *
 * Purpose:     Read data from a data file.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              21 January 2011
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int read_data(char *fname)
{
    char        pathname[1024];
    char       *srcdir = getenv("srcdir"); /*where the src code is located*/
    hid_t       file, dataset;         /* handles */
    double      data_in[NX+1][NY]; /* input buffer */
    double      data_out[NX+1][NY]; /* output buffer */
    long long   int_data_in[NX+1][NY]; /* input buffer */
    long long   int_data_out[NX+1][NY]; /* output buffer */
    int         i, j;
    unsigned 	nerrors = 0;
    const char  *not_supported= "    Scaleoffset filter is not enabled.";

    pathname[0] = '\0';
    /* Generate correct name for test file by prepending the source path */
    if(srcdir && ((strlen(srcdir) + strlen(fname) + 1) < sizeof(pathname))) {
        strcpy(pathname, srcdir);
        strcat(pathname, "/");
    }
    strcat(pathname, fname);

    /*
     * Open the file.
     */
    if((file = H5Fopen(pathname, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    TESTING("regular dataset");

    /* 
     * Open the regular dataset.
     */
    if((dataset = H5Dopen2(file, DATASETNAME, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /*
     * Data and output buffer initialization.
     */
    for (j = 0; j < NX; j++) {
	for (i = 0; i < NY; i++) {
	    data_in[j][i] = i + j;
	    data_out[j][i] = 0;
        }
    }
    for (i = 0; i < NY; i++) {
        data_in[NX][i] = -2.2;
        data_out[NX][i] = 0;
    }
    /*
     * 0 1 2 3 4 5
     * 1 2 3 4 5 6
     * 2 3 4 5 6 7
     * 3 4 5 6 7 8
     * 4 5 6 7 8 9
     * 5 6 7 8 9 10
     * -2.2 -2.2 -2.2 -2.2 -2.2 -2.2
     */

    /*
     * Read data from hyperslab in the file into the hyperslab in
     * memory and display.
     */
    if(H5Dread(dataset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
            data_out) < 0)
        TEST_ERROR;

    /* Check results */
    for (j=0; j<(NX+1); j++) {
        for (i=0; i<NY; i++) {
            /* if (data_out[j][i] != data_in[j][i]) { */
            if (!FLT_ABS_EQUAL(data_out[j][i], data_in[j][i])) {
                if (!nerrors++) {
                    H5_FAILED();
                    printf("element [%d][%d] is %g but should have been %g\n",
                           j, i, data_out[j][i], data_in[j][i]);
                }
            }
        }
    }

    /*
     * Close/release resources.
     */
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /* Failure */
    if (nerrors) {
        printf("total of %d errors out of %d elements\n", nerrors, NX*NY);
        return 1;
    }

    PASSED();

    TESTING("dataset of LE FLOAT with scale-offset filter");

#ifdef H5_HAVE_FILTER_SCALEOFFSET
    /*
     * Open the dataset with scale-offset filter.
     */
    if((dataset = H5Dopen2(file, DATASETNAME2, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /*
     * Data and output buffer initialization.
     */
    for (j = 0; j < NX; j++) {
        for (i = 0; i < NY; i++) {
            data_in[j][i] = ((double)(i + j + 1))/3;
            data_out[j][i] = 0;
        }
    }
    for (i = 0; i < NY; i++) {
        data_in[NX][i] = -2.2;
        data_out[NX][i] = 0;
    }

    /*
     * Read data from hyperslab in the file into the hyperslab in
     * memory and display.
     */
    if(H5Dread(dataset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
            data_out) < 0)
        TEST_ERROR;

    /* Check results */
    for (j=0; j<(NX+1); j++) {
        for (i=0; i<NY; i++) {
            if (!DBL_REL_EQUAL(data_out[j][i], data_in[j][i], 0.001)) {
                if (!nerrors++) {
                    H5_FAILED();
                    printf("element [%d][%d] is %g but should have been %g\n",
                           j, i, data_out[j][i], data_in[j][i]);
                }
            }
        }
    }

    /*
     * Close/release resources.
     */
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /* Failure */
    if (nerrors) {
        printf("total of %d errors out of %d elements\n", nerrors, NX*NY);
        return 1;
    }

    PASSED();
#else /*H5_HAVE_FILTER_SCALEOFFSET*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_SCALEOFFSET*/

    TESTING("dataset of BE FLOAT with scale-offset filter");

#ifdef H5_HAVE_FILTER_SCALEOFFSET
    /*
     * Open the dataset with scale-offset filter.
     */
    if((dataset = H5Dopen2(file, DATASETNAME3, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /*
     * Data and output buffer initialization.
     */
    for (j = 0; j < NX; j++) {
        for (i = 0; i < NY; i++) {
            data_in[j][i] = ((double)(i + j + 1))/3;
            data_out[j][i] = 0;
        }
    }
    for (i = 0; i < NY; i++) {
        data_in[NX][i] = -2.2;
        data_out[NX][i] = 0;
    }

    /*
     * Read data from hyperslab in the file into the hyperslab in
     * memory and display.
     */
    if(H5Dread(dataset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
            data_out) < 0)
        TEST_ERROR;

    /* Check results */
    for (j=0; j<(NX+1); j++) {
        for (i=0; i<NY; i++) {
            if (!DBL_REL_EQUAL(data_out[j][i], data_in[j][i], 0.001)) {
                if (!nerrors++) {
                    H5_FAILED();
                    printf("element [%d][%d] is %g but should have been %g\n",
                           j, i, data_out[j][i], data_in[j][i]);
                }
            }
        }
    }

    /*
     * Close/release resources.
     */
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /* Failure */
    if (nerrors) {
        printf("total of %d errors out of %d elements\n", nerrors, NX*NY);
        return 1;
    }

    PASSED();
#else /*H5_HAVE_FILTER_SCALEOFFSET*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_SCALEOFFSET*/

    TESTING("dataset of LE DOUBLE with scale-offset filter");

#ifdef H5_HAVE_FILTER_SCALEOFFSET
    /* 
     * Open the dataset with scale-offset filter.
     */
    if((dataset = H5Dopen2(file, DATASETNAME4, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /*
     * Data and output buffer initialization.
     */
    for (j = 0; j < NX; j++) {
	for (i = 0; i < NY; i++) {
	    data_in[j][i] = ((double)(i + j + 1))/3;
	    data_out[j][i] = 0;
        }
    }
    for (i = 0; i < NY; i++) {
        data_in[NX][i] = -2.2;
        data_out[NX][i] = 0;
    }

    /*
     * Read data from hyperslab in the file into the hyperslab in
     * memory and display.
     */
    if(H5Dread(dataset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
            data_out) < 0)
        TEST_ERROR;

    /* Check results */
    for (j=0; j<(NX+1); j++) {
        for (i=0; i<NY; i++) {
            if (!DBL_REL_EQUAL(data_out[j][i], data_in[j][i], 0.001)) {
                if (!nerrors++) {
                    H5_FAILED();
                    printf("element [%d][%d] is %g but should have been %g\n",
                           j, i, data_out[j][i], data_in[j][i]);
                }
            }
        }
    }

    /*
     * Close/release resources.
     */
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /* Failure */
    if (nerrors) {
        printf("total of %d errors out of %d elements\n", nerrors, NX*NY);
        return 1;
    }

    PASSED();
#else /*H5_HAVE_FILTER_SCALEOFFSET*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_SCALEOFFSET*/

    TESTING("dataset of BE DOUBLE with scale-offset filter");

#ifdef H5_HAVE_FILTER_SCALEOFFSET
    /*
     * Open the dataset with scale-offset filter.
     */
    if((dataset = H5Dopen2(file, DATASETNAME5, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /*
     * Data and output buffer initialization.
     */
    for (j = 0; j < NX; j++) {
        for (i = 0; i < NY; i++) {
            data_in[j][i] = ((double)(i + j + 1))/3;
            data_out[j][i] = 0;
        }
    }
    for (i = 0; i < NY; i++) {
        data_in[NX][i] = -2.2;
        data_out[NX][i] = 0;
    }

    /*
     * Read data from hyperslab in the file into the hyperslab in
     * memory and display.
     */
    if(H5Dread(dataset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
            data_out) < 0)
        TEST_ERROR;

    /* Check results */
    for (j=0; j<(NX+1); j++) {
        for (i=0; i<NY; i++) {
            if (!DBL_REL_EQUAL(data_out[j][i], data_in[j][i], 0.001)) {
                if (!nerrors++) {
                    H5_FAILED();
                    printf("element [%d][%d] is %g but should have been %g\n",
                           j, i, data_out[j][i], data_in[j][i]);
                }
            }
        }
    }

    /*
     * Close/release resources.
     */
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /* Failure */
    if (nerrors) {
        printf("total of %d errors out of %d elements\n", nerrors, NX*NY);
        return 1;
    }

    PASSED();
#else /*H5_HAVE_FILTER_SCALEOFFSET*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_SCALEOFFSET*/

    TESTING("dataset of LE CHAR with scale-offset filter");

#ifdef H5_HAVE_FILTER_SCALEOFFSET
    /*
     * Open the dataset with scale-offset filter.
     */
    if((dataset = H5Dopen2(file, DATASETNAME6, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /*
     * Data and output buffer initialization.
     */
    for (j = 0; j < NX; j++) {
        for (i = 0; i < NY; i++) {
            int_data_in[j][i] = i + j;
            int_data_out[j][i] = 0;
        }
    }
    for (i = 0; i < NY; i++) {
        int_data_in[NX][i] = -2;
        int_data_out[NX][i] = 0;
    }

    /*
     * Read data from hyperslab in the file into the hyperslab in
     * memory and display.
     */
    if(H5Dread(dataset, H5T_NATIVE_LLONG, H5S_ALL, H5S_ALL, H5P_DEFAULT,
            int_data_out) < 0)
        TEST_ERROR;

    /* Check results */
    for (j=0; j<(NX+1); j++) {
        for (i=0; i<NY; i++) {
            if (int_data_out[j][i] != int_data_in[j][i]) {
                if (!nerrors++) {
                    H5_FAILED();
                    printf("element [%d][%d] is %d but should have been %d\n",
                           j, i, (int)int_data_out[j][i],
                           (int)int_data_in[j][i]);
                }
            }
        }
    }

    /*
     * Close/release resources.
     */
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /* Failure */
    if (nerrors) {
        printf("total of %d errors out of %d elements\n", nerrors, NX*NY);
        return 1;
    }

    PASSED();

#else /*H5_HAVE_FILTER_SCALEOFFSET*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_SCALEOFFSET*/

    TESTING("dataset of BE CHAR with scale-offset filter");

#ifdef H5_HAVE_FILTER_SCALEOFFSET
    /*
     * Open the dataset with scale-offset filter.
     */
    if((dataset = H5Dopen2(file, DATASETNAME7, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /*
     * Data and output buffer initialization.
     */
    for (j = 0; j < NX; j++) {
        for (i = 0; i < NY; i++) {
            int_data_in[j][i] = i + j;
            int_data_out[j][i] = 0;
        }
    }
    for (i = 0; i < NY; i++) {
        int_data_in[NX][i] = -2;
        int_data_out[NX][i] = 0;
    }

    /*
     * Read data from hyperslab in the file into the hyperslab in
     * memory and display.
     */
    if(H5Dread(dataset, H5T_NATIVE_LLONG, H5S_ALL, H5S_ALL, H5P_DEFAULT,
            int_data_out) < 0)
        TEST_ERROR;

    /* Check results */
    for (j=0; j<(NX+1); j++) {
        for (i=0; i<NY; i++) {
            if (int_data_out[j][i] != int_data_in[j][i]) {
                if (!nerrors++) {
                    H5_FAILED();
                    printf("element [%d][%d] is %d but should have been %d\n",
                           j, i, (int)int_data_out[j][i],
                           (int)int_data_in[j][i]);
                }
            }
        }
    }

    /*
     * Close/release resources.
     */
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /* Failure */
    if (nerrors) {
        printf("total of %d errors out of %d elements\n", nerrors, NX*NY);
        return 1;
    }

    PASSED();

#else /*H5_HAVE_FILTER_SCALEOFFSET*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_SCALEOFFSET*/

    TESTING("dataset of LE SHORT with scale-offset filter");

#ifdef H5_HAVE_FILTER_SCALEOFFSET
    /*
     * Open the dataset with scale-offset filter.
     */
    if((dataset = H5Dopen2(file, DATASETNAME8, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /*
     * Data and output buffer initialization.
     */
    for (j = 0; j < NX; j++) {
        for (i = 0; i < NY; i++) {
            int_data_in[j][i] = i + j;
            int_data_out[j][i] = 0;
        }
    }
    for (i = 0; i < NY; i++) {
        int_data_in[NX][i] = -2;
        int_data_out[NX][i] = 0;
    }

    /*
     * Read data from hyperslab in the file into the hyperslab in
     * memory and display.
     */
    if(H5Dread(dataset, H5T_NATIVE_LLONG, H5S_ALL, H5S_ALL, H5P_DEFAULT,
            int_data_out) < 0)
        TEST_ERROR;

    /* Check results */
    for (j=0; j<(NX+1); j++) {
        for (i=0; i<NY; i++) {
            if (int_data_out[j][i] != int_data_in[j][i]) {
                if (!nerrors++) {
                    H5_FAILED();
                    printf("element [%d][%d] is %d but should have been %d\n",
                           j, i, (int)int_data_out[j][i],
                           (int)int_data_in[j][i]);
                }
            }
        }
    }

    /*
     * Close/release resources.
     */
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /* Failure */
    if (nerrors) {
        printf("total of %d errors out of %d elements\n", nerrors, NX*NY);
        return 1;
    }

    PASSED();

#else /*H5_HAVE_FILTER_SCALEOFFSET*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_SCALEOFFSET*/

    TESTING("dataset of BE SHORT with scale-offset filter");

#ifdef H5_HAVE_FILTER_SCALEOFFSET
    /*
     * Open the dataset with scale-offset filter.
     */
    if((dataset = H5Dopen2(file, DATASETNAME9, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /*
     * Data and output buffer initialization.
     */
    for (j = 0; j < NX; j++) {
        for (i = 0; i < NY; i++) {
            int_data_in[j][i] = i + j;
            int_data_out[j][i] = 0;
        }
    }
    for (i = 0; i < NY; i++) {
        int_data_in[NX][i] = -2;
        int_data_out[NX][i] = 0;
    }

    /*
     * Read data from hyperslab in the file into the hyperslab in
     * memory and display.
     */
    if(H5Dread(dataset, H5T_NATIVE_LLONG, H5S_ALL, H5S_ALL, H5P_DEFAULT,
            int_data_out) < 0)
        TEST_ERROR;

    /* Check results */
    for (j=0; j<(NX+1); j++) {
        for (i=0; i<NY; i++) {
            if (int_data_out[j][i] != int_data_in[j][i]) {
                if (!nerrors++) {
                    H5_FAILED();
                    printf("element [%d][%d] is %d but should have been %d\n",
                           j, i, (int)int_data_out[j][i],
                           (int)int_data_in[j][i]);
                }
            }
        }
    }

    /*
     * Close/release resources.
     */
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /* Failure */
    if (nerrors) {
        printf("total of %d errors out of %d elements\n", nerrors, NX*NY);
        return 1;
    }

    PASSED();

#else /*H5_HAVE_FILTER_SCALEOFFSET*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_SCALEOFFSET*/

    TESTING("dataset of LE INT with scale-offset filter");

#ifdef H5_HAVE_FILTER_SCALEOFFSET
    /* 
     * Open the dataset with scale-offset filter.
     */
    if((dataset = H5Dopen2(file, DATASETNAME10, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /*
     * Data and output buffer initialization.
     */
    for (j = 0; j < NX; j++) {
	for (i = 0; i < NY; i++) {
	    int_data_in[j][i] = i + j;
	    int_data_out[j][i] = 0;
        }
    }
    for (i = 0; i < NY; i++) {
        int_data_in[NX][i] = -2;
        int_data_out[NX][i] = 0;
    }

    /*
     * Read data from hyperslab in the file into the hyperslab in
     * memory and display.
     */
    if(H5Dread(dataset, H5T_NATIVE_LLONG, H5S_ALL, H5S_ALL, H5P_DEFAULT,
            int_data_out) < 0)
        TEST_ERROR;

    /* Check results */
    for (j=0; j<(NX+1); j++) {
        for (i=0; i<NY; i++) {
            if (int_data_out[j][i] != int_data_in[j][i]) {
                if (!nerrors++) {
                    H5_FAILED();
                    printf("element [%d][%d] is %d but should have been %d\n",
                           j, i, (int)int_data_out[j][i],
                           (int)int_data_in[j][i]);
                }
            }
        }
    }

    /*
     * Close/release resources.
     */
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /* Failure */
    if (nerrors) {
        printf("total of %d errors out of %d elements\n", nerrors, NX*NY);
        return 1;
    }

    PASSED();

#else /*H5_HAVE_FILTER_SCALEOFFSET*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_SCALEOFFSET*/

    TESTING("dataset of BE INT with scale-offset filter");

#ifdef H5_HAVE_FILTER_SCALEOFFSET
    /*
     * Open the dataset with scale-offset filter.
     */
    if((dataset = H5Dopen2(file, DATASETNAME11, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /*
     * Data and output buffer initialization.
     */
    for (j = 0; j < NX; j++) {
        for (i = 0; i < NY; i++) {
            int_data_in[j][i] = i + j;
            int_data_out[j][i] = 0;
        }
    }
    for (i = 0; i < NY; i++) {
        int_data_in[NX][i] = -2;
        int_data_out[NX][i] = 0;
    }

    /*
     * Read data from hyperslab in the file into the hyperslab in
     * memory and display.
     */
    if(H5Dread(dataset, H5T_NATIVE_LLONG, H5S_ALL, H5S_ALL, H5P_DEFAULT,
            int_data_out) < 0)
        TEST_ERROR;

    /* Check results */
    for (j=0; j<(NX+1); j++) {
        for (i=0; i<NY; i++) {
            if (int_data_out[j][i] != int_data_in[j][i]) {
                if (!nerrors++) {
                    H5_FAILED();
                    printf("element [%d][%d] is %d but should have been %d\n",
                           j, i, (int)int_data_out[j][i],
                           (int)int_data_in[j][i]);
                }
            }
        }
    }

    /*
     * Close/release resources.
     */
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /* Failure */
    if (nerrors) {
        printf("total of %d errors out of %d elements\n", nerrors, NX*NY);
        return 1;
    }

    PASSED();

#else /*H5_HAVE_FILTER_SCALEOFFSET*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_SCALEOFFSET*/

    TESTING("dataset of LE LONG LONG with scale-offset filter");

#ifdef H5_HAVE_FILTER_SCALEOFFSET
    /*
     * Open the dataset with scale-offset filter.
     */
    if((dataset = H5Dopen2(file, DATASETNAME12, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /*
     * Data and output buffer initialization.
     */
    for (j = 0; j < NX; j++) {
        for (i = 0; i < NY; i++) {
            int_data_in[j][i] = i + j;
            int_data_out[j][i] = 0;
        }
    }
    for (i = 0; i < NY; i++) {
        int_data_in[NX][i] = -2;
        int_data_out[NX][i] = 0;
    }

    /*
     * Read data from hyperslab in the file into the hyperslab in
     * memory and display.
     */
    if(H5Dread(dataset, H5T_NATIVE_LLONG, H5S_ALL, H5S_ALL, H5P_DEFAULT,
            int_data_out) < 0)
        TEST_ERROR;

    /* Check results */
    for (j=0; j<(NX+1); j++) {
        for (i=0; i<NY; i++) {
            if (int_data_out[j][i] != int_data_in[j][i]) {
                if (!nerrors++) {
                    H5_FAILED();
                    printf("element [%d][%d] is %d but should have been %d\n",
                           j, i, (int)int_data_out[j][i],
                           (int)int_data_in[j][i]);
                }
            }
        }
    }

    /*
     * Close/release resources.
     */
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /* Failure */
    if (nerrors) {
        printf("total of %d errors out of %d elements\n", nerrors, NX*NY);
        return 1;
    }

    PASSED();

#else /*H5_HAVE_FILTER_SCALEOFFSET*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_SCALEOFFSET*/

    TESTING("dataset of BE LONG LONG with scale-offset filter");

#ifdef H5_HAVE_FILTER_SCALEOFFSET
    /*
     * Open the dataset with scale-offset filter.
     */
    if((dataset = H5Dopen2(file, DATASETNAME13, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /*
     * Data and output buffer initialization.
     */
    for (j = 0; j < NX; j++) {
        for (i = 0; i < NY; i++) {
            int_data_in[j][i] = i + j;
            int_data_out[j][i] = 0;
        }
    }
    for (i = 0; i < NY; i++) {
        int_data_in[NX][i] = -2;
        int_data_out[NX][i] = 0;
    }

    /*
     * Read data from hyperslab in the file into the hyperslab in
     * memory and display.
     */
    if(H5Dread(dataset, H5T_NATIVE_LLONG, H5S_ALL, H5S_ALL, H5P_DEFAULT,
            int_data_out) < 0)
        TEST_ERROR;

    /* Check results */
    for (j=0; j<(NX+1); j++) {
        for (i=0; i<NY; i++) {
            if (int_data_out[j][i] != int_data_in[j][i]) {
                if (!nerrors++) {
                    H5_FAILED();
                    printf("element [%d][%d] is %d but should have been %d\n",
                           j, i, (int)int_data_out[j][i],
                           (int)int_data_in[j][i]);
                }
            }
        }
    }

    /*
     * Close/release resources.
     */
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /* Failure */
    if (nerrors) {
        printf("total of %d errors out of %d elements\n", nerrors, NX*NY);
        return 1;
    }

    PASSED();

#else /*H5_HAVE_FILTER_SCALEOFFSET*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_SCALEOFFSET*/

    if(H5Fclose(file))
        TEST_ERROR
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests the basic features of Virtual File Drivers
 *
 * Return:      Success:        exit(0)
 *              Failure:        exit(1)
 *
 * Programmer:  Raymond Lu
 *              Tuesday, Sept 24, 2002
 *
 *-------------------------------------------------------------------------
 */
int main(void)
{
    char        filename[1024];
    unsigned 	nerrors = 0;

    h5_reset();

    puts("Testing reading data created on OpenVMS");
    h5_fixname(FILENAME[0], H5P_DEFAULT, filename, sizeof filename);
    nerrors += read_data(filename);

    puts("Testing reading data created on Linux");
    h5_fixname(FILENAME[1], H5P_DEFAULT, filename, sizeof filename);
    nerrors += read_data(filename);

    puts("Testing reading data created on Solaris");
    h5_fixname(FILENAME[2], H5P_DEFAULT, filename, sizeof filename);
    nerrors += read_data(filename);

    if (nerrors) {
        printf("***** %u FAILURE%s! *****\n",
               nerrors, 1==nerrors?"":"S");
        HDexit(1);
    }

    printf("All data type tests passed.\n");
    return 0;
}
