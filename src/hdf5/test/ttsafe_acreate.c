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

/********************************************************************
 *
 * Testing for thread safety in H5A (dataset attribute) library
 * operations. -- Threaded program --
 * ------------------------------------------------------------------
 *
 * Plan: Attempt to break H5Acreate2 by making many simultaneous create
 *       calls.
 *
 * Claim: N calls to H5Acreate2 should create N attributes for a dataset
 *        if threadsafe. If some unprotected shared data exists for the
 *        dataset (eg, a count of the number of attributes in the
 *        dataset), there is a small chance that consecutive reads occur
 *        before a write to that shared variable.
 *
 * HDF5 APIs exercised in thread:
 * H5Acreate2, H5Awrite, H5Aclose.
 *
 * Created: Oct 5 1999
 * Programmer: Chee Wai LEE
 *
 * Modification History
 * --------------------
 *
 *	15 May 2000, Chee Wai LEE
 *	Incorporated into library tests.
 *
 *	19 May 2000, Bill Wendling
 *	Changed so that it creates its own HDF5 file and removes it at cleanup
 *	time.
 *
 ********************************************************************/

#include "ttsafe.h"

#ifdef H5_HAVE_THREADSAFE

#include <stdio.h>
#include <stdlib.h>

#define FILENAME	"ttsafe_acreate.h5"
#define DATASETNAME	"IntData"
#define NUM_THREADS	16

void *tts_acreate_thread(void *);

typedef struct acreate_data_struct {
	hid_t dataset;
	hid_t datatype;
	hid_t dataspace;
	int current_index;
} ttsafe_name_data_t;

void tts_acreate(void)
{
    /* Thread declarations */
    H5TS_thread_t threads[NUM_THREADS];

    /* HDF5 data declarations */
    hid_t   file, dataset;
    hid_t   dataspace, datatype;
    hid_t   attribute;
    hsize_t dimsf[1];		/* dataset dimensions */

    /* data declarations */
    int     data;			/* data to write */
    int     buffer, ret, i;

    ttsafe_name_data_t *attrib_data;

    /*
     * Create an HDF5 file using H5F_ACC_TRUNC access, default file
     * creation plist and default file access plist
     */
    file = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    assert(file>=0);

    /* create a simple dataspace for the dataset */
    dimsf[0] = 1;
    dataspace = H5Screate_simple(1, dimsf, NULL);
    assert(dataspace>=0);

    /* define datatype for the data using native little endian integers */
    datatype = H5Tcopy(H5T_NATIVE_INT);
    H5Tset_order(datatype, H5T_ORDER_LE);

    /* create a new dataset within the file */
    dataset = H5Dcreate2(file, DATASETNAME, datatype, dataspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    assert(dataset >= 0);

    /* initialize data for dataset and write value to dataset */
    data = NUM_THREADS;
    ret = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &data);
    assert(ret >= 0);

    /*
     * Simultaneously create a large number of attributes to be associated
     * with the dataset
     */
    for(i = 0; i < NUM_THREADS; i++) {
        attrib_data = malloc(sizeof(ttsafe_name_data_t));
        attrib_data->dataset = dataset;
        attrib_data->datatype = datatype;
        attrib_data->dataspace = dataspace;
        attrib_data->current_index = i;
        threads[i] = H5TS_create_thread(tts_acreate_thread, NULL, attrib_data);
    } /* end for */

    for(i = 0; i < NUM_THREADS; i++) {
        H5TS_wait_for_thread(threads[i]);
    } /* end for */


    /* verify the correctness of the test */
    for(i = 0; i < NUM_THREADS; i++) {
        attribute = H5Aopen(dataset, gen_name(i), H5P_DEFAULT);

        if(attribute < 0)
            TestErrPrintf("unable to open appropriate attribute.  Test failed!\n");
        else {
            ret = H5Aread(attribute, H5T_NATIVE_INT, &buffer);

            if(ret < 0 || buffer != i)
                TestErrPrintf("wrong data values. Test failed!\n");

            H5Aclose(attribute);
        } /* end else */
    } /* end for */

    /* close remaining resources */
    ret = H5Sclose(dataspace);
    assert(ret >= 0);
    ret = H5Tclose(datatype);
    assert(ret >= 0);
    ret = H5Dclose(dataset);
    assert(ret >= 0);
    ret = H5Fclose(file);
    assert(ret >= 0);
}

void *tts_acreate_thread(void *client_data)
{
    hid_t   attribute;
    char    *attribute_name;
    int     *attribute_data;	/* data for attributes */

    ttsafe_name_data_t *attrib_data;

    attrib_data = (ttsafe_name_data_t *)client_data;

    /* Create attribute */
    attribute_name = gen_name(attrib_data->current_index);
    attribute = H5Acreate2(attrib_data->dataset, attribute_name,
                          attrib_data->datatype, attrib_data->dataspace,
                          H5P_DEFAULT, H5P_DEFAULT);

    /* Write data to the attribute */
    attribute_data = malloc(sizeof(int));
    *attribute_data = attrib_data->current_index;
    H5Awrite(attribute, H5T_NATIVE_INT, attribute_data);
    H5Aclose(attribute);
    return NULL;
}

void cleanup_acreate(void)
{
    HDunlink(FILENAME);
}

#endif /*H5_HAVE_THREADSAFE*/
