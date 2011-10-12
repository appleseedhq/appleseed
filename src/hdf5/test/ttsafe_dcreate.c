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
 * Testing thread safety in dataset creation in the HDF5 library
 * -------------------------------------------------------------
 *
 * Set of tests to run multiple threads so that each creates a different
 * dataset. This is likely to cause race-conditions if run in a non
 * threadsafe environment.
 *
 * Temporary files generated:
 *   ttsafe_dcreate.h5
 *
 * HDF5 APIs exercised in thread:
 * H5Screate_simple, H5Tcopy, H5Tset_order, H5Dcreate2, H5Dwrite, H5Dclose,
 * H5Tclose, H5Sclose.
 *
 * Created: Apr 28 2000
 * Programmer: Chee Wai LEE
 *
 * Modification History
 * --------------------
 *
 *	19 May 2000, Bill Wendling
 *	Changed so that it creates its own HDF5 file and removes it at cleanup
 *	time.
 *
 ********************************************************************/
#include "ttsafe.h"

#ifdef H5_HAVE_THREADSAFE

#define FILENAME		"ttsafe_dcreate.h5"
#define DATASETNAME_LENGTH	10
#define NUM_THREAD		16

void *tts_dcreate_creator(void *);

typedef struct thread_info {
	int id;
	hid_t file;
	const char *dsetname;
} thread_info;

/*
 * Set individual dataset names (rather than generated the names
 * automatically)
 */
const char *dsetname[NUM_THREAD]={
    "zero",
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
    "ten",
    "eleven",
    "twelve",
    "thirteen",
    "fourteen",
    "fifteen"
};

thread_info thread_out[NUM_THREAD];

/*
 **********************************************************************
 * Thread safe test - multiple dataset creation
 **********************************************************************
 */
void tts_dcreate(void)
{
    /* thread definitions */
    H5TS_thread_t threads[NUM_THREAD];

    /* HDF5 data definitions */
    hid_t file, dataset;
    int datavalue, i;
    H5TS_attr_t attribute;
    int ret;

    /* set pthread attribute to perform global scheduling */
    H5TS_attr_init(&attribute);

    /* set thread scope to system */
#ifdef H5_HAVE_SYSTEM_SCOPE_THREADS
    H5TS_attr_setscope(&attribute, H5TS_SCOPE_SYSTEM);
#endif /* H5_HAVE_SYSTEM_SCOPE_THREADS */

    /*
     * Create a hdf5 file using H5F_ACC_TRUNC access, default file
     * creation plist and default file access plist
     */
    file = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    assert(file >= 0);

    /* simultaneously create a large number of datasets within the file */
    for(i = 0; i < NUM_THREAD; i++) {
        thread_out[i].id = i;
        thread_out[i].file = file;
        thread_out[i].dsetname = dsetname[i];
        threads[i] = H5TS_create_thread(tts_dcreate_creator, NULL, &thread_out[i]);
    } /* end for */

    for(i = 0;i < NUM_THREAD; i++) {
        H5TS_wait_for_thread(threads[i]);
    } /* end for */

    /* compare data to see if it is written correctly */

    for(i = 0; i < NUM_THREAD; i++) {
        if((dataset = H5Dopen2(file, dsetname[i], H5P_DEFAULT)) < 0) {
            TestErrPrintf("Dataset name not found - test failed\n");
            H5Fclose(file);
            return;
        } else {
            ret = H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &datavalue);
            assert(ret >= 0);

            if(datavalue != i) {
                TestErrPrintf("Wrong value read %d for dataset name %s - test failed\n",
                            datavalue, dsetname[i]);
                ret = H5Dclose(dataset);
                assert(ret >= 0);
                ret = H5Fclose(file);
                assert(ret >= 0);
                return;
            }

            ret = H5Dclose(dataset);
            assert(ret >= 0);
        }
    }

    /* close remaining resources */
    ret = H5Fclose(file);
    assert(ret >= 0);

    /* Destroy the thread attribute */
    H5TS_attr_destroy(&attribute);
}

void *tts_dcreate_creator(void *_thread_data)
{
    hid_t   dataspace, dataset;
    herr_t  ret;
    hsize_t dimsf[1]; /* dataset dimensions */
    struct thread_info thread_data;

    memcpy(&thread_data, _thread_data, sizeof(struct thread_info));

    /* define dataspace for dataset */
    dimsf[0] = 1;
    dataspace = H5Screate_simple(1, dimsf, NULL);
    assert(dataspace >= 0);

    /* create a new dataset within the file */
    dataset = H5Dcreate2(thread_data.file, thread_data.dsetname,
                        H5T_NATIVE_INT, dataspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    assert(dataset >= 0);

    /* initialize data for dataset and write value to dataset */
    ret = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
             H5P_DEFAULT, &thread_data.id);
    assert(ret >= 0);

    /* close dataset and dataspace resources */
    ret = H5Dclose(dataset);
    assert(ret >= 0);
    ret = H5Sclose(dataspace);
    assert(ret >= 0);

    return NULL;
}

void cleanup_dcreate(void)
{
    HDunlink(FILENAME);
}
#endif /*H5_HAVE_THREADSAFE*/

