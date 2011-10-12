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
 * Programmer:  Neil Fortner <nfortne2@hdfgroup.org>
 *              Thursday, August 14, 2008
 *
 * Purpose: Tests closing the library after reference counts have been
 *          manipulated.
 */
#include "h5test.h"

#define APPREF_DSET "test_dset"
#define APPREF_ATTR "test_attr"
#define APPREF_GROUP "test_grp"

#define ERR_WIDTH   40      /* Width of output for the SIGABRT handler */
#define MAX_NINC    16      /* Maximum increments of a reference count */

/* Macro to increment the reference count on id a random number of times (from
 * 1 to MAX_NINC).  Assumes integers i and ninc are in scope. */
#define RAND_INC(id)                                                           \
    ninc = (HDrand() % MAX_NINC) + 1;                                          \
                                                                               \
    for (i=0; i<ninc; i++)                                                     \
        if (H5Iinc_ref(ids[id]) != i + 2)                                      \
            TEST_ERROR                                                         \
                                                                               \
    rc[id] = ninc + 1;

typedef enum {
    T_FILE,
    T_PLIST,
    T_PCLASS,
    T_TYPE,
    T_SPACE,
    T_DSET,
    T_ATTR,
    T_GROUP,
    T_ECLASS,
    T_EMSG,
    T_ESTACK,
    T_NUMCLASSES
} id_class_t;

const char *FILENAME[] = {
    "app_ref",
    NULL
};

const char *IDNAME[T_NUMCLASSES] = {
    "File",
    "Property List",
    "Property Class",
    "Datatype",
    "Dataspace",
    "Dataset",
    "Attribute",
    "Group",
    "Error Class",
    "Error Message",
    "Error Stack"
};

int rc[T_NUMCLASSES];

void Abrt_Handler (int sig);

/* Handler for SIGABRT - prints the reference count on each id */
void
Abrt_Handler (int UNUSED sig)
{
    int i, n;

    for (i=0; i<T_NUMCLASSES; i++) {
        fprintf(stderr, "%s ID reference count: %n", IDNAME[i], &n);
        fprintf(stderr, "%*d\n", (n < ERR_WIDTH) ? (ERR_WIDTH - n) : 0, rc[i]);
    }
}

/* Main test routine */
int
main (void)
{
    hid_t   ids[T_NUMCLASSES];
    hid_t   fapl;               /* File Access Property List */
    int     ninc;
    int     i;
    char    filename[1024];

    h5_reset();
    h5_fixname (FILENAME[0], H5P_DEFAULT, filename, sizeof filename);

    HDsrand ((unsigned) HDtime (NULL));

    TESTING ("library shutdown with reference count > 1");

    /* Create the file */
    if ((ids[T_FILE] = H5Fcreate (filename, H5F_ACC_TRUNC, H5P_DEFAULT,
                H5P_DEFAULT)) < 0)
        TEST_ERROR

    RAND_INC (T_FILE)

    /* Create the property list */
    if ((ids[T_PLIST] = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR

    RAND_INC (T_PLIST)

    /* Create a property class */
    if ((ids[T_PCLASS] = H5Pcreate_class (H5P_DATASET_CREATE, "foo", NULL, NULL,
                NULL, NULL, NULL, NULL)) < 0)
        TEST_ERROR

    RAND_INC (T_PCLASS)

    /* Create a datatype */
    if ((ids[T_TYPE] = H5Tcreate (H5T_OPAQUE, (size_t) 16)) < 0)
        TEST_ERROR

    RAND_INC (T_TYPE)

    /* Create a dataspace */
    if ((ids[T_SPACE] = H5Screate (H5S_SCALAR)) < 0)
        TEST_ERROR

    RAND_INC (T_SPACE)

    /* Create a dataset */
    if ((ids[T_DSET] = H5Dcreate2 (ids[T_FILE], APPREF_DSET, H5T_NATIVE_INT,
                ids[T_SPACE], H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    RAND_INC (T_DSET)

    /* Create an attribute */
    if ((ids[T_ATTR] = H5Acreate2 (ids[T_DSET], APPREF_ATTR, H5T_NATIVE_INT,
                ids[T_SPACE], H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    RAND_INC (T_ATTR)

    /* Create a group */
    if ((ids[T_GROUP] = H5Gcreate2 (ids[T_FILE], APPREF_GROUP, H5P_DEFAULT,
                H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    RAND_INC (T_GROUP)

    /* Create an error class */
    if ((ids[T_ECLASS] = H5Eregister_class("foo","bar","baz")) < 0)
        TEST_ERROR

    RAND_INC (T_ECLASS)

    /* Create an error message */
    if ((ids[T_EMSG] = H5Ecreate_msg(ids[T_ECLASS],H5E_MAJOR,"mumble")) < 0)
        TEST_ERROR

    RAND_INC (T_EMSG)

    /* Create an error stack */
    if ((ids[T_ESTACK] = H5Eget_current_stack()) < 0)
        TEST_ERROR

    RAND_INC (T_ESTACK)

    HDsignal (SIGABRT, &Abrt_Handler);

    if (H5close() < 0)
        TEST_ERROR

    PASSED();

    h5_reset();
    fapl = H5Pcreate (H5P_FILE_ACCESS);
    h5_cleanup (FILENAME, fapl);

    return 0;

error:

    puts("***** APPLICATION REFERENCE COUNT TESTS FAILED *****");

    return 1;
}
