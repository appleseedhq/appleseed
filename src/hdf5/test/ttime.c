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

/***********************************************************
*
* Test program:	 ttime
*
* Test the Time Datatype functionality
*
*************************************************************/

#include "testhdf5.h"

#include "hdf5.h"

#define DATAFILE   "ttime.h5"
#define DATASETNAME   "Dataset"

/****************************************************************
**
**  test_time_commit(): Test committing time datatypes to a file
**
****************************************************************/
static void
test_time_commit(void)
{
    hid_t       file_id, tid;  /* identifiers */
    herr_t      status;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Committing Time Datatypes\n"));

    /* Create a new file using default properties. */
    file_id = H5Fcreate(DATAFILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file_id, FAIL, "H5Fcreate");

    tid = H5Tcopy (H5T_UNIX_D32LE);
    CHECK(tid, FAIL, "H5Tcopy");
    status = H5Tcommit2(file_id, "Committed D32LE type", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(status, FAIL, "H5Tcommit2");
    status = H5Tclose (tid);
    CHECK(status, FAIL, "H5Tclose");

    tid = H5Tcopy (H5T_UNIX_D32BE);
    CHECK(tid, FAIL, "H5Tcopy");
    status = H5Tcommit2(file_id, "Committed D32BE type", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(status, FAIL, "H5Tcommit2");
    status = H5Tclose (tid);
    CHECK(status, FAIL, "H5Tclose");

    tid = H5Tcopy (H5T_UNIX_D64LE);
    CHECK(tid, FAIL, "H5Tcopy");
    status = H5Tcommit2(file_id, "Committed D64LE type", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(status, FAIL, "H5Tcommit2");
    status = H5Tclose (tid);
    CHECK(status, FAIL, "H5Tclose");

    tid = H5Tcopy (H5T_UNIX_D64BE);
    CHECK(tid, FAIL, "H5Tcopy");
    status = H5Tcommit2(file_id, "Committed D64BE type", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(status, FAIL, "H5Tcommit2");
    status = H5Tclose (tid);
    CHECK(status, FAIL, "H5Tclose");

    /* Close the file. */
    status = H5Fclose(file_id);
    CHECK(status, FAIL, "H5Fclose");

    file_id = H5Fopen(DATAFILE, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(file_id, FAIL, "H5Fopen");

    tid = H5Topen2(file_id, "Committed D32LE type", H5P_DEFAULT);
    CHECK(tid, FAIL, "H5Topen2");

    if(!H5Tequal(tid, H5T_UNIX_D32LE))
        TestErrPrintf("H5T_UNIX_D32LE datatype not found\n");

    status = H5Tclose (tid);
    CHECK(status, FAIL, "H5Tclose");

    tid = H5Topen2(file_id, "Committed D32BE type", H5P_DEFAULT);
    CHECK(tid, FAIL, "H5Topen2");

    if(!H5Tequal(tid, H5T_UNIX_D32BE))
        TestErrPrintf("H5T_UNIX_D32BE datatype not found\n");

    status = H5Tclose (tid);
    CHECK(status, FAIL, "H5Tclose");

    tid = H5Topen2(file_id, "Committed D64LE type", H5P_DEFAULT);
    CHECK(tid, FAIL, "H5Topen2");

    if(!H5Tequal(tid, H5T_UNIX_D64LE))
        TestErrPrintf("H5T_UNIX_D64LE datatype not found");

    status = H5Tclose (tid);
    CHECK(status, FAIL, "H5Tclose");

    tid = H5Topen2(file_id, "Committed D64BE type", H5P_DEFAULT);
    CHECK(tid, FAIL, "H5Topen2");

    if(!H5Tequal(tid, H5T_UNIX_D64BE))
        TestErrPrintf("H5T_UNIX_D64BE datatype not found");

    status = H5Tclose (tid);
    CHECK(status, FAIL, "H5Tclose");

    status = H5Fclose(file_id);
    CHECK(status, FAIL, "H5Fclose");

}

#ifdef NOT_YET
/****************************************************************
**
**  test_time_io(): Test writing time data to a dataset
**
****************************************************************/
static void
test_time_io(void)
{
    hid_t       fid;            /* File identifier */
    hid_t       dsid;           /* Dataset identifier */
    hid_t       tid;            /* Datatype identifier */
    hid_t       sid;            /* Dataspace identifier */
    time_t      timenow, timethen;      /* Times */
    herr_t      status;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Committing Time Datatypes\n"));

    /* Create a new file using default properties. */
    fid = H5Fcreate(DATAFILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create a scalar dataspace */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create a dataset with a time datatype */
    dsid = H5Dcreate2(fid, DATASETNAME, H5T_UNIX_D32LE, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dsid, FAIL, "H5Dcreate2");

    /* Initialize time data value */
    timenow = HDtime(NULL);

    /* Write time to dataset */
    status = H5Dwrite (dsid, H5T_UNIX_D32LE, H5S_ALL, H5S_ALL, H5P_DEFAULT, &timenow);
    CHECK(status, FAIL, "H5Dwrite");

    /* Close objects */
    status = H5Dclose(dsid);
    CHECK(status, FAIL, "H5Dclose");

    status = H5Sclose(sid);
    CHECK(status, FAIL, "H5Sclose");

    status = H5Fclose (fid);
    CHECK(status, FAIL, "H5Fclose");

    /* Open file and dataset, read time back and print it in calendar format */
    fid = H5Fopen(DATAFILE, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    dsid = H5Dopen2(fid, DATASETNAME, H5P_DEFAULT);
    CHECK(dsid, FAIL, "H5Dopen2");

tid = H5Dget_type(dsid);
CHECK(tid, FAIL, "H5Dget_type");
if( H5Tget_class (tid) == H5T_TIME)
    fprintf(stderr,"datatype class is H5T_TIME\n");
status = H5Tclose (tid);
CHECK(status, FAIL, "H5Tclose");

    status = H5Dread (dsid, H5T_UNIX_D32LE, H5S_ALL, H5S_ALL, H5P_DEFAULT, &timethen);
    CHECK(status, FAIL, "H5Dread");
fprintf(stderr,"time written was: %s\n", HDctime(&timethen));

    status = H5Dclose(dsid);
    CHECK(status, FAIL, "H5Dclose");

    status = H5Fclose(fid);
    CHECK(status, FAIL, "H5Fclose");

}
#endif /* NOT_YET */

/****************************************************************
**
**  test_time(): Main time datatype testing routine.
**
****************************************************************/
void
test_time(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing Time Datatypes\n"));

    test_time_commit();         /* Test committing time datatypes to a file */
#ifdef NOT_YET
    test_time_io();             /* Test writing time data to a dataset */
#endif /* NOT_YET */

}   /* test_time() */


/*-------------------------------------------------------------------------
 * Function:	cleanup_time
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	Quincey Koziol
 *              October 19, 2000
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_time(void)
{
    remove(DATAFILE);
}

