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
 * Programmer:  Leon Arber  <larber@uiuc.edu>
 *              Sept. 28, 2006.
 *
 * Purpose:	This is the first half of a two-part test that makes sure
 *		that a file can be read after a parallel application crashes as long
 *		as the file was flushed first.  We simulate a crash by
 *		calling _exit(0) since this doesn't flush HDF5 caches but
 *		still exits with success.
 */
#include <mpi.h>
#include "h5test.h"

const char *FILENAME[] = {
    "flush",
    "noflush",
    NULL
};

static double	the_data[100][100];

/*-------------------------------------------------------------------------
 * Function:	create_file
 *
 * Purpose:	Creates file used in part 1 of the test
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Leon Arber
 *              Sept. 26, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hid_t
create_file(char* name, hid_t fapl)
{
    hid_t	file, dcpl, space, dset, groups, grp, plist;
    hsize_t	ds_size[2] = {100, 100};
    hsize_t	ch_size[2] = {5, 5};
    hsize_t	i, j;



    if((file=H5Fcreate(name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) goto error;

    /* Create a chunked dataset */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk(dcpl, 2, ch_size) < 0) goto error;
    if((space = H5Screate_simple(2, ds_size, NULL)) < 0) goto error;
    if((dset = H5Dcreate2(file, "dset", H5T_NATIVE_FLOAT, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
	goto error;

    plist = H5Pcreate(H5P_DATASET_XFER);
    H5Pset_dxpl_mpio(plist, H5FD_MPIO_COLLECTIVE);


    /* Write some data */
    for(i = 0; i < ds_size[0]; i++) {
	/*
	 * The extra cast in the following statement is a bug workaround
	 * for the Win32 version 5.0 compiler.
	 * 1998-11-06 ptl
	 */
	for(j = 0; j < ds_size[1]; j++)
	    the_data[i][j] = (double)(hssize_t)i/(hssize_t)(j+1);
    }
    if(H5Dwrite(dset, H5T_NATIVE_DOUBLE, space, space, plist, the_data) < 0) goto error;

    /* Create some groups */
    if((groups = H5Gcreate2(file, "some_groups", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) goto error;
    for(i = 0; i < 100; i++) {
	sprintf(name, "grp%02u", (unsigned)i);
	if((grp = H5Gcreate2(groups, name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) goto error;
	if(H5Gclose(grp) < 0) goto error;
    }

    return file;

error:
    HD_exit(1);
}

/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Part 1 of a two-part H5Fflush() test.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Robb Matzke
 *              Friday, October 23, 1998
 *
 * Modifications:
 * 		Leon Arber
 * 		Sept. 26, 2006, expand test to check for failure if H5Fflush is not called.
 *
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, char* argv[])
{
    hid_t file1, file2, fapl;
    MPI_File	*mpifh_p = NULL;
    char	name[1024];
    const char  *envval = NULL;
    int mpi_size, mpi_rank;
    MPI_Comm comm  = MPI_COMM_WORLD;
    MPI_Info info  = MPI_INFO_NULL;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(comm, &mpi_size);
    MPI_Comm_rank(comm, &mpi_rank);

    fapl = H5Pcreate(H5P_FILE_ACCESS);
    H5Pset_fapl_mpio(fapl, comm, info);

    if(mpi_rank == 0)
	TESTING("H5Fflush (part1)");
    envval = HDgetenv("HDF5_DRIVER");
    if(envval == NULL)
        envval = "nomatch";
    if(HDstrcmp(envval, "split")) {
	/* Create the file */
	h5_fixname(FILENAME[0], fapl, name, sizeof name);
	file1 = create_file(name, fapl);
	/* Flush and exit without closing the library */
	if(H5Fflush(file1, H5F_SCOPE_GLOBAL) < 0) goto error;

	/* Create the other file which will not be flushed */
	h5_fixname(FILENAME[1], fapl, name, sizeof name);
	file2 = create_file(name, fapl);


	if(mpi_rank == 0)
	    PASSED();
	fflush(stdout);
	fflush(stderr);
    } /* end if */
    else {
        SKIPPED();
        puts("    Test not compatible with current Virtual File Driver");
    } /* end else */

    /*
     * Some systems like Linux with mpich, if you just _exit without MPI_Finalize
     * called, it would terminate but left the launching process waiting forever.
     * OTHO, some systems like AIX do not like files not closed when MPI_Finalize
     * is called.  So, we need to get the MPI file handles, close them by hand,
     * then MPI_Finalize. Then the _exit is still needed to stop at_exit from
     * happening in some systems.
     * Note that MPIO VFD returns the address of the file-handle in the VFD struct
     * because MPI_File_close wants to modify the file-handle variable.
     */

    /* close file1 */
    if(H5Fget_vfd_handle(file1, fapl, (void **)&mpifh_p) < 0) {
	printf("H5Fget_vfd_handle for file1 failed\n");
	goto error;
    } /* end if */
    if(MPI_File_close(mpifh_p) != MPI_SUCCESS) {
	printf("MPI_File_close for file1 failed\n");
	goto error;
    } /* end if */
    /* close file2 */
    if(H5Fget_vfd_handle(file2, fapl, (void **)&mpifh_p) < 0) {
	printf("H5Fget_vfd_handle for file2 failed\n");
	goto error;
    } /* end if */
    if(MPI_File_close(mpifh_p) != MPI_SUCCESS) {
	printf("MPI_File_close for file2 failed\n");
	goto error;
    } /* end if */

    fflush(stdout);
    fflush(stderr);
    MPI_Finalize();
    HD_exit(0);

error:
    fflush(stdout);
    fflush(stderr);
    MPI_Finalize();
    HD_exit(1);
}

