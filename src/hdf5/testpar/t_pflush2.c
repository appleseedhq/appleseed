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
 * Purpose:     This is the second half of a two-part test that makes sure
 *              that a file can be read after a parallel application crashes as long
 *              as the file was flushed first.  We simulate a crash by
 *              calling _exit(0) since this doesn't flush HDF5 caches but
 *              still exits with success.
 */

#include "h5test.h"

const char *FILENAME[] = {
    "flush",
    "noflush",
    NULL
};

static double	the_data[100][100];


/*-------------------------------------------------------------------------
 * Function:	check_file
 *
 * Purpose:	Part 2 of a two-part H5Fflush() test.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Leon Arber
 *              Sept. 26, 2006.
 *
 *-------------------------------------------------------------------------
 */
static int
check_file(char* name, hid_t fapl)
{
    hid_t	file, space, dset, groups, grp, plist;
    hsize_t	ds_size[2];
    double	error;
    hsize_t	i, j;

    plist = H5Pcreate(H5P_DATASET_XFER);
    H5Pset_dxpl_mpio(plist, H5FD_MPIO_COLLECTIVE);
    if((file = H5Fopen(name, H5F_ACC_RDONLY, fapl)) < 0) goto error;

    /* Open the dataset */
    if((dset = H5Dopen2(file, "dset", H5P_DEFAULT)) < 0) goto error;
    if((space = H5Dget_space(dset)) < 0) goto error;
    if(H5Sget_simple_extent_dims(space, ds_size, NULL) < 0) goto error;
    assert(100==ds_size[0] && 100==ds_size[1]);

    /* Read some data */
    if (H5Dread(dset, H5T_NATIVE_DOUBLE, space, space, plist,
		the_data) < 0) goto error;
    for (i=0; i<ds_size[0]; i++) {
	for (j=0; j<ds_size[1]; j++) {
	    /*
	     * The extra cast in the following statement is a bug workaround
	     * for the Win32 version 5.0 compiler.
	     * 1998-11-06 ptl
	     */
	    error = fabs(the_data[i][j]-(double)(hssize_t)i/((hssize_t)j+1));
	    if (error>0.0001) {
		H5_FAILED();
		printf("    dset[%lu][%lu] = %g\n",
			(unsigned long)i, (unsigned long)j, the_data[i][j]);
		printf("    should be %g\n",
			(double)(hssize_t)i/(hssize_t)(j+1));
		goto error;
	    }
	}
    }

    /* Open some groups */
    if((groups = H5Gopen2(file, "some_groups", H5P_DEFAULT)) < 0) goto error;
    for(i = 0; i < 100; i++) {
	sprintf(name, "grp%02u", (unsigned)i);
	if((grp = H5Gopen2(groups, name, H5P_DEFAULT)) < 0) goto error;
	if(H5Gclose(grp) < 0) goto error;
    }

    if(H5Gclose(groups) < 0) goto error;
    if(H5Dclose(dset) < 0) goto error;
    if(H5Fclose(file) < 0) goto error;
    if(H5Pclose(plist) < 0) goto error;

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
        H5Pclose(plist);
    } H5E_END_TRY;
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Part 2 of a two-part H5Fflush() test.
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
 * 		Sept. 26, 2006, expand to check for case where the was file not flushed.
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, char* argv[])
{
    hid_t fapl1, fapl2;
    H5E_auto2_t func;

    char	name[1024];
    const char *envval = NULL;

    int mpi_size, mpi_rank;
    MPI_Comm comm  = MPI_COMM_WORLD;
    MPI_Info info  = MPI_INFO_NULL;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(comm, &mpi_size);
    MPI_Comm_rank(comm, &mpi_rank);

    fapl1 = H5Pcreate(H5P_FILE_ACCESS);
    H5Pset_fapl_mpio(fapl1, comm, info);

    fapl2 = H5Pcreate(H5P_FILE_ACCESS);
    H5Pset_fapl_mpio(fapl2, comm, info);


    if(mpi_rank == 0)
	TESTING("H5Fflush (part2 with flush)");

    /* Don't run this test using the core or split file drivers */
    envval = HDgetenv("HDF5_DRIVER");
    if (envval == NULL)
        envval = "nomatch";
    if (HDstrcmp(envval, "core") && HDstrcmp(envval, "split")) {
	/* Check the case where the file was flushed */
	h5_fixname(FILENAME[0], fapl1, name, sizeof name);
	if(check_file(name, fapl1))
	{
	    H5_FAILED()
	    goto error;
	}
	else if(mpi_rank == 0)
	{
	    PASSED()
	}

	/* Check the case where the file was not flushed.  This should give an error
	 * so we turn off the error stack temporarily */
	if(mpi_rank == 0)
	    TESTING("H5Fflush (part2 without flush)");
	H5Eget_auto2(H5E_DEFAULT,&func,NULL);
	H5Eset_auto2(H5E_DEFAULT, NULL, NULL);

	h5_fixname(FILENAME[1], fapl2, name, sizeof name);
	if(check_file(name, fapl2))
	{
	    if(mpi_rank == 0)
	    {
		PASSED()
	    }
	}
	else
	{
	    H5_FAILED()
	    goto error;
	}
	H5Eset_auto2(H5E_DEFAULT, func, NULL);


	h5_cleanup(&FILENAME[0], fapl1);
	h5_cleanup(&FILENAME[1], fapl2);
    }
    else
    {
        SKIPPED();
        puts("    Test not compatible with current Virtual File Driver");
    }

    MPI_Finalize();
    return 0;

    error:
        return 1;
}




