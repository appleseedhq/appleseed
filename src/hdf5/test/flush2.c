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
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Friday, October 23, 1998
 *
 * Purpose:	This is the second half of a two-part test that makes sure
 *		that a file can be read after an application crashes as long
 *		as the file was flushed first.  This half tries to read the
 *		file created by the first half.
 */
#include "h5test.h"

const char *FILENAME[] = {
    "flush",
    "noflush",
    "noflush_extend",
    NULL
};

static double	the_data[100][100];

/*-------------------------------------------------------------------------
 * Function:	check_dset
 *
 * Purpose:	Part 2 of a two-part H5Fflush() test, checks if the data in a dataset
 * 		is what it is supposed to be.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Leon Arber
 *              Oct. 4, 2006.
 *
 *-------------------------------------------------------------------------
 */
static int
check_dset(hid_t file, const char* name)
{
    hid_t	space, dset;
    hsize_t	ds_size[2] = {100, 100};
    double	error;
    size_t	i, j;

    /* Open the dataset */
    if((dset = H5Dopen2(file, name, H5P_DEFAULT)) < 0) goto error;
    if((space = H5Dget_space(dset)) < 0) goto error;
    if(H5Sget_simple_extent_dims(space, ds_size, NULL) < 0) goto error;
    assert(100 == ds_size[0] && 100 == ds_size[1]);

    /* Read some data */
    if(H5Dread(dset, H5T_NATIVE_DOUBLE, space, space, H5P_DEFAULT,
		the_data) < 0) goto error;
    for(i = 0; i < (size_t)ds_size[0]; i++)
	for(j = 0; j < (size_t)ds_size[1]; j++) {
	    /*
	     * The extra cast in the following statement is a bug workaround
	     * for the Win32 version 5.0 compiler.
	     * 1998-11-06 ptl
	     */
	    error = fabs(the_data[i][j] - (double)(hssize_t)i / ((hssize_t)j + 1));
	    if(error > 0.0001) {
		H5_FAILED();
		printf("    dset[%lu][%lu] = %g\n",
			(unsigned long)i, (unsigned long)j, the_data[i][j]);
		printf("    should be %g\n",
			(double)(hssize_t)i/(hssize_t)(j+1));
		goto error;
	    }
	}
    if(H5Dclose(dset) < 0) goto error;
    return 0;

error:
    return 1;
}


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
check_file(char* filename, hid_t fapl, int flag)
{
    hid_t	file, groups, grp;
    char	name[1024];
    int		i;

    if((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0) goto error;
    if(check_dset(file, "dset")) goto error;

    /* Open some groups */
    if((groups = H5Gopen2(file, "some_groups", H5P_DEFAULT)) < 0) goto error;
    for(i = 0; i < 100; i++) {
	sprintf(name, "grp%02u", (unsigned)i);
	if((grp = H5Gopen2(groups, name, H5P_DEFAULT)) < 0) goto error;
	if(H5Gclose(grp) < 0) goto error;
    } /* end for */

    /* Check to see if that last added dataset in the third file is accessible
     * (it shouldn't be...but it might.  Flag an error in case it is for now */
    if(flag && check_dset(file, "dset2")) goto error;

    if(H5Gclose(groups) < 0) goto error;
    if(H5Fclose(file) < 0) goto error;

    return 0;

error:
    return 1;
} /* end check_file() */


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
main(void)
{
    hid_t fapl;
    H5E_auto2_t func;
    char	name[1024];

    h5_reset();
    fapl = h5_fileaccess();
    TESTING("H5Fflush (part2 with flush)");

    /* Check the case where the file was flushed */
    h5_fixname(FILENAME[0], fapl, name, sizeof name);
    if(check_file(name, fapl, FALSE)) {
        H5_FAILED()
        goto error;
    }
    else
        PASSED();


    /* Check the case where the file was not flushed.  This should give an error
     * so we turn off the error stack temporarily */
    TESTING("H5Fflush (part2 without flush)");
    H5Eget_auto2(H5E_DEFAULT,&func,NULL);
    H5Eset_auto2(H5E_DEFAULT, NULL, NULL);

    h5_fixname(FILENAME[1], fapl, name, sizeof name);
    if(check_file(name, fapl, FALSE))
        PASSED()
    else
    {
#if defined _WIN32 && defined _HDF5USEDLL_
    SKIPPED();
    puts("   DLL will flush the file even when calling _exit, skip this test temporarily");
#elif defined H5_VMS
    SKIPPED();
#else
    H5_FAILED()
    goto error;
#endif
    }
    H5Eset_auto2(H5E_DEFAULT, func, NULL);

    /* Check the case where the file was flushed, but more data was added afterward.  This should give an error
     * so we turn off the error stack temporarily */
    TESTING("H5Fflush (part2 with flush and later addition)");
    H5Eget_auto2(H5E_DEFAULT,&func,NULL);
    H5Eset_auto2(H5E_DEFAULT, NULL, NULL);

    h5_fixname(FILENAME[2], fapl, name, sizeof name);
    if(check_file(name, fapl, TRUE))
        PASSED()
    else
    {
#if defined _WIN32 && defined _HDF5USEDLL_
    SKIPPED();
    puts("   DLL will flush the file even when calling _exit, skip this test temporarily");
#elif defined H5_VMS
    SKIPPED();
#else
    H5_FAILED()
    goto error;
#endif

    }
    H5Eset_auto2(H5E_DEFAULT, func, NULL);

    h5_cleanup(FILENAME, fapl);

    return 0;

error:
    return 1;
}

