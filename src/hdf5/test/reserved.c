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

#include "h5test.h"

#ifdef BROKEN
const char *FILENAME[] = {
    "rsrv_heap",
    "rsrv_ohdr",
    "rsrv_vlen",
    NULL
};

/*-------------------------------------------------------------------------
 * Function:	rsrv_heap
 *
 * Purpose:	Ensure that heaps reserve file address space.
 *			This function does this by creating datasets up to and past
 *			the limit of the file, then ensuring that an error (not an
 *			assert) was generated and that the file is readable.
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	James Laird
 *              Nat Furrer
 *              Friday, May 28, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
rsrv_heap(void)
{
    hid_t       file_id=(-1), dataset_id=(-1), dataspace_id=(-1);
    hid_t       fapl=(-1), fcpl=(-1);
    hsize_t     dims[1] = {1};
    char        filename[1024], dset_name[10];
    int         i;

    TESTING("Reserving file space for heap");

    /* Create a new file. */
    fapl = h5_fileaccess();

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    /* Set file address sizes to be very small. */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    if(fcpl < 0) TEST_ERROR;
    if(H5Pset_sizes(fcpl, (size_t)2,(size_t)2) < 0) TEST_ERROR;

    file_id = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl);
    if(file_id < 0) TEST_ERROR;

    /* Write datasets until the file is full, at which point HDF5
     * should throw an error.
     */
    for(i = 0; i < 200; i++) {
        H5E_BEGIN_TRY {
            dataspace_id = H5Screate_simple(1, dims, dims);
        } H5E_END_TRY

        sprintf(dset_name, "Dset %d", i);

        H5E_BEGIN_TRY {
            dataset_id = H5Dcreate2(file_id, dset_name, H5T_NATIVE_INT, dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        } H5E_END_TRY

        if(dataset_id < 0)
            break;

        H5E_BEGIN_TRY {
            H5Dwrite(dataset_id, H5T_NATIVE_INT, dataspace_id, dataspace_id, H5P_DEFAULT, &i);
        } H5E_END_TRY

        if(H5Dclose(dataset_id) < 0) TEST_ERROR;
        if(H5Sclose(dataspace_id) < 0) TEST_ERROR;
    } /* end for */

    /* The loop should have broken before completing--the file should not have had
     * enough address space to hold 200 datasets (or this test needs to be updated!).
     */
    if(i == 200)
        TEST_ERROR;

    /* Close the file, property lists, and library */
    if(H5Fclose(file_id) < 0) TEST_ERROR;
    if(H5Pclose(fapl) < 0) TEST_ERROR;
    if(H5Pclose(fcpl) < 0) TEST_ERROR;
    if(H5close() < 0) TEST_ERROR;

    /* Re-open the library and try to read a dataset from the file we created */
    if(H5open() < 0) TEST_ERROR;

    sprintf(dset_name, "Dset %d", i - 2);

    file_id = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT);
    if(file_id < 0) TEST_ERROR;

    dataset_id = H5Dopen2(file_id, dset_name, H5P_DEFAULT);

    /* If we can read a dataset from the file, the file has been flushed to disk
     * (if the heap or object headers weren't flushed, the file would be empty).
     */
    if(dataset_id == H5I_BADID)
       TEST_ERROR;

    if(H5Dclose(dataset_id) < 0) TEST_ERROR;
    if(H5Fclose(file_id) < 0) TEST_ERROR;

    PASSED();
    return 0;

error:
    /* Close everything we can and exit */
    H5E_BEGIN_TRY {
      H5Dclose(dataset_id);
      H5Sclose(dataspace_id);
      H5Pclose(fcpl);
      H5Pclose(fapl);
      H5Fclose(file_id);
    } H5E_END_TRY
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:	rsrv_ohdr
 *
 * Purpose:	Ensure that object headers reserve file address space.
 *			This function does this by creating attributes of a dataset
 *			past the limit of the file, then ensuring that an error (not
 *			an assert) was generated and that the file is readable.
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	James Laird
 *              Nat Furrer
 *              Friday, May 28, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
rsrv_ohdr(void)
{
    hid_t       file_id=(-1), dataset_id=(-1), dataspace_id=(-1);
    hid_t       fapl=(-1), fcpl=(-1), aid, attr_id;
    hsize_t     dims[2];
    herr_t      status;
    int         attrval[4][6];
    char        filename[1024], attrname[20];
    int         i;

    TESTING("Reserving file space for object headers");

    /* Create a new file */
    fapl = h5_fileaccess();
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);

    fcpl = H5Pcreate(H5P_FILE_CREATE);
    if(fcpl < 0) TEST_ERROR;
    if(H5Pset_sizes(fcpl, (size_t)2,(size_t)2) < 0) TEST_ERROR;

    file_id = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl);
    if(file_id < 0) TEST_ERROR;

    /* Create the data space for the dataset. */
    dims[0] = 4;
    dims[1] = 6;
    dataspace_id = H5Screate_simple(2, dims, NULL);
    if(dataspace_id < 0) TEST_ERROR;

    /* Create the dataset. */
    dataset_id = H5Dcreate2(file_id, "/dset", H5T_STD_I32BE, dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if(dataset_id < 0) TEST_ERROR;

    for(i = 0; i < 6; i++) {
        attrval[0][i] = 0;
        attrval[1][i] = 1;
        attrval[2][i] = 2;
        attrval[3][i] = 3;
    } /* end for */

    for(i = 0; i < 2000; i++) {
        sprintf(attrname, "attr %d", i);
        H5E_BEGIN_TRY{
            aid =  H5Screate_simple(2, dims, NULL);
            attr_id = H5Acreate2(dataset_id, attrname, H5T_STD_I32BE, aid, H5P_DEFAULT, H5P_DEFAULT);
            H5Awrite(attr_id, H5T_NATIVE_INT, attrval);
            status = H5Aclose(attr_id);
        } H5E_END_TRY

        if(status < 0)
            break;
    } /* end for */

    /* The loop should have broken before completing--the file should not have had
     * enough address space to hold 2000 attributes (or this test needs to be updated
!).
     */
    if(i == 2000)
        TEST_ERROR;

    /* End access to the dataset and dataspace and release resources. */
    if(H5Dclose(dataset_id) < 0) TEST_ERROR;
    if(H5Pclose(fapl) < 0) TEST_ERROR;
    if(H5Pclose(fcpl) < 0) TEST_ERROR;
    if(H5Sclose(dataspace_id) < 0) TEST_ERROR;

    /* Close the file and the library. */
    if(H5Fclose(file_id) < 0) TEST_ERROR;
    if(H5close() < 0) TEST_ERROR;

    /* Re-open the library and try to read a dataset from the file we created */
    if(H5open() < 0) TEST_ERROR;

    file_id = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT);
    if(file_id < 0) TEST_ERROR;

    dataset_id = H5Dopen2(file_id, "/dset", H5P_DEFAULT);

    /* If we can read the dataset from the file, the file has been flushed to disk
     * (if the heap or object headers weren't flushed, the file would be empty).
     */
    if(dataset_id == H5I_BADID)
        TEST_ERROR;

    if(H5Dclose(dataset_id) < 0) TEST_ERROR;
    if(H5Fclose(file_id) < 0) TEST_ERROR;

    PASSED();
    return 0;

error:
    /* Close everything we can and exit */
    H5E_BEGIN_TRY {
      H5Dclose(dataset_id);
      H5Sclose(dataspace_id);
      H5Pclose(fcpl);
      H5Pclose(fapl);
      H5Fclose(file_id);
    } H5E_END_TRY
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:	rsrv_vlen
 *
 * Purpose:	Ensure that variable length datatypes properly ensure that
 *              enough file address space exists before writing.
 *		This function does this by creating a dataset containing
 *              variable length data past the limit of the file, then
 *              ensuring that an error (not an assert) was generated and
 *              that the file is readable.
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	James Laird
 *		Nat Furrer
 *              Thursday, July 1, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
rsrv_vlen(void)
{
    hid_t       file_id=(-1), dataset_id=(-1), dataspace_id=(-1), type_id=(-1);
    hid_t       fapl=(-1), fcpl=(-1), mem_space_id=(-1);
    hssize_t    offset[1];
    hsize_t     start[1];
    hsize_t     dims[1], count[1];
    herr_t      status;
    int         i;
    int         write_buf[20];
    char        filename[1024];
    hvl_t       vlen_data;

    TESTING("Reserved space with variable length data");

    /* Create a new file */
    fapl = h5_fileaccess();
    h5_fixname(FILENAME[2], fapl, filename, sizeof filename);

    /* Make file address space very small */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    if(fcpl < 0) TEST_ERROR;
    if(H5Pset_sizes(fcpl, (size_t)2,(size_t)2) < 0) TEST_ERROR;

    file_id = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl);
    if(file_id < 0) TEST_ERROR;

    /* Create the data space for the dataset. */
    dims[0] = 2000;
    dataspace_id = H5Screate_simple(1, dims, NULL);
    if(dataspace_id < 0) TEST_ERROR;

    /* Create a variable length type */
    type_id = H5Tvlen_create(H5T_NATIVE_INT);
    if(type_id < 0) TEST_ERROR;

    /* Create the dataset. */
    dataset_id = H5Dcreate2(file_id, "/dset", type_id, dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if(dataset_id < 0) TEST_ERROR;

    /* Create some data to write */
    for(i = 0; i < 20; i++)
        write_buf[i] = i + 1;
    vlen_data.p = write_buf;

    /* Create a memory dataspace for writing */
    dims[0] = 1;
    mem_space_id = H5Screate_simple(1, dims, NULL);
    if(mem_space_id < 0) TEST_ERROR;

    /* Create a selection to write to */
    start[0] = 0;
    count[0] = 1;
    if(H5Sselect_hyperslab(dataspace_id, H5S_SELECT_SET, start, NULL, count, NULL)  < 0) TEST_ERROR;

    for(i = 0; i< 2000; i++) {
        vlen_data.len = (i%20) + 1;

        offset[0] = i;
        if( H5Soffset_simple(dataspace_id, offset) <0) TEST_ERROR;

        H5E_BEGIN_TRY
            status = H5Dwrite(dataset_id, type_id, mem_space_id, dataspace_id, H5P_DEFAULT, &vlen_data);
        H5E_END_TRY

        if(status < 0)
            break;
    } /* end for */

    /* The loop should have broken before completing--the file should not have had
     * enough address space to hold 2000 attributes (or this test needs to be updated!).
     */
    if(i == 2000)
        TEST_ERROR;

    /* End access to the dataset and dataspace and release resources. */
    if(H5Dclose(dataset_id) < 0) TEST_ERROR;
    if(H5Pclose(fcpl) < 0) TEST_ERROR;
    if(H5Pclose(fapl) < 0) TEST_ERROR;
    if(H5Sclose(dataspace_id) < 0) TEST_ERROR;
    if(H5Tclose(type_id) < 0) TEST_ERROR;
    if(H5Sclose(mem_space_id) < 0) TEST_ERROR;

    /* Close the file and the library. */
    if(H5Fclose(file_id) < 0) TEST_ERROR;
    if(H5close() < 0) TEST_ERROR;

    /* Re-open the library and try to read a dataset from the file we created */
    if(H5open() < 0) TEST_ERROR;

    file_id = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT);
    if(file_id < 0) TEST_ERROR;

    dataset_id = H5Dopen2(file_id, "/dset", H5P_DEFAULT);

    /* If we can read the dataset from the file, the file has been flushed to disk
     * (if the heap or object headers weren't flushed, the file would be empty).
     */
    if(dataset_id == H5I_BADID)
        TEST_ERROR;

    if(H5Dclose(dataset_id) < 0) TEST_ERROR;
    if(H5Fclose(file_id) < 0) TEST_ERROR;

    PASSED();
    return 0;

error:
    /* Close everything we can and exit */
    H5E_BEGIN_TRY {
      H5Dclose(dataset_id);
      H5Sclose(dataspace_id);
      H5Sclose(mem_space_id);
      H5Tclose(type_id);
      H5Pclose(fcpl);
      H5Pclose(fapl);
      H5Fclose(file_id);
    } H5E_END_TRY
    return 1;
}
#endif /* BROKEN */

/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:
 *
 * Return:	Success:
 *
 *		Failure:
 *
 * Programmer:	Nat Furrer and James Laird
 *              Thursday, July 1, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    /* This test is currently not working properly; it should be revisted
     * when we have time.
     *
     * (Also, we should try to make this test work with all the VFDs)
     */
#ifdef BROKEN
    int num_errs=0;
    hid_t fapl;
    const char *envval = NULL;

    envval = HDgetenv("HDF5_DRIVER");
    if (envval == NULL)
        envval = "nomatch";
/* QAK: should be able to use the core driver? */
    if (HDstrcmp(envval, "core") && HDstrcmp(envval, "split") && HDstrcmp(envval, "multi") && HDstrcmp(envval, "family")) {
	num_errs+=rsrv_ohdr();
	num_errs+=rsrv_heap();
	num_errs+=rsrv_vlen();

	if(num_errs > 0)
	    printf("**** %d FAILURE%s! ****\n", num_errs, num_errs==1?"":"S");
	else
	    puts("All address space reservation tests passed.");

	fapl = h5_fileaccess();
	h5_cleanup(FILENAME, fapl);
	return num_errs;
    }
    else
    {
        puts("All address space reservation tests skippped - Incompatible with current Virtual File Driver");
    }
#endif /* BROKEN */

    SKIPPED();
    return 0;

}

