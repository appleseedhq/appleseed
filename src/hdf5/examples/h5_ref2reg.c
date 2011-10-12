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
    This program shows how to create, store and dereference references
    to the dataset regions.

    It creates a file and writes a two dimensional integer dataset
    to it. Then it creates a dataset to store region references in. It
    stores references to a hyperslab and 3 points selected (for the
    integer dataset previously created).

    It then reopens the references dataset, reads and dereferences the
    region references, and then reads and displays the selected hyperslab
    and selected elements data from the integer dataset.
*/

#include "hdf5.h"

#define filename "REF_REG.h5"
#define dsetnamev "MATRIX"
#define dsetnamer "REGION_REFERENCES"

int main(void)
{
    hid_t file_id;        /* file identifier */
    hid_t space_id;       /* dataspace identifiers */
    hid_t spacer_id;
    hid_t dsetv_id;       /*dataset identifiers*/
    hid_t dsetr_id;
    hsize_t dims[2] =  {2,9};
    hsize_t dimsr[1] =  {2};
    int rank = 2;
    int rankr =1;
    herr_t status;
    hdset_reg_ref_t ref[2];
    hdset_reg_ref_t ref_out[2];
    int data[2][9] = {{1,1,2,3,3,4,5,5,6},{1,2,2,3,4,4,5,6,6}};
    int data_out[2][9] = {{0,0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0,0}};
    hsize_t start[2];
    hsize_t count[2];
    hsize_t coord[2][3] = {{0, 0, 1}, {6, 0, 8}};
    unsigned num_points = 3;
    int i, j;
    size_t name_size1, name_size2;
    char buf1[10], buf2[10];

    /*
     * Create file with default file access and file creation properties.
     */
    file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create dataspace for datasets.
     */
    space_id = H5Screate_simple(rank, dims, NULL);
    spacer_id = H5Screate_simple(rankr, dimsr, NULL);

    /*
     * Create integer dataset.
     */
    dsetv_id = H5Dcreate2(file_id, dsetnamev, H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Write data to the dataset.
     */
    status = H5Dwrite(dsetv_id, H5T_NATIVE_INT, H5S_ALL , H5S_ALL, H5P_DEFAULT,data);
    status = H5Dclose(dsetv_id);

    /*
     * Dataset with references.
     */
    dsetr_id = H5Dcreate2(file_id, dsetnamer, H5T_STD_REF_DSETREG, spacer_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create a reference to the hyperslab.
     */
    start[0] = 0;
    start[1] = 3;
    count[0] = 2;
    count[1] = 3;
    status = H5Sselect_hyperslab(space_id, H5S_SELECT_SET, start, NULL, count, NULL);
    status = H5Rcreate(&ref[0], file_id, dsetnamev, H5R_DATASET_REGION, space_id);

    /*
     * Create a reference to elements selection.
     */
    status = H5Sselect_none(space_id);
    status = H5Sselect_elements(space_id, H5S_SELECT_SET, num_points, (const hsize_t *)coord);
    status = H5Rcreate(&ref[1], file_id, dsetnamev, H5R_DATASET_REGION, space_id);

    /*
     * Write dataset with the references.
     */
    status = H5Dwrite(dsetr_id, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT,ref);

    /*
     * Close all objects.
     */
    status = H5Sclose(space_id);
    status = H5Sclose(spacer_id);
    status = H5Dclose(dsetr_id);
    status = H5Fclose(file_id);

    /*
     * Reopen the file to read selections back.
     */
    file_id = H5Fopen(filename, H5F_ACC_RDWR,  H5P_DEFAULT);

    /*
     * Reopen the dataset with object references and read references
     * to the buffer.
     */
    dsetr_id = H5Dopen2(file_id, dsetnamer, H5P_DEFAULT);

    status = H5Dread(dsetr_id, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL,
                   H5P_DEFAULT, ref_out);

    /*
     * Dereference the first reference.
     */
    dsetv_id = H5Rdereference(dsetr_id, H5R_DATASET_REGION, &ref_out[0]);
    /*
     * Get name of the dataset the first region reference points to
     * using H5Rget_name
     */
    name_size1 = H5Rget_name(dsetr_id, H5R_DATASET_REGION, &ref_out[0], (char*)buf1, 10);
    printf(" Dataset's name (returned by H5Rget_name) the reference points to is %s, name length is %d\n", buf1, (int)name_size1);
    /*
     * Get name of the dataset the first region reference points to
     * using H5Iget_name
     */
    name_size2 = H5Iget_name(dsetv_id, (char*)buf2, 10);
    printf(" Dataset's name (returned by H5Iget_name) the reference points to is %s, name length is %d\n", buf2, (int)name_size2);

    space_id = H5Rget_region(dsetr_id, H5R_DATASET_REGION,&ref_out[0]);

    /*
     * Read and display hyperslab selection from the dataset.
     */

    status = H5Dread(dsetv_id, H5T_NATIVE_INT, H5S_ALL, space_id,
                   H5P_DEFAULT, data_out);
    printf("Selected hyperslab: ");
    for (i = 0; i <= 1; i++)
    {
        printf("\n");
        for (j = 0; j <= 8; j++)
            printf("%d ", data_out[i][j]);
    }
    printf("\n");

    /*
     * Close dataspace and the dataset.
     */
    status = H5Sclose(space_id);
    status = H5Dclose(dsetv_id);

    /*
     * Initialize data_out array again to get point selection.
     */
    for (i = 0; i <= 1; i++)
        for (j = 0; j <= 8; j++)
            data_out[i][j] = 0;

    /*
     * Dereference the second reference.
     */
    dsetv_id = H5Rdereference(dsetr_id, H5R_DATASET_REGION, &ref_out[1]);
    space_id = H5Rget_region(dsetv_id, H5R_DATASET_REGION,&ref_out[1]);

    /*
     * Read selected data from the dataset.
     */

    status = H5Dread(dsetv_id, H5T_NATIVE_INT, H5S_ALL, space_id,
                   H5P_DEFAULT, data_out);
    printf("Selected points: ");
    for (i = 0; i <= 1; i++)
    {
        printf("\n");
        for (j = 0; j <= 8; j++)
            printf("%d ", data_out[i][j]);
    }
    printf("\n");

    /*
     * Close dataspace and the dataset.
     */
    status = H5Sclose(space_id);
    status = H5Dclose(dsetv_id);
    status = H5Dclose(dsetr_id);
    status = H5Fclose(file_id);

    return 0;
}



