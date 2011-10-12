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
* Test program:	 tcoords
*
* Test the element coordinates for dataspace selection.  For
* chunked dataset, when the hyperslab selection of some
* dimensions is full, the library optimize it by "flattenning"
* the fully selected dimensions.  This program tests if the
* coordinates of selected elements are correctly calculated.
*
*************************************************************/

#include "testhdf5.h"

#define FILENAME   "coord.h5"

#define SINGLE_END_DSET	                "single_end_dset"
#define MULTI_ENDS_SEL_HYPER_DSET	"multiple_ends_dset"

#define NAME_LEN        128

/* Data written to the dataset for single block test.  Global variable
 * for convenience. */
int da_buffer[2][3][6][2];

/***********************************************************
**
** test_singleEnd_selElements(): Test element selection of only
** one block.
**
*************************************************************/
static void test_singleEnd_selElements(hid_t file, hbool_t is_chunked)
{
    hid_t sid, plid, did, msid;
    char dset_name[NAME_LEN];        /* Dataset name */
    size_t elmts_numb;
    herr_t ret;                 /* Generic error return */
    int i, j, k;
    hsize_t da_dims[4] = { 2, 3, 6, 2 };
    hsize_t da_chunksize[4] = { 1, 3, 3, 2 };

    /* For testing the full selection in the fastest-growing end */
    int mem1_buffer[1][1][6][2];
    hsize_t mem1_dims[4] = { 1, 1, 6, 2 };
    hsize_t da_elmts1[12][4] = { {0, 0, 0, 0},
                                 {0, 0, 0, 1},
                                 {0, 0, 1, 0},
                                 {0, 0, 1, 1},
                                 {0, 0, 2, 0},
                                 {0, 0, 2, 1},
                                 {0, 0, 3, 0},
                                 {0, 0, 3, 1},
                                 {0, 0, 4, 0},
                                 {0, 0, 4, 1},
                                 {0, 0, 5, 0},
                                 {0, 0, 5, 1} };

    /* For testing the full selection in the slowest-growing end */
    int mem2_buffer[2][3][1][1];
    hsize_t mem2_dims[4] = { 2, 3, 1, 1 };
    hsize_t da_elmts2[6][4] = { {0, 0, 0, 0},
                                {0, 1, 0, 0},
                                {0, 2, 0, 0},
                                {1, 0, 0, 0},
                                {1, 1, 0, 0},
                                {1, 2, 0, 0} };

    /* For testing the full selection in the middle dimensions */
    int mem3_buffer[1][3][6][1];
    hsize_t mem3_dims[4] = { 1, 3, 6, 1 };
    hsize_t da_elmts3[18][4] = { {0, 0, 0, 0},
                                 {0, 0, 1, 0},
                                 {0, 0, 2, 0},
                                 {0, 0, 3, 0},
                                 {0, 0, 4, 0},
                                 {0, 0, 5, 0},
                                 {0, 1, 0, 0},
                                 {0, 1, 1, 0},
                                 {0, 1, 2, 0},
                                 {0, 1, 3, 0},
                                 {0, 1, 4, 0},
                                 {0, 1, 5, 0},
                                 {0, 2, 0, 0},
                                 {0, 2, 1, 0},
                                 {0, 2, 2, 0},
                                 {0, 2, 3, 0},
                                 {0, 2, 4, 0},
                                 {0, 2, 5, 0} };

    /* Create and write the dataset */
    sid = H5Screate_simple(4, da_dims, da_dims);
    CHECK(sid, FAIL, "H5Screate_simple");

    plid = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(plid, FAIL, "H5Pcreate");

    if(is_chunked) {
        ret = H5Pset_chunk(plid, 4, da_chunksize);
        CHECK(ret, FAIL, "H5Pset_chunk");
    }

    /* Construct dataset's name */
    memset(dset_name, 0, (size_t)NAME_LEN);
    strcat(dset_name, SINGLE_END_DSET);
    if(is_chunked)
        strcat(dset_name, "_chunked");

    did = H5Dcreate2(file, dset_name, H5T_NATIVE_INT, sid, H5P_DEFAULT, plid, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dcreate2");

    /* Initialize the data to be written to file */
    for(i=0; i<2; i++) {
        for(j=0; j<3; j++) {
            for(k=0; k<6; k++) {
                da_buffer[i][j][k][0] = i*100 + j*10 + k;
                da_buffer[i][j][k][1] = i*100 + j*10 + k + 1;
            }
        }
    }

    ret = H5Dwrite(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, da_buffer);
    CHECK(ret, FAIL, "H5Dwrite");

    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");


    /* ****** Case 1: ******
     * Testing the full selection in the fastest-growing end */
    did = H5Dopen2(file, dset_name, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dopen");

    /* Select the elements in the dataset */
    elmts_numb = 12;

    ret = H5Sselect_elements(sid, H5S_SELECT_SET, elmts_numb, (const hsize_t *)da_elmts1);
    CHECK(ret, FAIL, "H5Sselect_elements");

    /* Dataspace for memory buffer */
    msid = H5Screate_simple(4, mem1_dims, mem1_dims);
    CHECK(msid, FAIL, "H5Screate_simple");

    ret = H5Sselect_all(msid);
    CHECK(ret, FAIL, "H5Sselect_all");

    ret = H5Dread(did, H5T_NATIVE_INT, msid, sid, H5P_DEFAULT, mem1_buffer);
    CHECK(ret, FAIL, "H5Dread");

    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    ret = H5Sclose(msid);
    CHECK(ret, FAIL, "H5Sclose");

    for(i=0; i<6; i++)
        for(j=0; j<2; j++)
            if(da_buffer[0][0][i][j] != mem1_buffer[0][0][i][j]) {
                TestErrPrintf("%u: Read different values than written at index 0,0,%d,%d\n", __LINE__, i, j);
            }

    /* ****** Case 2: ******
     * Testing the full selection in the slowest-growing end */
    did = H5Dopen2(file, dset_name, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dopen");

    /* Select the elements in the dataset */
    elmts_numb = 6;

    ret = H5Sselect_elements(sid, H5S_SELECT_SET, elmts_numb, (const hsize_t *)da_elmts2);
    CHECK(ret, FAIL, "H5Sselect_elements");

    /* Dataspace for memory buffer */
    msid = H5Screate_simple(4, mem2_dims, mem2_dims);
    CHECK(msid, FAIL, "H5Screate_simple");

    ret = H5Sselect_all(msid);
    CHECK(ret, FAIL, "H5Sselect_all");

    ret = H5Dread(did, H5T_NATIVE_INT, msid, sid, H5P_DEFAULT, mem2_buffer);
    CHECK(ret, FAIL, "H5Dread");

    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    ret = H5Sclose(msid);
    CHECK(ret, FAIL, "H5Sclose");

    for(i=0; i<2; i++)
        for(j=0; j<3; j++)
            if(da_buffer[i][j][0][0] != mem2_buffer[i][j][0][0]) {
                TestErrPrintf("%u: Read different values than written at index %d,%d,0,0, da_buffer = %d, mem2_buffer = %d\n", __LINE__, i, j, da_buffer[i][j][0][0], mem2_buffer[i][j][0][0]);
            }

    /* ****** Case 3: ******
     * Testing the full selection in the middle dimensions */
    did = H5Dopen2(file, dset_name, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dopen");

    /* Select the elements in the dataset */
    elmts_numb = 18;

    ret = H5Sselect_elements(sid, H5S_SELECT_SET, elmts_numb, (const hsize_t *)da_elmts3);
    CHECK(ret, FAIL, "H5Sselect_elements");

    /* Dataspace for memory buffer */
    msid = H5Screate_simple(4, mem3_dims, mem3_dims);
    CHECK(msid, FAIL, "H5Screate_simple");

    ret = H5Sselect_all(msid);
    CHECK(ret, FAIL, "H5Sselect_all");

    ret = H5Dread(did, H5T_NATIVE_INT, msid, sid, H5P_DEFAULT, mem3_buffer);
    CHECK(ret, FAIL, "H5Dread");

    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    ret = H5Sclose(msid);
    CHECK(ret, FAIL, "H5Sclose");

    for(i=0; i<3; i++)
        for(j=0; j<6; j++)
            if(da_buffer[0][i][j][0] != mem3_buffer[0][i][j][0]) {
                TestErrPrintf("%u: Read different values than written at index 0,%d,%d,0\n", __LINE__, i, j);
            }


    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Pclose(plid);
    CHECK(ret, FAIL, "H5Pclose");
}

/***********************************************************
**
** test_singleEnd_selHyperslab(): Test full hyperslab selection
** of only one block.
**
*************************************************************/
static void test_singleEnd_selHyperslab(hid_t file, hbool_t is_chunked)
{
    hid_t sid, did, msid;
    char dset_name[NAME_LEN];        /* Dataset name */
    herr_t ret;                 /* Generic error return */
    int i, j;
    hsize_t da_dims[4] = { 2, 3, 6, 2 };

    /* For testing the full selection in the fastest-growing end */
    int mem1_buffer[1][1][6][2];
    hsize_t mem1_dims[4] = { 1, 1, 6, 2 };
    hsize_t mem1_start[4] = { 0, 0, 0, 0 };
    hsize_t mem1_count[4] = { 1, 1, 1, 1 };
    hsize_t mem1_stride[4] = { 1, 1, 1, 1 };
    hsize_t mem1_block[4] = { 1, 1, 6, 2 };

    /* For testing the full selection in the slowest-growing end */
    int mem2_buffer[2][3][1][1];
    hsize_t mem2_dims[4] = { 2, 3, 1, 1 };
    hsize_t mem2_start[4] = { 0, 0, 0, 0 };
    hsize_t mem2_count[4] = { 1, 1, 1, 1 };
    hsize_t mem2_stride[4] = { 1, 1, 1, 1 };
    hsize_t mem2_block[4] = { 2, 3, 1, 1 };

    /* For testing the full selection in the middle dimensions */
    int mem3_buffer[1][3][6][1];
    hsize_t mem3_dims[4] = { 1, 3, 6, 1 };
    hsize_t mem3_start[4] = { 0, 0, 0, 0 };
    hsize_t mem3_count[4] = { 1, 1, 1, 1 };
    hsize_t mem3_stride[4] = { 1, 1, 1, 1 };
    hsize_t mem3_block[4] = { 1, 3, 6, 1 };

    /* Construct dataset's name */
    memset(dset_name, 0, NAME_LEN);
    strcat(dset_name, SINGLE_END_DSET);
    if(is_chunked)
        strcat(dset_name, "_chunked");

    /* Dataspace for the dataset in file */
    sid = H5Screate_simple(4, da_dims, da_dims);
    CHECK(sid, FAIL, "H5Screate_simple");

    /* ****** Case 1: ******
     * Testing the full selection in the fastest-growing end */
    did = H5Dopen2(file, dset_name, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dopen");

    /* Select the elements in the dataset */
    ret = H5Sselect_hyperslab(sid, H5S_SELECT_SET, mem1_start, mem1_stride, mem1_count, mem1_block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Dataspace for memory buffer */
    msid = H5Screate_simple(4, mem1_dims, mem1_dims);
    CHECK(msid, FAIL, "H5Screate_simple");

    ret = H5Sselect_all(msid);
    CHECK(ret, FAIL, "H5Sselect_all");

    ret = H5Dread(did, H5T_NATIVE_INT, msid, sid, H5P_DEFAULT, mem1_buffer);
    CHECK(ret, FAIL, "H5Dread");

    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    ret = H5Sclose(msid);
    CHECK(ret, FAIL, "H5Sclose");

    for(i=0; i<6; i++)
        for(j=0; j<2; j++)
            if(da_buffer[0][0][i][j] != mem1_buffer[0][0][i][j]) {
                TestErrPrintf("%u: Read different values than written at index 0,0,%d,%d\n", __LINE__, i, j);
            }

    /* ****** Case 2: ******
     * Testing the full selection in the slowest-growing end */
    did = H5Dopen2(file, dset_name, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dopen");

    /* Select the elements in the dataset */
    ret = H5Sselect_hyperslab(sid, H5S_SELECT_SET, mem2_start, mem2_stride, mem2_count, mem2_block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Dataspace for memory buffer */
    msid = H5Screate_simple(4, mem2_dims, mem2_dims);
    CHECK(msid, FAIL, "H5Screate_simple");

    ret = H5Sselect_all(msid);
    CHECK(ret, FAIL, "H5Sselect_all");

    ret = H5Dread(did, H5T_NATIVE_INT, msid, sid, H5P_DEFAULT, mem2_buffer);
    CHECK(ret, FAIL, "H5Dread");

    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    ret = H5Sclose(msid);
    CHECK(ret, FAIL, "H5Sclose");

    for(i=0; i<2; i++)
        for(j=0; j<3; j++)
            if(da_buffer[i][j][0][0] != mem2_buffer[i][j][0][0]) {
                TestErrPrintf("%u: Read different values than written at index %d,%d,0,0\n", __LINE__, i, j);
            }

    /* ****** Case 3: ******
     * Testing the full selection in the middle dimensions */
    did = H5Dopen2(file, dset_name, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dopen");

    /* Select the elements in the dataset */
    ret = H5Sselect_hyperslab(sid, H5S_SELECT_SET, mem3_start, mem3_stride, mem3_count, mem3_block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Dataspace for memory buffer */
    msid = H5Screate_simple(4, mem3_dims, mem3_dims);
    CHECK(msid, FAIL, "H5Screate_simple");

    ret = H5Sselect_all(msid);
    CHECK(ret, FAIL, "H5Sselect_all");

    ret = H5Dread(did, H5T_NATIVE_INT, msid, sid, H5P_DEFAULT, mem3_buffer);
    CHECK(ret, FAIL, "H5Dread");

    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    ret = H5Sclose(msid);
    CHECK(ret, FAIL, "H5Sclose");

    for(i=0; i<3; i++)
        for(j=0; j<6; j++)
            if(da_buffer[0][i][j][0] != mem3_buffer[0][i][j][0]) {
                TestErrPrintf("%u: Read different values than written at index 0,%d,%d,0\n", __LINE__, i, j);
            }


    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
}



/***********************************************************
**
** test_multiple_end(): Test full hyperslab selection of
** multiple blocks.
**
*************************************************************/
static void test_multiple_ends(hid_t file, hbool_t is_chunked)
{
    hid_t sid, plid, did, msid;
    char dset_name[NAME_LEN];        /* Dataset name */
    herr_t ret;                 /* Generic error return */
    int i, j, k, l, m, n, p;
    hsize_t da_dims[8] = { 4, 5, 3, 4, 2, 3, 6, 2 };
    hsize_t da_chunksize[8] = { 1, 5, 3, 2, 2, 3, 3, 2 };
    int data_buf[4][5][3][4][2][3][6][2];

    /* For testing the full selections in the fastest-growing end and in the middle dimensions */
    int mem1_buffer[1][1][1][4][2][1][6][2];
    hsize_t mem1_dims[8] = { 1, 1, 1, 4, 2, 1, 6, 2 };
    hsize_t mem1_start[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };
    hsize_t mem1_count[8] = { 1, 1, 1, 1, 1, 1, 1, 1 };
    hsize_t mem1_stride[8] = { 1, 1, 1, 1, 1, 1, 1, 1 };
    hsize_t mem1_block[8] = { 1, 1, 1, 4, 2, 1, 6, 2 };

    /* For testing the full selections in the slowest-growing end and in the middle dimensions */
    int mem2_buffer[4][5][1][4][2][1][1][1];
    hsize_t mem2_dims[8] = { 4, 5, 1, 4, 2, 1, 1, 1 };
    hsize_t mem2_start[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };
    hsize_t mem2_count[8] = { 1, 1, 1, 1, 1, 1, 1, 1 };
    hsize_t mem2_stride[8] = { 1, 1, 1, 1, 1, 1, 1, 1 };
    hsize_t mem2_block[8] = { 4, 5, 1, 4, 2, 1, 1, 1 };

    /* For testing two unadjacent full selections in the middle dimensions */
    int mem3_buffer[1][5][3][1][1][3][6][1];
    hsize_t mem3_dims[8] = { 1, 5, 3, 1, 1, 3, 6, 1 };
    hsize_t mem3_start[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };
    hsize_t mem3_count[8] = { 1, 1, 1, 1, 1, 1, 1, 1 };
    hsize_t mem3_stride[8] = { 1, 1, 1, 1, 1, 1, 1, 1 };
    hsize_t mem3_block[8] = { 1, 5, 3, 1, 1, 3, 6, 1 };

    /* For testing the full selections in the fastest-growing end and the slowest-growing end */
    int mem4_buffer[4][5][1][1][1][1][6][2];
    hsize_t mem4_dims[8] = { 4, 5, 1, 1, 1, 1, 6, 2 };
    hsize_t mem4_start[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };
    hsize_t mem4_count[8] = { 1, 1, 1, 1, 1, 1, 1, 1 };
    hsize_t mem4_stride[8] = { 1, 1, 1, 1, 1, 1, 1, 1 };
    hsize_t mem4_block[8] = { 4, 5, 1, 1, 1, 1, 6, 2 };

    /* For testing the full selections in the fastest-growing end and slowest-growing end,
     * also in the middle dimensions */
    int mem5_buffer[4][5][1][4][2][1][6][2];
    hsize_t mem5_dims[8] = { 4, 5, 1, 4, 2, 1, 6, 2 };
    hsize_t mem5_start[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };
    hsize_t mem5_count[8] = { 1, 1, 1, 1, 1, 1, 1, 1 };
    hsize_t mem5_stride[8] = { 1, 1, 1, 1, 1, 1, 1, 1 };
    hsize_t mem5_block[8] = { 4, 5, 1, 4, 2, 1, 6, 2 };

    /* Create and write the dataset */
    sid = H5Screate_simple(8, da_dims, da_dims);
    CHECK(sid, FAIL, "H5Screate_simple");

    plid = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(plid, FAIL, "H5Pcreate");

    if(is_chunked) {
        ret = H5Pset_chunk(plid, 8, da_chunksize);
        CHECK(ret, FAIL, "H5Pset_chunk");
    }

    /* Construct dataset's name */
    memset(dset_name, 0, NAME_LEN);
    strcat(dset_name, MULTI_ENDS_SEL_HYPER_DSET);
    if(is_chunked)
        strcat(dset_name, "_chunked");

    did = H5Dcreate2(file, dset_name, H5T_NATIVE_INT, sid, H5P_DEFAULT, plid, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dcreate2");

    for(i=0; i<4; i++)
        for(j=0; j<5; j++)
            for(k=0; k<3; k++)
                for(l=0; l<4; l++)
                    for(m=0; m<2; m++)
                        for(n=0; n<3; n++)
                            for(p=0; p<6; p++) {
                                data_buf[i][j][k][l][m][n][p][0] = i*1000000 + j*100000 + k*10000 + l*1000 + m*100 + n*10 + p;
                                data_buf[i][j][k][l][m][n][p][1] = i*1000000 + j*100000 + k*10000 + l*1000 + m*100 + n*10 + p + 1;
                            }

    ret = H5Dwrite(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, data_buf);
    CHECK(ret, FAIL, "H5Dwrite");

    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    /* ****** Case 1: ******
     * Testing the full selections in the fastest-growing end and in the middle dimensions*/
    did = H5Dopen2(file, dset_name, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dopen");

    /* Select the elements in the dataset */
    ret = H5Sselect_hyperslab(sid, H5S_SELECT_SET, mem1_start, mem1_stride, mem1_count, mem1_block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    msid = H5Screate_simple(8, mem1_dims, mem1_dims);
    CHECK(msid, FAIL, "H5Screate_simple");

    ret = H5Sselect_all(msid);
    CHECK(ret, FAIL, "H5Sselect_all");

    ret = H5Dread(did, H5T_NATIVE_INT, msid, sid, H5P_DEFAULT, mem1_buffer);
    CHECK(ret, FAIL, "H5Dread");

    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    ret = H5Sclose(msid);
    CHECK(ret, FAIL, "H5Sclose");

    for(i=0; i<4; i++)
        for(j=0; j<2; j++)
            for(k=0; k<6; k++)
                for(l=0; l<2; l++)
                    if(data_buf[0][0][0][i][j][0][k][l] != mem1_buffer[0][0][0][i][j][0][k][l]) {
                        TestErrPrintf("%u: Read different values than written at index 0,0,0,%d,%d,0,%d,%d\n", __LINE__, i, j, k, l);
                    }

    /* ****** Case 2: ******
     * Testing the full selections in the slowest-growing end and in the middle dimensions*/
    did = H5Dopen2(file, dset_name, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dopen");

    /* Select the elements in the dataset */
    ret = H5Sselect_hyperslab(sid, H5S_SELECT_SET, mem2_start, mem2_stride, mem2_count, mem2_block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    msid = H5Screate_simple(8, mem2_dims, mem2_dims);
    CHECK(msid, FAIL, "H5Screate_simple");

    ret = H5Sselect_all(msid);
    CHECK(ret, FAIL, "H5Sselect_all");

    ret = H5Dread(did, H5T_NATIVE_INT, msid, sid, H5P_DEFAULT, mem2_buffer);
    CHECK(ret, FAIL, "H5Dread");

    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    ret = H5Sclose(msid);
    CHECK(ret, FAIL, "H5Sclose");

    for(i=0; i<4; i++)
        for(j=0; j<5; j++)
            for(k=0; k<4; k++)
                for(l=0; l<2; l++)
                    if(data_buf[i][j][0][k][l][0][0][0] != mem2_buffer[i][j][0][k][l][0][0][0]) {
                        TestErrPrintf("%u: Read different values than written at index %d,%d,0,%d,%d,0,0,0\n", __LINE__, i, j, k, l);
                    }

    /* ****** Case 3: ******
     * Testing two unadjacent full selections in the middle dimensions */
    did = H5Dopen2(file, dset_name, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dopen");

    /* Select the elements in the dataset */
    ret = H5Sselect_hyperslab(sid, H5S_SELECT_SET, mem3_start, mem3_stride, mem3_count, mem3_block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    msid = H5Screate_simple(8, mem3_dims, mem3_dims);
    CHECK(msid, FAIL, "H5Screate_simple");

    ret = H5Sselect_all(msid);
    CHECK(ret, FAIL, "H5Sselect_all");

    ret = H5Dread(did, H5T_NATIVE_INT, msid, sid, H5P_DEFAULT, mem3_buffer);
    CHECK(ret, FAIL, "H5Dread");

    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    ret = H5Sclose(msid);
    CHECK(ret, FAIL, "H5Sclose");

    for(i=0; i<5; i++)
        for(j=0; j<3; j++)
            for(k=0; k<3; k++)
                for(l=0; l<6; l++)
                    if(data_buf[0][i][j][0][0][k][l][0] != mem3_buffer[0][i][j][0][0][k][l][0]) {
                        TestErrPrintf("%u: Read different values than written at index 0,%d,%d,0,0,%d,%d,0\n", __LINE__, i, j, k, l);
                    }

    /* ****** Case 4: ******
     * Testing the full selections in the fastest-growing end and the slowest-growing end */
    did = H5Dopen2(file, dset_name, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dopen");

    /* Select the elements in the dataset */
    ret = H5Sselect_hyperslab(sid, H5S_SELECT_SET, mem4_start, mem4_stride, mem4_count, mem4_block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    msid = H5Screate_simple(8, mem4_dims, mem4_dims);
    CHECK(msid, FAIL, "H5Screate_simple");

    ret = H5Sselect_all(msid);
    CHECK(ret, FAIL, "H5Sselect_all");

    ret = H5Dread(did, H5T_NATIVE_INT, msid, sid, H5P_DEFAULT, mem4_buffer);
    CHECK(ret, FAIL, "H5Dread");

    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    ret = H5Sclose(msid);
    CHECK(ret, FAIL, "H5Sclose");

    for(i=0; i<4; i++)
        for(j=0; j<5; j++)
            for(k=0; k<6; k++)
                for(l=0; l<2; l++)
                    if(data_buf[i][j][0][0][0][0][k][l] != mem4_buffer[i][j][0][0][0][0][k][l]) {
                        TestErrPrintf("%u: Read different values than written at index %d,%d,0,0,0,0,%d,%d\n", __LINE__, i, j, k, l);
                    }


    /* ****** Case 5: ******
     * Testing the full selections in the fastest-growing end and the slowest-growing end,
     * and also in the middle dimensions */
    did = H5Dopen2(file, dset_name, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dopen");

    /* Select the elements in the dataset */
    ret = H5Sselect_hyperslab(sid, H5S_SELECT_SET, mem5_start, mem5_stride, mem5_count, mem5_block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    msid = H5Screate_simple(8, mem5_dims, mem5_dims);
    CHECK(msid, FAIL, "H5Screate_simple");

    ret = H5Sselect_all(msid);
    CHECK(ret, FAIL, "H5Sselect_all");

    ret = H5Dread(did, H5T_NATIVE_INT, msid, sid, H5P_DEFAULT, mem5_buffer);
    CHECK(ret, FAIL, "H5Dread");

    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    ret = H5Sclose(msid);
    CHECK(ret, FAIL, "H5Sclose");

    for(i=0; i<4; i++)
        for(j=0; j<5; j++)
            for(k=0; k<4; k++)
                for(l=0; l<2; l++)
                    for(m=0; m<6; m++)
                        for(n=0; n<2; n++)
                            if(data_buf[i][j][0][k][l][0][m][n] != mem5_buffer[i][j][0][k][l][0][m][n]) {
                                TestErrPrintf("%u: Read different values than written at index %d,%d,0,%d,%d,0,%d,%d\n", __LINE__, i, j, k, l, m, n);
                            }


    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Pclose(plid);
    CHECK(ret, FAIL, "H5Pclose");
}


/****************************************************************
**
**  test_coords(): Main testing routine.
**
****************************************************************/
void test_coords(void)
{
    hid_t   fid;
    int     i;
    hbool_t is_chunk;
    herr_t  ret;                 /* Generic error return */

    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    for(i=0, is_chunk=FALSE; i<2; i++, is_chunk++) {
        /* Test H5Sselect_elements with selection of one block of data */
        test_singleEnd_selElements(fid, is_chunk);

        /* Test H5Sselect_hyperslab with selection of one block of data */
        test_singleEnd_selHyperslab(fid, is_chunk);

        /* Test H5Sselect_hyperslab with selection of multiple blocks of data */
        test_multiple_ends(fid, is_chunk);
    }

    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
}


/*-------------------------------------------------------------------------
 * Function:	cleanup_coords
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	Raymond Lu
 *              20 Dec. 2007
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_coords(void)
{
    remove(FILENAME);
}
