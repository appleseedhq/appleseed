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

#include <stdlib.h>
#include <string.h>
#include "h5hltest.h"
#include "H5srcdir.h"
#include "H5DSpublic.h"
#include "H5LTpublic.h"
#include "H5IMpublic.h"

/* operator functions */
static herr_t verify_scale(hid_t dset, unsigned dim, hid_t scale, void *visitor_data);
static herr_t read_scale(hid_t dset, unsigned dim, hid_t scale, void *visitor_data);
static herr_t match_dim_scale(hid_t did, unsigned dim, hid_t dsid, void *visitor_data);
static herr_t op_continue(hid_t did, unsigned dim, hid_t dsid, void *visitor_data);
static herr_t op_stop(hid_t did, unsigned dim, hid_t dsid, void *visitor_data);

/* prototypes */
static int create_test_file(const char *fileext);
static int open_test_file(const char *fileext);
herr_t create_char_dataset(hid_t fid, const char *dsidx, int fulldims);
herr_t create_short_dataset(hid_t fid, const char *dsidx, int fulldims);
herr_t create_int_dataset(hid_t fid, const char *dsidx, int fulldims);
herr_t create_long_dataset(hid_t fid, const char *dsname, const char *dsidx, int fulldims);
herr_t create_float_dataset(hid_t fid, const char *dsidx, int fulldims);
herr_t create_DS1_char_datasets(hid_t fid, const char *dsidx, int rankds, hsize_t *s_dim, char *s_wbuf, char *s1_wbuf);
herr_t create_DS2_char_datasets(hid_t fid, const char *dsidx, int rankds, hsize_t *s_dim, char *s_wbuf, char *s1_wbuf, char *s2_wbuf);
herr_t create_DS3_char_datasets(hid_t fid, const char *dsidx, int rankds, hsize_t *s_dim, char *s_wbuf, char *s1_wbuf, char *s2_wbuf, char *s3_wbuf);
herr_t create_DS1_short_datasets(hid_t fid, const char *dsidx, int rankds, hsize_t *s_dim, short *s_wbuf, short *s1_wbuf);
herr_t create_DS2_short_datasets(hid_t fid, const char *dsidx, int rankds, hsize_t *s_dim, short *s_wbuf, short *s1_wbuf, short *s2_wbuf);
herr_t create_DS3_short_datasets(hid_t fid, const char *dsidx, int rankds, hsize_t *s_dim, short *s_wbuf, short *s1_wbuf, short *s2_wbuf, short *s3_wbuf);
herr_t create_DS1_int_datasets(hid_t fid, const char *dsidx, int rankds, hsize_t *s_dim, int *s_wbuf, int *s1_wbuf);
herr_t create_DS2_int_datasets(hid_t fid, const char *dsidx, int rankds, hsize_t *s_dim, int *s_wbuf, int *s1_wbuf, int *s2_wbuf);
herr_t create_DS3_int_datasets(hid_t fid, const char *dsidx, int rankds, hsize_t *s_dim, int *s_wbuf, int *s1_wbuf, int *s2_wbuf, int *s3_wbuf);
herr_t create_DS1_long_datasets(hid_t fid, const char *dsidx, int rankds, hsize_t *s_dim, long *s_wbuf, long *s1_wbuf);
herr_t create_DS2_long_datasets(hid_t fid, const char *dsidx, int rankds, hsize_t *s_dim, long *s_wbuf, long *s1_wbuf, long *s2_wbuf);
herr_t create_DS3_long_datasets(hid_t fid, const char *dsidx, int rankds, hsize_t *s_dim, long *s_wbuf, long *s1_wbuf, long *s2_wbuf, long *s3_wbuf);
herr_t create_DS4_long_datasets(hid_t fid, const char *dsidx, int rankds, hsize_t *s_dim, long *s_wbuf, long *s1_wbuf, long *s2_wbuf, long *s3_wbuf, long *s4_wbuf);
herr_t create_DS1_float_datasets(hid_t fid, const char *dsidx, int rankds, hsize_t *s_dim, float *s_wbuf, float *s1_wbuf);
herr_t create_DS2_float_datasets(hid_t fid, const char *dsidx, int rankds, hsize_t *s_dim, float *s_wbuf, float *s1_wbuf, float *s2_wbuf);
herr_t create_DS3_float_datasets(hid_t fid, const char *dsidx, int rankds, hsize_t *s_dim, float *s_wbuf, float *s1_wbuf, float *s2_wbuf, float *s3_wbuf);
herr_t test_attach_scale(hid_t fid, hid_t did, const char *name, unsigned int idx);
herr_t test_detach_scale(hid_t fid, hid_t did, const char *name, unsigned int idx);
herr_t test_set_scalename(hid_t fid, hid_t did, const char *name, const char *scalename, unsigned int idx);
herr_t test_cmp_scalename(hid_t fid, hid_t did, const char *name, const char *scalename, unsigned int idx);

static int test_char_attachscales(const char *fileext);
static int test_short_attachscales(const char *fileext);
static int test_int_attachscales(const char *fileext);
static int test_long_attachscales(const char *fileext);
static int test_duplicatelong_attachscales(const char *fileext);
static int test_float_attachscales(const char *fileext);
static int test_numberofscales(const char *fileext);
static int test_char_scalenames(const char *fileext);
static int test_short_scalenames(const char *fileext);
static int test_int_scalenames(const char *fileext);
static int test_long_scalenames(const char *fileext);
static int test_samelong_scalenames(const char *fileext);
static int test_float_scalenames(const char *fileext);
static int test_foreign_scaleattached(const char *fileforeign);
static int test_detachscales(void);

static int test_simple(void);
static int test_errors(void);
static int test_errors2(void);
static int test_rank(void);
static int test_types(void);
static int test_iterators(void);
static int test_data(void);
static int read_data( const char* fname, int ndims, hsize_t *dims, float **buf );


#define RANK          2
#define DIM_DATA      12
#define DIM1_SIZE     3
#define DIM2_SIZE     4
#define DIM3_SIZE     12
#define DIM4_SIZE     2
#define DIM0          0
#define DIM1          1
#define DIM2          2
#define DIM3          3

#define DATASET_NAME   "dset_"
#define DS_1_NAME      "ds_1_"
#define DS_11_NAME     "ds_11_"
#define DS_2_NAME      "ds_2_"
#define DS_21_NAME     "ds_21_"
#define DS_22_NAME     "ds_22_"
#define DS_3_NAME      "ds_3_"
#define DS_31_NAME     "ds_31_"
#define DS_32_NAME     "ds_32_"
#define DS_33_NAME     "ds_33_"
#define DS_4_NAME      "ds_4_"
#define DS_41_NAME     "ds_41_"
#define DS_42_NAME     "ds_42_"
#define DS_43_NAME     "ds_43_"
#define DS_44_NAME     "ds_44_"

#define SCALE_1_NAME   "scalename_1_"
#define SCALE_11_NAME  "scalename_11_"
#define SCALE_2_NAME   "scalename_2_"
#define SCALE_21_NAME  "scalename_21_"
#define SCALE_22_NAME  "scalename_22_"
#define SCALE_3_NAME   "scalename_3_"
#define SCALE_31_NAME  "scalename_31_"
#define SCALE_32_NAME  "scalename_32_"
#define SCALE_33_NAME  "scalename_33_"
#define SCALE_4_NAME   "scalename_4_"

#define DIM0_LABEL     "Latitude"
#define DIM1_LABEL     "Longitude"

#define FOREIGN_FILE1       "test_ds_le.h5"
#define FOREIGN_FILE2       "test_ds_be.h5"
#define FILENAME       "test_ds"
#define FILEEXT        ".h5"

#define FILE1          "test_ds3.h5"
#define FILE2          "test_ds4.h5"
#define FILE3          "test_ds5.h5"
#define FILE4          "test_ds6.h5"
#define FILE5          "test_ds7.h5"
#define FILE6          "test_ds8.h5"
#define FILE7          "test_ds9.h5"
 
#define DIMENSION_LIST "DIMENSION_LIST"
#define REFERENCE_LIST "REFERENCE_LIST"

/*-------------------------------------------------------------------------
 * the main program
 *-------------------------------------------------------------------------
 */
int main(void)
{
    int nerrors=0;

    /* create file to be used in following tests */
    if(create_test_file("1") < 0) {
        nerrors = 1;
        goto error;
    }
    nerrors += test_char_attachscales("1") < 0  ? 1 : 0;
    nerrors += test_short_attachscales("1") < 0  ? 1 : 0;
    nerrors += test_int_attachscales("1") < 0  ? 1 : 0;
    nerrors += test_long_attachscales("1") < 0  ? 1 : 0;
    nerrors += test_float_attachscales("1") < 0  ? 1 : 0;
    nerrors += test_char_scalenames("1") < 0  ? 1 : 0;
    nerrors += test_short_scalenames("1") < 0  ? 1 : 0;
    nerrors += test_int_scalenames("1") < 0  ? 1 : 0;
    nerrors += test_long_scalenames("1") < 0  ? 1 : 0;
    nerrors += test_float_scalenames("1") < 0  ? 1 : 0;
    nerrors += test_numberofscales("1") < 0  ? 1 : 0;
    if(create_test_file("2") < 0) {
        nerrors = 1;
        goto error;
    }
    nerrors += test_long_attachscales("2") < 0  ? 1 : 0;
    nerrors += test_duplicatelong_attachscales("2") < 0  ? 1 : 0;
    nerrors += test_samelong_scalenames("2") < 0  ? 1 : 0;
    nerrors += test_foreign_scaleattached(FOREIGN_FILE1) < 0  ? 1 : 0;
    nerrors += test_foreign_scaleattached(FOREIGN_FILE2) < 0  ? 1 : 0;
    nerrors += test_detachscales() < 0  ? 1 : 0;

/*  the following tests have not been rewritten to match those above */
    nerrors += test_simple() < 0  ?1:0;
    nerrors += test_errors() < 0  ?1:0;
    nerrors += test_errors2() < 0  ?1:0;
    nerrors += test_rank() < 0  ?1:0;
    nerrors += test_iterators() < 0  ?1:0;
    nerrors += test_types() < 0  ?1:0;
    nerrors += test_data() < 0  ?1:0;


    if(nerrors) goto error;
    printf("All dimension scales tests passed.\n");
    return 0;

error:
    printf("***** %d DIMENSION SCALES TEST%s FAILED! *****\n",nerrors, 1 == nerrors ? "" : "S");
    return 1;
}


/*-------------------------------------------------------------------------
 * DS API test
 *
 * Functions tested:
 *
 * H5DSattach_scale
 * H5DSget_num_scales
 * H5DSdetach_scale
 * H5DSset_label
 * H5DSget_label
 * H5DSset_scale
 * H5DSget_scale_name
 * H5DSis_scale
 * H5DSiterate_scales
 *
 *-------------------------------------------------------------------------
 */

static hid_t create_test_file(const char *fileext)
{
    char filename[65];

    strcpy(filename, FILENAME);
    strcat(filename, fileext);
    strcat(filename, FILEEXT);
    return H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
}

static hid_t open_test_file(const char *fileext)
{
    char filename[65];

    strcpy(filename, FILENAME);
    strcat(filename, fileext);
    strcat(filename, FILEEXT);

    return H5Fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT);
}

/*-------------------------------------------------------------------------
 * create "data" dataset
 *-------------------------------------------------------------------------
 */
herr_t create_char_dataset(hid_t fid, const char *dsidx, int fulldims)
{
    int     rank = 3;
    int     rankds = 1;
    hsize_t dims[3]  = {DIM1_SIZE,DIM2_SIZE,DIM3_SIZE};
    char   buf[DIM1_SIZE*DIM2_SIZE*DIM3_SIZE];
    hsize_t s1_dim[1]  = {DIM1_SIZE};
    hsize_t s2_dim[1]  = {DIM2_SIZE};
    hsize_t s3_dim[1]  = {DIM3_SIZE};
    char    s1_wbuf[DIM1_SIZE] = {1,2,3};
    char    s11_wbuf[DIM1_SIZE] = {10,20,30};
    char    s2_wbuf[DIM2_SIZE] = {10,20,30,40};
    char    s21_wbuf[DIM2_SIZE] = {1,2,3,4};
    char    s22_wbuf[DIM2_SIZE] = {5,10,50,100};
    char    s3_wbuf[DIM3_SIZE] = {10,10,10,20,20,20,30,30,30,40,40,40};
    char    s31_wbuf[DIM3_SIZE] = {1,1,1,2,2,2,3,3,3,4,4,4};
    char    s32_wbuf[DIM3_SIZE] = {5,5,5,10,10,10,50,50,50,100,100,100};
    char    s33_wbuf[DIM3_SIZE] = {6,6,6,12,12,12,53,53,53,120,120,120};
    char name[32];

    strcpy(name, DATASET_NAME);
    strcat(name, dsidx);
    /* make a dataset */
    if(H5LTmake_dataset_char(fid, name, rank, dims, buf) >= 0) {
        if(fulldims==0) {
            /* make a DS dataset for the first dimension */
            if(create_DS1_char_datasets(fid, dsidx, rankds, s1_dim, s1_wbuf, NULL) < 0)
             return FAIL;

            /* make a DS dataset for the second dimension */
            if(create_DS2_char_datasets(fid, dsidx, rankds, s2_dim, s2_wbuf, NULL, NULL) < 0)
             return FAIL;

            /* make a DS dataset for the third dimension */
            if(create_DS3_char_datasets(fid, dsidx, rankds, s3_dim, s3_wbuf, NULL, NULL, NULL) < 0)
             return FAIL;
        }
        else {
            if(create_DS1_char_datasets(fid, dsidx, rankds, s1_dim, s1_wbuf, s11_wbuf) < 0)
             return FAIL;

            if(create_DS2_char_datasets(fid, dsidx, rankds, s2_dim, s2_wbuf, s21_wbuf, s22_wbuf) < 0)
             return FAIL;

            if(create_DS3_char_datasets(fid, dsidx, rankds, s3_dim, s3_wbuf, s31_wbuf, s32_wbuf, s33_wbuf) < 0)
             return FAIL;
        }
     }
     else
         return FAIL;
    return SUCCEED;
}

herr_t create_short_dataset(hid_t fid, const char *dsidx, int fulldims)
{
    int     rank = 3;
    int     rankds = 1;
    hsize_t dims[3]  = {DIM1_SIZE,DIM2_SIZE,DIM3_SIZE};
    short   buf[DIM1_SIZE*DIM2_SIZE*DIM3_SIZE];
    hsize_t s1_dim[1]  = {DIM1_SIZE};
    hsize_t s2_dim[1]  = {DIM2_SIZE};
    hsize_t s3_dim[1]  = {DIM3_SIZE};
    short   s1_wbuf[DIM1_SIZE] = {10,20,30};
    short   s11_wbuf[DIM1_SIZE] = {10,100,300};
    short   s2_wbuf[DIM2_SIZE] = {100,200,300,400};
    short   s21_wbuf[DIM2_SIZE] = {10,20,30,40};
    short   s22_wbuf[DIM2_SIZE] = {5,10,50,300};
    short    s3_wbuf[DIM3_SIZE] = {10,10,10,20,20,20,30,30,30,40,40,40};
    short    s31_wbuf[DIM3_SIZE] = {1,1,1,2,2,2,3,3,3,4,4,4};
    short    s32_wbuf[DIM3_SIZE] = {5,5,5,10,10,10,50,50,50,100,100,100};
    short    s33_wbuf[DIM3_SIZE] = {6,6,6,12,12,12,53,53,53,140,140,140};
    char name[32];

    strcpy(name, DATASET_NAME);
    strcat(name, dsidx);

    /* make a dataset */
    if(H5LTmake_dataset_short(fid, name, rank, dims, buf) >= 0) {
        if(fulldims==0) {
            /* make a DS dataset for the first dimension */
            if(create_DS1_short_datasets(fid, dsidx, rankds, s1_dim, s1_wbuf, NULL) < 0)
             return FAIL;

            /* make a DS dataset for the second dimension */
            if(create_DS2_short_datasets(fid, dsidx, rankds, s2_dim, s2_wbuf, NULL, NULL) < 0)
             return FAIL;

            /* make a DS dataset for the third dimension */
            if(create_DS3_short_datasets(fid, dsidx, rankds, s3_dim, s3_wbuf, NULL, NULL, NULL) < 0)
             return FAIL;
        }
        else {
            if(create_DS1_short_datasets(fid, dsidx, rankds, s1_dim, s1_wbuf, s11_wbuf) < 0)
             return FAIL;

            if(create_DS2_short_datasets(fid, dsidx, rankds, s2_dim, s2_wbuf, s21_wbuf, s22_wbuf) < 0)
             return FAIL;

            if(create_DS3_short_datasets(fid, dsidx, rankds, s3_dim, s3_wbuf, s31_wbuf, s32_wbuf, s33_wbuf) < 0)
             return FAIL;
        }
     }
     else
         return FAIL;
    return SUCCEED;
}

herr_t create_int_dataset(hid_t fid, const char *dsidx, int fulldims)
{
    int     rank = RANK;
    int     rankds = 1;
    hsize_t dims[RANK]  = {DIM1_SIZE,DIM2_SIZE};
    int     buf[DIM1_SIZE*DIM2_SIZE];
    hsize_t s1_dim[1]  = {DIM1_SIZE};
    hsize_t s2_dim[1]  = {DIM2_SIZE};
    int     s1_wbuf[DIM1_SIZE] = {10,20,30};
    int     s11_wbuf[DIM1_SIZE] = {10,100,300};
    int     s2_wbuf[DIM2_SIZE] = {100,200,300,400};
    int     s21_wbuf[DIM2_SIZE] = {10,20,30,40};
    int     s22_wbuf[DIM2_SIZE] = {5,10,50,300};
    char name[32];

    strcpy(name, DATASET_NAME);
    strcat(name, dsidx);

    /* make a dataset */
    if(H5LTmake_dataset_int(fid, name, rank, dims, buf) >= 0) {
        if(fulldims==0) {
            /* make a DS dataset for the first dimension */
            if(create_DS1_int_datasets(fid, dsidx, rankds, s1_dim, s1_wbuf, NULL) < 0)
             return FAIL;

            /* make a DS dataset for the second dimension */
            if(create_DS2_int_datasets(fid, dsidx, rankds, s2_dim, s2_wbuf, NULL, NULL) < 0)
             return FAIL;
        }
        else {
            if(create_DS1_int_datasets(fid, dsidx, rankds, s1_dim, s1_wbuf, s11_wbuf) < 0)
             return FAIL;

            if(create_DS2_int_datasets(fid, dsidx, rankds, s2_dim, s2_wbuf, s21_wbuf, s22_wbuf) < 0)
             return FAIL;
        }
     }
     else
         return FAIL;
    return SUCCEED;
}

herr_t create_long_dataset(hid_t fid, const char *dsname, const char *dsidx, int fulldims)
{
    int     rank = 4;
    int     rankds = 1;
    hsize_t dims[4]  = {DIM1_SIZE,DIM2_SIZE,DIM3_SIZE,DIM4_SIZE};
    long    buf[DIM1_SIZE*DIM2_SIZE*DIM3_SIZE*DIM4_SIZE];
    hsize_t s1_dim[1]  = {DIM1_SIZE};
    hsize_t s2_dim[1]  = {DIM2_SIZE};
    hsize_t s3_dim[1]  = {DIM3_SIZE};
    hsize_t s4_dim[1]  = {DIM4_SIZE};
    long    s1_wbuf[DIM1_SIZE] = {10,20,30};
    long    s11_wbuf[DIM1_SIZE] = {10,100,300};
    long    s2_wbuf[DIM2_SIZE] = {100,200,300,400};
    long    s21_wbuf[DIM2_SIZE] = {10,20,30,40};
    long    s22_wbuf[DIM2_SIZE] = {5,10,50,300};
    long    s3_wbuf[DIM3_SIZE] = {10,10,10,20,20,20,30,30,30,40,40,40};
    long    s31_wbuf[DIM3_SIZE] = {1,1,1,2,2,2,3,3,3,4,4,4};
    long    s32_wbuf[DIM3_SIZE] = {5,5,5,10,10,10,50,50,50,100,100,100};
    long    s33_wbuf[DIM3_SIZE] = {6,6,6,12,12,12,53,53,53,140,140,140};
    long    s4_wbuf[DIM4_SIZE] = {18,18};
    long    s41_wbuf[DIM4_SIZE] = {8,8};
    long    s42_wbuf[DIM4_SIZE] = {80,80};
    long    s43_wbuf[DIM4_SIZE] = {180,180};
    long    s44_wbuf[DIM4_SIZE] = {280,280};
    char name[32];

    strcpy(name, dsname);

    /* make a dataset */
    if(H5LTmake_dataset_long(fid, name, rank, dims, buf) >= 0) {
        if(fulldims==0) {
            /* make a DS dataset for the first dimension */
            if(create_DS1_long_datasets(fid, dsidx, rankds, s1_dim, s1_wbuf, NULL) < 0)
             return FAIL;

            /* make a DS dataset for the second dimension */
            if(create_DS2_long_datasets(fid, dsidx, rankds, s2_dim, s2_wbuf, NULL, NULL) < 0)
             return FAIL;

            /* make a DS dataset for the third dimension */
            if(create_DS3_long_datasets(fid, dsidx, rankds, s3_dim, s3_wbuf, NULL, NULL, NULL) < 0)
             return FAIL;

            /* make a DS dataset for the fourth dimension */
            if(create_DS4_long_datasets(fid, dsidx, rankds, s4_dim, s4_wbuf, NULL, NULL, NULL, NULL) < 0)
             return FAIL;
        }
        else {
            if(create_DS1_long_datasets(fid, dsidx, rankds, s1_dim, s1_wbuf, s11_wbuf) < 0)
             return FAIL;

            if(create_DS2_long_datasets(fid, dsidx, rankds, s2_dim, s2_wbuf, s21_wbuf, s22_wbuf) < 0)
             return FAIL;

            if(create_DS3_long_datasets(fid, dsidx, rankds, s3_dim, s3_wbuf, s31_wbuf, s32_wbuf, s33_wbuf) < 0)
             return FAIL;

            if(create_DS4_long_datasets(fid, dsidx, rankds, s4_dim, s4_wbuf, s41_wbuf, s42_wbuf, s43_wbuf, s44_wbuf) < 0)
             return FAIL;
        }
     }
     else
         return FAIL;
    return SUCCEED;
}

herr_t create_float_dataset(hid_t fid, const char *dsidx, int fulldims)
{
    int     rank = RANK;
    int     rankds = 1;
    hsize_t dims[RANK]  = {DIM1_SIZE,DIM2_SIZE};
    float   buf[DIM1_SIZE*DIM2_SIZE];
    hsize_t s1_dim[1]  = {DIM1_SIZE};
    hsize_t s2_dim[1]  = {DIM2_SIZE};
    float   s1_wbuf[DIM1_SIZE] = {10,20,30};
    float   s11_wbuf[DIM1_SIZE] = {10,100,300};
    float   s2_wbuf[DIM2_SIZE] = {100,200,300,400};
    float   s21_wbuf[DIM2_SIZE] = {10,20,30,40};
    float   s22_wbuf[DIM2_SIZE] = {5,10,50,300};
    char name[32];

    strcpy(name, DATASET_NAME);
    strcat(name, dsidx);

    /* make a dataset */
    if(H5LTmake_dataset_float(fid, name, rank, dims, buf) >= 0) {
        if(fulldims==0) {
            /* make a DS dataset for the first dimension */
            if(create_DS1_float_datasets(fid, dsidx, rankds, s1_dim, s1_wbuf, NULL) < 0)
             return FAIL;

            /* make a DS dataset for the second dimension */
            if(create_DS2_float_datasets(fid, dsidx, rankds, s2_dim, s2_wbuf, NULL, NULL) < 0)
             return FAIL;
        }
        else {
            if(create_DS1_float_datasets(fid, dsidx, rankds, s1_dim, s1_wbuf, s11_wbuf) < 0)
             return FAIL;

            if(create_DS2_float_datasets(fid, dsidx, rankds, s2_dim, s2_wbuf, s21_wbuf, s22_wbuf) < 0)
             return FAIL;
        }
     }
     else
         return FAIL;
    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * create 2 dimension scales datasets for first dimension
 *-------------------------------------------------------------------------
 */
herr_t create_DS1_char_datasets(hid_t fid, const char *dsidx, int rankds, hsize_t *s_dim, char *s_wbuf, char *s1_wbuf)
{
    char name[32];

    strcpy(name, DS_1_NAME);
    strcat(name, dsidx);
    /* make a DS dataset for the first dimension */
    if(H5LTmake_dataset_char(fid, name, rankds, s_dim, s_wbuf) < 0)
        return FAIL;

    if(s1_wbuf!=NULL) {
        strcpy(name, DS_11_NAME);
        strcat(name, dsidx);
        /* make a DS dataset with an alternate scale for the first dimension  */
        if(H5LTmake_dataset_char(fid, name, rankds, s_dim, s1_wbuf) < 0)
            return FAIL;
    }

    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * create 3 dimension scales datasets for second dimension
 *-------------------------------------------------------------------------
 */
herr_t create_DS2_char_datasets(hid_t fid, const char *dsidx, int rankds, hsize_t *s_dim, char *s_wbuf, char *s1_wbuf, char *s2_wbuf)
{
    char name[32];

    strcpy(name, DS_2_NAME);
    strcat(name, dsidx);
    /* make a DS dataset for the second dimension */
    if(H5LTmake_dataset_char(fid, name, rankds, s_dim, s_wbuf) < 0)
        return FAIL;

    if(s1_wbuf!=NULL) {
        strcpy(name, DS_21_NAME);
        strcat(name, dsidx);
        /* make a DS dataset with an alternate scale for the second dimension  */
        if(H5LTmake_dataset_char(fid, name, rankds, s_dim, s1_wbuf) < 0)
            return FAIL;
    }

    if(s2_wbuf!=NULL) {
        strcpy(name, DS_22_NAME);
        strcat(name, dsidx);
        /* make a DS dataset with an alternate scale for the second dimension  */
        if(H5LTmake_dataset_char(fid, name, rankds, s_dim, s2_wbuf) < 0)
            return FAIL;
    }

    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * create 1 dimension scales datasets for third dimension of dataset
 *-------------------------------------------------------------------------
 */
herr_t create_DS3_char_datasets(hid_t fid, const char *dsidx, int rankds, hsize_t *s_dim, char *s_wbuf, char *s1_wbuf, char *s2_wbuf, char *s3_wbuf)
{
    char name[32];

    strcpy(name, DS_3_NAME);
    strcat(name, dsidx);
    /* make a DS dataset for the first dimension */
    if(H5LTmake_dataset_char(fid, name, rankds, s_dim, s_wbuf) < 0)
        return FAIL;

    if(s1_wbuf!=NULL) {
        strcpy(name, DS_31_NAME);
        strcat(name, dsidx);
        /* make a DS dataset with an alternate scale for the second dimension  */
        if(H5LTmake_dataset_char(fid, name, rankds, s_dim, s1_wbuf) < 0)
            return FAIL;
    }

    if(s2_wbuf!=NULL) {
        strcpy(name, DS_32_NAME);
        strcat(name, dsidx);
        /* make a DS dataset with an alternate scale for the second dimension  */
        if(H5LTmake_dataset_char(fid, name, rankds, s_dim, s2_wbuf) < 0)
            return FAIL;
    }

    if(s3_wbuf!=NULL) {
        strcpy(name, DS_33_NAME);
        strcat(name, dsidx);
        /* make a DS dataset with an alternate scale for the second dimension  */
        if(H5LTmake_dataset_char(fid, name, rankds, s_dim, s3_wbuf) < 0)
            return FAIL;
    }

    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * create 2 dimension scales datasets for first dimension
 *-------------------------------------------------------------------------
 */
herr_t create_DS1_short_datasets(hid_t fid, const char *dsidx, int rankds, hsize_t *s_dim, short *s_wbuf, short *s1_wbuf)
{
    char name[32];

    strcpy(name, DS_1_NAME);
    strcat(name, dsidx);
    /* make a DS dataset for the first dimension */
    if(H5LTmake_dataset_short(fid, name, rankds, s_dim, s_wbuf) < 0)
        return FAIL;

    if(s1_wbuf!=NULL) {
        strcpy(name, DS_11_NAME);
        strcat(name, dsidx);
        /* make a DS dataset with an alternate scale for the first dimension  */
        if(H5LTmake_dataset_short(fid, name, rankds, s_dim, s1_wbuf) < 0)
            return FAIL;
    }

    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * create 3 dimension scales datasets for second dimension
 *-------------------------------------------------------------------------
 */
herr_t create_DS2_short_datasets(hid_t fid, const char *dsidx, int rankds, hsize_t *s_dim, short *s_wbuf, short *s1_wbuf, short *s2_wbuf)
{
    char name[32];

    strcpy(name, DS_2_NAME);
    strcat(name, dsidx);

    /* make a DS dataset for the second dimension */
    if(H5LTmake_dataset_short(fid, name, rankds, s_dim, s_wbuf) < 0)
        return FAIL;

    if(s1_wbuf!=NULL) {
        strcpy(name, DS_21_NAME);
        strcat(name, dsidx);
        /* make a DS dataset with an alternate scale for the second dimension  */
        if(H5LTmake_dataset_short(fid, name, rankds, s_dim, s1_wbuf) < 0)
            return FAIL;
    }

    if(s2_wbuf!=NULL) {
        strcpy(name, DS_22_NAME);
        strcat(name, dsidx);
        /* make a DS dataset with an alternate scale for the second dimension  */
        if(H5LTmake_dataset_short(fid, name, rankds, s_dim, s2_wbuf) < 0)
            return FAIL;
    }

    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * create 1 dimension scales datasets for third dimension of dataset
 *-------------------------------------------------------------------------
 */
herr_t create_DS3_short_datasets(hid_t fid, const char *dsidx, int rankds, hsize_t *s_dim, short *s_wbuf, short *s1_wbuf, short *s2_wbuf, short *s3_wbuf)
{
    char name[32];

    strcpy(name, DS_3_NAME);
    strcat(name, dsidx);

    /* make a DS dataset for the first dimension */
    if(H5LTmake_dataset_short(fid, name, rankds, s_dim, s_wbuf) < 0)
        return FAIL;

    if(s1_wbuf!=NULL) {
        strcpy(name, DS_31_NAME);
        strcat(name, dsidx);
        /* make a DS dataset for the first dimension */
        if(H5LTmake_dataset_short(fid, name, rankds, s_dim, s1_wbuf) < 0)
            return FAIL;
    }

    if(s2_wbuf!=NULL) {
        strcpy(name, DS_32_NAME);
        strcat(name, dsidx);
        /* make a DS dataset for the first dimension */
        if(H5LTmake_dataset_short(fid, name, rankds, s_dim, s2_wbuf) < 0)
            return FAIL;
    }

    if(s3_wbuf!=NULL) {
        strcpy(name, DS_33_NAME);
        strcat(name, dsidx);
        /* make a DS dataset for the first dimension */
        if(H5LTmake_dataset_short(fid, name, rankds, s_dim, s3_wbuf) < 0)
            return FAIL;
    }

    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * create 2 dimension scales datasets for first dimension
 *-------------------------------------------------------------------------
 */
herr_t create_DS1_int_datasets(hid_t fid, const char *dsidx, int rankds, hsize_t *s_dim, int *s_wbuf, int *s1_wbuf)
{
    char name[32];

    strcpy(name, DS_1_NAME);
    strcat(name, dsidx);

    /* make a DS dataset for the first dimension */
    if(H5LTmake_dataset_int(fid, name, rankds, s_dim, s_wbuf) < 0)
        return FAIL;

    if(s1_wbuf!=NULL) {
        strcpy(name, DS_11_NAME);
        strcat(name, dsidx);
        /* make a DS dataset with an alternate scale for the first dimension  */
        if(H5LTmake_dataset_int(fid, name, rankds, s_dim, s1_wbuf) < 0)
            return FAIL;
    }

    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * create 3 dimension scales datasets for second dimension
 *-------------------------------------------------------------------------
 */
herr_t create_DS2_int_datasets(hid_t fid, const char *dsidx, int rankds, hsize_t *s_dim, int *s_wbuf, int *s1_wbuf, int *s2_wbuf)
{
    char name[32];

    strcpy(name, DS_2_NAME);
    strcat(name, dsidx);

    /* make a DS dataset for the second dimension */
    if(H5LTmake_dataset_int(fid, name, rankds, s_dim, s_wbuf) < 0)
        return FAIL;

    if(s1_wbuf!=NULL) {
        strcpy(name, DS_21_NAME);
        strcat(name, dsidx);
        /* make a DS dataset with an alternate scale for the second dimension  */
        if(H5LTmake_dataset_int(fid, name, rankds, s_dim, s1_wbuf) < 0)
            return FAIL;
    }

    if(s2_wbuf!=NULL) {
        strcpy(name, DS_22_NAME);
        strcat(name, dsidx);
        /* make a DS dataset with an alternate scale for the second dimension  */
        if(H5LTmake_dataset_int(fid, name, rankds, s_dim, s2_wbuf) < 0)
            return FAIL;
    }

    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * create 1 dimension scales datasets for third dimension of dataset
 *-------------------------------------------------------------------------
 */
herr_t create_DS3_int_datasets(hid_t fid, const char *dsidx, int rankds, hsize_t *s_dim, int *s_wbuf, int *s1_wbuf, int *s2_wbuf, int *s3_wbuf)
{
    char name[32];

    strcpy(name, DS_3_NAME);
    strcat(name, dsidx);

    /* make a DS dataset for the first dimension */
    if(H5LTmake_dataset_int(fid, name, rankds, s_dim, s_wbuf) < 0)
        return FAIL;

    if(s1_wbuf!=NULL) {
        strcpy(name, DS_31_NAME);
        strcat(name, dsidx);
        /* make a DS dataset with an alternate scale for the first dimension  */
        if(H5LTmake_dataset_int(fid, name, rankds, s_dim, s1_wbuf) < 0)
            return FAIL;
    }

    if(s2_wbuf!=NULL) {
        strcpy(name, DS_32_NAME);
        strcat(name, dsidx);
        /* make a DS dataset with an alternate scale for the first dimension  */
        if(H5LTmake_dataset_int(fid, name, rankds, s_dim, s2_wbuf) < 0)
            return FAIL;
    }

    if(s3_wbuf!=NULL) {
        strcpy(name, DS_33_NAME);
        strcat(name, dsidx);
        /* make a DS dataset with an alternate scale for the first dimension  */
        if(H5LTmake_dataset_int(fid, name, rankds, s_dim, s3_wbuf) < 0)
            return FAIL;
    }

    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * create 2 dimension scales datasets for first dimension
 *-------------------------------------------------------------------------
 */
herr_t create_DS1_long_datasets(hid_t fid, const char *dsidx, int rankds, hsize_t *s_dim, long *s_wbuf, long *s1_wbuf)
{
    char name[32];

    strcpy(name, DS_1_NAME);
    strcat(name, dsidx);

    /* make a DS dataset for the first dimension */
    if(H5LTmake_dataset_long(fid, name, rankds, s_dim, s_wbuf) < 0)
        return FAIL;

    if(s1_wbuf!=NULL) {
        strcpy(name, DS_11_NAME);
        strcat(name, dsidx);
        /* make a DS dataset with an alternate scale for the first dimension  */
        if(H5LTmake_dataset_long(fid, name, rankds, s_dim, s1_wbuf) < 0)
            return FAIL;
    }

    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * create 3 dimension scales datasets for second dimension
 *-------------------------------------------------------------------------
 */
herr_t create_DS2_long_datasets(hid_t fid, const char *dsidx, int rankds, hsize_t *s_dim, long *s_wbuf, long *s1_wbuf, long *s2_wbuf)
{
    char name[32];

    strcpy(name, DS_2_NAME);
    strcat(name, dsidx);

    /* make a DS dataset for the second dimension */
    if(H5LTmake_dataset_long(fid, name, rankds, s_dim, s_wbuf) < 0)
        return FAIL;

    if(s1_wbuf!=NULL) {
        strcpy(name, DS_21_NAME);
        strcat(name, dsidx);
        /* make a DS dataset with an alternate scale for the second dimension  */
        if(H5LTmake_dataset_long(fid, name, rankds, s_dim, s1_wbuf) < 0)
            return FAIL;
    }

    if(s2_wbuf!=NULL) {
        strcpy(name, DS_22_NAME);
        strcat(name, dsidx);
        /* make a DS dataset with an alternate scale for the second dimension  */
        if(H5LTmake_dataset_long(fid, name, rankds, s_dim, s2_wbuf) < 0)
            return FAIL;
    }

    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * create 3 dimension scales datasets for third dimension of dataset
 *-------------------------------------------------------------------------
 */
herr_t create_DS3_long_datasets(hid_t fid, const char *dsidx, int rankds, hsize_t *s_dim, long *s_wbuf, long *s1_wbuf, long *s2_wbuf, long *s3_wbuf)
{
    char name[32];

    strcpy(name, DS_3_NAME);
    strcat(name, dsidx);

    /* make a DS dataset for the first dimension */
    if(H5LTmake_dataset_long(fid, name, rankds, s_dim, s_wbuf) < 0)
        return FAIL;

    if(s1_wbuf!=NULL) {
        strcpy(name, DS_31_NAME);
        strcat(name, dsidx);
        /* make a DS dataset with an alternate scale for the second dimension  */
        if(H5LTmake_dataset_long(fid, name, rankds, s_dim, s1_wbuf) < 0)
            return FAIL;
    }

    if(s2_wbuf!=NULL) {
        strcpy(name, DS_32_NAME);
        strcat(name, dsidx);
        /* make a DS dataset with an alternate scale for the second dimension  */
        if(H5LTmake_dataset_long(fid, name, rankds, s_dim, s2_wbuf) < 0)
            return FAIL;
    }

    if(s3_wbuf!=NULL) {
        strcpy(name, DS_33_NAME);
        strcat(name, dsidx);
        /* make a DS dataset with an alternate scale for the second dimension  */
        if(H5LTmake_dataset_long(fid, name, rankds, s_dim, s3_wbuf) < 0)
            return FAIL;
    }

    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * create 4 dimension scales datasets for third dimension of dataset
 *-------------------------------------------------------------------------
 */
herr_t create_DS4_long_datasets(hid_t fid, const char *dsidx, int rankds, hsize_t *s_dim, long *s_wbuf, long *s1_wbuf, long *s2_wbuf, long *s3_wbuf, long *s4_wbuf)
{
    char name[32];

    strcpy(name, DS_4_NAME);
    strcat(name, dsidx);

    /* make a DS dataset for the first dimension */
    if(H5LTmake_dataset_long(fid, name, rankds, s_dim, s_wbuf) < 0)
        return FAIL;

    if(s1_wbuf!=NULL) {
        strcpy(name, DS_41_NAME);
        strcat(name, dsidx);
        /* make a DS dataset with an alternate scale for the second dimension  */
        if(H5LTmake_dataset_long(fid, name, rankds, s_dim, s1_wbuf) < 0)
            return FAIL;
    }

    if(s2_wbuf!=NULL) {
        strcpy(name, DS_42_NAME);
        strcat(name, dsidx);
        /* make a DS dataset with an alternate scale for the second dimension  */
        if(H5LTmake_dataset_long(fid, name, rankds, s_dim, s2_wbuf) < 0)
            return FAIL;
    }

    if(s3_wbuf!=NULL) {
        strcpy(name, DS_43_NAME);
        strcat(name, dsidx);
        /* make a DS dataset with an alternate scale for the second dimension  */
        if(H5LTmake_dataset_long(fid, name, rankds, s_dim, s3_wbuf) < 0)
            return FAIL;
    }

    if(s4_wbuf!=NULL) {
        strcpy(name, DS_44_NAME);
        strcat(name, dsidx);
        /* make a DS dataset with an alternate scale for the second dimension  */
        if(H5LTmake_dataset_long(fid, name, rankds, s_dim, s4_wbuf) < 0)
            return FAIL;
    }

    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * create 2 dimension scales datasets for first dimension
 *-------------------------------------------------------------------------
 */
herr_t create_DS1_float_datasets(hid_t fid, const char *dsidx, int rankds, hsize_t *s_dim, float *s_wbuf, float *s1_wbuf)
{
    char name[32];

    strcpy(name, DS_1_NAME);
    strcat(name, dsidx);

    /* make a DS dataset for the first dimension */
    if(H5LTmake_dataset_float(fid, name, rankds, s_dim, s_wbuf) < 0)
        return FAIL;

    if(s1_wbuf!=NULL) {
        strcpy(name, DS_11_NAME);
        strcat(name, dsidx);
        /* make a DS dataset with an alternate scale for the first dimension  */
        if(H5LTmake_dataset_float(fid, name, rankds, s_dim, s1_wbuf) < 0)
            return FAIL;
    }

    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * create 3 dimension scales datasets for second dimension
 *-------------------------------------------------------------------------
 */
herr_t create_DS2_float_datasets(hid_t fid, const char *dsidx, int rankds, hsize_t *s_dim, float *s_wbuf, float *s1_wbuf, float *s2_wbuf)
{
    char name[32];

    strcpy(name, DS_2_NAME);
    strcat(name, dsidx);

    /* make a DS dataset for the second dimension */
    if(H5LTmake_dataset_float(fid, name, rankds, s_dim, s_wbuf) < 0)
        return FAIL;

    if(s1_wbuf!=NULL) {
        strcpy(name, DS_21_NAME);
        strcat(name, dsidx);
        /* make a DS dataset with an alternate scale for the second dimension  */
        if(H5LTmake_dataset_float(fid, name, rankds, s_dim, s1_wbuf) < 0)
            return FAIL;
    }

    if(s2_wbuf!=NULL) {
        strcpy(name, DS_22_NAME);
        strcat(name, dsidx);
        /* make a DS dataset with an alternate scale for the second dimension  */
        if(H5LTmake_dataset_float(fid, name, rankds, s_dim, s2_wbuf) < 0)
            return FAIL;
    }

    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * create 3 dimension scales datasets for third dimension of dataset
 *-------------------------------------------------------------------------
 */
herr_t create_DS3_float_datasets(hid_t fid, const char *dsidx, int rankds, hsize_t *s_dim, float *s_wbuf, float *s1_wbuf, float *s2_wbuf, float *s3_wbuf)
{
    char name[32];

    strcpy(name, DS_3_NAME);
    strcat(name, dsidx);

    /* make a DS dataset for the first dimension */
    if(H5LTmake_dataset_float(fid, name, rankds, s_dim, s_wbuf) < 0)
        return FAIL;

    if(s1_wbuf!=NULL) {
        strcpy(name, DS_31_NAME);
        strcat(name, dsidx);
        /* make a DS dataset with an alternate scale for the second dimension  */
        if(H5LTmake_dataset_float(fid, name, rankds, s_dim, s1_wbuf) < 0)
            return FAIL;
    }

    if(s2_wbuf!=NULL) {
        strcpy(name, DS_32_NAME);
        strcat(name, dsidx);
        /* make a DS dataset with an alternate scale for the second dimension  */
        if(H5LTmake_dataset_float(fid, name, rankds, s_dim, s2_wbuf) < 0)
            return FAIL;
    }

    if(s3_wbuf!=NULL) {
        strcpy(name, DS_33_NAME);
        strcat(name, dsidx);
        /* make a DS dataset with an alternate scale for the second dimension  */
        if(H5LTmake_dataset_float(fid, name, rankds, s_dim, s3_wbuf) < 0)
            return FAIL;
    }

    return SUCCEED;
}

herr_t test_attach_scale(hid_t fid, hid_t did, const char *name, unsigned int idx)
{
    herr_t  ret_value = FAIL;
    hid_t   dsid = -1;

    if((dsid = H5Dopen2(fid, name, H5P_DEFAULT)) >= 0) {
        if(H5DSis_attached(did, dsid, idx) == 0) {
            if(H5DSattach_scale(did, dsid, idx) >= 0) {
                if(H5DSis_attached(did, dsid, idx) > 0) {
                    /* printf(" scale attached "); */
                    ret_value = SUCCEED;
                }
                else if(H5DSis_attached(did, dsid, idx) == 0) {
                    printf(" scale not attached ");
                }
            }
        }
        if(H5Dclose(dsid) < 0)
            ret_value = FAIL;
    }

    return ret_value;
}

herr_t test_detach_scale(hid_t fid, hid_t did, const char *name, unsigned int idx)
{
    herr_t  ret_value = FAIL;
    hid_t   dsid = -1;

    if((dsid = H5Dopen2(fid, name, H5P_DEFAULT)) >= 0) {
        if(H5DSis_attached(did, dsid, idx) == 1) {
            if(H5DSdetach_scale(did, dsid, idx) >= 0) {
                if(H5DSis_attached(did, dsid, idx) == 0) {
                    ret_value = SUCCEED;
                }
            }
        }
        if(H5Dclose(dsid) < 0)
            ret_value = FAIL;
    }

    return ret_value;
}

herr_t test_set_scalename(hid_t fid, hid_t did, const char *name, const char *scalename, unsigned int idx)
{
    herr_t  ret_value = FAIL;
    hid_t   dsid = -1;

    if((dsid = H5Dopen2(fid, name, H5P_DEFAULT)) >= 0) {
        if(H5DSis_attached(did, dsid, idx) == 1) {
            if(H5DSset_scale(dsid, scalename) >= 0) {
                if(H5DSis_attached(did, dsid, idx) == 1) {
                     ret_value = SUCCEED;
                }
            }
        }
        if(H5Dclose(dsid) < 0)
            ret_value = FAIL;
    }

    return ret_value;
}

herr_t test_cmp_scalename(hid_t fid, hid_t did, const char *name, const char *scalename, unsigned int idx)
{
    herr_t  ret_value = FAIL;
    hid_t   dsid = -1;
    ssize_t name_len;
    char    *name_out=NULL;

    if((dsid = H5Dopen2(fid, name, H5P_DEFAULT)) >= 0) {
        if(H5DSis_attached(did, dsid, idx) == 1) {
            if((name_len=H5DSget_scale_name(dsid,NULL,(size_t)0)) > 0) {
                name_out = (char*)malloc(name_len * sizeof (char));
                if(name_out != NULL) {
                    if(H5DSget_scale_name(dsid, name_out, (size_t)name_len) >= 0) {
                        if(strcmp(scalename,name_out)==0) {
                            ret_value = SUCCEED;
                        }
                        free(name_out);
                        name_out=NULL;
                    }
                }
            }
        }
        if(H5Dclose(dsid) < 0)
            ret_value = FAIL;
    }

    return ret_value;
}

static int test_detachscales(void)
{
    hid_t   fid = -1;
    hid_t   did = -1;
    hid_t   dsid = -1;
    int     rank1 = 1;
    int     rank3 = 3;
    hsize_t dims[] = {1,2,3}; /*some bogus numbers, not important for the test*/
    int     *buf = NULL;
    char    dname[10];
    int     i;

    /* This tests creates two three dimensional datasets; then it creates
       four integer datasets that are used as dim. scales; we will attach
       and detach them to check that at the end there is no attributes
       REFERENCE_LIST on a dimension scale and DIMENSION_LIST on a dataset */

    TESTING2("test_detachscales");

    if((fid = H5Fcreate("test_detach.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;

    /* make datasets; they are three dimensional*/
    for (i=0; i < 2; i++) {
        sprintf(dname,"D%d", i);
        if(H5LTmake_dataset_int(fid, dname, rank3, dims, buf) < 0)
            goto out;
    } 
    /* create datasets and make them dim. scales */

    for (i=0; i < 4; i++) {
        sprintf(dname, "DS%d", i);
        if(H5LTmake_dataset_int(fid, dname, rank1, dims, buf) < 0)
            goto out;
    }
    /* attach scales to the first dataset; first dimension will have 
       two scales attached  */
    if((did = H5Dopen2(fid, "D0", H5P_DEFAULT)) >= 0) {
        for (i=0; i<4; i++) {
           sprintf(dname, "DS%d", i);
           if((dsid = H5Dopen2(fid, dname, H5P_DEFAULT)) < 0)
               goto out;
           if(H5DSattach_scale(did, dsid, (unsigned int) i%3) < 0)
               goto out;
           if(H5Dclose(dsid) < 0)
               goto out;
        }
        if(H5Dclose(did) < 0)
            goto out;
    }
    else
        goto out;

   /* attach scales to the second dataset */
    if((did = H5Dopen2(fid, "D1", H5P_DEFAULT)) >= 0) {
        for (i=0; i<3; i++) {
           sprintf(dname, "DS%d", i);
           if((dsid = H5Dopen2(fid, dname, H5P_DEFAULT)) < 0)
               goto out;
           if(H5DSattach_scale(did, dsid, (unsigned int) i) < 0)
               goto out;
           if(H5Dclose(dsid) < 0)
               goto out;
        }
        if(H5Dclose(did) < 0)
            goto out;
    }
    else
        goto out;

    /* detach DS0 from first dimension of D0 and D1; then check
       that DS0 doesn't have attribute REFERENCE _LIST */

    if((dsid = H5Dopen2(fid, "DS0", H5P_DEFAULT)) < 0)
               goto out;

    for (i=0; i<2; i++) {
        sprintf(dname, "D%d", i);
        if((did = H5Dopen2(fid, dname, H5P_DEFAULT)) < 0)
            goto out;
        if(H5DSdetach_scale(did, dsid, (unsigned int)0) < 0)
            goto out;
        if(H5Dclose(did) < 0)
            goto out;
    } 
    /* Check that attribute "REFERENCE_LIST" doesn't exist anymore */
    if(H5Aexists(dsid, REFERENCE_LIST)!= 0)
        goto out;
    if(H5Dclose(dsid) < 0)
        goto out;
    /* Check that DS3 is the only dim. scale attached to the first 
       dimension of D0 */
    if((did = H5Dopen2(fid, "D0", H5P_DEFAULT)) < 0)
        goto out;
    if((dsid = H5Dopen2(fid, "DS3", H5P_DEFAULT)) < 0)
        goto out;
    if(H5DSis_attached(did, dsid, (unsigned int) 0) <= 0)
        goto out;
    if(H5Dclose(did) < 0)
        goto out;
    if(H5Dclose(dsid) < 0)
        goto out;

    /* Detach the rest of the scales DS3, DS1, DS2 from D0 and make 
       sure that attribute "DIMENSION_LIST" doesn't exist anymore */
    if((did = H5Dopen2(fid, "D0", H5P_DEFAULT)) >= 0) {
        for (i=1; i<4; i++) {
           sprintf(dname, "DS%d", i);
           if((dsid = H5Dopen2(fid, dname, H5P_DEFAULT)) < 0)
               goto out;
           if(H5DSdetach_scale(did, dsid, (unsigned int) i%3) < 0)
               goto out;
           if(H5Dclose(dsid) < 0)
               goto out;
        }
        /* Check that attribute "DIMENSION_LIST" doesn't exist anymore */
        if(H5Aexists(did, DIMENSION_LIST)!= 0) 
            goto out;
        if(H5Dclose(did) < 0)
            goto out;
    }
    else
        goto out;

    
    PASSED();

    H5Fclose(fid);
    return SUCCEED;

out:
    H5E_BEGIN_TRY  {
        H5Dclose(did);
        H5Dclose(dsid);
        H5Fclose(fid);
    } H5E_END_TRY;

    H5_FAILED();

    return FAIL;
}

static int test_char_attachscales(const char *fileext)
{
    hid_t   fid = -1;
    hid_t   did = -1;
    char    dsname[32];
    char    scalename[32];
    strcpy(dsname, DATASET_NAME);
    strcat(dsname, "ac");

    TESTING2("test_char_attachscales");

    if((fid = open_test_file(fileext)) < 0)
        goto out;

    /* make a dataset */
    if(create_char_dataset(fid, "ac", 0) < 0)
        goto out;

    if((did = H5Dopen2(fid, dsname, H5P_DEFAULT)) >= 0) {
        strcpy(scalename, DS_1_NAME);
        strcat(scalename, "ac");
        if(test_attach_scale(fid, did, scalename, DIM0) < 0)
            goto out;

        strcpy(scalename, DS_2_NAME);
        strcat(scalename, "ac");
        if(test_attach_scale(fid, did, scalename, DIM1) < 0)
            goto out;

        strcpy(scalename, DS_3_NAME);
        strcat(scalename, "ac");
        if(test_attach_scale(fid, did, scalename, DIM2) < 0)
            goto out;

        if(H5Dclose(did) < 0)
            goto out;
    }
    else
        goto out;

    PASSED();

    H5Fclose(fid);
    return SUCCEED;

out:
    H5E_BEGIN_TRY  {
        H5Dclose(did);
        H5Fclose(fid);
    } H5E_END_TRY;

    H5_FAILED();

    return FAIL;
}

static int test_short_attachscales(const char *fileext)
{
    hid_t   fid = -1;
    hid_t   did = -1;
    char    dsname[32];
    char    scalename[32];
    strcpy(dsname, DATASET_NAME);
    strcat(dsname, "as");

    TESTING2("test_short_attachscales");

    if((fid = open_test_file(fileext)) < 0)
        goto out;

    /* make a dataset */
    if(create_short_dataset(fid, "as", 1) < 0)
        goto out;

    if((did = H5Dopen2(fid, dsname, H5P_DEFAULT)) >= 0) {
        strcpy(scalename, DS_1_NAME);
        strcat(scalename, "as");
        if(test_attach_scale(fid, did, scalename, DIM0) < 0)
            goto out;

        strcpy(scalename, DS_11_NAME);
        strcat(scalename, "as");
        if(test_attach_scale(fid, did, scalename, DIM0) < 0)
            goto out;

        strcpy(scalename, DS_2_NAME);
        strcat(scalename, "as");
        if(test_attach_scale(fid, did, scalename, DIM1) < 0)
            goto out;

        strcpy(scalename, DS_21_NAME);
        strcat(scalename, "as");
        if(test_attach_scale(fid, did, scalename, DIM1) < 0)
            goto out;

        strcpy(scalename, DS_22_NAME);
        strcat(scalename, "as");
        if(test_attach_scale(fid, did, scalename, DIM1) < 0)
            goto out;

        strcpy(scalename, DS_3_NAME);
        strcat(scalename, "as");
        if(test_attach_scale(fid, did, scalename, DIM2) < 0)
            goto out;

        strcpy(scalename, DS_31_NAME);
        strcat(scalename, "as");
        if(test_attach_scale(fid, did, scalename, DIM2) < 0)
            goto out;

        strcpy(scalename, DS_32_NAME);
        strcat(scalename, "as");
        if(test_attach_scale(fid, did, scalename, DIM2) < 0)
            goto out;

        strcpy(scalename, DS_33_NAME);
        strcat(scalename, "as");
        if(test_attach_scale(fid, did, scalename, DIM2) < 0)
            goto out;

        if(H5Dclose(did) < 0)
            goto out;
    }
    else
        goto out;

    PASSED();

    H5Fclose(fid);
    return SUCCEED;

out:
    H5E_BEGIN_TRY  {
        H5Dclose(did);
        H5Fclose(fid);
    } H5E_END_TRY;

    H5_FAILED();

    return FAIL;
}

static int test_int_attachscales(const char *fileext)
{
    hid_t   fid = -1;
    hid_t   did = -1;
    char    dsname[32];
    char    scalename[32];
    strcpy(dsname, DATASET_NAME);
    strcat(dsname, "a");

    TESTING2("test_int_attachscales");

    if((fid = open_test_file(fileext)) < 0)
        goto out;

    /* make a dataset */
    if(create_int_dataset(fid, "a", 1) < 0)
        goto out;

    if((did = H5Dopen2(fid, dsname, H5P_DEFAULT)) >= 0) {
        strcpy(scalename, DS_1_NAME);
        strcat(scalename, "a");
        if(test_attach_scale(fid, did, scalename, DIM0) < 0)
            goto out;

        strcpy(scalename, DS_11_NAME);
        strcat(scalename, "a");
        if(test_attach_scale(fid, did, scalename, DIM0) < 0)
            goto out;

        strcpy(scalename, DS_2_NAME);
        strcat(scalename, "a");
        if(test_attach_scale(fid, did, scalename, DIM1) < 0)
            goto out;

        strcpy(scalename, DS_21_NAME);
        strcat(scalename, "a");
        if(test_attach_scale(fid, did, scalename, DIM1) < 0)
            goto out;

        strcpy(scalename, DS_22_NAME);
        strcat(scalename, "a");
        if(test_attach_scale(fid, did, scalename, DIM1) < 0)
            goto out;

        if(H5Dclose(did) < 0)
            goto out;
    }
    else
        goto out;

    PASSED();

    H5Fclose(fid);
    return SUCCEED;

out:
    H5E_BEGIN_TRY  {
        H5Dclose(did);
        H5Fclose(fid);
    } H5E_END_TRY;

    H5_FAILED();

    return FAIL;
}

static int test_long_attachscales(const char *fileext)
{
    hid_t   fid = -1;
    hid_t   did = -1;
    char    dsname[32];
    char    scalename[32];
    strcpy(dsname, DATASET_NAME);
    strcat(dsname, "al");

    TESTING2("test_long_attachscales");

    if((fid = open_test_file(fileext)) < 0)
        goto out;

    /* make a dataset */
    if(create_long_dataset(fid, dsname, "al", 0) < 0)
        goto out;

    if((did = H5Dopen2(fid, dsname, H5P_DEFAULT)) >= 0) {
        strcpy(scalename, DS_1_NAME);
        strcat(scalename, "al");
        if(test_attach_scale(fid, did, scalename, DIM0) < 0)
            goto out;

        strcpy(scalename, DS_2_NAME);
        strcat(scalename, "al");
        if(test_attach_scale(fid, did, scalename, DIM1) < 0)
            goto out;

        strcpy(scalename, DS_3_NAME);
        strcat(scalename, "al");
        if(test_attach_scale(fid, did, scalename, DIM2) < 0)
            goto out;

        strcpy(scalename, DS_4_NAME);
        strcat(scalename, "al");
        if(test_attach_scale(fid, did, scalename, DIM3) < 0)
            goto out;

        if(H5Dclose(did) < 0)
            goto out;
    }
    else
        goto out;

    PASSED();

    H5Fclose(fid);
    return SUCCEED;

out:
    H5E_BEGIN_TRY  {
        H5Dclose(did);
        H5Fclose(fid);
    } H5E_END_TRY;

    H5_FAILED();

    return FAIL;
}

static int test_duplicatelong_attachscales(const char *fileext)
{
    hid_t   fid = -1;
    hid_t   did = -1;
    char    dsname[32];
    char    scalename[32];
    strcpy(dsname, DATASET_NAME);
    strcat(dsname, "al2");

    TESTING2("test_duplicatelong_attachscales");

    if((fid = open_test_file(fileext)) < 0)
        goto out;

    /* make a dataset 2 */
    if(create_long_dataset(fid, dsname, "al2", 0) < 0)
        goto out;

    if((did = H5Dopen2(fid, dsname, H5P_DEFAULT)) >= 0) {
        strcpy(scalename, DS_1_NAME);
        strcat(scalename, "al");
        if(test_attach_scale(fid, did, scalename, DIM0) < 0)
            goto out;

        strcpy(scalename, DS_2_NAME);
        strcat(scalename, "al");
        if(test_attach_scale(fid, did, scalename, DIM1) < 0)
            goto out;

        strcpy(scalename, DS_3_NAME);
        strcat(scalename, "al");
        if(test_attach_scale(fid, did, scalename, DIM2) < 0)
            goto out;

        strcpy(scalename, DS_4_NAME);
        strcat(scalename, "al");
        if(test_attach_scale(fid, did, scalename, DIM3) < 0)
            goto out;

        if(H5Dclose(did) < 0)
            goto out;
    }
    else
        goto out;

    PASSED();

    H5Fclose(fid);
    return SUCCEED;

out:
    H5E_BEGIN_TRY  {
        H5Dclose(did);
        H5Fclose(fid);
    } H5E_END_TRY;

    H5_FAILED();

    return FAIL;
}

static int test_float_attachscales(const char *fileext)
{
    hid_t   fid = -1;
    hid_t   did = -1;
    char    dsname[32];
    char    scalename[32];
    strcpy(dsname, DATASET_NAME);
    strcat(dsname, "af");

    TESTING2("test_float_attachscales");

    if((fid = open_test_file(fileext)) < 0)
        goto out;

    /* make a dataset */
    if(create_float_dataset(fid, "af", 1) < 0)
        goto out;

    if((did = H5Dopen2(fid, dsname, H5P_DEFAULT)) >= 0) {
        strcpy(scalename, DS_1_NAME);
        strcat(scalename, "af");
        if(test_attach_scale(fid, did, scalename, DIM0) < 0)
            goto out;

        strcpy(scalename, DS_11_NAME);
        strcat(scalename, "af");
        if(test_attach_scale(fid, did, scalename, DIM0) < 0)
            goto out;

        strcpy(scalename, DS_2_NAME);
        strcat(scalename, "af");
        if(test_attach_scale(fid, did, scalename, DIM1) < 0)
            goto out;

        strcpy(scalename, DS_21_NAME);
        strcat(scalename, "af");
        if(test_attach_scale(fid, did, scalename, DIM1) < 0)
            goto out;

        strcpy(scalename, DS_22_NAME);
        strcat(scalename, "af");
        if(test_attach_scale(fid, did, scalename, DIM1) < 0)
            goto out;

        if(H5Dclose(did) < 0)
            goto out;
    }
    else
        goto out;

    PASSED();

    H5Fclose(fid);
    return SUCCEED;

out:
    H5E_BEGIN_TRY  {
        H5Dclose(did);
        H5Fclose(fid);
    } H5E_END_TRY;

    H5_FAILED();

    return FAIL;
}

static int test_numberofscales(const char *fileext)
{
    hid_t   fid = -1;
    hid_t   did = -1;
    int     nscales;                                          /* number of scales in DIM */
    char dsname[32];
    char scalename[32];
    strcpy(dsname, DATASET_NAME);
    strcat(dsname, "a");

    TESTING2("test_numberofscales");

    if((fid = open_test_file(fileext)) < 0)
        goto out;

    if((did = H5Dopen2(fid, dsname, H5P_DEFAULT)) >= 0) {
        if((nscales = H5DSget_num_scales(did, 0)) < 0)
            goto out;

        if(nscales != 2)
            goto out;

        if((nscales = H5DSget_num_scales(did, 1)) < 0)
            goto out;

        if(nscales != 3)
            goto out;

        if(H5Dclose(did) < 0)
            goto out;
    }
    else
        goto out;

    strcpy(dsname, DATASET_NAME);
    strcat(dsname, "b");

    /* make a dataset */
    if(create_int_dataset(fid, "b", 1) < 0)
        goto out;

    /* make a DS dataset for the first dimension */
    if((did = H5Dopen2(fid, dsname, H5P_DEFAULT)) >= 0) {
        strcpy(scalename, DS_1_NAME);
        strcat(scalename, "b");
        if(test_attach_scale(fid, did, scalename, DIM0) < 0)
            goto out;

        if((nscales = H5DSget_num_scales(did, 0)) < 0)
            goto out;

        if(nscales != 1)
            goto out;

        if((nscales = H5DSget_num_scales(did, 1)) < 0)
            goto out;

        if(nscales != 0)
            goto out;

        if(H5Dclose(did) < 0)
            goto out;
    }
    else
        goto out;

    PASSED();

    H5Fclose(fid);
    return SUCCEED;

out:
    H5E_BEGIN_TRY  {
        H5Dclose(did);
        H5Fclose(fid);
    } H5E_END_TRY;

    H5_FAILED();

    return FAIL;
}

static int test_char_scalenames(const char *fileext) {
    hid_t   fid = -1;
    hid_t   did = -1;
    char    dsname[32];
    char    scalename[32];
    char    name[32];
    strcpy(dsname, DATASET_NAME);
    strcat(dsname, "ac");

    if((fid = open_test_file(fileext)) < 0)
        goto out;

    TESTING2("set char scale/cmp scale name");
    if((did = H5Dopen2(fid, dsname, H5P_DEFAULT)) >= 0) {
        strcpy(scalename, DS_1_NAME);
        strcat(scalename, "ac");
        strcpy(name, SCALE_1_NAME);
        strcat(name, "ac");
        if(test_set_scalename(fid, did, scalename, name, DIM0) < 0)
            goto out;

        if(test_cmp_scalename(fid, did, scalename, name, DIM0) < 0)
            goto out;

        strcpy(scalename, DS_2_NAME);
        strcat(scalename, "ac");
        strcpy(name, SCALE_2_NAME);
        strcat(name, "ac");
        if(test_set_scalename(fid, did, scalename, name, DIM1) < 0)
            goto out;

        if(test_cmp_scalename(fid, did, scalename, name, DIM1) < 0)
            goto out;

        strcpy(scalename, DS_3_NAME);
        strcat(scalename, "ac");
        strcpy(name, SCALE_3_NAME);
        strcat(name, "ac");
        if(test_set_scalename(fid, did, scalename, name, DIM2) < 0)
            goto out;

        if(test_cmp_scalename(fid, did, scalename, name, DIM2) < 0)
            goto out;

        if(H5Dclose(did) < 0)
            goto out;
    }
    else
        goto out;

    PASSED();

    H5Fclose(fid);
    return SUCCEED;

out:
    H5E_BEGIN_TRY  {
        H5Dclose(did);
        H5Fclose(fid);
    } H5E_END_TRY;

    H5_FAILED();

    return FAIL;
}

static int test_short_scalenames(const char *fileext) {
    hid_t   fid = -1;
    hid_t   did = -1;
    char    dsname[32];
    char    scalename[32];
    char    name[32];
    strcpy(dsname, DATASET_NAME);
    strcat(dsname, "as");

    if((fid = open_test_file(fileext)) < 0)
        goto out;

    TESTING2("set short scale/cmp scale name");
    if((did = H5Dopen2(fid, dsname, H5P_DEFAULT)) >= 0) {
        strcpy(scalename, DS_1_NAME);
        strcat(scalename, "as");
        strcpy(name, SCALE_1_NAME);
        strcat(name, "as");
        if(test_set_scalename(fid, did, scalename, name, DIM0) < 0)
            goto out;

        if(test_cmp_scalename(fid, did, scalename, name, DIM0) < 0)
            goto out;

        strcpy(scalename, DS_11_NAME);
        strcat(scalename, "as");
        strcpy(name, SCALE_11_NAME);
        strcat(name, "as");
        if(test_set_scalename(fid, did, scalename, name, DIM0) < 0)
            goto out;

        if(test_cmp_scalename(fid, did, scalename, name, DIM0) < 0)
            goto out;

        strcpy(scalename, DS_2_NAME);
        strcat(scalename, "as");
        strcpy(name, SCALE_2_NAME);
        strcat(name, "as");
        if(test_set_scalename(fid, did, scalename, name, DIM1) < 0)
            goto out;

        if(test_cmp_scalename(fid, did, scalename, name, DIM1) < 0)
            goto out;

        strcpy(scalename, DS_21_NAME);
        strcat(scalename, "as");
        strcpy(name, SCALE_21_NAME);
        strcat(name, "as");
        if(test_set_scalename(fid, did, scalename, name, DIM1) < 0)
            goto out;

        if(test_cmp_scalename(fid, did, scalename, name, DIM1) < 0)
            goto out;

        strcpy(scalename, DS_22_NAME);
        strcat(scalename, "as");
        strcpy(name, SCALE_22_NAME);
        strcat(name, "as");
        if(test_set_scalename(fid, did, scalename, name, DIM1) < 0)
            goto out;

        if(test_cmp_scalename(fid, did, scalename, name, DIM1) < 0)
            goto out;

        strcpy(scalename, DS_3_NAME);
        strcat(scalename, "as");
        strcpy(name, SCALE_3_NAME);
        strcat(name, "as");
        if(test_set_scalename(fid, did, scalename, name, DIM2) < 0)
            goto out;

        if(test_cmp_scalename(fid, did, scalename, name, DIM2) < 0)
            goto out;

        strcpy(scalename, DS_31_NAME);
        strcat(scalename, "as");
        strcpy(name, SCALE_31_NAME);
        strcat(name, "as");
        if(test_set_scalename(fid, did, scalename, name, DIM2) < 0)
            goto out;

        if(test_cmp_scalename(fid, did, scalename, name, DIM2) < 0)
            goto out;

        strcpy(scalename, DS_32_NAME);
        strcat(scalename, "as");
        strcpy(name, SCALE_32_NAME);
        strcat(name, "as");
        if(test_set_scalename(fid, did, scalename, name, DIM2) < 0)
            goto out;

        if(test_cmp_scalename(fid, did, scalename, name, DIM2) < 0)
            goto out;

        strcpy(scalename, DS_33_NAME);
        strcat(scalename, "as");
        strcpy(name, SCALE_33_NAME);
        strcat(name, "as");
        if(test_set_scalename(fid, did, scalename, name, DIM2) < 0)
            goto out;

        if(test_cmp_scalename(fid, did, scalename, name, DIM2) < 0)
            goto out;

        if(H5Dclose(did) < 0)
            goto out;
    }
    else
        goto out;

    PASSED();

    H5Fclose(fid);
    return SUCCEED;

out:
    H5E_BEGIN_TRY  {
        H5Dclose(did);
        H5Fclose(fid);
    } H5E_END_TRY;

    H5_FAILED();

    return FAIL;
}

static int test_int_scalenames(const char *fileext) {
    hid_t   fid = -1;
    hid_t   did = -1;
    char    dsname[32];
    char    scalename[32];
    char    name[32];
    strcpy(dsname, DATASET_NAME);
    strcat(dsname, "a");

    if((fid = open_test_file(fileext)) < 0)
        goto out;

    TESTING2("set int scale/cmp scale name");
    if((did = H5Dopen2(fid, dsname, H5P_DEFAULT)) >= 0) {
        strcpy(scalename, DS_1_NAME);
        strcat(scalename, "a");
        strcpy(name, SCALE_1_NAME);
        strcat(name, "a");
        if(test_set_scalename(fid, did, scalename, name, DIM0) < 0)
            goto out;

        if(test_cmp_scalename(fid, did, scalename, name, DIM0) < 0)
            goto out;

        strcpy(scalename, DS_11_NAME);
        strcat(scalename, "a");
        strcpy(name, SCALE_11_NAME);
        strcat(name, "a");
        if(test_set_scalename(fid, did, scalename, name, DIM0) < 0)
            goto out;

        if(test_cmp_scalename(fid, did, scalename, name, DIM0) < 0)
            goto out;

        strcpy(scalename, DS_2_NAME);
        strcat(scalename, "a");
        strcpy(name, SCALE_2_NAME);
        strcat(name, "a");
        if(test_set_scalename(fid, did, scalename, name, DIM1) < 0)
            goto out;

        if(test_cmp_scalename(fid, did, scalename, name, DIM1) < 0)
            goto out;

        strcpy(scalename, DS_21_NAME);
        strcat(scalename, "a");
        strcpy(name, SCALE_21_NAME);
        strcat(name, "a");
        if(test_set_scalename(fid, did, scalename, name, DIM1) < 0)
            goto out;

        if(test_cmp_scalename(fid, did, scalename, name, DIM1) < 0)
            goto out;

        strcpy(scalename, DS_22_NAME);
        strcat(scalename, "a");
        strcpy(name, SCALE_22_NAME);
        strcat(name, "a");
        if(test_set_scalename(fid, did, scalename, name, DIM1) < 0)
            goto out;

        if(test_cmp_scalename(fid, did, scalename, name, DIM1) < 0)
            goto out;

        if(H5Dclose(did) < 0)
            goto out;
    }
    else
        goto out;

    PASSED();

    H5Fclose(fid);
    return 0;

out:
    H5E_BEGIN_TRY  {
        H5Dclose(did);
        H5Fclose(fid);
    } H5E_END_TRY;

    H5_FAILED();

    return FAIL;
}

static int test_long_scalenames(const char *fileext) {
    hid_t   fid = -1;
    hid_t   did = -1;
    char    dsname[32];
    char    scalename[32];
    char    name[32];
    strcpy(dsname, DATASET_NAME);
    strcat(dsname, "al");

    if((fid = open_test_file(fileext)) < 0)
        goto out;

    TESTING2("set long scale/cmp scale name");
    if((did = H5Dopen2(fid, dsname, H5P_DEFAULT)) >= 0) {
        strcpy(scalename, DS_1_NAME);
        strcat(scalename, "al");
        strcpy(name, SCALE_1_NAME);
        strcat(name, "al");
        if(test_set_scalename(fid, did, scalename, name, DIM0) < 0)
            goto out;

        if(test_cmp_scalename(fid, did, scalename, name, DIM0) < 0)
            goto out;

        strcpy(scalename, DS_2_NAME);
        strcat(scalename, "al");
        strcpy(name, SCALE_2_NAME);
        strcat(name, "al");
        if(test_set_scalename(fid, did, scalename, name, DIM1) < 0)
            goto out;

        if(test_cmp_scalename(fid, did, scalename, name, DIM1) < 0)
            goto out;

        strcpy(scalename, DS_3_NAME);
        strcat(scalename, "al");
        strcpy(name, SCALE_3_NAME);
        strcat(name, "al");
        if(test_set_scalename(fid, did, scalename, name, DIM2) < 0)
            goto out;

        if(test_cmp_scalename(fid, did, scalename, name, DIM2) < 0)
            goto out;

        strcpy(scalename, DS_4_NAME);
        strcat(scalename, "al");
        strcpy(name, SCALE_4_NAME);
        strcat(name, "al");
        if(test_set_scalename(fid, did, scalename, name, DIM3) < 0)
            goto out;

        if(test_cmp_scalename(fid, did, scalename, name, DIM3) < 0)
            goto out;

        if(H5Dclose(did) < 0)
            goto out;
    }
    else
        goto out;

    PASSED();

    H5Fclose(fid);
    return SUCCEED;

out:
    H5E_BEGIN_TRY  {
        H5Dclose(did);
        H5Fclose(fid);
    } H5E_END_TRY;

    H5_FAILED();

    return FAIL;
}

static int test_samelong_scalenames(const char *fileext) {
    hid_t   fid = -1;
    hid_t   did = -1;
    char    dsname[32];
    char    scalename[32];
    char    name[32];
    strcpy(dsname, DATASET_NAME);
    strcat(dsname, "al2");

    if((fid = open_test_file(fileext)) < 0)
        goto out;

    TESTING2("set same long scale/cmp scale name");
    if((did = H5Dopen2(fid, dsname, H5P_DEFAULT)) >= 0) {
        strcpy(scalename, DS_1_NAME);
        strcat(scalename, "al");
        strcpy(name, DS_1_NAME);
        strcat(name, "al");
        if(test_set_scalename(fid, did, scalename, name, DIM0) < 0)
            goto out;

        if(test_cmp_scalename(fid, did, scalename, name, DIM0) < 0)
            goto out;

        strcpy(scalename, DS_2_NAME);
        strcat(scalename, "al");
        strcpy(name, DS_2_NAME);
        strcat(name, "al");
        if(test_set_scalename(fid, did, scalename, name, DIM1) < 0)
            goto out;

        if(test_cmp_scalename(fid, did, scalename, name, DIM1) < 0)
            goto out;

        strcpy(scalename, DS_3_NAME);
        strcat(scalename, "al");
        strcpy(name, DS_3_NAME);
        strcat(name, "al");
        if(test_set_scalename(fid, did, scalename, name, DIM2) < 0)
            goto out;

        if(test_cmp_scalename(fid, did, scalename, name, DIM2) < 0)
            goto out;

        strcpy(scalename, DS_4_NAME);
        strcat(scalename, "al");
        strcpy(name, DS_4_NAME);
        strcat(name, "al");
        if(test_set_scalename(fid, did, scalename, name, DIM3) < 0)
            goto out;

        if(test_cmp_scalename(fid, did, scalename, name, DIM3) < 0)
            goto out;

        if(H5Dclose(did) < 0)
            goto out;
    }
    else
        goto out;

    PASSED();

    H5Fclose(fid);
    return SUCCEED;

out:
    H5E_BEGIN_TRY  {
        H5Dclose(did);
        H5Fclose(fid);
    } H5E_END_TRY;

    H5_FAILED();

    return FAIL;
}

static int test_float_scalenames(const char *fileext) {
    hid_t   fid = -1;
    hid_t   did = -1;
    char    dsname[32];
    char    scalename[32];
    char    name[32];
    strcpy(dsname, DATASET_NAME);
    strcat(dsname, "af");

    if((fid = open_test_file(fileext)) < 0)
        goto out;

    TESTING2("set float scale/cmp scale name");
    if((did = H5Dopen2(fid, dsname, H5P_DEFAULT)) >= 0) {
        strcpy(scalename, DS_1_NAME);
        strcat(scalename, "af");
        strcpy(name, SCALE_1_NAME);
        strcat(name, "af");
        if(test_set_scalename(fid, did, scalename, name, DIM0) < 0)
            goto out;

        if(test_cmp_scalename(fid, did, scalename, name, DIM0) < 0)
            goto out;

        strcpy(scalename, DS_11_NAME);
        strcat(scalename, "af");
        strcpy(name, SCALE_11_NAME);
        strcat(name, "af");
        if(test_set_scalename(fid, did, scalename, name, DIM0) < 0)
            goto out;

        if(test_cmp_scalename(fid, did, scalename, name, DIM0) < 0)
            goto out;

        strcpy(scalename, DS_2_NAME);
        strcat(scalename, "af");
        strcpy(name, SCALE_2_NAME);
        strcat(name, "af");
        if(test_set_scalename(fid, did, scalename, name, DIM1) < 0)
            goto out;

        if(test_cmp_scalename(fid, did, scalename, name, DIM1) < 0)
            goto out;

        strcpy(scalename, DS_21_NAME);
        strcat(scalename, "af");
        strcpy(name, SCALE_21_NAME);
        strcat(name, "af");
        if(test_set_scalename(fid, did, scalename, name, DIM1) < 0)
            goto out;

        if(test_cmp_scalename(fid, did, scalename, name, DIM1) < 0)
            goto out;

        strcpy(scalename, DS_22_NAME);
        strcat(scalename, "af");
        strcpy(name, SCALE_22_NAME);
        strcat(name, "af");
        if(test_set_scalename(fid, did, scalename, name, DIM1) < 0)
            goto out;

        if(test_cmp_scalename(fid, did, scalename, name, DIM1) < 0)
            goto out;

        if(H5Dclose(did) < 0)
            goto out;
    }
    else
        goto out;

    PASSED();

    H5Fclose(fid);
    return 0;

out:
    H5E_BEGIN_TRY  {
        H5Dclose(did);
        H5Fclose(fid);
    } H5E_END_TRY;

    H5_FAILED();

    return FAIL;
}

static int test_foreign_scaleattached(const char *fileforeign)
{
    herr_t  ret_value = FAIL;
    hid_t   fid = -1;
    hid_t   did = -1;
    hid_t   dsid = -1;
    char  *srcdir = getenv("srcdir"); /* the source directory */
    char  filename[512]="";          /* buffer to hold name of existing file */

    /* compose the name of the file to open, using the srcdir, if appropriate */
    if (srcdir) {
        strcpy(filename,srcdir);
        strcat(filename,"/");
    }
    strcat(filename, fileforeign);

    TESTING2("test_foreign_scaleattached");

    if((fid = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        goto out;

    if((did = H5Dopen2(fid, "/dset_al", H5P_DEFAULT)) >= 0) {
        if((dsid = H5Dopen2(fid, "/ds_4_al", H5P_DEFAULT)) >= 0) {
            if(H5DSis_attached(did, dsid, 3) == 1) {
                ret_value = SUCCEED;
            }
            if(H5Dclose(dsid) < 0)
                goto out;
        }
        if(H5Dclose(did) < 0)
            goto out;
    }
    else
        goto out;

    if(ret_value == FAIL)
        goto out;

    PASSED();

    H5Fclose(fid);
    return 0;

out:
    H5E_BEGIN_TRY  {
        H5Dclose(did);
        H5Fclose(fid);
    } H5E_END_TRY;

    H5_FAILED();

    return FAIL;
}

static int test_simple(void)
{
    hid_t   fid = -1;
    hid_t   did = -1;
    hid_t   dsid = -1;
    hid_t   sid = -1;
    hid_t   gid = -1;
    int     rank = RANK;
    int     rankds = 1;
    hsize_t dims[RANK]  = {DIM1_SIZE,DIM2_SIZE};
    int     buf[DIM_DATA] = {1,2,3,4,5,6,7,8,9,10,11,12};
    hsize_t s1_dim[1]  = {DIM1_SIZE};
    hsize_t s2_dim[1]  = {DIM2_SIZE};
    char sname[30];
    char dname[30];
    int     s1_wbuf[DIM1_SIZE] = {10,20,30};
    int     s11_wbuf[DIM1_SIZE] = {10,100,300};
    int     s2_wbuf[DIM2_SIZE] = {100,200,300,400};
    int     s21_wbuf[DIM2_SIZE] = {10,20,30,40};
    int     s22_wbuf[DIM2_SIZE] = {5,10,50,300};
    char dim0_label[16];
    char dim1_label[16];
    char *dim0_labeld;
    char *dim1_labeld;
    char dim0_labels[3];
    char dim1_labels[3];
    ssize_t dim0_label_size;
    ssize_t dim1_label_size;
    unsigned int dim;
    int scale_idx;
    int nscales;
    ssize_t name_len;
    char    *name_out=NULL;
    char snames[3];
    int i, j;

    printf("Testing API functions\n");

    /*-------------------------------------------------------------------------
    * create a file for the test
    *-------------------------------------------------------------------------
    */

    /* create a file using default properties */
    if((fid=H5Fcreate(FILE1,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT)) < 0)
        goto out;

    /*-------------------------------------------------------------------------
    * create datasets: 1 "data" dataset and 4 dimension scales
    *-------------------------------------------------------------------------
    */

    /* make a dataset */
    if(H5LTmake_dataset_int(fid,"dset_a",rank,dims,buf) < 0)
        goto out;

    /* make a DS dataset for the first dimension */
    if(H5LTmake_dataset_int(fid,"ds_a_1",rankds,s1_dim,s1_wbuf) < 0)
        goto out;

    /* make a DS dataset with an alternate scale for the 2nd dimension  */
    if(H5LTmake_dataset_int(fid,"ds_a_11",rankds,s1_dim,s11_wbuf) < 0)
        goto out;

    /* make a DS dataset for the second dimension */
    if(H5LTmake_dataset_int(fid,"ds_a_2",rankds,s2_dim,s2_wbuf) < 0)
        goto out;

    /* make a DS dataset with an alternate scale for the 2nd dimension  */
    if(H5LTmake_dataset_int(fid,"ds_a_21",rankds,s2_dim,s21_wbuf) < 0)
        goto out;

    /* make a DS dataset with an alternate scale for the 2nd dimension  */
    if(H5LTmake_dataset_int(fid,"ds_a_22",rankds,s2_dim,s22_wbuf) < 0)
        goto out;

    /* get the dataset id for "dset_a" */
    if((did = H5Dopen2(fid,"dset_a", H5P_DEFAULT)) < 0)
        goto out;

    /*-------------------------------------------------------------------------
    * attach the DS_1_NAME dimension scale to "dset_a"
    *-------------------------------------------------------------------------
    */

    /* get the DS dataset id */
    if((dsid = H5Dopen2(fid,"ds_a_1", H5P_DEFAULT)) < 0)
        goto out;

    /* attach the DS_1_NAME dimension scale to "dset_a" at dimension 0 */
    if(H5DSattach_scale(did,dsid,DIM0) < 0)
        goto out;

    /* close DS id */
    if(H5Dclose(dsid) < 0)
        goto out;

    /*-------------------------------------------------------------------------
    * attach the DS_11_NAME dimension scale to "dset_a"
    *-------------------------------------------------------------------------
    */

    /* get the DS dataset id */
    if((dsid = H5Dopen2(fid,"ds_a_11", H5P_DEFAULT)) < 0)
        goto out;

    /* attach the DS_11_NAME dimension scale to "dset_a" at dimension 0 */
    if(H5DSattach_scale(did,dsid,DIM0) < 0)
        goto out;

    /* close DS id */
    if(H5Dclose(dsid) < 0)
        goto out;

    /*-------------------------------------------------------------------------
    * attach the DS_2_NAME dimension scale to "dset_a"
    *-------------------------------------------------------------------------
    */

    /* get the DS dataset id */
    if((dsid = H5Dopen2(fid,"ds_a_2", H5P_DEFAULT)) < 0)
        goto out;

    /* attach the "ds2" dimension scale to "dset_a" as the 2nd dimension  */
    if(H5DSattach_scale(did,dsid,DIM1) < 0)
        goto out;

    /* close DS id */
    if(H5Dclose(dsid) < 0)
        goto out;

    /*-------------------------------------------------------------------------
    *  attach the DS_21_NAME dimension scale to "dset_a"
    *-------------------------------------------------------------------------
    */

    /* get the DS dataset id */
    if((dsid = H5Dopen2(fid,"ds_a_21", H5P_DEFAULT)) < 0)
        goto out;

    /* attach the DS_21_NAME dimension scale to "dset_a" as the 2nd dimension  */
    if(H5DSattach_scale(did,dsid,DIM1) < 0)
        goto out;

    /* close DS id */
    if(H5Dclose(dsid) < 0)
        goto out;

    /*-------------------------------------------------------------------------
    *  attach the DS_22_NAME dimension scale to "dset_a"
    *-------------------------------------------------------------------------
    */

    /* get the DS dataset id */
    if((dsid = H5Dopen2(fid,"ds_a_22", H5P_DEFAULT)) < 0)
        goto out;

    /* attach the "ds22" dimension scale to "dset_a" as the 2nd dimension  */
    if(H5DSattach_scale(did,dsid,DIM1) < 0)
        goto out;

    /* close DS id */
    if(H5Dclose(dsid) < 0)
        goto out;

    /* close dataset ID of "dset_a" */
    if(H5Dclose(did) < 0)
        goto out;

    /*-------------------------------------------------------------------------
    * create datasets: 1 "data" dataset and 1 dimension scale
    *-------------------------------------------------------------------------
    */

    /* make a dataset */
    if(H5LTmake_dataset_int(fid,"dset_b",rank,dims,buf) < 0)
        goto out;

    /* make a DS dataset for the first dimension */
    if(H5LTmake_dataset_int(fid,"ds_b_1",rankds,s1_dim,s1_wbuf) < 0)
        goto out;

    /*-------------------------------------------------------------------------
    *  attach the scale to "dset_b"
    *-------------------------------------------------------------------------
    */

    if((did = H5Dopen2(fid,"dset_b", H5P_DEFAULT)) < 0)
        goto out;
    if((dsid = H5Dopen2(fid,"ds_b_1", H5P_DEFAULT)) < 0)
        goto out;
    if(H5DSattach_scale(did,dsid,0) < 0)
        goto out;
    if(H5Dclose(dsid) < 0)
        goto out;

    /* close dataset ID of "dset_b" */
    if(H5Dclose(did) < 0)
        goto out;


    TESTING2("detach scales ");
    if(H5LTmake_dataset_int(fid, "dset_c", rank, dims, buf) < 0)
        goto out;

    if(H5LTmake_dataset_int(fid, "ds_c_1", rankds, s1_dim, s1_wbuf) < 0)
        goto out;

    if(H5LTmake_dataset_int(fid, "ds_c_2", rankds, s2_dim, s2_wbuf) < 0)
        goto out;

    if(H5LTmake_dataset_int(fid, "ds_c_21", rankds, s2_dim, s2_wbuf) < 0)
        goto out;

    if(H5LTmake_dataset_int(fid, "ds_c_22", rankds, s2_dim, s2_wbuf) < 0)
        goto out;

    if((did = H5Dopen2(fid,"dset_c", H5P_DEFAULT)) < 0)
        goto out;
    if((dsid = H5Dopen2(fid,"ds_c_1", H5P_DEFAULT)) < 0)
        goto out;
    if(H5DSattach_scale(did, dsid, 0) < 0)
        goto out;

    if(H5Dclose(dsid) < 0)
        goto out;

    if((dsid = H5Dopen2(fid,"ds_c_2", H5P_DEFAULT)) < 0)
        goto out;
    if(H5DSattach_scale(did, dsid, 1) < 0)
        goto out;

    if(H5Dclose(dsid) < 0)
        goto out;

    if((dsid = H5Dopen2(fid,"ds_c_21", H5P_DEFAULT)) < 0)
        goto out;
    if(H5DSattach_scale(did, dsid, 1) < 0)
        goto out;

    if(H5Dclose(dsid) < 0)
        goto out;

    if((dsid = H5Dopen2(fid,"ds_c_22", H5P_DEFAULT)) < 0)
        goto out;
    if(H5DSattach_scale(did, dsid, 1) < 0)
        goto out;

    if(H5Dclose(dsid) < 0)
        goto out;

    if(H5Dclose(did) < 0)
        goto out;

    if((did = H5Dopen2(fid,"dset_c", H5P_DEFAULT)) < 0)
        goto out;
    if((nscales = H5DSget_num_scales(did, 0)) < 0)
        goto out;

    if(nscales != 1)
        goto out;

    if((nscales = H5DSget_num_scales(did, 1)) < 0)
        goto out;

    if(nscales != 3)
        goto out;

    if(H5Dclose(did) < 0)
        goto out;

    if((did = H5Dopen2(fid,"dset_c", H5P_DEFAULT)) < 0)
        goto out;
    if((dsid = H5Dopen2(fid,"ds_c_21", H5P_DEFAULT)) < 0)
        goto out;
    if(H5DSdetach_scale(did, dsid, 1) < 0)
        goto out;

    if(H5Dclose(dsid) < 0)
        goto out;

    if(H5Dclose(did) < 0)
        goto out;

    if((did = H5Dopen2(fid,"dset_c", H5P_DEFAULT)) < 0)
        goto out;
    if((nscales = H5DSget_num_scales(did, 1)) < 0)
        goto out;

    if(nscales != 2)
        goto out;

    if(H5Dclose(did) < 0)
        goto out;

    if((did = H5Dopen2(fid,"dset_c", H5P_DEFAULT)) < 0)
        goto out;
    if((dsid = H5Dopen2(fid,"ds_c_22", H5P_DEFAULT)) < 0)
        goto out;
    if(H5DSdetach_scale(did, dsid, 1) < 0)
        goto out;

    if(H5Dclose(dsid) < 0)
        goto out;

    if(H5Dclose(did) < 0)
        goto out;

    if((did = H5Dopen2(fid,"dset_c", H5P_DEFAULT)) < 0)
        goto out;
    if((nscales = H5DSget_num_scales(did, 1)) < 0)
        goto out;

    if(nscales != 1)
        goto out;

    if(H5Dclose(did) < 0)
        goto out;

    if((did = H5Dopen2(fid,"dset_c", H5P_DEFAULT)) < 0)
        goto out;
    if((dsid = H5Dopen2(fid,"ds_c_2", H5P_DEFAULT)) < 0)
        goto out;
    if(H5DSdetach_scale(did, dsid, 1) < 0)
        goto out;

    if(H5Dclose(dsid) < 0)
        goto out;

    if(H5Dclose(did) < 0)
        goto out;

    if((did = H5Dopen2(fid,"dset_c", H5P_DEFAULT)) < 0)
        goto out;
    if((nscales = H5DSget_num_scales(did, 1)) < 0)
        goto out;

    if(nscales != 0)
        goto out;

    if(H5Dclose(did) < 0)
        goto out;

    if(H5LTmake_dataset_int(fid,"dset_d",rank,dims,NULL) < 0)
        goto out;
    if(H5LTmake_dataset_int(fid,"ds_d_1",rankds,s1_dim,NULL) < 0)
        goto out;
    if(H5LTmake_dataset_int(fid,"ds_d_2",rankds,s2_dim,NULL) < 0)
        goto out;
    if((did = H5Dopen2(fid,"dset_d", H5P_DEFAULT)) < 0)
        goto out;
    if((dsid = H5Dopen2(fid,"ds_d_1", H5P_DEFAULT)) < 0)
        goto out;
    if(H5DSattach_scale(did, dsid, 0) < 0)
        goto out;

    if(H5Dclose(dsid) < 0)
        goto out;

    if((dsid = H5Dopen2(fid,"ds_d_2", H5P_DEFAULT)) < 0)
        goto out;
    if(H5DSattach_scale(did, dsid, 1) < 0)
        goto out;

    if(H5Dclose(dsid) < 0)
        goto out;

    if(H5Dclose(did) < 0)
        goto out;

    if((did = H5Dopen2(fid,"dset_d", H5P_DEFAULT)) < 0)
        goto out;
    if((dsid = H5Dopen2(fid,"ds_d_1", H5P_DEFAULT)) < 0)
        goto out;
    if(H5DSis_attached(did,dsid,DIM0)<=0)
        goto out;
    if(H5Dclose(dsid) < 0)
        goto out;

    if((dsid = H5Dopen2(fid,"ds_d_2", H5P_DEFAULT)) < 0)
        goto out;
    if(H5DSis_attached(did,dsid,DIM1)<=0)
        goto out;
    if(H5Dclose(dsid) < 0)
        goto out;

    if(H5Dclose(did) < 0)
        goto out;

    if((did = H5Dopen2(fid,"dset_d", H5P_DEFAULT)) < 0)
        goto out;
    if((dsid = H5Dopen2(fid,"ds_d_1", H5P_DEFAULT)) < 0)
        goto out;
    if(H5DSdetach_scale(did,dsid,DIM0) < 0)
        goto out;
    if(H5DSis_attached(did,dsid,DIM0)!=0)
        goto out;
    if(H5Dclose(dsid) < 0)
        goto out;

    if(H5Dclose(did) < 0)
        goto out;

    if((did = H5Dopen2(fid,"dset_d", H5P_DEFAULT)) < 0)
        goto out;
    if((dsid = H5Dopen2(fid,"ds_d_1", H5P_DEFAULT)) < 0)
        goto out;
    if(H5DSattach_scale(did,dsid,DIM0) < 0)
        goto out;
    if(H5DSis_attached(did,dsid,DIM0)!=1)
        goto out;
    if((nscales = H5DSget_num_scales(did,DIM0)) < 0)
        goto out;
    if(nscales != 1)
        goto out;

    if(H5Dclose(dsid) < 0)
        goto out;

    if(H5Dclose(did) < 0)
        goto out;

    if((did = H5Dopen2(fid,"dset_d", H5P_DEFAULT)) < 0)
        goto out;
    if((dsid = H5Dopen2(fid,"ds_d_2", H5P_DEFAULT)) < 0)
        goto out;
    if(H5DSdetach_scale(did,dsid,DIM1) < 0)
        goto out;
    if(H5DSdetach_scale(did,dsid,DIM1)==SUCCEED)
        goto out;
    if(H5DSis_attached(did,dsid,DIM1)!=0)
        goto out;
    if((nscales = H5DSget_num_scales(did,DIM1)) < 0)
        goto out;
    if(nscales != 0)
        goto out;

    if(H5Dclose(dsid) < 0)
        goto out;

    if(H5Dclose(did) < 0)
        goto out;

    if((did = H5Dopen2(fid,"dset_d", H5P_DEFAULT)) < 0)
        goto out;
    if((dsid = H5Dopen2(fid,"ds_d_2", H5P_DEFAULT)) < 0)
        goto out;
    if(H5DSattach_scale(did,dsid,DIM1) < 0)
        goto out;
    if(H5DSis_attached(did,dsid,DIM1)!=1)
        goto out;
    if((nscales = H5DSget_num_scales(did,DIM0)) < 0)
        goto out;
    if(nscales != 1)
        goto out;

    if(H5DSattach_scale(did,dsid,DIM1) < 0)
        goto out;
    if(H5DSis_attached(did,dsid,DIM1)!=1)
        goto out;
    if((nscales = H5DSget_num_scales(did,DIM0)) < 0)
        goto out;
    if(nscales != 1)
        goto out;

    if(H5Dclose(dsid) < 0)
        goto out;

    if(H5Dclose(did) < 0)
        goto out;

    if((gid = H5Gcreate2(fid, "grp", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;
    if((sid = H5Screate_simple(rank,dims,NULL)) < 0)
        goto out;
    for(i = 0; i < 5; i++) {
        sprintf(dname,"dset_%d",i);
        if((did = H5Dcreate2(gid, dname, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto out;
        sprintf(sname,"ds_%d",i);
        if((dsid = H5Dcreate2(gid, sname, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto out;
        if(H5DSset_scale(dsid,"scale") < 0)
            goto out;
        if(H5Dclose(dsid) < 0)
            goto out;
        if(H5Dclose(did) < 0)
            goto out;
    }
    for(i = 0; i < 5; i++) {
        sprintf(dname, "dset_%d", i);
        if((did = H5Dopen2(gid, dname, H5P_DEFAULT)) < 0)
            goto out;
        for(j = 0; j < 5; j++) {
            sprintf(sname, "ds_%d", j);
            if((dsid = H5Dopen2(gid, sname, H5P_DEFAULT)) < 0)
                goto out;
            if(H5DSattach_scale(did, dsid, DIM0) < 0)
                goto out;
            if(H5Dclose(dsid) < 0)
                goto out;
        }
        if(H5Dclose(did) < 0)
            goto out;
    }
    for(i = 0; i < 5; i++) {
        sprintf(dname, "dset_%d", i);
        if((did = H5Dopen2(gid, dname, H5P_DEFAULT)) < 0)
            goto out;
        for(j = 0; j < 5; j++) {
            sprintf(sname, "ds_%d", j);
            if((dsid = H5Dopen2(gid, sname, H5P_DEFAULT)) < 0)
                goto out;
            if(H5DSdetach_scale(did, dsid, DIM0) < 0)
                goto out;
            if(H5Dclose(dsid) < 0)
                goto out;
        }
        if(H5Dclose(did) < 0)
            goto out;
    }
    for(i=0; i<5; i++)
    {
        sprintf(dname,"dset_%d",i);
        if((did = H5Dopen2(gid,dname, H5P_DEFAULT)) < 0)
            goto out;
        for(j=0; j<5; j++)
        {
            sprintf(sname,"ds_%d",j);
            if((dsid = H5Dopen2(gid,sname, H5P_DEFAULT)) < 0)
                goto out;
            if(H5DSattach_scale(did,dsid,DIM0) < 0)
                goto out;
            if(H5Dclose(dsid) < 0)
                goto out;
        }
        if(H5Dclose(did) < 0)
            goto out;
    }
    if(H5Sclose(sid) < 0)
        goto out;

    if(H5Gclose(gid) < 0)
        goto out;

    if(H5LTmake_dataset_int(fid,"dset_e",rank,dims,NULL) < 0)
        goto out;
    if(H5LTmake_dataset_int(fid,"ds_e_1",rankds,s1_dim,NULL) < 0)
        goto out;
    if((did = H5Dopen2(fid,"dset_e", H5P_DEFAULT)) < 0)
        goto out;
    if((dsid = H5Dopen2(fid,"ds_e_1", H5P_DEFAULT)) < 0)
        goto out;
    if(H5DSattach_scale(did,dsid,DIM1) < 0)
        goto out;
    if(H5DSis_attached(did,dsid,DIM1)<=0)
        goto out;
    for(i=0; i<rank; i++)
    {
        if( i==1 )
        {
            if(H5DSdetach_scale(did,dsid,(unsigned)i) < 0)
                goto out;
        }
        else
        {
            if(H5DSdetach_scale(did,dsid,(unsigned)i)!=FAIL)
                goto out;
        }
    }
    if(H5Dclose(dsid) < 0)
        goto out;

    if(H5Dclose(did) < 0)
        goto out;

    PASSED();
    TESTING2("set/get label");
    if((did = H5Dopen2(fid,"dset_a", H5P_DEFAULT)) < 0)
        goto out;
    if(H5DSset_label(did,DIM0,DIM0_LABEL) < 0)
        goto out;
    if(H5DSset_label(did,DIM1,DIM1_LABEL) < 0)
        goto out;
    if(H5DSget_label(did,DIM0,dim0_label,sizeof(dim0_label)) < 0)
        goto out;
    if(H5DSget_label(did,DIM1,dim1_label,sizeof(dim1_label)) < 0)
        goto out;
    if(strcmp(DIM0_LABEL,dim0_label)!=0)
        goto out;
    if(strcmp(DIM1_LABEL,dim1_label)!=0)
        goto out;
    if((dim0_label_size=H5DSget_label(did,DIM0,NULL,(size_t)0)) < 0)
        goto out;
    if((dim1_label_size=H5DSget_label(did,DIM1,NULL,(size_t)0)) < 0)
        goto out;
    dim0_labeld = (char*)malloc(dim0_label_size * sizeof (char));
    dim1_labeld = (char*)malloc(dim1_label_size * sizeof (char));
    if( dim0_labeld==NULL || dim1_labeld==NULL)
        goto out;
    if(H5DSget_label(did,DIM0,dim0_labeld,(size_t)dim0_label_size) < 0)
        goto out;
    if(H5DSget_label(did,DIM1,dim1_labeld,(size_t)dim1_label_size) < 0)
        goto out;
    if(strncmp(DIM0_LABEL,dim0_labeld,(size_t)(dim0_label_size-1))!=0)
        goto out;
    if(strncmp(DIM1_LABEL,dim1_labeld,(size_t)(dim1_label_size-1))!=0)
        goto out;
    if(dim0_labeld)
    {
        free(dim0_labeld);
        dim0_labeld=NULL;
    }
    if(dim1_labeld)
    {
        free(dim1_labeld);
        dim1_labeld=NULL;
    }
    if(H5DSget_label(did,DIM0,dim0_labels,sizeof(dim0_labels)) < 0)
        goto out;
    if(H5DSget_label(did,DIM1,dim1_labels,sizeof(dim1_labels)) < 0)
        goto out;
    if(strncmp(DIM0_LABEL,dim0_label,sizeof(dim0_labels)-1)!=0)
        goto out;
    if(strncmp(DIM1_LABEL,dim1_label,sizeof(dim1_labels)-1)!=0)
        goto out;
    if(H5Dclose(did))
        goto out;

    PASSED();
    TESTING2("set scale/get scale name");
    if((dsid = H5Dopen2(fid,"ds_a_1", H5P_DEFAULT)) < 0)
        goto out;
    if(H5DSset_scale(dsid,"Latitude set 0") < 0)
        goto out;
    if((H5DSis_scale(dsid)) == 0)
        goto out;

    if((name_len=H5DSget_scale_name(dsid,NULL,(size_t)0)) < 0)
        goto out;
    name_out = (char*)malloc(name_len * sizeof (char));
    if(name_out == NULL)
        goto out;
    if(H5DSget_scale_name(dsid, name_out, (size_t)name_len) < 0)
        goto out;

    if(strcmp("Latitude set 0",name_out)!=0)
        goto out;
    if(name_out)
    {
        free(name_out);
        name_out=NULL;
    }
    if(H5DSget_scale_name(dsid, sname, sizeof (sname)) < 0)
        goto out;

    if(strcmp("Latitude set 0",sname)!=0)
        goto out;
    if(H5DSget_scale_name(dsid, snames, sizeof (snames)) < 0)
        goto out;

    if(strncmp("Latitude set 0",snames,sizeof(snames)-1)!=0)
        goto out;
    if(H5Dclose(dsid))
        goto out;

    if((dsid = H5Dopen2(fid,"ds_a_11", H5P_DEFAULT)) < 0)
        goto out;
    if(H5DSset_scale(dsid,"Latitude set 1") < 0)
        goto out;
    if(H5Dclose(dsid))
        goto out;

    if((dsid = H5Dopen2(fid,"ds_a_2", H5P_DEFAULT)) < 0)
        goto out;
    if(H5DSset_scale(dsid,"Longitude set 0") < 0)
        goto out;
    if(H5Dclose(dsid))
        goto out;

    if((dsid = H5Dopen2(fid,"ds_a_21", H5P_DEFAULT)) < 0)
        goto out;
    if(H5DSset_scale(dsid,"Longitude set 1") < 0)
        goto out;
    if(H5Dclose(dsid))
        goto out;

    if((dsid = H5Dopen2(fid,"ds_a_22", H5P_DEFAULT)) < 0)
        goto out;
    if(H5DSset_scale(dsid,"Longitude set 2") < 0)
        goto out;
    if(H5Dclose(dsid))
        goto out;

    PASSED();
    TESTING2("iterate scales");
    if((did = H5Dopen2(fid,"dset_a", H5P_DEFAULT)) < 0)
        goto out;
    dim = 0;
    if(H5DSiterate_scales(did,dim,NULL,verify_scale,NULL) < 0)
        goto out;
    dim = 1;
    scale_idx = 2;
    if(H5DSiterate_scales(did,dim,&scale_idx,verify_scale,NULL) < 0)
        goto out;
    if(H5Dclose(did) < 0)
        goto out;

    if((did = H5Dopen2(fid,"dset_a", H5P_DEFAULT)) < 0)
        goto out;
    dim = 0;
    if(H5DSiterate_scales(did,dim,NULL,read_scale,s1_wbuf) < 0)
        goto out;
    dim = 1;
    scale_idx = 2;
    if(H5DSiterate_scales(did, dim, &scale_idx, read_scale, s22_wbuf) < 0)
        goto out;

    if(H5Dclose(did) < 0)
        goto out;

    if((did = H5Dopen2(fid,"dset_a", H5P_DEFAULT)) < 0)
        goto out;
    if((sid = H5Dget_space(did)) < 0)
        goto out;

    if((rank = H5Sget_simple_extent_ndims(sid)) < 0)
        goto out;

    if(H5Sget_simple_extent_dims(sid,dims,NULL) < 0)
        goto out;
    {
        int match_size;
        int idx = 0;
        for(dim=0; dim<(unsigned)rank; dim++)
        {
            if((match_size=H5DSiterate_scales(did,dim,&idx,match_dim_scale,NULL)) < 0)
                goto out;

            /* "dset_a" was defined with all dimension scales size matching the size of its dimensions */
            if(match_size==0)
                goto out;

            /* both DS_1_NAME and DS_2_NAME are the on the first index */
            if(idx!=0)
                goto out;
        }
    }if(H5Dclose(did) < 0)
        goto out;

    if(H5Sclose(sid) < 0)
        goto out;

    if(H5LTmake_dataset_int(fid, "dset_f", rank, dims, buf) < 0)
        goto out;

    if(H5LTmake_dataset_int(fid,"ds_f_1",rankds,s1_dim,NULL) < 0)
        goto out;
    if(H5LTmake_dataset_int(fid, "ds_f_11", rankds, s1_dim, s1_wbuf) < 0)
        goto out;

    if(H5LTmake_dataset_int(fid,"ds_f_2",rankds,s2_dim,NULL) < 0)
        goto out;
    if((did = H5Dopen2(fid,"dset_f", H5P_DEFAULT)) < 0)
        goto out;
    if((dsid = H5Dopen2(fid,"ds_f_1", H5P_DEFAULT)) < 0)
        goto out;
    if(H5DSattach_scale(did,dsid,DIM0) < 0)
        goto out;
    if(H5Dclose(dsid) < 0)
        goto out;

    if((dsid = H5Dopen2(fid,"ds_f_11", H5P_DEFAULT)) < 0)
        goto out;
    if(H5DSattach_scale(did,dsid,DIM0) < 0)
        goto out;
    if(H5Dclose(dsid) < 0)
        goto out;

    if((dsid = H5Dopen2(fid,"ds_f_2", H5P_DEFAULT)) < 0)
        goto out;
    if(H5DSattach_scale(did,dsid,DIM1) < 0)
        goto out;
    if(H5Dclose(dsid) < 0)
        goto out;

    if(H5Dclose(did) < 0)
        goto out;

    if((did = H5Dopen2(fid,"dset_f", H5P_DEFAULT)) < 0)
        goto out;
    if((sid = H5Dget_space(did)) < 0)
        goto out;

    if((rank = H5Sget_simple_extent_ndims(sid)) < 0)
        goto out;

    if(H5Sget_simple_extent_dims(sid,dims,NULL) < 0)
        goto out;
    {
        int match_size;
        int idx;
        for(dim=0; dim<(unsigned)rank; dim++)
        {
            /* always start at 1st scale */
            idx=0;

            if((match_size=H5DSiterate_scales(did,dim,&idx,match_dim_scale,NULL)) < 0)
                goto out;

            /* "dset_e" was defined with :
            dim 0: 2 scales, first is empty
            dim 1: 1 scale, empty */
            switch(dim)
            {
            case 0: /* for DIM 0, we get a valid scale at IDX 1 */
                if(match_size!=1 && idx!=1)
                    goto out;
                break;
            case 1: /* for DIM 1, we get no valid scales */
                if(match_size!=0 && idx!=0)
                    goto out;
                break;
            default:
                assert(0);
                break;
            }/*switch*/
        }
    }if(H5Dclose(did) < 0)
        goto out;

    if(H5Sclose(sid) < 0)
        goto out;

    PASSED();
    H5Fclose(fid);
    return 0;
    out:
    H5E_BEGIN_TRY
    {
        H5Dclose(did);
        H5Dclose(dsid);
        H5Fclose(fid);
        H5Sclose(sid);
        H5Gclose(gid);
    } H5E_END_TRY;
    H5_FAILED();
    return FAIL;
}



/*-------------------------------------------------------------------------
 * Function: verify_scale
 *
 * Purpose: example operator function used by H5DSiterate_scales, used
 *  to verify that SCALE_ID refers to a valid DS dataset
 *
 * Return:
 * The return values from an operator are:
 * Zero causes the iterator to continue, returning zero when all group members have been processed.
 * Positive causes the iterator to immediately return that positive value, indicating
 *  short-circuit success. The iterator can be restarted at the next group member.
 * Negative causes the iterator to immediately return that value, indicating failure.
 *  The iterator can be restarted at the next group member.
 *
 *-------------------------------------------------------------------------
 */

static herr_t verify_scale(hid_t dset, unsigned dim, hid_t scale_id, void *visitor_data)
{
    /* define a default zero value for return. This will cause the iterator to continue */
    int ret = 0;

    /* unused */
    dset=dset;
    dim=dim;
    visitor_data=visitor_data;

    /* define a positive value for return value. This will cause the iterator to
    immediately return that positive value, indicating short-circuit success
    */

    /* the parameter DS dataset must be a valid DS dataset */
    if((H5DSis_scale(scale_id))==1)
    {
        ret = 1;
    }

    return ret;
}


/*-------------------------------------------------------------------------
 * Function: read_scale
 *
 * Purpose: example operator function used by H5DSiterate_scales, used
 *  to read data from DS dataset. compare read and write buffers
 *
 * Return:
 * The return values from an operator are:
 * Zero causes the iterator to continue, returning zero when all group members have been processed.
 * Positive causes the iterator to immediately return that positive value, indicating
 *  short-circuit success. The iterator can be restarted at the next group member.
 * Negative causes the iterator to immediately return that value, indicating failure.
 *  The iterator can be restarted at the next group member
 *
 *
 *-------------------------------------------------------------------------
 */

static herr_t read_scale(hid_t dset, unsigned dim, hid_t scale_id, void *visitor_data)
{
    int      ret = 0;   /* define a default zero value for return. This will cause the iterator to continue */
    hid_t    sid = -1;  /* space ID */
    hid_t    tid = -1;  /* file type ID */
    hid_t    mtid = -1; /* memory type ID */
    hssize_t nelmts;    /* number of data elements */
    char     *buf=NULL; /* data buffer */
    size_t   size;
    int      i;
    char     *data = (char*) visitor_data;

    /* unused */
    dset=dset;
    dim=dim;

    /* get space */
    if((sid = H5Dget_space(scale_id)) < 0)
        goto out;
    /* get type */
    if((tid = H5Dget_type(scale_id)) < 0)
        goto out;
    /* get size of the DS array */
    if((nelmts = H5Sget_simple_extent_npoints(sid)) < 0)
        goto out;
    /* get type */
    if((mtid=H5Tget_native_type(tid,H5T_DIR_DEFAULT)) < 0)
        goto out;
    /* get type size */
    if((size=H5Tget_size(mtid))==0)
        goto out;

    if(nelmts)
    {
        buf=(char *) malloc((size_t)(nelmts*size));
        if( buf==NULL)
            goto out;
        if(H5Dread(scale_id,mtid,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf) < 0)
            goto out;

        for(i=0; i<nelmts; i++)
        {
            if(buf[i] != data[i])
            {
                printf("read and write buffers differ\n");
                goto out;
            }
        }

    } /* if */

    if(H5Sclose(sid) < 0)
        goto out;
    if(H5Tclose(tid) < 0)
        goto out;
    if(H5Tclose(mtid) < 0)
        goto out;
    if(buf)
        free(buf);


    return ret;

    /* error zone */
out:
    H5E_BEGIN_TRY
    {
        H5Sclose(sid);
        H5Tclose(tid);
        H5Tclose(mtid);
        if(buf)
        {
            free(buf);
        }
    } H5E_END_TRY;

    return FAIL;
}


/*-------------------------------------------------------------------------
 * Function: match_dim_scale
 *
 * Purpose: example operator function used by H5DSiterate_scales, used
 *  to verify the the DSID scale size matches the dataset DIM size
 *
 * Return:
 * The return values from an operator are:
 * Zero causes the iterator to continue, returning zero when all group members have been processed.
 * Positive causes the iterator to immediately return that positive value, indicating
 *  short-circuit success. The iterator can be restarted at the next group member.
 * Negative causes the iterator to immediately return that value, indicating failure.
 *  The iterator can be restarted at the next group member.
 *
 *-------------------------------------------------------------------------
 */

static herr_t match_dim_scale(hid_t did, unsigned dim, hid_t dsid, void *visitor_data)
{
    int       ret = 0;              /* define a default zero value for return. This will cause the iterator to continue */
    hid_t     sid;                  /* space ID */
    hssize_t  nelmts;               /* size of a dimension scale array */
    hsize_t   dims[H5S_MAX_RANK];   /* dimensions of dataset */
    hsize_t   storage_size;

    /* Stop compiler from whining about "unused parameters" */
    visitor_data = visitor_data;

    /*-------------------------------------------------------------------------
    * get DID (dataset) space info
    *-------------------------------------------------------------------------
    */

    /* get dataset space */
    if((sid = H5Dget_space(did)) < 0)
        goto out;

    /* get dimensions of dataset */
    if(H5Sget_simple_extent_dims(sid,dims,NULL) < 0)
        goto out;

    /* close the dataspace id */
    if(H5Sclose(sid) < 0)
        goto out;

    /*-------------------------------------------------------------------------
    * get DSID (scale) space info
    *-------------------------------------------------------------------------
    */

    /* get the space for the scale */
    if((sid = H5Dget_space(dsid)) < 0)
        goto out;

    /* get size of the DS array */
    if((nelmts = H5Sget_simple_extent_npoints(sid)) < 0)
        goto out;

    /* close */
    if(H5Sclose(sid) < 0)
        goto out;

    /* the size of the DS array must match the dimension of the dataset */
    if(nelmts == (hssize_t)dims[dim])
        ret = 1;

    /* if the scale is empty assume it cannot be used */
    storage_size=H5Dget_storage_size(dsid);

    if(storage_size==0)
        ret = 0;

    return ret;

out:
    H5E_BEGIN_TRY
    {
        H5Sclose(sid);
    } H5E_END_TRY;
    return FAIL;
}


/*-------------------------------------------------------------------------
 * Function: op_continue
 *
 * Purpose: example operator function used by H5DSiterate_scales that continues
 *  iteration and increments visitor_data (Note: int*)
 *
 * Return:
 * The return values from an operator are:
 * Zero causes the iterator to continue, returning zero when all group members have been processed.
 * Positive causes the iterator to immediately return that positive value, indicating
 *  short-circuit success. The iterator can be restarted at the next group member.
 * Negative causes the iterator to immediately return that value, indicating failure.
 *  The iterator can be restarted at the next group member.
 *
 *-------------------------------------------------------------------------
 */

static herr_t op_continue(hid_t dset, unsigned dim, hid_t scale_id, void *visitor_data)
{
    /* Stop compiler from whining about "unused parameters" */
    dset = dset;
    dim = dim;
    scale_id = scale_id;

    if ( visitor_data != NULL )
    {
        (*(int *)visitor_data)++;
    }

    /* define a default zero value for return. This will cause the iterator to continue */
    return 0;
}

/*-------------------------------------------------------------------------
 * Function: op_stop
 *
 * Purpose: example operator function used by H5DSiterate_scales that stops
 *  iteration and increments visitor_data (Note: int*)
 *
 * Return:
 * The return values from an operator are:
 * Zero causes the iterator to continue, returning zero when all group members have been processed.
 * Positive causes the iterator to immediately return that positive value, indicating
 *  short-circuit success. The iterator can be restarted at the next group member.
 * Negative causes the iterator to immediately return that value, indicating failure.
 *  The iterator can be restarted at the next group member.
 *
 *-------------------------------------------------------------------------
 */

static herr_t op_stop(hid_t dset, unsigned dim, hid_t scale_id, void *visitor_data)
{
    /* Stop compiler from whining about "unused parameters" */
    dset = dset;
    dim = dim;
    scale_id = scale_id;

    if ( visitor_data != NULL )
    {
        (*(int *)visitor_data)++;
    }

    /* define a default 1 value for return. This will cause the iterator to stop */
    return 1;
}

/*-------------------------------------------------------------------------
 * test error conditions
 *-------------------------------------------------------------------------
 */

static int test_errors(void)
{
    hid_t   fid;                                              /* file ID */
    int     rank     = RANK;                                  /* rank of data dataset */
    int     rankds   = 1;                                     /* rank of DS dataset */
    hsize_t dims[RANK]  = {DIM1_SIZE,DIM2_SIZE};              /* size of data dataset */
    hsize_t s1_dim[1]  = {DIM1_SIZE};                         /* size of DS 1 dataset */
    hid_t   did = -1;                                         /* dataset ID */
    hid_t   dsid = -1;                                        /* scale ID */
    hid_t   gid = -1;                                         /* group ID */
    hid_t   sid = -1;                                         /* space ID */
    hid_t   sidds = -1;                                       /* space ID */
    hsize_t pal_dims[] = {9,3};

    printf("Testing error conditions\n");

    /*-------------------------------------------------------------------------
    * create a file, spaces, dataset and group ids
    *-------------------------------------------------------------------------
    */

    /* create a file using default properties */
    if((fid = H5Fcreate(FILE2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;
    /* create a group */
    if((gid = H5Gcreate2(fid, "grp", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;
    /* create the data space for the dataset */
    if((sid = H5Screate_simple(rank, dims, NULL)) < 0)
        goto out;
    /* create the data space for the scale */
    if((sidds = H5Screate_simple(rankds, s1_dim, NULL)) < 0)
        goto out;
    /* create a dataset */
    if((did = H5Dcreate2(fid, "dset_a", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;
    /* create a dataset for the scale */
    if((dsid = H5Dcreate2(fid, "ds_a", H5T_NATIVE_INT, sidds, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;

    /*-------------------------------------------------------------------------
    * attempt to attach a dataset to itself, it should fail
    *-------------------------------------------------------------------------
    */

    TESTING2("attach a dataset to itself");

    if(H5DSattach_scale(did, did, 0) == SUCCEED)
        goto out;

    PASSED();

    /*-------------------------------------------------------------------------
    * attempt to attach a group with a dataset, it should fail
    *-------------------------------------------------------------------------
    */
    TESTING2("attach a group with a dataset");

    if(H5DSattach_scale(gid,dsid,0)==SUCCEED)
        goto out;

    PASSED();

    /*-------------------------------------------------------------------------
    * attempt to attach a dataset with a group, it should fail
    *-------------------------------------------------------------------------
    */
    TESTING2("attach a dataset with a group");

    if(H5DSattach_scale(did,gid,0)==SUCCEED)
        goto out;

    PASSED();

    /*-------------------------------------------------------------------------
    * attempt to set scale for a group, it should fail
    *-------------------------------------------------------------------------
    */
    TESTING2("set scale for a group");

    if(H5DSset_scale(gid,"scale 1")==SUCCEED)
        goto out;

    PASSED();

    /*-------------------------------------------------------------------------
    * close IDs for this set
    *-------------------------------------------------------------------------
    */

    /* close */
    if(H5Dclose(dsid) < 0)
        goto out;
    if(H5Dclose(did) < 0)
        goto out;
    if(H5Sclose(sid) < 0)
        goto out;
    if(H5Sclose(sidds) < 0)
        goto out;
    if(H5Gclose(gid) < 0)
        goto out;


    /*-------------------------------------------------------------------------
    * try to attach a scale that has scales
    *-------------------------------------------------------------------------
    */

    TESTING2("attach a scale that has scales");

    /* create the data space for the scale */
    if((sidds = H5Screate_simple(rankds, s1_dim, NULL)) < 0)
        goto out;

    /* create a dataset "ds_b" for the scale */
    if((dsid = H5Dcreate2(fid, "ds_b", H5T_NATIVE_INT, sidds, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;

    /* open the previous written "ds_a" */
    if((did = H5Dopen2(fid,"ds_a", H5P_DEFAULT)) < 0)
        goto out;

    /* attach "ds_b" to "ds_a", valid */
    if(H5DSattach_scale(did, dsid, 0) < 0)
        goto out;

    /* close */
    if(H5Dclose(dsid) < 0)
        goto out;
    if(H5Dclose(did) < 0)
        goto out;
    if(H5Sclose(sidds) < 0)
        goto out;

    /* open the previous written "dset_a" */
    if((did = H5Dopen2(fid, "dset_a", H5P_DEFAULT)) < 0)
        goto out;

    /* open the previous written "ds_a" */
    if((dsid = H5Dopen2(fid, "ds_a", H5P_DEFAULT)) < 0)
        goto out;

    /* try to attach "ds_a" to "dset_a", not valid */
    if(H5DSattach_scale(did,dsid,0)==SUCCEED)
        goto out;

    /* close */
    if(H5Dclose(dsid) < 0)
        goto out;
    if(H5Dclose(did) < 0)
        goto out;

    /* open the previous written "ds_a" */
    if((did = H5Dopen2(fid,"ds_a", H5P_DEFAULT)) < 0)
        goto out;

    /* open the previous written "ds_b" */
    if((dsid = H5Dopen2(fid,"ds_b", H5P_DEFAULT)) < 0)
        goto out;

    /* detach "ds_b" to "ds_a" */
    if(H5DSdetach_scale(did,dsid,0) < 0)
        goto out;

    /* close */
    if(H5Dclose(dsid) < 0)
        goto out;
    if(H5Dclose(did) < 0)
        goto out;

    PASSED();

    /*-------------------------------------------------------------------------
    * try to attach a dataset that is a scale
    *-------------------------------------------------------------------------
    */

    TESTING2("attach to a dataset that is a scale");

    /* open the previous written "ds_b", that is a scale */
    if((dsid = H5Dopen2(fid,"ds_b", H5P_DEFAULT)) < 0)
        goto out;

    /* open the previous written "ds_a" */
    if((did = H5Dopen2(fid,"ds_a", H5P_DEFAULT)) < 0)
        goto out;

    /* try to attach "ds_a" to "ds_b", not valid */
    if(H5DSattach_scale(dsid,did,0)==SUCCEED)
        goto out;

    /* close */
    if(H5Dclose(dsid) < 0)
        goto out;
    if(H5Dclose(did) < 0)
        goto out;

    PASSED();

    /*-------------------------------------------------------------------------
    * try to attach a scale to an image, pallete or table
    *-------------------------------------------------------------------------
    */

    TESTING2("attach to a dataset that is a reserved class dataset");

    /* make an image */
    if(H5IMmake_image_8bit(fid,"image",(hsize_t)100,(hsize_t)50,NULL) < 0)
        goto out;

    /* make a palette */
    if(H5IMmake_palette(fid,"pallete",pal_dims,NULL) < 0)
        goto out;

    /* open the previous written "ds_b" */
    if((dsid = H5Dopen2(fid,"ds_b", H5P_DEFAULT)) < 0)
        goto out;

    /* open the image dataset */
    if((did = H5Dopen2(fid,"image", H5P_DEFAULT)) < 0)
        goto out;

    /* try to attach "ds_a" to the image, not valid */
    if(H5DSattach_scale(did,dsid,0)==SUCCEED)
        goto out;

    /* close */
    if(H5Dclose(dsid) < 0)
        goto out;
    if(H5Dclose(did) < 0)
        goto out;

    PASSED();

    /*-------------------------------------------------------------------------
    * is scale
    *-------------------------------------------------------------------------
    */

    TESTING2("is scale");

    /* open a non scale dataset */
    if((did = H5Dopen2(fid,"dset_a", H5P_DEFAULT)) < 0)
        goto out;

    /* verify that it is not a dimension scale dataset  */
    if((H5DSis_scale(did))==1)
        goto out;

    /* close */
    if(H5Dclose(did) < 0)
        goto out;

    /* open the group. */
    if((gid = H5Gopen2(fid, "grp", H5P_DEFAULT)) < 0)
        goto out;

    /* verify that it is not a dimension scale dataset  */
    if((H5DSis_scale(gid))==1)
        goto out;

    /* close */
    if(H5Gclose(gid) < 0)
        goto out;

    PASSED();


    /*-------------------------------------------------------------------------
    * detach
    *-------------------------------------------------------------------------
    */

    TESTING2("detach scale from dataset it is not attached to");

    /* open the previous written "ds_a" */
    if((dsid = H5Dopen2(fid,"ds_a", H5P_DEFAULT)) < 0)
        goto out;

    /* open the previous written "dset_a" */
    if((did = H5Dopen2(fid,"dset_a", H5P_DEFAULT)) < 0)
        goto out;

    /* try to detach "ds_a" from "dset_a" */
    if(H5DSdetach_scale(did,dsid,0)==SUCCEED)
        goto out;

    /* close */
    if(H5Dclose(dsid) < 0)
        goto out;
    if(H5Dclose(did) < 0)
        goto out;

    PASSED();


    /*-------------------------------------------------------------------------
    * detach
    *-------------------------------------------------------------------------
    */

    TESTING2("detach scale from group");

    /* open the previous written "ds_a" */
    if((dsid = H5Dopen2(fid,"ds_a", H5P_DEFAULT)) < 0)
        goto out;

    /* open the group. */
    if((gid = H5Gopen2(fid, "grp", H5P_DEFAULT)) < 0)
        goto out;

    /* try to detach "ds_a" from "grp" */
    if(H5DSdetach_scale(gid,dsid,0)==SUCCEED)
        goto out;

    /* close */
    if(H5Dclose(dsid) < 0)
        goto out;
    if(H5Gclose(gid) < 0)
        goto out;

    PASSED();


    /*-------------------------------------------------------------------------
    * detach
    *-------------------------------------------------------------------------
    */

    TESTING2("detach scale when scale is group");

    /* open the previous written "dset_a" */
    if((did = H5Dopen2(fid,"dset_a", H5P_DEFAULT)) < 0)
        goto out;

    /* open the group. */
    if((gid = H5Gopen2(fid, "grp", H5P_DEFAULT)) < 0)
        goto out;

    /* try to detach "grp" from "dset_a" */
    if(H5DSdetach_scale(did,gid,0)==SUCCEED)
        goto out;

    /* close */
    if(H5Dclose(did) < 0)
        goto out;
    if(H5Gclose(gid) < 0)
        goto out;

    PASSED();


    /* close */
    if(H5Fclose(fid) < 0)
        goto out;

    return 0;

    /* error zone */
out:
    H5E_BEGIN_TRY
    {
        H5Sclose(sid);
        H5Sclose(sidds);
        H5Dclose(did);
        H5Dclose(dsid);
        H5Gclose(gid);
        H5Fclose(fid);
    } H5E_END_TRY;
    H5_FAILED();
    return FAIL;
}



/*-------------------------------------------------------------------------
 * test iterators
 *-------------------------------------------------------------------------
 */

static int test_iterators(void)
{
    hid_t   fid;                                              /* file ID */
    int     rank     = RANK;                                  /* rank of data dataset */
    int     rankds   = 1;                                     /* rank of DS dataset */
    hsize_t dims[RANK]  = {DIM1_SIZE,DIM2_SIZE};              /* size of data dataset */
    hsize_t s1_dim[1]   = {DIM1_SIZE};                        /* size of DS 1 dataset */
    hid_t   gid = -1;                                         /* group ID */
    hid_t   did = -1;                                         /* dataset ID */
    hid_t   dsid = -1;                                        /* scale ID */
    char    dname[30];                                        /* dataset name */
    int     i;

    printf("Testing iterators\n");

    /*-------------------------------------------------------------------------
    * create a file, spaces, dataset and group ids
    *-------------------------------------------------------------------------
    */

    /* create a file using default properties */
    if((fid=H5Fcreate(FILE3,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT)) < 0)
        goto out;
    /* create a group */
    if((gid = H5Gcreate2(fid, "grp", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;
    /* close */
    if(H5Gclose(gid) < 0)
        goto out;
    /* make a dataset */
    if(H5LTmake_dataset_int(fid,"dset_a",rank,dims,NULL) < 0)
        goto out;
    /* make a DS dataset */
    if(H5LTmake_dataset_int(fid,"ds_a",rankds,s1_dim,NULL) < 0)
        goto out;

    /*-------------------------------------------------------------------------
    * iterate when the dataset has no scales
    *-------------------------------------------------------------------------
    */

    TESTING2("iterate when the dataset has no scales ");

    /* get the dataset id for "dset_a" */
    if((did = H5Dopen2(fid,"dset_a", H5P_DEFAULT)) < 0)
        goto out;

    /* try to iterate trough the 1st dimension of "dset_a", return error */
    if(H5DSiterate_scales(did,0,NULL,verify_scale,NULL) < 0)
        goto out;

    /* close */
    if(H5Dclose(did) < 0)
        goto out;

    PASSED();


    /*-------------------------------------------------------------------------
    * iterate on dimension that is outside the rank
    *-------------------------------------------------------------------------
    */

    TESTING2("iterate on dimension that is outside the rank ");

    /* get the dataset id for "dset_a" */
    if((did = H5Dopen2(fid,"dset_a", H5P_DEFAULT)) < 0)
        goto out;

    /* try to iterate trough the 3rd dimension of "dset_a", return error */
    if(H5DSiterate_scales(did,3,NULL,verify_scale,NULL)==SUCCEED)
        goto out;

    /* close */
    if(H5Dclose(did) < 0)
        goto out;

    PASSED();

    /*-------------------------------------------------------------------------
    * iterate for dimension with many scales
    *-------------------------------------------------------------------------
    */

    TESTING2("iterate for dimension with many scales ");

    /* open the previously written "dset_a" */
    if((did = H5Dopen2(fid,"dset_a", H5P_DEFAULT)) < 0)
        goto out;

    for(i=0; i<100; i++)
    {
        /* make a DS */
        sprintf(dname,"ds_%d",i);
        if(H5LTmake_dataset_int(fid,dname,rankds,s1_dim,NULL) < 0)
            goto out;
        /* open */
        if((dsid = H5Dopen2(fid,dname, H5P_DEFAULT)) < 0)
            goto out;
        /* attach */
        if(H5DSattach_scale(did,dsid,0) < 0)
            goto out;
        /* close */
        if(H5Dclose(dsid) < 0)
            goto out;
    }

    /* iterate trough the 1st dimension of "dset_a" */
    if(H5DSiterate_scales(did,0,NULL,op_continue,NULL) < 0)
        goto out;

    /* close */
    if(H5Dclose(did) < 0)
        goto out;

    PASSED();

    /*-------------------------------------------------------------------------
    * iterate on group
    *-------------------------------------------------------------------------
    */

    TESTING2("iterate on group ");

    /* open */
    if((gid = H5Gopen2(fid, "grp", H5P_DEFAULT)) < 0)
        goto out;

    /* try to iterate, return error */
    if(H5DSiterate_scales(gid,0,NULL,verify_scale,NULL)==SUCCEED)
        goto out;

    /* close */
    if(H5Gclose(gid) < 0)
        goto out;

    PASSED();


    /*-------------------------------------------------------------------------
    * iterate in deleted scales
    *-------------------------------------------------------------------------
    */

    TESTING2("iterate in deleted scales ");

    if(H5Ldelete(fid, "ds_0", H5P_DEFAULT) < 0)
        goto out;

    /* open the previously written "dset_a" */
    if((did = H5Dopen2(fid, "dset_a", H5P_DEFAULT)) < 0)
        goto out;

    /* iterate  */
    if(H5DSiterate_scales(did, 0, NULL, op_continue, NULL) == SUCCEED)
        goto out;

    /* close */
    if(H5Dclose(did) < 0)
        goto out;

    PASSED();


    /* close */
    if(H5Fclose(fid) < 0)
        goto out;

    return 0;

    /* error zone */
out:
    H5E_BEGIN_TRY
    {
        H5Gclose(gid);
        H5Dclose(did);
        H5Fclose(fid);
    } H5E_END_TRY;
    H5_FAILED();
    return FAIL;
}


/*-------------------------------------------------------------------------
 * test several ranks
 *-------------------------------------------------------------------------
 */

static int test_rank(void)
{
    hid_t   fid;                                              /* file ID */
    hid_t   did = -1;                                         /* dataset ID */
    hid_t   dsid = -1;                                        /* scale ID */
    hid_t   sid = -1;                                         /* space ID */
    hid_t   sidds = -1;                                       /* space ID */
    hsize_t dims1[1]  = {DIM1_SIZE};                          /* size of data dataset */
    hsize_t dims2[2]  = {DIM1_SIZE,DIM2_SIZE};                /* size of data dataset */
    hsize_t dims3[3]  = {DIM1_SIZE,DIM2_SIZE,DIM3_SIZE};      /* size of data dataset */
    hsize_t dimss[2]  = {1,1};                                /* size of data dataset */
    char    name[30];                                         /* dataset name buffer */
    char    names[30];                                        /* dataset scale name buffer */
    char    namel[30];                                        /* dataset label name buffer */
    int     bufi[1]={2};
    float   buff[1]={1};
    int     i;

    printf("Testing ranks\n");

    /*-------------------------------------------------------------------------
    * create a file, a dataset, scales
    *-------------------------------------------------------------------------
    */

    /* create a file using default properties */
    if((fid=H5Fcreate(FILE4,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT)) < 0)
        goto out;

    /* make a dataset a 3D data dataset */
    if(H5LTmake_dataset_int(fid,"dset_a",3,dims3,NULL) < 0)
        goto out;

    /* make a 1D scale dataset  */
    if(H5LTmake_dataset_int(fid,"ds_a_0",1,dims1,NULL) < 0)
        goto out;

    /* make a 2D scale dataset  */
    if(H5LTmake_dataset_int(fid,"ds_a_1",2,dims2,NULL) < 0)
        goto out;

    /* make a 3D scale dataset  */
    if(H5LTmake_dataset_int(fid,"ds_a_2",3,dims3,NULL) < 0)
        goto out;

    /*-------------------------------------------------------------------------
    * attach
    *-------------------------------------------------------------------------
    */

    TESTING2("attach");

    if((did = H5Dopen2(fid,"dset_a", H5P_DEFAULT)) < 0)
        goto out;

    for(i=0; i<3; i++)
    {
        sprintf(name,"ds_a_%d",i);
        if((dsid = H5Dopen2(fid,name, H5P_DEFAULT)) < 0)
            goto out;
        if(H5DSattach_scale(did,dsid,(unsigned)i) < 0)
            goto out;
        if(H5DSis_attached(did,dsid,(unsigned)i)<=0)
            goto out;
        if(H5Dclose(dsid) < 0)
            goto out;
    }

    if(H5Dclose(did) < 0)
        goto out;

    PASSED();


    /*-------------------------------------------------------------------------
    * detach
    *-------------------------------------------------------------------------
    */

    TESTING2("detach");

    if((did = H5Dopen2(fid,"dset_a", H5P_DEFAULT)) < 0)
        goto out;

    for(i=0; i<3; i++)
    {
        sprintf(name,"ds_a_%d",i);
        if((dsid = H5Dopen2(fid,name, H5P_DEFAULT)) < 0)
            goto out;
        if(H5DSdetach_scale(did,dsid,(unsigned)i) < 0)
            goto out;
        if(H5DSis_attached(did,dsid,(unsigned)i)!=0)
            goto out;
        if(H5Dclose(dsid) < 0)
            goto out;
    }
    if(H5Dclose(did) < 0)
        goto out;

    PASSED();

    /*-------------------------------------------------------------------------
    * attach, set, get names, labels
    *-------------------------------------------------------------------------
    */

    TESTING2("attach, set, get names, labels");

    if((did = H5Dopen2(fid,"dset_a", H5P_DEFAULT)) < 0)
        goto out;

    for(i=0; i<3; i++)
    {
        sprintf(name,"ds_a_%d",i);
        if((dsid = H5Dopen2(fid,name, H5P_DEFAULT)) < 0)
            goto out;
        if(H5DSset_scale(dsid,name) < 0)
            goto out;
        if(H5DSattach_scale(did,dsid,(unsigned)i) < 0)
            goto out;
        if(H5DSis_attached(did,dsid,(unsigned)i)<=0)
            goto out;
        if(H5DSget_scale_name(dsid,names,sizeof(names)) < 0)
            goto out;
        if(H5Dclose(dsid) < 0)
            goto out;
        if(H5DSset_label(did,(unsigned)i,name) < 0)
            goto out;
        if(H5DSget_label(did,(unsigned)i,namel,sizeof(namel)) < 0)
            goto out;
        if(strcmp(name,names)!=0)
            goto out;
        if(strcmp(name,namel)!=0)
            goto out;
    }

    if(H5Dclose(did) < 0)
        goto out;

    PASSED();

    /*-------------------------------------------------------------------------
    * attach a scalar scale
    *-------------------------------------------------------------------------
    */

    TESTING2("attach a scalar scale");

    /* create the data space for the dataset */
    if((sid = H5Screate_simple(2, dimss, NULL)) < 0)
        goto out;
    /* create a dataset of rank 2 */
    if((did = H5Dcreate2(fid, "dset_b", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;
    /* create a scalar space */
    if((sidds = H5Screate(H5S_SCALAR)) < 0)
        goto out;
    /* create a dataset of scalar rank for the scale */
    if((dsid = H5Dcreate2(fid, "ds_b_1", H5T_NATIVE_FLOAT, sidds, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;
    /* write */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, bufi) < 0)
        goto out;
    if(H5Dwrite(dsid, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buff) < 0)
        goto out;
    /* attach */
    if(H5DSattach_scale(did, dsid, 0) < 0)
        goto out;
    if(H5DSattach_scale(did, dsid, 1) < 0)
        goto out;
    /* close */
    if(H5Sclose(sid) < 0)
        goto out;
    if(H5Sclose(sidds) < 0)
        goto out;
    if(H5Dclose(did) < 0)
        goto out;
    if(H5Dclose(dsid) < 0)
        goto out;

    PASSED();

    /*-------------------------------------------------------------------------
    * close
    *-------------------------------------------------------------------------
    */
    if(H5Fclose(fid) < 0)
        goto out;

    return 0;

    /* error zone */
out:
    H5E_BEGIN_TRY
    {
        H5Dclose(did);
        H5Dclose(dsid);
        H5Sclose(sidds);
        H5Sclose(sid);
        H5Fclose(fid);
    } H5E_END_TRY;
    H5_FAILED();
    return FAIL;
}


/*-------------------------------------------------------------------------
 * attach scales with several datatypes
 *-------------------------------------------------------------------------
 */

static int test_types(void)
{
    hid_t          fid;                                              /* file ID */
    hid_t          did = -1;                                         /* dataset ID */
    hid_t          dsid = -1;                                        /* DS dataset ID */
    int            rank     = RANK;                                  /* rank of data dataset */
    int            rankds   = 1;                                     /* rank of DS dataset */
    hsize_t        dims[RANK]  = {DIM1_SIZE,DIM2_SIZE};              /* size of data dataset */
    int            buf[DIM_DATA] = {1,2,3,4,5,6,7,8,9,10,11,12};     /* data of data dataset */
    hsize_t        s1_dim[1]  = {DIM1_SIZE};                         /* size of DS 1 dataset */
    hsize_t        s2_dim[1]  = {DIM2_SIZE};                         /* size of DS 2 dataset */
    float          s1_float[DIM1_SIZE] = {10,20,30};                 /* data of DS 1 dataset */
    unsigned short s2_ushort[DIM2_SIZE] = {10,20,30,40};             /* data of DS 2 dataset */
    const char     *s1_str = "ABC";
    const char     *s2_str = "ABCD";

    printf("Testing scales with several datatypes\n");

    /*-------------------------------------------------------------------------
    * create a file for the test
    *-------------------------------------------------------------------------
    */
    /* create a file using default properties */
    if((fid=H5Fcreate(FILE5,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT)) < 0)
        goto out;

    /*-------------------------------------------------------------------------
    * create datasets: 1 "data" dataset and 2 dimension scales
    *-------------------------------------------------------------------------
    */

    /* make a dataset */
    if(H5LTmake_dataset_int(fid,"dset_a",rank,dims,buf) < 0)
        goto out;

    /* make a DS dataset for the first dimension */
    if(H5LTmake_dataset_float(fid,DS_1_NAME,rankds,s1_dim,s1_float) < 0)
        goto out;

    /* make a DS dataset for the second dimension */
    if(H5LTmake_dataset(fid,DS_2_NAME,rankds,s2_dim,H5T_NATIVE_USHORT,s2_ushort) < 0)
        goto out;

    /*-------------------------------------------------------------------------
    * floating point and short scales
    *-------------------------------------------------------------------------
    */

    TESTING2("floating point and short scales");

    /* get the dataset id for "dset_a" */
    if((did = H5Dopen2(fid,"dset_a", H5P_DEFAULT)) < 0)
        goto out;

    /* get the DS dataset id */
    if((dsid = H5Dopen2(fid,DS_1_NAME, H5P_DEFAULT)) < 0)
        goto out;
    /* attach the DS_1_NAME dimension scale to "dset_a" at dimension 0 */
    if(H5DSattach_scale(did,dsid,DIM0) < 0)
        goto out;
    /* set name */
    if(H5DSset_scale(dsid,SCALE_1_NAME) < 0)
        goto out;
    /* close DS id */
    if(H5Dclose(dsid) < 0)
        goto out;
    /* get the DS dataset id */
    if((dsid = H5Dopen2(fid,DS_2_NAME, H5P_DEFAULT)) < 0)
        goto out;
    /* attach the DS_2_NAME dimension scale to "dset_a" at dimension 1 */
    if(H5DSattach_scale(did,dsid,DIM1) < 0)
        goto out;
    /* set name */
    if(H5DSset_scale(dsid,SCALE_2_NAME) < 0)
        goto out;
    /* close DS id */
    if(H5Dclose(dsid) < 0)
        goto out;
    /* set a label */
    if(H5DSset_label(did,DIM0,DIM0_LABEL) < 0)
        goto out;
    if(H5DSset_label(did,DIM1,DIM1_LABEL) < 0)
        goto out;
    /* close */
    if(H5Dclose(did) < 0)
        goto out;

    PASSED();

    /*-------------------------------------------------------------------------
    * create datasets: 1 "data" dataset and 2 dimension scales
    *-------------------------------------------------------------------------
    */

    /* make a dataset */
    if(H5LTmake_dataset_int(fid,"dset_b",rank,dims,buf) < 0)
        goto out;

    /* make a DS dataset for the first dimension */
    if(H5LTmake_dataset_string(fid,"ds_b_1",s1_str) < 0)
        goto out;

    /* make a DS dataset for the second dimension */
    if(H5LTmake_dataset_string(fid,"ds_b_2",s2_str) < 0)
        goto out;

    /*-------------------------------------------------------------------------
    * floating point and short scales
    *-------------------------------------------------------------------------
    */

    TESTING2("string scales");

    /* get the dataset id for "dset_b" */
    if((did = H5Dopen2(fid,"dset_b", H5P_DEFAULT)) < 0)
        goto out;
    /* get the DS dataset id */
    if((dsid = H5Dopen2(fid,"ds_b_1", H5P_DEFAULT)) < 0)
        goto out;
    /* attach the DS_1_NAME dimension scale to "dset_b" at dimension 0 */
    if(H5DSattach_scale(did,dsid,DIM0) < 0)
        goto out;
    /* set name */
    if(H5DSset_scale(dsid,SCALE_1_NAME) < 0)
        goto out;
    /* close DS id */
    if(H5Dclose(dsid) < 0)
        goto out;
    /* get the DS dataset id */
    if((dsid = H5Dopen2(fid,"ds_b_2", H5P_DEFAULT)) < 0)
        goto out;
    /* attach the DS_2_NAME dimension scale to "dset_b" at dimension 1 */
    if(H5DSattach_scale(did,dsid,DIM1) < 0)
        goto out;
    /* set name */
    if(H5DSset_scale(dsid,SCALE_2_NAME) < 0)
        goto out;
    /* close DS id */
    if(H5Dclose(dsid) < 0)
        goto out;
    /* set a label */
    if(H5DSset_label(did,DIM0,DIM0_LABEL) < 0)
        goto out;
    if(H5DSset_label(did,DIM1,DIM1_LABEL) < 0)
        goto out;
    /* close */
    if(H5Dclose(did) < 0)
        goto out;

    PASSED();

    /*-------------------------------------------------------------------------
    * close
    *-------------------------------------------------------------------------
    */
    if(H5Fclose(fid) < 0)
        goto out;

    return 0;

    /* error zone */
out:
    H5E_BEGIN_TRY
    {
        H5Dclose(did);
        H5Dclose(dsid);
        H5Fclose(fid);
    } H5E_END_TRY;
    H5_FAILED();
    return FAIL;
}

/*-------------------------------------------------------------------------
 * read realistic data and generate an HDF5 file with dimension scales
 *-------------------------------------------------------------------------
 */

static int test_data(void)
{
    hid_t   fid;                         /* file ID */
    hid_t   did = -1;                    /* dataset ID */
    hid_t   dsid = -1;                   /* DS dataset ID */
    hid_t   dcpl;                        /* dataset creation property list */
    hid_t   sid;                         /* dataspace ID */
    float   *vals=NULL;                  /* array to hold data values */
    float   *latbuf=NULL;                /* array to hold the latitude values */
    float   *lonbuf=NULL;                /* array to hold the longitude values */
    hsize_t dims[2];                     /* array to hold dimensions */
    hsize_t latdims[1];                  /* array to hold dimensions */
    hsize_t londims[1];                  /* array to hold dimensions */
    float   fill=-99;                    /* fill value */


    printf("Testing reading ASCII data and generate HDF5 data with scales\n");

    /*-------------------------------------------------------------------------
    * create a file for the test
    *-------------------------------------------------------------------------
    */
    /* create a file using default properties */
    if((fid=H5Fcreate(FILE6,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT)) < 0)
        goto out;

    /*-------------------------------------------------------------------------
    * generating scales
    *-------------------------------------------------------------------------
    */

    TESTING2("generating scales");

    /*-------------------------------------------------------------------------
    * create datasets: 1 "data" dataset and 2 dimension scales
    *-------------------------------------------------------------------------
    */

    /* read the latitude */
    if(read_data("dslat.txt",1,latdims,&latbuf) < 0)
        goto out;

    /* make a DS dataset for the first dimension */
    if(H5LTmake_dataset_float(fid, "lat", 1, latdims, latbuf) < 0)
        goto out;

    free( latbuf );
    latbuf = NULL;

     /* read the longitude */
    if(read_data("dslon.txt",1,londims,&lonbuf) < 0)
        goto out;

    /* make a DS dataset for the second dimension */
    if(H5LTmake_dataset_float(fid, "lon", 1, londims, lonbuf) < 0)
        goto out;

    free( lonbuf );
    lonbuf = NULL;

    /* make a dataset for the data. a fill value is set */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto out;
    if(H5Pset_fill_value(dcpl, H5T_NATIVE_FLOAT, &fill) < 0)
        goto out;

    /* read ASCII bathymetry data and dimensions to create dataset */
    if(read_data("dsdata.txt",2,dims,&vals) < 0)
        goto out;

    if((sid = H5Screate_simple(2, dims, NULL)) < 0)
        goto out;
    if((did = H5Dcreate2(fid, "data", H5T_NATIVE_FLOAT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto out;
    if(H5Dwrite(did, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, vals) < 0)
        goto out;

    free ( vals );
    vals = NULL;

    if(H5Dclose(did) < 0)
        goto out;
    if(H5Pclose(dcpl) < 0)
        goto out;
    if(H5Sclose(sid) < 0)
        goto out;

    /*-------------------------------------------------------------------------
    * attach
    *-------------------------------------------------------------------------
    */

    /* get the dataset id for "data" */
    if((did = H5Dopen2(fid,"data", H5P_DEFAULT)) < 0)
        goto out;

    /* get the DS dataset id */
    if((dsid = H5Dopen2(fid,"lat", H5P_DEFAULT)) < 0)
        goto out;
    /* attach the DS_1_NAME dimension scale to "data" at dimension 0 */
    if(H5DSattach_scale(did,dsid,DIM0) < 0)
        goto out;
    /* set name */
    if(H5DSset_scale(dsid,SCALE_1_NAME) < 0)
        goto out;
    /* close DS id */
    if(H5Dclose(dsid) < 0)
        goto out;
    /* get the DS dataset id */
    if((dsid = H5Dopen2(fid,"lon", H5P_DEFAULT)) < 0)
        goto out;
    /* attach the DS_2_NAME dimension scale to "data" at dimension 1 */
    if(H5DSattach_scale(did,dsid,DIM1) < 0)
        goto out;
    /* set name */
    if(H5DSset_scale(dsid,SCALE_2_NAME) < 0)
        goto out;
    /* close DS id */
    if(H5Dclose(dsid) < 0)
        goto out;
    /* set a label */
    if(H5DSset_label(did,DIM0,DIM0_LABEL) < 0)
        goto out;
    if(H5DSset_label(did,DIM1,DIM1_LABEL) < 0)
        goto out;
    /* close */
    if(H5Dclose(did) < 0)
        goto out;

    PASSED();



    /*-------------------------------------------------------------------------
    * close
    *-------------------------------------------------------------------------
    */
    if(H5Fclose(fid) < 0)
        goto out;

    return 0;

    /* error zone */
out:
    H5E_BEGIN_TRY
    {
        H5Dclose(did);
        H5Dclose(dsid);
        H5Fclose(fid);
    } H5E_END_TRY;
    H5_FAILED();

    if (latbuf)
        free( latbuf );
    if (lonbuf)
        free( lonbuf );
    if (vals)
        free( vals );
    return FAIL;
}



/*-------------------------------------------------------------------------
 * read_data
 * utility function to read ASCII data
 * the files have a header of the type
 *
 *   dimension i
 *   n
 *
 * followed by the data
 *
 *-------------------------------------------------------------------------
 */

static int read_data( const char* fname,
                      int ndims,
                      hsize_t *dims,
                      float **buf )
{
    int      i, n;
    unsigned j;
    char     str[20];
    size_t   nelms;
    FILE     *f;
    float    val;
    char     *srcdir = getenv("srcdir");  /* the source directory */
    char     data_file[512];              /* buffer to hold name of existing data file */

    strcpy(data_file, "");
    /* compose the name of the file to open, using the srcdir, if appropriate */
    if(srcdir)
    {
        strcpy(data_file, srcdir);
        strcat(data_file, "/");
    }
    /* read first data file */
    strcat(data_file,fname);

    f = fopen(data_file, "r");
    if( f == NULL )
    {
        printf( "Could not open file %s\n", data_file );
        return -1;
    }

    for(i=0, nelms=1; i < ndims; i++)
    {
        fscanf( f, "%s %u", str, &j);
        fscanf( f, "%d",&n );
        dims[i] = n;
        nelms *= n;
    }

    *buf = (float*) malloc (nelms * sizeof( float ));

    if ( *buf == NULL )
    {
        printf( "memory allocation failed\n" );
        fclose(f);
        return -1;
    }

    for(j = 0; j < nelms; j++)
    {
        fscanf( f, "%f",&val );
        (*buf)[j] = val;
    }
    fclose(f);

    return 1;

}


/*-------------------------------------------------------------------------
 * test parameter errors
 *-------------------------------------------------------------------------
 */

static int test_errors2(void)
{
    hid_t   fid;                              /* file ID */
    hid_t   did = -1;                         /* dataset ID */
    hid_t   dsid = -1;                        /* scale ID */
    hsize_t dimd[2]  = {3,3};                 /* size of data dataset */
    hsize_t dims[1]  = {3};                   /* size of scale dataset */
    char    lbuf[255];                        /* label buffer */
    ssize_t label_len;                        /* label lenght */
    int     scale_idx;                        /* scale index */
    int     nscales;                          /* number of scales in DIM */
    int     count;                            /* visitor data */

    printf("Testing parameter errors\n");

    /*-------------------------------------------------------------------------
    * create a file, a dataset, scales
    *-------------------------------------------------------------------------
    */

    /* create a file using default properties */
    if ((fid=H5Fcreate(FILE7,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT)) < 0)
        goto out;

    /* make a dataset */
    if (H5LTmake_dataset_int(fid,"dset",2,dimd,NULL) < 0)
        goto out;

    /* make a scale dataset  */
    if(H5LTmake_dataset_int(fid,"ds1",1,dims,NULL) < 0)
        goto out;

    /* make a scale dataset  */
    if(H5LTmake_dataset_int(fid,"ds2",1,dims,NULL) < 0)
        goto out;


    TESTING2("attach scales");


    /*-------------------------------------------------------------------------
    * attach with invalid indices
    *-------------------------------------------------------------------------
    */

    if ((did = H5Dopen2(fid,"dset", H5P_DEFAULT)) < 0)
        goto out;
    if ((dsid = H5Dopen2(fid,"ds1", H5P_DEFAULT)) < 0)
        goto out;
    if (H5DSattach_scale(did,dsid,2) == SUCCEED)
        goto out;
    if (H5DSattach_scale(did,dsid,0) < 0)
        goto out;
    if (H5Dclose(dsid) < 0)
        goto out;
    if (H5Dclose(did) < 0)
        goto out;

    PASSED();

    TESTING2("detach scales");

    /*-------------------------------------------------------------------------
    * detach with invalid indices
    *-------------------------------------------------------------------------
    */
    if ((did = H5Dopen2(fid,"dset", H5P_DEFAULT)) < 0)
        goto out;
    if ((dsid = H5Dopen2(fid,"ds1", H5P_DEFAULT)) < 0)
        goto out;
    if (H5DSdetach_scale(did,dsid,2) == SUCCEED)
        goto out;
    if (H5DSdetach_scale(did,dsid,0) < 0)
        goto out;
    if (H5Dclose(dsid) < 0)
        goto out;
    if (H5Dclose(did) < 0)
        goto out;

    PASSED();

    TESTING2("set/get label");

    /*-------------------------------------------------------------------------
    * set/get label invalid indices
    *-------------------------------------------------------------------------
    */
    if ((did = H5Dopen2(fid,"dset", H5P_DEFAULT)) < 0)
        goto out;
    if (H5DSset_label(did,2,"label")== SUCCEED)
        goto out;
    if (H5DSset_label(did,0,"label") < 0)
        goto out;
    if (H5DSget_label(did,2,lbuf,sizeof(lbuf)) == SUCCEED)
        goto out;
    if ((label_len=H5DSget_label(did,0,NULL,0)) < 0)
        goto out;
    if ( label_len != strlen("label") )
        goto out;
    if (H5DSget_label(did,0,lbuf,sizeof(lbuf)) < 0)
        goto out;
    if (H5Dclose(did) < 0)
        goto out;

    PASSED();

    TESTING2("iterate scales");


    /*-------------------------------------------------------------------------
    * iterate_scales invalid indices and return DS_IDX and visitor data
    *-------------------------------------------------------------------------
    */
    if ((did = H5Dopen2(fid,"dset", H5P_DEFAULT)) < 0)
        goto out;

    if ((dsid = H5Dopen2(fid,"ds1", H5P_DEFAULT)) < 0)
        goto out;
    if (H5DSattach_scale(did,dsid,0) < 0)
        goto out;
    if (H5Dclose(dsid) < 0)
        goto out;

    if ((dsid = H5Dopen2(fid,"ds2", H5P_DEFAULT)) < 0)
        goto out;
    if (H5DSattach_scale(did,dsid,0) < 0)
        goto out;
    if (H5Dclose(dsid) < 0)
        goto out;

    if((nscales = H5DSget_num_scales(did,0)) < 0)
        goto out;
    if(nscales!=2)
        goto out;

    /* invalid DIM */
    if (H5DSiterate_scales(did,2,NULL,op_continue,NULL)== SUCCEED)
        goto out;
    /* invalid DS_IDX */
    scale_idx = 2;
    if (H5DSiterate_scales(did,0,&scale_idx,op_continue,NULL)== SUCCEED)
        goto out;

    /* continue iteration */
    scale_idx = 0;
    count = 0;
    if (H5DSiterate_scales(did,0,&scale_idx,op_continue,(void *)&count) < 0)
        goto out;

    if ( scale_idx != 1 && count != nscales )
    {
        goto out;
    }

    /* stop iteration */
    scale_idx = 0;
    count = 0;
    if (H5DSiterate_scales(did,0,&scale_idx,op_stop,(void *)&count) < 0)
        goto out;

    if ( scale_idx != 0 && count != 1 )
    {
        goto out;
    }


    if (H5Dclose(did) < 0)
        goto out;

    /*-------------------------------------------------------------------------
    * close
    *-------------------------------------------------------------------------
    */
    if(H5Fclose(fid) < 0)
        goto out;

    PASSED();

    return 0;

    /* error zone */
out:
    H5E_BEGIN_TRY
    {
        H5Dclose(did);
        H5Dclose(dsid);
        H5Fclose(fid);
    } H5E_END_TRY;
    H5_FAILED();
    return FAIL;
}

