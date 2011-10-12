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
 * Purpose:     This program is run to generate an HDF5 data file with datasets
 *              that use dimension scales.
 *
 *              Compile and run this program to generate the "test_ds_xx.h5"
 *              file, where xx is "le" on a little-endian machine and "be"
 *              on a big-endian machine.
 *              Move it to the test directory in the current branch.
 *              The test: test_foreign_scaleattached(const char *fileforeign)
 *              in test_ds.c will read them.
 */

#include <stdlib.h>
#include <string.h>
#include "h5hltest.h"
#include "H5DSpublic.h"
#include "H5LTpublic.h"

/* prototypes */
static hid_t open_test_file(const char *fileext);
herr_t create_long_dataset(hid_t fid, const char *dsname, const char *dsidx);
herr_t test_attach_scale(hid_t fid, hid_t did, const char *name, unsigned int idx);
herr_t test_detach_scale(hid_t fid, hid_t did, const char *name, unsigned int idx);
herr_t test_set_scalename(hid_t fid, hid_t did, const char *name, const char *scalename, unsigned int idx);
herr_t test_cmp_scalename(hid_t fid, hid_t did, const char *name, const char *scalename, unsigned int idx);

static int test_long_attachscales(const char *filename);
static int test_duplicatelong_attachscales(const char *filename);
static int test_long_scalenames(const char *filename);
static int test_samelong_scalenames(const char *filename);
static int test_foreign_scaleattached(const char *filename);


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
#define DS_2_NAME      "ds_2_"
#define DS_3_NAME      "ds_3_"
#define DS_4_NAME      "ds_4_"

#define SCALE_1_NAME   "scalename_1_"
#define SCALE_2_NAME   "scalename_2_"
#define SCALE_3_NAME   "scalename_3_"
#define SCALE_4_NAME   "scalename_4_"

#define FILENAME       "test_ds_"
#define FILEEXT        ".h5"

/*-------------------------------------------------------------------------
 * the main program
 *-------------------------------------------------------------------------
 */
int main(int argc , char **argv)
{
    int nerrors=0;
    char filename[65];


    if (argc < 2) {
        printf("Usage: gen_test [le | be]\n");
        return 1;
    }

    if ( argv[1] && (strcmp("le",argv[1])!=0) && (strcmp("be",argv[1])!=0) ) {
        printf("Usage: gen_test [le | be]\n");
        return 1;
    }

    /* create file to be used in following tests */
    strcpy(filename, FILENAME);
    strcat(filename, argv[1]);
    strcat(filename, FILEEXT);
    if(H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT) < 0) {
        nerrors = 1;
        goto error;
    }
    nerrors += test_long_attachscales(filename) < 0  ? 1 : 0;
    nerrors += test_duplicatelong_attachscales(filename) < 0  ? 1 : 0;
    nerrors += test_samelong_scalenames(filename) < 0  ? 1 : 0;
    nerrors += test_foreign_scaleattached(filename) < 0  ? 1 : 0;


    if(nerrors) goto error;
    printf("Dimension scales file generation passed.\n");
    return 0;

error:
    printf("***** %d DIMENSION SCALES FILE GENERATION FAILED! *****\n",nerrors);
    return 1;
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

herr_t create_long_dataset(hid_t fid, const char *name, const char *dsidx)
{
    int     rank = 4;
    int     rankds = 1;
    hsize_t dims[4]  = {DIM1_SIZE,DIM2_SIZE,DIM3_SIZE,DIM4_SIZE};
    long    buf[DIM_DATA*3*2] = {1,2,3,4,5,6,7,8,9,10,11,12,
            1,2,3,4,5,6,7,8,9,10,11,12,1,2,3,4,5,6,7,8,9,10,11,12,
            1,2,3,4,5,6,7,8,9,10,11,12,1,2,3,4,5,6,7,8,9,10,11,12,
            1,2,3,4,5,6,7,8,9,10,11,12};
    hsize_t s1_dim[1]  = {DIM1_SIZE};
    hsize_t s2_dim[1]  = {DIM2_SIZE};
    hsize_t s3_dim[1]  = {DIM3_SIZE};
    hsize_t s4_dim[1]  = {DIM4_SIZE};
    long    s1_wbuf[DIM1_SIZE] = {10,20,30};
    long    s2_wbuf[DIM2_SIZE] = {100,200,300,400};
    long    s3_wbuf[DIM3_SIZE] = {10,10,10,20,20,20,30,30,30,40,40,40};
    long    s4_wbuf[DIM4_SIZE] = {18,18};

    /* make a dataset */
    if(H5LTmake_dataset_long(fid, name, rank, dims, buf) >= 0) {
        /* make a DS dataset for the first dimension */
        char dsname[32];

        strcpy(dsname, DS_1_NAME);
        strcat(dsname, dsidx);
        /* make a DS dataset for the first dimension */
        if(H5LTmake_dataset_long(fid, dsname, rankds, s1_dim, s1_wbuf) < 0)
            return FAIL;

        strcpy(dsname, DS_2_NAME);
        strcat(dsname, dsidx);
        /* make a DS dataset for the first dimension */
        if(H5LTmake_dataset_long(fid, dsname, rankds, s2_dim, s2_wbuf) < 0)
            return FAIL;

        strcpy(dsname, DS_3_NAME);
        strcat(dsname, dsidx);
        /* make a DS dataset for the first dimension */
        if(H5LTmake_dataset_long(fid, dsname, rankds, s3_dim, s3_wbuf) < 0)
            return FAIL;

        strcpy(dsname, DS_4_NAME);
        strcat(dsname, dsidx);
        /* make a DS dataset for the first dimension */
        if(H5LTmake_dataset_long(fid, dsname, rankds, s4_dim, s4_wbuf) < 0)
            return FAIL;
     }
     else
         return FAIL;
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
                name_out = (char*)malloc((size_t)name_len * sizeof (char));
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

static int test_long_attachscales(const char *filename)
{
    hid_t   fid = -1;
    hid_t   did = -1;
    char    dsname[32];
    char    scalename[32];
    strcpy(dsname, DATASET_NAME);
    strcat(dsname, "al");

    TESTING2("test_long_attachscales");

    if((fid = H5Fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
        goto out;

    /* make a dataset */
    if(create_long_dataset(fid, dsname, "al") < 0)
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

static int test_duplicatelong_attachscales(const char *filename)
{
    hid_t   fid = -1;
    hid_t   did = -1;
    char    dsname[32];
    char    scalename[32];
    strcpy(dsname, DATASET_NAME);
    strcat(dsname, "al2");

    TESTING2("test_duplicatelong_attachscales");

    if((fid = H5Fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
        goto out;

    /* make a dataset 2 */
    if(create_long_dataset(fid, dsname, "al2") < 0)
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

static int test_long_scalenames(const char *filename) {
    hid_t   fid = -1;
    hid_t   did = -1;
    char    dsname[32];
    char    scalename[32];
    char    name[32];
    strcpy(dsname, DATASET_NAME);
    strcat(dsname, "al");

    if((fid = H5Fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
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

static int test_samelong_scalenames(const char *filename) {
    hid_t   fid = -1;
    hid_t   did = -1;
    char    dsname[32];
    char    scalename[32];
    char    name[32];

    strcpy(dsname, DATASET_NAME);
    strcat(dsname, "al2");

    if((fid = open_test_file(filename)) < 0)
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

static int test_foreign_scaleattached(const char *filename)
{
    herr_t  ret_value = FAIL;
    hid_t   fid = -1;
    hid_t   did = -1;
    hid_t   dsid = -1;

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
