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
* Test program:	 tmisc
*
* Test miscellaneous features not tested elsewhere.  Generally
*       regression tests for bugs that are reported and don't
*       have an existing test to add them to.
*
*************************************************************/

#define H5D_PACKAGE		/*suppress error about including H5Dpkg	  */

/* Define this macro to indicate that the testing APIs should be available */
#define H5D_TESTING

#include "hdf5.h"
#include "testhdf5.h"
#include "H5srcdir.h"
#include "H5Dpkg.h"		/* Datasets 				*/

/* Definitions for misc. test #1 */
#define MISC1_FILE	"tmisc1.h5"
#define MISC1_VAL       (13417386)      /* 0xccbbaa */
#define MISC1_VAL2      (15654348)      /* 0xeeddcc */
#define MISC1_DSET_NAME "/scalar_set"

/* Definitions for misc. test #2 */
#define MISC2_FILE_1        "tmisc2a.h5"
#define MISC2_FILE_2        "tmisc2b.h5"
#define MISC2_ATT_NAME_1 "scalar_att_1"
#define MISC2_ATT_NAME_2 "scalar_att_2"

typedef struct {
      char *string;
} misc2_struct;

/* Definitions for misc. test #3 */
#define MISC3_FILE              "tmisc3.h5"
#define MISC3_RANK              2
#define MISC3_DIM1              6
#define MISC3_DIM2              6
#define MISC3_CHUNK_DIM1        2
#define MISC3_CHUNK_DIM2        2
#define MISC3_FILL_VALUE        2
#define MISC3_DSET_NAME         "/chunked"

/* Definitions for misc. test #4 */
#define MISC4_FILE_1            "tmisc4a.h5"
#define MISC4_FILE_2            "tmisc4b.h5"
#define MISC4_GROUP_1           "/Group1"
#define MISC4_GROUP_2           "/Group2"

/* Definitions for misc. test #5 */
#define MISC5_FILE              "tmisc5.h5"
#define MISC5_DSETNAME          "dset1"
#define MISC5_DSETRANK          1
#define MISC5_NELMTOPLVL        1
#define MISC5_DBGNELM1          2
#define MISC5_DBGNELM2          1
#define MISC5_DBGNELM3          1
#define MISC5_DBGELVAL1         999999999
#define MISC5_DBGELVAL2         888888888
#define MISC5_DBGELVAL3         777777777

typedef struct
{
    int st1_el1;
    hvl_t st1_el2;
} misc5_struct1;

typedef struct
{
    int st2_el1;
    hvl_t st2_el2;
} misc5_struct2;

typedef struct
{
    int st3_el1;
} misc5_struct3;

typedef struct
{
    hid_t         st3h_base;
    hid_t         st3h_id;
} misc5_struct3_hndl;

typedef struct
{
    hid_t         st2h_base;
    hid_t         st2h_id;
    misc5_struct3_hndl *st2h_st3hndl;
} misc5_struct2_hndl;

typedef struct
{
    hid_t         st1h_base;
    hid_t         st1h_id;
    misc5_struct2_hndl *st1h_st2hndl;
} misc5_struct1_hndl;

/* Definitions for misc. test #6 */
#define MISC6_FILE              "tmisc6.h5"
#define MISC6_DSETNAME1         "dset1"
#define MISC6_DSETNAME2         "dset2"
#define MISC6_NUMATTR           16

/* Definitions for misc. test #7 */
#define MISC7_FILE              "tmisc7.h5"
#define MISC7_DSETNAME1         "Dataset1"
#define MISC7_DSETNAME2         "Dataset2"
#define MISC7_TYPENAME1         "Datatype1"
#define MISC7_TYPENAME2         "Datatype2"

/* Definitions for misc. test #8 */
#define MISC8_FILE              "tmisc8.h5"
#define MISC8_DSETNAME1         "Dataset1"
#define MISC8_DSETNAME2         "Dataset2"
#define MISC8_DSETNAME3         "Dataset3"
#define MISC8_DSETNAME4         "Dataset4"
#define MISC8_DSETNAME5         "Dataset5"
#define MISC8_DSETNAME6         "Dataset6"
#define MISC8_DSETNAME7         "Dataset7"
#define MISC8_DSETNAME8         "Dataset8"
#define MISC8_DSETNAME9         "Dataset9"
#define MISC8_DSETNAME10        "Dataset10"
#define MISC8_RANK              2
#define MISC8_DIM0              50
#define MISC8_DIM1              50
#define MISC8_CHUNK_DIM0        10
#define MISC8_CHUNK_DIM1        10

/* Definitions for misc. test #9 */
#define MISC9_FILE              "tmisc9.h5"

/* Definitions for misc. test #10 */
#define MISC10_FILE_OLD         "tmtimeo.h5"
#define MISC10_FILE_NEW         "tmisc10.h5"
#define MISC10_DSETNAME         "Dataset1"

/* Definitions for misc. test #11 */
#define MISC11_FILE             "tmisc11.h5"
#define MISC11_USERBLOCK        1024
#define MISC11_SIZEOF_OFF       4
#define MISC11_SIZEOF_LEN       4
#define MISC11_SYM_LK           8
#define MISC11_SYM_IK           32
#define MISC11_ISTORE_IK        64
#define MISC11_NINDEXES    1

/* Definitions for misc. test #12 */
#define MISC12_FILE             "tmisc12.h5"
#define MISC12_DSET_NAME        "Dataset"
#define MISC12_SPACE1_RANK	1
#define MISC12_SPACE1_DIM1	4
#define MISC12_CHUNK_SIZE       2
#define MISC12_APPEND_SIZE      5

/* Definitions for misc. test #13 */
#define MISC13_FILE_1          "tmisc13a.h5"
#define MISC13_FILE_2          "tmisc13b.h5"
#define MISC13_DSET1_NAME      "Dataset1"
#define MISC13_DSET2_NAME      "Dataset2"
#define MISC13_DSET3_NAME      "Dataset3"
#define MISC13_GROUP1_NAME     "Group1"
#define MISC13_GROUP2_NAME     "Group2"
#define MISC13_DTYPE_NAME      "Datatype"
#define MISC13_RANK            2
#define MISC13_DIM1            20
#define MISC13_DIM2            30
#define MISC13_CHUNK_DIM1      10
#define MISC13_CHUNK_DIM2      15
#define MISC13_USERBLOCK_SIZE  512
#define MISC13_COPY_BUF_SIZE   4096

unsigned m13_data[MISC13_DIM1][MISC13_DIM2];           /* Data to write to dataset */
unsigned m13_rdata[MISC13_DIM1][MISC13_DIM2];          /* Data read from dataset */

/* Definitions for misc. test #14 */
#define MISC14_FILE             "tmisc14.h5"
#define MISC14_DSET1_NAME       "Dataset1"
#define MISC14_DSET2_NAME       "Dataset2"
#define MISC14_DSET3_NAME       "Dataset3"
#define MISC14_METADATA_SIZE    4096

/* Definitions for misc. test #15 */
#define MISC15_FILE             "tmisc15.h5"

/* Definitions for misc. test #16 */
#define MISC16_FILE             "tmisc16.h5"
#define MISC16_SPACE_DIM        4
#define MISC16_SPACE_RANK       1
#define MISC16_STR_SIZE         8
#define MISC16_DSET_NAME        "Dataset"

/* Definitions for misc. test #17 */
#define MISC17_FILE             "tmisc17.h5"
#define MISC17_SPACE_RANK       2
#define MISC17_SPACE_DIM1       4
#define MISC17_SPACE_DIM2       8
#define MISC17_DSET_NAME        "Dataset"

/* Definitions for misc. test #18 */
#define MISC18_FILE             "tmisc18.h5"
#define MISC18_DSET1_NAME       "Dataset1"
#define MISC18_DSET2_NAME       "Dataset2"

/* Definitions for misc. test #19 */
#define MISC19_FILE             "tmisc19.h5"
#define MISC19_DSET_NAME        "Dataset"
#define MISC19_ATTR_NAME        "Attribute"
#define MISC19_GROUP_NAME       "Group"

/* Definitions for misc. test #20 */
#define MISC20_FILE             "tmisc20.h5"
#define MISC20_FILE_OLD         "tlayouto.h5"
#define MISC20_DSET_NAME        "Dataset"
#define MISC20_DSET2_NAME       "Dataset2"
#define MISC20_SPACE_RANK       2
/* Make sure the product of the following 2 does not get too close to */
/* 64 bits, risking an overflow. */
#define MISC20_SPACE_DIM0       (8*1024*1024*(uint64_t)1024)
#define MISC20_SPACE_DIM1       ((256*1024*(uint64_t)1024)+1)
#define MISC20_SPACE2_DIM0      8
#define MISC20_SPACE2_DIM1      4

/* Definitions for misc. test #21 */
#define MISC21_FILE             "tmisc21.h5"
#define MISC21_DSET_NAME        "Dataset"
#define MISC21_SPACE_RANK       2
#define MISC21_SPACE_DIM0       7639
#define MISC21_SPACE_DIM1       6308
#define MISC21_CHUNK_DIM0       2048
#define MISC21_CHUNK_DIM1       2048

/* Definitions for misc. test #22 */
#define MISC22_FILE             "tmisc22.h5"
#define MISC22_DSET_NAME        "Dataset"
#define MISC22_SPACE_RANK       2
#define MISC22_CHUNK_DIM0       512
#define MISC22_CHUNK_DIM1       512
#define MISC22_SPACE_DIM0       639
#define MISC22_SPACE_DIM1       1308

/* Definitions for misc. test #23 */
#define MISC23_FILE             "tmisc23.h5"
#define MISC23_NAME_BUF_SIZE    40

/* Definitions for misc. test #24 */
#define MISC24_FILE             "tmisc24.h5"
#define MISC24_GROUP_NAME       "group"
#define MISC24_GROUP_LINK       "group_link"
#define MISC24_DATASET_NAME     "dataset"
#define MISC24_DATASET_LINK     "dataset_link"
#define MISC24_DATATYPE_NAME    "datatype"
#define MISC24_DATATYPE_LINK    "datatype_link"

/* Definitions for misc. test #25 'a', 'b' & 'c' */
#define MISC25A_FILE            "foo.h5"
#define MISC25A_GROUP0_NAME     "grp0"
#define MISC25A_GROUP1_NAME     "/grp0/grp1"
#define MISC25A_GROUP2_NAME     "/grp0/grp2"
#define MISC25A_GROUP3_NAME     "/grp0/grp3"
#define MISC25A_ATTR1_NAME      "_long attribute_"
#define MISC25A_ATTR1_LEN       11
#define MISC25A_ATTR2_NAME      "_short attr__"
#define MISC25A_ATTR2_LEN       11
#define MISC25A_ATTR3_NAME      "_short attr__"
#define MISC25A_ATTR3_LEN       1
#define MISC25B_FILE            "mergemsg.h5"
#define MISC25B_GROUP           "grp1"
#define MISC25C_FILE            "nc4_rename.h5"
#define MISC25C_DSETNAME        "da"
#define MISC25C_DSETNAME2       "dz"
#define MISC25C_DSETGRPNAME     "ga"
#define MISC25C_GRPNAME         "gb"
#define MISC25C_GRPNAME2        "gc"
#define MISC25C_ATTRNAME        "aa"
#define MISC25C_ATTRNAME2       "ab"

/* Definitions for misc. test #26 */
#define MISC26_FILE             "dcpl_file"

/* Definitions for misc. test #27 */
/* (Note that this test file is generated by the "gen_bad_ohdr.c" code) */
#define MISC27_FILE             "tbad_msg_count.h5"
#define MISC27_GROUP            "Group"

/* Definitions for misc. test #28 */
#define MISC28_FILE             "tmisc28.h5"
#define MISC28_SIZE             10
#define MISC28_NSLOTS           10000

/* Definitions for misc. test #29 */
#define MISC29_ORIG_FILE        "specmetaread.h5"
#define MISC29_COPY_FILE        "tmisc29.h5"
#define MISC29_DSETNAME         "dset2"

/****************************************************************
**
**  test_misc1(): test unlinking a dataset from a group and immediately
**                      re-using the dataset name
**
****************************************************************/
static void
test_misc1(void)
{
    int i;
    int i_check;
    hid_t file, dataspace, dataset;
    herr_t ret;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Unlinking Dataset and Re-creating It\n"));

    file = H5Fcreate(MISC1_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fcreate");

    dataspace = H5Screate(H5S_SCALAR);
    CHECK(dataspace, FAIL, "H5Screate");

    /* Write the dataset the first time. */
    dataset = H5Dcreate2(file, MISC1_DSET_NAME, H5T_NATIVE_INT, dataspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    i = MISC1_VAL;
    ret = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &i);
    CHECK(ret, FAIL, "H5Dwrite");

    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Remove the dataset. */
    ret = H5Ldelete(file, MISC1_DSET_NAME, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Ldelete");

    /* Write the dataset for the second time with a different value. */
    dataset = H5Dcreate2(file, MISC1_DSET_NAME, H5T_NATIVE_INT, dataspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    i = MISC1_VAL2;
    ret = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &i);
    CHECK(ret, FAIL, "H5Dwrite");

    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    ret = H5Sclose(dataspace);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    /* Now, check the value written to the dataset, after it was re-created */
    file = H5Fopen(MISC1_FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fopen");

    dataspace = H5Screate(H5S_SCALAR);
    CHECK(dataspace, FAIL, "H5Screate");

    dataset = H5Dopen2(file, MISC1_DSET_NAME, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dopen2");

    ret = H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &i_check);
    CHECK(ret, FAIL, "H5Dread");
    VERIFY(i_check,MISC1_VAL2,"H5Dread");

    ret = H5Sclose(dataspace);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

} /* end test_misc1() */

static hid_t misc2_create_type(void)
{
    hid_t type, type_tmp;
    herr_t ret;

    type_tmp = H5Tcopy (H5T_C_S1);
    CHECK(type_tmp, FAIL, "H5Tcopy");

    ret = H5Tset_size(type_tmp, H5T_VARIABLE);
    CHECK(ret, FAIL, "H5Tset_size");

    type = H5Tcreate(H5T_COMPOUND, sizeof(misc2_struct));
    CHECK(type, FAIL, "H5Tcreate");

    ret = H5Tinsert(type, "string", offsetof(misc2_struct, string), type_tmp);
    CHECK(ret, FAIL, "H5Tinsert");

    ret = H5Tclose(type_tmp);
    CHECK(ret, FAIL, "H5Tclose");

    return type;
}

static void test_misc2_write_attribute(void)
{
    hid_t file1, file2, root1, root2, dataspace, att1, att2;
    hid_t type;
    herr_t ret;
    misc2_struct data, data_check;
    char *string_att1 = HDstrdup("string attribute in file one");
    char *string_att2 = HDstrdup("string attribute in file two");

    type = misc2_create_type();

    dataspace = H5Screate(H5S_SCALAR);
    CHECK(dataspace, FAIL, "H5Screate");

    file2 = H5Fcreate(MISC2_FILE_2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file2, FAIL, "H5Fcreate");

    file1 = H5Fcreate(MISC2_FILE_1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file1, FAIL, "H5Fcreate");

    root1 = H5Gopen2(file1, "/", H5P_DEFAULT);
    CHECK(root1, FAIL, "H5Gopen2");

    att1 = H5Acreate2(root1, MISC2_ATT_NAME_1, type, dataspace, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(att1, FAIL, "H5Acreate2");

    data.string = string_att1;

    ret = H5Awrite(att1, type, &data);
    CHECK(ret, FAIL, "H5Awrite");

    ret = H5Aread(att1, type, &data_check);
    CHECK(ret, FAIL, "H5Aread");

    free(data_check.string);

    ret = H5Aclose(att1);
    CHECK(ret, FAIL, "HAclose");

    ret = H5Gclose(root1);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Fclose(file1);
    CHECK(ret, FAIL, "H5Fclose");



    root2 = H5Gopen2(file2, "/", H5P_DEFAULT);
    CHECK(root2, FAIL, "H5Gopen2");

    att2 = H5Acreate2(root2, MISC2_ATT_NAME_2, type, dataspace, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(att2, FAIL, "H5Acreate2");

    data.string = string_att2;

    ret = H5Awrite(att2, type, &data);
    CHECK(ret, FAIL, "H5Awrite");

    ret = H5Aread(att2, type, &data_check);
    CHECK(ret, FAIL, "H5Aread");

    free(data_check.string);

    ret = H5Aclose(att2);
    CHECK(ret, FAIL, "HAclose");

    ret = H5Gclose(root2);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Tclose(type);
    CHECK(ret, FAIL, "H5Tclose");

    ret = H5Sclose(dataspace);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Fclose(file2);
    CHECK(ret, FAIL, "H5Fclose");

    free(string_att1);
    free(string_att2);
    return;
}


static void test_misc2_read_attribute(const char *filename, const char *att_name)
{
    hid_t file, root, att;
    hid_t type;
    herr_t ret;
    misc2_struct data_check;

    type = misc2_create_type();

    file = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fopen");

    root = H5Gopen2(file, "/", H5P_DEFAULT);
    CHECK(root, FAIL, "H5Gopen2");

    att = H5Aopen(root, att_name, H5P_DEFAULT);
    CHECK(att, FAIL, "H5Aopen");

    ret = H5Aread(att, type, &data_check);
    CHECK(ret, FAIL, "H5Aread");

    free(data_check.string);

    ret = H5Aclose(att);
    CHECK(ret, FAIL, "H5Aclose");

    ret = H5Tclose(type);
    CHECK(ret, FAIL, "H5Tclose");

    ret = H5Gclose(root);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    return;
}
/****************************************************************
**
**  test_misc2(): test using the same VL-derived datatype in two
**      different files, which was causing problems with the
**      datatype conversion functions
**
****************************************************************/
static void
test_misc2(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing VL datatype in two different files\n"));

    test_misc2_write_attribute();
    test_misc2_read_attribute(MISC2_FILE_1, MISC2_ATT_NAME_1);
    test_misc2_read_attribute(MISC2_FILE_2, MISC2_ATT_NAME_2);
} /* end test_misc2() */

/****************************************************************
**
**  test_misc3(): Test reading from chunked dataset with non-zero
**      fill value
**
****************************************************************/
static void
test_misc3(void)
{
    hid_t file, dataspace, dataset, dcpl;
    int rank=MISC3_RANK;
    hsize_t dims[MISC3_RANK]={MISC3_DIM1,MISC3_DIM2};
    hsize_t chunk_dims[MISC3_RANK]={MISC3_CHUNK_DIM1,MISC3_CHUNK_DIM2};
    int fill=MISC3_FILL_VALUE;
    int read_buf[MISC3_DIM1][MISC3_DIM2];
    int i,j;
    herr_t ret;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing reading from chunked dataset with non-zero fill-value\n"));

    file = H5Fcreate(MISC3_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fcreate");

    /* Create a simple dataspace */
    dataspace = H5Screate_simple(rank,dims,NULL);
    CHECK(dataspace, FAIL, "H5Screate_simple");

    /* Create a dataset creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Set the chunk information */
    ret = H5Pset_chunk(dcpl,rank,chunk_dims);
    CHECK(dcpl, FAIL, "H5Pset_chunk");

    /* Set the fill-value information */
    ret = H5Pset_fill_value(dcpl,H5T_NATIVE_INT,&fill);
    CHECK(dcpl, FAIL, "H5Pset_fill_value");

    /* Create the dataset */
    dataset = H5Dcreate2(file, MISC3_DSET_NAME, H5T_NATIVE_INT, dataspace, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Read from the dataset (should be fill-values) */
    ret = H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &read_buf);
    CHECK(ret, FAIL, "H5Dread");

    for(i=0; i<MISC3_DIM1; i++)
        for(j=0; j<MISC3_DIM2; j++)
            VERIFY(read_buf[i][j],fill,"H5Dread");

    /* Release resources */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    ret = H5Sclose(dataspace);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");
} /* end test_misc3() */

/****************************************************************
**
**  test_misc4(): Test the that 'fileno' field in H5O_info_t is
**      valid.
**
****************************************************************/
static void
test_misc4(void)
{
    hid_t file1, file2, group1, group2, group3;
    H5O_info_t oinfo1, oinfo2, oinfo3;
    herr_t ret;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing fileno working in H5O_info_t\n"));

    file1 = H5Fcreate(MISC4_FILE_1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file1, FAIL, "H5Fcreate");

    /* Create the first group */
    group1 = H5Gcreate2(file1, MISC4_GROUP_1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(group1, FAIL, "H5Gcreate2");

    /* Create the second group */
    group2 = H5Gcreate2(file1, MISC4_GROUP_2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(group2, FAIL, "H5Gcreate2");

    file2 = H5Fcreate(MISC4_FILE_2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file2, FAIL, "H5Fcreate");

    /* Create the first group */
    group3 = H5Gcreate2(file2, MISC4_GROUP_1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(group3, FAIL, "H5Gcreate2");

    /* Get the stat information for each group */
    ret = H5Oget_info_by_name(file1, MISC4_GROUP_1, &oinfo1, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    ret = H5Oget_info_by_name(file1, MISC4_GROUP_2, &oinfo2, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    ret = H5Oget_info_by_name(file2, MISC4_GROUP_1, &oinfo3, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");

    /* Verify that the fileno values are the same for groups from file1 */
    VERIFY(oinfo1.fileno, oinfo2.fileno, "H5Oget_info_by_name");

    /* Verify that the fileno values are not the same between file1 & file2 */
    if(oinfo1.fileno == oinfo3.fileno)
        TestErrPrintf("Error on line %d: oinfo1.fileno != oinfo3.fileno\n", __LINE__);
    if(oinfo2.fileno == oinfo3.fileno)
        TestErrPrintf("Error on line %d: oinfo2.fileno != oinfo3.fileno\n", __LINE__);

    /* Close the objects */
    ret = H5Gclose(group1);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Gclose(group2);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Gclose(group3);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Fclose(file1);
    CHECK(ret, FAIL, "H5Fclose");

    ret = H5Fclose(file2);
    CHECK(ret, FAIL, "H5Fclose");
} /* end test_misc4() */

/****************************************************************
**
**  test_misc5(): Test several level deep nested compound & VL datatypes
**
****************************************************************/

/*********************** struct3 ***********************/

static misc5_struct3_hndl *
create_struct3(void)
{
    misc5_struct3_hndl *str3hndl;       /* New 'struct3' created */
    herr_t ret;                         /* For error checking */

    str3hndl = HDmalloc(sizeof(misc5_struct3_hndl));
    CHECK(str3hndl,NULL,"malloc");

    str3hndl->st3h_base = H5Tcreate(H5T_COMPOUND, sizeof(misc5_struct3));
    CHECK(str3hndl->st3h_base, FAIL, "H5Tcreate");

    ret = H5Tinsert(str3hndl->st3h_base, "st3_el1", HOFFSET( misc5_struct3, st3_el1), H5T_NATIVE_INT);
    CHECK(ret,FAIL,"H5Tinsert");

    str3hndl->st3h_id=H5Tvlen_create(str3hndl->st3h_base);
    CHECK(str3hndl->st3h_id,FAIL,"H5Tvlen_create");

    return(str3hndl);
}

static void
delete_struct3(misc5_struct3_hndl *str3hndl)
{
    herr_t ret;                         /* For error checking */

    ret=H5Tclose(str3hndl->st3h_id);
    CHECK(ret,FAIL,"H5Tclose");

    ret=H5Tclose(str3hndl->st3h_base);
    CHECK(ret,FAIL,"H5Tclose");

    free(str3hndl);
}

static void
set_struct3(misc5_struct3 *buf)
{
    buf->st3_el1=MISC5_DBGELVAL3;
}

/*********************** struct2 ***********************/

static misc5_struct2_hndl *
create_struct2(void)
{
    misc5_struct2_hndl *str2hndl;       /* New 'struct2' created */
    herr_t ret;                         /* For error checking */

    str2hndl = HDmalloc(sizeof(misc5_struct2_hndl));
    CHECK(str2hndl, NULL, "malloc");

    str2hndl->st2h_base = H5Tcreate(H5T_COMPOUND, sizeof(misc5_struct2));
    CHECK(str2hndl->st2h_base, FAIL, "H5Tcreate");

    ret = H5Tinsert(str2hndl->st2h_base, "st2_el1", HOFFSET(misc5_struct2, st2_el1), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    str2hndl->st2h_st3hndl=create_struct3();
    CHECK(str2hndl->st2h_st3hndl,NULL,"create_struct3");

    ret=H5Tinsert(str2hndl->st2h_base, "st2_el2", HOFFSET(misc5_struct2, st2_el2), str2hndl->st2h_st3hndl->st3h_id);
    CHECK(ret,FAIL,"H5Tinsert");

    str2hndl->st2h_id= H5Tvlen_create(str2hndl->st2h_base);
    CHECK(str2hndl->st2h_id,FAIL,"H5Tvlen_create");

    return(str2hndl);
}

static void
delete_struct2(misc5_struct2_hndl *str2hndl)
{
    herr_t ret;                         /* For error checking */

    ret=H5Tclose(str2hndl->st2h_id);
    CHECK(ret,FAIL,"H5Tclose");

    delete_struct3(str2hndl->st2h_st3hndl);

    H5Tclose(str2hndl->st2h_base);
    CHECK(ret,FAIL,"H5Tclose");

    free(str2hndl);
}

static void
set_struct2(misc5_struct2 *buf)
{
    unsigned i;         /* Local index variable */

    buf->st2_el1=MISC5_DBGELVAL2;
    buf->st2_el2.len=MISC5_DBGNELM3;

    buf->st2_el2.p=malloc((buf->st2_el2.len)*sizeof(misc5_struct3));
    CHECK(buf->st2_el2.p,NULL,"malloc");

    for(i=0; i<(buf->st2_el2.len); i++)
        set_struct3(&(((misc5_struct3 *)(buf->st2_el2.p))[i]));
}

static void
clear_struct2(misc5_struct2 *buf)
{
    free(buf->st2_el2.p);
}

/*********************** struct1 ***********************/

static misc5_struct1_hndl *
create_struct1(void)
{
    misc5_struct1_hndl *str1hndl;       /* New 'struct1' created */
    herr_t ret;                         /* For error checking */

    str1hndl = HDmalloc(sizeof(misc5_struct1_hndl));
    CHECK(str1hndl, NULL, "malloc");

    str1hndl->st1h_base = H5Tcreate(H5T_COMPOUND, sizeof(misc5_struct1));
    CHECK(str1hndl->st1h_base, FAIL, "H5Tcreate");

    ret = H5Tinsert(str1hndl->st1h_base, "st1_el1", HOFFSET(misc5_struct1, st1_el1), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    str1hndl->st1h_st2hndl=create_struct2();
    CHECK(str1hndl->st1h_st2hndl,NULL,"create_struct2");

    ret=H5Tinsert(str1hndl->st1h_base, "st1_el2", HOFFSET(misc5_struct1, st1_el2), str1hndl->st1h_st2hndl->st2h_id);
    CHECK(ret,FAIL,"H5Tinsert");

    str1hndl->st1h_id=H5Tvlen_create(str1hndl->st1h_base);
    CHECK(str1hndl->st1h_id,FAIL,"H5Tvlen_create");

    return(str1hndl);
}

static void
delete_struct1(misc5_struct1_hndl *str1hndl)
{
    herr_t ret;                         /* For error checking */

    ret=H5Tclose(str1hndl->st1h_id);
    CHECK(ret,FAIL,"H5Tclose");

    delete_struct2(str1hndl->st1h_st2hndl);

    ret=H5Tclose(str1hndl->st1h_base);
    CHECK(ret,FAIL,"H5Tclose");

    free(str1hndl);
}

static void
set_struct1(misc5_struct1 *buf)
{
    unsigned i;         /* Local index variable */

    buf->st1_el1=MISC5_DBGELVAL1;
    buf->st1_el2.len=MISC5_DBGNELM2;

    buf->st1_el2.p=malloc((buf->st1_el2.len)*sizeof(misc5_struct2));
    CHECK(buf->st1_el2.p,NULL,"malloc");

    for(i=0; i<(buf->st1_el2.len); i++)
        set_struct2(&(((misc5_struct2 *)(buf->st1_el2.p))[i]));
}

static void
clear_struct1(misc5_struct1 *buf)
{
    unsigned i;

    for(i=0;i<buf->st1_el2.len;i++)
        clear_struct2(&((( misc5_struct2 *)(buf->st1_el2.p))[i]));
    free(buf->st1_el2.p);
}

static void
test_misc5(void)
{
    hid_t loc_id, space_id, dataset_id;
    hid_t mem_type_id;
    misc5_struct1_hndl *str1hndl;
    hsize_t dims[MISC5_DSETRANK];
    hvl_t buf;
    unsigned i,j,k;
    herr_t ret;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing several level deep nested compound & VL datatypes \n"));

    /* Write the dataset out */
    loc_id = H5Fcreate(MISC5_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(loc_id, FAIL, "H5Fcreate");

    /* Create the memory structure to write */
    str1hndl = create_struct1();
    CHECK(str1hndl, NULL, "create_struct1");

    /* Create the dataspace */
    dims[0] = MISC5_NELMTOPLVL;
    space_id = H5Screate_simple(MISC5_DSETRANK, dims, NULL);
    CHECK(space_id, FAIL, "H5Screate_simple");

    /* Create the dataset */
    dataset_id = H5Dcreate2(loc_id, MISC5_DSETNAME, str1hndl->st1h_id, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset_id, FAIL, "H5Dcreate2");

    /* Create the variable-length buffer */
    buf.len = MISC5_DBGNELM1;
    buf.p = malloc((buf.len) * sizeof(misc5_struct1));
    CHECK(buf.p, NULL, "malloc");

    /* Create the top-level VL information */
    for(i = 0; i < MISC5_DBGNELM1; i++)
        set_struct1(&(((misc5_struct1 *)(buf.p))[i]));

    /* Write the data out */
    ret = H5Dwrite(dataset_id, str1hndl->st1h_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, &buf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Release the top-level VL information */
    for(j = 0; j < MISC5_DBGNELM1; j++)
        clear_struct1(&(((misc5_struct1 *)(buf.p))[j]));

    /* Free the variable-length buffer */
    free(buf.p);

    /* Close dataset */
    ret = H5Dclose(dataset_id);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close dataspace */
    ret=H5Sclose(space_id);
    CHECK(ret,FAIL,"H5Sclose");

    /* Delete memory structures */
    delete_struct1(str1hndl);

    /* Close file */
    ret=H5Fclose(loc_id);
    CHECK(ret,FAIL,"H5Fclose");


    /* Read the dataset back in & verify it */
    loc_id = H5Fopen(MISC5_FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(loc_id, FAIL, "H5Fopen");

    /* Open dataset again */
    dataset_id = H5Dopen2(loc_id, MISC5_DSETNAME, H5P_DEFAULT);
    CHECK(dataset_id, FAIL, "H5Dopen2");

    /* Get the dataset's datatype */
    mem_type_id = H5Dget_type(dataset_id);
    CHECK(mem_type_id, FAIL, "H5Dget_type");

    /* Get the dataset's dataspace */
    space_id = H5Dget_space(dataset_id);
    CHECK(space_id, FAIL, "H5Dget_space");

    /* Read the data back in */
    ret = H5Dread(dataset_id, mem_type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, &buf);
    CHECK(ret, FAIL, "H5Dread");

    /* Verify the correct information was read in */
    for(i=0; i<(buf.len); i++) {
        /* printf("[%d]=%d\n",i, ((misc5_struct1 *)(buf.p))[i].st1_el1); */
        VERIFY(((misc5_struct1 *)(buf.p))[i].st1_el1,MISC5_DBGELVAL1,"H5Dread");
        for(j=0; j<(((misc5_struct1 *)(buf.p)) [i].st1_el2.len); j++) {
            /* printf("   [%d]=%d\n",j, ((misc5_struct2 *)(((misc5_struct1 *) (buf.p))[i].st1_el2.p))[j].st2_el1); */
            VERIFY(((misc5_struct2 *)(((misc5_struct1 *) (buf.p))[i].st1_el2.p))[j].st2_el1, MISC5_DBGELVAL2,"H5Dread");
            for(k=0; k<(((misc5_struct2 *) (((misc5_struct1 *)(buf.p))[i].  st1_el2.p))[j].st2_el2.len); k++) {
                /* printf("      [%d]=%d\n",k, ((misc5_struct3 *)(((misc5_struct2 *) (((misc5_struct1 *)(buf.p))[i].  st1_el2.p))[j].st2_el2.p))[k].st3_el1); */
                VERIFY(((misc5_struct3 *)(((misc5_struct2 *) (((misc5_struct1 *)(buf.p))[i].  st1_el2.p))[j].st2_el2.p))[k].st3_el1, MISC5_DBGELVAL3,"H5Dread");
            } /* end for */
        }
    }

    /* Reclaim the memory for the VL information */
    ret=H5Dvlen_reclaim(mem_type_id, space_id, H5P_DEFAULT, &buf);
    CHECK(ret,FAIL,"H5Dvlen_reclaim");

    /* Close dataspace */
    ret=H5Sclose(space_id);
    CHECK(ret,FAIL,"H5Sclose");

    /* Close dataset */
    ret=H5Tclose(mem_type_id);
    CHECK(ret,FAIL,"H5Tclose");

    /* Close dataset */
    ret=H5Dclose(dataset_id);
    CHECK(ret,FAIL,"H5Dclose");

    /* Close file */
    ret=H5Fclose(loc_id);
    CHECK(ret,FAIL,"H5Fclose");

} /* end test_misc5() */

/****************************************************************
**
**  test_misc6(): Test that object header continuation messages are
**      created correctly.
**
****************************************************************/
static void
test_misc6(void)
{
    hid_t loc_id, space_id, dataset_id;
    hid_t attr_id;
    char attr_name[16];
    unsigned u;
    herr_t ret;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing object header continuation code \n"));

    /* Create the file */
    loc_id = H5Fcreate(MISC6_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(loc_id, FAIL, "H5Fcreate");

    /* Create the dataspace */
    space_id = H5Screate(H5S_SCALAR);
    CHECK(space_id, FAIL, "H5Screate");

    /* Create the first dataset */
    dataset_id = H5Dcreate2(loc_id, MISC6_DSETNAME1, H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset_id, FAIL, "H5Dcreate2");

    /* Close dataset */
    ret = H5Dclose(dataset_id);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create the second dataset */
    dataset_id = H5Dcreate2(loc_id, MISC6_DSETNAME2, H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset_id, FAIL, "H5Dcreate2");

    /* Close dataset */
    ret = H5Dclose(dataset_id);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(loc_id);
    CHECK(ret, FAIL, "H5Fclose");

    /* Loop through adding attributes to each dataset */
    for(u = 0; u < MISC6_NUMATTR; u++) {
        /* Create name for attribute */
        sprintf(attr_name, "Attr#%u", u);

        /* Open the file */
        loc_id = H5Fopen(MISC6_FILE, H5F_ACC_RDWR, H5P_DEFAULT);
        CHECK(loc_id, FAIL, "H5Fopen");


        /* Open first dataset */
        dataset_id = H5Dopen2(loc_id, MISC6_DSETNAME1, H5P_DEFAULT);
        CHECK(dataset_id, FAIL, "H5Dopen2");

        /* Add attribute to dataset */
        attr_id = H5Acreate2(dataset_id, attr_name, H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(attr_id, FAIL, "H5Acreate2");

        /* Close attribute */
        ret = H5Aclose(attr_id);
        CHECK(ret, FAIL, "H5Aclose");

        /* Close dataset */
        ret = H5Dclose(dataset_id);
        CHECK(ret, FAIL, "H5Dclose");


        /* Open second dataset */
        dataset_id = H5Dopen2(loc_id, MISC6_DSETNAME2, H5P_DEFAULT);
        CHECK(dataset_id, FAIL, "H5Dopen2");

        /* Add attribute to dataset */
        attr_id = H5Acreate2(dataset_id, attr_name, H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(attr_id, FAIL, "H5Acreate2");

        /* Close attribute */
        ret = H5Aclose(attr_id);
        CHECK(ret, FAIL, "H5Aclose");

        /* Close dataset */
        ret = H5Dclose(dataset_id);
        CHECK(ret, FAIL, "H5Dclose");


        /* Close file */
        ret = H5Fclose(loc_id);
        CHECK(ret,FAIL,"H5Fclose");
    } /* end for */

    /* Close dataspace */
    ret=H5Sclose(space_id);
    CHECK(ret,FAIL,"H5Sclose");

} /* end test_misc6() */

/****************************************************************
**
**  test_misc7(): Test that datatypes are sensible to store on
**      disk.  (i.e. not partially initialized)
**
****************************************************************/
static void
test_misc7(void)
{
    hid_t fid, did, tid, sid;
    int enum_value=1;
    herr_t ret;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing sensible datatype on disk code \n"));

    /* Attempt to commit a non-sensible datatype */

    /* Create the file */
    fid = H5Fcreate(MISC7_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create the dataspace */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create the compound datatype to commit*/
    tid = H5Tcreate(H5T_COMPOUND, (size_t)32);
    CHECK(tid, FAIL, "H5Tcreate");

    /* Attempt to commit an empty compound datatype */
    ret = H5Tcommit2(fid, MISC7_TYPENAME1, tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VERIFY(ret, FAIL, "H5Tcommit2");

    /* Attempt to use empty compound datatype to create dataset */
    did = H5Dcreate2(fid, MISC7_DSETNAME1, tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VERIFY(ret, FAIL, "H5Dcreate2");

    /* Add a field to the compound datatype */
    ret = H5Tinsert(tid, "a", (size_t)0, H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    /* Attempt to commit the compound datatype now - should work */
    ret = H5Tcommit2(fid, MISC7_TYPENAME1, tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");

    /* Attempt to use compound datatype to create dataset now - should work */
    did = H5Dcreate2(fid, MISC7_DSETNAME1, tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dcreate2");

    /* Close dataset */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close compound datatype */
    ret = H5Tclose(tid);
    CHECK(ret, FAIL, "H5Tclose");

    /* Create the enum datatype to commit*/
    tid = H5Tenum_create(H5T_NATIVE_INT);
    CHECK(tid, FAIL, "H5Tenum_create");

    /* Attempt to commit an empty enum datatype */
    ret = H5Tcommit2(fid, MISC7_TYPENAME2, tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VERIFY(ret, FAIL, "H5Tcommit2");

    /* Attempt to use empty enum datatype to create dataset */
    did = H5Dcreate2(fid, MISC7_DSETNAME2, tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VERIFY(did, FAIL, "H5Dcreate2");

    /* Add a member to the enum datatype */
    ret = H5Tenum_insert(tid, "a", &enum_value);
    CHECK(ret, FAIL, "H5Tenum_insert");

    /* Attempt to commit the enum datatype now - should work */
    ret = H5Tcommit2(fid, MISC7_TYPENAME2, tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");

    /* Attempt to use enum datatype to create dataset now - should work */
    did = H5Dcreate2(fid, MISC7_DSETNAME2, tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dcreate2");

    /* Close dataset */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close enum datatype */
    ret = H5Tclose(tid);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

} /* end test_misc7() */

/****************************************************************
**
**  test_misc8(): Test storage size of various types of dataset
**      storage methods.
**
****************************************************************/
static void
test_misc8(void)
{
    hid_t fid, did, sid;
    hid_t fapl;                 /* File access property list */
    hid_t dcpl;                 /* Dataset creation property list */
    int rank=MISC8_RANK;
    hsize_t dims[MISC8_RANK]={MISC8_DIM0,MISC8_DIM1};
    hsize_t chunk_dims[MISC8_RANK]={MISC8_CHUNK_DIM0,MISC8_CHUNK_DIM1};
    hsize_t storage_size;       /* Number of bytes of raw data storage used */
    int *wdata;                 /* Data to write */
    int *tdata;                 /* Temporary pointer to data write */
#ifdef VERIFY_DATA
    int *rdata;                 /* Data to read */
    int *tdata2;                /* Temporary pointer to data to read */
#endif /* VERIFY_DATA */
    unsigned u,v;               /* Local index variables */
    int mdc_nelmts;             /* Metadata number of elements */
    size_t rdcc_nelmts;         /* Raw data number of elements */
    size_t rdcc_nbytes;         /* Raw data number of bytes */
    double rdcc_w0;             /* Raw data write percentage */
    hsize_t start[MISC8_RANK];  /* Hyperslab start */
    hsize_t count[MISC8_RANK];  /* Hyperslab block count */
    herr_t ret;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing dataset storage sizes\n"));

    /* Allocate space for the data to write & read */
    wdata=malloc(sizeof(int)*MISC8_DIM0*MISC8_DIM1);
    CHECK(wdata,NULL,"malloc");
#ifdef VERIFY_DATA
    rdata=malloc(sizeof(int)*MISC8_DIM0*MISC8_DIM1);
    CHECK(rdata,NULL,"malloc");
#endif /* VERIFY_DATA */

    /* Initialize values */
    tdata=wdata;
    for(u=0; u<MISC8_DIM0; u++)
        for(v=0; v<MISC8_DIM1; v++)
            *tdata++=((u*MISC8_DIM1)+v)%13;

    /* Create a file acccess property list */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");

    /* Get the default file access properties for caching */
    ret=H5Pget_cache(fapl,&mdc_nelmts,&rdcc_nelmts,&rdcc_nbytes,&rdcc_w0);
    CHECK(ret, FAIL, "H5Pget_cache");

    /* Decrease the size of the raw data cache */
    rdcc_nbytes=0;

    /* Set the file access properties for caching */
    ret=H5Pset_cache(fapl,mdc_nelmts,rdcc_nelmts,rdcc_nbytes,rdcc_w0);
    CHECK(ret, FAIL, "H5Pset_cache");

    /* Create the file */
    fid=H5Fcreate(MISC8_FILE,H5F_ACC_TRUNC,H5P_DEFAULT,fapl);
    CHECK(fid,FAIL,"H5Fcreate");

    /* Close file access property list */
    ret=H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Create a simple dataspace */
    sid = H5Screate_simple(rank, dims, NULL);
    CHECK(sid, FAIL, "H5Screate_simple");

    /* Select a hyperslab which coincides with chunk boundaries */
    /* (For later use) */
    start[0] = 1; start[1] = 1;
    count[0] = (MISC8_CHUNK_DIM0 * 2) - 1; count[1] = (MISC8_CHUNK_DIM1 * 2) - 1;
    ret = H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, NULL, count, NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Create a dataset creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /*          I.  contiguous dataset tests    */

    ret = H5Pset_layout(dcpl, H5D_CONTIGUOUS);
    CHECK(ret, FAIL, "H5Pset_layout");

    /* Set the space allocation time to early */
    ret = H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY);
    CHECK(ret, FAIL, "H5Pset_alloc_time");

    /* Create a contiguous dataset, with space allocation early */
    did = H5Dcreate2(fid, MISC8_DSETNAME1, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dcreate2");

    /* Check the storage size */
    storage_size = H5Dget_storage_size(did);
    CHECK(storage_size, 0, "H5Dget_storage_size");
    VERIFY(storage_size, MISC8_DIM0 * MISC8_DIM1 * H5Tget_size(H5T_NATIVE_INT), "H5Dget_storage_size");

    /* Close dataset ID */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

#ifndef H5_HAVE_PARALLEL
    /* Set the space allocation time to late */
    ret = H5Pset_alloc_time(dcpl,H5D_ALLOC_TIME_LATE);
    CHECK(ret, FAIL, "H5Pset_alloc_time");

    /* Create a contiguous dataset, with space allocation late */
    did = H5Dcreate2(fid, MISC8_DSETNAME2, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dcreate2");

    /* Check the storage size before data is written */
    storage_size = H5Dget_storage_size(did);
    VERIFY(storage_size, 0, "H5Dget_storage_size");

    /* Write data */
    ret = H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Check the storage size after data is written */
    storage_size = H5Dget_storage_size(did);
    CHECK(storage_size, 0, "H5Dget_storage_size");
    VERIFY(storage_size, MISC8_DIM0 * MISC8_DIM1 * H5Tget_size(H5T_NATIVE_INT), "H5Dget_storage_size");

    /* Close dataset ID */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    /* Set the space allocation time to incremental */
    ret = H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_INCR);
    CHECK(ret, FAIL, "H5Pset_alloc_time");

    /* Create a contiguous dataset, with space allocation late */
    did = H5Dcreate2(fid, MISC8_DSETNAME3, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dcreate2");

    /* Check the storage size before data is written */
    storage_size = H5Dget_storage_size(did);
    VERIFY(storage_size, 0, "H5Dget_storage_size");

    /* Write data */
    ret = H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Check the storage size after data is written */
    storage_size = H5Dget_storage_size(did);
    CHECK(storage_size, 0, "H5Dget_storage_size");
    VERIFY(storage_size, MISC8_DIM0 * MISC8_DIM1 * H5Tget_size(H5T_NATIVE_INT), "H5Dget_storage_size");

    /* Close dataset ID */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");
#endif /* H5_HAVE_PARALLEL */

    /*          II.     compact dataset tests           */
    ret = H5Pset_layout(dcpl, H5D_COMPACT);
    CHECK(ret, FAIL, "H5Pset_layout");

    /* Set the space allocation time to late */
    ret = H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_LATE);
    CHECK(ret, FAIL, "H5Pset_alloc_time");

    /* Create a contiguous dataset, with space allocation late */
    /* Should fail */
    did = H5Dcreate2(fid, MISC8_DSETNAME4, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    VERIFY(did, FAIL, "H5Dcreate2");

    /* Set the space allocation time to incremental */
    ret = H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_INCR);
    CHECK(ret, FAIL, "H5Pset_alloc_time");

    /* Create a contiguous dataset, with space allocation incremental */
    /* Should fail */
    did = H5Dcreate2(fid, MISC8_DSETNAME4, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    VERIFY(did, FAIL, "H5Dcreate2");

    /* Set the space allocation time to early */
    ret = H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY);
    CHECK(ret, FAIL, "H5Pset_alloc_time");

    /* Set the fill time to allocation */
    ret = H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC);
    CHECK(ret, FAIL, "H5Pset_alloc_time");

    /* Create a contiguous dataset, with space allocation early */
    did = H5Dcreate2(fid, MISC8_DSETNAME4, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dcreate2");

    /* Check the storage size */
    storage_size = H5Dget_storage_size(did);
    CHECK(storage_size, 0, "H5Dget_storage_size");
    VERIFY(storage_size, MISC8_DIM0 * MISC8_DIM1 * H5Tget_size(H5T_NATIVE_INT), "H5Dget_storage_size");

    /* Close dataset ID */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");


    /*          III.    chunked dataset tests           */

    ret = H5Pset_layout(dcpl, H5D_CHUNKED);
    CHECK(ret, FAIL, "H5Pset_layout");

    /* Set the space allocation time to early */
    ret = H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY);
    CHECK(ret, FAIL, "H5Pset_alloc_time");

    /* Use chunked storage for this dataset */
    ret = H5Pset_chunk(dcpl, rank, chunk_dims);
    CHECK(ret, FAIL, "H5Pset_chunk");

    /* Create a chunked dataset, with space allocation early */
    did = H5Dcreate2(fid, MISC8_DSETNAME5, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dcreate2");

    /* Check the storage size after data is written */
    storage_size = H5Dget_storage_size(did);
    CHECK(storage_size, 0, "H5Dget_storage_size");
    VERIFY(storage_size, MISC8_DIM0 * MISC8_DIM1 * H5Tget_size(H5T_NATIVE_INT), "H5Dget_storage_size");

    /* Close dataset ID */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

#ifndef H5_HAVE_PARALLEL
    /* Set the space allocation time to late */
    ret = H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_LATE);
    CHECK(ret, FAIL, "H5Pset_alloc_time");

    /* Use chunked storage for this dataset */
    ret = H5Pset_chunk(dcpl, rank, chunk_dims);
    CHECK(ret, FAIL, "H5Pset_chunk");

    /* Create a chunked dataset, with space allocation late */
    did = H5Dcreate2(fid, MISC8_DSETNAME6, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dcreate2");

    /* Check the storage size after dataset is created */
    storage_size = H5Dget_storage_size(did);
    VERIFY(storage_size, 0, "H5Dget_storage_size");

    /* Write part of the dataset */
    ret = H5Dwrite(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, wdata);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Check the storage size after data is written */
    storage_size = H5Dget_storage_size(did);
    CHECK(storage_size, 0, "H5Dget_storage_size");
    VERIFY(storage_size, MISC8_DIM0 * MISC8_DIM1 * H5Tget_size(H5T_NATIVE_INT), "H5Dget_storage_size");

    /* Close dataset ID */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    /* Set the space allocation time to incremental */
    ret = H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_INCR);
    CHECK(ret, FAIL, "H5Pset_alloc_time");

    /* Create a chunked dataset, with space allocation incremental */
    did = H5Dcreate2(fid, MISC8_DSETNAME7, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dcreate2");

    /* Check the storage size before data is written */
    storage_size = H5Dget_storage_size(did);
    VERIFY(storage_size, 0, "H5Dget_storage_size");

    /* Write part of the dataset */
    ret = H5Dwrite(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, wdata);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Check the storage size after only four chunks are written */
    storage_size = H5Dget_storage_size(did);
    VERIFY(storage_size, 4 * MISC8_CHUNK_DIM0 * MISC8_CHUNK_DIM1 * H5Tget_size(H5T_NATIVE_INT), "H5Dget_storage_size");

    /* Write entire dataset */
    ret = H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    CHECK(ret, FAIL, "H5Dwrite");

#ifdef VERIFY_DATA
    /* Read data */
    ret = H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);
    CHECK(ret, FAIL, "H5Dread");

    /* Check values written */
    tdata=wdata;
    tdata2=rdata;
    for(u=0; u<MISC8_DIM0; u++)
        for(v=0; v<MISC8_DIM1; v++,tdata++,tdata2++)
            if(*tdata!=*tdata2)
                TestErrPrintf("Error on line %d: u=%u, v=%d, *tdata=%d, *tdata2=%d\n",__LINE__,(unsigned)u,(unsigned)v,(int)*tdata,(int)*tdata2);
#endif /* VERIFY_DATA */

    /* Check the storage size after data is written */
    storage_size = H5Dget_storage_size(did);
    CHECK(storage_size, 0, "H5Dget_storage_size");
    VERIFY(storage_size, MISC8_DIM0*MISC8_DIM1*H5Tget_size(H5T_NATIVE_INT), "H5Dget_storage_size");

    /* Close dataset ID */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");
#endif /* H5_HAVE_PARALLEL */

    /* Set the space allocation time to early */
    ret = H5Pset_alloc_time(dcpl,H5D_ALLOC_TIME_EARLY);
    CHECK(ret, FAIL, "H5Pset_alloc_time");

    /* Use compression as well as chunking for these datasets */
#ifdef H5_HAVE_FILTER_DEFLATE
    ret = H5Pset_deflate(dcpl,9);
    CHECK(ret, FAIL, "H5Pset_deflate");
#endif /* end H5_HAVE_FILTER_DEFLATE */

    /* Create a chunked dataset, with space allocation early */
    did = H5Dcreate2(fid, MISC8_DSETNAME8, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dcreate2");

    /* Write part of the dataset */
    ret = H5Dwrite(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, wdata);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Check the storage size after data is written */
    storage_size = H5Dget_storage_size(did);
    CHECK(storage_size, 0, "H5Dget_storage_size");
#ifdef H5_HAVE_FILTER_DEFLATE
    if(storage_size >= (MISC8_DIM0 * MISC8_DIM1 * H5Tget_size(H5T_NATIVE_INT)))
        TestErrPrintf("Error on line %d: data wasn't compressed! storage_size=%u\n",__LINE__,(unsigned)storage_size);
#else /* Compression is not configured */
    if(storage_size != (MISC8_DIM0 * MISC8_DIM1 * H5Tget_size(H5T_NATIVE_INT)))
        TestErrPrintf("Error on line %d: wrong storage size! storage_size=%u\n",__LINE__,(unsigned)storage_size);
#endif /* H5_HAVE_FILTER_DEFLATE */

    /* Close dataset ID */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

#ifndef H5_HAVE_PARALLEL
    /* Set the space allocation time to late */
    ret = H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_LATE);
    CHECK(ret, FAIL, "H5Pset_alloc_time");

    /* Create a chunked dataset, with space allocation late */
    did = H5Dcreate2(fid, MISC8_DSETNAME9, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dcreate2");

    /* Check the storage size before data is written */
    storage_size = H5Dget_storage_size(did);
    VERIFY(storage_size, 0, "H5Dget_storage_size");

    /* Write part of the dataset */
    ret = H5Dwrite(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, wdata);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Check the storage size after only four chunks are written */
    storage_size = H5Dget_storage_size(did);
    CHECK(storage_size, 0, "H5Dget_storage_size");
#ifdef H5_HAVE_FILTER_DEFLATE
    if(storage_size >= (MISC8_DIM0 * MISC8_DIM1 * H5Tget_size(H5T_NATIVE_INT)))
        TestErrPrintf("Error on line %d: data wasn't compressed! storage_size=%u\n",__LINE__,(unsigned)storage_size);
#else /* Compression is not configured */
    if(storage_size != (MISC8_DIM0 * MISC8_DIM1 * H5Tget_size(H5T_NATIVE_INT)))
        TestErrPrintf("Error on line %d: wrong storage size! storage_size=%u\n",__LINE__,(unsigned)storage_size);
#endif /* H5_HAVE_FILTER_DEFLATE */

    /* Write entire dataset */
    ret = H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    CHECK(ret, FAIL, "H5Dwrite");

#ifdef VERIFY_DATA
    /* Read data */
    ret = H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);
    CHECK(ret, FAIL, "H5Dread");

    /* Check values written */
    tdata=wdata;
    tdata2=rdata;
    for(u=0; u<MISC8_DIM0; u++)
        for(v=0; v<MISC8_DIM1; v++,tdata++,tdata2++)
            if(*tdata!=*tdata2)
                TestErrPrintf("Error on line %d: u=%u, v=%d, *tdata=%d, *tdata2=%d\n",__LINE__,(unsigned)u,(unsigned)v,(int)*tdata,(int)*tdata2);
#endif /* VERIFY_DATA */

    /* Check the storage size after data is written */
    storage_size=H5Dget_storage_size(did);
    CHECK(storage_size, 0, "H5Dget_storage_size");
#ifdef H5_HAVE_FILTER_DEFLATE
    if(storage_size>=(MISC8_DIM0*MISC8_DIM1*H5Tget_size(H5T_NATIVE_INT)))
        TestErrPrintf("Error on line %d: data wasn't compressed! storage_size=%u\n",__LINE__,(unsigned)storage_size);
#else
    if(storage_size!=(MISC8_DIM0*MISC8_DIM1*H5Tget_size(H5T_NATIVE_INT)))
        TestErrPrintf("Error on line %d: wrong storage size! storage_size=%u\n",__LINE__,(unsigned)storage_size);
#endif /*H5_HAVE_FILTER_DEFLATE*/

    /* Close dataset ID */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    /* Set the space allocation time to incremental */
    ret = H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_INCR);
    CHECK(ret, FAIL, "H5Pset_alloc_time");

    /* Create a chunked dataset, with space allocation incremental */
    did = H5Dcreate2(fid, MISC8_DSETNAME10, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dcreate2");

    /* Check the storage size before data is written */
    storage_size = H5Dget_storage_size(did);
    VERIFY(storage_size, 0, "H5Dget_storage_size");

    /* Write part of the dataset */
    ret = H5Dwrite(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, wdata);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Check the storage size after only four chunks are written */
    storage_size = H5Dget_storage_size(did);
    CHECK(storage_size, 0, "H5Dget_storage_size");
#ifdef H5_HAVE_FILTER_DEFLATE
    if(storage_size >= (4 * MISC8_CHUNK_DIM0 * MISC8_CHUNK_DIM1 * H5Tget_size(H5T_NATIVE_INT)))
        TestErrPrintf("Error on line %d: data wasn't compressed! storage_size=%u\n",__LINE__,(unsigned)storage_size);
#else /* Compression is not configured */
    if(storage_size != (4 * MISC8_CHUNK_DIM0 * MISC8_CHUNK_DIM1 * H5Tget_size(H5T_NATIVE_INT)))
        TestErrPrintf("Error on line %d: wrong storage size! storage_size=%u\n",__LINE__,(unsigned)storage_size);
#endif /* H5_HAVE_FILTER_DEFLATE */

    /* Write entire dataset */
    ret = H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    CHECK(ret, FAIL, "H5Dwrite");

#ifdef VERIFY_DATA
    /* Read data */
    ret = H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);
    CHECK(ret, FAIL, "H5Dread");

    /* Check values written */
    tdata=wdata;
    tdata2=rdata;
    for(u=0; u<MISC8_DIM0; u++)
        for(v=0; v<MISC8_DIM1; v++,tdata++,tdata2++)
            if(*tdata!=*tdata2)
                TestErrPrintf("Error on line %d: u=%u, v=%d, *tdata=%d, *tdata2=%d\n",__LINE__,(unsigned)u,(unsigned)v,(int)*tdata,(int)*tdata2);
#endif /* VERIFY_DATA */

    /* Check the storage size after data is written */
    storage_size=H5Dget_storage_size(did);
    CHECK(storage_size, 0, "H5Dget_storage_size");
#ifdef H5_HAVE_FILTER_DEFLATE
    if(storage_size>=(MISC8_DIM0*MISC8_DIM1*H5Tget_size(H5T_NATIVE_INT)))
        TestErrPrintf("Error on line %d: data wasn't compressed! storage_size=%u\n",__LINE__,(unsigned)storage_size);
#else
    if(storage_size!=(MISC8_DIM0*MISC8_DIM1*H5Tget_size(H5T_NATIVE_INT)))
        TestErrPrintf("Error on line %d: wrong storage size! storage_size=%u\n",__LINE__,(unsigned)storage_size);
#endif /*H5_HAVE_FILTER_DEFLATE*/

    /* Close dataset ID */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");
#endif /* H5_HAVE_PARALLEL */

    /* Close dataset creation property list */
    ret=H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close dataspace */
    ret=H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close file */
    ret=H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Free the read & write buffers */
    free(wdata);
#ifdef VERIFY_DATA
    free(rdata);
#endif /* VERIFY_DATA */
} /* end test_misc8() */

/****************************************************************
**
**  test_misc9(): Test that H5Fopen() does not succeed for core
**      files, H5Fcreate() must be used to open them.
**
****************************************************************/
static void
test_misc9(void)
{
    hid_t fapl, fid;
    herr_t ret;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing core file opening\n"));

    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");

    ret = H5Pset_fapl_core(fapl, (size_t)1024, 0);
    CHECK(ret, FAIL, "H5Pset_fapl_core");

    fid = H5Fopen(MISC9_FILE, H5F_ACC_RDWR, fapl);
    VERIFY(fid,FAIL,"H5Fopen");

    ret=H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pset_fapl_core");
} /* end test_misc9() */

/****************************************************************
**
**  test_misc10(): Test opening a dataset created with an older
**      version of the library (shares the tmtimeo.h5 file with the mtime.c
**      test - see notes in gen_old_mtime.c for notes on generating this
**      data file) and using the dataset creation property list from
**      that dataset to create a dataset with the current version of
**      the library.  Also tests using file creation property in same way.
**
****************************************************************/
static void
test_misc10(void)
{
    hid_t       file, file_new; /* File IDs for old & new files */
    hid_t       fcpl;           /* File creation property list */
    hid_t       dataset, dataset_new;   /* Dataset IDs for old & new datasets */
    hid_t       dcpl;           /* Dataset creation property list */
    hid_t       space, type;    /* Old dataset's dataspace & datatype */
    char testfile[512]="";          /* Character buffer for corrected test file name */
    char *srcdir = HDgetenv("srcdir");    /* Pointer to the directory the source code is located within */
    herr_t      ret;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing using old dataset creation property list\n"));

    /* Generate the correct name for the test file, by prepending the source path */
    if(srcdir && ((HDstrlen(srcdir) + HDstrlen(MISC10_FILE_OLD) + 1) < sizeof(testfile))) {
        HDstrcpy(testfile, srcdir);
        HDstrcat(testfile, "/");
    }
    HDstrcat(testfile, MISC10_FILE_OLD);

    /*
     * Open the old file and the dataset and get old settings.
     */
    file =    H5Fopen(testfile, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fopen");
    fcpl =  H5Fget_create_plist(file);
    CHECK(fcpl, FAIL, "H5Fget_create_plist");

    dataset = H5Dopen2(file, MISC10_DSETNAME, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dopen2");
    dcpl =  H5Dget_create_plist(dataset);
    CHECK(dcpl, FAIL, "H5Dget_create_plist");
    space =   H5Dget_space(dataset);
    CHECK(space, FAIL, "H5Dget_space");
    type =    H5Dget_type(dataset);
    CHECK(type, FAIL, "H5Dget_type");

    /* Create new file & dataset */
    file_new = H5Fcreate(MISC10_FILE_NEW, H5F_ACC_TRUNC , fcpl, H5P_DEFAULT);
    CHECK(file_new, FAIL, "H5Fcreate");

    dataset_new = H5Dcreate2(file_new, MISC10_DSETNAME, type, space, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(dataset_new, FAIL, "H5Dcreate2");

    /* Close new dataset & file */
    ret = H5Dclose(dataset_new);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Fclose(file_new);
    CHECK(ret, FAIL, "H5Fclose");

    /* Close old dataset information */
    ret = H5Tclose(type);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Sclose(space);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close old file information */
    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");
    ret = H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");
} /* end test_misc10() */

/****************************************************************
**
**  test_misc11(): Test that all properties in a file creation property
**      list are stored correctly in the file and can be retrieved
**      when the file is re-opened.
**
****************************************************************/
static void
test_misc11(void)
{
    hid_t       file;           /* File IDs for old & new files */
    hid_t       fcpl;           /* File creation property list */
    hsize_t     userblock;      /* Userblock size retrieved from FCPL */
    size_t      off_size;       /* Size of offsets in the file */
    size_t      len_size;       /* Size of lengths in the file */
    unsigned    sym_ik;         /* Symbol table B-tree initial 'K' value */
    unsigned    istore_ik;      /* Indexed storage B-tree initial 'K' value */
    unsigned    sym_lk;         /* Symbol table B-tree leaf 'K' value */
    unsigned super;             /* Superblock version # */
    unsigned freelist;          /* Free list version # */
    unsigned stab;              /* Symbol table entry version # */
    unsigned shhdr;             /* Shared object header version # */
    unsigned nindexes;          /* Shared message number of indexes */
    herr_t      ret;            /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing file creation properties retrieved correctly\n"));

    /* Creating a file with the default file creation property list should
     * create a version 0 superblock
     */

    /* Create file with default file creation property list */
    file= H5Fcreate(MISC11_FILE, H5F_ACC_TRUNC , H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fcreate");

    /* Get the file's dataset creation property list */
    fcpl =  H5Fget_create_plist(file);
    CHECK(fcpl, FAIL, "H5Fget_create_plist");

    /* Get the file's version information */
    ret=H5Pget_version(fcpl, &super, &freelist, &stab, &shhdr);
    CHECK(ret, FAIL, "H5Pget_version");
    VERIFY(super,0,"H5Pget_version");
    VERIFY(freelist,0,"H5Pget_version");
    VERIFY(stab,0,"H5Pget_version");
    VERIFY(shhdr,0,"H5Pget_version");

    /* Close FCPL */
    ret=H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close file */
    ret=H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");


    /* Create a file creation property list */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    CHECK(fcpl, FAIL, "H5Pcreate");

    /* Set all the properties in the FCPL */
    ret=H5Pset_userblock(fcpl,(hsize_t)MISC11_USERBLOCK);
    CHECK(ret, FAIL, "H5Pset_userblock");

    ret = H5Pset_sizes(fcpl, (size_t)MISC11_SIZEOF_OFF, (size_t)MISC11_SIZEOF_LEN);
    CHECK(ret, FAIL, "H5Pset_sizes");

    ret=H5Pset_sym_k(fcpl,MISC11_SYM_IK,MISC11_SYM_LK);
    CHECK(ret, FAIL, "H5Pset_sym_k");

    ret=H5Pset_istore_k(fcpl,MISC11_ISTORE_IK);
    CHECK(ret, FAIL, "H5Pset_istore_k");

    ret=H5Pset_shared_mesg_nindexes(fcpl,MISC11_NINDEXES);
    CHECK(ret, FAIL, "H5Pset_shared_mesg");

    /* Creating a file with the non-default file creation property list should
     * create a version 1 superblock
     */

    /* Create file with custom file creation property list */
    file= H5Fcreate(MISC11_FILE, H5F_ACC_TRUNC , fcpl, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fcreate");

    /* Close FCPL */
    ret=H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Get the file's dataset creation property list */
    fcpl =  H5Fget_create_plist(file);
    CHECK(fcpl, FAIL, "H5Fget_create_plist");

    /* Get the file's version information */
    ret=H5Pget_version(fcpl, &super, &freelist, &stab, &shhdr);
    CHECK(ret, FAIL, "H5Pget_version");
    VERIFY(super,2,"H5Pget_version");
    VERIFY(freelist,0,"H5Pget_version");
    VERIFY(stab,0,"H5Pget_version");
    VERIFY(shhdr,0,"H5Pget_version");

    /* Close FCPL */
    ret=H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close file */
    ret=H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open the file */
    file = H5Fopen(MISC11_FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fcreate");

    /* Get the file's dataset creation property list */
    fcpl =  H5Fget_create_plist(file);
    CHECK(fcpl, FAIL, "H5Fget_create_plist");

    /* Get the file's version information */
    ret=H5Pget_version(fcpl, &super, &freelist, &stab, &shhdr);
    CHECK(ret, FAIL, "H5Pget_version");
    VERIFY(super,2,"H5Pget_version");
    VERIFY(freelist,0,"H5Pget_version");
    VERIFY(stab,0,"H5Pget_version");
    VERIFY(shhdr,0,"H5Pget_version");

    /* Retrieve all the property values & check them */
    ret=H5Pget_userblock(fcpl,&userblock);
    CHECK(ret, FAIL, "H5Pget_userblock");
    VERIFY(userblock, MISC11_USERBLOCK, "H5Pget_userblock");

    ret=H5Pget_sizes(fcpl,&off_size,&len_size);
    CHECK(ret, FAIL, "H5Pget_sizes");
    VERIFY(off_size, MISC11_SIZEOF_OFF, "H5Pget_sizes");
    VERIFY(len_size, MISC11_SIZEOF_LEN, "H5Pget_sizes");

    ret=H5Pget_sym_k(fcpl,&sym_ik,&sym_lk);
    CHECK(ret, FAIL, "H5Pget_sym_k");
    VERIFY(sym_ik, MISC11_SYM_IK, "H5Pget_sym_k");
    VERIFY(sym_lk, MISC11_SYM_LK, "H5Pget_sym_k");

    ret=H5Pget_istore_k(fcpl,&istore_ik);
    CHECK(ret, FAIL, "H5Pget_istore_k");
    VERIFY(istore_ik, MISC11_ISTORE_IK, "H5Pget_istore_k");

    ret=H5Pget_shared_mesg_nindexes(fcpl,&nindexes);
    CHECK(ret, FAIL, "H5Pget_shared_mesg_nindexes");
    VERIFY(nindexes, MISC11_NINDEXES, "H5Pget_shared_mesg_nindexes");

    /* Close file */
    ret=H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    /* Close FCPL */
    ret=H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");
} /* end test_misc11() */

/****************************************************************
**
**  test_misc12(): Test that VL-types operate correctly in chunked
**      datasets that are extended.
**
****************************************************************/
static void
test_misc12(void)
{
    const char *wdata [MISC12_SPACE1_DIM1]= {
        "Four score and seven years ago our forefathers brought forth on this continent a new nation,",
        "conceived in liberty and dedicated to the proposition that all men are created equal.",
        "Now we are engaged in a great civil war,",
        "testing whether that nation or any nation so conceived and so dedicated can long endure."
        };
    const char *wdata1 [MISC12_APPEND_SIZE]= {
       "O Gloria inmarcesible! O Jubilo inmortal! En surcos de dolores, el",
       "bien germina ya! Ceso la horrible noche, La libertad sublime",
       "derrama las auroras de su invencible luz.",
       "La humanidad entera, que entre cadenas gime, comprende",
       "las palabras del que murio en la cruz."
        };
    char        *rdata [MISC12_SPACE1_DIM1+MISC12_APPEND_SIZE]; /* Information read in */
    hid_t		fid1;
    hid_t		dataset;
    hid_t		sid1, space, memspace;
    hid_t		tid1, cparms;
    hsize_t		dims1[] = {MISC12_SPACE1_DIM1};
    hsize_t		dimsn[] = {MISC12_APPEND_SIZE};
    hsize_t		maxdims1[1] = {H5S_UNLIMITED};
    hsize_t		chkdims1[1] = {MISC12_CHUNK_SIZE};
    hsize_t     	newsize[1] = {MISC12_SPACE1_DIM1+MISC12_APPEND_SIZE};
    hsize_t    		offset[1] = {MISC12_SPACE1_DIM1};
    hsize_t     	count[1] = {MISC12_APPEND_SIZE};
    int                 i;          /* counting variable */
    herr_t		ret;		/* Generic return value  */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing VL-type in chunked dataset\n"));

    /* This test requirese a relatively "fresh" library environment */
    ret = H5garbage_collect();
    CHECK(ret, FAIL, "H5garbage_collect");

    /* Create file */
    fid1 = H5Fcreate(MISC12_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(MISC12_SPACE1_RANK, dims1, maxdims1);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a datatype to refer to */
    tid1 = H5Tcopy(H5T_C_S1);
    CHECK(tid1, FAIL, "H5Tcopy");

    ret = H5Tset_size(tid1, H5T_VARIABLE);
    CHECK(ret, FAIL, "H5Tset_size");

    cparms = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(cparms, FAIL, "H5Pcreate");

    ret = H5Pset_chunk(cparms, 1, chkdims1);
    CHECK(ret, FAIL, "H5Pset_chunk");

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, MISC12_DSET_NAME, tid1, sid1, H5P_DEFAULT, cparms, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Write dataset to disk */
    ret = H5Dwrite(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Extend dataset */
    ret = H5Dset_extent(dataset, newsize);
    CHECK(ret, FAIL, "H5Dset_extent");

    memspace = H5Screate_simple(MISC12_SPACE1_RANK, dimsn, NULL);
    CHECK(memspace, FAIL, "H5Screate_simple");

    space = H5Dget_space(dataset);
    CHECK(space, FAIL, "H5Dget_space");

    ret = H5Sselect_hyperslab(space, H5S_SELECT_SET, offset, NULL, count, NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Write data to new portion of dataset */
    ret = H5Dwrite(dataset, tid1, memspace, space, H5P_DEFAULT, wdata1);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Read all data back */
    ret = H5Dread(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);
    CHECK(ret, FAIL, "H5Dread");

    for(i = 0; i < MISC12_SPACE1_DIM1; i++)
        if(HDstrcmp(wdata[i], rdata[i]))
            TestErrPrintf("Error on line %d: wdata[%d]=%s, rdata[%d]=%s\n", __LINE__, i, wdata[i], i, rdata[i]);
    for(; i < (MISC12_SPACE1_DIM1 + MISC12_APPEND_SIZE); i++)
        if(HDstrcmp(wdata1[i - MISC12_SPACE1_DIM1], rdata[i]))
            TestErrPrintf("Error on line %d: wdata1[%d]=%s, rdata[%d]=%s\n", __LINE__, i - MISC12_SPACE1_DIM1, wdata1[i - MISC12_SPACE1_DIM1], i, rdata[i]);

    ret = H5Sselect_all(space);
    CHECK(ret, FAIL, "H5Sselect_all");

    /* Reclaim VL data memory */
    ret = H5Dvlen_reclaim(tid1, space, H5P_DEFAULT, rdata);
    CHECK(ret, FAIL, "H5Dvlen_reclaim");

    /* Close Everything */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Sclose(space);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(memspace);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Pclose(cparms);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
} /* end test_misc12() */

/* Various routines for misc. 13 test */
static void
init_data(void)
{
    unsigned u,v;       /* Local index variables */

    for(u=0; u<MISC13_DIM1; u++)
        for(v=0; v<MISC13_DIM2; v++)
            m13_data[u][v]=(u*MISC13_DIM2)+v;
}

static int
verify_data(void)
{
    unsigned u,v;       /* Local index variables */

    for(u=0; u<MISC13_DIM1; u++)
        for(v=0; v<MISC13_DIM2; v++)
            if(m13_data[u][v]!=m13_rdata[u][v])
                return(-1);
    return(0);
}

static void
create_dataset(hid_t loc_id, const char *name, hid_t dcpl)
{
    hid_t dsid;         /* Dataset ID */
    hid_t sid;          /* Dataspace ID */
    hsize_t dims[MISC13_RANK]; /* Dataset dimensions */
    herr_t ret;         /* Generic return value */

    /* Create dataspace for use with dataset */
    dims[0] = MISC13_DIM1;
    dims[1] = MISC13_DIM2;
    sid = H5Screate_simple(MISC13_RANK, dims, NULL);
    CHECK(sid, FAIL, "H5Screate_simple");

    /* Create contiguous dataset in root group */
    dsid = H5Dcreate2(loc_id, name, H5T_NATIVE_UINT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(dsid, FAIL, "H5Dcreate2");

    /* Write some data to dataset */
    ret = H5Dwrite(dsid, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL, H5P_DEFAULT, m13_data);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close the contiguous dataset */
    ret = H5Dclose(dsid);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close the dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
}

static void
verify_dataset(hid_t loc_id, const char *name)
{
    hid_t dsid;         /* Dataset ID */
    herr_t ret;         /* Generic return value */

    /* Open the contiguous dataset in the root group */
    dsid = H5Dopen2(loc_id, name, H5P_DEFAULT);
    CHECK(dsid, FAIL, "H5Dopen2");

    /* Read the data */
    ret = H5Dread(dsid, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL, H5P_DEFAULT, m13_rdata);
    CHECK(ret, FAIL, "H5Dread");

    /* Verify that the data is correct */
    ret=verify_data();
    CHECK(ret, FAIL, "verify_data");

    /* Close the contiguous dataset */
    ret = H5Dclose(dsid);
    CHECK(ret, FAIL, "H5Dclose");
}

static void
create_hdf_file(const char *name)
{
    hid_t fid;          /* File ID */
    hid_t gid,gid2;     /* Group IDs */
    hid_t tid;          /* Datatype ID */
    hid_t dcpl;         /* Dataset creation property list ID */
    hsize_t chunk_dims[MISC13_RANK]; /* Chunk dimensions */
    herr_t ret;         /* Generic return value */

    /* Create file */
    fid=H5Fcreate(name, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create DCPL for use with datasets */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Set the DCPL to be chunked */
    ret = H5Pset_layout(dcpl, H5D_CHUNKED);
    CHECK(ret, FAIL, "H5Pset_layout");

    /* Use chunked storage for this DCPL */
    chunk_dims[0] = MISC13_CHUNK_DIM1;
    chunk_dims[1] = MISC13_CHUNK_DIM2;
    ret = H5Pset_chunk(dcpl, MISC13_RANK, chunk_dims);
    CHECK(ret, FAIL, "H5Pset_chunk");

    /* Create contiguous dataset in root group */
    create_dataset(fid, MISC13_DSET1_NAME, H5P_DEFAULT);

    /* Create chunked dataset in root group */
    create_dataset(fid, MISC13_DSET2_NAME, dcpl);

    /* Create a datatype to commit to the file */
    tid = H5Tcopy(H5T_NATIVE_INT);
    CHECK(tid, FAIL, "H5Tcopy");

    /* Create a named datatype in the root group */
    ret = H5Tcommit2(fid, MISC13_DTYPE_NAME, tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");

    /* Close named datatype */
    ret = H5Tclose(tid);
    CHECK(ret, FAIL, "H5Tclose");

    /* Create a group in the root group */
    gid = H5Gcreate2(fid, MISC13_GROUP1_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gcreate2");

    /* Create another group in the new group */
    gid2 = H5Gcreate2(gid, MISC13_GROUP2_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(gid2, FAIL, "H5Gcreate2");

    /* Close the second group */
    ret = H5Gclose(gid2);
    CHECK(ret, FAIL, "H5Gclose");

    /* Create contiguous dataset in new group */
    create_dataset(gid, MISC13_DSET1_NAME, H5P_DEFAULT);

    /* Create chunked dataset in new group */
    create_dataset(gid, MISC13_DSET2_NAME, dcpl);

    /* Create a datatype to commit to the new group */
    tid = H5Tcopy(H5T_NATIVE_INT);
    CHECK(tid, FAIL, "H5Tcopy");

    /* Create a named datatype in the new group */
    ret = H5Tcommit2(gid, MISC13_DTYPE_NAME, tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");

    /* Close named datatype */
    ret = H5Tclose(tid);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close the first group */
    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    /* Close the DCPL */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close the file */
    ret = H5Fclose(fid);
    assert(ret >= 0);
    CHECK(ret, FAIL, "H5Fclose");
}

static void
insert_user_block(const char *old_name, const char *new_name,const char *str,size_t size)
{
    FILE *new_fp, *old_fp;      /* Pointers to new & old files */
    void *user_block;           /* Pointer to user block to write to file */
    void *copy_buf;             /* Pointer to buffer for copying data */
    size_t written;             /* Amount of data written to new file */
    size_t read_in;             /* Amount of data read in from old file */
    int ret;                    /* Generic status value */

    /* Allocate space for the user block */
    user_block = HDcalloc(size, (size_t)1);
    CHECK(user_block, NULL, "HDcalloc");

    /* Copy in the user block data */
    HDmemcpy(user_block,str,strlen(str));

    /* Open the new file */
    new_fp=HDfopen(new_name,"wb");
    CHECK(new_fp, NULL, "HDfopen");

    /* Write the user block to the new file */
    written = HDfwrite(user_block, (size_t)1, size, new_fp);
    VERIFY(written, size, "HDfwrite");

    /* Open the old file */
    old_fp=fopen(old_name,"rb");
    CHECK(old_fp, NULL, "HDfopen");

    /* Allocate space for the copy buffer */
    copy_buf = HDmalloc((size_t)MISC13_COPY_BUF_SIZE);
    CHECK(copy_buf, NULL, "HDmalloc");

    /* Copy data from the old file to the new file */
    while((read_in = HDfread(copy_buf, (size_t)1, (size_t)MISC13_COPY_BUF_SIZE, old_fp)) > 0) {
        /* Write the data to the new file */
        written = HDfwrite(copy_buf, (size_t)1, read_in, new_fp);
        VERIFY(written, read_in, "HDfwrite");
    } /* end while */

    /* Close the old file */
    ret=HDfclose(old_fp);
    VERIFY(ret, 0, "HDfclose");

    /* Close the new file */
    ret=HDfclose(new_fp);
    VERIFY(ret, 0, "HDfclose");

    /* Free the copy buffer */
    free(copy_buf);

    /* Free the user block */
    free(user_block);
}

static void
verify_file(const char *name, hsize_t blk_size, unsigned check_new_data)
{
    hid_t fid;          /* File ID */
    hid_t gid,gid2;     /* Group IDs */
    hid_t tid;          /* Datatype ID */
    hid_t fcpl;         /* File creation property list ID */
    hsize_t userblock;  /* Userblock size retrieved from FCPL */
    herr_t ret;         /* Generic return value */

    /* Open the file */
    fid=H5Fopen(name, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /* Get the file's FCPL */
    fcpl=H5Fget_create_plist(fid);
    CHECK(fcpl, FAIL, "H5Fget_create_plist");

    /* Get the user block size for the file */
    ret=H5Pget_userblock(fcpl,&userblock);
    CHECK(ret, FAIL, "H5Pget_userblock");

    /* Check the userblock size */
    VERIFY(userblock, blk_size, "H5Pget_userblock");

    /* Close the FCPL */
    ret = H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Verify the contiguous dataset in the root group */
    verify_dataset(fid,MISC13_DSET1_NAME);

    /* Verify the chunked dataset in the root group */
    verify_dataset(fid,MISC13_DSET2_NAME);

    /* Verify the "new" contiguous dataset in the root group, if asked */
    if(check_new_data)
        verify_dataset(fid,MISC13_DSET3_NAME);

    /* Open the named datatype in the root group */
    tid = H5Topen2(fid, MISC13_DTYPE_NAME, H5P_DEFAULT);
    CHECK(tid, FAIL, "H5Topen2");

    /* Verify the type is correct */
    VERIFY(H5Tequal(tid,H5T_NATIVE_INT), TRUE, "H5Tequal");

    /* Close named datatype */
    ret=H5Tclose(tid);
    CHECK(ret, FAIL, "H5Tclose");

    /* Open the first group */
    gid = H5Gopen2(fid, MISC13_GROUP1_NAME, H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gopen2");

    /* Verify the contiguous dataset in the first group */
    verify_dataset(gid,MISC13_DSET1_NAME);

    /* Verify the chunked dataset in the first group */
    verify_dataset(gid,MISC13_DSET2_NAME);

    /* Open the named datatype in the first group */
    tid = H5Topen2(gid,MISC13_DTYPE_NAME, H5P_DEFAULT);
    CHECK(tid, FAIL, "H5Topen2");

    /* Verify the type is correct */
    VERIFY(H5Tequal(tid,H5T_NATIVE_INT), TRUE, "H5Tequal");

    /* Close named datatype */
    ret=H5Tclose(tid);
    CHECK(ret, FAIL, "H5Tclose");

    /* Open the second group */
    gid2 = H5Gopen2(gid, MISC13_GROUP2_NAME, H5P_DEFAULT);
    CHECK(gid2, FAIL, "H5Gopen2");

    /* Close the second group */
    ret = H5Gclose(gid2);
    CHECK(ret, FAIL, "H5Gclose");

    /* Close the first group */
    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
}

static void
add_to_new_file(const char *name)
{
    hid_t fid;          /* File ID */
    herr_t ret;         /* Generic return value */

    /* Open the file */
    fid=H5Fopen(name, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /* Create new contiguous dataset in root group */
    create_dataset(fid, MISC13_DSET3_NAME, H5P_DEFAULT);

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
}

/****************************************************************
**
**  test_misc13(): Test that file contents can be "slid down" by
**      inserting a user block in front of an existing file.
**
****************************************************************/
static void
test_misc13(void)
{
    /* Initialize data to write */
    init_data();

    /* Create first file, with no user block */
    create_hdf_file(MISC13_FILE_1);

    /* Verify file contents are correct */
    verify_file(MISC13_FILE_1,(hsize_t)0,0);

    /* Create a new file by inserting a user block in front of the first file */
    insert_user_block(MISC13_FILE_1, MISC13_FILE_2, "Test String", (size_t)MISC13_USERBLOCK_SIZE);

    /* Verify file contents are still correct */
    verify_file(MISC13_FILE_2,(hsize_t)MISC13_USERBLOCK_SIZE,0);

    /* Make certain we can modify the new file */
    add_to_new_file(MISC13_FILE_2);

    /* Verify file contents are still correct */
    verify_file(MISC13_FILE_2,(hsize_t)MISC13_USERBLOCK_SIZE,1);
} /* end test_misc13() */

/****************************************************************
**
**  test_misc14(): Test that file contents can be "slid down" by
**      inserting a user block in front of an existing file.
**
****************************************************************/
static void
test_misc14(void)
{
    hid_t file_id;              /* File ID */
    hid_t fapl;                 /* File access property list ID */
    hid_t DataSpace;            /* Dataspace ID */
    hid_t Dataset1;             /* Dataset ID #1 */
    hid_t Dataset2;             /* Dataset ID #2 */
    hid_t Dataset3;             /* Dataset ID #3 */
    double data1 = 5.0;         /* Data to write for dataset #1 */
    double data2 = 10.0;        /* Data to write for dataset #2 */
    double data3 = 15.0;        /* Data to write for dataset #3 */
    double rdata;               /* Data read in */
    herr_t ret;                 /* Generic return value */

    /* Test creating two datasets and deleting the second */

    /* Increase the metadata block size */
    /* (This makes certain that all the data blocks are allocated together) */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");

    ret = H5Pset_meta_block_size(fapl, (hsize_t)MISC14_METADATA_SIZE);
    CHECK(ret, FAIL, "H5Pset_meta_block_size");

    /* Create dataspace to use */
    DataSpace = H5Screate(H5S_SCALAR);
    CHECK(DataSpace, FAIL, "H5Screate");

    /* Open the file */
    file_id = H5Fcreate(MISC14_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(file_id, FAIL, "H5Fcreate");

    /* Create first dataset & write data */
    Dataset1 = H5Dcreate2(file_id, MISC14_DSET1_NAME, H5T_NATIVE_DOUBLE, DataSpace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(Dataset1, FAIL, "H5Dcreate2");

    ret = H5Dwrite(Dataset1, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, &data1);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Create second dataset (to be unlinked).  */
    Dataset2 = H5Dcreate2(file_id, MISC14_DSET2_NAME, H5T_NATIVE_DOUBLE, DataSpace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(Dataset2, FAIL, "H5Dcreate2");

    ret = H5Dwrite(Dataset2, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, &data2);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Check data from first dataset */
    ret = H5Dread(Dataset1, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdata);
    CHECK(ret, FAIL, "H5Dread");
    if(!DBL_ABS_EQUAL(rdata, data1))
        TestErrPrintf("Error on line %d: data1!=rdata\n",__LINE__);

    /* Unlink second dataset */
    ret = H5Ldelete(file_id, MISC14_DSET2_NAME, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Ldelete");

    /* Close second dataset */
    ret = H5Dclose(Dataset2);
    CHECK(ret, FAIL, "H5Dclose");

    /* Verify the data from dataset #1 */
    ret = H5Dread(Dataset1, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdata);
    CHECK(ret, FAIL, "H5Dread");
    if(!DBL_ABS_EQUAL(rdata,data1))
        TestErrPrintf("Error on line %d: data1!=rdata\n",__LINE__);

    /* Close first dataset */
    ret = H5Dclose(Dataset1);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close the file */
    ret = H5Fclose (file_id);
    CHECK(ret, FAIL, "H5Fclose");

    /* Test creating two datasets and deleting the first */

    /* Open the file */
    file_id = H5Fcreate(MISC14_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(file_id, FAIL, "H5Fcreate");

    /* Create first dataset & write data */
    Dataset1 = H5Dcreate2(file_id, MISC14_DSET1_NAME, H5T_NATIVE_DOUBLE, DataSpace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(Dataset1, FAIL, "H5Dcreate2");

    ret = H5Dwrite(Dataset1, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, &data1);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Create second dataset  */
    Dataset2 = H5Dcreate2(file_id, MISC14_DSET2_NAME, H5T_NATIVE_DOUBLE, DataSpace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(Dataset2, FAIL, "H5Dcreate2");

    ret = H5Dwrite(Dataset2, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, &data2);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Check data from second dataset */
    ret = H5Dread(Dataset2, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdata);
    CHECK(ret, FAIL, "H5Dread");
    if(!DBL_ABS_EQUAL(rdata,data2))
        TestErrPrintf("Error on line %d: data2!=rdata\n",__LINE__);

    /* Unlink first dataset */
    ret = H5Ldelete(file_id, MISC14_DSET1_NAME, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Ldelete");

    /* Close first dataset */
    ret = H5Dclose(Dataset1);
    CHECK(ret, FAIL, "H5Dclose");

    /* Verify the data from dataset #2 */
    ret = H5Dread(Dataset2, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdata);
    CHECK(ret, FAIL, "H5Dread");
    if(!DBL_ABS_EQUAL(rdata,data2))
        TestErrPrintf("Error on line %d: data2!=rdata\n",__LINE__);

    /* Close second dataset */
    ret = H5Dclose(Dataset2);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close the file */
    ret = H5Fclose (file_id);
    CHECK(ret, FAIL, "H5Fclose");

    /* Test creating three datasets and deleting the second */

    /* Open the file */
    file_id = H5Fcreate(MISC14_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(file_id, FAIL, "H5Fcreate");

    /* Create first dataset & write data */
    Dataset1 = H5Dcreate2(file_id, MISC14_DSET1_NAME, H5T_NATIVE_DOUBLE, DataSpace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(Dataset1, FAIL, "H5Dcreate2");

    ret = H5Dwrite(Dataset1, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, &data1);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Create second dataset */
    Dataset2 = H5Dcreate2(file_id, MISC14_DSET2_NAME, H5T_NATIVE_DOUBLE, DataSpace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(Dataset2, FAIL, "H5Dcreate2");

    ret = H5Dwrite(Dataset2, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, &data2);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Create third dataset */
    Dataset3 = H5Dcreate2(file_id, MISC14_DSET3_NAME, H5T_NATIVE_DOUBLE, DataSpace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(Dataset2, FAIL, "H5Dcreate2");

    ret = H5Dwrite(Dataset3, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, &data3);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Check data from first dataset */
    ret = H5Dread(Dataset1, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdata);
    CHECK(ret, FAIL, "H5Dread");
    if(!DBL_ABS_EQUAL(rdata,data1))
        TestErrPrintf("Error on line %d: data1!=rdata\n",__LINE__);

    /* Check data from third dataset */
    ret = H5Dread(Dataset3, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdata);
    CHECK(ret, FAIL, "H5Dread");
    if(!DBL_ABS_EQUAL(rdata,data3))
        TestErrPrintf("Error on line %d: data3!=rdata\n",__LINE__);

    /* Unlink second dataset */
    ret = H5Ldelete(file_id, MISC14_DSET2_NAME, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Ldelete");

    /* Close second dataset */
    ret = H5Dclose(Dataset2);
    CHECK(ret, FAIL, "H5Dclose");

    /* Verify the data from dataset #1 */
    ret = H5Dread(Dataset1, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdata);
    CHECK(ret, FAIL, "H5Dread");
    if(!DBL_ABS_EQUAL(rdata,data1))
        TestErrPrintf("Error on line %d: data1!=rdata\n",__LINE__);

    /* Verify the data from dataset #3 */
    ret = H5Dread(Dataset3, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdata);
    CHECK(ret, FAIL, "H5Dread");
    if(!DBL_ABS_EQUAL(rdata,data3))
        TestErrPrintf("Error on line %d: data3!=rdata\n",__LINE__);

    /* Close first dataset */
    ret = H5Dclose(Dataset1);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close third dataset */
    ret = H5Dclose(Dataset3);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close the file */
    ret = H5Fclose (file_id);
    CHECK(ret, FAIL, "H5Fclose");

    /* Close shared objects (dataspace & fapl) */
    ret = H5Sclose (DataSpace);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Pclose (fapl);
    CHECK(ret, FAIL, "H5Pclose");

} /* end test_misc14() */

/****************************************************************
**
**  test_misc15(): Test that checking a file's access property list
**      more than once correctly increments internal reference counts.
**
****************************************************************/
static void
test_misc15(void)
{
    hid_t file;         /* File ID */
    hid_t fapl;         /* File access property list */
    herr_t ret;         /* Generic return value */

    /* Create the file & get it's FAPL */
    file = H5Fcreate(MISC15_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fcreate");

    fapl = H5Fget_access_plist(file);
    CHECK(fapl, FAIL, "H5Fget_access_plist");

    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");

    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    /* Open the file & get it's FAPL again */
    file = H5Fopen(MISC15_FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fopen");

    fapl = H5Fget_access_plist(file);
    CHECK(fapl, FAIL, "H5Fget_access_plist");

    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");

    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    /* Verify that the file is still OK */
    ret = H5Fis_hdf5(MISC15_FILE);
    CHECK(ret, FAIL, "H5Fis_hdf5");

    file = H5Fopen(MISC15_FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fopen");

    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");
} /* end test_misc15() */

/****************************************************************
**
**  test_misc16(): Test array of NULL-terminated
**  fixed-length string.  It creates a dataset of fixed-length
**  strings.  Each string is MISC16_STR_SIZE long.  There are
**  totally MISC16_SPACE_DIM by MISC16_SPACE_RANK strings.
**
****************************************************************/
static void
test_misc16(void)
{
    hid_t file;         /* File ID */
    herr_t ret;         /* Generic return value */
    const char wdata[MISC16_SPACE_DIM][MISC16_STR_SIZE] =
                        {"1234567", "1234567\0", "12345678", {NULL}};
    char rdata[MISC16_SPACE_DIM][MISC16_STR_SIZE];  /* Information read in */
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid;       /* Dataspace ID			*/
    hid_t		tid;       /* Datatype ID			*/
    hsize_t		dims[] = {MISC16_SPACE_DIM};
    int                 i;

    /* Create the file */
    file = H5Fcreate(MISC16_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fcreate");

    /* Create dataspace for datasets */
    sid = H5Screate_simple(MISC16_SPACE_RANK, dims, NULL);
    CHECK(sid, FAIL, "H5Screate_simple");

    /* Create a datatype to refer to */
    tid = H5Tcopy(H5T_C_S1);
    CHECK(tid, FAIL, "H5Tcopy");

    ret = H5Tset_size(tid, (size_t)MISC16_STR_SIZE);
    CHECK(ret, FAIL, "H5Tset_size");

    /*ret = H5Tset_strpad(tid,H5T_STR_NULLPAD);
    CHECK(ret, FAIL, "H5Tset_strpad");*/

    /* Create a dataset */
    dataset = H5Dcreate2(file, MISC16_DSET_NAME, tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Write dataset to disk */
    ret = H5Dwrite(dataset, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Read dataset from disk */
    ret = H5Dread(dataset, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);
    CHECK(ret, FAIL, "H5Dread");

    /* Compare data read in */
    for(i = 0; i < MISC16_SPACE_DIM; i++) {
        if(HDstrlen(wdata[i]) != HDstrlen(rdata[i])) {
            TestErrPrintf("VL data length don't match!, strlen(wdata[%d])=%d, strlen(rdata[%d])=%d\n",(int)i,(int)strlen(wdata[i]),(int)i,(int)strlen(rdata[i]));
            continue;
        } /* end if */
        if(HDstrcmp(wdata[i], rdata[i]) != 0 ) {
            TestErrPrintf("VL data values don't match!, wdata[%d]=%s, rdata[%d]=%s\n",(int)i,wdata[i],(int)i,rdata[i]);
            continue;
        } /* end if */
    } /* end for */

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close datatype */
    ret = H5Tclose(tid);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close disk dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");
} /* end test_misc16() */

/****************************************************************
**
**  test_misc17(): Test array of characters.  It creates a dataset
**  of ASCII characters, with dimensionality of MISC17_SPACE_DIM1
**  by MISC17_SPACE_DIM2.
**
****************************************************************/
static void
test_misc17(void)
{
    hid_t file;         /* File ID */
    herr_t ret;         /* Generic return value */
    const char wdata[MISC17_SPACE_DIM1][MISC17_SPACE_DIM2] =
                        {"1234567", "1234567\0", "12345678", {NULL}};
    char rdata[MISC17_SPACE_DIM1][MISC17_SPACE_DIM2];  /* Information read in */
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid;       /* Dataspace ID			*/
    hid_t		tid;       /* Datatype ID			*/
    hsize_t		dims[] = {MISC17_SPACE_DIM1, MISC17_SPACE_DIM2};
    int                 i;

    /* Create the file */
    file = H5Fcreate(MISC17_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fcreate");

    /* Create dataspace for datasets */
    sid = H5Screate_simple(MISC17_SPACE_RANK, dims, NULL);
    CHECK(sid, FAIL, "H5Screate_simple");

    /* Create a datatype to refer to */
    tid = H5Tcopy (H5T_C_S1);
    CHECK(tid, FAIL, "H5Tcopy");

    ret = H5Tset_strpad (tid,H5T_STR_NULLPAD);
    CHECK(ret, FAIL, "H5Tset_strpad");

    /* Create a dataset */
    dataset = H5Dcreate2(file, MISC17_DSET_NAME, tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Write dataset to disk */
    ret = H5Dwrite(dataset, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Read dataset from disk */
    ret = H5Dread(dataset, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);
    CHECK(ret, FAIL, "H5Dread");

    /* Compare data in the way of strings. */
    for(i = 0; i < MISC17_SPACE_DIM1; i++) {
        if(HDstrlen(wdata[i]) != HDstrlen(rdata[i])) {
            TestErrPrintf("VL data length don't match!, strlen(wdata[%d])=%d, strlen(rdata[%d])=%d\n",(int)i,(int)strlen(wdata[i]),(int)i,(int)strlen(rdata[i]));
            continue;
        } /* end if */
        if(HDstrcmp(wdata[i], rdata[i]) != 0 ) {
            TestErrPrintf("VL data values don't match!, wdata[%d]=%s, rdata[%d]=%s\n",(int)i,wdata[i],(int)i,rdata[i]);
            continue;
        } /* end if */
    } /* end for */

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close datatype */
    ret = H5Tclose(tid);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close disk dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");
} /* end test_misc17() */

/****************************************************************
**
**  test_misc18(): Test new object header information in H5O_info_t
**  struct.
**
****************************************************************/
static void
test_misc18(void)
{
    hid_t fid;          /* File ID */
    hid_t sid;          /* 'Space ID */
    hid_t did1, did2;   /* Dataset IDs */
    hid_t aid;          /* Attribute ID */
    H5O_info_t oinfo;   /* Information about object */
    char attr_name[32]; /* Attribute name buffer */
    unsigned u;         /* Local index variable */
    herr_t ret;         /* Generic return value */

    /* Create the file */
    fid = H5Fcreate(MISC18_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create dataspace for attributes */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create first dataset */
    did1 = H5Dcreate2(fid, MISC18_DSET1_NAME, H5T_STD_U32LE, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(did1, FAIL, "H5Dcreate2");

    /* Get object information */
    ret = H5Oget_info_by_name(fid, MISC18_DSET1_NAME, &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.hdr.nmesgs, 6, "H5Oget_info_by_name");
    VERIFY(oinfo.hdr.nchunks, 1, "H5Oget_info_by_name");
    VERIFY(oinfo.hdr.space.total, 272, "H5Oget_info_by_name");
    VERIFY(oinfo.hdr.space.free, 152, "H5Oget_info_by_name");
    VERIFY(oinfo.num_attrs, 0, "H5Oget_info_by_name");

    /* Create second dataset */
    did2 = H5Dcreate2(fid, MISC18_DSET2_NAME, H5T_STD_U32LE, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(did2, FAIL, "H5Dcreate2");

    /* Get object information */
    ret = H5Oget_info_by_name(fid, MISC18_DSET2_NAME, &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.hdr.nmesgs, 6, "H5Oget_info_by_name");
    VERIFY(oinfo.hdr.nchunks, 1, "H5Oget_info_by_name");
    VERIFY(oinfo.hdr.space.total, 272, "H5Oget_info_by_name");
    VERIFY(oinfo.hdr.space.free, 152, "H5Oget_info_by_name");
    VERIFY(oinfo.num_attrs, 0, "H5Oget_info_by_name");

    /* Loop creating attributes on each dataset, flushing them to the file each time */
    for(u = 0; u < 10; u++) {
        /* Set up attribute name */
        sprintf(attr_name, "Attr %u", u);

        /* Create & close attribute on first dataset */
        aid = H5Acreate2(did1, attr_name, H5T_STD_U32LE, sid, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(aid, FAIL, "H5Acreate2");

        ret = H5Aclose(aid);
        CHECK(ret, FAIL, "HAclose");

        /* Create & close attribute on second dataset */
        aid = H5Acreate2(did2, attr_name, H5T_STD_U32LE, sid, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(aid, FAIL, "H5Acreate2");

        ret = H5Aclose(aid);
        CHECK(ret, FAIL, "HAclose");

        /* Flush file, to 'fix' size of dataset object headers */
        ret = H5Fflush(fid,H5F_SCOPE_GLOBAL);
        CHECK(ret, FAIL, "HAclose");
    } /* end for */

    /* Get object information for dataset #1 now */
    ret = H5Oget_info_by_name(fid, MISC18_DSET1_NAME, &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.hdr.nmesgs, 24, "H5Oget_info_by_name");
    VERIFY(oinfo.hdr.nchunks, 9, "H5Oget_info_by_name");
    VERIFY(oinfo.hdr.space.total, 888, "H5Oget_info_by_name");
    VERIFY(oinfo.hdr.space.free, 16, "H5Oget_info_by_name");
    VERIFY(oinfo.num_attrs, 10, "H5Oget_info_by_name");

    /* Get object information for dataset #2 now */
    ret = H5Oget_info_by_name(fid, MISC18_DSET2_NAME, &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.hdr.nmesgs, 24, "H5Oget_info_by_name");
    VERIFY(oinfo.hdr.nchunks, 9, "H5Oget_info_by_name");
    VERIFY(oinfo.hdr.space.total, 888, "H5Oget_info_by_name");
    VERIFY(oinfo.hdr.space.free, 16, "H5Oget_info_by_name");
    VERIFY(oinfo.num_attrs, 10, "H5Oget_info_by_name");

    /* Close second dataset */
    ret = H5Dclose(did2);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close first dataset */
    ret = H5Dclose(did1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close disk dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
} /* end test_misc18() */

/****************************************************************
**
**  test_misc19(): Test incrementing & decrementing ref count on IDs
**
****************************************************************/
static void
test_misc19(void)
{
    hid_t fid;          /* File ID */
    hid_t sid;          /* 'Space ID */
    hid_t did;          /* Dataset ID */
    hid_t tid;          /* 'Type ID */
    hid_t aid;          /* Attribute ID */
    hid_t plid;         /* Property List ID */
    hid_t pcid;         /* Property Class ID */
    hid_t gid;          /* Group ID */
    hid_t ecid;         /* Error Class ID */
    hid_t emid;         /* Error Message ID */
    hid_t esid;         /* Error Stack ID */
    int rc;             /* Reference count */
    herr_t ret;         /* Generic return value */

/* Check H5I operations on files */

    /* Create the file */
    fid = H5Fcreate(MISC19_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Check the reference count */
    rc = H5Iget_ref(fid);
    VERIFY(rc, 1, "H5Iget_ref");

    /* Inc the reference count */
    rc = H5Iinc_ref(fid);
    VERIFY(rc, 2, "H5Iinc_ref");

    /* Close the file normally */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Check the reference count */
    rc = H5Iget_ref(fid);
    VERIFY(rc, 1, "H5Iget_ref");

    /* Close the file by decrementing the reference count */
    rc = H5Idec_ref(fid);
    VERIFY(rc, 0, "H5Idec_ref");

    /* Try closing the file again (should fail) */
    H5E_BEGIN_TRY {
        ret = H5Fclose(fid);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Fclose");

/* Check H5I operations on property lists */

    /* Create the property list */
    plid = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(plid, FAIL, "H5Pcreate");

    /* Check the reference count */
    rc = H5Iget_ref(plid);
    VERIFY(rc, 1, "H5Iget_ref");

    /* Inc the reference count */
    rc = H5Iinc_ref(plid);
    VERIFY(rc, 2, "H5Iinc_ref");

    /* Close the property list normally */
    ret = H5Pclose(plid);
    CHECK(ret, FAIL, "H5Pclose");

    /* Check the reference count */
    rc = H5Iget_ref(plid);
    VERIFY(rc, 1, "H5Iget_ref");

    /* Close the property list by decrementing the reference count */
    rc = H5Idec_ref(plid);
    VERIFY(rc, 0, "H5Idec_ref");

    /* Try closing the property list again (should fail) */
    H5E_BEGIN_TRY {
        ret = H5Pclose(plid);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Pclose");

/* Check H5I operations on property classes */

    /* Create a property class */
    pcid = H5Pcreate_class(H5P_DATASET_CREATE,"foo",NULL,NULL,NULL,NULL,NULL,NULL);
    CHECK(pcid, FAIL, "H5Pcreate_class");

    /* Check the reference count */
    rc = H5Iget_ref(pcid);
    VERIFY(rc, 1, "H5Iget_ref");

    /* Inc the reference count */
    rc = H5Iinc_ref(pcid);
    VERIFY(rc, 2, "H5Iinc_ref");

    /* Close the property class normally */
    ret = H5Pclose_class(pcid);
    CHECK(ret, FAIL, "H5Pclose_class");

    /* Check the reference count */
    rc = H5Iget_ref(pcid);
    VERIFY(rc, 1, "H5Iget_ref");

    /* Close the property class by decrementing the reference count */
    rc = H5Idec_ref(pcid);
    VERIFY(rc, 0, "H5Idec_ref");

    /* Try closing the property class again (should fail) */
    H5E_BEGIN_TRY {
        ret = H5Pclose_class(pcid);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Pclose_class");

/* Check H5I operations on datatypes */

    /* Create a datatype */
    tid = H5Tcreate(H5T_OPAQUE, (size_t)16);
    CHECK(tid, FAIL, "H5Tcreate");

    /* Check the reference count */
    rc = H5Iget_ref(tid);
    VERIFY(rc, 1, "H5Iget_ref");

    /* Inc the reference count */
    rc = H5Iinc_ref(tid);
    VERIFY(rc, 2, "H5Iinc_ref");

    /* Close the datatype normally */
    ret = H5Tclose(tid);
    CHECK(ret, FAIL, "H5Tclose");

    /* Check the reference count */
    rc = H5Iget_ref(tid);
    VERIFY(rc, 1, "H5Iget_ref");

    /* Close the datatype by decrementing the reference count */
    rc = H5Idec_ref(tid);
    VERIFY(rc, 0, "H5Idec_ref");

    /* Try closing the datatype again (should fail) */
    H5E_BEGIN_TRY {
        ret = H5Tclose(tid);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Tclose");

/* Check H5I operations on dataspaces */

    /* Create a dataspace */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Check the reference count */
    rc = H5Iget_ref(sid);
    VERIFY(rc, 1, "H5Iget_ref");

    /* Inc the reference count */
    rc = H5Iinc_ref(sid);
    VERIFY(rc, 2, "H5Iinc_ref");

    /* Close the dataspace normally */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Check the reference count */
    rc = H5Iget_ref(sid);
    VERIFY(rc, 1, "H5Iget_ref");

    /* Close the dataspace by decrementing the reference count */
    rc = H5Idec_ref(sid);
    VERIFY(rc, 0, "H5Idec_ref");

    /* Try closing the dataspace again (should fail) */
    H5E_BEGIN_TRY {
        ret = H5Sclose(sid);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Sclose");

/* Check H5I operations on datasets */

    /* Create a file */
    fid = H5Fcreate(MISC19_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create a dataspace */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create a dataset */
    did = H5Dcreate2(fid, MISC19_DSET_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dcreate2");

    /* Check the reference count */
    rc = H5Iget_ref(did);
    VERIFY(rc, 1, "H5Iget_ref");

    /* Inc the reference count */
    rc = H5Iinc_ref(did);
    VERIFY(rc, 2, "H5Iinc_ref");

    /* Close the dataset normally */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    /* Check the reference count */
    rc = H5Iget_ref(did);
    VERIFY(rc, 1, "H5Iget_ref");

    /* Close the dataset by decrementing the reference count */
    rc = H5Idec_ref(did);
    VERIFY(rc, 0, "H5Idec_ref");

    /* Try closing the dataset again (should fail) */
    H5E_BEGIN_TRY {
        ret = H5Dclose(did);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Dclose");

    /* Close the dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

/* Check H5I operations on attributes */

    /* Create a file */
    fid = H5Fcreate(MISC19_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Open the root group */
    gid = H5Gopen2(fid, "/", H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gopen2");

    /* Create a dataspace */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create an attribute */
    aid = H5Acreate2(gid, MISC19_ATTR_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(aid, FAIL, "H5Acreate2");

    /* Check the reference count */
    rc = H5Iget_ref(aid);
    VERIFY(rc, 1, "H5Iget_ref");

    /* Inc the reference count */
    rc = H5Iinc_ref(aid);
    VERIFY(rc, 2, "H5Iinc_ref");

    /* Close the dataset normally */
    ret = H5Aclose(aid);
    CHECK(ret, FAIL, "H5Aclose");

    /* Check the reference count */
    rc = H5Iget_ref(aid);
    VERIFY(rc, 1, "H5Iget_ref");

    /* Close the attribute by decrementing the reference count */
    rc = H5Idec_ref(aid);
    VERIFY(rc, 0, "H5Idec_ref");

    /* Try closing the attribute again (should fail) */
    H5E_BEGIN_TRY {
        ret = H5Aclose(aid);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Aclose");

    /* Close the root group */
    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    /* Close the dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

/* Check H5I operations on groups */

    /* Create a file */
    fid = H5Fcreate(MISC19_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create a group */
    gid = H5Gcreate2(fid, MISC19_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gcreate2");

    /* Check the reference count */
    rc = H5Iget_ref(gid);
    VERIFY(rc, 1, "H5Iget_ref");

    /* Inc the reference count */
    rc = H5Iinc_ref(gid);
    VERIFY(rc, 2, "H5Iinc_ref");

    /* Close the group normally */
    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    /* Check the reference count */
    rc = H5Iget_ref(gid);
    VERIFY(rc, 1, "H5Iget_ref");

    /* Close the group by decrementing the reference count */
    rc = H5Idec_ref(gid);
    VERIFY(rc, 0, "H5Idec_ref");

    /* Try closing the group again (should fail) */
    H5E_BEGIN_TRY {
        ret = H5Gclose(gid);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Gclose");

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

/* Check H5I operations on error classes */

    /* Create an error class */
    ecid = H5Eregister_class("foo","bar","baz");
    CHECK(ecid, FAIL, "H5Eregister_class");

    /* Check the reference count */
    rc = H5Iget_ref(ecid);
    VERIFY(rc, 1, "H5Iget_ref");

    /* Inc the reference count */
    rc = H5Iinc_ref(ecid);
    VERIFY(rc, 2, "H5Iinc_ref");

    /* Close the error class normally */
    ret = H5Eunregister_class(ecid);
    CHECK(ret, FAIL, "H5Eunregister_class");

    /* Check the reference count */
    rc = H5Iget_ref(ecid);
    VERIFY(rc, 1, "H5Iget_ref");

    /* Close the error class by decrementing the reference count */
    rc = H5Idec_ref(ecid);
    VERIFY(rc, 0, "H5Idec_ref");

    /* Try closing the error class again (should fail) */
    H5E_BEGIN_TRY {
        ret = H5Eunregister_class(ecid);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Eunregister_class");

/* Check H5I operations on error messages */

    /* Create an error class */
    ecid = H5Eregister_class("foo","bar","baz");
    CHECK(ecid, FAIL, "H5Eregister_class");

    /* Create an error message */
    emid = H5Ecreate_msg(ecid,H5E_MAJOR,"mumble");
    CHECK(emid, FAIL, "H5Ecreate_msg");

    /* Check the reference count */
    rc = H5Iget_ref(emid);
    VERIFY(rc, 1, "H5Iget_ref");

    /* Inc the reference count */
    rc = H5Iinc_ref(emid);
    VERIFY(rc, 2, "H5Iinc_ref");

    /* Close the error message normally */
    ret = H5Eclose_msg(emid);
    CHECK(ret, FAIL, "H5Eclose_msg");

    /* Check the reference count */
    rc = H5Iget_ref(emid);
    VERIFY(rc, 1, "H5Iget_ref");

    /* Close the error message by decrementing the reference count */
    rc = H5Idec_ref(emid);
    VERIFY(rc, 0, "H5Idec_ref");

    /* Try closing the error message again (should fail) */
    H5E_BEGIN_TRY {
        ret = H5Eclose_msg(emid);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Eclose_msg");

    /* Close the error class */
    ret = H5Eunregister_class(ecid);
    CHECK(ret, FAIL, "H5Eunregister_class");

/* Check H5I operations on error stacks */

    /* Create an error stack */
    esid = H5Eget_current_stack();
    CHECK(esid, FAIL, "H5Eget_current_stack");

    /* Check the reference count */
    rc = H5Iget_ref(esid);
    VERIFY(rc, 1, "H5Iget_ref");

    /* Inc the reference count */
    rc = H5Iinc_ref(esid);
    VERIFY(rc, 2, "H5Iinc_ref");

    /* Close the error stack normally */
    ret = H5Eclose_stack(esid);
    CHECK(ret, FAIL, "H5Eclose_stack");

    /* Check the reference count */
    rc = H5Iget_ref(esid);
    VERIFY(rc, 1, "H5Iget_ref");

    /* Close the error stack by decrementing the reference count */
    rc = H5Idec_ref(esid);
    VERIFY(rc, 0, "H5Idec_ref");

    /* Try closing the error stack again (should fail) */
    H5E_BEGIN_TRY {
        ret = H5Eclose_stack(esid);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Eclose_stack");

} /* end test_misc19() */

/****************************************************************
**
**  test_misc20(): Test problems with version 2 of storage layout
**                      message truncating dimensions
**
****************************************************************/
static void
test_misc20(void)
{
    hid_t fid;          /* File ID */
    hid_t sid;          /* 'Space ID */
    hid_t did;          /* Dataset ID */
    hid_t dcpl;         /* Dataset creation property list ID */
    int rank=MISC20_SPACE_RANK;    /* Rank of dataspace */
    hsize_t big_dims[MISC20_SPACE_RANK]={MISC20_SPACE_DIM0,MISC20_SPACE_DIM1};      /* Large dimensions */
    hsize_t small_dims[MISC20_SPACE_RANK]={MISC20_SPACE2_DIM0,MISC20_SPACE2_DIM1};      /* Small dimensions */
    unsigned version;   /* Version of storage layout info */
    hsize_t contig_size;        /* Size of contiguous storage size from layout into */
    char testfile[512]="";          /* Character buffer for corrected test file name */
    char *srcdir = HDgetenv("srcdir");    /* Pointer to the directory the source code is located within */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing large dimension truncation fix\n"));

    /* Verify that chunks with dimensions that are too large get rejected */

    /* Create a dataset creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Try to use chunked storage for this dataset */
    ret = H5Pset_chunk(dcpl, rank, big_dims);
    VERIFY(ret, FAIL, "H5Pset_chunk");

    /* Verify that the storage for the dataset is the correct size and hasn't
     * been truncated.
     */

    /* Create the file */
    fid = H5Fcreate(MISC20_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create dataspace with _really_ big dimensions */
    sid = H5Screate_simple(rank, big_dims, NULL);
    CHECK(sid, FAIL, "H5Screate_simple");

    /* Make certain that the dataset's storage doesn't get allocated :-) */
    ret = H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_LATE);
    CHECK(ret, FAIL, "H5Pset_alloc_time");

    /* Create dataset with big dataspace */
    did = H5Dcreate2(fid, MISC20_DSET_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dcreate2");

    /* Close datasset */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Create dataspace with small dimensions */
    sid = H5Screate_simple(rank, small_dims, NULL);
    CHECK(sid, FAIL, "H5Screate_simple");

    /* Create dataset with big dataspace */
    did = H5Dcreate2(fid, MISC20_DSET2_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dcreate2");

    /* Close datasset */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close dataset creation property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open the file */
    fid = H5Fopen(MISC20_FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /* Open dataset with big dimensions */
    did = H5Dopen2(fid, MISC20_DSET_NAME, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dopen2");

    /* Get the layout version */
    ret = H5D_layout_version_test(did,&version);
    CHECK(ret, FAIL, "H5D_layout_version_test");
    VERIFY(version, 3, "H5D_layout_version_test");

    /* Get the layout contiguous storage size */
    ret = H5D_layout_contig_size_test(did,&contig_size);
    CHECK(ret, FAIL, "H5D_layout_contig_size_test");
    VERIFY(contig_size, (MISC20_SPACE_DIM0 * MISC20_SPACE_DIM1 * H5Tget_size(H5T_NATIVE_INT)), "H5D_layout_contig_size_test");

    /* Close datasset */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    /* Open dataset with small dimensions */
    did = H5Dopen2(fid, MISC20_DSET2_NAME, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dopen2");

    /* Get the layout version */
    ret = H5D_layout_version_test(did,&version);
    CHECK(ret, FAIL, "H5D_layout_version_test");
    VERIFY(version, 3, "H5D_layout_version_test");

    /* Get the layout contiguous storage size */
    ret = H5D_layout_contig_size_test(did,&contig_size);
    CHECK(ret, FAIL, "H5D_layout_contig_size_test");
    VERIFY(contig_size, (MISC20_SPACE2_DIM0 * MISC20_SPACE2_DIM1 * H5Tget_size(H5T_NATIVE_INT)), "H5D_layout_contig_size_test");

    /* Close datasset */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Verify that the storage size is computed correctly for older versions of layout info */

    /* Generate the correct name for the test file, by prepending the source path */
    if(srcdir && ((HDstrlen(srcdir) + HDstrlen(MISC20_FILE_OLD) + 1) < sizeof(testfile))) {
        HDstrcpy(testfile, srcdir);
        HDstrcat(testfile, "/");
    }
    HDstrcat(testfile, MISC20_FILE_OLD);

    /*
     * Open the old file and the dataset and get old settings.
     */
    fid = H5Fopen(testfile, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /* Open dataset with small dimensions */
    did = H5Dopen2(fid, MISC20_DSET_NAME, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dopen2");

    /* Get the layout version */
    ret = H5D_layout_version_test(did,&version);
    CHECK(ret, FAIL, "H5D_layout_version_test");
    VERIFY(version, 2, "H5D_layout_version_test");

    /* Get the layout contiguous storage size */
    ret = H5D_layout_contig_size_test(did,&contig_size);
    CHECK(ret, FAIL, "H5D_layout_contig_size_test");
    VERIFY(contig_size, (MISC20_SPACE_DIM0 * MISC20_SPACE_DIM1 * H5Tget_size(H5T_STD_I32LE)), "H5D_layout_contig_size_test");

    /* Close datasset */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

} /* end test_misc20() */

/*
   test_misc21 and test_misc22 should be executed when SZIP is present
   and encoder is available.
                            EIP 2004/8/04
*/
#if defined H5_HAVE_FILTER_SZIP

/****************************************************************
**
**  test_misc21(): Test that late allocation time is treated the same
**                      as incremental allocation time, for chunked datasets
**                      when overwriting entire dataset where the chunks
**                      don't exactly match the dataspace.
**
****************************************************************/
static void
test_misc21(void)
{
    hid_t fid, sid, dcpl, dsid;
    char *buf;
    hsize_t dims[2]={MISC21_SPACE_DIM0,MISC21_SPACE_DIM1},
        chunk_size[2]={MISC21_CHUNK_DIM0,MISC21_CHUNK_DIM1};
    herr_t ret;         /* Generic return value */

    if (h5_szip_can_encode() != 1) return;
    /* Output message about test being performed */
    MESSAGE(5, ("Testing late allocation time w/chunks & filters\n"));

    /* Allocate space for the buffer */
    buf = (char *)HDcalloc(MISC21_SPACE_DIM0 * MISC21_SPACE_DIM1, 1);
    CHECK(buf, NULL, "HDcalloc");

    /* Create the file */
    fid = H5Fcreate(MISC21_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create the DCPL */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Set custom DCPL properties */
    ret = H5Pset_chunk(dcpl, MISC21_SPACE_RANK, chunk_size);
    CHECK(ret, FAIL, "H5Pset_chunk");
    ret = H5Pset_szip(dcpl, H5_SZIP_NN_OPTION_MASK, 8);
    CHECK(ret, FAIL, "H5Pset_deflate");
    ret = H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_LATE);
    CHECK(ret, FAIL, "H5Pset_alloc_time");

    /* Create the dataspace for the dataset */
    sid = H5Screate_simple(MISC21_SPACE_RANK, dims, NULL);
    CHECK(ret, FAIL, "H5Screate_simple");

    /* Create the dataset */
    dsid = H5Dcreate2(fid, MISC21_DSET_NAME, H5T_NATIVE_UINT8, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(dsid, FAIL, "H5Dcreate2");

    /* Write out the whole dataset */
    ret = H5Dwrite(dsid, H5T_NATIVE_UINT8, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close everything */
    ret = H5Dclose(dsid);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    HDfree(buf);
} /* end test_misc21() */

/****************************************************************
**
**  test_misc22(): Test SZIP bits-per-pixel paramter.
**                      This should be set according to the datatype.
**                      Tests for precision and offset combo's.
**
****************************************************************/
static void
test_misc22(void)
{
    hid_t fid, sid, dcpl, dsid, dcpl2;
    char *buf;
    hsize_t dims[2]={MISC22_SPACE_DIM0,MISC22_SPACE_DIM1},
        chunk_size[2]={MISC22_CHUNK_DIM0,MISC22_CHUNK_DIM1};
    herr_t ret;         /* Generic return value */
    hid_t dtype;
    /* should extend test to signed ints */
    hid_t idts[4];
/*  do the same for floats
    hid_t fdts[2]={H5T_NATIVE_FLOAT32,
              H5T_NATIVE_FLOAT64}
*/
    size_t prec[4] = {3,11,19,27};
    size_t offsets[5] = {0,3,11,19,27};
    int i,j,k;
    unsigned int flags;
    size_t cd_nelmts=32;
    unsigned int cd_values[32];
    unsigned correct;

    if (h5_szip_can_encode() != 1) return;
    idts[0]=H5Tcopy(H5T_NATIVE_UINT8);
    idts[1]=H5Tcopy(H5T_NATIVE_UINT16);
    idts[2]=H5Tcopy(H5T_NATIVE_UINT32);
    idts[3]=H5Tcopy(H5T_NATIVE_UINT64);

    /* Output message about test being performed */
    MESSAGE(5, ("Testing datatypes with SZIP filter\n"));

    /* Allocate space for the buffer */
    buf = (char *)HDcalloc(MISC22_SPACE_DIM0*MISC22_SPACE_DIM1, 8);
    CHECK(buf, NULL, "HDcalloc");

    /* Create the file */
    fid = H5Fcreate(MISC22_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create the dataspace for the dataset */
    sid = H5Screate_simple(MISC22_SPACE_RANK, dims, NULL);
    CHECK(sid, FAIL, "H5Screate_simple");

    for (i = 0; i < 4; i++) {
        for (j = 0; j < 4; j++) {
            if (prec[j] > (H5Tget_size(idts[i])*8)) continue; /* skip irrelevant combination */
            for (k = 0; k < 5; k++) {
                if (offsets[k] > (H5Tget_size(idts[i])*8)) continue; /* skip irrelevant combinations */
                if ((prec[j]+offsets[k]) > (H5Tget_size(idts[i])*8)) continue;

                MESSAGE(5, ("  Testing datatypes size=%d precision=%u offset=%d\n",H5Tget_size(idts[i]),(unsigned)prec[j],(unsigned)offsets[k]));

                /* Create the DCPL */
                dcpl = H5Pcreate(H5P_DATASET_CREATE);
                CHECK(dcpl, FAIL, "H5Pcreate");

                /* Set DCPL properties */
                ret = H5Pset_chunk(dcpl, MISC22_SPACE_RANK, chunk_size);
                CHECK(ret, FAIL, "H5Pset_chunk");
                /* Set custom DCPL properties */
                ret = H5Pset_szip(dcpl, H5_SZIP_NN_OPTION_MASK, 32);  /* vary the PPB */
                CHECK(ret, FAIL, "H5Pset_szip");

                /* set up the datatype according to the loop */
                dtype = H5Tcopy(idts[i]);
                CHECK(dtype, FAIL, "H5Tcopy");
                ret = H5Tset_precision(dtype,prec[j]);
                CHECK(ret, FAIL, "H5Tset_precision");
                ret = H5Tset_offset(dtype,offsets[k]);
                CHECK(ret, FAIL, "H5Tset_precision");

                /* compute the correct PPB that should be set by SZIP */
                if(offsets[k] == 0)
            	    correct = prec[j];
                else
                    correct = H5Tget_size(idts[i]) * 8;
                if(correct > 24) {
            	    if(correct <= 32)
                        correct = 32;
            	    else if(correct <= 64)
                        correct = 64;
                } /* end if */

                /* Create the dataset */
                dsid = H5Dcreate2(fid, MISC22_DSET_NAME, dtype, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
                CHECK(dsid, FAIL, "H5Dcreate2");

                /* Write out the whole dataset */
                ret = H5Dwrite(dsid, dtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
                CHECK(ret, FAIL, "H5Dwrite");

                /* Close everything */
                ret = H5Dclose(dsid);
                CHECK(ret, FAIL, "H5Dclose");
                ret = H5Tclose(dtype);
                CHECK(ret, FAIL, "H5Tclose");
                ret = H5Pclose(dcpl);
                CHECK(ret, FAIL, "H5Pclose");

                dsid = H5Dopen2(fid, MISC22_DSET_NAME, H5P_DEFAULT);
                CHECK(dsid, FAIL, "H5Dopen2");

                dcpl2 = H5Dget_create_plist(dsid);
                CHECK(dcpl2, FAIL, "H5Dget_create_plist");

                ret = H5Pget_filter_by_id2(dcpl2, H5Z_FILTER_SZIP, &flags,
                      &cd_nelmts, cd_values, 0, NULL , NULL);
                CHECK(ret, FAIL, "H5Pget_filter_by_id2");

                VERIFY(cd_values[2], correct, "SZIP filter returned value for precision");

                ret = H5Dclose(dsid);
                CHECK(ret, FAIL, "H5Dclose");

                ret = H5Ldelete(fid, MISC22_DSET_NAME, H5P_DEFAULT);
                CHECK(ret, FAIL, "H5Ldelete");

                ret = H5Pclose(dcpl2);
                CHECK(ret, FAIL, "H5Pclose");
            }
        }
    }
    ret = H5Tclose(idts[0]);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Tclose(idts[1]);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Tclose(idts[2]);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Tclose(idts[3]);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    HDfree(buf);
} /* end test_misc22() */
#endif /* H5_HAVE_FILTER_SZIP */

/****************************************************************
**
**  test_misc23(): Test intermediate group creation.
**
****************************************************************/
static void
test_misc23(void)
{
    hsize_t     dims[] = {10};
    hid_t       file_id=0, group_id=0, type_id=0, space_id=0,
                tmp_id=0, create_id=H5P_DEFAULT, access_id=H5P_DEFAULT;
    char        objname[MISC23_NAME_BUF_SIZE];  /* Name of object */
    H5O_info_t  oinfo;
    htri_t      tri_status;
    herr_t      status;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing intermediate group creation\n"));

    /* Create a new file using default properties. */
    file_id = H5Fcreate(MISC23_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file_id, FAIL, "H5Fcreate");


    /* Build some infrastructure */
    group_id = H5Gcreate2(file_id, "/A", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(group_id, FAIL, "H5Gcreate2");

    space_id = H5Screate_simple(1, dims, NULL);
    CHECK(space_id, FAIL, "H5Screate_simple");

    type_id = H5Tcopy( H5T_STD_I32BE);
    CHECK(type_id, FAIL, "H5Tcopy");


#ifndef H5_NO_DEPRECATED_SYMBOLS
    /**********************************************************************
     * test the old APIs
     **********************************************************************/

    H5E_BEGIN_TRY {
        tmp_id = H5Gcreate1(file_id, "/A/B00a/grp", (size_t)0);
    } H5E_END_TRY;
    VERIFY(tmp_id, FAIL, "H5Gcreate1");


    tmp_id = H5Gcreate1(file_id, "/A/grp", (size_t)0);
    CHECK(tmp_id, FAIL, "H5Gcreate1");

    status = H5Gclose(tmp_id);
    CHECK(status, FAIL, "H5Gclose");


    H5E_BEGIN_TRY {
        tmp_id = H5Dcreate1(file_id, "/A/B00c/dset", type_id, space_id, create_id);
    } H5E_END_TRY;
    VERIFY(tmp_id, FAIL, "H5Dcreate1");


    tmp_id = H5Dcreate1(file_id, "/A/dset", type_id, space_id, create_id);
    CHECK(tmp_id, FAIL, "H5Dcreate1");

    status = H5Dclose(tmp_id);
    CHECK(status, FAIL, "H5Dclose");
#endif /* H5_NO_DEPRECATED_SYMBOLS */

    /**********************************************************************
     * test H5Gcreate2()
     **********************************************************************/

    /* Create link creation property list */
    create_id = H5Pcreate(H5P_LINK_CREATE);
    CHECK(create_id, FAIL, "H5Pcreate");

    /* Set flag for intermediate group creation */
    status = H5Pset_create_intermediate_group(create_id, TRUE);
    CHECK(status, FAIL, "H5Pset_create_intermediate_group");


    tmp_id = H5Gcreate2(file_id, "/A/B01/grp", create_id, H5P_DEFAULT, access_id);
    CHECK(tmp_id, FAIL, "H5Gcreate2");

    /* Query that the name of the new group is correct */
    status = H5Iget_name(tmp_id, objname, (size_t)MISC23_NAME_BUF_SIZE);
    CHECK(status, FAIL, "H5Iget_name");
    VERIFY_STR(objname, "/A/B01/grp", "H5Iget_name");

    status = H5Gclose(tmp_id);
    CHECK(status, FAIL, "H5Gclose");

    /* Check that intermediate group is set up correctly */
    tmp_id = H5Gopen2(file_id, "/A/B01", H5P_DEFAULT);
    CHECK(tmp_id, FAIL, "H5Gopen2");

    status = H5Oget_info(tmp_id, &oinfo);
    CHECK(status, FAIL, "H5Oget_info");
    VERIFY(oinfo.rc, 1, "H5Oget_info");

    status = H5Gclose(tmp_id);
    CHECK(status, FAIL, "H5Gclose");


    tmp_id = H5Gcreate2(file_id, "/A/B02/C02/grp", create_id, H5P_DEFAULT, access_id);
    CHECK(tmp_id, FAIL, "H5Gcreate2");

    status = H5Gclose(tmp_id);
    CHECK(status, FAIL, "H5Gclose");


    tmp_id = H5Gcreate2(group_id, "B03/grp/", create_id, H5P_DEFAULT, access_id);
    CHECK(tmp_id, FAIL, "H5Gcreate2");

    status = H5Gclose(tmp_id);
    CHECK(status, FAIL, "H5Gclose");


    tmp_id = H5Gcreate2(group_id, "/A/B04/grp/", create_id, H5P_DEFAULT, access_id);
    CHECK(tmp_id, FAIL, "H5Gcreate2");

    status = H5Gclose(tmp_id);
    CHECK(status, FAIL, "H5Gclose");


    tmp_id = H5Gcreate2(file_id, "/A/B05/C05/A", create_id, H5P_DEFAULT, access_id);
    CHECK(tmp_id, FAIL, "H5Gcreate2");

    status = H5Gclose(tmp_id);
    CHECK(status, FAIL, "H5Gclose");


    status = H5Pclose(create_id);
    CHECK(status, FAIL, "H5Pclose");


    /**********************************************************************
     * test H5Dcreate2()
     **********************************************************************/

    /* Create link creation property list */
    create_id = H5Pcreate(H5P_LINK_CREATE);
    CHECK(create_id, FAIL, "H5Pcreate");

    /* Set flag for intermediate group creation */
    status = H5Pset_create_intermediate_group(create_id, TRUE);
    CHECK(status, FAIL, "H5Pset_create_intermediate_group");


    tmp_id = H5Dcreate2(file_id, "/A/B06/dset", type_id, space_id, create_id, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(tmp_id, FAIL, "H5Dcreate2");

    status = H5Dclose(tmp_id);
    CHECK(status, FAIL, "H5Dclose");


    tmp_id = H5Dcreate2(file_id, "/A/B07/B07/dset", type_id, space_id, create_id, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(tmp_id, FAIL, "H5Dcreate2");

    status = H5Dclose(tmp_id);
    CHECK(status, FAIL, "H5Dclose");


    tmp_id = H5Dcreate2(group_id, "B08/dset", type_id, space_id, create_id, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(tmp_id, FAIL, "H5Dcreate2");

    status = H5Dclose(tmp_id);
    CHECK(status, FAIL, "H5Dclose");


    tmp_id = H5Dcreate2(group_id, "/A/B09/dset", type_id, space_id, create_id, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(tmp_id, FAIL, "H5Dcreate2");

    status = H5Dclose(tmp_id);
    CHECK(status, FAIL, "H5Dclose");


    tmp_id = H5Dcreate2(file_id, "/A/B10/C10/A/dset", type_id, space_id, create_id, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(tmp_id, FAIL, "H5Dcreate2");

    status = H5Dclose(tmp_id);
    CHECK(status, FAIL, "H5Dclose");


    status = H5Tclose(type_id);
    CHECK(status, FAIL, "H5Tclose");

    status = H5Sclose(space_id);
    CHECK(status, FAIL, "H5Sclose");

    status = H5Pclose(create_id);
    CHECK(status, FAIL, "H5Pclose");


    /**********************************************************************
    * test H5Tcommit2()
    **********************************************************************/

    /* Create link creation property list */
    create_id = H5Pcreate(H5P_LINK_CREATE);
    CHECK(create_id, FAIL, "H5Pcreate");

    /* Set flag for intermediate group creation */
    status = H5Pset_create_intermediate_group(create_id, TRUE);
    CHECK(status, FAIL, "H5Pset_create_intermediate_group");

    tmp_id = H5Tcopy(H5T_NATIVE_INT16);
    CHECK(tmp_id, FAIL, "H5Tcopy");

    status = H5Tcommit2(file_id, "/A/B11/dtype", tmp_id, create_id, H5P_DEFAULT, access_id);
    CHECK(status, FAIL, "H5Tcommit2");

    status = H5Tclose(tmp_id);
    CHECK(status, FAIL, "H5Tclose");


    tmp_id = H5Tcopy(H5T_NATIVE_INT32);
    CHECK(tmp_id, FAIL, "H5Tcopy");

    status = H5Tcommit2(file_id, "/A/B12/C12/dtype", tmp_id, create_id, H5P_DEFAULT, access_id);
    CHECK(status, FAIL, "H5Tcommit2");

    status = H5Tclose(tmp_id);
    CHECK(status, FAIL, "H5Tclose");


    tmp_id = H5Tcopy(H5T_NATIVE_INT64);
    CHECK(tmp_id, FAIL, "H5Tcopy");

    status = H5Tcommit2(group_id, "B13/C12/dtype", tmp_id, create_id, H5P_DEFAULT, access_id);
    CHECK(status, FAIL, "H5Tcommit2");

    status = H5Tclose(tmp_id);
    CHECK(status, FAIL, "H5Tclose");


    tmp_id = H5Tcopy(H5T_NATIVE_FLOAT);
    CHECK(tmp_id, FAIL, "H5Tcopy");

    status = H5Tcommit2(group_id, "/A/B14/dtype", tmp_id, create_id, H5P_DEFAULT, access_id);
    CHECK(status, FAIL, "H5Tcommit2");

    status = H5Tclose(tmp_id);
    CHECK(status, FAIL, "H5Tclose");


    tmp_id = H5Tcopy(H5T_NATIVE_DOUBLE);
    CHECK(tmp_id, FAIL, "H5Tcopy");

    status = H5Tcommit2(file_id, "/A/B15/C15/A/dtype", tmp_id, create_id, H5P_DEFAULT, access_id);
    CHECK(status, FAIL, "H5Tcommit2");

    status = H5Tclose(tmp_id);
    CHECK(status, FAIL, "H5Tclose");


    status = H5Pclose(create_id);
    CHECK(status, FAIL, "H5Pclose");

    /**********************************************************************
    * test H5Lcopy()
    **********************************************************************/

    /* Create link creation property list */
    create_id = H5Pcreate(H5P_LINK_CREATE);
    CHECK(create_id, FAIL, "H5Pcreate");

    /* Set flag for intermediate group creation */
    status = H5Pset_create_intermediate_group(create_id, TRUE);
    CHECK(status, FAIL, "H5Pset_create_intermediate_group");

    status = H5Lcopy(file_id, "/A/B01/grp", file_id, "/A/B16/grp", create_id, access_id);
    CHECK(status, FAIL, "H5Lcopy");

    tri_status = H5Lexists(file_id, "/A/B16/grp", access_id);
    VERIFY(tri_status, TRUE, "H5Lexists");

    tri_status = H5Lexists(file_id, "/A/B01/grp", access_id);
    VERIFY(tri_status, TRUE, "H5Lexists");

    /**********************************************************************
    * test H5Lmove()
    **********************************************************************/

    status = H5Lmove(file_id, "/A/B16/grp", file_id, "/A/B17/grp", create_id, access_id);
    CHECK(status, FAIL, "H5Lmove");

    tri_status = H5Lexists(file_id, "/A/B17/grp", access_id);
    VERIFY(tri_status, TRUE, "H5Lexists");

    tri_status = H5Lexists(file_id, "/A/B16/grp", access_id);
    VERIFY(tri_status, FALSE, "H5Lexists");

    /**********************************************************************
    * test H5Lcreate_hard()
    **********************************************************************/

    status = H5Lcreate_hard(file_id, "/A/B01/grp", file_id, "/A/B18/grp", create_id, access_id);
    CHECK(status, FAIL, "H5Lcreate_hard");

    tri_status = H5Lexists(file_id, "/A/B18/grp", access_id);
    VERIFY(tri_status, TRUE, "H5Lexists");

    /**********************************************************************
    * test H5Lcreate_soft()
    **********************************************************************/

    status = H5Lcreate_soft("/A/B01/grp", file_id, "/A/B19/grp", create_id, access_id);
    CHECK(status, FAIL, "H5Lcreate_soft");

    tri_status = H5Lexists(file_id, "/A/B19/grp", access_id);
    VERIFY(tri_status, TRUE, "H5Lexists");

    /**********************************************************************
    * test H5Lcreate_external()
    **********************************************************************/

    status = H5Lcreate_external("fake_filename", "fake_path", file_id, "/A/B20/grp", create_id, access_id);
    CHECK(status, FAIL, "H5Lcreate_external");

    tri_status = H5Lexists(file_id, "/A/B20/grp", access_id);
    VERIFY(tri_status, TRUE, "H5Lexists");

    /**********************************************************************
    * test H5Lcreate_ud()
    **********************************************************************/

    status = H5Lcreate_ud(file_id, "/A/B21/grp", H5L_TYPE_EXTERNAL, "file\0obj", (size_t) 9, create_id, access_id);
    CHECK(status, FAIL, "H5Lcreate_ud");

    tri_status = H5Lexists(file_id, "/A/B21/grp", access_id);
    VERIFY(tri_status, TRUE, "H5Lexists");

    /**********************************************************************
    * close
    **********************************************************************/

    status = H5Pclose(create_id);
    CHECK(status, FAIL, "H5Pclose");

    status = H5Gclose(group_id);
    CHECK(status, FAIL, "H5Gclose");

    status = H5Fclose(file_id);
    CHECK(status, FAIL, "H5Fclose");

} /* end test_misc23() */

/****************************************************************
**
**  test_misc24(): Test opening objects with inappropriate APIs
**
****************************************************************/
static void
test_misc24(void)
{
    hid_t       file_id = 0, group_id = 0, type_id = 0, space_id = 0,
                dset_id = 0, tmp_id = 0;
    herr_t      ret;            /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing opening objects with inappropriate APIs\n"));

    /* Create a new file using default properties. */
    file_id = H5Fcreate(MISC24_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file_id, FAIL, "H5Fcreate");

    /* Create dataspace for dataset */
    space_id = H5Screate(H5S_SCALAR);
    CHECK(space_id, FAIL, "H5Screate");

    /* Create group, dataset & named datatype objects */
    group_id = H5Gcreate2(file_id, MISC24_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(group_id, FAIL, "H5Gcreate2");

    dset_id = H5Dcreate2(file_id, MISC24_DATASET_NAME, H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dset_id, FAIL, "H5Dcreate2");

    type_id = H5Tcopy(H5T_NATIVE_INT);
    CHECK(type_id, FAIL, "H5Tcopy");

    ret = H5Tcommit2(file_id, MISC24_DATATYPE_NAME, type_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");

    /* Create soft links to the objects created */
    ret = H5Lcreate_soft(MISC24_GROUP_NAME, file_id, MISC24_GROUP_LINK, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Lcreate_soft");

    ret = H5Lcreate_soft(MISC24_DATASET_NAME, file_id, MISC24_DATASET_LINK, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Lcreate_soft");

    ret = H5Lcreate_soft(MISC24_DATATYPE_NAME, file_id, MISC24_DATATYPE_LINK, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Lcreate_soft");

    /* Close IDs for objects */
    ret = H5Dclose(dset_id);
    CHECK(ret, FAIL, "H5Dclose");

    ret = H5Sclose(space_id);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Gclose(group_id);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Tclose(type_id);
    CHECK(ret, FAIL, "H5Tclose");

    /* Attempt to open each kind of object with wrong API, including using soft links */
    H5E_BEGIN_TRY {
        tmp_id = H5Dopen2(file_id, MISC24_GROUP_NAME, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(tmp_id, FAIL, "H5Dopen2");

    H5E_BEGIN_TRY {
        tmp_id = H5Dopen2(file_id, MISC24_GROUP_LINK, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(tmp_id, FAIL, "H5Dopen2");

    H5E_BEGIN_TRY {
        tmp_id = H5Topen2(file_id, MISC24_GROUP_NAME, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(tmp_id, FAIL, "H5Topen2");

    H5E_BEGIN_TRY {
        tmp_id = H5Topen2(file_id, MISC24_GROUP_LINK, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(tmp_id, FAIL, "H5Topen2");

    H5E_BEGIN_TRY {
        tmp_id = H5Gopen2(file_id, MISC24_DATASET_NAME, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(tmp_id, FAIL, "H5Gopen2");

    H5E_BEGIN_TRY {
        tmp_id = H5Gopen2(file_id, MISC24_DATASET_LINK, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(tmp_id, FAIL, "H5Gopen2");

    H5E_BEGIN_TRY {
        tmp_id = H5Topen2(file_id, MISC24_DATASET_NAME, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(tmp_id, FAIL, "H5Topen2");

    H5E_BEGIN_TRY {
        tmp_id = H5Topen2(file_id, MISC24_DATASET_LINK, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(tmp_id, FAIL, "H5Topen2");

    H5E_BEGIN_TRY {
        tmp_id = H5Gopen2(file_id, MISC24_DATATYPE_NAME, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(tmp_id, FAIL, "H5Gopen2");

    H5E_BEGIN_TRY {
        tmp_id = H5Gopen2(file_id, MISC24_DATATYPE_LINK, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(tmp_id, FAIL, "H5Gopen2");

    H5E_BEGIN_TRY {
        tmp_id = H5Dopen2(file_id, MISC24_DATATYPE_NAME, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(tmp_id, FAIL, "H5Dopen2");

    H5E_BEGIN_TRY {
        tmp_id = H5Dopen2(file_id, MISC24_DATATYPE_LINK, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(tmp_id, FAIL, "H5Dopen2");

    /* Try again, with the object already open through valid call */
    /* Open group */
    group_id = H5Gopen2(file_id, MISC24_GROUP_NAME, H5P_DEFAULT);
    CHECK(group_id, FAIL, "H5Gopen2");

    H5E_BEGIN_TRY {
        tmp_id = H5Dopen2(file_id, MISC24_GROUP_NAME, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(tmp_id, FAIL, "H5Dopen2");

    H5E_BEGIN_TRY {
        tmp_id = H5Dopen2(file_id, MISC24_GROUP_LINK, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(tmp_id, FAIL, "H5Dopen2");

    H5E_BEGIN_TRY {
        tmp_id = H5Topen2(file_id, MISC24_GROUP_NAME, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(tmp_id, FAIL, "H5Topen2");

    H5E_BEGIN_TRY {
        tmp_id = H5Topen2(file_id, MISC24_GROUP_LINK, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(tmp_id, FAIL, "H5Topen2");

    ret = H5Gclose(group_id);
    CHECK(ret, FAIL, "H5Gclose");

    /* Open dataset */
    dset_id = H5Dopen2(file_id, MISC24_DATASET_NAME, H5P_DEFAULT);
    CHECK(dset_id, FAIL, "H5Dopen2");

    H5E_BEGIN_TRY {
        tmp_id = H5Gopen2(file_id, MISC24_DATASET_NAME, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(tmp_id, FAIL, "H5Gopen2");

    H5E_BEGIN_TRY {
        tmp_id = H5Gopen2(file_id, MISC24_DATASET_LINK, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(tmp_id, FAIL, "H5Gopen2");

    H5E_BEGIN_TRY {
        tmp_id = H5Topen2(file_id, MISC24_DATASET_NAME, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(tmp_id, FAIL, "H5Topen2");

    H5E_BEGIN_TRY {
        tmp_id = H5Topen2(file_id, MISC24_DATASET_LINK, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(tmp_id, FAIL, "H5Topen2");

    ret = H5Dclose(dset_id);
    CHECK(ret, FAIL, "H5Dclose");

    /* Open named datatype */
    type_id = H5Topen2(file_id, MISC24_DATATYPE_NAME, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Topen2");

    H5E_BEGIN_TRY {
        tmp_id = H5Gopen2(file_id, MISC24_DATATYPE_NAME, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(tmp_id, FAIL, "H5Gopen2");

    H5E_BEGIN_TRY {
        tmp_id = H5Gopen2(file_id, MISC24_DATATYPE_LINK, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(tmp_id, FAIL, "H5Gopen2");

    H5E_BEGIN_TRY {
        tmp_id = H5Dopen2(file_id, MISC24_DATATYPE_NAME, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(tmp_id, FAIL, "H5Dopen2");

    H5E_BEGIN_TRY {
        tmp_id = H5Dopen2(file_id, MISC24_DATATYPE_LINK, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(tmp_id, FAIL, "H5Dopen2");

    ret = H5Tclose(type_id);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close file */
    ret = H5Fclose(file_id);
    CHECK(ret, FAIL, "H5Fclose");
} /* end test_misc24() */

/****************************************************************
**
**  test_misc25a(): Exercise null object header message merge bug
**                      with new file
**
****************************************************************/
static void
test_misc25a(void)
{
    hid_t fid;          /* File ID */
    hid_t gid, gid2, gid3;      /* Group IDs */
    hid_t aid;          /* Attribute ID */
    hid_t sid;          /* Dataspace ID */
    hid_t tid;          /* Datatype ID */
    herr_t      ret;            /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Exercise null object header message bug\n"));

    /* Create file */
    fid = H5Fcreate(MISC25A_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create top group */
    gid = H5Gcreate2(fid, MISC25A_GROUP0_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gcreate2");

    /* Close top group */
    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    /* Create first group */
    gid = H5Gcreate2(fid, MISC25A_GROUP1_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gcreate2");

    /* Close first group */
    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    /* Create second group */
    gid2 = H5Gcreate2(fid, MISC25A_GROUP2_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(gid2, FAIL, "H5Gcreate2");

    /* Close second group */
    ret = H5Gclose(gid2);
    CHECK(ret, FAIL, "H5Gclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    /* Re-open file */
    fid = H5Fopen(MISC25A_FILE, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /* Re-open first group */
    gid = H5Gopen2(fid, MISC25A_GROUP1_NAME, H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gopen2");

    /* Create dataspace for attribute */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create dataype for attribute */
    tid = H5Tcopy(H5T_C_S1);
    CHECK(tid, FAIL, "H5Tcopy");
    ret = H5Tset_size(tid, (size_t)MISC25A_ATTR1_LEN);
    CHECK(ret, FAIL, "H5Tset_size");

    /* Add 1st attribute on first group */
    aid = H5Acreate2(gid, MISC25A_ATTR1_NAME, tid, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(aid, FAIL, "H5Acreate2");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close datatype */
    ret = H5Tclose(tid);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close attribute */
    ret = H5Aclose(aid);
    CHECK(ret, FAIL, "H5Aclose");

    /* Create dataspace for 2nd attribute */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create dataype for attribute */
    tid = H5Tcopy(H5T_C_S1);
    CHECK(tid, FAIL, "H5Tcopy");
    ret = H5Tset_size(tid, (size_t)MISC25A_ATTR2_LEN);
    CHECK(ret, FAIL, "H5Tset_size");

    /* Add 2nd attribute on first group */
    aid = H5Acreate2(gid, MISC25A_ATTR2_NAME, tid, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(aid, FAIL, "H5Acreate2");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close datatype */
    ret = H5Tclose(tid);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close 2nd attribute */
    ret = H5Aclose(aid);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close first group */
    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    /* Re-open file */
    fid = H5Fopen(MISC25A_FILE, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /* Create third group */
    gid3 = H5Gcreate2(fid, MISC25A_GROUP3_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(gid3, FAIL, "H5Gcreate2");

    /* Close third group */
    ret = H5Gclose(gid3);
    CHECK(ret, FAIL, "H5Gclose");

    /* Re-open first group */
    gid = H5Gopen2(fid, MISC25A_GROUP1_NAME, H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gopen2");

    /* Delete 2nd attribute */
    ret = H5Adelete(gid, MISC25A_ATTR2_NAME);
    CHECK(ret, FAIL, "H5Adelete");

    /* Close first group */
    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");



    /* Re-open file */
    fid = H5Fopen(MISC25A_FILE, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /* Re-open first group */
    gid = H5Gopen2(fid, MISC25A_GROUP1_NAME, H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gopen2");

    /* Create dataspace for 3rd attribute */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create dataype for attribute */
    tid = H5Tcopy(H5T_C_S1);
    CHECK(tid, FAIL, "H5Tcopy");
    ret = H5Tset_size(tid, (size_t)MISC25A_ATTR3_LEN);
    CHECK(ret, FAIL, "H5Tset_size");

    /* Add 3rd attribute on first group (smaller than 2nd attribute) */
    aid = H5Acreate2(gid, MISC25A_ATTR3_NAME, tid, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(aid, FAIL, "H5Acreate2");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close datatype */
    ret = H5Tclose(tid);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close 3rd attribute */
    ret = H5Aclose(aid);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close first group */
    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");



    /* Re-open file */
    fid = H5Fopen(MISC25A_FILE, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /* Re-open first group */
    gid = H5Gopen2(fid, MISC25A_GROUP1_NAME, H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gopen2");

    /* Delete 3rd attribute */
    ret = H5Adelete(gid, MISC25A_ATTR3_NAME);
    CHECK(ret, FAIL, "H5Adelete");

    /* Create dataspace for 3rd attribute */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create dataype for attribute */
    tid = H5Tcopy(H5T_C_S1);
    CHECK(tid, FAIL, "H5Tcopy");
    ret = H5Tset_size(tid, (size_t)MISC25A_ATTR2_LEN);
    CHECK(ret, FAIL, "H5Tset_size");

    /* Re-create 2nd attribute on first group */
    aid = H5Acreate2(gid, MISC25A_ATTR2_NAME, tid, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(aid, FAIL, "H5Acreate2");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close datatype */
    ret = H5Tclose(tid);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close 2nd attribute */
    ret = H5Aclose(aid);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close first group */
    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    /* Re-open file */
    fid = H5Fopen(MISC25A_FILE, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /* Re-open first group */
    gid = H5Gopen2(fid, MISC25A_GROUP1_NAME, H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gopen2");

    /* Delete 2nd attribute */
    ret = H5Adelete(gid, MISC25A_ATTR2_NAME);
    CHECK(ret, FAIL, "H5Adelete");

    /* Close first group */
    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    /* Re-open file */
    fid = H5Fopen(MISC25A_FILE, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /* Re-open first group */
    gid = H5Gopen2(fid, MISC25A_GROUP1_NAME, H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gopen2");

    /* Create dataspace for 3rd attribute */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create dataype for attribute */
    tid = H5Tcopy(H5T_C_S1);
    CHECK(tid, FAIL, "H5Tcopy");
    ret = H5Tset_size(tid, (size_t)MISC25A_ATTR2_LEN);
    CHECK(ret, FAIL, "H5Tset_size");

    /* Re-create 2nd attribute on first group */
    aid = H5Acreate2(gid, MISC25A_ATTR2_NAME, tid, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(aid, FAIL, "H5Acreate2");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close datatype */
    ret = H5Tclose(tid);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close 2nd attribute */
    ret = H5Aclose(aid);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close first group */
    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
} /* end test_misc25a() */

/****************************************************************
**
**  test_misc25b(): Exercise null object header message merge bug
**                      with existing file  (This test relies on
**                      the file produced by test/gen_mergemsg.c)
**
****************************************************************/
static void
test_misc25b(void)
{
    hid_t fid;          /* File ID */
    hid_t gid;          /* Group ID */
    char testfile[512]="";
    char *srcdir = HDgetenv("srcdir");
    herr_t      ret;            /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Exercise null object header message bug\n"));

    /* Build the name of the file, with the source directory */
    if (srcdir && ((HDstrlen(srcdir) + HDstrlen(MISC25B_FILE) + 1) < sizeof(testfile))){
	HDstrcpy(testfile, srcdir);
	HDstrcat(testfile, "/");
    }
    HDstrcat(testfile, MISC25B_FILE);

    /* Open file */
    fid = H5Fopen(testfile, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /* Re-open group with object header messages that will merge */
    gid = H5Gopen2(fid, MISC25B_GROUP, H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gopen2");

    /* Close first group */
    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
} /* end test_misc25b() */


/****************************************************************
**
**  test_misc25c(): Exercise another null object header message merge bug.
**
****************************************************************/
static void
test_misc25c(void)
{
    hid_t fid;          /* File ID */
    hid_t fapl;         /* File access property list ID */
    hid_t gcpl;         /* Group creation property list ID */
    hid_t sid;          /* Dataspace ID */
    hid_t did;          /* Dataset ID */
    hid_t gid;          /* Group ID */
    hid_t gid2;         /* Group ID */
    hid_t aid;          /* Attribute ID */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Exercise another null object header message bug\n"));

    /* Compose file access property list */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");
    ret = H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST);
    CHECK(ret, FAIL, "H5Pset_libver_bounds");

    /* Create the file */
    fid = H5Fcreate(MISC25C_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Compose group creation property list */
    gcpl = H5Pcreate(H5P_GROUP_CREATE);
    CHECK(gcpl, FAIL, "H5Pcreate");
    ret = H5Pset_link_creation_order(gcpl, (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED));
    CHECK(ret, FAIL, "H5Pset_link_creation_order");
    ret = H5Pset_attr_creation_order(gcpl, (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED));
    CHECK(ret, FAIL, "H5Pset_attr_creation_order");
    ret = H5Pset_est_link_info(gcpl, 1, 18);
    CHECK(ret, FAIL, "H5Pset_est_link_info");

    /* Create a group for the dataset */
    gid = H5Gcreate2(fid, MISC25C_DSETGRPNAME, H5P_DEFAULT, gcpl, H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gcreate2");

    /* Create the dataspace */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create the dataset */
    did = H5Dcreate2(gid, MISC25C_DSETNAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dcreate2");

    /* Create an extra group */
    gid2 = H5Gcreate2(fid, MISC25C_GRPNAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(gid2, FAIL, "H5Gcreate2");

    /* Close the extra group */
    ret = H5Gclose(gid2);
    CHECK(ret, FAIL, "H5Gclose");

    /* Add an attribute to the dataset group */
    aid = H5Acreate2(gid, MISC25C_ATTRNAME, H5T_NATIVE_CHAR, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(aid, FAIL, "H5Acreate2");

    /* Close the attribute */
    ret = H5Aclose(aid);
    CHECK(ret, FAIL, "H5Aclose");

    /* Create a second extra group */
    gid2 = H5Gcreate2(fid, MISC25C_GRPNAME2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(gid2, FAIL, "H5Gcreate2");

    /* Close the second extra group */
    ret = H5Gclose(gid2);
    CHECK(ret, FAIL, "H5Gclose");

    /* Add second attribute to the dataset group */
    aid = H5Acreate2(gid, MISC25C_ATTRNAME2, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(aid, FAIL, "H5Acreate2");

    /* Close the attribute */
    ret = H5Aclose(aid);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close the dataset */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close the dataset group */
    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    /* Close the dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Close the property lists */
    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(gcpl);
    CHECK(ret, FAIL, "H5Pclose");


    /* Re-open the file */
    fid = H5Fopen(MISC25C_FILE, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /* Re-open the dataset group */
    gid = H5Gopen2(fid, MISC25C_DSETGRPNAME, H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gopen2");

    /* Rename the dataset */
    ret = H5Lmove(gid, MISC25C_DSETNAME, H5L_SAME_LOC, MISC25C_DSETNAME2, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Lmove");

    /* Delete the first attribute */
    ret = H5Adelete(gid, MISC25C_ATTRNAME);
    CHECK(ret, FAIL, "H5Adelete");

    /* Close the dataset group */
    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
} /* end test_misc25c() */


/****************************************************************
**
**  test_misc26(): Regression test: ensure that copying filter
**                      pipelines works properly.
**
****************************************************************/
static void
test_misc26(void)
{
    hid_t fid;          /* File ID */
    hid_t sid;          /* Dataspace ID */
    hid_t did;          /* Dataset ID */
    hid_t dcpl1, dcpl2, dcpl3;         /* Property List IDs */
    hsize_t dims[] = {1};
    herr_t      ret;            /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Copying filter pipelines\n"));

    /* Create the property list.  It needs chunking so we can add filters */
    dcpl1 = H5Pcreate(H5P_DATASET_CREATE);
    CHECK_I(dcpl1, "H5Pcreate");
    ret = H5Pset_chunk(dcpl1, 1, dims);
    CHECK_I(ret, "H5Pset_chunk");

    /* Add a filter with a data value to the property list */
    ret = H5Pset_deflate(dcpl1, 1);
    CHECK_I(ret, "H5Pset_deflate");

    /* Copy the property list */
    dcpl2 = H5Pcopy(dcpl1);
    CHECK_I(dcpl2, "H5Pcopy");

    /* Add a filter with no data values to the copy */
    ret = H5Pset_shuffle(dcpl2);
    CHECK_I(ret, "H5Pset_shuffle");

    /* Copy the copy */
    dcpl3 = H5Pcopy(dcpl2);
    CHECK_I(dcpl3, "H5Pcopy");

    /* Add another filter */
    ret = H5Pset_deflate(dcpl3, 2);
    CHECK_I(ret, "H5Pset_deflate");


    /* Create a new file and datasets within that file that use these
     * property lists
     */
    fid = H5Fcreate(MISC26_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    sid = H5Screate_simple(1, dims, dims);
    CHECK(sid, FAIL, "H5Screate_simple");

    did = H5Dcreate2(fid, "dataset1", H5T_NATIVE_FLOAT, sid, H5P_DEFAULT, dcpl1, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dcreate2");
    ret = H5Dclose(did);
    CHECK_I(ret, "H5Dclose");

    did = H5Dcreate2(fid, "dataset2", H5T_NATIVE_FLOAT, sid, H5P_DEFAULT, dcpl2, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dcreate2");
    ret = H5Dclose(did);
    CHECK_I(ret, "H5Dclose");

    did = H5Dcreate2(fid, "dataset3", H5T_NATIVE_FLOAT, sid, H5P_DEFAULT, dcpl3, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dcreate2");
    ret = H5Dclose(did);
    CHECK_I(ret, "H5Dclose");

    /* Close the dataspace and file */
    ret = H5Sclose(sid);
    CHECK_I(ret, "H5Sclose");
    ret = H5Fclose(fid);
    CHECK_I(ret, "H5Fclose");

    /* Close the property lists.  */
    ret = H5Pclose(dcpl1);
    CHECK_I(ret, "H5Pclose");
    ret = H5Pclose(dcpl2);
    CHECK_I(ret, "H5Pclose");
    ret = H5Pclose(dcpl3);
    CHECK_I(ret, "H5Pclose");
}


/****************************************************************
**
**  test_misc27(): Ensure that objects with incorrect # of object
**                      header messages are handled appropriately.
**
** (Note that this test file is generated by the "gen_bad_ohdr.c" code)
**
****************************************************************/
static void
test_misc27(void)
{
    hid_t fid;          /* File ID */
    hid_t gid;          /* Group ID */
    char testfile[512]="";          /* Character buffer for corrected test file name */
    char *srcdir = HDgetenv("srcdir");    /* Pointer to the directory the source code is located within */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Corrupt object header handling\n"));

    /* Generate the correct name for the test file, by prepending the source path */
    if(srcdir && ((HDstrlen(srcdir) + HDstrlen(MISC27_FILE) + 1) < sizeof(testfile))) {
        HDstrcpy(testfile, srcdir);
        HDstrcat(testfile, "/");
    }
    HDstrcat(testfile, MISC27_FILE);

    /* Open the file */
    fid = H5Fopen(testfile, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

#ifdef H5_STRICT_FORMAT_CHECKS
    /* Open group with incorrect # of object header messages (should fail) */
    H5E_BEGIN_TRY {
        gid = H5Gopen2(fid, MISC27_GROUP, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(gid, FAIL, "H5Gopen2");
#else /* H5_STRICT_FORMAT_CHECKS */
    /* Open group with incorrect # of object header messages */
    gid = H5Gopen2(fid, MISC27_GROUP, H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gopen2");

    /* Close group */
    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");
#endif /* H5_STRICT_FORMAT_CHECKS */

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
} /* end test_misc27() */


/****************************************************************
**
**  test_misc28(): Ensure that the dataset chunk cache will hold
**                 the correct number of chunks in cache without
**                 evicting them.
**
****************************************************************/
static void
test_misc28(void)
{
    hid_t   fid;            /* File ID */
    hid_t   sidf;           /* File Dataspace ID */
    hid_t   sidm;           /* Memory Dataspace ID */
    hid_t   did;            /* Dataset ID */
    hid_t   dcpl, fapl;     /* Property List IDs */
    hsize_t dims[] = {MISC28_SIZE, MISC28_SIZE};
    hsize_t mdims[] = {MISC28_SIZE};
    hsize_t cdims[] = {1, 1};
    hsize_t start[] = {0,0};
    hsize_t count[] = {MISC28_SIZE, 1};
    size_t  nbytes_used;
    int     nused;
    char    buf[MISC28_SIZE];
    int     i;
    herr_t  ret;            /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Dataset chunk cache\n"));

    /* Create the fapl and set the cache size.  Set nelmts to larger than the
     * file size so we can be guaranteed that no chunks will be evicted due to
     * a hash collision.  Set nbytes to fit exactly 1 column of chunks (10
     * bytes). */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");
    ret = H5Pset_cache(fapl, MISC28_NSLOTS, MISC28_NSLOTS, MISC28_SIZE, 0.75);
    CHECK(ret, FAIL, "H5Pset_cache");

    /* Create the dcpl and set the chunk size */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");
    ret = H5Pset_chunk(dcpl, 2, cdims);
    CHECK(ret, FAIL, "H5Pset_chunk");


    /* Create a new file and datasets within that file that use these
     * property lists
     */
    fid = H5Fcreate(MISC28_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    sidf = H5Screate_simple(2, dims, NULL);
    CHECK(sidf, FAIL, "H5Screate_simple");

    did = H5Dcreate2(fid, "dataset", H5T_NATIVE_CHAR, sidf, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dcreate2");

    /* Verify that the chunk cache is empty */
    ret = H5D_current_cache_size_test(did, &nbytes_used, &nused);
    CHECK(ret, FAIL, "H5D_current_cache_size_test");
    VERIFY(nbytes_used, (size_t) 0, "H5D_current_cache_size_test");
    VERIFY(nused, 0, "H5D_current_cache_size_test");

    /* Initialize write buffer */
    for(i=0; i<MISC28_SIZE; i++)
        buf[i] = i;

    /* Create memory dataspace and selection in file dataspace */
    sidm = H5Screate_simple(1, mdims, NULL);
    CHECK(sidm, FAIL, "H5Screate_simple");

    ret = H5Sselect_hyperslab(sidf, H5S_SELECT_SET, start, NULL, count, NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Write hypserslab */
    ret = H5Dwrite(did, H5T_NATIVE_CHAR, sidm, sidf, H5P_DEFAULT, buf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Verify that all 10 chunks written have been cached */
    ret = H5D_current_cache_size_test(did, &nbytes_used, &nused);
    CHECK(ret, FAIL, "H5D_current_cache_size_test");
    VERIFY(nbytes_used, (size_t) MISC28_SIZE, "H5D_current_cache_size_test");
    VERIFY(nused, MISC28_SIZE, "H5D_current_cache_size_test");

    /* Initialize write buffer */
    for(i=0; i<MISC28_SIZE; i++)
        buf[i] = MISC28_SIZE - 1 - i;

    /* Select new hyperslab */
    start[1] = 1;
    ret = H5Sselect_hyperslab(sidf, H5S_SELECT_SET, start, NULL, count, NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Write hyperslab */
    ret = H5Dwrite(did, H5T_NATIVE_CHAR, sidm, sidf, H5P_DEFAULT, buf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Verify that the size of the cache remains at 10 */
    ret = H5D_current_cache_size_test(did, &nbytes_used, &nused);
    CHECK(ret, FAIL, "H5D_current_cache_size_test");
    VERIFY(nbytes_used, (size_t) MISC28_SIZE, "H5D_current_cache_size_test");
    VERIFY(nused, MISC28_SIZE, "H5D_current_cache_size_test");

    /* Close dataset */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");


    /* Re open dataset */
    did = H5Dopen2(fid, "dataset", H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dopen2");

    /* Verify that the chunk cache is empty */
    ret = H5D_current_cache_size_test(did, &nbytes_used, &nused);
    CHECK(ret, FAIL, "H5D_current_cache_size_test");
    VERIFY(nbytes_used, (size_t) 0, "H5D_current_cache_size_test");
    VERIFY(nused, 0, "H5D_current_cache_size_test");

    /* Select hyperslabe for reading */
    start[1] = 0;
    ret = H5Sselect_hyperslab(sidf, H5S_SELECT_SET, start, NULL, count, NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Read hypserslab */
    ret = H5Dread(did, H5T_NATIVE_CHAR, sidm, sidf, H5P_DEFAULT, buf);
    CHECK(ret, FAIL, "H5Dread");

    /* Verify the data read */
    for(i=0; i<MISC28_SIZE; i++)
        VERIFY(buf[i], i, "H5Dread");

    /* Verify that all 10 chunks read have been cached */
    ret = H5D_current_cache_size_test(did, &nbytes_used, &nused);
    CHECK(ret, FAIL, "H5D_current_cache_size_test");
    VERIFY(nbytes_used, (size_t) MISC28_SIZE, "H5D_current_cache_size_test");
    VERIFY(nused, MISC28_SIZE, "H5D_current_cache_size_test");

    /* Select new hyperslab */
    start[1] = 1;
    ret = H5Sselect_hyperslab(sidf, H5S_SELECT_SET, start, NULL, count, NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Read hyperslab */
    ret = H5Dread(did, H5T_NATIVE_CHAR, sidm, sidf, H5P_DEFAULT, buf);
    CHECK(ret, FAIL, "H5Dread");

    /* Verify the data read */
    for(i=0; i<MISC28_SIZE; i++)
        VERIFY(buf[i], MISC28_SIZE - 1 - i, "H5Dread");

    /* Verify that the size of the cache remains at 10 */
    ret = H5D_current_cache_size_test(did, &nbytes_used, &nused);
    CHECK(ret, FAIL, "H5D_current_cache_size_test");
    VERIFY(nbytes_used, (size_t) MISC28_SIZE, "H5D_current_cache_size_test");
    VERIFY(nused, MISC28_SIZE, "H5D_current_cache_size_test");

    /* Close dataset */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");


    /* Close the dataspaces and file */
    ret = H5Sclose(sidf);
    CHECK_I(ret, "H5Sclose");
    ret = H5Sclose(sidm);
    CHECK_I(ret, "H5Sclose");
    ret = H5Fclose(fid);
    CHECK_I(ret, "H5Fclose");

    /* Close the property lists.  */
    ret = H5Pclose(dcpl);
    CHECK_I(ret, "H5Pclose");
    ret = H5Pclose(fapl);
    CHECK_I(ret, "H5Pclose");
} /* end test_misc28() */


/****************************************************************
**
**  test_misc29(): Ensure that speculative metadata reads don't
**                 get raw data into the metadata accumulator.
**
****************************************************************/
static void
test_misc29(void)
{
    hid_t fid;              /* File ID */
    herr_t  ret;            /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Speculative metadata reads\n"));

    /* Make a copy of the data file from svn. */
    ret = h5_make_local_copy(MISC29_ORIG_FILE, MISC29_COPY_FILE);
    CHECK(ret, -1, "h5_make_local_copy");

    /* Open the copied file */
    fid = H5Fopen(MISC29_COPY_FILE, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /* Delete the last dataset */
    ret = H5Ldelete(fid, MISC29_DSETNAME, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Ldelete");

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
} /* end test_misc29() */

/****************************************************************
**
**  test_misc(): Main misc. test routine.
**
****************************************************************/
void
test_misc(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing Miscellaneous Routines\n"));

    test_misc1();       /* Test unlinking a dataset & immediately re-using name */
    test_misc2();       /* Test storing a VL-derived datatype in two different files */
    test_misc3();       /* Test reading from chunked dataset with non-zero fill value */
    test_misc4();       /* Test retrieving the fileno for various objects with H5Oget_info() */
    test_misc5();       /* Test several level deep nested compound & VL datatypes */
    test_misc6();       /* Test object header continuation code */
    test_misc7();       /* Test for sensible datatypes stored on disk */
    test_misc8();       /* Test storage sizes of various types of dataset storage */
    test_misc9();       /* Test for opening (not creating) core files */
    test_misc10();      /* Test for using dataset creation property lists from old files */
    test_misc11();      /* Test for all properties of a file creation property list being stored */
    test_misc12();      /* Test VL-strings in chunked datasets operating correctly */
    test_misc13();      /* Test that a user block can be insert in front of file contents */
    test_misc14();      /* Test that deleted dataset's data is removed from sieve buffer correctly */
    test_misc15();      /* Test that checking a file's access property list more than once works */
    test_misc16();      /* Test array of fixed-length string */
    test_misc17();      /* Test array of ASCII character */
    test_misc18();      /* Test new object header information in H5O_info_t struct */
    test_misc19();      /* Test incrementing & decrementing ref count on IDs */
    test_misc20();      /* Test problems with truncated dimensions in version 2 of storage layout message */
#if defined H5_HAVE_FILTER_SZIP
    test_misc21();      /* Test that "late" allocation time is treated the same as "incremental", for chunked datasets w/a filters */
    test_misc22();      /* check szip bits per pixel */
#endif /* H5_HAVE_FILTER_SZIP */
    test_misc23();      /* Test intermediate group creation */
    test_misc24();      /* Test inappropriate API opens of objects */
    test_misc25a();     /* Exercise null object header message merge bug */
    test_misc25b();     /* Exercise null object header message merge bug on existing file */
    test_misc25c();     /* Exercise another null object header message merge bug */
    test_misc26();      /* Test closing property lists with long filter pipelines */
    test_misc27();      /* Test opening file with object that has bad # of object header messages */
    test_misc28();      /* Test that chunks are cached appropriately */
    test_misc29();      /* Test that speculative metadata reads are handled correctly */


} /* test_misc() */


/*-------------------------------------------------------------------------
 * Function:	cleanup_misc
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	Albert Cheng
 *              July 2, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_misc(void)
{
    HDremove(MISC1_FILE);
    HDremove(MISC2_FILE_1);
    HDremove(MISC2_FILE_2);
    HDremove(MISC3_FILE);
    HDremove(MISC4_FILE_1);
    HDremove(MISC4_FILE_2);
    HDremove(MISC5_FILE);
    HDremove(MISC6_FILE);
    HDremove(MISC7_FILE);
    HDremove(MISC8_FILE);
    HDremove(MISC9_FILE);
    HDremove(MISC10_FILE_NEW);
    HDremove(MISC11_FILE);
    HDremove(MISC12_FILE);
    HDremove(MISC13_FILE_1);
    HDremove(MISC13_FILE_2);
    HDremove(MISC14_FILE);
    HDremove(MISC15_FILE);
    HDremove(MISC16_FILE);
    HDremove(MISC17_FILE);
    HDremove(MISC18_FILE);
    HDremove(MISC19_FILE);
    HDremove(MISC20_FILE);
#if defined H5_HAVE_FILTER_SZIP
    HDremove(MISC21_FILE);
    HDremove(MISC22_FILE);
#endif /* H5_HAVE_FILTER_SZIP */
    HDremove(MISC23_FILE);
    HDremove(MISC24_FILE);
    HDremove(MISC25A_FILE);
    HDremove(MISC25C_FILE);
    HDremove(MISC26_FILE);
    HDremove(MISC28_FILE);
    HDremove(MISC29_COPY_FILE);
}

