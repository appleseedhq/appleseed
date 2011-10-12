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

/* Unicode test */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "testhdf5.h"

#define NUM_CHARS 16
#define MAX_STRING_LENGTH ((NUM_CHARS * 4) + 1) /* Max length in bytes */
#define MAX_PATH_LENGTH (MAX_STRING_LENGTH + 20) /* Max length in bytes */
#define MAX_CODE_POINT 0x200000
#define FILENAME "unicode.h5"
/* A buffer to hold two copies of the UTF-8 string */
#define LONG_BUF_SIZE (2 * MAX_STRING_LENGTH + 4)

#define DSET1_NAME "fl_string_dataset"
#define DSET2_NAME "dataset2"
#define DSET3_NAME "dataset3"
#define DSET4_NAME "dataset4"
#define VL_DSET1_NAME "vl_dset_1"
#define VL_DSET2_NAME "vl_dset_2"
#define GROUP1_NAME "group1"
#define GROUP2_NAME "group2"
#define GROUP3_NAME "group3"
#define GROUP4_NAME "group4"
#define SLINK_NAME "soft_link"

#define RANK 1
#define COMP_INT_VAL 7
#define COMP_FLOAT_VAL -42.0
#define COMP_DOUBLE_VAL 42.0

/* Test function prototypes */
void test_fl_string(hid_t fid, const char *string);
void test_strpad(hid_t fid, const char *string);
void test_vl_string(hid_t fid, const char *string);
void test_objnames(hid_t fid, const char *string);
void test_attrname(hid_t fid, const char *string);
void test_compound(hid_t fid, const char *string);
void test_enum(hid_t fid, const char *string);
void test_opaque(hid_t fid, const char *string);

/* Utility function prototypes */
static hid_t mkstr(size_t len, H5T_str_t strpad);
unsigned int write_char(unsigned int c, char * test_string, unsigned int cur_pos);
void dump_string(const char * string);

/*
 * test_fl_string
 * Tests that UTF-8 can be used for fixed-length string data.
 * Writes the string to a dataset and reads it back again.
 */
void test_fl_string(hid_t fid, const char *string)
{
  hid_t dtype_id, space_id, dset_id;
  hsize_t dims = 1;
  char read_buf[MAX_STRING_LENGTH];
  H5T_cset_t cset;
  herr_t ret;

  /* Create the datatype, ensure that the character set behaves
   * correctly (it should default to ASCII and can be set to UTF8)
   */
  dtype_id = H5Tcopy(H5T_C_S1);
  CHECK(dtype_id, FAIL, "H5Tcopy");
  ret = H5Tset_size(dtype_id, (size_t)MAX_STRING_LENGTH);
  CHECK(ret, FAIL, "H5Tset_size");
  cset = H5Tget_cset(dtype_id);
  VERIFY(cset, H5T_CSET_ASCII, "H5Tget_cset");
  ret = H5Tset_cset(dtype_id, H5T_CSET_UTF8);
  CHECK(ret, FAIL, "H5Tset_cset");
  cset = H5Tget_cset(dtype_id);
  VERIFY(cset, H5T_CSET_UTF8, "H5Tget_cset");

  /* Create dataspace for a dataset */
  space_id = H5Screate_simple(RANK, &dims, NULL);
  CHECK(space_id, FAIL, "H5Screate_simple");

  /* Create a dataset */
  dset_id = H5Dcreate2(fid, DSET1_NAME, dtype_id, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  CHECK(dset_id, FAIL, "H5Dcreate2");

  /* Write UTF-8 string to dataset */
  ret = H5Dwrite(dset_id, dtype_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, string);
  CHECK(ret, FAIL, "H5Dwrite");

  /* Read string back and make sure it is unchanged */
  ret = H5Dread(dset_id, dtype_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, read_buf);
  CHECK(ret, FAIL, "H5Dread");

  VERIFY(HDstrcmp(string, read_buf), 0, "strcmp");

  /* Close all */
  ret = H5Dclose(dset_id);
  CHECK(ret, FAIL, "H5Dclose");

  ret = H5Tclose(dtype_id);
  CHECK(ret, FAIL, "H5Tclose");
  ret = H5Sclose(space_id);
  CHECK(ret, FAIL, "H5Sclose");
}

/*
 * test_strpad
 * Tests string padding for a UTF-8 string.
 * Converts strings to shorter and then longer strings.
 * Borrows heavily from dtypes.c, but is more complicated because
 * the string is randomly generated.
 */
void test_strpad(hid_t UNUSED fid, const char *string)
{
    /* buf is used to hold the data that H5Tconvert operates on. */
    char     buf[LONG_BUF_SIZE];

    /* cmpbuf holds the output that H5Tconvert should produce,
     * to compare against the actual output. */
    char     cmpbuf[LONG_BUF_SIZE];

    /* new_string is a slightly modified version of the UTF-8
     * string to make the tests run more smoothly. */
    char     new_string[MAX_STRING_LENGTH + 2];

    size_t   length;  /* Length of new_string in bytes */
    size_t   small_len;  /* Size of the small datatype */
    size_t   big_len;   /* Size of the larger datatype */
    hid_t    src_type, dst_type;
    herr_t   ret;

    /* The following tests are simpler if the UTF-8 string contains
     * the right number of bytes (even or odd, depending on the test).
     * We create a 'new_string' whose length is convenient by prepending
     * an 'x' to 'string' when necessary. */
    length = HDstrlen(string);
    if(length % 2 != 1)
    {
      HDstrcpy(new_string, "x");
      HDstrcat(new_string, string);
      length++;
    } else {
      HDstrcpy(new_string, string);
    }


    /* Convert a null-terminated string to a shorter and longer null
     * terminated string. */

    /* Create a src_type that holds the UTF-8 string and its final NULL */
    big_len = length + 1;                     /* +1 byte for final NULL */
    src_type = mkstr(big_len, H5T_STR_NULLTERM);
    CHECK(src_type, FAIL, "mkstr");
    /* Create a dst_type that holds half of the UTF-8 string and a final
     * NULL */
    small_len = (length + 1) / 2;
    dst_type = mkstr(small_len, H5T_STR_NULLTERM);
    CHECK(dst_type, FAIL, "mkstr");

    /* Fill the buffer with two copies of the UTF-8 string, each with a
     * terminating NULL.  It will look like "abcdefg\0abcdefg\0". */
    strncpy(buf, new_string, big_len);
    strncpy(&buf[big_len], new_string, big_len);

    ret = H5Tconvert(src_type, dst_type, (size_t)2, buf, NULL, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tconvert");

    /* After conversion, the buffer should look like
     * "abc\0abc\0abcdefg\0".  Note that this is just what the bytes look
     * like; UTF-8 characters may well have been truncated.
     * To check that the conversion worked properly, we'll build this
     * string manually. */
    HDstrncpy(cmpbuf, new_string, small_len - 1);
    cmpbuf[small_len - 1] = '\0';
    HDstrncpy(&cmpbuf[small_len], new_string, small_len -1);
    cmpbuf[2 * small_len - 1] = '\0';
    HDstrcpy(&cmpbuf[2 * small_len], new_string);

    VERIFY(HDmemcmp(buf, cmpbuf, 2*big_len), 0, "HDmemcmp");

    /* Now convert from smaller datatype to bigger datatype.  This should
     * leave our buffer looking like: "abc\0\0\0\0\0abc\0\0\0\0\0" */
    ret = H5Tconvert(dst_type, src_type, (size_t)2, buf, NULL, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tconvert");

    /* First fill the buffer with NULLs */
    HDmemset(cmpbuf, '\0', (size_t)LONG_BUF_SIZE);
    /* Copy in the characters */
    HDstrncpy(cmpbuf, new_string, small_len -1);
    HDstrncpy(&cmpbuf[big_len], new_string, small_len -1);

    VERIFY(HDmemcmp(buf, cmpbuf, 2*big_len), 0, "HDmemcmp");

    ret = H5Tclose(src_type);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Tclose(dst_type);
    CHECK(ret, FAIL, "H5Tclose");


    /* Now test null padding.  Null-padded strings do *not* need
     * terminating NULLs, so the sizes of the datatypes are slightly
     * different and we want a string with an even number of characters. */
    length = HDstrlen(string);
    if(length % 2 != 0)
    {
      HDstrcpy(new_string, "x");
      HDstrcat(new_string, string);
      length++;
    } else {
      HDstrcpy(new_string, string);
    }

    /* Create a src_type that holds the UTF-8 string */
    big_len = length;
    src_type = mkstr(big_len, H5T_STR_NULLPAD);
    CHECK(src_type, FAIL, "mkstr");
    /* Create a dst_type that holds half of the UTF-8 string */
    small_len = length / 2;
    dst_type = mkstr(small_len, H5T_STR_NULLPAD);
    CHECK(dst_type, FAIL, "mkstr");

    /* Fill the buffer with two copies of the UTF-8 string.
     * It will look like "abcdefghabcdefgh". */
    strncpy(buf, new_string, big_len);
    strncpy(&buf[big_len], new_string, big_len);

    ret = H5Tconvert(src_type, dst_type, (size_t)2, buf, NULL, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tconvert");

    /* After conversion, the buffer should look like
     * "abcdabcdabcdefgh".  Note that this is just what the bytes look
     * like; UTF-8 characters may well have been truncated.
     * To check that the conversion worked properly, we'll build this
     * string manually. */
    HDstrncpy(cmpbuf, new_string, small_len);
    HDstrncpy(&cmpbuf[small_len], new_string, small_len);
    HDstrncpy(&cmpbuf[2 * small_len], new_string, big_len);

    VERIFY(HDmemcmp(buf, cmpbuf, 2*big_len), 0, "HDmemcmp");

    /* Now convert from smaller datatype to bigger datatype.  This should
     * leave our buffer looking like: "abcd\0\0\0\0abcd\0\0\0\0" */
    ret = H5Tconvert(dst_type, src_type, (size_t)2, buf, NULL, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tconvert");

    /* First fill the buffer with NULLs */
    HDmemset(cmpbuf, '\0', (size_t)LONG_BUF_SIZE);
    /* Copy in the characters */
    HDstrncpy(cmpbuf, new_string, small_len);
    HDstrncpy(&cmpbuf[big_len], new_string, small_len);

    VERIFY(HDmemcmp(buf, cmpbuf, 2*big_len), 0, "HDmemcmp");

    ret = H5Tclose(src_type);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Tclose(dst_type);
    CHECK(ret, FAIL, "H5Tclose");


    /* Test space padding.  This is very similar to null-padding; we can
       use the same values of length, small_len, and big_len. */

    src_type = mkstr(big_len, H5T_STR_SPACEPAD);
    CHECK(src_type, FAIL, "mkstr");
    dst_type = mkstr(small_len, H5T_STR_SPACEPAD);
    CHECK(src_type, FAIL, "mkstr");

    /* Fill the buffer with two copies of the UTF-8 string.
     * It will look like "abcdefghabcdefgh". */
    HDstrcpy(buf, new_string);
    HDstrcpy(&buf[big_len], new_string);

    ret = H5Tconvert(src_type, dst_type, (size_t)2, buf, NULL, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tconvert");

    /* After conversion, the buffer should look like
     * "abcdabcdabcdefgh".  Note that this is just what the bytes look
     * like; UTF-8 characters may have been truncated.
     * To check that the conversion worked properly, we'll build this
     * string manually. */
    HDstrncpy(cmpbuf, new_string, small_len);
    HDstrncpy(&cmpbuf[small_len], new_string, small_len);
    HDstrncpy(&cmpbuf[2 * small_len], new_string, big_len);

    VERIFY(HDmemcmp(buf, cmpbuf, 2*big_len), 0, "HDmemcmp");

    /* Now convert from smaller datatype to bigger datatype.  This should
     * leave our buffer looking like: "abcd    abcd    " */
    ret = H5Tconvert(dst_type, src_type, (size_t)2, buf, NULL, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tconvert");

    /* First fill the buffer with spaces */
    HDmemset(cmpbuf, ' ', (size_t)LONG_BUF_SIZE);
    /* Copy in the characters */
    HDstrncpy(cmpbuf, new_string, small_len);
    HDstrncpy(&cmpbuf[big_len], new_string, small_len);

    VERIFY(HDmemcmp(buf, cmpbuf, 2*big_len), 0, "HDmemcmp");

    ret = H5Tclose(src_type);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Tclose(dst_type);
    CHECK(ret, FAIL, "H5Tclose");
}


/*
 * test_vl_string
 * Tests variable-length string datatype with UTF-8 strings.
 */
void test_vl_string(hid_t fid, const char *string)
{
  hid_t type_id, space_id, dset_id;
  hsize_t dims = 1;
  hsize_t size;  /* Number of bytes used */
  char *read_buf[1];
  herr_t ret;

  /* Create dataspace for datasets */
  space_id = H5Screate_simple(RANK, &dims, NULL);
  CHECK(space_id, FAIL, "H5Screate_simple");

  /* Create a datatype to refer to */
  type_id = H5Tcopy(H5T_C_S1);
  CHECK(type_id, FAIL, "H5Tcopy");
  ret = H5Tset_size(type_id, H5T_VARIABLE);
  CHECK(ret, FAIL, "H5Tset_size");

  /* Create a dataset */
  dset_id = H5Dcreate2(fid, VL_DSET1_NAME, type_id, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  CHECK(dset_id, FAIL, "H5Dcreate2");

  /* Write dataset to disk */
  ret = H5Dwrite(dset_id, type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, &string);
  CHECK(ret, FAIL, "H5Dwrite");

  /* Make certain the correct amount of memory will be used */
  ret = H5Dvlen_get_buf_size(dset_id, type_id, space_id, &size);
  CHECK(ret, FAIL, "H5Dvlen_get_buf_size");
  VERIFY(size, (hsize_t)HDstrlen(string) + 1, "H5Dvlen_get_buf_size");

  /* Read dataset from disk */
  ret = H5Dread(dset_id, type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, read_buf);
  CHECK(ret, FAIL, "H5Dread");

  /* Compare data read in */
  VERIFY(HDstrcmp(string, read_buf[0]), 0, "strcmp");

  /* Reclaim the read VL data */
  ret = H5Dvlen_reclaim(type_id, space_id, H5P_DEFAULT, read_buf);
  CHECK(ret, FAIL, "H5Dvlen_reclaim");

  /* Close all */
  ret = H5Dclose(dset_id);
  CHECK(ret, FAIL, "H5Dclose");
  ret = H5Tclose(type_id);
  CHECK(ret, FAIL, "H5Tclose");
  ret = H5Sclose(space_id);
  CHECK(ret, FAIL, "H5Sclose");
}

/*
 * test_objnames
 * Tests that UTF-8 can be used for object names in the file.
 * Tests groups, datasets, named datatypes, and soft links.
 * Note that this test doesn't actually mark the names as being
 * in UTF-8.  At the time this test was written, that feature
 * didn't exist in HDF5, and when the character encoding property
 * was added to links it didn't change how they were stored in the file,
 * -JML 2/2/2006
 */
void test_objnames(hid_t fid, const char* string)
{
  hid_t grp_id, grp1_id, grp2_id, grp3_id;
  hid_t type_id, dset_id, space_id;
  char read_buf[MAX_STRING_LENGTH];
  char path_buf[MAX_PATH_LENGTH];
  hsize_t dims=1;
  hobj_ref_t obj_ref;
  herr_t ret;

  /* Create a group with a UTF-8 name */
  grp_id = H5Gcreate2(fid, string, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  CHECK(grp_id, FAIL, "H5Gcreate2");

  /* Set a comment on the group to test that we can access the group
   * Also test that UTF-8 comments can be read.
   */
  ret = H5Oset_comment_by_name(fid, string, string, H5P_DEFAULT);
  CHECK(ret, FAIL, "H5Oset_comment_by_name");
  ret = H5Oget_comment_by_name(fid, string, read_buf, (size_t)MAX_STRING_LENGTH, H5P_DEFAULT);
  CHECK(ret, FAIL, "H5Oget_comment_by_name");

  ret = H5Gclose(grp_id);
  CHECK(ret, FAIL, "H5Gclose");

  VERIFY(HDstrcmp(string, read_buf), 0, "strcmp");

  /* Create a new dataset with a UTF-8 name */
  grp1_id = H5Gcreate2(fid, GROUP1_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  CHECK(grp1_id, FAIL, "H5Gcreate2");

  space_id = H5Screate_simple(RANK, &dims, NULL);
  CHECK(space_id, FAIL, "H5Screate_simple");
  dset_id = H5Dcreate2(grp1_id, string, H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  CHECK(dset_id, FAIL, "H5Dcreate2");

  /* Make sure that dataset can be opened again */
  ret = H5Dclose(dset_id);
  CHECK(ret, FAIL, "H5Dclose");
  ret = H5Sclose(space_id);
  CHECK(ret, FAIL, "H5Sclose");

  dset_id = H5Dopen2(grp1_id, string, H5P_DEFAULT);
  CHECK(ret, FAIL, "H5Dopen2");
  ret = H5Dclose(dset_id);
  CHECK(ret, FAIL, "H5Dclose");
  ret = H5Gclose(grp1_id);
  CHECK(ret, FAIL, "H5Gclose");

  /* Do the same for a named datatype */
  grp2_id = H5Gcreate2(fid, GROUP2_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  CHECK(grp2_id, FAIL, "H5Gcreate2");

  type_id = H5Tcreate(H5T_OPAQUE, (size_t)1);
  CHECK(type_id, FAIL, "H5Tcreate");
  ret = H5Tcommit2(grp2_id, string, type_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  CHECK(type_id, FAIL, "H5Tcommit2");
  ret = H5Tclose(type_id);
  CHECK(type_id, FAIL, "H5Tclose");

  type_id = H5Topen2(grp2_id, string, H5P_DEFAULT);
  CHECK(type_id, FAIL, "H5Topen2");
  ret = H5Tclose(type_id);
  CHECK(type_id, FAIL, "H5Tclose");

  /* Don't close the group -- use it to test that object references
   * can refer to objects named in UTF-8 */

  space_id = H5Screate_simple(RANK, &dims, NULL);
  CHECK(space_id, FAIL, "H5Screate_simple");
  dset_id = H5Dcreate2(grp2_id, DSET3_NAME, H5T_STD_REF_OBJ, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  CHECK(ret, FAIL, "H5Dcreate2");

  /* Create reference to named datatype */
  ret = H5Rcreate(&obj_ref, grp2_id, string, H5R_OBJECT, -1);
  CHECK(ret, FAIL, "H5Rcreate");
  /* Write selection and read it back*/
  ret = H5Dwrite(dset_id, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, H5P_DEFAULT, &obj_ref);
  CHECK(ret, FAIL, "H5Dwrite");
  ret = H5Dread(dset_id, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, H5P_DEFAULT, &obj_ref);
  CHECK(ret, FAIL, "H5Dread");

  /* Ensure that we can open named datatype using object reference */
  type_id = H5Rdereference(dset_id, H5R_OBJECT, &obj_ref);
  CHECK(type_id, FAIL, "H5Rdereference");
  ret = H5Tcommitted(type_id);
  VERIFY(ret, 1, "H5Tcommitted");

  ret = H5Tclose(type_id);
  CHECK(type_id, FAIL, "H5Tclose");
  ret = H5Dclose(dset_id);
  CHECK(ret, FAIL, "H5Dclose");
  ret = H5Sclose(space_id);
  CHECK(ret, FAIL, "H5Sclose");

  ret = H5Gclose(grp2_id);
  CHECK(ret, FAIL, "H5Gclose");

  /* Create "group3".  Build a hard link from group3 to group2, which has
   * a datatype with the UTF-8 name.  Create a soft link in group3
   * pointing through the hard link to the datatype.  Give the soft
   * link a name in UTF-8.  Ensure that the soft link works. */

  grp3_id = H5Gcreate2(fid, GROUP3_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  CHECK(grp3_id, FAIL, "H5Gcreate2");

  ret = H5Lcreate_hard(fid, GROUP2_NAME, grp3_id, GROUP2_NAME, H5P_DEFAULT, H5P_DEFAULT);
  CHECK(ret, FAIL, "H5Lcreate_hard");
  HDstrcpy(path_buf, GROUP2_NAME);
  HDstrcat(path_buf, "/");
  HDstrcat(path_buf, string);
  ret = H5Lcreate_hard(grp3_id, path_buf, H5L_SAME_LOC, string, H5P_DEFAULT, H5P_DEFAULT);
  CHECK(ret, FAIL, "H5Lcreate_hard");

  /* Open named datatype using soft link */
  type_id = H5Topen2(grp3_id, string, H5P_DEFAULT);
  CHECK(type_id, FAIL, "H5Topen2");

  ret = H5Tclose(type_id);
  CHECK(type_id, FAIL, "H5Tclose");
  ret = H5Gclose(grp3_id);
  CHECK(ret, FAIL, "H5Gclose");
}

/*
 * test_attrname
 * Test that attributes can deal with UTF-8 strings
 */
void test_attrname(hid_t fid, const char * string)
{
  hid_t group_id, attr_id;
  hid_t dtype_id, space_id;
  hsize_t dims=1;
  char read_buf[MAX_STRING_LENGTH];
  herr_t ret;

 /* Create a new group and give it an attribute whose
  * name and value are UTF-8 strings.
  */
  group_id = H5Gcreate2(fid, GROUP4_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  CHECK(group_id, FAIL, "H5Gcreate2");

  space_id = H5Screate_simple(RANK, &dims, NULL);
  CHECK(space_id, FAIL, "H5Screate_simple");
  dtype_id = H5Tcopy(H5T_C_S1);
  CHECK(dtype_id, FAIL, "H5Tcopy");
  ret = H5Tset_size(dtype_id, (size_t)MAX_STRING_LENGTH);
  CHECK(ret, FAIL, "H5Tset_size");

  /* Create the attribute and check that its name is correct */
  attr_id = H5Acreate2(group_id, string, dtype_id, space_id, H5P_DEFAULT, H5P_DEFAULT);
  CHECK(attr_id, FAIL, "H5Acreate2");
  ret = H5Aget_name(attr_id, (size_t)MAX_STRING_LENGTH, read_buf);
  CHECK(ret, FAIL, "H5Aget_name");
  ret = strcmp(read_buf, string);
  VERIFY(ret, 0, "strcmp");
  read_buf[0] = '\0';

  /* Try writing and reading from the attribute */
  ret = H5Awrite(attr_id, dtype_id, string);
  CHECK(ret, FAIL, "H5Awrite");
  ret = H5Aread(attr_id, dtype_id, read_buf);
  CHECK(ret, FAIL, "H5Aread");
  ret = strcmp(read_buf, string);
  VERIFY(ret, 0, "strcmp");

  /* Clean up */
  ret = H5Aclose(attr_id);
  CHECK(ret, FAIL, "H5Aclose");
  ret = H5Tclose(dtype_id);
  CHECK(ret, FAIL, "H5Tclose");
  ret = H5Sclose(space_id);
  CHECK(ret, FAIL, "H5Sclose");
  ret = H5Gclose(group_id);
  CHECK(ret, FAIL, "H5Gclose");
}

/*
 * test_compound
 * Test that compound datatypes can have UTF-8 field names.
 */
void test_compound(hid_t fid, const char * string)
{
  /* Define two compound structures, s1_t and s2_t.
   * s2_t is a subset of s1_t, with two out of three
   * fields.
   * This is stolen from the h5_compound example.
   */
  typedef struct s1_t {
      int    a;
      double c;
      float b;
  } s1_t;
  typedef struct s2_t {
      double c;
      int    a;
  } s2_t;
  /* Actual variable declarations */
  s1_t       s1;
  s2_t       s2;
  hid_t      s1_tid, s2_tid;
  hid_t      space_id, dset_id;
  hsize_t    dim = 1;
  char      *readbuf;
  herr_t     ret;

  /* Initialize compound data */
  HDmemset(&s1, 0, sizeof(s1_t));        /* To make purify happy */
  s1.a = COMP_INT_VAL;
  s1.c = COMP_DOUBLE_VAL;
  s1.b = COMP_FLOAT_VAL;

  /* Create compound datatypes using UTF-8 field name */
  s1_tid = H5Tcreate (H5T_COMPOUND, sizeof(s1_t));
  CHECK(s1_tid, FAIL, "H5Tcreate");
  ret = H5Tinsert(s1_tid, string, HOFFSET(s1_t, a), H5T_NATIVE_INT);
  CHECK(ret, FAIL, "H5Tinsert");

  /* Check that the field name was stored correctly */
  readbuf = H5Tget_member_name(s1_tid, 0);
  ret = HDstrcmp(readbuf, string);
  VERIFY(ret, 0, "strcmp");
  free(readbuf);

  /* Add the other fields to the datatype */
  ret = H5Tinsert(s1_tid, "c_name", HOFFSET(s1_t, c), H5T_NATIVE_DOUBLE);
  CHECK(ret, FAIL, "H5Tinsert");
  ret = H5Tinsert(s1_tid, "b_name", HOFFSET(s1_t, b), H5T_NATIVE_FLOAT);
  CHECK(ret, FAIL, "H5Tinsert");

  /* Create second datatype, with only two fields. */
  s2_tid = H5Tcreate (H5T_COMPOUND, sizeof(s2_t));
  CHECK(s2_tid, FAIL, "H5Tcreate");
  ret = H5Tinsert(s2_tid, "c_name", HOFFSET(s2_t, c), H5T_NATIVE_DOUBLE);
  CHECK(ret, FAIL, "H5Tinsert");
  ret = H5Tinsert(s2_tid, string, HOFFSET(s2_t, a), H5T_NATIVE_INT);
  CHECK(ret, FAIL, "H5Tinsert");

  /* Create the dataspace and dataset. */
  space_id = H5Screate_simple(1, &dim, NULL);
  CHECK(space_id, FAIL, "H5Screate_simple");
  dset_id = H5Dcreate2(fid, DSET4_NAME, s1_tid, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  CHECK(dset_id, FAIL, "H5Dcreate2");

  /* Write data to the dataset. */
  ret = H5Dwrite(dset_id, s1_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &s1);
  CHECK(ret, FAIL, "H5Dwrite");

  /* Ensure that data can be read back by field name into s2 struct */
  ret = H5Dread(dset_id, s2_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &s2);
  CHECK(ret, FAIL, "H5Dread");

  VERIFY(s2.a, COMP_INT_VAL, "H5Dread");
  VERIFY(s2.c, COMP_DOUBLE_VAL, "H5Dread");

  /* Clean up */
  ret = H5Tclose(s1_tid);
  CHECK(ret, FAIL, "H5Tclose");
  ret = H5Tclose(s2_tid);
  CHECK(ret, FAIL, "H5Tclose");
  ret = H5Sclose(space_id);
  CHECK(ret, FAIL, "H5Sclose");
  ret = H5Dclose(dset_id);
  CHECK(ret, FAIL, "H5Dclose");
}

/*
 * test_enum
 * Test that enumerated datatypes can have UTF-8 member names.
 */
void test_enum(hid_t UNUSED fid, const char * string)
{
  /* Define an enumerated type */
  typedef enum {
    E1_RED,
    E1_GREEN,
    E1_BLUE,
    E1_WHITE
  } c_e1;
  /* Variable declarations */
  c_e1 val;
  herr_t ret;
  hid_t type_id;
  char readbuf[MAX_STRING_LENGTH];

  /* Create an enumerated datatype in HDF5 with a UTF-8 member name*/
  type_id = H5Tcreate(H5T_ENUM, sizeof(c_e1));
  CHECK(type_id, FAIL, "H5Tcreate");
  val = E1_RED;
  ret = H5Tenum_insert(type_id, "RED", &val);
  CHECK(ret, FAIL, "H5Tenum_insert");
  val = E1_GREEN;
  ret = H5Tenum_insert(type_id, "GREEN", &val);
  CHECK(ret, FAIL, "H5Tenum_insert");
  val = E1_BLUE;
  ret = H5Tenum_insert(type_id, "BLUE", &val);
  CHECK(ret, FAIL, "H5Tenum_insert");
  val = E1_WHITE;
  ret = H5Tenum_insert(type_id, string, &val);
  CHECK(ret, FAIL, "H5Tenum_insert");

  /* Ensure that UTF-8 member name gives the right value and vice versa. */
  ret = H5Tenum_valueof(type_id, string, &val);
  CHECK(ret, FAIL, "H5Tenum_valueof");
  VERIFY(val, E1_WHITE, "H5Tenum_valueof");
  ret = H5Tenum_nameof(type_id, &val, readbuf, (size_t)MAX_STRING_LENGTH);
  CHECK(ret, FAIL, "H5Tenum_nameof");
  ret = strcmp(readbuf, string);
  VERIFY(ret, 0, "strcmp");

  /* Close the datatype */
  ret = H5Tclose(type_id);
  CHECK(ret, FAIL, "H5Tclose");
}

/*
 * test_opaque
 * Test comments on opaque datatypes
 */
void test_opaque(hid_t UNUSED fid, const char * string)
{
  hid_t type_id;
  char * read_buf;
  herr_t ret;

  /* Create an opaque type and give it a UTF-8 tag */
  type_id = H5Tcreate(H5T_OPAQUE, (size_t)4);
  CHECK(type_id, FAIL, "H5Tcreate");
  ret = H5Tset_tag(type_id, string);
  CHECK(ret, FAIL, "H5Tset_tag");

  /* Read the tag back. */
  read_buf = H5Tget_tag(type_id);
  ret = strcmp(read_buf, string);
  VERIFY(ret, 0, "H5Tget_tag");
  free(read_buf);

  ret = H5Tclose(type_id);
  CHECK(ret, FAIL, "H5Tclose");
}

/*********************/
/* Utility functions */
/*********************/

/* mkstr
 * Borrwed from dtypes.c.
 * Creates a new string data type.  Used in string padding tests */
static hid_t mkstr(size_t len, H5T_str_t strpad)
{
    hid_t       t;
    if((t = H5Tcopy(H5T_C_S1)) < 0) return -1;
    if(H5Tset_size(t, len) < 0) return -1;
    if(H5Tset_strpad(t, strpad) < 0) return -1;
    return t;
}

/* write_char
 * Append a unicode code point c to test_string in UTF-8 encoding.
 * Return the new end of the string.
 */
unsigned int write_char(unsigned int c, char * test_string, unsigned int cur_pos)
{
  if (c < 0x80) {
    test_string[cur_pos] = c;
    cur_pos++;
  }
  else if (c < 0x800) {
    test_string[cur_pos] = (0xC0 | c>>6);
    test_string[cur_pos+1] = (0x80 | (c & 0x3F));
    cur_pos += 2;
  }
  else if (c < 0x10000) {
    test_string[cur_pos] = (0xE0 | c>>12);
    test_string[cur_pos+1] = (0x80 | (c>>6 & 0x3F));
    test_string[cur_pos+2] = (0x80 | (c & 0x3F));
    cur_pos += 3;
  }
  else if (c < 0x200000) {
    test_string[cur_pos] = (0xF0 | c>>18);
    test_string[cur_pos+1] = (0x80 | (c>>12 & 0x3F));
    test_string[cur_pos+2] = (0x80 | (c>>6 & 0x3F));
    test_string[cur_pos+3] = (0x80 | (c & 0x3F));
    cur_pos += 4;
  }

  return cur_pos;
}

/* dump_string
 * Print a string both as text (which will look like garbage) and as hex.
 * The text display is not guaranteed to be accurate--certain characters
 * could confuse printf (e.g., '\n'). */
void dump_string(const char * string)
{
  unsigned int length;
  unsigned int x;

  printf("The string was:\n %s", string);
  printf("Or in hex:\n");

  length = strlen(string);

  for(x=0; x<length; x++)
    printf("%x ", string[x] & (0x000000FF));

  printf("\n");
}

/* Main test.
 * Create a string of random Unicode characters, then run each test with
 * that string.
 */
void test_unicode(void)
{
  char test_string[MAX_STRING_LENGTH];
  unsigned int cur_pos=0;      /* Current position in test_string */
  unsigned int unicode_point;  /* Unicode code point for a single character */
  hid_t fid;                   /* ID of file */
  int x;                       /* Temporary variable */
  herr_t ret;                  /* Generic return value */

  /* Output message about test being performed */
  MESSAGE(5, ("Testing UTF-8 Encoding\n"));

  /* Create a random string with length NUM_CHARS */
  HDsrandom((unsigned)HDtime(NULL));

  HDmemset(test_string, 0, sizeof(test_string));
  for(x=0; x<NUM_CHARS; x++)
  {
    /* We need to avoid unprintable characters (codes 0-31) and the
     * . and / characters, since they aren't allowed in path names.
     */
    unicode_point = (HDrandom() % (MAX_CODE_POINT-32)) + 32;
    if(unicode_point != 46 && unicode_point != 47)
      cur_pos = write_char(unicode_point, test_string, cur_pos);
  }

  /* Avoid unlikely case of the null string */
  if(cur_pos == 0)
  {
    test_string[cur_pos] = 'Q';
    cur_pos++;
  }
  test_string[cur_pos]='\0';

  /* Create file */
  fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  CHECK(fid, FAIL, "H5Fcreate");

  test_fl_string(fid, test_string);
  test_strpad(fid, "abcdefgh");
  test_strpad(fid, test_string);
  test_vl_string(fid, test_string);
  test_objnames(fid, test_string);
  test_attrname(fid, test_string);
  test_compound(fid, test_string);
  test_enum(fid, test_string);
  test_opaque(fid, test_string);

  /* Close file */
  ret = H5Fclose(fid);
  CHECK(ret, FAIL, "H5Fclose");

  /* This function could be useful in debugging if certain strings
   * create errors.
   */
#ifdef DEBUG
  dump_string(test_string);
#endif /* DEBUG */
}

/* cleanup_unicode(void)
 * Delete the file this test created.
 */
void cleanup_unicode(void)
{
    remove(FILENAME);
}


