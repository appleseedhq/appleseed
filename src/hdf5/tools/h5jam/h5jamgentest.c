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
 * Generate the binary hdf5 files and user block data for the jam/unjam tests.
 * Usage: just execute the program without any arguments will
 * generate all the files in the local directory.
 *
 * If you regenerate the test files (e.g., changing some code,
 * trying it on a new platform, ...), you need to verify the correctness
 * of the expected output and update the corresponding *.ddl files.
 */
#include <assert.h>
#include <limits.h>

#include "hdf5.h"
#include "H5private.h"

#define UBTXT1 "u0.txt"
#define UBTXT2 "u10.txt"
#define UBTXT3 "u511.txt"
#define UBTXT4 "u512.txt"
#define UBTXT5 "u513.txt"
/* not used yet
#define UBTXT6 "u1023.txt"
#define UBTXT7 "u1024.txt"
#define UBTXT8 "u1025.txt"
#define UBTXT9 "u2047.txt"
#define UBTXT10 "u2048.txt"
#define UBTXT11 "u2049.txt"
#define UBBIN1 "u0.dat"
#define UBBIN2 "u10.dat"
#define UBBIN3 "u511.dat"
#define UBBIN4 "u512.dat"
#define UBBIN5 "u513.dat"
*/

/* not used yet
#define FILE1 "tnull.h5"
#define FILE2 "tnullwithub.h5"
*/
/* tall is same as dumper test */
#define FILE7 "tall.h5"
#define FILE8 "twithub.h5"
#define FILE9 "twithub513.h5"

/*
 * This pattern is used to fill text files
 */
char pattern[11] = "abcdefghij";

/*-------------------------------------------------------------------------
 * prototypes
 *-------------------------------------------------------------------------
 */


#define BUF_SIZE 1024

#define LENSTR  50
#define LENSTR2  11

#define SPACE2_RANK 2
#define SPACE2_DIM1 10
#define SPACE2_DIM2 10

#define SPACE1_RANK 1
#define SPACE1_DIM1 4

#define DIM1  20
#define DIM2  10
#define CDIM1 DIM1/2
#define CDIM2 DIM2/2
#define RANK  2

/* Element selection information */
#define POINT1_NPOINTS 10

typedef enum{
     RED,
     GREEN,
     BLUE,
     WHITE,
     BLACK
} enumtype;

/* Compound datatype */
typedef struct s1_t {
    unsigned int a;
    unsigned int b;
    float c;
} s1_t;


/* 1-D array datatype */
#define ARRAY1_RANK 1
#define ARRAY1_DIM1 4

/* 3-D array datatype */
#define ARRAY2_RANK 3
#define ARRAY2_DIM1 3
#define ARRAY2_DIM2 4
#define ARRAY2_DIM3 5

/* 2-D array datatype */
#define ARRAY3_RANK 2
#define ARRAY3_DIM1 6
#define ARRAY3_DIM2 3

/* VL string datatype name */
#define VLSTR_TYPE      "vl_string_type"

/* A UD link traversal function.  Shouldn't actually be called. */
static hid_t UD_traverse(const char UNUSED * link_name, hid_t UNUSED cur_group,
    const void UNUSED * udata, size_t UNUSED udata_size, hid_t UNUSED lapl_id)
{
    return -1;
}

#define MY_LINKCLASS 187
const H5L_class_t UD_link_class[1] = {{
    H5L_LINK_CLASS_T_VERS,    /* H5L_class_t version       */
    (H5L_type_t)MY_LINKCLASS, /* Link type id number            */
    "UD link class",          /* name for debugging             */
    NULL,                     /* Creation callback              */
    NULL,                     /* Move/rename callback           */
    NULL,                     /* Copy callback                  */
    UD_traverse,              /* The actual traversal function  */
    NULL,                     /* Deletion callback              */
    NULL                      /* Query callback                 */
}};



/* gent_ub
    with no ub, identical to gent_all from h5dumpgentest.c

    FILENAME is the name of the file to create
    UB_SIZE is the size the buffer should be
    UB_FILL characters will be set to the PATTERN array,
        the rest of the user block will be NULL.

/ : g1  g2  attr1  attr2
g1 : g1.1  g1.2
g1.1 : dset1.1.1(attr1, attr2)   dset1.1.2
g1.2 : g1.2.1 extlink
g1.2.1 : slink
g2 : dset2.1  dset2.2 udlink

*/

static void
gent_ub(const char * filename, size_t ub_size, size_t ub_fill)
{
    hid_t fid, group, attr, dataset, space;
    hid_t create_plist;
    hsize_t dims[2];
    int data[2][2], dset1[10][10], dset2[20];
    char buf[BUF_SIZE];
    int i, j;
    size_t u;
    float dset2_1[10], dset2_2[3][5];
    int fd;
    char *bp;

  if(ub_size > 0)
  {
      create_plist = H5Pcreate(H5P_FILE_CREATE);
      H5Pset_userblock(create_plist, (hsize_t)ub_size);
      fid = H5Fcreate(filename, H5F_ACC_TRUNC, create_plist, H5P_DEFAULT);
  }
  else
  {
      fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  }

  /* create groups */
  group = H5Gcreate2(fid, "/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  H5Gclose(group);

  group = H5Gcreate2(fid, "/g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  H5Gclose(group);

  group = H5Gcreate2(fid, "/g1/g1.1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  H5Gclose(group);

  group = H5Gcreate2(fid, "/g1/g1.2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  H5Gclose(group);

  group = H5Gcreate2(fid, "/g1/g1.2/g1.2.1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  H5Gclose(group);

  /* root attributes */
  group = H5Gopen2(fid, "/", H5P_DEFAULT);

  dims[0] = 10;
  space = H5Screate_simple(1, dims, NULL);
  attr = H5Acreate2(group, "attr1", H5T_STD_I8BE, space, H5P_DEFAULT, H5P_DEFAULT);
  sprintf(buf, "abcdefghi");
  H5Awrite(attr, H5T_NATIVE_SCHAR, buf);
  H5Sclose(space);
  H5Aclose(attr);

  dims[0] = 2; dims[1] = 2;
  space = H5Screate_simple(2, dims, NULL);
  attr = H5Acreate2(group, "attr2", H5T_STD_I32BE, space, H5P_DEFAULT, H5P_DEFAULT);
  data[0][0] = 0; data[0][1] = 1; data[1][0] = 2; data[1][1] = 3;
  H5Awrite(attr, H5T_NATIVE_INT, data);
  H5Sclose(space);
  H5Aclose(attr);

  H5Gclose(group);

  group = H5Gopen2(fid, "/g1/g1.1", H5P_DEFAULT);

  /* dset1.1.1 */
  dims[0] = 10; dims[1] = 10;
  space = H5Screate_simple(2, dims, NULL);
  dataset = H5Dcreate2(group, "dset1.1.1", H5T_STD_I32BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  for (i = 0; i < 10; i++)
       for (j = 0; j < 10; j++)
            dset1[i][j] = j*i;
  H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1);
  H5Sclose(space);

  /* attributes of dset1.1.1 */
  dims[0] = 27;
  space = H5Screate_simple(1, dims, NULL);
  attr = H5Acreate2(dataset, "attr1", H5T_STD_I8BE, space, H5P_DEFAULT, H5P_DEFAULT);
  sprintf(buf, "1st attribute of dset1.1.1");
  H5Awrite(attr, H5T_NATIVE_SCHAR, buf);
  H5Sclose(space);
  H5Aclose(attr);

  dims[0] = 27;
  space = H5Screate_simple(1, dims, NULL);
  attr = H5Acreate2(dataset, "attr2", H5T_STD_I8BE, space, H5P_DEFAULT, H5P_DEFAULT);
  sprintf(buf, "2nd attribute of dset1.1.1");
  H5Awrite(attr, H5T_NATIVE_SCHAR, buf);
  H5Sclose(space);
  H5Aclose(attr);

  H5Dclose(dataset);

  /* dset1.1.2 */
  dims[0] = 20;
  space = H5Screate_simple(1, dims, NULL);
  dataset = H5Dcreate2(group, "dset1.1.2", H5T_STD_I32BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  for (i = 0; i < 20; i++)
       dset2[i] = i;
  H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2);
  H5Sclose(space);
  H5Dclose(dataset);

  H5Gclose(group);

  /* external link */
  H5Lcreate_external("somefile", "somepath", fid, "/g1/g1.2/extlink", H5P_DEFAULT, H5P_DEFAULT);

  /* soft link */
  group = H5Gopen2(fid, "/g1/g1.2/g1.2.1", H5P_DEFAULT);
  H5Lcreate_soft("somevalue", group, "slink", H5P_DEFAULT, H5P_DEFAULT);
  H5Gclose(group);

  group = H5Gopen2(fid, "/g2", H5P_DEFAULT);

  /* dset2.1 */
  dims[0] = 10;
  space = H5Screate_simple(1, dims, NULL);
  dataset = H5Dcreate2(group, "dset2.1", H5T_IEEE_F32BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  for (i = 0; i < 10; i++)
       dset2_1[i] = (float)(i*0.1+1);
  H5Dwrite(dataset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2_1);
  H5Sclose(space);
  H5Dclose(dataset);

  /* dset2.2 */
  dims[0] = 3; dims[1] = 5;
  space = H5Screate_simple(2, dims, NULL);
  dataset = H5Dcreate2(group, "dset2.2", H5T_IEEE_F32BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  for (i = 0; i < 3; i++)
       for (j = 0; j < 5; j++)
            dset2_2[i][j] = (float)((i+1)*j*0.1);
  H5Dwrite(dataset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2_2);
  H5Sclose(space);
  H5Dclose(dataset);

  H5Gclose(group);

  /* user-defined link */
  H5Lregister(UD_link_class);
  H5Lcreate_ud(fid, "/g2/udlink", (H5L_type_t)MY_LINKCLASS, NULL, (size_t)0, H5P_DEFAULT, H5P_DEFAULT);

  H5Fclose(fid);

  /* If a user block is being used, write to it here */
  if(ub_size > 0)
  {
        HDassert(ub_size <= BUF_SIZE);

	fd = HDopen(filename, O_RDWR, 0);
        HDassert(fd >= 0);

	/* fill buf with pattern */
	HDmemset(buf, '\0', ub_size);
	bp = buf;
	for (u = 0; u < ub_fill; u++)
            *bp++ = pattern[u % 10];

	HDwrite(fd, buf, ub_size);

	HDclose(fd);
  }
}

static void
create_textfile(const char *name, size_t size)
{
    char *buf;
    int fd;
    size_t i;
    char *bp;

    fd = HDcreat(name,0777);
    assert(fd >= 0);
    buf = calloc(size, (size_t)1);
    assert(buf);

    /* fill buf with pattern */
    bp = buf;
    for(i = 0; i < size; i++)
        *bp++ = pattern[i % 10];

    HDwrite(fd, buf, size);

    free(buf);

    HDclose(fd);
}

#ifdef notdef
/* not used yet */
void
create_binfile(char *name, off_t size)
{
    char *buf;
    int fd;
    int i;
    char *bp;

    fd = creat(name,0777);
    HDassert(fd >= 0);

    buf = calloc(size,1);
    HDassert(buf);

    /* fill buf with pattern */
    bp = buf;
    for (i = 0; i < size; i++)
        *bp++ = (char) i & 0xff;

    HDwrite(fd,buf,size);

    HDclose(fd);
}
#endif

/*-------------------------------------------------------------------------
 * Function: main
 *
 *-------------------------------------------------------------------------
 */


int main(void)
{

/*
create_textfile(UBTXT1, (size_t)0);
*/
create_textfile(UBTXT2, (size_t)10);
create_textfile(UBTXT3, (size_t)511);
create_textfile(UBTXT4, (size_t)512);
create_textfile(UBTXT5, (size_t)513);
/*
create_textfile(UBTXT6, (size_t)1023);
create_textfile(UBTXT7, (size_t)1024);
create_textfile(UBTXT8, (size_t)1025);
create_textfile(UBTXT9, (size_t)2047);
create_textfile(UBTXT10, (size_t)2048);
create_textfile(UBTXT11, (size_t)2049);

create_binfile(UBBIN1, (off_t)0);
create_binfile(UBBIN2, (off_t)10);
create_binfile(UBBIN3, (off_t)511);
create_binfile(UBBIN4, (off_t)512);
create_binfile(UBBIN5, (off_t)513);

*/
    gent_ub(FILE7, (size_t)0, (size_t)0);
    gent_ub(FILE8, (size_t)512, HDstrlen(pattern));
    gent_ub(FILE9, (size_t)1024, (size_t)513);

    return 0;
}
