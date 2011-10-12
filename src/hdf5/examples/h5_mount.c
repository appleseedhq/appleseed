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
 * This program shows the concept of "mounting files".
 * Program creates one file with group G in it, and another
 * file with dataset D. Then second file is mounted in the first one
 * under the "mounting point" G. Dataset D is accessed in the first file
 * under name /G/D and data is printed out.
 */

#include "hdf5.h"

#define FILE1 "mount1.h5"
#define FILE2 "mount2.h5"

#define RANK 2
#define NX 4
#define NY 5

int main(void)
{

   hid_t fid1, fid2, gid;  /* Files and group identifiers */
   hid_t did, tid, sid;    /* Dataset and datatype identifiers */

   herr_t status;
   hsize_t dims[] = {NX,NY}; /* Dataset dimensions */

   int i, j;
   int bm[NX][NY], bm_out[NX][NY]; /* Data buffers */

   /*
    * Initialization of buffer matrix "bm"
    */
   for(i =0; i < NX; i++)
    for(j = 0; j < NY; j++)
      bm[i][j] = i + j;

   /*
    * Create first file and a group in it.
    */
   fid1 = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
   gid = H5Gcreate2(fid1, "/G", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

   /*
    * Close group and file
    */
   H5Gclose(gid);
   H5Fclose(fid1);

   /*
    * Create second file and dataset "D" in it.
    */
   fid2 = H5Fcreate(FILE2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
   dims[0] = NX;
   dims[1] = NY;
   sid = H5Screate_simple(RANK, dims, NULL);
   did = H5Dcreate2(fid2, "D", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

   /*
    * Write data to the dataset.
    */
   status = H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, bm);

   /*
    * Close all identifiers.
    */
   H5Sclose(sid);
   H5Dclose(did);
   H5Fclose(fid2);

   /*
    * Reopen both files
    */
   fid1 = H5Fopen(FILE1, H5F_ACC_RDONLY, H5P_DEFAULT);
   fid2 = H5Fopen(FILE2, H5F_ACC_RDONLY, H5P_DEFAULT);

   /*
    * Mount second file under G in the first file.
    */
   H5Fmount(fid1, "/G", fid2, H5P_DEFAULT);

   /*
    * Access dataset D in the first file under /G/D name.
    */
   did = H5Dopen2(fid1, "/G/D", H5P_DEFAULT);
   tid = H5Dget_type(did);
   status = H5Dread(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, bm_out);

   /*
    * Print out the data.
    */
   for(i=0; i<NX; i++){
    for(j=0; j<NY; j++)
        printf("  %d", bm_out[i][j]);
	printf("\n");
   }

   /*
    * Close all identifers
    */
   H5Tclose(tid);
   H5Dclose(did);

   /*
    * Unmounting second file
    */
   H5Funmount(fid1, "/G");

   /*
    * Close both files
    */
   H5Fclose(fid1);
   H5Fclose(fid2);

   return 0;
}



