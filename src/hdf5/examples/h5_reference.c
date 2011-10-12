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
  *       This program illustrates how references to objects can be used.
  *       Program creates a dataset and a group in a file. It also creates
  *       second dataset, and references to the first dataset and the group
  *       are stored in it.
  *       Program reopens the file and reads dataset with the references.
  *       References are used to open the objects. Information about the
  *       objects is displayed.
  */

#include <stdlib.h>

#include "hdf5.h"

#define H5FILE_NAME "refere.h5"

int
main(void) {
   hid_t fid;                         /* File, group, datasets, datatypes */
   hid_t gid_a;                       /* and  dataspaces identifiers   */
   hid_t did_b, sid_b, tid_b;
   hid_t did_r, tid_r, sid_r;
   H5O_type_t obj_type;
   herr_t status;

   hobj_ref_t *wbuf; /* buffer to write to disk */
   hobj_ref_t *rbuf; /* buffer to read from disk */


   hsize_t dim_r[1];
   hsize_t dim_b[2];

   /*
    *  Create a file using default properties.
    */
   fid = H5Fcreate(H5FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

   /*
    *  Create  group "A" in the file.
    */
   gid_a = H5Gcreate2(fid, "A", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

  /*
   *  Create dataset "B" in the file.
   */
   dim_b[0] = 2;
   dim_b[1] = 6;
   sid_b = H5Screate_simple(2, dim_b, NULL);
   did_b = H5Dcreate2(fid, "B", H5T_NATIVE_FLOAT, sid_b, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

   /*
    *  Create dataset "R" to store references to the objects "A" and "B".
    */
   dim_r[0] = 2;
   sid_r = H5Screate_simple(1, dim_r, NULL);
   tid_r = H5Tcopy(H5T_STD_REF_OBJ);
   did_r = H5Dcreate2(fid, "R", tid_r, sid_r, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

   /*
    *  Allocate write and read buffers.
    */
   wbuf = (hobj_ref_t *)malloc(sizeof(hobj_ref_t) * 2);
   rbuf = (hobj_ref_t *)malloc(sizeof(hobj_ref_t) * 2);

   /*
    *  Create references to the group "A" and dataset "B"
    *  and store them in the wbuf.
    */
   H5Rcreate(&wbuf[0], fid, "A", H5R_OBJECT, -1);
   H5Rcreate(&wbuf[1], fid, "B", H5R_OBJECT, -1);

   /*
    *  Write dataset R using default transfer properties.
    */
   status = H5Dwrite(did_r, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf);

   /*
    *  Close all objects.
    */
   H5Gclose(gid_a);

   H5Sclose(sid_b);
   H5Dclose(did_b);

   H5Tclose(tid_r);
   H5Sclose(sid_r);
   H5Dclose(did_r);

   H5Fclose(fid);

   /*
    * Reopen the file.
    */
   fid = H5Fopen(H5FILE_NAME, H5F_ACC_RDWR, H5P_DEFAULT);

   /*
    *  Open and read dataset "R".
    */
   did_r  = H5Dopen2(fid, "R", H5P_DEFAULT);
   status = H5Dread(did_r, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf);

   /*
    * Find the type of referenced objects.
    */
    status = H5Rget_obj_type2(did_r, H5R_OBJECT, &rbuf[0], &obj_type);
    if(obj_type == H5O_TYPE_GROUP)
        printf("First dereferenced object is a group. \n");

    status = H5Rget_obj_type2(did_r, H5R_OBJECT, &rbuf[1], &obj_type);
    if(obj_type == H5O_TYPE_DATASET)
        printf("Second dereferenced object is a dataset. \n");

   /*
    *  Get datatype of the dataset "B"
    */
   did_b = H5Rdereference(did_r, H5R_OBJECT, &rbuf[1]);
   tid_b = H5Dget_type(did_b);
   if(H5Tequal(tid_b, H5T_NATIVE_FLOAT))
     printf("Datatype of the dataset is H5T_NATIVE_FLOAT.\n");
   printf("\n");

   /*
    * Close all objects and free memory buffers.
    */
   H5Dclose(did_r);
   H5Dclose(did_b);
   H5Tclose(tid_b);
   H5Fclose(fid);
   free(rbuf);
   free(wbuf);

   return 0;
 }

