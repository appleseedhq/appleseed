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

#include "hdf5.h"
#include "hdf5_hl.h"

int main( void )
{
 hid_t       file_id;
 int         data[6];
 hsize_t     dims[2];
 herr_t      status;
 size_t     i, j, nrow, n_values;

 /* open file from ex_lite1.c */
 file_id = H5Fopen ("ex_lite1.h5", H5F_ACC_RDONLY, H5P_DEFAULT);

 /* read dataset */
 status = H5LTread_dataset_int(file_id,"/dset",data);

 /* get the dimensions of the dataset */
 status = H5LTget_dataset_info(file_id,"/dset",dims,NULL,NULL);

 /* print it by rows */
 n_values = (size_t)(dims[0] * dims[1]);
 nrow = (size_t)dims[1];
 for (i=0; i<n_values/nrow; i++ )
 {
  for (j=0; j<nrow; j++)
   printf ("  %d", data[i*nrow + j]);
  printf ("\n");
 }

 /* close file */
 status = H5Fclose (file_id);

 return 0;


}


