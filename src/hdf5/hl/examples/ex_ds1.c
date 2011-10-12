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


#define RANK          2
#define DIM_DATA      12
#define DIM1_SIZE     3
#define DIM2_SIZE     4
#define DIM0          0
#define DIM1          1

#define DSET_NAME      "Mydata"
#define DS_1_NAME      "Yaxis"
#define DS_2_NAME      "Xaxis"

int main(void)
{

 hid_t   fid;                                              /* file ID */
 hid_t   did;                                              /* dataset ID */
 hid_t   dsid;                                             /* DS dataset ID */
 int     rank     = RANK;                                  /* rank of data dataset */
 int     rankds   = 1;                                     /* rank of DS dataset */
 hsize_t dims[RANK]  = {DIM1_SIZE,DIM2_SIZE};              /* size of data dataset */
 int     buf[DIM_DATA] = {1,2,3,4,5,6,7,8,9,10,11,12};     /* data of data dataset */
 hsize_t s1_dim[1]  = {DIM1_SIZE};                         /* size of DS 1 dataset */
 hsize_t s2_dim[1]  = {DIM2_SIZE};                         /* size of DS 2 dataset */
 float   s1_wbuf[DIM1_SIZE] = {10,20,30};                  /* data of DS 1 dataset */
 int     s2_wbuf[DIM2_SIZE] = {10,20,50,100};              /* data of DS 2 dataset */


 /* create a file using default properties */
 if ((fid=H5Fcreate("ex_ds1.h5",H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
  goto out;

 /* make a dataset */
 if (H5LTmake_dataset_int(fid,DSET_NAME,rank,dims,buf)<0)
  goto out;

 /* make a DS dataset for the first dimension */
 if (H5LTmake_dataset_float(fid,DS_1_NAME,rankds,s1_dim,s1_wbuf)<0)
  goto out;

 /* make a DS dataset for the second dimension */
 if (H5LTmake_dataset_int(fid,DS_2_NAME,rankds,s2_dim,s2_wbuf)<0)
  goto out;


/*-------------------------------------------------------------------------
 * attach the DS_1_NAME dimension scale to DSET_NAME at dimension 0
 *-------------------------------------------------------------------------
 */

 /* get the dataset id for DSET_NAME */
 if ((did = H5Dopen2(fid,DSET_NAME, H5P_DEFAULT))<0)
  goto out;

 /* get the DS dataset id */
 if ((dsid = H5Dopen2(fid,DS_1_NAME, H5P_DEFAULT))<0)
  goto out;

 /* attach the DS_1_NAME dimension scale to DSET_NAME at dimension index 0 */
 if (H5DSattach_scale(did,dsid,DIM0)<0)
  goto out;

 /* close DS id */
 if (H5Dclose(dsid)<0)
  goto out;

/*-------------------------------------------------------------------------
 * attach the DS_2_NAME dimension scale to DSET_NAME
 *-------------------------------------------------------------------------
 */

 /* get the DS dataset id */
 if ((dsid = H5Dopen2(fid,DS_2_NAME, H5P_DEFAULT))<0)
  goto out;

 /* attach the DS_2_NAME dimension scale to DSET_NAME as the 2nd dimension (index 1)  */
 if (H5DSattach_scale(did,dsid,DIM1)<0)
  goto out;

 /* close DS id */
 if (H5Dclose(dsid)<0)
  goto out;

 /* close file */
 H5Fclose(fid);

 return 0;

out:
 printf("Error on return function...Exiting\n");
 return 1;

}



