
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
   This program will test irregular hyperslab selections with collective write and read.
   The way to test whether collective write and read works is to use independent IO
   output to verify the collective output.

   1) We will write two datasets with the same hyperslab selection settings;
   one in independent mode,
   one in collective mode,
   2) We will read two datasets with the same hyperslab selection settings,
      1.  independent read to read independent output,
          independent read to read collecive   output,
	  Compare the result,
	  If the result is the same, then collective write succeeds.
      2.  collective read to read independent output,
          independent read to read independent output,
	  Compare the result,
	  If the result is the same, then collective read succeeds.

 */

#include "hdf5.h"
#include "H5private.h"
#include "testphdf5.h"


static void coll_write_test(int chunk_factor);
static void coll_read_test(int chunk_factor);


/*-------------------------------------------------------------------------
 * Function:	coll_irregular_cont_write
 *
 * Purpose:	Wrapper to test the collectively irregular hyperslab write in
                contiguous storage
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Unknown
 *		Dec 2nd, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
coll_irregular_cont_write(void)
{

  coll_write_test(0);

}



/*-------------------------------------------------------------------------
 * Function:	coll_irregular_cont_read
 *
 * Purpose:	Wrapper to test the collectively irregular hyperslab read in
                contiguous storage
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Unknown
 *		Dec 2nd, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
coll_irregular_cont_read(void)
{

  coll_read_test(0);

}


/*-------------------------------------------------------------------------
 * Function:	coll_irregular_simple_chunk_write
 *
 * Purpose:	Wrapper to test the collectively irregular hyperslab write in
                chunk storage(1 chunk)
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Unknown
 *		Dec 2nd, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
coll_irregular_simple_chunk_write(void)
{

  coll_write_test(1);

}



/*-------------------------------------------------------------------------
 * Function:	coll_irregular_simple_chunk_read
 *
 * Purpose:	Wrapper to test the collectively irregular hyperslab read in chunk
                storage(1 chunk)
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Unknown
 *		Dec 2nd, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
coll_irregular_simple_chunk_read(void)
{

  coll_read_test(1);

}

/*-------------------------------------------------------------------------
 * Function:	coll_irregular_complex_chunk_write
 *
 * Purpose:	Wrapper to test the collectively irregular hyperslab write in chunk
                storage(4 chunks)
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Unknown
 *		Dec 2nd, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
coll_irregular_complex_chunk_write(void)
{

  coll_write_test(4);

}



/*-------------------------------------------------------------------------
 * Function:	coll_irregular_complex_chunk_read
 *
 * Purpose:	Wrapper to test the collectively irregular hyperslab read in chunk
                storage(1 chunk)
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Unknown
 *		Dec 2nd, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
coll_irregular_complex_chunk_read(void)
{

  coll_read_test(4);

}


/*-------------------------------------------------------------------------
 * Function:	coll_write_test
 *
 * Purpose:	To test the collectively irregular hyperslab write in chunk
                storage
 *  Input:      number of chunks on each dimension
                if number is equal to 0, contiguous storage
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Unknown
 *		Dec 2nd, 2004
 *
 * Modifications: Oct 18th, 2005
 *
 *-------------------------------------------------------------------------
 */
void coll_write_test(int chunk_factor)
{

  const    char *filename;
  hid_t    facc_plist,dxfer_plist,dcrt_plist;
  hid_t    file, datasetc,dataseti;      /* File and dataset identifiers */
  hid_t    mspaceid1, mspaceid, fspaceid,fspaceid1; /* Dataspace identifiers */

  hsize_t mdim1[1],fsdim[2],mdim[2];

#if 0
  hsize_t  mdim1[] = {MSPACE1_DIM};  /* Dimension size of the first dataset
				      (in memory) */
  hsize_t  fsdim[] = {FSPACE_DIM1, FSPACE_DIM2}; /* Dimension sizes of the dataset
                                                    (on disk) */

  hsize_t  mdim[] = {MSPACE_DIM1, MSPACE_DIM2}; /* Dimension sizes of the
						  dataset in memory when we
						  read selection from the
						  dataset on the disk */
#endif

  hsize_t  start[2];  /* Start of hyperslab */
  hsize_t  stride[2]; /* Stride of hyperslab */
  hsize_t  count[2];  /* Block count */
  hsize_t  block[2];  /* Block sizes */
  hsize_t  chunk_dims[2];

  herr_t   ret;
  unsigned i;
  int      fillvalue = 0;   /* Fill value for the dataset */

#if 0
  int      matrix_out[MSPACE_DIM1][MSPACE_DIM2];
  int      matrix_out1[MSPACE_DIM1][MSPACE_DIM2];   /* Buffer to read from the
						       dataset */
  int      vector[MSPACE1_DIM];
#endif


  int      *matrix_out, *matrix_out1, *vector;

  hbool_t  use_gpfs = FALSE;
  int      mpi_size,mpi_rank;

  MPI_Comm comm = MPI_COMM_WORLD;
  MPI_Info info = MPI_INFO_NULL;

  /*set up MPI parameters */
  MPI_Comm_size(comm,&mpi_size);
  MPI_Comm_rank(comm,&mpi_rank);

  /* Obtain file name */
  filename = GetTestParameters();

  /*
   * Buffers' initialization.
   */

  mdim1[0] = MSPACE1_DIM *mpi_size;
  mdim[0]  = MSPACE_DIM1;
  mdim[1]  = MSPACE_DIM2*mpi_size;
  fsdim[0] = FSPACE_DIM1;
  fsdim[1] = FSPACE_DIM2*mpi_size;

  vector = (int*)HDmalloc(sizeof(int)*mdim1[0]*mpi_size);
  matrix_out  = (int*)HDmalloc(sizeof(int)*mdim[0]*mdim[1]*mpi_size);
  matrix_out1 = (int*)HDmalloc(sizeof(int)*mdim[0]*mdim[1]*mpi_size);

  HDmemset(vector,0,sizeof(int)*mdim1[0]*mpi_size);
  vector[0] = vector[MSPACE1_DIM*mpi_size - 1] = -1;
  for (i = 1; i < MSPACE1_DIM*mpi_size - 1; i++) vector[i] = i;

  /* Grab file access property list */
  facc_plist = create_faccess_plist(comm, info, facc_type, use_gpfs);
  VRFY((facc_plist >= 0),"");

  /*
   * Create a file.
   */
  file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, facc_plist);
  VRFY((file >= 0),"H5Fcreate succeeded");

  /*
   * Create property list for a dataset and set up fill values.
   */
  dcrt_plist = H5Pcreate(H5P_DATASET_CREATE);
  VRFY((dcrt_plist >= 0),"");

  ret   = H5Pset_fill_value(dcrt_plist, H5T_NATIVE_INT, &fillvalue);
  VRFY((ret >= 0),"Fill value creation property list succeeded");

  if(chunk_factor != 0) {
    chunk_dims[0] = fsdim[0] / chunk_factor;
    chunk_dims[1] = fsdim[1] / chunk_factor;
    ret = H5Pset_chunk(dcrt_plist, 2, chunk_dims);
    VRFY((ret >= 0),"chunk creation property list succeeded");
  }

  /*
   *
   * Create dataspace for the first dataset in the disk.
   * dim1 = 9
   * dim2 = 3600
   *
   *
   */
  fspaceid = H5Screate_simple(FSPACE_RANK, fsdim, NULL);
  VRFY((fspaceid >= 0),"file dataspace created succeeded");

  /*
   * Create dataset in the file. Notice that creation
   * property list dcrt_plist is used.
   */
  datasetc = H5Dcreate2(file, "collect_write", H5T_NATIVE_INT, fspaceid, H5P_DEFAULT, dcrt_plist, H5P_DEFAULT);
  VRFY((datasetc >= 0),"dataset created succeeded");

  dataseti = H5Dcreate2(file, "independ_write", H5T_NATIVE_INT, fspaceid, H5P_DEFAULT, dcrt_plist, H5P_DEFAULT);
  VRFY((dataseti >= 0),"dataset created succeeded");

  /* The First selection for FILE
   *
   *  block (3,2)
   *  stride(4,3)
   *  count (1,768/mpi_size)
   *  start (0,1+768*3*mpi_rank/mpi_size)
   *
   */

  start[0]  = FHSTART0;
  start[1]  = FHSTART1 + mpi_rank * FHSTRIDE1 * FHCOUNT1;
  stride[0] = FHSTRIDE0;
  stride[1] = FHSTRIDE1;
  count[0]  = FHCOUNT0;
  count[1]  = FHCOUNT1;
  block[0]  = FHBLOCK0;
  block[1]  = FHBLOCK1;

  ret = H5Sselect_hyperslab(fspaceid, H5S_SELECT_SET, start, stride, count, block);
  VRFY((ret >= 0),"hyperslab selection succeeded");

  /* The Second selection for FILE
   *
   *  block  (3,768)
   *  stride (1,1)
   *  count  (1,1)
   *  start  (4,768*mpi_rank/mpi_size)
   *
   */

  start[0]  = SHSTART0;
  start[1]  = SHSTART1+SHCOUNT1*SHBLOCK1*mpi_rank;
  stride[0] = SHSTRIDE0;
  stride[1] = SHSTRIDE1;
  count[0]  = SHCOUNT0;
  count[1]  = SHCOUNT1;
  block[0]  = SHBLOCK0;
  block[1]  = SHBLOCK1;

  ret = H5Sselect_hyperslab(fspaceid, H5S_SELECT_OR, start, stride, count, block);
  VRFY((ret >= 0),"hyperslab selection succeeded");

  /*
   * Create dataspace for the first dataset in the memory
   * dim1 = 27000
   *
   */
  mspaceid1 = H5Screate_simple(MSPACE1_RANK, mdim1, NULL);
  VRFY((mspaceid1 >= 0),"memory dataspace created succeeded");

  /*
   * Memory space is 1-D, this is a good test to check
   * whether a span-tree derived datatype needs to be built.
   * block  1
   * stride 1
   * count  6912/mpi_size
   * start  1
   *
   */
  start[0]  = MHSTART0;
  stride[0] = MHSTRIDE0;
  count[0]  = MHCOUNT0;
  block[0]  = MHBLOCK0;

  ret = H5Sselect_hyperslab(mspaceid1, H5S_SELECT_SET, start, stride, count, block);
  VRFY((ret >= 0),"hyperslab selection succeeded");

  /* independent write */
  ret = H5Dwrite(dataseti, H5T_NATIVE_INT, mspaceid1, fspaceid, H5P_DEFAULT, vector);
  VRFY((ret >= 0),"dataset independent write succeed");

  dxfer_plist = H5Pcreate(H5P_DATASET_XFER);
  VRFY((dxfer_plist >= 0),"");

  ret = H5Pset_dxpl_mpio(dxfer_plist, H5FD_MPIO_COLLECTIVE);
  VRFY((ret >= 0),"MPIO data transfer property list succeed");
  if(dxfer_coll_type == DXFER_INDEPENDENT_IO) {
     ret = H5Pset_dxpl_mpio_collective_opt(dxfer_plist,H5FD_MPIO_INDIVIDUAL_IO);
     VRFY((ret>= 0),"set independent IO collectively succeeded");
  }


  /* collective write */
  ret = H5Dwrite(datasetc, H5T_NATIVE_INT, mspaceid1, fspaceid, dxfer_plist, vector);
  VRFY((ret >= 0),"dataset collective write succeed");

  ret = H5Sclose(mspaceid1);
  VRFY((ret >= 0),"");

  ret = H5Sclose(fspaceid);
  VRFY((ret >= 0),"");

  /*
   * Close dataset.
   */
  ret = H5Dclose(datasetc);
  VRFY((ret >= 0),"");

  ret = H5Dclose(dataseti);
  VRFY((ret >= 0),"");

  /*
   * Close the file.
   */
  ret = H5Fclose(file);
  VRFY((ret >= 0),"");
  /*
   * Close property list
   */

  ret = H5Pclose(facc_plist);
  VRFY((ret >= 0),"");
  ret = H5Pclose(dxfer_plist);
  VRFY((ret >= 0),"");
  ret = H5Pclose(dcrt_plist);
  VRFY((ret >= 0),"");

  /*
   * Open the file.
   */

  /***

       For testing collective hyperslab selection write
       In this test, we are using independent read to check
       the correctedness of collective write compared with
       independent write,

       In order to throughly test this feature, we choose
       a different selection set for reading the data out.


  ***/

  /* Obtain file access property list with MPI-IO driver */
  facc_plist = create_faccess_plist(comm, info, facc_type, use_gpfs);
  VRFY((facc_plist >= 0),"");

  file = H5Fopen(filename, H5F_ACC_RDONLY, facc_plist);
  VRFY((file >= 0),"H5Fopen succeeded");

  /*
   * Open the dataset.
   */
  datasetc = H5Dopen2(file,"collect_write", H5P_DEFAULT);
  VRFY((datasetc >= 0),"H5Dopen2 succeeded");

  dataseti = H5Dopen2(file,"independ_write", H5P_DEFAULT);
  VRFY((dataseti >= 0),"H5Dopen2 succeeded");

  /*
   * Get dataspace of the open dataset.
   */
  fspaceid  = H5Dget_space(datasetc);
  VRFY((fspaceid >= 0),"file dataspace obtained succeeded");

  fspaceid1 = H5Dget_space(dataseti);
  VRFY((fspaceid1 >= 0),"file dataspace obtained succeeded");


  /* The First selection for FILE to read
   *
   *  block (1,1)
   *  stride(1.1)
   *  count (3,768/mpi_size)
   *  start (1,2+768*mpi_rank/mpi_size)
   *
   */
  start[0]  = RFFHSTART0;
  start[1]  = RFFHSTART1+mpi_rank*RFFHCOUNT1;
  block[0]  = RFFHBLOCK0;
  block[1]  = RFFHBLOCK1;
  stride[0] = RFFHSTRIDE0;
  stride[1] = RFFHSTRIDE1;
  count[0]  = RFFHCOUNT0;
  count[1]  = RFFHCOUNT1;


  /* The first selection of the dataset generated by collective write */
  ret = H5Sselect_hyperslab(fspaceid, H5S_SELECT_SET, start, stride, count, block);
  VRFY((ret >= 0),"hyperslab selection succeeded");

  /* The first selection of the dataset generated by independent write */
  ret = H5Sselect_hyperslab(fspaceid1, H5S_SELECT_SET, start, stride, count, block);
  VRFY((ret >= 0),"hyperslab selection succeeded");

  /* The Second selection for FILE to read
   *
   *  block (1,1)
   *  stride(1.1)
   *  count (3,1536/mpi_size)
   *  start (2,4+1536*mpi_rank/mpi_size)
   *
   */

  start[0] = RFSHSTART0;
  start[1] = RFSHSTART1+RFSHCOUNT1*mpi_rank;
  block[0] = RFSHBLOCK0;
  block[1] = RFSHBLOCK1;
  stride[0] = RFSHSTRIDE0;
  stride[1] = RFSHSTRIDE0;
  count[0]  = RFSHCOUNT0;
  count[1]  = RFSHCOUNT1;

  /* The second selection of the dataset generated by collective write */
  ret = H5Sselect_hyperslab(fspaceid, H5S_SELECT_OR, start, stride, count, block);
  VRFY((ret >= 0),"hyperslab selection succeeded");

  /* The second selection of the dataset generated by independent write */
  ret = H5Sselect_hyperslab(fspaceid1, H5S_SELECT_OR, start, stride, count, block);
  VRFY((ret >= 0),"hyperslab selection succeeded");

  /*
   * Create memory dataspace.
   * rank  = 2
   * mdim1 = 9
   * mdim2 = 3600
   *
   */
  mspaceid = H5Screate_simple(MSPACE_RANK, mdim, NULL);

  /*
   * Select two hyperslabs in memory. Hyperslabs has the same
   * size and shape as the selected hyperslabs for the file dataspace
   * Only the starting point is different.
   * The first selection
   *  block (1,1)
   *  stride(1.1)
   *  count (3,768/mpi_size)
   *  start (0,768*mpi_rank/mpi_size)
   *
   */


  start[0]  = RMFHSTART0;
  start[1]  = RMFHSTART1+mpi_rank*RMFHCOUNT1;
  block[0]  = RMFHBLOCK0;
  block[1]  = RMFHBLOCK1;
  stride[0] = RMFHSTRIDE0;
  stride[1] = RMFHSTRIDE1;
  count[0]  = RMFHCOUNT0;
  count[1]  = RMFHCOUNT1;

  ret = H5Sselect_hyperslab(mspaceid, H5S_SELECT_SET, start, stride, count, block);
  VRFY((ret >= 0),"hyperslab selection succeeded");

  /*
   * Select two hyperslabs in memory. Hyperslabs has the same
   * size and shape as the selected hyperslabs for the file dataspace
   * Only the starting point is different.
   * The second selection
   *  block (1,1)
   *  stride(1,1)
   *  count (3,1536/mpi_size)
   *  start (1,2+1536*mpi_rank/mpi_size)
   *
   */
  start[0]  = RMSHSTART0;
  start[1]  = RMSHSTART1+mpi_rank*RMSHCOUNT1;
  block[0]  = RMSHBLOCK0;
  block[1]  = RMSHBLOCK1;
  stride[0] = RMSHSTRIDE0;
  stride[1] = RMSHSTRIDE1;
  count[0]  = RMSHCOUNT0;
  count[1]  = RMSHCOUNT1;

  ret = H5Sselect_hyperslab(mspaceid, H5S_SELECT_OR, start, stride, count, block);
  VRFY((ret >= 0),"hyperslab selection succeeded");

  /*
   * Initialize data buffer.
   */

  HDmemset(matrix_out,0,sizeof(int)*MSPACE_DIM1*MSPACE_DIM2*mpi_size);
  HDmemset(matrix_out1,0,sizeof(int)*MSPACE_DIM1*MSPACE_DIM2*mpi_size);
  /*
   * Read data back to the buffer matrix_out.
   */

  ret = H5Dread(datasetc, H5T_NATIVE_INT, mspaceid, fspaceid,
		H5P_DEFAULT, matrix_out);
  VRFY((ret >= 0),"H5D independent read succeed");


  ret = H5Dread(dataseti, H5T_NATIVE_INT, mspaceid, fspaceid,
		H5P_DEFAULT, matrix_out1);
  VRFY((ret >= 0),"H5D independent read succeed");

  ret = 0;

  for (i = 0; i < MSPACE_DIM1*MSPACE_DIM2*mpi_size; i++){
         if(matrix_out[i]!=matrix_out1[i]) ret = -1;
      if(ret < 0) break;
    }

  VRFY((ret >= 0),"H5D irregular collective write succeed");

  /*
   * Close memory file and memory dataspaces.
   */
  ret = H5Sclose(mspaceid);
  VRFY((ret >= 0),"");
  ret = H5Sclose(fspaceid);
  VRFY((ret >= 0),"");

  /*
   * Close dataset.
   */
  ret = H5Dclose(dataseti);
  VRFY((ret >= 0),"");

  ret = H5Dclose(datasetc);
  VRFY((ret >= 0),"");

  /*
   * Close property list
   */

  ret = H5Pclose(facc_plist);
  VRFY((ret >= 0),"");


  /*
   * Close the file.
   */
  ret = H5Fclose(file);
  VRFY((ret >= 0),"");

  return ;
}

/*-------------------------------------------------------------------------
 * Function:	coll_read_test
 *
 * Purpose:	To test the collectively irregular hyperslab read in chunk
                storage
 * Input:       number of chunks on each dimension
                if number is equal to 0, contiguous storage
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Unknown
 *		Dec 2nd, 2004
 *
 * Modifications: Oct 18th, 2005
 * Note:        This test must be used with the correpsonding
                coll_write_test.
 *-------------------------------------------------------------------------
 */
static void
coll_read_test(int chunk_factor)
{

  const   char *filename;
  hid_t   facc_plist,dxfer_plist;
  hid_t   file, dataseti;           /* File and dataset identifiers */
  hid_t   mspaceid, fspaceid1;      /* Dataspace identifiers */


  /* Dimension sizes of the dataset (on disk) */
#if 0
  hsize_t mdim[] = {MSPACE_DIM1, MSPACE_DIM2}; /* Dimension sizes of the
						  dataset in memory when we
						  read selection from the
						  dataset on the disk */

#endif
  hsize_t mdim[2];
  hsize_t  start[2];  /* Start of hyperslab */
  hsize_t  stride[2]; /* Stride of hyperslab */
  hsize_t  count[2];  /* Block count */
  hsize_t  block[2];  /* Block sizes */
  herr_t   ret;

  unsigned i;

  int     *matrix_out;
  int     *matrix_out1;
#if 0
  int      matrix_out[MSPACE_DIM1][MSPACE_DIM2];
  int      matrix_out1[MSPACE_DIM1][MSPACE_DIM2];   /* Buffer to read from the
						       dataset */

#endif
  hbool_t  use_gpfs = FALSE;
  int      mpi_size,mpi_rank;

  MPI_Comm comm = MPI_COMM_WORLD;
  MPI_Info info = MPI_INFO_NULL;

  /*set up MPI parameters */
  MPI_Comm_size(comm,&mpi_size);
  MPI_Comm_rank(comm,&mpi_rank);


  /* Obtain file name */
  filename = GetTestParameters();


  /* Initialize the buffer */

  mdim[0] = MSPACE_DIM1;
  mdim[1] = MSPACE_DIM2*mpi_size;
  matrix_out =(int*)HDmalloc(sizeof(int)*MSPACE_DIM1*MSPACE_DIM2*mpi_size);
  matrix_out1=(int*)HDmalloc(sizeof(int)*MSPACE_DIM1*MSPACE_DIM2*mpi_size);

  /*** For testing collective hyperslab selection read ***/

  /* Obtain file access property list */
  facc_plist = create_faccess_plist(comm, info, facc_type, use_gpfs);
  VRFY((facc_plist >= 0),"");

   /*
   * Open the file.
   */
  file = H5Fopen(filename, H5F_ACC_RDONLY, facc_plist);
  VRFY((file >= 0),"H5Fopen succeeded");

  /*
   * Open the dataset.
   */
  dataseti = H5Dopen2(file,"independ_write", H5P_DEFAULT);
  VRFY((dataseti >= 0),"H5Dopen2 succeeded");

  /*
   * Get dataspace of the open dataset.
   */
  fspaceid1 = H5Dget_space(dataseti);
  VRFY((fspaceid1 >= 0),"file dataspace obtained succeeded");

  /* The First selection for FILE to read
   *
   *  block (1,1)
   *  stride(1.1)
   *  count (3,768/mpi_size)
   *  start (1,2+768*mpi_rank/mpi_size)
   *
   */
  start[0]  = RFFHSTART0;
  start[1]  = RFFHSTART1+mpi_rank*RFFHCOUNT1;
  block[0]  = RFFHBLOCK0;
  block[1]  = RFFHBLOCK1;
  stride[0] = RFFHSTRIDE0;
  stride[1] = RFFHSTRIDE1;
  count[0]  = RFFHCOUNT0;
  count[1]  = RFFHCOUNT1;

  ret = H5Sselect_hyperslab(fspaceid1, H5S_SELECT_SET, start, stride, count, block);
  VRFY((ret >= 0),"hyperslab selection succeeded");

  /* The Second selection for FILE to read
   *
   *  block (1,1)
   *  stride(1.1)
   *  count (3,1536/mpi_size)
   *  start (2,4+1536*mpi_rank/mpi_size)
   *
   */
  start[0]  = RFSHSTART0;
  start[1]  = RFSHSTART1+RFSHCOUNT1*mpi_rank;
  block[0]  = RFSHBLOCK0;
  block[1]  = RFSHBLOCK1;
  stride[0] = RFSHSTRIDE0;
  stride[1] = RFSHSTRIDE0;
  count[0]  = RFSHCOUNT0;
  count[1]  = RFSHCOUNT1;

  ret = H5Sselect_hyperslab(fspaceid1, H5S_SELECT_OR, start, stride, count, block);
  VRFY((ret >= 0),"hyperslab selection succeeded");


  /*
   * Create memory dataspace.
   */
  mspaceid = H5Screate_simple(MSPACE_RANK, mdim, NULL);

  /*
   * Select two hyperslabs in memory. Hyperslabs has the same
   * size and shape as the selected hyperslabs for the file dataspace.
   * Only the starting point is different.
   * The first selection
   *  block (1,1)
   *  stride(1.1)
   *  count (3,768/mpi_size)
   *  start (0,768*mpi_rank/mpi_size)
   *
   */

  start[0]  = RMFHSTART0;
  start[1]  = RMFHSTART1+mpi_rank*RMFHCOUNT1;
  block[0]  = RMFHBLOCK0;
  block[1]  = RMFHBLOCK1;
  stride[0] = RMFHSTRIDE0;
  stride[1] = RMFHSTRIDE1;
  count[0]  = RMFHCOUNT0;
  count[1]  = RMFHCOUNT1;
  ret = H5Sselect_hyperslab(mspaceid, H5S_SELECT_SET, start, stride, count, block);
  VRFY((ret >= 0),"hyperslab selection succeeded");

  /*
   * Select two hyperslabs in memory. Hyperslabs has the same
   * size and shape as the selected hyperslabs for the file dataspace
   * Only the starting point is different.
   * The second selection
   *  block (1,1)
   *  stride(1,1)
   *  count (3,1536/mpi_size)
   *  start (1,2+1536*mpi_rank/mpi_size)
   *
   */
  start[0]  = RMSHSTART0;
  start[1]  = RMSHSTART1+mpi_rank*RMSHCOUNT1;
  block[0]  = RMSHBLOCK0;
  block[1]  = RMSHBLOCK1;
  stride[0] = RMSHSTRIDE0;
  stride[1] = RMSHSTRIDE1;
  count[0]  = RMSHCOUNT0;
  count[1]  = RMSHCOUNT1;
  ret = H5Sselect_hyperslab(mspaceid, H5S_SELECT_OR, start, stride, count, block);
  VRFY((ret >= 0),"hyperslab selection succeeded");


  /*
   * Initialize data buffer.
   */

  HDmemset(matrix_out,0,sizeof(int)*MSPACE_DIM1*MSPACE_DIM2*mpi_size);
  HDmemset(matrix_out1,0,sizeof(int)*MSPACE_DIM1*MSPACE_DIM2*mpi_size);

  /*
   * Read data back to the buffer matrix_out.
   */

  dxfer_plist = H5Pcreate(H5P_DATASET_XFER);
  VRFY((dxfer_plist >= 0),"");

  ret = H5Pset_dxpl_mpio(dxfer_plist, H5FD_MPIO_COLLECTIVE);
  VRFY((ret >= 0),"MPIO data transfer property list succeed");
  if(dxfer_coll_type == DXFER_INDEPENDENT_IO) {
     ret = H5Pset_dxpl_mpio_collective_opt(dxfer_plist,H5FD_MPIO_INDIVIDUAL_IO);
     VRFY((ret>= 0),"set independent IO collectively succeeded");
  }


  /* Collective read */
  ret = H5Dread(dataseti, H5T_NATIVE_INT, mspaceid, fspaceid1,
		dxfer_plist, matrix_out);
  VRFY((ret >= 0),"H5D collecive read succeed");

  ret = H5Pclose(dxfer_plist);
  VRFY((ret >= 0),"");

  /* Independent read */
  ret = H5Dread(dataseti, H5T_NATIVE_INT, mspaceid, fspaceid1,
		H5P_DEFAULT, matrix_out1);
  VRFY((ret >= 0),"H5D independent read succeed");

  ret = 0;
  for (i = 0; i < MSPACE_DIM1*MSPACE_DIM2*mpi_size; i++){
      if(matrix_out[i]!=matrix_out1[i])ret = -1;
      if(ret < 0) break;
  }
  VRFY((ret >= 0),"H5D contiguous irregular collective read succeed");

  /*
   * Close memory file and memory dataspaces.
   */
  ret = H5Sclose(mspaceid);
  VRFY((ret >= 0),"");
  ret = H5Sclose(fspaceid1);
  VRFY((ret >= 0),"");

  /*
   * Close dataset.
   */
  ret = H5Dclose(dataseti);
  VRFY((ret >= 0),"");

  /*
   * Close property list
   */
  ret = H5Pclose(facc_plist);
  VRFY((ret >= 0),"");


  /*
   * Close the file.
   */
  ret = H5Fclose(file);
  VRFY((ret >= 0),"");

  return ;
}


/****************************************************************
**
**  lower_dim_size_comp_test__select_checker_board():  
**
**	Given a data space of tgt_rank, and dimensions:
**
**		(mpi_size + 1), edge_size, ... , edge_size
**
**	edge_size, and a checker_edge_size, select a checker
**	board selection of a sel_rank (sel_rank < tgt_rank) 
**	dimensional slice through the data space parallel to the 
**      sel_rank fastest changing indicies, with origin (in the
**	higher indicies) as indicated by the start array.
**
**	Note that this function, is hard coded to presume a 
**	maximum data space rank of 5.
**
**	While this maximum is declared as a constant, increasing
**	it will require extensive coding in addition to changing
**      the value of the constant.
**
**					JRM -- 11/11/09
**
****************************************************************/

#define LDSCT_DS_RANK	5
#define LOWER_DIM_SIZE_COMP_TEST_DEBUG_TARGET_RANK 0

#define LOWER_DIM_SIZE_COMP_TEST__SELECT_CHECKER_BOARD__DEBUG 0

static void
lower_dim_size_comp_test__select_checker_board(
                                 const int mpi_rank,
                                 const hid_t tgt_sid,
                                 const int tgt_rank,
                                 const hsize_t dims[LDSCT_DS_RANK],
                                 const int checker_edge_size,
                                 const int sel_rank,
                                 hsize_t sel_start[])
{
#if LOWER_DIM_SIZE_COMP_TEST__SELECT_CHECKER_BOARD__DEBUG 
    const char *	fcnName = 
			"lower_dim_size_comp_test__select_checker_board():";
#endif 
    hbool_t		first_selection = TRUE;
    int                 i, j, k, l, m;
    int			ds_offset;
    int			sel_offset;
    const int		test_max_rank = LDSCT_DS_RANK;  /* must update code if */
                                                        /* this changes        */
    hsize_t		base_count;
    hsize_t             offset_count;
    hsize_t     	start[LDSCT_DS_RANK];
    hsize_t     	stride[LDSCT_DS_RANK];
    hsize_t     	count[LDSCT_DS_RANK];
    hsize_t     	block[LDSCT_DS_RANK];
    herr_t      	ret;            /* Generic return value */

#if LOWER_DIM_SIZE_COMP_TEST__SELECT_CHECKER_BOARD__DEBUG 
    if ( mpi_rank == LOWER_DIM_SIZE_COMP_TEST_DEBUG_TARGET_RANK ) {
        HDfprintf(stdout, 
                  "%s:%d: dims/checker_edge_size = %d %d %d %d %d / %d\n",
                  fcnName, mpi_rank, (int)dims[0], (int)dims[1], (int)dims[2],
                  (int)dims[3], (int)dims[4], checker_edge_size);
    }
#endif /* LOWER_DIM_SIZE_COMP_TEST__SELECT_CHECKER_BOARD__DEBUG */ 

    HDassert( 0 < checker_edge_size );
    HDassert( 0 < sel_rank );
    HDassert( sel_rank <= tgt_rank );
    HDassert( tgt_rank <= test_max_rank );
    HDassert( test_max_rank <= LDSCT_DS_RANK );

    sel_offset = test_max_rank - sel_rank;
    HDassert( sel_offset >= 0 );

    ds_offset = test_max_rank - tgt_rank;
    HDassert( ds_offset >= 0 );
    HDassert( ds_offset <= sel_offset );

    HDassert( (hsize_t)checker_edge_size <= dims[sel_offset] );
    HDassert( dims[sel_offset] == 10 );

#if LOWER_DIM_SIZE_COMP_TEST__SELECT_CHECKER_BOARD__DEBUG 
    if ( mpi_rank == LOWER_DIM_SIZE_COMP_TEST_DEBUG_TARGET_RANK ) {
        HDfprintf(stdout, "%s:%d: sel_rank/sel_offset = %d/%d.\n", 
                  fcnName, mpi_rank, sel_rank, sel_offset);
        HDfprintf(stdout, "%s:%d: tgt_rank/ds_offset = %d/%d.\n", 
                  fcnName, mpi_rank, tgt_rank, ds_offset);
    }
#endif /* LOWER_DIM_SIZE_COMP_TEST__SELECT_CHECKER_BOARD__DEBUG */ 

    /* First, compute the base count (which assumes start == 0
     * for the associated offset) and offset_count (which
     * assumes start == checker_edge_size for the associated
     * offset).
     *
     * Note that the following computation depends on the C99
     * requirement that integer division discard any fraction
     * (truncation towards zero) to function correctly. As we
     * now require C99, this shouldn't be a problem, but noting
     * it may save us some pain if we are ever obliged to support
     * pre-C99 compilers again.
     */

    base_count = dims[sel_offset] / (checker_edge_size * 2);

    if ( (dims[sel_rank] % (checker_edge_size * 2)) > 0 ) {

        base_count++;
    }

    offset_count = 
	(hsize_t)((dims[sel_offset] - (hsize_t)checker_edge_size) / 
                  ((hsize_t)(checker_edge_size * 2)));

    if ( ((dims[sel_rank] - (hsize_t)checker_edge_size) % 
          ((hsize_t)(checker_edge_size * 2))) > 0 ) {

        offset_count++;
    }

#if LOWER_DIM_SIZE_COMP_TEST__SELECT_CHECKER_BOARD__DEBUG 
    if ( mpi_rank == LOWER_DIM_SIZE_COMP_TEST_DEBUG_TARGET_RANK ) {
        HDfprintf(stdout, "%s:%d: base_count/offset_count = %d/%d.\n", 
                  fcnName, mpi_rank, base_count, offset_count);
    }
#endif /* LOWER_DIM_SIZE_COMP_TEST__SELECT_CHECKER_BOARD__DEBUG */ 

    /* Now set up the stride and block arrays, and portions of the start
     * and count arrays that will not be altered during the selection of 
     * the checker board.
     */
    i = 0;
    while ( i < ds_offset ) {

        /* these values should never be used */
        start[i] = 0;
        stride[i] = 0;
        count[i] = 0;
        block[i] = 0;

        i++;
    }

    while ( i < sel_offset ) {

        start[i] = sel_start[i];
        stride[i] = 2 * dims[i];
        count[i] = 1;
        block[i] = 1;

        i++;
    }

    while ( i < test_max_rank ) {

        stride[i] = (hsize_t)(2 * checker_edge_size);
        block[i] = (hsize_t)checker_edge_size;

        i++;
    }
   
    i = 0;
    do {
        if ( 0 >= sel_offset ) {

            if ( i == 0 ) {

                start[0] = 0;
                count[0] = base_count;

            } else {

                start[0] = (hsize_t)checker_edge_size;
                count[0] = offset_count;

            }
        }

        j = 0;
        do { 
            if ( 1 >= sel_offset ) {

                if ( j == 0 ) {

                    start[1] = 0;
                    count[1] = base_count;

                } else {

                    start[1] = (hsize_t)checker_edge_size;
                    count[1] = offset_count;

                }
            }

            k = 0;
            do {
                if ( 2 >= sel_offset ) {

                    if ( k == 0 ) {

                        start[2] = 0;
                        count[2] = base_count;

                    } else {

                        start[2] = (hsize_t)checker_edge_size;
                        count[2] = offset_count;

                    }
                }

                l = 0;
                do {
                    if ( 3 >= sel_offset ) {

                        if ( l == 0 ) {

                            start[3] = 0;
                            count[3] = base_count;

                        } else {

                            start[3] = (hsize_t)checker_edge_size;
                            count[3] = offset_count;

                        }
                    }

                    m = 0;
                    do {
                        if ( 4 >= sel_offset ) {

                            if ( m == 0 ) {

                                start[4] = 0;
                                count[4] = base_count;

                            } else {

                                start[4] = (hsize_t)checker_edge_size;
                                count[4] = offset_count;

                            }
                        }

                        if ( ((i + j + k + l + m) % 2) == 0 ) {

#if LOWER_DIM_SIZE_COMP_TEST__SELECT_CHECKER_BOARD__DEBUG 
                             if ( mpi_rank == 
                                  LOWER_DIM_SIZE_COMP_TEST_DEBUG_TARGET_RANK ) {

                                HDfprintf(stdout, 
                                      "%s%d: *** first_selection = %d ***\n", 
                                      fcnName, mpi_rank, (int)first_selection);
                                HDfprintf(stdout, 
                                      "%s:%d: i/j/k/l/m = %d/%d/%d/%d/%d\n",
                                      fcnName, mpi_rank, i, j, k, l, m);
                                HDfprintf(stdout, 
                                      "%s:%d: start = %d %d %d %d %d.\n", 
                                      fcnName, mpi_rank, 
                                      (int)start[0], (int)start[1], 
                                      (int)start[2], (int)start[3], 
                                      (int)start[4]);
                                HDfprintf(stdout, 
                                      "%s:%d: stride = %d %d %d %d %d.\n", 
                                      fcnName, mpi_rank, 
                                      (int)stride[0], (int)stride[1], 
                                      (int)stride[2], (int)stride[3], 
                                      (int)stride[4]);
                                HDfprintf(stdout, 
                                      "%s:%d: count = %d %d %d %d %d.\n", 
                                      fcnName, mpi_rank, 
                                      (int)count[0], (int)count[1], 
                                      (int)count[2], (int)count[3], 
                                      (int)count[4]);
                                HDfprintf(stdout, 
                                      "%s:%d: block = %d %d %d %d %d.\n", 
                                      fcnName, mpi_rank, 
                                      (int)block[0], (int)block[1], 
                                      (int)block[2], (int)block[3], 
                                      (int)block[4]);
                                HDfprintf(stdout, 
                                      "%s:%d: n-cube extent dims = %d.\n", 
                                      fcnName, mpi_rank,
                                      H5Sget_simple_extent_ndims(tgt_sid));
                                HDfprintf(stdout, 
                                      "%s:%d: selection rank = %d.\n", 
                                      fcnName, mpi_rank, sel_rank);
                            }
#endif

                            if ( first_selection ) {

                                first_selection = FALSE; 

                                ret = H5Sselect_hyperslab
                                      (
                                        tgt_sid, 
                                        H5S_SELECT_SET,
                                        &(start[ds_offset]), 
                                        &(stride[ds_offset]), 
                                        &(count[ds_offset]), 
                                        &(block[ds_offset])
                                      );
    
                                VRFY((ret != FAIL), "H5Sselect_hyperslab(SET) succeeded");

                            } else {

                                ret = H5Sselect_hyperslab
                                      (
                                        tgt_sid, 
                                        H5S_SELECT_OR,
                                        &(start[ds_offset]), 
                                        &(stride[ds_offset]), 
                                        &(count[ds_offset]), 
                                        &(block[ds_offset])
                                      );
    
                                VRFY((ret != FAIL), "H5Sselect_hyperslab(OR) succeeded");

                            }
                        }

                        m++;

                    } while ( ( m <= 1 ) &&
                              ( 4 >= sel_offset ) );

                    l++;

                } while ( ( l <= 1 ) &&
                          ( 3 >= sel_offset ) );

                k++;

            } while ( ( k <= 1 ) &&
                      ( 2 >= sel_offset ) );

            j++;

        } while ( ( j <= 1 ) &&
                  ( 1 >= sel_offset ) );


        i++;

    } while ( ( i <= 1 ) &&
              ( 0 >= sel_offset ) );

#if LOWER_DIM_SIZE_COMP_TEST__SELECT_CHECKER_BOARD__DEBUG 
    if ( mpi_rank == LOWER_DIM_SIZE_COMP_TEST_DEBUG_TARGET_RANK ) {
        HDfprintf(stdout, "%s%d: H5Sget_select_npoints(tgt_sid) = %d.\n",
                  fcnName, mpi_rank, (int)H5Sget_select_npoints(tgt_sid));
    }
#endif /* LOWER_DIM_SIZE_COMP_TEST__SELECT_CHECKER_BOARD__DEBUG */

    /* Clip the selection back to the data space proper. */

    for ( i = 0; i < test_max_rank; i++ ) {

        start[i]  = 0;
        stride[i] = dims[i];
        count[i]  = 1;
        block[i]  = dims[i];
    }

    ret = H5Sselect_hyperslab(tgt_sid, H5S_SELECT_AND,
                              start, stride, count, block);

    VRFY((ret != FAIL), "H5Sselect_hyperslab(AND) succeeded");

#if LOWER_DIM_SIZE_COMP_TEST__SELECT_CHECKER_BOARD__DEBUG 
    if ( mpi_rank == LOWER_DIM_SIZE_COMP_TEST_DEBUG_TARGET_RANK ) {
        HDfprintf(stdout, "%s%d: H5Sget_select_npoints(tgt_sid) = %d.\n",
                  fcnName, mpi_rank, (int)H5Sget_select_npoints(tgt_sid));
        HDfprintf(stdout, "%s%d: done.\n", fcnName, mpi_rank);
    }
#endif /* LOWER_DIM_SIZE_COMP_TEST__SELECT_CHECKER_BOARD__DEBUG */

    return;

} /* lower_dim_size_comp_test__select_checker_board() */


/****************************************************************
**
**  lower_dim_size_comp_test__verify_data(): 
**
**	Examine the supplied buffer to see if it contains the 
**	expected data.  Return TRUE if it does, and FALSE 
**      otherwise.
**
**	The supplied buffer is presumed to this process's slice 
**	of the target data set.  Each such slice will be an
**	n-cube of rank (rank -1) and the supplied edge_size with
**	origin (mpi_rank, 0, ... , 0) in the target data set.
**
**	Further, the buffer is presumed to be the result of reading
**	or writing a checker board selection of an m (1 <= m < 
**      rank) dimensional slice through this processes slice
**	of the target data set.  Also, this slice must be parallel
**	to the fastest changing indicies.  
**
**	It is further presumed that the buffer was zeroed before
**	the read/write, and that the full target data set (i.e.
**	the buffer/data set for all processes) was initialized
**      with the natural numbers listed in order from the origin 
**	along the fastest changing axis.
**
**      Thus for a 20x10x10 dataset, the value stored in location
**	(x, y, z) (assuming that z is the fastest changing index
**	and x the slowest) is assumed to be:
**
**		(10 * 10 * x) + (10 * y) + z
**
**	Further, supposing that this is process 10, this process's 
**	slice of the dataset would be a 10 x 10 2-cube with origin
**	(10, 0, 0) in the data set, and would be initialize (prior
**	to the checkerboard selection) as follows:
**
**		1000, 1001, 1002, ... 1008, 1009
**		1010, 1011, 1012, ... 1018, 1019
**		  .     .     .         .     .
**		  .     .     .         .     .
**		  .     .     .         .     .
**		1090, 1091, 1092, ... 1098, 1099
**
**	In the case of a read from the processors slice of another
**	data set of different rank, the values expected will have
**	to be adjusted accordingly.  This is done via the 
**	first_expected_val parameter.
**
**	Finally, the function presumes that the first element 
**	of the buffer resides either at the origin of either
**	a selected or an unselected checker.  (Translation:
**	if partial checkers appear in the buffer, they will
**	intersect the edges of the n-cube oposite the origin.)
**
****************************************************************/

#define LOWER_DIM_SIZE_COMP_TEST__VERIFY_DATA__DEBUG 0

static hbool_t
lower_dim_size_comp_test__verify_data(uint32_t * buf_ptr,
#if LOWER_DIM_SIZE_COMP_TEST__VERIFY_DATA__DEBUG 
                                      const int mpi_rank,
#endif /* LOWER_DIM_SIZE_COMP_TEST__VERIFY_DATA__DEBUG */
                                      const int rank,
                                      const int edge_size,
                                      const int checker_edge_size,
                                      uint32_t first_expected_val,
                                      hbool_t buf_starts_in_checker)
{
#if LOWER_DIM_SIZE_COMP_TEST__VERIFY_DATA__DEBUG
    const char *	fcnName = 
			"lower_dim_size_comp_test__verify_data():";
#endif
    hbool_t good_data = TRUE;
    hbool_t in_checker;
    hbool_t start_in_checker[5];
    uint32_t expected_value;
    uint32_t * val_ptr;
    int i, j, k, l, m;  /* to track position in n-cube */
    int v, w, x, y, z;  /* to track position in checker */
    const int test_max_rank = 5; /* code changes needed if this is increased */

    HDassert( buf_ptr != NULL );
    HDassert( 0 < rank );
    HDassert( rank <= test_max_rank );
    HDassert( edge_size >= 6 );
    HDassert( 0 < checker_edge_size );
    HDassert( checker_edge_size <= edge_size );
    HDassert( test_max_rank <= LDSCT_DS_RANK );

#if LOWER_DIM_SIZE_COMP_TEST__VERIFY_DATA__DEBUG 
    if ( mpi_rank == LOWER_DIM_SIZE_COMP_TEST_DEBUG_TARGET_RANK ) {
        HDfprintf(stdout, "%s mpi_rank = %d.\n", fcnName, mpi_rank);
        HDfprintf(stdout, "%s rank = %d.\n", fcnName, rank);
        HDfprintf(stdout, "%s edge_size = %d.\n", fcnName, edge_size);
        HDfprintf(stdout, "%s checker_edge_size = %d.\n", 
                  fcnName, checker_edge_size);
        HDfprintf(stdout, "%s first_expected_val = %d.\n", 
                  fcnName, (int)first_expected_val);
        HDfprintf(stdout, "%s starts_in_checker = %d.\n", 
                  fcnName, (int)buf_starts_in_checker);
    }
#endif

    val_ptr = buf_ptr;
    expected_value = first_expected_val;

    i = 0;
    v = 0;
    start_in_checker[0] = buf_starts_in_checker;
    do
    {
        if ( v >= checker_edge_size ) {

            start_in_checker[0] = ! start_in_checker[0];
            v = 0;
        }

        j = 0;
        w = 0;
        start_in_checker[1] = start_in_checker[0];
        do
        {
            if ( w >= checker_edge_size ) {

                start_in_checker[1] = ! start_in_checker[1];
                w = 0;
            }

            k = 0;
            x = 0;
            start_in_checker[2] = start_in_checker[1];
            do
            {
                if ( x >= checker_edge_size ) {

                    start_in_checker[2] = ! start_in_checker[2];
                    x = 0;
                }

                l = 0;
                y = 0;
                start_in_checker[3] = start_in_checker[2];
                do
                { 
                    if ( y >= checker_edge_size ) {

                        start_in_checker[3] = ! start_in_checker[3];
                        y = 0;
                    }

                    m = 0;
                    z = 0;
#if LOWER_DIM_SIZE_COMP_TEST__VERIFY_DATA__DEBUG 
                    if ( mpi_rank == 
                         LOWER_DIM_SIZE_COMP_TEST_DEBUG_TARGET_RANK ) {
                        HDfprintf(stdout, "%d, %d, %d, %d, %d:", i, j, k, l, m);
                    }
#endif
                    in_checker = start_in_checker[3];
                    do
                    {
#if LOWER_DIM_SIZE_COMP_TEST__VERIFY_DATA__DEBUG 
                        if ( mpi_rank == 
                             LOWER_DIM_SIZE_COMP_TEST_DEBUG_TARGET_RANK ) {
                            HDfprintf(stdout, " %d", (int)(*val_ptr));
                        }
#endif
                        if ( z >= checker_edge_size ) {

                            in_checker = ! in_checker;
                            z = 0;
                        }
         
                        if ( in_checker ) {
                   
                            if ( *val_ptr != expected_value ) {

                                good_data = FALSE;
                            }
 
                            /* zero out buffer for re-use */
                            *val_ptr = 0;

                        } else if ( *val_ptr != 0 ) {

                            good_data = FALSE;
 
                            /* zero out buffer for re-use */
                            *val_ptr = 0;

                        }

                        val_ptr++;
                        expected_value++;
                        m++;
                        z++;
 
                    } while ( ( rank >= (test_max_rank - 4) ) &&
                              ( m < edge_size ) );
#if LOWER_DIM_SIZE_COMP_TEST__VERIFY_DATA__DEBUG 
                    if ( mpi_rank == 
                         LOWER_DIM_SIZE_COMP_TEST_DEBUG_TARGET_RANK ) {
                        HDfprintf(stdout, "\n");
                    }
#endif
                    l++;
                    y++;
                } while ( ( rank >= (test_max_rank - 3) ) &&
                          ( l < edge_size ) );
                k++;
                x++;
            } while ( ( rank >= (test_max_rank - 2) ) &&
                      ( k < edge_size ) );
            j++;
            w++;
        } while ( ( rank >= (test_max_rank - 1) ) &&
                  ( j < edge_size ) );
        i++;
        v++;
    } while ( ( rank >= test_max_rank ) &&
              ( i < edge_size ) );

    return(good_data);

} /* lower_dim_size_comp_test__verify_data() */


/*-------------------------------------------------------------------------
 * Function:	lower_dim_size_comp_test__run_test()
 *
 * Purpose:	Verify that a bug in the computation of the size of the 
 *		lower dimensions of a data space in H5S_obtain_datatype()
 *		has been corrected.
 *
 * Return:	void
 *
 * Programmer:	JRM -- 11/11/09
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

#define LDSCT_DS_RANK	5
#define LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG 0

static void
lower_dim_size_comp_test__run_test(const int chunk_edge_size,
                                   const hbool_t use_collective_io,
                                   const hid_t dset_type)
{
#if LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG 
    const char   *fcnName = "lower_dim_size_comp_test__run_test()";
    int		  rank;
    hsize_t	  dims[32];
    hsize_t	  max_dims[32];
#endif /* LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG */
    const char   *filename;
    hbool_t	  use_gpfs = FALSE;   /* Use GPFS hints */
    hbool_t	  data_ok = FALSE;
    hbool_t	  mis_match = FALSE;
    int           i;
    int           start_index;
    int           stop_index;
    int		  mrc;
    int		  mpi_rank;
    int		  mpi_size;
    MPI_Comm      mpi_comm = MPI_COMM_NULL;
    MPI_Info      mpi_info = MPI_INFO_NULL;
    hid_t         fid;                    /* HDF5 file ID */
    hid_t         acc_tpl;                /* File access templates */
    hid_t         xfer_plist = H5P_DEFAULT;
    size_t        small_ds_size;
    size_t        small_ds_slice_size;
    size_t        large_ds_size;
    size_t        large_ds_slice_size;
    uint32_t      expected_value;
    uint32_t    * small_ds_buf_0 = NULL;
    uint32_t    * small_ds_buf_1 = NULL;
    uint32_t    * large_ds_buf_0 = NULL;
    uint32_t    * large_ds_buf_1 = NULL;
    uint32_t    * ptr_0;
    uint32_t    * ptr_1;
    hsize_t       small_chunk_dims[LDSCT_DS_RANK];
    hsize_t       large_chunk_dims[LDSCT_DS_RANK];
    hsize_t       small_dims[LDSCT_DS_RANK];
    hsize_t       large_dims[LDSCT_DS_RANK];
    hsize_t       start[LDSCT_DS_RANK];
    hsize_t       stride[LDSCT_DS_RANK];
    hsize_t       count[LDSCT_DS_RANK];
    hsize_t       block[LDSCT_DS_RANK];
    hsize_t       small_sel_start[LDSCT_DS_RANK];
    hsize_t       large_sel_start[LDSCT_DS_RANK];
    hid_t         full_mem_small_ds_sid;
    hid_t         full_file_small_ds_sid;
    hid_t         mem_small_ds_sid;
    hid_t         file_small_ds_sid;
    hid_t         full_mem_large_ds_sid;
    hid_t         full_file_large_ds_sid;
    hid_t         mem_large_ds_sid;
    hid_t         file_large_ds_sid;
    hid_t         small_ds_dcpl_id = H5P_DEFAULT;
    hid_t         large_ds_dcpl_id = H5P_DEFAULT;
    hid_t         small_dataset;     /* Dataset ID                   */
    hid_t         large_dataset;     /* Dataset ID                   */
    htri_t        check;          /* Shape comparison return value */
    herr_t        ret;            /* Generic return value */

    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    HDassert( mpi_size >= 1 );

    mpi_comm = MPI_COMM_WORLD;
    mpi_info = MPI_INFO_NULL;

#if LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG 
    if ( mpi_rank == LOWER_DIM_SIZE_COMP_TEST_DEBUG_TARGET_RANK ) {
        HDfprintf(stdout, "%s:%d: chunk_edge_size = %d.\n",
                  fcnName, mpi_rank, (int)chunk_edge_size);
        HDfprintf(stdout, "%s:%d: use_collective_io = %d.\n",
                  fcnName, mpi_rank, (int)use_collective_io);
    }
#endif /* LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG */


    small_ds_size       = (size_t)((mpi_size + 1) *  1 *  1 * 10 * 10);
    small_ds_slice_size = (size_t)                 ( 1 *  1 * 10 * 10);
    large_ds_size       = (size_t)((mpi_size + 1) * 10 * 10 * 10 * 10);
    large_ds_slice_size = (size_t)                 (10 * 10 * 10 * 10);

#if LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG 
    if ( mpi_rank == LOWER_DIM_SIZE_COMP_TEST_DEBUG_TARGET_RANK ) {
        HDfprintf(stdout, "%s:%d: small ds size / slice size = %d / %d.\n",
                  fcnName, mpi_rank, 
                  (int)small_ds_size, (int)small_ds_slice_size);
        HDfprintf(stdout, "%s:%d: large ds size / slice size = %d / %d.\n",
                  fcnName, mpi_rank, 
                  (int)large_ds_size, (int)large_ds_slice_size);
    }
#endif /* LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG */

    /* Allocate buffers */
    small_ds_buf_0 = (uint32_t *)HDmalloc(sizeof(uint32_t) * small_ds_size);
    VRFY((small_ds_buf_0 != NULL), "malloc of small_ds_buf_0 succeeded");

    small_ds_buf_1 = (uint32_t *)HDmalloc(sizeof(uint32_t) * small_ds_size);
    VRFY((small_ds_buf_1 != NULL), "malloc of small_ds_buf_1 succeeded");

    large_ds_buf_0 = (uint32_t *)HDmalloc(sizeof(uint32_t) * large_ds_size);
    VRFY((large_ds_buf_0 != NULL), "malloc of large_ds_buf_0 succeeded");

    large_ds_buf_1 = (uint32_t *)HDmalloc(sizeof(uint32_t) * large_ds_size);
    VRFY((large_ds_buf_1 != NULL), "malloc of large_ds_buf_1 succeeded");


    /* initialize the buffers */

    ptr_0 = small_ds_buf_0;
    ptr_1 = small_ds_buf_1;

    for ( i = 0; i < (int)small_ds_size; i++ ) {

        *ptr_0 = (uint32_t)i;
        *ptr_1 = 0;

        ptr_0++;
        ptr_1++;
    }

    ptr_0 = large_ds_buf_0;
    ptr_1 = large_ds_buf_1;

    for ( i = 0; i < (int)large_ds_size; i++ ) {

        *ptr_0 = (uint32_t)i;
        *ptr_1 = 0;

        ptr_0++;
        ptr_1++;
    }


    /* get the file name */

    filename = (const char *)GetTestParameters();
    HDassert( filename != NULL );


    /* ----------------------------------------
     * CREATE AN HDF5 FILE WITH PARALLEL ACCESS
     * ---------------------------------------*/
    /* setup file access template */
    acc_tpl = create_faccess_plist(mpi_comm, mpi_info, facc_type, use_gpfs);
    VRFY((acc_tpl >= 0), "create_faccess_plist() succeeded");

    /* create the file collectively */
    fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, acc_tpl);
    VRFY((fid >= 0), "H5Fcreate succeeded");

    MESG("File opened.");

    /* Release file-access template */
    ret = H5Pclose(acc_tpl);
    VRFY((ret >= 0), "H5Pclose(acc_tpl) succeeded");


    /* setup dims: */
    small_dims[0] = (hsize_t)(mpi_size + 1);
    small_dims[1] =  1;
    small_dims[2] =  1;
    small_dims[3] = 10;
    small_dims[4] = 10;

    large_dims[0] = (hsize_t)(mpi_size + 1);
    large_dims[1] = 10;
    large_dims[2] = 10;
    large_dims[3] = 10;
    large_dims[4] = 10;

#if LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG 
    if ( mpi_rank == LOWER_DIM_SIZE_COMP_TEST_DEBUG_TARGET_RANK ) {
        HDfprintf(stdout, "%s:%d: small_dims[] = %d %d %d %d %d\n",
                  fcnName, mpi_rank, (int)small_dims[0], (int)small_dims[1],
                  (int)small_dims[2], (int)small_dims[3], (int)small_dims[4]);
        HDfprintf(stdout, "%s:%d: large_dims[] = %d %d %d %d %d\n",
                  fcnName, mpi_rank, (int)large_dims[0], (int)large_dims[1],
                  (int)large_dims[2], (int)large_dims[3], (int)large_dims[4]);
    }
#endif 

    /* create data spaces */

    full_mem_small_ds_sid = H5Screate_simple(5, small_dims, NULL);
    VRFY((full_mem_small_ds_sid != 0), 
         "H5Screate_simple() full_mem_small_ds_sid succeeded");

    full_file_small_ds_sid = H5Screate_simple(5, small_dims, NULL);
    VRFY((full_file_small_ds_sid != 0), 
         "H5Screate_simple() full_file_small_ds_sid succeeded");

    mem_small_ds_sid = H5Screate_simple(5, small_dims, NULL);
    VRFY((mem_small_ds_sid != 0), 
         "H5Screate_simple() mem_small_ds_sid succeeded");

    file_small_ds_sid = H5Screate_simple(5, small_dims, NULL);
    VRFY((file_small_ds_sid != 0), 
          "H5Screate_simple() file_small_ds_sid succeeded");


    full_mem_large_ds_sid = H5Screate_simple(5, large_dims, NULL);
    VRFY((full_mem_large_ds_sid != 0), 
         "H5Screate_simple() full_mem_large_ds_sid succeeded");

    full_file_large_ds_sid = H5Screate_simple(5, large_dims, NULL);
    VRFY((full_file_large_ds_sid != 0), 
         "H5Screate_simple() full_file_large_ds_sid succeeded");

    mem_large_ds_sid = H5Screate_simple(5, large_dims, NULL);
    VRFY((mem_large_ds_sid != 0), 
         "H5Screate_simple() mem_large_ds_sid succeeded");

    file_large_ds_sid = H5Screate_simple(5, large_dims, NULL);
    VRFY((file_large_ds_sid != 0), 
          "H5Screate_simple() file_large_ds_sid succeeded");


    /* Select the entire extent of the full small ds dataspaces */
    ret = H5Sselect_all(full_mem_small_ds_sid);
    VRFY((ret != FAIL), "H5Sselect_all(full_mem_small_ds_sid) succeeded");

    ret = H5Sselect_all(full_file_small_ds_sid);
    VRFY((ret != FAIL), "H5Sselect_all(full_file_small_ds_sid) succeeded");


    /* Select the entire extent of the full large ds dataspaces */
    ret = H5Sselect_all(full_mem_large_ds_sid);
    VRFY((ret != FAIL), "H5Sselect_all(full_mem_large_ds_sid) succeeded");

    ret = H5Sselect_all(full_file_large_ds_sid);
    VRFY((ret != FAIL), "H5Sselect_all(full_file_large_ds_sid) succeeded");


    /* if chunk edge size is greater than zero, set up the small and
     * large data set creation property lists to specify chunked
     * datasets.
     */
    if ( chunk_edge_size > 0 ) {

        small_chunk_dims[0] = (hsize_t)(1);
        small_chunk_dims[1] = small_chunk_dims[2] = (hsize_t)1;
        small_chunk_dims[3] = small_chunk_dims[4] = (hsize_t)chunk_edge_size;

#if LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG 
        if ( mpi_rank == LOWER_DIM_SIZE_COMP_TEST_DEBUG_TARGET_RANK ) {
            HDfprintf(stdout, "%s:%d: small chunk dims[] = %d %d %d %d %d\n",
                      fcnName, mpi_rank, (int)small_chunk_dims[0], 
                      (int)small_chunk_dims[1], (int)small_chunk_dims[2], 
                      (int)small_chunk_dims[3], (int)small_chunk_dims[4]);
        }
#endif 

        small_ds_dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
        VRFY((ret != FAIL), "H5Pcreate() small_ds_dcpl_id succeeded");

        ret = H5Pset_layout(small_ds_dcpl_id, H5D_CHUNKED);
        VRFY((ret != FAIL), "H5Pset_layout() small_ds_dcpl_id succeeded");

        ret = H5Pset_chunk(small_ds_dcpl_id, 5, small_chunk_dims);
        VRFY((ret != FAIL), "H5Pset_chunk() small_ds_dcpl_id succeeded");

        large_chunk_dims[0] = (hsize_t)(1);
        large_chunk_dims[1] = large_chunk_dims[2] = 
        	large_chunk_dims[3] = large_chunk_dims[4] = (hsize_t)chunk_edge_size;


#if LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG 
        if ( mpi_rank == LOWER_DIM_SIZE_COMP_TEST_DEBUG_TARGET_RANK ) {
            HDfprintf(stdout, "%s:%d: large chunk dims[] = %d %d %d %d %d\n",
                      fcnName, mpi_rank, (int)large_chunk_dims[0], 
                      (int)large_chunk_dims[1], (int)large_chunk_dims[2], 
                      (int)large_chunk_dims[3], (int)large_chunk_dims[4]);
        }
#endif 

        large_ds_dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
        VRFY((ret != FAIL), "H5Pcreate() large_ds_dcpl_id succeeded");

        ret = H5Pset_layout(large_ds_dcpl_id, H5D_CHUNKED);
        VRFY((ret != FAIL), "H5Pset_layout() large_ds_dcpl_id succeeded");

        ret = H5Pset_chunk(large_ds_dcpl_id, 5, large_chunk_dims);
        VRFY((ret != FAIL), "H5Pset_chunk() large_ds_dcpl_id succeeded");
    }


    /* create the small dataset */
    small_dataset = H5Dcreate2(fid, "small_dataset", dset_type,
                               file_small_ds_sid, H5P_DEFAULT,
                               small_ds_dcpl_id, H5P_DEFAULT);
    VRFY((ret >= 0), "H5Dcreate2() small_dataset succeeded");


    /* create the large dataset */
    large_dataset = H5Dcreate2(fid, "large_dataset", dset_type,
                               file_large_ds_sid, H5P_DEFAULT,
                               large_ds_dcpl_id, H5P_DEFAULT);
    VRFY((ret >= 0), "H5Dcreate2() large_dataset succeeded");

#if LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG 
    if ( mpi_rank == LOWER_DIM_SIZE_COMP_TEST_DEBUG_TARGET_RANK ) {
        HDfprintf(stdout, 
                  "%s:%d: small/large ds id = %d / %d.\n", 
                  fcnName, mpi_rank, (int)small_dataset, 
                  (int)large_dataset);
    }
#endif /* LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG */


    /* setup xfer property list */
    xfer_plist = H5Pcreate(H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "H5Pcreate(H5P_DATASET_XFER) succeeded");

    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");

    if ( ! use_collective_io ) {

        ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist,
                                              H5FD_MPIO_INDIVIDUAL_IO);
        VRFY((ret>= 0), "H5Pset_dxpl_mpio_collective_opt() suceeded");
    }


    /* setup selection to write initial data to the small data sets */
    start[0] = (hsize_t)(mpi_rank + 1);
    start[1] = start[2] = start[3] = start[4] = 0;

    stride[0] = (hsize_t)(2 * (mpi_size + 1));
    stride[1] = stride[2] = 2;
    stride[3] = stride[4] = 2 * 10;

    count[0] = count[1] = count[2] = count[3] = count[4] = 1;

    block[0] = block[1] = block[2] = 1;
    block[3] = block[4] = 10;

#if LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG 
    if ( mpi_rank == LOWER_DIM_SIZE_COMP_TEST_DEBUG_TARGET_RANK ) {
        HDfprintf(stdout, 
                  "%s:%d: settings for small data set initialization.\n", 
                  fcnName, mpi_rank);
        HDfprintf(stdout, "%s:%d: start[]  = %d %d %d %d %d\n",
                  fcnName, mpi_rank, (int)start[0], (int)start[1],
                  (int)start[2], (int)start[3], (int)start[4]);
        HDfprintf(stdout, "%s:%d: stride[] = %d %d %d %d %d\n",
                  fcnName, mpi_rank, (int)stride[0], (int)stride[1],
                  (int)stride[2], (int)stride[3], (int)stride[4]);
        HDfprintf(stdout, "%s:%d: count[]  = %d %d %d %d %d\n",
                  fcnName, mpi_rank, (int)count[0], (int)count[1],
                  (int)count[2], (int)count[3], (int)count[4]);
        HDfprintf(stdout, "%s:%d: block[]  = %d %d %d %d %d\n",
                  fcnName, mpi_rank, (int)block[0], (int)block[1],
                  (int)block[2], (int)block[3], (int)block[4]);
    }
#endif /* LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG */

    /* setup selections for writing initial data to the small data set */
    ret = H5Sselect_hyperslab(mem_small_ds_sid,
                              H5S_SELECT_SET,
                              start,
                              stride,
                              count,
                              block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(mem_small_ds_sid, set) suceeded");

    ret = H5Sselect_hyperslab(file_small_ds_sid,
                              H5S_SELECT_SET,
                              start,
                              stride,
                              count,
                              block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(file_small_ds_sid, set) suceeded");

    if ( MAINPROCESS ) { /* add an additional slice to the selections */

        start[0] = 0;

#if LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG 
        if ( mpi_rank == LOWER_DIM_SIZE_COMP_TEST_DEBUG_TARGET_RANK ) {
            HDfprintf(stdout, 
                      "%s:%d: added settings for main process.\n", 
                      fcnName, mpi_rank);
            HDfprintf(stdout, "%s:%d: start[]  = %d %d %d %d %d\n",
                      fcnName, mpi_rank, (int)start[0], (int)start[1],
                      (int)start[2], (int)start[3], (int)start[4]);
            HDfprintf(stdout, "%s:%d: stride[] = %d %d %d %d %d\n",
                      fcnName, mpi_rank, (int)stride[0], (int)stride[1],
                      (int)stride[2], (int)stride[3], (int)stride[4]);
            HDfprintf(stdout, "%s:%d: count[]  = %d %d %d %d %d\n",
                      fcnName, mpi_rank, (int)count[0], (int)count[1],
                      (int)count[2], (int)count[3], (int)count[4]);
            HDfprintf(stdout, "%s:%d: block[]  = %d %d %d %d %d\n",
                      fcnName, mpi_rank, (int)block[0], (int)block[1],
                      (int)block[2], (int)block[3], (int)block[4]);
        }
#endif /* LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG */

        ret = H5Sselect_hyperslab(mem_small_ds_sid,
                                  H5S_SELECT_OR,
                                  start,
                                  stride,
                                  count,
                                  block);
        VRFY((ret>= 0), "H5Sselect_hyperslab(mem_small_ds_sid, or) suceeded");

        ret = H5Sselect_hyperslab(file_small_ds_sid,
                                  H5S_SELECT_OR,
                                  start,
                                  stride,
                                  count,
                                  block);
        VRFY((ret>= 0), "H5Sselect_hyperslab(file_small_ds_sid, or) suceeded");
    }

    check =  H5Sselect_valid(mem_small_ds_sid);
    VRFY((check == TRUE),"H5Sselect_valid(mem_small_ds_sid) returns TRUE");

    check =  H5Sselect_valid(file_small_ds_sid);
    VRFY((check == TRUE),"H5Sselect_valid(file_small_ds_sid) returns TRUE");


    /* write the initial value of the small data set to file */
#if LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG 
    if ( mpi_rank == LOWER_DIM_SIZE_COMP_TEST_DEBUG_TARGET_RANK ) {
        HDfprintf(stdout, "%s:%d: writing init value of small ds to file.\n",
                  fcnName, mpi_rank);
    }
#endif /* LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG */
    ret = H5Dwrite(small_dataset, 
                   dset_type, 
                   mem_small_ds_sid, 
                   file_small_ds_sid,
                   xfer_plist, 
                   small_ds_buf_0);
    VRFY((ret >= 0), "H5Dwrite() small_dataset initial write succeeded");


    /* read the small data set back to verify that it contains the 
     * expected data.  Note that each process reads in the entire 
     * data set and verifies it.
     */
    ret = H5Dread(small_dataset,
                  H5T_NATIVE_UINT32,
                  full_mem_small_ds_sid,
                  full_file_small_ds_sid,
                  xfer_plist,
                  small_ds_buf_1);
    VRFY((ret >= 0), "H5Dread() small_dataset initial read succeeded");


    /* sync with the other processes before checking data */
    mrc = MPI_Barrier(MPI_COMM_WORLD);
    VRFY((mrc==MPI_SUCCESS), "Sync after small dataset writes");


    /* verify that the correct data was written to the small data set,
     * and reset the buffer to zero in passing.
     */
    expected_value = 0;
    mis_match = FALSE;
    ptr_1 = small_ds_buf_1;

    i = 0;
    for ( i = 0; i < (int)small_ds_size; i++ ) {

        if ( *ptr_1 != expected_value ) {

            mis_match = TRUE;
        }

        *ptr_1 = (uint32_t)0;

        ptr_1++;
        expected_value++;
    }
    VRFY( (mis_match == FALSE), "small ds init data good.");



    /* setup selections for writing initial data to the large data set */
    start[0] = (hsize_t)(mpi_rank + 1);
    start[1] = start[2] = start[3] = start[4] = (hsize_t)0;

    stride[0] = (hsize_t)(2 * (mpi_size + 1));
    stride[1] = stride[2] = stride[3] = stride[4] = (hsize_t)(2 * 10);

    count[0] = count[1] = count[2] = count[3] = count[4] = (hsize_t)1;

    block[0] = (hsize_t)1;
    block[1] = block[2] = block[3] = block[4] = (hsize_t)10;

#if LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG 
    if ( mpi_rank == LOWER_DIM_SIZE_COMP_TEST_DEBUG_TARGET_RANK ) {
        HDfprintf(stdout, 
                  "%s:%d: settings for large data set initialization.\n", 
                  fcnName, mpi_rank);
        HDfprintf(stdout, "%s:%d: start[]  = %d %d %d %d %d\n",
                  fcnName, mpi_rank, (int)start[0], (int)start[1],
                  (int)start[2], (int)start[3], (int)start[4]);
        HDfprintf(stdout, "%s:%d: stride[] = %d %d %d %d %d\n",
                  fcnName, mpi_rank, (int)stride[0], (int)stride[1],
                  (int)stride[2], (int)stride[3], (int)stride[4]);
        HDfprintf(stdout, "%s:%d: count[]  = %d %d %d %d %d\n",
                  fcnName, mpi_rank, (int)count[0], (int)count[1],
                  (int)count[2], (int)count[3], (int)count[4]);
        HDfprintf(stdout, "%s:%d: block[]  = %d %d %d %d %d\n",
                  fcnName, mpi_rank, (int)block[0], (int)block[1],
                  (int)block[2], (int)block[3], (int)block[4]);
    }
#endif /* LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG */

    ret = H5Sselect_hyperslab(mem_large_ds_sid,
                              H5S_SELECT_SET,
                              start,
                              stride,
                              count,
                              block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(mem_large_ds_sid, set) suceeded");

    ret = H5Sselect_hyperslab(file_large_ds_sid,
                              H5S_SELECT_SET,
                              start,
                              stride,
                              count,
                              block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(file_large_ds_sid, set) suceeded");

#if LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG 
    if ( mpi_rank == LOWER_DIM_SIZE_COMP_TEST_DEBUG_TARGET_RANK ) {
	 HDfprintf(stdout, 
              "%s%d: H5Sget_select_npoints(mem_large_ds_sid) = %d.\n",
              fcnName, mpi_rank, 
              (int)H5Sget_select_npoints(mem_large_ds_sid));
	 HDfprintf(stdout, 
              "%s%d: H5Sget_select_npoints(file_large_ds_sid) = %d.\n",
              fcnName, mpi_rank, 
              (int)H5Sget_select_npoints(file_large_ds_sid));
    }
#endif /* LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG */

    if ( MAINPROCESS ) { /* add an additional slice to the selections */

        start[0] = (hsize_t)0;

#if LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG 
        if ( mpi_rank == LOWER_DIM_SIZE_COMP_TEST_DEBUG_TARGET_RANK ) {
            HDfprintf(stdout, 
                      "%s:%d: added settings for main process.\n", 
                      fcnName, mpi_rank);
            HDfprintf(stdout, "%s:%d: start[]  = %d %d %d %d %d\n",
                      fcnName, mpi_rank, (int)start[0], (int)start[1],
                      (int)start[2], (int)start[3], (int)start[4]);
            HDfprintf(stdout, "%s:%d: stride[] = %d %d %d %d %d\n",
                      fcnName, mpi_rank, (int)stride[0], (int)stride[1],
                      (int)stride[2], (int)stride[3], (int)stride[4]);
            HDfprintf(stdout, "%s:%d: count[]  = %d %d %d %d %d\n",
                      fcnName, mpi_rank, (int)count[0], (int)count[1],
                      (int)count[2], (int)count[3], (int)count[4]);
            HDfprintf(stdout, "%s:%d: block[]  = %d %d %d %d %d\n",
                      fcnName, mpi_rank, (int)block[0], (int)block[1],
                      (int)block[2], (int)block[3], (int)block[4]);
        }
#endif /* LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG */

        ret = H5Sselect_hyperslab(mem_large_ds_sid,
                                  H5S_SELECT_OR,
                                  start,
                                  stride,
                                  count,
                                  block);
        VRFY((ret>= 0), "H5Sselect_hyperslab(mem_large_ds_sid, or) suceeded");

        ret = H5Sselect_hyperslab(file_large_ds_sid,
                                  H5S_SELECT_OR,
                                  start,
                                  stride,
                                  count,
                                  block);
        VRFY((ret>= 0), "H5Sselect_hyperslab(file_large_ds_sid, or) suceeded");

#if LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG 
        if ( mpi_rank == LOWER_DIM_SIZE_COMP_TEST_DEBUG_TARGET_RANK ) {
 	     HDfprintf(stdout, 
                  "%s%d: H5Sget_select_npoints(mem_large_ds_sid) = %d.\n",
                  fcnName, mpi_rank, 
                  (int)H5Sget_select_npoints(mem_large_ds_sid));
	     HDfprintf(stdout, 
                  "%s%d: H5Sget_select_npoints(file_large_ds_sid) = %d.\n",
                  fcnName, mpi_rank, 
                  (int)H5Sget_select_npoints(file_large_ds_sid));
        }
#endif /* LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG */
    }

    /* try clipping the selection back to the large data space proper */
    start[0] = start[1] = start[2] = start[3] = start[4] = (hsize_t)0;

    stride[0] = (hsize_t)(2 * (mpi_size + 1));
    stride[1] = stride[2] = stride[3] = stride[4] = (hsize_t)(2 * 10);

    count[0] = count[1] = count[2] = count[3] = count[4] = (hsize_t)1;

    block[0] = (hsize_t)(mpi_size + 1);
    block[1] = block[2] = block[3] = block[4] = (hsize_t)10;

    ret = H5Sselect_hyperslab(mem_large_ds_sid, H5S_SELECT_AND,
                              start, stride, count, block);
    VRFY((ret != FAIL),"H5Sselect_hyperslab(mem_large_ds_sid, and) succeeded");

    ret = H5Sselect_hyperslab(file_large_ds_sid, H5S_SELECT_AND,
                              start, stride, count, block);
    VRFY((ret != FAIL),"H5Sselect_hyperslab(file_large_ds_sid, and) succeeded");

#if LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG 
    if ( mpi_rank == LOWER_DIM_SIZE_COMP_TEST_DEBUG_TARGET_RANK ) { 

        rank = H5Sget_simple_extent_dims(mem_large_ds_sid, dims, max_dims);
        HDfprintf(stdout, 
                  "%s:%d: mem_large_ds_sid dims[%d] = %d %d %d %d %d\n",
                  fcnName, mpi_rank, rank, (int)dims[0], (int)dims[1], 
                  (int)dims[2], (int)dims[3], (int)dims[4]);

        rank = H5Sget_simple_extent_dims(file_large_ds_sid, dims, max_dims);
        HDfprintf(stdout, 
                  "%s:%d: file_large_ds_sid dims[%d] = %d %d %d %d %d\n",
                  fcnName, mpi_rank, rank, (int)dims[0], (int)dims[1], 
                  (int)dims[2], (int)dims[3], (int)dims[4]);
    }
#endif /* LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG */

    check =  H5Sselect_valid(mem_large_ds_sid);
    VRFY((check == TRUE),"H5Sselect_valid(mem_large_ds_sid) returns TRUE");

    check =  H5Sselect_valid(file_large_ds_sid);
    VRFY((check == TRUE),"H5Sselect_valid(file_large_ds_sid) returns TRUE");


    /* write the initial value of the large data set to file */
#if LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG 
    if ( mpi_rank == LOWER_DIM_SIZE_COMP_TEST_DEBUG_TARGET_RANK ) { 
        HDfprintf(stdout, "%s:%d: writing init value of large ds to file.\n",
                  fcnName, mpi_rank);
        HDfprintf(stdout, 
                  "%s:%d: large_dataset = %d.\n",
                  fcnName, mpi_rank, 
                  (int)large_dataset);
        HDfprintf(stdout, 
                  "%s:%d: mem_large_ds_sid = %d, file_large_ds_sid = %d.\n",
                  fcnName, mpi_rank, 
                  (int)mem_large_ds_sid, (int)file_large_ds_sid);
    }
#endif /* LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG */

    ret = H5Dwrite(large_dataset, 
                   dset_type, 
                   mem_large_ds_sid, 
                   file_large_ds_sid,
                   xfer_plist, 
                   large_ds_buf_0);

    if ( ret < 0 ) H5Eprint2(H5E_DEFAULT, stderr);
    VRFY((ret >= 0), "H5Dwrite() large_dataset initial write succeeded");


    /* sync with the other processes before checking data */
    mrc = MPI_Barrier(MPI_COMM_WORLD);
    VRFY((mrc==MPI_SUCCESS), "Sync after large dataset writes");

    /* read the large data set back to verify that it contains the 
     * expected data.  Note that each process reads in the entire 
     * data set.
     */
    ret = H5Dread(large_dataset,
                  H5T_NATIVE_UINT32,
                  full_mem_large_ds_sid,
                  full_file_large_ds_sid,
                  xfer_plist,
                  large_ds_buf_1);
    VRFY((ret >= 0), "H5Dread() large_dataset initial read succeeded");


    /* verify that the correct data was written to the large data set.
     * in passing, reset the buffer to zeros
     */
    expected_value = 0;
    mis_match = FALSE;
    ptr_1 = large_ds_buf_1;

    i = 0;
    for ( i = 0; i < (int)large_ds_size; i++ ) {

        if ( *ptr_1 != expected_value ) {

            mis_match = TRUE;
        }

        *ptr_1 = (uint32_t)0;

        ptr_1++;
        expected_value++;
    }
    VRFY( (mis_match == FALSE), "large ds init data good.");

    /***********************************/
    /***** INITIALIZATION COMPLETE *****/
    /***********************************/


    /* read a checkerboard selection of the process slice of the 
     * small on disk data set into the process slice of the large 
     * in memory data set, and verify the data read.
     */

    small_sel_start[0] = (hsize_t)(mpi_rank + 1);
    small_sel_start[1] = small_sel_start[2] = 
    small_sel_start[3] = small_sel_start[4] = 0;

    lower_dim_size_comp_test__select_checker_board(mpi_rank,
                                                   file_small_ds_sid,
                                                   /* tgt_rank          = */  5,
                                                   small_dims,
                                                   /* checker_edge_size = */  3,
                                                   /* sel_rank */             2,
                                                   small_sel_start);

    expected_value = (uint32_t)
                     ((small_sel_start[0] * small_dims[1] * small_dims[2] * 
                                            small_dims[3] * small_dims[4]) +
                      (small_sel_start[1] * small_dims[2] * small_dims[3] * 
                                            small_dims[4]) +
                      (small_sel_start[2] * small_dims[3] * small_dims[4]) +
                      (small_sel_start[3] * small_dims[4]) +
                      (small_sel_start[4]));


    large_sel_start[0] = (hsize_t)(mpi_rank + 1);
    large_sel_start[1] = 5;
    large_sel_start[2] = large_sel_start[3] = large_sel_start[4] = 0;

    lower_dim_size_comp_test__select_checker_board(mpi_rank,
                                                   mem_large_ds_sid,
                                                   /* tgt_rank          = */  5,
                                                   large_dims,
                                                   /* checker_edge_size = */  3,
                                                   /* sel_rank          = */  2,
                                                   large_sel_start);


    /* verify that H5S_select_shape_same() reports the two
     * selections as having the same shape.
     */
    check = H5S_select_shape_same_test(mem_large_ds_sid,
                                       file_small_ds_sid);
    VRFY((check == TRUE), "H5S_select_shape_same_test passed (1)");


    ret = H5Dread(small_dataset,
                  H5T_NATIVE_UINT32,
                  mem_large_ds_sid,
                  file_small_ds_sid,
                  xfer_plist,
                  large_ds_buf_1);

    VRFY((ret >= 0), "H5Sread() slice from small ds succeeded.");

#if LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG 
    if ( mpi_rank == LOWER_DIM_SIZE_COMP_TEST_DEBUG_TARGET_RANK ) {
        HDfprintf(stdout, "%s:%d: H5Dread() returns.\n", fcnName, mpi_rank);
    }
#endif /* LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG */

    /* verify that expected data is retrieved */

    data_ok = TRUE;

    start_index = (int)((large_sel_start[0] * large_dims[1] * large_dims[2] * 
                                              large_dims[3] * large_dims[4]) +
                        (large_sel_start[1] * large_dims[2] * large_dims[3] * 
                                              large_dims[4]) +
                        (large_sel_start[2] * large_dims[3] * large_dims[4]) +
                        (large_sel_start[3] * large_dims[4]) +
                        (large_sel_start[4]));

    stop_index  = start_index + (int)small_ds_slice_size;

    HDassert( 0 <= start_index );
    HDassert( start_index < stop_index );
    HDassert( stop_index <= (int)large_ds_size );

    ptr_1 = large_ds_buf_1;

    for ( i = 0; i < start_index; i++ ) {

        if ( *ptr_1 != (uint32_t)0 ) {

            data_ok = FALSE;
            *ptr_1 = (uint32_t)0;
        }

        ptr_1++;
    }

    VRFY((data_ok == TRUE), "slice read from small ds data good(1).");


    data_ok = lower_dim_size_comp_test__verify_data(ptr_1,
#if LOWER_DIM_SIZE_COMP_TEST__VERIFY_DATA__DEBUG
                                            mpi_rank,
#endif /* LOWER_DIM_SIZE_COMP_TEST__VERIFY_DATA__DEBUG */
                                            /* rank */ 2,
                                            /* edge_size */ 10,
                                            /*  checker_edge_size */ 3,
                                            expected_value,
                                            /* buf_starts_in_checker */ TRUE);

    VRFY((data_ok == TRUE), "slice read from small ds data good(2).");

    data_ok = TRUE;

    ptr_1 += small_ds_slice_size;


    for ( i = stop_index; i < (int)large_ds_size; i++ ) {

        if ( *ptr_1 != (uint32_t)0 ) {

            data_ok = FALSE;
            *ptr_1 = (uint32_t)0;
        }

        ptr_1++;
    }

    VRFY((data_ok == TRUE), "slice read from small ds data good(3).");





    /* read a checkerboard selection of a slice of the process slice of 
     * the large on disk data set into the process slice of the small 
     * in memory data set, and verify the data read.
     */

    small_sel_start[0] = (hsize_t)(mpi_rank + 1);
    small_sel_start[1] = small_sel_start[2] = 
    small_sel_start[3] = small_sel_start[4] = 0;

    lower_dim_size_comp_test__select_checker_board(mpi_rank,
                                                   mem_small_ds_sid,
                                                   /* tgt_rank          = */  5,
                                                   small_dims,
                                                   /* checker_edge_size = */  3,
                                                   /* sel_rank */             2,
                                                   small_sel_start);

    large_sel_start[0] = (hsize_t)(mpi_rank + 1);
    large_sel_start[1] = 5;
    large_sel_start[2] = large_sel_start[3] = large_sel_start[4] = 0;

    lower_dim_size_comp_test__select_checker_board(mpi_rank,
                                                   file_large_ds_sid,
                                                   /* tgt_rank          = */  5,
                                                   large_dims,
                                                   /* checker_edge_size = */  3,
                                                   /* sel_rank          = */  2,
                                                   large_sel_start);


    /* verify that H5S_select_shape_same() reports the two
     * selections as having the same shape.
     */
    check = H5S_select_shape_same_test(mem_small_ds_sid,
                                       file_large_ds_sid);
    VRFY((check == TRUE), "H5S_select_shape_same_test passed (2)");


    ret = H5Dread(large_dataset,
                  H5T_NATIVE_UINT32,
                  mem_small_ds_sid,
                  file_large_ds_sid,
                  xfer_plist,
                  small_ds_buf_1);

    VRFY((ret >= 0), "H5Sread() slice from large ds succeeded.");

#if LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG 
    if ( mpi_rank == LOWER_DIM_SIZE_COMP_TEST_DEBUG_TARGET_RANK ) {
        HDfprintf(stdout, "%s:%d: H5Dread() returns.\n", fcnName, mpi_rank);
    }
#endif /* LOWER_DIM_SIZE_COMP_TEST__RUN_TEST__DEBUG */

    /* verify that expected data is retrieved */

    data_ok = TRUE;

    expected_value = (uint32_t)
                     ((large_sel_start[0] * large_dims[1] * large_dims[2] * 
                                            large_dims[3] * large_dims[4]) +
                      (large_sel_start[1] * large_dims[2] * large_dims[3] * 
                                            large_dims[4]) +
                      (large_sel_start[2] * large_dims[3] * large_dims[4]) +
                      (large_sel_start[3] * large_dims[4]) +
                      (large_sel_start[4]));

    start_index = (int)(mpi_rank + 1) * (int)small_ds_slice_size;

    stop_index  = start_index + (int)small_ds_slice_size;

    HDassert( 0 <= start_index );
    HDassert( start_index < stop_index );
    HDassert( stop_index <= (int)small_ds_size );

    ptr_1 = small_ds_buf_1;

    for ( i = 0; i < start_index; i++ ) {

        if ( *ptr_1 != (uint32_t)0 ) {

            data_ok = FALSE;
            *ptr_1 = (uint32_t)0;
        }

        ptr_1++;
    }

    VRFY((data_ok == TRUE), "slice read from large ds data good(1).");


    data_ok = lower_dim_size_comp_test__verify_data(ptr_1,
#if LOWER_DIM_SIZE_COMP_TEST__VERIFY_DATA__DEBUG
                                                    mpi_rank,
#endif /* LOWER_DIM_SIZE_COMP_TEST__VERIFY_DATA__DEBUG */
                                                    /* rank */ 2,
                                                    /* edge_size */ 10,
                                                    /*  checker_edge_size */ 3,
                                                    expected_value,
                                                    /* buf_starts_in_checker */ TRUE);

    VRFY((data_ok == TRUE), "slice read from large ds data good(2).");

    data_ok = TRUE;

    ptr_1 += small_ds_slice_size;


    for ( i = stop_index; i < (int)small_ds_size; i++ ) {

        if ( *ptr_1 != (uint32_t)0 ) {

#if LOWER_DIM_SIZE_COMP_TEST__VERIFY_DATA__DEBUG
            if ( mpi_rank == LOWER_DIM_SIZE_COMP_TEST_DEBUG_TARGET_RANK ) {
                HDfprintf(stdout, "%s:%d: unexpected value at index %d: %d.\n", 
                          fcnName, mpi_rank, (int)i, (int)(*ptr_1));
            }
#endif /* LOWER_DIM_SIZE_COMP_TEST__VERIFY_DATA__DEBUG */

            data_ok = FALSE;
            *ptr_1 = (uint32_t)0;
        }

        ptr_1++;
    }

    VRFY((data_ok == TRUE), "slice read from large ds data good(3).");


    /* Close dataspaces */
    ret = H5Sclose(full_mem_small_ds_sid);
    VRFY((ret != FAIL), "H5Sclose(full_mem_small_ds_sid) succeeded");

    ret = H5Sclose(full_file_small_ds_sid);
    VRFY((ret != FAIL), "H5Sclose(full_file_small_ds_sid) succeeded");

    ret = H5Sclose(mem_small_ds_sid);
    VRFY((ret != FAIL), "H5Sclose(mem_small_ds_sid) succeeded");

    ret = H5Sclose(file_small_ds_sid);
    VRFY((ret != FAIL), "H5Sclose(file_small_ds_sid) succeeded");


    ret = H5Sclose(full_mem_large_ds_sid);
    VRFY((ret != FAIL), "H5Sclose(full_mem_large_ds_sid) succeeded");

    ret = H5Sclose(full_file_large_ds_sid);
    VRFY((ret != FAIL), "H5Sclose(full_file_large_ds_sid) succeeded");

    ret = H5Sclose(mem_large_ds_sid);
    VRFY((ret != FAIL), "H5Sclose(mem_large_ds_sid) succeeded");

    ret = H5Sclose(file_large_ds_sid);
    VRFY((ret != FAIL), "H5Sclose(file_large_ds_sid) succeeded");


    /* Close Datasets */
    ret = H5Dclose(small_dataset);
    VRFY((ret != FAIL), "H5Dclose(small_dataset) succeeded");

    ret = H5Dclose(large_dataset);
    VRFY((ret != FAIL), "H5Dclose(large_dataset) succeeded");


    /* close the file collectively */
    MESG("about to close file.");
    ret = H5Fclose(fid);
    VRFY((ret != FAIL), "file close succeeded");

    /* Free memory buffers */
    if ( small_ds_buf_0 != NULL ) HDfree(small_ds_buf_0);
    if ( small_ds_buf_1 != NULL ) HDfree(small_ds_buf_1);

    if ( large_ds_buf_0 != NULL ) HDfree(large_ds_buf_0);
    if ( large_ds_buf_1 != NULL ) HDfree(large_ds_buf_1);

    return;

} /* lower_dim_size_comp_test__run_test() */


/*-------------------------------------------------------------------------
 * Function:	lower_dim_size_comp_test()
 *
 * Purpose:	Test to see if an error in the computation of the size
 *		of the lower dimensions in H5S_obtain_datatype() has
 *		been corrected.
 *
 * Return:	void
 *
 * Programmer:	JRM -- 11/11/09
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

void
lower_dim_size_comp_test(void)
{
    /* const char *fcnName = "lower_dim_size_comp_test()"; */
    int		chunk_edge_size = 0;
    int  	use_collective_io = 1;
    hid_t	dset_type = H5T_NATIVE_UINT;
#if 0
   sleep(60);
#endif
    HDcompile_assert(sizeof(uint32_t) == sizeof(unsigned));
    for ( use_collective_io = (hbool_t)0; 
          (int)use_collective_io <= 1; 
          (hbool_t)(use_collective_io++) ) {

        chunk_edge_size = 0;
        lower_dim_size_comp_test__run_test(chunk_edge_size,
                                           (hbool_t)use_collective_io,
                                           dset_type);


        chunk_edge_size = 5;
        lower_dim_size_comp_test__run_test(chunk_edge_size,
                                           (hbool_t)use_collective_io,
                                           dset_type);
    }

    return;

} /* lower_dim_size_comp_test() */


/*-------------------------------------------------------------------------
 * Function:	link_chunk_collective_io_test()
 *
 * Purpose:	Test to verify that an error in MPI type management in 
 *		H5D_link_chunk_collective_io() has been corrected.
 *		In this bug, we used to free MPI types regardless of 
 *		whether they were basic or derived.
 *
 *		This test is based on a bug report kindly provided by
 *		Rob Latham of the MPICH team and ANL.
 *
 *		The basic thrust of the test is to cause a process
 *		to participate in a collective I/O in which it:
 *
 *		1) Reads or writes exactly one chunk,
 *
 *		2) Has no in memory buffer for any other chunk.
 *
 *		The test differers from Rob Latham's bug report in 
 *		that is runs with an arbitrary number of proceeses,
 *		and uses a 1 dimensional dataset.
 *
 * Return:	void
 *
 * Programmer:	JRM -- 12/16/09
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

#define LINK_CHUNK_COLLECTIVE_IO_TEST_CHUNK_SIZE	16

void
link_chunk_collective_io_test(void)
{
    /* const char *fcnName = "link_chunk_collective_io_test()"; */
    const char *filename;
    hbool_t    mis_match = FALSE;
    hbool_t    use_gpfs = FALSE;   /* Use GPFS hints */
    int	       i;
    int	       mrc;
    int        mpi_rank;
    int        mpi_size;
    MPI_Comm   mpi_comm = MPI_COMM_WORLD;
    MPI_Info   mpi_info = MPI_INFO_NULL;
    hsize_t    count[1] = {1};
    hsize_t    stride[1] = {2 * LINK_CHUNK_COLLECTIVE_IO_TEST_CHUNK_SIZE};
    hsize_t    block[1] = {LINK_CHUNK_COLLECTIVE_IO_TEST_CHUNK_SIZE};
    hsize_t    start[1];
    hsize_t    dims[1];
    hsize_t    chunk_dims[1] = {LINK_CHUNK_COLLECTIVE_IO_TEST_CHUNK_SIZE};
    herr_t     ret;            /* Generic return value */
    hid_t      file_id;
    hid_t      acc_tpl;
    hid_t      dset_id;
    hid_t      file_ds_sid;
    hid_t      write_mem_ds_sid;
    hid_t      read_mem_ds_sid;
    hid_t      ds_dcpl_id;
    hid_t      xfer_plist;
    double     diff;
    double     expected_value;
    double     local_data_written[LINK_CHUNK_COLLECTIVE_IO_TEST_CHUNK_SIZE];
    double     local_data_read[LINK_CHUNK_COLLECTIVE_IO_TEST_CHUNK_SIZE];

    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    HDassert( mpi_size > 0 );

    /* get the file name */
    filename = (const char *)GetTestParameters();
    HDassert( filename != NULL );

    /* setup file access template */
    acc_tpl = create_faccess_plist(mpi_comm, mpi_info, facc_type, use_gpfs);
    VRFY((acc_tpl >= 0), "create_faccess_plist() succeeded");

    /* create the file collectively */
    file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, acc_tpl);
    VRFY((file_id >= 0), "H5Fcreate succeeded");

    MESG("File opened.");

    /* Release file-access template */
    ret = H5Pclose(acc_tpl);
    VRFY((ret >= 0), "H5Pclose(acc_tpl) succeeded");

    /* setup dims */
    dims[0] = ((hsize_t)mpi_size) * ((hsize_t)(LINK_CHUNK_COLLECTIVE_IO_TEST_CHUNK_SIZE));

    /* setup mem and file data spaces */
    write_mem_ds_sid = H5Screate_simple(1, chunk_dims, NULL);
    VRFY((write_mem_ds_sid != 0),
         "H5Screate_simple() write_mem_ds_sid succeeded");

    read_mem_ds_sid = H5Screate_simple(1, chunk_dims, NULL);
    VRFY((read_mem_ds_sid != 0),
         "H5Screate_simple() read_mem_ds_sid succeeded");

    file_ds_sid = H5Screate_simple(1, dims, NULL);
    VRFY((file_ds_sid != 0),
         "H5Screate_simple() file_ds_sid succeeded");

    /* setup data set creation property list */
    ds_dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((ds_dcpl_id != FAIL), "H5Pcreate() ds_dcpl_id succeeded");

    ret = H5Pset_layout(ds_dcpl_id, H5D_CHUNKED);
    VRFY((ret != FAIL), "H5Pset_layout() ds_dcpl_id succeeded");

    ret = H5Pset_chunk(ds_dcpl_id, 1, chunk_dims);
    VRFY((ret != FAIL), "H5Pset_chunk() small_ds_dcpl_id succeeded");

    /* create the data set */
    dset_id = H5Dcreate2(file_id, "dataset", H5T_NATIVE_DOUBLE,
                         file_ds_sid, H5P_DEFAULT,
                         ds_dcpl_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "H5Dcreate2() dataset succeeded");

    /* close the dataset creation property list */
    ret = H5Pclose(ds_dcpl_id);
    VRFY((ret >= 0), "H5Pclose(ds_dcpl_id) succeeded");

    /* setup local data */
    expected_value = (double)(LINK_CHUNK_COLLECTIVE_IO_TEST_CHUNK_SIZE) *
                     (double)(mpi_rank);
    for ( i = 0; i < LINK_CHUNK_COLLECTIVE_IO_TEST_CHUNK_SIZE; i++ ) {

        local_data_written[i] = expected_value;
	local_data_read[i] = 0.0;
	expected_value += 1.0;
    }

    /* select the file and mem spaces */
    start[0] = (hsize_t)(mpi_rank * LINK_CHUNK_COLLECTIVE_IO_TEST_CHUNK_SIZE);
    ret = H5Sselect_hyperslab(file_ds_sid,
                              H5S_SELECT_SET,
                              start,
                              stride,
                              count,
                              block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(file_ds_sid, set) suceeded");

    ret = H5Sselect_all(write_mem_ds_sid);
    VRFY((ret != FAIL), "H5Sselect_all(mem_ds_sid) succeeded");

    /* Note that we use NO SELECTION on the read memory dataspace */

    /* setup xfer property list */
    xfer_plist = H5Pcreate(H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "H5Pcreate(H5P_DATASET_XFER) succeeded");

    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");

    /* write the data set */
    ret = H5Dwrite(dset_id, 
                   H5T_NATIVE_DOUBLE, 
                   write_mem_ds_sid, 
                   file_ds_sid,
                   xfer_plist, 
                   local_data_written);

    VRFY((ret >= 0), "H5Dwrite() dataset initial write succeeded");
    
    /* sync with the other processes before checking data */
    mrc = MPI_Barrier(MPI_COMM_WORLD);
    VRFY((mrc==MPI_SUCCESS), "Sync after dataset write");

    /* read this processes slice of the dataset back in */
    ret = H5Dread(dset_id,
                  H5T_NATIVE_DOUBLE,
                  read_mem_ds_sid,
                  file_ds_sid,
                  xfer_plist,
                  local_data_read);
    VRFY((ret >= 0), "H5Dread() dataset read succeeded");

    /* close the xfer property list */
    ret = H5Pclose(xfer_plist);
    VRFY((ret >= 0), "H5Pclose(xfer_plist) succeeded");

    /* verify the data */
    mis_match = FALSE;
    for ( i = 0; i < LINK_CHUNK_COLLECTIVE_IO_TEST_CHUNK_SIZE; i++ ) {

        diff = local_data_written[i] - local_data_read[i];
        diff = fabs(diff);

        if ( diff >= 0.001 ) {

            mis_match = TRUE;
        } 
    }
    VRFY( (mis_match == FALSE), "dataset data good.");

    /* Close dataspaces */
    ret = H5Sclose(write_mem_ds_sid);
    VRFY((ret != FAIL), "H5Sclose(write_mem_ds_sid) succeeded");

    ret = H5Sclose(read_mem_ds_sid);
    VRFY((ret != FAIL), "H5Sclose(read_mem_ds_sid) succeeded");

    ret = H5Sclose(file_ds_sid);
    VRFY((ret != FAIL), "H5Sclose(file_ds_sid) succeeded");

    /* Close Dataset */
    ret = H5Dclose(dset_id);
    VRFY((ret != FAIL), "H5Dclose(dset_id) succeeded");

    /* close the file collectively */
    ret = H5Fclose(file_id);
    VRFY((ret != FAIL), "file close succeeded");

    return;

} /* link_chunk_collective_io_test() */

