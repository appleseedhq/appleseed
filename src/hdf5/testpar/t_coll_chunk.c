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

#include "testphdf5.h"
#include "H5Dprivate.h"


/* some commonly used routines for collective chunk IO tests*/

static void ccslab_set(int mpi_rank,int mpi_size,hsize_t start[],hsize_t count[],
		       hsize_t stride[],hsize_t block[],int mode);

static void ccdataset_fill(hsize_t start[],hsize_t count[],
			   hsize_t stride[],hsize_t block[],DATATYPE*dataset);

static void ccdataset_print(hsize_t start[],hsize_t block[],DATATYPE*dataset);

static int ccdataset_vrfy(hsize_t start[], hsize_t count[], hsize_t stride[],
			  hsize_t block[], DATATYPE *dataset, DATATYPE *original);

static void coll_chunktest(const char* filename,int chunk_factor,int select_factor,int api_option);


/*-------------------------------------------------------------------------
 * Function:	coll_chunk1
 *
 * Purpose:	Wrapper to test the collective chunk IO for regular JOINT
                selection with a single chunk
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Unknown
 *		July 12th, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

/* ------------------------------------------------------------------------
 *  Descriptions for the selection: One big singluar selection inside one chunk
 *  Two dimensions,
 *
 *  dim1       = SPACE_DIM1(5760)*mpi_size
 *  dim2       = SPACE_DIM2(3)
 *  chunk_dim1 = dim1
 *  chunk_dim2 = dim2
 *  block      = 1 for all dimensions
 *  stride     = 1 for all dimensions
 *  count0     = SPACE_DIM1(5760)
 *  count1     = SPACE_DIM2(3)
 *  start0     = mpi_rank*SPACE_DIM1
 *  start1     = 0
 * ------------------------------------------------------------------------
 */

void
coll_chunk1(void)
{
    const char *filename = GetTestParameters();

    coll_chunktest(filename, 1, BYROW_CONT, API_NONE);
}


/*-------------------------------------------------------------------------
 * Function:	coll_chunk2
 *
 * Purpose:	Wrapper to test the collective chunk IO for regular DISJOINT
                selection with a single chunk
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Unknown
 *		July 12th, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

 /* ------------------------------------------------------------------------
 *  Descriptions for the selection: many disjoint selections inside one chunk
 *  Two dimensions,
 *
 *  dim1       = SPACE_DIM1*mpi_size(5760)
 *  dim2       = SPACE_DIM2(3)
 *  chunk_dim1 = dim1
 *  chunk_dim2 = dim2
 *  block      = 1 for all dimensions
 *  stride     = 3 for all dimensions
 *  count0     = SPACE_DIM1/stride0(5760/3)
 *  count1     = SPACE_DIM2/stride(3/3 = 1)
 *  start0     = mpi_rank*SPACE_DIM1
 *  start1     = 0
 *
 * ------------------------------------------------------------------------
 */
void
coll_chunk2(void)
{
    const char *filename = GetTestParameters();

    coll_chunktest(filename, 1, BYROW_DISCONT, API_NONE);
}


/*-------------------------------------------------------------------------
 * Function:	coll_chunk3
 *
 * Purpose:	Wrapper to test the collective chunk IO for regular JOINT
                selection with at least number of 2*mpi_size chunks
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Unknown
 *		July 12th, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

/* ------------------------------------------------------------------------
 *  Descriptions for the selection: one singular selection accross many chunks
 *  Two dimensions, Num of chunks = 2* mpi_size
 *
 *  dim1       = SPACE_DIM1*mpi_size
 *  dim2       = SPACE_DIM2(3)
 *  chunk_dim1 = SPACE_DIM1
 *  chunk_dim2 = dim2/2
 *  block      = 1 for all dimensions
 *  stride     = 1 for all dimensions
 *  count0     = SPACE_DIM1
 *  count1     = SPACE_DIM2(3)
 *  start0     = mpi_rank*SPACE_DIM1
 *  start1     = 0
 *
 * ------------------------------------------------------------------------
 */

void
coll_chunk3(void)
{
    const char *filename = GetTestParameters();
    int mpi_size;

    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    coll_chunktest(filename, mpi_size, BYROW_CONT, API_NONE);
}

/*-------------------------------------------------------------------------
 * Function:	coll_chunk4
 *
 * Purpose:	Wrapper to test the collective chunk IO for regular JOINT
                selection with at least number of 2*mpi_size chunks
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Unknown
 *		July 12th, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

/* ------------------------------------------------------------------------
 *  Descriptions for the selection: one singular selection accross many chunks
 *  Two dimensions, Num of chunks = 2* mpi_size
 *
 *  dim1       = SPACE_DIM1*mpi_size
 *  dim2       = SPACE_DIM2
 *  chunk_dim1 = dim1
 *  chunk_dim2 = dim2
 *  block      = 1 for all dimensions
 *  stride     = 1 for all dimensions
 *  count0     = SPACE_DIM1
 *  count1     = SPACE_DIM2(3)
 *  start0     = mpi_rank*SPACE_DIM1
 *  start1     = 0
 *
 * ------------------------------------------------------------------------
 */

void
coll_chunk4(void)
{
    const char *filename = GetTestParameters();

    coll_chunktest(filename, 1, BYROW_SELECTNONE, API_NONE);
}

/*-------------------------------------------------------------------------
 * Function:	coll_chunk4
 *
 * Purpose:	Wrapper to test the collective chunk IO for regular JOINT
                selection with at least number of 2*mpi_size chunks
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Unknown
 *		July 12th, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

/* ------------------------------------------------------------------------
 *  Descriptions for the selection: one singular selection accross many chunks
 *  Two dimensions, Num of chunks = 2* mpi_size
 *
 *  dim1       = SPACE_DIM1*mpi_size
 *  dim2       = SPACE_DIM2
 *  chunk_dim1 = dim1
 *  chunk_dim2 = dim2
 *  block      = 1 for all dimensions
 *  stride     = 1 for all dimensions
 *  count0     = SPACE_DIM1
 *  count1     = SPACE_DIM2(3)
 *  start0     = mpi_rank*SPACE_DIM1
 *  start1     = 0
 *
 * ------------------------------------------------------------------------
 */

void
coll_chunk5(void)
{
    const char *filename = GetTestParameters();

    coll_chunktest(filename, 4, BYROW_SELECTUNBALANCE, API_LINK_HARD);
}

/*-------------------------------------------------------------------------
 * Function:	coll_chunk6
 *
 * Purpose:	Wrapper to test the collective chunk IO for regular JOINT
                selection with at least number of 2*mpi_size chunks
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Unknown
 *		July 12th, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

/* ------------------------------------------------------------------------
 *  Descriptions for the selection: one singular selection accross many chunks
 *  Two dimensions, Num of chunks = 2* mpi_size
 *
 *  dim1       = SPACE_DIM1*mpi_size
 *  dim2       = SPACE_DIM2
 *  chunk_dim1 = dim1
 *  chunk_dim2 = dim2
 *  block      = 1 for all dimensions
 *  stride     = 1 for all dimensions
 *  count0     = SPACE_DIM1
 *  count1     = SPACE_DIM2(3)
 *  start0     = mpi_rank*SPACE_DIM1
 *  start1     = 0
 *
 * ------------------------------------------------------------------------
 */

void
coll_chunk6(void)
{
    const char *filename = GetTestParameters();

    coll_chunktest(filename, 4, BYROW_SELECTUNBALANCE, API_MULTI_HARD);
}

/*-------------------------------------------------------------------------
 * Function:	coll_chunk7
 *
 * Purpose:	Wrapper to test the collective chunk IO for regular JOINT
                selection with at least number of 2*mpi_size chunks
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Unknown
 *		July 12th, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

/* ------------------------------------------------------------------------
 *  Descriptions for the selection: one singular selection accross many chunks
 *  Two dimensions, Num of chunks = 2* mpi_size
 *
 *  dim1       = SPACE_DIM1*mpi_size
 *  dim2       = SPACE_DIM2
 *  chunk_dim1 = dim1
 *  chunk_dim2 = dim2
 *  block      = 1 for all dimensions
 *  stride     = 1 for all dimensions
 *  count0     = SPACE_DIM1
 *  count1     = SPACE_DIM2(3)
 *  start0     = mpi_rank*SPACE_DIM1
 *  start1     = 0
 *
 * ------------------------------------------------------------------------
 */

void
coll_chunk7(void)
{
    const char *filename = GetTestParameters();

    coll_chunktest(filename, 4, BYROW_SELECTUNBALANCE, API_LINK_TRUE);
}

/*-------------------------------------------------------------------------
 * Function:	coll_chunk8
 *
 * Purpose:	Wrapper to test the collective chunk IO for regular JOINT
                selection with at least number of 2*mpi_size chunks
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Unknown
 *		July 12th, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

/* ------------------------------------------------------------------------
 *  Descriptions for the selection: one singular selection accross many chunks
 *  Two dimensions, Num of chunks = 2* mpi_size
 *
 *  dim1       = SPACE_DIM1*mpi_size
 *  dim2       = SPACE_DIM2
 *  chunk_dim1 = dim1
 *  chunk_dim2 = dim2
 *  block      = 1 for all dimensions
 *  stride     = 1 for all dimensions
 *  count0     = SPACE_DIM1
 *  count1     = SPACE_DIM2(3)
 *  start0     = mpi_rank*SPACE_DIM1
 *  start1     = 0
 *
 * ------------------------------------------------------------------------
 */

void
coll_chunk8(void)
{
    const char *filename = GetTestParameters();

    coll_chunktest(filename, 4, BYROW_SELECTUNBALANCE, API_LINK_FALSE);
}

/*-------------------------------------------------------------------------
 * Function:	coll_chunk9
 *
 * Purpose:	Wrapper to test the collective chunk IO for regular JOINT
                selection with at least number of 2*mpi_size chunks
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Unknown
 *		July 12th, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

/* ------------------------------------------------------------------------
 *  Descriptions for the selection: one singular selection accross many chunks
 *  Two dimensions, Num of chunks = 2* mpi_size
 *
 *  dim1       = SPACE_DIM1*mpi_size
 *  dim2       = SPACE_DIM2
 *  chunk_dim1 = dim1
 *  chunk_dim2 = dim2
 *  block      = 1 for all dimensions
 *  stride     = 1 for all dimensions
 *  count0     = SPACE_DIM1
 *  count1     = SPACE_DIM2(3)
 *  start0     = mpi_rank*SPACE_DIM1
 *  start1     = 0
 *
 * ------------------------------------------------------------------------
 */

void
coll_chunk9(void)
{
  const char *filename = GetTestParameters();

  coll_chunktest(filename, 4, BYROW_SELECTUNBALANCE, API_MULTI_COLL);
}

/*-------------------------------------------------------------------------
 * Function:	coll_chunk10
 *
 * Purpose:	Wrapper to test the collective chunk IO for regular JOINT
                selection with at least number of 2*mpi_size chunks
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Unknown
 *		July 12th, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

/* ------------------------------------------------------------------------
 *  Descriptions for the selection: one singular selection accross many chunks
 *  Two dimensions, Num of chunks = 2* mpi_size
 *
 *  dim1       = SPACE_DIM1*mpi_size
 *  dim2       = SPACE_DIM2
 *  chunk_dim1 = dim1
 *  chunk_dim2 = dim2
 *  block      = 1 for all dimensions
 *  stride     = 1 for all dimensions
 *  count0     = SPACE_DIM1
 *  count1     = SPACE_DIM2(3)
 *  start0     = mpi_rank*SPACE_DIM1
 *  start1     = 0
 *
 * ------------------------------------------------------------------------
 */

void
coll_chunk10(void)
{
  const char *filename = GetTestParameters();

  coll_chunktest(filename, 4, BYROW_SELECTINCHUNK, API_MULTI_IND);
}


/*-------------------------------------------------------------------------
 * Function:	coll_chunktest
 *
 * Purpose:     The real testing routine for regular selection of collective
                chunking storage
                testing both write and read,
		If anything fails, it may be read or write. There is no
		separation test between read and write.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Unknown
 *		July 12th, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */


static void
coll_chunktest(const char* filename,
	       int chunk_factor,
	       int select_factor,
               int api_option)
{
  hid_t	   file,dataset, file_dataspace;
  hid_t    acc_plist,xfer_plist,crp_plist;

  hsize_t  dims[RANK], chunk_dims[RANK];
  int*     data_array1  = NULL;
  int*     data_origin1 = NULL;

  hsize_t  start[RANK],count[RANK],stride[RANK],block[RANK];

#ifdef H5_HAVE_INSTRUMENTED_LIBRARY
  unsigned prop_value;
#endif /* H5_HAVE_INSTRUMENTED_LIBRARY */

  hbool_t  use_gpfs = FALSE;
  int      mpi_size,mpi_rank;

  herr_t   status;
  MPI_Comm comm = MPI_COMM_WORLD;
  MPI_Info info = MPI_INFO_NULL;

  /* set up MPI parameters */
  MPI_Comm_size(comm,&mpi_size);
  MPI_Comm_rank(comm,&mpi_rank);

  /* Create the data space */

  acc_plist = create_faccess_plist(comm,info,facc_type,use_gpfs);
  VRFY((acc_plist >= 0),"");

  file = H5Fcreate(filename,H5F_ACC_TRUNC,H5P_DEFAULT,acc_plist);
  VRFY((file >= 0),"H5Fcreate succeeded");

  status = H5Pclose(acc_plist);
  VRFY((status >= 0),"");

  /* setup dimensionality object */
  dims[0] = SPACE_DIM1*mpi_size;
  dims[1] = SPACE_DIM2;


  /* allocate memory for data buffer */
  data_array1 = (int *)malloc(dims[0] * dims[1] * sizeof(int));
  VRFY((data_array1 != NULL), "data_array1 malloc succeeded");

  /* set up dimensions of the slab this process accesses */
  ccslab_set(mpi_rank, mpi_size, start, count, stride, block, select_factor);

  file_dataspace = H5Screate_simple(2, dims, NULL);
  VRFY((file_dataspace >= 0), "file dataspace created succeeded");

  crp_plist = H5Pcreate(H5P_DATASET_CREATE);
  VRFY((crp_plist >= 0),"");

  /* Set up chunk information.  */
  chunk_dims[0] = dims[0]/chunk_factor;

  /* to decrease the testing time, maintain bigger chunk size */

  (chunk_factor == 1) ? (chunk_dims[1] = SPACE_DIM2) : (chunk_dims[1] = SPACE_DIM2/2);
  status = H5Pset_chunk(crp_plist, 2, chunk_dims);
  VRFY((status >= 0),"chunk creation property list succeeded");

  dataset = H5Dcreate2(file, DSET_COLLECTIVE_CHUNK_NAME, H5T_NATIVE_INT,
		      file_dataspace, H5P_DEFAULT, crp_plist, H5P_DEFAULT);
  VRFY((dataset >= 0),"dataset created succeeded");

  status = H5Pclose(crp_plist);
  VRFY((status >= 0), "");

  /*put some trivial data in the data array */
  ccdataset_fill(start, stride, count,block, data_array1);
  MESG("data_array initialized");

  status = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride,
			     count, block);
  VRFY((status >= 0),"hyperslab selection succeeded");

  /* set up the collective transfer property list */
  xfer_plist = H5Pcreate(H5P_DATASET_XFER);
  VRFY((xfer_plist >= 0), "");

  status = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
  VRFY((status>= 0),"MPIO collective transfer property succeeded");
  if(dxfer_coll_type == DXFER_INDEPENDENT_IO) {
     status = H5Pset_dxpl_mpio_collective_opt(xfer_plist, H5FD_MPIO_INDIVIDUAL_IO);
     VRFY((status>= 0),"set independent IO collectively succeeded");
  }

  switch(api_option){
	case API_LINK_HARD:
	   status = H5Pset_dxpl_mpio_chunk_opt(xfer_plist,H5FD_MPIO_CHUNK_ONE_IO);
           VRFY((status>= 0),"collective chunk optimization succeeded");
	break;
	case API_MULTI_HARD:
	   status = H5Pset_dxpl_mpio_chunk_opt(xfer_plist,H5FD_MPIO_CHUNK_MULTI_IO);
	   VRFY((status>= 0),"collective chunk optimization succeeded ");
	break;
	case API_LINK_TRUE:
           status = H5Pset_dxpl_mpio_chunk_opt_num(xfer_plist,2);
	   VRFY((status>= 0),"collective chunk optimization set chunk number succeeded");
	break;
	case API_LINK_FALSE:
           status = H5Pset_dxpl_mpio_chunk_opt_num(xfer_plist,6);
           VRFY((status>= 0),"collective chunk optimization set chunk number succeeded");
	break;
	case API_MULTI_COLL:
           status = H5Pset_dxpl_mpio_chunk_opt_num(xfer_plist,8);/* make sure it is using multi-chunk IO */
           VRFY((status>= 0),"collective chunk optimization set chunk number succeeded");
	   status = H5Pset_dxpl_mpio_chunk_opt_ratio(xfer_plist,50);
           VRFY((status>= 0),"collective chunk optimization set chunk ratio succeeded");
	break;
	case API_MULTI_IND:
           status = H5Pset_dxpl_mpio_chunk_opt_num(xfer_plist,8);/* make sure it is using multi-chunk IO */
           VRFY((status>= 0),"collective chunk optimization set chunk number succeeded");
	   status = H5Pset_dxpl_mpio_chunk_opt_ratio(xfer_plist,100);
           VRFY((status>= 0),"collective chunk optimization set chunk ratio succeeded");
	break;
	default:
	;
   }

#ifdef H5_HAVE_INSTRUMENTED_LIBRARY
  if(facc_type == FACC_MPIO) {
      switch(api_option) {
            case API_LINK_HARD:
               prop_value = H5D_XFER_COLL_CHUNK_DEF;
               status = H5Pinsert2(xfer_plist, H5D_XFER_COLL_CHUNK_LINK_HARD_NAME, H5D_XFER_COLL_CHUNK_SIZE, &prop_value,
                           NULL, NULL, NULL, NULL, NULL, NULL);
               VRFY((status >= 0),"testing property list inserted succeeded");

               prop_value = H5D_XFER_COLL_CHUNK_FIX;
               status = H5Pinsert2(xfer_plist, H5D_XFER_COLL_CHUNK_LINK_TO_MULTI, H5D_XFER_COLL_CHUNK_SIZE, &prop_value,
                           NULL, NULL, NULL, NULL, NULL, NULL);
               VRFY((status >= 0),"testing property list inserted succeeded");

            break;

            case API_MULTI_HARD:
               prop_value = H5D_XFER_COLL_CHUNK_DEF;
               status = H5Pinsert2(xfer_plist, H5D_XFER_COLL_CHUNK_MULTI_HARD_NAME, H5D_XFER_COLL_CHUNK_SIZE, &prop_value,
                           NULL, NULL, NULL, NULL, NULL, NULL);
               VRFY((status >= 0),"testing property list inserted succeeded");
            break;

            case API_LINK_TRUE:
               prop_value = H5D_XFER_COLL_CHUNK_DEF;
               status = H5Pinsert2(xfer_plist, H5D_XFER_COLL_CHUNK_LINK_NUM_TRUE_NAME, H5D_XFER_COLL_CHUNK_SIZE, &prop_value,
                           NULL, NULL, NULL, NULL, NULL, NULL);
               VRFY((status >= 0),"testing property list inserted succeeded");

               prop_value = H5D_XFER_COLL_CHUNK_FIX;
               status = H5Pinsert2(xfer_plist, H5D_XFER_COLL_CHUNK_LINK_TO_MULTI_OPT, H5D_XFER_COLL_CHUNK_SIZE, &prop_value,
                           NULL, NULL, NULL, NULL, NULL, NULL);
               VRFY((status >= 0),"testing property list inserted succeeded");

            break;

            case API_LINK_FALSE:
               prop_value = H5D_XFER_COLL_CHUNK_DEF;
               status = H5Pinsert2(xfer_plist, H5D_XFER_COLL_CHUNK_LINK_NUM_FALSE_NAME, H5D_XFER_COLL_CHUNK_SIZE, &prop_value,
                           NULL, NULL, NULL, NULL, NULL, NULL);
               VRFY((status >= 0),"testing property list inserted succeeded");
            break;

            case API_MULTI_COLL:
               prop_value = H5D_XFER_COLL_CHUNK_DEF;
               status = H5Pinsert2(xfer_plist, H5D_XFER_COLL_CHUNK_MULTI_RATIO_COLL_NAME, H5D_XFER_COLL_CHUNK_SIZE, &prop_value,
                           NULL, NULL, NULL, NULL, NULL, NULL);
               VRFY((status >= 0),"testing property list inserted succeeded");
            break;

            case API_MULTI_IND:
               prop_value = H5D_XFER_COLL_CHUNK_DEF;
               status = H5Pinsert2(xfer_plist, H5D_XFER_COLL_CHUNK_MULTI_RATIO_IND_NAME, H5D_XFER_COLL_CHUNK_SIZE, &prop_value,
                           NULL, NULL, NULL, NULL, NULL, NULL);
               VRFY((status >= 0),"testing property list inserted succeeded");
            break;

            default:
                ;
       }
   }
#endif

  /* write data collectively */
  status = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, file_dataspace,
		    xfer_plist, data_array1);
  VRFY((status >= 0),"dataset write succeeded");

#ifdef H5_HAVE_INSTRUMENTED_LIBRARY
  if(facc_type == FACC_MPIO) {
      switch(api_option){
            case API_LINK_HARD:
               status = H5Pget(xfer_plist,H5D_XFER_COLL_CHUNK_LINK_HARD_NAME,&prop_value);
               VRFY((status >= 0),"testing property list get succeeded");
               if(prop_value !=0){/*double check if the option is switched to multiple chunk internally.*/
                 status = H5Pget(xfer_plist,H5D_XFER_COLL_CHUNK_LINK_TO_MULTI, &prop_value);
                 VRFY((status >= 0),"testing property list get succeeded");
                 VRFY((prop_value == 1),"API to set LINK COLLECTIVE IO without optimization succeeded");
               }
            break;
            case API_MULTI_HARD:
               status = H5Pget(xfer_plist,H5D_XFER_COLL_CHUNK_MULTI_HARD_NAME,&prop_value);
               VRFY((status >= 0),"testing property list get succeeded");
               VRFY((prop_value == 0),"API to set MULTI-CHUNK COLLECTIVE IO without optimization succeeded");
            break;
            case API_LINK_TRUE:
               status = H5Pget(xfer_plist,H5D_XFER_COLL_CHUNK_LINK_NUM_TRUE_NAME,&prop_value);
               VRFY((status >= 0),"testing property list get succeeded");
               if(prop_value !=0){/*double check if the option is switched to multiple chunk internally.*/
                 status = H5Pget(xfer_plist,H5D_XFER_COLL_CHUNK_LINK_TO_MULTI_OPT, &prop_value);
                 VRFY((status >= 0),"testing property list get succeeded");
                 VRFY((prop_value == 1),"API to set LINK COLLECTIVE IO without optimization succeeded");
               }
            break;
            case API_LINK_FALSE:
               status = H5Pget(xfer_plist,H5D_XFER_COLL_CHUNK_LINK_NUM_FALSE_NAME,&prop_value);
               VRFY((status >= 0),"testing property list get succeeded");
               VRFY((prop_value == 0),"API to set LINK IO transferring to multi-chunk IO succeeded");
            break;
            case API_MULTI_COLL:
               status = H5Pget(xfer_plist,H5D_XFER_COLL_CHUNK_MULTI_RATIO_COLL_NAME,&prop_value);
               VRFY((status >= 0),"testing property list get succeeded");
               VRFY((prop_value == 0),"API to set MULTI-CHUNK COLLECTIVE IO with optimization succeeded");
            break;
            case API_MULTI_IND:
               status = H5Pget(xfer_plist,H5D_XFER_COLL_CHUNK_MULTI_RATIO_IND_NAME,&prop_value);
               VRFY((status >= 0),"testing property list get succeeded");
               VRFY((prop_value == 0),"API to set MULTI-CHUNK IO transferring to independent IO  succeeded");
            break;
            default:
            ;
       }
   }
#endif

  status = H5Dclose(dataset);
  VRFY((status >= 0),"");

  status = H5Pclose(xfer_plist);
  VRFY((status >= 0),"property list closed");

  status = H5Sclose(file_dataspace);
  VRFY((status >= 0),"");

  status = H5Fclose(file);
  VRFY((status >= 0),"");

  if (data_array1) HDfree(data_array1);


  /* Use collective read to verify the correctness of collective write. */

  /* allocate memory for data buffer */
  data_array1 = (int *)malloc(dims[0]*dims[1]*sizeof(int));
  VRFY((data_array1 != NULL), "data_array1 malloc succeeded");

  /* allocate memory for data buffer */
  data_origin1 = (int *)malloc(dims[0]*dims[1]*sizeof(int));
  VRFY((data_origin1 != NULL), "data_origin1 malloc succeeded");

  acc_plist = create_faccess_plist(comm, info, facc_type, use_gpfs);
  VRFY((acc_plist >= 0),"MPIO creation property list succeeded");

  file = H5Fopen(filename,H5F_ACC_RDONLY,acc_plist);
  VRFY((file >= 0),"H5Fcreate succeeded");

  status = H5Pclose(acc_plist);
  VRFY((status >= 0),"");

  /* open the collective dataset*/
  dataset = H5Dopen2(file, DSET_COLLECTIVE_CHUNK_NAME, H5P_DEFAULT);
  VRFY((dataset >= 0), "");

  /* set up dimensions of the slab this process accesses */
  ccslab_set(mpi_rank, mpi_size, start, count, stride, block, select_factor);

  /* obtain the file dataspace*/
  file_dataspace = H5Dget_space (dataset);
  VRFY((file_dataspace >= 0), "");

  status=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
  VRFY((status >= 0), "");

  /* fill dataset with test data */
  ccdataset_fill(start, stride,count,block, data_origin1);
  xfer_plist = H5Pcreate (H5P_DATASET_XFER);
  VRFY((xfer_plist >= 0),"");

  status = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
  VRFY((status>= 0),"MPIO collective transfer property succeeded");
  if(dxfer_coll_type == DXFER_INDEPENDENT_IO) {
     status = H5Pset_dxpl_mpio_collective_opt(xfer_plist,H5FD_MPIO_INDIVIDUAL_IO);
     VRFY((status>= 0),"set independent IO collectively succeeded");
  }


  status = H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, file_dataspace,
                   xfer_plist, data_array1);
  VRFY((status >=0),"dataset read succeeded");

  /* verify the read data with original expected data */
  status = ccdataset_vrfy(start, count, stride, block, data_array1, data_origin1);
  if (status) nerrors++;

  status = H5Pclose(xfer_plist);
  VRFY((status >= 0),"property list closed");

  /* close dataset collectively */
  status=H5Dclose(dataset);
  VRFY((status >= 0), "");

  /* release all IDs created */
  H5Sclose(file_dataspace);

  /* close the file collectively */
  H5Fclose(file);

  /* release data buffers */
  if (data_array1) free(data_array1);
  if (data_origin1) free(data_origin1);

}


/* Set up the selection */
static void
ccslab_set(int mpi_rank,
	   int mpi_size,
	   hsize_t start[],
	   hsize_t count[],
	   hsize_t stride[],
	   hsize_t block[],
	   int mode)
{

    switch (mode){

    case BYROW_CONT:
	/* Each process takes a slabs of rows. */
	block[0]  =  1;
	block[1]  =  1;
	stride[0] =  1;
	stride[1] =  1;
	count[0]  =  SPACE_DIM1;
	count[1]  =  SPACE_DIM2;
	start[0]  =  mpi_rank*count[0];
	start[1]  =  0;

	break;

    case BYROW_DISCONT:
	/* Each process takes several disjoint blocks. */
	block[0]  =  1;
	block[1]  =  1;
        stride[0] =  3;
        stride[1] =  3;
        count[0]  =  SPACE_DIM1/(stride[0]*block[0]);
        count[1]  =  (SPACE_DIM2)/(stride[1]*block[1]);
	start[0]  =  SPACE_DIM1*mpi_rank;
	start[1]  =  0;

	break;

    case BYROW_SELECTNONE:
	/* Each process takes a slabs of rows, there are
           no selections for the last process. */
	block[0]  =  1;
	block[1]  =  1;
	stride[0] =  1;
	stride[1] =  1;
	count[0]  =  ((mpi_rank >= MAX(1,(mpi_size-2)))?0:SPACE_DIM1);
	count[1]  =  SPACE_DIM2;
	start[0]  =  mpi_rank*count[0];
	start[1]  =  0;

	break;

    case BYROW_SELECTUNBALANCE:
      /* The first one-third of the number of processes only
         select top half of the domain, The rest will select the bottom
         half of the domain. */

        block[0]  = 1;
	count[0]  = 2;
        stride[0] = SPACE_DIM1*mpi_size/4+1;
        block[1]  = SPACE_DIM2;
        count[1]  = 1;
        start[1]  = 0;
        stride[1] = 1;
	if((mpi_rank *3)<(mpi_size*2)) start[0]  = mpi_rank;
	else start[0] = 1 + SPACE_DIM1*mpi_size/2 + (mpi_rank-2*mpi_size/3);
        break;

    case BYROW_SELECTINCHUNK:
      /* Each process will only select one chunk */

        block[0] = 1;
        count[0] = 1;
	start[0] = mpi_rank*SPACE_DIM1;
        stride[0]= 1;
	block[1] = SPACE_DIM2;
	count[1] = 1;
	stride[1]= 1;
	start[1] = 0;

        break;

    default:
	/* Unknown mode.  Set it to cover the whole dataset. */
	block[0]  = SPACE_DIM1*mpi_size;
	block[1]  = SPACE_DIM2;
	stride[0] = block[0];
	stride[1] = block[1];
	count[0]  = 1;
	count[1]  = 1;
	start[0]  = 0;
	start[1]  = 0;

	break;
    }
    if (VERBOSE_MED){
      printf("start[]=(%lu,%lu), count[]=(%lu,%lu), stride[]=(%lu,%lu), block[]=(%lu,%lu), total datapoints=%lu\n",
	(unsigned long)start[0], (unsigned long)start[1], (unsigned long)count[0], (unsigned long)count[1],
	(unsigned long)stride[0], (unsigned long)stride[1], (unsigned long)block[0], (unsigned long)block[1],
	(unsigned long)(block[0]*block[1]*count[0]*count[1]));
    }
}


/*
 * Fill the dataset with trivial data for testing.
 * Assume dimension rank is 2.
 */
static void
ccdataset_fill(hsize_t start[],
	       hsize_t stride[],
	       hsize_t count[],
	       hsize_t block[],
	       DATATYPE * dataset)
{
    DATATYPE *dataptr = dataset;
    DATATYPE *tmptr;
    hsize_t   i,j,k1,k2;

    /* put some trivial data in the data_array */
    tmptr = dataptr;

    /* assign the disjoint block (two-dimensional)data array value
       through the pointer */

    for (k1 = 0; k1 < count[0]; k1++) {
      for(i = 0;  i < block[0]; i++) {
        for(k2 = 0; k2 < count[1]; k2++) {
          for(j = 0;j < block[1]; j++) {

            dataptr  =  tmptr + ((start[0]+k1*stride[0]+i)*SPACE_DIM2+
				  start[1]+k2*stride[1]+j);

	    *dataptr = (DATATYPE)(k1+k2+i+j);
          }
        }
      }
    }
}

/*
 * Print the first block of the content of the dataset.
 */
static void
ccdataset_print(hsize_t start[],
		hsize_t block[],
		DATATYPE * dataset)

{
    DATATYPE *dataptr = dataset;
    hsize_t i, j;

    /* print the column heading */
    printf("Print only the first block of the dataset\n");
    printf("%-8s", "Cols:");
    for (j=0; j < block[1]; j++){
	printf("%3lu ", (unsigned long)(start[1]+j));
    }
    printf("\n");

    /* print the slab data */
    for (i=0; i < block[0]; i++){
	printf("Row %2lu: ", (unsigned long)(i+start[0]));
	for (j=0; j < block[1]; j++){
	    printf("%03d ", *dataptr++);
	}
	printf("\n");
    }
}


/*
 * Print the content of the dataset.
 */
static int
ccdataset_vrfy(hsize_t start[],
	       hsize_t count[],
	       hsize_t stride[],
	       hsize_t block[],
	       DATATYPE *dataset,
	       DATATYPE *original)
{
    hsize_t i, j,k1,k2;
    int vrfyerrs;
    DATATYPE *dataptr,*oriptr;

    /* print it if VERBOSE_MED */
    if (VERBOSE_MED) {
	printf("dataset_vrfy dumping:::\n");
	printf("start(%lu, %lu), count(%lu, %lu), stride(%lu, %lu), block(%lu, %lu)\n",
	    (unsigned long)start[0], (unsigned long)start[1], (unsigned long)count[0], (unsigned long)count[1],
	    (unsigned long)stride[0], (unsigned long)stride[1], (unsigned long)block[0], (unsigned long)block[1]);
	printf("original values:\n");
	ccdataset_print(start, block, original);
	printf("compared values:\n");
	ccdataset_print(start, block, dataset);
    }

    vrfyerrs = 0;

    for (k1 = 0; k1 < count[0];k1++) {
      for(i = 0;i < block[0];i++) {
        for(k2 = 0; k2<count[1];k2++) {
          for(j=0;j<block[1];j++) {

             dataptr = dataset + ((start[0]+k1*stride[0]+i)*SPACE_DIM2+
			           start[1]+k2*stride[1]+j);
	     oriptr =  original + ((start[0]+k1*stride[0]+i)*SPACE_DIM2+
			            start[1]+k2*stride[1]+j);

	    if (*dataptr != *oriptr){
		if (vrfyerrs++ < MAX_ERR_REPORT || VERBOSE_MED){
		    printf("Dataset Verify failed at [%lu][%lu]: expect %d, got %d\n",
			(unsigned long)i, (unsigned long)j,
	     	    	*(original), *(dataset));
		}
	    }
	  }
	}
      }
    }
    if (vrfyerrs > MAX_ERR_REPORT && !VERBOSE_MED)
	printf("[more errors ...]\n");
    if (vrfyerrs)
	printf("%d errors found in ccdataset_vrfy\n", vrfyerrs);
    return(vrfyerrs);
}
