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
 * Example of using the parallel HDF5 library to access datasets.
 * Last revised: April 24, 2001.
 *
 * This program contains two parts.  In the first part, the mpi processes
 * collectively create a new parallel HDF5 file and create two fixed
 * dimension datasets in it.  Then each process writes a hyperslab into
 * each dataset in an independent mode.  All processes collectively
 * close the datasets and the file.
 * In the second part, the processes collectively open the created file
 * and the two datasets in it.  Then each process reads a hyperslab from
 * each dataset in an independent mode and prints them out.
 * All processes collectively close the datasets and the file.
 *
 * The need of requirement of parallel file prefix is that in general
 * the current working directory in which compiling is done, is not suitable
 * for parallel I/O and there is no standard pathname for parallel file
 * systems.  In some cases, the parallel file name may even needs some
 * parallel file type prefix such as: "pfs:/GF/...".  Therefore, this
 * example requires an explicite parallel file prefix.  See the usage
 * for more detail.
 */

#include <assert.h>
#include "hdf5.h"
#include <string.h>
#include <stdlib.h>

#ifdef H5_HAVE_PARALLEL
/* Temporary source code */
#define FAIL -1
/* temporary code end */

/* Define some handy debugging shorthands, routines, ... */
/* debugging tools */
#define MESG(x)\
	if (verbose) printf("%s\n", x);\

#define MPI_BANNER(mesg)\
    {printf("--------------------------------\n");\
    printf("Proc %d: ", mpi_rank); \
    printf("*** %s\n", mesg);\
    printf("--------------------------------\n");}

#define SYNC(comm)\
    {MPI_BANNER("doing a SYNC"); MPI_Barrier(comm); MPI_BANNER("SYNC DONE");}
/* End of Define some handy debugging shorthands, routines, ... */

/* Constants definitions */
/* 24 is a multiple of 2, 3, 4, 6, 8, 12.  Neat for parallel tests. */
#define SPACE1_DIM1	24
#define SPACE1_DIM2	24
#define SPACE1_RANK	2
#define DATASETNAME1	"Data1"
#define DATASETNAME2	"Data2"
#define DATASETNAME3	"Data3"
/* hyperslab layout styles */
#define BYROW		1	/* divide into slabs of rows */
#define BYCOL		2	/* divide into blocks of columns */

#define PARAPREFIX	"HDF5_PARAPREFIX"	/* file prefix environment variable name */


/* dataset data type.  Int's can be easily octo dumped. */
typedef int DATATYPE;

/* global variables */
int nerrors = 0;				/* errors count */
#ifndef PATH_MAX
#define PATH_MAX    512
#endif  /* !PATH_MAX */
char    testfiles[2][PATH_MAX];


int mpi_size, mpi_rank;				/* mpi variables */

/* option flags */
int verbose = 0;			/* verbose, default as no. */
int doread=1;				/* read test */
int dowrite=1;				/* write test */
int docleanup=1;			/* cleanup */

/* Prototypes */
void slab_set(hsize_t start[], hsize_t count[], hsize_t stride[], int mode);
void dataset_fill(hsize_t start[], hsize_t count[], hsize_t stride[], DATATYPE * dataset);
void dataset_print(hsize_t start[], hsize_t count[], hsize_t stride[], DATATYPE * dataset);
int dataset_vrfy(hsize_t start[], hsize_t count[], hsize_t stride[], DATATYPE *dataset, DATATYPE *original);
void phdf5writeInd(char *filename);
void phdf5readInd(char *filename);
void phdf5writeAll(char *filename);
void phdf5readAll(char *filename);
void test_split_comm_access(char filenames[][PATH_MAX]);
int parse_options(int argc, char **argv);
void usage(void);
int mkfilenames(char *prefix);
void cleanup(void);


/*
 * Setup the dimensions of the hyperslab.
 * Two modes--by rows or by columns.
 * Assume dimension rank is 2.
 */
void
slab_set(hsize_t start[], hsize_t count[], hsize_t stride[], int mode)
{
    switch (mode){
    case BYROW:
	/* Each process takes a slabs of rows. */
	stride[0] = 1;
	stride[1] = 1;
	count[0] = SPACE1_DIM1/mpi_size;
	count[1] = SPACE1_DIM2;
	start[0] = mpi_rank*count[0];
	start[1] = 0;
	break;
    case BYCOL:
	/* Each process takes a block of columns. */
	stride[0] = 1;
	stride[1] = 1;
	count[0] = SPACE1_DIM1;
	count[1] = SPACE1_DIM2/mpi_size;
	start[0] = 0;
	start[1] = mpi_rank*count[1];
	break;
    default:
	/* Unknown mode.  Set it to cover the whole dataset. */
	printf("unknown slab_set mode (%d)\n", mode);
	stride[0] = 1;
	stride[1] = 1;
	count[0] = SPACE1_DIM1;
	count[1] = SPACE1_DIM2;
	start[0] = 0;
	start[1] = 0;
	break;
    }
}


/*
 * Fill the dataset with trivial data for testing.
 * Assume dimension rank is 2 and data is stored contiguous.
 */
void
dataset_fill(hsize_t start[], hsize_t count[], hsize_t stride[], DATATYPE * dataset)
{
    DATATYPE *dataptr = dataset;
    hsize_t i, j;

    /* put some trivial data in the data_array */
    for (i=0; i < count[0]; i++){
	for (j=0; j < count[1]; j++){
	    *dataptr++ = (i*stride[0]+start[0])*100 + (j*stride[1]+start[1]+1);
	}
    }
}


/*
 * Print the content of the dataset.
 */
void dataset_print(hsize_t start[], hsize_t count[], hsize_t stride[], DATATYPE * dataset)
{
    DATATYPE *dataptr = dataset;
    hsize_t i, j;

    /* print the slab read */
    for (i=0; i < count[0]; i++){
	printf("Row %lu: ", (unsigned long)(i*stride[0]+start[0]));
	for (j=0; j < count[1]; j++){
	    printf("%03d ", *dataptr++);
	}
	printf("\n");
    }
}


/*
 * Print the content of the dataset.
 */
int dataset_vrfy(hsize_t start[], hsize_t count[], hsize_t stride[], DATATYPE *dataset, DATATYPE *original)
{
#define MAX_ERR_REPORT	10		/* Maximum number of errors reported */

    hsize_t i, j;
    int nerr;

    /* print it if verbose */
    if (verbose)
	dataset_print(start, count, stride, dataset);

    nerr = 0;
    for (i=0; i < count[0]; i++){
	for (j=0; j < count[1]; j++){
	    if (*dataset++ != *original++){
		nerr++;
		if (nerr <= MAX_ERR_REPORT){
		    printf("Dataset Verify failed at [%lu][%lu](row %lu, col %lu): expect %d, got %d\n",
			(unsigned long)i, (unsigned long)j,
			(unsigned long)(i*stride[0]+start[0]), (unsigned long)(j*stride[1]+start[1]),
			*(dataset-1), *(original-1));
		}
	    }
	}
    }
    if (nerr > MAX_ERR_REPORT)
	printf("[more errors ...]\n");
    if (nerr)
	printf("%d errors found in dataset_vrfy\n", nerr);
    return(nerr);
}


/*
 * Example of using the parallel HDF5 library to create two datasets
 * in one HDF5 files with parallel MPIO access support.
 * The Datasets are of sizes (number-of-mpi-processes x DIM1) x DIM2.
 * Each process controls only a slab of size DIM1 x DIM2 within each
 * dataset.
 */

void
phdf5writeInd(char *filename)
{
    hid_t fid1;			/* HDF5 file IDs */
    hid_t acc_tpl1;		/* File access templates */
    hid_t sid1;   		/* Dataspace ID */
    hid_t file_dataspace;	/* File dataspace ID */
    hid_t mem_dataspace;	/* memory dataspace ID */
    hid_t dataset1, dataset2;	/* Dataset ID */
    hsize_t dims1[SPACE1_RANK] =
	{SPACE1_DIM1,SPACE1_DIM2};	/* dataspace dim sizes */
    DATATYPE data_array1[SPACE1_DIM1][SPACE1_DIM2];	/* data buffer */

    hsize_t start[SPACE1_RANK];			/* for hyperslab setting */
    hsize_t count[SPACE1_RANK], stride[SPACE1_RANK];	/* for hyperslab setting */

    herr_t ret;         	/* Generic return value */

    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;

    if (verbose)
	printf("Independent write test on file %s\n", filename);

    /* -------------------
     * START AN HDF5 FILE
     * -------------------*/
    /* setup file access template with parallel IO access. */
    acc_tpl1 = H5Pcreate (H5P_FILE_ACCESS);
    assert(acc_tpl1 != FAIL);
    MESG("H5Pcreate access succeed");
    /* set Parallel access with communicator */
    ret = H5Pset_fapl_mpio(acc_tpl1, comm, info);
    assert(ret != FAIL);
    MESG("H5Pset_fapl_mpio succeed");

    /* create the file collectively */
    fid1 = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, acc_tpl1);
    assert(fid1 != FAIL);
    MESG("H5Fcreate succeed");

    /* Release file-access template */
    ret = H5Pclose(acc_tpl1);
    assert(ret != FAIL);


    /* --------------------------
     * Define the dimensions of the overall datasets
     * and the slabs local to the MPI process.
     * ------------------------- */
    /* setup dimensionality object */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    assert (sid1 != FAIL);
    MESG("H5Screate_simple succeed");


    /* create a dataset collectively */
    dataset1 = H5Dcreate2(fid1, DATASETNAME1, H5T_NATIVE_INT, sid1,
			H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    assert(dataset1 != FAIL);
    MESG("H5Dcreate2 succeed");

    /* create another dataset collectively */
    dataset2 = H5Dcreate2(fid1, DATASETNAME2, H5T_NATIVE_INT, sid1,
			H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    assert(dataset2 != FAIL);
    MESG("H5Dcreate2 succeed");



    /* set up dimensions of the slab this process accesses */
    start[0] = mpi_rank*SPACE1_DIM1/mpi_size;
    start[1] = 0;
    count[0] = SPACE1_DIM1/mpi_size;
    count[1] = SPACE1_DIM2;
    stride[0] = 1;
    stride[1] =1;
if (verbose)
    printf("start[]=(%lu,%lu), count[]=(%lu,%lu), total datapoints=%lu\n",
	(unsigned long)start[0], (unsigned long)start[1],
        (unsigned long)count[0], (unsigned long)count[1],
        (unsigned long)(count[0]*count[1]));

    /* put some trivial data in the data_array */
    dataset_fill(start, count, stride, &data_array1[0][0]);
    MESG("data_array initialized");

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);
    assert(file_dataspace != FAIL);
    MESG("H5Dget_space succeed");
    ret=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride,
	    count, NULL);
    assert(ret != FAIL);
    MESG("H5Sset_hyperslab succeed");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (SPACE1_RANK, count, NULL);
    assert (mem_dataspace != FAIL);

    /* write data independently */
    ret = H5Dwrite(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    H5P_DEFAULT, data_array1);
    assert(ret != FAIL);
    MESG("H5Dwrite succeed");

    /* write data independently */
    ret = H5Dwrite(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    H5P_DEFAULT, data_array1);
    assert(ret != FAIL);
    MESG("H5Dwrite succeed");

    /* release dataspace ID */
    H5Sclose(file_dataspace);

    /* close dataset collectively */
    ret=H5Dclose(dataset1);
    assert(ret != FAIL);
    MESG("H5Dclose1 succeed");
    ret=H5Dclose(dataset2);
    assert(ret != FAIL);
    MESG("H5Dclose2 succeed");

    /* release all IDs created */
    H5Sclose(sid1);

    /* close the file collectively */
    H5Fclose(fid1);
}

/* Example of using the parallel HDF5 library to read a dataset */
void
phdf5readInd(char *filename)
{
    hid_t fid1;			/* HDF5 file IDs */
    hid_t acc_tpl1;		/* File access templates */
    hid_t file_dataspace;	/* File dataspace ID */
    hid_t mem_dataspace;	/* memory dataspace ID */
    hid_t dataset1, dataset2;	/* Dataset ID */
    DATATYPE data_array1[SPACE1_DIM1][SPACE1_DIM2];	/* data buffer */
    DATATYPE data_origin1[SPACE1_DIM1][SPACE1_DIM2];	/* expected data buffer */

    hsize_t start[SPACE1_RANK];			/* for hyperslab setting */
    hsize_t count[SPACE1_RANK], stride[SPACE1_RANK];	/* for hyperslab setting */

    herr_t ret;         	/* Generic return value */

    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;

    if (verbose)
	printf("Independent read test on file %s\n", filename);

    /* setup file access template */
    acc_tpl1 = H5Pcreate (H5P_FILE_ACCESS);
    assert(acc_tpl1 != FAIL);
    /* set Parallel access with communicator */
    ret = H5Pset_fapl_mpio(acc_tpl1, comm, info);
    assert(ret != FAIL);


    /* open the file collectively */
    fid1=H5Fopen(filename,H5F_ACC_RDWR,acc_tpl1);
    assert(fid1 != FAIL);

    /* Release file-access template */
    ret=H5Pclose(acc_tpl1);
    assert(ret != FAIL);

    /* open the dataset1 collectively */
    dataset1 = H5Dopen2(fid1, DATASETNAME1, H5P_DEFAULT);
    assert(dataset1 != FAIL);

    /* open another dataset collectively */
    dataset2 = H5Dopen2(fid1, DATASETNAME1, H5P_DEFAULT);
    assert(dataset2 != FAIL);


    /* set up dimensions of the slab this process accesses */
    start[0] = mpi_rank*SPACE1_DIM1/mpi_size;
    start[1] = 0;
    count[0] = SPACE1_DIM1/mpi_size;
    count[1] = SPACE1_DIM2;
    stride[0] = 1;
    stride[1] =1;
if (verbose)
    printf("start[]=(%lu,%lu), count[]=(%lu,%lu), total datapoints=%lu\n",
        (unsigned long)start[0], (unsigned long)start[1],
        (unsigned long)count[0], (unsigned long)count[1],
        (unsigned long)(count[0]*count[1]));

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);
    assert(file_dataspace != FAIL);
    ret=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride,
	    count, NULL);
    assert(ret != FAIL);

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (SPACE1_RANK, count, NULL);
    assert (mem_dataspace != FAIL);

    /* fill dataset with test data */
    dataset_fill(start, count, stride, &data_origin1[0][0]);

    /* read data independently */
    ret = H5Dread(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    H5P_DEFAULT, data_array1);
    assert(ret != FAIL);

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, &data_array1[0][0], &data_origin1[0][0]);
    assert(ret != FAIL);

    /* read data independently */
    ret = H5Dread(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    H5P_DEFAULT, data_array1);
    assert(ret != FAIL);

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, &data_array1[0][0], &data_origin1[0][0]);
    assert(ret == 0);

    /* close dataset collectively */
    ret=H5Dclose(dataset1);
    assert(ret != FAIL);
    ret=H5Dclose(dataset2);
    assert(ret != FAIL);

    /* release all IDs created */
    H5Sclose(file_dataspace);

    /* close the file collectively */
    H5Fclose(fid1);
}


/*
 * Example of using the parallel HDF5 library to create two datasets
 * in one HDF5 file with collective parallel access support.
 * The Datasets are of sizes (number-of-mpi-processes x DIM1) x DIM2.
 * Each process controls only a slab of size DIM1 x DIM2 within each
 * dataset. [Note: not so yet.  Datasets are of sizes DIM1xDIM2 and
 * each process controls a hyperslab within.]
 */

void
phdf5writeAll(char *filename)
{
    hid_t fid1;			/* HDF5 file IDs */
    hid_t acc_tpl1;		/* File access templates */
    hid_t xfer_plist;		/* Dataset transfer properties list */
    hid_t sid1;   		/* Dataspace ID */
    hid_t file_dataspace;	/* File dataspace ID */
    hid_t mem_dataspace;	/* memory dataspace ID */
    hid_t dataset1, dataset2;	/* Dataset ID */
    hsize_t dims1[SPACE1_RANK] =
	{SPACE1_DIM1,SPACE1_DIM2};	/* dataspace dim sizes */
    DATATYPE data_array1[SPACE1_DIM1][SPACE1_DIM2];	/* data buffer */

    hsize_t start[SPACE1_RANK];			/* for hyperslab setting */
    hsize_t count[SPACE1_RANK], stride[SPACE1_RANK];	/* for hyperslab setting */

    herr_t ret;         	/* Generic return value */

    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;

    if (verbose)
	printf("Collective write test on file %s\n", filename);

    /* -------------------
     * START AN HDF5 FILE
     * -------------------*/
    /* setup file access template with parallel IO access. */
    acc_tpl1 = H5Pcreate (H5P_FILE_ACCESS);
    assert(acc_tpl1 != FAIL);
    MESG("H5Pcreate access succeed");
    /* set Parallel access with communicator */
    ret = H5Pset_fapl_mpio(acc_tpl1, comm, info);
    assert(ret != FAIL);
    MESG("H5Pset_fapl_mpio succeed");

    /* create the file collectively */
    fid1=H5Fcreate(filename,H5F_ACC_TRUNC,H5P_DEFAULT,acc_tpl1);
    assert(fid1 != FAIL);
    MESG("H5Fcreate succeed");

    /* Release file-access template */
    ret=H5Pclose(acc_tpl1);
    assert(ret != FAIL);


    /* --------------------------
     * Define the dimensions of the overall datasets
     * and create the dataset
     * ------------------------- */
    /* setup dimensionality object */
    sid1 = H5Screate_simple (SPACE1_RANK, dims1, NULL);
    assert (sid1 != FAIL);
    MESG("H5Screate_simple succeed");


    /* create a dataset collectively */
    dataset1 = H5Dcreate2(fid1, DATASETNAME1, H5T_NATIVE_INT, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    assert(dataset1 != FAIL);
    MESG("H5Dcreate2 succeed");

    /* create another dataset collectively */
    dataset2 = H5Dcreate2(fid1, DATASETNAME2, H5T_NATIVE_INT, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    assert(dataset2 != FAIL);
    MESG("H5Dcreate2 2 succeed");

    /*
     * Set up dimensions of the slab this process accesses.
     */

    /* Dataset1: each process takes a block of rows. */
    slab_set(start, count, stride, BYROW);
if (verbose)
    printf("start[]=(%lu,%lu), count[]=(%lu,%lu), total datapoints=%lu\n",
	(unsigned long)start[0], (unsigned long)start[1],
        (unsigned long)count[0], (unsigned long)count[1],
        (unsigned long)(count[0]*count[1]));

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);
    assert(file_dataspace != FAIL);
    MESG("H5Dget_space succeed");
    ret=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride,
	    count, NULL);
    assert(ret != FAIL);
    MESG("H5Sset_hyperslab succeed");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (SPACE1_RANK, count, NULL);
    assert (mem_dataspace != FAIL);

    /* fill the local slab with some trivial data */
    dataset_fill(start, count, stride, &data_array1[0][0]);
    MESG("data_array initialized");
    if (verbose){
	MESG("data_array created");
	dataset_print(start, count, stride, &data_array1[0][0]);
    }

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    assert(xfer_plist != FAIL);
    ret=H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    assert(ret != FAIL);
    MESG("H5Pcreate xfer succeed");

    /* write data collectively */
    ret = H5Dwrite(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);
    assert(ret != FAIL);
    MESG("H5Dwrite succeed");

    /* release all temporary handles. */
    /* Could have used them for dataset2 but it is cleaner */
    /* to create them again.*/
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);

    /* Dataset2: each process takes a block of columns. */
    slab_set(start, count, stride, BYCOL);
if (verbose)
    printf("start[]=(%lu,%lu), count[]=(%lu,%lu), total datapoints=%lu\n",
	(unsigned long)start[0], (unsigned long)start[1],
        (unsigned long)count[0], (unsigned long)count[1],
        (unsigned long)(count[0]*count[1]));

    /* put some trivial data in the data_array */
    dataset_fill(start, count, stride, &data_array1[0][0]);
    MESG("data_array initialized");
    if (verbose){
	MESG("data_array created");
	dataset_print(start, count, stride, &data_array1[0][0]);
    }

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);
    assert(file_dataspace != FAIL);
    MESG("H5Dget_space succeed");
    ret=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride,
	    count, NULL);
    assert(ret != FAIL);
    MESG("H5Sset_hyperslab succeed");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (SPACE1_RANK, count, NULL);
    assert (mem_dataspace != FAIL);

    /* fill the local slab with some trivial data */
    dataset_fill(start, count, stride, &data_array1[0][0]);
    MESG("data_array initialized");
    if (verbose){
	MESG("data_array created");
	dataset_print(start, count, stride, &data_array1[0][0]);
    }

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    assert(xfer_plist != FAIL);
    ret=H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    assert(ret != FAIL);
    MESG("H5Pcreate xfer succeed");

    /* write data independently */
    ret = H5Dwrite(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);
    assert(ret != FAIL);
    MESG("H5Dwrite succeed");

    /* release all temporary handles. */
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);


    /*
     * All writes completed.  Close datasets collectively
     */
    ret=H5Dclose(dataset1);
    assert(ret != FAIL);
    MESG("H5Dclose1 succeed");
    ret=H5Dclose(dataset2);
    assert(ret != FAIL);
    MESG("H5Dclose2 succeed");

    /* release all IDs created */
    H5Sclose(sid1);

    /* close the file collectively */
    H5Fclose(fid1);
}

/*
 * Example of using the parallel HDF5 library to read two datasets
 * in one HDF5 file with collective parallel access support.
 * The Datasets are of sizes (number-of-mpi-processes x DIM1) x DIM2.
 * Each process controls only a slab of size DIM1 x DIM2 within each
 * dataset. [Note: not so yet.  Datasets are of sizes DIM1xDIM2 and
 * each process controls a hyperslab within.]
 */

void
phdf5readAll(char *filename)
{
    hid_t fid1;			/* HDF5 file IDs */
    hid_t acc_tpl1;		/* File access templates */
    hid_t xfer_plist;		/* Dataset transfer properties list */
    hid_t file_dataspace;	/* File dataspace ID */
    hid_t mem_dataspace;	/* memory dataspace ID */
    hid_t dataset1, dataset2;	/* Dataset ID */
    DATATYPE data_array1[SPACE1_DIM1][SPACE1_DIM2];	/* data buffer */
    DATATYPE data_origin1[SPACE1_DIM1][SPACE1_DIM2];	/* expected data buffer */

    hsize_t start[SPACE1_RANK];			/* for hyperslab setting */
    hsize_t count[SPACE1_RANK], stride[SPACE1_RANK];	/* for hyperslab setting */

    herr_t ret;         	/* Generic return value */

    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;

    if (verbose)
	printf("Collective read test on file %s\n", filename);

    /* -------------------
     * OPEN AN HDF5 FILE
     * -------------------*/
    /* setup file access template with parallel IO access. */
    acc_tpl1 = H5Pcreate (H5P_FILE_ACCESS);
    assert(acc_tpl1 != FAIL);
    MESG("H5Pcreate access succeed");
    /* set Parallel access with communicator */
    ret = H5Pset_fapl_mpio(acc_tpl1, comm, info);
    assert(ret != FAIL);
    MESG("H5Pset_fapl_mpio succeed");

    /* open the file collectively */
    fid1=H5Fopen(filename,H5F_ACC_RDWR,acc_tpl1);
    assert(fid1 != FAIL);
    MESG("H5Fopen succeed");

    /* Release file-access template */
    ret=H5Pclose(acc_tpl1);
    assert(ret != FAIL);


    /* --------------------------
     * Open the datasets in it
     * ------------------------- */
    /* open the dataset1 collectively */
    dataset1 = H5Dopen2(fid1, DATASETNAME1, H5P_DEFAULT);
    assert(dataset1 != FAIL);
    MESG("H5Dopen2 succeed");

    /* open another dataset collectively */
    dataset2 = H5Dopen2(fid1, DATASETNAME1, H5P_DEFAULT);
    assert(dataset2 != FAIL);
    MESG("H5Dopen2 2 succeed");

    /*
     * Set up dimensions of the slab this process accesses.
     */

    /* Dataset1: each process takes a block of columns. */
    slab_set(start, count, stride, BYCOL);
if (verbose)
    printf("start[]=(%lu,%lu), count[]=(%lu,%lu), total datapoints=%lu\n",
	(unsigned long)start[0], (unsigned long)start[1],
        (unsigned long)count[0], (unsigned long)count[1],
        (unsigned long)(count[0]*count[1]));

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);
    assert(file_dataspace != FAIL);
    MESG("H5Dget_space succeed");
    ret=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride,
	    count, NULL);
    assert(ret != FAIL);
    MESG("H5Sset_hyperslab succeed");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (SPACE1_RANK, count, NULL);
    assert (mem_dataspace != FAIL);

    /* fill dataset with test data */
    dataset_fill(start, count, stride, &data_origin1[0][0]);
    MESG("data_array initialized");
    if (verbose){
	MESG("data_array created");
	dataset_print(start, count, stride, &data_array1[0][0]);
    }

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    assert(xfer_plist != FAIL);
    ret=H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    assert(ret != FAIL);
    MESG("H5Pcreate xfer succeed");

    /* read data collectively */
    ret = H5Dread(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);
    assert(ret != FAIL);
    MESG("H5Dread succeed");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, &data_array1[0][0], &data_origin1[0][0]);
    assert(ret != FAIL);

    /* release all temporary handles. */
    /* Could have used them for dataset2 but it is cleaner */
    /* to create them again.*/
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);

    /* Dataset2: each process takes a block of rows. */
    slab_set(start, count, stride, BYROW);
if (verbose)
    printf("start[]=(%lu,%lu), count[]=(%lu,%lu), total datapoints=%lu\n",
	(unsigned long)start[0], (unsigned long)start[1],
        (unsigned long)count[0], (unsigned long)count[1],
        (unsigned long)(count[0]*count[1]));

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);
    assert(file_dataspace != FAIL);
    MESG("H5Dget_space succeed");
    ret=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride,
	    count, NULL);
    assert(ret != FAIL);
    MESG("H5Sset_hyperslab succeed");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (SPACE1_RANK, count, NULL);
    assert (mem_dataspace != FAIL);

    /* fill dataset with test data */
    dataset_fill(start, count, stride, &data_origin1[0][0]);
    MESG("data_array initialized");
    if (verbose){
	MESG("data_array created");
	dataset_print(start, count, stride, &data_array1[0][0]);
    }

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    assert(xfer_plist != FAIL);
    ret=H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    assert(ret != FAIL);
    MESG("H5Pcreate xfer succeed");

    /* read data independently */
    ret = H5Dread(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);
    assert(ret != FAIL);
    MESG("H5Dread succeed");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, &data_array1[0][0], &data_origin1[0][0]);
    assert(ret != FAIL);

    /* release all temporary handles. */
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);


    /*
     * All reads completed.  Close datasets collectively
     */
    ret=H5Dclose(dataset1);
    assert(ret != FAIL);
    MESG("H5Dclose1 succeed");
    ret=H5Dclose(dataset2);
    assert(ret != FAIL);
    MESG("H5Dclose2 succeed");

    /* close the file collectively */
    H5Fclose(fid1);
}

/*
 * test file access by communicator besides COMM_WORLD.
 * Split COMM_WORLD into two, one (even_comm) contains the original
 * processes of even ranks.  The other (odd_comm) contains the original
 * processes of odd ranks.  Processes in even_comm creates a file, then
 * cloose it, using even_comm.  Processes in old_comm just do a barrier
 * using odd_comm.  Then they all do a barrier using COMM_WORLD.
 * If the file creation and cloose does not do correct collective action
 * according to the communicator argument, the processes will freeze up
 * sooner or later due to barrier mixed up.
 */
void
test_split_comm_access(char filenames[][PATH_MAX])
{
    MPI_Comm comm;
    MPI_Info info = MPI_INFO_NULL;
    int color, mrc;
    int newrank, newprocs;
    hid_t fid;			/* file IDs */
    hid_t acc_tpl;		/* File access properties */
    herr_t ret;			/* generic return value */

    if (verbose)
	printf("Independent write test on file %s %s\n",
	    filenames[0], filenames[1]);

    color = mpi_rank%2;
    mrc = MPI_Comm_split (MPI_COMM_WORLD, color, mpi_rank, &comm);
    assert(mrc==MPI_SUCCESS);
    MPI_Comm_size(comm,&newprocs);
    MPI_Comm_rank(comm,&newrank);

    if (color){
	/* odd-rank processes */
	mrc = MPI_Barrier(comm);
	assert(mrc==MPI_SUCCESS);
    }else{
	/* even-rank processes */
	/* setup file access template */
	acc_tpl = H5Pcreate (H5P_FILE_ACCESS);
	assert(acc_tpl != FAIL);

	/* set Parallel access with communicator */
	ret = H5Pset_fapl_mpio(acc_tpl, comm, info);
	assert(ret != FAIL);

	/* create the file collectively */
	fid=H5Fcreate(filenames[color],H5F_ACC_TRUNC,H5P_DEFAULT,acc_tpl);
	assert(fid != FAIL);
	MESG("H5Fcreate succeed");

	/* Release file-access template */
	ret=H5Pclose(acc_tpl);
	assert(ret != FAIL);

	ret=H5Fclose(fid);
	assert(ret != FAIL);
    }
    if (mpi_rank == 0){
	mrc = MPI_File_delete(filenames[color], info);
	assert(mrc==MPI_SUCCESS);
    }
}

/*
 * Show command usage
 */
void
usage(void)
{
    printf("Usage: testphdf5 [-f <prefix>] [-r] [-w] [-v]\n");
    printf("\t-f\tfile prefix for parallel test files.\n");
    printf("\t  \te.g. pfs:/PFS/myname\n");
    printf("\t  \tcan be set via $" PARAPREFIX ".\n");
    printf("\t  \tDefault is current directory.\n");
    printf("\t-c\tno cleanup\n");
    printf("\t-r\tno read\n");
    printf("\t-w\tno write\n");
    printf("\t-v\tverbose on\n");
    printf("\tdefault do write then read\n");
    printf("\n");
}


/*
 * compose the test filename with the prefix supplied.
 * return code: 0 if no error
 *              1 otherwise.
 */
int
mkfilenames(char *prefix)
{
    int i, n;
    size_t strsize;

    /* filename will be prefix/ParaEgN.h5 where N is 0 to 9. */
    /* So, string must be big enough to hold the prefix, / and 10 more chars */
    /* and the terminating null. */
    strsize = strlen(prefix) + 12;
    if (strsize > PATH_MAX){
	printf("File prefix too long;  Use a short path name.\n");
	return(1);
    }
    n = sizeof(testfiles)/sizeof(testfiles[0]);
    if (n > 9){
	printf("Warning: Too many entries in testfiles. "
	    "Need to adjust the code to accommodate the large size.\n");
    }
    for (i=0; i<n; i++){
	sprintf(testfiles[i], "%s/ParaEg%d.h5", prefix, i);
    }
    return(0);

}


/*
 * parse the command line options
 */
int
parse_options(int argc, char **argv){
    int i, n;

    /* initialize testfiles to nulls */
    n = sizeof(testfiles)/sizeof(testfiles[0]);
    for (i=0; i<n; i++){
	testfiles[i][0] = '\0';
    }

    while (--argc){
	if (**(++argv) != '-'){
	    break;
	}else{
	    switch(*(*argv+1)){
		case 'f':   ++argv;
			    if (--argc < 1){
				usage();
				nerrors++;
				return(1);
			    }
			    if (mkfilenames(*argv)){
				nerrors++;
				return(1);
			    }
			    break;
		case 'c':   docleanup = 0;	/* no cleanup */
			    break;
		case 'r':   doread = 0;
			    break;
		case 'w':   dowrite = 0;
			    break;
		case 'v':   verbose = 1;
			    break;
		default:    usage();
			    nerrors++;
			    return(1);
	    }
	}
    }

    /* check the file prefix */
    if (testfiles[0][0] == '\0'){
	/* try get it from environment variable HDF5_PARAPREFIX */
	char *env;
	char *env_default = ".";	/* default to current directory */
	if ((env=getenv(PARAPREFIX))==NULL){
	    env = env_default;
	}
	mkfilenames(env);
    }
    return(0);
}


/*
 * cleanup test files created
 */
void
cleanup(void)
{
    int i, n;

    n = sizeof(testfiles)/sizeof(testfiles[0]);
    for (i=0; i<n; i++){
	MPI_File_delete(testfiles[i], MPI_INFO_NULL);
    }
}


/* Main Program */
int
main(int argc, char **argv)
{
    int mpi_namelen;
    char mpi_name[MPI_MAX_PROCESSOR_NAME];
    int i, n;

    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD,&mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);
    MPI_Get_processor_name(mpi_name,&mpi_namelen);
    /* Make sure datasets can be divided into equal chunks by the processes */
    if ((SPACE1_DIM1 % mpi_size) || (SPACE1_DIM2 % mpi_size)){
	printf("DIM1(%d) and DIM2(%d) must be multiples of processes (%d)\n",
	    SPACE1_DIM1, SPACE1_DIM2, mpi_size);
	nerrors++;
	goto finish;
    }

    if (parse_options(argc, argv) != 0)
	goto finish;

    /* show test file names */
    if (mpi_rank == 0){
	n = sizeof(testfiles)/sizeof(testfiles[0]);
	printf("Parallel test files are:\n");
	for (i=0; i<n; i++){
	    printf("   %s\n", testfiles[i]);
	}
    }

    if (dowrite){
	MPI_BANNER("testing PHDF5 dataset using split communicators...");
	test_split_comm_access(testfiles);
	MPI_BANNER("testing PHDF5 dataset independent write...");
	phdf5writeInd(testfiles[0]);
	MPI_BANNER("testing PHDF5 dataset collective write...");
	phdf5writeAll(testfiles[1]);
    }
    if (doread){
	MPI_BANNER("testing PHDF5 dataset independent read...");
	phdf5readInd(testfiles[0]);
	MPI_BANNER("testing PHDF5 dataset collective read...");
	phdf5readAll(testfiles[1]);
    }

    if (!(dowrite || doread)){
	usage();
	nerrors++;
    }

finish:
    if (mpi_rank == 0){		/* only process 0 reports */
	if (nerrors)
	    printf("***PHDF5 tests detected %d errors***\n", nerrors);
	else{
	    printf("===================================\n");
	    printf("PHDF5 tests finished with no errors\n");
	    printf("===================================\n");
	}
    }
    if (docleanup)
	cleanup();
    MPI_Finalize();

    return(nerrors);
}

#else /* H5_HAVE_PARALLEL */
/* dummy program since H5_HAVE_PARALLE is not configured in */
int
main(void)
{
printf("No PHDF5 example because parallel is not configured in\n");
return(0);
}
#endif /* H5_HAVE_PARALLEL */
