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
 * This verifies if the storage space allocation methods are compatible between
 * serial and parallel modes.
 *
 * Created by: Christian Chilan and Albert Cheng
 * Date: 2006/05/25
 */

#include "testphdf5.h"
static int	mpi_size, mpi_rank;

#define DSET_NAME "ExtendibleArray"
#define CHUNK_SIZE	1000		/* #elements per chunk */
#define CHUNK_FACTOR	200     /* default dataset size in terms of chunks */
#define CLOSE           1
#define NO_CLOSE        0

static MPI_Offset
get_filesize(const char *filename)
{
    int		mpierr;
    MPI_File	fd;
    MPI_Offset	filesize;
#ifndef H5_HAVE_MPI_GET_SIZE
    struct stat stat_buf;
#endif

#ifdef H5_HAVE_MPI_GET_SIZE
    mpierr = MPI_File_open(MPI_COMM_SELF, (char*)filename, MPI_MODE_RDONLY,
	MPI_INFO_NULL, &fd);
    VRFY((mpierr == MPI_SUCCESS), "");

    mpierr = MPI_File_get_size(fd, &filesize);
    VRFY((mpierr == MPI_SUCCESS), "");

    mpierr = MPI_File_close(&fd);
    VRFY((mpierr == MPI_SUCCESS), "");
#else
    /* Some systems (only SGI Altix Propack 4 so far) doesn't return correct
     * file size for MPI_File_get_size.  Use stat instead.
     */
    if((mpierr=stat(filename, &stat_buf))<0)
    VRFY((mpierr == MPI_SUCCESS), "");

    /* Hopefully this casting is safe */
    filesize = (MPI_Offset)(stat_buf.st_size);
#endif

    return(filesize);
}

typedef enum write_pattern {
    none,
    sec_last,
    all
} write_type;

typedef enum access_ {
    write_all,
    open_only,
    extend_only
} access_type;


/*
 * This creates a dataset serially with chunks, each of CHUNK_SIZE
 * elements. The allocation time is set to H5D_ALLOC_TIME_EARLY. Another
 * routine will open this in parallel for extension test.
 */
static void
create_chunked_dataset(const char *filename, int chunk_factor, write_type write_pattern)
{
    hid_t       file_id, dataset;                          /* handles */
    hid_t       dataspace,memspace;
    hid_t       cparms;
    hsize_t      dims[1];
    hsize_t      maxdims[1] = {H5S_UNLIMITED};

    hsize_t      chunk_dims[1] ={CHUNK_SIZE};
    hsize_t     count[1];
    hsize_t     stride[1];
    hsize_t     block[1];
    hsize_t     offset[1];            /* Selection offset within dataspace */
    /* Variables used in reading data back */
    char         buffer[CHUNK_SIZE];
    long         nchunks;
    herr_t       hrc;

    MPI_Offset  filesize,	    /* actual file size */
		est_filesize;	    /* estimated file size */

    /* set up MPI parameters */
    MPI_Comm_size(MPI_COMM_WORLD,&mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);

    /* Only MAINPROCESS should create the file.  Others just wait. */
    if (MAINPROCESS){
        nchunks=chunk_factor*mpi_size;
	dims[0]=nchunks*CHUNK_SIZE;
	/* Create the data space with unlimited dimensions. */
	dataspace = H5Screate_simple (1, dims, maxdims);
	VRFY((dataspace >= 0), "");

	memspace = H5Screate_simple(1, chunk_dims, NULL);
	VRFY((memspace >= 0), "");

	/* Create a new file. If file exists its contents will be overwritten. */
	file_id = H5Fcreate(h5_rmprefix(filename), H5F_ACC_TRUNC, H5P_DEFAULT,
		    H5P_DEFAULT);
	VRFY((file_id >= 0), "H5Fcreate");

	/* Modify dataset creation properties, i.e. enable chunking  */
	cparms = H5Pcreate(H5P_DATASET_CREATE);
	VRFY((cparms >= 0), "");

	hrc = H5Pset_alloc_time(cparms, H5D_ALLOC_TIME_EARLY);
	VRFY((hrc >= 0), "");

	hrc = H5Pset_chunk(cparms, 1, chunk_dims);
	VRFY((hrc >= 0), "");

	/* Create a new dataset within the file using cparms creation properties. */
	dataset = H5Dcreate2(file_id, DSET_NAME, H5T_NATIVE_UCHAR, dataspace, H5P_DEFAULT, cparms, H5P_DEFAULT);
	VRFY((dataset >= 0), "");

	if(write_pattern == sec_last) {
            HDmemset(buffer, 100, CHUNK_SIZE);

            count[0] = 1;
            stride[0] = 1;
            block[0] = chunk_dims[0];
            offset[0] = (nchunks-2)*chunk_dims[0];

            hrc = H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, stride, count, block);
                VRFY((hrc >= 0), "");

            /* Write sec_last chunk */
            hrc = H5Dwrite(dataset, H5T_NATIVE_UCHAR, memspace, dataspace, H5P_DEFAULT, buffer);
            VRFY((hrc >= 0), "H5Dwrite");
        } /* end if */

	/* Close resources */
	hrc = H5Dclose (dataset);
	VRFY((hrc >= 0), "");
	dataset = -1;

	hrc = H5Sclose (dataspace);
	VRFY((hrc >= 0), "");

	hrc = H5Sclose (memspace);
	VRFY((hrc >= 0), "");

	hrc = H5Pclose (cparms);
	VRFY((hrc >= 0), "");

	hrc = H5Fclose (file_id);
	VRFY((hrc >= 0), "");
	file_id = -1;

	/* verify file size */
	filesize = get_filesize(filename);
	est_filesize = nchunks * CHUNK_SIZE * sizeof(unsigned char);
	VRFY((filesize >= est_filesize), "file size check");

    }

    /* Make sure all processes are done before exiting this routine.  Otherwise,
     * other tests may start and change the test data file before some processes
     * of this test are still accessing the file.
     */

    MPI_Barrier(MPI_COMM_WORLD);
}


/*
 * This program performs three different types of parallel access. It writes on
 * the entire dataset, it extends the dataset to nchunks*CHUNK_SIZE, and it only
 * opens the dataset. At the end, it verifies the size of the dataset to be
 * consistent with argument 'chunk_factor'.
 */
static void
parallel_access_dataset(const char *filename, int chunk_factor, access_type action, hid_t *file_id, hid_t *dataset)
{
    /* HDF5 gubbins */
    hid_t    memspace, dataspace;     /* HDF5 file identifier */
    hid_t    access_plist;         /* HDF5 ID for file access property list */
    herr_t   hrc;                  /* HDF5 return code */
    hsize_t  size[1];

    hsize_t     chunk_dims[1] ={CHUNK_SIZE};
    hsize_t     count[1];
    hsize_t     stride[1];
    hsize_t     block[1];
    hsize_t     offset[1];            /* Selection offset within dataspace */
    hsize_t     dims[1];
    hsize_t     maxdims[1];

    /* Variables used in reading data back */
    char         buffer[CHUNK_SIZE];
    int         i;
    long        nchunks;
    /* MPI Gubbins */
    MPI_Offset  filesize,	    /* actual file size */
		est_filesize;	    /* estimated file size */

    /* Initialize MPI */
    MPI_Comm_size(MPI_COMM_WORLD,&mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);

    nchunks=chunk_factor*mpi_size;

    /* Set up MPIO file access property lists */
    access_plist  = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((access_plist >= 0), "");

    hrc = H5Pset_fapl_mpio(access_plist, MPI_COMM_WORLD, MPI_INFO_NULL);
    VRFY((hrc >= 0), "");

    /* Open the file */
    if (*file_id<0){
        *file_id = H5Fopen(filename, H5F_ACC_RDWR, access_plist);
        VRFY((*file_id >= 0), "");
    }

    /* Open dataset*/
    if (*dataset<0){
        *dataset = H5Dopen2(*file_id, DSET_NAME, H5P_DEFAULT);
        VRFY((*dataset >= 0), "");
    }

    memspace = H5Screate_simple(1, chunk_dims, NULL);
    VRFY((memspace >= 0), "");

    dataspace = H5Dget_space(*dataset);
    VRFY((dataspace >= 0), "");

    size[0] = nchunks*CHUNK_SIZE;

    switch (action) {

        /* all chunks are written by all the processes in an interleaved way*/
        case write_all:

	    memset(buffer, mpi_rank+1, CHUNK_SIZE);
	    count[0] = 1;
	    stride[0] = 1;
	    block[0] = chunk_dims[0];
            for (i=0; i<nchunks/mpi_size; i++){
		    offset[0] = (i*mpi_size+mpi_rank)*chunk_dims[0];

		    hrc = H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, stride, count, block);
		    VRFY((hrc >= 0), "");

		    /* Write the buffer out */
		    hrc = H5Dwrite(*dataset, H5T_NATIVE_UCHAR, memspace, dataspace, H5P_DEFAULT, buffer);
		    VRFY((hrc >= 0), "H5Dwrite");
            }

            break;

        /* only extends the dataset */
        case extend_only:
            /* check if new size is larger than old size */
            hrc = H5Sget_simple_extent_dims(dataspace, dims, maxdims);
            VRFY((hrc >= 0), "");

            /* Extend dataset*/
            if (size[0] > dims[0]) {
                hrc = H5Dset_extent(*dataset, size);
                VRFY((hrc >= 0), "");
            }
            break;

        /* only opens the *dataset */
        case open_only:

            break;
    }

    /* Close up */
    hrc = H5Dclose(*dataset);
    VRFY((hrc >= 0), "");
    *dataset = -1;

    hrc = H5Sclose (dataspace);
    VRFY((hrc >= 0), "");

    hrc = H5Sclose (memspace);
    VRFY((hrc >= 0), "");

    hrc = H5Fclose(*file_id);
    VRFY((hrc >= 0), "");
    *file_id = -1;

    /* verify file size */
    filesize = get_filesize(filename);
    est_filesize = nchunks*CHUNK_SIZE*sizeof(unsigned char);
    VRFY((filesize >= est_filesize), "file size check");

    /* Can close some plists */
    hrc = H5Pclose(access_plist);
    VRFY((hrc >= 0), "");

    /* Make sure all processes are done before exiting this routine.  Otherwise,
     * other tests may start and change the test data file before some processes
     * of this test are still accessing the file.
     */
    MPI_Barrier(MPI_COMM_WORLD);
}

/*
 * This routine verifies the data written in the dataset. It does one of the
 * three cases according to the value of parameter `write_pattern'.
 * 1. it returns correct fill values though the dataset has not been written;
 * 2. it still returns correct fill values though only a small part is written;
 * 3. it returns correct values when the whole dataset has been written in an
 *    interleaved pattern.
 */
static void
verify_data(const char *filename, int chunk_factor, write_type write_pattern, int close, hid_t *file_id, hid_t *dataset)
{
    /* HDF5 gubbins */
    hid_t    dataspace, memspace;     /* HDF5 file identifier */
    hid_t    access_plist;         /* HDF5 ID for file access property list */
    herr_t   hrc;                  /* HDF5 return code */

    hsize_t     chunk_dims[1] ={CHUNK_SIZE};
    hsize_t     count[1];
    hsize_t     stride[1];
    hsize_t     block[1];
    hsize_t     offset[1];            /* Selection offset within dataspace */
    /* Variables used in reading data back */
    char         buffer[CHUNK_SIZE];
    int         value, i;
    int         index;
    long        nchunks;
    /* Initialize MPI */
    MPI_Comm_size(MPI_COMM_WORLD,&mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);

    nchunks=chunk_factor*mpi_size;

    /* Set up MPIO file access property lists */
    access_plist  = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((access_plist >= 0), "");

    hrc = H5Pset_fapl_mpio(access_plist, MPI_COMM_WORLD, MPI_INFO_NULL);
    VRFY((hrc >= 0), "");

    /* Open the file */
    if (*file_id<0){
        *file_id = H5Fopen(filename, H5F_ACC_RDWR, access_plist);
        VRFY((*file_id >= 0), "");
    }

    /* Open dataset*/
    if (*dataset<0){
        *dataset = H5Dopen2(*file_id, DSET_NAME, H5P_DEFAULT);
        VRFY((*dataset >= 0), "");
    }

    memspace = H5Screate_simple(1, chunk_dims, NULL);
    VRFY((memspace >= 0), "");

    dataspace = H5Dget_space(*dataset);
    VRFY((dataspace >= 0), "");

    /* all processes check all chunks. */
    count[0] = 1;
    stride[0] = 1;
    block[0] = chunk_dims[0];
    for (i=0; i<nchunks; i++){
	/* reset buffer values */
	memset(buffer, -1, CHUNK_SIZE);

        offset[0] = i*chunk_dims[0];

        hrc = H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, stride, count, block);
        VRFY((hrc >= 0), "");

        /* Read the chunk */
        hrc = H5Dread(*dataset, H5T_NATIVE_UCHAR, memspace, dataspace, H5P_DEFAULT, buffer);
        VRFY((hrc >= 0), "H5Dread");

        /* set expected value according the write pattern */
	switch (write_pattern) {
	    case all:
		value = i%mpi_size + 1;
		break;
	    case none:
		value = 0;
		break;
            case sec_last:
		if (i==nchunks-2)
		    value = 100;
		else
		    value = 0;
	}

        /* verify content of the chunk */
        for (index = 0; index < CHUNK_SIZE; index++)
            VRFY((buffer[index] == value), "data verification");
    }

    hrc = H5Sclose (dataspace);
	VRFY((hrc >= 0), "");

	hrc = H5Sclose (memspace);
	VRFY((hrc >= 0), "");

    /* Can close some plists */
    hrc = H5Pclose(access_plist);
    VRFY((hrc >= 0), "");

    /* Close up */
    if (close){
        hrc = H5Dclose(*dataset);
        VRFY((hrc >= 0), "");
        *dataset = -1;

        hrc = H5Fclose(*file_id);
        VRFY((hrc >= 0), "");
        *file_id = -1;
    }

    /* Make sure all processes are done before exiting this routine.  Otherwise,
     * other tests may start and change the test data file before some processes
     * of this test are still accessing the file.
     */
    MPI_Barrier(MPI_COMM_WORLD);
}



/*
 * Test following possible scenarios,
 * Case 1:
 * Sequential create a file and dataset with H5D_ALLOC_TIME_EARLY and large
 * size, no write, close, reopen in parallel, read to verify all return
 * the fill value.
 * Case 2:
 * Sequential create a file and dataset with H5D_ALLOC_TIME_EARLY but small
 * size, no write, close, reopen in parallel, extend to large size, then close,
 * then reopen in parallel and read to verify all return the fill value.
 * Case 3:
 * Sequential create a file and dataset with H5D_ALLOC_TIME_EARLY and large
 * size, write just a small part of the dataset (second to the last), close,
 * then reopen in parallel, read to verify all return the fill value except
 * those small portion that has been written.  Without closing it, writes
 * all parts of the dataset in a interleave pattern, close it, and reopen
 * it, read to verify all data are as written.
 */
void
test_chunk_alloc(void)
{
    const char *filename;
    hid_t file_id, dataset;

    file_id = dataset = -1;

    /* Initialize MPI */
    MPI_Comm_size(MPI_COMM_WORLD,&mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);

    filename = GetTestParameters();
    if (VERBOSE_MED)
	printf("Extend Chunked allocation test on file %s\n", filename);

    /* Case 1 */
    /* Create chunked dataset without writing anything.*/
    create_chunked_dataset(filename, CHUNK_FACTOR, none);
    /* reopen dataset in parallel and check for file size */
    parallel_access_dataset(filename, CHUNK_FACTOR, open_only, &file_id, &dataset);
    /* reopen dataset in parallel, read and verify the data */
    verify_data(filename, CHUNK_FACTOR, none, CLOSE, &file_id, &dataset);

    /* Case 2 */
    /* Create chunked dataset without writing anything */
    create_chunked_dataset(filename, 20, none);
    /* reopen dataset in parallel and only extend it */
    parallel_access_dataset(filename, CHUNK_FACTOR, extend_only, &file_id, &dataset);
    /* reopen dataset in parallel, read and verify the data */
    verify_data(filename, CHUNK_FACTOR, none, CLOSE, &file_id, &dataset);

    /* Case 3 */
    /* Create chunked dataset and write in the second to last chunk */
    create_chunked_dataset(filename, CHUNK_FACTOR, sec_last);
    /* Reopen dataset in parallel, read and verify the data. The file and dataset are not closed*/
    verify_data(filename, CHUNK_FACTOR, sec_last, NO_CLOSE, &file_id, &dataset);
    /* All processes write in all the chunks in a interleaved way */
    parallel_access_dataset(filename, CHUNK_FACTOR, write_all, &file_id, &dataset);
    /* reopen dataset in parallel, read and verify the data */
    verify_data(filename, CHUNK_FACTOR, all, CLOSE, &file_id, &dataset);

}
