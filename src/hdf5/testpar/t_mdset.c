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

#define DIM  2
#define SIZE 32
#define NDATASET 4
#define GROUP_DEPTH 128
enum obj_type { is_group, is_dset };


static int get_size(void);
static void write_dataset(hid_t, hid_t, hid_t);
static int  read_dataset(hid_t, hid_t, hid_t);
static void create_group_recursive(hid_t, hid_t, hid_t, int);
static void recursive_read_group(hid_t, hid_t, hid_t, int);
static void group_dataset_read(hid_t fid, int mpi_rank, int m);
static void write_attribute(hid_t, int, int);
static int  read_attribute(hid_t, int, int);
static int  check_value(DATATYPE *, DATATYPE *, int);
static void get_slab(hsize_t[], hsize_t[], hsize_t[], hsize_t[], int);


/*
 * The size value computed by this function is used extensively in
 * configuring tests for the current number of processes.
 *
 * This function was created as part of an effort to allow the
 * test functions in this file to run on an arbitrary number of
 * processors.
 *                                       JRM - 8/11/04
 */

static int
get_size(void)
{
    int mpi_rank;
    int mpi_size;
    int size = SIZE;

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank); /* needed for VRFY */
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

    if(mpi_size > size ) {

        if((mpi_size % 2) == 0 ) {

            size = mpi_size;

        } else {

            size = mpi_size + 1;
        }
    }

    VRFY((mpi_size <= size), "mpi_size <= size");
    VRFY(((size % 2) == 0), "size isn't even");

    return(size);

} /* get_size() */

/*
 * Example of using PHDF5 to create ndatasets datasets.  Each process write
 * a slab of array to the file.
 *
 * Changes:	Updated function to use a dynamically calculated size,
 *		instead of the old SIZE #define.  This should allow it
 *		to function with an arbitrary number of processors.
 *
 *						JRM - 8/11/04
 */
void multiple_dset_write(void)
{
    int i, j, n, mpi_size, mpi_rank, size;
    hid_t iof, plist, dataset, memspace, filespace;
    hid_t dcpl;                         /* Dataset creation property list */
    hbool_t use_gpfs = FALSE;           /* Use GPFS hints */
    hsize_t chunk_origin [DIM];
    hsize_t chunk_dims [DIM], file_dims [DIM];
    hsize_t count[DIM]={1,1};
    double * outme = NULL;
    double fill=1.0;                    /* Fill value */
    char dname [100];
    herr_t ret;
    const H5Ptest_param_t *pt;
    char	*filename;
    int		ndatasets;

    pt = GetTestParameters();
    filename = pt->name;
    ndatasets = pt->count;

    size = get_size();

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

    outme = HDmalloc((size_t)(size * size * sizeof(double)));
    VRFY((outme != NULL), "HDmalloc succeeded for outme");

    plist = create_faccess_plist(MPI_COMM_WORLD, MPI_INFO_NULL, facc_type, use_gpfs);
    VRFY((plist>=0), "create_faccess_plist succeeded");
    iof = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, plist);
    VRFY((iof>=0), "H5Fcreate succeeded");
    ret = H5Pclose(plist);
    VRFY((ret>=0), "H5Pclose succeeded");

    /* decide the hyperslab according to process number. */
    get_slab(chunk_origin, chunk_dims, count, file_dims, size);

    memspace = H5Screate_simple(DIM, chunk_dims, NULL);
    filespace = H5Screate_simple(DIM, file_dims, NULL);
    ret = H5Sselect_hyperslab(filespace, H5S_SELECT_SET, chunk_origin, chunk_dims, count, chunk_dims);
    VRFY((ret>=0), "mdata hyperslab selection");

    /* Create a dataset creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((dcpl>=0), "dataset creation property list succeeded");

    ret = H5Pset_fill_value(dcpl, H5T_NATIVE_DOUBLE, &fill);
    VRFY((ret>=0), "set fill-value succeeded");

    for(n = 0; n < ndatasets; n++) {
	sprintf(dname, "dataset %d", n);
	dataset = H5Dcreate2(iof, dname, H5T_NATIVE_DOUBLE, filespace, H5P_DEFAULT, dcpl, H5P_DEFAULT);
	VRFY((dataset > 0), dname);

	/* calculate data to write */
	for(i = 0; i < size; i++)
	    for(j = 0; j < size; j++)
                outme [(i * size) + j] = n*1000 + mpi_rank;

	H5Dwrite(dataset, H5T_NATIVE_DOUBLE, memspace, filespace, H5P_DEFAULT, outme);

	H5Dclose(dataset);
#ifdef BARRIER_CHECKS
	if(!((n+1) % 10)) {
	    printf("created %d datasets\n", n+1);
	    MPI_Barrier(MPI_COMM_WORLD);
	}
#endif /* BARRIER_CHECKS */
    }

    H5Sclose(filespace);
    H5Sclose(memspace);
    H5Pclose(dcpl);
    H5Fclose(iof);

    HDfree(outme);
}


/* Example of using PHDF5 to create, write, and read compact dataset.
 *
 * Changes:	Updated function to use a dynamically calculated size,
 *		instead of the old SIZE #define.  This should allow it
 *		to function with an arbitrary number of processors.
 *
 *						JRM - 8/11/04
 */
void compact_dataset(void)
{
    int i, j, mpi_size, mpi_rank, size, err_num=0;
    hbool_t use_gpfs = FALSE;
    hid_t iof, plist, dcpl, dxpl, dataset, filespace;
    hsize_t file_dims [DIM];
    double * outme;
    double * inme;
    char dname[]="dataset";
    herr_t ret;
    const char *filename;

    size = get_size();

    for(i = 0; i < DIM; i++ )
        file_dims[i] = size;

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

    outme = HDmalloc((size_t)(size * size * sizeof(double)));
    VRFY((outme != NULL), "HDmalloc succeeded for outme");

    inme = HDmalloc((size_t)(size * size * sizeof(double)));
    VRFY((outme != NULL), "HDmalloc succeeded for inme");

    filename = GetTestParameters();
    VRFY((mpi_size <= size), "mpi_size <= size");

    plist = create_faccess_plist(MPI_COMM_WORLD, MPI_INFO_NULL, facc_type, use_gpfs);
    iof = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, plist);

    /* Define data space */
    filespace = H5Screate_simple(DIM, file_dims, NULL);

    /* Create a compact dataset */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((dcpl>=0), "dataset creation property list succeeded");
    ret = H5Pset_layout(dcpl, H5D_COMPACT);
    VRFY((dcpl >= 0), "set property list for compact dataset");
    ret = H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY);
    VRFY((ret >= 0), "set space allocation time for compact dataset");

    dataset = H5Dcreate2(iof, dname, H5T_NATIVE_DOUBLE, filespace, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    VRFY((dataset >= 0), "H5Dcreate2 succeeded");

    /* set up the collective transfer properties list */
    dxpl = H5Pcreate(H5P_DATASET_XFER);
    VRFY((dxpl >= 0), "");
    ret = H5Pset_dxpl_mpio(dxpl, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pcreate xfer succeeded");
    if(dxfer_coll_type == DXFER_INDEPENDENT_IO) {
     ret = H5Pset_dxpl_mpio_collective_opt(dxpl, H5FD_MPIO_INDIVIDUAL_IO);
     VRFY((ret>= 0),"set independent IO collectively succeeded");
    }


    /* Recalculate data to write.  Each process writes the same data. */
    for(i = 0; i < size; i++)
         for(j = 0; j < size; j++)
              outme[(i * size) + j] =(i + j) * 1000;

    ret = H5Dwrite(dataset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, dxpl, outme);
    VRFY((ret >= 0), "H5Dwrite succeeded");

    H5Pclose(dcpl);
    H5Pclose(plist);
    H5Dclose(dataset);
    H5Sclose(filespace);
    H5Fclose(iof);

    /* Open the file and dataset, read and compare the data. */
    plist = create_faccess_plist(MPI_COMM_WORLD, MPI_INFO_NULL, facc_type, use_gpfs);
    iof = H5Fopen(filename, H5F_ACC_RDONLY, plist);
    VRFY((iof >= 0), "H5Fopen succeeded");

    /* set up the collective transfer properties list */
    dxpl = H5Pcreate(H5P_DATASET_XFER);
    VRFY((dxpl >= 0), "");
    ret = H5Pset_dxpl_mpio(dxpl, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pcreate xfer succeeded");
    if(dxfer_coll_type == DXFER_INDEPENDENT_IO) {
     ret = H5Pset_dxpl_mpio_collective_opt(dxpl,H5FD_MPIO_INDIVIDUAL_IO);
     VRFY((ret>= 0),"set independent IO collectively succeeded");
    }


    dataset = H5Dopen2(iof, dname, H5P_DEFAULT);
    VRFY((dataset >= 0), "H5Dopen2 succeeded");

    ret = H5Dread(dataset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, dxpl, inme);
    VRFY((ret >= 0), "H5Dread succeeded");

    /* Verify data value */
    for(i = 0; i < size; i++)
        for(j = 0; j < size; j++)
            if(inme[(i * size) + j] != outme[(i * size) + j])
                if(err_num++ < MAX_ERR_REPORT || VERBOSE_MED)
                    printf("Dataset Verify failed at [%d][%d]: expect %f, got %f\n", i, j, outme[(i * size) + j], inme[(i * size) + j]);

    H5Pclose(plist);
    H5Pclose(dxpl);
    H5Dclose(dataset);
    H5Fclose(iof);
    HDfree(inme);
    HDfree(outme);
}

/*
 * Example of using PHDF5 to create, write, and read dataset and attribute
 * of Null dataspace.
 *
 * Changes:	Removed the assert that mpi_size <= the SIZE #define.
 *		As best I can tell, this assert isn't needed here,
 *		and in any case, the SIZE #define is being removed
 *		in an update of the functions in this file to run
 *		with an arbitrary number of processes.
 *
 *                                         JRM - 8/24/04
 */
void null_dataset(void)
{
    int mpi_size, mpi_rank;
    hbool_t use_gpfs = FALSE;
    hid_t iof, plist, dxpl, dataset, attr, sid;
    unsigned uval=2;    /* Buffer for writing to dataset */
    int val=1;          /* Buffer for writing to attribute */
    int nelem;
    char dname[]="dataset";
    char attr_name[]="attribute";
    herr_t ret;
    const char *filename;

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

    filename = GetTestParameters();

    plist = create_faccess_plist(MPI_COMM_WORLD, MPI_INFO_NULL,
                                 facc_type, use_gpfs);
    iof = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, plist);

    /* Define data space */
    sid = H5Screate(H5S_NULL);

    /* Check that the null dataspace actually has 0 elements */
    nelem = H5Sget_simple_extent_npoints(sid);
    VRFY((nelem == 0), "H5Sget_simple_extent_npoints");

    /* Create a compact dataset */
    dataset = H5Dcreate2(iof, dname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((dataset >= 0), "H5Dcreate2 succeeded");

    /* set up the collective transfer properties list */
    dxpl = H5Pcreate(H5P_DATASET_XFER);
    VRFY((dxpl >= 0), "");
    ret = H5Pset_dxpl_mpio(dxpl, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pcreate xfer succeeded");
    if(dxfer_coll_type == DXFER_INDEPENDENT_IO) {
     ret = H5Pset_dxpl_mpio_collective_opt(dxpl, H5FD_MPIO_INDIVIDUAL_IO);
     VRFY((ret>= 0),"set independent IO collectively succeeded");
    }


    /* Write "nothing" to the dataset(with type conversion) */
    ret = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, &uval);
    VRFY((ret >= 0), "H5Dwrite succeeded");

    /* Create an attribute for the group */
    attr = H5Acreate2(dataset, attr_name, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((attr >= 0), "H5Acreate2");

    /* Write "nothing" to the attribute(with type conversion) */
    ret = H5Awrite(attr, H5T_NATIVE_INT, &val);
    VRFY((ret >= 0), "H5Awrite");

    H5Aclose(attr);
    H5Dclose(dataset);
    H5Pclose(plist);
    H5Sclose(sid);
    H5Fclose(iof);

    /* Open the file and dataset, read and compare the data. */
    plist = create_faccess_plist(MPI_COMM_WORLD, MPI_INFO_NULL, facc_type, use_gpfs);
    iof = H5Fopen(filename, H5F_ACC_RDONLY, plist);
    VRFY((iof >= 0), "H5Fopen succeeded");

    /* set up the collective transfer properties list */
    dxpl = H5Pcreate(H5P_DATASET_XFER);
    VRFY((dxpl >= 0), "");
    ret = H5Pset_dxpl_mpio(dxpl, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pcreate xfer succeeded");
    if(dxfer_coll_type == DXFER_INDEPENDENT_IO) {
     ret = H5Pset_dxpl_mpio_collective_opt(dxpl,H5FD_MPIO_INDIVIDUAL_IO);
     VRFY((ret>= 0),"set independent IO collectively succeeded");
    }


    dataset = H5Dopen2(iof, dname, H5P_DEFAULT);
    VRFY((dataset >= 0), "H5Dopen2 succeeded");

    /* Try reading from the dataset(make certain our buffer is unmodified) */
    ret = H5Dread(dataset, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL, dxpl, &uval);
    VRFY((ret>=0), "H5Dread");
    VRFY((uval==2), "H5Dread");

    /* Open the attribute for the dataset */
    attr = H5Aopen(dataset, attr_name, H5P_DEFAULT);
    VRFY((attr >= 0), "H5Aopen");

    /* Try reading from the attribute(make certain our buffer is unmodified) */    ret = H5Aread(attr, H5T_NATIVE_INT, &val);
    VRFY((ret>=0), "H5Aread");
    VRFY((val==1), "H5Aread");

    H5Pclose(plist);
    H5Pclose(dxpl);
    H5Aclose(attr);
    H5Dclose(dataset);
    H5Fclose(iof);
}

/* Example of using PHDF5 to create "large" datasets. (>2GB, >4GB, >8GB)
 * Actual data is _not_ written to these datasets.  Dataspaces are exact
 * sizes(2GB, 4GB, etc.), but the metadata for the file pushes the file over
 * the boundary of interest.
 *
 * Changes:	Removed the assert that mpi_size <= the SIZE #define.
 *		As best I can tell, this assert isn't needed here,
 *		and in any case, the SIZE #define is being removed
 *		in an update of the functions in this file to run
 *		with an arbitrary number of processes.
 *
 *                                         JRM - 8/11/04
 */
void big_dataset(void)
{
    int mpi_size, mpi_rank;     /* MPI info */
    hbool_t use_gpfs = FALSE;   /* Don't use GPFS stuff for this test */
    hid_t iof,                  /* File ID */
        fapl,                   /* File access property list ID */
        dataset,                /* Dataset ID */
        filespace;              /* Dataset's dataspace ID */
    hsize_t file_dims [4];      /* Dimensions of dataspace */
    char dname[]="dataset";     /* Name of dataset */
    MPI_Offset file_size;       /* Size of file on disk */
    herr_t ret;                 /* Generic return value */
    const char *filename;

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

    /* Verify MPI_Offset can handle larger than 2GB sizes */
    VRFY((sizeof(MPI_Offset) > 4), "sizeof(MPI_Offset)>4");

    filename = GetTestParameters();

    fapl = create_faccess_plist(MPI_COMM_WORLD, MPI_INFO_NULL, facc_type, use_gpfs);
    VRFY((fapl >= 0), "create_faccess_plist succeeded");

    /*
     * Create >2GB HDF5 file
     */
    iof = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    VRFY((iof >= 0), "H5Fcreate succeeded");

    /* Define dataspace for 2GB dataspace */
    file_dims[0]= 2;
    file_dims[1]= 1024;
    file_dims[2]= 1024;
    file_dims[3]= 1024;
    filespace = H5Screate_simple(4, file_dims, NULL);
    VRFY((filespace >= 0), "H5Screate_simple succeeded");

    dataset = H5Dcreate2(iof, dname, H5T_NATIVE_UCHAR, filespace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((dataset >= 0), "H5Dcreate2 succeeded");

    /* Close all file objects */
    ret = H5Dclose(dataset);
    VRFY((ret >= 0), "H5Dclose succeeded");
    ret = H5Sclose(filespace);
    VRFY((ret >= 0), "H5Sclose succeeded");
    ret = H5Fclose(iof);
    VRFY((ret >= 0), "H5Fclose succeeded");

    /* Check that file of the correct size was created */
    file_size = h5_get_file_size(filename, fapl);
    VRFY((file_size == 2147485792ULL), "File is correct size(~2GB)");

    /*
     * Create >4GB HDF5 file
     */
    iof = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    VRFY((iof >= 0), "H5Fcreate succeeded");

    /* Define dataspace for 4GB dataspace */
    file_dims[0]= 4;
    file_dims[1]= 1024;
    file_dims[2]= 1024;
    file_dims[3]= 1024;
    filespace = H5Screate_simple(4, file_dims, NULL);
    VRFY((filespace >= 0), "H5Screate_simple succeeded");

    dataset = H5Dcreate2(iof, dname, H5T_NATIVE_UCHAR, filespace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((dataset >= 0), "H5Dcreate2 succeeded");

    /* Close all file objects */
    ret = H5Dclose(dataset);
    VRFY((ret >= 0), "H5Dclose succeeded");
    ret = H5Sclose(filespace);
    VRFY((ret >= 0), "H5Sclose succeeded");
    ret = H5Fclose(iof);
    VRFY((ret >= 0), "H5Fclose succeeded");

    /* Check that file of the correct size was created */
    file_size = h5_get_file_size(filename, fapl);
    VRFY((file_size == 4294969440ULL), "File is correct size(~4GB)");

    /*
     * Create >8GB HDF5 file
     */
    iof = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    VRFY((iof >= 0), "H5Fcreate succeeded");

    /* Define dataspace for 8GB dataspace */
    file_dims[0]= 8;
    file_dims[1]= 1024;
    file_dims[2]= 1024;
    file_dims[3]= 1024;
    filespace = H5Screate_simple(4, file_dims, NULL);
    VRFY((filespace >= 0), "H5Screate_simple succeeded");

    dataset = H5Dcreate2(iof, dname, H5T_NATIVE_UCHAR, filespace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((dataset >= 0), "H5Dcreate2 succeeded");

    /* Close all file objects */
    ret = H5Dclose(dataset);
    VRFY((ret >= 0), "H5Dclose succeeded");
    ret = H5Sclose(filespace);
    VRFY((ret >= 0), "H5Sclose succeeded");
    ret = H5Fclose(iof);
    VRFY((ret >= 0), "H5Fclose succeeded");

    /* Check that file of the correct size was created */
    file_size = h5_get_file_size(filename, fapl);
    VRFY((file_size == 8589936736ULL), "File is correct size(~8GB)");

    /* Close fapl */
    ret = H5Pclose(fapl);
    VRFY((ret >= 0), "H5Pclose succeeded");
}

/* Example of using PHDF5 to read a partial written dataset.   The dataset does
 * not have actual data written to the entire raw data area and relies on the
 * default fill value of zeros to work correctly.
 *
 * Changes:	Removed the assert that mpi_size <= the SIZE #define.
 *		As best I can tell, this assert isn't needed here,
 *		and in any case, the SIZE #define is being removed
 *		in an update of the functions in this file to run
 *		with an arbitrary number of processes.
 *
 *		Also added code to free dynamically allocated buffers.
 *
 *                                         JRM - 8/11/04
 */
void dataset_fillvalue(void)
{
    int mpi_size, mpi_rank;     /* MPI info */
    hbool_t use_gpfs = FALSE;   /* Don't use GPFS stuff for this test */
    int err_num;                /* Number of errors */
    hid_t iof,                  /* File ID */
        fapl,                   /* File access property list ID */
        dxpl,                   /* Data transfer property list ID */
        dataset,                /* Dataset ID */
        memspace,               /* Memory dataspace ID */
        filespace;              /* Dataset's dataspace ID */
    char dname[]="dataset";     /* Name of dataset */
    hsize_t     dset_dims[4] = {0, 6, 7, 8};
    hsize_t     req_start[4] = {0, 0, 0, 0};
    hsize_t     req_count[4] = {1, 6, 7, 8};
    hsize_t     dset_size;      /* Dataset size */
    int *rdata, *wdata;         /* Buffers for data to read and write */
    int *twdata, *trdata;        /* Temporary pointer into buffer */
    int acc, i, j, k, l;        /* Local index variables */
    herr_t ret;                 /* Generic return value */
    const char *filename;

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

    filename = GetTestParameters();

    /* Set the dataset dimension to be one row more than number of processes */
    /* and calculate the actual dataset size. */
    dset_dims[0]=mpi_size+1;
    dset_size=dset_dims[0]*dset_dims[1]*dset_dims[2]*dset_dims[3];

    /* Allocate space for the buffers */
    rdata=HDmalloc((size_t)(dset_size*sizeof(int)));
    VRFY((rdata != NULL), "HDcalloc succeeded for read buffer");
    wdata=HDmalloc((size_t)(dset_size*sizeof(int)));
    VRFY((wdata != NULL), "HDmalloc succeeded for write buffer");

    fapl = create_faccess_plist(MPI_COMM_WORLD, MPI_INFO_NULL, facc_type, use_gpfs);
    VRFY((fapl >= 0), "create_faccess_plist succeeded");

    /*
     * Create HDF5 file
     */
    iof = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    VRFY((iof >= 0), "H5Fcreate succeeded");

    filespace = H5Screate_simple(4, dset_dims, NULL);
    VRFY((filespace >= 0), "File H5Screate_simple succeeded");

    dataset = H5Dcreate2(iof, dname, H5T_NATIVE_INT, filespace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((dataset >= 0), "H5Dcreate2 succeeded");

    memspace = H5Screate_simple(4, dset_dims, NULL);
    VRFY((memspace >= 0), "Memory H5Screate_simple succeeded");

    /*
     * Read dataset before any data is written.
     */
    /* set entire read buffer with the constant 2 */
    HDmemset(rdata,2,(size_t)(dset_size*sizeof(int)));
    /* Independently read the entire dataset back */
    ret = H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);
    VRFY((ret >= 0), "H5Dread succeeded");

    /* Verify all data read are the fill value 0 */
    trdata = rdata;
    err_num = 0;
    for(i = 0; i < (int)dset_dims[0]; i++)
        for(j = 0; j < (int)dset_dims[1]; j++)
            for(k = 0; k < (int)dset_dims[2]; k++)
                for(l = 0; l < (int)dset_dims[3]; l++, twdata++, trdata++)
		    if(*trdata != 0)
			if(err_num++ < MAX_ERR_REPORT || VERBOSE_MED)
			    printf("Dataset Verify failed at [%d][%d][%d][%d]: expect 0, got %d\n", i, j, k, l, *trdata);
    if(err_num > MAX_ERR_REPORT && !VERBOSE_MED)
        printf("[more errors ...]\n");
    if(err_num){
        printf("%d errors found in check_value\n", err_num);
	nerrors++;
    }

    /* Barrier to ensure all processes have completed the above test. */
    MPI_Barrier(MPI_COMM_WORLD);

    /*
     * Each process writes 1 row of data. Thus last row is not written.
     */
    /* Create hyperslabs in memory and file dataspaces */
    req_start[0]=mpi_rank;
    ret = H5Sselect_hyperslab(filespace, H5S_SELECT_SET, req_start, NULL, req_count, NULL);
    VRFY((ret >= 0), "H5Sselect_hyperslab succeeded on memory dataspace");
    ret = H5Sselect_hyperslab(memspace, H5S_SELECT_SET, req_start, NULL, req_count, NULL);
    VRFY((ret >= 0), "H5Sselect_hyperslab succeeded on memory dataspace");

    /* Create DXPL for collective I/O */
    dxpl = H5Pcreate(H5P_DATASET_XFER);
    VRFY((dxpl >= 0), "H5Pcreate succeeded");

    ret = H5Pset_dxpl_mpio(dxpl, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");
    if(dxfer_coll_type == DXFER_INDEPENDENT_IO) {
     ret = H5Pset_dxpl_mpio_collective_opt(dxpl,H5FD_MPIO_INDIVIDUAL_IO);
     VRFY((ret>= 0),"set independent IO collectively succeeded");
    }


    /* Fill write buffer with some values */
    twdata=wdata;
    for(i=0, acc=0; i<(int)dset_dims[0]; i++)
        for(j=0; j<(int)dset_dims[1]; j++)
            for(k=0; k<(int)dset_dims[2]; k++)
                for(l=0; l<(int)dset_dims[3]; l++)
                    *twdata++ = acc++;

    /* Collectively write a hyperslab of data to the dataset */
    ret = H5Dwrite(dataset, H5T_NATIVE_INT, memspace, filespace, dxpl, wdata);
    VRFY((ret >= 0), "H5Dwrite succeeded");

    /* Barrier here, to allow MPI-posix I/O to sync */
    MPI_Barrier(MPI_COMM_WORLD);

    /*
     * Read dataset after partial write.
     */
    /* set entire read buffer with the constant 2 */
    HDmemset(rdata,2,(size_t)(dset_size*sizeof(int)));
    /* Independently read the entire dataset back */
    ret = H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);
    VRFY((ret >= 0), "H5Dread succeeded");

    /* Verify correct data read */
    twdata=wdata;
    trdata=rdata;
    err_num=0;
    for(i=0; i<(int)dset_dims[0]; i++)
        for(j=0; j<(int)dset_dims[1]; j++)
            for(k=0; k<(int)dset_dims[2]; k++)
                for(l=0; l<(int)dset_dims[3]; l++, twdata++, trdata++)
                    if(i<mpi_size) {
                        if(*twdata != *trdata )
                            if(err_num++ < MAX_ERR_REPORT || VERBOSE_MED)
                                printf("Dataset Verify failed at [%d][%d][%d][%d]: expect %d, got %d\n", i,j,k,l, *twdata, *trdata);
                    } /* end if */
                    else {
                        if(*trdata != 0)
                            if(err_num++ < MAX_ERR_REPORT || VERBOSE_MED)
                                printf("Dataset Verify failed at [%d][%d][%d][%d]: expect 0, got %d\n", i,j,k,l, *trdata);
                    } /* end else */
    if(err_num > MAX_ERR_REPORT && !VERBOSE_MED)
        printf("[more errors ...]\n");
    if(err_num){
        printf("%d errors found in check_value\n", err_num);
	nerrors++;
    }

    /* Close all file objects */
    ret = H5Dclose(dataset);
    VRFY((ret >= 0), "H5Dclose succeeded");
    ret = H5Sclose(filespace);
    VRFY((ret >= 0), "H5Sclose succeeded");
    ret = H5Fclose(iof);
    VRFY((ret >= 0), "H5Fclose succeeded");

    /* Close memory dataspace */
    ret = H5Sclose(memspace);
    VRFY((ret >= 0), "H5Sclose succeeded");

    /* Close dxpl */
    ret = H5Pclose(dxpl);
    VRFY((ret >= 0), "H5Pclose succeeded");

    /* Close fapl */
    ret = H5Pclose(fapl);
    VRFY((ret >= 0), "H5Pclose succeeded");

    /* free the buffers */
    HDfree(rdata);
    HDfree(wdata);
}

/* Write multiple groups with a chunked dataset in each group collectively.
 * These groups and datasets are for testing independent read later.
 *
 * Changes:     Updated function to use a dynamically calculated size,
 *              instead of the old SIZE #define.  This should allow it
 *              to function with an arbitrary number of processors.
 *
 *                                              JRM - 8/16/04
 */
void collective_group_write(void)
{
    int mpi_rank, mpi_size, size;
    int i, j, m;
    hbool_t use_gpfs = FALSE;
    char gname[64], dname[32];
    hid_t fid, gid, did, plist, dcpl, memspace, filespace;
    DATATYPE * outme = NULL;
    hsize_t chunk_origin[DIM];
    hsize_t chunk_dims[DIM], file_dims[DIM], count[DIM];
    hsize_t chunk_size[2];  /* Chunk dimensions - computed shortly */
    herr_t ret1, ret2;
    const H5Ptest_param_t *pt;
    char	*filename;
    int		ngroups;

    pt = GetTestParameters();
    filename = pt->name;
    ngroups = pt->count;

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

    size = get_size();

    chunk_size[0] =(hsize_t)(size / 2);
    chunk_size[1] =(hsize_t)(size / 2);

    outme = HDmalloc((size_t)(size * size * sizeof(DATATYPE)));
    VRFY((outme != NULL), "HDmalloc succeeded for outme");

    plist = create_faccess_plist(MPI_COMM_WORLD, MPI_INFO_NULL, facc_type, use_gpfs);
    fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, plist);
    H5Pclose(plist);

    /* decide the hyperslab according to process number. */
    get_slab(chunk_origin, chunk_dims, count, file_dims, size);

    /* select hyperslab in memory and file spaces.  These two operations are
     * identical since the datasets are the same. */
    memspace  = H5Screate_simple(DIM, file_dims, NULL);
    ret1 = H5Sselect_hyperslab(memspace, H5S_SELECT_SET, chunk_origin,
                               chunk_dims, count, chunk_dims);
    filespace = H5Screate_simple(DIM, file_dims,  NULL);
    ret2 = H5Sselect_hyperslab(filespace, H5S_SELECT_SET, chunk_origin,
                               chunk_dims, count, chunk_dims);
    VRFY((memspace>=0), "memspace");
    VRFY((filespace>=0), "filespace");
    VRFY((ret1>=0), "mgroup memspace selection");
    VRFY((ret2>=0), "mgroup filespace selection");

    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    ret1 = H5Pset_chunk(dcpl, 2, chunk_size);
    VRFY((dcpl>=0), "dataset creation property");
    VRFY((ret1>=0), "set chunk for dataset creation property");

    /* creates ngroups groups under the root group, writes chunked
     * datasets in parallel. */
    for(m = 0; m < ngroups; m++) {
        sprintf(gname, "group%d", m);
        gid = H5Gcreate2(fid, gname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        VRFY((gid > 0), gname);

        sprintf(dname, "dataset%d", m);
        did = H5Dcreate2(gid, dname, H5T_NATIVE_INT, filespace, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        VRFY((did > 0), dname);

        for(i = 0; i < size; i++)
            for(j = 0; j < size; j++)
                outme[(i * size) + j] =(i + j) * 1000 + mpi_rank;

        H5Dwrite(did, H5T_NATIVE_INT, memspace, filespace, H5P_DEFAULT,
                 outme);

        H5Dclose(did);
        H5Gclose(gid);

#ifdef BARRIER_CHECKS
        if(!((m+1) % 10)) {
            printf("created %d groups\n", m+1);
            MPI_Barrier(MPI_COMM_WORLD);
	}
#endif /* BARRIER_CHECKS */
    }

    H5Pclose(dcpl);
    H5Sclose(filespace);
    H5Sclose(memspace);
    H5Fclose(fid);

    HDfree(outme);
}

/* Let two sets of processes open and read different groups and chunked
 * datasets independently.
 */
void independent_group_read(void)
{
    int      mpi_rank, m;
    hid_t    plist, fid;
    hbool_t  use_gpfs = FALSE;
    const H5Ptest_param_t *pt;
    char	*filename;
    int		ngroups;

    pt = GetTestParameters();
    filename = pt->name;
    ngroups = pt->count;

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    plist = create_faccess_plist(MPI_COMM_WORLD, MPI_INFO_NULL, facc_type, use_gpfs);
    fid = H5Fopen(filename, H5F_ACC_RDONLY, plist);
    H5Pclose(plist);

    /* open groups and read datasets. Odd number processes read even number
     * groups from the end; even number processes read odd number groups
     * from the beginning. */
    if(mpi_rank%2==0) {
        for(m=ngroups-1; m==0; m-=2)
            group_dataset_read(fid, mpi_rank, m);
    } else {
        for(m=0; m<ngroups; m+=2)
            group_dataset_read(fid, mpi_rank, m);
    }

    H5Fclose(fid);
}

/* Open and read datasets and compare data
 *
 * Changes:     Updated function to use a dynamically calculated size,
 *              instead of the old SIZE #define.  This should allow it
 *              to function with an arbitrary number of processors.
 *
 *		Also added code to verify the results of dynamic memory
 *		allocations, and to free dynamically allocated memeory
 *		when we are done with it.
 *
 *                                              JRM - 8/16/04
 */
static void
group_dataset_read(hid_t fid, int mpi_rank, int m)
{
    int      ret, i, j, size;
    char     gname[64], dname[32];
    hid_t    gid, did;
    DATATYPE *outdata = NULL;
    DATATYPE *indata = NULL;

    size = get_size();

    indata =(DATATYPE*)HDmalloc((size_t)(size * size * sizeof(DATATYPE)));
    VRFY((indata != NULL), "HDmalloc succeeded for indata");

    outdata =(DATATYPE*)HDmalloc((size_t)(size * size * sizeof(DATATYPE)));
    VRFY((outdata != NULL), "HDmalloc succeeded for outdata");

    /* open every group under root group. */
    sprintf(gname, "group%d", m);
    gid = H5Gopen2(fid, gname, H5P_DEFAULT);
    VRFY((gid > 0), gname);

    /* check the data. */
    sprintf(dname, "dataset%d", m);
    did = H5Dopen2(gid, dname, H5P_DEFAULT);
    VRFY((did>0), dname);

    H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, indata);

    /* this is the original value */
    for(i=0; i<size; i++)
       for(j=0; j<size; j++) {
           outdata[(i * size) + j] =(i+j)*1000 + mpi_rank;
       }

    /* compare the original value(outdata) to the value in file(indata).*/
    ret = check_value(indata, outdata, size);
    VRFY((ret==0), "check the data");

    H5Dclose(did);
    H5Gclose(gid);

    HDfree(indata);
    HDfree(outdata);
}

/*
 * Example of using PHDF5 to create multiple groups.  Under the root group,
 * it creates ngroups groups.  Under the first group just created, it creates
 * recursive subgroups of depth GROUP_DEPTH.  In each created group, it
 * generates NDATASETS datasets.  Each process write a hyperslab of an array
 * into the file.  The structure is like
 *
 *                             root group
 *                                 |
 *            ---------------------------- ... ... ------------------------
 *           |          |         |        ... ...  |                      |
 *       group0*+'   group1*+' group2*+'   ... ...             group ngroups*+'
 *           |
 *      1st_child_group*'
 *           |
 *      2nd_child_group*'
 *           |
 *           :
 *           :
 *           |
 * GROUP_DEPTHth_child_group*'
 *
 *      * means the group has dataset(s).
 *      + means the group has attribute(s).
 *      ' means the datasets in the groups have attribute(s).
 *
 * Changes:     Updated function to use a dynamically calculated size,
 *              instead of the old SIZE #define.  This should allow it
 *              to function with an arbitrary number of processors.
 *
 *                                              JRM - 8/16/04
 */
void multiple_group_write(void)
{
    int mpi_rank, mpi_size, size;
    int m;
    hbool_t use_gpfs = FALSE;
    char gname[64];
    hid_t fid, gid, plist, memspace, filespace;
    hsize_t chunk_origin[DIM];
    hsize_t chunk_dims[DIM], file_dims[DIM], count[DIM];
    herr_t ret;
    const H5Ptest_param_t *pt;
    char	*filename;
    int		ngroups;

    pt = GetTestParameters();
    filename = pt->name;
    ngroups = pt->count;

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

    size = get_size();

    plist = create_faccess_plist(MPI_COMM_WORLD, MPI_INFO_NULL, facc_type, use_gpfs);
    fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, plist);
    H5Pclose(plist);

    /* decide the hyperslab according to process number. */
    get_slab(chunk_origin, chunk_dims, count, file_dims, size);

    /* select hyperslab in memory and file spaces.  These two operations are
     * identical since the datasets are the same. */
    memspace  = H5Screate_simple(DIM, file_dims, NULL);
    VRFY((memspace>=0), "memspace");
    ret = H5Sselect_hyperslab(memspace, H5S_SELECT_SET, chunk_origin,
                               chunk_dims, count, chunk_dims);
    VRFY((ret>=0), "mgroup memspace selection");

    filespace = H5Screate_simple(DIM, file_dims,  NULL);
    VRFY((filespace>=0), "filespace");
    ret = H5Sselect_hyperslab(filespace, H5S_SELECT_SET, chunk_origin,
                               chunk_dims, count, chunk_dims);
    VRFY((ret>=0), "mgroup filespace selection");

    /* creates ngroups groups under the root group, writes datasets in
     * parallel. */
    for(m = 0; m < ngroups; m++) {
        sprintf(gname, "group%d", m);
        gid = H5Gcreate2(fid, gname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        VRFY((gid > 0), gname);

        /* create attribute for these groups. */
	write_attribute(gid, is_group, m);

        if(m != 0)
	    write_dataset(memspace, filespace, gid);

        H5Gclose(gid);

#ifdef BARRIER_CHECKS
        if(!((m+1) % 10)) {
            printf("created %d groups\n", m+1);
            MPI_Barrier(MPI_COMM_WORLD);
	}
#endif /* BARRIER_CHECKS */
    }

    /* recursively creates subgroups under the first group. */
    gid = H5Gopen2(fid, "group0", H5P_DEFAULT);
    create_group_recursive(memspace, filespace, gid, 0);
    ret = H5Gclose(gid);
    VRFY((ret>=0), "H5Gclose");

    ret = H5Sclose(filespace);
    VRFY((ret>=0), "H5Sclose");
    ret = H5Sclose(memspace);
    VRFY((ret>=0), "H5Sclose");
    ret = H5Fclose(fid);
    VRFY((ret>=0), "H5Fclose");
}

/*
 * In a group, creates NDATASETS datasets.  Each process writes a hyperslab
 * of a data array to the file.
 *
 * Changes:     Updated function to use a dynamically calculated size,
 *              instead of the old SIZE #define.  This should allow it
 *              to function with an arbitrary number of processors.
 *
 *                                              JRM - 8/16/04
 */
static void
write_dataset(hid_t memspace, hid_t filespace, hid_t gid)
{
    int i, j, n, size;
    int mpi_rank, mpi_size;
    char dname[32];
    DATATYPE * outme = NULL;
    hid_t did;

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

    size = get_size();

    outme = HDmalloc((size_t)(size * size * sizeof(double)));
    VRFY((outme != NULL), "HDmalloc succeeded for outme");

    for(n = 0; n < NDATASET; n++) {
         sprintf(dname, "dataset%d", n);
         did = H5Dcreate2(gid, dname, H5T_NATIVE_INT, filespace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
         VRFY((did > 0), dname);

         for(i = 0; i < size; i++)
             for(j = 0; j < size; j++)
     	         outme[(i * size) + j] = n * 1000 + mpi_rank;

         H5Dwrite(did, H5T_NATIVE_INT, memspace, filespace, H5P_DEFAULT, outme);

         /* create attribute for these datasets.*/
         write_attribute(did, is_dset, n);

         H5Dclose(did);
    }
    HDfree(outme);
}

/*
 * Creates subgroups of depth GROUP_DEPTH recursively.  Also writes datasets
 * in parallel in each group.
 */
static void
create_group_recursive(hid_t memspace, hid_t filespace, hid_t gid, int counter)
{
   hid_t child_gid;
   int   mpi_rank;
   char  gname[64];

   MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

#ifdef BARRIER_CHECKS
   if(!((counter+1) % 10)) {
        printf("created %dth child groups\n", counter+1);
        MPI_Barrier(MPI_COMM_WORLD);
   }
#endif /* BARRIER_CHECKS */

   sprintf(gname, "%dth_child_group", counter+1);
   child_gid = H5Gcreate2(gid, gname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
   VRFY((child_gid > 0), gname);

   /* write datasets in parallel. */
   write_dataset(memspace, filespace, gid);

   if(counter < GROUP_DEPTH )
       create_group_recursive(memspace, filespace, child_gid, counter+1);

   H5Gclose(child_gid);
}

/*
 * This function is to verify the data from multiple group testing.  It opens
 * every dataset in every group and check their correctness.
 *
 * Changes:     Updated function to use a dynamically calculated size,
 *              instead of the old SIZE #define.  This should allow it
 *              to function with an arbitrary number of processors.
 *
 *                                              JRM - 8/11/04
 */
void multiple_group_read(void)
{
    int      mpi_rank, mpi_size, error_num, size;
    int      m;
    hbool_t  use_gpfs = FALSE;
    char     gname[64];
    hid_t    plist, fid, gid, memspace, filespace;
    hsize_t  chunk_origin[DIM];
    hsize_t  chunk_dims[DIM], file_dims[DIM], count[DIM];
    const H5Ptest_param_t *pt;
    char	*filename;
    int		ngroups;

    pt = GetTestParameters();
    filename = pt->name;
    ngroups = pt->count;

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

    size = get_size();

    plist = create_faccess_plist(MPI_COMM_WORLD, MPI_INFO_NULL, facc_type, use_gpfs);
    fid = H5Fopen(filename, H5F_ACC_RDONLY, plist);
    H5Pclose(plist);

    /* decide hyperslab for each process */
    get_slab(chunk_origin, chunk_dims, count, file_dims, size);

    /* select hyperslab for memory and file space */
    memspace  = H5Screate_simple(DIM, file_dims, NULL);
    H5Sselect_hyperslab(memspace, H5S_SELECT_SET, chunk_origin, chunk_dims,
                        count, chunk_dims);
    filespace = H5Screate_simple(DIM, file_dims, NULL);
    H5Sselect_hyperslab(filespace, H5S_SELECT_SET, chunk_origin, chunk_dims,
                        count, chunk_dims);

    /* open every group under root group. */
    for(m=0; m<ngroups; m++) {
        sprintf(gname, "group%d", m);
        gid = H5Gopen2(fid, gname, H5P_DEFAULT);
        VRFY((gid > 0), gname);

        /* check the data. */
        if(m != 0)
            if((error_num = read_dataset(memspace, filespace, gid))>0)
	        nerrors += error_num;

        /* check attribute.*/
        error_num = 0;
        if((error_num = read_attribute(gid, is_group, m))>0 )
	    nerrors += error_num;

        H5Gclose(gid);

#ifdef BARRIER_CHECKS
        if(!((m+1)%10))
            MPI_Barrier(MPI_COMM_WORLD);
#endif /* BARRIER_CHECKS */
    }

    /* open all the groups in vertical direction. */
    gid = H5Gopen2(fid, "group0", H5P_DEFAULT);
    VRFY((gid>0), "group0");
    recursive_read_group(memspace, filespace, gid, 0);
    H5Gclose(gid);

    H5Sclose(filespace);
    H5Sclose(memspace);
    H5Fclose(fid);

}

/*
 * This function opens all the datasets in a certain, checks the data using
 * dataset_vrfy function.
 *
 * Changes:     Updated function to use a dynamically calculated size,
 *              instead of the old SIZE #define.  This should allow it
 *              to function with an arbitrary number of processors.
 *
 *                                              JRM - 8/11/04
 */
static int
read_dataset(hid_t memspace, hid_t filespace, hid_t gid)
{
    int i, j, n, mpi_rank, mpi_size, size, attr_errors=0, vrfy_errors=0;
    char dname[32];
    DATATYPE *outdata = NULL, *indata = NULL;
    hid_t did;

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

    size = get_size();

    indata =(DATATYPE*)HDmalloc((size_t)(size * size * sizeof(DATATYPE)));
    VRFY((indata != NULL), "HDmalloc succeeded for indata");

    outdata =(DATATYPE*)HDmalloc((size_t)(size * size * sizeof(DATATYPE)));
    VRFY((outdata != NULL), "HDmalloc succeeded for outdata");

    for(n=0; n<NDATASET; n++) {
        sprintf(dname, "dataset%d", n);
        did = H5Dopen2(gid, dname, H5P_DEFAULT);
        VRFY((did>0), dname);

        H5Dread(did, H5T_NATIVE_INT, memspace, filespace, H5P_DEFAULT,
                indata);

        /* this is the original value */
        for(i=0; i<size; i++)
	    for(j=0; j<size; j++) {
	         *outdata = n*1000 + mpi_rank;
                 outdata++;
	    }
        outdata -= size * size;

        /* compare the original value(outdata) to the value in file(indata).*/
        vrfy_errors = check_value(indata, outdata, size);

        /* check attribute.*/
        if((attr_errors = read_attribute(did, is_dset, n))>0 )
            vrfy_errors += attr_errors;

        H5Dclose(did);
    }

    HDfree(indata);
    HDfree(outdata);

    return vrfy_errors;
}

/*
 * This recursive function opens all the groups in vertical direction and
 * checks the data.
 */
static void
recursive_read_group(hid_t memspace, hid_t filespace, hid_t gid, int counter)
{
    hid_t child_gid;
    int   mpi_rank, err_num=0;
    char  gname[64];

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
#ifdef BARRIER_CHECKS
    if((counter+1) % 10)
        MPI_Barrier(MPI_COMM_WORLD);
#endif /* BARRIER_CHECKS */

    if((err_num = read_dataset(memspace, filespace, gid)) )
        nerrors += err_num;

    if(counter < GROUP_DEPTH ) {
        sprintf(gname, "%dth_child_group", counter+1);
        child_gid = H5Gopen2(gid, gname, H5P_DEFAULT);
        VRFY((child_gid>0), gname);
        recursive_read_group(memspace, filespace, child_gid, counter+1);
        H5Gclose(child_gid);
    }
}

/* Create and write attribute for a group or a dataset.  For groups, attribute
 * is a scalar datum; for dataset, it is a one-dimensional array.
 */
static void
write_attribute(hid_t obj_id, int this_type, int num)
{
    hid_t   sid, aid;
    hsize_t dspace_dims[1]={8};
    int     i, mpi_rank, attr_data[8], dspace_rank=1;
    char    attr_name[32];

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    if(this_type == is_group) {
        sprintf(attr_name, "Group Attribute %d", num);
        sid = H5Screate(H5S_SCALAR);
        aid = H5Acreate2(obj_id, attr_name, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT);
        H5Awrite(aid, H5T_NATIVE_INT,  &num);
        H5Aclose(aid);
        H5Sclose(sid);
    } /* end if */
    else if(this_type == is_dset) {
        sprintf(attr_name, "Dataset Attribute %d", num);
        for(i=0; i<8; i++)
            attr_data[i] = i;
        sid = H5Screate_simple(dspace_rank, dspace_dims, NULL);
        aid = H5Acreate2(obj_id, attr_name, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT);
        H5Awrite(aid, H5T_NATIVE_INT, attr_data);
        H5Aclose(aid);
        H5Sclose(sid);
    } /* end else-if */

}

/* Read and verify attribute for group or dataset. */
static int
read_attribute(hid_t obj_id, int this_type, int num)
{
    hid_t aid;
    hsize_t group_block[2]={1,1}, dset_block[2]={1, 8};
    int  i, mpi_rank, in_num, in_data[8], out_data[8], vrfy_errors = 0;
    char attr_name[32];

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    if(this_type == is_group) {
        sprintf(attr_name, "Group Attribute %d", num);
        aid = H5Aopen(obj_id, attr_name, H5P_DEFAULT);
        if(MAINPROCESS) {
            H5Aread(aid, H5T_NATIVE_INT, &in_num);
            vrfy_errors =  dataset_vrfy(NULL, NULL, NULL, group_block, &in_num, &num);
	}
        H5Aclose(aid);
    }
    else if(this_type == is_dset) {
        sprintf(attr_name, "Dataset Attribute %d", num);
        for(i=0; i<8; i++)
            out_data[i] = i;
        aid = H5Aopen(obj_id, attr_name, H5P_DEFAULT);
        if(MAINPROCESS) {
            H5Aread(aid, H5T_NATIVE_INT, in_data);
            vrfy_errors = dataset_vrfy(NULL, NULL, NULL, dset_block, in_data, out_data);
	}
        H5Aclose(aid);
    }

    return vrfy_errors;
}

/* This functions compares the original data with the read-in data for its
 * hyperslab part only by process ID.
 *
 * Changes:	Modified function to use a passed in size parameter
 *		instead of the old SIZE #define.  This should let us
 *		run with an arbitrary number of processes.
 *
 *					JRM - 8/16/04
 */
static int
check_value(DATATYPE *indata, DATATYPE *outdata, int size)
{
    int mpi_rank, mpi_size, err_num=0;
    hsize_t i, j;
    hsize_t chunk_origin[DIM];
    hsize_t  chunk_dims[DIM], count[DIM];

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

    get_slab(chunk_origin, chunk_dims, count, NULL, size);

    indata += chunk_origin[0]*size;
    outdata += chunk_origin[0]*size;
    for(i=chunk_origin[0]; i<(chunk_origin[0]+chunk_dims[0]); i++)
         for(j=chunk_origin[1]; j<(chunk_origin[1]+chunk_dims[1]); j++) {
              if(*indata != *outdata )
	          if(err_num++ < MAX_ERR_REPORT || VERBOSE_MED)
		      printf("Dataset Verify failed at [%lu][%lu](row %lu, col%lu): expect %d, got %d\n",(unsigned long)i,(unsigned long)j,(unsigned long)i,(unsigned long)j, *outdata, *indata);
	 }
    if(err_num > MAX_ERR_REPORT && !VERBOSE_MED)
        printf("[more errors ...]\n");
    if(err_num)
        printf("%d errors found in check_value\n", err_num);
    return err_num;
}

/* Decide the portion of data chunk in dataset by process ID.
 *
 * Changes:	Modified function to use a passed in size parameter
 *		instead of the old SIZE #define.  This should let us
 *		run with an arbitrary number of processes.
 *
 *					JRM - 8/11/04
 */

static void
get_slab(hsize_t chunk_origin[], hsize_t chunk_dims[], hsize_t count[],
    hsize_t file_dims[], int size)
{
    int mpi_rank, mpi_size;

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

    if(chunk_origin != NULL) {
        chunk_origin[0] = mpi_rank *(size/mpi_size);
        chunk_origin[1] = 0;
    }
    if(chunk_dims != NULL) {
        chunk_dims[0]   = size/mpi_size;
        chunk_dims[1]   = size;
    }
    if(file_dims != NULL)
        file_dims[0] = file_dims[1] = size;
    if(count != NULL)
        count[0] = count[1] = 1;
}

/*
 * This function is based on bug demonstration code provided by Thomas
 * Guignon(thomas.guignon@ifp.fr), and is intended to verify the
 * correctness of my fix for that bug.
 *
 * In essence, the bug appeared when at least one process attempted to
 * write a point selection -- for which collective I/O is not supported,
 * and at least one other attempted to write some other type of selection
 * for which collective I/O is supported.
 *
 * Since the processes did not compare notes before performing the I/O,
 * some would attempt collective I/O while others performed independent
 * I/O.  A hang resulted.
 *
 * This function reproduces this situation.  At present the test hangs
 * on failure.
 *                                         JRM - 9/13/04
 *
 * Changes:	None.
 */

#define N 4

void io_mode_confusion(void)
{
    /*
     * HDF5 APIs definitions
     */

    const int   rank = 1;
    const char *dataset_name = "IntArray";

    hid_t       file_id, dset_id;         /* file and dataset identifiers */
    hid_t       filespace, memspace;      /* file and memory dataspace */
                                          /* identifiers               */
    hsize_t     dimsf[1];                 /* dataset dimensions */
    int         data[N] = {1};            /* pointer to data buffer to write */
    hsize_t     coord[N] = {0L,1L,2L,3L};
    hid_t       plist_id;                 /* property list identifier */
    herr_t      status;


    /*
     * MPI variables
     */

    int mpi_size, mpi_rank;


    /*
     * test bed related variables
     */

    const char *	fcn_name = "io_mode_confusion";
    const hbool_t	verbose = FALSE;
    const H5Ptest_param_t *	pt;
    char *		filename;


    pt = GetTestParameters();
    filename = pt->name;

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

    /*
     * Set up file access property list with parallel I/O access
     */

    if(verbose )
        HDfprintf(stdout, "%0d:%s: Setting up property list.\n",
                  mpi_rank, fcn_name);

    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id != -1), "H5Pcreate() failed");

    status = H5Pset_fapl_mpio(plist_id, MPI_COMM_WORLD, MPI_INFO_NULL);
    VRFY((status >= 0 ), "H5Pset_fapl_mpio() failed");


    /*
     * Create a new file collectively and release property list identifier.
     */

    if(verbose )
        HDfprintf(stdout, "%0d:%s: Creating new file.\n", mpi_rank, fcn_name);

    file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, plist_id);
    VRFY((file_id >= 0 ), "H5Fcreate() failed");

    status = H5Pclose(plist_id);
    VRFY((status >= 0 ), "H5Pclose() failed");


    /*
     * Create the dataspace for the dataset.
     */

    if(verbose )
        HDfprintf(stdout, "%0d:%s: Creating the dataspace for the dataset.\n",
                  mpi_rank, fcn_name);

    dimsf[0] = N;
    filespace = H5Screate_simple(rank, dimsf, NULL);
    VRFY((filespace >= 0 ), "H5Screate_simple() failed.");


    /*
     * Create the dataset with default properties and close filespace.
     */

    if(verbose )
        HDfprintf(stdout,
                  "%0d:%s: Creating the dataset, and closing filespace.\n",
                  mpi_rank, fcn_name);

    dset_id = H5Dcreate2(file_id, dataset_name, H5T_NATIVE_INT, filespace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((dset_id >= 0 ), "H5Dcreate2() failed");

    status = H5Sclose(filespace);
    VRFY((status >= 0 ), "H5Sclose() failed");


    if(verbose )
        HDfprintf(stdout, "%0d:%s: Calling H5Screate_simple().\n",
                mpi_rank, fcn_name);

    memspace = H5Screate_simple(rank, dimsf, NULL);
    VRFY((memspace >= 0 ), "H5Screate_simple() failed.");


    if(mpi_rank == 0 ) {
        if(verbose )
            HDfprintf(stdout, "%0d:%s: Calling H5Sselect_all(memspace).\n",
                      mpi_rank, fcn_name);

        status = H5Sselect_all(memspace);
        VRFY((status >= 0 ), "H5Sselect_all() failed");
    } else {
        if(verbose )
            HDfprintf(stdout, "%0d:%s: Calling H5Sselect_none(memspace).\n",
                      mpi_rank, fcn_name);

        status = H5Sselect_none(memspace);
        VRFY((status >= 0 ), "H5Sselect_none() failed");
    }


    if(verbose )
        HDfprintf(stdout, "%0d:%s: Calling MPI_Barrier().\n",
                  mpi_rank, fcn_name);

    MPI_Barrier(MPI_COMM_WORLD);


    if(verbose )
        HDfprintf(stdout, "%0d:%s: Calling H5Dget_space().\n",
                  mpi_rank, fcn_name);

    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0 ), "H5Dget_space() failed");


    /* select all */
    if(mpi_rank == 0 ) {
        if(verbose )
             HDfprintf(stdout,
                       "%0d:%s: Calling H5Sselect_elements() -- set up hang?\n",
                       mpi_rank, fcn_name);

        status = H5Sselect_elements(filespace, H5S_SELECT_SET, N, (const hsize_t *)&coord);
        VRFY((status >= 0 ), "H5Sselect_elements() failed");
    } else { /* select nothing */
        if(verbose )
            HDfprintf(stdout, "%0d:%s: Calling H5Sselect_none().\n",
                      mpi_rank, fcn_name);

        status = H5Sselect_none(filespace);
        VRFY((status >= 0 ), "H5Sselect_none() failed");
    }

    if(verbose )
        HDfprintf(stdout, "%0d:%s: Calling MPI_Barrier().\n",
                  mpi_rank, fcn_name);

    MPI_Barrier(MPI_COMM_WORLD);


    if(verbose )
        HDfprintf(stdout, "%0d:%s: Calling H5Pcreate().\n", mpi_rank, fcn_name);

    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id != -1 ), "H5Pcreate() failed");


    if(verbose )
        HDfprintf(stdout, "%0d:%s: Calling H5Pset_dxpl_mpio().\n",
                  mpi_rank, fcn_name);

    status = H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE);
    VRFY((status >= 0 ), "H5Pset_dxpl_mpio() failed");
    if(dxfer_coll_type == DXFER_INDEPENDENT_IO) {
     status = H5Pset_dxpl_mpio_collective_opt(plist_id, H5FD_MPIO_INDIVIDUAL_IO);
     VRFY((status>= 0),"set independent IO collectively succeeded");
    }




    if(verbose )
        HDfprintf(stdout, "%0d:%s: Calling H5Dwrite() -- hang here?.\n",
                  mpi_rank, fcn_name);

    status = H5Dwrite(dset_id, H5T_NATIVE_INT, memspace, filespace,
                      plist_id, data);

    if(verbose )
        HDfprintf(stdout, "%0d:%s: Returned from H5Dwrite(), status=%d.\n",
                  mpi_rank, fcn_name, status);
    VRFY((status >= 0 ), "H5Dwrite() failed");

    /*
     * Close/release resources.
     */

    if(verbose )
        HDfprintf(stdout, "%0d:%s: Cleaning up from test.\n",
                  mpi_rank, fcn_name);

    status = H5Dclose(dset_id);
    VRFY((status >= 0 ), "H5Dclose() failed");

    status = H5Sclose(filespace);
    VRFY((status >= 0 ), "H5Dclose() failed");

    status = H5Sclose(memspace);
    VRFY((status >= 0 ), "H5Sclose() failed");

    status = H5Pclose(plist_id);
    VRFY((status >= 0 ), "H5Pclose() failed");

    status = H5Fclose(file_id);
    VRFY((status >= 0 ), "H5Fclose() failed");


    if(verbose )
        HDfprintf(stdout, "%0d:%s: Done.\n", mpi_rank, fcn_name);

    return;

} /* io_mode_confusion() */

#undef N

/*
 * At present, the object header code maintains an image of its on disk
 * representation, which is updates as necessary instead of generating on
 * request.  
 *
 * Prior to the fix that this test in designed to verify, the image of the
 * on disk representation was only updated on flush -- not when the object
 * header was marked clean.
 *
 * This worked perfectly well as long as all writes of a given object 
 * header were written from a single process.  However, with the implementation
 * of round robin metadata data writes in parallel HDF5, this is no longer
 * the case -- it is possible for a given object header to be flushed from
 * several different processes, with the object header simply being marked
 * clean in all other processes on each flush.  This resulted in NULL or
 * out of data object header information being written to disk.
 *
 * To repair this, I modified the object header code to update its 
 * on disk image both on flush on when marked clean.  
 *
 * This test is directed at verifying that the fix performs as expected.
 *
 * The test functions by creating a HDF5 file with several small datasets,
 * and then flushing the file.  This should result of at least one of 
 * the associated object headers being flushed by a process other than 
 * process 0.
 *
 * Then for each data set, add an attribute and flush the file again.
 *
 * Close the file and re-open it.
 *
 * Open the each of the data sets in turn.  If all opens are successful,
 * the test passes.  Otherwise the test fails.
 *
 * Note that this test will probably become irrelevent shortly, when we 
 * land the journaling modifications on the trunk -- at which point all
 * cache clients will have to construct on disk images on demand.
 *
 *						JRM -- 10/13/10
 *
 * Changes:
 *	Break it into two parts, a writer to write the file and a reader
 *	the correctness of the writer. AKC -- 2010/10/27
 */

#define NUM_DATA_SETS	4
#define LOCAL_DATA_SIZE	4
#define LARGE_ATTR_SIZE	256
/* Since all even and odd processes are split into writer and reader comm
 * respectively, process 0 and 1 in COMM_WORLD become the root process of
 * the writer and reader comm respectively.
 */
#define Writer_Root	0
#define Reader_Root	1
#define Reader_wait(mpi_err, xsteps) \
    mpi_err = MPI_Bcast(&xsteps, 1, MPI_INT, Writer_Root, MPI_COMM_WORLD)
#define Reader_result(mpi_err, xsteps_done) \
    mpi_err = MPI_Bcast(&xsteps_done, 1, MPI_INT, Reader_Root, MPI_COMM_WORLD)
#define Reader_check(mpi_err, xsteps, xsteps_done) \
    { Reader_wait(mpi_err, xsteps); \
      Reader_result(mpi_err, xsteps_done);}

/* object names used by both rr_obj_hdr_flush_confusion and
 * rr_obj_hdr_flush_confusion_reader.
 */
const char * dataset_name[NUM_DATA_SETS] = 
    { 
	"dataset_0",
	"dataset_1",
	"dataset_2",
	"dataset_3"
    };
const char * att_name[NUM_DATA_SETS] = 
    { 
	"attribute_0",
	"attribute_1",
	"attribute_2",
	"attribute_3"
    };
const char * lg_att_name[NUM_DATA_SETS] = 
    { 
	"large_attribute_0",
	"large_attribute_1",
	"large_attribute_2",
	"large_attribute_3"
    };

void rr_obj_hdr_flush_confusion(void)
{
    /* MPI variables */
    /* private communicator size and rank */
    int mpi_size;
    int mpi_rank;
    int mrc;		/* mpi error code */
    int is_reader;	/* 1 for reader process; 0 for writer process. */
    MPI_Comm comm;


    /* test bed related variables */
    const char *		fcn_name = "rr_obj_hdr_flush_confusion";
    const hbool_t		verbose = FALSE;

    /* Create two new private communicators from MPI_COMM_WORLD.
     * Even and odd ranked processes go to comm_writers and comm_readers
     * respectively.
     */
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    if (mpi_size < 3){
	HDfprintf(stdout, "%s needs at least 3 processes to run. Terminated.\n",
	    fcn_name);
	nerrors++;
	return;
    }
    is_reader = mpi_rank % 2;
    mrc = MPI_Comm_split(MPI_COMM_WORLD, is_reader, mpi_rank, &comm);
    VRFY((mrc==MPI_SUCCESS), "MPI_Comm_split");

    /* The reader proocesses branches off to do reading
     * while the writer processes continues to do writing
     * Whenever writers finish one writing step, including a H5Fflush,
     * they inform the readers, via MPI_COMM_WORLD, to verify.
     * They will wait for the result from the readers before doing the next
     * step. When all steps are done, they inform readers to end.
     */
    if (is_reader)
	rr_obj_hdr_flush_confusion_reader(comm);
    else
	rr_obj_hdr_flush_confusion_writer(comm);

    MPI_Comm_free(&comm);
    if(verbose )
        HDfprintf(stdout, "%0d:%s: Done.\n", mpi_rank, fcn_name);

    return;

} /* rr_obj_hdr_flush_confusion() */

void rr_obj_hdr_flush_confusion_writer(MPI_Comm comm)
{
    int i;
    int j;
    hid_t file_id = -1;
    hid_t fapl_id = -1;
    hid_t dxpl_id = -1;
    hid_t att_id[NUM_DATA_SETS];
    hid_t att_space[NUM_DATA_SETS];
    hid_t lg_att_id[NUM_DATA_SETS];
    hid_t lg_att_space[NUM_DATA_SETS];
    hid_t disk_space[NUM_DATA_SETS];
    hid_t mem_space[NUM_DATA_SETS];
    hid_t dataset[NUM_DATA_SETS];
    hsize_t att_size[1];
    hsize_t lg_att_size[1];
    hsize_t disk_count[1];
    hsize_t disk_size[1];
    hsize_t disk_start[1];
    hsize_t mem_count[1];
    hsize_t mem_size[1];
    hsize_t mem_start[1];
    herr_t err;
    double data[LOCAL_DATA_SIZE];
    double att[LOCAL_DATA_SIZE];
    double lg_att[LARGE_ATTR_SIZE];

    /* MPI variables */
    /* world communication size and rank */
    int mpi_world_size;
    int mpi_world_rank;
    /* private communicator size and rank */
    int mpi_size;
    int mpi_rank;
    int mrc;		/* mpi error code */
    /* steps to verify and have been verified */
    int steps = 0;
    int steps_done = 0;

    /* test bed related variables */
    const char *		fcn_name = "rr_obj_hdr_flush_confusion_writer";
    const hbool_t		verbose = FALSE;
    const H5Ptest_param_t *	pt;
    char *			filename;

    /*
     * setup test bed related variables:
     */

    pt = (const H5Ptest_param_t *)GetTestParameters();
    filename = pt->name;

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_world_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_world_size);
    MPI_Comm_rank(comm, &mpi_rank);
    MPI_Comm_size(comm, &mpi_size);

    /*
     * Set up file access property list with parallel I/O access
     */

    if(verbose )
        HDfprintf(stdout, "%0d:%s: Setting up property list.\n",
                  mpi_rank, fcn_name);

    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((fapl_id != -1), "H5Pcreate(H5P_FILE_ACCESS) failed");

    err = H5Pset_fapl_mpio(fapl_id, comm, MPI_INFO_NULL);
    VRFY((err >= 0 ), "H5Pset_fapl_mpio() failed");


    /*
     * Create a new file collectively and release property list identifier.
     */

    if(verbose )
        HDfprintf(stdout, "%0d:%s: Creating new file \"%s\".\n", 
                  mpi_rank, fcn_name, filename);

    file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    VRFY((file_id >= 0 ), "H5Fcreate() failed");

    err = H5Pclose(fapl_id);
    VRFY((err >= 0 ), "H5Pclose(fapl_id) failed");


    /*
     * Step 1: create the data sets and write data.
     */

    if(verbose )
        HDfprintf(stdout, "%0d:%s: Creating the datasets.\n", 
                  mpi_rank, fcn_name);

    disk_size[0] = (hsize_t)(LOCAL_DATA_SIZE * mpi_size);
    mem_size[0] = (hsize_t)(LOCAL_DATA_SIZE);

    for ( i = 0; i < NUM_DATA_SETS; i++ ) {

        disk_space[i] = H5Screate_simple(1, disk_size, NULL);
	VRFY((disk_space[i] >= 0), "H5Screate_simple(1) failed.\n");

	dataset[i] = H5Dcreate2(file_id, dataset_name[i], H5T_NATIVE_DOUBLE,
                      disk_space[i], H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

        VRFY((dataset[i] >= 0), "H5Dcreate(1) failed.\n");
    }

    /* 
     * setup data transfer property list
     */

    if(verbose )
        HDfprintf(stdout, "%0d:%s: Setting up dxpl.\n", mpi_rank, fcn_name);

    dxpl_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((dxpl_id != -1), "H5Pcreate(H5P_DATASET_XFER) failed.\n");

    err = H5Pset_dxpl_mpio(dxpl_id, H5FD_MPIO_COLLECTIVE);
    VRFY((err >= 0), 
         "H5Pset_dxpl_mpio(dxpl_id, H5FD_MPIO_COLLECTIVE) failed.\n");

    /* 
     * write data to the data sets 
     */

    if(verbose )
        HDfprintf(stdout, "%0d:%s: Writing datasets.\n", mpi_rank, fcn_name);

    disk_count[0] = (hsize_t)(LOCAL_DATA_SIZE);
    disk_start[0] = (hsize_t)(LOCAL_DATA_SIZE * mpi_rank);
    mem_count[0] = (hsize_t)(LOCAL_DATA_SIZE);
    mem_start[0] = (hsize_t)(0);

    for ( j = 0; j < LOCAL_DATA_SIZE; j++ ) {
        data[j] = (double)(mpi_rank + 1);
    }

    for ( i = 0; i < NUM_DATA_SETS; i++ ) {
	err = H5Sselect_hyperslab(disk_space[i], H5S_SELECT_SET, disk_start,
                            NULL, disk_count, NULL);
        VRFY((err >= 0), "H5Sselect_hyperslab(1) failed.\n");
        mem_space[i] = H5Screate_simple(1, mem_size, NULL);
	VRFY((mem_space[i] >= 0), "H5Screate_simple(2) failed.\n");
	err = H5Sselect_hyperslab(mem_space[i], H5S_SELECT_SET,
                            mem_start, NULL, mem_count, NULL);
        VRFY((err >= 0), "H5Sselect_hyperslab(2) failed.\n");
	err = H5Dwrite(dataset[i], H5T_NATIVE_DOUBLE, mem_space[i], 
                       disk_space[i], dxpl_id, data);
        VRFY((err >= 0), "H5Dwrite(1) failed.\n");
        for ( j = 0; j < LOCAL_DATA_SIZE; j++ )
	    data[j] *= 10.0;
    }

    /* 
     * close the data spaces
     */

    if(verbose )
        HDfprintf(stdout, "%0d:%s: closing dataspaces.\n", mpi_rank, fcn_name);

    for ( i = 0; i < NUM_DATA_SETS; i++ ) {
        err = H5Sclose(disk_space[i]);
        VRFY((err >= 0), "H5Sclose(disk_space[i]) failed.\n");
        err = H5Sclose(mem_space[i]);
        VRFY((err >= 0), "H5Sclose(mem_space[i]) failed.\n");
    }

    /* End of Step 1: create the data sets and write data. */

    /* 
     * flush the metadata cache
     */

    if(verbose )
        HDfprintf(stdout, "%0d:%s: flushing metadata cache.\n", 
                  mpi_rank, fcn_name);
    err = H5Fflush(file_id, H5F_SCOPE_GLOBAL);
    VRFY((err >= 0), "H5Fflush(1) failed.\n");

    /* Tell the reader to check the file up to steps. */
    steps++;
    Reader_check(mrc, steps, steps_done);

    /*
     * Step 2: write attributes to each dataset
     */

    if(verbose )
        HDfprintf(stdout, "%0d:%s: writing attributes.\n", mpi_rank, fcn_name);

    att_size[0] = (hsize_t)(LOCAL_DATA_SIZE);
    for ( j = 0; j < LOCAL_DATA_SIZE; j++ ) {
        att[j] = (double)(j + 1);
    }

    for ( i = 0; i < NUM_DATA_SETS; i++ ) {
        att_space[i] = H5Screate_simple(1, att_size, NULL);
        VRFY((att_space[i] >= 0), "H5Screate_simple(3) failed.\n");
	att_id[i] = H5Acreate2(dataset[i], att_name[i], H5T_NATIVE_DOUBLE,
                               att_space[i], H5P_DEFAULT, H5P_DEFAULT);
        VRFY((att_id[i] >= 0), "H5Acreate(1) failed.\n");
        err = H5Awrite(att_id[i], H5T_NATIVE_DOUBLE, att);
        VRFY((err >= 0), "H5Awrite(1) failed.\n");
        for ( j = 0; j < LOCAL_DATA_SIZE; j++ ) {
            att[j] /= 10.0;
        }
    }

    /*
     * close attribute IDs and spaces 
     */

    if(verbose )
        HDfprintf(stdout, "%0d:%s: closing attr ids and spaces .\n", 
                  mpi_rank, fcn_name);

    for ( i = 0; i < NUM_DATA_SETS; i++ ) {

        err = H5Sclose(att_space[i]);
        VRFY((err >= 0), "H5Sclose(att_space[i]) failed.\n");
        err = H5Aclose(att_id[i]);
        VRFY((err >= 0), "H5Aclose(att_id[i]) failed.\n");
    }

    /* End of Step 2: write attributes to each dataset */

    /* 
     * flush the metadata cache again
     */

    if(verbose )
        HDfprintf(stdout, "%0d:%s: flushing metadata cache.\n", 
                  mpi_rank, fcn_name);
    err = H5Fflush(file_id, H5F_SCOPE_GLOBAL);
    VRFY((err >= 0), "H5Fflush(2) failed.\n");

    /* Tell the reader to check the file up to steps. */
    steps++;
    Reader_check(mrc, steps, steps_done);

    /*
     * Step 3: write large attributes to each dataset
     */

    if(verbose )
        HDfprintf(stdout, "%0d:%s: writing large attributes.\n", 
                  mpi_rank, fcn_name);

    lg_att_size[0] = (hsize_t)(LARGE_ATTR_SIZE);

    for ( j = 0; j < LARGE_ATTR_SIZE; j++ ) {
        lg_att[j] = (double)(j + 1);
    }

    for ( i = 0; i < NUM_DATA_SETS; i++ ) {
        lg_att_space[i] = H5Screate_simple(1, lg_att_size, NULL);
        VRFY((lg_att_space[i] >= 0), "H5Screate_simple(4) failed.\n");
	lg_att_id[i] = H5Acreate2(dataset[i], lg_att_name[i], H5T_NATIVE_DOUBLE,
                                  lg_att_space[i], H5P_DEFAULT, H5P_DEFAULT);
        VRFY((lg_att_id[i] >= 0), "H5Acreate(2) failed.\n");
        err = H5Awrite(lg_att_id[i], H5T_NATIVE_DOUBLE, lg_att);
        VRFY((err >= 0), "H5Awrite(2) failed.\n");
        for ( j = 0; j < LARGE_ATTR_SIZE; j++ ) {
            lg_att[j] /= 10.0;
        }
    }
    
    /* Step 3: write large attributes to each dataset */

    /* 
     * flush the metadata cache yet again to clean the object headers.
     *
     * This is an attempt to crate a situation where we have dirty
     * object header continuation chunks, but clean opject headers
     * to verify a speculative bug fix -- it doesn't seem to work,
     * but I will leave the code in anyway, as the object header 
     * code is going to change a lot in the near future.
     */

    if(verbose )
        HDfprintf(stdout, "%0d:%s: flushing metadata cache.\n", 
                  mpi_rank, fcn_name);
    err = H5Fflush(file_id, H5F_SCOPE_GLOBAL);
    VRFY((err >= 0), "H5Fflush(3) failed.\n");

    /* Tell the reader to check the file up to steps. */
    steps++;
    Reader_check(mrc, steps, steps_done);

    /*
     * Step 4: write different large attributes to each dataset
     */

    if(verbose )
        HDfprintf(stdout, "%0d:%s: writing different large attributes.\n", 
                  mpi_rank, fcn_name);

    for ( j = 0; j < LARGE_ATTR_SIZE; j++ ) {
        lg_att[j] = (double)(j + 2);
    }

    for ( i = 0; i < NUM_DATA_SETS; i++ ) {
        err = H5Awrite(lg_att_id[i], H5T_NATIVE_DOUBLE, lg_att);
        VRFY((err >= 0), "H5Awrite(2) failed.\n");
        for ( j = 0; j < LARGE_ATTR_SIZE; j++ ) {
            lg_att[j] /= 10.0;
        }
    }

    /* End of Step 4: write different large attributes to each dataset */

    /* 
     * flush the metadata cache again
     */
    if(verbose )
        HDfprintf(stdout, "%0d:%s: flushing metadata cache.\n", 
                  mpi_rank, fcn_name);
    err = H5Fflush(file_id, H5F_SCOPE_GLOBAL);
    VRFY((err >= 0), "H5Fflush(3) failed.\n");

    /* Tell the reader to check the file up to steps. */
    steps++;
    Reader_check(mrc, steps, steps_done);

    /* Step 5: Close all objects and the file */

    /*
     * close large attribute IDs and spaces 
     */

    if(verbose )
        HDfprintf(stdout, "%0d:%s: closing large attr ids and spaces .\n", 
                  mpi_rank, fcn_name);

    for ( i = 0; i < NUM_DATA_SETS; i++ ) {

        err = H5Sclose(lg_att_space[i]);
        VRFY((err >= 0), "H5Sclose(lg_att_space[i]) failed.\n");
        err = H5Aclose(lg_att_id[i]);
        VRFY((err >= 0), "H5Aclose(lg_att_id[i]) failed.\n");
    }


    /* 
     * close the data sets
     */

    if(verbose )
        HDfprintf(stdout, "%0d:%s: closing datasets .\n", mpi_rank, fcn_name);

    for ( i = 0; i < NUM_DATA_SETS; i++ ) {
        err = H5Dclose(dataset[i]);
        VRFY((err >= 0), "H5Dclose(dataset[i])1 failed.\n");
    }

    /*
     * close the data transfer property list.
     */

    if(verbose )
        HDfprintf(stdout, "%0d:%s: closing dxpl .\n", mpi_rank, fcn_name);

    err = H5Pclose(dxpl_id);
    VRFY((err >= 0), "H5Pclose(dxpl_id) failed.\n");


    /*
     * Close file.
     */

    if(verbose )
        HDfprintf(stdout, "%0d:%s: closing file.\n", mpi_rank, fcn_name);

    err = H5Fclose(file_id);
    VRFY((err >= 0 ), "H5Fclose(1) failed");
    
    /* End of Step 5: Close all objects and the file */
    /* Tell the reader to check the file up to steps. */
    steps++;
    Reader_check(mrc, steps, steps_done);


    /* All done. Inform reader to end. */
    steps=0;
    Reader_check(mrc, steps, steps_done);

    if(verbose )
        HDfprintf(stdout, "%0d:%s: Done.\n", mpi_rank, fcn_name);

    return;

} /* rr_obj_hdr_flush_confusion_writer() */

void rr_obj_hdr_flush_confusion_reader(MPI_Comm comm)
{
    int i;
    int j;
    hid_t file_id = -1;
    hid_t fapl_id = -1;
    hid_t dxpl_id = -1;
    hid_t lg_att_id[NUM_DATA_SETS];
    hid_t lg_att_type[NUM_DATA_SETS];
    hid_t disk_space[NUM_DATA_SETS];
    hid_t mem_space[NUM_DATA_SETS];
    hid_t dataset[NUM_DATA_SETS];
    hsize_t disk_count[1];
    hsize_t disk_start[1];
    hsize_t mem_count[1];
    hsize_t mem_size[1];
    hsize_t mem_start[1];
    herr_t err;
    htri_t tri_err;
    double data[LOCAL_DATA_SIZE];
    double data_read[LOCAL_DATA_SIZE];
    double att[LOCAL_DATA_SIZE];
    double att_read[LOCAL_DATA_SIZE];
    double lg_att[LARGE_ATTR_SIZE];
    double lg_att_read[LARGE_ATTR_SIZE];

    /* MPI variables */
    /* world communication size and rank */
    int mpi_world_size;
    int mpi_world_rank;
    /* private communicator size and rank */
    int mpi_size;
    int mpi_rank;
    int mrc;		/* mpi error code */
    int	steps = -1;		/* How far (steps) to verify the file */
    int	steps_done = -1;	/* How far (steps) have been verified */

    /* test bed related variables */
    const char *		fcn_name = "rr_obj_hdr_flush_confusion_reader";
    const hbool_t		verbose = FALSE;
    const H5Ptest_param_t *	pt;
    char *			filename;

    /*
     * setup test bed related variables:
     */

    pt = (const H5Ptest_param_t *)GetTestParameters();
    filename = pt->name;

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_world_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_world_size);
    MPI_Comm_rank(comm, &mpi_rank);
    MPI_Comm_size(comm, &mpi_size);

    /* Repeatedly re-open the file and verify its contents until it is */
    /* told to end (when steps=0). */
    while (steps_done != 0){
	Reader_wait(mrc, steps);
	VRFY((mrc >= 0), "Reader_wait failed");
	steps_done = 0;

	if (steps > 0 ){
	    /*
	     * Set up file access property list with parallel I/O access
	     */

	    if(verbose )
		HDfprintf(stdout, "%0d:%s: Setting up property list.\n",
			  mpi_rank, fcn_name);

	    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
	    VRFY((fapl_id != -1), "H5Pcreate(H5P_FILE_ACCESS) failed");
	    err = H5Pset_fapl_mpio(fapl_id, comm, MPI_INFO_NULL);
	    VRFY((err >= 0 ), "H5Pset_fapl_mpio() failed");

	    /*
	     * Create a new file collectively and release property list identifier.
	     */

	    if(verbose )
		HDfprintf(stdout, "%0d:%s: Re-open file \"%s\".\n", 
			  mpi_rank, fcn_name, filename);

	    file_id = H5Fopen(filename, H5F_ACC_RDONLY, fapl_id);
	    VRFY((file_id >= 0 ), "H5Fopen() failed");
	    err = H5Pclose(fapl_id);
	    VRFY((err >= 0 ), "H5Pclose(fapl_id) failed");

#if 1
	    if (steps >= 1){
	        /*=====================================================*
		 * Step 1: open the data sets and read data.
		 *=====================================================*/

		if(verbose )
		    HDfprintf(stdout, "%0d:%s: opening the datasets.\n", 
			      mpi_rank, fcn_name);

		for ( i = 0; i < NUM_DATA_SETS; i++ ) { 
		    dataset[i] = -1;
		}

		for ( i = 0; i < NUM_DATA_SETS; i++ ) {
		    dataset[i] = H5Dopen2(file_id, dataset_name[i], H5P_DEFAULT);
		    VRFY((dataset[i] >= 0), "H5Dopen(1) failed.\n");
		    disk_space[i] = H5Dget_space(dataset[i]);
		    VRFY((disk_space[i] >= 0), "H5Dget_space failed.\n");
		}

		/* 
		 * setup data transfer property list
		 */

		if(verbose )
		    HDfprintf(stdout, "%0d:%s: Setting up dxpl.\n", mpi_rank, fcn_name);

		dxpl_id = H5Pcreate(H5P_DATASET_XFER);
		VRFY((dxpl_id != -1), "H5Pcreate(H5P_DATASET_XFER) failed.\n");
		err = H5Pset_dxpl_mpio(dxpl_id, H5FD_MPIO_COLLECTIVE);
		VRFY((err >= 0), 
		     "H5Pset_dxpl_mpio(dxpl_id, H5FD_MPIO_COLLECTIVE) failed.\n");

		/* 
		 * read data from the data sets 
		 */

		if(verbose )
		    HDfprintf(stdout, "%0d:%s: Reading datasets.\n", mpi_rank, fcn_name);

		disk_count[0] = (hsize_t)(LOCAL_DATA_SIZE);
		disk_start[0] = (hsize_t)(LOCAL_DATA_SIZE * mpi_rank);

    		mem_size[0] = (hsize_t)(LOCAL_DATA_SIZE);

		mem_count[0] = (hsize_t)(LOCAL_DATA_SIZE);
		mem_start[0] = (hsize_t)(0);

		/* set up expected data for verification */
		for ( j = 0; j < LOCAL_DATA_SIZE; j++ ) {
		    data[j] = (double)(mpi_rank + 1);
		}

		for ( i = 0; i < NUM_DATA_SETS; i++ ) {
		    err = H5Sselect_hyperslab(disk_space[i], H5S_SELECT_SET, disk_start,
					NULL, disk_count, NULL);
		    VRFY((err >= 0), "H5Sselect_hyperslab(1) failed.\n");
		    mem_space[i] = H5Screate_simple(1, mem_size, NULL);
		    VRFY((mem_space[i] >= 0), "H5Screate_simple(2) failed.\n");
		    err = H5Sselect_hyperslab(mem_space[i], H5S_SELECT_SET,
					mem_start, NULL, mem_count, NULL);
		    VRFY((err >= 0), "H5Sselect_hyperslab(2) failed.\n");
		    err = H5Dread(dataset[i], H5T_NATIVE_DOUBLE, mem_space[i], 
				   disk_space[i], dxpl_id, data_read);
		    VRFY((err >= 0), "H5Dread(1) failed.\n");

		    /* compare read data with expected data */
		    for ( j = 0; j < LOCAL_DATA_SIZE; j++ )
			if (data_read[j] != data[j]){
			    HDfprintf(stdout,
				"%0d:%s: Reading datasets value failed in "
				"Dataset %d, at position %d: expect %f, got %f.\n",
				mpi_rank, fcn_name, i, j, data[j], data_read[j]);
			    nerrors++;
			}
		    for ( j = 0; j < LOCAL_DATA_SIZE; j++ )
			data[j] *= 10.0;
		}

		/* 
		 * close the data spaces
		 */

		if(verbose )
		    HDfprintf(stdout, "%0d:%s: closing dataspaces.\n", mpi_rank, fcn_name);

		for ( i = 0; i < NUM_DATA_SETS; i++ ) {
		    err = H5Sclose(disk_space[i]);
		    VRFY((err >= 0), "H5Sclose(disk_space[i]) failed.\n");
		    err = H5Sclose(mem_space[i]);
		    VRFY((err >= 0), "H5Sclose(mem_space[i]) failed.\n");
		}
		steps_done++;
	    }
	    /* End of Step 1: open the data sets and read data. */
#endif

#if 1
	    /*=====================================================*
	     * Step 2: reading attributes from each dataset
	     *=====================================================*/

	    if (steps >= 2){
		if(verbose )
		    HDfprintf(stdout, "%0d:%s: reading attributes.\n", mpi_rank, fcn_name);

		for ( j = 0; j < LOCAL_DATA_SIZE; j++ ) {

		    att[j] = (double)(j + 1);
		}

		for ( i = 0; i < NUM_DATA_SETS; i++ ) {
		    hid_t att_id, att_type;

		    att_id = H5Aopen(dataset[i], att_name[i], H5P_DEFAULT);
		    VRFY((att_id >= 0), "H5Aopen failed.\n");
		    att_type = H5Aget_type(att_id);
		    VRFY((att_type >= 0), "H5Aget_type failed.\n");
		    tri_err = H5Tequal(att_type, H5T_NATIVE_DOUBLE);
		    VRFY((tri_err >= 0), "H5Tequal failed.\n");
		    if (tri_err==0){
			HDfprintf(stdout,
			    "%0d:%s: Mismatched Attribute type of Dataset %d.\n",
			    mpi_rank, fcn_name, i);
			    nerrors++;
		    }else{
			/* should verify attribute size before H5Aread */
			err = H5Aread(att_id, H5T_NATIVE_DOUBLE, att_read);
			VRFY((err >= 0), "H5Aread failed.\n");
			/* compare read attribute data with expected data */
			for ( j = 0; j < LOCAL_DATA_SIZE; j++ )
			    if (att_read[j] != att[j]){
				HDfprintf(stdout,
				    "%0d:%s: Mismatched attribute data read in Dataset %d, at position %d: expect %f, got %f.\n",
				    mpi_rank, fcn_name, i, j, att[j], att_read[j]);
				    nerrors++;
			    }
			for ( j = 0; j < LOCAL_DATA_SIZE; j++ ) {

			    att[j] /= 10.0;
			}
		    }
		    err = H5Aclose(att_id);
		    VRFY((err >= 0), "H5Aclose failed.\n");
		}
		steps_done++;
	    }
	    /* End of Step 2: reading attributes from each dataset */
#endif


#if 1
	    /*=====================================================*
	     * Step 3 or 4: read large attributes from each dataset.
	     * Step 4 has different attribute value from step 3.
	     *=====================================================*/

	    if (steps >= 3){
		if(verbose )
		    HDfprintf(stdout, "%0d:%s: reading large attributes.\n", mpi_rank, fcn_name);

		for ( j = 0; j < LARGE_ATTR_SIZE; j++ ) {

		    lg_att[j] = (steps==3) ? (double)(j + 1) : (double)(j+2);
		}

		for ( i = 0; i < NUM_DATA_SETS; i++ ) {
		    lg_att_id[i] = H5Aopen(dataset[i], lg_att_name[i], H5P_DEFAULT);
		    VRFY((lg_att_id[i] >= 0), "H5Aopen(2) failed.\n");
		    lg_att_type[i] = H5Aget_type(lg_att_id[i]);
		    VRFY((err >= 0), "H5Aget_type failed.\n");
		    tri_err = H5Tequal(lg_att_type[i], H5T_NATIVE_DOUBLE);
		    VRFY((tri_err >= 0), "H5Tequal failed.\n");
		    if (tri_err==0){
			HDfprintf(stdout,
			    "%0d:%s: Mismatched Large attribute type of Dataset %d.\n",
			    mpi_rank, fcn_name, i);
			    nerrors++;
		    }else{
			/* should verify large attribute size before H5Aread */
			err = H5Aread(lg_att_id[i], H5T_NATIVE_DOUBLE, lg_att_read);
			VRFY((err >= 0), "H5Aread failed.\n");
			/* compare read attribute data with expected data */
			for ( j = 0; j < LARGE_ATTR_SIZE; j++ )
			    if (lg_att_read[j] != lg_att[j]){
				HDfprintf(stdout,
				    "%0d:%s: Mismatched large attribute data read in Dataset %d, at position %d: expect %f, got %f.\n",
				    mpi_rank, fcn_name, i, j, lg_att[j], lg_att_read[j]);
				    nerrors++;
			    }
			for ( j = 0; j < LARGE_ATTR_SIZE; j++ ) {

			    lg_att[j] /= 10.0;
			}
		    }
		    err = H5Tclose(lg_att_type[i]);
		    VRFY((err >= 0), "H5Tclose failed.\n");
		    err = H5Aclose(lg_att_id[i]);
		    VRFY((err >= 0), "H5Aclose failed.\n");
		}
		/* Both step 3 and 4 use this same read checking code. */
		steps_done = (steps==3) ? 3 : 4;
	    }

	    /* End of Step 3 or 4: read large attributes from each dataset */
#endif


	    /*=====================================================*
	     * Step 5: read all objects from the file
	     *=====================================================*/
	    if (steps>=5){
		/* nothing extra to verify. The file is closed normally. */
		/* Just increment steps_done */
		steps_done++;
	    }

	    /* 
	     * Close the data sets
	     */

	    if(verbose )
		HDfprintf(stdout, "%0d:%s: closing datasets again.\n", 
			  mpi_rank, fcn_name);

	    for ( i = 0; i < NUM_DATA_SETS; i++ ) {
		if ( dataset[i] >= 0 ) {
		    err = H5Dclose(dataset[i]);
		    VRFY((err >= 0), "H5Dclose(dataset[i])1 failed.\n");
		}
	    }

	    /*
	     * close the data transfer property list.
	     */

	    if(verbose )
		HDfprintf(stdout, "%0d:%s: closing dxpl .\n", mpi_rank, fcn_name);

	    err = H5Pclose(dxpl_id);
	    VRFY((err >= 0), "H5Pclose(dxpl_id) failed.\n");

	    /*
	     * Close the file 
	     */
	    if(verbose)
		HDfprintf(stdout, "%0d:%s: closing file again.\n", 
			  mpi_rank, fcn_name);
	    err = H5Fclose(file_id);
	    VRFY((err >= 0 ), "H5Fclose(1) failed");

	} /* else if (steps_done==0) */
	Reader_result(mrc, steps_done);
    } /* end while(1) */

    if(verbose )
        HDfprintf(stdout, "%0d:%s: Done.\n", mpi_rank, fcn_name);

    return;
} /* rr_obj_hdr_flush_confusion_reader() */

#undef NUM_DATA_SETS
#undef LOCAL_DATA_SIZE
#undef LARGE_ATTR_SIZE
#undef Reader_check
#undef Reader_wait
#undef Reader_result
#undef Writer_Root
#undef Reader_Root

/*=============================================================================
 *                         End of t_mdset.c
 *===========================================================================*/

