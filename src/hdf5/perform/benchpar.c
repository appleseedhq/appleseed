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

#include <unistd.h>
#include <stdlib.h>
#include <assert.h>
#include <math.h>
#include <float.h>
#include <string.h>

#include "hdf5.h"

/* Local macros */

/*
 * HDF Boolean type.
 */
#ifndef FALSE
#   define FALSE 0
#endif
#ifndef TRUE
#   define TRUE 1
#endif

/* defines for type of VFL driver to use */
#define FACC_DEFAULT    0
#define FACC_MPIO       1
#define FACC_MPIPOSIX   2

/* Defines for computing performance information */
#define ONE_KB              1024
#define ONE_MB              (ONE_KB * ONE_KB)
#define ONE_GB              (ONE_MB * ONE_KB)

/* report 0.0 in case t is zero too */
#define MB_PER_SEC(bytes,t) ((fabs(t)<0.0000000001) ? 0.0 : ((((double)bytes) / ONE_MB) / (t)))

/* Control default behavior (with no command line arguments) */
#define DEFAULT_RANK    3
#define DEFAULT_DIM     1024
#define DEFAULT_PREFIX      "/tmp"
#define DEFAULT_USERNAME    "koziol"
#define DEFAULT_FILENAME    "benchpar.h5"
#define DEFAULT_SLICE   0
#define DEFAULT_C_TYPE      int
#define DEFAULT_HDF5_DATATYPE   H5T_NATIVE_INT  /* Keep this in sync with the DEFAULT_C_TYPE */
#define DEFAULT_DATASET_NAME    "Dataset"
#define DEFAULT_VFL_DRIVER      FACC_MPIO
#define DEFAULT_PAR_MODE        H5FD_MPIO_COLLECTIVE
#define DEFAULT_CHUNK_STORAGE   0
#define DEFAULT_ITER    3

/* MPI info */
int mpi_rank, mpi_size;
int mpi_namelen;
char mpi_name[MPI_MAX_PROCESSOR_NAME];

/* Usage information */
static void usage(void)
{
    printf("usage: benchpar [-d <# of dims>] [-s <dim_size>] [-f <file name>] [-h]\n");
    printf("            [-S <slice dim>] [-p] [-I] [-c] [-i <# of iterations>\n");
    printf("    -c              - Use chunked storage for dataset with 1-1 exact\n");
    printf("                      mapping of chunks to hyperslabs\n");
    printf("                      Default: off (i.e. contiguous storage)\n");
    printf("    -d <# of dims>  - Number of dimensions of the dataset\n");
    printf("                      Default: 3\n");
    printf("    -f <file name>  - Set the name of the test file\n");
    printf("                      Default: /tmp/<login>/benchpar.h5\n");
    printf("    -h              - Prints usage information\n");
    printf("    -i <# of iters> - Set the number of test iterations to perform\n");
    printf("                      Default: 3\n");
    printf("    -I              - Use independent parallel I/O\n");
    printf("                      Default: use collective parallel I/O\n");
    printf("    -p              - Use MPI-posix VFL driver\n");
    printf("                      Default: use MPI-I/O VFL driver\n");
    printf("    -s <dim_size>   - Set the size of each of the dataset's dimensions\n");
    printf("                      Default: 1024\n");
    printf("    -S <slice dim>  - Set the dimension to slice the dataset along\n");
    printf("                      Default: 0\n");
} /* end usage() */

/* Create & initialize file creation property list with appropriate properties */
static hid_t create_fcpl(void)
{
    hid_t fcpl;         /* File creation property list */

    fcpl=H5Pcreate(H5P_FILE_CREATE);
    assert(fcpl>0);

    return(fcpl);
} /* end create_fcpl() */

/* Create & initialize file access property list with appropriate properties */
static hid_t create_fapl(MPI_Comm comm, MPI_Info info, int acc_type )
{
    hid_t fapl;                 /* File access property list    */
    hbool_t use_gpfs = FALSE;   /* Use GPFS hints               */
    herr_t ret;                 /* Generic return value         */

    fapl = H5Pcreate (H5P_FILE_ACCESS);
    assert(fapl>0);

    /* set parallel access with communicator, using MPI-I/O driver */
    if (acc_type == FACC_MPIO) {
	ret = H5Pset_fapl_mpio(fapl, comm, info);
        assert(ret>=0);
    } /* end if */

    /* set parallel access with communicator, using MPI-posix driver */
    if (acc_type == FACC_MPIPOSIX) {
	ret = H5Pset_fapl_mpiposix(fapl, comm, use_gpfs);
        assert(ret>=0);
    } /* end if */

    return (fapl);
} /* end create_fapl() */

/* Create & initialize dataset creation property list with appropriate properties */
static hid_t create_dcpl(unsigned use_chunks, int rank, hsize_t *dims)
{
    hid_t dcpl;         /* Dataset creation property list */
    herr_t ret;         /* Generic return value */

    dcpl=H5Pcreate(H5P_DATASET_CREATE);
    assert(dcpl>0);

    /* Check if the dataset should be chunked */
    if(use_chunks) {
        ret = H5Pset_chunk(dcpl, rank, dims);
        assert(ret>=0);
    } /* end if */

    return(dcpl);
} /* end create_dcpl() */

/* Create & initialize dataset transfer property list with appropriate properties */
static hid_t create_dxpl(H5FD_mpio_xfer_t par_mode)
{
    hid_t dxpl;         /* Dataset creation property list */
    herr_t ret;         /* Generic return value */

    dxpl=H5Pcreate(H5P_DATASET_XFER);
    assert(dxpl>0);

    /* Set collective I/O on this transfer */
    ret=H5Pset_dxpl_mpio(dxpl, par_mode);
    assert(ret>=0);

    return(dxpl);
} /* end create_dcpl() */

int main(int argc, char *argv[])
{
    int curr_arg;       /* Current command line argument being processed */
    int rank;           /* Number of dimensions of the dataset */
    hsize_t dim_size;   /* Dimension size of each dimension */
    hsize_t dims[H5S_MAX_RANK];      /* Pointer to array of dimensions */
    hsize_t start[H5S_MAX_RANK];     /* Pointer to array of starting locations for hyperslab selection */
    hsize_t count[H5S_MAX_RANK];     /* Pointer to array of counts for hyperslab selection */
    unsigned slice_dim; /* Dimension to slice up */
    char *file_name=NULL;    /* Name of file to put data into */
    hid_t fcpl;         /* HDF5 File creation property list ID */
    hid_t fapl;         /* HDF5 File access property list ID */
    hid_t dcpl;         /* HDF5 Dataset creation property list ID */
    hid_t dxpl;         /* HDF5 Dataset transfer property list ID */
    hid_t fid;          /* HDF5 file ID */
    hid_t dsid;         /* HDF5 dataset ID */
    hid_t file_sid;     /* HDF5 dataspace ID for dataset on disk */
    hid_t mem_sid;      /* HDF5 dataspace ID for dataset in memory */
    DEFAULT_C_TYPE *buf;        /* Buffer to write out */
    hsize_t buf_size;   /* Size of buffer to write */
    int i;              /* Local index variable */
    herr_t ret;         /* Generic return value */
    double start_write_time, end_write_time, elap_write_time;   /* Start, end and elapsed time to write raw data */
    double tmp_max_write_time;      /* Temporary holders for maximum time for all nodes to perform raw data I/O */
    double max_write_time=-DBL_MAX, min_write_time=DBL_MAX;      /* Minimum & maximum time for all nodes to perform raw data I/O */
    double start_file_time, end_file_time, elap_file_time;   /* Start, end and elapsed time from file open to file close */
    double tmp_max_file_time;        /* Temporary holders for maximum time for all nodes from file open to file close */
    double max_file_time=-DBL_MAX, min_file_time=DBL_MAX;        /* Minimum & maximum time for all nodes from file open to file close */
    int vfl_type;       /* Type of VFL driver to use */
    H5FD_mpio_xfer_t par_mode;  /* Type of parallel I/O to perform */
    unsigned use_chunks;        /* Whether to use chunks for dataset or not */
    unsigned num_iter;  /* Number of iterations to perform */
    unsigned u;         /* Local index variable */

    /* Un-buffer the stdout and stderr */
    setbuf(stderr, NULL);
    setbuf(stdout, NULL);

    /* MPI initialization */
    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD,&mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);
    MPI_Get_processor_name(mpi_name,&mpi_namelen);

    /* Set some defaults */
    rank=DEFAULT_RANK;
    dim_size=DEFAULT_DIM;
    slice_dim=DEFAULT_SLICE;
    vfl_type=DEFAULT_VFL_DRIVER;
    par_mode=DEFAULT_PAR_MODE;
    use_chunks=DEFAULT_CHUNK_STORAGE;
    num_iter=DEFAULT_ITER;

    /* Parse command line arguments */
    if(argc>1) {
        curr_arg=1;
        while(curr_arg<argc) {
            /* Trap any unknown command line parameters */
            if(argv[curr_arg][0]!='-') {
                printf("unknown command line parameter: %s\n",argv[curr_arg]);
                goto done;
            } /* end if */

            /* Skip over command line flag */
            curr_arg++;

            switch(argv[curr_arg-1][1]) {
                case 'c':       /* Use chunked storage for dataset */
                    use_chunks=1;
                    break;

                case 'd':       /* Change number of dimensions */
                    /* Get new number of dimensions */
                    rank=atoi(argv[curr_arg]);
                    curr_arg++; /* Skip over number of dimensions */

                    /* Sanity check */
                    if(rank<=0) {
                        printf("rank=%d, invalid number of dimensions: %d\n",mpi_rank,rank);
                        goto done;
                    } /* end if */
                    break;

                case 'f':       /* Change test file name */
                    /* Get new file name */
                    file_name=strdup(argv[curr_arg]);
                    curr_arg++; /* Skip over file name from command line */
                    break;

                case 'h':       /* Print usage information */
                    usage();
                    goto done;
                    break;

                case 'i':       /* Change number of iterations */
                    /* Get new number of dimensions */
                    num_iter=atoi(argv[curr_arg]);
                    curr_arg++; /* Skip over number of iterations */

                    /* Sanity check */
                    if(num_iter<1) {
                        printf("rank=%d, invalid number of iterations: %u\n",mpi_rank,num_iter);
                        goto done;
                    } /* end if */
                    break;

                case 'I':       /* Use independent I/O for parallel I/O */
                    par_mode=H5FD_MPIO_INDEPENDENT;
                    break;

                case 'p':       /* Use MPI-posix VFL driver */
                    vfl_type=FACC_MPIPOSIX;
                    break;

                case 's':       /* Change dimension size */
                    /* Get new dimension size */
                    dim_size=atoi(argv[curr_arg]);
                    curr_arg++; /* Skip over dimension size from command line */

                    /* Sanity check */
                    if(dim_size<=0) {
                        printf("rank=%d, invalid dimension size: %ld\n",mpi_rank,(long)dim_size);
                        goto done;
                    } /* end if */
                    break;

                case 'S':       /* Change dimension to slice */
                    /* Get new dimension to slice */
                    slice_dim=atoi(argv[curr_arg]);
                    curr_arg++; /* Skip over slice dimension from command line */
                    break;

                default:
                    printf("rank=%d, unknown command line parameter: %s\n",mpi_rank,argv[curr_arg-1]);
                    goto done;
            } /* end switch */
        } /* end while */
    } /* end if */

    /* Sanity check */
    if(slice_dim>=rank) {
        printf("rank=%d, error, slice dim larger than rank: slice_dim=%d, rank=%d\n",mpi_rank,slice_dim,rank);
        goto done;
    } /* end if */

    /* Set rest of defaults */
    if(file_name==NULL) {
        char *login;    /* Pointer to login name */

        /* Get the login name for this user */
        login=getlogin();
        if(login==NULL)
            login=DEFAULT_USERNAME;

        /* Allocate enough room for the prefix, the login name, two '/'s, the filename and the string terminator */
        file_name=malloc(strlen(DEFAULT_PREFIX)+1+strlen(login)+1+strlen(DEFAULT_FILENAME)+1);
        strcpy(file_name,DEFAULT_PREFIX);
        strcat(file_name,"/");
        strcat(file_name,login);
        strcat(file_name,"/");
        strcat(file_name,DEFAULT_FILENAME);
    } /* end if */

    /* Allocate memory for this process's portion of dataset */
    buf_size=sizeof(DEFAULT_C_TYPE);
    for(i=0; i<rank; i++)
        buf_size *= dim_size;
    buf_size /= mpi_size;

    /* Sanity check for overflow */
    assert((hsize_t)((size_t)buf_size)==buf_size);

    buf=malloc((size_t)buf_size);
    assert(buf);

    /* Initialize dataset portion to something unique for each process */
    memset(buf,mpi_rank,(size_t)buf_size);

    for(u=0; u<num_iter; u++) {
        /* Create file creation property list */
        fcpl=create_fcpl();
        assert(fcpl>0);

        /* Create file access property list */
        fapl=create_fapl(MPI_COMM_WORLD,MPI_INFO_NULL,vfl_type);
        assert(fapl>0);

        /* Get file start time */
        start_file_time = MPI_Wtime();

        /* Create file */
        fid=H5Fcreate(file_name,H5F_ACC_TRUNC,fcpl,fapl);
        assert(fid>0);

        /* Close file creation property list */
        ret=H5Pclose(fcpl);
        assert(ret>=0);

        /* Close file access property list */
        ret=H5Pclose(fapl);
        assert(ret>=0);

        /* Create dataspace for dataset on disk */
        for(i=0; i<rank; i++)
            dims[i]=dim_size;

        file_sid=H5Screate_simple(rank,dims,NULL);
        assert(file_sid>0);

        /* Create dataspace for buffer in memory */
        for(i=0; i<rank; i++)
            dims[i]=dim_size;
        dims[slice_dim] /= mpi_size;

        mem_sid=H5Screate_simple(rank,dims,NULL);
        assert(mem_sid>0);

        /* Create dataset creation property list */
        dcpl=create_dcpl(use_chunks,rank,dims);
        assert(dcpl>0);

        /* Create dataset */
        dsid = H5Dcreate2(fid, DEFAULT_DATASET_NAME, DEFAULT_HDF5_DATATYPE, file_sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        assert(dsid > 0);

        /* Close dataset creation property list */
        ret=H5Pclose(dcpl);
        assert(ret>=0);

        /* Select hyperslab for file dataspace */
        for(i=0; i<rank; i++) {
            start[i]=0;
            count[i]=dim_size;
        } /* end for */
        start[slice_dim]=mpi_rank*(dim_size/mpi_size);
        count[slice_dim]=dim_size/mpi_size;

        ret = H5Sselect_hyperslab(file_sid,H5S_SELECT_SET,start,NULL,count,NULL);
        assert(ret>=0);

        /* Create dataset transfer property list */
        dxpl=create_dxpl(par_mode);
        assert(dxpl>0);

        /* Get raw data start time */
        start_write_time = MPI_Wtime();

        /* Write hyperslab to dataset */
        ret = H5Dwrite(dsid, DEFAULT_HDF5_DATATYPE, mem_sid,
                       file_sid, dxpl, buf);
        assert(ret>=0);

        /* Get stop time for raw data timer */
        end_write_time = MPI_Wtime();

        /* Close dataset transfer property list */
        ret=H5Pclose(dxpl);
        assert(ret>=0);

        /* Close memory dataspace */
        ret=H5Sclose(mem_sid);
        assert(ret>=0);

        /* Close file dataspace */
        ret=H5Sclose(file_sid);
        assert(ret>=0);

        /* Close dataset */
        ret=H5Dclose(dsid);
        assert(ret>=0);

        /* Close file */
        ret=H5Fclose(fid);
        assert(ret>=0);

        /* Get stop time for file timer */
        end_file_time = MPI_Wtime();

        /* Compute timing results */
        elap_write_time=end_write_time-start_write_time;
        elap_file_time=end_file_time-start_file_time;

        /* Collect the minimum and maximum times by MPI reduces */
        MPI_Allreduce(&elap_write_time, &tmp_max_write_time, 1, MPI_DOUBLE, MPI_MAX, MPI_COMM_WORLD);
        MPI_Allreduce(&elap_file_time, &tmp_max_file_time, 1, MPI_DOUBLE, MPI_MAX, MPI_COMM_WORLD);

        /* Track the fastest & slowest total runs */
        if(tmp_max_write_time>max_write_time)
            max_write_time=tmp_max_write_time;
        if(tmp_max_write_time<min_write_time)
            min_write_time=tmp_max_write_time;
        if(tmp_max_file_time>max_file_time)
            max_file_time=tmp_max_file_time;
        if(tmp_max_file_time<min_file_time)
            min_file_time=tmp_max_file_time;
    } /* end for */

    /* Only print information from one node */
    if(mpi_rank==0) {
        /* Print information about test */
        printf("File driver used: %s\n",vfl_type==FACC_MPIO ? "MPI-I/O" : "MPI-posix");
        printf("Type of parallel access: %s\n",par_mode==H5FD_MPIO_COLLECTIVE ? "Collective" : "Independent");
        printf("Type of dataset storage: %s\n",use_chunks ? "Chunked" : "Contiguous");
        printf("Number of processes: %d\n",mpi_size);
        printf("Element size: %u\n",(unsigned)sizeof(DEFAULT_C_TYPE));
        printf("# of dimensions: %d\n",rank);
        printf("Dimension size: %ld\n",(long)dim_size);
        printf("Dimension sliced: %u\n",slice_dim);
        printf("Number of elements: %lu\n",(unsigned long)((buf_size/sizeof(DEFAULT_C_TYPE))*mpi_size));
        printf("Total dataset size (bytes): %lu\n",(unsigned long)(buf_size*mpi_size));
        printf("Dataset size per process (bytes): %lu\n",(unsigned long)buf_size);

        /* Print timing results */
        printf("# of iterations: %u\n",num_iter);
        printf("Maximum raw data write throughput=%6.2f MB/s (%7.3f s)\n",MB_PER_SEC((buf_size*mpi_size),min_write_time),min_write_time);
        printf("Minimum raw data write throughput=%6.2f MB/s (%7.3f s)\n",MB_PER_SEC((buf_size*mpi_size),max_write_time),max_write_time);
        printf("Maximum file throughput=%6.2f MB/s (%7.3f s)\n",MB_PER_SEC((buf_size*mpi_size),min_file_time),min_file_time);
        printf("Minimum file throughput=%6.2f MB/s (%7.3f s)\n",MB_PER_SEC((buf_size*mpi_size),max_file_time),max_file_time);
    } /* end if */

done:
    /* Free buffers allocated */
    if(file_name)
        free(file_name);
    if(buf)
        free(buf);

    /* MPI termination */
    MPI_Finalize();
    return(0);
} /* end main() */
