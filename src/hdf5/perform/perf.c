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
 * Author: Albert Cheng of NCSA, May 1, 2001.
 * This is derived from code given to me by Robert Ross.
 *
 * NOTE: This code assumes that all command line arguments make it out to all
 * the processes that make up the parallel job, which isn't always the case.
 * So if it doesn't work on some platform, that might be why.
 */

#include "hdf5.h"
#include "H5private.h"
#ifdef H5_HAVE_PARALLEL
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <sys/time.h>
#include <mpi.h>
#ifndef MPI_FILE_NULL           /*MPIO may be defined in mpi.h already       */
#   include <mpio.h>
#endif


/* Macro definitions */
/* Verify:
 * if val is false (0), print mesg and if fatal is true (non-zero), die.
 */
#define H5FATAL 1
#define VRFY(val, mesg, fatal) do {                                            \
    if (!val) {                                                                \
	printf("Proc %d: ", mynod);					       \
        printf("*** Assertion failed (%s) at line %4d in %s\n",                \
	    mesg, (int)__LINE__, __FILE__);     			       \
	if (fatal){							       \
	    fflush(stdout);						       \
	    goto die_jar_jar_die;					       \
	}								       \
    }                                                                          \
} while(0)
#define RANK 1
hsize_t dims[RANK];   	/* dataset dim sizes */
hsize_t block[RANK], stride[RANK], count[RANK];
hssize_t start[RANK];
hid_t fid;                  /* HDF5 file ID */
hid_t acc_tpl;		/* File access templates */
hid_t sid;   		/* Dataspace ID */
hid_t file_dataspace;	/* File dataspace ID */
hid_t mem_dataspace;	/* memory dataspace ID */
hid_t dataset;		/* Dataset ID */
hsize_t opt_alignment	= 1;
hsize_t opt_threshold	= 1;
int	opt_split_vfd	= 0;
char	*meta_ext, *raw_ext;	/* holds the meta and raw file extension if */
				/* opt_split_vfd is set */


/* DEFAULT VALUES FOR OPTIONS */
int64_t opt_block     = 1048576*16;
int     opt_iter      = 1;
int     opt_stripe    = -1;
int     opt_correct   = 0;
int     amode         = O_RDWR | O_CREAT;
char    opt_file[256] = "perftest.out";
char    opt_pvfstab[256] = "notset";
int     opt_pvfstab_set = 0;

/* function prototypes */
static int parse_args(int argc, char **argv);

extern int errno;

/* globals needed for getopt */
extern char *optarg;

int main(int argc, char **argv)
{
    char *buf, *tmp, *buf2, *tmp2, *check;
    int i, j, mynod=0, nprocs=1, err, my_correct = 1, correct, myerrno;
    double stim, etim;
    double write_tim = 0;
    double read_tim = 0;
    double read_bw, write_bw;
    double max_read_tim, max_write_tim;
    double min_read_tim, min_write_tim;
    double ave_read_tim, ave_write_tim;
    int64_t iter_jump = 0;
    int64_t seek_position = 0;
    MPI_File fh;
    MPI_Status status;
    int nchars;
    herr_t ret;         	/* Generic return value */

    /* startup MPI and determine the rank of this process */
    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &mynod);

    /* parse the command line arguments */
    parse_args(argc, argv);

    if (mynod == 0) printf("# Using hdf5-io calls.\n");


    /* kindof a weird hack- if the location of the pvfstab file was
     * specified on the command line, then spit out this location into
     * the appropriate environment variable: */

#if H5_HAVE_SETENV
/* no setenv or unsetenv */
    if (opt_pvfstab_set) {
            if((setenv("PVFSTAB_FILE", opt_pvfstab, 1)) < 0){
                    perror("setenv");
                    goto die_jar_jar_die;
            }
    }
#endif

    /* this is how much of the file data is covered on each iteration of
     * the test.  used to help determine the seek offset on each
     * iteration */
    iter_jump = nprocs * opt_block;

    /* setup a buffer of data to write */
    if (!(tmp = (char *) malloc(opt_block + 256))) {
            perror("malloc");
            goto die_jar_jar_die;
    }
    buf = tmp + 128 - (((long)tmp) % 128);  /* align buffer */

    if (opt_correct) {
            /* do the same buffer setup for verifiable data */
            if (!(tmp2 = (char *) malloc(opt_block + 256))) {
                    perror("malloc2");
                    goto die_jar_jar_die;
             }
            buf2 = tmp + 128 - (((long)tmp) % 128);
    }

    /* setup file access template with parallel IO access. */
    if (opt_split_vfd){
	hid_t mpio_pl;

	mpio_pl = H5Pcreate (H5P_FILE_ACCESS);
	VRFY((acc_tpl >= 0), "", H5FATAL);
	ret = H5Pset_fapl_mpio(mpio_pl, MPI_COMM_WORLD, MPI_INFO_NULL);
	VRFY((ret >= 0), "", H5FATAL);

	/* set optional allocation alignment */
	if (opt_alignment*opt_threshold != 1){
	    ret = H5Pset_alignment(acc_tpl, opt_threshold, opt_alignment );
	    VRFY((ret >= 0), "H5Pset_alignment succeeded", !H5FATAL);
	}

	/* setup file access template */
	acc_tpl = H5Pcreate (H5P_FILE_ACCESS);
	VRFY((acc_tpl >= 0), "", H5FATAL);
	ret = H5Pset_fapl_split(acc_tpl, meta_ext, mpio_pl, raw_ext, mpio_pl);
	VRFY((ret >= 0), "H5Pset_fapl_split succeeded", H5FATAL);
	ret = H5Pclose(mpio_pl);
	VRFY((ret >= 0), "H5Pclose mpio_pl succeeded", H5FATAL);
    }else{
	/* setup file access template */
	acc_tpl = H5Pcreate (H5P_FILE_ACCESS);
	VRFY((acc_tpl >= 0), "", H5FATAL);
	ret = H5Pset_fapl_mpio(acc_tpl, MPI_COMM_WORLD, MPI_INFO_NULL);
	VRFY((ret >= 0), "", H5FATAL);

	/* set optional allocation alignment */
	if (opt_alignment*opt_threshold != 1){
	    ret = H5Pset_alignment(acc_tpl, opt_threshold, opt_alignment );
	    VRFY((ret >= 0), "H5Pset_alignment succeeded", !H5FATAL);
	}
    }

    /* create the parallel file */
    fid = H5Fcreate(opt_file, H5F_ACC_TRUNC, H5P_DEFAULT, acc_tpl);
    VRFY((fid >= 0), "H5Fcreate succeeded", H5FATAL);

    /* define a contiquous dataset of opt_iter*nprocs*opt_block chars */
    dims[0] = opt_iter * nprocs * opt_block;
    sid = H5Screate_simple(RANK, dims, NULL);
    VRFY((sid >= 0), "H5Screate_simple succeeded", H5FATAL);
    dataset = H5Dcreate2(fid, "Dataset1", H5T_NATIVE_CHAR, sid,
			H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((dataset >= 0), "H5Dcreate2 succeeded", H5FATAL);

    /* create the memory dataspace and the file dataspace */
    dims[0] = opt_block;
    mem_dataspace = H5Screate_simple(RANK, dims, NULL);
    VRFY((mem_dataspace >= 0), "", H5FATAL);
    file_dataspace = H5Dget_space(dataset);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded", H5FATAL);

    /* now each process writes a block of opt_block chars in round robbin
     * fashion until the whole dataset is covered.
     */
    for(j=0; j < opt_iter; j++) {
        /* setup a file dataspace selection */
        start[0] = (j*iter_jump)+(mynod*opt_block);
        stride[0] = block[0] = opt_block;
        count[0]= 1;
        ret=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
        VRFY((ret >= 0), "H5Sset_hyperslab succeeded", H5FATAL);

            if (opt_correct) /* fill in buffer for iteration */ {
                    for (i=mynod+j, check=buf; i<opt_block; i++,check++) *check=(char)i;
            }

            /* discover the starting time of the operation */
       MPI_Barrier(MPI_COMM_WORLD);
       stim = MPI_Wtime();

        /* write data */
        ret = H5Dwrite(dataset, H5T_NATIVE_CHAR, mem_dataspace, file_dataspace,
                H5P_DEFAULT, buf);
        VRFY((ret >= 0), "H5Dwrite dataset1 succeeded", !H5FATAL);

            /* discover the ending time of the operation */
       etim = MPI_Wtime();

       write_tim += (etim - stim);

            /* we are done with this "write" iteration */
    }

    /* close dataset and file */
    ret=H5Dclose(dataset);
    VRFY((ret >= 0), "H5Dclose succeeded", H5FATAL);
    ret=H5Fclose(fid);
    VRFY((ret >= 0), "H5Fclose succeeded", H5FATAL);



    /* wait for everyone to synchronize at this point */
    MPI_Barrier(MPI_COMM_WORLD);

    /* reopen the file for reading */
    fid=H5Fopen(opt_file,H5F_ACC_RDONLY,acc_tpl);
    VRFY((fid >= 0), "", H5FATAL);

    /* open the dataset */
    dataset = H5Dopen2(fid, "Dataset1", H5P_DEFAULT);
    VRFY((dataset >= 0), "H5Dopen succeeded", H5FATAL);

    /* we can re-use the same mem_dataspace and file_dataspace
     * the H5Dwrite used since the dimension size is the same.
     */

    /* we are going to repeat the read the same pattern the write used */
    for (j=0; j < opt_iter; j++) {
        /* setup a file dataspace selection */
        start[0] = (j*iter_jump)+(mynod*opt_block);
        stride[0] = block[0] = opt_block;
        count[0]= 1;
        ret=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
        VRFY((ret >= 0), "H5Sset_hyperslab succeeded", H5FATAL);
            /* seek to the appropriate spot give the current iteration and
             * rank within the MPI processes */

            /* discover the start time */
       MPI_Barrier(MPI_COMM_WORLD);
       stim = MPI_Wtime();

        /* read in the file data */
        if (!opt_correct){
            ret = H5Dread(dataset, H5T_NATIVE_CHAR, mem_dataspace, file_dataspace, H5P_DEFAULT, buf);
        }
        else{
            ret = H5Dread(dataset, H5T_NATIVE_CHAR, mem_dataspace, file_dataspace, H5P_DEFAULT, buf2);
        }
        myerrno = errno;

        /* discover the end time */
       etim = MPI_Wtime();
       read_tim += (etim - stim);
        VRFY((ret >= 0), "H5Dwrite dataset1 succeeded", !H5FATAL);


       if (ret < 0) fprintf(stderr, "node %d, read error, loc = %Ld: %s\n",
                    mynod, mynod*opt_block, strerror(myerrno));

        /* if the user wanted to check correctness, compare the write
         * buffer to the read buffer */
        if (opt_correct && memcmp(buf, buf2, opt_block)) {
                fprintf(stderr, "node %d, correctness test failed\n", mynod);
                my_correct = 0;
                MPI_Allreduce(&my_correct, &correct, 1, MPI_INT, MPI_MIN,
                        MPI_COMM_WORLD);
        }

        /* we are done with this read iteration */
    }

    /* close dataset and file */
    ret=H5Dclose(dataset);
    VRFY((ret >= 0), "H5Dclose succeeded", H5FATAL);
    ret=H5Fclose(fid);
    VRFY((ret >= 0), "H5Fclose succeeded", H5FATAL);
    ret=H5Pclose(acc_tpl);
    VRFY((ret >= 0), "H5Pclose succeeded", H5FATAL);

    /* compute the read and write times */
    MPI_Allreduce(&read_tim, &max_read_tim, 1, MPI_DOUBLE, MPI_MAX,
            MPI_COMM_WORLD);
    MPI_Allreduce(&read_tim, &min_read_tim, 1, MPI_DOUBLE, MPI_MIN,
            MPI_COMM_WORLD);
    MPI_Allreduce(&read_tim, &ave_read_tim, 1, MPI_DOUBLE, MPI_SUM,
            MPI_COMM_WORLD);

    /* calculate the average from the sum */
    ave_read_tim = ave_read_tim / nprocs;

    MPI_Allreduce(&write_tim, &max_write_tim, 1, MPI_DOUBLE, MPI_MAX,
            MPI_COMM_WORLD);
    MPI_Allreduce(&write_tim, &min_write_tim, 1, MPI_DOUBLE, MPI_MIN,
            MPI_COMM_WORLD);
    MPI_Allreduce(&write_tim, &ave_write_tim, 1, MPI_DOUBLE, MPI_SUM,
            MPI_COMM_WORLD);

    /* calculate the average from the sum */
    ave_write_tim = ave_write_tim / nprocs;

    /* print out the results on one node */
    if (mynod == 0) {
       read_bw = ((int64_t)(opt_block*nprocs*opt_iter))/(max_read_tim*1000000.0);
       write_bw = ((int64_t)(opt_block*nprocs*opt_iter))/(max_write_tim*1000000.0);

                    printf("nr_procs = %d, nr_iter = %d, blk_sz = %ld\n", nprocs,
            opt_iter, (long)opt_block);

                    printf("# total_size = %ld\n", (long)(opt_block*nprocs*opt_iter));

                    printf("# Write:  min_time = %f, max_time = %f, mean_time = %f\n",
                            min_write_tim, max_write_tim, ave_write_tim);
                    printf("# Read:  min_time = %f, max_time = %f, mean_time = %f\n",
                            min_read_tim, max_read_tim, ave_read_tim);

       printf("Write bandwidth = %f Mbytes/sec\n", write_bw);
       printf("Read bandwidth = %f Mbytes/sec\n", read_bw);

            if (opt_correct) {
                    printf("Correctness test %s.\n", correct ? "passed" : "failed");
            }
    }


die_jar_jar_die:

#if H5_HAVE_SETENV
/* no setenv or unsetenv */
    /* clear the environment variable if it was set earlier */
    if	(opt_pvfstab_set){
            unsetenv("PVFSTAB_FILE");
    }
#endif

    free(tmp);
    if (opt_correct) free(tmp2);
    MPI_Finalize();

    return(0);
}

static int
parse_args(int argc, char **argv)
{
    int c;

    while ((c = getopt(argc, argv, "s:b:i:f:p:a:2:c")) != EOF) {
        switch (c) {
            case 's': /* stripe */
                opt_stripe = atoi(optarg);
                break;
            case 'b': /* block size */
                opt_block = atoi(optarg);
                break;
            case 'i': /* iterations */
                opt_iter = atoi(optarg);
                break;
            case 'f': /* filename */
                strncpy(opt_file, optarg, 255);
                break;
            case 'p': /* pvfstab file */
                strncpy(opt_pvfstab, optarg, 255);
                opt_pvfstab_set = 1;
                break;
            case 'a': /* aligned allocation.
                       * syntax: -a<alignment>/<threshold>
                       * e.g., -a4096/512  allocate at 4096 bytes
                       * boundary if request size >= 512.
                       */
                {char *p;
                opt_alignment = atoi(optarg);
                if (p=(char*)strchr(optarg, '/'))
                    opt_threshold = atoi(p+1);
                }
                HDfprintf(stdout,
                    "alignment/threshold=%Hu/%Hu\n",
                     opt_alignment, opt_threshold);
                break;
            case '2': /* use 2-files, i.e., split file driver */
                opt_split_vfd=1;
                /* get meta and raw file extension. */
                /* syntax is <raw_ext>,<meta_ext> */
                meta_ext = raw_ext = optarg;
                while (*raw_ext != '\0'){
                    if (*raw_ext == ','){
                        *raw_ext = '\0';
                        raw_ext++;
                        break;
                    }
                    raw_ext++;
                }
                printf("split-file-vfd used: %s,%s\n",
                    meta_ext, raw_ext);
                break;
            case 'c': /* correctness */
                opt_correct = 1;
                break;
            case '?': /* unknown */
            default:
                break;
        }
    }

    return(0);
}

/*
 * Local variables:
 *  c-indent-level: 3
 *  c-basic-offset: 3
 *  tab-width: 3
 * End:
 */

#else /* H5_HAVE_PARALLEL */
/* dummy program since H5_HAVE_PARALLEL is not configured in */
int
main(int UNUSED argc, char UNUSED **argv)
{
    printf("No parallel performance because parallel is not configured in\n");
    return(0);
}
#endif /* H5_HAVE_PARALLEL */

