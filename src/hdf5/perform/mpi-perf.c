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
 * (C) 1995-2001 Clemson University and Argonne National Laboratory.
 *
 * See COPYING in top-level directory.
 *
 * This is contributed by Robert Ross to the HDF5 software.
 * and was called mpi-io-test.c
 */

#include "hdf5.h"
#include "H5private.h"
#ifdef H5_HAVE_PARALLEL
/* mpi-perf.c
 *
 * This is derived from code given to me by Rajeev Thakur.  Dunno where
 * it originated.
 *
 * It's purpose is to produce aggregate bandwidth numbers for varying
 * block sizes, number of processors, an number of iterations.
 *
 * This is strictly an mpi program - it is used to test the MPI I/O
 * functionality implemented by Romio.
 *
 * Compiling is usually easiest with something like:
 * mpicc -Wall -Wstrict-prototypes mpi-io-test.c -o mpi-io-test
 *
 * NOTE: This code assumes that all command line arguments make it out to all
 * the processes that make up the parallel job, which isn't always the case.
 * So if it doesn't work on some platform, that might be why.
 */
/* Modifications:
 *    Albert Cheng, Apr 30, 20001
 *    Changed MPI_File_open to use MPI_COMM_WORLD (was MPI_COMM_SELF).
 *    Albert Cheng, May 5, 20001
 *    Changed MPI_File_seek then MPI_File_write or MPI_File_read to just
 *    MPI_File_write_at and MPI_File_read_at.  Some compiler, e.g., IBM
 *    mpcc_r does not support MPI_File_seek and MPI_File_read or MPI_File_write.
 */

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



/* DEFAULT VALUES FOR OPTIONS */
int64_t opt_block     = 1048576*16;
int     opt_iter      = 1;
int     opt_stripe    = -1;
int     opt_correct   = 0;
int     amode         = O_RDWR | O_CREAT;
char    opt_file[256] = "/tmp/test.out\0";
char    opt_pvfstab[256] = "notset\0";
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

    /* startup MPI and determine the rank of this process */
    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &mynod);

    /* parse the command line arguments */
    parse_args(argc, argv);

    if (mynod == 0) printf("# Using mpi-io calls.\n");


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

    /* open the file for writing */
    err = MPI_File_open(MPI_COMM_WORLD, opt_file,
    MPI_MODE_CREATE | MPI_MODE_RDWR, MPI_INFO_NULL, &fh);
    if (err < 0) {
            fprintf(stderr, "node %d, open error: %s\n", mynod, strerror(errno));
            goto die_jar_jar_die;
    }

    /* now repeat the write operations the number of times
     * specified on the command line */
    for (j=0; j < opt_iter; j++) {

            /* calculate the appropriate position depending on the iteration
             * and rank of the current process */
            seek_position = (j*iter_jump)+(mynod*opt_block);

            if (opt_correct) /* fill in buffer for iteration */ {
                    for (i=mynod+j, check=buf; i<opt_block; i++,check++) *check=(char)i;
            }

            /* discover the starting time of the operation */
       MPI_Barrier(MPI_COMM_WORLD);
       stim = MPI_Wtime();

            /* write out the data */
            nchars = opt_block/sizeof(char);
            err = MPI_File_write_at(fh, seek_position, buf, nchars, MPI_CHAR, &status);
            if(err){
                    fprintf(stderr, "node %d, write error: %s\n", mynod,
                    strerror(errno));
            }

            /* discover the ending time of the operation */
       etim = MPI_Wtime();

       write_tim += (etim - stim);

            /* we are done with this "write" iteration */
    }

    err = MPI_File_close(&fh);
    if(err){
            fprintf(stderr, "node %d, close error after write\n", mynod);
    }

    /* wait for everyone to synchronize at this point */
    MPI_Barrier(MPI_COMM_WORLD);

    /* reopen the file to read the data back out */
    err = MPI_File_open(MPI_COMM_WORLD, opt_file,
    MPI_MODE_CREATE | MPI_MODE_RDWR, MPI_INFO_NULL, &fh);
    if (err < 0) {
            fprintf(stderr, "node %d, open error: %s\n", mynod, strerror(errno));
            goto die_jar_jar_die;
    }


    /* we are going to repeat the read operation the number of iterations
     * specified */
    for (j=0; j < opt_iter; j++) {
            /* calculate the appropriate spot give the current iteration and
             * rank within the MPI processes */
            seek_position = (j*iter_jump)+(mynod*opt_block);

            /* discover the start time */
       MPI_Barrier(MPI_COMM_WORLD);
       stim = MPI_Wtime();

            /* read in the file data */
            if (!opt_correct){
                    err = MPI_File_read_at(fh, seek_position, buf, nchars, MPI_CHAR, &status);
            }
            else{
                    err = MPI_File_read_at(fh, seek_position, buf2, nchars, MPI_CHAR, &status);
            }
            myerrno = errno;

            /* discover the end time */
       etim = MPI_Wtime();
       read_tim += (etim - stim);

       if (err < 0) fprintf(stderr, "node %d, read error, loc = %Ld: %s\n",
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

    /* close the file */
    err = MPI_File_close(&fh);
    if(err){
            fprintf(stderr, "node %d, close error after write\n", mynod);
    }

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

    while ((c = getopt(argc, argv, "s:b:i:f:p:c")) != EOF) {
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

