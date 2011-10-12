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

/* A series of tests for posix compliance
 *
 * These tests do increasingly complicated sets of writes followed by reads.
 * POSIX standards say that any read that can be proven to occur after a write
 * must include the data in that write.  These tests attempt to verify whether the
 * underlying filesystem and i/o layer provide such guarantees.
 *
 * There are two sets of tests, one which uses POSIX i/o (fread, fwrite) and one which
 * uses MPI I/O (MPI_File_read, MPI_File_write).  Each set has multiple sub-tests, which
 * test varying patters of writes and reads.
 *
 *
 * TODO:
 * 	Add corresponding posix i/o tests for each MPI i/o test.  Currently, not all of the
 * 	MPI IO tests are implemented using fwrite/fread.
 *
 * Leon Arber
 * larber@ncsa.uiuc.edu
*/

/* To compile this outside of the HDF5 library, you can do so by defining the
 * macro STANDALONE. E.g.,
 *     mpicc -DSTANDALONE t_posix_compliant.c -o t_posix_compliant
 * then run it as an MPI application.  E.g.,
 *     mpiexec -np 3 ./t_posix_compliant
 */

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <mpi.h>
#ifndef STANDALONE
#include "h5test.h"
#else
#define HDmalloc(sz)	malloc(sz)
#define HDfree(p)	free(p)
#define getenv_all(comm, root, name)	getenv(name)
#endif
#define  TESTFNAME	"posix_test"	/* test file name */

static char*		testfile = NULL;
static int		err_flag = 0;
static int		max_err_print = 5;
int			nmismatches = 0;	/* warnings encountered */

/* globals needed for getopt
 * Although they *should* be defined in unistd.h */
extern char *optarg;
extern int optind, opterr;


#define CHECK_SUCCESS(res)	\
{				\
    char        err_got[MPI_MAX_ERROR_STRING];	\
    int		err_len;			\
    if(res != MPI_SUCCESS)			\
    {						\
	MPI_Error_string(res, err_got, &err_len);	\
	fprintf(stderr, "Line %d, Error: %s\n", __LINE__, err_got); \
	MPI_Abort(MPI_COMM_WORLD, -2);		\
    }						\
}

#define PRINT_RESULT()								\
{										\
    int err_result;								\
    MPI_Reduce(&err_flag, &err_result, 1, MPI_INT, MPI_MIN, 0, MPI_COMM_WORLD);	\
    if( (rank == 0) && (err_result == 0) )					\
	printf("PASSED\n");							\
    fflush(stdout);								\
    fflush(stderr);								\
    err_flag = 0;								\
}

static void vrfy_elements(int* a, int* b, int size, int rank);
static int find_writesize(int rank, int numprocs, int write_size);


/* All writes are to non-overlapping locations in the file
 * Then, each task reads another tasks' data
 * */

static int allwrite_allread_blocks(int numprocs, int rank, int write_size)
{
    MPI_File	fh = MPI_FILE_NULL;
    int		mpio_result;
    int		amode, i;
    MPI_Offset	offset = rank*write_size*sizeof(int);
    MPI_Status	Status;
    int* writebuf = (int*)malloc(write_size*sizeof(int));
    int* readbuf = (int*)malloc (write_size*sizeof(int));

    for(i=0; i<write_size; i++)
	writebuf[i] = i;

    amode = MPI_MODE_CREATE | MPI_MODE_RDWR | MPI_MODE_DELETE_ON_CLOSE;

    mpio_result = MPI_File_open(MPI_COMM_WORLD, testfile, amode,
	    MPI_INFO_NULL, &fh);
    CHECK_SUCCESS(mpio_result);

    mpio_result = MPI_File_write_at(fh, offset, writebuf, write_size, MPI_INT, &Status);
    CHECK_SUCCESS(mpio_result);

    MPI_Barrier(MPI_COMM_WORLD);

    offset = ( (rank+(numprocs-1)) % numprocs)*write_size*sizeof(int);

    mpio_result = MPI_File_read_at(fh, offset, readbuf, write_size, MPI_INT, &Status);
    CHECK_SUCCESS(mpio_result);

    vrfy_elements(writebuf, readbuf, write_size, rank);

    mpio_result = MPI_File_close(&fh);
    CHECK_SUCCESS(mpio_result);
    HDfree(writebuf);
    HDfree(readbuf);

    return err_flag;

}

static int posix_allwrite_allread_blocks(int numprocs, int rank, int write_size)
{
    int		ret;
    int		i;
    int	offset = rank*write_size*sizeof(int);
    int* writebuf = (int*)malloc(write_size*sizeof(int));
    int* readbuf = (int*)malloc (write_size*sizeof(int));
    FILE*	file = NULL;

    for(i=0; i<write_size; i++)
	writebuf[i] = i;

    if(rank==0)
	file = fopen(testfile, "w+");
    MPI_Barrier(MPI_COMM_WORLD);
    if(rank != 0)
	file = fopen(testfile, "r+");

    if(file == NULL)
    {
	fprintf(stderr, "Could not create testfile\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    ret = fseek(file, offset, SEEK_SET);
    if(ret == -1)
    {
	perror("fseek");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }


    ret = fwrite(writebuf, sizeof(int), write_size, file);
    if(ret != write_size)
    {
	perror("fwrite");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    MPI_Barrier(MPI_COMM_WORLD);

    offset = ( (rank+(numprocs-1)) % numprocs)*write_size*sizeof(int);

    ret = fseek(file, offset, SEEK_SET);
    if(ret == -1)
    {
	perror("fseek");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    ret = fread(readbuf, sizeof(int), write_size, file);
    if( (ret == 0) && feof(file))
	printf("Process %d: Error.  Prematurely reached end of file\n", rank);
    else if( (ret != write_size) && ferror(file))
    {
	perror("Error encountered in fread");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    vrfy_elements(writebuf, readbuf, write_size, rank);

    fclose(file);

    MPI_Barrier(MPI_COMM_WORLD);
    if(rank == 0)
	unlink(testfile);

    HDfree(writebuf);
    HDfree(readbuf);
    return err_flag;

}

static int posix_onewrite_allread_blocks(int numprocs, int rank, int write_size)
{
    int		ret;
    int		i;
    int	offset = rank*write_size*sizeof(int);
    int* writebuf = (int*)malloc(write_size*sizeof(int));
    int* readbuf = (int*)malloc (write_size*sizeof(int));
    FILE*	file = NULL;

    for(i=0; i<write_size; i++)
	writebuf[i] = i;

    if(rank==0)
	file = fopen(testfile, "w+");
    MPI_Barrier(MPI_COMM_WORLD);
    if(rank != 0)
	file = fopen(testfile, "r+");

    if(file == NULL)
    {
	fprintf(stderr, "Could not create testfile\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    if(rank == 0)
    {
	for(offset = 0; offset<numprocs*write_size*sizeof(int); offset+=(write_size*sizeof(int)))
	{
	    ret = fseek(file, offset, SEEK_SET);
	    if(ret == -1)
	    {
		perror("fseek");
		MPI_Abort(MPI_COMM_WORLD, 1);
	    }


	    ret = fwrite(writebuf, sizeof(int), write_size, file);
	    if(ret != write_size)
	    {
		perror("fwrite");
		MPI_Abort(MPI_COMM_WORLD, 1);
	    }
	}

    }
    MPI_Barrier(MPI_COMM_WORLD);

    offset = rank*write_size*sizeof(int);

    ret = fseek(file, offset, SEEK_SET);
    if(ret == -1)
    {
	perror("fseek");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    ret = fread(readbuf, sizeof(int), write_size, file);
    if( (ret == 0) && feof(file))
	printf("Process %d: Error.  Prematurely reached end of file\n", rank);
    else if( (ret != write_size) && ferror(file))
    {
	perror("Error encountered in fread");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    vrfy_elements(writebuf, readbuf, write_size, rank);

    fclose(file);

    MPI_Barrier(MPI_COMM_WORLD);
    if(rank == 0)
	unlink(testfile);

    HDfree(writebuf);
    HDfree(readbuf);
    return err_flag;

}

static int posix_onewrite_allread_interlaced(int numprocs, int rank, int write_size)
{
    int		ret;
    int		i, fill, index;
    int	offset = rank*write_size*sizeof(int);
    int* writebuf = (int*)malloc(write_size*sizeof(int));
    int* readbuf = (int*)malloc (write_size*sizeof(int));
    FILE*	file = NULL;

    if(rank==0)
	file = fopen(testfile, "w+");
    MPI_Barrier(MPI_COMM_WORLD);
    if(rank != 0)
	file = fopen(testfile, "r+");

    if(file == NULL)
    {
	fprintf(stderr, "Could not create testfile\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    if(rank == 0)
    {
	for(offset = 0; offset<numprocs*write_size*sizeof(int); offset+=(numprocs*sizeof(int)))
	{
	    ret = fseek(file, offset, SEEK_SET);
	    if(ret == -1)
	    {
		perror("fseek");
		MPI_Abort(MPI_COMM_WORLD, 1);
	    }

	    fill = offset / (numprocs*sizeof(int));
	    for(i=0; i<numprocs; i++)
		writebuf[i] = fill;


	    ret = fwrite(writebuf, sizeof(int), numprocs, file);
	    if(ret != numprocs)
	    {
		perror("fwrite");
		MPI_Abort(MPI_COMM_WORLD, 1);
	    }
	}

    }
    MPI_Barrier(MPI_COMM_WORLD);

    index = 0;
    for(offset = rank*sizeof(int); offset<numprocs*write_size*sizeof(int); offset+=(numprocs*sizeof(int)))
    {

	ret = fseek(file, offset, SEEK_SET);
	if(ret == -1)
	{
	    perror("fseek");
	    MPI_Abort(MPI_COMM_WORLD, 1);
	}

	ret = fread(readbuf+index, sizeof(int), 1, file);
	if( (ret == 0) && feof(file))
	    printf("Process %d: Error.  Prematurely reached end of file\n", rank);
	else if( (ret != 1) && ferror(file))
	{
	    perror("Error encountered in fread");
	    MPI_Abort(MPI_COMM_WORLD, 1);
	}

	index++;
    }

    for(i=0; i<write_size; i++)
	writebuf[i] = i;

    vrfy_elements(writebuf, readbuf, write_size, rank);

    fclose(file);

    MPI_Barrier(MPI_COMM_WORLD);
    if(rank == 0)
	unlink(testfile);

    HDfree(writebuf);
    HDfree(readbuf);
    return err_flag;

}
/* Each proc wites out 0 1 2 3 with displacement i, so file contents are:
 * 0000 1111 2222 3333 etc. (with 4 procs)
 *
 * Each proc then reads in the whole file and verifies that the data is what it is supposed to be*/

static int allwrite_allread_interlaced(int numprocs, int rank, int write_size)
{
    MPI_File	fh = MPI_FILE_NULL;
    int		mpio_result;
    int		amode, i, counter = 0;
    MPI_Datatype filetype;
    MPI_Status	Status;
    int* writebuf = (int*)malloc(write_size*sizeof(int));
    int*	readbuf = (int*) malloc(numprocs*sizeof(int));
    int		offset=0;

    for(i=0; i<write_size; i++)
	writebuf[i] = i;


    amode = MPI_MODE_CREATE | MPI_MODE_RDWR | MPI_MODE_DELETE_ON_CLOSE;
    mpio_result = MPI_File_open(MPI_COMM_WORLD, testfile, amode, MPI_INFO_NULL, &fh);
    CHECK_SUCCESS(mpio_result);

    mpio_result = MPI_Type_vector(write_size, 1, numprocs, MPI_INT, &filetype);
    CHECK_SUCCESS(mpio_result);

    mpio_result = MPI_Type_commit(&filetype);
    CHECK_SUCCESS(mpio_result);

    mpio_result = MPI_File_set_view(fh, rank*sizeof(int), MPI_INT, filetype, "native", MPI_INFO_NULL);
    CHECK_SUCCESS(mpio_result);

    mpio_result = MPI_File_write(fh, writebuf, write_size, MPI_INT, &Status);
    CHECK_SUCCESS(mpio_result);

    MPI_Barrier(MPI_COMM_WORLD);

    mpio_result = MPI_File_set_view(fh, 0, MPI_BYTE, MPI_BYTE, "native", MPI_INFO_NULL);
    CHECK_SUCCESS(mpio_result);

    for(offset = 0; offset<(write_size*numprocs*sizeof(int)); offset+=(numprocs*sizeof(int)))
    {
	mpio_result = MPI_File_read_at(fh, offset, readbuf, numprocs, MPI_INT, &Status);
	CHECK_SUCCESS(mpio_result);

	for(i=0; i<numprocs; i++)
	{
	    if(writebuf[offset/(numprocs*sizeof(int))] != readbuf[i])
	    {
		if( (rank == 0) && (counter == 0))
		    printf("\n");
		if(counter++ < max_err_print)
		    fprintf(stderr, "Arrays do not match!  Prcoess %d, element %d: [%d, %d]\n", rank, i, writebuf[offset/(numprocs*sizeof(int))], readbuf[i]);
		else if(counter++ == max_err_print+1)
		    fprintf(stderr, "Printed %d errors.  Omitting the rest\n", max_err_print);
		err_flag = -1;
	    }

	}
    }
    nmismatches += counter;
    mpio_result = MPI_File_close(&fh);
    CHECK_SUCCESS(mpio_result);

    HDfree(writebuf);
    HDfree(readbuf);
    return err_flag;


}

/* Overlapping pattern works as follows (this test requires at least 2 procs:
 * Writes:
 * Task 0: 0	2	4	6	etc...
 * Task 1:    1	    3       5      7    etc...
 * Task 2: 0	    3		6	etc..
 * Task 3: 0		4		8	etc...
 *
 * The above describes only the pattern of the elements being written.  The actual
 * number of elements written is going to be:
 *
 * Task i where i=(numprocs-1) writes write_size elements.  All other tasks do:
 *  x = ((write_size-1)*numprocs)
 *  Task i's write_size is the smallest multiple of i<=x divided by i, with the exception
 *  of tasks 0 and 1, for whom i is 2, since they are writing the even and odd multiples.
 *
 *  So, if there are 5 tasks with write_size=4, the resulting pattern of writes is:
 *
 *  Task 0: 0	2	4	6	8	10	12	14
 *  Task 1:   1     3       5       7       9       11      13       15
 *  Task 2: 0       3		6	    9		12	     15
 *  Task 3: 0		4		8		12
 *  Task 4: 0		    5			10		     15
 *
 *
 *
 *  * * All the entires that overlap will therefore be writing the same value
 *
 *  At the end, all tasks read in the file and verify that it is correct should be
 *  (1,2...((numprocs-1)*WRTE_SIZE).
 *  */

static int allwrite_allread_overlap(int numprocs, int rank, int write_size)
{

    MPI_File	fh = MPI_FILE_NULL;
    int		mpio_result;
    int		amode, i, counter = 0;
    MPI_Datatype filetype;
    MPI_Status	Status;
    int*	writebuf = (int*) malloc(write_size*(numprocs-1)*sizeof(int)); /* An upper bound...not all the elements will be written */
    int*	readbuf = (int*) malloc(write_size*(numprocs-1)*sizeof(int));

    if(numprocs < 2)
    {
	fprintf(stderr, "The allwrite_allread_overlap test requires at least 2 procs\n");
	return -1;
    }

    if(rank == 0)
    {
	for(i=0; i<write_size*(numprocs-1); i++)
	    writebuf[i] = 2*i;
    }
    else if(rank == 1)
    {
	for(i=0; i<write_size*(numprocs-1); i++)
	    writebuf[i] = (2*i)+1;
    }
    else
    {
	for(i=0; i<write_size*(numprocs-1); i++)
	    writebuf[i] = (rank+1)*i;
    }

    amode = MPI_MODE_CREATE | MPI_MODE_RDWR | MPI_MODE_DELETE_ON_CLOSE;
    mpio_result = MPI_File_open(MPI_COMM_WORLD, testfile, amode, MPI_INFO_NULL, &fh);
    CHECK_SUCCESS(mpio_result);

    if( (rank == 0) || (rank == 1) )
	mpio_result = MPI_Type_vector(write_size*(numprocs-1), 1, 2, MPI_INT, &filetype);
    else
	mpio_result = MPI_Type_vector(write_size*(numprocs-1), 1, rank+1, MPI_INT, &filetype);
    CHECK_SUCCESS(mpio_result);

    mpio_result = MPI_Type_commit(&filetype);
    CHECK_SUCCESS(mpio_result);

    if( rank == 1)
	mpio_result = MPI_File_set_view(fh, sizeof(int), MPI_INT, filetype, "native", MPI_INFO_NULL);
    else
	mpio_result = MPI_File_set_view(fh, 0, MPI_INT, filetype, "native", MPI_INFO_NULL);
    CHECK_SUCCESS(mpio_result);

    if( rank == (numprocs - 1))
	mpio_result = MPI_File_write(fh, writebuf, write_size, MPI_INT, &Status);
    else
	mpio_result = MPI_File_write(fh, writebuf, find_writesize(rank, numprocs, write_size), MPI_INT, &Status);

    CHECK_SUCCESS(mpio_result);

    MPI_Barrier(MPI_COMM_WORLD);
    mpio_result = MPI_File_set_view(fh, 0, MPI_BYTE, MPI_BYTE, "native", MPI_INFO_NULL);
    CHECK_SUCCESS(mpio_result);

    mpio_result = MPI_File_read_at(fh, 0, readbuf, write_size*(numprocs-1), MPI_INT, &Status);
    CHECK_SUCCESS(mpio_result);

    for(i=0; i<write_size*(numprocs-1); i++)
    {
	if(i != readbuf[i])
	{
    	    if( (rank == 0) && (counter == 0))
		    printf("\n");
	    if(counter++ < max_err_print)
		fprintf(stderr, "Arrays do not match!  Prcoess %d, element %d: [%d, %d]\n", rank, i, i, readbuf[i]);
	    else if(counter++ == max_err_print+1)
		fprintf(stderr, "Printed %d errors.  Omitting the rest\n", max_err_print);
	    err_flag = -1;
    	}
    }

    nmismatches += counter;
    mpio_result = MPI_File_close(&fh);
    CHECK_SUCCESS(mpio_result);
    HDfree(writebuf);
    HDfree(readbuf);

    return err_flag;

}

/* A random process writes out the following to the file:
 * 0 1 2 3 4 0 1 2 3 4 0 1 2 3 4 (assuming write_size=5, and numprocs=3)
 *
 * Process i read's in write_size bytes at offset=i*write_size
 */
static int onewrite_allread_blocks(int numprocs, int rank, int write_size)
{
    MPI_File	fh = MPI_FILE_NULL;
    int		mpio_result;
    int		amode, i;
    MPI_Status	Status;
    int* writebuf = (int*)malloc(write_size*sizeof(int));
    int* readbuf = (int*)malloc (write_size*sizeof(int));

    for(i=0; i<write_size; i++)
	writebuf[i] = i;

    amode = MPI_MODE_CREATE | MPI_MODE_RDWR | MPI_MODE_DELETE_ON_CLOSE;

    mpio_result = MPI_File_open(MPI_COMM_WORLD, testfile, amode,
	    MPI_INFO_NULL, &fh);
    CHECK_SUCCESS(mpio_result);

    /* A random process writes out all the data */
    if(rank == (rand() % numprocs))
    {
	for(i=0; i<numprocs; i++)
	{
	    mpio_result = MPI_File_write_at(fh, write_size*i*sizeof(int), writebuf, write_size, MPI_INT, &Status);
	    CHECK_SUCCESS(mpio_result);
	}
    }

    MPI_Barrier(MPI_COMM_WORLD);

    mpio_result = MPI_File_read_at(fh, write_size*rank*sizeof(int), readbuf, write_size, MPI_INT, &Status);
    CHECK_SUCCESS(mpio_result);

    vrfy_elements(writebuf, readbuf, write_size, rank);

    mpio_result = MPI_File_close(&fh);
    CHECK_SUCCESS(mpio_result);
    HDfree(writebuf);
    HDfree(readbuf);

    return err_flag;



}

/* Process zero writes out:
 * 0000 1111 2222 3333 etc. (with 4 procs)
 *
 * Each proc reads out 0 1 2 3 starting at displacement i */
static int onewrite_allread_interlaced(int numprocs, int rank, int write_size)
{
    MPI_File	fh = MPI_FILE_NULL;
    int		mpio_result;
    int		amode, i;
    MPI_Datatype filetype;
    MPI_Status	Status;
    int*	writebuf = (int*) malloc(numprocs*write_size*sizeof(int)); /* Upper bound, not all used */
    int* readbuf = (int*)malloc (write_size*sizeof(int));


    amode = MPI_MODE_CREATE | MPI_MODE_RDWR;
    amode = MPI_MODE_CREATE | MPI_MODE_RDWR | MPI_MODE_DELETE_ON_CLOSE;
    mpio_result = MPI_File_open(MPI_COMM_WORLD, testfile, amode, MPI_INFO_NULL, &fh);
    CHECK_SUCCESS(mpio_result);

    mpio_result = MPI_Type_vector(write_size, 1, numprocs, MPI_INT, &filetype);
    CHECK_SUCCESS(mpio_result);

    mpio_result = MPI_Type_commit(&filetype);
    CHECK_SUCCESS(mpio_result);

    if(rank == (rand() % numprocs))
    {
	for(i=0; i<write_size; i++)
	{
	    int j;
	    for(j=0; j<numprocs; j++)
		writebuf[j] = i;

	    mpio_result = MPI_File_write_at(fh, i*numprocs*sizeof(int), writebuf, numprocs, MPI_INT, &Status);
	    CHECK_SUCCESS(mpio_result);
	}
    }

    MPI_Barrier(MPI_COMM_WORLD);

    mpio_result = MPI_File_set_view(fh, rank*sizeof(int), MPI_INT, filetype, "native", MPI_INFO_NULL);
    CHECK_SUCCESS(mpio_result);

    mpio_result = MPI_File_read_at(fh, 0, readbuf, write_size, MPI_INT, &Status);
    CHECK_SUCCESS(mpio_result);

    for(i=0; i<write_size; i++)
	writebuf[i] = i;

    vrfy_elements(writebuf, readbuf, write_size, rank);

    mpio_result = MPI_File_close(&fh);
    CHECK_SUCCESS(mpio_result);
    HDfree(writebuf);
    HDfree(readbuf);

    return err_flag;

}

static int find_writesize(int rank, int numprocs, int size)
{
    /* Largest number in the file */
    int tmp = (size-1)*numprocs;
    int x = 0;
    int write_size = 0;

    /* Find largest multiple not greater than tmp */
    while(x <= tmp)
    {
	if( (rank == 0) || (rank == 1) )
	    x+=2;
	else
	    x += (rank+1);

	write_size++;
    }

    return write_size;
}

static void
vrfy_elements(int* a, int* b, int size, int rank)
{
    int i, counter = 0;

    for(i=0; i<size; i++)
    {
	if(a[i] != b[i])
	{
	    if( (rank == 0) && (counter == 0))
		printf("\n");
	    if(counter++ < max_err_print)
		fprintf(stderr, "Arrays do not match!  Prcoess %d, element %d: [%d, %d]\n", rank, i, a[i], b[i]);
	    else if(counter++ == max_err_print+1)
		fprintf(stderr, "Printed %d errors.  Omitting the rest\n", max_err_print);
	    err_flag = -1;
	}
    }
    nmismatches += counter;
    fflush(stderr);
    fflush(stdout);
}

/* print an explanation message by MAIN (0) process.
 */
static void
header_msg(void)
{
    printf(
"Purpose:\n"
"This tests if the file system is posix compliant when POSIX and MPI IO APIs\n"
"are used.  This is for information only and always exits with 0 even when\n"
"non-compliance errors are encounter.  This is to prevent this test from\n"
"aborting the remaining parallel HDF5 tests unnecessarily.\n\n"
    );
}

int
main(int argc, char* argv[])
{

    int numprocs, rank, opt, mpi_tests=1, posix_tests=1;
    int lb, ub, inc;
    int	write_size = 0;
    char    optstring[] = "h x m p: s: v:";
    char *prefix;

    err_flag = 0;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    if (rank == 0)
	header_msg();
    while((opt = getopt(argc, argv, optstring)) != -1)
    {
	switch(opt)
	{
	    case 'h':
		if(rank == 0)
		    printf("Usage: %s [options]\n"
			    "-h       prints this help message\n"
			    "-x	      run the posix i/o tests ONLY (default: posix and MPI)\n"
			    "-m	      run the mpi i/o tests ONLY (default: posix and MPI)\n"
			    "-s size  Run the test for the specific size.  Default is 1024, 4096, 16384, ..., 1048576\n"
			    "-p path   specifies path for test file.  Default is current directory\n"
			    "-v num   Specifies number of unmatching entries to print (default 10, pass -1 for all)\n", argv[0]);
		goto done;
	    case 'x':
		mpi_tests = 0;
		posix_tests = 1;
		break;
	    case 'm':
		mpi_tests = 1;
		posix_tests = 0;
		break;
	    case 'p':
		/* need 2 extra--1 for the / and 1 for the terminating NULL. */
		testfile = (char*) HDmalloc(strlen(optarg) + 2 + strlen(TESTFNAME));
		strcpy(testfile, optarg);
		/* Append a / just in case they didn't end their path with one */
		strcat(testfile, "/" TESTFNAME);
		break;
	    case 's':
		write_size = atoi(optarg);
		break;
	    case 'v':
		max_err_print = atoi(optarg);
		break;
	}
    }

    if( (optind < argc) && (rank == 0))
	fprintf(stderr, "Unkown command-line argument passed.  Continuing anyway...\n");

    if (!testfile){
	/* Try environment variable if not given as option. */
	prefix = getenv_all(MPI_COMM_WORLD, 0, "HDF5_PARAPREFIX");
	if (prefix)
	{
	    /* need 2 extra--1 for the / and 1 for the terminating NULL. */
	    testfile = (char*) HDmalloc(strlen(prefix) + 2 + strlen(TESTFNAME));
	    strcpy(testfile, prefix);
	    /* Append a / just in case they didn't end their path with one */
	    strcat(testfile, "/" TESTFNAME);
	}
	else
	{
	    testfile = strdup(TESTFNAME);
	}
    }
    printf("Process %d: testfile=%s\n", rank, testfile);
    fflush(stdout);
    MPI_Barrier(MPI_COMM_WORLD);

    if(write_size == 0)
    {
	lb = 16*numprocs*sizeof(int);
	/* 1MB MPIO-IO overlapping is failing in copper. Lower it now pending
	   permenant fix for copper.*/
	/* ub = 1024*1024;*/
	ub = lb*128;
	inc = 4;
    }
    else
    {
	lb = write_size;
	ub = write_size+1;
	inc = 2;
    }

#ifndef STANDALONE
    /* set alarm. */
    ALARM_ON;
#endif

    for(write_size = lb; write_size <= ub; write_size*=inc)
    {
	if(rank == 0)
	    printf("\nTesting size %d\n", write_size);

	if(mpi_tests)
	{
	    if(rank == 0)
		printf("Testing allwrite_allread_blocks with MPI IO\t\t"); fflush(stdout);
	    allwrite_allread_blocks(numprocs, rank, write_size/(numprocs*sizeof(int)));
	    PRINT_RESULT();
	    MPI_Barrier(MPI_COMM_WORLD);

	    if(rank == 0)
		printf("Testing allwrite_allread_interlaced with MPI IO\t\t"); fflush(stdout);
	    allwrite_allread_interlaced(numprocs, rank, write_size/(numprocs*sizeof(int)));
	    PRINT_RESULT();
	    MPI_Barrier(MPI_COMM_WORLD);

	    if(rank == 0)
		printf("Testing allwrite_allread_overlap with MPI IO\t\t"); fflush(stdout);
	    allwrite_allread_overlap(numprocs, rank, write_size);
	    PRINT_RESULT();
	    MPI_Barrier(MPI_COMM_WORLD);

	    if(rank == 0)
		printf("Testing onewrite_allread_blocks with MPI IO\t\t"); fflush(stdout);
	    onewrite_allread_blocks(numprocs, rank, write_size/(numprocs*sizeof(int)));
	    PRINT_RESULT();
	    MPI_Barrier(MPI_COMM_WORLD);

	    if(rank == 0)
		printf("Testing onewrite_allread_interlaced with MPI IO\t\t"); fflush(stdout);
	    onewrite_allread_interlaced(numprocs, rank, write_size/(numprocs*sizeof(int)));
	    PRINT_RESULT();
	    MPI_Barrier(MPI_COMM_WORLD);
	}

	if(posix_tests)
	{
	    if(rank == 0)
		printf("Testing allwrite_allread_blocks with POSIX IO\t\t"); fflush(stdout);
	    posix_allwrite_allread_blocks(numprocs, rank, write_size/(numprocs*sizeof(int)));
	    PRINT_RESULT();
	    MPI_Barrier(MPI_COMM_WORLD);

	    if(rank == 0)
		printf("Testing onewrite_allread_blocks with POSIX IO\t\t"); fflush(stdout);
	    posix_onewrite_allread_blocks(numprocs, rank, write_size/(numprocs*sizeof(int)));
	    PRINT_RESULT();
	    MPI_Barrier(MPI_COMM_WORLD);

	    if(rank == 0)
		printf("Testing onewrite_allread_interlaced with POSIX IO\t"); fflush(stdout);
	    posix_onewrite_allread_interlaced(numprocs, rank, write_size/(numprocs*sizeof(int)));
	    PRINT_RESULT();
	    MPI_Barrier(MPI_COMM_WORLD);

	/*    if(rank == 0)
		printf("Testing allwrite_allread_overlap with POSIX IO\t\t"); fflush(stdout);
	    posix_allwrite_allread_overlap(numprocs, rank, write_size);
	    PRINT_RESULT();
	    MPI_Barrier(MPI_COMM_WORLD);
*/
	}
    }

#ifndef STANDALONE
    /* turn off alarm */
    ALARM_OFF;
#endif

done:
    if (testfile)
	HDfree(testfile);
    if (rank == 0){
	printf("\nSummary:\n");
	fflush(stdout);
    }
    MPI_Barrier(MPI_COMM_WORLD);
    printf("Process %d: encountered %d mismatches.\n", rank, nmismatches);
    MPI_Finalize();

    return 0;
}
