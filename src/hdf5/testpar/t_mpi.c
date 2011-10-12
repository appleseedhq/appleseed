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
 * MPIO independent overlapping writes.
 *
 * First n-1 processes open 1 file.
 * Each of the n-1 process writes chunks of data to the file in round-robin
 * fashion, in a interleaved but not overlapped fashion.  Using increasing
 * chunk sizes for the benefits of testing different write sizes and also
 * reducing the numbers of writes.
 *
 * Last process (n-1) just waits.
 * First n-1 processes finish writing and cloose the file.
 * Last process opens the same file and verifies the data.
 */

#include "testpar.h"

/* FILENAME and filenames must have the same number of names */
const char *FILENAME[2]={
	    "MPItest",
	    NULL};
char	filenames[2][200];
int	nerrors = 0;
hid_t	fapl;				/* file access property list */

/* protocols */
static int errors_sum(int nerrs);

#define MPIO_TEST_WRITE_SIZE 1024*1024     /* 1 MB */

static int
test_mpio_overlap_writes(char *filename)
{
    int mpi_size, mpi_rank;
    MPI_Comm comm;
    MPI_Info info = MPI_INFO_NULL;
    int color, mrc;
    MPI_File	fh;
    int i;
    int vrfyerrs, nerrs;
    unsigned char  buf[4093];		/* use some prime number for size */
    int bufsize = sizeof(buf);
    MPI_Offset  stride;
    MPI_Offset  mpi_off;
    MPI_Status  mpi_stat;


    if (VERBOSE_MED)
	printf("MPIO independent overlapping writes test on file %s\n",
	    filename);

    nerrs = 0;
    /* set up MPI parameters */
    MPI_Comm_size(MPI_COMM_WORLD,&mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);

    /* Need at least 2 processes */
    if (mpi_size < 2) {
	if (MAINPROCESS)
	    printf("Need at least 2 processes to run MPIO test.\n");
	    printf(" -SKIP- \n");
	return 0;
    }

    /* splits processes 0 to n-2 into one comm. and the last one into another */
    color = ((mpi_rank < (mpi_size - 1)) ? 0 : 1);
    mrc = MPI_Comm_split (MPI_COMM_WORLD, color, mpi_rank, &comm);
    VRFY((mrc==MPI_SUCCESS), "Comm_split succeeded");

    if (color==0){
	/* First n-1 processes (color==0) open a file and write it */
	mrc = MPI_File_open(comm, filename, MPI_MODE_CREATE|MPI_MODE_RDWR,
		info, &fh);
	VRFY((mrc==MPI_SUCCESS), "");

	stride = 1;
	mpi_off = mpi_rank*stride;
	while (mpi_off < MPIO_TEST_WRITE_SIZE){
	    /* make sure the write does not exceed the TEST_WRITE_SIZE */
	    if (mpi_off+stride > MPIO_TEST_WRITE_SIZE)
		stride = MPIO_TEST_WRITE_SIZE - mpi_off;

	    /* set data to some trivial pattern for easy verification */
	    for (i=0; i<stride; i++)
		buf[i] = (unsigned char)(mpi_off+i);
	    mrc = MPI_File_write_at(fh, mpi_off, buf, (int)stride, MPI_BYTE,
		    &mpi_stat);
	    VRFY((mrc==MPI_SUCCESS), "");

	    /* move the offset pointer to last byte written by all processes */
	    mpi_off += (mpi_size - 1 - mpi_rank) * stride;

	    /* Increase chunk size without exceeding buffer size. */
	    /* Then move the starting offset for next write. */
	    stride *= 2;
	    if (stride > bufsize)
		stride = bufsize;
	    mpi_off += mpi_rank*stride;
	}

	/* close file and free the communicator */
	mrc = MPI_File_close(&fh);
	VRFY((mrc==MPI_SUCCESS), "MPI_FILE_CLOSE");
	mrc = MPI_Comm_free(&comm);
	VRFY((mrc==MPI_SUCCESS), "MPI_Comm_free");

	/* sync with the other waiting processes */
	mrc = MPI_Barrier(MPI_COMM_WORLD);
	VRFY((mrc==MPI_SUCCESS), "Sync after writes");
    }else{
	/* last process waits till writes are done,
	 * then opens file to verify data.
	 */
	mrc = MPI_Barrier(MPI_COMM_WORLD);
	VRFY((mrc==MPI_SUCCESS), "Sync after writes");

	mrc = MPI_File_open(comm, filename, MPI_MODE_RDONLY,
		info, &fh);
	VRFY((mrc==MPI_SUCCESS), "");

	stride = bufsize;
	for (mpi_off=0; mpi_off < MPIO_TEST_WRITE_SIZE; mpi_off += bufsize){
	    /* make sure it does not read beyond end of data */
	    if (mpi_off+stride > MPIO_TEST_WRITE_SIZE)
		stride = MPIO_TEST_WRITE_SIZE - mpi_off;
	    mrc = MPI_File_read_at(fh, mpi_off, buf, (int)stride, MPI_BYTE,
		    &mpi_stat);
	    VRFY((mrc==MPI_SUCCESS), "");
	    vrfyerrs=0;
	    for (i=0; i<stride; i++){
		unsigned char expected;
		expected = (unsigned char)(mpi_off+i);
		if ((expected != buf[i]) &&
		    (vrfyerrs++ < MAX_ERR_REPORT || VERBOSE_MED)) {
			printf("proc %d: found data error at [%ld], expect %u, got %u\n",
			    mpi_rank, (long)(mpi_off+i), expected, buf[i]);
		}
	    }
	    if (vrfyerrs > MAX_ERR_REPORT && !VERBOSE_MED)
		printf("proc %d: [more errors ...]\n", mpi_rank);

	    nerrs += vrfyerrs;
	}

	/* close file and free the communicator */
	mrc = MPI_File_close(&fh);
	VRFY((mrc==MPI_SUCCESS), "MPI_FILE_CLOSE");
	mrc = MPI_Comm_free(&comm);
	VRFY((mrc==MPI_SUCCESS), "MPI_Comm_free");
    }

    /*
     * one more sync to ensure all processes have done reading
     * before ending this test.
     */
    mrc = MPI_Barrier(MPI_COMM_WORLD);
    VRFY((mrc==MPI_SUCCESS), "Sync before leaving test");
    return (nerrs);
}


#define MB      1048576        /* 1024*1024 == 2**20 */
#define GB      1073741824     /* 1024**3 == 2**30 */
#define TWO_GB_LESS1    2147483647     /* 2**31 - 1 */
#define FOUR_GB_LESS1   4294967295L     /* 2**32 - 1 */
/*
 * Verify that MPI_Offset exceeding 2**31 can be computed correctly.
 * Print any failure as information only, not as an error so that this
 * won't abort the remaining test or other separated tests.
 *
 * Test if MPIO can write file from under 2GB to over 2GB and then
 * from under 4GB to over 4GB.
 * Each process writes 1MB in round robin fashion.
 * Then reads the file back in by reverse order, that is process 0
 * reads the data of process n-1 and vice versa.
 */
static int
test_mpio_gb_file(char *filename)
{
    int mpi_size, mpi_rank;
    MPI_Info info = MPI_INFO_NULL;
    int mrc;
    MPI_File	fh;
    int i, j, n;
    int vrfyerrs;
    int writerrs;		/* write errors */
    int nerrs;
    int ntimes;			/* how many times */
    char  *buf = NULL;
    char  expected;
    MPI_Offset  size;
    MPI_Offset  mpi_off;
    MPI_Offset  mpi_off_old;
    MPI_Status  mpi_stat;
    struct stat stat_buf;
    int is_signed, sizeof_mpi_offset;

    nerrs = 0;
    /* set up MPI parameters */
    MPI_Comm_size(MPI_COMM_WORLD,&mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);

    if (VERBOSE_MED)
        printf("MPI_Offset range test\n");

    /* figure out the signness and sizeof MPI_Offset */
    mpi_off = 0;
    is_signed = ((MPI_Offset)(mpi_off - 1)) < 0;
    sizeof_mpi_offset = (int)(sizeof(MPI_Offset));

    /*
     * Verify the sizeof MPI_Offset and correctness of handling multiple GB
     * sizes.
     */
    if (MAINPROCESS){			/* only process 0 needs to check it*/
	printf("MPI_Offset is %s %d bytes integeral type\n",
	    is_signed ? "signed" : "unsigned", (int)sizeof(MPI_Offset));
	if (sizeof_mpi_offset <= 4 && is_signed){
	    printf("Skipped 2GB range test "
		    "because MPI_Offset cannot support it\n");
	}else {
	    /* verify correctness of assigning 2GB sizes */
	    mpi_off = 2 * 1024 * (MPI_Offset)MB;
	    INFO((mpi_off>0), "2GB OFFSET assignment no overflow");
	    INFO((mpi_off-1)==TWO_GB_LESS1, "2GB OFFSET assignment succeed");

	    /* verify correctness of increasing from below 2 GB to above 2GB */
	    mpi_off = TWO_GB_LESS1;
	    for (i=0; i < 3; i++){
		mpi_off_old = mpi_off;
		mpi_off = mpi_off + 1;
		/* no overflow */
		INFO((mpi_off>0), "2GB OFFSET increment no overflow");
		/* correct inc. */
		INFO((mpi_off-1)==mpi_off_old, "2GB OFFSET increment succeed");
	    }
	}

	if (sizeof_mpi_offset <= 4){
	    printf("Skipped 4GB range test "
		    "because MPI_Offset cannot support it\n");
	}else {
	    /* verify correctness of assigning 4GB sizes */
	    mpi_off = 4 * 1024 * (MPI_Offset)MB;
	    INFO((mpi_off>0), "4GB OFFSET assignment no overflow");
	    INFO((mpi_off-1)==FOUR_GB_LESS1, "4GB OFFSET assignment succeed");

	    /* verify correctness of increasing from below 4 GB to above 4 GB */
	    mpi_off = FOUR_GB_LESS1;
	    for (i=0; i < 3; i++){
		mpi_off_old = mpi_off;
		mpi_off = mpi_off + 1;
		/* no overflow */
		INFO((mpi_off>0), "4GB OFFSET increment no overflow");
		/* correct inc. */
		INFO((mpi_off-1)==mpi_off_old, "4GB OFFSET increment succeed");
	    }
	}
    }

    /*
     * Verify if we can write to a file of multiple GB sizes.
     */
    if (VERBOSE_MED)
	printf("MPIO GB file test %s\n", filename);

    if (sizeof_mpi_offset <= 4){
	printf("Skipped GB file range test "
		"because MPI_Offset cannot support it\n");
    }else{
	buf = malloc(MB);
	VRFY((buf!=NULL), "malloc succeed");

	/* open a new file. Remove it first in case it exists. */
	/* Must delete because MPI_File_open does not have a Truncate mode. */
	/* Don't care if it has error. */
	MPI_File_delete(filename, MPI_INFO_NULL);
	MPI_Barrier(MPI_COMM_WORLD);	/* prevent racing condition */

	mrc = MPI_File_open(MPI_COMM_WORLD, filename, MPI_MODE_CREATE|MPI_MODE_RDWR,
		    info, &fh);
	VRFY((mrc==MPI_SUCCESS), "MPI_FILE_OPEN");

	printf("MPIO GB file write test %s\n", filename);

	/* instead of writing every bytes of the file, we will just write
	 * some data around the 2 and 4 GB boundaries.  That should cover
	 * potential integer overflow and filesystem size limits.
	 */
	writerrs = 0;
	for (n=2; n <= 4; n+=2){
	    ntimes = GB/MB*n/mpi_size + 1;
	    for (i=ntimes-2; i <= ntimes; i++){
		mpi_off = (i*mpi_size + mpi_rank)*(MPI_Offset)MB;
		if (VERBOSE_MED)
		    HDfprintf(stdout,"proc %d: write to mpi_off=%016llx, %lld\n",
			mpi_rank, mpi_off, mpi_off);
		/* set data to some trivial pattern for easy verification */
		for (j=0; j<MB; j++)
		    *(buf+j) = i*mpi_size + mpi_rank;
		if (VERBOSE_MED)
		    HDfprintf(stdout,"proc %d: writing %d bytes at offset %lld\n",
			mpi_rank, MB, mpi_off);
		mrc = MPI_File_write_at(fh, mpi_off, buf, MB, MPI_BYTE, &mpi_stat);
		INFO((mrc==MPI_SUCCESS), "GB size file write");
		if (mrc!=MPI_SUCCESS)
		    writerrs++;
	    }
	}

	/* close file and free the communicator */
	mrc = MPI_File_close(&fh);
	VRFY((mrc==MPI_SUCCESS), "MPI_FILE_CLOSE");

	mrc = MPI_Barrier(MPI_COMM_WORLD);
	VRFY((mrc==MPI_SUCCESS), "Sync after writes");

	/*
	 * Verify if we can read the multiple GB file just created.
	 */
	/* open it again to verify the data written */
	/* but only if there was no write errors */
	printf("MPIO GB file read test %s\n", filename);
	if (errors_sum(writerrs)>0){
	    printf("proc %d: Skip read test due to previous write errors\n",
		mpi_rank);
	    goto finish;
	}
	mrc = MPI_File_open(MPI_COMM_WORLD, filename, MPI_MODE_RDONLY, info, &fh);
	VRFY((mrc==MPI_SUCCESS), "");

	/* Only read back parts of the file that have been written. */
	for (n=2; n <= 4; n+=2){
	    ntimes = GB/MB*n/mpi_size + 1;
	    for (i=ntimes-2; i <= ntimes; i++){
		mpi_off = (i*mpi_size + (mpi_size - mpi_rank - 1))*(MPI_Offset)MB;
		if (VERBOSE_MED)
		    HDfprintf(stdout,"proc %d: read from mpi_off=%016llx, %lld\n",
			mpi_rank, mpi_off, mpi_off);
		mrc = MPI_File_read_at(fh, mpi_off, buf, MB, MPI_BYTE, &mpi_stat);
		INFO((mrc==MPI_SUCCESS), "GB size file read");
		expected = i*mpi_size + (mpi_size - mpi_rank - 1);
		vrfyerrs=0;
		for (j=0; j<MB; j++){
		    if ((*(buf+j) != expected) &&
			(vrfyerrs++ < MAX_ERR_REPORT || VERBOSE_MED)){
			    printf("proc %d: found data error at [%ld+%d], expect %d, got %d\n",
				mpi_rank, (long)mpi_off, j, expected, *(buf+j));
		    }
		}
		if (vrfyerrs > MAX_ERR_REPORT && !VERBOSE_MED)
		    printf("proc %d: [more errors ...]\n", mpi_rank);

		nerrs += vrfyerrs;
	    }
	}

	/* close file and free the communicator */
	mrc = MPI_File_close(&fh);
	VRFY((mrc==MPI_SUCCESS), "MPI_FILE_CLOSE");

	/*
	 * one more sync to ensure all processes have done reading
	 * before ending this test.
	 */
	mrc = MPI_Barrier(MPI_COMM_WORLD);
	VRFY((mrc==MPI_SUCCESS), "Sync before leaving test");

        /*
         * Check if MPI_File_get_size works correctly.  Some systems (only SGI Altix
         * Propack 4 so far) return wrong file size.  It can be avoided by reconfiguring
         * with "--disable-mpi-size".
         */
#ifdef H5_HAVE_MPI_GET_SIZE
	printf("Test if MPI_File_get_size works correctly with %s\n", filename);

	mrc = MPI_File_open(MPI_COMM_WORLD, filename, MPI_MODE_RDONLY, info, &fh);
        VRFY((mrc==MPI_SUCCESS), "");

        if (MAINPROCESS){			/* only process 0 needs to check it*/
            mrc = MPI_File_get_size(fh, &size);
	    VRFY((mrc==MPI_SUCCESS), "");

            mrc=stat(filename, &stat_buf);
	    VRFY((mrc==0), "");

            /* Hopefully this casting is safe */
            if(size != (MPI_Offset)(stat_buf.st_size)) {
                printf("Warning: MPI_File_get_size doesn't return correct file size.  To avoid using it in the library, reconfigure and rebuild the library with --disable-mpi-size.\n");
            }
        }

	/* close file and free the communicator */
	mrc = MPI_File_close(&fh);
	VRFY((mrc==MPI_SUCCESS), "MPI_FILE_CLOSE");

	/*
	 * one more sync to ensure all processes have done reading
	 * before ending this test.
	 */
	mrc = MPI_Barrier(MPI_COMM_WORLD);
	VRFY((mrc==MPI_SUCCESS), "Sync before leaving test");
#else
        printf("Skipped testing MPI_File_get_size because it's disabled\n");
#endif
    }

finish:
    if (buf)
	HDfree(buf);
    return (nerrs);
}


/*
 * MPI-IO Test: One writes, Many reads.
 * Verify if only one process writes some data and then all other
 * processes can read them back correctly. This tests if the
 * underlaying parallel I/O and file system supports parallel I/O
 * correctly.
 *
 * Algorithm: Only one process (e.g., process 0) writes some data.
 * Then all processes, including the writing process, read the data
 * back and verify them against the original values.
 */

/*
 * Default filename can be specified via first program argument.
 * Each process writes something, then reads all data back.
 */

#define DIMSIZE	32		/* Dimension size. */
#define PRINTID printf("Proc %d: ", mpi_rank)
#define USENONE 0
#define USEATOM 1		/* request atomic I/O */
#define USEFSYNC 2		/* request file_sync */


static int
test_mpio_1wMr(char *filename, int special_request)
{
    char hostname[128];
    int  mpi_size, mpi_rank;
    MPI_File fh;
    char mpi_err_str[MPI_MAX_ERROR_STRING];
    int  mpi_err_strlen;
    int  mpi_err;
    unsigned char writedata[DIMSIZE], readdata[DIMSIZE];
    unsigned char expect_val;
    int  i, irank;
    int  nerrs = 0;		/* number of errors */
    int  atomicity;
    MPI_Offset  mpi_off;
    MPI_Status  mpi_stat;

    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    if (MAINPROCESS && VERBOSE_MED){
        printf("Testing one process writes, all processes read.\n");
	printf("Using %d processes accessing file %s\n", mpi_size, filename);
        printf("    (Filename can be specified via program argument)\n");
    }

    /* show the hostname so that we can tell where the processes are running */
    if (VERBOSE_DEF){
	if (gethostname(hostname, 128) < 0){
	    PRINTID;
	    printf("gethostname failed\n");
	    return 1;
	}
	PRINTID;
	printf("hostname=%s\n", hostname);
    }

    /* Delete any old file in order to start anew. */
    /* Must delete because MPI_File_open does not have a Truncate mode. */
    /* Don't care if it has error. */
    MPI_File_delete(filename, MPI_INFO_NULL);
    MPI_Barrier(MPI_COMM_WORLD);	/* prevent racing condition */

    if ((mpi_err = MPI_File_open(MPI_COMM_WORLD, filename,
	    MPI_MODE_RDWR | MPI_MODE_CREATE ,
	    MPI_INFO_NULL, &fh))
	    != MPI_SUCCESS){
	MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
	PRINTID;
	printf("MPI_File_open failed (%s)\n", mpi_err_str);
	return 1;
    }

if (special_request & USEATOM){
    /* ==================================================
     * Set atomcity to true (1).  A POSIX compliant filesystem
     * should not need this.
     * ==================================================*/
    if ((mpi_err = MPI_File_get_atomicity(fh, &atomicity)) != MPI_SUCCESS){
	MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
	PRINTID;
	printf("MPI_File_get_atomicity failed (%s)\n", mpi_err_str);
    }
    if (VERBOSE_HI)
	printf("Initial atomicity = %d\n", atomicity);
    if ((mpi_err = MPI_File_set_atomicity(fh, 1)) != MPI_SUCCESS){
	MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
	PRINTID;
	printf("MPI_File_set_atomicity failed (%s)\n", mpi_err_str);
    }
    if ((mpi_err = MPI_File_get_atomicity(fh, &atomicity)) != MPI_SUCCESS){
	MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
	PRINTID;
	printf("MPI_File_get_atomicity failed (%s)\n", mpi_err_str);
    }
    if (VERBOSE_HI)
	printf("After set_atomicity atomicity = %d\n", atomicity);
}

    /* This barrier is not necessary but do it anyway. */
    MPI_Barrier(MPI_COMM_WORLD);
    if (VERBOSE_HI){
	PRINTID;
	printf("between MPI_Barrier and MPI_File_write_at\n");
    }

    /* ==================================================
     * Each process calculates what to write but
     * only process irank(0) writes.
     * ==================================================*/
    irank=0;
    for (i=0; i < DIMSIZE; i++)
	writedata[i] = irank*DIMSIZE + i;
    mpi_off = irank*DIMSIZE;

    /* Only one process writes */
    if (mpi_rank==irank){
	if (VERBOSE_HI){
	    PRINTID; printf("wrote %d bytes at %ld\n", DIMSIZE, (long)mpi_off);
	}
	if ((mpi_err = MPI_File_write_at(fh, mpi_off, writedata, DIMSIZE,
			MPI_BYTE, &mpi_stat))
		!= MPI_SUCCESS){
	    MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
	    PRINTID;
	    printf("MPI_File_write_at offset(%ld), bytes (%d), failed (%s)\n",
		    (long) mpi_off, DIMSIZE, mpi_err_str);
	    return 1;
	};
    };

    /* Bcast the return code and */
    /* make sure all writing are done before reading. */
    MPI_Bcast(&mpi_err, 1, MPI_INT, irank, MPI_COMM_WORLD);
    if (VERBOSE_HI){
	PRINTID;
	printf("MPI_Bcast: mpi_err = %d\n", mpi_err);
    }

if (special_request & USEFSYNC){
    /* ==================================================
     * Do a file sync.  A POSIX compliant filesystem
     * should not need this.
     * ==================================================*/
    if (VERBOSE_HI)
	printf("Apply MPI_File_sync\n");
    /* call file_sync to force the write out */
    if ((mpi_err = MPI_File_sync(fh)) != MPI_SUCCESS){
	MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
	PRINTID;
	printf("MPI_File_sync failed (%s)\n", mpi_err_str);
    }
    MPI_Barrier(MPI_COMM_WORLD);
    /* call file_sync to force the write out */
    if ((mpi_err = MPI_File_sync(fh)) != MPI_SUCCESS){
	MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
	PRINTID;
	printf("MPI_File_sync failed (%s)\n", mpi_err_str);
    }
}

    /* This barrier is not necessary because the Bcase or File_sync above */
    /* should take care of it.  Do it anyway. */
    MPI_Barrier(MPI_COMM_WORLD);
    if (VERBOSE_HI){
	PRINTID;
	printf("after MPI_Barrier\n");
    }

    /* ==================================================
     * Each process reads what process 0 wrote and verify.
     * ==================================================*/
    irank=0;
    mpi_off = irank*DIMSIZE;
    if ((mpi_err = MPI_File_read_at(fh, mpi_off, readdata, DIMSIZE, MPI_BYTE,
	    &mpi_stat))
	    != MPI_SUCCESS){
	MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
	PRINTID;
	printf("MPI_File_read_at offset(%ld), bytes (%d), failed (%s)\n",
		(long) mpi_off, DIMSIZE, mpi_err_str);
	return 1;
    };
    for (i=0; i < DIMSIZE; i++){
	expect_val = irank*DIMSIZE + i;
	if (readdata[i] != expect_val){
	    PRINTID;
	    printf("read data[%d:%d] got %02x, expect %02x\n", irank, i,
		    readdata[i], expect_val);
	    nerrs++;
	}
    }

    MPI_File_close(&fh);

    if (VERBOSE_HI){
	PRINTID;
	printf("%d data errors detected\n", nerrs);
    }

    mpi_err = MPI_Barrier(MPI_COMM_WORLD);
    return nerrs;
}

/*

Function: test_mpio_derived_dtype

Test Whether the Displacement of MPI derived datatype
(+ File_set_view + MPI_write)works or not on this MPI-IO package
and this platform.

1. Details for the test:
1) Create two derived datatypes with MPI_Type_hindexed:
        datatype1:
	count = 1, blocklens = 1, offsets = 0,
	base type = MPI_BYTE(essentially a char)
        datatype2:
	count = 1, blocklens = 1, offsets = 1(byte),
	base type = MPI_BYTE

2) Using these two derived datatypes,
   Build another derived datatype with MPI_Type_struct:
        advtype: derived from datatype1 and datatype2
        advtype:
	count = 2, blocklens[0] = 1, blocklens[1]=1,
	offsets[0] = 0, offsets[1] = 1(byte),
        bas_type[0]=datatype1,
        bas_type[1] = datatype2;

3) Setting MPI file view with advtype
4) Writing 2 bytes 1 to 2 using MPI_File_write to a file
5) File content:
Suppose the fill value of the file is 0(most machines indeed do so)
and Fill value is embraced with "() in the following output:
Expected output should be:
1,0,2



However, at some platforms, for example, IBM AIX(at March 23rd, 2005):
the following values were obtained:
1,2,0

The problem is that the displacement of the second derived datatype(datatype2) which formed the final derived datatype(advtype)
 has been put after the basic datatype(MPI_BYTE) of datatype2. This is a bug.


2. This test will verify whether the complicated derived datatype is working on
the current platform.

If this bug has been fixed in the previous not-working package, this test will issue a printf message to tell the developer to change
the configuration specific file of HDF5 so that we can change our configurationsetting to support collective IO for irregular selections.

If it turns out that the previous working MPI-IO package no longer works, this test will also issue a message to inform the corresponding failure so that
we can turn off collective IO support for irregular selections.
*/

static int test_mpio_derived_dtype(char *filename) {

    MPI_File fh;
    char mpi_err_str[MPI_MAX_ERROR_STRING];
    int  mpi_err_strlen;
    int  mpi_err;
    int  i;
    int  nerrors = 0;		/* number of errors */
    MPI_Datatype  etype,filetype;
    MPI_Datatype  adv_filetype,bas_filetype[2];
    MPI_Datatype  etypenew, filetypenew;
    MPI_Offset    disp;
    MPI_Status    Status;
    MPI_Aint      adv_disp[2];
    MPI_Aint      offsets[1];
    int           blocklens[1],adv_blocklens[2];
    int           count,outcount;
    int           retcode;

    int mpi_rank,mpi_size;

    char          buf[3],outbuf[3] = {0};

    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    retcode = 0;
    for(i=0;i<3;i++)
      buf[i] = i+1;


    if ((mpi_err = MPI_File_open(MPI_COMM_WORLD, filename,
				 MPI_MODE_RDWR | MPI_MODE_CREATE,
				 MPI_INFO_NULL, &fh))
	    != MPI_SUCCESS){
	MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
	printf("MPI_File_open failed (%s)\n", mpi_err_str);
	return 1;
    }

    disp  = 0;
    etype = MPI_BYTE;

    count = 1;
    blocklens[0] = 1;
    offsets[0]   = 0;

    if((mpi_err= MPI_Type_hindexed(count,blocklens,offsets,MPI_BYTE,&filetype))
       != MPI_SUCCESS){
      	MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
	printf("MPI_Type_contiguous failed (%s)\n", mpi_err_str);
	return 1;
    }

    if((mpi_err=MPI_Type_commit(&filetype))!=MPI_SUCCESS){
        MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
	printf("MPI_Type_commit failed (%s)\n", mpi_err_str);
	return 1;
    }

    count = 1;
    blocklens[0]=1;
    offsets[0] = 1;
    if((mpi_err= MPI_Type_hindexed(count,blocklens,offsets,MPI_BYTE,&filetypenew))
       != MPI_SUCCESS){
      	MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
	printf("MPI_Type_contiguous failed (%s)\n", mpi_err_str);
	return 1;
    }

    if((mpi_err=MPI_Type_commit(&filetypenew))!=MPI_SUCCESS){
        MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
	printf("MPI_Type_commit failed (%s)\n", mpi_err_str);
	return 1;
    }

    outcount         = 2;
    adv_blocklens[0] = 1;
    adv_blocklens[1] = 1;
    adv_disp[0]      = 0;
    adv_disp[1]      = 1;
    bas_filetype[0]  = filetype;
    bas_filetype[1]  = filetypenew;

    if((mpi_err= MPI_Type_struct(outcount,adv_blocklens,adv_disp,bas_filetype,&adv_filetype))
       != MPI_SUCCESS){
      	MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
	printf("MPI_Type_struct failed (%s)\n", mpi_err_str);
	return 1;
    }
    if((mpi_err=MPI_Type_commit(&adv_filetype))!=MPI_SUCCESS){
        MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
	printf("MPI_Type_commit failed (%s)\n", mpi_err_str);
	return 1;
    }


    if((mpi_err = MPI_File_set_view(fh,disp,etype,adv_filetype,"native",MPI_INFO_NULL))!= MPI_SUCCESS){
      MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
	printf("MPI_File_set_view failed (%s)\n", mpi_err_str);
	return 1;
    }

    if((mpi_err = MPI_File_write(fh,buf,3,MPI_BYTE,&Status))!= MPI_SUCCESS){
        MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
	printf("MPI_File_write failed (%s)\n", mpi_err_str);
	return 1;
      ;
    }


    if((mpi_err = MPI_File_close(&fh)) != MPI_SUCCESS){
       MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
	printf("MPI_File_close failed (%s)\n", mpi_err_str);
	return 1;
    }


    if((mpi_err = MPI_File_open(MPI_COMM_WORLD,filename,MPI_MODE_RDONLY,MPI_INFO_NULL,&fh)) != MPI_SUCCESS){
       MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
	printf("MPI_File_open failed (%s)\n", mpi_err_str);
	return 1;
    }

    if((mpi_err = MPI_File_set_view(fh,0,MPI_BYTE,MPI_BYTE,"native",MPI_INFO_NULL))!= MPI_SUCCESS){
        MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
	printf("MPI_File_set_view failed (%s)\n", mpi_err_str);
	return 1;
    }
    if((mpi_err = MPI_File_read(fh,outbuf,3,MPI_BYTE,&Status))!=MPI_SUCCESS){
      MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
      printf("MPI_File_read failed (%s)\n", mpi_err_str);
      return 1;
    }

    if(outbuf[2]==2) {
       retcode = 0;
    }
    else {
/*      if(mpi_rank == 0) {
       printf("complicated derived datatype is NOT working at this platform\n");
       printf("go back to hdf5/config and find the corresponding\n");
       printf("configure-specific file and change ?????\n");
      }
*/
       retcode = -1;
   }

    if((mpi_err = MPI_File_close(&fh)) != MPI_SUCCESS){
       MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
	printf("MPI_File_close failed (%s)\n", mpi_err_str);
	return 1;
    }


    mpi_err = MPI_Barrier(MPI_COMM_WORLD);
#ifdef H5_MPI_COMPLEX_DERIVED_DATATYPE_WORKS
    if(retcode == -1) {
	if(mpi_rank == 0) {
	    printf("Complicated derived datatype is NOT working at this platform\n");
	    printf("Go back to hdf5/config and find the corresponding\n");
	    printf("configure-specific file (for example, powerpc-ibm-aix5.x) and add\n");
	    printf("hdf5_cv_mpi_complex_derived_datatype_works=${hdf5_cv_mpi_complex_derived_datatype-works='no'}\n");
	    printf(" at the end of the file.\n");
	    printf(" Please report to hdfhelp@ncsa.uiuc.edu about this problem.\n");
	}
	retcode = 1;
    }
#else
    if(retcode == 0) {
	if(mpi_rank == 0) {
	    printf(" This is NOT an error, What it really says is\n");
	    printf("Complicated derived datatype is WORKING at this platform\n");
	    printf(" Go back to hdf5/config and find the corresponding \n");
	    printf(" configure-specific file (for example, powerpc-ibm-aix5.x) and delete the line\n");
	    printf("hdf5_cv_mpi_complex_derived_datatype_works=${hdf5_cv_mpi_complex_derived_datatype-works='no'}\n");
	    printf(" at the end of the file.\n");
	    printf("Please report to hdfhelp@ncsa.uiuc.edu about this problem.\n");
	}
	retcode = 1;
    }
    if(retcode == -1) retcode = 0;
#endif
    return retcode;
}
/*

Function: test_mpio_special_collective

Test Whether collective IO is still working when more than one process
has no contribution to IO. To properly test this case, at least FOUR
processes are needed.

1. Details for the test:
1) Create one derived datatype with MPI_Type_hindexed:

2) Choosing at least two processes to contribute none for IO with
   the buf size inside MPI_Write_at_all to 0.
3) Choosing at least two processes to have real contributions for IO.
4) Do collective IO.

2. This test will fail with the MPI-IO package that doesn't support this. For example,
mpich 1.2.6.

If this bug has been fixed in the previous not-working package, this test will issue a printf message to tell the developer to change
the configuration specific file of HDF5 so that we can change our configurationsetting to support special collective IO; currently only special collective IO.

If it turns out that the previous working MPI-IO package no longer works, this test will also issue a message to inform the corresponding failure so that
we can turn off the support for special collective IO; currently only special collective IO.
*/

static int
test_mpio_special_collective(char *filename)
{
    int  mpi_size, mpi_rank;
    MPI_File fh;
    MPI_Datatype etype,buftype,filetype;
    char mpi_err_str[MPI_MAX_ERROR_STRING];
    int  mpi_err_strlen;
    int  mpi_err;
    char writedata[2];
    char *buf;
    int  i;
    int  count,bufcount;
    int blocklens[2];
    MPI_Aint offsets[2];
    MPI_Offset  mpi_off;
    MPI_Status  mpi_stat;
    int  retcode;

    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    retcode = 0;

    /* create MPI data type */
    etype = MPI_BYTE;
    if(mpi_rank == 0 || mpi_rank == 1) {
        count = DIMSIZE;
        bufcount = 1;
    }
    else {
        count = 0;
        bufcount = 0;
    }

    blocklens[0] = count;
    offsets[0] = mpi_rank*count;
    blocklens[1] = count;
    offsets[1] = (mpi_size+mpi_rank)*count;

    if(count !=0) {
      if((mpi_err= MPI_Type_hindexed(2,blocklens,offsets,etype,&filetype))
       != MPI_SUCCESS){
      	MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
	printf("MPI_Type_contiguous failed (%s)\n", mpi_err_str);
	return 1;
      }

      if((mpi_err=MPI_Type_commit(&filetype))!=MPI_SUCCESS){
        MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
	printf("MPI_Type_commit failed (%s)\n", mpi_err_str);
	return 1;
      }


      if((mpi_err= MPI_Type_hindexed(2,blocklens,offsets,etype,&buftype))
       != MPI_SUCCESS){
      	MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
	printf("MPI_Type_contiguous failed (%s)\n", mpi_err_str);
	return 1;
      }

      if((mpi_err=MPI_Type_commit(&buftype))!=MPI_SUCCESS){
        MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
	printf("MPI_Type_commit failed (%s)\n", mpi_err_str);
	return 1;
      }
     }
     else {

       filetype = MPI_BYTE;
       buftype  = MPI_BYTE;
     }

   /* Open a file */
    if ((mpi_err = MPI_File_open(MPI_COMM_WORLD, filename,
	    MPI_MODE_RDWR | MPI_MODE_CREATE ,
	    MPI_INFO_NULL, &fh))
	    != MPI_SUCCESS){
	MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
	printf("MPI_File_open failed (%s)\n", mpi_err_str);
	return 1;
    }

    /* each process writes some data */
    for (i=0; i < 2*DIMSIZE; i++)
	writedata[i] = mpi_rank*DIMSIZE + i;


     mpi_off = 0;
    if((mpi_err = MPI_File_set_view(fh, mpi_off, MPI_BYTE, filetype, "native", MPI_INFO_NULL))
        != MPI_SUCCESS) {
	MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
	printf("MPI_File_set_view failed (%s)\n", mpi_err_str);
	return 1;
    }

    buf   = writedata;
    if ((mpi_err = MPI_File_write_at_all(fh, mpi_off, buf, bufcount, buftype,
	    &mpi_stat))
	    != MPI_SUCCESS){
	MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
	printf("MPI_File_write_at offset(%ld), bytes (%d), failed (%s)\n",
		(long) mpi_off, bufcount, mpi_err_str);
	return 1;
    };

     if ((mpi_err = MPI_File_close(&fh))
	    != MPI_SUCCESS){
	MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
	printf("MPI_File_close failed. \n");
	return 1;
    };

    mpi_err = MPI_Barrier(MPI_COMM_WORLD);
#ifdef H5_MPI_SPECIAL_COLLECTIVE_IO_WORKS
    if(retcode != 0) {
	if(mpi_rank == 0) {
	    printf("special collective IO is NOT working at this platform\n");
	    printf("Go back to hdf5/config and find the corresponding\n");
	    printf("configure-specific file (for example, powerpc-ibm-aix5.x) and add\n");
	    printf("hdf5_cv_mpi_special_collective_io_works=${hdf5_cv_mpi_special_collective_io_works='no'}\n");
	    printf(" at the end of the file.\n");
	    printf(" Please report to hdfhelp@ncsa.uiuc.edu about this problem.\n");
	}
	retcode = 1;
    }
#else
    if(retcode == 0) {
	if(mpi_rank == 0) {
	    printf(" This is NOT an error, What it really says is\n");
	    printf("special collective IO is WORKING at this platform\n");
	    printf(" Go back to hdf5/config and find the corresponding \n");
	    printf(" configure-specific file (for example, powerpc-ibm-aix5.x) and delete the line\n");
	    printf("hdf5_cv_mpi_special_collective_io_works=${hdf5_cv_mpi_special_collective_io_works='no'}\n");
	    printf(" at the end of the file.\n");
	    printf("Please report to hdfhelp@ncsa.uiuc.edu about this problem.\n");
	}
	retcode = 1;
    }
#endif
    return retcode;
}

/*
 * parse the command line options
 */
static int
parse_options(int argc, char **argv)
{
    while (--argc){
	if (**(++argv) != '-'){
	    break;
	}else{
	    switch(*(*argv+1)){
		case 'v':   if (*((*argv+1)+1))
				ParseTestVerbosity((*argv+1)+1);
			    else
				SetTestVerbosity(VERBO_MED);
			    break;
		case 'f':   if (--argc < 1) {
				nerrors++;
				return(1);
			    }
			    if (**(++argv) == '-') {
				nerrors++;
				return(1);
			    }
			    paraprefix = *argv;
			    break;
		case 'h':   /* print help message--return with nerrors set */
			    return(1);
		default:    nerrors++;
			    return(1);
	    }
	}
    } /*while*/

    /* compose the test filenames */
    {
	int i, n;
	hid_t plist;

	plist = H5Pcreate (H5P_FILE_ACCESS);
	H5Pset_fapl_mpio(plist, MPI_COMM_WORLD, MPI_INFO_NULL);
	n = sizeof(FILENAME)/sizeof(FILENAME[0]) - 1;	/* exclude the NULL */

	for (i=0; i < n; i++)
	    if (h5_fixname(FILENAME[i],plist,filenames[i],sizeof(filenames[i]))
		== NULL){
		printf("h5_fixname failed\n");
		nerrors++;
		return(1);
	    }
	H5Pclose(plist);
	if (VERBOSE_MED){
	    printf("Test filenames are:\n");
	    for (i=0; i < n; i++)
		printf("    %s\n", filenames[i]);
	}
    }

    return(0);
}


/*
 * Show command usage
 */
static void
usage(void)
{
    printf("Usage: t_mpi [-v<verbosity>] [-f <prefix>]\n");
    printf("\t-v<verbosity>\tset verbose level (0-9,l,m,h)\n");
    printf("\t-f <prefix>\tfilename prefix\n");
    printf("\n");
}

/*
 * return the sum of all errors.
 */
static int
errors_sum(int nerrs)
{
    int temp;
    MPI_Allreduce(&nerrs, &temp, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
    return(temp);
}


int
main(int argc, char **argv)
{
    int mpi_size, mpi_rank;				/* mpi variables */
    int ret_code;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    /* Attempt to turn off atexit post processing so that in case errors
     * happen during the test and the process is aborted, it will not get
     * hang in the atexit post processing in which it may try to make MPI
     * calls.  By then, MPI calls may not work.
     */
    if (H5dont_atexit() < 0){
	printf("Failed to turn off atexit processing. Continue.\n", mpi_rank);
    };
    H5open();
    if (parse_options(argc, argv) != 0){
	if (MAINPROCESS)
	    usage();
	goto finish;
    }

    if (MAINPROCESS){
	printf("===================================\n");
	printf("MPI functionality tests\n");
	printf("===================================\n");
    }

    if (VERBOSE_MED)
	h5_show_hostname();

    fapl = H5Pcreate (H5P_FILE_ACCESS);
    H5Pset_fapl_mpio(fapl, MPI_COMM_WORLD, MPI_INFO_NULL);

    /* set alarm. */
    ALARM_ON;


    /*=======================================
     * MPIO 1 write Many read test
     *=======================================*/
    MPI_BANNER("MPIO 1 write Many read test...");
    ret_code = test_mpio_1wMr(filenames[0], USENONE);
    ret_code = errors_sum(ret_code);
    if (mpi_rank==0 && ret_code > 0){
	printf("***FAILED with %d total errors\n", ret_code);
	nerrors += ret_code;
    }

    /* test atomicity and file sync in high verbose mode only         */
    /* since they often hang when broken and PHDF5 does not use them. */
    if (VERBOSE_HI){
	MPI_BANNER("MPIO 1 write Many read test with atomicity...");
	ret_code = test_mpio_1wMr(filenames[0], USEATOM);
	ret_code = errors_sum(ret_code);
	if (mpi_rank==0 && ret_code > 0){
	    printf("***FAILED with %d total errors\n", ret_code);
	    nerrors += ret_code;
	}

	MPI_BANNER("MPIO 1 write Many read test with file sync...");
	ret_code = test_mpio_1wMr(filenames[0], USEFSYNC);
	ret_code = errors_sum(ret_code);
	if (mpi_rank==0 && ret_code > 0){
	    printf("***FAILED with %d total errors\n", ret_code);
	    nerrors += ret_code;
	}
    }


    /*=======================================
     * MPIO MPIO File size range test
     *=======================================*/
    MPI_BANNER("MPIO File size range test...");
    ret_code = test_mpio_gb_file(filenames[0]);
    ret_code = errors_sum(ret_code);
    if (mpi_rank==0 && ret_code > 0){
	printf("***FAILED with %d total errors\n", ret_code);
	nerrors += ret_code;
    }


    /*=======================================
     * MPIO independent overlapping writes
     *=======================================*/
    MPI_BANNER("MPIO independent overlapping writes...");
    ret_code = test_mpio_overlap_writes(filenames[0]);
    ret_code = errors_sum(ret_code);
    if (mpi_rank==0 && ret_code > 0){
	printf("***FAILED with %d total errors\n", ret_code);
	nerrors += ret_code;
    }

    /*=======================================
     * MPIO complicated derived datatype test
     *=======================================*/
    /* test_mpio_derived_dtype often hangs when fails.
     * Do not run it if it is known NOT working unless ask to
     * run explicitly by high verbose mode.
     */
#ifdef H5_MPI_COMPLEX_DERIVED_DATATYPE_WORKS
    MPI_BANNER("MPIO complicated derived datatype test...");
    ret_code = test_mpio_derived_dtype(filenames[0]);
#else
    if (VERBOSE_HI){
	MPI_BANNER("MPIO complicated derived datatype test...");
	ret_code = test_mpio_derived_dtype(filenames[0]);
    }else{
	MPI_BANNER("MPIO complicated derived datatype test SKIPPED.");
	ret_code = 0;	/* fake ret_code */
    }
#endif
    ret_code = errors_sum(ret_code);
    if (mpi_rank==0 && ret_code > 0){
	printf("***FAILED with %d total errors\n", ret_code);
	nerrors += ret_code;
    }

    /*=======================================
     * MPIO special collective IO  test
     *=======================================*/
    /* test_special_collective_io  often hangs when fails.
     * Do not run it if it is known NOT working unless ask to
     * run explicitly by high verbose mode.
     */
    if(mpi_size !=4){
      MPI_BANNER("MPIO special collective io test SKIPPED.");
      if(mpi_rank == 0){
        printf("Use FOUR processes to run this test\n");
        printf("If you still see the <test SKIPPED>, use <-vh> option to verify the test\n");
  }
      ret_code = 0;
      goto sc_finish;
    }

#ifdef H5_MPI_SPECIAL_COLLECTIVE_IO_WORKS
    MPI_BANNER("MPIO special collective io test...");
    ret_code = test_mpio_special_collective(filenames[0]);

#else
    if (VERBOSE_HI){
	MPI_BANNER("MPIO special collective io test...");
	ret_code = test_mpio_special_collective(filenames[0]);
    }else{
	MPI_BANNER("MPIO special collective io test SKIPPED.");
	ret_code = 0;	/* fake ret_code */
    }
#endif

sc_finish:
    ret_code = errors_sum(ret_code);
    if (mpi_rank==0 && ret_code > 0){
	printf("***FAILED with %d total errors\n", ret_code);
	nerrors += ret_code;
    }


finish:
    /* make sure all processes are finished before final report, cleanup
     * and exit.
     */
    MPI_Barrier(MPI_COMM_WORLD);
    if (MAINPROCESS){		/* only process 0 reports */
	printf("===================================\n");
	if (nerrors){
	    printf("***MPI tests detected %d errors***\n", nerrors);
	}
	else{
	    printf("MPI tests finished with no errors\n");
	}
	printf("===================================\n");
    }

    /* turn off alarm */
    ALARM_OFF;

    h5_cleanup(FILENAME, fapl);
    H5close();

    /* MPI_Finalize must be called AFTER H5close which may use MPI calls */
    MPI_Finalize();

    /* cannot just return (nerrors) because exit code is limited to 1byte */
    return(nerrors!=0);
}

