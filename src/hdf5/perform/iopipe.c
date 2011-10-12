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
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Thursday, March 12, 1998
 */

/* See H5private.h for how to include headers */
#include "hdf5.h"

#include "H5private.h"

#ifdef H5_HAVE_SYS_TIMEB
#include <sys/timeb.h>
#endif


#define RAW_FILE_NAME	"iopipe.raw"
#define HDF5_FILE_NAME	"iopipe.h5"
#define HEADING		"%-16s"
#define PROGRESS	'='

#if 0
/* Normal testing */
#define REQUEST_SIZE_X	4579
#define REQUEST_SIZE_Y	4579
#define NREAD_REQUESTS	45
#define NWRITE_REQUESTS	45
#else
/* Speedy testing */
#define REQUEST_SIZE_X	1000
#define REQUEST_SIZE_Y	1000
#define NREAD_REQUESTS	45
#define NWRITE_REQUESTS	45
#endif


/*-------------------------------------------------------------------------
 * Function:	print_stats
 *
 * Purpose:	Prints statistics
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Thursday, March 12, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_GETRUSAGE
static void
print_stats (const char *prefix,
	     struct rusage *r_start, struct rusage *r_stop,
	     struct timeval *t_start, struct timeval *t_stop,
	     size_t nbytes)
#else /* H5_HAVE_GETRUSAGE */
static void
print_stats (const char *prefix,
	     struct timeval *r_start, struct timeval *r_stop,
	     struct timeval *t_start, struct timeval *t_stop,
	     size_t nbytes)
#endif /* H5_HAVE_GETRUSAGE */
{
    double	e_time, bw;
#ifdef H5_HAVE_GETRUSAGE
    double	u_time, s_time;

    u_time = ((double)(r_stop->ru_utime.tv_sec)+
	      (double)(r_stop->ru_utime.tv_usec)/1000000.0) -
	     ((double)(r_start->ru_utime.tv_sec)+
	      (double)(r_start->ru_utime.tv_usec)/1000000.0);

    s_time = ((double)(r_stop->ru_stime.tv_sec)+
	      (double)(r_stop->ru_stime.tv_usec)/1000000.0) -
	     ((double)(r_start->ru_stime.tv_sec)+
	      (double)(r_start->ru_stime.tv_usec)/1000000.0);
#endif
#ifndef H5_HAVE_SYS_TIMEB
    e_time = ((double)(t_stop->tv_sec)+
	      (double)(t_stop->tv_usec)/1000000.0) -
	     ((double)(t_start->tv_sec)+
	      (double)(t_start->tv_usec)/1000000.0);
#else
    e_time = ((double)(t_stop->tv_sec)+
	      (double)(t_stop->tv_usec)/1000.0) -
	     ((double)(t_start->tv_sec)+
	      (double)(t_start->tv_usec)/1000.0);
#endif
    bw = (double)nbytes / e_time;

#ifdef H5_HAVE_GETRUSAGE
    printf (HEADING "%1.2fuser %1.2fsystem %1.2felapsed %1.2fMB/s\n",
	    prefix, u_time, s_time, e_time, bw/(1024*1024));
#else
    printf (HEADING "%1.2felapsed %1.2fMB/s\n",
	    prefix, e_time, bw/(1024*1024));
#endif

}


/*-------------------------------------------------------------------------
 * Function:	synchronize
 *
 * Purpose:
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Thursday, March 12, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
synchronize (void)
{
#ifdef H5_HAVE_SYSTEM
#if defined(_WIN32) && ! defined(__CYGWIN__)
    _flushall();
#else
    HDsystem("sync");
    HDsystem("df >/dev/null");
#endif
#endif
}


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:
 *
 * Return:	Success:
 *
 *		Failure:
 *
 * Programmer:	Robb Matzke
 *              Thursday, March 12, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main (void)
{
    static hsize_t	size[2] = {REQUEST_SIZE_X, REQUEST_SIZE_Y};
    static unsigned	nread = NREAD_REQUESTS, nwrite = NWRITE_REQUESTS;

    unsigned char	*the_data = NULL;
    hid_t		file, dset, file_space = -1;
    herr_t		status;
#ifdef H5_HAVE_GETRUSAGE
    struct rusage	r_start, r_stop;
#else
    struct timeval r_start, r_stop;
#endif
    struct timeval	t_start, t_stop;
    int			fd;
    unsigned		u;
    hssize_t		n;
    off_t		offset;
    hsize_t		start[2];
    hsize_t		count[2];


#ifdef H5_HAVE_SYS_TIMEB
	struct _timeb *tbstart = malloc(sizeof(struct _timeb));
	struct _timeb *tbstop = malloc(sizeof(struct _timeb));
#endif
    /*
     * The extra cast in the following statement is a bug workaround for the
     * Win32 version 5.0 compiler.
     * 1998-11-06 ptl
     */
    printf ("I/O request size is %1.1fMB\n",
	    (double)(hssize_t)(size[0]*size[1])/1024.0*1024);

    /* Open the files */
    file = H5Fcreate (HDF5_FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    assert (file>=0);
    fd = HDopen (RAW_FILE_NAME, O_RDWR|O_CREAT|O_TRUNC, 0666);
    assert (fd>=0);

    /* Create the dataset */
    file_space = H5Screate_simple (2, size, size);
    assert(file_space >= 0);
    dset = H5Dcreate2(file, "dset", H5T_NATIVE_UCHAR, file_space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    assert(dset >= 0);
    the_data = (unsigned char *)malloc((size_t)(size[0] * size[1]));

    /* initial fill for lazy malloc */
    HDmemset(the_data, 0xAA, (size_t)(size[0] * size[1]));

    /* Fill raw */
    synchronize ();
#ifdef H5_HAVE_GETRUSAGE
    HDgetrusage(RUSAGE_SELF, &r_start);
#endif
#ifdef H5_HAVE_GETTIMEOFDAY
    HDgettimeofday(&t_start, NULL);
#else
#ifdef H5_HAVE_SYS_TIMEB
	_ftime(tbstart);
#endif
#endif
    fprintf (stderr, HEADING, "fill raw");
    for(u = 0; u < nwrite; u++) {
	putc (PROGRESS, stderr);
	HDfflush(stderr);
	HDmemset(the_data, 0xAA, (size_t)(size[0]*size[1]));
    }
#ifdef H5_HAVE_GETRUSAGE
    HDgetrusage(RUSAGE_SELF, &r_stop);
#endif
#ifdef H5_HAVE_GETTIMEOFDAY
    HDgettimeofday(&t_stop, NULL);
#else
#ifdef H5_HAVE_SYS_TIMEB
	_ftime(tbstop);
	t_start.tv_sec = tbstart->time;
	t_start.tv_usec = tbstart->millitm;
	t_stop.tv_sec = tbstop->time;
	t_stop.tv_usec = tbstop->millitm;
#endif
#endif
    putc ('\n', stderr);
    print_stats ("fill raw",
		 &r_start, &r_stop, &t_start, &t_stop,
		 (size_t)(nread*size[0]*size[1]));


    /* Fill hdf5 */
    synchronize ();
#ifdef H5_HAVE_GETRUSAGE
    HDgetrusage(RUSAGE_SELF, &r_start);
#endif
#ifdef H5_HAVE_GETTIMEOFDAY
    HDgettimeofday(&t_start, NULL);
#else
#ifdef H5_HAVE_SYS_TIMEB
	_ftime(tbstart);
#endif
#endif
    fprintf (stderr, HEADING, "fill hdf5");
    for(u = 0; u < nread; u++) {
	putc (PROGRESS, stderr);
	HDfflush(stderr);
	status = H5Dread (dset, H5T_NATIVE_UCHAR, file_space, file_space,
			  H5P_DEFAULT, the_data);
	assert (status>=0);
    }
#ifdef H5_HAVE_GETRUSAGE
    HDgetrusage(RUSAGE_SELF, &r_stop);
#endif
#ifdef H5_HAVE_GETTIMEOFDAY
    HDgettimeofday(&t_stop, NULL);
#else
#ifdef H5_HAVE_SYS_TIMEB
	_ftime(tbstop);
	t_start.tv_sec = tbstart->time;
	t_start.tv_usec = tbstart->millitm;
	t_stop.tv_sec = tbstop->time;
	t_stop.tv_usec = tbstop->millitm;
#endif
#endif
    putc ('\n', stderr);
    print_stats ("fill hdf5",
		 &r_start, &r_stop, &t_start, &t_stop,
		 (size_t)(nread*size[0]*size[1]));

    /* Write the raw dataset */
    synchronize ();
#ifdef H5_HAVE_GETRUSAGE
    HDgetrusage(RUSAGE_SELF, &r_start);
#endif
#ifdef H5_HAVE_GETTIMEOFDAY
    HDgettimeofday(&t_start, NULL);
#else
#ifdef H5_HAVE_SYS_TIMEB
	_ftime(tbstart);
#endif
#endif
    fprintf (stderr, HEADING, "out raw");
    for(u = 0; u < nwrite; u++) {
	putc (PROGRESS, stderr);
	HDfflush(stderr);
	offset = HDlseek (fd, (off_t)0, SEEK_SET);
	assert (0==offset);
	n = HDwrite (fd, the_data, (size_t)(size[0]*size[1]));
	assert (n>=0 && (size_t)n==size[0]*size[1]);
    }
#ifdef H5_HAVE_GETRUSAGE
    HDgetrusage(RUSAGE_SELF, &r_stop);
#endif
#ifdef H5_HAVE_GETTIMEOFDAY
    HDgettimeofday(&t_stop, NULL);
#else
#ifdef H5_HAVE_SYS_TIMEB
	_ftime(tbstop);
	t_start.tv_sec = tbstart->time;
	t_start.tv_usec = tbstart->millitm;
	t_stop.tv_sec = tbstop->time;
	t_stop.tv_usec = tbstop->millitm;
#endif
#endif
    putc ('\n', stderr);
    print_stats ("out raw",
		 &r_start, &r_stop, &t_start, &t_stop,
		 (size_t)(nread*size[0]*size[1]));

    /* Write the hdf5 dataset */
    synchronize ();
#ifdef H5_HAVE_GETRUSAGE
    HDgetrusage(RUSAGE_SELF, &r_start);
#endif
#ifdef H5_HAVE_GETTIMEOFDAY
    HDgettimeofday(&t_start, NULL);
#else
#ifdef H5_HAVE_SYS_TIMEB
	_ftime(tbstart);
#endif
#endif
    fprintf (stderr, HEADING, "out hdf5");
    for(u = 0; u < nwrite; u++) {
	putc (PROGRESS, stderr);
	HDfflush(stderr);
	status = H5Dwrite (dset, H5T_NATIVE_UCHAR, H5S_ALL, H5S_ALL,
			   H5P_DEFAULT, the_data);
	assert (status>=0);
    }
#ifdef H5_HAVE_GETRUSAGE
    HDgetrusage(RUSAGE_SELF, &r_stop);
#endif
#ifdef H5_HAVE_GETTIMEOFDAY
    HDgettimeofday(&t_stop, NULL);
#else
#ifdef H5_HAVE_SYS_TIMEB
	_ftime(tbstop);
	t_start.tv_sec = tbstart->time;
	t_start.tv_usec = tbstart->millitm;
	t_stop.tv_sec = tbstop->time;
	t_stop.tv_usec = tbstop->millitm;
#endif
#endif
    putc ('\n', stderr);
    print_stats ("out hdf5",
		 &r_start, &r_stop, &t_start, &t_stop,
		 (size_t)(nread*size[0]*size[1]));

    /* Read the raw dataset */
    synchronize ();
#ifdef H5_HAVE_GETRUSAGE
    HDgetrusage(RUSAGE_SELF, &r_start);
#endif
#ifdef H5_HAVE_GETTIMEOFDAY
    HDgettimeofday(&t_start, NULL);
#else
#ifdef H5_HAVE_SYS_TIMEB
	_ftime(tbstart);
#endif
#endif
    fprintf (stderr, HEADING, "in raw");
    for(u = 0; u < nread; u++) {
	putc (PROGRESS, stderr);
	HDfflush(stderr);
	offset = HDlseek (fd, (off_t)0, SEEK_SET);
	assert (0==offset);
	n = HDread (fd, the_data, (size_t)(size[0]*size[1]));
	assert (n>=0 && (size_t)n==size[0]*size[1]);
    }
#ifdef H5_HAVE_GETRUSAGE
    HDgetrusage(RUSAGE_SELF, &r_stop);
#endif
#ifdef H5_HAVE_GETTIMEOFDAY
    HDgettimeofday(&t_stop, NULL);
#else
#ifdef H5_HAVE_SYS_TIMEB
	_ftime(tbstop);
	t_start.tv_sec = tbstart->time;
	t_start.tv_usec = tbstart->millitm;
	t_stop.tv_sec = tbstop->time;
	t_stop.tv_usec = tbstop->millitm;
#endif
#endif
    putc ('\n', stderr);
    print_stats ("in raw",
		 &r_start, &r_stop, &t_start, &t_stop,
		 (size_t)(nread*size[0]*size[1]));


    /* Read the hdf5 dataset */
    synchronize ();
#ifdef H5_HAVE_GETRUSAGE
    HDgetrusage(RUSAGE_SELF, &r_start);
#endif
#ifdef H5_HAVE_GETTIMEOFDAY
    HDgettimeofday(&t_start, NULL);
#else
#ifdef H5_HAVE_SYS_TIMEB
	_ftime(tbstart);
#endif
#endif
    fprintf (stderr, HEADING, "in hdf5");
    for(u = 0; u < nread; u++) {
	putc (PROGRESS, stderr);
	HDfflush(stderr);
	status = H5Dread (dset, H5T_NATIVE_UCHAR, file_space, file_space,
			  H5P_DEFAULT, the_data);
	assert (status>=0);
    }
#ifdef H5_HAVE_GETRUSAGE
    HDgetrusage(RUSAGE_SELF, &r_stop);
#endif
#ifdef H5_HAVE_GETTIMEOFDAY
    HDgettimeofday(&t_stop, NULL);
#else
#ifdef H5_HAVE_SYS_TIMEB
	_ftime(tbstop);
	t_start.tv_sec = tbstart->time;
	t_start.tv_usec = tbstart->millitm;
	t_stop.tv_sec = tbstop->time;
	t_stop.tv_usec = tbstop->millitm;
#endif
#endif
    putc ('\n', stderr);
    print_stats ("in hdf5",
		 &r_start, &r_stop, &t_start, &t_stop,
		 (size_t)(nread*size[0]*size[1]));

    /* Read hyperslab */
    assert (size[0]>20 && size[1]>20);
    start[0] = start[1] = 10;
    count[0] = count[1] = size[0]-20;
    status = H5Sselect_hyperslab (file_space, H5S_SELECT_SET, start, NULL, count, NULL);
    assert (status>=0);
    synchronize ();
#ifdef H5_HAVE_GETRUSAGE
    HDgetrusage(RUSAGE_SELF, &r_start);
#endif
#ifdef H5_HAVE_GETTIMEOFDAY
    HDgettimeofday(&t_start, NULL);
#else
#ifdef H5_HAVE_SYS_TIMEB
	_ftime(tbstart);
#endif
#endif
    fprintf (stderr, HEADING, "in hdf5 partial");
    for(u = 0; u < nread; u++) {
	putc (PROGRESS, stderr);
	HDfflush(stderr);
	status = H5Dread (dset, H5T_NATIVE_UCHAR, file_space, file_space,
			  H5P_DEFAULT, the_data);
	assert (status>=0);
    }
#ifdef H5_HAVE_GETRUSAGE
    HDgetrusage(RUSAGE_SELF, &r_stop);
#endif
#ifdef H5_HAVE_GETTIMEOFDAY
    HDgettimeofday(&t_stop, NULL);
#else
#ifdef H5_HAVE_SYS_TIMEB
	_ftime(tbstop);
	t_start.tv_sec = tbstart->time;
	t_start.tv_usec = tbstart->millitm;
	t_stop.tv_sec = tbstop->time;
	t_stop.tv_usec = tbstop->millitm;
#endif
#endif
    putc('\n', stderr);
    print_stats("in hdf5 partial",
		 &r_start, &r_stop, &t_start, &t_stop,
		 (size_t)(nread*count[0]*count[1]));



    /* Close everything */
    HDclose(fd);
    H5Dclose(dset);
    H5Sclose(file_space);
    H5Fclose(file);
    free(the_data);

    return 0;
}

