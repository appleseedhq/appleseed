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
 * Purpose:
 *
 * This is a module of useful timing functions for performance testing.
 */

#include <stdio.h>
#include <stdlib.h>

#include "pio_timer.h"

#ifdef H5_HAVE_PARALLEL

#include <mpi.h>

#include "pio_perf.h"

/*
 * The number to divide the tv_usec field with to get a nice decimal to add to
 * the number of seconds.
 */
#define MICROSECOND     1000000.0

/* global variables */
pio_time   *timer_g;            /* timer: global for stub functions     */

/*
 * Function:  sub_time
 * Purpose:   Struct two time values, and return the difference, in microseconds
 *
 * 	      Note that the function assumes that a > b
 * Programmer: Leon Arber, 1/27/06
 */
static double sub_time(struct timeval* a, struct timeval* b)
{
    return (((double)a->tv_sec +
     ((double)a->tv_usec) / MICROSECOND) -
	((double)b->tv_sec +
	 ((double)b->tv_usec) / MICROSECOND));
}


/*
 * Function:    pio_time_new
 * Purpose:     Build us a brand, spankin', new performance time object.
 *              The object is a black box to the user. They just tell us
 *              what type of timer they want (MPI_TIMER for MPI_Wtime or
 *              SYS_TIMER for system time).
 * Return:      Pointer to pio_time object
 * Programmer:  Bill Wendling, 01. October 2001
 * Modifications:
 */
pio_time *
pio_time_new(clock_type type)
{
    pio_time *pt = (pio_time *)calloc(1, sizeof(struct pio_time_));

    /* set global timer variable */
    timer_g = pt;

    pt->type = type;
    return pt;
}

/*
 * Function:    pio_time_destroy
 * Purpose:     Remove the memory allocated for the pio_time object. Only
 *              need to call on a pointer allocated with the ``pio_time_new''
 *              function.
 * Return:      Nothing
 * Programmer:  Bill Wendling, 01. October 2001
 * Modifications:
 */
void
pio_time_destroy(pio_time *pt)
{
    free(pt);
    /* reset the global timer pointer too. */
    timer_g = NULL;
}

/*
 * Function:    set_timer_type
 * Purpose:     Set the type of the timer to either MPI_TIMER or SYS_TIMER.
 *              This really only needs to be called if you didn't construct a
 *              timer with the pio_timer_new function (shame!).
 * Return:      Nothing
 * Programmer:  Bill Wendling, 04. October 2001
 * Modifications:
 */
void
set_timer_type(pio_time *pt, clock_type type)
{
    pt->type = type;
}

/*
 * Function:    get_timer_type
 * Purpose:     Get the type of the timer.
 * Return:      MPI_TIMER or SYS_TIMER.
 * Programmer:  Bill Wendling, 04. October 2001
 * Modifications:
 */
clock_type
get_timer_type(pio_time *pt)
{
    return pt->type;
}

/*
 * Function:    set_time
 * Purpose:     Set the time in a ``pio_time'' object.
 * Return:      Pointer to the passed in ``pio_time'' object.
 * Programmer:  Bill Wendling, 01. October 2001
 * Modifications:
 */
pio_time *
set_time(pio_time *pt, timer_type t, int start_stop)
{
    if (pt) {
        if (pt->type == MPI_TIMER) {
            if (start_stop == START) {
                pt->mpi_timer[t] = MPI_Wtime();

		/* When we start the timer for HDF5_FINE_WRITE_FIXED_DIMS or HDF5_FINE_READ_FIXED_DIMS
		 * we compute the time it took to only open the file */
		if(t == HDF5_FINE_WRITE_FIXED_DIMS)
		    pt->total_time[HDF5_FILE_WRITE_OPEN] += pt->mpi_timer[t] - pt->mpi_timer[HDF5_GROSS_WRITE_FIXED_DIMS];
		else if(t == HDF5_FINE_READ_FIXED_DIMS)
		    pt->total_time[HDF5_FILE_READ_OPEN] += pt->mpi_timer[t] - pt->mpi_timer[HDF5_GROSS_READ_FIXED_DIMS];

            } else {
                pt->total_time[t] += MPI_Wtime() - pt->mpi_timer[t];
		pt->mpi_timer[t] = MPI_Wtime();

		/* When we stop the timer for HDF5_GROSS_WRITE_FIXED_DIMS or HDF5_GROSS_READ_FIXED_DIMS
		 * we compute the time it took to close the file after the last read/write finished */
		if(t == HDF5_GROSS_WRITE_FIXED_DIMS)
		    pt->total_time[HDF5_FILE_WRITE_CLOSE] += pt->mpi_timer[t] - pt->mpi_timer[HDF5_FINE_WRITE_FIXED_DIMS];
		else if(t == HDF5_GROSS_READ_FIXED_DIMS)
		    pt->total_time[HDF5_FILE_READ_CLOSE] += pt->mpi_timer[t] - pt->mpi_timer[HDF5_FINE_READ_FIXED_DIMS];
            }
        } else {
            if (start_stop == START) {
                gettimeofday(&pt->sys_timer[t], NULL);

		/* When we start the timer for HDF5_FINE_WRITE_FIXED_DIMS or HDF5_FINE_READ_FIXED_DIMS
		 * we compute the time it took to only open the file */
		if(t == HDF5_FINE_WRITE_FIXED_DIMS)
		    pt->total_time[HDF5_FILE_WRITE_OPEN] += sub_time(&(pt->sys_timer[t]), &(pt->sys_timer[HDF5_GROSS_WRITE_FIXED_DIMS]));
		else if(t == HDF5_FINE_READ_FIXED_DIMS)
		    pt->total_time[HDF5_FILE_READ_OPEN] += sub_time(&(pt->sys_timer[t]), &(pt->sys_timer[HDF5_GROSS_READ_FIXED_DIMS]));


            } else {
                struct timeval sys_t;

                gettimeofday(&sys_t, NULL);
                pt->total_time[t] += sub_time(&sys_t, &(pt->sys_timer[t]));

/*                    ((double)sys_t.tv_sec +
                                ((double)sys_t.tv_usec) / MICROSECOND) -
                    ((double)pt->sys_timer[t].tv_sec +
                            ((double)pt->sys_timer[t].tv_usec) / MICROSECOND);*/

		/* When we stop the timer for HDF5_GROSS_WRITE_FIXED_DIMS or HDF5_GROSS_READ_FIXED_DIMS
		 * we compute the time it took to close the file after the last read/write finished */
		if(t == HDF5_GROSS_WRITE_FIXED_DIMS)
		    pt->total_time[HDF5_FILE_WRITE_CLOSE] += sub_time(&(pt->sys_timer[t]), &(pt->sys_timer[HDF5_FINE_WRITE_FIXED_DIMS]));
		else if(t == HDF5_GROSS_READ_FIXED_DIMS)
		    pt->total_time[HDF5_FILE_READ_CLOSE] += sub_time(&(pt->sys_timer[t]), &(pt->sys_timer[HDF5_FINE_READ_FIXED_DIMS]));

            }
        }

        if (pio_debug_level >= 4) {
            const char *msg;
            int myrank;

            MPI_Comm_rank(pio_comm_g, &myrank);

            switch (t) {
            case HDF5_FILE_OPENCLOSE:
                msg = "File Open/Close";
                break;
            case HDF5_DATASET_CREATE:
                msg = "Dataset Create";
                break;
            case HDF5_MPI_WRITE:
                msg = "MPI Write";
                break;
            case HDF5_MPI_READ:
                msg = "MPI Read";
                break;
            case HDF5_FINE_WRITE_FIXED_DIMS:
                msg = "Fine Write";
                break;
            case HDF5_FINE_READ_FIXED_DIMS:
                msg = "Fine Read";
                break;
            case HDF5_GROSS_WRITE_FIXED_DIMS:
                msg = "Gross Write";
                break;
            case HDF5_GROSS_READ_FIXED_DIMS:
                msg = "Gross Read";
                break;
            case HDF5_RAW_WRITE_FIXED_DIMS:
                msg = "Raw Write";
                break;
            case HDF5_RAW_READ_FIXED_DIMS:
                msg = "Raw Read";
                break;
            default:
                msg = "Unknown Timer";
                break;
            }

            fprintf(output, "    Proc %d: %s %s: %.2f\n", myrank, msg,
                    (start_stop == START ? "Start" : "Stop"),
                    pt->total_time[t]);
        }
    }

    return pt;
}

/*
 * Function:    get_time
 * Purpose:     Get the time from a ``pio_time'' object.
 * Return:      The number of seconds as a DOUBLE.
 * Programmer:  Bill Wendling, 01. October 2001
 * Modifications:
 */
double
get_time(pio_time *pt, timer_type t)
{
    return pt->total_time[t];
}

#endif /* H5_HAVE_PARALLEL */
#ifdef STANDALONE
#include "pio_standalone.c"
#endif
