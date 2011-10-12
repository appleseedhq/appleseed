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

#ifndef PIO_TIMER__
#define PIO_TIMER__

#include "hdf5.h"

#if defined(H5_TIME_WITH_SYS_TIME)
#   include <sys/time.h>
#   include <time.h>
#elif defined(H5_HAVE_SYS_TIME_H)
#   include <sys/time.h>
#else
#   include <time.h>
#endif

/* The different types of timers we can have */
typedef enum timer_type_ {
    HDF5_FILE_OPENCLOSE,
    HDF5_DATASET_CREATE,
    HDF5_MPI_WRITE,
    HDF5_MPI_READ,
    HDF5_FILE_READ_OPEN,
    HDF5_FILE_READ_CLOSE,
    HDF5_FILE_WRITE_OPEN,
    HDF5_FILE_WRITE_CLOSE,
    HDF5_FINE_WRITE_FIXED_DIMS,
    HDF5_FINE_READ_FIXED_DIMS,
    HDF5_GROSS_WRITE_FIXED_DIMS,
    HDF5_GROSS_READ_FIXED_DIMS,
    HDF5_RAW_WRITE_FIXED_DIMS,
    HDF5_RAW_READ_FIXED_DIMS,
    NUM_TIMERS
} timer_type;

typedef enum clock_type_ {
    MPI_TIMER = 0,  /* Use MPI timer to measure time        */
    SYS_TIMER = 1   /* Use system clock to measure time     */
} clock_type;

/* Miscellaneous identifiers */
enum {
    START,          /* Start a specified timer              */
    STOP            /* Stop a specified timer               */
};

/* The performance time structure */
typedef struct pio_time_ {
    clock_type type;
    double total_time[NUM_TIMERS];
    double mpi_timer[NUM_TIMERS];
    struct timeval sys_timer[NUM_TIMERS];
} pio_time;

/* External function declarations */
#ifdef __cplusplus
extern "C" {
#endif  /* __cplusplus */
extern pio_time    *pio_time_new(clock_type t);
extern void         pio_time_destroy(pio_time *pt);
extern void         set_timer_type(pio_time *pt, clock_type type);
extern clock_type   get_timer_type(pio_time *pt);
extern pio_time    *set_time(pio_time *pt, timer_type t, int start_stop);
extern double       get_time(pio_time *pt, timer_type t);
#ifdef __cplusplus
}
#endif  /* __cplusplus */

#endif  /* PIO_TIMER__ */
