/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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

#ifndef SIO_PERF_H__
#define SIO_PERF_H__

#include "sio_timer.h"
#ifndef STANDALONE
#include "H5private.h"
#include "h5test.h"
#include "h5tools_utils.h"
#else
#include "sio_standalone.h"
#endif

/* setup the dataset no fill option if this is v1.5 or more */
#if H5_VERS_MAJOR > 1 || H5_VERS_MINOR > 4
#define H5_HAVE_NOFILL 1
#endif

#define MAX_DIMS 32

typedef enum iotype_ {
    POSIXIO,
    HDF5
    /*NUM_TYPES*/
} iotype;

typedef enum vfdtype_ {
    sec2,
    stdio,
    core,
    split,
    multi,
    family,
    direct
    /*NUM_TYPES*/
} vfdtype;

typedef struct parameters_ {
    iotype	io_type;        /* The type of IO test to perform       */
    vfdtype     vfd;
    long	num_files;      /* Number of files to create            */
    long	num_dsets;      /* Number of datasets to create         */
    off_t	num_bytes;      /* Number of bytes in each dset         */
    int         num_iters;      /* Number of times to loop doing the IO */
    int         rank;           /* Rank of dataset */
    off_t 	dset_size[MAX_DIMS]; /* Dataset size             */
    size_t 	buf_size[MAX_DIMS]; /* Buffer size               */
    size_t 	chk_size[MAX_DIMS]; /* Chunk size               */
    int  	order[MAX_DIMS]; /* Buffer size               */
    hsize_t 	h5_align;       /* HDF5 object alignment                */
    hsize_t 	h5_thresh;      /* HDF5 object alignment threshold      */
    int 	h5_use_chunks;  /* Make HDF5 dataset chunked            */
    int 	h5_extendable;  /* Make HDF5 dataset chunked            */
    int    	h5_write_only;  /* Perform the write tests only         */
    unsigned    h5_use_mpi_posix;   /* VFD for HDF5 I/O  */
    int 	verify;    	/* Verify data correctness              */
} parameters;

typedef struct results_ {
    herr_t      ret_code;
    sio_time   *timers;
} results;

#ifndef SUCCESS
#define SUCCESS     0
#endif  /* !SUCCESS */

#ifndef FAIL
#define FAIL        -1
#endif  /* !FAIL */

extern FILE     *output;            /* output file                          */
extern sio_time *timer_g;           /* timer: global for stub functions     */
extern int      sio_debug_level;    /* The debug level:
                                     *   0 - Off
                                     *   1 - Minimal
                                     *   2 - Some more
                                     *   3 - Maximal
                                     *   4 - Even More Debugging (timer stuff)
                                     */
#ifdef __cplusplus
extern "C" {
#endif  /* __cplusplus */

extern results do_sio(parameters param);

#ifdef __cplusplus
}
#endif  /* __cplusplus */

#endif  /* PIO_PERF_H__ */
