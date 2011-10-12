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
 * This header file contains information required for testing the HDF5 library.
 */

#ifndef TTSAFE_H
#define TTSAFE_H

#include <string.h>

/*
 * Include required headers.  This file tests internal library functions,
 * so we include the private headers here.
 */
#include "hdf5.h"
#include "H5private.h"
#include "H5Eprivate.h"
#include "testhdf5.h"

#ifdef H5_HAVE_THREADSAFE
/* Include pthread library for threadsafe tests */
#ifdef H5_HAVE_PTHREAD_H
#include <pthread.h>
#endif /* H5_HAVE_PTHREAD_H */

/* Prototypes for the support routines */
extern char*            gen_name(int);

/* Prototypes for the test routines */
void                    tts_dcreate(void);
void                    tts_error(void);
void                    tts_cancel(void);
void                    tts_acreate(void);

/* Prototypes for the cleanup routines */
void                    cleanup_dcreate(void);
void                    cleanup_error(void);
void                    cleanup_cancel(void);
void                    cleanup_acreate(void);

#endif /* H5_HAVE_THREADSAFE */
#endif /* TTSAFE_H */
