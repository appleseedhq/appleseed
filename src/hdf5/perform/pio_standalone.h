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

#ifndef PIO_STANDALONE_H__
#define PIO_PERF_H__

/* Header file for building h5perf by standalone mode.
 * Created: Christian Chilan, 2005/5/18.
 */

/** From H5private.h **/

#include "H5public.h"		/* Include Public Definitions		*/


/*
 * Include ANSI-C header files.
 */
#ifdef H5_STDC_HEADERS
#   include <assert.h>
#   include <ctype.h>
#   include <errno.h>
#   include <fcntl.h>
#   include <float.h>
#   include <limits.h>
#   include <math.h>
#   include <signal.h>
#   include <stdarg.h>
#   include <stdio.h>
#   include <stdlib.h>
#   include <string.h>
#endif

/*
 * And now for a couple non-Posix functions...  Watch out for systems that
 * define these in terms of macros.
 */
#ifdef _WIN32
#define HDstrdup(S)		   _strdup(S)
#else /* _WIN32 */

#if !defined strdup && !defined H5_HAVE_STRDUP
extern char *strdup(const char *s);
#endif

#define HDstrdup(S)		  strdup(S)

#endif /* _WIN32 */

H5_DLL int HDfprintf (FILE *stream, const char *fmt, ...);
#define HDstrcmp(S,T)		strcmp(S,T)
#define HDstrlen(S)		strlen(S)
#define HDstrncmp(S,T,L)	strncmp(S,T,L)
#define HDstrncpy(X,Y,Z)	strncpy(X,Y,Z)
#define HDstrchr(S,C)		strchr(S,C)
#define HDfree(M)		free(M)


#ifdef _O_BINARY
#define HDopen(S,F,M)		open(S,F|_O_BINARY,M)
#else
#define HDopen(S,F,M)		open(S,F,M)
#endif
#define HDclose(F)		close(F)

#ifdef _WIN32
#define HDlseek(F,O,W)  	_lseeki64(F,O,W)
#else
#define HDlseek(F,O,W)		lseek(F,O,W)
#endif

#define HDwrite(F,M,Z)		write(F,M,Z)

#define HDread(F,M,Z)		read(F,M,Z)

#ifdef _WIN32
     #define HDstat(S,B)	_stati64(S,B)
#else
#define HDstat(S,B)  stat(S,B)
#endif

#ifdef _WIN32
#define HDfstat(F,B)		_fstati64(F,B)
typedef struct _stati64		h5_stat_t;
typedef __int64            	h5_stat_size_t;
#else
#define HDfstat(F,B)            fstat(F,B)
typedef struct stat		h5_stat_t;
typedef off_t                   h5_stat_size_t;
#endif

/*
 * HDF Boolean type.
 */
#ifndef FALSE
#   define FALSE 0
#endif
#ifndef TRUE
#   define TRUE 1
#endif


/** From h5test.h **/

#ifdef H5_HAVE_PARALLEL
extern MPI_Info h5_io_info_g;         /* MPI INFO object for IO */
#endif

#ifdef H5_HAVE_PARALLEL
H5TEST_DLL int h5_set_info_object(void);
H5TEST_DLL void h5_dump_info_object(MPI_Info info);
#endif



/** From h5tools_utils.h **/

extern int         opt_err;     /* getoption prints errors if this is on    */
extern int         opt_ind;     /* token pointer                            */
extern const char *opt_arg;     /* flag argument (or value)                 */


enum {
    no_arg = 0,         /* doesn't take an argument     */
    require_arg,        /* requires an argument	        */
    optional_arg        /* argument is optional         */
};


typedef struct long_options {
    const char  *name;          /* name of the long option              */
    int          has_arg;       /* whether we should look for an arg    */
    char         shortval;      /* the shortname equivalent of long arg
                                 * this gets returned from get_option   */
} long_options;

extern int    get_option(int argc, const char **argv, const char *opt,
                         const struct long_options *l_opt);

extern int     nCols;               /*max number of columns for outputting  */

/* Definitions of useful routines */
extern void     print_version(const char *progname);

#endif
