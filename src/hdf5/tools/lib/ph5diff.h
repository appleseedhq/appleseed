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

#ifndef _PH5DIFF_H__
#define _PH5DIFF_H__

/* Send from manager to workers */
#define MPI_TAG_ARGS		1
#define MPI_TAG_PRINT_TOK	2

/*Sent from workers to manager */
#define MPI_TAG_TOK_REQUEST	3
#define MPI_TAG_DONE		4
#define MPI_TAG_TOK_RETURN	5
#define MPI_TAG_PRINT_DATA	6

/* Operational tags used to init and complete diff */
#define MPI_TAG_END		7
#define MPI_TAG_PARALLEL	8

H5TOOLS_DLLVAR int	g_nTasks;
H5TOOLS_DLLVAR unsigned char g_Parallel;
H5TOOLS_DLLVAR char    outBuff[];
H5TOOLS_DLLVAR int	outBuffOffset;
H5TOOLS_DLLVAR FILE *	overflow_file;

struct diff_args
{
    char	name1[256];
    char	name2[256];
    h5trav_type_t   type;
    diff_opt_t	options;
};

struct diffs_found
{
    hsize_t nfound;
    int	    not_cmp;
};

#ifdef H5_HAVE_PARALLEL
#include <mpi.h>
#endif

#endif  /* _PH5DIFF_H__ */

