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
 * Programmer:  Quincey Koziol <koziol@hdfgroup.org>
 *              Wednesday, March 17, 2010
 *
 * Purpose:     srcdir querying support.
 */
#ifndef _H5SRCDIR_H
#define _H5SRCDIR_H

/* Include the header file with the correct relative path for the srcdir string */
#include "H5srcdir_str.h"

/* Buffer to construct path in and return pointer to */
static char srcdir_path[1024] = "";

/* Buffer to construct file in and return pointer to */
static char srcdir_testpath[1024] = "";

/* Append the test file name to the srcdir path and return the whole string */
#ifdef H5_VMS
static const char *H5_get_srcdir_filename(char *filename)
#else
static const char *H5_get_srcdir_filename(const char *filename)
#endif
{
    const char *srcdir = HDgetenv("srcdir");

    /* Check for using the srcdir from configure time */
    if(NULL == srcdir)
        srcdir = config_srcdir;

    /* Build path to test file */
    if((HDstrlen(srcdir) + HDstrlen(filename) + 2) < sizeof(srcdir_testpath)) {
        HDstrcpy(srcdir_testpath, srcdir);
#ifdef H5_VMS
        if(filename[0] == '[') {
            char *tmp = filename;
            srcdir_testpath[strlen(srcdir)-1] = '\0';
            strcat(srcdir_testpath, ++tmp);
        } else
            strcat(srcdir_testpath, filename);
#else
        HDstrcat(srcdir_testpath, "/");
        HDstrcat(srcdir_testpath, filename);
#endif
        return(srcdir_testpath);
    } /* end if */
    else
        return(NULL);
}

/* Just return the srcdir path */
static const char *H5_get_srcdir(void)
{
    const char *srcdir = HDgetenv("srcdir");

    /* Check for using the srcdir from configure time */
    if(NULL == srcdir)
        srcdir = config_srcdir;

    /* Build path to all test files */
    if((HDstrlen(srcdir) + 2) < sizeof(srcdir_path)) {
        HDstrcpy(srcdir_path, srcdir);
        HDstrcat(srcdir_path, "/");
        return(srcdir_path);
    } /* end if */
    else
        return(NULL);
}
#endif /* _H5SRCDIR_H */


