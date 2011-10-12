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

#ifndef H5DIFFCOMMON_H__
#define H5DIFFCOMMON_H__

#include "h5tools.h"

#ifdef __cplusplus
extern "C" {
#endif

H5TOOLS_DLLVAR unsigned char g_Parallel;
H5TOOLS_DLLVAR int g_nTasks;

void usage(void);
void parse_command_line(int argc, const char* argv[], const char** fname1, const char** fname2, const char** objname1, const char** objname2, diff_opt_t* options);
void h5diff_exit(int status);
void print_info(diff_opt_t* options);

#ifdef __cplusplus
}
#endif

#endif /* H5DIFFCOMMON_H__ */
