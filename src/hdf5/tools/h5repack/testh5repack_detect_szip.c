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

#include <stdio.h>
#include "h5repack.h"
#include "h5tools.h"
#include "h5test.h"


/* Name of tool */
#define PROGRAMNAME "h5repack_detect_szip"

/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose: detects szip encoder, prints "yes" or "no" to stdout.
 *          Intended to be used in test scripts.
 *
 * Return:
 *
 * Programmer:
 *
 * Date:
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */


int main(void)
{
    h5tools_setprogname(PROGRAMNAME);
    h5tools_setstatus(EXIT_SUCCESS);

#ifdef H5_HAVE_FILTER_SZIP
    if (h5tools_can_encode(H5Z_FILTER_SZIP) == 1) {
        printf("yes\n");
        return(1);
    }
#endif /* H5_HAVE_FILTER_SZIP */
    printf("no\n");
    return(0);
}
