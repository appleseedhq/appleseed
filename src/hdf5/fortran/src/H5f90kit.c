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


#include <ctype.h>
#include <stddef.h>
#include "H5f90.h"


/*
 * Routines from HDF4 to deal with C-FORTRAN issues.
 *
 * HD5f2cstring   -- convert a Fortran string to a C string
 * HD5packFstring -- convert a C string into a Fortran string
 */

/* ----------------------------- HDf2cstring ------------------------------ */
/*
NAME
   HD5f2cstring -- convert a Fortran string to a C string
USAGE
   char * HDf2cstring(fdesc, len)
   _fcd  fdesc;     IN: Fortran string descriptor
   int  len;       IN: length of Fortran string
RETURNS
   Pointer to the C string if success, else NULL
DESCRIPTION
   Chop off trailing blanks off of a Fortran string and
   move it into a newly allocated C string.  It is up
   to the user to free this string.

---------------------------------------------------------------------------*/
char *
HD5f2cstring(_fcd fdesc, size_t len)
{
    char       *cstr;   /* C string to return */
    char       *str;    /* Pointer to FORTRAN string */
    int         i;      /* Local index variable */

    /* Search for the end of the string */
    str = _fcdtocp(fdesc);
    for(i = (int)len - 1; i >= 0 && !HDisgraph((int)str[i]); i--)
        /*EMPTY*/;

    /* Allocate C string */
    if(NULL == (cstr = (char *)HDmalloc((size_t)(i + 2))))
        return NULL;

    /* Copy text from FORTRAN to C string */
    HDmemcpy(cstr, str, (size_t)(i + 1));

    /* Terminate C string */
    cstr[i + 1] = '\0';

    return cstr;
}   /* HD5f2cstring */

/* ---------------------------- HD5packFstring ----------------------------- */
/*
NAME
   HD5packFstring -- convert a C string into a Fortran string
USAGE
   int HD5packFstring(src, dest, len)
   char * src;          IN:  source string
   char * dest;         OUT: destination
   int   len;          IN:  length of string
RETURNS
   SUCCEED / FAIL
DESCRIPTION
   given a NULL terminated C string 'src' convert it to
   a space padded Fortran string 'dest' of length 'len'

   This is very similar to HDc2fstr except that function does
   it in place and this one copies.  We should probably only
   support one of these.

---------------------------------------------------------------------------*/
void
HD5packFstring(char *src, char *dest, size_t dst_len)
{
    size_t src_len=HDstrlen(src);

    /* Copy over the string information, up to the length of the src */
    /* (Don't copy the NUL terminator from the C string to the FORTRAN string */
    HDmemcpy(dest,src,MIN(src_len,dst_len));

    /* Pad out any remaining space in the FORTRAN string with ' 's */
    if(src_len<dst_len)
        HDmemset(&dest[src_len],' ',dst_len-src_len);
}   /* HD5packFstring */

