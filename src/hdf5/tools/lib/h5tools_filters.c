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

#include "hdf5.h"
#include "h5tools.h"

/*-------------------------------------------------------------------------
 * print a warning message
 *-------------------------------------------------------------------------
 */
static void print_warning(const char *dname, const char *fname)
{
 fprintf(stderr,"warning: dataset <%s> cannot be read, %s filter is not available\n",
  dname,fname);
}

/*-------------------------------------------------------------------------
 * Function: h5tools_canreadf
 *
 * Purpose: check if the dataset creation property list has filters that
 * are not registered in the current configuration
 * 1) the external filters GZIP and SZIP might not be available
 * 2) the internal filters might be turned off
 *
 * Return: 1, can read, 0, cannot, -1 error
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: March 1, 2004
 *
 *-------------------------------------------------------------------------
 */
int h5tools_canreadf(const char* name, /* object name, serves also as boolean print */
                     hid_t dcpl_id)    /* dataset creation property list */
{

 int          nfilters;       /* number of filters */
 H5Z_filter_t filtn;          /* filter identification number */
 int          i;              /* index */


 /* get information about filters */
 if ((nfilters = H5Pget_nfilters(dcpl_id))<0)
  return -1;

 /* if we do not have filters, we can read the dataset safely */
 if (!nfilters)
  return 1;

 /* check availability of filters */
 for(i = 0; i < nfilters; i++)
 {
  if((filtn = H5Pget_filter2(dcpl_id, (unsigned)i, 0, 0, 0, 0, 0, NULL)) < 0)
   return -1;

  switch(filtn)
  {
/*-------------------------------------------------------------------------
 * user defined filter
 *-------------------------------------------------------------------------
 */
  default:
    if(name)
     print_warning(name, "user defined");
    return 0;

/*-------------------------------------------------------------------------
 * H5Z_FILTER_DEFLATE      1 , deflation like gzip
 *-------------------------------------------------------------------------
 */
  case H5Z_FILTER_DEFLATE:
#ifndef H5_HAVE_FILTER_DEFLATE
    if (name)
     print_warning(name,"deflate");
    return 0;
#endif
   break;
/*-------------------------------------------------------------------------
 * H5Z_FILTER_SZIP       4 , szip compression
 *-------------------------------------------------------------------------
 */
  case H5Z_FILTER_SZIP:
#ifndef H5_HAVE_FILTER_SZIP
    if (name)
     print_warning(name,"SZIP");
    return 0;
#endif
   break;
/*-------------------------------------------------------------------------
 * H5Z_FILTER_SHUFFLE    2 , shuffle the data
 *-------------------------------------------------------------------------
 */
  case H5Z_FILTER_SHUFFLE:
#ifndef H5_HAVE_FILTER_SHUFFLE
    if (name)
     print_warning(name,"shuffle");
    return 0;
#endif
   break;
/*-------------------------------------------------------------------------
 * H5Z_FILTER_FLETCHER32 3 , fletcher32 checksum of EDC
 *-------------------------------------------------------------------------
 */
  case H5Z_FILTER_FLETCHER32:
#ifndef H5_HAVE_FILTER_FLETCHER32
    if (name)
     print_warning(name,"fletcher32");
    return 0;
#endif
   break;
/*-------------------------------------------------------------------------
 * H5Z_FILTER_NBIT
 *-------------------------------------------------------------------------
 */
  case H5Z_FILTER_NBIT:
#ifndef H5_HAVE_FILTER_NBIT
    if (name)
     print_warning(name,"nbit");
    return 0;
#endif
   break;
/*-------------------------------------------------------------------------
 * H5Z_FILTER_SCALEOFFSET
 *-------------------------------------------------------------------------
 */
  case H5Z_FILTER_SCALEOFFSET:
#ifndef H5_HAVE_FILTER_SCALEOFFSET
    if (name)
     print_warning(name,"scaleoffset");
    return 0;
#endif
   break;
  }/*switch*/
 }/*for*/

 return 1;
}


/*-------------------------------------------------------------------------
 * Function: h5tools_canwritef
 *
 * Purpose: check if the filter is available and can write data.
 * At this time, all filters that are available can write data,
 * except SZIP, which may be configured decoder-only.
 *
 * Return: 1, can write, 0, cannot, -1 error
 *
 * Programmer:
 *
 * Date: October 5, 2004
 *
 *-------------------------------------------------------------------------
 */
int h5tools_can_encode( H5Z_filter_t filtn)
{
  switch (filtn)
  {
    /* user defined filter     */
  default:
    return 0;

  case H5Z_FILTER_DEFLATE:
#ifndef H5_HAVE_FILTER_DEFLATE
   return 0;
#endif
   break;
  case H5Z_FILTER_SZIP:
#ifndef H5_HAVE_FILTER_SZIP
   return 0;
#else
   {
   unsigned int filter_config_flags;

   if(H5Zget_filter_info(filtn, &filter_config_flags)<0)
       return -1;
   if ((filter_config_flags &
          (H5Z_FILTER_CONFIG_ENCODE_ENABLED|H5Z_FILTER_CONFIG_DECODE_ENABLED)) == 0) {
    /* filter present but neither encode nor decode is supported (???) */
    return -1;
   } else if ((filter_config_flags &
          (H5Z_FILTER_CONFIG_ENCODE_ENABLED|H5Z_FILTER_CONFIG_DECODE_ENABLED)) ==
    H5Z_FILTER_CONFIG_DECODE_ENABLED) {
     /* decoder only: read but not write */
    return 0;
   } else if ((filter_config_flags &
          (H5Z_FILTER_CONFIG_ENCODE_ENABLED|H5Z_FILTER_CONFIG_DECODE_ENABLED)) ==
    H5Z_FILTER_CONFIG_ENCODE_ENABLED) {
     /* encoder only: write but not read (???) */
     return -1;
   } else if ((filter_config_flags &
          (H5Z_FILTER_CONFIG_ENCODE_ENABLED|H5Z_FILTER_CONFIG_DECODE_ENABLED)) ==
          (H5Z_FILTER_CONFIG_ENCODE_ENABLED|H5Z_FILTER_CONFIG_DECODE_ENABLED)) {
    return 1;
   }
   }
#endif
   break;
  case H5Z_FILTER_SHUFFLE:
#ifndef H5_HAVE_FILTER_SHUFFLE
    return 0;
#endif
   break;
  case H5Z_FILTER_FLETCHER32:
#ifndef H5_HAVE_FILTER_FLETCHER32
    return 0;
#endif
   break;
  case H5Z_FILTER_NBIT:
#ifndef H5_HAVE_FILTER_NBIT
    return 0;
#endif
   break;
  case H5Z_FILTER_SCALEOFFSET:
#ifndef H5_HAVE_FILTER_SCALEOFFSET
    return 0;
#endif
   break;
  }/*switch*/

 return 1;
}
