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

/* This files contains C stubs for H5Z Fortran APIs */

#include "H5f90.h"

/*----------------------------------------------------------------------------
 * Name:        h5zunregister_c
 * Purpose:     Call H5Zunregister to unregister filter
 * Inputs:      filter identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, March 12, 2003
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5zunregister_c (int_f *filter)
{
     int ret_value = -1;
     herr_t status;
     H5Z_filter_t c_filter;

     /*
      * Call H5Zunregister function.
      */
     c_filter = (H5Z_filter_t)*filter;
     printf(" filter # %d \n", (int)c_filter);
     status = H5Zunregister(c_filter);
     printf("From C zunregister %d \n", status);
     if (status < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5zfiletr_avail_c
 * Purpose:     Call H5Zfilter_avail to find if filter is available
 * Inputs:      filter - filter identifier
 * Outputs:     flag - status flag
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, March 12, 2003
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5zfilter_avail_c ( int_f *filter , int_f *flag )
{
  int ret_value = 0;
  H5Z_filter_t c_filter;
  htri_t status;

  c_filter = (H5Z_filter_t)*filter;
  status = H5Zfilter_avail(c_filter);
  *flag = (int_f)status;
  if ( status < 0  ) ret_value = -1;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5zget_filter_info_c
 * Purpose:     Call H5Zget_filter_info to find if filter has its encoder
 *              and/or its decoder available
 * Inputs:      filter - filter identifier
 * Outputs:     flag - status flag
 * Returns:     0 on success, -1 on failure
 * Programmer:  Nat Furrer and James Laird
 *              Wednesday, June 16, 2004
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5zget_filter_info_c ( int_f *filter , int_f *flag )
{
  int ret_value = 0;
  H5Z_filter_t c_filter;
  unsigned int c_flag;

  c_filter = (H5Z_filter_t)*filter;
  ret_value = H5Zget_filter_info(c_filter, &c_flag);
  *flag = (int_f)c_flag;

  return ret_value;
}
