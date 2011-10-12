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

/* This files contains C stubs for H5O Fortran APIs */

#include "H5f90.h"
#include "H5Eprivate.h"

/*----------------------------------------------------------------------------
 * Name:        h5olink_c
 * Purpose:     Calls H5Olink
 * Inputs:
 *      object_id        - Object to be linked.
 *      new_loc_id       - File or group identifier specifying location at which object is to be linked.
 *      name             - Name of link to be created, relative to new_loc_id.
 *      namelen          - Length of buffer for link to be created.
 *      lcpl_id          - Link creation property list identifier.
 *      lapl_id          - Link access property list identifier.
 * Outputs:
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              April 21, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5olink_c (hid_t_f *object_id, hid_t_f *new_loc_id, _fcd name, size_t_f *namelen,
            hid_t_f *lcpl_id, hid_t_f *lapl_id)
{
  char *c_name = NULL;          /* Buffer to hold C string */
  int_f ret_value = 0;          /* Return value */

  /*
   * Convert FORTRAN name to C name
   */
  if( (c_name = HD5f2cstring(name, (size_t)*namelen)) == NULL)
    HGOTO_DONE(FAIL);

  /*
   * Call H5Olink function.
   */
  if((hid_t_f)H5Olink((hid_t)*object_id, (hid_t)*new_loc_id, c_name,
		       (hid_t)*lcpl_id, (hid_t)*lapl_id) < 0)
    HGOTO_DONE(FAIL);

 done:
  if(c_name)
    HDfree(c_name);
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5oopen_c
 * Purpose:     Calls H5Oopen
 * Inputs:      loc_id  - File or group identifier
 *	        name    - Attribute access property list
 *              namelen - Size of name
 *              lapl_id - Link access property list
 * Outputs:     obj_id  - Dataset identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              April 18, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5oopen_c (hid_t_f *loc_id, _fcd name, size_t_f *namelen, hid_t_f *lapl_id, hid_t_f *obj_id)
{
  char *c_name = NULL;          /* Buffer to hold C string */
  int_f ret_value = 0;          /* Return value */

  /*
   * Convert FORTRAN name to C name
   */
  if((c_name = HD5f2cstring(name, (size_t)*namelen)) == NULL)
    HGOTO_DONE(FAIL);

  /*
   * Call H5Oopen function.
   */
  if((*obj_id = (hid_t_f)H5Oopen((hid_t)*loc_id, c_name, (hid_t)*lapl_id)) < 0)
    HGOTO_DONE(FAIL);

 done:
  if(c_name)
    HDfree(c_name);
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5oopen_by_addr_c
 * Purpose:     Calls H5open_by_addr
 * Inputs:      loc_id  - File or group identifier
 *              addr     - Object’s address in the file
 * Outputs:     obj_id  - Dataset identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  M. Scot Breitenfeld
 *              September 14, 2009
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5oopen_by_addr_c (hid_t_f *loc_id, haddr_t_f *addr, hid_t_f *obj_id)
{
  int_f ret_value = 0;          /* Return value */

  /*
   * Call H5Oopen_by_address function.
   */
  if((*obj_id = (hid_t_f)H5Oopen_by_addr((hid_t)*loc_id, (haddr_t)*addr)) < 0)
    HGOTO_DONE(FAIL);

 done:
  return ret_value;
}
