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

#include "t.h"
#include "H5Eprivate.h"

/*----------------------------------------------------------------------------
 * Name:        h5_fixname_c
 * Purpose:     Call h5_fixname to modify file name
 * Inputs:      base_name - name of the file
 *              base_namelen - name length
 *              fapl - file access property list
 *              full_name - buffer to return full name
 *              full_namelen - name length
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Friday, September 13, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5_fixname_c(_fcd base_name, size_t_f *base_namelen, hid_t_f* fapl, _fcd full_name, size_t_f *full_namelen)
{
     char *c_base_name = NULL;
     char *c_full_name = NULL;
     int_f ret_value = 0;

     /*
      * Convert FORTRAN name to C name
      */
     if(NULL == (c_base_name = (char *)HD5f2cstring(base_name, (size_t)*base_namelen)))
         HGOTO_DONE(FAIL)
     if(NULL == (c_full_name = (char *)HDmalloc((size_t)*full_namelen + 1)))
         HGOTO_DONE(FAIL)

     /*
      * Call h5_fixname function.
      */
     if(NULL == h5_fixname(c_base_name, (hid_t)*fapl, c_full_name, (size_t)*full_namelen + 1))
         HGOTO_DONE(FAIL)
     HD5packFstring(c_full_name, _fcdtocp(full_name), (size_t)*full_namelen);

done:
     if(c_base_name)
        HDfree(c_base_name);
     if(c_full_name)
        HDfree(c_full_name);

     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5_cleanup_c
 * Purpose:     Call h5_cleanup to clean temporary files.
 * Inputs:      base_name - name of the file
 *              base_namelen - name length
 *              fapl - file access property list
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, September 19, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5_cleanup_c(_fcd base_name, size_t_f *base_namelen, hid_t_f* fapl)
{
     char filename[1024];
     int ret_value = -1;
     char *c_base_name[1];
     hid_t c_fapl;

     /*
      * Define ifile access property list
      */
     c_fapl = (hid_t)*fapl;
     /*c_fapl = H5Pcreate(H5P_FILE_ACCESS);*/
     /*
      * Convert FORTRAN name to C name
      */
     c_base_name[0] = (char *)HD5f2cstring(base_name, (size_t)*base_namelen);
     if (c_base_name[0] == NULL) goto DONE;

     /*
      * Call h5_cleanup function.
      */
     /*if (h5_cleanup(c_base_name, c_fapl) != 0) {
     ret_value = 0;
     goto DONE;
     }
*/
     h5_fixname(c_base_name[0], c_fapl, filename, sizeof(filename));
     HDremove(filename);
     ret_value =0;

DONE:
     if (NULL != c_base_name[0]) HDfree(c_base_name[0]);
     return ret_value;

}

/*----------------------------------------------------------------------------
 * Name:        h5_exit_c
 * Purpose:     Call 'exit()' to terminate application.  Be careful not to
 *              overflow the exit value range since UNIX supports a very
 *              small range such as 1 byte.  Therefore, exit(256) may end
 *              up as exit(0).
 * Inputs:      status - status for exit() to return
 * Returns:     none
 * Programmer:  Quincey Koziol
 *              Tuesday, December 14, 2004
 * Modifications:
 *---------------------------------------------------------------------------*/
void
nh5_exit_c(int_f *status)
{
    HDexit((int)*status);
}   /* h5_exit_c */

/*----------------------------------------------------------------------------
 * Name:        h5_env_nocleanup_c
 * Purpose:     Determines the state of the environment variable HDF5_NOCLEANUP
 * Input:       none
 * Output:      status:  1 - HDF5_NOCLEANUP is set
 *                       0 - HDF5_NOCLEANUP is not set
 * Returns:     none
 * Programmer:  M.S. Breitenfeld
 *              September 30, 3008
 * Modifications:
 *---------------------------------------------------------------------------*/
void
nh5_env_nocleanup_c(int_f *status)
{
  *status = (int_f)0;
  if (HDgetenv("HDF5_NOCLEANUP"))
    *status = (int_f)1;
}   /* h5_env_nocleanup_c */

