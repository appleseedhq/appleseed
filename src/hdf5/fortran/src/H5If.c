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

/* This files contains C stubs for H5I Fortran APIs */

#include "H5f90.h"
#include "H5Eprivate.h"

/*----------------------------------------------------------------------------
 * Name:        h5iget_type_c
 * Purpose:     Call H5Iget_type to get the type of an object
 * Inputs:      obj_id - object identifier
 * Outputs:     type - object type
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Thursday, March 24, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5iget_type_c (hid_t_f *obj_id, int_f *type)
{
     int ret_value = -1;
     hid_t c_obj_id;
     H5I_type_t c_type;

     /*
      * Call H5Iget_type function.
      */
     c_obj_id = *obj_id;
     c_type = H5Iget_type(c_obj_id);
     if (c_type == H5I_BADID) return ret_value;
     *type = (int_f)c_type;
     ret_value = 0;
     return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5iget_name_c
 * Purpose:     Call H5Iget_name to get object's name
 * Inputs:      obj_id - object identifier
 *              buf_size - size of the buffer
 * Outputs:     buf - buffer to hold the name
 * Returns:     length of the name on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, March 12, 2003
 * Modifications:
 *               Changed the size of c_buf_size to c_buf_size + 1, which
 *               fixes the problem of truncating the string by 1 if the
 *               exact size of the string (buf_size) is passed in.
 *               M.S. Breitenfeld, April 21, 2008
 *---------------------------------------------------------------------------*/
int_f
nh5iget_name_c(hid_t_f *obj_id, _fcd buf, size_t_f *buf_size, size_t_f *name_size)
{
     int ret_value = -1;
     hid_t c_obj_id;
     ssize_t c_size;
     size_t c_buf_size;
     char *c_buf =NULL;

     /*
      * Allocate buffer to hold name of an object
      */
     c_buf_size = (size_t)*buf_size +1;
     c_buf = (char *)HDmalloc(c_buf_size);
     if (c_buf == NULL) return ret_value;

     /*
      * Call H5IAget_name function
      */
     c_obj_id = (hid_t)*obj_id;
     c_size = H5Iget_name(c_obj_id, c_buf, c_buf_size);
     if (c_size < 0) goto DONE;

     /*
      * Convert C name to FORTRAN and place it in the given buffer
      */
      HD5packFstring(c_buf, _fcdtocp(buf), c_buf_size-1);
      *name_size = (size_t_f)c_size;
      ret_value = 0;

DONE:
      HDfree(c_buf);
      return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5iinc_ref_c
 * Purpose:     Call H5Iinc_ref to increment object's reference count
 * Inputs:      obj_id - object identifier
 * Outputs:     ref_count - Reference count of ID
 * Returns:     current reference count on success, -1 on failure
 * Programmer:  Quincey Koziol
 *              Tuesday, December  9, 2003
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5iinc_ref_c(hid_t_f *obj_id, int_f *ref_count)
{
     int ret_value;

     /*
      * Call H5Iinc_ref function
      */
     if ((ret_value = H5Iinc_ref(*obj_id)) < 0)
         HGOTO_DONE(FAIL);

    /* Set output & return values */
    *ref_count=ret_value;
    ret_value=0;

done:
      return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5idec_ref_c
 * Purpose:     Call H5Idec_ref to decrement object's reference count
 * Inputs:      obj_id - object identifier
 * Outputs:     ref_count - Reference count of ID
 * Returns:     current reference count on success, -1 on failure
 * Programmer:  Quincey Koziol
 *              Tuesday, December  9, 2003
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5idec_ref_c(hid_t_f *obj_id, int_f *ref_count)
{
     int ret_value;

     /*
      * Call H5Idec_ref function
      */
     if ((ret_value = H5Idec_ref(*obj_id)) < 0)
         HGOTO_DONE(FAIL);

    /* Set output & return values */
    *ref_count=ret_value;
    ret_value=0;

done:
      return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5iget_ref_c
 * Purpose:     Call H5Iget_ref to retrieve object's reference count
 * Inputs:      obj_id - object identifier
 * Outputs:     ref_count - Reference count of ID
 * Returns:     current reference count on success, -1 on failure
 * Programmer:  Quincey Koziol
 *              Tuesday, December  9, 2003
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5iget_ref_c(hid_t_f *obj_id, int_f *ref_count)
{
     int ret_value;

     /*
      * Call H5Iget_ref function
      */
     if ((ret_value = H5Iget_ref(*obj_id)) < 0)
         HGOTO_DONE(FAIL);

    /* Set output & return values */
    *ref_count=ret_value;
    ret_value=0;

done:
      return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5iget_file_id_c
 * Purpose:     Call H5Iget_file_id to obtain file identifier from object identifier
 * Inputs:      obj_id - object identifier
 * Outputs:     file_id - file identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Tuesday, August 24, 2004
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5iget_file_id_c(hid_t_f *obj_id, hid_t_f *file_id)
{
     int ret_value;
     hid_t c_file_id;

     /*
      * Call H5Iget_file_id
      */
     if ((c_file_id = H5Iget_file_id(*obj_id)) < 0)
         HGOTO_DONE(FAIL);

    /* Set output & return values */
    *file_id=(hid_t_f)c_file_id;
    ret_value=0;

done:
      return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5iis_valid_c
 * Purpose:     Calls H5Iis_valid
 * Inputs:      obj_id - object identifier
 * Outputs:     0 = false, 1 = true
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Tuesday, August 24, 2004
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5iis_valid_c(hid_t_f *obj_id, int_f *c_valid)
{
     int ret_value;
     htri_t c_ret_value;

     /*
      * Call H5Iis_valid
      */
     if ((c_ret_value = H5Iis_valid(*obj_id)) < 0)
       HGOTO_DONE(FAIL);

     /* Set output & return values */
     *c_valid = (int_f)c_ret_value;
     ret_value=0;

done:
      return ret_value;
}
