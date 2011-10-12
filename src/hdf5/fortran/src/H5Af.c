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

/* This files contains C stubs for H5A Fortran APIs */

#include "H5f90.h"
#include "H5Eprivate.h"

/*----------------------------------------------------------------------------
 * Name:        h5acreate_c
 * Purpose:     Call H5Acreate2 to create an attribute
 * Inputs:      obj_id - object identifier
 *              name - name of the attribute
 *              namelen - name length
 *              type_id - datatype identifier
 *              space_id - dataspace identifier
 *              crt_pr  - identifier of creation property list
 * Outputs:     attr_id - attribute identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, August 12, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5acreate_c(hid_t_f *obj_id, _fcd name, size_t_f *namelen, hid_t_f *type_id,
    hid_t_f *space_id, hid_t_f *crt_prp, hid_t_f *aapl, hid_t_f *attr_id)
{
    char *c_name = NULL;        /* Buffer to hold C string */
    int_f ret_value = 0;        /* Return value */

     /*
      * Convert FORTRAN name to C name
      */
    if(NULL == (c_name = HD5f2cstring(name, (size_t)*namelen)))
        HGOTO_DONE(FAIL);

     /*
      * Call H5Acreate2 function.
      */
    if((*attr_id = (hid_t_f)H5Acreate2((hid_t)*obj_id, c_name, (hid_t)*type_id, (hid_t)*space_id, (hid_t)*crt_prp, (hid_t)*aapl)) < 0)
        HGOTO_DONE(FAIL);

done:
    if(c_name)
        HDfree(c_name);
    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5aopen_name _c
 * Purpose:     Call H5Aopen to open an attribute
 * Inputs:      obj_id - object identifier
 *              name - name of the attribute
 *              namelen - name length
 * Outputs:     attr_id - dataset identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, August 12, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5aopen_name_c (hid_t_f *obj_id, _fcd name, size_t_f *namelen, hid_t_f *attr_id)
{
    char *c_name = NULL;          /* Buffer to hold C string */
    int_f ret_value = 0;          /* Return value */

     /*
      * Convert FORTRAN name to C name
      */
     if((c_name = HD5f2cstring(name, (size_t)*namelen)) == NULL)
        HGOTO_DONE(FAIL);

     /*
      * Call H5Aopen function.
      */
     if((*attr_id = (hid_t_f)H5Aopen((hid_t)*obj_id, c_name, H5P_DEFAULT)) < 0)
         HGOTO_DONE(FAIL);

done:
    if(c_name)
        HDfree(c_name);
    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5awritec_c
 * Purpose:     Call h5awrite_c to write a character  attribute
 * Inputs:      attr_id - dataset identifier
 *              mem_type_id - memory datatype identifier
 *              buf      - character data buffer
 *              dims     - array to store dimensions sizes of buf; used only
 *                         by Fortran routine.
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday , August 12, 1999
 * Modifications: dims paramete added.
 *                April 4, 2001
 *---------------------------------------------------------------------------*/
int_f
nh5awritec_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}
int_f
nh5awritec_s_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}

int_f
nh5awritec_1_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}

int_f
nh5awritec_2_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}

int_f
nh5awritec_3_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}

int_f
nh5awritec_4_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}

int_f
nh5awritec_5_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}

int_f
nh5awritec_6_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}

int_f
nh5awritec_7_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}



/*----------------------------------------------------------------------------
 * Name:        h5awrite_c
 * Purpose:     Call H5Awrite to write a attribute
 * Inputs:      attr_id - attribute identifier
 *              mem_type_id - memory datatype identifier
 *              buf      - data buffer
 *              dims     - array to store dimensions sizes of buf; used only
 *                         by Fortran routine.
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, August 12, 1999
 * Modifications: dims parameter added
 *                                           April 4, 2001
 *                Added nh5awrite_integer(real,double)_s,1-7 functions to eliminate
 *                complains about wrong parameters types in h5awrite_c function
 *                called by Fortran routines.
 *                                           October 9, 2006 EIP
 *---------------------------------------------------------------------------*/
int_f
nh5awrite_integer_s_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_integer_1_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_integer_2_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_integer_3_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_integer_4_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_integer_5_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_integer_6_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_integer_7_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_real_s_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_real_1_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_real_2_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_real_3_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_real_4_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_real_5_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_real_6_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_real_7_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_double_s_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_double_1_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_double_2_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_double_3_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_double_4_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_double_5_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_double_6_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_double_7_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
    int_f ret_value=0;          /* Return value */

     /*
      * Call H5Awrite function.
      */
     if (H5Awrite((hid_t)*attr_id, (hid_t)*mem_type_id, buf) < 0)
        HGOTO_DONE(FAIL);

done:
     return ret_value;
}


/*----------------------------------------------------------------------------
 * Name:        h5areadc_c
 * Purpose:     Call h5aread_c to read character  attribute
 * Inputs:      dset_id - dataset identifier
 *              mem_type_id - memory datatype identifier
 *              dims     - array to store dimensions sizes of buf; used only
 *                         by Fortran routine.
 * Outputs:     buf      - character data buffer
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, August 12, 1999
 * Modifications: dims parameter added.
 *                April 4, 2001
 *                Added nh5areadc_s,1-7 functions to eliminate
 *                complains about wrong parameters types in h5awrite_c function
 *                called by Fortran routines.
 *                                           October 9, 2006 EIP
 *---------------------------------------------------------------------------*/
int_f
nh5areadc_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}
int_f
nh5areadc_s_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}

int_f
nh5areadc_1_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}

int_f
nh5areadc_2_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}

int_f
nh5areadc_3_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}

int_f
nh5areadc_4_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}

int_f
nh5areadc_5_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}

int_f
nh5areadc_6_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}
int_f
nh5areadc_7_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}



/*----------------------------------------------------------------------------
 * Name:        h5aread_c
 * Purpose:     Call H5Aread to read an attribute
 * Inputs:      dset_id - dataset identifier
 *              mem_type_id - memory datatype identifier
 *              dims     - array to store dimensions sizes of buf; used only
 *                         by Fortran routine.
 * Outputs:     buf      - data buffer
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, August 12, 1999
 * Modifications: dims paramete added.
 *                April 4, 2001
 *                Added nh5aread_integer(real,double)_s,1-7 functions to eliminate
 *                complains about wrong parameters types in h5awrite_c function
 *                called by Fortran routines.
 *                                           October 9, 2006 EIP
 *---------------------------------------------------------------------------*/
int_f
nh5aread_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
    int_f ret_value=0;          /* Return value */

     /*
      * Call H5Aread function.
      */
     if (H5Aread((hid_t)*attr_id, (hid_t)*mem_type_id, buf) < 0)
         HGOTO_DONE(FAIL);

done:
     return ret_value;
}

int_f
nh5aread_integer_s_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_integer_1_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_integer_2_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_integer_3_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_integer_4_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_integer_5_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_integer_6_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_integer_7_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_real_s_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_real_1_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_real_2_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_real_3_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_real_4_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_real_5_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_real_6_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_real_7_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_double_s_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_double_1_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_double_2_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_double_3_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_double_4_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_double_5_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_double_6_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_double_7_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}


/*----------------------------------------------------------------------------
 * Name:        h5aclose_c
 * Purpose:     Call H5Aclose to close an attribute
 * Inputs:      attr_id - identifier of an attribute to be closed
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, August 12, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5aclose_c ( hid_t_f *attr_id )
{
    int_f ret_value=0;          /* Return value */

    if (H5Aclose((hid_t)*attr_id) < 0)
        HGOTO_DONE(FAIL);

done:
    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5adelete_c
 * Purpose:     Call H5Adelete to delete an attribute
 * Inputs:      obj_id - object identifier
 *              name - name of the attribute
 *              namelen - name length
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, August 12, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5adelete_c (hid_t_f *obj_id, _fcd name, size_t_f *namelen)
{
    char *c_name = NULL;        /* Buffer to hold C string */
    int_f ret_value = 0;        /* Return value */

     /*
      * Convert FORTRAN name to C name
      */
     if((c_name = HD5f2cstring(name, (size_t)*namelen)) == NULL)
        HGOTO_DONE(FAIL);

     /*
      * Call H5Adelete function.
      */
     if(H5Adelete((hid_t)*obj_id, c_name) < 0)
         HGOTO_DONE(FAIL);

done:
    if(c_name)
        HDfree(c_name);

    return ret_value;
}


/*----------------------------------------------------------------------------
 * Name:        h5aopen_idx_c
 * Purpose:     Call H5Aopen_by_idx to open an attribute
 * Inputs:      obj_id - object identifier
 *              idx    - attribute index ( zero based)
 * Outputs:     attr_id - attribute identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, August 12, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5aopen_idx_c (hid_t_f *obj_id, int_f *idx, hid_t_f *attr_id)
{
    int_f ret_value = 0;          /* Return value */

     /*
      * Call H5Aopen_by_idx function.
      */
     if((*attr_id = (hid_t_f)H5Aopen_by_idx((hid_t)*obj_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)*idx, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        HGOTO_DONE(FAIL);

done:
     return ret_value;
}


/*----------------------------------------------------------------------------
 * Name:        h5aget_space_c
 * Purpose:     Call H5Aget_space to get attribute's dataspace
 * Inputs:      attr_id - attribute identifier
 * Outputs:     space_id - dataspace identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, August 12, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5aget_space_c (hid_t_f *attr_id, hid_t_f *space_id)
{
    int_f ret_value=0;          /* Return value */

     /*
      * Call H5Aget_space function.
      */
     if ((*space_id = (hid_t_f)H5Aget_space((hid_t)*attr_id)) < 0)
         HGOTO_DONE(FAIL);

done:
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5aget_type_c
 * Purpose:     Call H5Aget_space to get attribute's datatype
 * Inputs:      attr_id - attribute identifier
 * Outputs:     type_id - datatype identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, August 12, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5aget_type_c (hid_t_f *attr_id, hid_t_f *type_id)
{
    int_f ret_value=0;          /* Return value */

     /*
      * Call H5Aget_type function.
      */
     if ((*type_id = (hid_t_f)H5Aget_type((hid_t)*attr_id)) < 0)
         HGOTO_DONE(FAIL);

done:
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5aget_num_attrs_c
 * Purpose:     Call H5Oget_info to determine number of
 *              attributes of an object
 * Inputs:      obj_id - object identifier
 *              attr_num - number of attributes
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, August 12, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5aget_num_attrs_c (hid_t_f *obj_id, int_f *attr_num)
{
    H5O_info_t oinfo;           /* Object info */
    int_f ret_value = 0;        /* Return value */

    /*
     * Call H5Oget_info function.
     */
    if(H5Oget_info((hid_t)*obj_id, &oinfo) < 0)
        HGOTO_DONE(FAIL);

    /* Set number of attributes */
    *attr_num = (int_f)oinfo.num_attrs;

done:
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5aget_name_c
 * Purpose:     Call H5Aget_name to get attribute's name
 * Inputs:      attr_id - attribute identifier
 *              bufsize - size of the buffer
 * Outputs:     buf - buffer to hold the name
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, August 12, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5aget_name_c(hid_t_f *attr_id, size_t_f *bufsize, _fcd buf)
{
  size_t c_bufsize;
  char *c_buf=NULL;           /* Buffer to hold C string */
  int_f ret_value=0;          /* Return value */

  c_bufsize = (size_t)*bufsize+1;

  /*
   * Allocate buffer to hold name of an attribute
   */
  if(NULL == (c_buf = (char *)HDmalloc(c_bufsize)))
    HGOTO_DONE(FAIL);

  /*
   * Call H5Aget_name function
   */
  if ((ret_value = (int_f)H5Aget_name((hid_t)*attr_id, c_bufsize, c_buf)) < 0)
    HGOTO_DONE(FAIL);

  /*
   * Convert C name to FORTRAN and place it in the given buffer
   */
  HD5packFstring(c_buf, _fcdtocp(buf), c_bufsize-1);

done:
  if(c_buf) HDfree(c_buf);
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5aget_storage_size_c
 * Purpose:     Call H5Aget_storage_size
 * Inputs:      attr_id - identifier of an attribute
 * Outputs:     size    - attributes storage requirements
 * Returns:     0 on success, -1 on failure
 * Programmer:  M. S. Breitenfeld
 *              January, 2008
 * Modifications: N/A
 *---------------------------------------------------------------------------*/

int_f
nh5aget_storage_size_c ( hid_t_f *attr_id,  hsize_t_f *size)
{
    int_f ret_value=0;          /* Return value */

    if ((*size = (hsize_t_f)H5Aget_storage_size((hid_t)*attr_id)) < 0)
        HGOTO_DONE(FAIL);

done:
    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5aget_create_plist_c
 * Purpose:     Call H5Aget_create_plist
 * Inputs:      attr_id - identifier of an attribute
 * Outputs:     creation_prop_id - Identifier for the attribute’s creation property
 * Returns:     0 on success, -1 on failure
 * Programmer:  M. S. Breitenfeld
 *              January, 2008
 * Modifications: N/A
 *---------------------------------------------------------------------------*/

int_f
nh5aget_create_plist_c ( hid_t_f *attr_id,  hid_t_f *creation_prop_id)
{
    int_f ret_value=0;          /* Return value */

    if ((*creation_prop_id = (hid_t_f)H5Aget_create_plist((hid_t)*attr_id)) < 0)
        HGOTO_DONE(FAIL);

done:
    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:      h5arename_by_name_c
 * Purpose:   Calls H5Arename_by_name
 * Inputs:    loc_id        - Object identifier
 *            obj_name      - Name of object, relative to location,
 *                             whose attribute is to be renamed
 *            obj_name_len      - Object name length
 *            old_attr_name     - Prior attribute name
 *            old_attr_name_len - Prior attribute name length
 *            new_attr_name     - New attribute name
 *            new_attr_name_len - New attribute name length
 *            lapl_id       - Link access property list identifier
 * Outputs:     N/A
 * Returns:     0 on success, -1 on failure
 * Programmer:  M. S. Breitenfeld
 *              January, 2008
 * Modifications: N/A
 *---------------------------------------------------------------------------*/

int_f
nh5arename_by_name_c( hid_t_f *loc_id, _fcd obj_name, size_t_f *obj_namelen,
		      _fcd old_attr_name, size_t_f *old_attr_namelen,
		      _fcd new_attr_name, size_t_f *new_attr_namelen,
		      hid_t_f *lapl_id )
{
    char *c_obj_name = NULL;          /* Buffer to hold C string */
    char *c_old_attr_name = NULL;     /* Buffer to hold C string */
    char *c_new_attr_name = NULL;     /* Buffer to hold C string */
    int_f ret_value=0;          /* Return value */
     /*
      * Convert FORTRAN name to C name
      */
    if((c_obj_name = HD5f2cstring(obj_name, (size_t)*obj_namelen)) == NULL)
      HGOTO_DONE(FAIL);
    if((c_old_attr_name = HD5f2cstring(old_attr_name, (size_t)*old_attr_namelen)) == NULL)
      HGOTO_DONE(FAIL);
    if((c_new_attr_name = HD5f2cstring(new_attr_name, (size_t)*new_attr_namelen)) == NULL)
      HGOTO_DONE(FAIL);

    if(H5Arename_by_name((hid_t)*loc_id,c_obj_name,c_old_attr_name,c_new_attr_name,(hid_t)*lapl_id) < 0)
        HGOTO_DONE(FAIL);

done:
    if(c_obj_name)
      HDfree(c_obj_name);
    if(c_old_attr_name)
      HDfree(c_old_attr_name);
    if(c_new_attr_name)
      HDfree(c_new_attr_name);
    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5aopen_c
 * Purpose:     Call H5Aopen to open an attribute
 * Inputs:      obj_id       - Identifer for object to which attribute is attached
 *	        attr_name    - Attribute access property list
 *              attr_namelen - size of attr_name
 *              aapl_id      - Link access property list
 * Outputs:     attr_id - dataset identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              January, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5aopen_c (hid_t_f *obj_id, _fcd attr_name, size_t_f *attr_namelen, hid_t_f *aapl_id, hid_t_f *attr_id)
{
    char *c_attr_name = NULL;          /* Buffer to hold C string */
    int_f ret_value = 0;          /* Return value */

     /*
      * Convert FORTRAN name to C name
      */
     if((c_attr_name = HD5f2cstring(attr_name, (size_t)*attr_namelen)) == NULL)
        HGOTO_DONE(FAIL);
     /*
      * Call H5Aopen function.
      */

      if((*attr_id = (hid_t_f)H5Aopen((hid_t)*obj_id, c_attr_name, (hid_t)*aapl_id)) < 0)
	HGOTO_DONE(FAIL);

done:
    if(c_attr_name)
        HDfree(c_attr_name);
    return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5adelete_by_name_c
 * Purpose:     Call h5adelete_by_name to remove an attribute from a specified location
 * Inputs:      loc_id - identifer for object to which attribute is attached
 *              obj_name - object identifier
 *              obj_namelen - name length
 *              attr_name - name of the attribute
 *              attr_namelen - name length
 *              lapl_id - link access property list
 *
 * Outputs:     N/A
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              January, 2008
 * Modifications: N/A
 *---------------------------------------------------------------------------*/
int_f
nh5adelete_by_name_c (hid_t_f *loc_id, _fcd obj_name, size_t_f *obj_namelen, _fcd attr_name, size_t_f *attr_namelen, hid_t_f *lapl_id)
{
    char *c_obj_name = NULL;          /* Buffer to hold C string */
    char *c_attr_name = NULL;         /* Buffer to hold C string */
    int_f ret_value = 0;          /* Return value */

     /*
      * Convert FORTRAN name to C name
      */
    if((c_obj_name = HD5f2cstring(obj_name, (size_t)*obj_namelen)) == NULL)
      HGOTO_DONE(FAIL);
    if((c_attr_name = HD5f2cstring(attr_name, (size_t)*attr_namelen)) == NULL)
      HGOTO_DONE(FAIL);

     /*
      * Call H5Adelete_by_name function.
      */
    if(H5Adelete_by_name((hid_t)*loc_id, c_obj_name, c_attr_name, (hid_t)*lapl_id) < 0)
      HGOTO_DONE(FAIL);

done:
    if(c_attr_name)
        HDfree(c_attr_name);
    if(c_obj_name)
        HDfree(c_obj_name);
    return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5adelete_by_idx_c
 * Purpose:     Call h5adelete_by_idx
 * Inputs:      loc_id - Location or object identifier; may be dataset or group
 *              obj_name - object identifier
 *              obj_namelen - name length
 *              attr_name - name of the attribute
 *              attr_namelen - name length
 *              lapl_id - link access property list
 *
 * Outputs:     N/A
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              January, 2008
 * Modifications: N/A
 *---------------------------------------------------------------------------*/
int_f
nh5adelete_by_idx_c (hid_t_f *loc_id, _fcd obj_name, size_t_f *obj_namelen,
		     int_f *idx_type, int_f *order, hsize_t_f *n, hid_t_f *lapl_id)
{
    char *c_obj_name = NULL;          /* Buffer to hold C string */
    int_f ret_value = 0;          /* Return value */

     /*
      * Convert FORTRAN name to C name
      */
    if(NULL == (c_obj_name = HD5f2cstring(obj_name, (size_t)*obj_namelen)))
        HGOTO_DONE(FAIL)

     /*
      * Call H5Adelete_by_name function.
      */
    if(H5Adelete_by_idx((hid_t)*loc_id, c_obj_name, (H5_index_t)*idx_type, (H5_iter_order_t)*order, (hsize_t)*n, (hid_t)*lapl_id) < 0)
        HGOTO_DONE(FAIL)

done:
    if(c_obj_name)
        HDfree(c_obj_name);

    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5aget_name_by_idx_c
 * Purpose:     Call h5aget_name_by_idx
 * Inputs:
 *
 *        loc_id - Identifer for object to which attribute is attached
 *      obj_name - Name of object, relative to location,
 *                  from which attribute is to be removed *TEST* check NULL
 *      idx_type - Type of index; Possible values are:
 *                         H5_INDEX_UNKNOWN   - Unknown index type
 *                         H5_INDEX_NAME      - Index on names
 *                         H5_INDEX_CRT_ORDER - Index on creation order
 *                         H5_INDEX_N	      - Number of indices defined
 *
 *      order    - Order in which to iterate over index; Possible values are:
 *                          H5_ITER_UNKNOWN  - Unknown order
 *                          H5_ITER_INC      - Increasing order
 *                          H5_ITER_DEC      - Decreasing order
 *                          H5_ITER_NATIVE   - No particular order, whatever is fastest
 *                          H5_ITER_N	     - Number of iteration orders
 *
 *            n  - Attribute’s position in index
 *      attr_id  - Attribute identifier
 *         size  - Buffer size ! *TEST* check for 0 value *CHECK* should this return the correct value
 *
 *     lapl_id   - Link access property list
 *      hdferr   - Error code:
 *                            Returns attribute name size, -1 if fail
 *
 * Outputs:     name - Attribute name
 *
 * Returns:     Size of buffer on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              January, 2008
 * Modifications: N/A
 *---------------------------------------------------------------------------*/
int_f
nh5aget_name_by_idx_c (hid_t_f *loc_id, _fcd obj_name, size_t_f *obj_namelen,
		       int_f *idx_type, int_f *order, hsize_t_f *n, _fcd name,
		       size_t_f *size, hid_t_f *lapl_id)
{
    char *c_obj_name = NULL;          /* Buffer to hold C string */
    ssize_t c_size;
    size_t c_buf_size;
    char *c_buf = NULL;
    int_f ret_value = 0;          /* Return value */

    /*
     * Convert FORTRAN name to C name
     */
    if(NULL == (c_obj_name = HD5f2cstring(obj_name, (size_t)*obj_namelen)))
        HGOTO_DONE(FAIL)

    /*
     * Allocate buffer to hold name of an attribute
     */
    c_buf_size = (size_t)*size + 1;
    if(NULL == (c_buf = (char *)HDmalloc(c_buf_size)))
        HGOTO_DONE(FAIL)

     /*
      * Call H5Aget_name_by_idx function.
      */
     c_size = H5Aget_name_by_idx((hid_t)*loc_id, c_obj_name, (H5_index_t)*idx_type, (H5_iter_order_t)*order, (hsize_t)*n, c_buf, c_buf_size,(hid_t)*lapl_id);
     if(c_size < 0)
        HGOTO_DONE(FAIL)


     /*
      * Convert C name to FORTRAN and place it in the given buffer
      */
     HD5packFstring(c_buf, _fcdtocp(name), c_buf_size - 1);
     *size = (size_t_f)c_size;

done:
   if(c_obj_name)
       HDfree(c_obj_name);
   if(c_buf)
       HDfree(c_buf);
    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5aopen_by_idx_c
 * Purpose:     Call H5Aopen_by_idx
 * Inputs:   loc_id    - Object identifier
 *            obj_name - Name of object to which attribute is attached
 *         obj_namelen - name length
 *            idx_type - Type of index; Possible values are:
 *                         H5_INDEX_UNKNOWN   - Unknown index type
 *                         H5_INDEX_NAME      - Index on names
 *                         H5_INDEX_CRT_ORDER - Index on creation order
 *                         H5_INDEX_N	      - Number of indices defined
 *
 *               order - Order in which to iterate over index; Possible values are:
 *                          H5_ITER_UNKNOWN  - Unknown order
 *                          H5_ITER_INC      - Increasing order
 *                          H5_ITER_DEC      - Decreasing order
 *                          H5_ITER_NATIVE   - No particular order, whatever is fastest
 *                          H5_ITER_N	     - Number of iteration orders
 *
 *                   n - Attribute’s position in index
 *             aapl_id - Attribute access property list
 *             lapl_id - Link access property list
 * Outputs:    attr_id - attribute identifer
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              January, 2008
 * Modifications: N/A
 *---------------------------------------------------------------------------*/
int_f
nh5aopen_by_idx_c (hid_t_f *loc_id, _fcd obj_name, size_t_f *obj_namelen,
		   int_f *idx_type, int_f *order, hsize_t_f *n, hid_t_f *aapl_id, hid_t_f *lapl_id, hid_t_f *attr_id )
{
    char *c_obj_name = NULL;          /* Buffer to hold C string */
    int_f ret_value = 0;          /* Return value */

    /*
     * Convert FORTRAN name to C name
     */
    if(NULL == (c_obj_name = HD5f2cstring(obj_name, (size_t)*obj_namelen)))
        HGOTO_DONE(FAIL)

     /*
      * Call H5Aopen_by_idx function.
      */
    if((*attr_id = (hid_t_f)H5Aopen_by_idx((hid_t)*loc_id, c_obj_name, (H5_index_t)*idx_type, (H5_iter_order_t)*order, (hsize_t)*n, (hid_t)*aapl_id, (hid_t)*lapl_id)) < 0)
        HGOTO_DONE(FAIL)

done:
    if(c_obj_name)
        HDfree(c_obj_name);

    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5aget_info_c
 * Purpose:     Call H5Aget_info
 * Inputs:     loc_id  - Object identifier
 * Outputs:
 *        corder_valid - Indicates whether the the creation order data is valid for this attribute
 *              corder - Is a positive integer containing the creation order of the attribute
 *                cset - Indicates the character set used for the attribute’s name
 *           data_size - indicates the size, in the number of characters, of the attribute
 *
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              January, 2008
 * Modifications: N/A
 *---------------------------------------------------------------------------*/
int_f
nh5aget_info_c (hid_t_f *loc_id, int_f *corder_valid, int_f *corder,
		int_f *cset, hsize_t_f *data_size )
{

    int_f ret_value = 0;          /* Return value */
    H5A_info_t ainfo;


     /*
      * Call H5Aget_info function.
      */
    if(H5Aget_info((hid_t)*loc_id,&ainfo) < 0)
      HGOTO_DONE(FAIL);

    /* Unpack the structure */

    *corder_valid = 0;
    if(ainfo.corder_valid > 0) *corder_valid = 1;

    *corder = (int_f)ainfo.corder;
    *cset = (int_f)ainfo.cset;
    *data_size = (hsize_t)ainfo.data_size;

done:
    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5aget_info_by_idx_c
 * Purpose:     Call  H5Aget_info_by_idx
 * Inputs:    loc_id  - Object identifier
 *            obj_name - Name of object to which attribute is attached
 *         obj_namelen - name length
 *            idx_type - Type of index; Possible values are:
 *                         H5_INDEX_UNKNOWN   - Unknown index type
 *                         H5_INDEX_NAME      - Index on names
 *                         H5_INDEX_CRT_ORDER - Index on creation order
 *                         H5_INDEX_N	      - Number of indices defined
 *
 *               order - Order in which to iterate over index; Possible values are:
 *                          H5_ITER_UNKNOWN  - Unknown order
 *                          H5_ITER_INC      - Increasing order
 *                          H5_ITER_DEC      - Decreasing order
 *                          H5_ITER_NATIVE   - No particular order, whatever is fastest
 *                          H5_ITER_N	     - Number of iteration orders
 *
 *                   n - Attribute’s position in index
 *             lapl_id - Link access property list
 * Outputs:
 *        corder_valid - Indicates whether the the creation order data is valid for this attribute
 *              corder - Is a positive integer containing the creation order of the attribute
 *                cset - Indicates the character set used for the attribute’s name
 *           data_size - indicates the size, in the number of characters, of the attribute
 *
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              January, 2008
 * Modifications: N/A
 *---------------------------------------------------------------------------*/
int_f
nh5aget_info_by_idx_c (hid_t_f *loc_id, _fcd obj_name, size_t_f *obj_namelen,
		int_f *idx_type, int_f *order, hsize_t_f *n, hid_t_f *lapl_id,
		int_f *corder_valid, int_f *corder,
		int_f *cset, hsize_t_f *data_size )
{
    char *c_obj_name = NULL;          /* Buffer to hold C string */
    H5A_info_t ainfo;
    int_f ret_value = 0;          /* Return value */

    /*
     * Convert FORTRAN name to C name
     */
    if(NULL == (c_obj_name = HD5f2cstring(obj_name, (size_t)*obj_namelen)))
        HGOTO_DONE(FAIL)

     /*
      * Call H5Ainfo_by_idx function.
      */
    if(H5Aget_info_by_idx((hid_t)*loc_id, c_obj_name, (H5_index_t)*idx_type, (H5_iter_order_t)*order, (hsize_t)*n,
            &ainfo, (hid_t)*lapl_id) < 0)
      HGOTO_DONE(FAIL)

    /* Unpack the structure */
    *corder_valid = 0;
    if(ainfo.corder_valid > 0)
        *corder_valid = 1;
    *corder = (int_f)ainfo.corder;
    *cset = (int_f)ainfo.cset;
    *data_size = (hsize_t)ainfo.data_size;

done:
    if(c_obj_name)
        HDfree(c_obj_name);

    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5aget_info_by_name_c
 * Purpose:     Call  H5Aget_info_by_name
 * Inputs:      loc_id - Object identifier
 *            obj_name - Name of object to which attribute is attached
 *         obj_namelen - name length
 *           attr_name - Attribute name
 *        attr_namelen - attribute name length
 *             lapl_id - Link access property list
 * Outputs:
 *        corder_valid - Indicates whether the the creation order data is valid for this attribute
 *              corder - Is a positive integer containing the creation order of the attribute
 *                cset - Indicates the character set used for the attribute’s name
 *           data_size - indicates the size, in the number of characters, of the attribute
 *
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              January, 2008
 * Modifications: N/A
 *---------------------------------------------------------------------------*/
int_f
nh5aget_info_by_name_c (hid_t_f *loc_id, _fcd obj_name, size_t_f *obj_namelen,
			_fcd attr_name, size_t_f *attr_namelen, hid_t_f *lapl_id,
			int_f *corder_valid, int_f *corder,
			int_f *cset, hsize_t_f *data_size )
{
    char *c_obj_name = NULL;          /* Buffer to hold C string */
    char *c_attr_name = NULL;          /* Buffer to hold C string */
    H5A_info_t ainfo;
    int_f ret_value = 0;          /* Return value */

    /*
     * Convert FORTRAN name to C name
     */
    if(NULL == (c_obj_name = HD5f2cstring(obj_name, (size_t)*obj_namelen)))
        HGOTO_DONE(FAIL)
    if(NULL == (c_attr_name = HD5f2cstring(attr_name, (size_t)*attr_namelen)))
        HGOTO_DONE(FAIL)

     /*
      * Call H5Ainfo_by_name function.
      */
    if(H5Aget_info_by_name((hid_t)*loc_id, c_obj_name, c_attr_name, &ainfo, (hid_t)*lapl_id) < 0)
        HGOTO_DONE(FAIL)

    /* Unpack the structure */
    *corder_valid = 0;
    if(ainfo.corder_valid > 0)
        *corder_valid = 1;
    *corder = (int_f)ainfo.corder;
    *cset = (int_f)ainfo.cset;
    *data_size = (hsize_t)ainfo.data_size;

done:
    if(c_obj_name)
        HDfree(c_obj_name);
    if(c_attr_name)
        HDfree(c_attr_name);

    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5acreate_by_name_c
 * Purpose:     Call h5acreate_by_name

 * Inputs:
 *         loc_id  - Object identifier
 *        obj_name - Name of object to which attribute is attached
 *     obj_namelen - name length
 *       attr_name - Attribute name
 *    attr_namelen - attribute name length
 *         type_id - Attribute datatype identifier
 *       space_id  - Attribute dataspace identifier
 *         acpl_id - Attribute creation property list identifier (Currently not used.)
 *         aapl_id - Attribute access property list identifier (Currently not used.)
 *         lapl_id - Link access property list
 *
 * Outputs:
 *            attr - an attribute identifier
 *
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              February, 2008
 * Modifications: N/A
 *---------------------------------------------------------------------------*/
int_f
nh5acreate_by_name_c(hid_t_f *loc_id, _fcd obj_name, size_t_f *obj_namelen,
		     _fcd attr_name, size_t_f *attr_namelen,  hid_t_f *type_id,
		     hid_t_f *space_id, hid_t_f *acpl_id, hid_t_f *aapl_id,
		     hid_t_f *lapl_id, hid_t_f *attr_id )
{
  char *c_obj_name = NULL;      /* Buffer to hold C string */
  char *c_attr_name = NULL;     /* Buffer to hold C string */
  int_f ret_value = 0;          /* Return value */

  /*
   * Convert FORTRAN name to C name
   */
  if((c_obj_name = HD5f2cstring(obj_name, (size_t)*obj_namelen)) == NULL)
    HGOTO_DONE(FAIL);
  if((c_attr_name = HD5f2cstring(attr_name, (size_t)*attr_namelen)) == NULL)
    HGOTO_DONE(FAIL);

  /*
   * Call H5Acreate_by_name function.
   */
  if((*attr_id = (hid_t_f)H5Acreate_by_name((hid_t)*loc_id, c_obj_name, c_attr_name,
					    (hid_t)*type_id, (hid_t)*space_id,(hid_t)*acpl_id,(hid_t)*aapl_id,(hid_t)*lapl_id )) < 0)
    HGOTO_DONE(FAIL);

done:
  if(c_obj_name)
    HDfree(c_obj_name);
  if(c_attr_name)
    HDfree(c_attr_name);
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5aexists_c
 * Purpose:     CAll h5aexists
 * Inputs:
 *             obj_id - Object identifier
 *          attr_name - Attribute name
 * Outputs:
 *     attr_exists_c  - returns a positive value, for TRUE, or 0 (zero), for FALSE.
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              February, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5aexists_c (hid_t_f *obj_id, _fcd name, size_t_f *namelen, hid_t_f *attr_exists)
{
  char *c_name = NULL;          /* Buffer to hold C string */
  int_f ret_value = 0;          /* Return value */

  /*
   * Convert FORTRAN name to C name
   */
  if((c_name = HD5f2cstring(name, (size_t)*namelen)) == NULL)
    HGOTO_DONE(FAIL);

  /*
   * Call H5Aexists function.
   */
  if((*attr_exists = (hid_t_f)H5Aexists((hid_t)*obj_id, c_name)) < 0)
         HGOTO_DONE(FAIL);

done:
    if(c_name)
        HDfree(c_name);
    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5aexists_by_name_c
 * Purpose:     CAll H5Aexists_by_name
 * Inputs:
 *     loc_id - Location identifier
 *   obj_name - Object name either relative to loc_id, absolute from the file’s root group, or '.' (a dot)
 *  attr_name - Attribute name
 *    lapl_id - Link access property list identifier
 * Outputs:
 *     attr_exists_c  - returns a positive value, for TRUE, or 0 (zero), for FALSE.
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              February, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5aexists_by_name_c (hid_t_f *loc_id, _fcd obj_name, size_t_f *obj_namelen, _fcd attr_name, size_t_f *attr_namelen,
		      hid_t_f *lapl_id, int_f *attr_exists)
{
  char *c_obj_name = NULL;          /* Buffer to hold object name C string */
  char *c_attr_name = NULL;          /* Buffer to hold attribute name C string */
  int_f ret_value = 0;          /* Return value */

  /*
   * Convert FORTRAN name to C name
   */
  if((c_obj_name = HD5f2cstring(obj_name, (size_t)*obj_namelen)) == NULL)
    HGOTO_DONE(FAIL);
  if((c_attr_name = HD5f2cstring(attr_name, (size_t)*attr_namelen)) == NULL)
    HGOTO_DONE(FAIL);

  /*
   * Call H5Aexists_by_name function.
   */
  if((*attr_exists = (int_f)H5Aexists_by_name((hid_t)*loc_id, c_obj_name, c_attr_name, (hid_t)*lapl_id)) < 0)
         HGOTO_DONE(FAIL);

done:
    if(c_obj_name)
        HDfree(c_obj_name);
    if(c_attr_name)
        HDfree(c_attr_name);
    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5aopen_by_name_c
 * Purpose:     Call H5Aopen_by_name
 * Inputs:
 *     loc_id - Location identifier
 *   obj_name - Object name either relative to loc_id, absolute from the file’s root group, or '.' (a dot)
 *  attr_name - Attribute name
 *    aapl_id - Attribute access property list (Currently unused; should be passed in as H5P_DEFAULT.)
 *    lapl_id - Link access property list identifier
 * Outputs:
 *     attr_id  - attribute identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              February, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5aopen_by_name_c (hid_t_f *loc_id, _fcd obj_name, size_t_f *obj_namelen, _fcd attr_name, size_t_f *attr_namelen,
		    hid_t_f *aapl_id, hid_t_f *lapl_id, hid_t_f *attr_id)
{
  char *c_obj_name = NULL;          /* Buffer to hold object name C string */
  char *c_attr_name = NULL;          /* Buffer to hold attribute name C string */
  int_f ret_value = 0;          /* Return value */

  /*
   * Convert FORTRAN name to C name
   */
  if((c_obj_name = HD5f2cstring(obj_name, (size_t)*obj_namelen)) == NULL)
    HGOTO_DONE(FAIL);
  if((c_attr_name = HD5f2cstring(attr_name, (size_t)*attr_namelen)) == NULL)
    HGOTO_DONE(FAIL);

  /*
   * Call H5Aopen function.
   */
  if((*attr_id = (hid_t_f)H5Aopen_by_name((hid_t)*loc_id, c_obj_name, c_attr_name, (hid_t)*aapl_id, (hid_t)*lapl_id)) < 0)
    HGOTO_DONE(FAIL);

 done:
  if(c_obj_name)
    HDfree(c_obj_name);
  if(c_attr_name)
    HDfree(c_attr_name);
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:      h5arename_c
 * Purpose:   Calls H5Arename
 * Inputs:    loc_id            - Object identifier
 *            old_attr_name     - Prior attribute name
 *            old_attr_name_len - Prior attribute name length
 *            new_attr_name     - New attribute name
 *            new_attr_name_len - New attribute name length
 * Outputs:     N/A
 * Returns:     0 on success, -1 on failure
 * Programmer:  M. S. Breitenfeld
 *              January, 2008
 * Modifications: N/A
 *---------------------------------------------------------------------------*/

int_f
nh5arename_c( hid_t_f *loc_id,
		      _fcd old_attr_name, size_t_f *old_attr_namelen,
		      _fcd new_attr_name, size_t_f *new_attr_namelen)
{
  char *c_old_attr_name = NULL;     /* Buffer to hold C string */
  char *c_new_attr_name = NULL;     /* Buffer to hold C string */
  int_f ret_value=0;          /* Return value */
  /*
   * Convert FORTRAN name to C name
   */
  if((c_old_attr_name = HD5f2cstring(old_attr_name, (size_t)*old_attr_namelen)) == NULL)
    HGOTO_DONE(FAIL);
  if((c_new_attr_name = HD5f2cstring(new_attr_name, (size_t)*new_attr_namelen)) == NULL)
    HGOTO_DONE(FAIL);

    if(H5Arename((hid_t)*loc_id,c_old_attr_name,c_new_attr_name) < 0)
        HGOTO_DONE(FAIL);

done:
    if(c_old_attr_name)
      HDfree(c_old_attr_name);
    if(c_new_attr_name)
      HDfree(c_new_attr_name);
    return ret_value;
}
