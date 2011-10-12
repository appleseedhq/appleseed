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

#include "h5tools.h"

/*-------------------------------------------------------------------------
 * Function: h5tools_get_native_type
 *
 * Purpose: Wrapper around H5Tget_native_type() to work around
 *  Problems with bitfields.
 *
 * Return: Success:    datatype ID
 *
 *  Failure:    FAIL
 *
 * Programmer: Quincey Koziol
 *              Tuesday, October  5, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
h5tools_get_native_type(hid_t type)
{
    hid_t p_type;
    H5T_class_t type_class;

    type_class = H5Tget_class(type);
    if(type_class==H5T_BITFIELD)
        p_type=H5Tcopy(type);
    else
        p_type = H5Tget_native_type(type,H5T_DIR_DEFAULT);

    return(p_type);
}


/*-------------------------------------------------------------------------
 * Function: h5tools_get_little_endian_type
 *
 * Purpose: Get a little endian type from a file type
 *
 * Return: Success:    datatype ID
 *         Failure:    FAIL
 *
 * Programmer: Pedro Vicente Nunes
 *             Tuesday, July 18, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
h5tools_get_little_endian_type(hid_t tid)
{
 hid_t       p_type=-1;
 H5T_class_t type_class;
 size_t      size;
 H5T_sign_t  sign;

 type_class = H5Tget_class(tid);
 size       = H5Tget_size(tid);
 sign       = H5Tget_sign(tid);

 switch( type_class )
 {
 case H5T_INTEGER:
  {
   if ( size == 1 && sign == H5T_SGN_2)
    p_type=H5Tcopy(H5T_STD_I8LE);
   else if ( size == 2 && sign == H5T_SGN_2)
    p_type=H5Tcopy(H5T_STD_I16LE);
   else if ( size == 4 && sign == H5T_SGN_2)
    p_type=H5Tcopy(H5T_STD_I32LE);
   else if ( size == 8 && sign == H5T_SGN_2)
    p_type=H5Tcopy(H5T_STD_I64LE);
   else if ( size == 1 && sign == H5T_SGN_NONE)
    p_type=H5Tcopy(H5T_STD_U8LE);
   else if ( size == 2 && sign == H5T_SGN_NONE)
    p_type=H5Tcopy(H5T_STD_U16LE);
   else if ( size == 4 && sign == H5T_SGN_NONE)
    p_type=H5Tcopy(H5T_STD_U32LE);
   else if ( size == 8 && sign == H5T_SGN_NONE)
    p_type=H5Tcopy(H5T_STD_U64LE);
  }
  break;

 case H5T_FLOAT:
  if ( size == 4)
   p_type=H5Tcopy(H5T_IEEE_F32LE);
  else if ( size == 8)
   p_type=H5Tcopy(H5T_IEEE_F64LE);
  break;

 case H5T_TIME:
 case H5T_BITFIELD:
 case H5T_OPAQUE:
 case H5T_STRING:
 case H5T_COMPOUND:
 case H5T_REFERENCE:
 case H5T_ENUM:
 case H5T_VLEN:
 case H5T_ARRAY:
  break;

 default:
  break;

 }

 return(p_type);
}


/*-------------------------------------------------------------------------
 * Function: h5tools_get_big_endian_type
 *
 * Purpose: Get a big endian type from a file type
 *
 * Return: Success:    datatype ID
 *         Failure:    FAIL
 *
 * Programmer: Pedro Vicente Nunes
 *             Tuesday, July 18, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
h5tools_get_big_endian_type(hid_t tid)
{
 hid_t       p_type=-1;
 H5T_class_t type_class;
 size_t      size;
 H5T_sign_t  sign;

 type_class = H5Tget_class(tid);
 size       = H5Tget_size(tid);
 sign       = H5Tget_sign(tid);

 switch( type_class )
 {
 case H5T_INTEGER:
  {
   if ( size == 1 && sign == H5T_SGN_2)
    p_type=H5Tcopy(H5T_STD_I8BE);
   else if ( size == 2 && sign == H5T_SGN_2)
    p_type=H5Tcopy(H5T_STD_I16BE);
   else if ( size == 4 && sign == H5T_SGN_2)
    p_type=H5Tcopy(H5T_STD_I32BE);
   else if ( size == 8 && sign == H5T_SGN_2)
    p_type=H5Tcopy(H5T_STD_I64BE);
   else if ( size == 1 && sign == H5T_SGN_NONE)
    p_type=H5Tcopy(H5T_STD_U8BE);
   else if ( size == 2 && sign == H5T_SGN_NONE)
    p_type=H5Tcopy(H5T_STD_U16BE);
   else if ( size == 4 && sign == H5T_SGN_NONE)
    p_type=H5Tcopy(H5T_STD_U32BE);
   else if ( size == 8 && sign == H5T_SGN_NONE)
    p_type=H5Tcopy(H5T_STD_U64BE);
  }
  break;

 case H5T_FLOAT:
  if ( size == 4)
   p_type=H5Tcopy(H5T_IEEE_F32BE);
  else if ( size == 8)
   p_type=H5Tcopy(H5T_IEEE_F64BE);
  break;

 case H5T_TIME:
 case H5T_BITFIELD:
 case H5T_OPAQUE:
 case H5T_STRING:
 case H5T_COMPOUND:
 case H5T_REFERENCE:
 case H5T_ENUM:
 case H5T_VLEN:
 case H5T_ARRAY:
  break;

 default:
  break;

 }


 return(p_type);
}

