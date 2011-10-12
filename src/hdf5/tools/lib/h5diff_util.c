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

#include "H5private.h"
#include "h5tools.h"
#include "h5tools_utils.h"
#include "h5diff.h"
#include "ph5diff.h"


/* global variables */
int      g_nTasks = 1;

/*-------------------------------------------------------------------------
 * Function: print_dimensions
 *
 * Purpose: print dimensions
 *
 *-------------------------------------------------------------------------
 */
void
print_dimensions (int rank, hsize_t *dims)
{
    int  i;

    if ( rank > 0 )
    {

        parallel_print("[" );
        for ( i = 0; i < rank-1; i++)
        {
            parallel_print(HSIZE_T_FORMAT, dims[i]);
            parallel_print("x");
        }

        parallel_print(HSIZE_T_FORMAT,  dims[rank-1]);
        parallel_print("]" );
    }
    else
    {
        parallel_print("H5S_SCALAR" );
    }

}


/*-------------------------------------------------------------------------
 * Function: print_type
 *
 * Purpose: Print name of datatype
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 * Comments: Adapted from h5dump for H5T_INTEGER and H5T_FLOAT classes only
 *
 *-------------------------------------------------------------------------
 */
void print_type(hid_t type)
{
    switch (H5Tget_class(type))
    {
    default:
        return;
    case H5T_INTEGER:
        if (H5Tequal(type, H5T_STD_I8BE)) {
            parallel_print("H5T_STD_I8BE");
        } else if (H5Tequal(type, H5T_STD_I8LE)) {
            parallel_print("H5T_STD_I8LE");
        } else if (H5Tequal(type, H5T_STD_I16BE)) {
            parallel_print("H5T_STD_I16BE");
        } else if (H5Tequal(type, H5T_STD_I16LE)) {
            parallel_print("H5T_STD_I16LE");
        } else if (H5Tequal(type, H5T_STD_I32BE)) {
            parallel_print("H5T_STD_I32BE");
        } else if (H5Tequal(type, H5T_STD_I32LE)) {
            parallel_print("H5T_STD_I32LE");
        } else if (H5Tequal(type, H5T_STD_I64BE)) {
            parallel_print("H5T_STD_I64BE");
        } else if (H5Tequal(type, H5T_STD_I64LE)) {
            parallel_print("H5T_STD_I64LE");
        } else if (H5Tequal(type, H5T_STD_U8BE)) {
            parallel_print("H5T_STD_U8BE");
        } else if (H5Tequal(type, H5T_STD_U8LE)) {
            parallel_print("H5T_STD_U8LE");
        } else if (H5Tequal(type, H5T_STD_U16BE)) {
            parallel_print("H5T_STD_U16BE");
        } else if (H5Tequal(type, H5T_STD_U16LE)) {
            parallel_print("H5T_STD_U16LE");
        } else if (H5Tequal(type, H5T_STD_U32BE)) {
            parallel_print("H5T_STD_U32BE");
        } else if (H5Tequal(type, H5T_STD_U32LE)) {
            parallel_print("H5T_STD_U32LE");
        } else if (H5Tequal(type, H5T_STD_U64BE)) {
            parallel_print("H5T_STD_U64BE");
        } else if (H5Tequal(type, H5T_STD_U64LE)) {
            parallel_print("H5T_STD_U64LE");
        } else if (H5Tequal(type, H5T_NATIVE_SCHAR)) {
            parallel_print("H5T_NATIVE_SCHAR");
        } else if (H5Tequal(type, H5T_NATIVE_UCHAR)) {
            parallel_print("H5T_NATIVE_UCHAR");
        } else if (H5Tequal(type, H5T_NATIVE_SHORT)) {
            parallel_print("H5T_NATIVE_SHORT");
        } else if (H5Tequal(type, H5T_NATIVE_USHORT)) {
            parallel_print("H5T_NATIVE_USHORT");
        } else if (H5Tequal(type, H5T_NATIVE_INT)) {
            parallel_print("H5T_NATIVE_INT");
        } else if (H5Tequal(type, H5T_NATIVE_UINT)) {
            parallel_print("H5T_NATIVE_UINT");
        } else if (H5Tequal(type, H5T_NATIVE_LONG)) {
            parallel_print("H5T_NATIVE_LONG");
        } else if (H5Tequal(type, H5T_NATIVE_ULONG)) {
            parallel_print("H5T_NATIVE_ULONG");
        } else if (H5Tequal(type, H5T_NATIVE_LLONG)) {
            parallel_print("H5T_NATIVE_LLONG");
        } else if (H5Tequal(type, H5T_NATIVE_ULLONG)) {
            parallel_print("H5T_NATIVE_ULLONG");
        } else {
            parallel_print("undefined integer");
        }
        break;

    case H5T_FLOAT:
        if (H5Tequal(type, H5T_IEEE_F32BE)) {
            parallel_print("H5T_IEEE_F32BE");
        } else if (H5Tequal(type, H5T_IEEE_F32LE)) {
            parallel_print("H5T_IEEE_F32LE");
        } else if (H5Tequal(type, H5T_IEEE_F64BE)) {
            parallel_print("H5T_IEEE_F64BE");
        } else if (H5Tequal(type, H5T_IEEE_F64LE)) {
            parallel_print("H5T_IEEE_F64LE");
        } else if (H5Tequal(type, H5T_NATIVE_FLOAT)) {
            parallel_print("H5T_NATIVE_FLOAT");
        } else if (H5Tequal(type, H5T_NATIVE_DOUBLE)) {
            parallel_print("H5T_NATIVE_DOUBLE");
#if H5_SIZEOF_LONG_DOUBLE !=0
        } else if (H5Tequal(type, H5T_NATIVE_LDOUBLE)) {
            parallel_print("H5T_NATIVE_LDOUBLE");
#endif
        } else {
            parallel_print("undefined float");
        }
        break;

    }/*switch*/
}

/*-------------------------------------------------------------------------
 * Function: diff_basename
 *
 * Purpose: Returns a pointer to the last component absolute name
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 *-------------------------------------------------------------------------
 */
const char*
diff_basename(const char *name)
{
    size_t i;

    if (name==NULL)
        return NULL;

    /* Find the end of the base name */
    i = strlen(name);
    while (i>0 && '/'==name[i-1])
        --i;

    /* Skip backward over base name */
    while (i>0 && '/'!=name[i-1])
        --i;

    return(name+i);
}

/*-------------------------------------------------------------------------
 * Function: get_type
 *
 * Purpose: Returns the type as a string
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 *-------------------------------------------------------------------------
 */
const char*
get_type(h5trav_type_t type)
{
    switch(type) {
        case H5TRAV_TYPE_DATASET:
            return("H5G_DATASET");
        case H5TRAV_TYPE_GROUP:
            return("H5G_GROUP");
        case H5TRAV_TYPE_NAMED_DATATYPE:
            return("H5G_TYPE");
        case H5TRAV_TYPE_LINK:
            return("H5G_LINK");
        case H5TRAV_TYPE_UDLINK:
            return("H5G_UDLINK");
        default:
            return("unknown type");
    }
}

/*-------------------------------------------------------------------------
 * Function: get_sign
 *
 * Purpose: Returns the sign as a string
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 * Comments:
 *
 *-------------------------------------------------------------------------
 */
const char*
get_sign(H5T_sign_t sign)
{
    switch (sign)
    {
    default:
        return("H5T_SGN_ERROR");
    case H5T_SGN_NONE:
        return("H5T_SGN_NONE");
    case H5T_SGN_2:
        return("H5T_SGN_2");
    }
}


/*-------------------------------------------------------------------------
 * Function: get_class
 *
 * Purpose: Returns the class as a string
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 *-------------------------------------------------------------------------
 */
const char*
get_class(H5T_class_t tclass)
{
    switch (tclass)
    {
    default:
        return("Invalid class");
    case H5T_TIME:
        return("H5T_TIME");
    case H5T_INTEGER:
        return("H5T_INTEGER");
    case H5T_FLOAT:
        return("H5T_FLOAT");
    case H5T_STRING:
        return("H5T_STRING");
    case H5T_BITFIELD:
        return("H5T_BITFIELD");
    case H5T_OPAQUE:
        return("H5T_OPAQUE");
    case H5T_COMPOUND:
        return("H5T_COMPOUND");
    case H5T_REFERENCE:
        return("H5T_REFERENCE");
    case H5T_ENUM:
        return("H5T_ENUM");
    case H5T_VLEN:
        return("H5T_VLEN");
    case H5T_ARRAY:
        return("H5T_ARRAY");
    }
}

/*-------------------------------------------------------------------------
 * Function: print_found
 *
 * Purpose: print number of differences found
 *
 *-------------------------------------------------------------------------
 */
void print_found(hsize_t nfound)
{
 if(g_Parallel)
  parallel_print("%"H5_PRINTF_LL_WIDTH"u differences found\n", (unsigned long long)nfound);
 else
  HDfprintf(stdout,"%Hu differences found\n",nfound);
}



