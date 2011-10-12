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

/* This files contains C stubs for H5D Fortran APIs */

#include "H5LTprivate.h"
#include "H5LTf90proto.h"



/*-------------------------------------------------------------------------
* Function: H5LTmake_dataset_c
*
* Purpose: Call H5LTmake_dataset
*
* Return: Success: 0, Failure: -1
*
* Programmer: pvn@ncsa.uiuc.edu
*
* Date: September 09, 2004
*
* Comments:
*
* Modifications:
*
*
*-------------------------------------------------------------------------
*/

int_f
nh5ltmake_dataset_c (hid_t_f *loc_id,
                     int_f *namelen,
                     _fcd name,
                     int_f *rank,
                     hsize_t_f *dims,
                     hid_t_f *type_id,
                     void *buf)
{
    int     ret_value = -1;
    herr_t  ret;
    hid_t   c_loc_id;
    hid_t   c_type_id;
    char    *c_name = NULL;
    hsize_t *c_dims = NULL;
    int     c_namelen;
    int     i;

    /*
    * convert FORTRAN name to C name
    */
    c_namelen = *namelen;
    c_name = (char *)HD5f2cstring(name, c_namelen);
    if (c_name == NULL)
        goto done;

    c_dims =  malloc(sizeof(hsize_t) * (*rank ));
    if (c_dims == NULL)
        goto done;

    /*
    * transpose dimension arrays because of C-FORTRAN storage order
    */
    for (i = 0; i < *rank ; i++)
    {
        c_dims[i] =  dims[*rank - i - 1];
    }

    /*
    * call H5LTmake_dataset function.
    */
    c_loc_id = (hid_t)*loc_id;
    c_type_id = (hid_t)*type_id;

    ret = H5LTmake_dataset(c_loc_id, c_name, *rank, c_dims, c_type_id, buf );

    if (ret < 0)
        goto done;

    ret_value = 0;

done:
    if(c_name!=NULL)
        free(c_name);
    if(c_dims!=NULL)
        free(c_dims);
    return ret_value;
}

int_f
nh5ltmake_dataset_int1_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_int2_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_int3_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_int4_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_int5_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_int6_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_int7_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_fl1_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *rank,
                         hsize_t_f *dims,
                         hid_t_f *type_id,
                         void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_fl2_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *rank,
                         hsize_t_f *dims,
                         hid_t_f *type_id,
                         void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_fl3_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *rank,
                         hsize_t_f *dims,
                         hid_t_f *type_id,
                         void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_fl4_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *rank,
                         hsize_t_f *dims,
                         hid_t_f *type_id,
                         void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_fl5_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *rank,
                         hsize_t_f *dims,
                         hid_t_f *type_id,
                         void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_fl6_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *rank,
                         hsize_t_f *dims,
                         hid_t_f *type_id,
                         void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_fl7_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *rank,
                         hsize_t_f *dims,
                         hid_t_f *type_id,
                         void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_dl1_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *rank,
                         hsize_t_f *dims,
                         hid_t_f *type_id,
                         void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_dl2_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *rank,
                         hsize_t_f *dims,
                         hid_t_f *type_id,
                         void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_dl3_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *rank,
                         hsize_t_f *dims,
                         hid_t_f *type_id,
                         void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_dl4_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *rank,
                         hsize_t_f *dims,
                         hid_t_f *type_id,
                         void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_dl5_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *rank,
                         hsize_t_f *dims,
                         hid_t_f *type_id,
                         void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_dl6_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *rank,
                         hsize_t_f *dims,
                         hid_t_f *type_id,
                         void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_dl7_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *rank,
                         hsize_t_f *dims,
                         hid_t_f *type_id,
                         void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_nint1_c (hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           int_f *rank,
                           hsize_t_f *dims,
                           hid_t_f *type_id,
                           void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_nint2_c (hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           int_f *rank,
                           hsize_t_f *dims,
                           hid_t_f *type_id,
                           void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_nint3_c (hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           int_f *rank,
                           hsize_t_f *dims,
                           hid_t_f *type_id,
                           void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_nint4_c (hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           int_f *rank,
                           hsize_t_f *dims,
                           hid_t_f *type_id,
                           void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_nint5_c (hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           int_f *rank,
                           hsize_t_f *dims,
                           hid_t_f *type_id,
                           void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_nint6_c (hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           int_f *rank,
                           hsize_t_f *dims,
                           hid_t_f *type_id,
                           void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_nint7_c (hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           int_f *rank,
                           hsize_t_f *dims,
                           hid_t_f *type_id,
                           void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_nfl1_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_nfl2_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_nfl3_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_nfl4_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_nfl5_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_nfl6_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_nfl7_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_ndl1_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_ndl2_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_ndl3_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_ndl4_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_ndl5_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_ndl6_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
int_f
nh5ltmake_dataset_ndl7_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf)
{
    return nh5ltmake_dataset_c (loc_id, namelen, name, rank, dims, type_id, buf);
}
/*-------------------------------------------------------------------------
* Function: H5LTread_dataset_c
*
* Purpose: Call H5LTmake_dataset
*
* Return: Success: 0, Failure: -1
*
* Programmer: pvn@ncsa.uiuc.edu
*
* Date: September 09, 2004
*
* Comments:
*
* Modifications:
*
*
*-------------------------------------------------------------------------
*/

int_f
nh5ltread_dataset_c (hid_t_f *loc_id,
                     int_f *namelen,
                     _fcd name,
                     hid_t_f *type_id,
                     void *buf,
                     hsize_t_f *dims)
{
    int     ret_value = -1;
    herr_t  ret;
    hid_t   c_loc_id;
    hid_t   c_type_id;
    char    *c_name = NULL;
    int     c_namelen;

    /*
    * convert FORTRAN name to C name
    */
    c_namelen = *namelen;
    c_name = (char *)HD5f2cstring(name, c_namelen);
    if (c_name == NULL)
        goto done;

    /*
    * call H5LTread_dataset function.
    */
    c_loc_id = (hid_t)*loc_id;
    c_type_id = (hid_t)*type_id;

    ret = H5LTread_dataset(c_loc_id, c_name, c_type_id, buf );

    if (ret < 0)
        goto done;

    ret_value = 0;

done:
    if(c_name!=NULL)
        free(c_name);

    return ret_value;
}

int_f
nh5ltread_dataset_int1_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_int2_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_int3_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_int4_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_int5_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_int6_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_int7_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_fl1_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         hid_t_f *type_id,
                         void *buf,
                         hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_fl2_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         hid_t_f *type_id,
                         void *buf,
                         hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_fl3_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         hid_t_f *type_id,
                         void *buf,
                         hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_fl4_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         hid_t_f *type_id,
                         void *buf,
                         hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_fl5_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         hid_t_f *type_id,
                         void *buf,
                         hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_fl6_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         hid_t_f *type_id,
                         void *buf,
                         hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_fl7_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         hid_t_f *type_id,
                         void *buf,
                         hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_dl1_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         hid_t_f *type_id,
                         void *buf,
                         hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_dl2_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         hid_t_f *type_id,
                         void *buf,
                         hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_dl3_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         hid_t_f *type_id,
                         void *buf,
                         hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_dl4_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         hid_t_f *type_id,
                         void *buf,
                         hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_dl5_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         hid_t_f *type_id,
                         void *buf,
                         hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_dl6_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         hid_t_f *type_id,
                         void *buf,
                         hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_dl7_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         hid_t_f *type_id,
                         void *buf,
                         hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_nint1_c (hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           hid_t_f *type_id,
                           void *buf,
                           hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_nint2_c (hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           hid_t_f *type_id,
                           void *buf,
                           hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_nint3_c (hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           hid_t_f *type_id,
                           void *buf,
                           hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_nint4_c (hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           hid_t_f *type_id,
                           void *buf,
                           hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_nint5_c (hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           hid_t_f *type_id,
                           void *buf,
                           hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_nint6_c (hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           hid_t_f *type_id,
                           void *buf,
                           hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_nint7_c (hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           hid_t_f *type_id,
                           void *buf,
                           hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_nfl1_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_nfl2_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_nfl3_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_nfl4_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_nfl5_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_nfl6_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_nfl7_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_ndl1_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_ndl2_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_ndl3_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_ndl4_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_ndl5_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_ndl6_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
int_f
nh5ltread_dataset_ndl7_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims)
{
    return nh5ltread_dataset_c(loc_id, namelen, name, type_id, buf, dims);
}
/*-------------------------------------------------------------------------
* Function: H5LTmake_dataset_string_c
*
* Purpose: Call H5LTmake_dataset
*
* Return: Success: 0, Failure: -1
*
* Programmer: pvn@ncsa.uiuc.edu
*
* Date: September 09, 2004
*
* Comments:
*
* Modifications:
*
*
*-------------------------------------------------------------------------
*/

int_f
nh5ltmake_dataset_string_c (hid_t_f *loc_id,
                            int_f *namelen,
                            _fcd name,
                            int_f *buflen,
                            char *buf)
{
    int     ret_value = -1;
    herr_t  ret;
    hid_t   c_loc_id;
    char    *c_name = NULL;
    int     c_namelen;
    char    *c_buf = NULL;
    int     c_buflen;

    /*
    * convert FORTRAN name to C name
    */
    c_namelen = *namelen;
    c_name = (char *)HD5f2cstring(name, c_namelen);
    if (c_name == NULL)
        goto done;

    c_buflen = *buflen;
    c_buf = (char *)HD5f2cstring(buf, c_buflen);
    if (c_buf == NULL)
        goto done;

    /*
    * call H5LTmake_dataset_string function.
    */
    c_loc_id = (hid_t)*loc_id;

    ret = H5LTmake_dataset_string(c_loc_id,c_name,c_buf);

    if (ret < 0)
        goto done;

    ret_value = 0;

done:
    if(c_name!=NULL)
        free(c_name);
    if(c_buf!=NULL)
        free(c_buf);

    return ret_value;
}


/*-------------------------------------------------------------------------
* Function: H5LTread_dataset_string_c
*
* Purpose: Call H5LTread_dataset_string
*
* Return: Success: 0, Failure: -1
*
* Programmer: pvn@ncsa.uiuc.edu
*
* Date: September 09, 2004
*
* Comments:
*
* Modifications:
*
*
*-------------------------------------------------------------------------
*/

int_f
nh5ltread_dataset_string_c (hid_t_f *loc_id,
                            int_f *namelen,
                            _fcd name,
                            char *buf)
{
    int     ret_value = -1;
    herr_t  ret;
    hid_t   c_loc_id;
    char    *c_name = NULL;
    int     c_namelen;

    /*
    * convert FORTRAN name to C name
    */
    c_namelen = *namelen;
    c_name = (char *)HD5f2cstring(name, c_namelen);
    if (c_name == NULL)
        goto done;

    /*
    * call H5LTread_dataset_string function.
    */
    c_loc_id = (hid_t)*loc_id;

    ret = H5LTread_dataset_string(c_loc_id,c_name,buf);

    if (ret < 0)
        goto done;

    ret_value = 0;

done:
    if(c_name!=NULL)
        free(c_name);

    return ret_value;
}



/*-------------------------------------------------------------------------
* Function: H5LTset_attribute_int_c
*
* Purpose: Call H5LTset_attribute_int
*
* Return: Success: 0, Failure: -1
*
* Programmer: pvn@ncsa.uiuc.edu
*
* Date: October 05, 2004
*
* Comments:
*
* Modifications:
*
*
*-------------------------------------------------------------------------
*/

int_f
nh5ltset_attribute_int_c(hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd dsetname,
                         int_f *attrnamelen,
                         _fcd attrname,
                         size_t_f *size,
                         void *buf)
{
    int     ret_value = -1;
    herr_t  ret;
    hid_t   c_loc_id;
    char    *c_name = NULL;
    char    *c_attrname = NULL;
    int     c_namelen;
    int     c_attrnamelen;
    size_t  c_size;

    /*
    * convert FORTRAN name to C name
    */
    c_namelen = *namelen;
    c_name = (char *)HD5f2cstring(dsetname, c_namelen);
    if (c_name == NULL)
        goto done;

    c_attrnamelen = (int)*attrnamelen;
    c_attrname = (char *)HD5f2cstring(attrname, c_attrnamelen);
    if (c_attrname == NULL)
        goto done;

    /*
    * call H5LTset_attribute_int function.
    */
    c_loc_id = (hid_t)*loc_id;
    c_size   = (size_t)*size;

    if (sizeof(int_f) == sizeof(int))
        ret = H5LTset_attribute_int(c_loc_id,c_name,c_attrname,buf,c_size);
    else if (sizeof(int_f) == sizeof(long))
        ret = H5LTset_attribute_long(c_loc_id,c_name,c_attrname,buf,c_size);
    else if (sizeof(int_f) == sizeof(long long))
        ret = H5LTset_attribute_long_long(c_loc_id,c_name,c_attrname,buf,c_size);
    else
        goto done;

    if (ret < 0)
        goto done;

    ret_value = 0;

done:
    if(c_name!=NULL)
        free(c_name);
     if(c_attrname!=NULL)
        free(c_attrname);

    return ret_value;
}

/*-------------------------------------------------------------------------
* Function: H5LTset_attribute_float_c
*
* Purpose: Call H5LTset_attribute_float
*
* Return: Success: 0, Failure: -1
*
* Programmer: pvn@ncsa.uiuc.edu
*
* Date: October 05, 2004
*
* Comments:
*
* Modifications:
*
*
*-------------------------------------------------------------------------
*/

int_f
nh5ltset_attribute_float_c(hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd dsetname,
                           int_f *attrnamelen,
                           _fcd attrname,
                           size_t_f *size,
                           void *buf)
{
    int     ret_value = -1;
    herr_t  ret;
    hid_t   c_loc_id;
    char    *c_name = NULL;
    char    *c_attrname = NULL;
    int     c_namelen;
    int     c_attrnamelen;
    size_t  c_size;

    /*
    * convert FORTRAN name to C name
    */
    c_namelen = *namelen;
    c_name = (char *)HD5f2cstring(dsetname, c_namelen);
    if (c_name == NULL)
         goto done;

    c_attrnamelen = *attrnamelen;
    c_attrname = (char *)HD5f2cstring(attrname, c_attrnamelen);
    if (c_attrname == NULL)
         goto done;

    /*
    * Call H5LTset_attribute_float function.
    */
    c_loc_id = (hid_t)*loc_id;
    c_size   = (size_t)*size;

    ret = H5LTset_attribute_float(c_loc_id,c_name,c_attrname,buf,c_size);

    if (ret < 0)
         goto done;

    ret_value = 0;

done:
    if(c_name!=NULL)
        free(c_name);
    if(c_attrname!=NULL)
        free(c_attrname);

    return ret_value;
}


/*-------------------------------------------------------------------------
* Function: H5LTset_attribute_double_c
*
* Purpose: Call H5LTset_attribute_double
*
* Return: Success: 0, Failure: -1
*
* Programmer: pvn@ncsa.uiuc.edu
*
* Date: October 05, 2004
*
* Comments:
*
* Modifications:
*
*
*-------------------------------------------------------------------------
*/

int_f
nh5ltset_attribute_double_c(hid_t_f *loc_id,
                            int_f *namelen,
                            _fcd dsetname,
                            int_f *attrnamelen,
                            _fcd attrname,
                            size_t_f *size,
                            void *buf)
{
    int     ret_value = -1;
    herr_t  ret;
    hid_t   c_loc_id;
    char    *c_name = NULL;
    char    *c_attrname = NULL;
    int     c_namelen;
    int     c_attrnamelen;
    size_t  c_size;

    /*
    * Convert FORTRAN name to C name
    */
    c_namelen = *namelen;
    c_name = (char *)HD5f2cstring(dsetname, c_namelen);
    if (c_name == NULL)
        goto done;

    c_attrnamelen = *attrnamelen;
    c_attrname = (char *)HD5f2cstring(attrname, c_attrnamelen);
    if (c_attrname == NULL)
        goto done;

    /*
    * Call H5LTset_attribute_double function.
    */
    c_loc_id = (hid_t)*loc_id;
    c_size   = (size_t)*size;

    ret = H5LTset_attribute_double(c_loc_id,c_name,c_attrname,buf,c_size);

    if (ret < 0)
        goto done;

    ret_value = 0;


done:
    if(c_name!=NULL)
        free(c_name);
    if(c_attrname!=NULL)
        free(c_attrname);

    return ret_value;
}

/*-------------------------------------------------------------------------
* Function: H5LTset_attribute_string_c
*
* Purpose: Call H5LTset_attribute_string
*
* Return: Success: 0, Failure: -1
*
* Programmer: pvn@ncsa.uiuc.edu
*
* Date: October 05, 2004
*
* Comments:
*
* Modifications:
*
*
*-------------------------------------------------------------------------
*/

int_f
nh5ltset_attribute_string_c(hid_t_f *loc_id,
                            int_f *namelen,
                            _fcd dsetname,
                            int_f *attrnamelen,
                            _fcd attrname,
                            int_f *buflen,
                            void *buf)
{
    int     ret_value = -1;
    herr_t  ret;
    hid_t   c_loc_id;
    char    *c_name = NULL;
    char    *c_attrname = NULL;
    int     c_namelen;
    int     c_attrnamelen;
    char    *c_buf = NULL;
    int     c_buflen;

    /*
    * convert FORTRAN name to C name
    */
    c_namelen = *namelen;
    c_name = (char *)HD5f2cstring(dsetname, c_namelen);
    if (c_name == NULL)
        goto done;

    c_attrnamelen = *attrnamelen;
    c_attrname = (char *)HD5f2cstring(attrname, c_attrnamelen);
    if (c_attrname == NULL)
        goto done;

    c_buflen = *buflen;
    c_buf = (char *)HD5f2cstring(buf, c_buflen);
    if (c_buf == NULL)
        goto done;


    /*
    * call H5LTset_attribute_string function.
    */
    c_loc_id = (hid_t)*loc_id;

    ret = H5LTset_attribute_string(c_loc_id,c_name,c_attrname,c_buf);

    if (ret < 0)
        goto done;

    ret_value = 0;


done:
    if(c_name!=NULL)
        free(c_name);
    if(c_attrname!=NULL)
        free(c_attrname);
    if(c_buf!=NULL)
        free(c_buf);

    return ret_value;
}

/*-------------------------------------------------------------------------
* Function: H5LTget_attribute_int_c
*
* Purpose: Call H5LTget_attribute_int
*
* Return: Success: 0, Failure: -1
*
* Programmer: pvn@ncsa.uiuc.edu
*
* Date: October 05, 2004
*
* Comments:
*
* Modifications:
*
*
*-------------------------------------------------------------------------
*/

int_f
nh5ltget_attribute_int_c(hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd dsetname,
                         int_f *attrnamelen,
                         _fcd attrname,
                         void *buf)
{
    int     ret_value = -1;
    herr_t  ret;
    hid_t   c_loc_id;
    char    *c_name = NULL;
    char    *c_attrname = NULL;
    int     c_namelen;
    int     c_attrnamelen;

    /*
    * convert FORTRAN name to C name
    */
    c_namelen = (int)*namelen;
    c_name = (char *)HD5f2cstring(dsetname, c_namelen);
    if (c_name == NULL)
        goto done;

    c_attrnamelen = (int)*attrnamelen;
    c_attrname = (char *)HD5f2cstring(attrname, c_attrnamelen);
    if (c_attrname == NULL)
        goto done;

    /*
    * call H5LTget_attribute_int function.
    */
    c_loc_id = (hid_t)*loc_id;

    if(sizeof(int_f) == sizeof(int))
        ret = H5LTget_attribute_int(c_loc_id,c_name,c_attrname,buf);
    else if (sizeof(int_f) == sizeof(long))
        ret = H5LTget_attribute_long(c_loc_id,c_name,c_attrname,buf);
    else if (sizeof(int_f) == sizeof(long long))
        ret = H5LTget_attribute_long_long(c_loc_id,c_name,c_attrname,buf);
    else
        goto done;

    if (ret < 0)
        goto done;

    ret_value = 0;

done:
    if(c_name!=NULL)
        free(c_name);
    if(c_attrname!=NULL)
        free(c_attrname);


    return ret_value;
}


/*-------------------------------------------------------------------------
* Function: H5LTget_attribute_float_c
*
* Purpose: Call H5LTget_attribute_float
*
* Return: Success: 0, Failure: -1
*
* Programmer: pvn@ncsa.uiuc.edu
*
* Date: October 05, 2004
*
* Comments:
*
* Modifications:
*
*
*-------------------------------------------------------------------------
*/

int_f
nh5ltget_attribute_float_c(hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd dsetname,
                           int_f *attrnamelen,
                           _fcd attrname,
                           void *buf)
{
    int     ret_value = -1;
    herr_t  ret;
    hid_t   c_loc_id;
    char    *c_name = NULL;
    char    *c_attrname = NULL;
    int     c_namelen;
    int     c_attrnamelen;

    /*
    * convert FORTRAN name to C name
    */
    c_namelen = (int)*namelen;
    c_name = (char *)HD5f2cstring(dsetname, c_namelen);
    if (c_name == NULL)
        goto done;

    c_attrnamelen = (int)*attrnamelen;
    c_attrname = (char *)HD5f2cstring(attrname, c_attrnamelen);
    if (c_attrname == NULL)
        goto done;

    /*
    * call H5LTget_attribute_int function.
    */
    c_loc_id = (hid_t)*loc_id;

    ret = H5LTget_attribute_float(c_loc_id,c_name,c_attrname,buf);

    if (ret < 0)
        goto done;

    ret_value = 0;

done:
    if(c_name!=NULL)
        free(c_name);
    if(c_attrname!=NULL)
        free(c_attrname);

    return ret_value;
}

/*-------------------------------------------------------------------------
* Function: H5LTget_attribute_double_c
*
* Purpose: Call H5LTget_attribute_double
*
* Return: Success: 0, Failure: -1
*
* Programmer: pvn@ncsa.uiuc.edu
*
* Date: October 05, 2004
*
* Comments:
*
* Modifications:
*
*
*-------------------------------------------------------------------------
*/

int_f
nh5ltget_attribute_double_c(hid_t_f *loc_id,
                            int_f *namelen,
                            _fcd dsetname,
                            int_f *attrnamelen,
                            _fcd attrname,
                            void *buf)
{
    int     ret_value = -1;
    herr_t  ret;
    hid_t   c_loc_id;
    char    *c_name = NULL;
    char    *c_attrname = NULL;
    int     c_namelen;
    int     c_attrnamelen;

    /*
    * convert FORTRAN name to C name
    */
    c_namelen = (int)*namelen;
    c_name = (char *)HD5f2cstring(dsetname, c_namelen);
    if (c_name == NULL)
        goto done;

    c_attrnamelen = (int)*attrnamelen;
    c_attrname = (char *)HD5f2cstring(attrname, c_attrnamelen);
    if (c_attrname == NULL)
        goto done;

    /*
    * call H5LTget_attribute_int function.
    */
    c_loc_id = (hid_t)*loc_id;

    ret = H5LTget_attribute_double(c_loc_id,c_name,c_attrname,buf);

    if (ret < 0)
        goto done;

    ret_value = 0;


done:
    if(c_name!=NULL)
        free(c_name);
    if(c_attrname!=NULL)
        free(c_attrname);

    return ret_value;
}

/*-------------------------------------------------------------------------
* Function: H5LTget_attribute_string_c
*
* Purpose: Call H5LTget_attribute_string
*
* Return: Success: 0, Failure: -1
*
* Programmer: pvn@ncsa.uiuc.edu
*
* Date: October 05, 2004
*
* Comments:
*
* Modifications:
*
*
*-------------------------------------------------------------------------
*/

int_f
nh5ltget_attribute_string_c(hid_t_f *loc_id,
                            int_f *namelen,
                            _fcd dsetname,
                            int_f *attrnamelen,
                            _fcd attrname,
                            void *buf)
{
    int     ret_value = -1;
    herr_t  ret;
    hid_t   c_loc_id;
    char    *c_name = NULL;
    char    *c_attrname = NULL;
    int     c_namelen;
    int     c_attrnamelen;

    /*
    * Convert FORTRAN name to C name
    */
    c_namelen = (int)*namelen;
    c_name = (char *)HD5f2cstring(dsetname, c_namelen);
    if (c_name == NULL)
        goto done;

    c_attrnamelen = (int)*attrnamelen;
    c_attrname = (char *)HD5f2cstring(attrname, c_attrnamelen);
    if (c_attrname == NULL)
        goto done;

    /*
    * Call H5LTget_attribute_int function.
    */
    c_loc_id = (hid_t)*loc_id;

    ret = H5LTget_attribute_string(c_loc_id,c_name,c_attrname,buf);

    if (ret < 0)
        goto done;

    ret_value = 0;


done:
    if(c_name!=NULL)
        free(c_name);
    if(c_attrname!=NULL)
        free(c_attrname);


    return ret_value;
}


/*-------------------------------------------------------------------------
* Function: H5LTget_dataset_ndims_c
*
* Purpose: Call H5LTget_dataset_ndims
*
* Return: Success: 0, Failure: -1
*
* Programmer: pvn@ncsa.uiuc.edu
*
* Date: September 09, 2004
*
* Comments:
*
* Modifications:
*
*
*-------------------------------------------------------------------------
*/

int_f
nh5ltget_dataset_ndims_c(hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *rank)
{
    int     ret_value = -1;
    herr_t  ret;
    hid_t   c_loc_id;
    char    *c_name = NULL;
    int     c_namelen;
    int     c_rank;

    /*
    * Convert FORTRAN name to C name
    */
    c_namelen = (int)*namelen;
    c_name = (char *)HD5f2cstring(name, c_namelen);
    if (c_name == NULL)
        goto done;

    /*
    * Call H5LTget_dataset_ndims function.
    */
    c_loc_id = (hid_t)*loc_id;

    ret = H5LTget_dataset_ndims(c_loc_id, c_name, &c_rank);

    if (ret < 0)
        goto done;

    *rank = (int_f)c_rank;
    ret_value = 0;


done:
    if(c_name!=NULL)
        free(c_name);

    return ret_value;
}


/*-------------------------------------------------------------------------
* Function: h5ltfind_dataset_c
*
* Purpose: Call H5LTfind_dataset
*
* Return: Success: 0, Failure: -1
*
* Programmer: pvn@ncsa.uiuc.edu
*
* Date: September 09, 2004
*
* Comments:
*
* Modifications:
*
*
*-------------------------------------------------------------------------
*/

int_f
nh5ltfind_dataset_c(hid_t_f *loc_id,
                    int_f *namelen,
                    _fcd name)
{
    hid_t   c_loc_id;
    char    *c_name = NULL;
    int     c_namelen;
    herr_t  ret;

    /*
    * Convert FORTRAN name to C name
    */
    c_namelen = (int)*namelen;
    c_name = (char *)HD5f2cstring(name, c_namelen);
    if (c_name == NULL) return -1;

    /*
    * Call H5LTget_dataset_ndims function.
    */
    c_loc_id = (hid_t)*loc_id;

    ret = H5LTfind_dataset(c_loc_id, c_name);

    if(c_name!=NULL)
       free(c_name);

    return ret;

}

/*-------------------------------------------------------------------------
* Function: h5ltget_dataset_info_c
*
* Purpose: Call H5LTget_dataset_info
*
* Return: Success: 0, Failure: -1
*
* Programmer: pvn@ncsa.uiuc.edu
*
* Date: September 09, 2004
*
* Comments:
*
* Modifications:
*
*
*-------------------------------------------------------------------------
*/

int_f
nh5ltget_dataset_info_c(hid_t_f *loc_id,
                        int_f *namelen,
                        _fcd name,
                        hsize_t_f *dims,
                        int_f *type_class,
                        size_t_f *type_size)
{
    int          ret_value = -1;
    herr_t       ret;
    hid_t        c_loc_id;
    char         *c_name = NULL;
    int          c_namelen;
    H5T_class_t  c_classtype;
    size_t       c_type_size;
    hsize_t      c_dims[32];
    int          i;
    int          c_rank;

    /*
    * convert FORTRAN name to C name
    */
    c_namelen = (int)*namelen;
    c_name = (char *)HD5f2cstring(name, c_namelen);
    if (c_name == NULL)
        goto done;

    /*
    * call H5LTget_dataset_ndims function.
    */
    c_loc_id = (hid_t)*loc_id;

    ret = H5LTget_dataset_info(c_loc_id, c_name, c_dims, &c_classtype, &c_type_size);
    if (ret < 0)
        goto done;

    *type_class = c_classtype;
    *type_size = (size_t_f)c_type_size;

    /*
    * transpose dimension arrays because of C-FORTRAN storage order
    */

    ret = H5LTget_dataset_ndims(c_loc_id, c_name, &c_rank);
    if (ret < 0)
        goto done;

    for (i = 0; i < c_rank ; i++)
    {
        dims[i] = (hsize_t_f) c_dims[c_rank - i - 1];
    }


    ret_value = 0;

done:
    if(c_name!=NULL)
        free(c_name);

    return ret_value;
}

/*-------------------------------------------------------------------------
* Function: h5ltget_attribute_ndims_c
*
* Purpose: Call H5LTget_attribute_ndims
*
* Return: Success: 0, Failure: -1
*
* Programmer: pvn@ncsa.uiuc.edu
*
* Date: October 05, 2004
*
* Comments:
*
* Modifications:
*
*
*-------------------------------------------------------------------------
*/

int_f
nh5ltget_attribute_ndims_c(hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd dsetname,
                           int_f *attrnamelen,
                           _fcd attrname,
                           int_f *rank)
{
    int     ret_value = -1;
    herr_t  ret;
    hid_t   c_loc_id;
    char    *c_name = NULL;
    char    *c_attrname = NULL;
    int     c_namelen;
    int     c_attrnamelen;
    int     c_rank;

    /*
    * Convert FORTRAN name to C name
    */
    c_namelen =(int) *namelen;
    c_name = (char *)HD5f2cstring(dsetname, c_namelen);
    if (c_name == NULL)
        goto done;

    c_attrnamelen = (int)*attrnamelen;
    c_attrname = (char *)HD5f2cstring(attrname, c_attrnamelen);
    if (c_attrname == NULL)
        goto done;

    /*
    * Call H5LTset_attribute_ndims function.
    */
    c_loc_id = (hid_t)*loc_id;

    ret = H5LTget_attribute_ndims(c_loc_id,c_name,c_attrname,&c_rank);

    if (ret < 0)
        goto done;

    *rank = (int_f)c_rank;
    ret_value = 0;


done:
    if(c_name!=NULL)
        free(c_name);
    if(c_attrname!=NULL)
        free(c_attrname);

    return ret_value;
}


/*-------------------------------------------------------------------------
* Function: h5ltget_attribute_info_c
*
* Purpose: Call H5LTget_attribute_info
*
* Return: Success: 0, Failure: -1
*
* Programmer: pvn@ncsa.uiuc.edu
*
* Date: September 09, 2004
*
* Comments:
*
* Modifications:
*
*
*-------------------------------------------------------------------------
*/

int_f
nh5ltget_attribute_info_c(hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *attrnamelen,
                          _fcd attrname,
                          hsize_t_f *dims,
                          int_f *type_class,
                          size_t_f *type_size)
{
    int          ret_value = -1;
    herr_t       ret;
    hid_t        c_loc_id;
    char         *c_name = NULL;
    char         *c_attrname = NULL;
    int          c_namelen;
    int          c_attrnamelen;
    H5T_class_t  c_classtype;
    size_t       c_type_size;
    hsize_t      c_dims[32];
    int          i;
    int          c_rank;

    /*
    * convert FORTRAN name to C name
    */
    c_namelen = (int)*namelen;
    c_name = (char *)HD5f2cstring(name, c_namelen);
    if (c_name == NULL)
        goto done;

    c_attrnamelen = (int)*attrnamelen;
    c_attrname = (char *)HD5f2cstring(attrname, c_attrnamelen);
    if (c_attrname == NULL)
        goto done;

    /*
    * call H5LTget_attribute_info function.
    */
    c_loc_id = (hid_t)*loc_id;

    ret = H5LTget_attribute_info(c_loc_id,c_name,c_attrname,c_dims,&c_classtype,&c_type_size);
    if (ret < 0)
        goto done;

    *type_class = c_classtype;
    *type_size = (size_t_f)c_type_size;

    /*
    * transpose dimension arrays because of C-FORTRAN storage order
    */

    ret = H5LTget_attribute_ndims(c_loc_id,c_name,c_attrname,&c_rank);
    if (ret < 0)
        goto done;

    for (i = 0; i < c_rank ; i++)
    {
        dims[i] = (hsize_t_f) c_dims[c_rank - i - 1];
    }

    ret_value = 0;


done:
    if(c_name!=NULL)
        free(c_name);
    if(c_attrname!=NULL)
        free(c_attrname);


    return ret_value;
}
