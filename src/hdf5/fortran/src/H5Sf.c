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

#include "H5f90.h"
#include "H5Eprivate.h"

/*----------------------------------------------------------------------------
 * Name:        h5screate_simple_c
 * Purpose:     Call H5Screate_simple to create a dataspace
 * Inputs:      rank - number of dimensions of dataspace
 *              dims - array of the size of each dimension
                maxdims - an array of the maximum size of each dimension
 * Outputs:     space_id - identifier of the created dataspace
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, August 4, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5screate_simple_c ( int_f *rank, hsize_t_f *dims, hsize_t_f *maxdims, hid_t_f *space_id )
{
    hsize_t c_dims[H5S_MAX_RANK];
    hsize_t c_maxdims[H5S_MAX_RANK];
    hid_t c_space_id;
    int i;
    int_f ret_value = 0;

    /*
     * Transpose dimension arrays because of C-FORTRAN storage order
     */
    for(i = 0; i < *rank ; i++) {
         c_dims[i] =  dims[*rank - i - 1];
         c_maxdims[i] = maxdims[*rank - i - 1];
    } /* end for */

    c_space_id = H5Screate_simple(*rank, c_dims, c_maxdims);
    if(c_space_id < 0)
        HGOTO_DONE(FAIL)

    *space_id = (hid_t_f)c_space_id;

done:
    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5sclose_c
 * Purpose:     Call H5Sclose to close the dataspace
 * Inputs:      space_id - identifier of the dataspace to be closed
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, August 4, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5sclose_c ( hid_t_f *space_id )
{
  int ret_value = 0;
  hid_t c_space_id;

  c_space_id = *space_id;
  if ( H5Sclose(c_space_id) < 0  ) ret_value = -1;
  return ret_value;
}


/*----------------------------------------------------------------------------
 * Name:        h5screate_c
 * Purpose:     Call H5Screate to create a dataspace
 * Inputs:      classtype - type of the dataspace class
 * Outputs:     space_id - identifier of the created dataspace
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Tuesday, August 10, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5screate_c ( int_f *classtype, hid_t_f *space_id )
{
  H5S_class_t c_classtype;
  int ret_value = 0;
  hid_t c_space_id;
  c_classtype = (H5S_class_t) *classtype;
  c_space_id = H5Screate(c_classtype);

  if ( c_space_id  < 0  ) ret_value = -1;
  *space_id = (hid_t_f) c_space_id;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5scopy_c
 * Purpose:     Call H5Scopy to copy dataspace
 * Inputs:      space_id - identifier of the dataspace to be copied
 * Outputs:     new_space_id - identifier of the new datspace
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Tuesday, August 10, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5scopy_c( hid_t_f *space_id , hid_t_f *new_space_id)
{
  int ret_value = 0;
  hid_t c_new_space_id;
  hid_t c_space_id;

  c_space_id = *space_id;
  c_new_space_id = H5Scopy(c_space_id);
  if ( c_new_space_id < 0  ) ret_value = -1;

  *new_space_id = (hid_t_f)c_new_space_id;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5sget_select_hyper_nblocks_c
 * Purpose:     Call H5SH5Sget_select_hyper_nblocks to
 *              get the the number of hyperslab blocks in
 *              the current dataspace selection if successful
 * Inputs:      space_id - identifier of the dataspace
 * Outputs:     num_blocks -  number of hyperslab blocks in
 *              the current dataspace selection
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, November 12, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5sget_select_hyper_nblocks_c( hid_t_f *space_id , hssize_t_f * num_blocks)
{
  int ret_value = 0;
  hid_t c_space_id;
  hssize_t c_num_blocks;

  c_space_id = *space_id;
  c_num_blocks = H5Sget_select_hyper_nblocks(c_space_id);
  if ( c_num_blocks < 0  ) ret_value = -1;

  *num_blocks = (hssize_t_f)c_num_blocks;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5sget_select_elem_npoints_c
 * Purpose:     Call H5Sget_select_elem_npoints to
 *              get the the number of element points in
 *              the current dataspace selection if successful
 * Inputs:      space_id - identifier of the dataspace
 * Outputs:     num_points -  number of element points in
 *              the current dataspace selection
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Monday, November 15, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5sget_select_elem_npoints_c( hid_t_f *space_id , hssize_t_f * num_points)
{
  int ret_value = 0;
  hid_t c_space_id;
  hssize_t c_num_points;

  c_space_id = *space_id;
  c_num_points = H5Sget_select_elem_npoints(c_space_id);
  if ( c_num_points < 0  ) ret_value = -1;

  *num_points = (hssize_t_f)c_num_points;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5sget_select_hyper_blocklist_c
 * Purpose:     Call H5Sget_select_hyper_blocklist to
 *              get a list of the hyperslab blocks currently selected
 *              Starting with the startblock-th block in the
 *              list of blocks, num_blocks blocks are put into the user's
 *              buffer. If the user's buffer fills up before numblocks
 *              blocks are inserted, the buffer
 *              will contain only as many blocks as fit.
 * Inputs:      space_id - identifier of the dataspace
 *              startblock - Hyperslab block to start with
 *              num_blocks -  number of hyperslab blocks in
 *                            the current dataspace selection
 * Outputs:     buf - List of hyperslab blocks selected
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Monday, November 15, 1999
 * Modifications:
 *              Transpose dimension arrays because of C-FORTRAN storage order
 *              M.S. Breitenfeld
 *---------------------------------------------------------------------------*/

int_f
nh5sget_select_hyper_blocklist_c( hid_t_f *space_id ,hsize_t_f * startblock,
                                  hsize_t_f * num_blocks, hsize_t_f * buf)
{
  int ret_value = -1;
  hid_t c_space_id;
  hsize_t c_num_blocks;

  hsize_t i;
  int j,k,m,n;
  int rank;
  hsize_t c_startblock, *c_buf;

  c_space_id = *space_id;
  c_num_blocks = * num_blocks;

  rank = H5Sget_simple_extent_ndims(c_space_id);
  if (rank < 0 ) return ret_value;
  c_startblock = (hsize_t)*startblock;

  c_buf = (hsize_t*)malloc(sizeof(hsize_t)*(size_t)(c_num_blocks*2*rank));
  if (!c_buf) return ret_value;

  ret_value = H5Sget_select_hyper_blocklist(c_space_id, c_startblock,
                                            c_num_blocks, c_buf);

  /*
   * Transpose dimension arrays because of C-FORTRAN storage order and add 1
   */
  n = 0;
  m = 0;
  for (i=0; i < c_num_blocks; i++) {
    for (j=0; j < rank; j++) {
      for (k=0; k < rank; k++) {
	int t= (m + rank - k - 1);
	buf[n] = (hsize_t_f)c_buf[t]+1;
	n = n + 1;
      }
      m = m + rank;
    }
  }

  HDfree(c_buf);
  if (ret_value  >= 0  ) ret_value = 0;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5sget_select_bounds_c
 * Purpose:     Call H5Sget_select_bounds to retrieve the coordinates
 *              of the bounding box containing the current selection
 *              and places them into user-supplied buffers
 * Inputs:      space_id - identifier of the dataspace
 * Outputs:     start -  Starting coordinates of the bounding box
 *              end -  Ending coordinates of the bounding box,
 *                     i.e., the coordinates of the diagonally opposite corne
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Wednesday, November 17, 1999
 * Modifications: swapped array bounds to account for C and Fortran reversed
 *                matrix notation.
 *                M.S. Breitenfeld
 *---------------------------------------------------------------------------*/

int_f
nh5sget_select_bounds_c( hid_t_f *space_id , hsize_t_f * start, hsize_t_f * end)
{
    hid_t c_space_id;
    hsize_t c_start[H5S_MAX_RANK];
    hsize_t c_end[H5S_MAX_RANK];
    int i, rank;
    int_f ret_value = 0;

    c_space_id = *space_id;
    rank = H5Sget_simple_extent_ndims(c_space_id);
    if(rank < 0 )
        HGOTO_DONE(FAIL)

    if(H5Sget_select_bounds(c_space_id, c_start, c_end) < 0)
        HGOTO_DONE(FAIL)

    for(i = 0; i < rank; i++) {
        start[i] = (hsize_t_f)(c_start[rank - i - 1] + 1);
        end[i] = (hsize_t_f)(c_end[rank - i - 1] + 1);
    } /* end for */

done:
    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5sget_select_elem_pointlist_c
 * Purpose:     Call  H5Sget_select_elem_pointlist
 *              get a list of  element points in the
 *              current dataspace selectin.
 *              Starting with the startpoint-th point in the
 *              list of points, numpoints points are put into the user's
 *              buffer. If the user's buffer fills up before numpoints
 *              points are inserted, the buffer
 *              will contain only as many points as fit.
 * Inputs:      space_id - identifier of the dataspace
 *              startpoint - Element point to start with
 *              numpoints -  Number of element points to get
 * Outputs:     buf - List of element points selected
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Wednesday, November 17, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5sget_select_elem_pointlist_c( hid_t_f *space_id ,hsize_t_f * startpoint,
                                  hsize_t_f * numpoints, hsize_t_f * buf)
{
  int ret_value = -1;
  hid_t c_space_id;
  hsize_t c_num_points;
  hsize_t c_startpoint,* c_buf;
  hsize_t i, i1;
  int rank;
  int j,i2;

  c_space_id = *space_id;
  c_num_points = (hsize_t)* numpoints;

  rank = H5Sget_simple_extent_ndims(c_space_id);
  if (rank < 0 ) return ret_value;

  c_startpoint = (hsize_t)*startpoint;
  c_buf = (hsize_t*)malloc(sizeof(hsize_t)*(size_t)(c_num_points*rank));
  if (!c_buf) return ret_value;
  ret_value = H5Sget_select_elem_pointlist(c_space_id, c_startpoint,
                                            c_num_points, c_buf);

  /* re-arrange the return buffer to account for Fortran ordering of 2D arrays */
  /* and add 1 to account for array's starting at one in Fortran */
  i2 = 0;
  for( i = 0; i < c_num_points; i++) {
    i1 =  rank*(i+1);
    for(j = 0; j < rank; j++) {
      buf[i2] = (hsize_t_f)(c_buf[i1-1]+1);
      i2 = i2 + 1;
      i1 = i1 - 1;
    }
  }

/*   for( i = 0; i < c_num_points*rank; i++) { */
/*     printf("%i \n", (int)c_buf[i]+1); */
/*   } */

  if (ret_value  >= 0  ) ret_value = 0;

  HDfree(c_buf);

  return ret_value;
}



/*----------------------------------------------------------------------------
 * Name:        h5sselect_all_c
 * Purpose:     Call H5Sselect_all to select entire dataspace
 * Inputs:      space_id - identifier of the dataspace
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Tuesday, August 10, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5sselect_all_c ( hid_t_f *space_id )
{
  int ret_value = 0;
  hid_t c_space_id;

  c_space_id = *space_id;
  if ( H5Sselect_all(c_space_id) < 0  ) ret_value = -1;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5sselect_none_c
 * Purpose:     Call H5Sselect_none to reset the selection region
 * Inputs:      space_id - identifier of the dataspace
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Tuesday, August 10, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5sselect_none_c ( hid_t_f *space_id )
{
  int ret_value = 0;
  hid_t c_space_id;

  c_space_id = *space_id;
  if ( H5Sselect_none(c_space_id) < 0  ) ret_value = -1;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5sselect_valid_c
 * Purpose:     Call H5Sselect_valid to verify that selection
 *              is within dataspace extent.
 * Inputs:      space_id - identifier of the dataspace
 * Outputs:     flag - 0 if not valid selection, 1 if is valid selection,
 *              and negative on failure.
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Tuesday, August 10, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5sselect_valid_c ( hid_t_f *space_id , int_f *flag )
{
  int ret_value = 0;
  hid_t c_space_id;
  htri_t status;

  c_space_id = *space_id;
  status = H5Sselect_valid(c_space_id);
  *flag = (int_f)status;
  if ( status < 0  ) ret_value = -1;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5sget_simple_extent_npoints_c
 * Purpose:     Call H5Sget_simple_extent_npoints to determine the number
 *              of elements in a dataspace
 * Inputs:      space_id - identifier of the dataspace
 * Outputs:     npoints - number of points in a dataspace
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, August 11, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5sget_simple_extent_npoints_c ( hid_t_f *space_id , hsize_t_f *npoints )
{
  int ret_value = 0;
  hid_t c_space_id;
  hsize_t c_npoints;

  c_space_id = *space_id;
  c_npoints = H5Sget_simple_extent_npoints(c_space_id);
  if ( c_npoints == 0  ) ret_value = -1;
  *npoints = (hsize_t_f)c_npoints;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5sget_select_npoints_c
 * Purpose:     Call H5Sget_select_npoints to determine the number
 *              of elements in a dataspace selection
 * Inputs:      space_id - identifier of the dataspace
 * Outputs:     npoints - number of points in a dataspace selection
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, August 11, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5sget_select_npoints_c ( hid_t_f *space_id , hssize_t_f *npoints )
{
  int ret_value = 0;
  hssize_t c_npoints;
  hid_t c_space_id;

  c_space_id = *space_id;
  c_npoints = H5Sget_select_npoints(c_space_id);
  if ( c_npoints < 0  ) ret_value = -1;
  *npoints = (hssize_t_f)c_npoints;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5sget_simple_extent_ndims_c
 * Purpose:     Call H5Sget_simple_extent_ndims to determine the number
 *              dimensions
 * Inputs:      space_id - identifier of the dataspace
 * Outputs:     rank - number of dataspace dimensions
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, August 11, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5sget_simple_extent_ndims_c ( hid_t_f *space_id , int_f *ndims )
{
  int ret_value = 0;
  hid_t c_space_id;
  int c_ndims;

  c_space_id = *space_id;
  c_ndims = H5Sget_simple_extent_ndims(c_space_id);
  if ( c_ndims < 0  ) ret_value = -1;
  *ndims = (int_f)c_ndims;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5sget_simple_extent_type_c
 * Purpose:     Call H5Sget_simple_extent_type to determine the class type
 *              of a dataspace
 * Inputs:      space_id - identifier of the dataspace
 * Outputs:     classtype - class type; possible values are:
 *              H5S_SCALAR_F (0), H5S_SIMPLE_F (1), H5S_NULL_F (2)
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, August 11, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5sget_simple_extent_type_c ( hid_t_f *space_id , int_f *classtype)
{
  int ret_value = 0;
  hid_t c_space_id;
  H5S_class_t c_classtype;

  c_space_id = *space_id;
  c_classtype = H5Sget_simple_extent_type(c_space_id);
  if ( c_classtype < 0  ) ret_value = -1;
   *classtype = c_classtype;
/*
  if (c_classtype == H5S_SCALAR) *classtype = H5S_SCALAR_F;
  if (c_classtype == H5S_SIMPLE) *classtype = H5S_SIMPLE_F;
  if (c_classtype == H5S_NULL)   *classtype = H5S_NULL_F;
*/
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5soffset_simple_c
 * Purpose:     Call H5Soffset_simple to set the offset of a simple
 *              dataspace
 * Inputs:      space_id - identifier of the dataspace
 *              offset - offset array
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, August 11, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5soffset_simple_c ( hid_t_f *space_id , hssize_t_f *offset)
{
    hid_t c_space_id;
    int rank;
    hssize_t c_offset[H5S_MAX_RANK];
    int i;
    int_f ret_value = 0;

    c_space_id = *space_id;
    rank = H5Sget_simple_extent_ndims(c_space_id);
    if(rank < 0)
        HGOTO_DONE(FAIL)

    /*
     * Reverse dimensions due to C-FORTRAN storage order.
     */
    for(i = 0; i < rank; i++)
        c_offset[i] = offset[rank - i - 1];

    if(H5Soffset_simple(c_space_id, c_offset) < 0)
        HGOTO_DONE(FAIL)

done:
    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5sset_extent_simple_c
 * Purpose:     Call H5Sset_extent_simple to set or reset size of
 *              existing  dataspace
 * Inputs:      space_id - identifier of the dataspace
 *              rank - dataspace rank
 *              current_size - array with the new dimension sizes
 *              maximum_size - aray with maximum sizes of dimensions
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, August 11, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5sset_extent_simple_c ( hid_t_f *space_id , int_f *rank, hsize_t_f *current_size, hsize_t_f *maximum_size)
{
    hsize_t c_current_size[H5S_MAX_RANK];
    hsize_t c_maximum_size[H5S_MAX_RANK];
    int i;
    int_f ret_value = 0;

    /*
     * Reverse dimensions due to C-FORTRAN storage order.
     */
    for(i = 0; i < *rank; i++) {
        c_current_size[i] = (hsize_t)current_size[*rank - i - 1];
        c_maximum_size[i] = (hsize_t)maximum_size[*rank - i - 1];
    } /* end for */

    if(H5Sset_extent_simple((hid_t)*space_id, (int)*rank, c_current_size, c_maximum_size) < 0)
        HGOTO_DONE(FAIL)

done:
    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5sget_simple_extent_dims_c
 * Purpose:     Call H5Sget_simple_extent_dims to retrieve sizes of an
 *              existing  dataspace
 * Inputs:      space_id - identifier of the dataspace
 * Outputs:     dims - array with the dimension sizes
 *              maxdims - aray with maximum sizes of dimensions
 * Returns:     number of dataspace dimensions (rank) on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, August 11, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5sget_simple_extent_dims_c ( hid_t_f *space_id , hsize_t_f *dims, hsize_t_f *maxdims)
{
    hid_t c_space_id;
    hsize_t c_dims[H5S_MAX_RANK];
    hsize_t c_maxdims[H5S_MAX_RANK];
    int rank;
    int i;
    int_f ret_value;

    c_space_id = *space_id;
    rank = H5Sget_simple_extent_ndims(c_space_id);
    if(rank < 0)
        HGOTO_DONE(FAIL)

    if(H5Sget_simple_extent_dims(c_space_id, c_dims, c_maxdims) < 0)
        HGOTO_DONE(FAIL)

    /*
     * Reverse dimensions due to C-FORTRAN storage order.
     */
    for(i = 0; i < rank; i++) {
        dims[rank - i - 1] = (hsize_t_f)c_dims[i];
        maxdims[rank - i - 1] = (hsize_t_f)c_maxdims[i];
    } /* end for */

    ret_value = rank;

done:
    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5sis_simple_c
 * Purpose:     Call H5Sis_simple to detrmine if the dataspace
 *              is simple.
 * Inputs:      space_id - identifier of the dataspace
 * Outputs:     flag - 0 if not simple, 1 if is simple,
 *              and negative on failure.
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, August 11, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5sis_simple_c ( hid_t_f *space_id , int_f *flag )
{
  int ret_value = 0;
  hid_t c_space_id;
  htri_t status;

  c_space_id = *space_id;
  status = H5Sis_simple(c_space_id);
  *flag = (int_f)status;
  if ( status < 0  ) ret_value = -1;
  return ret_value;
}


/*----------------------------------------------------------------------------
 * Name:        h5sextent_copy_c
 * Purpose:     Call H5Sextent_copy to copy an extent of dataspace
 * Inputs:      dest_space_id - identifier of the destination dataspace
 *              source_space_id - identifier of the source dataspace
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, August 11, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5sextent_copy_c ( hid_t_f *dest_space_id , hid_t_f *source_space_id)
{
  int ret_value = 0;
  hid_t c_dest_space_id, c_source_space_id;
  herr_t status;

  c_dest_space_id = *dest_space_id;
  c_source_space_id = *source_space_id;
  status = H5Sextent_copy(c_dest_space_id, c_source_space_id);
  if ( status < 0  ) ret_value = -1;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5sset_extent_none_c
 * Purpose:     Call H5Sset_extent_none to remove extent from a dataspace
 * Inputs:      space_id - dataspace identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, August 11, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5sset_extent_none_c ( hid_t_f *space_id )
{
  int ret_value = 0;
  hid_t c_space_id;
  herr_t status;

  c_space_id = *space_id;
  status = H5Sset_extent_none(c_space_id);
  if ( status < 0  ) ret_value = -1;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5sselect_hyperslab_c
 * Purpose:     Call H5Sselect_hyperslab to select a hyperslab
 * Inputs:      space_id - identifier of the dataspace
 *              operator - defines how the new selection is combined
 *              with the previous one; current values are
 *              H5S_SELECT_SET_F (0) and H5S_SELECT_OR_F (1)
 *              start - offset of start of hyperslab
 *              count - number of blocks included in the hyperslab
 *              stride - hyperslab stride (interval between blocks)
 *              block - size of block in the hyperslab
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, August 11, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5sselect_hyperslab_c ( hid_t_f *space_id , int_f *op, hsize_t_f *start, hsize_t_f *count, hsize_t_f *stride, hsize_t_f *block)
{
    hsize_t c_start[H5S_MAX_RANK];
    hsize_t c_count[H5S_MAX_RANK];
    hsize_t c_stride[H5S_MAX_RANK];
    hsize_t c_block[H5S_MAX_RANK];
    int rank;
    int i;
    int_f ret_value = 0;

    rank = H5Sget_simple_extent_ndims((hid_t)*space_id);
    if(rank < 0 )
        HGOTO_DONE(FAIL)

    /*
     * Reverse dimensions due to C-FORTRAN storage order.
     */
    for(i = 0; i < rank; i++) {
        int t = (rank - i) - 1;

        c_start[i] = (hsize_t)start[t];
        c_count[i] = (hsize_t)count[t];
        c_stride[i] = (hsize_t)stride[t];
        c_block[i] = (hsize_t)block[t];
    } /* end for */

    if(H5Sselect_hyperslab((hid_t)*space_id, (H5S_seloper_t)*op, c_start, c_stride, c_count, c_block) < 0)
        HGOTO_DONE(FAIL)

done:
    return ret_value;
}

#ifdef NEW_HYPERSLAB_API
/*----------------------------------------------------------------------------
 * Name:        h5scombine_hyperslab_c
 * Purpose:     Call H5Scombine_hyperslab
 * Inputs:      space_id - identifier of the dataspace
 *              operator - defines how the new selection is combined
 *              start - offset of start of hyperslab
 *              count - number of blocks included in the hyperslab
 *              stride - hyperslab stride (interval between blocks)
 *              block - size of block in the hyperslab
 * Outputs:     hyper_id - identifier for the new dataspace
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Monday, October 7, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5scombine_hyperslab_c ( hid_t_f *space_id , int_f *op, hsize_t_f *start, hsize_t_f *count, hsize_t_f *stride, hsize_t_f *block, hid_t_f *hyper_id)
{
  int ret_value = -1;
  hid_t c_space_id;
  hid_t c_hyper_id;
  hsize_t *c_start = NULL;
  hsize_t *c_count = NULL;
  hsize_t *c_stride = NULL;
  hsize_t *c_block = NULL;

  H5S_seloper_t c_op;
  herr_t  status;
  int rank;
  int i;

  rank = H5Sget_simple_extent_ndims(*space_id);
  if (rank < 0 ) return ret_value;
  c_start = (hsize_t *)HDmalloc(sizeof(hsize_t)*rank);
  if (c_start == NULL) goto DONE;

  c_count = (hsize_t *)HDmalloc(sizeof(hsize_t)*rank);
  if (c_count == NULL) goto DONE;

  c_stride = (hsize_t *)HDmalloc(sizeof(hsize_t)*rank);
  if (c_stride == NULL) goto DONE;

  c_block = (hsize_t *)HDmalloc(sizeof(hsize_t)*rank);
  if (c_block == NULL) goto DONE;


  /*
   * Reverse dimensions due to C-FORTRAN storage order.
   */

  for (i=0; i < rank; i++) {
      int t= (rank - i) - 1;
      c_start[i] = (hsize_t)start[t];
      c_count[i] = (hsize_t)count[t];
      c_stride[i] = (hsize_t)stride[t];
      c_block[i] = (hsize_t)block[t];
  }

   c_op = (H5S_seloper_t)*op;

  c_space_id = (hid_t)*space_id;
  c_hyper_id = H5Scombine_hyperslab(c_space_id, c_op, c_start, c_stride, c_count, c_block);
  if ( c_hyper_id < 0  ) goto DONE;
  *hyper_id = (hid_t_f)c_hyper_id;
  ret_value = 0;
DONE:
  if(c_start != NULL) HDfree(c_start);
  if(c_count != NULL) HDfree(c_count);
  if(c_stride!= NULL) HDfree(c_stride);
  if(c_block != NULL) HDfree(c_block);
  return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5scombine_select_c
 * Purpose:     Call H5Scombine_ select
 * Inputs:      space1_id - identifier of the first dataspace
 *              operator - defines how the new selection is combined
 *              space2_id - identifier of the second dataspace
 * Outputs:     ds_id   - identifier for the new dataspace
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Monday, October 7, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5scombine_select_c ( hid_t_f *space1_id , int_f *op, hid_t_f *space2_id, hid_t_f *ds_id)
{
  int ret_value = -1;
  hid_t c_space1_id;
  hid_t c_space2_id;
  hid_t c_ds_id;
  H5S_seloper_t c_op;

  c_op = (H5S_seloper_t)*op;

  c_space1_id = (hid_t)*space1_id;
  c_space2_id = (hid_t)*space2_id;
  c_ds_id = H5Scombine_select(c_space1_id, c_op, c_space2_id);
  if ( c_ds_id < 0  ) return ret_value;
  *ds_id = (hid_t_f)c_ds_id;
  ret_value = 0;
  return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5sselect_select_c
 * Purpose:     Call H5Sselect_ select
 * Inputs:      space1_id - identifier of the first dataspace  to modify
 *              operator - defines how the new selection is combined
 *              space2_id - identifier of the second dataspace
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Monday, October 7, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5sselect_select_c ( hid_t_f *space1_id , int_f *op, hid_t_f *space2_id)
{
  int ret_value = -1;
  hid_t c_space1_id;
  hid_t c_space2_id;
  H5S_seloper_t c_op;

  c_op = (H5S_seloper_t)*op;

  c_space1_id = (hid_t)*space1_id;
  c_space2_id = (hid_t)*space2_id;
  if( H5Sselect_select(c_space1_id, c_op, c_space2_id)< 0) return ret_value;
  ret_value = 0;
  return ret_value;
}
#endif /*NEW_HYPERSLAB_API*/
/*----------------------------------------------------------------------------
 * Name:        h5sget_select_type_c
 * Purpose:     Call H5Sget_select_type
 * Inputs:      space_id - identifier of the dataspace
 * Outputs:     type - type of selection
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Monday, October 7, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5sget_select_type_c ( hid_t_f *space_id , int_f *type)
{
  int ret_value = -1;
  hid_t c_space_id;
  H5S_sel_type c_type;

  c_space_id = (hid_t)*space_id;
  c_type = H5Sget_select_type(c_space_id);
  if(c_type < 0) return ret_value;
  *type = (int_f)c_type;
  ret_value = 0;
  return ret_value;
}


/*----------------------------------------------------------------------------
 * Name:        h5sselect_elements_c
 * Purpose:     Call H5Sselect_elements to select elements of a dataspace
 * Inputs:      space_id - identifier of the dataspace
 *              operator - defines how the new selection is combined
 *              with the previous one; current values are
 *              H5S_SELECT_SET_F (0)
 *              nelements - number of elements in the selection
 *              coord - arrays with the elements coordinates
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, August 11, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5sselect_elements_c ( hid_t_f *space_id , int_f *op, size_t_f *nelements,  hsize_t_f *coord)
{
  int ret_value = -1;
  hid_t c_space_id;
  H5S_seloper_t c_op;
  herr_t  status;
  int rank;
  int i, j;
  hsize_t *c_coord;
  size_t c_nelements;

  c_op = (H5S_seloper_t)*op;

  c_space_id = *space_id;
  rank = H5Sget_simple_extent_ndims(c_space_id);

  c_coord = (hsize_t *)HDmalloc(sizeof(hsize_t)*rank*(*nelements));
  if(!c_coord) return ret_value;
  for (i=0; i< *nelements; i++) {
      for (j = 0; j < rank; j++) {
          c_coord[j+i*rank] = (hsize_t)coord[j + i*rank];
      }
  }

  c_nelements = *nelements;
  status = H5Sselect_elements(c_space_id, c_op, c_nelements, c_coord);
  if ( status >= 0  ) ret_value = 0;
  HDfree(c_coord);
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5sdecode_c
 * Purpose:     Call H5Sdecode
 * Inputs:
 *		buf     - Buffer for the data space object to be decoded.
 * Outputs:
 *              obj_id  - Object_id (non-negative)
 *
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              March 26, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5sdecode_c ( _fcd buf, hid_t_f *obj_id )
{
  int ret_value = -1;
  unsigned char *c_buf = NULL;  /* Buffer to hold C string */
  hid_t c_obj_id;

  /*
   * Call H5Sdecode function.
   */

  c_buf = (unsigned char*)buf;

  c_obj_id = H5Sdecode(c_buf);
  if(c_obj_id < 0)
    return ret_value;

  *obj_id = (hid_t_f)c_obj_id;
  ret_value = 0;

  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5sencode_c
 * Purpose:     Call H5Sencode
 * Inputs:
 *            obj_id - Identifier of the object to be encoded.
 *		 buf - Buffer for the object to be encoded into.
 *            nalloc - The size of the allocated buffer.
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              March 26, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5sencode_c (_fcd buf, hid_t_f *obj_id, size_t_f *nalloc )
{
  int ret_value = -1;
  unsigned char *c_buf = NULL;          /* Buffer to hold C string */
  size_t c_size;

  /* return just the size of the allocated buffer;
   * equivalent to C routine for which 'name' is set equal to NULL
   */

  if (*nalloc == 0) {

    if(H5Sencode((hid_t)*obj_id, c_buf, &c_size) < 0)
      return ret_value;

    *nalloc = (size_t_f)c_size;

    ret_value = 0;
    return ret_value;
  }

  c_size = (size_t)*nalloc;
  /*
   * Allocate buffer
   */
  if(NULL == (c_buf = (unsigned char *)HDmalloc(c_size)))
    return ret_value;
  /*
   * Call H5Sencode function.
   */
  if(H5Sencode((hid_t)*obj_id, c_buf, &c_size) < 0){
    return ret_value;
  }

  /* copy the C buffer to the FORTRAN buffer.
   * Can not use HD5packFstring because we don't want to
   * eliminate the NUL terminator or pad remaining space
   * with blanks.
   */

  HDmemcpy(_fcdtocp(buf),(char *)c_buf,c_size);

  ret_value = 0;
  if(c_buf) HDfree(c_buf);
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5sextent_equal_c
 * Purpose:     Call H5Sextent_equal
 * Inputs:
 *		space1_id - First dataspace identifier.
 *              space2_id - Second dataspace identifier.
 * Outputs:
 *              equal - TRUE if equal, FALSE if unequal.
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              April 4, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5sextent_equal_c ( hid_t_f * space1_id, hid_t_f *space2_id, hid_t_f *c_equal)
{
  int ret_value = -1;

  if( (*c_equal = (hid_t_f)H5Sextent_equal((hid_t)*space1_id, (hid_t)*space2_id)) < 0)
    return ret_value;

  ret_value = 0;
  return ret_value;
}

