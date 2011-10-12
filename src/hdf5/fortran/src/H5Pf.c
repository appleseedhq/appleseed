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

/* This files contains C stubs for H5P Fortran APIs */

#include "H5f90.h"
#include "H5Eprivate.h"

/*----------------------------------------------------------------------------
 * Name:        h5pcreate_c
 * Purpose:     Call H5Pcreate to create a property list
 * Inputs:      cls - property list class identifier
 * Outputs:     prp_id - identifier of the created property list
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, October 9, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pcreate_c ( hid_t_f *cls, hid_t_f *prp_id )
{
    hid_t c_prp_id;
    int_f ret_value = 0;

    c_prp_id = H5Pcreate((hid_t)*cls);
    if(c_prp_id  < 0)
        HGOTO_DONE(FAIL)

    *prp_id = (hid_t_f)c_prp_id;

done:
    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pclose_c
 * Purpose:     Call H5Pclose to close property lis
 * Inputs:      prp_id - identifier of the property list to be closed
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Saturday, August 14, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pclose_c ( hid_t_f *prp_id )
{
    int_f ret_value = 0;

    if(H5Pclose((hid_t)*prp_id) < 0)
        ret_value = -1;

    return ret_value;
}


/*----------------------------------------------------------------------------
 * Name:        h5pcopy_c
 * Purpose:     Call H5Pcopy to copy property list
 * Inputs:      prp_id - identifier of the property list to be copied
 * Outputs:     new_prp_id - identifier of the new property list
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Saturday, August 14, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pcopy_c ( hid_t_f *prp_id , hid_t_f *new_prp_id)
{
    hid_t c_new_prp_id;
    int_f ret_value = 0;

    c_new_prp_id = H5Pcopy((hid_t)*prp_id);
    if(c_new_prp_id < 0)
        HGOTO_DONE(FAIL)

    *new_prp_id = (hid_t_f)c_new_prp_id;

done:
    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pequal_c
 * Purpose:     Call H5Pequal to check if two property lists are equal
 * Inputs:      plist1_id - property list identifier
 *              plist2_id - property list identifier
 * Outputs:     c_flag    - flag to indicate that lists are eqaul
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Monday, September 30, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pequal_c ( hid_t_f *plist1_id , hid_t_f *plist2_id, int_f * c_flag)
{
    htri_t c_c_flag;
    int_f ret_value = 0;

    c_c_flag = H5Pequal((hid_t)*plist1_id, (hid_t)*plist2_id);
    if(c_c_flag < 0)
        HGOTO_DONE(FAIL)

    *c_flag = (int_f)c_c_flag;

done:
    return ret_value;
}


/*----------------------------------------------------------------------------
 * Name:        h5pget_class_c
 * Purpose:     Call H5Pget_class to determine property list class
 * Inputs:      prp_id - identifier of the dataspace
 * Outputs:     classtype - class type; possible values are:
 *              H5P_ROOT_F       -1
 *              H5P_FILE_CREATE_F     0
 *              H5P_FILE_ACCESS_F     1
 *              H5P_DATASET_CREATE_F  2
 *              H5P_DATASET_XFER_F    3
 *              H5P_FILE_MOUNT_F      4
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Saturday, August 14, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pget_class_c ( hid_t_f *prp_id , int_f *classtype)
{
    hid_t c_classtype;
    int_f ret_value = 0;

    c_classtype = H5Pget_class((hid_t)*prp_id);
    if(c_classtype == H5P_ROOT) {
      *classtype = H5P_ROOT;
       HGOTO_DONE(FAIL)
    }

    *classtype = (int_f)c_classtype;

done:
    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_preserve_c
 * Purpose:     Call H5Pset_preserve to set  transfer property for compound
 *              datatype
 * Inputs:      prp_id - property list identifier
 *              flag - TRUE/FALSE flag
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, February 17, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pset_preserve_c ( hid_t_f *prp_id , int_f *flag)
{
  int ret_value = 0;
  hid_t c_prp_id;
  herr_t status;
  hbool_t c_flag = 0;

  if (*flag > 0) c_flag = 1;
  c_prp_id = (hid_t)*prp_id;
  status = H5Pset_preserve(c_prp_id, c_flag);
  if ( status < 0  ) ret_value = -1;
  return ret_value;
}


/*----------------------------------------------------------------------------
 * Name:        h5pget_preserve_c
 * Purpose:     Call H5Pget_preserve to set  transfer property for compound
 *              datatype
 * Inputs:      prp_id - property list identifier
 * Outputs:     flag - TRUE/FALSE flag
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, February 17, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pget_preserve_c ( hid_t_f *prp_id , int_f *flag)
{
  int ret_value = 0;
  hid_t c_prp_id;
  int c_flag;

  c_prp_id = (hid_t)*prp_id;
  c_flag = H5Pget_preserve(c_prp_id);
  if ( c_flag < 0  ) ret_value = -1;
  *flag = (int_f)c_flag;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_deflate_c
 * Purpose:     Call H5Pset_deflate to set deflate level
 * Inputs:      prp_id - property list identifier
 *              level - level of deflation
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Saturday, August 14, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pset_deflate_c ( hid_t_f *prp_id , int_f *level)
{
  int ret_value = 0;
  hid_t c_prp_id;
  unsigned c_level;
  herr_t status;

  c_prp_id = (hid_t)*prp_id;
  c_level = (unsigned)*level;
  status = H5Pset_deflate(c_prp_id, c_level);
  if ( status < 0  ) ret_value = -1;
  return ret_value;
}



/*----------------------------------------------------------------------------
 * Name:        h5pset_chunk_c
 * Purpose:     Call H5Pset_chunk to set the sizes of chunks for a chunked
 *              layout dataset
 * Inputs:      prp_id - property list identifier
 *              rank - number of dimensions of each chunk
 *              dims - array of the size of each chunk
 * Returns:     0 on success, -1 on failure
 *              Saturday, August 14, 1999
 * Programmer:  Elena Pourmal
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pset_chunk_c ( hid_t_f *prp_id, int_f *rank, hsize_t_f *dims )
{
  int ret_value = -1;
  hid_t c_prp_id;
  int c_rank;
  hsize_t *c_dims;
  herr_t status;
  int i;

  c_dims =  (hsize_t *)HDmalloc(sizeof(hsize_t) * (*rank ));
  if (!c_dims) return ret_value;

  /*
   * Transpose dimension arrays because of C-FORTRAN storage order
   */
  for (i = 0; i < *rank ; i++) {
       c_dims[i] =  dims[*rank - i - 1];
  }

  c_prp_id = (hid_t)*prp_id;
  c_rank = (int)*rank;
  status = H5Pset_chunk(c_prp_id, c_rank, c_dims);
  if (status < 0) goto DONE;
  ret_value = 0;

DONE:
  HDfree (c_dims);
  return ret_value;
}


/*----------------------------------------------------------------------------
 * Name:        h5pget_chunk_c
 * Purpose:     Call H5Pget_chunk to get the sizes of chunks for a chunked
 *              layout dataset  for at list max_rank number of dimensions
 * Inputs:      prp_id - property list identifier
 *              max rank - maximum number of dimensions to return
 *              dims - array of the size of each chunk
 * Returns:     number of chunk's dimnesion on success, -1 on failure
 *              Saturday, August 14, 1999
 * Programmer:  Elena Pourmal
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pget_chunk_c ( hid_t_f *prp_id, int_f *max_rank, hsize_t_f *dims )
{
  int ret_value = -1;
  hid_t c_prp_id;
  hsize_t *c_dims;
  int rank;
  int c_max_rank;
  int i;

  c_dims =  (hsize_t *)HDmalloc(sizeof(hsize_t) * (*max_rank ));
  if (!c_dims) return ret_value;

  c_prp_id = (hid_t)*prp_id;
  c_max_rank = (int)*max_rank;
  rank = H5Pget_chunk(c_prp_id, c_max_rank, c_dims);

  /*
   * Transpose dimension arrays because of C-FORTRAN storage order
   */
  for (i = 0; i < *max_rank ; i++) {
       dims[*max_rank - i - 1] = (hsize_t_f)c_dims[i];
  }
  HDfree (c_dims);
  if (rank < 0) return ret_value;
  ret_value = (int_f)rank;
  return ret_value;
}



/*----------------------------------------------------------------------------
 * Name:        h5pset_fill_valuec_c
 * Purpose:     Call h5pset_fill_value_c to a character fill value
 * Inputs:      prp_id - property list identifier
 *              type_id - datatype identifier (fill value is of type type_id)
 *              fillvalue  - character value
 * Returns:     0 on success, -1 on failure
 *              Saturday, August 14, 1999
 * Programmer:  Elena Pourmal
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_fill_valuec_c (hid_t_f *prp_id, hid_t_f *type_id, _fcd fillvalue)
{
     int ret_value = -1;

     /*
      * Call h5pset_fill_value_c  function.
      */
     ret_value = nh5pset_fill_value_c(prp_id, type_id, _fcdtocp(fillvalue));

     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_fill_value_c
 * Purpose:     Call H5Pset_fill_value to set a fillvalue for a dataset
 * Inputs:      prp_id - property list identifier
 *              type_id - datatype identifier (fill value is of type type_id)
 *              fillvalue - fillvalue
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Saturday, August 14, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_fill_value_c (hid_t_f *prp_id, hid_t_f *type_id, void *fillvalue)
{
     int ret_value = -1;
     hid_t c_prp_id;
     hid_t c_type_id;
     herr_t ret;

     /*
      * Call H5Pset_fill_value function.
      */
     c_prp_id = (hid_t)*prp_id;
     c_type_id = (int)*type_id;
     ret = H5Pset_fill_value(c_prp_id, c_type_id, fillvalue);

     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}

int_f
nh5pset_fill_value_integer_c (hid_t_f *prp_id, hid_t_f *type_id, void *fillvalue)
{
     /*
      * Call h5pset_fill_value_c  function.
      */
     return nh5pset_fill_value_c(prp_id, type_id, fillvalue);
}

int_f
nh5pset_fill_value_real_c (hid_t_f *prp_id, hid_t_f *type_id, void *fillvalue)
{
     /*
      * Call h5pset_fill_value_c  function.
      */
     return nh5pset_fill_value_c(prp_id, type_id, fillvalue);
}

int_f
nh5pset_fill_value_double_c (hid_t_f *prp_id, hid_t_f *type_id, void *fillvalue)
{
     /*
      * Call h5pset_fill_value_c  function.
      */
     return nh5pset_fill_value_c(prp_id, type_id, fillvalue);
}


/*----------------------------------------------------------------------------
 * Name:        h5pget_fill_valuec_c
 * Purpose:     Call h5pget_fill_value_c to a character fill value
 * Inputs:      prp_id - property list identifier
 *              type_id - datatype identifier (fill value is of type type_id)
 *              fillvalue  - character value
 * Returns:     0 on success, -1 on failure
 *              Saturday, August 14, 1999
 * Programmer:  Elena Pourmal
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_fill_valuec_c (hid_t_f *prp_id, hid_t_f *type_id, _fcd fillvalue)
{
     int ret_value = -1;

     /*
      * Call h5pget_fill_value_c  function.
      */
     ret_value = nh5pset_fill_value_c(prp_id, type_id, _fcdtocp(fillvalue));

     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_fill_value_c
 * Purpose:     Call H5Pget_fill_value to set a fillvalue for a dataset
 * Inputs:      prp_id - property list identifier
 *              type_id - datatype identifier (fill value is of type type_id)
 *              fillvalue - fillvalue
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Saturday, August 14, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_fill_value_c (hid_t_f *prp_id, hid_t_f *type_id, void *fillvalue)
{
     int ret_value = -1;
     hid_t c_prp_id;
     hid_t c_type_id;
     herr_t ret;

     /*
      * Call H5Pget_fill_value function.
      */
     c_prp_id = (hid_t)*prp_id;
     c_type_id = (int)*type_id;
     ret = H5Pget_fill_value(c_prp_id, c_type_id, fillvalue);

     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}

int_f
nh5pget_fill_value_integer_c (hid_t_f *prp_id, hid_t_f *type_id, void *fillvalue)
{
     /*
      * Call h5pget_fill_value_c  function.
      */
     return nh5pset_fill_value_c(prp_id, type_id, fillvalue);
}

int_f
nh5pget_fill_value_real_c (hid_t_f *prp_id, hid_t_f *type_id, void *fillvalue)
{
     /*
      * Call h5pget_fill_value_c  function.
      */
     return nh5pset_fill_value_c(prp_id, type_id, fillvalue);
}

int_f
nh5pget_fill_value_double_c (hid_t_f *prp_id, hid_t_f *type_id, void *fillvalue)
{
     /*
      * Call h5pget_fill_value_c  function.
      */
     return nh5pset_fill_value_c(prp_id, type_id, fillvalue);
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_version_c
 * Purpose:     Call H5Pget_version to get the version information
 *              of various objects for a file creation property list
 * Inputs:      prp_id - property list identifier
 * Outputs:     boot - array to put boot block version number
 *              freelist - array to put global freelist version number
 *              stab - array to put symbol table version number
 *              shhdr - array to put shared object header version number
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Wednesday, February 23, 2000
 * Modifications: Removed extra length parameters EP 7/6/00
 *---------------------------------------------------------------------------*/
int_f
nh5pget_version_c (hid_t_f *prp_id, int_f * boot,int_f * freelist, int_f * stab, int_f *shhdr)
{
     int ret_value = -1;
#ifndef H5_NO_DEPRECATED_SYMBOLS
     herr_t ret;
     unsigned c_boot;
     unsigned c_freelist;
     unsigned c_stab;
     unsigned c_shhdr;

     /*
      * Call H5Pget_version function.
      */
     ret = H5Pget_version((hid_t)*prp_id, &c_boot, &c_freelist, &c_stab, &c_shhdr);
     if (ret < 0) return ret_value;

     *boot = (int_f)c_boot;
     *freelist = (int_f)c_freelist;
     *stab = (int_f)c_stab;
     *shhdr = (int_f)c_shhdr;
#else /* H5_NO_DEPRECATED_SYMBOLS */
     /*
      * Fill in fake values [since we need a file ID to call H5Fget_info :-( -QAK ]
      */
     *boot = (int_f)0;
     *freelist = (int_f)0;
     *stab = (int_f)0;
     *shhdr = (int_f)0;
#endif /* H5_NO_DEPRECATED_SYMBOLS */
     ret_value = 0;

     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_userblock_c
 * Purpose:     Call H5Pget_userblock to get the size of a user block in
 *              a file creation property list
 * Inputs:      prp_id - property list identifier
 * Outputs      size - Size of the user-block in bytes
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Wednesday, February 23, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_userblock_c (hid_t_f *prp_id, hsize_t_f * size)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     hsize_t c_size;

     /*
      * Call H5Pget_userblock function.
      */
     c_prp_id = (hid_t)*prp_id;
     ret = H5Pget_userblock(c_prp_id, &c_size);
     if (ret < 0) return ret_value;

     *size = (hsize_t_f)c_size;
     ret_value = 0;

     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_userblock_c
 * Purpose:     Call H5Pset_userblock to set the size of a user block in
 *              a file creation property list
 * Inputs:      prp_id - property list identifier
 *              size - Size of the user-block in bytes
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Wednesday, February 23, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_userblock_c (hid_t_f *prp_id, hsize_t_f * size)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     hsize_t c_size;
     c_size = (hsize_t)*size;

     /*
      * Call H5Pset_userblock function.
      */
     c_prp_id = (hid_t)*prp_id;
     ret = H5Pset_userblock(c_prp_id, c_size);

     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_sizes_c
 * Purpose:     Call H5Pget_sizes to get the size of the offsets
 *              and lengths used in an HDF5 file
 * Inputs:      prp_id - property list identifier
 * Outputs      sizeof_addr - Size of an object offset in bytes
 *              sizeof_size - Size of an object length in bytes
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Wednesday, February 23, 2000
 * Modifications: Deleted extra length parameters. EP 6/7/00
 *---------------------------------------------------------------------------*/
int_f
nh5pget_sizes_c (hid_t_f *prp_id, size_t_f * sizeof_addr, size_t_f * sizeof_size)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     size_t c_sizeof_addr;
     size_t c_sizeof_size;

     /*
      * Call H5Pget_sizes function.
      */
     c_prp_id = (hid_t)*prp_id;
     ret = H5Pget_sizes(c_prp_id, &c_sizeof_addr, &c_sizeof_size);
     if (ret < 0) return ret_value;

     *sizeof_addr = (size_t_f)c_sizeof_addr;
     *sizeof_size = (size_t_f)c_sizeof_size;
     ret_value = 0;

     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_sizes_c
 * Purpose:     Call H5Pset_sizes to set the size of the offsets
 * Inputs:      prp_id - property list identifier
 *              sizeof_addr - Size of an object offset in bytes
 *              sizeof_size - Size of an object length in bytes
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Wednesday, February 23, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_sizes_c (hid_t_f *prp_id, size_t_f * sizeof_addr, size_t_f * sizeof_size)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     size_t c_addr, c_size;
     c_addr = (size_t)*sizeof_addr;
     c_size = (size_t)*sizeof_size;

     /*
      * Call H5Pset_sizes function.
      */
     c_prp_id = (hid_t)*prp_id;
     ret = H5Pset_sizes(c_prp_id, c_addr, c_size);

     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_sym_k_c
 * Purpose:     Call H5Pset_sym_k to set the size of parameters used
 *              to control the symbol table node
 * Inputs:      prp_id - property list identifier
 *              ik - Symbol table tree rank
 *              lk - Symbol table node size
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_sym_k_c (hid_t_f *prp_id, int_f* ik, int_f* lk)
{
     int ret_value = -1;
     hid_t c_prp_id;
     unsigned c_ik;
     unsigned c_lk;
     herr_t ret;

     /*
      * Call H5Pset_sym_k function.
      */
     c_prp_id = (hid_t)*prp_id;
     c_ik = (unsigned)*ik;
     c_lk = (unsigned)*lk;
     ret = H5Pset_sym_k(c_prp_id, c_ik, c_lk);

     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_sym_k_c
 * Purpose:     Call H5Pget_sym_k to get the size of parameters used
 *              to control the symbol table node
 * Inputs:      prp_id - property list identifier
 * Outputs:     ik - Symbol table tree rank
 *              lk - Symbol table node size
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_sym_k_c (hid_t_f *prp_id, int_f* ik, int_f* lk)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     unsigned c_ik;
     unsigned c_lk;

     /*
      * Call H5Pget_sym_k function.
      */
     c_prp_id = (hid_t)*prp_id;
     ret = H5Pget_sym_k(c_prp_id, &c_ik, &c_lk);
     *ik = (int_f)c_ik;
     *lk = (int_f)c_lk;
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_istore_k_c
 * Purpose:     Call H5Pset_istore_k to set the size of the parameter
 *              used to control the B-trees for indexing chunked datasets
 * Inputs:      prp_id - property list identifier
 *              ik - Symbol table tree rank
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_istore_k_c (hid_t_f *prp_id, int_f* ik)
{
     int ret_value = -1;
     hid_t c_prp_id;
     unsigned c_ik;
     herr_t ret;

     /*
      * Call H5Pset_istore_k function.
      */
     c_prp_id = (hid_t)*prp_id;
     c_ik = (unsigned)*ik;
     ret = H5Pset_istore_k(c_prp_id, c_ik);

     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_istore_k_c
 * Purpose:     Call H5Pget_istore_k to get the size of parameters used
 *              to control the B-trees for indexing chunked datasets
 * Inputs:      prp_id - property list identifier
 * Outputs:     ik - Symbol table tree rank
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_istore_k_c (hid_t_f *prp_id, int_f* ik)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     unsigned c_ik;

     /*
      * Call H5Pget_istore_k function.
      */
     c_prp_id = (hid_t)*prp_id;
     ret = H5Pget_istore_k(c_prp_id, &c_ik);
     *ik = (int_f)c_ik;
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_driver_c
 * Purpose:     Call H5Pget_driver to get low-level file driver identifier
 * Inputs:      prp_id - property list identifier
 * Outputs:     driver - low-level file driver identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_driver_c (hid_t_f *prp_id, hid_t_f* driver)
{
     int ret_value = -1;
     hid_t c_driver;

     /*
      * Call H5Pget_driver function.
      */
     c_driver = H5Pget_driver((hid_t)*prp_id);
     if (c_driver < 0) goto DONE;

     *driver = (hid_t_f) c_driver;
     ret_value = 0;

DONE:
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_fapl_stdio_c
 * Purpose:     Call H5Pset_stdio to set the low level file driver to
 *              use the functions declared in the stdio.h
 * Inputs:      prp_id - property list identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              March 7, 2001
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_fapl_stdio_c (hid_t_f *prp_id)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret = -1;
     /*
      * Call H5Pset_fapl_stdio function.
      */
     c_prp_id = (hid_t)*prp_id;
     ret = H5Pset_fapl_stdio(c_prp_id);
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}
#ifdef NO_SUCH_F90_FUNCTION
/*----------------------------------------------------------------------------
 * Name:        h5pget_fapl_stdio_c
 * Purpose:     Call H5Pget_fapl_stdio to determine whther the low level file driver
 *              uses the functions declared in the stdio.h
 * Inputs:      prp_id - property list identifier
 * Outputs:     io - value indicates whether the file driver uses
 *                   the functions declared in the stdio.h
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              March 9, 2001
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_fapl_stdio_c (hid_t_f *prp_id, int_f* io)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret = -1;
     /*
      * Call H5Pget_fapl_stdio function.
      */
     c_prp_id = *prp_id;
     ret = H5Pget_fapl_stdio(c_prp_id);
     if (ret < 0) return ret_value;
     *io = (int_f)ret;
     ret_value = 0;
     return ret_value;
}

#endif /*NO_SUCH_F90_FUNCTION*/

/*----------------------------------------------------------------------------
 * Name:        h5pset_fapl_sec2_c
 * Purpose:     Call H5Pset_fapl_sec2 to set the low level file driver to
 *              use the functions declared in the  unistd.h
 * Inputs:      prp_id - property list identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              March 9, 2001
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_fapl_sec2_c (hid_t_f *prp_id)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret = -1;
     /*
      * Call H5Pset_fapl_sec2 function.
      */
     c_prp_id = (hid_t)*prp_id;
     ret = H5Pset_fapl_sec2(c_prp_id);
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}

#ifdef NO_SUCH_F90_FUNCTION
/*----------------------------------------------------------------------------
 * Name:        h5pget_fapl_sec2_c
 * Purpose:     Call H5Pget_fapl_stdio to determine whther the low level file driver
 *              uses the functions declared in the  unistd.h
 * Inputs:      prp_id - property list identifier
 * Outputs:     sec2 - value indicates whether the file driver uses
 *                   the functions declared in the  unistd.h
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              March 9, 2001
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_fapl_sec2_c (hid_t_f *prp_id, int_f* sec2)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret = -1;
     /*
      * Call H5Pget_fapl_sec2 function.
      */
     c_prp_id = (hid_t)*prp_id;
     ret = H5Pget_fapl_sec2(c_prp_id);
     if (ret < 0) return ret_value;
     *sec2 = (int_f)ret;
     ret_value = 0;
     return ret_value;
}
#endif /*NO_SUCH_F90_FUNCTION*/

/*----------------------------------------------------------------------------
 * Name:        h5pset_alignment_c
 * Purpose:     Call H5Pset_alignment to set alignment properties of
 *              a file access property list
 * Inputs:      prp_id - property list identifier
 *              threshold - Threshold value
 *              alignment - Alignment value
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_alignment_c (hid_t_f *prp_id, hsize_t_f* threshold, hsize_t_f* alignment)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     hsize_t c_threshold, c_alignment;
     c_threshold = (hsize_t)*threshold;
     c_alignment = (hsize_t)* alignment;
     /*
      * Call H5Pset_alignment function.
      */
     c_prp_id = (hid_t)*prp_id;
     ret = H5Pset_alignment(c_prp_id, c_threshold, c_alignment);
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_alignment_c
 * Purpose:     Call H5Pget_alignment to get alignment properties of
 *              a file access property list
 * Inputs:      prp_id - property list identifier
 *              threshold - Threshold value
 *              alignment - Alignment value
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_alignment_c (hid_t_f *prp_id, hsize_t_f* threshold, hsize_t_f* alignment)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     hsize_t c_threshold, c_alignment;
     /*
      * Call H5Pget_alignment function.
      */
     c_prp_id = (hid_t)*prp_id;
     ret = H5Pget_alignment(c_prp_id, &c_threshold, &c_alignment);
     if (ret < 0) return ret_value;
     *threshold = (hsize_t_f)c_threshold;
     *alignment = (hsize_t_f)c_alignment;

     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_fapl_core_c
 * Purpose:     Call H5Pset_fapl_core to set the low-level file driver
 *              to use malloc() and free()
 * Inputs:      prp_id - property list identifier
 *              increment - File block size in bytes
 *              flag - Boolean flag indicating whether to write the
 *              file contents to disk when the file is closed.
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              March 9, 2001
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_fapl_core_c (hid_t_f *prp_id, size_t_f* increment, int_f *flag)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret = -1;
     size_t c_increment;
     hbool_t c_backing_store;
     c_increment = (size_t)*increment;
     c_backing_store = (hbool_t)*flag;

     /*
      * Call H5Pset_fapl_core function.
      */
     c_prp_id = (hid_t)*prp_id;
     ret = H5Pset_fapl_core(c_prp_id, c_increment, c_backing_store);
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_fapl_core_c
 * Purpose:     Call H5Pget_fapl_core to determine whether the file access
 *              property list is set to the core drive
 * Inputs:      prp_id - property list identifier
 * Outputs      increment - File block size in bytes
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              March 9, 2001
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_fapl_core_c (hid_t_f *prp_id, size_t_f* increment, int_f *flag)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret = -1;
     size_t c_increment = 0;
     hbool_t c_backing_store;
     *flag = 0;
     /*
      * Call H5Pset_fapl_core function.
      */
     c_prp_id = (hid_t)*prp_id;
     ret = H5Pget_fapl_core(c_prp_id, &c_increment, &c_backing_store);
     if (ret < 0) return ret_value;
     *increment = (size_t_f)c_increment;
     if(c_backing_store  > 0) *flag = 1;
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_fapl_family_c
 * Purpose:     Call H5Pset_fapl_family to set the file access properties list
 *              to the family driver
 * Inputs:      prp_id - property list identifier
 *              memb_size -  Logical size, in bytes, of each family member.
 *              memb_plist - Identifier of the file access property list
 *                           for each member of the family
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              March 9, 2001
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_fapl_family_c(hid_t_f *prp_id, hsize_t_f* memb_size, hid_t_f* memb_plist )
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret = -1;
     hsize_t c_memb_size;
     hid_t c_memb_plist;
     c_memb_size =(hsize_t) *memb_size;
     c_memb_plist =(hid_t) *memb_plist;
     /*
      * Call H5Pset_fapl_family function.
      */
     c_prp_id = (hid_t)*prp_id;
     ret = H5Pset_fapl_family(c_prp_id, c_memb_size, c_memb_plist);
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_fapl_family_c
 * Purpose:     Call H5Pget_fapl_family to determine whether the file access
 *              property list is set to the family driver
 * Inputs:      prp_id - property list identifier
 *              memb_size -  Logical size, in bytes, of each family member.
 *              memb_plist - Identifier of the file access property list
 *                           for each member of the family
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              March 9, 2001
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_fapl_family_c(hid_t_f *prp_id, hsize_t_f* memb_size, hid_t_f* memb_plist)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret = -1;
     hsize_t c_memb_size = 0;
     hid_t c_memb_plist = -1;
     /*
      * Call H5Pget_fapl_family function.
      */
     c_prp_id = (hid_t)*prp_id;
     ret = H5Pget_fapl_family(c_prp_id, &c_memb_size, &c_memb_plist);
     if (ret < 0) return ret_value;
     *memb_size = (hsize_t_f)c_memb_size;
     *memb_plist = (hid_t_f)c_memb_plist;

     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_cache_c
 * Purpose:     Call H5Pset_cache to set he number of elements in
 *              the meta data cache and the total number of bytes in
 *              the raw data chunk cache
 * Inputs:      prp_id - property list identifier
 *              mdc_nelmts - Number of elements (objects) in the
 *                           meta data cache
 *              rdcc_nbytes - Total size of the raw data chunk cache, in bytes
 *              rdcc_w0 - Preemption policy
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications: Changed the type of the rdcc_w0 parameter to be real_f EP 7/7/00
 *                instead of double
 *---------------------------------------------------------------------------*/
int_f
nh5pset_cache_c(hid_t_f *prp_id, int_f* mdc_nelmts, size_t_f* rdcc_nelmts,  size_t_f* rdcc_nbytes , real_f* rdcc_w0 )
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     int c_mdc_nelmts;
     size_t c_rdcc_nelmts;
     size_t c_rdcc_nbytes;
     double c_rdcc_w0;
     c_rdcc_nbytes =(size_t) *rdcc_nbytes;
     c_rdcc_w0 = (double)*rdcc_w0;

     /*
      * Call H5Pset_cache function.
      */
     c_prp_id = (hid_t)*prp_id;
     c_mdc_nelmts = (int)*mdc_nelmts;
     c_rdcc_nelmts = (size_t)*rdcc_nelmts;
     ret = H5Pset_cache(c_prp_id, c_mdc_nelmts, c_rdcc_nelmts, c_rdcc_nbytes, c_rdcc_w0  );
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_cache_c
 * Purpose:     Call H5Pget_cache to get he number of elements in
 *              the meta data cache and the total number of bytes in
 *              the raw data chunk cache
 * Inputs:      prp_id - property list identifier
 * Outputs:     mdc_nelmts - Number of elements (objects) in the
 *                           meta data cache
 *              rdcc_nelmts - Number of elements in the raw data chunk
 *              rdcc_nbytes - Total size of the raw data chunk cache, in bytes
 *              rdcc_w0 - Preemption policy
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications: Changed type of the rdcc_w0 parameter to be real_f instead of double
 *                Changed type of the rdcc_nelmts parameter to be int_f.
 *                                                          EIP  October 10, 2003
 *---------------------------------------------------------------------------*/
int_f
nh5pget_cache_c(hid_t_f *prp_id, int_f* mdc_nelmts, size_t_f* rdcc_nelmts, size_t_f* rdcc_nbytes , real_f* rdcc_w0)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     int c_mdc_nelmts;
     size_t c_rdcc_nelmts;
     size_t c_rdcc_nbytes;
     double c_rdcc_w0;
     /*
      * Call H5Pget_cache function.
      */
     c_prp_id = (hid_t)*prp_id;
     ret = H5Pget_cache(c_prp_id, &c_mdc_nelmts, &c_rdcc_nelmts, &c_rdcc_nbytes, &c_rdcc_w0);
     if (ret < 0) return ret_value;
     *mdc_nelmts = (int_f)c_mdc_nelmts;
     *rdcc_nelmts = (size_t_f)c_rdcc_nelmts;
     *rdcc_nbytes = (size_t_f)c_rdcc_nbytes;
     *rdcc_w0 = (real_f)c_rdcc_w0;

     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_fapl_split_c
 * Purpose:     Call H5Pset_fapl_split to set he low-level driver to split meta data
 *              from raw data
 * Inputs:      prp_id - property list identifier
 *              meta_len - Length of meta_ext
 *              meta_ext - Name of the extension for the metafile filename.
 *              meta_plist - Identifier of the meta file access property list
 *              raw_len - Length of raw _ext
 *              raw_ext - Name of the extension for the raw file filename.
 *              raw_plist - Identifier of the raw  file access property list
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              March 9, 2001
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_fapl_split_c(hid_t_f *prp_id, int_f* meta_len, _fcd meta_ext, hid_t_f* meta_plist, int_f* raw_len, _fcd raw_ext, hid_t_f * raw_plist)
{
     int ret_value = -1;
     hid_t c_prp_id;
     hid_t c_meta_plist;
     hid_t c_raw_plist;
     herr_t ret = -1;
     char* c_meta_ext;
     char* c_raw_ext;

     c_meta_ext = (char *)HD5f2cstring(meta_ext, (size_t)*meta_len);
     if (c_meta_ext == NULL) return ret_value;
     c_raw_ext = (char *)HD5f2cstring(raw_ext, (size_t)*raw_len);
     if (c_raw_ext == NULL) { HDfree(c_meta_ext);
                              return ret_value;
                            }

     /*
      * Call H5Pset_fapl_split function.
      */
     c_prp_id = (hid_t)*prp_id;
     c_meta_plist = (hid_t)*meta_plist;
     c_raw_plist = (hid_t)*raw_plist;
     ret = H5Pset_fapl_split(c_prp_id, c_meta_ext, c_meta_plist, c_raw_ext, c_raw_plist );

     if (ret < 0) goto DONE;
     ret_value = 0;

DONE:
     HDfree(c_meta_ext);
     HDfree(c_raw_ext);
     return ret_value;
}


#ifdef NO_SUCH_F90_FUNCTION
/*----------------------------------------------------------------------------
 * Name:        h5pget_fapl_split_c
 * Purpose:     Call H5Pget_fapl_split to determine whether the file access
 *              property list is set to the split driver
 * Inputs:      prp_id - property list identifier
 *              meta_ext_size - Number of characters of the meta file extension
 *                              to be copied to the meta_ext buffer
 *              raw_ext_size - Number of characters of the raw file extension
 *                              to be copied to the raw_ext buffer
 *Outputs:      meta_ext - Name of the extension for the metafile filename.
 *              meta_plist - Identifier of the meta file access property list
 *              raw_ext - Name of the extension for the raw file filename.
 *              raw_plist - Identifier of the raw  file access property list
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              March 9 , 2001
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_fapl_split_c(hid_t_f *prp_id, size_t_f* meta_ext_size , _fcd meta_ext, hid_t_f* meta_plist, size_t_f* raw_ext_size, _fcd raw_ext, hid_t_f * raw_plist)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret = -1;
     size_t c_meta_ext_size, c_raw_ext_size;
     hid_t c_meta_plist = -1;
     hid_t c_raw_plist = -1;

     char* c_meta_ext = NULL;
     char* c_raw_ext  = NULL;

     c_meta_ext_size = (size_t) *meta_ext_size;
     c_raw_ext_size = (size_t) *raw_ext_size;
     c_meta_ext = (char *)HDmalloc(sizeof(char) * c_meta_ext_size);
     c_raw_ext = (char *)HDmalloc(sizeof(char) * c_raw_ext_size);
     if(c_meta_ext == NULL || c_raw_ext == NULL) return ret_value;

     /*
      * Call H5Pget_fapl_split function.
      */
     c_prp_id = *prp_id;
     ret = H5Pget_fapl_split(c_prp_id, c_meta_ext_size, c_meta_ext,&c_meta_plist, c_raw_ext_size, c_raw_ext, &c_raw_plist );

     if (ret < 0) return ret_value;
     *meta_plist = c_meta_plist;
     *raw_plist = c_raw_plist;
     HD5packFstring(c_meta_ext, _fcdtocp(meta_ext), strlen(c_meta_ext));
     HD5packFstring(c_raw_ext, _fcdtocp(raw_ext), strlen(c_raw_ext));

     ret_value = 0;
     return ret_value;
}
#endif /*NO_SUCH_F90_FUNCTION*/

/*----------------------------------------------------------------------------
 * Name:        h5pset_gc_references_c
 * Purpose:     Call H5Pset_gc_references to set garbage
 *              collecting references flag
 * Inputs:      prp_id - property list identifier
 *              gc_reference - flag for garbage collecting references
 *                             for the file
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_gc_references_c (hid_t_f *prp_id, int_f* gc_references)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     unsigned c_gc_references;
     c_gc_references = (unsigned)*gc_references;

     /*
      * Call H5Pset_gc_references function.
      */
     c_prp_id = *prp_id;
     ret = H5Pset_gc_references(c_prp_id, c_gc_references);

     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_gc_references_c
 * Purpose:     Call H5Pget_gc_references to set garbage
 *              collecting references flag
 * Inputs:      prp_id - property list identifier
 * Outputs      gc_reference - flag for garbage collecting references
 *                             for the file
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_gc_references_c (hid_t_f *prp_id, int_f* gc_references)
{
     int ret_value = -1;
     hid_t c_prp_id;
     unsigned c_gc_references;
     herr_t ret;
     /*
      * Call H5Pget_gc_references function.
      */
     c_prp_id = (hid_t)*prp_id;
     ret = H5Pget_gc_references(c_prp_id, &c_gc_references);
     if (ret < 0) return ret_value;
     *gc_references = (int_f)c_gc_references;
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_layout_c
 * Purpose:     Call H5Pset_layout to the type of storage used
 *              store the raw data for a dataset
 * Inputs:      prp_id - property list identifier
 *              layout - Type of storage layout for raw data.
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_layout_c (hid_t_f *prp_id, int_f* layout)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     H5D_layout_t c_layout;
     c_layout = (H5D_layout_t)*layout;
     /*
      * Call H5Pset_layout function.
      */
     c_prp_id = (hid_t)*prp_id;
     ret = H5Pset_layout(c_prp_id, c_layout);

     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_layout_c
 * Purpose:     Call H5Pget_layout to the type of storage used
 *              store the raw data for a dataset
 * Inputs:      prp_id - property list identifier
 * Outputs:     layout - Type of storage layout for raw data.
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_layout_c (hid_t_f *prp_id, int_f* layout)
{
     int ret_value = -1;
     hid_t c_prp_id;
     H5D_layout_t c_layout;
     /*
      * Call H5Pget_layout function.
      */
     c_prp_id = (hid_t)*prp_id;
     c_layout = H5Pget_layout(c_prp_id);
     if (c_layout < 0) return ret_value;
     *layout = (int_f)c_layout;
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_filter_c
 * Purpose:     Call H5Pset_filter to add a filter to the filter pipeline.
 * Inputs:      prp_id - property list identifier
 *              filter - Filter to be added to the pipeline.
 *              flags - Bit vector specifying certain general
 *                      properties of the filter.
 *              cd_nelmts - Number of elements in cd_values.
 *              cd_values - Auxiliary data for the filter.
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Wednesday, February 23, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_filter_c (hid_t_f *prp_id, int_f* filter, int_f* flags, size_t_f* cd_nelmts, int_f* cd_values )
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     size_t c_cd_nelmts;
     unsigned int c_flags;
     H5Z_filter_t c_filter;
     unsigned int * c_cd_values;
     unsigned i;

     c_filter = (H5Z_filter_t)*filter;
     c_flags = (unsigned)*flags;
     c_cd_nelmts = (size_t)*cd_nelmts;
     c_cd_values = (unsigned int*)HDmalloc(sizeof(unsigned int) * ((int)c_cd_nelmts));
     if (!c_cd_values) return ret_value;
     for (i = 0; i < c_cd_nelmts; i++)
          c_cd_values[i] = (unsigned int)cd_values[i];

     /*
      * Call H5Pset_filter function.
      */
     c_prp_id = (hid_t)*prp_id;
     ret = H5Pset_filter(c_prp_id, c_filter, c_flags, c_cd_nelmts,c_cd_values );

     if (ret < 0) goto DONE;
     ret_value = 0;

DONE:
     HDfree(c_cd_values);
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_nfilters_c
 * Purpose:     Call H5Pget_nfilters to get the number of filters
 *              in the pipeline
 * Inputs:      prp_id - property list identifier
 * Outputs:     nfilters - number of filters defined in the filter pipline
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_nfilters_c (hid_t_f *prp_id, int_f* nfilters)
{
     int ret_value = -1;
     hid_t c_prp_id;
     int c_nfilters;
     /*
      * Call H5Pget_nfilters function.
      */
     c_prp_id = (hid_t)*prp_id;
     c_nfilters = H5Pget_nfilters(c_prp_id);
     if (c_nfilters < 0) return ret_value;

     *nfilters = (int_f)c_nfilters;
     ret_value = 0;

     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_filter_c
 * Purpose:     Call H5Pget_filter2 to get information about a filter
 *              in a pipeline
 * Inputs:      prp_id - property list identifier
 *              filter_number - Sequence number within the filter
 *                              pipeline of the filter for which
 *                              information is sought.
 *              namelen - Anticipated number of characters in name.
 *Outputs:      flags - Bit vector specifying certain general
 *                      properties of the filter.
 *              cd_nelmts - Number of elements in cd_value
 *              cd_values - Auxiliary data for the filter.
 *              name - Name of the filter
 *              filter_id - filter identification number
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications:
 *              Since cd_nelmts has IN/OUT attributes, fixed the input and
 *              returned value of cd_nelmnts to satisfy this specification.
 *              MSB January 27, 2009
 *---------------------------------------------------------------------------*/
int_f
nh5pget_filter_c(hid_t_f *prp_id, int_f* filter_number, int_f* flags, size_t_f* cd_nelmts, int_f* cd_values, size_t_f *namelen, _fcd name, int_f* filter_id)
{
    unsigned int  c_flags;
    size_t c_cd_nelmts;
    H5Z_filter_t c_filter;
    unsigned int *c_cd_values = NULL;
    char *c_name = NULL;
    unsigned i;
    int ret_value = -1;

    c_cd_nelmts = (size_t)*cd_nelmts;

    if(NULL == (c_name = (char *)HDmalloc((size_t)*namelen + 1)))
        goto DONE;

    if(NULL == (c_cd_values = (unsigned int *)HDmalloc(sizeof(unsigned int) * c_cd_nelmts)))
        goto DONE;

    /*
     * Call H5Pget_filter2 function.
     */
    if((c_filter = H5Pget_filter2((hid_t)*prp_id, (unsigned)*filter_number, &c_flags, &c_cd_nelmts, c_cd_values, (size_t)*namelen, c_name, NULL)) < 0)
        goto DONE;

    *filter_id = (int_f)c_filter;
    *cd_nelmts = (size_t_f)c_cd_nelmts;
    *flags = (int_f)c_flags;
    HD5packFstring(c_name, _fcdtocp(name), strlen(c_name));

    for(i = 0; i < c_cd_nelmts; i++)
         cd_values[i] = (int_f)c_cd_values[i];

    ret_value = 0;

DONE:
    if(c_name)
        HDfree(c_name);
    if(c_cd_values)
        HDfree(c_cd_values);
    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_external_c
 * Purpose:     Call H5Pset_external to add an external file to the
 *              list of external files.
 * Inputs:      prp_id - property list identifier
 *              name - Name of an external file
 *              namelen - length of name
 *              offset - Offset, in bytes, from the beginning of the file
 *                       to the location in the file where the data starts.
 *              bytes - Number of bytes reserved in the file for the data.
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Wednesday, February 23, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_external_c (hid_t_f *prp_id, _fcd name, int_f* namelen, int_f* offset, hsize_t_f*bytes)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     hsize_t c_bytes;
     char* c_name;
     size_t c_namelen;
     off_t c_offset;
     c_bytes = (hsize_t) *bytes;
     c_offset = (off_t) *offset;


     c_namelen = (int)*namelen;
     c_name = (char *)HD5f2cstring(name, c_namelen);
     if (c_name == NULL) return ret_value;

     /*
      * Call H5Pset_external function.
      */
     c_prp_id = (hid_t)*prp_id;
     ret = H5Pset_external(c_prp_id, c_name, c_offset, c_bytes);

     if (ret < 0) goto DONE;
     ret_value = 0;

DONE:
     HDfree(c_name);
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_external_count_c
 * Purpose:     Call H5Pget_external_count to get the number of external
 *              files for the specified dataset.
 * Inputs:      prp_id - property list identifier
 * Outputs:     count - number of external files
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_external_count_c (hid_t_f *prp_id, int_f* count)
{
     int ret_value = -1;
     hid_t c_prp_id;
     int c_count;
     /*
      * Call H5Pget_external_count function.
      */
     c_prp_id = (hid_t)*prp_id;
     c_count = H5Pget_external_count(c_prp_id);
     if (c_count < 0) return ret_value;
     *count = (int_f)c_count;
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_external_c
 * Purpose:     Call H5Pget_external to get nformation about an external file.
 * Inputs:      prp_id - property list identifier
 *              name_size - length of name
 *              idx - External file index.
 *Outputs:      name - Name of an external file
 *              offset - Offset, in bytes, from the beginning of the file
 *                       to the location in the file where the data starts.
 *              bytes - Number of bytes reserved in the file for the data.
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Wednesday, February 23, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_external_c(hid_t_f *prp_id, int_f *idx, size_t_f* name_size, _fcd name, int_f* offset, hsize_t_f*bytes)
{
     int ret_value = -1;
     hid_t c_prp_id;
     unsigned c_idx;
     herr_t status;
     size_t c_namelen;
     char* c_name = NULL;
     off_t c_offset;
     hsize_t size;

     c_namelen = (size_t)*name_size;
     /*
      * Allocate memory to store the name of the external file.
      */
     if(c_namelen) c_name = (char*)HDmalloc(c_namelen + 1);
     if (c_name == NULL) return ret_value;

     /*
      * Call H5Pget_external function.
      */
     c_prp_id = (hid_t)*prp_id;
     c_idx = (unsigned)*idx;
     status = H5Pget_external(c_prp_id, c_idx, c_namelen+1, c_name, &c_offset, &size );

     if (status < 0) goto DONE;

     *offset = (int_f)c_offset;
     *bytes = (hsize_t_f)size;
     /* Note: if the size of the fortran buffer is larger then the returned string
      *       from the function then we need to give HD5packFstring the fortran buffer size so
      *       that it fills the remaining unused characters with blanks. MSB
      */
     HD5packFstring(c_name, _fcdtocp(name), c_namelen+1);
     ret_value = 0;

DONE:
     HDfree(c_name);
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_btree_ratios_c
 * Purpose:     Call H5Pset_btree_ratios to set B-tree split ratios for B-tree split ratios for a dataset transfer property list. a
 *              dataset transfer property list.
 * Inputs:      prp_id - property list identifier
 *              left - The B-tree split ratio for left-most nodes.
 *              middle - The B-tree split ratio for all other nodes
 *              right - The B-tree split ratio for right-most nodes
 *                      and lone nodes.
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications: Changed the type of the last three parameters from double to real_f
 *---------------------------------------------------------------------------*/
int_f
nh5pset_btree_ratios_c(hid_t_f *prp_id, real_f* left, real_f* middle, real_f* right)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     double c_left;
     double c_middle;
     double c_right;
     c_left = (double)*left;
     c_middle = (double)*middle;
     c_right = (double)*right;

     /*
      * Call H5Pset_btree_ratios function.
      */
     c_prp_id = (hid_t)*prp_id;
     ret = H5Pset_btree_ratios(c_prp_id, c_left, c_middle, c_right);
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_btree_ratios_c
 * Purpose:     Call H5Pget_btree_ratios to Gets B-tree split ratios
 *              for a dataset transfer property list.
 * Inputs:      prp_id - property list identifier
 *              left - The B-tree split ratio for left-most nodes.
 *              middle - The B-tree split ratio for all other nodes
 *              right - The B-tree split ratio for right-most nodes
 *                      and lone nodes.
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications: Changed the type of the last three parameters from double to real_f
 *---------------------------------------------------------------------------*/
int_f
nh5pget_btree_ratios_c(hid_t_f *prp_id, real_f* left, real_f* middle, real_f* right)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     double c_left, c_right, c_middle;

     /*
      * Call H5Pget_btree_ratios function.
      */
     c_prp_id = (hid_t)*prp_id;
     ret = H5Pget_btree_ratios(c_prp_id, &c_left, &c_middle, &c_right);
     *left = (real_f)c_left;
     *middle = (real_f)c_middle;
     *right = (real_f)c_right;
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5pget_fclose_degree_c
 * Purpose:     Call H5Pget_fclose_degree to determine file close behavior
 * Inputs:      fapl_id - file access identifier
 * Outputs:
 *              degree  - possible values are:
 *              		H5F_CLOSE_DEFAULT
 *              		H5F_CLOSE_WEAK
 *              		H5F_CLOSE_SEMI
 *              		H5F_CLOSE_STRONG
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, September 26, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pget_fclose_degree_c ( hid_t_f *fapl_id , int_f *degree)
{
  int ret_value = -1;
  hid_t c_fapl_id;
  H5F_close_degree_t c_degree;

  c_fapl_id = (hid_t)*fapl_id;
  if( H5Pget_fclose_degree(c_fapl_id, &c_degree) < 0) return ret_value;

  *degree = (int_f)c_degree;
  ret_value = 0;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_fclose_degree_c
 * Purpose:     Call H5Pset_fclose_degree to set file close behavior
 * Inputs:      fapl_id - file access identifier
 *              degree  - possible values are:
 *              		H5F_CLOSE_DEFAULT
 *              		H5F_CLOSE_WEAK
 *              		H5F_CLOSE_SEMI
 *              		H5F_CLOSE_STRONG
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, September 26, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pset_fclose_degree_c ( hid_t_f *fapl_id , int_f *degree)
{
  int ret_value = -1;
  hid_t c_fapl_id;
  H5F_close_degree_t c_degree;

  c_fapl_id = (hid_t)*fapl_id;
  c_degree = (H5F_close_degree_t)*degree;
  if( H5Pset_fclose_degree(c_fapl_id, c_degree) < 0) return ret_value;

  ret_value = 0;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_buffer_c
 * Purpose:     Call H5Pset_buffer to set size of conversion buffer
 * Inputs:      prp_id - t`dataset trasfer property list identifier
 *              size   - size of the buffer
 * Outputs:     NONE
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, October 2, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pset_buffer_c ( hid_t_f *prp_id , hsize_t_f *size)
{
  int ret_value = 0;
  hid_t c_prp_id;
  size_t c_size;

  c_prp_id = (hid_t)*prp_id;
  c_size = (size_t)*size;
  if ( H5Pset_buffer(c_prp_id, c_size, NULL, NULL) < 0  ) ret_value = -1;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_buffer_c
 * Purpose:     Call H5Pget_buffer to get size of conversion buffer
 * Inputs:      prp_id - t`dataset trasfer property list identifier
 * Outputs:     size - size of conversion buffer
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, October 2, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pget_buffer_c ( hid_t_f *prp_id , hsize_t_f *size)
{
  int ret_value = -1;
  hid_t c_prp_id;
  hsize_t c_size;

  c_prp_id = (hid_t)*prp_id;
  c_size = H5Pget_buffer(c_prp_id, NULL, NULL);
  if ( c_size == 0  ) return ret_value;
  *size = (hsize_t_f)c_size;
  ret_value = 0;
  return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5pfill_value_defined_c
 * Purpose:     Call H5Pfill_value_defined to check if fill value is defined
 * Inputs:      prp_id - dataset creation property list identifier
 * Outputs:     flag - fill value status flag
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Friday, October 4, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pfill_value_defined_c ( hid_t_f *prp_id , int_f *flag)
{
  int ret_value = -1;
  hid_t c_prp_id;
  H5D_fill_value_t c_flag;

  c_prp_id = (hid_t)*prp_id;
  if ( H5Pfill_value_defined(c_prp_id, &c_flag) < 0  ) return ret_value;
  *flag = (int_f)c_flag;
  ret_value = 0;
  return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5pget_alloc_time_c
 * Purpose:     Call H5Pget_alloc_time to get space allocation
 *              time for dataset during creation
 * Inputs:      prp_id - dataset creation property list identifier
 * Outputs:     flag - allocation time flag
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Friday, October 4, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pget_alloc_time_c ( hid_t_f *prp_id , int_f *flag)
{
  int ret_value = -1;
  hid_t c_prp_id;
  H5D_alloc_time_t c_flag;

  c_prp_id = (hid_t)*prp_id;
  if ( H5Pget_alloc_time(c_prp_id, &c_flag) < 0  ) return ret_value;
  *flag = (int_f)c_flag;
  ret_value = 0;
  return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5pset_alloc_time_c
 * Purpose:     Call H5Pset_alloc_time to get space allocation
 *              time for dataset during creation
 * Inputs:      prp_id - dataset creation property list identifier
 *              flag - allocation time flag
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Friday, October 4, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pset_alloc_time_c ( hid_t_f *prp_id , int_f *flag)
{
  int ret_value = -1;
  hid_t c_prp_id;
  H5D_alloc_time_t c_flag;

  c_prp_id = (hid_t)*prp_id;
  c_flag = (H5D_alloc_time_t)*flag;
  if ( H5Pset_alloc_time(c_prp_id, c_flag) < 0  ) return ret_value;
  ret_value = 0;
  return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5pget_fill_time_c
 * Purpose:     Call H5Pget_fill_time to get fill value writing
 *              time for dataset during creation
 * Inputs:      prp_id - dataset creation property list identifier
 * Outputs:     flag - fill value writing  time flag
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Friday, October 4, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pget_fill_time_c ( hid_t_f *prp_id , int_f *flag)
{
  int ret_value = -1;
  hid_t c_prp_id;
  H5D_fill_time_t c_flag;

  c_prp_id = (hid_t)*prp_id;
  if ( H5Pget_fill_time(c_prp_id, &c_flag) < 0  ) return ret_value;
  *flag = (int_f)c_flag;
  ret_value = 0;
  return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5pset_fill_time_c
 * Purpose:     Call H5Pset_fill_time to set fill value writing
 *              time for dataset during creation
 * Inputs:      prp_id - dataset creation property list identifier
 *              flag - fill value writing  time flag
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Friday, October 4, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pset_fill_time_c ( hid_t_f *prp_id , int_f *flag)
{
  int ret_value = -1;
  hid_t c_prp_id;
  H5D_fill_time_t c_flag;

  c_prp_id = (hid_t)*prp_id;
  c_flag = (H5D_fill_time_t)*flag;
  if ( H5Pset_fill_time(c_prp_id, c_flag) < 0  ) return ret_value;
  ret_value = 0;
  return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5pset_meta_block_size_c
 * Purpose:     Call H5Pset_meta_block_size to set size of  metadata block
 * Inputs:      prp_id - file access  property list identifier
 *              size   - size of the metadata block
 * Outputs:     NONE
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Monday, October 7, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pset_meta_block_size_c ( hid_t_f *prp_id , hsize_t_f *size)
{
  int ret_value = 0;
  hid_t c_prp_id;
  hsize_t c_size;

  c_prp_id = (hid_t)*prp_id;
  c_size = (hsize_t)*size;
  if ( H5Pset_meta_block_size(c_prp_id, c_size) < 0  ) ret_value = -1;
  return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5pget_meta_block_size_c
 * Purpose:     Call H5Pget_meta_block_size to get size of  metadata block
 * Inputs:      prp_id - file access  property list identifier
 * Outputs:
 *              size   - size of the metadata block
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Monday, October 7, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pget_meta_block_size_c ( hid_t_f *prp_id , hsize_t_f *size)
{
  int ret_value = 0;
  hid_t c_prp_id;
  hsize_t c_size;

  c_prp_id = (hid_t)*prp_id;
  if ( H5Pget_meta_block_size(c_prp_id, &c_size) < 0  ) ret_value = -1;
  *size = (hsize_t_f)c_size;
  return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5pset_sieve_buf_size_c
 * Purpose:     Call H5Pset_sieve_buf_size to set size of datasieve buffer
 * Inputs:      prp_id - file access  property list identifier
 *              size   - size of the buffer
 * Outputs:     NONE
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Monday, October 7, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pset_sieve_buf_size_c ( hid_t_f *prp_id , size_t_f *size)
{
  int ret_value = 0;
  hid_t c_prp_id;
  size_t c_size;

  c_prp_id = (hid_t)*prp_id;
  c_size = (size_t)*size;
  if ( H5Pset_sieve_buf_size(c_prp_id, c_size) < 0  ) ret_value = -1;
  return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5pget_sieve_buf_size_c
 * Purpose:     Call H5Pget_sieve_buf_size to get size of datasieve buffer
 * Inputs:      prp_id - file access  property list identifier
 * Outputs:
 *              size   - size of the buffer
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Monday, October 7, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pget_sieve_buf_size_c ( hid_t_f *prp_id , size_t_f *size)
{
  int ret_value = 0;
  hid_t c_prp_id;
  size_t c_size;

  c_prp_id = (hid_t)*prp_id;
  if ( H5Pget_sieve_buf_size(c_prp_id, &c_size) < 0  ) ret_value = -1;
  *size = (size_t_f)c_size;
  return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5pset_small_data_block_size_c
 * Purpose:     Call H5Pset_small_data_block_size to set size of raw small data block
 * Inputs:      prp_id - file access  property list identifier
 *              size   - size of the block
 * Outputs:     NONE
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Monday, October 7, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pset_small_data_block_size_c ( hid_t_f *prp_id , hsize_t_f *size)
{
  int ret_value = 0;
  hid_t c_prp_id;
  hsize_t c_size;

  c_prp_id = (hid_t)*prp_id;
  c_size = (hsize_t)*size;
  if ( H5Pset_small_data_block_size(c_prp_id, c_size) < 0  ) ret_value = -1;
  return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5pget_small_data_block_size_c
 * Purpose:     Call H5Pget_small_data_block_size to get size of raw small data block
 * Inputs:      prp_id - file access  property list identifier
 * Outputs:
 *              size   - size of the block
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Monday, October 7, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pget_small_data_block_size_c ( hid_t_f *prp_id , hsize_t_f *size)
{
  int ret_value = 0;
  hid_t c_prp_id;
  hsize_t c_size;

  c_prp_id = (hid_t)*prp_id;
  if ( H5Pget_small_data_block_size(c_prp_id, &c_size) < 0  ) ret_value = -1;
  *size = (hsize_t_f)c_size;
  return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5pset_hyper_vector_size_c
 * Purpose:     Call H5Pset_hyper_vector_size to set size of the hyper vector
 * Inputs:      prp_id - dataset transfer property list identifier
 *              size   - size of the vector
 * Outputs:     NONE
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Monday, October 7, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pset_hyper_vector_size_c ( hid_t_f *prp_id , size_t_f *size)
{
  int ret_value = 0;
  hid_t c_prp_id;
  size_t c_size;

  c_prp_id = (hid_t)*prp_id;
  c_size = (size_t)*size;
  if ( H5Pset_hyper_vector_size(c_prp_id, c_size) < 0  ) ret_value = -1;
  return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5pget_hyper_vector_size_c
 * Purpose:     Call H5Pget_hyper_vector_size to get size of the hyper vector
 * Inputs:      prp_id - dataset transfer property list identifier
 * Outputs:
 *              size   - size of the vector
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Monday, October 7, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pget_hyper_vector_size_c ( hid_t_f *prp_id , size_t_f *size)
{
  int ret_value = 0;
  hid_t c_prp_id;
  size_t c_size;

  c_prp_id = (hid_t)*prp_id;
  if ( H5Pget_hyper_vector_size(c_prp_id, &c_size) < 0  ) ret_value = -1;
  *size = (size_t_f)c_size;
  return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5pcreate_class_c
 * Purpose:     Call H5Pcreate_class ito create a new property class
 * Inputs:      parent - property list class identifier
 *              name   - name of the new class
 *              name_len - lenght of the "name" buffer
 * Outputs:     cls - new class identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              October 11, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pcreate_class_c(hid_t_f *parent, _fcd name, int_f *name_len, hid_t_f *cls)
{
     int ret_value = -1;
     hid_t c_parent;
     hid_t c_class;
     char* c_name;

     c_name = (char *)HD5f2cstring(name, (size_t)*name_len);
     if (c_name == NULL) goto DONE;
     c_parent = (hid_t)*parent;

     /*
      * Call H5Pcreate_class function.
      */
     c_class = H5Pcreate_class(c_parent, c_name, NULL, NULL,NULL,NULL,NULL,NULL);
     if (c_class < 0) goto DONE;
     *cls = (hid_t_f)c_class;
     ret_value = 0;

DONE:
     if(c_name != NULL) HDfree(c_name);
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pregisterc_c
 * Purpose:     Call h5pregister_c to registers a permanent property
 * Inputs:      cls - property list class identifier
 *              name   - name of the new property
 *              name_len - length of the "name" buffer
 *              size - property size
 *              value - property value of character type
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              October 11, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pregisterc_c(hid_t_f *cls, _fcd name, int_f *name_len, size_t_f *size, _fcd value, int_f UNUSED *value_len)
{
     int ret_value = -1;

     /*
      * Call h5pregister_c function
      */
      ret_value = nh5pregister_c(cls, name, name_len, size, _fcdtocp(value));
      return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pregister_c
 * Purpose:     Call H5Pregister2 to registers a permanent property
 * Inputs:      cls - property list class identifier
 *              name   - name of the new property
 *              name_len - length of the "name" buffer
 *              size - property size
 *              value - property value
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              October 11, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pregister_c(hid_t_f *cls, _fcd name, int_f *name_len, size_t_f *size, void UNUSED *value)
{
     char* c_name = NULL;
     int_f ret_value = -1;

     if(NULL == (c_name = (char *)HD5f2cstring(name, (size_t)*name_len)))
         goto DONE;

     /*
      * Call H5Pregister2 function.
      */
     if(H5Pregister2((hid_t)*cls, c_name, (size_t)*size, value, NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
         goto DONE;
     ret_value = 0;

DONE:
     if(c_name != NULL)
         HDfree(c_name);
     return ret_value;
}

int_f
nh5pregister_integer_c(hid_t_f *cls, _fcd name, int_f *name_len, size_t_f *size, void *value)
{
     /*
      * Call h5pregister_c function
      */
     return nh5pregister_c(cls, name, name_len, size, value);
}

int_f
nh5pregister_real_c(hid_t_f *cls, _fcd name, int_f *name_len, size_t_f *size, void *value)
{
     /*
      * Call h5pregister_c function
      */
     return nh5pregister_c(cls, name, name_len, size, value);
}

int_f
nh5pregister_double_c(hid_t_f *cls, _fcd name, int_f *name_len, size_t_f *size, void *value)
{
     /*
      * Call h5pregister_c function
      */
     return nh5pregister_c(cls, name, name_len, size, value);
}

/*----------------------------------------------------------------------------
 * Name:        h5pinsertc_c
 * Purpose:     Call h5pinsert_c to register a temporary property
 * Inputs:      plist - property list identifier
 *              name   - name of the new property
 *              name_len - length of the "name" buffer
 *              size - property size
 *              value - property value of character type
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              October 11, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pinsertc_c(hid_t_f *plist, _fcd name, int_f *name_len, size_t_f *size, _fcd value, int_f UNUSED *value_len)
{
     int_f ret_value = -1;

     /*
      * Call h5pinsert_c function
      */
      ret_value = nh5pinsert_c(plist, name, name_len, size, _fcdtocp(value));
      return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pinsert_c
 * Purpose:     Call H5Pinsert2 to iinsert a temporary property
 * Inputs:      plist - property list class identifier
 *              name   - name of the new property
 *              name_len - length of the "name" buffer
 *              size - property size
 *              value - property value
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              October 11, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pinsert_c(hid_t_f *plist, _fcd name, int_f *name_len, size_t_f *size, void UNUSED *value)
{
     char* c_name = NULL;
     int_f ret_value = -1;

     if(NULL == ( c_name = (char *)HD5f2cstring(name, (size_t)*name_len)))
         goto DONE;

     /*
      * Call H5Pinsert2 function.
      */
     if(H5Pinsert2((hid_t)*plist, c_name, (size_t)*size, value, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
         goto DONE;
     ret_value = 0;

DONE:
     if(c_name)
         HDfree(c_name);
     return ret_value;
}

int_f
nh5pinsert_integer_c(hid_t_f *plist, _fcd name, int_f *name_len, size_t_f *size, void *value)
{
     /*
      * Call h5pinsert_c function
      */
     return nh5pinsert_c(plist, name, name_len, size, value);
}

int_f
nh5pinsert_real_c(hid_t_f *plist, _fcd name, int_f *name_len, size_t_f *size, void *value)
{
     /*
      * Call h5pinsert_c function
      */
     return nh5pinsert_c(plist, name, name_len, size, value);
}

int_f
nh5pinsert_double_c(hid_t_f *plist, _fcd name, int_f *name_len, size_t_f *size, void *value)
{
     /*
      * Call h5pinsert_c function
      */
     return nh5pinsert_c(plist, name, name_len, size, value);
}

/*----------------------------------------------------------------------------
 * Name:        h5pexist_c
 * Purpose:     Call H5Pexist to querie whether a property name exists
 *              in a property list or class
 * Inputs:      plist - property list or property class identifier
 *              name   - name of the new property
 *              name_len - length of the "name" buffer
 * Returns:     nonnegative on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              October 11, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pexist_c(hid_t_f *cls, _fcd name, int_f *name_len)
{
     int_f ret_value = -1;
     hid_t c_class;
     char* c_name;
     htri_t status;

     c_name = (char *)HD5f2cstring(name, (size_t)*name_len);
     if (c_name == NULL) goto DONE;

     c_class = (hid_t)*cls;
     /*
      * Call H5Pexist function.
      */
     status = H5Pexist(c_class, c_name);
     ret_value = status;

DONE:
     if(c_name != NULL) HDfree(c_name);
     return  ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5pisa_class_c
 * Purpose:     Call H5Pisa_class to querie whether a property is a
 *              member of a class
 * Inputs:      plist - property list identifier
 *              cls - property class identifier
 * Returns:     nonnegative on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              October 11, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pisa_class_c(hid_t_f *plist, hid_t_f *cls)
{
     int_f ret_value = -1;
     hid_t c_class;
     hid_t c_plist;
     htri_t status;

     c_class = (hid_t)*cls;
     c_plist = (hid_t)*plist;

     /*
      * Call H5Pisa_class function.
      */
     status = H5Pisa_class(c_plist, c_class);
     ret_value = status;
     return  ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5pget_size_c
 * Purpose:     Call H5Pget_size to querie the size of the property
 * Inputs:      plist - property list to query
 *              name   - name of the property
 *              name_len - length of the "name" buffer
 * Outputs:     size - size of the property in bytes
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              October 11, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_size_c(hid_t_f *plist, _fcd name, int_f *name_len, size_t_f *size)
{
     int_f ret_value = -1;
     hid_t c_plist;
     char* c_name;
     size_t c_size;

     c_name = (char *)HD5f2cstring(name, (size_t)*name_len);
     if (c_name == NULL) goto DONE;

     c_plist = (hid_t)*plist;
     /*
      * Call H5Pget_size function.
      */
     if( H5Pget_size(c_plist, c_name, &c_size) < 0) goto DONE;
     *size = (size_t_f)c_size;
      ret_value = 0;

DONE:
     if(c_name != NULL) HDfree(c_name);
     return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5pget_nprops_c
 * Purpose:     Call H5Pget_nporps to get number of the properties in the list
 * Inputs:      plist - property list to query
 * Outputs:     nprops - number of properties in the list
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              October 11, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_nprops_c(hid_t_f *plist, size_t_f *nprops)
{
     int_f ret_value = -1;
     hid_t c_plist;
     size_t c_nprops;

     c_plist = (hid_t)*plist;

     /*
      * Call H5Pget_nprops function.
      */
     if( H5Pget_nprops(c_plist, &c_nprops) < 0) return ret_value;

     *nprops = (size_t_f)c_nprops;
     ret_value = 0;
     return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5pget_class_parent_c
 * Purpose:     Call H5Pget_class_parent to get the parent class of
 *              a genereic property class
 * Inputs:      prp_id - property list to query
 * Outputs:     parent_id - parent classs identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              October 11, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_class_parent_c(hid_t_f *prp_id, hid_t_f *parent_id)
{
     int_f ret_value = -1;
     hid_t c_prp_id;
     hid_t c_parent_id;

     c_prp_id = (hid_t)*prp_id;

     /*
      * Call H5Pget_class_parent function.
      */
     c_parent_id = H5Pget_class_parent(c_prp_id);
     if( c_parent_id < 0) return ret_value;

     *parent_id =(hid_t_f)c_parent_id;
     ret_value = 0;
     return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5pcopy_prop_c
 * Purpose:     Call H5Pcopy_prop to copy a property from one list or
 *              class to another
 * Inputs:      dst_id - identifier of destination property list
 *              src_id - identifier of source property list
 *              name   - name of the property
 *              name_len - length of the "name" buffer
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              October 11, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pcopy_prop_c(hid_t_f *dst_id, hid_t_f *src_id, _fcd name, int_f *name_len)
{
     int_f ret_value = -1;
     hid_t c_dst_id, c_src_id;
     char* c_name;

     c_name = (char *)HD5f2cstring(name, (size_t)*name_len);
     if (c_name == NULL) goto DONE;

     c_dst_id = (hid_t)*dst_id;
     c_src_id = (hid_t)*src_id;
     /*
      * Call H5Pcopy_prop function.
      */
     if( H5Pcopy_prop(c_dst_id, c_src_id, c_name) < 0) goto DONE;
     ret_value = 0;

DONE:
     if(c_name != NULL) HDfree(c_name);
     return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5premove_c
 * Purpose:     Call H5Premove to remove a property from a list
 * Inputs:      plid - identifier of property list
 *              name   - name of the property to remove
 *              name_len - length of the "name" buffer
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              October 11, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5premove_c(hid_t_f *plid, _fcd name, int_f *name_len)
{
     int_f ret_value = -1;
     hid_t c_plid;
     char* c_name;

     c_name = (char *)HD5f2cstring(name, (size_t)*name_len);
     if (c_name == NULL) goto DONE;

     c_plid = (hid_t)*plid;
     /*
      * Call H5Premove function.
      */
     if( H5Premove(c_plid, c_name) < 0) goto DONE;
     ret_value = 0;

DONE:
     if(c_name != NULL) HDfree(c_name);
     return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5punregister_c
 * Purpose:     Call H5Punregister to remove a property from a property class
 * Inputs:      cls - identifier of property class
 *              name   - name of the property to unregister
 *              name_len - length of the "name" buffer
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              October 11, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5punregister_c(hid_t_f *cls, _fcd name, int_f *name_len)
{
     int_f ret_value = -1;
     hid_t c_class;
     char* c_name;

     c_name = (char *)HD5f2cstring(name, (size_t)*name_len);
     if (c_name == NULL) goto DONE;

     c_class = (hid_t)*cls;
     /*
      * Call H5Punregister function.
      */
     if( H5Punregister(c_class, c_name) < 0) goto DONE;
     ret_value = 0;

DONE:
     if(c_name != NULL) HDfree(c_name);
     return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5pclose_class_c
 * Purpose:     Call H5Pclose_class to close property class
 * Inputs:      cls - identifier of property class to close
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              October 11, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pclose_class_c(hid_t_f *cls)
{
     int_f ret_value = -1;
     hid_t c_class;

     c_class = (hid_t)*cls;
     /*
      * Call H5Pclose_class function.
      */
     if( H5Pclose_class(c_class) < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5pget_class_name_c
 * Purpose:     Call H5Pget_class_name to get property class name
 * Inputs:      cls - identifier of property class
 *              name   - ibuffer to retrieve name in
 *              name_len - length of the "name" buffer
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              October 11, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_class_name_c(hid_t_f *cls, _fcd name, int_f *name_len)
{
     int_f ret_value = -1;
     char *c_name = NULL;          /* Buffer to hold C string */
     size_t c_size;

     c_size = (size_t)*name_len + 1;

     /*
      * Allocate buffer to hold name
      */
     if(NULL == (c_name = (char *)HDmalloc(c_size)))
       goto DONE;

     /*
      * Call H5Pget_class_name function.
      */
     c_name = H5Pget_class_name((hid_t)*cls);
     if(c_name == NULL) goto DONE;

     HD5packFstring(c_name, _fcdtocp(name), (size_t)*name_len);
     ret_value = (int_f)HDstrlen(c_name);

DONE:
     HDfree(c_name);
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_c
 * Purpose:     Call h5setc_c to set property with the character string value
 * Inputs:      plist - property list identifier
 *              name   - name of property
 *              name_len - length of the "name" buffer
 *              value - property value of character type
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              October 11, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5psetc_c(hid_t_f *plist, _fcd name, int_f *name_len, _fcd value, int_f UNUSED *value_len)
{
     int_f ret_value = -1;

     /*
      * Call h5pset_c function
      */
      ret_value = nh5pset_c(plist, name, name_len, _fcdtocp(value));
      return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_c
 * Purpose:     Call H5Pset to set property value
 * Inputs:      plist - property list class identifier
 *              name   - name of the new property
 *              name_len - length of the "name" buffer
 *              value - property value
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              October 11, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_c(hid_t_f *plist, _fcd name, int_f *name_len, void UNUSED *value)
{
     int_f ret_value = -1;
     hid_t c_plist;
     char* c_name;

     c_name = (char *)HD5f2cstring(name, (size_t)*name_len);
     if (c_name == NULL) goto DONE;
     c_plist = (hid_t)*plist;

     /*
      * Call H5Pset function.
      */
     if( H5Pset(c_plist, c_name, value) <0) goto DONE;
     ret_value = 0;

DONE:
     if(c_name != NULL) HDfree(c_name);
     return ret_value;
}

int_f
nh5pset_integer_c(hid_t_f *plist, _fcd name, int_f *name_len, void *value)
{
     /*
      * Call h5pset_c function
      */
     return nh5pset_c(plist, name, name_len, value);
}

int_f
nh5pset_real_c(hid_t_f *plist, _fcd name, int_f *name_len, void *value)
{
     /*
      * Call h5pset_c function
      */
     return nh5pset_c(plist, name, name_len, value);
}

int_f
nh5pset_double_c(hid_t_f *plist, _fcd name, int_f *name_len, void *value)
{
     /*
      * Call h5pset_c function
      */
     return nh5pset_c(plist, name, name_len, value);
}
/*----------------------------------------------------------------------------
 * Name:        h5pgetc_c
 * Purpose:     Call h5set_c to set property with the character string value
 * Inputs:      plist - property list identifier
 *              name   - name of property
 *              name_len - length of the "name" buffer
 * Output:      value - property value of character type
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              October 11, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pgetc_c(hid_t_f *plist, _fcd name, int_f *name_len, _fcd value, int_f UNUSED *value_len)
{
     int_f ret_value = -1;

     /*
      * Call h5pget_c function
      */
      ret_value = nh5pget_c(plist, name, name_len, _fcdtocp(value));
      return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_c
 * Purpose:     Call H5Pget to set property value
 * Inputs:      plist - property list class identifier
 *              name   - name of the new property
 *              name_len - length of the "name" buffer
 * Output:      value - property value
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              October 11, 2002
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_c(hid_t_f *plist, _fcd name, int_f *name_len, void UNUSED *value)
{
     int_f ret_value = -1;
     hid_t c_plist;
     char* c_name;

     c_name = (char *)HD5f2cstring(name, (size_t)*name_len);
     if (c_name == NULL) goto DONE;
     c_plist = (hid_t)*plist;

     /*
      * Call H5Pset function.
      */
     if( H5Pget(c_plist, c_name, value) <0) goto DONE;
     ret_value = 0;

DONE:
     if(c_name != NULL) HDfree(c_name);
     return ret_value;
}

int_f
nh5pget_integer_c(hid_t_f *plist, _fcd name, int_f *name_len, void *value)
{
     /*
      * Call h5pget_c function
      */
     return nh5pget_c(plist, name, name_len, value);
}

int_f
nh5pget_real_c(hid_t_f *plist, _fcd name, int_f *name_len, void *value)
{
     /*
      * Call h5pget_c function
      */
     return nh5pget_c(plist, name, name_len, value);
}

int_f
nh5pget_double_c(hid_t_f *plist, _fcd name, int_f *name_len, void *value)
{
     /*
      * Call h5pget_c function
      */
     return nh5pget_c(plist, name, name_len, value);
}


/*----------------------------------------------------------------------------
 * Name:        h5pset_shuffle_c
 * Purpose:     Call H5Pset_shuffle
 * Inputs:      prp_id - property list identifier
 *              type_size - size of the datatype in bytes
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, March 12, 2003
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pset_shuffle_c ( hid_t_f *prp_id )
{
  int_f ret_value = 0;
  hid_t c_prp_id;
  herr_t status;

  c_prp_id = (hid_t)*prp_id;
  status = H5Pset_shuffle(c_prp_id);
  if ( status < 0  ) ret_value = -1;
  return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5pset_fletcher32_c
 * Purpose:     Call H5Pset_fletcher32 to enable EDC
 * Inputs:      prp_id - dataset creation property list identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, March 13, 2003
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pset_fletcher32_c ( hid_t_f *prp_id )
{
  int_f ret_value = 0;
  hid_t c_prp_id;
  herr_t status;

  c_prp_id = (hid_t)*prp_id;
  status = H5Pset_fletcher32(c_prp_id);
  if ( status < 0  ) ret_value = -1;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_edc_check_c
 * Purpose:     Call H5Pset_edc_check to enable EDC
 * Inputs:      prp_id - dataset transfer property list identifier
 *              flag   - EDC flag
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, March 13, 2003
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pset_edc_check_c ( hid_t_f *prp_id, int_f *flag )
{
  int_f ret_value = 0;
  hid_t c_prp_id;
  H5Z_EDC_t c_flag;
  herr_t status;

  c_prp_id = (hid_t)*prp_id;
  c_flag = (H5Z_EDC_t)*flag;
  status = H5Pset_edc_check(c_prp_id, c_flag);
  if ( status < 0  ) ret_value = -1;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_edc_check_c
 * Purpose:     Call H5Pget_edc_check to query EDC
 * Inputs:      prp_id - dataset transfer property list identifier
 * Outouts:     flag   - EDC flag
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, March 13, 2003
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pget_edc_check_c ( hid_t_f *prp_id, int_f *flag )
{
  int_f ret_value = 0;
  hid_t c_prp_id;
  H5Z_EDC_t c_flag;

  c_prp_id = (hid_t)*prp_id;
  c_flag = H5Pget_edc_check(c_prp_id);
  if ( c_flag  < 0  ) ret_value = -1;
  *flag = (int_f)c_flag;
  return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5pset_family_offset_c
 * Purpose:     Call H5Pset_family_offset to set and offset for family driver
 * Inputs:      prp_id - property list identifier
 *              offset - offset in bytes
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, 19 March 2003
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pset_family_offset_c ( hid_t_f *prp_id , hsize_t_f *offset)
{
  int_f ret_value = 0;
  hid_t c_prp_id;
  hsize_t c_offset;
  herr_t status;

  c_prp_id = (hid_t)*prp_id;
  c_offset = (hsize_t)*offset;
  status = H5Pset_family_offset(c_prp_id, c_offset);
  if ( status < 0  ) ret_value = -1;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_fapl_multi_c
 * Purpose:     Call H5Pset_fapl_multi to set multi file dirver
 * Inputs:      prp_id - file_creation property list identifier
 *              mem_map - memory mapping array
 *              memb_fapl - property list for each memory usage type
 *              memb_name - array with members names
 *              len - array with the lenght of each name
 *              lenmax - lenght of the name a sdeclared in Fortran
 *              flag - flag allowing partila access when one of the files is missing
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Monday 24, March 2003
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
/*nh5pset_fapl_multi_c ( hid_t_f *prp_id , int_f *memb_map, hid_t_f *memb_fapl, _fcd memb_name, int_f *len, int_f *lenmax, haddr_t_f *memb_addr, int_f *flag) */
nh5pset_fapl_multi_c ( hid_t_f *prp_id , int_f *memb_map, hid_t_f *memb_fapl, _fcd memb_name, int_f *len, int_f *lenmax, real_f *memb_addr, int_f *flag)
{
  int_f ret_value = -1;
  hid_t c_prp_id;
  H5FD_mem_t c_memb_map[H5FD_MEM_NTYPES];
  hid_t c_memb_fapl[H5FD_MEM_NTYPES];
  char *c_memb_name[H5FD_MEM_NTYPES];
  haddr_t c_memb_addr[H5FD_MEM_NTYPES];
  hbool_t relax;
  herr_t status;
  char *tmp, *tmp_p, *tmp_pp;
  int i;
  int c_lenmax;
  long double  tmp_max_addr;
  c_lenmax = (int)*lenmax;
  relax = (hbool_t)*flag;
/*
 * Check that we got correct values from Fortran for memb_addr array
 */
  for (i=0; i < H5FD_MEM_NTYPES; i++) {
       if(memb_addr[i] >= 1.) return ret_value;
  }
/*
 * Take care of names array
 */

  tmp = (char *)HD5f2cstring(memb_name, (size_t)c_lenmax*(H5FD_MEM_NTYPES));
  if (tmp ==NULL) return ret_value;
  tmp_p = tmp;
  for (i=0; i < H5FD_MEM_NTYPES; i++) {
       c_memb_name[i] = (char *)HDmalloc((size_t)len[i] + 1);
       HDmemcpy(c_memb_name[i], tmp_p, (size_t)len[i]);
       tmp_pp = c_memb_name[i];
       tmp_pp[len[i]] = '\0';
       tmp_p = tmp_p + c_lenmax;
 }
/*
 * Take care of othe arguments
 */
  tmp_max_addr =  (long double)(HADDR_MAX);
  c_prp_id = (hid_t)*prp_id;
  for (i=0; i < H5FD_MEM_NTYPES; i++) {
       c_memb_map[i] = (H5FD_mem_t)memb_map[i];
       c_memb_fapl[i] = (hid_t)memb_fapl[i];
       if(memb_addr[i] < 0) c_memb_addr[i] = HADDR_UNDEF;
       else c_memb_addr[i] = (haddr_t)(((float)memb_addr[i])*(tmp_max_addr));
  }
/*
 * Call  H5Pset_fapl_multi function
 */

  status = H5Pset_fapl_multi(c_prp_id, c_memb_map, c_memb_fapl, (const char * const *)c_memb_name, c_memb_addr, relax);
  if ( status < 0  ) goto DONE;
  ret_value = 0;

DONE:
  HDfree(tmp);
  for (i=0; i < H5FD_MEM_NTYPES; i++)
      HDfree(c_memb_name[i]);
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_fapl_multi_sc
 * Purpose:     Call H5Pset_fapl_multi to set multi file dirver
 * Inputs:      prp_id - file_creation property list identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              March 31 2003
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pset_fapl_multi_sc ( hid_t_f *prp_id , int_f *flag)
{
  int_f ret_value = -1;
  hid_t c_prp_id;
  hbool_t relax;
  herr_t status;

  relax = (hbool_t)*flag;
  c_prp_id = (hid_t)*prp_id;
/*
 * Call  H5Pset_fapl_multi function
 */

  status = H5Pset_fapl_multi(c_prp_id, NULL, NULL, NULL, NULL, relax);
  if ( status < 0  ) return ret_value; /* error occurred */
  ret_value = 0;
  return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5pget_fapl_multi_c
 * Purpose:     Call H5Pget_fapl_multi to set multi file dirver
 * Inputs:      prp_id - file_creation property list identifier
 *              lenmax - lenght of the name a sdeclared in Fortran
 * Outputs:     memb_map - memory mapping array
 *              memb_fapl - property list for each memory usage type
 *              memb_name - array with members names
 *              len - array with the lenght of each name
 *              flag - flag allowing partila access when one of the files is missing
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Monday 24, March 2003
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pget_fapl_multi_c ( hid_t_f *prp_id , int_f *memb_map, hid_t_f *memb_fapl, _fcd memb_name, int_f *len, int_f *lenmax, real_f *memb_addr, int_f *flag, int_f *maxlen_out)
{
  int_f ret_value = -1;
  hid_t c_prp_id;
  H5FD_mem_t c_memb_map[H5FD_MEM_NTYPES];
  hid_t c_memb_fapl[H5FD_MEM_NTYPES];
  char *c_memb_name[H5FD_MEM_NTYPES];
  haddr_t c_memb_addr[H5FD_MEM_NTYPES];
  hbool_t relax;
  herr_t status;
  char *tmp, *tmp_p;
  int i;
  size_t c_lenmax;
  size_t length = 0;
  c_lenmax = (size_t)*lenmax;

  c_prp_id = (hid_t)*prp_id;
/*
 * Call  H5Pget_fapl_multi function
 */

  status = H5Pget_fapl_multi(c_prp_id, c_memb_map, c_memb_fapl, c_memb_name, c_memb_addr, &relax);
  if ( status < 0  ) return ret_value;

/*
 * Take care of names array
 */
  tmp = (char *)HDmalloc(c_lenmax*H5FD_MEM_NTYPES + 1);
  tmp_p = tmp;
  HDmemset(tmp,' ', c_lenmax*H5FD_MEM_NTYPES);
  tmp[c_lenmax*H5FD_MEM_NTYPES] = '\0';
  for (i=0; i < H5FD_MEM_NTYPES; i++) {
       memcpy(tmp_p, c_memb_name[i], strlen(c_memb_name[i]));
       len[i] = (int_f)strlen(c_memb_name[i]);
       length = H5_MAX(length, strlen(c_memb_name[i]));
       tmp_p = tmp_p + c_lenmax;
 }
HD5packFstring(tmp, _fcdtocp(memb_name), (size_t)(c_lenmax*H5FD_MEM_NTYPES));

/*
 * Take care of other arguments
 */

  for (i=0; i < H5FD_MEM_NTYPES; i++) {
       memb_map[i] = (int_f)c_memb_map[i];
       memb_fapl[i] = (hid_t_f)c_memb_fapl[i];
       if(c_memb_addr[i] == HADDR_UNDEF) memb_addr[i] = -1;
       else memb_addr[i] = (real_f) ((long)c_memb_addr[i]/HADDR_MAX);
  }
  *flag = (int_f)relax;
  *maxlen_out = (int_f)length;
  ret_value = 0;
  HDfree(tmp);
  for (i=0; i < H5FD_MEM_NTYPES; i++)
      HDfree(c_memb_name[i]);
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_szip_c
 * Purpose:     Call H5Pset_szip to set szip compression
 * Inputs:      prp_id - dataset creation property list identifier
 *              options_mask
 *              pixels_per_block -szip compression parameters
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              April 8 2003
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pset_szip_c ( hid_t_f *prp_id , int_f *options_mask, int_f *pixels_per_block)
{
  int_f ret_value = -1;
  hid_t c_prp_id;
  unsigned   c_options_mask;
  unsigned  c_pixels_per_block;
  herr_t status;

  c_prp_id = (hid_t)*prp_id;
  c_options_mask = (unsigned)*options_mask;
  c_pixels_per_block = (unsigned)*pixels_per_block;
/*
 * Call  H5Pset_szip function
 */

  status = H5Pset_szip(c_prp_id, c_options_mask, c_pixels_per_block);
  if ( status < 0  ) return ret_value;
  ret_value = 0;
  return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5pall_filters_avail_c
 * Purpose:     Call H5Pall_filters_avail
 * Inputs:      prp_id - dataset creation property list identifier
 * Outputs:     status - logical flag
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              April 10 2003
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pall_filters_avail_c ( hid_t_f *prp_id , int_f *status)
{
  int_f ret_value = -1;
  hid_t c_prp_id;
  htri_t c_status;


  c_prp_id = (hid_t)*prp_id;
/*
 * Call  H5Pall_filters_avail function
 */

  c_status = H5Pall_filters_avail(c_prp_id);
  if ( c_status < 0  ) return ret_value;
  *status = 0;
  if (c_status == 1) *status = 1;
  ret_value = 0;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_filter_by_id_c
 * Purpose:     Call H5Pget_filter_by_id2 to get information about a filter
 *              in a pipeline
 * Inputs:      prp_id - property list identifier
 *              filter_id - filter id
 *              namelen - Anticipated number of characters in name.
 *Outputs:      flags - Bit vector specifying certain general
 *                      properties of the filter.
 *              cd_nelmts - Number of elements in cd_value
 *              cd_values - Auxiliary data for the filter.
 *              name - Name of the filter
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena POurmal
 *              April 10, 2003
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_filter_by_id_c(hid_t_f *prp_id, int_f* filter_id, int_f* flags, size_t_f* cd_nelmts, int_f* cd_values, size_t_f *namelen, _fcd name)
{
     unsigned int c_flags;
     size_t c_cd_nelmts = (size_t)*cd_nelmts;
     size_t c_cd_nelmts_in = (size_t)*cd_nelmts;
     unsigned int *c_cd_values = NULL;
     char *c_name = NULL;
     unsigned i;
     int_f ret_value = -1;

     if(NULL == (c_name = (char *)HDmalloc((size_t)*namelen + 1)))
         goto DONE;

     if(NULL == (c_cd_values = (unsigned int *)HDmalloc(sizeof(unsigned int) * ((int)c_cd_nelmts_in))))
         goto DONE;

     /*
      * Call H5Pget_filter_by_id2 function.
      */
     if(H5Pget_filter_by_id2((hid_t)*prp_id, (H5Z_filter_t)*filter_id, &c_flags, &c_cd_nelmts, c_cd_values, (size_t)*namelen, c_name, NULL) < 0)
         goto DONE;

     *cd_nelmts = (size_t_f)c_cd_nelmts;
     *flags = (int_f)c_flags;
     HD5packFstring(c_name, _fcdtocp(name), HDstrlen(c_name));

     for(i = 0; i < c_cd_nelmts_in; i++)
          cd_values[i] = (int_f)c_cd_values[i];

     ret_value = 0;

DONE:
    if(c_name)
        HDfree(c_name);
    if(c_cd_values)
        HDfree(c_cd_values);

    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pmodify_filter_c
 * Purpose:     Call H5Pmodify_filter to modify a filter
 * Inputs:      prp_id - property list identifier
 *              filter - Filter to be modified
 *              flags - Bit vector specifying certain general
 *                      properties of the filter.
 *              cd_nelmts - Number of elements in cd_values.
 *              cd_values - Auxiliary data for the filter.
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              April 10 2003
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pmodify_filter_c (hid_t_f *prp_id, int_f* filter, int_f* flags, size_t_f* cd_nelmts, int_f* cd_values )
{
     int_f ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     size_t c_cd_nelmts;
     unsigned int c_flags;
     H5Z_filter_t c_filter;
     unsigned int * c_cd_values;
     unsigned i;

     c_filter = (H5Z_filter_t)*filter;
     c_flags = (unsigned)*flags;
     c_cd_nelmts = (size_t)*cd_nelmts;
     c_cd_values = (unsigned int *)HDmalloc(sizeof(unsigned int) * ((int)c_cd_nelmts));
     if (!c_cd_values) return ret_value;
     for (i = 0; i < c_cd_nelmts; i++)
          c_cd_values[i] = (unsigned int)cd_values[i];

     /*
      * Call H5Pmodify_filter function.
      */
     c_prp_id = (hid_t)*prp_id;
     ret = H5Pmodify_filter(c_prp_id, c_filter, c_flags, c_cd_nelmts,c_cd_values );

     if (ret < 0) goto DONE;
     ret_value = 0;

DONE:
     HDfree(c_cd_values);
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5premove_filter_c
 * Purpose:     Call H5Premove_filter to delete one or more filters
 * Inputs:      prp_id - property list identifier
 *              filter - Filter to be deleted
 * Returns:     0 on success, -1 on failure
 * Programmer:  Quincey Koziol
 *              January 27 2004
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5premove_filter_c (hid_t_f *prp_id, int_f* filter)
{
     int_f ret_value = -1;
     hid_t c_prp_id;
     H5Z_filter_t c_filter;

     c_filter = (H5Z_filter_t)*filter;
     c_prp_id = (hid_t)*prp_id;

     /*
      * Call H5Premove_filter function.
      */
     if(H5Premove_filter(c_prp_id, c_filter) < 0) goto DONE;
     ret_value = 0;

DONE:
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_attr_phase_change_c
 * Purpose:     Calls H5Pget_attr_phase_change
 *
 * Inputs:      ocpl_id		- Object (dataset or group) creation property list identifier
 * Outputs      max_compact     - Maximum number of attributes to be stored in compact storage
 *              min_dense       - Minimum number of attributes to be stored in dense storage
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              January, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_attr_phase_change_c(hid_t_f *ocpl_id, int_f *max_compact, int_f *min_dense )
{
  int ret_value = -1;
  hid_t c_ocpl_id;
  unsigned c_max_compact;
  unsigned c_min_dense;
  herr_t ret;
  /*
   * Call H5Pget_attr_phase_change function.
   */
  c_ocpl_id = (hid_t)*ocpl_id;
  ret = H5Pget_attr_phase_change(c_ocpl_id, &c_max_compact,&c_min_dense);
  if (ret < 0) return ret_value;

  *max_compact = (int_f)c_max_compact;
  *min_dense = (int_f)c_min_dense;
  ret_value = 0;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_attr_creation_order_c
 * Purpose:     Calls H5Ppset_attr_creation_order
 *
 * Inputs:      ocpl_id		- Object (dataset or group) creation property list identifier
 * Outputs      crt_order_flags - Flags specifying whether to track and index attribute creation order
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              January, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_attr_creation_order_c(hid_t_f *ocpl_id, int_f *crt_order_flags )
{
  int ret_value = -1;
  unsigned c_crt_order_flags;
  herr_t ret;
  /*
   * Call h5pset_attr_creation_order function.
   */
  c_crt_order_flags = (unsigned)*crt_order_flags;
  ret = H5Pset_attr_creation_order((hid_t)*ocpl_id, c_crt_order_flags);
  if (ret < 0) return ret_value;

  *crt_order_flags = (int_f)c_crt_order_flags;
  ret_value = 0;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_shared_mesg_nindexes_c
 * Purpose:     Calls h5pset_shared_mesg_nindexes
 *
 * Inputs:
 *       plist_id - file creation property list
 *       nindexes - Number of shared object header message indexes
 *                   available in files created WITH this property list
 *
 * Outputs:
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              January, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_shared_mesg_nindexes_c(hid_t_f *plist_id, int_f *nindexes )
{
  int ret_value = -1;
  hid_t c_plist_id;
  unsigned c_nindexes;
  herr_t ret;
  /*
   * Call h5pset_shared_mesg_nindexes function.
   */
  c_plist_id = (hid_t)*plist_id;
  c_nindexes = (unsigned)*nindexes;
  ret = H5Pset_shared_mesg_nindexes(c_plist_id, c_nindexes );
  if (ret < 0) return ret_value;

  ret_value = 0;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_shared_mesg_index_c
 * Purpose:     Calls H5Pset_shared_mesg_index
 *
 * Inputs:
 *            fcpl_id - File creation property list identifier.
 *          index_num - Index being configured.
 *    mesg_type_flags - Types of messages that should be stored in this index.
 *      min_mesg_size - Minimum message size.
 *
 * Outputs:
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              January, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_shared_mesg_index_c(hid_t_f *fcpl_id, int_f *index_num, int_f *mesg_type_flags, int_f *min_mesg_size)
{
  int ret_value = -1;
  herr_t ret;
  /*
   * Call h5pset_shared_mesg_index function.
   */
  ret = H5Pset_shared_mesg_index((hid_t)*fcpl_id,(unsigned)*index_num, (unsigned)*mesg_type_flags, (unsigned)*min_mesg_size);
  if (ret < 0) return ret_value;

  ret_value = 0;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_attr_creation_order_c
 * Purpose:     Calls H5Pget_attr_creation_order
 *
 * Inputs:
 *           ocpl_id - Object (group or dataset) creation property list identifier
 * Outputs:
 *           crt_order_flags - Flags specifying whether to track and index attribute creation order
 *
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              February, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_attr_creation_order_c(hid_t_f *ocpl_id, int_f *crt_order_flags)
{
  int ret_value = -1;
  herr_t ret;

  unsigned c_crt_order_flags;
  /*
   * Call h5pget_attr_creation_order function.
   */

  ret = H5Pget_attr_creation_order((hid_t)*ocpl_id, &c_crt_order_flags);
  if (ret < 0) return ret_value;

  *crt_order_flags = (int_f)c_crt_order_flags;

  ret_value = 0;
  return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5pset_libver_bounds_c
 * Purpose:     Calls H5Pset_libver_bounds
 *
 * Inputs:
 *             fapl_id - File access property list identifier
 *                 low - The earliest version of the library that will be used for writing objects.
 *                high - The latest version of the library that will be used for writing objects.
 * Outputs:
 *
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              February 18, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_libver_bounds_c(hid_t_f *fapl_id, int_f *low, int_f *high )
{
  int ret_value = -1;
  herr_t ret;

  /*
   * Call H5Pset_libver_bounds function.
   */
  ret = H5Pset_libver_bounds( (hid_t)*fapl_id, (H5F_libver_t)*low, (H5F_libver_t)*high );
  if (ret < 0) return ret_value;

  ret_value = 0;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_link_creation_order_c
 * Purpose:     Calls H5Pset_link_creation_order
 *
 * Inputs:      gcpl_id		- Group creation property list identifier
 *              crt_order_flags - Creation order flag(s)
 * Outputs:
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              February 18, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_link_creation_order_c(hid_t_f *gcpl_id, int_f *crt_order_flags )
{
  int ret_value = -1;
  herr_t ret;
  /*
   * Call H5Pset_link_creation_order function.
   */
  ret = H5Pset_link_creation_order((hid_t)*gcpl_id, (unsigned)*crt_order_flags);
  if (ret < 0) return ret_value;

  ret_value = 0;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_link_phase_change_c
 * Purpose:     Calls H5Pget_link_phase_change
 *
 * Inputs:      gcpl_id  	- Group creation property list identifier
 * Outputs      max_compact     - Maximum number of attributes to be stored in compact storage
 *              min_dense       - Minimum number of attributes to be stored in dense storage
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              February 20, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_link_phase_change_c(hid_t_f *gcpl_id, int_f *max_compact, int_f *min_dense )
{
  int ret_value = -1;
  unsigned c_max_compact;
  unsigned c_min_dense;
  herr_t ret;

  /*
   * Call H5Pget_link_phase_change function.
   */
  ret = H5Pget_link_phase_change((hid_t)*gcpl_id, &c_max_compact,&c_min_dense);
  if (ret < 0) return ret_value;

  *max_compact = (int_f)c_max_compact;
  *min_dense = (int_f)c_min_dense;
  ret_value = 0;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_obj_track_times_c
 * Purpose:     Call H5Pget_obj_track_times
 *
 * Inputs:      plist_id - property list id
 * Outputs:
 *              flag     - TRUE/FALSE flag
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              February 22, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pget_obj_track_times_c(hid_t_f *plist_id, int_f *flag)
{
  int ret_value = -1;
  hbool_t c_track_times=0;
  herr_t ret;

  /*
   * Call H5Pget_obj_track_times function.
   */
  ret = H5Pget_obj_track_times((hid_t)*plist_id, &c_track_times);

  if (ret < 0) return ret_value; /* error occurred */

  *flag = 0;
  if(c_track_times > 0) *flag = 1;
  ret_value = 0;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_obj_track_times_c
 * Purpose:     Call H5Pset_obj_track_times
 *
 * Inputs:      plist_id - property list id
 *              flag     - TRUE/FALSE flag
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              February 22, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pset_obj_track_times_c(hid_t_f *plist_id, int_f *flag)
{
  int ret_value = -1;
  hbool_t c_track_times;
  herr_t ret;


  c_track_times = (hbool_t)*flag;

  /*
   * Call H5Pset_obj_track_times function.
   */
  ret = H5Pset_obj_track_times((hid_t)*plist_id, c_track_times);

  if (ret < 0) return ret_value; /* error occurred */
  ret_value = 0;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_create_inter_group_c
 * Purpose:     Calls H5Pset_create_intermediate_group
 *
 * Inputs:
 *		lcpl_id - Link creation property list identifier
 *   crt_intermed_group - crt_intermed_group specifying whether
 *                        to create intermediate groups upon the
 *                        creation of an object
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              February 22, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pset_create_inter_group_c(hid_t_f *lcpl_id, int_f *crt_intermed_group)
{
  int ret_value = -1;
  herr_t ret;

  /*
   * Call H5Pset_create_intermediate_group function.
   */
  ret = H5Pset_create_intermediate_group((hid_t)*lcpl_id, (unsigned)*crt_intermed_group);

  if (ret < 0) return ret_value; /* error occurred */
  ret_value = 0;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_link_creation_order_c
 * Purpose:     Calls H5Pget_link_creation_order
 *
 * Inputs:
 *           gcpl_id - Group creation property list identifier
 * Outputs:
 *           crt_order_flags - Creation order flag(s)
 *
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              March 3, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_link_creation_order_c(hid_t_f *gcpl_id, int_f *crt_order_flags)
{
  int ret_value = -1;
  herr_t ret;

  unsigned c_crt_order_flags;
  /*
   * Call h5pget_link_creation_order function.
   */

  ret = H5Pget_link_creation_order((hid_t)*gcpl_id, &c_crt_order_flags);
  if (ret < 0) return ret_value;

  *crt_order_flags = (int_f)c_crt_order_flags;

  ret_value = 0;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:     h5pset_char_encoding_c
 * Purpose:  Calls H5Pset_char_encoding
 *
 * Inputs:
 *           plist_id - Property list identifier
 *           encoding - String encoding character set:
 *     	                     H5T_CSET_ASCII_F -> US ASCII
 *     	                     H5T_CSET_UTF8_F -> UTF-8 Unicode encoding
 * Outputs:  NONE
 *
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              March 3, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_char_encoding_c(hid_t_f *plist_id, int_f *encoding)
{
  int ret_value = -1;
  herr_t ret;

  /*
   * Call H5Pset_char_encoding function.
   */
  ret = H5Pset_char_encoding((hid_t)*plist_id, (H5T_cset_t)*encoding);
  if (ret < 0) return ret_value;

  ret_value = 0;
  return ret_value;
}


/*----------------------------------------------------------------------------
 * Name:     h5pget_char_encoding_c
 * Purpose:  Calls H5Pget_char_encoding
 *
 * Inputs:
 *           plist_id - Property list identifier
 * Outputs:
 *           encoding - Encoding character set:
 *     	                  H5T_CSET_ASCII_F -> US ASCII
 *     	                  H5T_CSET_UTF8_F -> UTF-8 Unicode encoding
 *
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              March 3, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_char_encoding_c(hid_t_f *plist_id, int_f *encoding)
{
  int ret_value = -1;
  H5T_cset_t c_encoding;
  herr_t ret;
  /*
   * Call H5Pget_char_encoding function.
   */
  ret = H5Pget_char_encoding((hid_t)*plist_id, &c_encoding);
  if (ret < 0) return ret_value;

  *encoding = (int_f)c_encoding;

  ret_value = 0;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:     h5pset_copy_object_c
 * Purpose:  Calls H5Pset_copy_object
 *
 * Inputs:
 *    ocp_plist_id - Object copy property list identifier
 *    copy_options - Copy option(s) to be set
 *
 * Outputs:
 *            NONE
 *
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              March 3, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_copy_object_c(hid_t_f *ocp_plist_id, int_f *copy_options)
{
  int ret_value = -1;
  herr_t ret;
  /*
   * Call H5Pset_copy_object function.
   */
  ret = H5Pset_copy_object((hid_t)*ocp_plist_id, (unsigned)*copy_options);
  if (ret < 0) return ret_value;

  ret_value = 0;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:     h5pget_copy_object_c
 * Purpose:  Calls H5Pget_copy_object
 *
 * Inputs:
 *    ocp_plist_id - Object copy property list identifier
 *
 * Outputs:
 *    copy_options - Copy option(s) to be get
 *
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              March 3, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_copy_object_c(hid_t_f *ocp_plist_id, int_f *copy_options)
{
  int ret_value = -1;
  unsigned c_copy_options;
  herr_t ret;
  /*
   * Call H5Pget_copy_object function.
   */
  ret = H5Pget_copy_object((hid_t)*ocp_plist_id, &c_copy_options);
  if (ret < 0) return ret_value;

  *copy_options = (int_f)c_copy_options;

  ret_value = 0;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_data_transform_c
 * Purpose:     Calls H5Pget_data_transform
 * Inputs:
 *              prp_id - property list identifier to query
 *      expression_len - buffer size transorm expression
 *
 * Output:
 *          expression - buffer to hold transform expression
 *
 * Returns:
 *          Success:  0
 *	    Failure: -1
 *
 * Programmer:  M.S. Breitenfeld
 *              March 19, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_data_transform_c(hid_t_f *plist_id, _fcd expression, int_f *expression_len, size_t_f *size)
{
    char *c_expression = NULL;          /* Buffer to hold C string */
    size_t c_expression_len;
    ssize_t ret;
    int_f ret_value = 0;

    c_expression_len = (size_t)*expression_len + 1;

    /*
     * Allocate memory to store the expression.
     */
    if(c_expression_len) {
        c_expression = (char *)HDmalloc(c_expression_len);
        if(NULL == c_expression)
           HGOTO_DONE(FAIL)
    } /* end if */

    /*
     * Call H5Pget_data_transform function.
     */
    ret = H5Pget_data_transform((hid_t)*plist_id, c_expression, c_expression_len);
    if(ret < 0)
       HGOTO_DONE(FAIL)

    /* or strlen ? */
    HD5packFstring(c_expression, _fcdtocp(expression), c_expression_len - 1);

    *size = (size_t_f)ret;

done:
    if(c_expression)
        HDfree(c_expression);

    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_data_transform_c
 * Purpose:     Calls H5Pset_data_transform
 * Inputs:
 *              prp_id - property list identifier to query
 *          expression - buffer to hold transform expression
 *      expression_len - buffer size transorm expression
 *
 * Output:
 *
 * Returns:
 *          Success:  0
 *	    Failure: -1
 *
 * Programmer:  M.S. Breitenfeld
 *              March 19, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_data_transform_c(hid_t_f *plist_id, _fcd expression, int_f *expression_len)
{
     char* c_expression = NULL; /* Buffer to hold C string */
     int_f ret_value = 0; /* Return value */

     /*
      * Convert FORTRAN name to C name
      */
     if(NULL == (c_expression = HD5f2cstring(expression, (size_t)*expression_len)))
        HGOTO_DONE(FAIL)

     /*
      * Call H5Pset_data_transform function.
      */
     if(H5Pset_data_transform((hid_t)*plist_id, c_expression) < 0)
        HGOTO_DONE(FAIL)

done:
    if(c_expression)
        HDfree(c_expression);

     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_local_heap_size_hint_c
 * Purpose:     Calls H5Pget_local_heap_size_hint
 * Inputs:
 *         gcpl_id - Group creation property list identifier
 *
 * Output:
 *       size_hint - Hint for size of local heap
 * Returns:
 *          Success:  0
 *	    Failure: -1
 *
 * Programmer:  M.S. Breitenfeld
 *              March 21, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_local_heap_size_hint_c(hid_t_f *gcpl_id, size_t_f *size_hint)
{
     int_f ret_value = -1; /* Return value */
     size_t c_size_hint;
     herr_t ret;
     /*
      * Call H5Pget_local_heap_size_hint function.
      */
     ret = H5Pget_local_heap_size_hint((hid_t)*gcpl_id, &c_size_hint);
     if(ret<0) return ret_value;

     *size_hint = (size_t_f)c_size_hint;
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_est_link_info_c
 * Purpose:     Calls H5Pget_est_link_info
 * Inputs:
 *              gcpl_id - Group creation property list identifier
 *
 * Output:
 *      est_num_entries - Estimated number of links to be inserted into group
 *         est_name_len - Estimated average length of link names
 * Returns:
 *          Success:  0
 *	    Failure: -1
 *
 * Programmer:  M.S. Breitenfeld
 *              March 21, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_est_link_info_c(hid_t_f *gcpl_id, int_f *est_num_entries, int_f *est_name_len)
{
     int_f ret_value = -1; /* Return value */
     unsigned c_est_num_entries;
     unsigned c_est_name_len;
     herr_t ret;
     /*
      * Call h5pget_est_link_info function.
      */
     ret = H5Pget_est_link_info((hid_t)*gcpl_id, &c_est_num_entries, &c_est_name_len);
     if(ret<0) return ret_value;

     *est_num_entries = (int_f)c_est_num_entries;
     *est_name_len = (int_f)c_est_name_len;

     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_local_heap_size_hint_c
 * Purpose:     Calls H5Pset_local_heap_size_hint
 * Inputs:
 *         gcpl_id - Group creation property list identifier
 *       size_hint - Hint for size of local heap
 *
 * Output:
 *
 * Returns:
 *          Success:  0
 *	    Failure: -1
 *
 * Programmer:  M.S. Breitenfeld
 *              March 21, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_local_heap_size_hint_c(hid_t_f *gcpl_id, size_t_f *size_hint)
{
     int_f ret_value = -1; /* Return value */
     herr_t ret;
     /*
      * Call H5Pget_local_heap_size_hint function.
      */
     ret = H5Pset_local_heap_size_hint((hid_t)*gcpl_id, (size_t)*size_hint);
     if(ret<0) return ret_value;

     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_est_link_info_c
 * Purpose:     Calls H5Pset_est_link_info
 * Inputs:
 *              gcpl_id - Group creation property list identifier
 *      est_num_entries - Estimated number of links to be inserted into group
 *         est_name_len - Estimated average length of link names
 *
 * Output:
 * Returns:
 *          Success:  0
 *	    Failure: -1
 *
 * Programmer:  M.S. Breitenfeld
 *              March 21, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_est_link_info_c(hid_t_f *gcpl_id, int_f *est_num_entries, int_f *est_name_len)
{
     int_f ret_value = -1; /* Return value */
     herr_t ret;
     /*
      * Call h5pset_est_link_info function.
      */
     ret = H5Pset_est_link_info((hid_t)*gcpl_id, (unsigned)*est_num_entries, (unsigned)*est_name_len);
     if(ret<0) return ret_value;

     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_link_phase_change_c
 * Purpose:     Calls H5Pset_link_phase_change
 *
 * Inputs:      gcpl_id     - Group creation property list identifier
 *              max_compact - Maximum number of attributes to be stored in compact storage
 *              min_dense   - Minimum number of attributes to be stored in dense storage
 * Outputs
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              March 21, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_link_phase_change_c(hid_t_f *gcpl_id, int_f *max_compact, int_f *min_dense )
{
  int ret_value = -1;
  herr_t ret;

  /*
   * Call H5Pset_link_phase_change function.
   */
  ret = H5Pset_link_phase_change((hid_t)*gcpl_id, (unsigned)*max_compact,(unsigned)*min_dense);
  if (ret < 0) return ret_value;

  ret_value = 0;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_fapl_direct_c
 * Purpose:     Calls H5Pset_fapl_direct
 *
 * Inputs:
 *    fapl_id 	 - File access property list identifier
 *    alignment  - Required memory alignment boundary
 *    block_size - File system block size
 *    cbuf_size  - Copy buffer size
 * Outputs
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              March 21, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_fapl_direct_c(hid_t_f UNUSED *fapl_id, size_t_f UNUSED *alignment, size_t_f UNUSED *block_size, size_t_f UNUSED *cbuf_size)
{
  int ret_value = -1;
#ifdef H5_HAVE_DIRECT
  herr_t ret;

  /*
   * Call H5Pset_link_phase_change function.
   */
  ret = H5Pset_fapl_direct((hid_t)*fapl_id, (size_t)*alignment, (size_t)*block_size, (size_t)*cbuf_size );
    if (ret < 0) return ret_value;

  ret_value = 0;

#endif
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_fapl_direct_c
 * Purpose:     Calls H5Pget_fapl_direct
 *
 * Inputs:
 *    fapl_id 	 - File access property list identifier
 * Outputs:
 *    alignment  - Required memory alignment boundary
 *    block_size - File system block size
 *    cbuf_size  - Copy buffer size
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              March 21, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_fapl_direct_c(hid_t_f UNUSED *fapl_id, size_t_f UNUSED *alignment, size_t_f UNUSED *block_size, size_t_f UNUSED *cbuf_size)
{
  int ret_value = -1;
#ifdef H5_HAVE_DIRECT
  herr_t ret;
  size_t c_alignment;
  size_t c_block_size;
  size_t c_cbuf_size;

  /*
   * Call H5Pget_link_phase_change function.
   */
  ret = H5Pget_fapl_direct((hid_t)*fapl_id, &c_alignment, &c_block_size, &c_cbuf_size );
  if (ret < 0) return ret_value;

  *alignment  = (size_t_f)c_alignment;
  *block_size = (size_t_f)c_block_size;
  *cbuf_size  = (size_t_f)c_cbuf_size;

  ret_value = 0;
#endif
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_attr_phase_change_c
 * Purpose:     Calls H5Pset_attr_phase_change
 *
 * Inputs:      ocpl_id		- Object (dataset or group) creation property list identifier
 *              max_compact     - Maximum number of attributes to be stored in compact storage
 *              min_dense       - Minimum number of attributes to be stored in dense storage
 * Outputs:
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              March 21, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_attr_phase_change_c(hid_t_f *ocpl_id, int_f *max_compact, int_f *min_dense )
{
  int ret_value = -1;
  herr_t ret;
  /*
   * Call H5Pset_attr_phase_change function.
   */
  ret = H5Pset_attr_phase_change((hid_t)*ocpl_id, (unsigned)*max_compact,(unsigned)*min_dense);
  if (ret < 0) return ret_value;

  ret_value = 0;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_nbit_c
 * Purpose:     Calls H5Pset_nbit
 *
 * Inputs:      plist_id - Dataset creation property list identifier
 * Outputs:
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              March 21, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_nbit_c(hid_t_f *plist_id )
{
  int ret_value = -1;
  herr_t ret;
  /*
   * Call H5Pset_nbit_change function.
   */
  ret = H5Pset_nbit((hid_t)*plist_id);
  if (ret < 0) return ret_value;

  ret_value = 0;
  return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5pset_scaleoffset_c
 * Purpose:     Calls H5Pset_scaleoffset
 *
 * Inputs:
 *      plist_id - Dataset creation property list identifier
 *    scale_type - Flag indicating compression method.
 *  scale_factor - Parameter related to scale.
 * Outputs:
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              March 21, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_scaleoffset_c(hid_t_f *plist_id, int_f *scale_type, int_f *scale_factor )
{
  int ret_value = -1;
  H5Z_SO_scale_type_t c_scale_type;
  herr_t ret;
  /*
   * Call H5Pset_scaleoffset_change function.
   */
  c_scale_type = (H5Z_SO_scale_type_t)*scale_type;

  ret = H5Pset_scaleoffset((hid_t)*plist_id, c_scale_type, (int)*scale_factor);
  if (ret < 0) return ret_value;

  ret_value = 0;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_nlinks
 * Purpose:     Calls H5Pset_nlinks
 *
 * Inputs:
 *            lapl_id - File access property list identifier
 *             nlinks - Maximum number of links to traverse
 * Outputs:
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              March 24, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_nlinks_c(hid_t_f *lapl_id, size_t_f *nlinks)
{
  int ret_value = -1;
  herr_t ret;
  /*
   * Call H5Pset_nlinks function.
   */
  ret = H5Pset_nlinks((hid_t)*lapl_id, (size_t)*nlinks);
  if (ret < 0) return ret_value;

  ret_value = 0;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_nlinks
 * Purpose:     Calls H5Pget_nlinks
 *
 * Inputs:
 *            lapl_id - File access property list identifier
 *
 * Outputs:
 *             nlinks - Maximum number of links to traverse
 *
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              March 24, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_nlinks_c(hid_t_f *lapl_id, size_t_f *nlinks)
{
  int ret_value = -1;
  herr_t ret;
  size_t c_nlinks;
  /*
   * Call H5Pget_nlinks function.
   */
  ret = H5Pget_nlinks((hid_t)*lapl_id, &c_nlinks);
  if (ret < 0) return ret_value;

  *nlinks = (size_t_f)c_nlinks;
  ret_value = 0;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_create_inter_group_c
 * Purpose:     Calls H5Pget_create_intermediate_group
 *
 * Inputs:
 *		lcpl_id - Link creation property list identifier
 *   crt_intermed_group - Specifying whether to create intermediate groups upon
 *                        the creation of an object
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              April 4, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pget_create_inter_group_c(hid_t_f *lcpl_id, int_f *crt_intermed_group)
{
  int ret_value = -1;
  herr_t ret;
  unsigned c_crt_intermed_group;

  /*
   * Call H5Pget_create_intermediate_group function.
   */
  ret = H5Pget_create_intermediate_group((hid_t)*lcpl_id, &c_crt_intermed_group);

  if (ret < 0) return ret_value; /* error occurred */

  *crt_intermed_group = (int_f)c_crt_intermed_group;
  ret_value = 0;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_chunk_cache_c
 * Purpose:     Calls H5Pset_chunk_cache
 *
 * Inputs:	dapl_id            - Link creation property list identifier
 *              rdcc_nslots        -
 *              rdcc_nbytes        -
 *              rdcc_w0            -
 *
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              April 13, 2009
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pset_chunk_cache_c(hid_t_f *dapl_id, size_t_f *rdcc_nslots, size_t_f *rdcc_nbytes, real_f *rdcc_w0)
{
  int ret_value = -1;

  /*
   * Call H5Pset_chunk_cache function.
   */
  if( (H5Pset_chunk_cache((hid_t)*dapl_id, (size_t)*rdcc_nslots, (size_t)*rdcc_nbytes, (double)*rdcc_w0)) <0 )
    return ret_value; /* error occurred */

  ret_value = 0;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_chunk_cache_c
 * Purpose:     Calls H5Pget_chunk_cache
 *
 * Inputs:	dapl_id            - Link creation property list identifier
 * Outputs:
 *              rdcc_nslots        -
 *              rdcc_nbytes        -
 *              rdcc_w0            -
 *
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              April 13, 2009
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5pget_chunk_cache_c(hid_t_f *dapl_id, size_t_f *rdcc_nslots, size_t_f *rdcc_nbytes, real_f *rdcc_w0)
{
  int ret_value = -1;
  size_t c_rdcc_nslots;
  size_t c_rdcc_nbytes;
  double c_rdcc_w0;
  /*
   * Call H5Pget_chunk_cache function.
   */
  if( (H5Pget_chunk_cache((hid_t)*dapl_id, &c_rdcc_nslots, &c_rdcc_nbytes, &c_rdcc_w0)) <0 )
    return ret_value; /* error occurred */

  *rdcc_nslots=(size_t_f)c_rdcc_nslots;
  *rdcc_nbytes=(size_t_f)c_rdcc_nbytes;
  *rdcc_w0=(real_f)c_rdcc_w0;

  ret_value = 0;
  return ret_value;
}
