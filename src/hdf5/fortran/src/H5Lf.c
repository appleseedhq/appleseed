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

/* This files contains C stubs for H5L Fortran APIs */

#include "H5f90.h"
#include "H5Eprivate.h"

/*----------------------------------------------------------------------------
 * Name:        h5lcopy_c
 * Purpose:     Call H5Lcopy
 * Inputs:
 *     src_loc_id - Location identifier of the source link
 *       src_name - Name of the link to be copied
 *    src_namelen - length of the name
 *    dest_loc_id - Location identifier specifying the destination of the copy
 *      dest_name - Name to be assigned to the NEW copy
 *   dest_namelen - Length of the name
 *         loc_id - Identifier of the file or group containing the object
 *           name - Name of the link to delete
 *        lcpl_id - Link creation property list identifier
 *        lapl_id - Link access property list identifier
 *
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              January, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5lcopy_c(hid_t_f *src_loc_id, _fcd src_name, size_t_f *src_namelen, hid_t_f *dest_loc_id,
	     _fcd dest_name, size_t_f *dest_namelen,
	     hid_t_f *lcpl_id, hid_t_f *lapl_id)
{
    char *c_src_name = NULL;
    char *c_dest_name = NULL;
    int_f ret_value = 0;

    /*
     * Convert FORTRAN name to C name
     */
    if(NULL == (c_src_name = HD5f2cstring(src_name, (size_t)*src_namelen)))
        HGOTO_DONE(FAIL)
    if(NULL == (c_dest_name = HD5f2cstring(dest_name, (size_t)*dest_namelen)))
        HGOTO_DONE(FAIL)

    /*
     * Call H5Lcopy function.
     */
    if(H5Lcopy((hid_t)*src_loc_id, c_src_name, (hid_t) *dest_loc_id,
  	       c_dest_name, (hid_t)*lcpl_id, (hid_t)*lapl_id ) < 0)
        HGOTO_DONE(FAIL)

done:
    if(c_src_name)
        HDfree(c_src_name);
    if(c_dest_name)
        HDfree(c_dest_name);

  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5lcreate_external_c
 * Purpose:     Call H5Lcreate_external_c
 * Inputs:
 *    file_name - Name of the file containing the target object. Neither the file nor the target object is
 *                required to exist. May be the file the link is being created in.
 *     obj_name - Path within the target file to the target object.
 *  link_loc_id - The file or group identifier for the new link.
 *    link_name - The name of the new link.
 *      lcpl_id - Link creation property list identifier.
 *      lapl_id - Link access property list identifier.
 *
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              February 29, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5lcreate_external_c(_fcd file_name, size_t_f *file_namelen, _fcd obj_name, size_t_f *obj_namelen,
		      hid_t_f *link_loc_id, _fcd link_name, size_t_f *link_namelen,
		      hid_t_f *lcpl_id, hid_t_f *lapl_id)

{
  char *c_file_name = NULL;
  char *c_obj_name = NULL;
  char *c_link_name = NULL;
  int_f ret_value = 0;

  /*
   * Convert FORTRAN name to C name
   */
  if((c_file_name = HD5f2cstring(file_name, (size_t)*file_namelen)) == NULL)
    HGOTO_DONE(FAIL);
  if((c_obj_name = HD5f2cstring(obj_name, (size_t)*obj_namelen)) == NULL)
    HGOTO_DONE(FAIL);
  if((c_link_name = HD5f2cstring(link_name, (size_t)*link_namelen)) == NULL)
    HGOTO_DONE(FAIL);

  /*
   * Call H5Lcopy function.
   */
  if( H5Lcreate_external( c_file_name, c_obj_name, (hid_t) *link_loc_id, c_link_name,
			  (hid_t) *lcpl_id, (hid_t) *lapl_id) < 0)
    HGOTO_DONE(FAIL);

done:
  if(c_file_name)
    HDfree(c_file_name);
  if(c_obj_name)
    HDfree(c_obj_name);
  if(c_link_name)
    HDfree(c_link_name);

  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5ldelete_c
 * Purpose:     Call H5Ldelete
 * Inputs:
 *
 *    loc_id  - Identifier of the file or group containing the object
 *    name    - Name of the link to delete
 *    lapl_id - Link access property list identifier
 *    namelen - length of name
 *
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              January, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5ldelete_c ( hid_t_f *loc_id, _fcd name, size_t_f *namelen, hid_t_f *lapl_id )
{
  char *c_name = NULL;
  int_f ret_value = 0;

  /*
   * Convert FORTRAN name to C name
   */
  if((c_name = HD5f2cstring(name, (size_t)*namelen)) == NULL)
    HGOTO_DONE(FAIL);

  /*
   * Call H5Ldelete function.
   */
  if( H5Ldelete( (hid_t)*loc_id, c_name, (hid_t)*lapl_id  ) < 0)
    HGOTO_DONE(FAIL);

done:
    if(c_name)
        HDfree(c_name);

    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5lcreate_soft_c
 * Purpose:     Call H5Lcreate_soft
 * Inputs:
 *
 *       target_path - Path to the target object, which is not required to exist.
 *       link_loc_id - The file or group identifier for the new link.
 *       link_name   - The name of the new link.
 *       lcpl_id     - Link creation property list identifier.
 *       lapl_id     - Link access property list identifier.
 *
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              February 20, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5lcreate_soft_c(_fcd target_path, size_t_f *target_path_len,
		  hid_t_f *link_loc_id,
		  _fcd link_name, size_t_f *link_name_len,
		  hid_t_f *lcpl_id, hid_t_f *lapl_id )
{
  char *c_target_path = NULL;
  char *c_link_name = NULL;
  int_f ret_value = 0;

  /*
   * Convert FORTRAN name to C name
   */
  if((c_target_path = HD5f2cstring(target_path, (size_t)*target_path_len)) == NULL)
    HGOTO_DONE(FAIL);
  if((c_link_name = HD5f2cstring(link_name, (size_t)*link_name_len)) == NULL)
    HGOTO_DONE(FAIL);

  /*
   * Call H5Adelete function.
   */
  if ( H5Lcreate_soft(c_target_path,(hid_t)*link_loc_id, c_link_name, (hid_t)*lcpl_id, (hid_t)*lapl_id) < 0)
    HGOTO_DONE(FAIL);

 done:
    if(c_target_path)
        HDfree(c_target_path);
    if(c_link_name)
        HDfree(c_link_name);

    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5lcreate_hard_c
 * Purpose:     Call H5Lcreate_hard
 * Inputs:
 *       obj_loc_id  - The file or group identifier for the target object.
 *       obj_name    - Name of the target object, which must already exist.
 *       obj_namelen - Name length
 *       link_loc_id - The file or group identifier for the new link.
 *       link_name   - The name of the new link.
 *       link_namelen- Name length
 *       lcpl_id     - Link creation property list identifier.
 *       lapl_id     - Link access property list identifier.
 *
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              February 27, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5lcreate_hard_c(hid_t_f *obj_loc_id, _fcd obj_name, size_t_f *obj_namelen,
		  hid_t_f *link_loc_id,
		  _fcd link_name, size_t_f *link_namelen,
		  hid_t_f *lcpl_id, hid_t_f *lapl_id )
{
  char *c_obj_name = NULL;
  char *c_link_name = NULL;
  int_f ret_value = 0;

  /*
   * Convert FORTRAN name to C name
   */
  if((c_obj_name = HD5f2cstring(obj_name, (size_t)*obj_namelen)) == NULL)
    HGOTO_DONE(FAIL);
  if((c_link_name = HD5f2cstring(link_name, (size_t)*link_namelen)) == NULL)
    HGOTO_DONE(FAIL);

  /*
   * Call H5Lcreate_hard function.
   */
  if ( H5Lcreate_hard((hid_t)*obj_loc_id, c_obj_name, (hid_t)*link_loc_id, c_link_name, (hid_t)*lcpl_id, (hid_t)*lapl_id) < 0)
    HGOTO_DONE(FAIL);

 done:
    if(c_obj_name)
        HDfree(c_obj_name);
    if(c_link_name)
        HDfree(c_link_name);

    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5ldelete_by_idx_c
 * Purpose:     Calls h5ldelete_by_idx
 * Inputs:
 *          loc_id - File or group identifier specifying location of subject group
 *      group_name - Name of subject group
 *   group_namelen - Name length
 *     index_field - Type of index; Possible values are:
 *                    H5_INDEX_UNKNOWN_F = -1  - Unknown index type
 *                    H5_INDEX_NAME_F          - Index on names
 *                    H5_INDEX_CRT_ORDER_F     - Index on creation order
 *                    H5_INDEX_N_F	       - Number of indices defined
 *           order - Order within field or index; Possible values are:
 *                    H5_ITER_UNKNOWN_F   - Unknown order
 *                    H5_ITER_INC_F       - Increasing order
 *                    H5_ITER_DEC_F       - Decreasing order
 *                    H5_ITER_NATIVE_F    - No particular order, whatever is fastest
 *                    H5_ITER_N_F	  - Number of iteration orders
 *               n - Link for which to retrieve information
 *         lapl_id - Link access property list
 *
 * Outputs:     N/A
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              February 29, 2008
 * Modifications: N/A
 *---------------------------------------------------------------------------*/
int_f
nh5ldelete_by_idx_c (hid_t_f *loc_id, _fcd group_name, size_t_f *group_namelen,
		     int_f *index_field, int_f *order, hsize_t_f *n, hid_t_f *lapl_id)
{
    char *c_group_name = NULL;          /* Buffer to hold C string */
    H5_index_t c_index_field;
    H5_iter_order_t c_order;
    int_f ret_value = 0;          /* Return value */

     /*
      * Convert FORTRAN name to C name
      */
    if((c_group_name = HD5f2cstring(group_name, (size_t)*group_namelen)) == NULL)
      HGOTO_DONE(FAIL);

    c_index_field = (H5_index_t)*index_field;
    c_order = (H5_iter_order_t)*order;

     /*
      * Call H5Ldelete_by_name function.
      */
    if(H5Ldelete_by_idx((hid_t)*loc_id, c_group_name, c_index_field, c_order, (hsize_t)*n, (hid_t)*lapl_id) < 0)
      HGOTO_DONE(FAIL);

done:
    if(c_group_name)
        HDfree(c_group_name);
    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5lexists_c
 * Purpose:     Calls H5Lexists
 * Inputs:
 *      loc_id - Identifier of the file or group to query.
 *        name - Link name to check
 *     lapl_id - Link access property list identifier.
 * Outputs:
 *     link_exists_c  - returns a positive value, for TRUE, or 0 (zero), for FALSE.
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              February 29, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5lexists_c (hid_t_f *loc_id, _fcd name, size_t_f *namelen, hid_t_f *lapl_id, int_f *link_exists)
{
  char *c_name = NULL;          /* Buffer to hold C string */
  int_f ret_value = 0;          /* Return value */

  /*
   * Convert FORTRAN name to C name
   */
  if((c_name = HD5f2cstring(name, (size_t)*namelen)) == NULL)
    HGOTO_DONE(FAIL);

  /*
   * Call H5Lexists function.
   */
  if((*link_exists = (int_f)H5Lexists((hid_t)*loc_id, c_name, (hid_t)*lapl_id)) < 0)
         HGOTO_DONE(FAIL);

done:
    if(c_name)
        HDfree(c_name);
    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5lget_info_c
 * Purpose:     Call  H5Lget_info
 * Inputs:
 *		link_loc_id - File or group identifier.
 *                link_name - Name of the link for which information is being sought
 *             link_namelen - Name length
 *                  lapl_id - Link access property list
 * Outputs:
 *
 *              cset - indicates the character set used for link’s name.
 *            corder - specifies the link’s creation order position.
 *      corder_valid - indicates whether the value in corder is valid.
 *         link_type -  specifies the link class:
 *     	                H5L_LINK_HARD_F      - Hard link
 *     	                H5L_LINK_SOFT_F      - Soft link
 *     	                H5L_LINK_EXTERNAL_F  - External link
 *     	                H5L_LINK_ERROR_F     - Error
 *           address - If the link is a hard link, address specifies the file address that the link points to
 *          val_size - If the link is a symbolic link, val_size will be the length of the link value
 *
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              January, 2008
 * Modifications: N/A
 *---------------------------------------------------------------------------*/
int_f
nh5lget_info_c(hid_t_f *link_loc_id, _fcd link_name, size_t_f *link_namelen,
		int_f *cset, int_f *corder, int_f *corder_valid, int_f *link_type,
		haddr_t_f *address, size_t_f *val_size,
		hid_t_f *lapl_id)
{
    char *c_link_name = NULL;          /* Buffer to hold C string */
    H5L_info_t link_buff;
    int_f ret_value = 0;          /* Return value */

    /*
     * Convert FORTRAN name to C name
     */
    if(NULL == (c_link_name = HD5f2cstring(link_name, (size_t)*link_namelen)))
        HGOTO_DONE(FAIL);

    /*
     * Call H5Linfo function.
     */
    if(H5Lget_info((hid_t)*link_loc_id, c_link_name, &link_buff, (hid_t)*lapl_id) < 0)
        HGOTO_DONE(FAIL);

    /* Unpack the structure */
    *cset = (int_f)link_buff.cset;
    *corder = (int_f)link_buff.corder;
    *corder_valid = 0;
    if(link_buff.corder_valid > 0) *corder_valid = 1;
    *link_type = (int_f)link_buff.type;
    *address = (haddr_t_f)link_buff.u.address;
    *val_size = (size_t_f)link_buff.u.val_size;

done:
    if(c_link_name)
        HDfree(c_link_name);

    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5lget_info_by_idx_c
 * Purpose:     Call  H5Lget_info_by_idx
 * Inputs:
 *	loc_id  - File or group identifier specifying location of subject group
 *   group_name - Name of subject group
 *group_namelen - Name length
 *  index_field - Index or field which determines the order
 *        order - Order within field or index
 *            n - Link for which to retrieve information
 *      lapl_id - Link access property list
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
nh5lget_info_by_idx_c(hid_t_f *loc_id, _fcd group_name, size_t_f *group_namelen,
		      int_f *index_field, int_f *order, hsize_t_f *n,
		      int_f *link_type, int_f *corder_valid, int_f *corder, int_f *cset,  haddr_t_f *address, size_t_f *val_size, hid_t_f *lapl_id)
{
    char *c_group_name = NULL;          /* Buffer to hold C string */
    H5L_info_t link_buff;
    int_f ret_value = 0;          /* Return value */

    /*
     * Convert FORTRAN name to C name
     */
    if(NULL == (c_group_name = HD5f2cstring(group_name, (size_t)*group_namelen)))
        HGOTO_DONE(FAIL)

     /*
      * Call H5Linfo_by_idx function.
      */
    if(H5Lget_info_by_idx((hid_t)*loc_id, c_group_name, (H5_index_t)*index_field, (H5_iter_order_t)*order, (hsize_t)*n,
			  &link_buff, (hid_t)*lapl_id) < 0)
        HGOTO_DONE(FAIL)

    /* Unpack the structure */
    *corder_valid = 0;
    if(link_buff.corder_valid > 0)
        *corder_valid = 1;
    *corder = (int_f)link_buff.corder;
    *cset = (int_f)link_buff.cset;
    *link_type = (int_f)link_buff.type;
    *address = (haddr_t_f)link_buff.u.address;
    *val_size = (size_t_f)link_buff.u.val_size;

done:
    if(c_group_name)
        HDfree(c_group_name);

    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        H5Lis_registered_c
 * Purpose:     Call H5Lis_registered
 * Inputs:
 *  link_cls_id - User-defined link class identifier
 * Outputs:     NONE
 *
 * Returns:     Returns a positive value if the link class has been registered
 *              and zero if it is unregistered. Otherwise returns a negative value
 * Programmer:  M.S. Breitenfeld
 *              March 3, 2008
 * Modifications: N/A
 *---------------------------------------------------------------------------*/
int_f
nh5lis_registered_c(int_f *link_cls_id)
{
    int_f ret_value;      /* Return value */

    /*
     * Call H5Lis_registered
     */
    ret_value = (int_f)H5Lis_registered((H5L_type_t)*link_cls_id);

    return ret_value;
}


/*----------------------------------------------------------------------------
 * Name:        h5lmove_c
 * Purpose:     Call  H5Lmove
 * Inputs:
 *    src_loc_id   - Original file or group identifier.
 *    src_name     - Original link name.
 *    src_namelen  - name length
 *    dest_loc_id  - Destination file or group identifier.
 *    dest_name    - NEW link name.
 *    dest_namelen - name length
 * Outputs:
 *
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              March 3, 2008
 * Modifications: N/A
 *---------------------------------------------------------------------------*/
int_f
nh5lmove_c(hid_t_f *src_loc_id, _fcd src_name, size_t_f *src_namelen, hid_t_f *dest_loc_id,
	   _fcd dest_name, size_t_f *dest_namelen, hid_t_f *lcpl_id, hid_t_f *lapl_id)

{
    char *c_src_name = NULL;  /* Buffer to hold C string */
    char *c_dest_name = NULL; /* Buffer to hold C string */
    int_f ret_value = 0;          /* Return value */

    /*
     * Convert FORTRAN name to C name
     */
    if(NULL == (c_src_name = HD5f2cstring(src_name, (size_t)*src_namelen)))
        HGOTO_DONE(FAIL)
    if(NULL == (c_dest_name = HD5f2cstring(dest_name, (size_t)*dest_namelen)))
        HGOTO_DONE(FAIL)

     /*
      * Call H5Lmove function.
      */
    if(H5Lmove((hid_t)*src_loc_id, c_src_name, (hid_t)*dest_loc_id,
	       c_dest_name, (hid_t)*lcpl_id, (hid_t)*lapl_id) < 0)
        HGOTO_DONE(FAIL)

done:
    if(c_src_name)
        HDfree(c_src_name);
    if(c_dest_name)
        HDfree(c_dest_name);

    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5lget_name_by_idx_c
 * Purpose:     Call  H5Lget_name_by_idx
 * Inputs:
 *   loc_id      - File or group identifier specifying location of subject group
 *   group_name  - Name of subject group
 *   index_field - Index or field which determines the order
 *   order       - Order within field or index
 *   n           - Link for which to retrieve information
 *   size        - Maximum number of characters of link value to be returned.
 *   lapl_id     - Link access property list
 * Outputs:
 *   name        - Buffer in which link value is returned
 *   size        - The size of the link name on success
 * Returns:      0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              March 10, 2008
 * Modifications: N/A
 *---------------------------------------------------------------------------*/
int_f
nh5lget_name_by_idx_c(hid_t_f *loc_id, _fcd group_name, size_t_f *group_namelen,
		      int_f *index_field, int_f *order, hsize_t_f *n,
		      size_t_f *size, _fcd name, hid_t_f *lapl_id)
{
    char *c_group_name = NULL;          /* Buffer to hold C string */
    char *c_name = NULL;          /* Buffer to hold C string */
    size_t c_size;
    int_f ret_value = 0;          /* Return value */

    /*
     * Convert FORTRAN name to C name
     */
    if(NULL == (c_group_name = HD5f2cstring(group_name, (size_t)*group_namelen)))
        HGOTO_DONE(FAIL)

    c_size = (size_t)*size + 1;

    /*
     * Allocate buffer to hold name of an attribute
     */
    if(NULL == (c_name = (char *)HDmalloc(c_size)))
        HGOTO_DONE(FAIL)

    if((*size = (size_t)H5Lget_name_by_idx((hid_t)*loc_id, c_group_name, (H5_index_t)*index_field,
            (H5_iter_order_t)*order, (hsize_t)*n,c_name, c_size, (hid_t)*lapl_id)) < 0)
        HGOTO_DONE(FAIL)

    /*
     * Convert C name to FORTRAN and place it in the given buffer
     */
    if(c_name)
        HD5packFstring(c_name, _fcdtocp(name), c_size - 1);

done:
    if(c_group_name)
        HDfree(c_group_name);
    if(c_name)
        HDfree(c_name);

    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        H5Lregistered_c
 * Purpose:     Call H5Lregistered
 * Inputs:
 *
 * Inputs:
 *     version         - Version number of this struct
 *     class_id        - Link class identifier
 *     comment         - Comment for debugging
 *     comment_len     - Comment for debugging
 *     create_func     - Callback during link creation
 *     create_func_len - length
 *    move_func        - Callback after moving link
 *    move_func_len    - length
 *     copy_func       - Callback after copying link
 *     copy_func_len   - length
 *     trav_func       - The main traversal function
 *     trav_func_len   - length
 *     del_func        - Callback for link deletion
 *     del_func_len    - length
 *     query_func      - Callback for queries
 *     query_func_len  - length
 *
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              February 3, 2008
 * Modifications:
 *---------------------------------------------------------------------------*/

/* int_f */

/* nh5lregistered_c(int_f *version, int_f *class_id, */
/* 		 _fcd comment, size_t_f *comment_len, */
/* 		 _fcd create_func, size_t_f *create_func_len, */
/* 		 _fcd move_func, size_t_f *move_func_len, */
/* 		 _fcd copy_func, size_t_f *copy_func_len, */
/* 		 _fcd trav_func, size_t_f *trav_func_len, */
/* 		 _fcd del_func , size_t_f *del_func_len, */
/* 		 _fcd query_func, size_t_f *query_func_len) */
/* { */
/*   char *c_comment = NULL;         */
/*   char *c_create_func = NULL; */
/*   char *c_move_func = NULL;    */
/*   char *c_copy_func = NULL;   */
/*   char *c_trav_func = NULL;  */
/*   char *c_del_func = NULL;  */
/*   char *c_query_func = NULL; */

/*   H5L_class_t class_t; */

/*   int ret_value = 0; */

/*   /\* */
/*    * Convert FORTRAN name to C name */
/*    *\/ */
/*   if((c_comment = HD5f2cstring(c_comment, (size_t)*sc_comment_len)) == NULL) */
/*     HGOTO_DONE(FAIL); */
/*   if((c_create_func = HD5f2cstring(c_create_func, (size_t)*c_create_func_len)) == NULL) */
/*     HGOTO_DONE(FAIL); */
/*   if((c_move_func = HD5f2cstring(c_move_func, (size_t)*sc_move_func_len)) == NULL) */
/*     HGOTO_DONE(FAIL); */
/*   if((c_copy_func = HD5f2cstring(c_copy_func, (size_t)*c_copy_func_len)) == NULL) */
/*     HGOTO_DONE(FAIL); */
/*   if((c_trav_func = HD5f2cstring(c_trav_func, (size_t)*sc_trav_func_len)) == NULL) */
/*     HGOTO_DONE(FAIL); */
/*   if((c_del_func = HD5f2cstring(c_del_func, (size_t)*c_del_func_len)) == NULL) */
/*     HGOTO_DONE(FAIL); */
/*   if((c_query_func = HD5f2cstring(c_query_func, (size_t)*c_query_func_len)) == NULL) */
/*     HGOTO_DONE(FAIL); */
/*   /\* */
/*    * Pack into C struct H5L_class_t */
/*    *\/ */
/*       int version;                    /\* Version number of this struct  *\/ */
/*       H5L_type_t class_id;            /\* Link class identifier          *\/ */
/*       const char *comment;            /\* Comment for debugging          *\/ */
/*       H5L_create_func_t create_func;  /\* Callback during link creation  *\/ */
/*       H5L_move_func_t move_func;      /\* Callback after moving link     *\/ */
/*       H5L_copy_func_t copy_func;      /\* Callback after copying link    *\/ */
/*       H5L_traverse_func_t trav_func;  /\* The main traversal function    *\/ */
/*       H5L_delete_func_t del_func;     /\* Callback for link deletion     *\/ */
/*       H5L_query_func_t query_func;    /\* Callback for queries           *\/ */

/*       class_t.version = (int)*version; */
/*       class_t.class_id = (H5L_type_t)*class_id; */
/*       class_t.comment = c_comment; */
/*       class_t. */

/*   /\* */
/*    * Call H5Lcopy function. */
/*    *\/ */
/*   if( H5Lcopy( (hid_t)*src_loc_id, c_src_name, (hid_t) *dest_loc_id,  */
/* 	       c_dest_name, (hid_t)*lcpl_id, (hid_t)*lapl_id ) < 0) */
/*     HGOTO_DONE(FAIL); */

/* done: */
/*   if(c_src_name) */
/*     HDfree(c_src_name); */
/*   if(c_dest_name) */
/*     HDfree(c_dest_name); */

/*   return ret_value; */
/* } */

/*----------------------------------------------------------------------------
 * Name:        h5lget_val_c
 * Purpose:     Call H5Lget_val_c
 * Inputs:
 *               link_loc_id - File or group identifier.
 *                 link_name - Link whose value is to be returned.
 *             link_name_len - length of link_name
 *                      size - Maximum number of characters of link value to be returned.
 *                  lapl_id  - List access property list identifier
 * Outputs:
 *             linkval_buff  - The buffer to hold the returned link value.
 *
 * Returns:      0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              April 11, 2008
 * Modifications: N/A
 *---------------------------------------------------------------------------*/
int_f
nh5lget_val_c(hid_t_f *link_loc_id, _fcd link_name, size_t_f *link_namelen, size_t_f *size,
	      void *linkval_buff, hid_t_f *lapl_id)
{
    char *c_link_name = NULL; /* Buffer to hold C string */
    int_f ret_value = 0;       /* Return value */

    /*
     * Convert FORTRAN name to C name
     */
    if(NULL == (c_link_name = HD5f2cstring(link_name, (size_t)*link_namelen)))
        HGOTO_DONE(FAIL)

    /*
     * Call H5Lget_val
     */
    if(H5Lget_val((hid_t)*link_loc_id, c_link_name, &linkval_buff, (size_t)*size, (hid_t)*lapl_id )< 0)
        HGOTO_DONE(FAIL)

done:
   if(c_link_name)
        HDfree(c_link_name);

    return ret_value;
}

