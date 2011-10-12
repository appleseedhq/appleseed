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

/* This files contains C stubs for H5R Fortran APIs */

#include "H5f90.h"
#include "H5Eprivate.h"

/*----------------------------------------------------------------------------
 * Name:        h5rcreate_object_c
 * Purpose:     Call H5Rcreate to create a reference to an object
 * Inputs:      loc_id - file or group identifier
 *              name - name of the dataset
 *              namelen - name length
 * Outputs:     ref  - reference to the object
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, December 1, 1999
 *---------------------------------------------------------------------------*/
int_f
nh5rcreate_object_c(haddr_t_f *ref, hid_t_f *loc_id, _fcd name, int_f *namelen)
{
     char *c_name = NULL;
     hobj_ref_t ref_c;
     int_f ret_value = 0;

     /*
      * Convert FORTRAN name to C name
      */
     if(NULL == (c_name = (char *)HD5f2cstring(name, (size_t)*namelen)))
         HGOTO_DONE(FAIL)

     /*
      * Call H5Rcreate function.
      */
     if(H5Rcreate(&ref_c, *loc_id, c_name, H5R_OBJECT, -1) < 0)
         HGOTO_DONE(FAIL)

     /* Copy the reference created */
     *ref = (haddr_t_f)ref_c;

done:
     if(c_name)
         HDfree(c_name);
     return ret_value;
} /* nh5rcreate_object_c() */

/*----------------------------------------------------------------------------
 * Name:        h5rcreate_region_c
 * Purpose:     Call H5Rcreate to create a reference to dataset region
 *              region
 * Inputs:      loc_id - file or group identifier
 *              name - name of the dataset
 *              namelen - name length
 *              space_id - dataset space identifier
 * Outputs:     ref  - reference to the dataset region
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, December 1, 1999
 *---------------------------------------------------------------------------*/
int_f
nh5rcreate_region_c(int_f *ref, hid_t_f *loc_id, _fcd name, int_f *namelen, hid_t_f *space_id)
{
     char *c_name = NULL;
     hdset_reg_ref_t ref_c;
     int_f ret_value = 0;

     /*
      * Convert FORTRAN name to C name
      */
     if(NULL == (c_name = (char *)HD5f2cstring(name, (size_t)*namelen)))
         HGOTO_DONE(FAIL)

     /*
      * Call H5Rcreate function.
      */
     if(H5Rcreate(&ref_c, (hid_t)*loc_id, c_name, H5R_DATASET_REGION, (hid_t)*space_id) < 0)
         HGOTO_DONE(FAIL)

     /* Copy the reference created */
     HDmemcpy(ref, &ref_c, H5R_DSET_REG_REF_BUF_SIZE);

done:
     if(c_name)
         HDfree(c_name);
     return ret_value;
} /* end nh5rcreate_region_c() */

/*----------------------------------------------------------------------------
 * Name:        h5rdereference_region_c
 * Purpose:     Call H5Rdereference to dereference to dataset region
 * Inputs:      dset_id - dataset identifier
 *              ref - reference to the dataset region
 * Outputs:     obj_id - dereferenced dataset identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, December 1, 1999
 *---------------------------------------------------------------------------*/
int_f
nh5rdereference_region_c(hid_t_f *dset_id, int_f *ref, hid_t_f *obj_id)
{
     hdset_reg_ref_t ref_c;
     hid_t c_obj_id;
     int_f ret_value = 0;

     /* Copy the reference to dereference */
     HDmemcpy(&ref_c, ref, H5R_DSET_REG_REF_BUF_SIZE);

     /*
      * Call H5Rdereference function.
      */
     if((c_obj_id = H5Rdereference((hid_t)*dset_id, H5R_DATASET_REGION, &ref_c)) < 0)
         HGOTO_DONE(FAIL)

     /* Copy the object's ID */
     *obj_id = (hid_t_f)c_obj_id;

done:
     return ret_value;
} /* end nh5rdereference_region_c() */

/*----------------------------------------------------------------------------
 * Name:        h5rdereference_object_c
 * Purpose:     Call H5Rdereference to dereference an object
 * Inputs:      dset_id - dataset identifier
 *              ref - reference to an object
 * Outputs:     obj_id - dereferenced  object identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, December 1, 1999
 *---------------------------------------------------------------------------*/
int_f
nh5rdereference_object_c(hid_t_f *dset_id, haddr_t_f *ref, hid_t_f *obj_id)
{
     hid_t c_obj_id;
     hobj_ref_t ref_c = (hobj_ref_t)*ref;
     int_f ret_value = 0;

     /*
      * Call H5Rdereference function.
      */
     if((c_obj_id = H5Rdereference((hid_t)*dset_id, H5R_OBJECT, &ref_c)) < 0)
         HGOTO_DONE(FAIL)

     /* Copy the object's ID */
     *obj_id = (hid_t_f)c_obj_id;

done:
     return ret_value;
} /* end nh5rdereference_object_c() */

/*----------------------------------------------------------------------------
 * Name:        h5rget_region_region_object_c
 * Purpose:     Call H5Rget_region to dereference dataspace region
 * Inputs:      dset_id - dataset identifier
 *              ref - reference to the dataset region
 * Outputs:     space_id - dereferenced  dataset dataspace identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, December 1, 1999
 *---------------------------------------------------------------------------*/
int_f
nh5rget_region_region_c(hid_t_f *dset_id, int_f *ref, hid_t_f *space_id)
{
     hid_t c_space_id;
     hdset_reg_ref_t ref_c;
     int_f ret_value = 0;

     /* Copy the reference to dereference */
     HDmemcpy(&ref_c, ref, H5R_DSET_REG_REF_BUF_SIZE);

     /*
      * Call H5Rget_region function.
      */
     if((c_space_id = H5Rget_region((hid_t)*dset_id, H5R_DATASET_REGION, &ref_c)) < 0)
         HGOTO_DONE(FAIL)

     /* Copy the dataspace ID */
     *space_id = (hid_t_f)c_space_id;

done:
     return ret_value;
} /* end nh5rget_region_region_c() */

/*----------------------------------------------------------------------------
 * Name:        h5rget_object_type_obj_c
 * Purpose:     Call H5Rget_object_type to retrieve the type of the object reference points
 *              to
 * Inputs:      dset_id - dataset identifier
 *              ref - reference to the dataset region
 * Outputs:     obj_type - type of dereferenced object
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, December 1, 1999
 *---------------------------------------------------------------------------*/
int_f
nh5rget_object_type_obj_c(hid_t_f *dset_id, haddr_t_f *ref, int_f *obj_type)
{
     H5O_type_t c_obj_type;
     hobj_ref_t ref_c = (hobj_ref_t)*ref;
     int_f ret_value = 0;

     /*
      * Call H5Rget_object_type function.
      */
     if(H5Rget_obj_type2((hid_t)*dset_id, H5R_OBJECT, &ref_c, &c_obj_type) < 0)
         HGOTO_DONE(FAIL)

     /* Copy the object type */
     *obj_type = (int_f)c_obj_type;

done:
     return ret_value;
} /* end nh5rget_object_type_obj_c() */

/*----------------------------------------------------------------------------
 * Name:        h5rget_name_object_c
 * Purpose:     Call H5Rget_name for an object
 * Inputs:
 *       loc_id - Identifier for the dataset containing the reference or for the group that dataset is in.
 *          ref - An object or dataset region reference.
 *
 * Outputs:     name - A name associated with the referenced object or dataset region.
 *              size - The size of the name buffer.
 *
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              March 31, 2008
 *---------------------------------------------------------------------------*/
int_f
nh5rget_name_object_c(hid_t_f *loc_id, haddr_t_f *ref, _fcd name, size_t_f *name_len, size_t_f *size_default)
{
     hobj_ref_t ref_c = (hobj_ref_t)*ref;
     ssize_t c_size;
     size_t c_bufsize = (size_t)*name_len + 1;
     char *c_buf = NULL;  /* Buffer to hold C string */
     int_f ret_value = 0;


     /*
      * Allocate buffer to hold name of an attribute
      */
     if(NULL == (c_buf = (char *)HDmalloc(c_bufsize)))
         HGOTO_DONE(FAIL)
        
     /*
      * Call H5Rget_name function.
      */
     if((c_size = H5Rget_name((hid_t)*loc_id, H5R_OBJECT, &ref_c, c_buf, c_bufsize)) < 0)
         HGOTO_DONE(FAIL)

     /*
      * Convert C name to FORTRAN and place it in the given buffer
      */
     HD5packFstring(c_buf, _fcdtocp(name), c_bufsize-1);
     *size_default = (size_t_f)c_size;

done:
     if(c_buf) 
         HDfree(c_buf);
     return ret_value;
} /* end nh5rget_name_object_c() */

/*----------------------------------------------------------------------------
 * Name:        h5rget_name_region_c
 * Purpose:     Call H5Rget_name for a dataset region
 * Inputs:
 *       loc_id - Identifier for the dataset containing the reference or for the group that dataset is in.
 *          ref - An object or dataset region reference.
 *
 * Outputs:     name - A name associated with the referenced object or dataset region.
 *              size - The size of the name buffer.
 *
 * Returns:     0 on success, -1 on failure
 * Programmer:  M.S. Breitenfeld
 *              March 31, 2008
 *---------------------------------------------------------------------------*/
int_f
nh5rget_name_region_c(hid_t_f *loc_id, int_f *ref, _fcd name, size_t_f *name_len, size_t_f *size_default)
{
     hdset_reg_ref_t ref_c;
     ssize_t c_size;
     size_t c_bufsize = (size_t)*name_len + 1;
     char *c_buf = NULL;  /* Buffer to hold C string */
     int_f ret_value = 0;

     /* Copy the reference to query */
     HDmemcpy(&ref_c, ref, H5R_DSET_REG_REF_BUF_SIZE);

     /*
      * Allocate buffer to hold name of an attribute
      */
     if(NULL == (c_buf = (char *)HDmalloc(c_bufsize)))
         HGOTO_DONE(FAIL)

     /*
      * Call H5Rget_name function.
      */
     if((c_size = H5Rget_name((hid_t)*loc_id, H5R_DATASET_REGION, &ref_c, c_buf, c_bufsize)) < 0)
         HGOTO_DONE(FAIL)

     /*
      * Convert C name to FORTRAN and place it in the given buffer
      */
     HD5packFstring(c_buf, _fcdtocp(name), c_bufsize - 1);
     *size_default = (size_t_f)c_size;

done:
     if(c_buf) 
         HDfree(c_buf);
     return ret_value;
} /* end nh5rget_name_region_c() */

