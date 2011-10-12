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

#include "H5IMcc.h"

#include <string.h>
#include <stdlib.h>


/*-------------------------------------------------------------------------
 * private functions
 *-------------------------------------------------------------------------
 */
herr_t H5IM_get_palette(hid_t loc_id,
                         const char *image_name,
                         int pal_number,
                         hid_t tid,
                         void *pal_data);

/*-------------------------------------------------------------------------
 * Function: H5IMmake_image_8bitf
 *
 * Purpose: Creates and writes an image an 8 bit image
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente Nunes, pvn@ncsa.uiuc.edu
 *
 * Date: May 10, 2005
 *
 * Comments:
 *  This function allows the creation and writing of an 8bit image on disk.
 *  The memory datatype is H5T_NATIVE_INT. It is supposed to be called from
 *  the FORTRAN interface where the image buffer is defined as type "integer"
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5IMmake_image_8bitf(hid_t loc_id,
                             const char *dset_name,
                             hsize_t width,
                             hsize_t height,
                             int_f *buf)
{
 hid_t    did;                  /* dataset ID */
 hid_t    sid;                  /* space ID */
 hsize_t  dims[IMAGE8_RANK];  /* dimensions */

 /* initialize the image dimensions */
 dims[0] = height;
 dims[1] = width;

/*-------------------------------------------------------------------------
 * create and write the dataset
 *-------------------------------------------------------------------------
 */

 /* create the data space for the dataset. */
 if((sid = H5Screate_simple(IMAGE8_RANK, dims, NULL)) < 0)
  return -1;

 /* create the dataset as H5T_NATIVE_UCHAR */
 if((did = H5Dcreate2(loc_id, dset_name, H5T_NATIVE_UINT8, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
  return -1;

 /* write with memory type H5T_NATIVE_INT */
 /* Use long type if Fortran integer is 8 bytes and C long long is also 8 bytes*/
 /* Fail if otherwise */
 if(buf) {
  if(sizeof(int_f) == sizeof(int)) {
      if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
       return -1;
  } else if(sizeof(int_f) == sizeof(long)) {
      if(H5Dwrite(did, H5T_NATIVE_LONG, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
       return -1;
  } else if(sizeof(int_f) == sizeof(long long)) {
      if(H5Dwrite(did, H5T_NATIVE_LLONG, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
       return -1;
  } else
       return -1;
 }

 /* close */
 if(H5Dclose(did) < 0)
  return -1;
 if(H5Sclose(sid) < 0)
  return -1;

/*-------------------------------------------------------------------------
 * attach the specification attributes
 *-------------------------------------------------------------------------
 */

 /* attach the CLASS attribute */
 if(H5LTset_attribute_string(loc_id, dset_name, "CLASS", IMAGE_CLASS) < 0)
  return -1;

 /* attach the VERSION attribute */
 if(H5LTset_attribute_string(loc_id, dset_name, "IMAGE_VERSION", IMAGE_VERSION) < 0)
  return -1;

 /* attach the IMAGE_SUBCLASS attribute */
 if(H5LTset_attribute_string(loc_id, dset_name, "IMAGE_SUBCLASS", "IMAGE_INDEXED") < 0)
  return -1;

 return 0;
}



/*-------------------------------------------------------------------------
 * Function: H5IMmake_image_24bitf
 *
 * Purpose:
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente Nunes, pvn@ncsa.uiuc.edu
 *
 * Date: May 10, 2005
 *
 * Comments:
 *  This function allows the creation and writing of an 8bit image on disk.
 *  The memory datatype is H5T_NATIVE_INT. It is supposed to be called from
 *  the FORTRAN interface where the image buffer is defined as type "integer"
 *
 * Interlace Mode Dimensions in the Dataspace
 * INTERLACE_PIXEL [height][width][pixel components]
 * INTERLACE_PLANE [pixel components][height][width]
 *
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5IMmake_image_24bitf(hid_t loc_id,
                              const char *dset_name,
                              hsize_t width,
                              hsize_t height,
                              const char *interlace,
                              int_f *buf)
{
 hid_t    did;                /* dataset ID */
 hid_t    sid;                /* space ID */
 hsize_t  dims[IMAGE24_RANK]; /* dimensions */

/*-------------------------------------------------------------------------
 * attach the image dimensions according to the interlace mode
 *-------------------------------------------------------------------------
 */
 if(strcmp(interlace, "INTERLACE_PIXEL") == 0) {
  /* Number of color planes is defined as the third dimension */
  dims[0] = height;
  dims[1] = width;
  dims[2] = IMAGE24_RANK;
 }
 else
 if(strcmp(interlace, "INTERLACE_PLANE") == 0) {
  /* Number of color planes is defined as the first dimension */
  dims[0] = IMAGE24_RANK;
  dims[1] = height;
  dims[2] = width;
 }
 else
  return -1;

/*-------------------------------------------------------------------------
 * create and write the dataset
 *-------------------------------------------------------------------------
 */

 /* create the data space for the dataset. */
 if((sid = H5Screate_simple(IMAGE24_RANK, dims, NULL)) < 0)
  return -1;

 /* create the dataset as H5T_NATIVE_UCHAR */
 if((did = H5Dcreate2(loc_id, dset_name, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
  return -1;

 /* write with memory type H5T_NATIVE_INT */
 if(buf) {
  if(sizeof(int_f) == sizeof(int)) {
      if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
       return -1;
  } else if(sizeof(int_f) == sizeof(long)) {
      if(H5Dwrite(did, H5T_NATIVE_LONG, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
       return -1;
  } else if(sizeof(int_f) == sizeof(long long)) {
      if(H5Dwrite(did, H5T_NATIVE_LLONG, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
       return -1;
  } else
   return -1;
 }

 /* close */
 if(H5Dclose(did) < 0)
  return -1;
 if(H5Sclose(sid) < 0)
  return -1;

/*-------------------------------------------------------------------------
 * attach the specification attributes
 *-------------------------------------------------------------------------
 */

 /* Attach the CLASS attribute */
 if(H5LTset_attribute_string(loc_id, dset_name, "CLASS", IMAGE_CLASS) < 0)
  return -1;

 /* Attach the VERSION attribute */
 if(H5LTset_attribute_string(loc_id, dset_name, "IMAGE_VERSION", IMAGE_VERSION) < 0)
  return -1;

 /* Attach the IMAGE_SUBCLASS attribute */
 if(H5LTset_attribute_string(loc_id, dset_name, "IMAGE_SUBCLASS", "IMAGE_TRUECOLOR") < 0)
  return -1;

 /* Attach the INTERLACE_MODE attribute. This attributes is only for true color images */
 if(H5LTset_attribute_string(loc_id, dset_name, "INTERLACE_MODE", interlace) < 0)
  return -1;

 return 0;

}


/*-------------------------------------------------------------------------
 * Function: H5IMread_imagef
 *
 * Purpose: Reads image data from disk.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente Nunes, pvn@ncsa.uiuc.edu
 *
 * Date: May 10, 2005
 *
 * Comments:
 *  This function allows reading of an 8bit image on disk.
 *  The memory datatype is H5T_NATIVE_INT. It is supposed to be called from
 *  the FORTRAN interface where the image buffer is defined as type "integer"
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5IMread_imagef(hid_t loc_id,
                        const char *dset_name,
                        int_f *buf)
{
    hid_t   did;
    hid_t   tid;

    /* open the dataset */
    if((did = H5Dopen2(loc_id, dset_name, H5P_DEFAULT)) < 0)
        return -1;

    /* determine appropriate datatype to use */
    if(sizeof(int_f) == sizeof(int))
        tid = H5T_NATIVE_INT;
    else if(sizeof(int_f) == sizeof(long))
        tid = H5T_NATIVE_LONG;
    else if(sizeof(int_f) == sizeof(long long))
        tid = H5T_NATIVE_LLONG;
    else
        goto out;

    /* read to memory */
    if(H5Dread(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        goto out;

    /* close */
    if(H5Dclose(did))
        return -1;

    return 0;

out:
    H5Dclose(did);
    return -1;
}


/*-------------------------------------------------------------------------
 * Function: H5IMmake_palettef
 *
 * Purpose: Creates and writes a palette.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente Nunes, pvn@ncsa.uiuc.edu
 *
 * Date: May 10, 2005
 *
 * Comments:
 *  This function allows writing of an 8bit palette to disk.
 *  The memory datatype is H5T_NATIVE_INT. It is supposed to be called from
 *  the FORTRAN interface where the image buffer is defined as type "integer"
 *
 *  based on HDF5 Image and Palette Specification
 *  http://hdf.ncsa.uiuc.edu/HDF5/H5Image/ImageSpec.html
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5IMmake_palettef(hid_t loc_id,
                          const char *pal_name,
                          const hsize_t *pal_dims,
                          int_f *pal_data)

{

 hid_t did;                /* dataset ID */
 hid_t sid;                /* space ID */
 int   has_pal;

 /* Check if the dataset already exists */
 has_pal = H5LTfind_dataset(loc_id, pal_name);

 /* It exists. Return */
 if(has_pal == 1)
  return 0;

/*-------------------------------------------------------------------------
 * create and write the dataset
 *-------------------------------------------------------------------------
 */

 /* create the data space for the dataset. */
 if((sid = H5Screate_simple(2, pal_dims, NULL)) < 0)
  return -1;

 /* create the dataset as H5T_NATIVE_UCHAR */
 if((did = H5Dcreate2(loc_id, pal_name, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
  return -1;

 /* write with memory type H5T_NATIVE_INT */
 if(pal_data) {
  if(sizeof(int_f) == sizeof(int)) {
      if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, pal_data) < 0)
       return -1;
  } else if(sizeof(int_f) == sizeof(long)) {
      if(H5Dwrite(did, H5T_NATIVE_LONG, H5S_ALL, H5S_ALL, H5P_DEFAULT, pal_data) < 0)
       return -1;
  } else if(sizeof(int_f) == sizeof(long long)) {
      if(H5Dwrite(did, H5T_NATIVE_LLONG, H5S_ALL, H5S_ALL, H5P_DEFAULT, pal_data) < 0)
       return -1;
  } else
      return -1;
 }

 /* close */
 if(H5Dclose(did) < 0)
  return -1;
 if(H5Sclose(sid) < 0)
  return -1;

/*-------------------------------------------------------------------------
 * attach the specification attributes
 *-------------------------------------------------------------------------
 */

 /* Attach the attribute "CLASS" to the >>palette<< dataset*/
 if(H5LTset_attribute_string(loc_id, pal_name, "CLASS", PALETTE_CLASS) < 0)
  return -1;

 /* Attach the attribute "PAL_VERSION" to the >>palette<< dataset*/
 if(H5LTset_attribute_string(loc_id, pal_name, "PAL_VERSION", "1.2") < 0)
  return -1;

 return 0;

}


/*-------------------------------------------------------------------------
 * Function: H5IMget_palettef
 *
 * Purpose: Read palette
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente Nunes, pvn@ncsa.uiuc.edu
 *
 * Date: May 10, 2005
 *
 * Comments:
 *  This function allows reading of an 8bit palette from disk.
 *  The memory datatype is H5T_NATIVE_INT. It is supposed to be called from
 *  the FORTRAN interface where the image buffer is defined as type "integer"
 *
 *  based on HDF5 Image and Palette Specification
 *  http://hdf.ncsa.uiuc.edu/HDF5/H5Image/ImageSpec.html
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5IMget_palettef(hid_t loc_id,
                         const char *image_name,
                         int pal_number,
                         int_f *pal_data)
{
 if(sizeof(int_f) == sizeof(int))
  return H5IM_get_palette(loc_id,image_name,pal_number,H5T_NATIVE_INT,pal_data);
 else if(sizeof(int_f) == sizeof(long))
  return H5IM_get_palette(loc_id,image_name,pal_number,H5T_NATIVE_LONG,pal_data);
 else if(sizeof(int_f) == sizeof(long long))
  return H5IM_get_palette(loc_id,image_name,pal_number,H5T_NATIVE_LLONG,pal_data);
 else
  return -1;

}

/*-------------------------------------------------------------------------
 * Function: H5IM_get_palette
 *
 * Purpose: private function that reads a palette to memory type TID
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente Nunes, pvn@ncsa.uiuc.edu
 *
 * Date: May 10, 2005
 *
 * Comments:
 *  This function allows reading of an 8bit palette from disk disk
 *   to memory type TID
 *  The memory datatype can be H5T_NATIVE_INT or H5T_NATIVE_UCHAR currently.
 *   the H5T_NATIVE_INT is supposed to be called from
 *   the FORTRAN interface where the image buffer is defined as type "integer"
 *
 * Comments:
 *  based on HDF5 Image and Palette Specification
 *  http://hdf.ncsa.uiuc.edu/HDF5/H5Image/ImageSpec.html
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t H5IM_get_palette(hid_t loc_id,
                         const char *image_name,
                         int pal_number,
                         hid_t tid,
                         void *pal_data)
{
 hid_t      image_id;
 int        has_pal;
 hid_t      attr_type;
 hid_t      attr_id;
 hid_t      attr_space_id;
 hid_t      attr_class;
 hssize_t   n_refs;
 hsize_t    dim_ref;
 hobj_ref_t *refbuf;     /* buffer to read references */
 hid_t      pal_id;

 /* Open the dataset. */
 if((image_id = H5Dopen2(loc_id, image_name, H5P_DEFAULT)) < 0)
  return -1;

 /* Try to find the attribute "PALETTE" on the >>image<< dataset */
 has_pal = H5IM_find_palette(image_id);

 if(has_pal ==  1)
 {

  if((attr_id = H5Aopen(image_id, "PALETTE", H5P_DEFAULT)) < 0)
   goto out;

  if((attr_type = H5Aget_type(attr_id)) < 0)
   goto out;

  if((attr_class = H5Tget_class(attr_type)) < 0)
   goto out;

  /* Check if it is really a reference */
  if(attr_class == H5T_REFERENCE)
  {

   /* Get the reference(s) */
   if((attr_space_id = H5Aget_space(attr_id)) < 0)
    goto out;

   n_refs = H5Sget_simple_extent_npoints(attr_space_id);

   dim_ref = n_refs;

   refbuf = malloc(sizeof(hobj_ref_t) * (int)dim_ref);

   if(H5Aread(attr_id, attr_type, refbuf) < 0)
    goto out;

   /* Get the palette id */
   if((pal_id = H5Rdereference(image_id, H5R_OBJECT, &refbuf[pal_number])) < 0)
    goto out;

   /* Read the palette dataset using the memory type TID */
   if(H5Dread(pal_id, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, pal_data) < 0)
    goto out;

   if(H5Sclose(attr_space_id) < 0)
    goto out;

   /* close the dereferenced dataset */
   if(H5Dclose(pal_id) < 0)
    goto out;

   free(refbuf);

  } /* H5T_REFERENCE */

  if(H5Tclose(attr_type) < 0)
   goto out;

  /* Close the attribute. */
  if(H5Aclose(attr_id) < 0)
   goto out;

 }

 /* Close the image dataset. */
 if(H5Dclose(image_id) < 0)
  return -1;

 return 0;

out:
 H5Dclose(image_id);
 return -1;


}
