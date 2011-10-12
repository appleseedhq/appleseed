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

#ifndef _H5IMCC_H
#define _H5IMCC_H

#include "H5LTprivate.h"
#include "H5IMprivate.h"
#include "H5f90i_gen.h"

#ifdef __cplusplus
extern "C" {
#endif


herr_t H5IMmake_image_8bitf( hid_t loc_id,
                             const char *dset_name,
                             hsize_t width,
                             hsize_t height,
                             int_f *buf );

herr_t H5IMmake_image_24bitf( hid_t loc_id,
                              const char *dset_name,
                              hsize_t width,
                              hsize_t height,
                              const char *interlace,
                              int_f *buf);

herr_t H5IMread_imagef( hid_t loc_id,
                        const char *dset_name,
                        int_f *buf );

herr_t H5IMmake_palettef( hid_t loc_id,
                          const char *pal_name,
                          const hsize_t *pal_dims,
                          int_f *pal_data );

herr_t H5IMget_palettef( hid_t loc_id,
                         const char *image_name,
                         int pal_number,
                         int_f *pal_data );


#ifdef __cplusplus
}
#endif

#endif
