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

#include "hdf5.h"
#include "hdf5_hl.h"

#define WIDTH         400
#define HEIGHT        200
#define PAL_ENTRIES   9
unsigned char buf [ WIDTH*HEIGHT ];

int main( void )
{
 hid_t         file_id;
 herr_t        status;
 hsize_t       pal_dims[] = {PAL_ENTRIES,3};
 size_t        i, j;
 int           n, space;
 unsigned char pal[PAL_ENTRIES*3] = {  /* create a palette with 9 colors */
 0,0,168,      /* dark blue */
 0,0,252,      /* blue */
 0,168,252,    /* ocean blue */
 84,252,252,   /* light blue */
 168,252,168,  /* light green */
 0,252,168,    /* green */
 252,252,84,   /* yellow */
 252,168,0,    /* orange */
 252,0,0};     /* red */

 /* create an image of 9 values divided evenly by the array */
 space = WIDTH*HEIGHT / PAL_ENTRIES;
 for (i=0, j=0, n=0; i < WIDTH*HEIGHT; i++, j++ )
 {
  buf[i] = n;
  if ( j > space )
  {
   n++;
   j=0;
  }
  if (n>PAL_ENTRIES-1) n=0;
 }

 /* create a new HDF5 file using default properties. */
 file_id = H5Fcreate( "ex_image1.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT );

 /* make the image */
 status = H5IMmake_image_8bit( file_id, "image1", (hsize_t)WIDTH, (hsize_t)HEIGHT, buf );

 /* make a palette */
 status = H5IMmake_palette( file_id, "pallete", pal_dims, pal );

 /* attach the palette to the image */
 status = H5IMlink_palette( file_id, "image1", "pallete" );

 /* close the file. */
 status = H5Fclose( file_id );

 return 0;

}
