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
#include <stdlib.h>
#include <string.h>

#define DATA_FILE1   "image8.txt"
#define DATA_FILE2   "image24pixel.txt"
#define IMAGE1_NAME  "image8bit"
#define IMAGE2_NAME  "image24bitpixel"
#define PAL_NAME     "palette"
#define PAL_ENTRIES  256

static int    read_data(const char* file_name, hsize_t *width, hsize_t *height );
unsigned char *gbuf = 0;  /* global buffer for image data */

int main( void )
{
 hid_t          file_id;                       /* HDF5 file identifier */
 hsize_t        width;                         /* width of image */
 hsize_t        height;                        /* height of image */
 unsigned char  pal[ PAL_ENTRIES * 3 ];        /* palette array */
 hsize_t        pal_dims[2] = {PAL_ENTRIES,3}; /* palette dimensions */
 herr_t         status, i, n;

 /* create a new HDF5 file using default properties. */
 file_id = H5Fcreate( "ex_image2.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT );

 /* read first data file */
 if (read_data(DATA_FILE1,&width,&height)<0)
  goto out;

 /* make the image */
 status=H5IMmake_image_8bit( file_id, IMAGE1_NAME, width, height, gbuf );

/*-------------------------------------------------------------------------
 * define a palette, blue to red tones
 *-------------------------------------------------------------------------
 */
 for ( i=0, n=0; i<PAL_ENTRIES*3; i+=3, n++)
 {
  pal[i]  =n;      /* red */
  pal[i+1]=0;      /* green */
  pal[i+2]=255-n;  /* blue */
 }

 /* make a palette */
 status=H5IMmake_palette( file_id, PAL_NAME, pal_dims, pal );

 /* attach the palette to the image */
 status=H5IMlink_palette( file_id, IMAGE1_NAME, PAL_NAME );

/*-------------------------------------------------------------------------
 * True color image example with pixel interlace
 *-------------------------------------------------------------------------
 */

 /* read second data file */
 if (read_data(DATA_FILE2,&width,&height)<0)
  goto out;

 /* make dataset */
 status=H5IMmake_image_24bit( file_id, IMAGE2_NAME, width, height, "INTERLACE_PIXEL", gbuf );

 /* close the file. */
 H5Fclose( file_id );

 return 0;

out:
 printf("Error on return function...Exiting\n");
 return 1;
}


/*-------------------------------------------------------------------------
 * read_data
 * utility function to read ASCII image data
 * the files have a header of the type
 *
 *   components
 *   n
 *   height
 *   n
 *   width
 *   n
 *
 * followed by the image data
 *
 *-------------------------------------------------------------------------
 */

static int read_data( const char* fname, /*IN*/
                      hsize_t *width, /*OUT*/
                      hsize_t *height /*OUT*/ )
{
 int    i, n;
 int    color_planes;
 char   str[20];
 FILE   *f;
 int    w, h;
 char   *srcdir = getenv("srcdir"); /* the source directory */
 char   data_file[512]="";          /* buffer to hold name of existing data file */

/*-------------------------------------------------------------------------
 * compose the name of the file to open, using "srcdir", if appropriate
 *-------------------------------------------------------------------------
 */
 strcpy(data_file, "");
 if (srcdir)
 {
  strcpy(data_file, srcdir);
  strcat(data_file, "/");
 }
 strcat(data_file,fname);

/*-------------------------------------------------------------------------
 * read
 *-------------------------------------------------------------------------
 */

 f = fopen(data_file, "r");
 if ( f == NULL )
 {
  printf( "Could not open file %s. Try set $srcdir \n", data_file );
  return -1;
 }

 fscanf( f, "%s", str );
 fscanf( f, "%d", &color_planes );
 fscanf( f, "%s", str );
 fscanf( f, "%d", &h);
 fscanf( f, "%s", str );
 fscanf( f, "%d", &w);

 *width = (hsize_t)w;
 *height = (hsize_t)h;

 if ( gbuf )
 {
  free( gbuf );
  gbuf=NULL;
 }

 gbuf = (unsigned char*) malloc (w * h * color_planes * sizeof( unsigned char ));

 for (i = 0; i < h * w * color_planes ; i++)
 {
  fscanf( f, "%d",&n );
  gbuf[i] = (unsigned char)n;
 }
 fclose(f);

 return 1;

}

