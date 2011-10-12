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
#include <string.h>
#include <stdlib.h>
#include <string.h>

#include "gif.h"
#include "H5IMpublic.h"

#define PAL_NAME "global"

/*-------------------------------------------------------------------------
 * Function: WriteHDF
 *
 * Purpose: Write the GIF image with the HDF5 Image API
 *
 * Programmer: Unknown
 *
 * Modifications:  pvn
 *   Use the HDF5 IMAGE API to write the HDF5 image and pallete
 *
 * Date: January, 31, 2006
 *
 *-------------------------------------------------------------------------
 */

int
WriteHDF(GIFTOMEM GifMemoryStruct, char *HDFName)
{
    GIFHEAD          gifHead;           /* GIF Header structure            */
    GIFIMAGEDESC    *gifImageDesc;      /* Logical Image Descriptor struct */
    int              has_pal=0;

    long ImageCount;                    /* number of images */
#ifdef UNUSED
    long CommentCount,                  /* number of comments */
         ApplicationCount,              /* number of application extensions */
         PlainTextCount;                /* number of plain text extensions */
#endif /* UNUSED */

    char ImageName[256];                /* Image name for the Image */

    /* H5 variables */
    hid_t file_id;      /* H5 file id */

    /* temp counter */
    int i;

    /* get the GIFMem stuff */
    gifHead = *(GifMemoryStruct.GifHeader);

    /* get some data from gifHead */
    ImageCount = gifHead.ImageCount;
#ifdef UNUSED
    CommentCount = (WORD)gifHead.CommentCount;
    ApplicationCount = (WORD)gifHead.ApplicationCount;
    PlainTextCount = (WORD)gifHead.PlainTextCount;
#endif /* UNUSED */

    if ((file_id = H5Fcreate(HDFName , H5F_ACC_TRUNC , H5P_DEFAULT , H5P_DEFAULT)) < 0) {
        /* error occured opening the HDF File for write */
        fprintf(stderr , "HDF file could not be opened for writing\n");
        fprintf(stderr , "NOTE: GIF file must be present in the same directory as the binary on UNIX systems.\n");
        exit(1);
    }

    /* first create the global palette if there is one */
    if (gifHead.PackedField & 0x80) { /* global palette exists */
        hsize_t dims[2]; /* specify the dimensions of the palette */

        /* size of the palette is tablesize (rows) X 3 (columns) */
        dims[0] = gifHead.TableSize;
        dims[1] = 3;

        /* make a palette */
        if (H5IMmake_palette(file_id,PAL_NAME,dims,(unsigned char *)gifHead.HDFPalette)<0)
         return -1;

        has_pal=1;
    }

    for(i = 0; i < ImageCount; i++) {
        hsize_t dims[2];        /* dimensions for the dataset */
        /* get the gifImageDesc */
        gifImageDesc = GifMemoryStruct.GifImageDesc[i];

        /* set the dimensions */
        dims[0] = gifImageDesc->ImageHeight;
        dims[1] = gifImageDesc->ImageWidth;

        /* create the image name */
        sprintf(ImageName , "Image%d" , i);

        /* write image */
        if (H5IMmake_image_8bit(file_id,ImageName,dims[1],dims[0],(gifImageDesc->Image))<0)
         return -1;

        /* attach the palette to the image dataset */
        if (has_pal)
        {
            if (H5IMlink_palette(file_id,ImageName,PAL_NAME)<0)
                return -1;
        }
    }

    /* close the H5 file */
    if (H5Fclose(file_id) < 0) {
        fprintf(stderr , "Could not close HDF5 file. Aborting...\n");
        return -1;
    }

    return 0;
}

