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

#include <stdio.h>
#include <assert.h>
#include "gif.h"
#include "H5IMpublic.h"
#include "h5tools_utils.h"



int EndianOrder;

static void
putword(int w, FILE *fp)
{
    /* writes a 16-bit integer in GIF order (LSB first) */
    fputc(w &0xff, fp);
    fputc((w>>8)&0xff,fp);
}

static void
usage(void)
{
    printf("Usage: h52gif <h5_file> <gif_file> -i <h5_image>\n");
    fprintf(stdout, "       h52gif -V \n");
    fprintf(stdout, "        Print HDF5 library version and exit\n");
    printf("h52gif expects *at least* one h5_image.\n");

}

FILE *fpGif = NULL;
int main(int argc , char **argv)
{
    BYTE *Image;

    /* compression structs */
    CHAR *HDFName = NULL;
    CHAR *GIFName = NULL;

    BYTE* b;

    BYTE  GlobalPalette[256][3];
    BYTE  Red[256];
    BYTE  Green[256];
    BYTE  Blue[256];

    int   RWidth, RHeight;
    int   ColorMapSize, InitCodeSize, Background, BitsPerPixel;
    int   j,nc;
    int   i;
    int   numcols;

    BYTE pc2nc[256] , r1[256] , g1[256] , b1[256];

    int arg_index = 2;
    int bool_is_image = 0; /* 0 = false , 1 = true */
    char *image_name = NULL;
    int idx;

    if ( argv[1] && (strcmp("-V",argv[1])==0) )
    {
        print_version("gif2h5");
        exit(EXIT_SUCCESS);

    }


    if (argc < 4)
    {
        /* they didn't supply at least one image -- bail */
        usage();
        return EXIT_FAILURE;
    }

    HDFName = argv[1];
    GIFName = argv[2];

    /* get the options */
    while (arg_index++ < argc - 1)
    {
        if (!strcmp(argv[arg_index] , "-i")) {
            bool_is_image = 1;
            continue;
        }

        if (bool_is_image)
        {
            /* allocate space to store the image name */
            size_t len = strlen(argv[arg_index]);
            image_name = (CHAR*) malloc( len + 1);
            strcpy(image_name , argv[arg_index]);

            bool_is_image = 0;
            continue;
        }

        /* oops. This was not meant to happen */
        usage();

        goto out;
    }

   /* Do Endian Order testing and set Endian Order */
    idx = 0x0001;
    b = (BYTE *) &idx;
    EndianOrder = (b[0] ? 1:0);

    if (!(fpGif = fopen(GIFName , "wb")))
    {
        printf("Error opening gif file for output. Aborting.\n");
        goto out;
    }

    Background = 0;
    {
        hsize_t       width, height, planes;
        hid_t         fid;
        char          interlace[20];
        hssize_t      npals;
        hsize_t       pal_dims[2];
        unsigned char *pal;

        if ((fid = H5Fopen(HDFName , H5F_ACC_RDONLY , H5P_DEFAULT)) < 0)
        {
            fprintf(stderr , "Unable to open HDF file for input. Aborting.\n");
            goto out;
        }

        /* read image */
        if ( H5IMget_image_info( fid, image_name, &width, &height, &planes, interlace, &npals ) < 0 )
            goto out;

        Image = (BYTE*) malloc( (size_t) width * (size_t) height );

        if ( H5IMread_image( fid, image_name, Image ) < 0 )
            goto out;

        if (npals)
        {
            if ( H5IMget_palette_info( fid, image_name, 0, pal_dims ) < 0 )
                goto out;

            pal = (BYTE*) malloc( (size_t) pal_dims[0] * (size_t) pal_dims[1] );

            if ( H5IMget_palette( fid, image_name, 0, pal ) < 0 )
                goto out;

            numcols = (int) pal_dims[0];

            for (i = 0, j = 0 ; i < numcols ; j+=3, i++)
            {
                GlobalPalette[i][0] = pal[j];
                GlobalPalette[i][1] = pal[j+1];
                GlobalPalette[i][2] = pal[j+2];
            }

            free(pal);
        }

        H5Fclose(fid);

        RWidth  = (int)width;
        RHeight = (int)height;


        /*
         * If the first image does not have a palette, I make my own global
         * color table Obviously this is not the best thing to do, better
         * steps would be:
         *
         *   1. Check for either a global palette or a global attribute called
         *      palette
         *   2. Check for palettes in any of the other images.
         */
        if (!npals)
        {
            numcols = 256;
            for (i = 0 ; i < numcols ; i++)
            {
                Red[i] = 255 - i;
                Green[i] = 255 - i;
                Blue[i] = 255 - i;
            }
        }
        else
        {
            for (i = 0 ; i < numcols ; i++)
            {
                Red[i] = GlobalPalette[i][0];
                Green[i] = GlobalPalette[i][1];
                Blue[i] = GlobalPalette[i][2];
            }
        }

        for (i = 0; i < numcols; i++)
        {
            pc2nc[i] = r1[i] = g1[i] = b1[i] = 0;
        }

        /* compute number of unique colors */
        nc = 0;

        for (i = 0; i < numcols; i++)
        {
            /* see if color #i is already used */
            for (j = 0; j < i; j++)
            {
                if (Red[i] == Red[j] && Green[i] == Green[j] && Blue[i] == Blue[j])
                    break;
            }

            if (j==i)
            {
                /* wasn't found */
                pc2nc[i] = nc;
                r1[nc] = Red[i];
                g1[nc] = Green[i];
                b1[nc] = Blue[i];
                nc++;
            }
            else
            {
                pc2nc[i] = pc2nc[j];
            }
        }

        /* figure out 'BitsPerPixel' */
        for (i = 1; i < 8; i++)
        {
            if ((1<<i) >= nc)
                break;
        }

        BitsPerPixel = i;
        ColorMapSize = 1 << BitsPerPixel;

        if (BitsPerPixel <= 1)
            InitCodeSize = 2;
        else
            InitCodeSize = BitsPerPixel;

        if (!fpGif)
        {
            fprintf(stderr,  "WriteGIF: file not open for writing\n" );
            goto out;
        }


        fwrite("GIF87a", sizeof( char ), 6, fpGif);  /* the GIF magic number */

        putword(RWidth, fpGif);             /* screen descriptor */
        putword(RHeight, fpGif);

        i = 0x00;                   /* No, there is no color map */
        i |= (8-1)<<4;              /* OR in the color resolution (hardwired 8) */
        i |= (BitsPerPixel - 1);    /* OR in the # of bits per pixel */
        fputc(i,fpGif);

        fputc(Background,fpGif);    /* background color */
        fputc(0, fpGif);            /* future expansion byte */


        /*
         * Put Image Descriptor
         * Hardwiring Left Offset and Top Offset to 0x00
         */
        fputc(0x2c , fpGif);
        putword(0x00 , fpGif);
        putword(0x00  , fpGif);
        putword(RWidth   , fpGif);
        putword(RHeight  , fpGif);

        /* since we always have a local color palette ... */
        fputc((0x80 | (BitsPerPixel - 1)) , fpGif);

        for (i = 0; i < ColorMapSize; i++)
        {
            /* write out Global colormap */
            fputc(r1[i], fpGif);
            fputc(g1[i], fpGif);
            fputc(b1[i], fpGif);
        }

        fputc(InitCodeSize , fpGif);

        i = hdfWriteGIF(fpGif , Image , 0 , RHeight , RWidth , r1, g1 , b1 , pc2nc , 256 , 8 , BitsPerPixel);
        fputc(0x00, fpGif);
        free(Image);
    }

    if (fputc(';',fpGif) == EOF)
    {
        /* Write GIF file terminator */
        fprintf(stderr , "Error!");
        goto out;
    }

    if (fpGif != NULL)
        fclose(fpGif);
    if (image_name != NULL)
        free(image_name);

    return EXIT_SUCCESS;


out:

    if (fpGif != NULL)
        fclose(fpGif);
    if (image_name != NULL)
        free(image_name);

    return EXIT_FAILURE;
}
