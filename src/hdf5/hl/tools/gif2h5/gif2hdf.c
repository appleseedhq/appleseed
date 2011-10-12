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
#include <stdio.h>
#include <stdlib.h>

#include "gif.h"
#include "h5tools_utils.h"


int
main(int argv , char *argc[])
{
    GIFTOMEM GifMemoryStruct;
    GIFIMAGEDESC gifImageDesc;

    FILE *fpGif;

    /* replacing int32 with long */
    long i, ImageCount;
    long filesize;

    BYTE *MemGif;
    BYTE *StartPos;

    char *GIFFileName;
    char *HDFFileName;

    /*
     * Initialize all GifMemoryStruct pointers to null to prevent hassles
     * later on
     */
    GifMemoryStruct.GifHeader                  = NULL;
    GifMemoryStruct.GifImageDesc               = NULL;
    GifMemoryStruct.GifGraphicControlExtension = NULL;
    GifMemoryStruct.GifPlainTextExtension      = NULL;
    GifMemoryStruct.GifApplicationExtension    = NULL;
    GifMemoryStruct.GifCommentExtension        = NULL;

    if ( argc[1] && (strcmp("-V",argc[1])==0) )
    {
        print_version("gif2h5");
        exit(EXIT_SUCCESS);

    }

    if (argv < 3) {
        printf("Usage: gif2h5 <GIFFILE> <HDFFILE>\n");
        fprintf(stdout, "       gif2h5 -V \n");
        fprintf(stdout, "        Print HDF5 library version and exit\n");
        exit(EXIT_FAILURE);
    }

    GIFFileName = argc[1];
    HDFFileName = argc[2];

    if (!(fpGif = fopen(GIFFileName,"rb"))) {
        printf("Unable to open GIF file for reading.\n");
        exit(EXIT_FAILURE);
    }

    /* Get the whole file into memory. Mem's much faster than I/O */
    fseek(fpGif, 0L , 2);
    filesize = ftell(fpGif);
    fseek(fpGif, 0L , 0);

    if (filesize == 0)
        printf("File Size Zero");

    if (!(MemGif = StartPos = (BYTE *)malloc((size_t)filesize))) {
        printf("Out of memory");
        exit(EXIT_FAILURE);
    }

    if (fread(MemGif,(size_t)filesize,1,fpGif) != 1) {
        printf("Corrupted Input File");
        exit(EXIT_FAILURE);
    }

    fseek(fpGif,0L,0);

    /*
     * Call Gif2Mem and break the whole file into parts.  Gif2Mem also calls
     * decompresses the images so we don't have to worry about that
     */
    GifMemoryStruct = Gif2Mem(MemGif);

    if (ferror(fpGif)) {
            printf("File Stream Error\n\n");
            exit(EXIT_FAILURE);
    }

    fclose(fpGif);

    /*
     * Call WriteHDF from here. Go ahead and change WriteHDF to write whatever
     * format you want
     */
    if (WriteHDF(GifMemoryStruct , HDFFileName))
        printf("HDF Write Error\n\n");

    /* Free all buffers */
    /* replacing int32 with long */
    ImageCount = (long)((GifMemoryStruct.GifHeader)->ImageCount);

    for(i = 0; i < ImageCount ; i++) {
        gifImageDesc = *(GifMemoryStruct.GifImageDesc[i]);

        if (gifImageDesc.Image != NULL)
            free(gifImageDesc.Image);

        if (GifMemoryStruct.GifImageDesc[i] != NULL)
        {
            free(GifMemoryStruct.GifImageDesc[i]);
            GifMemoryStruct.GifImageDesc[i] = NULL;
        }

        if (GifMemoryStruct.GifGraphicControlExtension[i] != NULL)
        {
            free(GifMemoryStruct.GifGraphicControlExtension[i]);
            GifMemoryStruct.GifGraphicControlExtension[i] = NULL;
        }
    }

    free(StartPos);
    if (GifMemoryStruct.GifHeader != NULL)
    {
        free(GifMemoryStruct.GifHeader);
        GifMemoryStruct.GifHeader = NULL;
    }
    if (GifMemoryStruct.GifApplicationExtension != NULL)
    {
        free(GifMemoryStruct.GifApplicationExtension);
        GifMemoryStruct.GifApplicationExtension = NULL;
    }
    if (GifMemoryStruct.GifImageDesc != NULL)
    {
        free(GifMemoryStruct.GifImageDesc);
        GifMemoryStruct.GifImageDesc = NULL;
    }
    if (GifMemoryStruct.GifGraphicControlExtension != NULL)
    {
        free(GifMemoryStruct.GifGraphicControlExtension);
        GifMemoryStruct.GifGraphicControlExtension = NULL;
    }


    return EXIT_SUCCESS;
}
