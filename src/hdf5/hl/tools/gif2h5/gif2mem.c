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

/*
 * This file contains snippets of code from James Murray's original file to
 * display the GIF header information, but most of it has been modified to
 * suit gif2hdf
 */

/****************************************************************************\
**  Title:       GIFHEAD.C                                                  **
**  Purpose:     Display the data in a GIF image file.                      **
**  Version:     1.0                                                        **
**  Date:        March 1992                                                 **
**  Author:      James D. Murray, Anaheim, CA, USA                          **
**  C Compilers: Borland C++ v2.0, Microsoft C v6.00a                       **
**                                                                          **
**  GIFHEAD displays all real information contained within a GIF image      **
**  file, including all color tables and extension block information.       **
**  GIFHEAD reads both GIF 87a abd 89a-format files.                        **
**                                                                          **
**  Copyright (C) 1991-92 by Graphics Software Labs.  All rights reserved.  **
\****************************************************************************/
#include <stdio.h>

#include "gif.h"

#define VERSION                     "1.00"

GIFTOMEM
Gif2Mem(BYTE *MemGif)
{
    /*
     * The gif structure outline for passing data to memory is given in gif.h.
     * These pointers are redunant, should take them out in ver. 2
     */
    GIFHEAD            *gifHead;           /* GIF Header structure            */
    GIFIMAGEDESC      **gifImageDesc;      /* Logical Image Descriptor struct */
    GIFPLAINTEXT      **gifPlainText;      /* Plain Text Extension structure  */
    GIFAPPLICATION    **gifApplication;    /* Application Extension structure */
    GIFCOMMENT        **gifComment;        /* Comment Extension structure     */
    GIFGRAPHICCONTROL **gifGraphicControl; /* Graphic Control Extension strct */

    GIFTOMEM GifMemoryStruct;

    register WORD i;        /* Loop counter                                 */
    BYTE Identifier;        /* Extension block identifier holder            */
    BYTE Label;             /* Extension block label holder                 */
    BYTE ImageCount;        /* Count of the number of images in the file    */
    BYTE ImageArray;        /* Keep the size of the array to store Images   */
    BYTE CommentCount;
    BYTE CommentArray;
    BYTE ApplicationCount;
    BYTE ApplicationArray;
    BYTE PlainTextCount;
    BYTE PlainTextArray;
    BYTE GCEflag;
    BYTE aTemp;
    BYTE j;
    BYTE w;                 /* Two more variables needed only while testing */
    BYTE *b;                /* Endian Ordering                              */

    /* Allocate memory for the GIF structures           */
    /* Plug the structs into GifMemoryStruct at the end */
    /****************************************************/
    if (!(gifHead = (GIFHEAD *)malloc(sizeof(GIFHEAD)))) {
        printf("Could not allocate memory for gifHead\n");
        exit(EXIT_FAILURE);
    }

    /*
     * The next three have to grow dynamically so we leave them for now and
     * let realloc handle it later on.
     */
    gifImageDesc      = NULL;
    gifPlainText      = NULL;
    gifGraphicControl = NULL;
    gifComment        = NULL;
    gifApplication    = NULL;

    /******************************/
    /* Memory allocation complete */
    /******************************/

    /* Carry out Endian Testing and set Endian Order */
    w = 0x0001;
    b = (BYTE *) &w;
    EndianOrder = (b[0] ? 1 : 0);

    /* Read the GIF image file header information */
    ReadGifHeader(gifHead, &MemGif);

    /* Check for FILE stream error */
#if 0
    if (ferror(fpGif))
    {
        fputs("GIFHEAD: Error reading header information!\n", stderr);
        exit(EXIT_FAILURE);
    }
#endif  /* 0 */

    /*
     * Identify, read, and display block information.
     */
    ImageCount       = ImageArray       = 0;
    CommentCount     = CommentArray     = 0;
    ApplicationCount = ApplicationArray = 0;
    PlainTextCount   = PlainTextArray   = 0;
    GCEflag          = 0;

    for (;;) {
        Identifier = *MemGif++;

        switch (Identifier) {
            case 0x3B:  /* Trailer */
                /*
                 * The counts are stored to make it easier while putting stuff
                 * into the HDF file and then deallocating space.
                 */
                gifHead->ImageCount       = ImageCount;
                gifHead->CommentCount     = CommentCount;
                gifHead->ApplicationCount = ApplicationCount;
                gifHead->PlainTextCount   = PlainTextCount;

                /* putting stuff into the gif2mem structure */
                GifMemoryStruct.GifHeader = gifHead;
                GifMemoryStruct.GifImageDesc = gifImageDesc;
                GifMemoryStruct.GifPlainTextExtension = gifPlainText;
                GifMemoryStruct.GifApplicationExtension = gifApplication;
                GifMemoryStruct.GifCommentExtension = gifComment;
                GifMemoryStruct.GifGraphicControlExtension = gifGraphicControl;

                /* return the struct */
                return GifMemoryStruct;

            case 0x2C:  /* Image Descriptor */
                /*
                 * If there was no image descriptor before this increase image
                 * count. If an imagedescriptor was present, reset GCEflag
                 */
                if (GCEflag == 0)
                    ImageCount++;
                else
                    GCEflag = 0;

                if (ImageCount > ImageArray) {
                    aTemp = ImageArray;
                    ImageArray = (ImageArray << 1) + 1;
                    if (!(gifImageDesc = (GIFIMAGEDESC **)realloc(gifImageDesc,
                                            sizeof(GIFIMAGEDESC *) * ImageArray))) {
                        printf("Out of memory!");
                        exit(EXIT_FAILURE);
                    }

                    if (!(gifGraphicControl = (GIFGRAPHICCONTROL **)realloc(
                                        gifGraphicControl,
                                        sizeof(GIFGRAPHICCONTROL *) * ImageArray))) {
                        printf("Out of memory!");
                        exit(EXIT_FAILURE);
                    }

                    for (j = aTemp ; j < ImageArray ; j++) {
                        gifGraphicControl[j] = NULL;
                        gifImageDesc[j] = NULL;
                    }
                }

                if(!(gifImageDesc[ImageCount-1] = (GIFIMAGEDESC*)malloc(sizeof(GIFIMAGEDESC)))) {
                    printf("Out of memory!");
                    exit(EXIT_FAILURE);
                }


                if (ReadGifImageDesc(gifImageDesc[ImageCount-1], &MemGif) == -1)
                    fputs("Error reading Image Descriptor information\n", stderr);

                /* Decompress the Image */
                gifImageDesc[ImageCount-1]->Image = Decompress(gifImageDesc[ImageCount-1],
                                                               gifHead);
                free(gifImageDesc[ImageCount-1]->GIFImage);

                /*
                 * Convert the local palette into an HDF compatible palette In
                 * case the local color table is present, it is written out as
                 * the HDFPalette If it is absent the global table is written
                 * as the HDFPalette.
                 */
                if (!((gifImageDesc[ImageCount-1]->PackedField) & 0x80)) {
                    /* Check to see if the global color table exists.... */
                    if (gifHead->PackedField & 0x80) {
                        for (i=0 ; i<gifHead->TableSize ; i++) {
                            gifImageDesc[ImageCount-1]->HDFPalette[i][0] =
                                                    gifHead->HDFPalette[i][0];
                            gifImageDesc[ImageCount-1]->HDFPalette[i][1] =
                                                    gifHead->HDFPalette[i][1];
                            gifImageDesc[ImageCount-1]->HDFPalette[i][2] =
                                                    gifHead->HDFPalette[i][2];
                        }
                    }

                    gifImageDesc[ImageCount-1]->TableSize = gifHead->TableSize;
                }

                break;

            case 0x21:  /* Extension Block */
                    Label = *MemGif++;

                    switch (Label) {
                    case 0x01:  /* Plain Text Extension */
                        puts("Plain Text Extension\n");
                        PlainTextCount++;

                        if (PlainTextCount > PlainTextArray)
                                PlainTextArray = (PlainTextArray << 1) + 1;

                        if (!(gifPlainText = (GIFPLAINTEXT **)realloc(gifPlainText , sizeof(GIFPLAINTEXT *) * PlainTextArray))) {
                            printf("Out of memory!");
                            exit(EXIT_FAILURE);
                        }

                        if(!(gifPlainText[PlainTextCount - 1] = (GIFPLAINTEXT*)malloc(sizeof(GIFPLAINTEXT)))) {
                            printf("Out of memory!");
                            exit(EXIT_FAILURE);
                        }

                        if (ReadGifPlainText(gifPlainText[PlainTextCount - 1], &MemGif))
                            fprintf(stderr,
                                    "Error reading Plain Text Extension information.\n");

                        break;

                    case 0xFE:  /* Comment Extension */
                        CommentCount++;

                        if (CommentCount > CommentArray)
                            CommentArray = (CommentArray << 1) + 1;

                        if (!(gifComment = (GIFCOMMENT **)realloc(gifComment , sizeof(GIFCOMMENT *) * CommentArray))) {
                            printf("Out of memory!");
                            exit(EXIT_FAILURE);
                        }

                        if(!(gifComment[CommentCount - 1] = (GIFCOMMENT *)malloc(sizeof(GIFCOMMENT)))) {
                            printf("Out of memory!");
                            exit(EXIT_FAILURE);
                        }


                        if (ReadGifComment(gifComment[CommentCount - 1], &MemGif))
                            fprintf(stderr,
                                    "Error reading Comment Extension information\n");

                        break;

                    case 0xF9:  /* Graphic Control Extension */
                        if (GCEflag == 0 )
                            ImageCount++;

                        GCEflag = 1;

                        if (ImageCount > ImageArray) {
                            aTemp = ImageArray;
                            ImageArray = (ImageArray << 1) + 1;

                            if (!(gifGraphicControl = (GIFGRAPHICCONTROL **)realloc(gifGraphicControl , sizeof(GIFGRAPHICCONTROL *) * ImageArray))) {
                                printf("Out of memory!");
                                exit(EXIT_FAILURE);
                            }

                            if (!(gifImageDesc = (GIFIMAGEDESC **)realloc(gifImageDesc , sizeof(GIFIMAGEDESC *) * ImageArray))) {
                                printf("Out of memory!");
                                exit(EXIT_FAILURE);
                            }

                            for (j = aTemp ; j < ImageArray ; j++) {
                                gifGraphicControl[j] = NULL;
                                gifImageDesc[j]      = NULL;
                            }
                        }

                        if(!(gifGraphicControl[ImageCount-1] = (GIFGRAPHICCONTROL*)malloc(sizeof(GIFGRAPHICCONTROL)))) {
                            printf("Out of memory!");
                            exit(EXIT_FAILURE);
                        }


                        if (ReadGifGraphicControl(gifGraphicControl[ImageCount-1], &MemGif))
                            fprintf(stderr,
                                    "Error reading Graphic Control Extension information\n");

                        if (!*MemGif++ == 0)
                            fprintf(stderr,
                                    "Error reading Graphic Control Extension\n");

                        break;

                    case 0xFF:  /* Application Extension */
                        ApplicationCount++;

                        if (ApplicationCount > ApplicationArray)
                            ApplicationArray = (ApplicationArray << 1) + 1;

                        if (!(gifApplication = (GIFAPPLICATION **)realloc(gifApplication , sizeof(GIFAPPLICATION *) * ApplicationArray))) {
                            printf("Out of memory!");
                            exit(EXIT_FAILURE);
                        }

                        if(!(gifApplication[ApplicationCount - 1] = (GIFAPPLICATION *)malloc(sizeof(GIFAPPLICATION)))) {
                            printf("Out of memory!");
                            exit(EXIT_FAILURE);
                        }


                        if (ReadGifApplication(gifApplication[ApplicationCount - 1], &MemGif))
                            fprintf(stderr,
                                    "Error reading Application Extension information\n");

                        break;

                    default:
                        printf("Unknown Extension Label: 0x%02x\n", Label);
                        break;
                    }

                    break;

            default:
                fprintf(stderr,
                        "Unknown Block Separator Character: 0x%02x\n", Identifier);
        }
    }
}
