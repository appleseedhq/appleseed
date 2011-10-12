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
 *  Title:       GIF.H
 *  Purpose:     GIF Header file
 */
#ifndef GIF_H_
#define GIF_H_   1

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hdf5.h"

#define MAX_PAL 768

/* typedef H5T_NATIVE_UINT8  BYTE; */
typedef unsigned char BYTE;

/* typedef H5T_NATIVE_UINT16  WORD; */
typedef unsigned long WORD;

typedef char CHAR;
typedef unsigned char boolean;

#define false       0
#define true        1

/* Set the EndianOrder.
** The GIF Reader file should do this.
** Set EndianOrder = 0 if machine is little endian
**     EndianOrder = 1 if machine is big endian.
*/
extern int  EndianOrder;

/*
**  The GIF header format.
**
**  This structure actually contains the header, logical screen
**  descriptor, and the global color table for the GIF image.
*/
typedef struct _GifHeader { /* Offset   Description                         */
    BYTE PackedField;       /*  0Ah     Color Information                   */
    WORD TableSize;
    BYTE ImageCount;        /* Keep a count of the number of images         */
    BYTE CommentCount;
    BYTE ApplicationCount;
    BYTE PlainTextCount;
    BYTE HDFPalette[256][3];
    BYTE HeaderDump[6];     /* BYTE array to dump header contents           */
    BYTE LSDDump[7];        /* Logical Screen Descriptor dump               */
} GIFHEAD;


/*
**  The GIF Image Descriptor.
*/
typedef struct _GifImageDescriptor {
    WORD ImageWidth;            /* Width of the image in pixels             */
    WORD ImageHeight;           /* Height of the image in pixels            */
    BYTE PackedField;           /* Image and Color Table Data Information   */
    WORD TableSize;
    WORD CodeSize;              /* Minimum LZW CodeSize for image data      */
    BYTE HDFPalette[256][3];
    BYTE GIDDump[9];            /* GifImageDescriptor dump                  */

    BYTE *Image;                /* Decompressed Raster Image                */
    BYTE *GIFImage;
} GIFIMAGEDESC;

/*
**  GIF 89a Graphic Control Extension Block
*/
typedef struct _GifGraphicControlExtension {
    BYTE GCEDump[5];            /* Graphic Control Extension Dump           */
} GIFGRAPHICCONTROL;

/*
**  GIF 89a Plain Text Extension Block
*/
typedef struct _GifPlainTextExtension {
    BYTE PTEDump[15];           /* Plain Text Extension Dump                */
    BYTE *PlainTextData;        /* Plain Text data sub-blocks               */
    WORD DataSize;
} GIFPLAINTEXT;


/*
**  GIF 89a Application Extension Block
*/
typedef struct _GifApplicationExtension {
    BYTE AEDump[14];            /* Application Extension Dump               */
    BYTE *ApplicationData;      /* Application data sub-blocks              */
    WORD DataSize;
} GIFAPPLICATION;

/*
**  GIF 89a Comment Extension Block
*/
typedef struct _GifCommentExtension {
    BYTE CEDump[2];             /* Comment Extension Dump                   */
    BYTE *CommentData;          /* Comment data sub-blocks                  */
    WORD DataSize;
    BYTE Terminator;            /* Block Terminator (always 0)              */
} GIFCOMMENT;

/*
** GIF to HDF Memory Struct
** Purpose : The gif to hdf structure is used to pass all the
**           gif data to the memory, which gets caught by the hdf driver
**           Its the drivers job to put the data in the appropriate places
**           in the HDF file.
**           I have assumed that the ImageDescriptors and GraphicControls follow
**           one another, ie. I have not associated them with each other. The driver
**           must assume a 1-1 correspondance. The same discussion with plain text
**           extension.
*/
typedef struct _GifToMem {
	GIFHEAD            *GifHeader;
	GIFIMAGEDESC      **GifImageDesc;
	GIFGRAPHICCONTROL **GifGraphicControlExtension;
	GIFPLAINTEXT      **GifPlainTextExtension;
	GIFAPPLICATION    **GifApplicationExtension;
	GIFCOMMENT        **GifCommentExtension;
} GIFTOMEM;

/*
**  Function Prototypes
*/

/* GIF2MEM.C */
GIFTOMEM Gif2Mem(BYTE *);

/* GIFREAD.C */
int ReadGifHeader(GIFHEAD *, BYTE **);
int ReadGifImageDesc(GIFIMAGEDESC *, BYTE **);
int ReadGifGraphicControl(GIFGRAPHICCONTROL *, BYTE **);
int ReadGifPlainText(GIFPLAINTEXT *, BYTE **);
int ReadGifApplication(GIFAPPLICATION *, BYTE **);
int ReadGifComment(GIFCOMMENT *, BYTE **);

/* HDFGIFWR.C */
int hdfWriteGIF(FILE *fp, BYTE *pic, int ptype, int w, int h, BYTE *rmap,
                BYTE *gmap, BYTE *bmap, BYTE *pc2ncmap, int numcols,
                int colorstyle, int BitsPerPixel);

/* WRITEHDF.C */
int WriteHDF(GIFTOMEM , CHAR * );

/* Function:    ReadHDF
** Return:      0 on completion without error, -1 on error
** Input:       CHAR *h5_file - HDF file name
**              CHAR *dset_name - Name of the HDF Image dataset
**              CHAR *pal_name - Name of the HDF palette
** Output:      BYTE* data - the HDF Image to be converted
**              BYTE  palette[256][3] - the corresponding palette
**              hsize_t* image_size - the size of each dimension of the image
*/
int ReadHDF(BYTE** data, BYTE palette[256][3], hsize_t *image_size,
            CHAR *h5_file, CHAR *dset_name, CHAR *pal_name);

BYTE *Decompress(GIFIMAGEDESC *, GIFHEAD *);
BYTE GetByte(BYTE *);
WORD GetWord(BYTE *);

void cleanup(BYTE*);

#endif  /* GIF_H_ */
