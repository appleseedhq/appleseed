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
#include <stdlib.h>

#include "gif.h"

int EndianOrder;

static BYTE *ReadDataSubBlocks(BYTE **MemGif2, WORD *DSize);

WORD
GetWord(BYTE *MemGif)
{
    WORD w;

    if (EndianOrder == 1) {
        /* LittleEndian */
        w  = (WORD) (*MemGif++ & 0xFF);
        w |= (WORD) ((*MemGif++ & 0xFF) << 0x08);
    } else {
        w = (WORD) (*MemGif++ & 0xFF);
        w = ((WORD) (*MemGif++ & 0xFF)) | (w << 0x08);
    }

    return w;
}

BYTE
GetByte(BYTE *MemGif)
{
    return *MemGif;
}

/*
 *  Read a GIF image BYTE Header.
 *
 *  This function reads the Header, Logical Screen Descriptor, and
 *  Global Color Table (if any) from a GIF image file.  The information
 *  is stored in a GIFHEAD structure.
 *
 *  Returns: -1 if a FILE stream error occured during the read,
 *           otherwise 0 if no error occured.
 */
int
ReadGifHeader(GIFHEAD *GifHead, /* Pointer to GIF header structure  */
              BYTE **MemGif2)   /* GIF image file input FILE stream */
{
    WORD i;             /* Loop counter                                */
    WORD tableSize;     /* Number of entires in the Global Color Table */

    GifHead->TableSize = 0;
    for (i = 0 ; i < 6 ; i++) {
        GifHead->HeaderDump[i] = *(*MemGif2)++;
    }

    if (strncmp((const char *)GifHead->HeaderDump , "GIF" , (size_t)3)) {
        printf("The file does not appear to be a valid GIF file.\n");
        exit(EXIT_FAILURE);
    }

    for (i = 0 ; i < 7 ; i++) {
        GifHead->LSDDump[i] = *(*MemGif2)++;
    }

    GifHead->PackedField = GifHead->LSDDump[4];

    /* Check if a Global Color Table is present */
    if (GifHead->PackedField & 0x80) {
        /* Read number of color table entries */
        tableSize = (WORD) (1L << ((GifHead->PackedField & 0x07) + 1));
        GifHead->TableSize = tableSize;

        /* Read the Global Color Table */

        /*
         * There are some changes made here apart from just reading in the
         * global color table as would seem intuitively obvious.  The colors
         * are stored in the bottom part of the palette as opposed to the top
         */

        for (i = 0; i < tableSize; i++) {
            GifHead->HDFPalette[i][0] = *(*MemGif2)++;
            GifHead->HDFPalette[i][1] = *(*MemGif2)++;
            GifHead->HDFPalette[i][2] = *(*MemGif2)++;
        }
    }

    /* Check for a FILE stream error */
#if 0
    if (ferror(FpGif))
        return -1;
#endif  /* 0 */

    return 0;   /* No FILE stream error occured */
}


/*
**  Read a GIF Local Image Descriptor.
**
**  This function reads the Local Image Descriptor, and Local Color
**  Table (if any) from a GIF image file.  The information is stored
**  in a GIFIMAGEDESC structure.
**
**  Note that the ImageSeparator field value in the GIFIMAGEDESC
**  structure is assigned by the function calling ReadGifImageDesc().
**
**  Returns: -1 if a FILE stream error occured during the read,
**           otherwise 0 if no error occured.
*/
int
ReadGifImageDesc(
    GIFIMAGEDESC *GifImageDesc, /* Pointer to GIF image descriptor structure  */
    BYTE **MemGif2              /* GIF image file input FILE stream           */
    )
{
    WORD i;        /* Loop counter                               */
    WORD tableSize;         /* Number of entries in the Local Color Table */
 /* BYTE Interlace; */         /* PackedField & 0x20 gives information on interlacing */
 BYTE *TempPtr;
 int  ch , ch1;

 GifImageDesc->TableSize = 0;
 for (i = 0 ; i < 9 ; i++) {
  GifImageDesc->GIDDump[i] = *(*MemGif2)++;
 }

 /*
 ** Get the relevant fields. I need ImageWidth and Height actively hence I have
 ** taken information from those fields. I intend to keep the GifImageDesc data
 ** structure as it is so that anyone needing the rest of the fields can do so
 ** quickly.
 */

 if (EndianOrder == 1) /* LittleEndian */
 {
  GifImageDesc->ImageWidth  = (WORD) (GifImageDesc->GIDDump[4] & 0xFF);
  GifImageDesc->ImageWidth |= (WORD) ((GifImageDesc->GIDDump[5] & 0xFF) << 0x08);

  GifImageDesc->ImageHeight  = (WORD) (GifImageDesc->GIDDump[6] & 0xFF);
  GifImageDesc->ImageHeight |= (WORD) ((GifImageDesc->GIDDump[7] & 0xFF) << 0x08);

 }
 else
 {
  GifImageDesc->ImageWidth = (WORD) (GifImageDesc->GIDDump[4] & 0xFF);
  GifImageDesc->ImageWidth = ((WORD) (GifImageDesc->GIDDump[5] & 0xFF)) | (GifImageDesc->ImageWidth << 0x08);

  GifImageDesc->ImageHeight = (WORD) (GifImageDesc->GIDDump[6] & 0xFF);
  GifImageDesc->ImageHeight = ((WORD) (GifImageDesc->GIDDump[7] & 0xFF)) | (GifImageDesc->ImageWidth << 0x08);


 }

 GifImageDesc->PackedField = GifImageDesc->GIDDump[8];

 /* Interlace = GifImageDesc->PackedField & 0x20; */

    /* Check if a Local Color Table is present */
    if (GifImageDesc->PackedField & 0x80)
    {
        /* Read number of color table entries */
        tableSize = (WORD) (1L << ((GifImageDesc->PackedField & 0x07) + 1));
  GifImageDesc->TableSize = tableSize;
        /* Read the Local Color Table */
        for (i = 0; i < tableSize; i++)
        {
            GifImageDesc->HDFPalette[i][0] = *(*MemGif2)++;
            GifImageDesc->HDFPalette[i][1] = *(*MemGif2)++;
            GifImageDesc->HDFPalette[i][2] = *(*MemGif2)++;
        }
    }

 /*
 ** Get LZW minimum Code Size
 */
 GifImageDesc->CodeSize = (WORD)*(*MemGif2)++;

 /*GifImageDesc->GIFImage = ReadDataSubBlocks(FpGif);*/
 if (!(GifImageDesc->GIFImage = (BYTE *)malloc((GifImageDesc->ImageWidth) * (GifImageDesc->ImageHeight)))) {
  printf("Out of memory");
  exit(EXIT_FAILURE);
 }


 TempPtr = GifImageDesc->GIFImage;
 do
 {
  ch = ch1 = (int)*(*MemGif2)++;
  while (ch--) *TempPtr++ = *(*MemGif2)++;
 }
 while (ch1);


    return(0);          /* No FILE stream error occured */
}


/*
**  Read a GIF Graphic Control Extension block.
**
**  Note that the Introducer and Label field values in the GIFGRAPHICCONTROL
**  structure are assigned by the function calling ReadGifGraphicControl().
**
**  Returns: -1 if a FILE stream error occured during the read,
**           otherwise 0 if no error occured.
*/
int
ReadGifGraphicControl(
    GIFGRAPHICCONTROL *GifGraphicControl, /* Pointer to GC Extension structure */
    BYTE              **MemGif2           /* GIF image file input FILE stream  */
    )
{
    int i;

    for (i = 0 ; i < 5 ; i++) {
            GifGraphicControl->GCEDump[i] = *(*MemGif2)++;
    }

    return(0);          /* No FILE stream error occured */
}


/*
**  Read a GIF Plain Text Extension block.
**
**  Note that the Introducer and Label field values in the GIFLPLAINTEXT
**  structure are assigned by the function calling ReadGifPlainText().
**
**  Returns: -1 if a FILE stream error occured during the read,
**           otherwise 0 if no error occured.
*/
int
ReadGifPlainText(
    GIFPLAINTEXT *GifPlainText, /* Pointer to Plain Text Extension structure */
    BYTE         **MemGif2        /* GIF image file input FILE stream          */
    )
{
    int i;

 for (i = 0 ; i < 13 ; i++) {
  GifPlainText->PTEDump[i] = *(*MemGif2)++;
 }

    /* Read in the Plain Text data sub-blocks */
    if (!(GifPlainText->PlainTextData = ReadDataSubBlocks(MemGif2 , &(GifPlainText->DataSize))))
        return(1);

    /*
 GifPlainText->Terminator       = 0;
 */

    /* Check for a FILE stream error */
 /*
    if (ferror(FpGif))
        return(-1);
 */

    return(0);          /* No FILE stream error occured */
}


/*
**  Read a GIF Application Extension block.
**
**  Note that the Introducer and Label field values in the GIFAPPLICATION
**  structure are assigned by the function calling ReadGifApplication().
**
**  Returns: -1 if a FILE stream error occured during the read,
**           otherwise 0 if no error occured.
*/
int
ReadGifApplication(
    GIFAPPLICATION *GifApplication, /* Pointer to Application Extension structure */
    BYTE           **MemGif2           /* GIF image file input FILE stream           */
    )
{
    int i;

 for (i = 0 ; i < 12 ; i++) {
  GifApplication->AEDump[i] = *(*MemGif2)++;
 }

    /* Read in the Plain Text data sub-blocks */
    if (!(GifApplication->ApplicationData = ReadDataSubBlocks(MemGif2 , &(GifApplication->DataSize))))
        return(1);
 /*
    GifApplication->Terminator     = 0;
 */

 /* Check for a FILE stream error */
 /*
    if (ferror(FpGif))
        return(-1);
 */

    return(0);          /* No FILE stream error occured */
}


/*
**  Read a GIF Comment Extension block.
**
**  Note that the Introducer and Label field values in the GIFCOMMENT
**  structure are assigned by the function calling ReadGifComment().
**
**  Returns: -1 if a FILE stream error occured during the read,
**           otherwise 0 if no error occured.
*/
int
ReadGifComment(
    GIFCOMMENT *GifComment, /* Pointer to GIF Comment Extension structure */
    BYTE       **MemGif2       /* GIF image file input FILE stream           */
    )
{

    /* Read in the Plain Text data sub-blocks */
    if (!(GifComment->CommentData = ReadDataSubBlocks(MemGif2 , &(GifComment->DataSize))))
        return(1);

    GifComment->Terminator = 0;

    return(0);          /* No FILE stream error occured */
}


/*
**  Read one or more GIF data sub-blocks and write the information
**  to a buffer.
**
**  A GIF "sub-block" is a single count byte followed by 1 to 255
**  additional data bytes.
**
**  Returns: A NULL pointer if a memory allocation error occured,
**           otherwise a valid pointer if no error occured.
*/
static BYTE *
ReadDataSubBlocks(BYTE **MemGif2, /* GIF image file input FILE stream              */
    WORD *DSize)
{
    BYTE *ptr1;     /* Pointer used to "walk the heap"               */
    BYTE *ptr2;     /* Pointer used to mark the top of the heap      */
    BYTE dataSize;  /* Size of the current data sub-block being read */
    WORD bufSize;   /* Total size of the Plain Text data buffer      */
 int tempcount = 0;

 bufSize = 0;    /* The output buffer is empty          */

    dataSize = *(*MemGif2)++;  /* Get the size of the first sub-block */

    /* Allocate initial data buffer */
    if (!(ptr1 = ptr2 = (BYTE *) malloc((size_t)dataSize + 1))) {
  printf("Out of memory. Allocation of memory for data sub-blocks for\neither Comment, Plain Text or Application Extensions failed");
        return((BYTE *) NULL);
 }
    for (;;)
    {
  tempcount++;
        bufSize += (dataSize);  /* Running total of the buffer size */
  *DSize = bufSize;

#ifdef COMMENTED_OUT
        *ptr1++ = dataSize;   /* Write the data count */
#endif /* COMMENTED_OUT */
        while (dataSize--)   /* Read/write the Plain Text data */
             *ptr1++ = *(*MemGif2)++;

        /* Check if there is another data sub-block */
        if ((dataSize = *(*MemGif2)++) == 0)
            break;  /* Block Terminator encountered */

        /* Increase the buffer size to accomodate the next sub-block */
        if (!(ptr1 = ptr2 = (BYTE *) realloc(ptr2, bufSize + dataSize + 1)))
            return((BYTE *) NULL);


  ptr1 += bufSize;   /* Move pointer to the end of the data */


    }

    *ptr1++ = '\0';

    return(ptr2);     /* Return a pointer to the sub-block data */
}





