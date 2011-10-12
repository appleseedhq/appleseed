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

#define NEXTBYTE        (*ptr++)

#define IMAGESEP        0x2c
#define INTERLACEMASK   0x40
#define COLORMAPMASK    0x80

WORD iWIDE, iHIGH, eWIDE, eHIGH, expand, numcols, strip, nostrip;
unsigned long  cols[256];
char *cmd;

FILE *fp;

static WORD
    XC = 0, YC = 0,         /* Output X and Y coords of current pixel       */
    InitCodeSize,           /* Starting code size, used during Clear        */
    CodeSize,               /* Code size, read from GIF header              */
    BytesPerScanline,       /* Bytes per scanline in output raster          */
    IWidth, IHeight;        /* image dimensions                             */
static int
    BitOffset = 0,          /* Bit Offset of next code                      */
    Pass = 0,               /* Used by output routine if WORDerlaced pic    */
    OutCount = 0,           /* Decompressor output 'stack count'            */
    Code,                   /* Value returned by ReadCode                   */
    MaxCode,                /* limiting value for current code size         */
    ClearCode,              /* GIF clear code                               */
    EOFCode,                /* GIF end-of-information code                  */
    CurCode, OldCode, InCode,   /* Decompressor variables                   */
    FirstFree,              /* First free code, generated per GIF spec      */
    FreeCode,               /* Decompressor, next free slot in hash table   */
    FinChar,                /* Decompressor variable                        */
    DataMask,               /* AND mask for data size                       */
    ReadMask;               /* Code AND mask for current code size          */

/*MODIFICATIONS*/
BYTE tempbyte[10];
BYTE * tempBYTEptr[10];
WORD tempint[10];
WORD ImageCount = 0;
/*END MODIFICATION*/

boolean Interlace, HasColormap;

BYTE *Image;                /* The result array                             */
BYTE *RawGIF;               /* The heap array to hold it, raw               */
BYTE *Raster;               /* The raster data stream, unblocked            */

/* The hash table used by the decompressor */

int Prefix[4096];
int Suffix[4096];

/* An output array used by the decompressor */

int OutCode[1025];

/* The color map, read from the GIF header */

int  numused;

/*
 * Fetch the next code from the raster data stream.  The codes can be any
 * length from 3 to 12 bits, packed WORDo 8-bit BYTEs, so we have to maWORDain
 * our location in the Raster array as a BIT Offset.  We compute the BYTE
 * Offset WORDo the raster array by dividing this by 8, pick up three BYTEs,
 * compute the bit Offset WORDo our 24-bit chunk, shift to bring the desired
 * code to the bottom, then mask it off and return it.
 */
static int
ReadCode(void)
{
    int RawCode, ByteOffset;

    ByteOffset = BitOffset / 8;
    RawCode = Raster[ByteOffset] + (0x100 * Raster[ByteOffset + 1]);

    if (CodeSize >= 8)
        RawCode += (0x10000 * Raster[ByteOffset + 2]);

    RawCode >>= (BitOffset % 8);
    BitOffset += CodeSize;
    return (RawCode & ReadMask);
}

static void
AddToPixel(BYTE Index)
{
    if (YC<IHeight)
    *(Image + YC * BytesPerScanline + XC) = Index;



    /* Update the X-coordinate, and if it overflows, update the
     * Y-coordinate */
    if (++XC == IWidth) {
        /*
         * If a non-WORDerlaced picture, just increment YC to the next scan
         * line.  If it's WORDerlaced, deal with the WORDerlace as described
         * in the GIF spec.  Put the decoded scan line out to the screen if we
         * haven't gone past the bottom of it.
         */
        XC = 0;

        if (!Interlace) {
            YC++;
        } else {
            switch (Pass) {
            case 0:
                YC += 8;

                if (YC >= IHeight) {
                    Pass++;
                    YC = 4;
                }

                break;
            case 1:
                YC += 8;

                if (YC >= IHeight) {
                    Pass++;
                    YC = 2;
                }

                break;
            case 2:
                YC += 4;

                if (YC >= IHeight) {
                    Pass++;
                    YC = 1;
                }

                break;
            case 3:
                YC += 2;
                break;
            default:
                break;
            }
        }
    }
}

/* Main routine.  Convert a GIF image to an HDF image */

BYTE *
Decompress(GIFIMAGEDESC *GifImageDesc, GIFHEAD *GifHead)
{
    int i;

    XC = 0;
    YC = 0;
    Pass = 0;
    OutCount = 0;
    BitOffset = 0;

    DataMask = (1 << ((GifHead->PackedField & 0x07) +1)) -1;
    Raster = GifImageDesc->GIFImage;

    /* Check for image seperator */

    /* Now read in values from the image descriptor */
    IWidth = GifImageDesc->ImageWidth;
    IHeight = GifImageDesc->ImageHeight;
    Interlace = GifImageDesc->PackedField & 0x40;

    /*
     * Note that I ignore the possible existence of a local color map.  I'm
     * told there aren't many files around that use them, and the spec says
     * it's defined for future use.  This could lead to an error reading some
     * files.
     */

    /*
     * Start reading the raster data. First we get the WORDial code size and
     * compute decompressor constant values, based on this code size.
     */

    CodeSize = GifImageDesc->CodeSize;
    ClearCode = (1 << CodeSize);
    EOFCode = ClearCode + 1;
    FreeCode = FirstFree = ClearCode + 2;

    /*
     * The GIF spec has it that the code size is the code size used to compute
     * the above values is the code size given in the file, but the code size
     * used in compression/decompression is the code size given in the file
     * plus one. (thus the ++).
     */

    CodeSize++;
    InitCodeSize = CodeSize;
    MaxCode = (1 << CodeSize);
    ReadMask = MaxCode - 1;

    /*
     * Read the raster data.  Here we just transpose it from the GIF array to
     * the Raster array, turning it from a series of blocks WORDo one long
     * data stream, which makes life much easier for ReadCode().
     */

    /* Allocate the Image */

    if (!(Image = (BYTE *)malloc((size_t)IWidth*(size_t)IHeight))) {
        printf("Out of memory");
        exit(EXIT_FAILURE);
    }

    BytesPerScanline = IWidth;

    /*
     * Decompress the file, continuing until you see the GIF EOF code.  One
     * obvious enhancement is to add checking for corrupt files here.
     */

    Code = ReadCode();

    while (Code != EOFCode) {
        /*
         * Clear code sets everything back to its initial value, then reads
         * the immediately subsequent code as uncompressed data.
         */
        if (Code == ClearCode) {
            CodeSize = InitCodeSize;
            MaxCode = (1 << CodeSize);
            ReadMask = MaxCode - 1;
            FreeCode = FirstFree;
            CurCode = OldCode = Code = ReadCode();
            FinChar = CurCode & DataMask;
            AddToPixel((BYTE)FinChar);
        } else {
            /*
             * If not a clear code, then must be data: save same as CurCode
             * and InCode
             */
            CurCode = InCode = Code;

            /*
             * If greater or equal to FreeCode, not in the hash table yet;
             * repeat the last character decoded
             */
            if (CurCode >= FreeCode) {
                CurCode = OldCode;
                OutCode[OutCount++] = FinChar;
            }

            /*
             * Unless this code is raw data, pursue the chain poWORDed to by
             * CurCode through the hash table to its end; each code in the
             * chain puts its associated output code on the output queue.
             */
            while (CurCode > DataMask) {
                if (OutCount > 1024) {
                    /*return error message*/
                }

                OutCode[OutCount++] = Suffix[CurCode];
                CurCode = Prefix[CurCode];
            }

            /* The last code in the chain is treated as raw data. */
            FinChar = CurCode & DataMask;
            OutCode[OutCount++] = FinChar;

            /*
             * Now we put the data out to the Output routine. It's been
             * stacked LIFO, so deal with it that way...
             */
            for (i = OutCount - 1; i >= 0; i--)
                AddToPixel((BYTE)OutCode[i]);

            OutCount = 0;

            /*
             * Build the hash table on-the-fly. No table is stored in the
             * file.
             */
            Prefix[FreeCode] = OldCode;
            Suffix[FreeCode] = FinChar;
            OldCode = InCode;

            /*
             * PoWORD to the next slot in the table.  If we exceed the current
             * MaxCode value, increment the code size unless it's already 12.
             * If it is, do nothing: the next code decompressed better be
             * CLEAR
             */
            FreeCode++;

            if (FreeCode >= MaxCode)
                if (CodeSize < 12) {
                    CodeSize++;
                    MaxCode *= 2;
                    ReadMask = (1 << CodeSize) - 1;
                }
        }

        Code = ReadCode();
    }

    return Image;
}
