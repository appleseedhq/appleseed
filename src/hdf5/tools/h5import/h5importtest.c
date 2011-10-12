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
#include "H5private.h"

/*
 * Name:
 *      h5importtest
 *
 * Description:
 *      This program creates that can be
 *      used to test the h5import program.
 *
 */

int
main(void)
{
    int       nrow = 3, ncol = 4, npln = 5;
    int       i, j, k;
    FILE      *sp;

    float     row4[3], col4[4], pln4[5];
    float     rowo4 = (float)11.0e0, colo4 = (float)21.0e0, plno4 = (float)51.0e0;
    float     rowi4 = (float)1.0e0, coli4 = (float)2.0e0, plni4 = (float)5.0e0;

    int       b32i3[5][3][4];
    int       row4i[3], col4i[4], pln4i[5];
    int       rowo4i = (int)11 , colo4i = (int)21 , plno4i = (int)51 ;
    int       rowi4i = (int)1 , coli4i = (int)2 , plni4i = (int)5 ;

#ifndef WIN32
    long long row4i64[3], col4i64[4], pln4i64[5];
    long long rowo4i64 = (long long)11 , colo4i64 = (long long)21 , plno4i64 = (long long)51 ;
    long long rowi4i64 = (long long)1 , coli4i64 = (long long)2 , plni4i64 = (long long)5 ;
#endif

    short     b16i3[5][3][4];
    short     row4i16[3], col4i16[4], pln4i16[5];
    short     rowo4i16 = (short)11 , colo4i16 = (short)21 , plno4i16 = (short)51 ;
    short     rowi4i16 = (short)1 , coli4i16 = (short)2 , plni4i16 = (short)5 ;

    char      b8i3[5][3][4];
    char      row4i8[3], col4i8[4], pln4i8[5];
    char      rowo4i8 = (char)11 , colo4i8 = (char)21 , plno4i8 = (char)51 ;
    char      rowi4i8 = (char)1 , coli4i8 = (char)2 , plni4i8 = (char)5 ;

    double    b64r3[5][3][4];
    double    row8[3], col8[4], pln8[5];
    double    rowo8 = 11.0e0, colo8 = 21.0e0, plno8 = 51.0e0;
    double    rowi8 = 1.0e0, coli8 = 2.0e0, plni8 = 5.0e0;


    /*
    * initialize the row, column, and plane vectors
    *
    * row values start at 11 and increment by 1 => 11, 12, 13
    * column values start at 21 and increment by 2 => 21, 23, 25, 27
    * plane values start at 51 and increment by 5 => 51, 56, 61, 66, 71
    */


    /*
    * build array elements - rank 2
    *
    * element value = sum of row value and col values
    */

    row4[0] = rowo4;
    col4[0] = colo4;
    pln4[0] = plno4;

    row8[0] = rowo8;
    col8[0] = colo8;
    pln8[0] = plno8;

    row4i[0] = rowo4i;
    col4i[0] = colo4i;
    pln4i[0] = plno4i;

#ifndef WIN32
    row4i64[0] = rowo4i64;
    col4i64[0] = colo4i64;
    pln4i64[0] = plno4i64;
#endif

    row4i16[0] = rowo4i16;
    col4i16[0] = colo4i16;
    pln4i16[0] = plno4i16;

    row4i8[0] = rowo4i8;
    col4i8[0] = colo4i8;
    pln4i8[0] = plno4i8;

    for (i = 1; i < nrow; i++)
    {
        row4[i] = row4[i - 1] + rowi4;
        row8[i] = row8[i - 1] + rowi8;
        row4i[i] = row4i[i - 1] + rowi4i;
#ifndef WIN32
        row4i64[i] = row4i64[i - 1] + rowi4i64;
#endif
        row4i16[i] = row4i16[i - 1] + rowi4i16;
        row4i8[i] = row4i8[i - 1] + rowi4i8;
    }

    for (j = 1; j < ncol; j++)
    {
        col4[j] = col4[j - 1] + coli4;
        col8[j] = col8[j - 1] + coli8;
        col4i[j] = col4i[j - 1] + coli4i;
#ifndef WIN32
        col4i64[j] = col4i64[j - 1] + coli4i64;
#endif
        col4i16[j] = col4i16[j - 1] + coli4i16;
        col4i8[j] = col4i8[j - 1] + coli4i8;
    }
    for (k = 1; k < npln; k++)
    {
        pln4[k] = pln4[k - 1] + plni4;
        pln8[k] = pln8[k - 1] + plni8;
        pln4i[k] = pln4i[k - 1] + plni4i;
#ifndef WIN32
        pln4i64[k] = pln4i64[k - 1] + plni4i64;
#endif
        pln4i16[k] = pln4i16[k - 1] + plni4i16;
        pln4i8[k] = pln4i8[k - 1] + plni4i8;
    }

    /*
    * build array elements - rank 3
    *
    * element value = sum of row value, col, and plane values
    */

    for (i = 0; i < nrow; i++)
        for (j = 0; j < ncol; j++)
            for (k = 0; k < npln; k++) {
                b64r3[k][i][j] = row8[i] + col8[j] + pln8[k];
                b32i3[k][i][j] = row4i[i] + col4i[j] + pln4i[k];
                b16i3[k][i][j] = row4i16[i] + col4i16[j] + pln4i16[k];
                b8i3[k][i][j] = row4i8[i] + col4i8[j] + pln4i8[k];
            }



#ifndef UNICOS

 /*-------------------------------------------------------------------------
  * TOOLTEST txtin16.txt -c $srcdir/testfiles/txtin16.conf -o txtin16.h5
  *-------------------------------------------------------------------------
  */


    sp = fopen("txtin16.txt", "w");
    for (k = 0; k < npln; k++)
    {
        for (i = 0; i < nrow; i++)
        {
            for (j = 0; j < ncol; j++)
                (void) fprintf(sp, "%10u", b16i3[k][i][j]);
            (void) fprintf(sp, "\n");
        }
    }
    (void) fclose(sp);

 /*-------------------------------------------------------------------------
  * TOOLTEST txtin32.txt -c $srcdir/testfiles/textin32.conf -o textin32.h5
  *-------------------------------------------------------------------------
  */

    sp = fopen("txtin32.txt", "w");
    for (k = 0; k < npln; k++)
    {
        for (i = 0; i < nrow; i++)
        {
            for (j = 0; j < ncol; j++)
                (void) fprintf(sp, "%10d", b32i3[k][i][j]);
            (void) fprintf(sp, "\n");
        }
    }
    (void) fclose(sp);

 /*-------------------------------------------------------------------------
  * TOOLTEST binin32.bin -c $srcdir/testfiles/binin32.conf -o binin32.h5
  *-------------------------------------------------------------------------
  */

#ifdef WIN32
    sp = fopen("binin32.bin", "wb");
#else
    sp = fopen("binin32.bin", "w");
#endif
    for (k = 0; k < npln; k++)
    {
        for (i = 0; i < nrow; i++)
        {
            for (j = 0; j < ncol; j++)
            {
                (void) fwrite((char *) &b32i3[k][i][j], sizeof(int), 1, sp);
            }
        }
    }
    (void) fclose(sp);

 /*-------------------------------------------------------------------------
  * TOOLTEST binuin32.bin -c $srcdir/testfiles/binuin32.conf -o binuin32.h5
  *-------------------------------------------------------------------------
  */

#ifdef WIN32
    sp = fopen("binuin32.bin", "wb");
#else
    sp = fopen("binuin32.bin", "w");
#endif
    for (k = 0; k < npln; k++)
    {
        for (i = 0; i < nrow; i++)
        {
            for (j = 0; j < ncol; j++)
            {
                (void) fwrite((char *) &b32i3[k][i][j], sizeof(unsigned int), 1, sp);
            }
        }
    }
    (void) fclose(sp);




 /*-------------------------------------------------------------------------
  * TOOLTEST binin16.bin -c $srcdir/testfiles/binin16.conf -o binin16.h5
  *-------------------------------------------------------------------------
  */

#ifdef WIN32
    sp = fopen("binin16.bin", "wb");
#else
    sp = fopen("binin16.bin", "w");
#endif
    for (k = 0; k < npln; k++)
    {
        for (i = 0; i < nrow; i++)
        {
            for (j = 0; j < ncol; j++)
            {
                (void) fwrite((char *) &b16i3[k][i][j], sizeof(short), 1, sp);
            }
        }
    }
    (void) fclose(sp);

 /*-------------------------------------------------------------------------
  * TOOLTEST binuin16.bin -c $srcdir/testfiles/binuin16.conf -o binuin16.h5
  *-------------------------------------------------------------------------
  */
#ifdef WIN32
    sp = fopen("binuin16.bin", "wb");
#else
    sp = fopen("binuin16.bin", "w");
#endif
    for (k = 0; k < npln; k++)
    {
        for (i = 0; i < nrow; i++)
        {
            for (j = 0; j < ncol; j++)
            {
                (void) fwrite((char *) &b16i3[k][i][j], sizeof(unsigned short), 1, sp);
            }
        }
    }
    (void) fclose(sp);



 /*-------------------------------------------------------------------------
  * TOOLTEST binin8.bin -c $srcdir/testfiles/binin8.conf  -o binin8.h5
  *-------------------------------------------------------------------------
  */

#ifdef WIN32
    sp = fopen("binin8.bin", "wb");
#else
    sp = fopen("binin8.bin", "w");
#endif
    for (k = 0; k < npln; k++)
    {
        for (i = 0; i < nrow; i++)
        {
            for (j = 0; j < ncol; j++)
            {
                (void) fwrite((char *) &b8i3[k][i][j], sizeof(char), 1, sp);
            }
        }
    }
    (void) fclose(sp);

#endif /* UNICOS */




 /*-------------------------------------------------------------------------
  * TOOLTEST binfp64.bin -c $srcdir/testfiles/binfp64.conf -o binfp64.h5
  *-------------------------------------------------------------------------
  */

 /*
  * binary 64-bit file - rank 2 & 3
  */

#ifdef WIN32
    sp = fopen("binfp64.bin", "wb");
#else
    sp = fopen("binfp64.bin", "w");
#endif
    for (k = 0; k < npln; k++)
    {
        for (i = 0; i < nrow; i++)
        {
            for (j = 0; j < ncol; j++)
            {
                (void) fwrite((char *) &b64r3[k][i][j], sizeof(double), 1, sp);
            }
        }
    }
    (void) fclose(sp);



 /*-------------------------------------------------------------------------
  * TOOLTEST binin8w.bin -c $srcdir/testfiles/binin8w.conf -o binin8w.h5
  *-------------------------------------------------------------------------
  */

    {
        /* test CR+LF (13,10) and EOF (26) in windows */
        char bin8w[4] = {13,10,26,0};

#ifdef WIN32
        sp = fopen("binin8w.bin", "wb");
#else
        sp = fopen("binin8w.bin", "w");
#endif
        for (i = 0; i < 4; i++)
        {
            char c = bin8w[i];
            if ( fwrite( &c, sizeof(char), 1, sp) != 1 )
                printf("error writing file\n");
        }
        fclose(sp);


    }





    return (EXIT_SUCCESS);
}

