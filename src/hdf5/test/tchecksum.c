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

/*-------------------------------------------------------------------------
 *
 * Created:		tchecksum.c
 *			Aug 21 2006
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Test internal checksum routine(s)
 *
 *-------------------------------------------------------------------------
 */

/***********/
/* Headers */
/***********/
#include "testhdf5.h"

/**********/
/* Macros */
/**********/
#define BUF_LEN 3093    /* No particular value */

/*******************/
/* Local variables */
/*******************/
uint8_t large_buf[BUF_LEN];


/****************************************************************
**
**  test_chksum_size_one(): Checksum 1 byte buffer
**
****************************************************************/
static void
test_chksum_size_one(void)
{
    uint8_t buf[1] = {23};      /* Buffer to checksum */
    uint32_t chksum;    /* Checksum value */

    /* Buffer w/real data */
    chksum = H5_checksum_fletcher32(buf, sizeof(buf));
    VERIFY(chksum, 0x17001700, "H5_checksum_fletcher32");

    chksum = H5_checksum_crc(buf, sizeof(buf));
    VERIFY(chksum, 0xfa2568b7, "H5_checksum_crc");

    chksum = H5_checksum_lookup3(buf, sizeof(buf), 0);
    VERIFY(chksum, 0xa209c931, "H5_checksum_lookup3");

    /* Buffer w/zero(s) for data */
    HDmemset(buf, 0, sizeof(buf));
    chksum = H5_checksum_fletcher32(buf, sizeof(buf));
    VERIFY(chksum, 0, "H5_checksum_fletcher32");

    chksum = H5_checksum_crc(buf, sizeof(buf));
    VERIFY(chksum, 0xfa60fb57, "H5_checksum_crc");

    chksum = H5_checksum_lookup3(buf, sizeof(buf), 0);
    VERIFY(chksum, 0x8ba9414b, "H5_checksum_lookup3");
} /* test_chksum_size_one() */


/****************************************************************
**
**  test_chksum_size_two(): Checksum 2 byte buffer
**
****************************************************************/
static void
test_chksum_size_two(void)
{
    uint8_t buf[2] = {23, 187};         /* Buffer to checksum */
    uint32_t chksum;                    /* Checksum value */

    /* Buffer w/real data */
    chksum = H5_checksum_fletcher32(buf, sizeof(buf));
    VERIFY(chksum, 0x17bb17bb, "H5_checksum_fletcher32");

    chksum = H5_checksum_crc(buf, sizeof(buf));
    VERIFY(chksum, 0xfc856608, "H5_checksum_crc");

    chksum = H5_checksum_lookup3(buf, sizeof(buf), 0);
    VERIFY(chksum, 0x8ba7a6c9, "H5_checksum_lookup3");

    /* Buffer w/zero(s) for data */
    HDmemset(buf, 0, sizeof(buf));
    chksum = H5_checksum_fletcher32(buf, sizeof(buf));
    VERIFY(chksum, 0, "H5_checksum_fletcher32");

    chksum = H5_checksum_crc(buf, sizeof(buf));
    VERIFY(chksum, 0xfc7e9b20, "H5_checksum_crc");

    chksum = H5_checksum_lookup3(buf, sizeof(buf), 0);
    VERIFY(chksum, 0x62cd61b3, "H5_checksum_lookup3");
} /* test_chksum_size_two() */


/****************************************************************
**
**  test_chksum_size_three(): Checksum 3 byte buffer
**
****************************************************************/
static void
test_chksum_size_three(void)
{
    uint8_t buf[3] = {23, 187, 98};     /* Buffer to checksum */
    uint32_t chksum;                    /* Checksum value */

    /* Buffer w/real data */
    chksum = H5_checksum_fletcher32(buf, sizeof(buf));
    VERIFY(chksum, 0x917679bb, "H5_checksum_fletcher32");

    chksum = H5_checksum_crc(buf, sizeof(buf));
    VERIFY(chksum, 0xfebc5d70, "H5_checksum_crc");

    chksum = H5_checksum_lookup3(buf, sizeof(buf), 0);
    VERIFY(chksum, 0xcebdf4f0, "H5_checksum_lookup3");

    /* Buffer w/zero(s) for data */
    HDmemset(buf, 0, sizeof(buf));
    chksum = H5_checksum_fletcher32(buf, sizeof(buf));
    VERIFY(chksum, 0, "H5_checksum_fletcher32");

    chksum = H5_checksum_crc(buf, sizeof(buf));
    VERIFY(chksum, 0xf9cc4c7a, "H5_checksum_crc");

    chksum = H5_checksum_lookup3(buf, sizeof(buf), 0);
    VERIFY(chksum, 0x6bd0060f, "H5_checksum_lookup3");
} /* test_chksum_size_three() */


/****************************************************************
**
**  test_chksum_size_four(): Checksum 4 byte buffer
**
****************************************************************/
static void
test_chksum_size_four(void)
{
    uint8_t buf[4] = {23, 187, 98, 217};/* Buffer to checksum */
    uint32_t chksum;                    /* Checksum value */

    /* Buffer w/real data */
    chksum = H5_checksum_fletcher32(buf, sizeof(buf));
    VERIFY(chksum, 0x924f7a94, "H5_checksum_fletcher32");

    chksum = H5_checksum_crc(buf, sizeof(buf));
    VERIFY(chksum, 0xff398a46, "H5_checksum_crc");

    chksum = H5_checksum_lookup3(buf, sizeof(buf), 0);
    VERIFY(chksum, 0x2c88bb51, "H5_checksum_lookup3");

    /* Buffer w/zero(s) for data */
    HDmemset(buf, 0, sizeof(buf));
    chksum = H5_checksum_fletcher32(buf, sizeof(buf));
    VERIFY(chksum, 0, "H5_checksum_fletcher32");

    chksum = H5_checksum_crc(buf, sizeof(buf));
    VERIFY(chksum, 0xff117081, "H5_checksum_crc");

    chksum = H5_checksum_lookup3(buf, sizeof(buf), 0);
    VERIFY(chksum, 0x049396b8, "H5_checksum_lookup3");
} /* test_chksum_size_four() */


/****************************************************************
**
**  test_chksum_large(): Checksum larger buffer
**
****************************************************************/
static void
test_chksum_large(void)
{
    uint32_t chksum;            /* Checksum value */
    size_t u;                   /* Local index variable */

    /* Initialize buffer w/known data */
    for(u = 0; u < BUF_LEN; u++)
        large_buf[u] = u * 3;

    /* Buffer w/real data */
    chksum = H5_checksum_fletcher32(large_buf, sizeof(large_buf));
    VERIFY(chksum, 0x85b4e2a, "H5_checksum_fletcher32");

    chksum = H5_checksum_crc(large_buf, sizeof(large_buf));
    VERIFY(chksum, 0xfbd0f7c0, "H5_checksum_crc");

    chksum = H5_checksum_lookup3(large_buf, sizeof(large_buf), 0);
    VERIFY(chksum, 0x1bd2ee7b, "H5_checksum_lookup3");

    /* Buffer w/zero(s) for data */
    HDmemset(large_buf, 0, sizeof(large_buf));
    chksum = H5_checksum_fletcher32(large_buf, sizeof(large_buf));
    VERIFY(chksum, 0, "H5_checksum_fletcher32");

    chksum = H5_checksum_crc(large_buf, sizeof(large_buf));
    VERIFY(chksum, 0xfac8b4c4, "H5_checksum_crc");

    chksum = H5_checksum_lookup3(large_buf, sizeof(large_buf), 0);
    VERIFY(chksum, 0x930c7afc, "H5_checksum_lookup3");
} /* test_chksum_large() */


/****************************************************************
**
**  test_checksum(): Main checksum testing routine.
**
****************************************************************/
void
test_checksum(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing checksum algorithms\n"));

    /* Various checks for fletcher32 checksum algorithm */
    test_chksum_size_one();		/* Test buffer w/only 1 byte */
    test_chksum_size_two();		/* Test buffer w/only 2 bytes */
    test_chksum_size_three();		/* Test buffer w/only 3 bytes */
    test_chksum_size_four();		/* Test buffer w/only 4 bytes */
    test_chksum_large();		/* Test buffer w/larger # of bytes */

} /* test_checksum() */


/*-------------------------------------------------------------------------
 * Function:	cleanup_checksum
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	Quincey Koziol
 *              August 21, 2006
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_checksum(void)
{
    /* no file to clean */
}

