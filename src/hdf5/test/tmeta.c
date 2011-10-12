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

/***********************************************************
*
* Test program:  tmeta
*
* Test the basic meta-data encode/decode macros calls.
*
*************************************************************/

#include "testhdf5.h"
#include "H5Fprivate.h"

#define TEST_INT16_VALUE    -7641
#define TEST_UINT16_VALUE   45002
#define TEST_INT32_VALUE    -981236
#define TEST_UINT32_VALUE   3476589

uint8_t                   compar_buffer[] =
{
    /* Little-endian encoded version of the 16-bit signed integer */
    (uint8_t) ((TEST_INT16_VALUE) & 0xff), (uint8_t) ((TEST_INT16_VALUE >> 8) & 0xff),
    /* Little-endian encoded version of the 16-bit unsigned integer */
    (uint8_t) ((TEST_UINT16_VALUE) & 0xff), (uint8_t) ((TEST_UINT16_VALUE >> 8) & 0xff),
    /* Little-endian encoded version of the 32-bit signed integer */
    (uint8_t) ((TEST_INT32_VALUE) & 0xff), (uint8_t) ((TEST_INT32_VALUE >> 8) & 0xff),
    (uint8_t) ((TEST_INT32_VALUE >> 16) & 0xff), (uint8_t) ((TEST_INT32_VALUE >> 24) & 0xff),
    /* Little-endian encoded version of the 32-bit unsigned integer */
    (uint8_t) ((TEST_UINT32_VALUE) & 0xff), (uint8_t) ((TEST_UINT32_VALUE >> 8) & 0xff),
    (uint8_t) ((TEST_UINT32_VALUE >> 16) & 0xff), (uint8_t) ((TEST_UINT32_VALUE >> 24) & 0xff),
};

uint8_t                   encode_buffer[sizeof(compar_buffer)];

/****************************************************************
**
**  test_metadata(): Main meta-data encode/decode testing routine.
**
****************************************************************/
void
test_metadata(void)
{
    int16_t     ei16 = TEST_INT16_VALUE;    /* variables to hold the values to encode */
    uint16_t    eu16 = TEST_UINT16_VALUE;
    int32_t     ei32 = TEST_INT32_VALUE;
    uint32_t    eu32 = TEST_UINT32_VALUE;
    int16_t     di16;       /* variables to hold the decoded values */
    uint16_t    du16;
    int32_t     di32;
    uint32_t    du32;
    uint8_t	*p;  /* pointer into the buffer being en/de-coded */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Metadata Encoding/decoding\n"));

    /* Start by encoding the values above */
    p = encode_buffer;
    INT16ENCODE(p, ei16);       /* Encode the int16 value */
    UINT16ENCODE(p, eu16);      /* Encode the uint16 value */
    INT32ENCODE(p, ei32);       /* Encode the int32 value */
    UINT32ENCODE(p, eu32);      /* Encode the uint32 value */

    /* Check if we got what we asked for */
    if (HDmemcmp(encode_buffer, compar_buffer, sizeof(compar_buffer)) != 0) {
        unsigned                   u;      /* local counting variable */

        for (u = 0; u < sizeof(compar_buffer); u++) {
            if (compar_buffer[u] != encode_buffer[u])
                TestErrPrintf("Error encoding meta-data at offset %u, wanted: %u, got: %u\n", (unsigned) u, (unsigned) compar_buffer[u], (unsigned) encode_buffer[u]);
        }                       /* end for */
    }                           /* end if */
    /* Test decoding macros */
    p = encode_buffer;
    INT16DECODE(p, di16);       /* Decode the int16 value */
    UINT16DECODE(p, du16);      /* Decode the uint16 value */
    INT32DECODE(p, di32);       /* Decode the int32 value */
    UINT32DECODE(p, du32);      /* Decode the uint32 value */

    /* Check the values decoded */
    if (di16 != TEST_INT16_VALUE)
        TestErrPrintf("Error decoding int16 meta-data wanted: %d, got: %d "
                   "at %s:%d\n", (int) TEST_INT16_VALUE, (int) di16,
                   __FILE__, __LINE__);
    if (du16 != TEST_UINT16_VALUE)
        TestErrPrintf("Error decoding uint16 meta-data wanted: %u, got: %u "
                   "at %s:%d\n", (unsigned) TEST_UINT16_VALUE, (unsigned) du16,
                   __FILE__, __LINE__);
    if (di32 != TEST_INT32_VALUE)
        TestErrPrintf("Error decoding int32 meta-data wanted: %ld, got: %ld "
                   "at %s:%d\n", (long) TEST_INT32_VALUE, (long) di32,
                   __FILE__, __LINE__);
    if (du32 != TEST_UINT32_VALUE)
        TestErrPrintf("Error decoding uint32 meta-data wanted: %lu, got: %lu "
                   "at %s:%d\n", (unsigned long) TEST_UINT32_VALUE, (unsigned long) du32,
                   __FILE__, __LINE__);
}                               /* test_metadata() */


/*-------------------------------------------------------------------------
 * Function:	cleanup_metadata
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	Albert Cheng
 *              July 2, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_metadata(void)
{
    /* no file to clean */
}

