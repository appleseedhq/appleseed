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
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Tuesday, June 16, 1998
 *
 * Purpose:	Tests functions in H5Tbit.c
 */
#include "h5test.h"

#define H5T_PACKAGE
#include "H5Tpkg.h"

#define NTESTS	100000


/*-------------------------------------------------------------------------
 * Function:	test_find
 *
 * Purpose:	Test bit find operations.  This is just the basic stuff; more
 *		rigorous testing will be performed by the other test
 *		functions.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Tuesday, June 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_find (void)
{
    uint8_t	v1[8];
    int	i;
    ssize_t	n;

    TESTING("bit search operations");

    /* The zero length buffer */
    HDmemset(v1, 0xaa, sizeof v1);
    n = H5T_bit_find(v1, (size_t)0, (size_t)0, H5T_BIT_LSB, TRUE);
    if(-1 != n) {
	H5_FAILED();
	puts ("    Zero length test failed (lsb)!");
	goto failed;
    }
    n = H5T_bit_find(v1, (size_t)0, (size_t)0, H5T_BIT_MSB, TRUE);
    if(-1 != n) {
	H5_FAILED();
	puts ("    Zero length test failed (msb)!");
	goto failed;
    }


    /* The zero buffer */
    HDmemset(v1, 0, sizeof v1);
    n = H5T_bit_find(v1, (size_t)0, 8 * sizeof(v1), H5T_BIT_LSB, TRUE);
    if(-1 != n) {
	H5_FAILED();
	puts ("    Zero buffer test failed (lsb)!");
	goto failed;
    }
    n = H5T_bit_find(v1, (size_t)0, 8 * sizeof(v1), H5T_BIT_MSB, TRUE);
    if(-1 != n) {
	H5_FAILED();
	puts ("    Zero buffer test failed (msb)!");
	goto failed;
    }

    /* Try all combinations of one byte */
    for(i = 0; i < 8 * (int)sizeof(v1); i++) {
	HDmemset(v1, 0, sizeof v1);
	v1[i / 8] = 1 << (i % 8);
	n = H5T_bit_find(v1, (size_t)0, 8 * sizeof(v1), H5T_BIT_LSB, TRUE);
	if((ssize_t)i != n) {
	    H5_FAILED();
	    printf ("    Test for set bit %d failed (lsb)!\n", i);
	    goto failed;
	}
	n = H5T_bit_find(v1, (size_t)0, 8 * sizeof(v1), H5T_BIT_MSB, TRUE);
	if((ssize_t)i != n) {
	    H5_FAILED();
	    printf ("    Test for set bit %d failed (msb)!\n", i);
	    goto failed;
	}
    }

    /* The one buffer */
    HDmemset(v1, 0xff, sizeof v1);
    n = H5T_bit_find(v1, (size_t)0, 8 * sizeof(v1), H5T_BIT_LSB, FALSE);
    if(-1 != n) {
	H5_FAILED();
	puts ("    One buffer test failed (lsb)!");
	goto failed;
    }
    n = H5T_bit_find(v1, (size_t)0, 8 * sizeof(v1), H5T_BIT_MSB, FALSE);
    if(-1 != n) {
	H5_FAILED();
	puts ("    One buffer test failed (msb)!");
	goto failed;
    }

    /* Try all combinations of one byte */
    for (i=0; i<8*(int)sizeof(v1); i++) {
	memset (v1, 0xff, sizeof v1);
	v1[i/8] &= ~(1<<(i%8));
	n = H5T_bit_find (v1, (size_t)0, 8*sizeof(v1), H5T_BIT_LSB, FALSE);
	if ((ssize_t)i!=n) {
	    H5_FAILED();
	    printf ("    Test for clear bit %d failed (lsb)!\n", i);
	    goto failed;
	}
	n = H5T_bit_find (v1, (size_t)0, 8*sizeof(v1), H5T_BIT_MSB, FALSE);
	if ((ssize_t)i!=n) {
	    H5_FAILED();
	    printf ("    Test for clear bit %d failed (lsb)!\n", i);
	    goto failed;
	}
    }


    PASSED();
    return 0;

 failed:
    printf ("    v = 0x");
    for (i=0; i<(int)sizeof(v1); i++) printf ("%02x", v1[i]);
    printf ("\n");
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_copy
 *
 * Purpose:	Test bit copy operations.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Tuesday, June 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_copy (void)
{
    uint8_t	v1[8], v2[8];
    size_t	s_offset, d_offset, size;
    int	i, j;
    ssize_t	n;

    TESTING("bit copy operations");

    for (i=0; i<NTESTS; i++) {
	s_offset = HDrand() % (8*sizeof v1);
	d_offset = HDrand() % (8*sizeof v2);
	size = (unsigned)HDrand() % MIN (8*sizeof(v1), 8*sizeof(v2));
	size = MIN3 (size, 8*sizeof(v1)-s_offset, 8*sizeof(v2)-d_offset);
	memset (v1, 0xff, sizeof v1);
	memset (v2, 0x00, sizeof v2);

	/* Copy some bits to v2 and make sure something was copied */
	H5T_bit_copy (v2, d_offset, v1, s_offset, size);
	for (j=0; j<(int)sizeof(v2); j++) if (v2[j]) break;
	if (size>0 && j>=(int)sizeof(v2)) {
	    H5_FAILED();
	    puts ("    Unabled to find copied region in destination");
	    goto failed;
	}
	if (0==size && j<(int)sizeof(v2)) {
	    H5_FAILED();
	    puts ("    Found copied bits when we shouldn't have");
	    goto failed;
	}


	/* Look for the zeros and ones */
	n = H5T_bit_find (v2, (size_t)0, 8*sizeof(v2), H5T_BIT_LSB, 1);
	if (size>0 && n!=(ssize_t)d_offset) {
	    H5_FAILED();
	    printf ("    Unable to find first copied bit in destination "
		    "(n=%d)\n", (int)n);
	    goto failed;
	}
	if (0==size && n>=0) {
	    H5_FAILED();
	    puts ("    Found copied bits and shouldn't have!");
	    goto failed;
	}
	n = H5T_bit_find (v2, d_offset, 8*sizeof(v2)-d_offset, H5T_BIT_LSB, 0);
	if (d_offset+size<8*sizeof(v2) && n!=(ssize_t)size) {
	    H5_FAILED();
	    printf ("    Unable to find last copied bit in destination "
		    "(n=%d)\n", (int)n);
	    goto failed;
	}
	if (d_offset+size==8*sizeof(v2) && n>=0) {
	    H5_FAILED();
	    puts ("    High-order zeros are present and shouldn't be!");
	    goto failed;
	}

	/*
	 * Look for zeros and ones in reverse order.  This is only to test
	 * that reverse searches work as expected.
	 */
	n = H5T_bit_find (v2, (size_t)0, 8*sizeof(v2), H5T_BIT_MSB, 1);
	if (size>0 && (size_t)(n+1)!=d_offset+size) {
	    H5_FAILED();
	    printf ("    Unable to find last copied bit in destination "
		    "(reverse, n=%d)\n", (int)n);
	    goto failed;
	}
	if (0==size && n>=0) {
	    H5_FAILED();
	    puts ("    Found copied bits but shouldn't have (reverse)!");
	    goto failed;
	}
	n = H5T_bit_find (v2, (size_t)0, d_offset+size, H5T_BIT_MSB, 0);
	if (d_offset>0 && n+1!=(ssize_t)d_offset) {
	    H5_FAILED();
	    printf ("    Unable to find beginning of copied data "
		    "(reverse, n=%d)\n", (int)n);
	    goto failed;
	}
	if (0==d_offset && n>=0) {
	    H5_FAILED();
	    puts ("    Found leading original data but shouldn't have!");
	    goto failed;
	}

    }

    PASSED();
    return 0;

 failed:
    printf ("    i=%d, s_offset=%lu, d_offset=%lu, size=%lu\n",
	    i, (unsigned long)s_offset, (unsigned long)d_offset,
	    (unsigned long)size);
    printf ("    s = 0x");
    for (j=sizeof(v1)-1; j>=0; --j) printf ("%02x", v1[j]);
    printf ("\n    d = 0x");
    for (j=sizeof(v2)-1; j>=0; --j) printf ("%02x", v2[j]);
    printf ("\n");
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_shift
 *
 * Purpose:	Test bit shifting operations.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *              Monday, April 12, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_shift (void)
{
    uint8_t	vector[8];
    size_t	offset, size;
    int	        i, j;
    ssize_t	shift_dist, n;

    TESTING("bit shift operations");

    for (i=0; i<NTESTS; i++) {
	offset = HDrand() % (8*sizeof vector);
	size = (unsigned)HDrand() % (8*sizeof(vector)-offset);
        /* Don't want size to be 0 */
        if(size == 0) continue;
        shift_dist = HDrand() % size;

	/*-------- LEFT-shift some bits and make sure something was shifted --------*/
	memset (vector, 0x00, sizeof vector);
        H5T_bit_set (vector, offset, size, 1);

        H5T_bit_shift (vector, shift_dist, offset, size);

	/* Look for the ones */
	n = H5T_bit_find (vector, (size_t)0, 8*sizeof(vector), H5T_BIT_LSB, 1);
	if ((size_t)n!=offset+shift_dist) {
	    H5_FAILED();
	    printf ("    Unable to find first bit in destination "
		    "(n=%d)\n", (int)n);
	    goto failed;
	}

	/*
	 * Look for zeros and ones in reverse order.  This is only to test
	 * that reverse searches work as expected.
	 */
	n = H5T_bit_find (vector, (size_t)0, 8*sizeof(vector), H5T_BIT_MSB, 1);
	if (n!=(ssize_t)(offset+size-1)) {
	    H5_FAILED();
	    printf ("    Unable to find last bit in destination "
		    "(reverse, n=%d)\n", (int)n);
	    goto failed;
	}

	/*-------- RIGHT-shift some bits and make sure something was shifted --------*/
	memset (vector, 0x00, sizeof vector);
        H5T_bit_set (vector, offset, size, 1);

        H5T_bit_shift (vector, -shift_dist, offset, size);

	/* Look for the ones */
	n = H5T_bit_find (vector, (size_t)0, 8*sizeof(vector), H5T_BIT_LSB, 1);
	if ((size_t)n!=offset) {
	    H5_FAILED();
	    printf ("    Unable to find first bit in destination "
		    "(n=%d)\n", (int)n);
	    goto failed;
	}

	/*
	 * Look for zeros and ones in reverse order.  This is only to test
	 * that reverse searches work as expected.
	 */
	n = H5T_bit_find (vector, (size_t)0, 8*sizeof(vector), H5T_BIT_MSB, 1);
	if (n!=(ssize_t)(offset+size-shift_dist-1)) {
	    H5_FAILED();
	    printf ("    Unable to find last bit in destination "
		    "(reverse, n=%d)\n", (int)n);
	    goto failed;
	}

        /*-------- Shift the bits out of sight --------*/
        /* A sequence 111111 will be 000000 if shift_dist=6 */

        /* Randomly decide shift direction */
        if(size % 2 == 0)
            shift_dist = size;
        else
            shift_dist = -((ssize_t)size);

	memset (vector, 0x00, sizeof vector);
        H5T_bit_set (vector, offset, size, 1);

        H5T_bit_shift (vector, shift_dist, offset, size);

	/* Supposed to fail to find any ones */
	n = H5T_bit_find (vector, (size_t)0, 8*sizeof(vector), H5T_BIT_LSB, 1);
	if (n >= 0) {
	    H5_FAILED();
	    printf ("    Unable to verify all bits are zero in destination(LSB) "
		    "(n=%d)\n", (int)n);
	    goto failed;
	}

        /* Look from the other direction */
	n = H5T_bit_find (vector, (size_t)0, 8*sizeof(vector), H5T_BIT_MSB, 1);
	if (n >= 0) {
	    H5_FAILED();
	    printf ("    Unable to verify all bits are zero in destination(MSB) "
		    "(n=%d)\n", (int)n);
	    goto failed;
	}
    }

    PASSED();
    return 0;

 failed:
    printf ("    i=%d, offset=%lu, size=%lu, shift_dist=%lu\n",
	    i, (unsigned long)offset, (unsigned long)size,
	    (unsigned long)shift_dist);
    for (j=sizeof(vector)-1; j>=0; --j) printf ("%02x", vector[j]);
    printf ("\n");
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_increment
 *
 * Purpose:	Test operation to increment bit vector by 1.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *              Monday, April 12, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_increment (void)
{
    uint8_t	vector[8];
    size_t	offset, size;
    int	        i, j;
    ssize_t	n;

    TESTING("bit increment operations");

    for (i=0; i<NTESTS; i++) {
	offset = HDrand() % (8*sizeof vector);
	size = (unsigned)HDrand() % (8*sizeof(vector)-offset);
        /* Don't want size to be 0 */
        if(size == 0) continue;

	memset (vector, 0x00, sizeof vector);
        if(size>1)  /* if size=6, make a sequence like 011111 */
            H5T_bit_set (vector, offset, size-1, 1);
        else  /* if size=1, just set this one bit to 1 */
            H5T_bit_set (vector, offset, size, 1);

	/* Increment the sequence by one */
        H5T_bit_inc (vector, offset, size);

	/* Look for the one */
	n = H5T_bit_find (vector, (size_t)0, 8*sizeof(vector), H5T_BIT_LSB, 1);
	if (size!=1 && (size_t)n!=offset+size-1) {
	    H5_FAILED();
	    printf ("    Unable to find first bit in destination "
		    "(n=%d)\n", (int)n);
	    goto failed;
	}
        if(size==1 && n>=0) {
	    H5_FAILED();
	    printf ("    Unable to verify all-zero bit in destination "
		    "(n=%d)\n", (int)n);
	    goto failed;
	}

	/*
	 * Look for one in reverse order.  This is only to test
	 * that reverse searches work as expected.
	 */
	n = H5T_bit_find (vector, (size_t)0, 8*sizeof(vector), H5T_BIT_MSB, 1);
	if (size!=1 && n!=(ssize_t)(offset+size-1)) {
	    H5_FAILED();
	    printf ("    Unable to find last bit in destination "
		    "(reverse, n=%d)\n", (int)n);
	    goto failed;
	}
        if(size==1 && n>=0) {
	    H5_FAILED();
	    printf ("    Unable to verify all-zero bit in destination "
		    "(reverse, n=%d)\n", (int)n);
	    goto failed;
	}
    }

    PASSED();
    return 0;

 failed:
    printf ("    i=%d, offset=%lu, size=%lu\n",
	    i, (unsigned long)offset, (unsigned long)size);
    for (j=sizeof(vector)-1; j>=0; --j) printf ("%02x", vector[j]);
    printf ("\n");
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_decrement
 *
 * Purpose:	Test operation to decrement bit vector by 1.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *              Monday, April 12, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_decrement (void)
{
    uint8_t	vector[8];
    size_t	offset, size;
    int	        i, j;
    ssize_t	n;

    TESTING("bit decrement operations");

    for (i=0; i<NTESTS; i++) {
	offset = HDrand() % (8*sizeof vector);
	size = (unsigned)HDrand() % (8*sizeof(vector)-offset);
        /* Don't want size to be 0 */
        if(size == 0) continue;

        /* All-zero sequence will become 111111(size=6) after decrement */
	memset (vector, 0x00, sizeof vector);

	/* decrement the sequence by one */
        H5T_bit_dec (vector, offset, size);

	/* Look for the ones */
	n = H5T_bit_find (vector, (size_t)0, 8*sizeof(vector), H5T_BIT_LSB, 1);
	if ((size_t)n!=offset) {
	    H5_FAILED();
	    printf ("    Unable to find first bit in destination "
		    "(n=%d)\n", (int)n);
	    goto failed;
	}

	/*
	 * Look for zeros and ones in reverse order.  This is only to test
	 * that reverse searches work as expected.
	 */
	n = H5T_bit_find (vector, (size_t)0, 8*sizeof(vector), H5T_BIT_MSB, 1);
	if (n!=(ssize_t)(offset+size-1)) {
	    H5_FAILED();
	    printf ("    Unable to find last bit in destination "
		    "(reverse, n=%d)\n", (int)n);
	    goto failed;
	}
    }

    PASSED();
    return 0;

 failed:
    printf ("    i=%d, offset=%lu, size=%lu\n",
	    i, (unsigned long)offset, (unsigned long)size);
    for (j=sizeof(vector)-1; j>=0; --j) printf ("%02x", vector[j]);
    printf ("\n");
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_negate
 *
 * Purpose:	Test operation to negate bit vector.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *              Monday, April 12, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_negate (void)
{
    uint8_t	vector[8];
    size_t	offset, size;
    int	        i, j;
    ssize_t	n;

    TESTING("bit negate operations");

    for (i=0; i<NTESTS; i++) {
	offset = HDrand() % (8*sizeof vector);
	size = (unsigned)HDrand() % (8*sizeof(vector)-offset);
        /* Don't want size to be 0 */
        if(size == 0) continue;

        /* All-zero sequence will become 111111(size=6) after negating */
	memset (vector, 0x00, sizeof vector);

	/* negate the sequence */
        H5T_bit_neg (vector, offset, size);

	/* Look for the ones */
	n = H5T_bit_find (vector, (size_t)0, 8*sizeof(vector), H5T_BIT_LSB, 1);
	if ((size_t)n!=offset) {
	    H5_FAILED();
	    printf ("    Unable to find first bit in destination "
		    "(n=%d)\n", (int)n);
	    goto failed;
	}

	/*
	 * Look for zeros and ones in reverse order.  This is only to test
	 * that reverse searches work as expected.
	 */
	n = H5T_bit_find (vector, (size_t)0, 8*sizeof(vector), H5T_BIT_MSB, 1);
	if (n!=(ssize_t)(offset+size-1)) {
	    H5_FAILED();
	    printf ("    Unable to find last bit in destination "
		    "(reverse, n=%d)\n", (int)n);
	    goto failed;
	}

        /* All-one sequence will become 000000(size=6) after negating */
	memset (vector, 0x00, sizeof vector);
        H5T_bit_set (vector, offset, size, 1);

	/* negate the sequence */
        H5T_bit_neg (vector, offset, size);

	/* Look for the ones */
	n = H5T_bit_find (vector, (size_t)0, 8*sizeof(vector), H5T_BIT_LSB, 1);
	if (n>=0) {
	    H5_FAILED();
	    printf ("    Unable to verify all-zero bits in destination "
		    "(n=%d)\n", (int)n);
	    goto failed;
	}

	/*
	 * Look for ones in reverse order.  This is only to test
	 * that reverse searches work as expected.
	 */
	n = H5T_bit_find (vector, (size_t)0, 8*sizeof(vector), H5T_BIT_MSB, 1);
	if (n>=0) {
	    H5_FAILED();
	    printf ("    Unable to verify all-zero bits in destination "
		    "(reverse, n=%d)\n", (int)n);
	    goto failed;
	}
    }

    PASSED();
    return 0;

 failed:
    printf ("    i=%d, offset=%lu, size=%lu\n",
	    i, (unsigned long)offset, (unsigned long)size);
    for (j=sizeof(vector)-1; j>=0; --j) printf ("%02x", vector[j]);
    printf ("\n");
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_set
 *
 * Purpose:	Test bit set operations
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Tuesday, June 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_set (void)
{
    uint8_t	v2[8];
    size_t	d_offset, size;
    int	i, j;
    ssize_t	n;

    TESTING("bit set operations");

    for (i=0; i<NTESTS; i++) {
	d_offset = HDrand() % (8*sizeof v2);
	size = (unsigned)HDrand() % (8*sizeof(v2));
	size = MIN (size, 8*sizeof(v2)-d_offset);
	memset (v2, 0x00, sizeof v2);

	/* Set some bits in v2 */
	H5T_bit_set (v2, d_offset, size, TRUE);
	for (j=0; j<(int)sizeof(v2); j++) if (v2[j]) break;
	if (size>0 && j>=(int)sizeof(v2)) {
	    H5_FAILED();
	    puts ("    Unabled to find set region in buffer");
	    goto failed;
	}
	if (0==size && j<(int)sizeof(v2)) {
	    H5_FAILED();
	    puts ("    Found set bits when we shouldn't have");
	    goto failed;
	}


	/* Look for the zeros and ones */
	n = H5T_bit_find (v2, (size_t)0, 8*sizeof(v2), H5T_BIT_LSB, 1);
	if (size>0 && n!=(ssize_t)d_offset) {
	    H5_FAILED();
	    printf ("    Unable to find first set bit in destination "
		    "(n=%d)\n", (int)n);
	    goto failed;
	}
	if (0==size && n>=0) {
	    H5_FAILED();
	    puts ("    Found set bits and shouldn't have!");
	    goto failed;
	}
	n = H5T_bit_find (v2, d_offset, 8*sizeof(v2)-d_offset, H5T_BIT_LSB, 0);
	if (d_offset+size<8*sizeof(v2) && n!=(ssize_t)size) {
	    H5_FAILED();
	    printf ("    Unable to find last set bit in destination "
		    "(n=%d)\n", (int)n);
	    goto failed;
	}
	if (d_offset+size==8*sizeof(v2) && n>=0) {
	    H5_FAILED();
	    puts ("    High-order zeros are present and shouldn't be!");
	    goto failed;
	}

	/*
	 * Look for zeros and ones in reverse order.  This is only to test
	 * that reverse searches work as expected.
	 */
	n = H5T_bit_find (v2, (size_t)0, 8*sizeof(v2), H5T_BIT_MSB, 1);
	if (size>0 && (size_t)(n+1)!=d_offset+size) {
	    H5_FAILED();
	    printf ("    Unable to find last set bit in destination "
		    "(reverse, n=%d)\n", (int)n);
	    goto failed;
	}
	if (0==size && n>=0) {
	    H5_FAILED();
	    puts ("    Found set bits but shouldn't have (reverse)!");
	    goto failed;
	}
	n = H5T_bit_find (v2, (size_t)0, d_offset+size, H5T_BIT_MSB, 0);
	if (d_offset>0 && n+1!=(ssize_t)d_offset) {
	    H5_FAILED();
	    printf ("    Unable to find beginning of set bit region "
		    "(reverse, n=%d)\n", (int)n);
	    goto failed;
	}
	if (0==d_offset && n>=0) {
	    H5_FAILED();
	    puts ("    Found leading zeros but shouldn't have!");
	    goto failed;
	}

    }

    PASSED();
    return 0;

 failed:
    printf ("    i=%d, d_offset=%lu, size=%lu\n",
	    i, (unsigned long)d_offset, (unsigned long)size);
    printf ("    d = 0x");
    for (j=sizeof(v2)-1; j>=0; --j) printf ("%02x", v2[j]);
    printf ("\n");
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_clear
 *
 * Purpose:	Test bit clear operations
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Tuesday, June 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_clear (void)
{
    uint8_t	v2[8];
    size_t	d_offset, size;
    int	i, j;
    ssize_t	n;

    TESTING("bit clear operations");

    for (i=0; i<NTESTS; i++) {
	d_offset = HDrand() % (8*sizeof v2);
	size = (unsigned)HDrand() % (8*sizeof(v2));
	size = MIN (size, 8*sizeof(v2)-d_offset);
	memset (v2, 0xff, sizeof v2);

	/* Clear some bits in v2 */
	H5T_bit_set (v2, d_offset, size, FALSE);
	for (j=0; j<(int)sizeof(v2); j++) if (0xff!=v2[j]) break;
	if (size>0 && j>=(int)sizeof(v2)) {
	    H5_FAILED();
	    puts ("    Unabled to find cleared region in buffer");
	    goto failed;
	}
	if (0==size && j<(int)sizeof(v2)) {
	    H5_FAILED();
	    puts ("    Found cleared bits when we shouldn't have");
	    goto failed;
	}


	/* Look for the zeros and ones */
	n = H5T_bit_find (v2, (size_t)0, 8*sizeof(v2), H5T_BIT_LSB, 0);
	if (size>0 && n!=(ssize_t)d_offset) {
	    H5_FAILED();
	    printf ("    Unable to find first cleared bit in destination "
		    "(n=%d)\n", (int)n);
	    goto failed;
	}
	if (0==size && n>=0) {
	    H5_FAILED();
	    puts ("    Found cleared bits and shouldn't have!");
	    goto failed;
	}
	n = H5T_bit_find (v2, d_offset, 8*sizeof(v2)-d_offset, H5T_BIT_LSB, 1);
	if (d_offset+size<8*sizeof(v2) && n!=(ssize_t)size) {
	    H5_FAILED();
	    printf ("    Unable to find last cleared bit in destination "
		    "(n=%d)\n", (int)n);
	    goto failed;
	}
	if (d_offset+size==8*sizeof(v2) && n>=0) {
	    H5_FAILED();
	    puts ("    High-order ones are present and shouldn't be!");
	    goto failed;
	}

	/*
	 * Look for zeros and ones in reverse order.  This is only to test
	 * that reverse searches work as expected.
	 */
	n = H5T_bit_find (v2, (size_t)0, 8*sizeof(v2), H5T_BIT_MSB, 0);
	if (size>0 && (size_t)(n+1)!=d_offset+size) {
	    H5_FAILED();
	    printf ("    Unable to find last cleared bit in destination "
		    "(reverse, n=%d)\n", (int)n);
	    goto failed;
	}
	if (0==size && n>=0) {
	    H5_FAILED();
	    puts ("    Found cleared bits but shouldn't have (reverse)!");
	    goto failed;
	}
	n = H5T_bit_find (v2, (size_t)0, d_offset+size, H5T_BIT_MSB, 1);
	if (d_offset>0 && n+1!=(ssize_t)d_offset) {
	    H5_FAILED();
	    printf ("    Unable to find beginning of cleared bit region "
		    "(reverse, n=%d)\n", (int)n);
	    goto failed;
	}
	if (0==d_offset && n>=0) {
	    H5_FAILED();
	    puts ("    Found leading ones but shouldn't have!");
	    goto failed;
	}

    }

    PASSED();
    return 0;

 failed:
    printf ("    i=%d, d_offset=%lu, size=%lu\n",
	    i, (unsigned long)d_offset, (unsigned long)size);
    printf ("    d = 0x");
    for (j=sizeof(v2)-1; j>=0; --j) printf ("%02x", v2[j]);
    printf ("\n");
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:
 *
 * Return:	Success:
 *
 *		Failure:
 *
 * Programmer:	Robb Matzke
 *              Tuesday, June 16, 1998
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    int	nerrors = 0;

    /*
     * Open the library explicitly.
     */
    H5open();

    nerrors += test_find() < 0 ? 1 : 0;
    nerrors += test_set() < 0 ? 1 : 0;
    nerrors += test_clear() < 0 ? 1 : 0;
    nerrors += test_copy() < 0 ? 1 : 0;
    nerrors += test_shift() < 0 ? 1 : 0;
    nerrors += test_increment() < 0 ? 1 : 0;
    nerrors += test_decrement() < 0 ? 1 : 0;
    nerrors += test_negate() < 0 ? 1 : 0;

    if(nerrors) {
        printf("***** %u FAILURE%s! *****\n",
               nerrors, 1 == nerrors ? "" : "S");
        exit(1);
    }
    printf("All bit tests passed.\n");

    H5close();

    return 0;
}

