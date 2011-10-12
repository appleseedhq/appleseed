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
 *              Friday, January 23, 1998
 */

/* See H5private.h for how to include headers */
#undef NDEBUG

#define H5T_PACKAGE
#include "H5Tpkg.h"		/*to turn off hardware conversions*/
#include "H5Iprivate.h"

#include "h5test.h"

const char *FILENAME[] = {
    "cmpd_dset",
    "src_subset",
    "dst_subset",
    NULL
};

const char *DSET_NAME[] = {
    "contig_src_subset",
    "chunk_src_subset",
    "contig_dst_subset",
    "chunk_dst_subset",
    NULL
};

/* The first dataset */
typedef struct s1_t {
    unsigned int a;
    unsigned int b;
    unsigned int c[4];
    unsigned int d;
    unsigned int e;
} s1_t;

/* The second dataset (same as first) */
typedef s1_t s2_t;

/* The third dataset (reversed fields of s1) */
typedef struct s3_t {
    unsigned int e;
    unsigned int d;
    unsigned int c[4];
    unsigned int b;
    unsigned int a;
} s3_t;

/* The fourth dataset (a subset of s1) */
typedef struct s4_t {
    unsigned int b;
    unsigned int d;
} s4_t;

/* The fifth dataset (a superset of s1) */
typedef struct s5_t {
    unsigned int pre;
    unsigned int a;
    unsigned int b;
    unsigned int mid1;
    unsigned int c[4];
    unsigned int mid2;
    unsigned int d;
    unsigned int e;
    unsigned int post;
} s5_t;

/* The sixth dataset (a superset of s1).  This is for
 * testing the optimization for the Chicago company. */
typedef struct s6_t {
    unsigned int a;
    unsigned int b;
    unsigned int c[4];
    unsigned int d;
    unsigned int e;
    unsigned int pre;
    unsigned int mid1;
    unsigned int mid2;
    unsigned int post;
} s6_t;

/* Structures for testing the optimization for the Chicago company. */
typedef struct {
    int a, b, c[8], d, e;
    float f, g, h[16], i, j;
    double k, l, m, n;
} stype1;
typedef struct {
    int a, b, c[8], d, e;
    float f, g, h[16], i, j;
    double k, l, m, n;
    long o, p, q;
} stype2;
typedef struct {
    int a, b, c[8], d, e;
} stype3;
typedef struct {
    int a, b, c[8], d, e;
    float f, g, h[16], i, j;
    double k, l, m, n;
    long o, p, q;
    long long r, s, t;
} stype4;

#define NX	100u
#define NY	2000u
#define PACK_NMEMBS     100


/*-------------------------------------------------------------------------
 * Function:	test_compound
 *
 * Purpose:	Creates a simple dataset of a compound type and then reads
 *		it back.  The dataset is read back in various ways to
 *		exercise the I/O pipeline and compound type conversion.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Robb Matzke
 *              Friday, January 23, 1998
 *
 * Modifications:
 *		Robb Matzke, 1999-06-23
 *		If the command line switch `--noopt' is present then the fast
 *		compound datatype conversion is turned off.
 *
 *              Raymond Lu, 15 June 2007
 *              Moved this part of code from MAIN to TEST_COMPOUND function.
 *-------------------------------------------------------------------------
 */
static int
test_compound (char *filename, hid_t fapl)
{
    /* First dataset */
    static s1_t		s1[NX*NY];
    hid_t		s1_tid;

    /* Second dataset */
    static s2_t		s2[NX*NY];
    hid_t		s2_tid;

    /* Third dataset */
    static s3_t		s3[NX*NY];
    hid_t		s3_tid;

    /* Fourth dataset */
    static s4_t		s4[NX*NY];
    hid_t		s4_tid;

    /* Fifth dataset */
    static s5_t		s5[NX*NY];
    hid_t		s5_tid;

    static s6_t		s6[NX*NY];
    hid_t		s6_tid;


    /* Sixth dataset */

    /* Seventh dataset */
    hid_t		s7_sid;

    /* Eighth dataset */
    s1_t		*s8 = NULL;
    hid_t		s8_f_sid;	/*file data space		*/
    hid_t		s8_m_sid;	/*memory data space		*/

    /* Ninth dataset */

    /* Tenth dataset */

    /* Eleventh dataset */
    s4_t		*s11 = NULL;

    /* Other variables */
    unsigned int	i, j;
    hid_t		file, dataset, space, PRESERVE;
    hid_t       array_dt;
    static hsize_t	dim[] = {NX, NY};
    hsize_t 		f_offset[2];	/*offset of hyperslab in file	*/
    hsize_t 		h_size[2];	/*size of hyperslab		*/
    hsize_t		memb_size[1] = {4};
    int			ret_code;

    /* Create the file */
    if ((file = H5Fcreate (filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) {
	goto error;
    }

    /* Create the data space */
    if ((space = H5Screate_simple (2, dim, NULL)) < 0) goto error;

    /* Create xfer properties to preserve initialized data */
    /* Also verify H5Pset_preserve is initially 0 and then is set to 1. */
    if ((PRESERVE = H5Pcreate (H5P_DATASET_XFER))<0) goto error;
    if ((ret_code=H5Pget_preserve (PRESERVE)) != 0){
	printf("Preserve status of dataset transfer property list should be"
	   " 0 (FALSE), got %d\n", ret_code);
	goto error;
    }
    if (H5Pset_preserve (PRESERVE, 1)<0) goto error;
    if ((ret_code=H5Pget_preserve (PRESERVE)) != 1){
	printf("Preserve status of dataset transfer property list should be"
	   " 1 (TRUE), got %d\n", ret_code);
	goto error;
    }

    /*
     *######################################################################
     * STEP 1: Save the original dataset natively.
     */
    TESTING("basic compound write");

    /* Initialize the dataset */
    for (i=0; i<NX*NY; i++) {
	s1[i].a = 8*i+0;
	s1[i].b = 2000+2*i;
	s1[i].c[0] = 8*i+2;
	s1[i].c[1] = 8*i+3;
	s1[i].c[2] = 8*i+4;
	s1[i].c[3] = 8*i+5;
	s1[i].d = 2001+2*i;
	s1[i].e = 8*i+7;
    }

    /* Create the memory data type */
    if((s1_tid = H5Tcreate(H5T_COMPOUND, sizeof(s1_t))) < 0)
        goto error;
    array_dt = H5Tarray_create2(H5T_NATIVE_INT, 1, memb_size);
    if(H5Tinsert(s1_tid, "a", HOFFSET(s1_t, a), H5T_NATIVE_INT) < 0 ||
            H5Tinsert(s1_tid, "b", HOFFSET(s1_t, b), H5T_NATIVE_INT) < 0 ||
            H5Tinsert(s1_tid, "c", HOFFSET(s1_t, c), array_dt) < 0 ||
            H5Tinsert(s1_tid, "d", HOFFSET(s1_t, d), H5T_NATIVE_INT) < 0 ||
            H5Tinsert(s1_tid, "e", HOFFSET(s1_t, e), H5T_NATIVE_INT) < 0)
        goto error;
    H5Tclose(array_dt);

    /* Create the dataset */
    if((dataset = H5Dcreate2(file, "s1", s1_tid, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
	goto error;

    /* Write the data */
    if(H5Dwrite(dataset, s1_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s1) < 0)
	goto error;

    PASSED();

    /*
     *######################################################################
     * STEP 2: We create a new type ID for the second dataset even though
     * 	       it's the same as the first just to test things better, but
     *	       in fact, we could have used s1_tid.
     */
    TESTING("basic compound read");

    /* Create a data type for s2 */
    if ((s2_tid = H5Tcreate (H5T_COMPOUND, sizeof(s2_t))) < 0)
        goto error;
    array_dt = H5Tarray_create2(H5T_NATIVE_INT, 1, memb_size);
    if (H5Tinsert (s2_tid, "a", HOFFSET(s2_t,a), H5T_NATIVE_INT) < 0 ||
            H5Tinsert (s2_tid, "b", HOFFSET(s2_t,b), H5T_NATIVE_INT) < 0 ||
            H5Tinsert (s2_tid, "c", HOFFSET(s2_t,c), array_dt) < 0 ||
            H5Tinsert (s2_tid, "d", HOFFSET(s2_t,d), H5T_NATIVE_INT) < 0 ||
            H5Tinsert (s2_tid, "e", HOFFSET(s2_t,e), H5T_NATIVE_INT) < 0)
        goto error;
    H5Tclose(array_dt);

    /* Read the data */
    if (H5Dread (dataset, s2_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s2) < 0) {
	goto error;
    }

    /* Compare s2 with s1.  They should be the same */
    for (i=0; i<NX*NY; i++) {
	if (s1[i].a!=s2[i].a ||
	    s1[i].b!=s2[i].b ||
	    s1[i].c[0]!=s2[i].c[0] ||
	    s1[i].c[1]!=s2[i].c[1] ||
	    s1[i].c[2]!=s2[i].c[2] ||
	    s1[i].c[3]!=s2[i].c[3] ||
	    s1[i].d!=s2[i].d ||
	    s1[i].e!=s2[i].e) {
	    H5_FAILED();
	    puts("    Incorrect values read from the file");
	    goto error;
	}
    }
    PASSED();

    /*
     *######################################################################
     * STEP 3: Read the dataset back into a third memory buffer. This buffer
     * 	       has the same data space but the data type is different: the
     *	       data type is a struct whose members are in the opposite order.
     */
    TESTING("reversal of struct members");

    /* Create a data type for s3 */
    if ((s3_tid = H5Tcreate (H5T_COMPOUND, sizeof(s3_t))) < 0)
        goto error;
    array_dt = H5Tarray_create2(H5T_NATIVE_INT, 1, memb_size);
    if (H5Tinsert (s3_tid, "a", HOFFSET(s3_t,a), H5T_NATIVE_INT) < 0 ||
            H5Tinsert (s3_tid, "b", HOFFSET(s3_t,b), H5T_NATIVE_INT) < 0 ||
            H5Tinsert (s3_tid, "c", HOFFSET(s3_t,c), array_dt) < 0 ||
            H5Tinsert (s3_tid, "d", HOFFSET(s3_t,d), H5T_NATIVE_INT) < 0 ||
            H5Tinsert (s3_tid, "e", HOFFSET(s3_t,e), H5T_NATIVE_INT) < 0)
        goto error;
    H5Tclose(array_dt);

    /* Read the data */
    if (H5Dread (dataset, s3_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s3) < 0) {
	goto error;
    }

    /* Compare s3 with s1.  They should be the same */
    for (i=0; i<NX*NY; i++) {
	if (s1[i].a!=s3[i].a ||
	    s1[i].b!=s3[i].b ||
	    s1[i].c[0]!=s3[i].c[0] ||
	    s1[i].c[1]!=s3[i].c[1] ||
	    s1[i].c[2]!=s3[i].c[2] ||
	    s1[i].c[3]!=s3[i].c[3] ||
	    s1[i].d!=s3[i].d ||
	    s1[i].e!=s3[i].e) {
	    H5_FAILED();
	    puts("    Incorrect values read from the file");
	    goto error;
	}
    }
    PASSED();

    /*
     *######################################################################
     * STEP 4: Read a subset of the members.  Of the <a,b,c,d,e> members
     *         stored on disk we'll read <b,d>.
     */
    TESTING("subset struct read");

    /* Create a datatype for s4 */
    if ((s4_tid = H5Tcreate (H5T_COMPOUND, sizeof(s4_t))) < 0) goto error;
    if (H5Tinsert (s4_tid, "b", HOFFSET(s4_t,b), H5T_NATIVE_INT) < 0) goto error;
    if (H5Tinsert (s4_tid, "d", HOFFSET(s4_t,d), H5T_NATIVE_INT) < 0) goto error;

    /* Read the data */
    if (H5Dread (dataset, s4_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s4) < 0) {
	goto error;
    }

    /* Compare s4 with s1 */
    for (i=0; i<NX*NY; i++) {
	if (s1[i].b!=s4[i].b ||
	    s1[i].d!=s4[i].d) {
	    H5_FAILED();
	    puts("    Incorrect values read from the file");
	    goto error;
	}
    }
    PASSED();

    /*
     *######################################################################
     * STEP 5: Read all the members into a struct which has other members
     * 	       which have already been initialized.
     */
    TESTING("partially initialized superset read");

    /* Initialize some members */
    for (i=0; i<NX*NY; i++) {
	s5[i].pre =  1000+4*i;
	s5[i].mid1 = 1001+4*i;
	s5[i].mid2 = 1002+4*i;
	s5[i].post = 1003+4*i;
    }

    /* Create a data type for s5 */
    if ((s5_tid = H5Tcreate (H5T_COMPOUND, sizeof(s5_t))) < 0)
        goto error;
    array_dt = H5Tarray_create2(H5T_NATIVE_INT, 1, memb_size);
    if (H5Tinsert (s5_tid, "a", HOFFSET(s5_t,a), H5T_NATIVE_INT) < 0 ||
            H5Tinsert (s5_tid, "b", HOFFSET(s5_t,b), H5T_NATIVE_INT) < 0 ||
            H5Tinsert (s5_tid, "c", HOFFSET(s5_t,c), array_dt) < 0 ||
            H5Tinsert (s5_tid, "d", HOFFSET(s5_t,d), H5T_NATIVE_INT) < 0 ||
            H5Tinsert (s5_tid, "e", HOFFSET(s5_t,e), H5T_NATIVE_INT))
        goto error;
    H5Tclose(array_dt);

    /* Read the data */
    if (H5Dread (dataset, s5_tid, H5S_ALL, H5S_ALL, PRESERVE, s5) < 0) {
	goto error;
    }

    /* Check that the data was read properly */
    for (i=0; i<NX*NY; i++) {
	if (s1[i].a!=s5[i].a ||
	    s1[i].b!=s5[i].b ||
	    s1[i].c[0]!=s5[i].c[0] ||
	    s1[i].c[1]!=s5[i].c[1] ||
	    s1[i].c[2]!=s5[i].c[2] ||
	    s1[i].c[3]!=s5[i].c[3] ||
	    s1[i].d!=s5[i].d ||
	    s1[i].e!=s5[i].e) {
	    H5_FAILED();
	    puts("    Incorrect values read from the file");
	    goto error;
	}
    }

    /* Check that no previous values were clobbered */
    for (i=0; i<NX*NY; i++) {
	if (s5[i].pre  != 1000+4*i ||
	    s5[i].mid1 != 1001+4*i ||
	    s5[i].mid2 != 1002+4*i ||
	    s5[i].post != 1003+4*i) {
	    H5_FAILED();
	    puts("    Memory values were clobbered");
	    goto error;
	}
    }
    PASSED();

    /*
     *######################################################################
     * STEP 6: Read all the members into a struct which has other members
     * 	       which have already been initialized.  This is to test the
     *         optimization for the Chicago company.  The optimization is
     *         for the special case when the source members are a subset of
     *         destination, and the order is the same, and no conversion
     *         is needed.  For example:
     *              struct source {            struct destination {
     *                  TYPE1 A;      -->          TYPE1 A;
     *                  TYPE2 B;      -->          TYPE2 B;
     *                  TYPE3 C;      -->          TYPE3 C;
     *              };                             TYPE4 D;
     *                                             TYPE5 E;
     *                                         };
     */
    TESTING("partially initialized superset optimized read");

    /* Initialize some members */
    for (i=0; i<NX*NY; i++) {
	s6[i].pre =  1000+4*i;
	s6[i].mid1 = 1001+4*i;
	s6[i].mid2 = 1002+4*i;
	s6[i].post = 1003+4*i;
    }

    /* Create a data type for s6 */
    if ((s6_tid = H5Tcreate (H5T_COMPOUND, sizeof(s6_t))) < 0)
        goto error;
    array_dt = H5Tarray_create2(H5T_NATIVE_INT, 1, memb_size);
    if (H5Tinsert (s6_tid, "a", HOFFSET(s6_t,a), H5T_NATIVE_INT) < 0 ||
            H5Tinsert (s6_tid, "b", HOFFSET(s6_t,b), H5T_NATIVE_INT) < 0 ||
            H5Tinsert (s6_tid, "c", HOFFSET(s6_t,c), array_dt) < 0 ||
            H5Tinsert (s6_tid, "d", HOFFSET(s6_t,d), H5T_NATIVE_INT) < 0 ||
            H5Tinsert (s6_tid, "e", HOFFSET(s6_t,e), H5T_NATIVE_INT) < 0 ||
            H5Tinsert (s6_tid, "pre", HOFFSET(s6_t,pre), H5T_NATIVE_INT) < 0 ||
            H5Tinsert (s6_tid, "mid1", HOFFSET(s6_t,mid1), H5T_NATIVE_INT) < 0 ||
            H5Tinsert (s6_tid, "mid2", HOFFSET(s6_t,mid2), H5T_NATIVE_INT) < 0 ||
            H5Tinsert (s6_tid, "post", HOFFSET(s6_t,post), H5T_NATIVE_INT) < 0)
        goto error;
    H5Tclose(array_dt);

    /* Read the data */
    if (H5Dread (dataset, s6_tid, H5S_ALL, H5S_ALL, PRESERVE, s6) < 0) {
	goto error;
    }

    /* Check that the data was read properly */
    for (i=0; i<NX*NY; i++) {
	if (s1[i].a!=s6[i].a ||
	    s1[i].b!=s6[i].b ||
	    s1[i].c[0]!=s6[i].c[0] ||
	    s1[i].c[1]!=s6[i].c[1] ||
	    s1[i].c[2]!=s6[i].c[2] ||
	    s1[i].c[3]!=s6[i].c[3] ||
	    s1[i].d!=s6[i].d ||
	    s1[i].e!=s6[i].e) {
	    H5_FAILED();
	    puts("    Incorrect values read from the file");
	    goto error;
	}
    }

    /* Check that no previous values were clobbered */
    for (i=0; i<NX*NY; i++) {
	if (s6[i].pre  != 1000+4*i ||
	    s6[i].mid1 != 1001+4*i ||
	    s6[i].mid2 != 1002+4*i ||
	    s6[i].post != 1003+4*i) {
	    H5_FAILED();
	    puts("    Memory values were clobbered");
	    goto error;
	}
    }
    PASSED();


    /*
     *######################################################################
     * STEP 7: Update fields `b' and `d' on the file leaving the other
     *         fields unchanged.  This tests member alignment and background
     *	       buffers.
     */
    TESTING("partially initialized superset write");

    /* Initialize `s4' with new values */
    for (i=0; i<NX*NY; i++) {
	s4[i].b = 8*i+1;
	s4[i].d = 8*i+6;
    }

    /* Write the data to file */
    if (H5Dwrite (dataset, s4_tid, H5S_ALL, H5S_ALL, PRESERVE, s4) < 0) {
	goto error;
    }

    /* Read the data back */
    if (H5Dread (dataset, s1_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s1) < 0) {
	goto error;
    }

    /* Compare */
    for (i=0; i<NX*NY; i++) {
	if (s1[i].a != 8*i+0 ||
	    s1[i].b != 8*i+1 ||
	    s1[i].c[0] != 8*i+2 ||
	    s1[i].c[1] != 8*i+3 ||
	    s1[i].c[2] != 8*i+4 ||
	    s1[i].c[3] != 8*i+5 ||
	    s1[i].d != 8*i+6 ||
	    s1[i].e != 8*i+7) {
	    H5_FAILED();
	    printf("    i==%u, row=%u, col=%u\n", i, i/NY, i%NY);
	    printf("    got: {%7d,%7d,[%7d,%7d,%7d,%7d],%7d,%7d}\n",
		   s1[i].a, s1[i].b, s1[i].c[0], s1[i].c[1], s1[i].c[2],
		   s1[i].c[3], s1[i].d, s1[i].e);
	    printf("    ans: {%7d,%7d,[%7d,%7d,%7d,%7d],%7d,%7d}\n",
		   8*i+0, 8*i+1, 8*i+2, 8*i+3, 8*i+4, 8*i+5, 8*i+6, 8*i+7);
	    goto error;
	}
    }
    PASSED();

    /*
     *######################################################################
     * STEP 8. Read the original dataset with an explicit data space.  Even
     * though these data spaces are equal it tests a different part of the
     * library.
     */
    TESTING("explicit data space");

    /* Create the data space */
    if ((s7_sid = H5Screate_simple (2, dim, NULL)) < 0) goto error;

    /* Read the dataset */
    if (H5Dread (dataset, s2_tid, s7_sid, H5S_ALL, H5P_DEFAULT, s2) < 0) {
	goto error;
    }

    /* Compare */
    for (i=0; i<NX*NY; i++) {
	if (s2[i].a != s1[i].a ||
	    s2[i].b != s1[i].b ||
	    s2[i].c[0] != s1[i].c[0] ||
	    s2[i].c[1] != s1[i].c[1] ||
	    s2[i].c[2] != s1[i].c[2] ||
	    s2[i].c[3] != s1[i].c[3] ||
	    s2[i].d != s1[i].d ||
	    s2[i].e != s1[i].e) {
	    H5_FAILED();
	    puts("    Incorrect values read from file");
	    goto error;
	}
    }
    PASSED();


    /*
     *######################################################################
     * STEP 9. Read a hyperslab of the file into a complete array in memory.
     * The hyperslab is the middle third of the array.
     */
    TESTING("hyperslab partial read to array");

    /* Create the file data space */
    if ((s8_f_sid = H5Dget_space (dataset)) < 0) goto error;
    f_offset[0] = NX/3;
    f_offset[1] = NY/3;
    h_size[0] = 2*NX/3 - f_offset[0];
    h_size[1] = 2*NY/3 - f_offset[1];
    if (H5Sselect_hyperslab (s8_f_sid, H5S_SELECT_SET, f_offset, NULL,
			     h_size, NULL) < 0) goto error;

    /* Create memory data space */
    if ((s8_m_sid = H5Screate_simple (2, h_size, NULL)) < 0) goto error;

    /* Read the dataset */
    s8 = (s1_t *) calloc ((size_t)(h_size[0]*h_size[1]), sizeof(s1_t));
    assert (s8);
    if (H5Dread (dataset, s1_tid, s8_m_sid, s8_f_sid, H5P_DEFAULT, s8) < 0) {
	goto error;
    }

    /* Compare */
    for (i=0; i<h_size[0]; i++) {
	for (j=0; j<h_size[1]; j++) {
	    s1_t *ps1 = s1 + (f_offset[0]+i)*NY + f_offset[1] + j;
	    s1_t *ps8 = s8 + i*h_size[1] + j;

	    if (ps8->a != ps1->a ||
		ps8->b != ps1->b ||
		ps8->c[0] != ps1->c[0] ||
		ps8->c[1] != ps1->c[1] ||
		ps8->c[2] != ps1->c[2] ||
		ps8->c[3] != ps1->c[3] ||
		ps8->d != ps1->d ||
		ps8->e != ps1->e) {
		H5_FAILED();
		puts("    Incorrect values read from file");
		goto error;
	    }
	}
    }

    free (s8);
    s8 = NULL;
    PASSED();


    /*
     *######################################################################
     * STEP 10.  Read a hyperslab of the file into a hyperslab of memory.  The
     * part of memory not read is already initialized and must not change.
     */
    TESTING("hyperslab partial read to another hyperslab");

    /* Initialize */
    for (i=0; i<NX*NY; i++) {
	s2[i].a = s2[i].b = s2[i].d = s2[i].e = (unsigned)(-1);
	s2[i].c[0] = s2[i].c[1] = s2[i].c[2] = s2[i].c[3] = (unsigned)(-1);
    }

    /* Read the hyperslab */
    if (H5Dread (dataset, s2_tid, s8_f_sid, s8_f_sid, H5P_DEFAULT, s2) < 0) {
	goto error;
    }

    /* Compare */
    for (i=0; i<NX; i++) {
	for (j=0; j<NY; j++) {
	    s1_t *ps1 = s1 + i*NY + j;
	    s2_t *ps2 = s2 + i*NY + j;
	    if (i>=f_offset[0] &&
		i<f_offset[0]+h_size[0] &&
		j>=f_offset[1] &&
		j<f_offset[1]+h_size[1]) {
		if (ps2->a != ps1->a ||
		    ps2->b != ps1->b ||
		    ps2->c[0] != ps1->c[0] ||
		    ps2->c[1] != ps1->c[1] ||
		    ps2->c[2] != ps1->c[2] ||
		    ps2->c[3] != ps1->c[3] ||
		    ps2->d != ps1->d ||
		    ps2->e != ps1->e) {
		    H5_FAILED();
		    puts("    Memory values clobbered");
		    goto error;
		}
	    } else {
		if (ps2->a != (unsigned)(-1) ||
		    ps2->b != (unsigned)(-1) ||
		    ps2->c[0] != (unsigned)(-1) ||
		    ps2->c[1] != (unsigned)(-1) ||
		    ps2->c[2] != (unsigned)(-1) ||
		    ps2->c[3] != (unsigned)(-1) ||
		    ps2->d != (unsigned)(-1) ||
		    ps2->e != (unsigned)(-1)) {
		    H5_FAILED();
		    puts("    Incorrect values read from file");
		    goto error;
		}
	    }
	}
    }
    PASSED();

    /*
     *######################################################################
     * STEP 11. Same as step 9 except the memory array contains some members
     * which are already initialized, like step 5.
     */
    TESTING("hyperslab to hyperslab part initialized read");

    /* Initialize */
    for (i=0; i<NX*NY; i++) {
	s5[i].a = s5[i].b = s5[i].d = s5[i].e = (unsigned)(-1);
	s5[i].c[0] = s5[i].c[1] = s5[i].c[2] = s5[i].c[3] = (unsigned)(-1);
	s5[i].pre = s5[i].mid1 = s5[i].mid2 = s5[i].post = (unsigned)(-1);
    }

    /* Read the hyperslab */
    if (H5Dread (dataset, s5_tid, s8_f_sid, s8_f_sid, PRESERVE, s5) < 0) {
	goto error;
    }

    /* Compare */
    for (i=0; i<NX; i++) {
	for (j=0; j<NY; j++) {
	    s1_t *ps1 = s1 + i*NY + j;
	    s5_t *ps5 = s5 + i*NY + j;
	    if (i>=f_offset[0] &&
		i<f_offset[0]+h_size[0] &&
		j>=f_offset[1] &&
		j<f_offset[1]+h_size[1]) {
		if (ps5->pre != (unsigned)(-1) ||
		    ps5->a != ps1->a ||
		    ps5->b != ps1->b ||
		    ps5->mid1 != (unsigned)(-1) ||
		    ps5->c[0] != ps1->c[0] ||
		    ps5->c[1] != ps1->c[1] ||
		    ps5->c[2] != ps1->c[2] ||
		    ps5->c[3] != ps1->c[3] ||
		    ps5->mid2 != (unsigned)(-1) ||
		    ps5->d != ps1->d ||
		    ps5->e != ps1->e ||
		    ps5->post != (unsigned)(-1)) {
		    H5_FAILED();
		    puts("    Memory values clobbered");
		    goto error;
		}
	    } else {
		if (ps5->pre != (unsigned)(-1) ||
		    ps5->a != (unsigned)(-1) ||
		    ps5->b != (unsigned)(-1) ||
		    ps5->mid1 != (unsigned)(-1) ||
		    ps5->c[0] != (unsigned)(-1) ||
		    ps5->c[1] != (unsigned)(-1) ||
		    ps5->c[2] != (unsigned)(-1) ||
		    ps5->c[3] != (unsigned)(-1) ||
		    ps5->mid2 != (unsigned)(-1) ||
		    ps5->d != (unsigned)(-1) ||
		    ps5->e != (unsigned)(-1) ||
		    ps5->post != (unsigned)(-1)) {
		    H5_FAILED();
		    puts("    Incorrect values read from file");
		    goto error;
		}
	    }
	}
    }
    PASSED();

    /*
     *######################################################################
     * Step 12: Write an array into the middle third of the dataset
     * initializeing only members `b' and `d' to -1.
     */
    TESTING("hyperslab part initialized write");

    /* Create the memory array and initialize all fields to zero */
    f_offset[0] = NX/3;
    f_offset[1] = NY/3;
    h_size[0] = 2*NX/3 - f_offset[0];
    h_size[1] = 2*NY/3 - f_offset[1];
    s11 = (s4_t *) malloc ((size_t)h_size[0]*(size_t)h_size[1]*sizeof(s4_t));
    assert (s11);

    /* Initialize */
    for (i=0; i<h_size[0]*h_size[1]; i++) {
	s11[i].b = s11[i].d = (unsigned)(-1);
    }

    /* Write to disk */
    if (H5Dwrite (dataset, s4_tid, s8_m_sid, s8_f_sid, PRESERVE, s11) < 0) {
	goto error;
    }
    free (s11);
    s11=NULL;

    /* Read the whole thing */
    if (H5Dread (dataset, s1_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s1) < 0) {
	goto error;
    }

    /* Compare */
    for (i=0; i<NX; i++) {
	for (j=0; j<NY; j++) {
	    s1_t *ps1 = s1 + i*NY + j;

	    if (ps1->a != 8*(i*NY+j)+0 ||
		ps1->c[0] != 8*(i*NY+j)+2 ||
		ps1->c[1] != 8*(i*NY+j)+3 ||
		ps1->c[2] != 8*(i*NY+j)+4 ||
		ps1->c[3] != 8*(i*NY+j)+5 ||
		ps1->e != 8*(i*NY+j)+7) {
		H5_FAILED();
		puts("    Write clobbered values");
		goto error;
	    }

	    if (i>=f_offset[0] &&
		i<f_offset[0]+h_size[0] &&
		j>=f_offset[1] &&
		j<f_offset[1]+h_size[1]) {
		if (ps1->b != (unsigned)(-1) ||
		    ps1->d != (unsigned)(-1)) {
		    H5_FAILED();
		    puts("    Wrong values written or read");
		    goto error;
		}
	    } else {
		if (ps1->b != 8*(i*NY+j)+1 ||
		    ps1->d != 8*(i*NY+j)+6) {
		    H5_FAILED();
		    puts("    Write clobbered values");
		    goto error;
		}
	    }
	}
    }

    /*
     * Release resources.
     */
    H5Pclose (PRESERVE);
    H5Dclose (dataset);
    H5Fclose (file);

    PASSED();
    return 0;

error:
    puts("*** DATASET TESTS FAILED ***");
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	initialize_stype1
 *
 * Purpose:	Initialize data buffer.
 *
 * Return:	void
 *
 * Programmer:  Raymond Lu
 *              Friday, 15 June 2007
 *
 * Modifications:
 *-------------------------------------------------------------------------
 */
static void
initialize_stype1(unsigned char *buf, const size_t num)
{
    int	  i, j;
    stype1 *s_ptr;

    for (i=0; i<(int)num; i++) {
	s_ptr = (stype1*)buf + i;
	s_ptr->a    = i*8+0;
	s_ptr->b    = i*8+1;
        for(j=0; j<8; j++)
	    s_ptr->c[j] = i*8+j;
	s_ptr->d    = i*8+6;
	s_ptr->e    = i*8+7;

        s_ptr->f    = i*2/3;
        s_ptr->g    = i*2/3+1;
        for(j=0; j<16; j++)
	    s_ptr->h[j] = i*j/5+j;
        s_ptr->i    = i*2/3+2;
        s_ptr->j    = i*2/3+3;

        s_ptr->k    = i/7+1;
        s_ptr->l    = i/7+2;
        s_ptr->m    = i/7+3;
        s_ptr->n    = i/7+4;
    }
}


/*-------------------------------------------------------------------------
 * Function:	initialize_stype2
 *
 * Purpose:	Initialize data buffer.
 *
 * Return:	void
 *
 * Programmer:  Raymond Lu
 *              Friday, 15 June 2007
 *
 * Modifications:
 *-------------------------------------------------------------------------
 */
static void
initialize_stype2(unsigned char *buf, const size_t num)
{
    size_t i, j;
    stype2 *s_ptr;

    for (i=0; i<num; i++) {
	s_ptr = (stype2*)buf + i;
	s_ptr->a    = i*8+0;
	s_ptr->b    = i*8+1;
        for(j=0; j<8; j++)
	    s_ptr->c[j] = i*8+j;
	s_ptr->d    = i*8+6;
	s_ptr->e    = i*8+7;

        s_ptr->f    = i*2/3;
        s_ptr->g    = i*2/3+1;
        for(j=0; j<16; j++)
	    s_ptr->h[j] = i*j/5+j;
        s_ptr->i    = i*2/3+2;
        s_ptr->j    = i*2/3+3;

        s_ptr->k    = i/7+1;
        s_ptr->l    = i/7+2;
        s_ptr->m    = i/7+3;
        s_ptr->n    = i/7+4;

        s_ptr->o    = i*3+0;
        s_ptr->p    = i*3+1;
        s_ptr->q    = i*3+2;
    }
}


/*-------------------------------------------------------------------------
 * Function:	initialize_stype3
 *
 * Purpose:	Initialize data buffer.
 *
 * Return:	Success:
 *
 * Programmer:  Raymond Lu
 *              Friday, 15 June 2007
 *
 * Modifications:
 *-------------------------------------------------------------------------
 */
static void
initialize_stype3(unsigned char *buf, const size_t num)
{
    int	  i, j;
    stype3 *s_ptr;

    for (i=0; i<(int)num; i++) {
	s_ptr = (stype3*)buf + i;
	s_ptr->a    = i*8+0;
	s_ptr->b    = i*8+1;
        for(j=0; j<8; j++)
	    s_ptr->c[j] = i*8+j;
	s_ptr->d    = i*8+6;
	s_ptr->e    = i*8+7;
    }
}


/*-------------------------------------------------------------------------
 * Function:	initialize_stype4
 *
 * Purpose:	Initialize data buffer.
 *
 * Return:	void
 *
 * Programmer:  Raymond Lu
 *              Friday, 15 June 2007
 *
 * Modifications:
 *-------------------------------------------------------------------------
 */
static void
initialize_stype4(unsigned char *buf, const size_t num)
{
    size_t i, j;
    stype4 *s_ptr;

    for (i=0; i<num; i++) {
	s_ptr = (stype4*)buf + i;
	s_ptr->a    = i*8+0;
	s_ptr->b    = i*8+1;
        for(j=0; j<8; j++)
	    s_ptr->c[j] = i*8+j;
	s_ptr->d    = i*8+6;
	s_ptr->e    = i*8+7;

        s_ptr->f    = i*2/3;
        s_ptr->g    = i*2/3+1;
        for(j=0; j<16; j++)
	    s_ptr->h[j] = i*j/5+j;
        s_ptr->i    = i*2/3+2;
        s_ptr->j    = i*2/3+3;

        s_ptr->k    = i/7+1;
        s_ptr->l    = i/7+2;
        s_ptr->m    = i/7+3;
        s_ptr->n    = i/7+4;

        s_ptr->o    = i*3+0;
        s_ptr->p    = i*3+1;
        s_ptr->q    = i*3+2;

        s_ptr->r    = i*5+1;
        s_ptr->s    = i*5+2;
        s_ptr->t    = i*5+3;
    }
}


/*-------------------------------------------------------------------------
 * Function:	create_stype1
 *
 * Purpose:	Create HDF5 compound datatype for stype1.
 *
 * Return:	Success:        datatype ID
 *
 *              Failure:        negative
 *
 * Programmer:  Raymond Lu
 *              Friday, 15 June 2007
 *
 * Modifications:
 *-------------------------------------------------------------------------
 */
static hid_t
create_stype1(void)
{
    hid_t   array_dt1, array_dt2, tid;
    const hsize_t	eight = 8, sixteen = 16;

    /* Build hdf5 datatypes */
    if((array_dt1 = H5Tarray_create2(H5T_NATIVE_INT,1, &eight)) < 0)
        goto error;
    if((array_dt2 = H5Tarray_create2(H5T_NATIVE_FLOAT,1, &sixteen)) < 0)
        goto error;

    if((tid = H5Tcreate(H5T_COMPOUND, sizeof(stype1))) < 0 ||
            H5Tinsert(tid, "a", HOFFSET(stype1, a), H5T_NATIVE_INT) < 0 ||
            H5Tinsert(tid, "b", HOFFSET(stype1, b), H5T_NATIVE_INT) < 0 ||
            H5Tinsert(tid, "c", HOFFSET(stype1, c), array_dt1) < 0 ||
            H5Tinsert(tid, "d", HOFFSET(stype1, d), H5T_NATIVE_INT) < 0 ||
            H5Tinsert(tid, "e", HOFFSET(stype1, e), H5T_NATIVE_INT) < 0 ||
            H5Tinsert(tid, "f", HOFFSET(stype1, f), H5T_NATIVE_FLOAT) < 0 ||
            H5Tinsert(tid, "g", HOFFSET(stype1, g), H5T_NATIVE_FLOAT) < 0 ||
            H5Tinsert(tid, "h", HOFFSET(stype1, h), array_dt2) < 0 ||
            H5Tinsert(tid, "i", HOFFSET(stype1, i), H5T_NATIVE_FLOAT) < 0 ||
            H5Tinsert(tid, "j", HOFFSET(stype1, j), H5T_NATIVE_FLOAT) < 0 ||
            H5Tinsert(tid, "k", HOFFSET(stype1, k), H5T_NATIVE_DOUBLE) < 0 ||
            H5Tinsert(tid, "l", HOFFSET(stype1, l), H5T_NATIVE_DOUBLE) < 0 ||
            H5Tinsert(tid, "m", HOFFSET(stype1, m), H5T_NATIVE_DOUBLE) < 0 ||
            H5Tinsert(tid, "n", HOFFSET(stype1, n), H5T_NATIVE_DOUBLE) < 0)
        goto error;

    if(H5Tclose(array_dt1) < 0)
        goto error;
    if(H5Tclose(array_dt2) < 0)
        goto error;

    return tid;

error:
    return FAIL;
}


/*-------------------------------------------------------------------------
 * Function:	create_stype2
 *
 * Purpose:	Create HDF5 compound datatype for stype2.
 *
 * Return:	Success:        datatype ID
 *
 *              Failure:        negative
 *
 * Programmer:  Raymond Lu
 *              Friday, 15 June 2007
 *
 * Modifications:
 *-------------------------------------------------------------------------
 */
static hid_t
create_stype2(void)
{
    hid_t   array_dt1, array_dt2, tid;
    const hsize_t	eight = 8, sixteen = 16;

    /* Build hdf5 datatypes */
    if((array_dt1 = H5Tarray_create2(H5T_NATIVE_INT,1, &eight)) < 0)
        goto error;
    if((array_dt2 = H5Tarray_create2(H5T_NATIVE_FLOAT,1, &sixteen)) < 0)
        goto error;

    if((tid = H5Tcreate(H5T_COMPOUND, sizeof(stype2))) < 0 ||
            H5Tinsert(tid, "a", HOFFSET(stype2, a), H5T_NATIVE_INT) < 0 ||
            H5Tinsert(tid, "b", HOFFSET(stype2, b), H5T_NATIVE_INT) < 0 ||
            H5Tinsert(tid, "c", HOFFSET(stype2, c), array_dt1) < 0 ||
            H5Tinsert(tid, "d", HOFFSET(stype2, d), H5T_NATIVE_INT) < 0 ||
            H5Tinsert(tid, "e", HOFFSET(stype2, e), H5T_NATIVE_INT) < 0 ||
            H5Tinsert(tid, "f", HOFFSET(stype2, f), H5T_NATIVE_FLOAT) < 0 ||
            H5Tinsert(tid, "g", HOFFSET(stype2, g), H5T_NATIVE_FLOAT) < 0 ||
            H5Tinsert(tid, "h", HOFFSET(stype2, h), array_dt2) < 0 ||
            H5Tinsert(tid, "i", HOFFSET(stype2, i), H5T_NATIVE_FLOAT) < 0 ||
            H5Tinsert(tid, "j", HOFFSET(stype2, j), H5T_NATIVE_FLOAT) < 0 ||
            H5Tinsert(tid, "k", HOFFSET(stype2, k), H5T_NATIVE_DOUBLE) < 0 ||
            H5Tinsert(tid, "l", HOFFSET(stype2, l), H5T_NATIVE_DOUBLE) < 0 ||
            H5Tinsert(tid, "m", HOFFSET(stype2, m), H5T_NATIVE_DOUBLE) < 0 ||
            H5Tinsert(tid, "n", HOFFSET(stype2, n), H5T_NATIVE_DOUBLE) < 0 ||
            H5Tinsert(tid, "o", HOFFSET(stype2, o), H5T_NATIVE_LONG) < 0 ||
            H5Tinsert(tid, "p", HOFFSET(stype2, p), H5T_NATIVE_LONG) < 0 ||
            H5Tinsert(tid, "q", HOFFSET(stype2, q), H5T_NATIVE_LONG) < 0)
        goto error;

    if(H5Tclose(array_dt1) < 0)
        goto error;
    if(H5Tclose(array_dt2) < 0)
        goto error;

    return tid;

error:
    return FAIL;
}


/*-------------------------------------------------------------------------
 * Function:	create_stype3
 *
 * Purpose:	Create HDF5 compound datatype for stype3.
 *
 * Return:	Success:        datatype ID
 *
 *              Failure:        negative
 *
 * Programmer:  Raymond Lu
 *              Friday, 15 June 2007
 *
 * Modifications:
 *-------------------------------------------------------------------------
 */
static hid_t
create_stype3(void)
{
    hid_t   array_dt1, tid;
    const hsize_t	eight = 8;

    /* Build hdf5 datatypes */
    if((array_dt1 = H5Tarray_create2(H5T_NATIVE_INT,1, &eight)) < 0)
        goto error;

    if((tid = H5Tcreate(H5T_COMPOUND, sizeof(stype3))) < 0 ||
            H5Tinsert(tid, "a", HOFFSET(stype3, a), H5T_NATIVE_INT) < 0 ||
            H5Tinsert(tid, "b", HOFFSET(stype3, b), H5T_NATIVE_INT) < 0 ||
            H5Tinsert(tid, "c", HOFFSET(stype3, c), array_dt1) < 0 ||
            H5Tinsert(tid, "d", HOFFSET(stype3, d), H5T_NATIVE_INT) < 0 ||
            H5Tinsert(tid, "e", HOFFSET(stype3, e), H5T_NATIVE_INT) < 0)
        goto error;

    if(H5Tclose(array_dt1) < 0)
        goto error;

    return tid;

error:
    return FAIL;
}


/*-------------------------------------------------------------------------
 * Function:	create_stype4
 *
 * Purpose:	Create HDF5 compound datatype for stype4.
 *
 * Return:	Success:        datatype ID
 *
 *              Failure:        negative
 *
 * Programmer:  Raymond Lu
 *              Friday, 15 June 2007
 *
 * Modifications:
 *-------------------------------------------------------------------------
 */
static hid_t
create_stype4(void)
{
    hid_t   array_dt1, array_dt2, tid;
    const hsize_t	eight = 8, sixteen = 16;

    /* Build hdf5 datatypes */
    if((array_dt1 = H5Tarray_create2(H5T_NATIVE_INT,1, &eight)) < 0)
        goto error;
    if((array_dt2 = H5Tarray_create2(H5T_NATIVE_FLOAT,1, &sixteen)) < 0)
        goto error;

    if((tid = H5Tcreate(H5T_COMPOUND, sizeof(stype4))) < 0 ||
            H5Tinsert(tid, "a", HOFFSET(stype4, a), H5T_NATIVE_INT) < 0 ||
            H5Tinsert(tid, "b", HOFFSET(stype4, b), H5T_NATIVE_INT) < 0 ||
            H5Tinsert(tid, "c", HOFFSET(stype4, c), array_dt1) < 0 ||
            H5Tinsert(tid, "d", HOFFSET(stype4, d), H5T_NATIVE_INT) < 0 ||
            H5Tinsert(tid, "e", HOFFSET(stype4, e), H5T_NATIVE_INT) < 0 ||
            H5Tinsert(tid, "f", HOFFSET(stype4, f), H5T_NATIVE_FLOAT) < 0 ||
            H5Tinsert(tid, "g", HOFFSET(stype4, g), H5T_NATIVE_FLOAT) < 0 ||
            H5Tinsert(tid, "h", HOFFSET(stype4, h), array_dt2) < 0 ||
            H5Tinsert(tid, "i", HOFFSET(stype4, i), H5T_NATIVE_FLOAT) < 0 ||
            H5Tinsert(tid, "j", HOFFSET(stype4, j), H5T_NATIVE_FLOAT) < 0 ||
            H5Tinsert(tid, "k", HOFFSET(stype4, k), H5T_NATIVE_DOUBLE) < 0 ||
            H5Tinsert(tid, "l", HOFFSET(stype4, l), H5T_NATIVE_DOUBLE) < 0 ||
            H5Tinsert(tid, "m", HOFFSET(stype4, m), H5T_NATIVE_DOUBLE) < 0 ||
            H5Tinsert(tid, "n", HOFFSET(stype4, n), H5T_NATIVE_DOUBLE) < 0 ||
            H5Tinsert(tid, "o", HOFFSET(stype4, o), H5T_NATIVE_LONG) < 0 ||
            H5Tinsert(tid, "p", HOFFSET(stype4, p), H5T_NATIVE_LONG) < 0 ||
            H5Tinsert(tid, "q", HOFFSET(stype4, q), H5T_NATIVE_LONG) < 0 ||
            H5Tinsert(tid, "r", HOFFSET(stype4, r), H5T_NATIVE_LLONG) < 0 ||
            H5Tinsert(tid, "s", HOFFSET(stype4, s), H5T_NATIVE_LLONG) < 0 ||
            H5Tinsert(tid, "t", HOFFSET(stype4, t), H5T_NATIVE_LLONG) < 0)
        goto error;

    if(H5Tclose(array_dt1) < 0)
        goto error;
    if(H5Tclose(array_dt2) < 0)
        goto error;

    return tid;

error:
    return FAIL;
}


/*-------------------------------------------------------------------------
 * Function:	compare_data
 *
 * Purpose:	Compare data of stype1 and stype2.
 *
 * Return:	Success:        0
 *
 *              Failure:        negative
 *
 * Programmer:  Raymond Lu
 *              Friday, 15 June 2007
 *
 * Modifications:
 *-------------------------------------------------------------------------
 */
static int
compare_data(void *src_data, void *dst_data, hbool_t src_subset)
{
    stype1  *s_ptr;
    stype2  *d_ptr;
    int     i;

    for(i = 0; i < (int)(NX * NY); i++) {
        if(src_subset) {
	   s_ptr = ((stype1 *)src_data) + i;
	   d_ptr = ((stype2 *)dst_data) + i;
        } else {
	   s_ptr = (stype1 *)(((stype2 *)src_data) + i);
	   d_ptr = (stype2 *)(((stype1 *)dst_data) + i);
        }

	if (s_ptr->a    != d_ptr->a    ||
	    s_ptr->b    != d_ptr->b    ||
	    s_ptr->c[0] != d_ptr->c[0] ||
	    s_ptr->c[1] != d_ptr->c[1] ||
	    s_ptr->c[2] != d_ptr->c[2] ||
	    s_ptr->c[3] != d_ptr->c[3] ||
	    s_ptr->d    != d_ptr->d    ||
	    s_ptr->e    != d_ptr->e    ||
            !FLT_ABS_EQUAL(s_ptr->f, d_ptr->f) ||
            !FLT_ABS_EQUAL(s_ptr->g, d_ptr->g) ||
            !FLT_ABS_EQUAL(s_ptr->h[0], d_ptr->h[0]) ||
            !FLT_ABS_EQUAL(s_ptr->h[1], d_ptr->h[1]) ||
            !FLT_ABS_EQUAL(s_ptr->i, d_ptr->i) ||
            !FLT_ABS_EQUAL(s_ptr->j, d_ptr->j) ||
            !DBL_ABS_EQUAL(s_ptr->k, d_ptr->k) ||
            !DBL_ABS_EQUAL(s_ptr->l, d_ptr->l) ||
            !DBL_ABS_EQUAL(s_ptr->m, d_ptr->m) ||
            !DBL_ABS_EQUAL(s_ptr->n, d_ptr->n) ) {

	    H5_FAILED();
	    printf("    i=%d\n", i);
	    printf("    src={a=%d, b=%d, c=[%d,%d,%d,%d,%d,%d,%d,%d], d=%d, e=%d, f=%f, g=%f, h=[%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f], i=%f, j=%f, k=%f, l=%f, m=%f, n=%f}\n",
		   s_ptr->a, s_ptr->b, s_ptr->c[0], s_ptr->c[1], s_ptr->c[2],
		   s_ptr->c[3], s_ptr->c[4], s_ptr->c[5], s_ptr->c[6], s_ptr->c[7],
                   s_ptr->d, s_ptr->e, s_ptr->f, s_ptr->g,s_ptr->h[0],s_ptr->h[1],s_ptr->h[2],
                   s_ptr->h[3],s_ptr->h[4],s_ptr->h[5],s_ptr->h[6],s_ptr->h[7],s_ptr->h[8],
                   s_ptr->h[9],s_ptr->h[10],s_ptr->h[11],s_ptr->h[12],s_ptr->h[13],s_ptr->h[14],
                   s_ptr->h[15], s_ptr->i,s_ptr->j,s_ptr->k,s_ptr->l,s_ptr->m,s_ptr->n);
	    printf("    dst={a=%d, b=%d, c=[%d,%d,%d,%d,%d,%d,%d,%d], d=%d, e=%d, f=%f, g=%f, h=[%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f], i=%f, j=%f, k=%f, l=%f, m=%f, n=%f}\n",
		   d_ptr->a, d_ptr->b, d_ptr->c[0], d_ptr->c[1], d_ptr->c[2],
		   d_ptr->c[3], d_ptr->c[4], d_ptr->c[5], d_ptr->c[6], d_ptr->c[7],
                   d_ptr->d, d_ptr->e, d_ptr->f, d_ptr->g,d_ptr->h[0],d_ptr->h[1],d_ptr->h[2],
                   d_ptr->h[3],d_ptr->h[4],d_ptr->h[5],d_ptr->h[6],d_ptr->h[7],d_ptr->h[8],
                   d_ptr->h[9],d_ptr->h[10],d_ptr->h[11],d_ptr->h[12],d_ptr->h[13],
                   d_ptr->h[14], d_ptr->h[15], d_ptr->i,d_ptr->j,d_ptr->k,d_ptr->l,
                   d_ptr->m,d_ptr->n);
	    goto error;
	}
    }

    return SUCCEED;

error:
    return FAIL;
}


/*-------------------------------------------------------------------------
 * Function:	test_hdf5_src_subset
 *
 * Purpose:	Test the optimization of compound data writing, rewriting,
 *              and reading when the source type is a subset of destination
 *              type.  For example:
 *                  struct source {            struct destination {
 *                      TYPE1 A;      -->          TYPE1 A;
 *                      TYPE2 B;      -->          TYPE2 B;
 *                      TYPE3 C;      -->          TYPE3 C;
 *                  };                             TYPE4 D;
 *                                                 TYPE5 E;
 *                                             };
 *              This optimization is for the Chicago company.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Raymond Lu
 *              Friday, 15 June 2007
 *
 * Modifications:
 *-------------------------------------------------------------------------
 */
static int
test_hdf5_src_subset(char *filename, hid_t fapl)
{
    hid_t   file;
    hid_t   rew_tid, src_tid, dst_tid;
    hid_t   dataset;
    hid_t   space;
    hid_t   dcpl, dxpl;
    hsize_t dims[2] = {NX, NY};
    hsize_t chunk_dims[2] = {NX/10, NY/10};
    unsigned char *orig=NULL, *rew_buf=NULL, *rbuf=NULL;

    /* Create the file for this test */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
	goto error;

    /* Build hdf5 datatypes */
    if ((src_tid=create_stype1()) < 0)
        goto error;

    if ((dst_tid=create_stype2()) < 0)
        goto error;

    if ((rew_tid=create_stype3()) < 0)
        goto error;

    /* Create the data space */
    if((space = H5Screate_simple(2, dims, NULL)) < 0)
	goto error;

    /* Allocate space and initialize data */
    orig = (unsigned char*)malloc(NX * NY * sizeof(stype1));
    initialize_stype1(orig, (size_t)NX*NY);

    rbuf = (unsigned char*)malloc(NX * NY * sizeof(stype2));

    rew_buf = (unsigned char*)malloc(NX * NY * sizeof(stype3));
    initialize_stype3(rew_buf, (size_t)NX*NY);


    /* Create dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;

    /*
     *######################################################################
     * STEP 1. Write data to contiguous and chunked datasets.
     */
    TESTING("writing data to contiguous and chunked datasets");

    /* Create contiguous data set */
    if((dataset = H5Dcreate2(file, DSET_NAME[0], src_tid, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    /* Write the data to the dataset */
    if(H5Dwrite(dataset, src_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, orig) < 0)
	goto error;

    if(H5Dclose(dataset) < 0)
        goto error;

    /* Set chunking */
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        goto error;

    /* Create chunked data set */
    if((dataset = H5Dcreate2(file, DSET_NAME[1], src_tid, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    /* Write the data to the dataset */
    if(H5Dwrite(dataset, src_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, orig) < 0)
	goto error;

    if(H5Dclose(dataset) < 0)
        goto error;

    PASSED();

    /*
     *######################################################################
     * STEP 2. Rewrite the data with a subset of original data type.
     */
    TESTING("rewriting data with a subset of original data type");

    /* Create xfer properties to preserve initialized data */
    if((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
       FAIL_STACK_ERROR

    if(H5Pset_preserve(dxpl, TRUE) < 0)
       FAIL_STACK_ERROR

    /* Rewrite contiguous data set */
    if((dataset = H5Dopen2(file, DSET_NAME[0], H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Write the data to the dataset */
    if(H5Dwrite(dataset, rew_tid, H5S_ALL, H5S_ALL, dxpl, rew_buf) < 0)
	FAIL_STACK_ERROR

    if(H5Dclose(dataset) < 0)
        FAIL_STACK_ERROR

    /* Rewrite chunked data set */
    if((dataset = H5Dopen2(file, DSET_NAME[1], H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Write the data to the dataset */
    if(H5Dwrite(dataset, rew_tid, H5S_ALL, H5S_ALL, dxpl, rew_buf) < 0)
	FAIL_STACK_ERROR

    if(H5Dclose(dataset) < 0)
        FAIL_STACK_ERROR

    PASSED();

    /*
     *######################################################################
     * STEP 3. Read the data into a subset of the original compound type.
     */
    TESTING("reading data with a subset of original data type");

    /* Check contiguous data set */
    if((dataset = H5Dopen2(file, DSET_NAME[0], H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    if(H5Dread(dataset, dst_tid, H5S_ALL, H5S_ALL, dxpl, rbuf) < 0)
        FAIL_STACK_ERROR

    if(compare_data(orig, rbuf, TRUE) < 0)
        TEST_ERROR

    if(H5Dclose(dataset) < 0)
        FAIL_STACK_ERROR

    /* Check chunked data set */
    if((dataset = H5Dopen2(file, DSET_NAME[1], H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    if(H5Dread(dataset, dst_tid, H5S_ALL, H5S_ALL, dxpl, rbuf) < 0)
        FAIL_STACK_ERROR

    if(compare_data(orig, rbuf, TRUE) < 0)
        TEST_ERROR

    if(H5Dclose(dataset) < 0)
        FAIL_STACK_ERROR

    /* Finishing test and release resources */
    if(H5Sclose(space) < 0)
        FAIL_STACK_ERROR

    if(H5Pclose(dcpl) < 0)
        FAIL_STACK_ERROR

    if(H5Pclose(dxpl) < 0)
        FAIL_STACK_ERROR

    if(H5Tclose(src_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Tclose(dst_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Tclose(rew_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    free(orig);
    free(rbuf);
    free(rew_buf);

    PASSED();
    return 0;

error:
    puts("*** DATASET TESTS FAILED ***");
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_hdf5_dst_subset
 *
 * Purpose:	Test the optimization of compound data writing, rewriting,
 *              and reading when the destination type is a subset of the
 *              source type.  For example:
 *                  struct source {            struct destination {
 *                      TYPE1 A;      -->          TYPE1 A;
 *                      TYPE2 B;      -->          TYPE2 B;
 *                      TYPE3 C;      -->          TYPE3 C;
 *                      TYPE4 D;               }
 *                      TYPE5 E;
 *                  };
 *              This optimization is for the Chicago company.  This test
 *              is in opposite of test_hdf5_src_subset.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Raymond Lu
 *              Friday, 15 June 2007
 *
 * Modifications:
 *-------------------------------------------------------------------------
 */
static int
test_hdf5_dst_subset(char *filename, hid_t fapl)
{
    hid_t   file;
    hid_t   rew_tid, src_tid, dst_tid;
    hid_t   dataset;
    hid_t   space;
    hid_t   dcpl, dxpl;
    hsize_t dims[2] = {NX, NY};
    hsize_t chunk_dims[2] = {NX/10, NY/10};
    unsigned char *orig=NULL, *rew_buf=NULL, *rbuf=NULL;

    /* Create the file for this test */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
	goto error;

    /* Build hdf5 datatypes */
    if ((src_tid=create_stype2()) < 0)
        goto error;

    if ((dst_tid=create_stype1()) < 0)
        goto error;

    if ((rew_tid=create_stype4()) < 0)
        goto error;

    /* Create the data space */
    if((space = H5Screate_simple(2, dims, NULL)) < 0)
	goto error;

    /* Allocate space and initialize data */
    orig = (unsigned char*)malloc(NX * NY * sizeof(stype2));
    initialize_stype2(orig, (size_t)NX*NY);

    rbuf = (unsigned char*)malloc(NX * NY * sizeof(stype1));

    rew_buf = (unsigned char*)malloc(NX * NY * sizeof(stype4));
    initialize_stype4(rew_buf, (size_t)NX*NY);

    /* Create dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;

    /*
     *######################################################################
     * STEP 1. Write data to contiguous and chunked datasets.
     */
    TESTING("writing data to contiguous and chunked datasets");

    /* Create contiguous data set */
    if((dataset = H5Dcreate2(file, DSET_NAME[2], src_tid, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    /* Write the data to the dataset */
    if(H5Dwrite(dataset, src_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, orig) < 0)
	goto error;

    if(H5Dclose(dataset) < 0)
        goto error;

    /* Set chunking */
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        goto error;

    /* Create chunked data set */
    if((dataset = H5Dcreate2(file, DSET_NAME[3], src_tid, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    /* Write the data to the dataset */
    if(H5Dwrite(dataset, src_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, orig) < 0)
	goto error;

    if(H5Dclose(dataset) < 0)
        goto error;

    PASSED();

    /*
     *######################################################################
     * STEP 2. Rewrite the data with a subset of original data type.
     */
    TESTING("rewriting data with a subset of original data type");

    /* Create xfer properties to preserve initialized data */
    if((dxpl = H5Pcreate (H5P_DATASET_XFER)) < 0)
       goto error;

    if(H5Pset_preserve(dxpl, TRUE) < 0)
       goto error;

    /* Rewrite contiguous data set */
    if((dataset = H5Dopen2(file, DSET_NAME[2], H5P_DEFAULT)) < 0)
        goto error;

    /* Write the data to the dataset */
    if(H5Dwrite(dataset, rew_tid, H5S_ALL, H5S_ALL, dxpl, rew_buf) < 0)
	goto error;

    if(H5Dclose(dataset) < 0)
        goto error;

    /* Rewrite chunked data set */
    if((dataset = H5Dopen2(file, DSET_NAME[3], H5P_DEFAULT)) < 0)
        goto error;

    /* Write the data to the dataset */
    if(H5Dwrite(dataset, rew_tid, H5S_ALL, H5S_ALL, dxpl, rew_buf) < 0)
	goto error;

    if(H5Dclose(dataset) < 0)
        goto error;

    PASSED();

    /*
     *######################################################################
     * STEP 3. Read the data into a subset of the original compound type.
     */
    TESTING("reading data with a subset of original data type");

    /* Check contiguous data set */
    if((dataset = H5Dopen2(file, DSET_NAME[2], H5P_DEFAULT)) < 0)
        goto error;

    if(H5Dread(dataset, dst_tid, H5S_ALL, H5S_ALL, dxpl, rbuf) < 0)
        goto error;

    if(compare_data(orig, rbuf, FALSE) < 0)
        goto error;

    if(H5Dclose(dataset) < 0)
        goto error;

    /* Check chunked data set */
    if((dataset = H5Dopen2(file, DSET_NAME[3], H5P_DEFAULT)) < 0)
        goto error;

    if(H5Dread(dataset, dst_tid, H5S_ALL, H5S_ALL, dxpl, rbuf) < 0)
        goto error;

    if(compare_data(orig, rbuf, FALSE) < 0)
        goto error;

    if(H5Dclose(dataset) < 0)
        goto error;

    /* Finishing test and release resources */
    if(H5Sclose(space) < 0)
        goto error;

    if(H5Pclose(dcpl) < 0)
        goto error;

    if(H5Pclose(dxpl) < 0)
        goto error;

    if(H5Tclose(src_tid) < 0)
        goto error;
    if(H5Tclose(dst_tid) < 0)
        goto error;
    if(H5Tclose(rew_tid) < 0)
        goto error;
    if(H5Fclose(file) < 0)
        goto error;

    free(orig);
    free(rbuf);
    free(rew_buf);

    PASSED();
    return 0;

error:
    puts("*** DATASET TESTS FAILED ***");
    return 1;
}

/* Error macro that outputs the state of the randomly generated variables so the
 * failure can be reproduced */
#define PACK_OOO_ERROR                                                         \
{                                                                              \
    int _i;                                                                    \
    H5_FAILED(); AT();                                                         \
    printf("    Insertion order =");                                           \
    for(_i=0; _i<PACK_NMEMBS; _i++)                                            \
        printf(" %d", order[_i]);                                              \
    printf("\n    Inner compound order = %d, location = %d\n", sub_cmpd_order, order[sub_cmpd_order]); \
    fflush(stdout);                                                            \
    goto error;                                                                \
}


/*-------------------------------------------------------------------------
 * Function:	test_pack_ooo
 *
 * Purpose:	Test inserting fields into a compound out of offset order.
 *              Verifies that the compound is correctly marked as packed
 *              or non-packed.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Neil Fortner
 *              Thursday, 22 January 2009
 *
 * Modifications:
 *-------------------------------------------------------------------------
 */
static int
test_pack_ooo(void)
{
    hid_t       cmpd, sub_cmpd;     /* Datatype IDs */
    H5T_t       *dt;                /* Datatype pointer */
    unsigned    order[PACK_NMEMBS]; /* Order of insertion */
    unsigned    free_order[PACK_NMEMBS]; /* Index of remaining free slots in order */
    unsigned    num_free;           /* Number of free slots in order */
    unsigned    sub_cmpd_order;     /* Order to insert the inner compound */
    char        name[6];            /* Member name */
    unsigned    extra_space;        /* Whether to add extra space to the end of
                                     * the compound */
    unsigned    i, j;               /* Indices */

    HDsrand((unsigned) time(NULL));

    /* Initialize "free_order" array to indicate that all slots in order are
     * free */
    for(i=0; i<PACK_NMEMBS; i++)
        free_order[i] = i;

    /* Create "order" array */
    for(i=0; i<PACK_NMEMBS; i++) {
        /* Generate index into free_order array */
        num_free = PACK_NMEMBS - i;
        j = HDrand() % num_free;

        /* Update order array at the randomly generated (but guaranteed to be
         * free) location */
        order[free_order[j]] = i;

        /* Reshape free_order to remove j (which is no longer free) */
        if(j < (num_free - 1))
            HDmemmove(&free_order[j], &free_order[j+1], (num_free - j - 1) * sizeof(free_order[0]));
    } /* end for */

    /* Generate order to insert inner compound type */
    sub_cmpd_order = HDrand() % PACK_NMEMBS;

    for(extra_space=0; extra_space<2; extra_space ++) {
        if(extra_space)
            puts("With extra space at the end of compound...");
        else
            puts("Without extra space at the end of compound...");

        TESTING("random member insertion with empty compound subtype");

        /* Create inner compound type.  It will be empty for the first run */
        if((sub_cmpd = H5Tcreate(H5T_COMPOUND, (size_t)4)) < 0) PACK_OOO_ERROR

        /* Create main compound type, with extra space at the end */
        if((cmpd = H5Tcreate(H5T_COMPOUND, (size_t)((4 * PACK_NMEMBS) + extra_space))) < 0) PACK_OOO_ERROR

        /* Insert the compound members in the random order previously generated */
        for(i=0; i<PACK_NMEMBS; i++) {
            sprintf(name, "%05d", i);
            if(i == sub_cmpd_order) {
                if(H5Tinsert(cmpd, name, (size_t)(4 * order[i]), sub_cmpd) < 0) PACK_OOO_ERROR
            } else
                if(H5Tinsert(cmpd, name, (size_t)(4 * order[i]), H5T_STD_I32BE) < 0) PACK_OOO_ERROR
        } /* end for */

        /* Verify that the compound is not packed */
        if(NULL == (dt = (H5T_t *) H5I_object_verify(cmpd, H5I_DATATYPE))) PACK_OOO_ERROR
        if(dt->shared->u.compnd.packed) PACK_OOO_ERROR

        /* Close the main compound */
        if(H5Tclose(cmpd) < 0) PACK_OOO_ERROR

        PASSED();

        TESTING("random member insertion with full compound subtype");

        /* Complete the inner compound type */
        if(H5Tinsert(sub_cmpd, "int", (size_t)0, H5T_STD_I32LE) < 0) PACK_OOO_ERROR

        /* Recreate main compound type */
        if((cmpd = H5Tcreate(H5T_COMPOUND, (size_t)((4 * PACK_NMEMBS) + extra_space))) < 0) PACK_OOO_ERROR

        /* Insert the compound members in the random order previously generated */
        for(i=0; i<PACK_NMEMBS; i++) {
            sprintf(name, "%05d", i);
            if(i == sub_cmpd_order) {
                if(H5Tinsert(cmpd, name, (size_t)(4 * order[i]), sub_cmpd) < 0) PACK_OOO_ERROR
            } else
                if(H5Tinsert(cmpd, name, (size_t)(4 * order[i]), H5T_STD_I32BE) < 0) PACK_OOO_ERROR
        } /* end for */

        /* Verify that the compound is not packed */
        if(NULL == (dt = (H5T_t *) H5I_object_verify(cmpd, H5I_DATATYPE))) PACK_OOO_ERROR
        if(dt->shared->u.compnd.packed != !extra_space) PACK_OOO_ERROR

        /* Close */
        if(H5Tclose(cmpd) < 0) PACK_OOO_ERROR
        if(H5Tclose(sub_cmpd) < 0) PACK_OOO_ERROR

        PASSED();

        TESTING("reverse member insertion with empty compound subtype");

        /* Create inner compound type.  It will be empty for the first run */
        if((sub_cmpd = H5Tcreate(H5T_COMPOUND, (size_t)4)) < 0) PACK_OOO_ERROR

        /* Create main compound type, with extra space at the end */
        if((cmpd = H5Tcreate(H5T_COMPOUND, (size_t)((4 * PACK_NMEMBS) + extra_space))) < 0) PACK_OOO_ERROR

        /* Insert the compound members in reverse order, with compound last */
        for(i=0; i<PACK_NMEMBS; i++) {
            sprintf(name, "%05d", i);
            if(i == PACK_NMEMBS - 1) {
                if(H5Tinsert(cmpd, name, (size_t)(4 * (PACK_NMEMBS - i - 1)), sub_cmpd) < 0) PACK_OOO_ERROR
            } else
                if(H5Tinsert(cmpd, name, (size_t)(4 * (PACK_NMEMBS - i - 1)), H5T_STD_I32BE) < 0) PACK_OOO_ERROR
        } /* end for */

        /* Verify that the compound is not packed */
        if(NULL == (dt = (H5T_t *) H5I_object_verify(cmpd, H5I_DATATYPE))) PACK_OOO_ERROR
        if(dt->shared->u.compnd.packed) PACK_OOO_ERROR

        /* Close the main compound */
        if(H5Tclose(cmpd) < 0) PACK_OOO_ERROR

        PASSED();

        TESTING("reverse member insertion with full compound subtype");

        /* Complete the inner compound type */
        if(H5Tinsert(sub_cmpd, "int", (size_t)0, H5T_STD_I32LE) < 0) PACK_OOO_ERROR

        /* Recreate main compound type */
        if((cmpd = H5Tcreate(H5T_COMPOUND, (size_t)((4 * PACK_NMEMBS) + extra_space))) < 0) PACK_OOO_ERROR

        /* Insert the compound members in reverse order, with compound last */
        for(i=0; i<PACK_NMEMBS; i++) {
            sprintf(name, "%05d", i);
            if(i == PACK_NMEMBS - 1) {
                if(H5Tinsert(cmpd, name, (size_t)(4 * (PACK_NMEMBS - i - 1)), sub_cmpd) < 0) PACK_OOO_ERROR
            } else
                if(H5Tinsert(cmpd, name, (size_t)(4 * (PACK_NMEMBS - i - 1)), H5T_STD_I32BE) < 0) PACK_OOO_ERROR
        } /* end for */

        /* Verify that the compound is packed */
        if(NULL == (dt = (H5T_t *) H5I_object_verify(cmpd, H5I_DATATYPE))) PACK_OOO_ERROR
        if(dt->shared->u.compnd.packed != !extra_space) PACK_OOO_ERROR

        /* Close */
        if(H5Tclose(cmpd) < 0) PACK_OOO_ERROR
        if(H5Tclose(sub_cmpd) < 0) PACK_OOO_ERROR

        PASSED();

        TESTING("forward member insertion with empty compound subtype");

        /* Create inner compound type.  It will be empty for the first run */
        if((sub_cmpd = H5Tcreate(H5T_COMPOUND, (size_t)4)) < 0) PACK_OOO_ERROR

        /* Create main compound type, with extra space at the end */
        if((cmpd = H5Tcreate(H5T_COMPOUND, (size_t)((4 * PACK_NMEMBS) + extra_space))) < 0) PACK_OOO_ERROR

        /* Insert the compound members in forward order, with compound first */
        for(i=0; i<PACK_NMEMBS; i++) {
            sprintf(name, "%05d", i);
            if(i == 0) {
                if(H5Tinsert(cmpd, name, (size_t)(4 * i), sub_cmpd) < 0) PACK_OOO_ERROR
            } else
                if(H5Tinsert(cmpd, name, (size_t)(4 * i), H5T_STD_I32BE) < 0) PACK_OOO_ERROR
        } /* end for */

        /* Verify that the compound is not packed */
        if(NULL == (dt = (H5T_t *) H5I_object_verify(cmpd, H5I_DATATYPE))) PACK_OOO_ERROR
        if(dt->shared->u.compnd.packed) PACK_OOO_ERROR

        /* Close the main compound */
        if(H5Tclose(cmpd) < 0) PACK_OOO_ERROR

        PASSED();

        TESTING("forward member insertion with full compound subtype");

        /* Complete the inner compound type */
        if(H5Tinsert(sub_cmpd, "int", (size_t)0, H5T_STD_I32LE) < 0) PACK_OOO_ERROR

        /* Recreate main compound type */
        if((cmpd = H5Tcreate(H5T_COMPOUND, (size_t)((4 * PACK_NMEMBS) + extra_space))) < 0) PACK_OOO_ERROR

        /* Insert the compound members in forward order */
        for(i=0; i<PACK_NMEMBS; i++) {
            sprintf(name, "%05d", i);
            if(i == 0) {
                if(H5Tinsert(cmpd, name, (size_t)(4 * i), sub_cmpd) < 0) PACK_OOO_ERROR
            } else
                if(H5Tinsert(cmpd, name, (size_t)(4 * i), H5T_STD_I32BE) < 0) PACK_OOO_ERROR
        } /* end for */

        /* Verify that the compound is packed */
        if(NULL == (dt = (H5T_t *) H5I_object_verify(cmpd, H5I_DATATYPE))) PACK_OOO_ERROR
        if(dt->shared->u.compnd.packed != !extra_space) PACK_OOO_ERROR

        /* Close */
        if(H5Tclose(cmpd) < 0) PACK_OOO_ERROR
        if(H5Tclose(sub_cmpd) < 0) PACK_OOO_ERROR

        PASSED();
    } /* end for */

    return 0;

error:
    puts("*** DATASET TESTS FAILED ***");
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_ooo_order
 *
 * Purpose:	Test inserting fields into a compound out of offset order.
 *              Verifies that the order of compound members is the same as
 *              the order in which they were inserted.  While this is
 *              explicitly not guaranteed by the documentation, the H5TB
 *              API currently makes this assumption.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Neil Fortner
 *              Monday, 19 October 2009
 *
 * Modifications:
 *-------------------------------------------------------------------------
 */
static int
test_ooo_order(char *filename)
{
    hid_t       file = -1;          /* File ID */
    hid_t       dtype = -1;         /* Datatype IDs */
    hid_t       dtype_tmp = -1;     /* Temp Datatype ID */
    H5T_t       *dt = NULL;         /* Datatype pointer */

    TESTING("that compound member insertion order is preserved")

    /* Create the file */
    if ((file = H5Fcreate (filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Create the compound */
    if((dtype = H5Tcreate(H5T_COMPOUND, (size_t)20)) < 0) TEST_ERROR
    if(H5Tinsert(dtype, "A", (size_t)8, H5T_STD_I32LE) < 0) TEST_ERROR
    if(H5Tinsert(dtype, "B", (size_t)12, H5T_STD_I32LE) < 0) TEST_ERROR
    if(H5Tinsert(dtype, "C", (size_t)0, H5T_STD_I32LE) < 0) TEST_ERROR
    if(H5Tinsert(dtype, "D", (size_t)16, H5T_STD_I32LE) < 0) TEST_ERROR

    /* Verify that the compound is not packed */
    if(NULL == (dt = (H5T_t *) H5I_object_verify(dtype, H5I_DATATYPE)))
        TEST_ERROR
    if(dt->shared->u.compnd.packed) TEST_ERROR

    /* Verify that the order is the same as the insertion order */
    if(H5Tget_member_offset(dtype, 0) != 8) TEST_ERROR
    if(H5Tget_member_offset(dtype, 1) != 12) TEST_ERROR
    if(H5Tget_member_offset(dtype, 2) != 0) TEST_ERROR
    if(H5Tget_member_offset(dtype, 3) != 16) TEST_ERROR

    /* Commit the datatype */
    if(H5Tcommit2(file, "dtype", dtype, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Close and reopen the file */
    if(H5Tclose(dtype)) TEST_ERROR
    if(H5Fclose(file)) TEST_ERROR
    if((file = H5Fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Open the type */
    if((dtype_tmp = H5Topen2(file, "dtype", H5P_DEFAULT)) < 0) TEST_ERROR

    /* Verify that the compound is not packed */
    if(NULL == (dt = (H5T_t *) H5I_object_verify(dtype_tmp, H5I_DATATYPE)))
        TEST_ERROR
    if(dt->shared->u.compnd.packed) TEST_ERROR

    /* Verify that the order is the same as the insertion order */
    if(H5Tget_member_offset(dtype_tmp, 0) != 8) TEST_ERROR
    if(H5Tget_member_offset(dtype_tmp, 1) != 12) TEST_ERROR
    if(H5Tget_member_offset(dtype_tmp, 2) != 0) TEST_ERROR
    if(H5Tget_member_offset(dtype_tmp, 3) != 16) TEST_ERROR

    /* Copy the datatype */
    if((dtype = H5Tcopy(dtype_tmp)) < 0) TEST_ERROR

    /* Verify that the compound is not packed */
    if(NULL == (dt = (H5T_t *) H5I_object_verify(dtype, H5I_DATATYPE)))
        TEST_ERROR
    if(dt->shared->u.compnd.packed) TEST_ERROR

    /* Verify that the order is the same as the insertion order */
    if(H5Tget_member_offset(dtype, 0) != 8) TEST_ERROR
    if(H5Tget_member_offset(dtype, 1) != 12) TEST_ERROR
    if(H5Tget_member_offset(dtype, 2) != 0) TEST_ERROR
    if(H5Tget_member_offset(dtype, 3) != 16) TEST_ERROR

    /* Insert the last member */
    if(H5Tinsert(dtype, "E", (size_t)4, H5T_STD_I32LE) < 0) TEST_ERROR

    /* Verify that the compound is packed */
    if(NULL == (dt = (H5T_t *) H5I_object_verify(dtype, H5I_DATATYPE)))
        TEST_ERROR
    if(!dt->shared->u.compnd.packed) TEST_ERROR

    /* Verify that the order is the same as the insertion order */
    if(H5Tget_member_offset(dtype, 0) != 8) TEST_ERROR
    if(H5Tget_member_offset(dtype, 1) != 12) TEST_ERROR
    if(H5Tget_member_offset(dtype, 2) != 0) TEST_ERROR
    if(H5Tget_member_offset(dtype, 3) != 16) TEST_ERROR
    if(H5Tget_member_offset(dtype, 4) != 4) TEST_ERROR

    /* Commit the modified datatype */
    if(H5Tcommit2(file, "dtype2", dtype, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Close and reopen the file */
    if(H5Tclose(dtype_tmp)) TEST_ERROR
    if(H5Tclose(dtype)) TEST_ERROR
    if(H5Fclose(file)) TEST_ERROR
    if((file = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Open the type, and verify status */
    if((dtype_tmp = H5Topen2(file, "dtype2", H5P_DEFAULT)) < 0) TEST_ERROR
    if(NULL == (dt = (H5T_t *) H5I_object_verify(dtype_tmp, H5I_DATATYPE)))
        TEST_ERROR
    if(!dt->shared->u.compnd.packed) TEST_ERROR
    if(H5Tget_member_offset(dtype_tmp, 0) != 8) TEST_ERROR
    if(H5Tget_member_offset(dtype_tmp, 1) != 12) TEST_ERROR
    if(H5Tget_member_offset(dtype_tmp, 2) != 0) TEST_ERROR
    if(H5Tget_member_offset(dtype_tmp, 3) != 16) TEST_ERROR
    if(H5Tget_member_offset(dtype_tmp, 4) != 4) TEST_ERROR

    /* Copy the datatype, and verify status */
    if((dtype = H5Tcopy(dtype_tmp)) < 0) TEST_ERROR
    if(NULL == (dt = (H5T_t *) H5I_object_verify(dtype, H5I_DATATYPE)))
        TEST_ERROR
    if(!dt->shared->u.compnd.packed) TEST_ERROR
    if(H5Tget_member_offset(dtype, 0) != 8) TEST_ERROR
    if(H5Tget_member_offset(dtype, 1) != 12) TEST_ERROR
    if(H5Tget_member_offset(dtype, 2) != 0) TEST_ERROR
    if(H5Tget_member_offset(dtype, 3) != 16) TEST_ERROR
    if(H5Tget_member_offset(dtype, 4) != 4) TEST_ERROR

    /* Expand the type, and verify that it became unpacked */
    if(H5Tset_size(dtype, (size_t)21) < 0) TEST_ERROR
    if(NULL == (dt = (H5T_t *) H5I_object_verify(dtype, H5I_DATATYPE)))
        TEST_ERROR
    if(dt->shared->u.compnd.packed) TEST_ERROR
    if(H5Tget_member_offset(dtype, 0) != 8) TEST_ERROR
    if(H5Tget_member_offset(dtype, 1) != 12) TEST_ERROR
    if(H5Tget_member_offset(dtype, 2) != 0) TEST_ERROR
    if(H5Tget_member_offset(dtype, 3) != 16) TEST_ERROR
    if(H5Tget_member_offset(dtype, 4) != 4) TEST_ERROR

    /* Shrink the type, and verify that it became packed */
    if(H5Tset_size(dtype, (size_t)20) < 0) TEST_ERROR
    if(NULL == (dt = (H5T_t *) H5I_object_verify(dtype, H5I_DATATYPE)))
        TEST_ERROR
    if(!dt->shared->u.compnd.packed) TEST_ERROR
    if(H5Tget_member_offset(dtype, 0) != 8) TEST_ERROR
    if(H5Tget_member_offset(dtype, 1) != 12) TEST_ERROR
    if(H5Tget_member_offset(dtype, 2) != 0) TEST_ERROR
    if(H5Tget_member_offset(dtype, 3) != 16) TEST_ERROR
    if(H5Tget_member_offset(dtype, 4) != 4) TEST_ERROR

    /* Close */
    if(H5Tclose(dtype_tmp)) TEST_ERROR
    if(H5Tclose(dtype)) TEST_ERROR
    if(H5Fclose(file)) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Tclose(dtype_tmp);
        H5Tclose(dtype);
        H5Fclose(file);
    } H5E_END_TRY
    puts("*** DATASET TESTS FAILED ***");
    return 1;
} /* test_ooo_order */


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Test different cases of I/O for compound data and the
 *              compound optimization for the Chicago company.
 *
 * Return:	Success:         0
 *
 *              Failure:         1
 *
 * Programmer:  Raymond Lu
 *              Friday, 15 June 2007
 *
 * Modifications:
 *-------------------------------------------------------------------------
 */
int
main (int argc, char *argv[])
{
    hid_t	fapl_id;
    char	fname[256];
    unsigned 	nerrors = 0;

    h5_reset();

    /* Turn off optimized compound converter? */
    if (argc>1) {
	if (argc>2 || strcmp("--noopt", argv[1])) {
	    fprintf(stderr, "usage: %s [--noopt]\n", argv[0]);
	    exit(1);
	}
	H5Tunregister(H5T_PERS_DONTCARE, NULL, -1, -1, H5T_conv_struct_opt);
    }

    /* Create the file */
    fapl_id = h5_fileaccess();

    h5_fixname(FILENAME[0], fapl_id, fname, sizeof(fname));

    puts("Testing compound dataset:");
    nerrors += test_compound(fname, fapl_id);

    puts("Testing the optimization of when the source type is a subset of the dest:");
    h5_fixname(FILENAME[1], fapl_id, fname, sizeof(fname));
    nerrors += test_hdf5_src_subset(fname, fapl_id);

    puts("Testing the optimization of when the dest type is a subset of the source:");
    h5_fixname(FILENAME[2], fapl_id, fname, sizeof(fname));
    nerrors += test_hdf5_dst_subset(fname, fapl_id);

    puts("Testing that compound types can be packed out of order:");
    nerrors += test_pack_ooo();

    puts("Testing compound member ordering:");
    nerrors += test_ooo_order(fname);

    /* Verify symbol table messages are cached */
    nerrors += (h5_verify_cached_stabs(FILENAME, fapl_id) < 0 ? 1 : 0);

    if (nerrors) {
        printf("***** %u FAILURE%s! *****\n",
               nerrors, 1==nerrors?"":"S");
        HDexit(1);
    }

    h5_cleanup(FILENAME, fapl_id);
    puts("All compound dataset tests passed.");
    return 0;
}
