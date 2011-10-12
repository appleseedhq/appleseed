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
 *              Tuesday, March 31, 1998
 *
 * Purpose:	Tests the global heap.  The global heap is the set of all
 *		collections but the collections are not related to one
 *		another by anything that appears in the file format.
 */
#include "h5test.h"
#include "H5private.h"
#include "H5Eprivate.h"
#include "H5Fprivate.h"
#include "H5Gprivate.h"
#include "H5HGprivate.h"
#include "H5Iprivate.h"
#include "H5Pprivate.h"

/* Macros for printing error messages in loops.  These print up to
 * GHEAP_REPEATED_ERR_LIM errors, and suppress the rest */
#define GHEAP_REPEATED_ERR_LIM 20

#define GHEAP_REPEATED_ERR(MSG)                                                \
{                                                                              \
    nerrors++;                                                                 \
    if(nerrors <= GHEAP_REPEATED_ERR_LIM) {                                    \
        H5_FAILED();                                                           \
        puts(MSG);                                                             \
        if(nerrors == GHEAP_REPEATED_ERR_LIM)                                  \
            puts("    Suppressing further errors...");                         \
    } /* end if */                                                             \
} /* end GHEAP_REPEATED_ERR */

const char *FILENAME[] = {
    "gheap1",
    "gheap2",
    "gheap3",
    "gheap4",
    "gheapooo",
    NULL
};


/*-------------------------------------------------------------------------
 * Function:	test_1
 *
 * Purpose:	Writes a sequence of objects to the global heap where each
 *		object is larger than the one before.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March 31, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_1 (hid_t fapl)
{
    hid_t	file = -1;
    H5F_t 	*f = NULL;
    H5HG_t	obj[1024];
    uint8_t	out[1024];
    uint8_t	in[1024];
    int		i;
    size_t	size;
    herr_t	status;
    int		nerrors = 0;
    char	filename[1024];

    TESTING("monotonically increasing lengths");

    /* Open a clean file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
	goto error;
    if(NULL == (f = (H5F_t *)H5I_object(file))) {
	H5_FAILED();
	puts("    Unable to create file");
	goto error;
    }

    /*
     * Write the objects, monotonically increasing in length.  Since this is
     * a clean file, the addresses allocated for the collections should also
     * be monotonically increasing.
     */
    for(i = 0; i < 1024; i++) {
	size = i + 1;
	HDmemset(out, 'A' + i % 26, size);
	H5Eclear2(H5E_DEFAULT);
	status = H5HG_insert(f, H5P_DATASET_XFER_DEFAULT, size, out, obj + i);
	if(status < 0) {
	    H5_FAILED();
	    puts("    Unable to insert object into global heap");
	    nerrors++;
	} else if(i && H5F_addr_gt(obj[i - 1].addr, obj[i].addr)) {
	    H5_FAILED();
	    puts("    Collection addresses are not monotonically increasing");
	    nerrors++;
	}
    }

    /*
     * Now try to read each object back.
     */
    for(i = 0; i < 1024; i++) {
	size = i + 1;
	HDmemset(out, 'A' + i % 26, size);
	H5Eclear2(H5E_DEFAULT);
	if(NULL == H5HG_read(f, H5P_DATASET_XFER_DEFAULT, obj + i, in, NULL)) {
	    H5_FAILED();
	    puts("    Unable to read object");
	    nerrors++;
	} else if(HDmemcmp(in, out, size)) {
	    H5_FAILED();
	    puts("    Value read doesn't match value written");
	    nerrors++;
	}
    }

    if(H5Fclose(file) < 0) goto error;
    if(nerrors) goto error;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return MAX(1, nerrors);
}


/*-------------------------------------------------------------------------
 * Function:	test_2
 *
 * Purpose:	Writes a sequence of objects to the global heap where each
 *		object is smaller than the one before.
 *
 * Return:	Success:	0
 *
 *		Failure:        number of errors
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March 31, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_2 (hid_t fapl)
{
    hid_t	file = -1;
    H5F_t 	*f = NULL;
    H5HG_t	obj[1024];
    uint8_t	out[1024];
    uint8_t	in[1024];
    int		i;
    size_t	size;
    int		nerrors = 0;
    char	filename[1024];

    TESTING("monotonically decreasing lengths");

    /* Open a clean file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
	goto error;
    if(NULL == (f = (H5F_t *)H5I_object(file))) {
	H5_FAILED();
	puts("    Unable to create file");
	goto error;
    }

    /*
     * Write the objects, monotonically decreasing in length.
     */
    for (i=0; i<1024; i++) {
	size = 1024-i;
	memset (out, 'A'+i%26, size);
	H5Eclear2(H5E_DEFAULT);
	if (H5HG_insert (f, H5P_DATASET_XFER_DEFAULT, size, out, obj+i)<0) {
	    H5_FAILED();
	    puts("    Unable to insert object into global heap");
	    nerrors++;
	}
    }

    /*
     * Now try to read each object back.
     */
    for (i=0; i<1024; i++) {
	size = 1024-i;
	memset (out, 'A'+i%26, size);
	H5Eclear2(H5E_DEFAULT);
	if (NULL==H5HG_read (f, H5P_DATASET_XFER_DEFAULT, obj+i, in, NULL)) {
	    H5_FAILED();
	    puts("    Unable to read object");
	    nerrors++;
	} else if (memcmp (in, out, size)) {
	    H5_FAILED();
	    puts("    Value read doesn't match value written");
	    nerrors++;
	}
    }

    if (H5Fclose(file)<0) goto error;
    if (nerrors) goto error;
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return MAX(1, nerrors);
}


/*-------------------------------------------------------------------------
 * Function:	test_3
 *
 * Purpose:	Creates a few global heap objects and then removes them all.
 *		The collection should also be removed.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March 31, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_3 (hid_t fapl)
{
    hid_t	file = -1;
    H5F_t 	*f = NULL;
    H5HG_t	obj[1024];
    uint8_t	out[1024];
    int		i;
    size_t	size;
    herr_t	status;
    int		nerrors = 0;
    char	filename[1024];

    TESTING("complete object removal");

    /* Open a clean file */
    h5_fixname(FILENAME[2], fapl, filename, sizeof filename);
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
	goto error;
    if(NULL == (f = (H5F_t *)H5I_object(file))) {
	H5_FAILED();
	puts("    Unable to create file");
	goto error;
    }

    /* Create some stuff */
    for (i=0; i<1024; i++) {
	size = i%30+100;
	memset (out, 'A'+i%26, size);
	H5Eclear2(H5E_DEFAULT);
	status = H5HG_insert (f, H5P_DATASET_XFER_DEFAULT, size, out, obj+i);
	if (status<0) {
	    H5_FAILED();
	    puts("    Unable to insert object into global heap");
	    nerrors++;
	}
    }

    /* Remove everything */
    for (i=0; i<1024; i++) {
	status = H5HG_remove (f, H5P_DATASET_XFER_DEFAULT, obj+i);
	if (status<0) {
	    H5_FAILED();
	    puts("    Unable to remove object");
	    nerrors++;
	}
    }

    if (H5Fclose(file)<0) goto error;
    if (nerrors) goto error;
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return MAX(1, nerrors);
}


/*-------------------------------------------------------------------------
 * Function:	test_4
 *
 * Purpose:	Tests the H5HG_remove() feature by writing lots of objects
 *		and occassionally removing some.  When we're done they're all
 *		removed.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March 31, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_4 (hid_t fapl)
{
    hid_t	file = -1;
    H5F_t 	*f = NULL;
    H5HG_t	obj[1024];
    uint8_t	out[1024];
    int		i;
    size_t	size;
    herr_t	status;
    int		nerrors = 0;
    char	filename[1024];

    TESTING("partial object removal");

    /* Open a clean file */
    h5_fixname(FILENAME[3], fapl, filename, sizeof filename);
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
	goto error;
    if(NULL == (f = (H5F_t *)H5I_object(file))) {
	H5_FAILED();
	puts("    Unable to create file");
	goto error;
    }

    for (i=0; i<1024; i++) {
	/* Insert */
	size = i%30+100;
	memset (out, 'A'+i%26, size);
	H5Eclear2(H5E_DEFAULT);
	status = H5HG_insert (f, H5P_DATASET_XFER_DEFAULT, size, out, obj+i);
	if (status<0) {
	    H5_FAILED();
	    puts("    Unable to insert object into global heap");
	    nerrors++;
	}

	/*
	 * Remove every third one beginning with the second, but after the
	 * next one has already been inserted.  That is, insert A, B, C;
	 * remove B, insert D, E, F; remove E; etc.
	 */
	if (1==i%3) {
	    H5Eclear2(H5E_DEFAULT);
	    status = H5HG_remove (f, H5P_DATASET_XFER_DEFAULT, obj+i-1);
	    if (status<0) {
		H5_FAILED();
		puts("    Unable to remove object");
		nerrors++;
	    }
	    memset (obj+i-1, 0, sizeof *obj);
	}
    }

    if (H5Fclose(file)<0) goto error;
    if (nerrors) goto error;
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return MAX(1, nerrors);
}


/*-------------------------------------------------------------------------
 * Function:	test_ooo_indices
 *
 * Purpose:	Tests that indices can be stored out of order.  This can
 *              happen when the indices "wrap around" due to many
 *              insertions and deletions (for example, from rewriting a
 *              VL dataset).
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Neil Fortner
 *              Monday, October 26, 2009
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_ooo_indices(hid_t fapl)
{
    hid_t	file = -1;
    H5F_t 	*f = NULL;
    unsigned	i, j;
    H5HG_t	*obj = NULL;
    herr_t	status;
    int		nerrors=0;
    char	filename[1024];

    TESTING("out of order indices");

    if(NULL == (obj = (H5HG_t *)HDmalloc(2000 * sizeof(*obj))))
        goto error;

    /* Open a clean file */
    h5_fixname(FILENAME[4], fapl, filename, sizeof filename);
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        goto error;
    if(NULL == (f = (H5F_t *)H5I_object(file))) {
        H5_FAILED();
        puts("    Unable to create file");
        goto error;
    } /* end if */

    /* Alternately insert 1000 entries and remove the previous group of 1000
     * entries, until the indices wrap around */
    for(i=0; i<66; i++) {
        /* Insert 1000 entries.  The index into the obj array will alternate up
         * and down by 1000 so the previous set of insertions is preserved and
         * can be deleted. */
        for(j=1000*((~i&1)); j<1000*((~i&1)+1); j++) {
            H5Eclear2(H5E_DEFAULT);
            status = H5HG_insert(f, H5P_DATASET_XFER_DEFAULT, sizeof(j), &j, &obj[j]);
            if (status<0)
                GHEAP_REPEATED_ERR("    Unable to insert object into global heap")

            /* Check that the index is as expected */
            if(obj[j].idx != ((1000 * i) + j - (1000 * ((~i & 1)))) % ((1u << 16) - 1) + 1)
                GHEAP_REPEATED_ERR("    Unexpected global heap index");
        } /* end for */

        /* Remove the previous 1000 entries */
        if(i>0)
            for(j=1000*(i&1); j<1000*((i&1)+1); j++) {
                H5Eclear2(H5E_DEFAULT);
                status = H5HG_remove(f, H5P_DATASET_XFER_DEFAULT, &obj[j]);
                if (status<0)
                    GHEAP_REPEATED_ERR("    Unable to remove object from global heap");
            } /* end for */
    } /* end for */

    /* The indices should have "wrapped around" on the last iteration */
    HDassert(obj[534].idx == 65535);
    HDassert(obj[535].idx == 1);

    /* Reopen the file */
    if (H5Fclose(file)<0) goto error;
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        goto error;
    if(NULL == (f = (H5F_t *)H5I_object(file))) {
        H5_FAILED();
        puts("    Unable to open file");
        goto error;
    } /* end if */

    /* Read the objects to make sure the heap is still readable */
    for(i=0; i<1000; i++) {
        if(NULL == H5HG_read(f, H5P_DATASET_XFER_DEFAULT, &obj[i], &j, NULL))
            goto error;
        if(i != j) {
            H5_FAILED();
            puts("    Incorrect read value");
            goto error;
        } /* end if */
    } /* end for */

    if (H5Fclose(file)<0) goto error;
    if (nerrors) goto error;
    HDfree(obj);
    obj = NULL;
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    if(obj)
        HDfree(obj);
    return MAX(1, nerrors);
} /* end test_ooo_indices */


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Tests global heap.
 *
 * Return:	Success:	zero
 *
 *		Failure:	non-zero
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March 31, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main (void)
{
    int		nerrors=0;
    hid_t	fapl;

    h5_reset();
    fapl = h5_fileaccess();

    nerrors += test_1(fapl);
    nerrors += test_2(fapl);
    nerrors += test_3(fapl);
    nerrors += test_4(fapl);
    nerrors += test_ooo_indices(fapl);

    /* Verify symbol table messages are cached */
    nerrors += (h5_verify_cached_stabs(FILENAME, fapl) < 0 ? 1 : 0);

    if (nerrors) goto error;

    puts("All global heap tests passed.");
    h5_cleanup(FILENAME, fapl);
    return 0;

 error:
    puts("*** TESTS FAILED ***");
    return 1;
}
