/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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

/* Programmer:  Neil Fortner
 *              December 16, 2010
 */

#include "h5test.h"

#define H5F_PACKAGE
#include "H5Fpkg.h"
#include "H5Iprivate.h"

const char *FILENAME[] = {
    "efc0",
    "efc1",
    "efc2",
    "efc3",
    "efc4",
    "efc5",
    NULL
};

/* Global patched filename buffer */
static char filename[6][1024];

/* Global property lists - just copies of the defaults (necessary to use
 * internal functions */
hid_t fcpl_id = -1;
hid_t fapl_id = -1;
hid_t dxpl_id = -1;


/*-------------------------------------------------------------------------
 * Function:    test_single
 *
 * Purpose:     Tests manipulations on a single external file cache.
 *
 * Return:      Success: 0
 *              Failure: Number of errors
 *
 * Programmer:  Neil Fortner
 *              December 16, 2010
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_single(void)
{
    H5F_t       *f0 = NULL;     /* Parent file containing EFC */
    H5F_t       *f1 = NULL;     /* Child file */
    H5F_t       *f2 = NULL;     /* Child file */
    H5F_t       *f3 = NULL;     /* Child file */
    H5F_t       *f4 = NULL;     /* Child file */
    H5F_t       *ftmp1 = NULL;  /* Temp file */
    H5F_t       *ftmp2 = NULL;  /* Temp file */
    H5F_t       *ftmp3 = NULL;  /* Temp file */
    H5F_t       *ftmp4 = NULL;  /* Temp file */

    TESTING("single EFC");

    /* Set EFC size to 3.  Do this instead of H5F_efc_create() so we can pass
     * a file pointer to H5F_efc_open containing the EFC. */
    if(H5Pset_elink_file_cache_size(fapl_id, 3) < 0)
        TEST_ERROR

    /* Open parent file */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR

    /* Disable EFC for child files */
    if(H5Pset_elink_file_cache_size(fapl_id, 0) < 0)
        TEST_ERROR


    /* Test 1: Open file 1 through EFC, close, then open normally, verify ref
     * count = 2, release EFC, verify ref count = 1.  Verifies a file can be
     * held open by the EFC. */
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (ftmp1 = H5F_open(filename[1], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(ftmp1->shared->nrefs != 2)
        TEST_ERROR
    if(H5F_efc_release(f0->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(ftmp1->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(ftmp1) < 0)
        FAIL_STACK_ERROR


    /* Test 2: Verify that subsequent efc_open requests return the cached top
     * level file pointer.  Open file 1 through EFC, close, open again, verify
     * file pointers are the same. */
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    ftmp1 = f1;
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(f1 != ftmp1)
        TEST_ERROR
    if(f1->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_release(f0->shared->efc) < 0)
        FAIL_STACK_ERROR


    /* Test 3: Verify LRU functionality.  Add four files to the EFC and verify
     * that the one added first is evicted.  Then reopen files in a different
     * order.  Open each file normally after closing through EFC the first time
     * to track ref counts. */
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_open(filename[1], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 2)
        TEST_ERROR

    if(NULL == (f2 = H5F_efc_open(f0, filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f2) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_open(filename[2], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 2)
        TEST_ERROR
    if(f2->shared->nrefs != 2)
        TEST_ERROR

    if(NULL == (f3 = H5F_efc_open(f0, filename[3],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f3) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_open(filename[3], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 2)
        TEST_ERROR
    if(f2->shared->nrefs != 2)
        TEST_ERROR
    if(f3->shared->nrefs != 2)
        TEST_ERROR

    if(NULL == (f4 = H5F_efc_open(f0, filename[4],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f4) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f4 = H5F_open(filename[4], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 1)
        TEST_ERROR
    if(f2->shared->nrefs != 2)
        TEST_ERROR
    if(f3->shared->nrefs != 2)
        TEST_ERROR
    if(f4->shared->nrefs != 2)
        TEST_ERROR

    if(NULL == (ftmp3 = H5F_efc_open(f0, filename[3], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, ftmp3) < 0)
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 1)
        TEST_ERROR
    if(f2->shared->nrefs != 2)
        TEST_ERROR
    if(f3->shared->nrefs != 2)
        TEST_ERROR
    if(f4->shared->nrefs != 2)
        TEST_ERROR

    if(NULL == (ftmp2 = H5F_efc_open(f0, filename[2], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, ftmp2) < 0)
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 1)
        TEST_ERROR
    if(f2->shared->nrefs != 2)
        TEST_ERROR
    if(f3->shared->nrefs != 2)
        TEST_ERROR
    if(f4->shared->nrefs != 2)
        TEST_ERROR

    if(NULL == (ftmp1 = H5F_efc_open(f0, filename[1], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, ftmp1) < 0)
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 2)
        TEST_ERROR
    if(f2->shared->nrefs != 2)
        TEST_ERROR
    if(f3->shared->nrefs != 2)
        TEST_ERROR
    if(f4->shared->nrefs != 1)
        TEST_ERROR

    if(NULL == (ftmp4 = H5F_efc_open(f0, filename[4], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, ftmp4) < 0)
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 2)
        TEST_ERROR
    if(f2->shared->nrefs != 2)
        TEST_ERROR
    if(f3->shared->nrefs != 1)
        TEST_ERROR
    if(f4->shared->nrefs != 2)
        TEST_ERROR

    if(H5F_efc_release(f0->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 1)
        TEST_ERROR
    if(f2->shared->nrefs != 1)
        TEST_ERROR
    if(f3->shared->nrefs != 1)
        TEST_ERROR
    if(f4->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f1) < 0)
        FAIL_STACK_ERROR
    if(H5F_try_close(f2) < 0)
        FAIL_STACK_ERROR
    if(H5F_try_close(f3) < 0)
        FAIL_STACK_ERROR
    if(H5F_try_close(f4) < 0)
        FAIL_STACK_ERROR


    /* Test 4: Verify that files kept open through the EFC are not evicted by
     * H5F_efc_release(). */
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp1 = H5F_open(filename[1], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(ftmp1->shared->nrefs != 2)
        TEST_ERROR
    if(H5F_efc_release(f0->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(ftmp1->shared->nrefs != 2)
        TEST_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(ftmp1->shared->nrefs != 2)
        TEST_ERROR
    if(H5F_efc_release(f0->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(ftmp1->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(ftmp1) < 0)
        FAIL_STACK_ERROR


    /* Test 5: Verify that files kept open through the EFC are not evicted by
     * filling up the cache.  Open 4 files while holding the first open.  Verify
     * that the second file is evicted.  Close the first file, reopen the
     * second, and verify that the first file is evicted. */
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp1 = H5F_open(filename[1], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(ftmp1->shared != f1->shared)
        TEST_ERROR
    if(ftmp1->shared->nrefs != 2)
        TEST_ERROR

    if(NULL == (f2 = H5F_efc_open(f0, filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f2) < 0)
        FAIL_STACK_ERROR
    if(NULL == (ftmp2 = H5F_open(filename[2], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(ftmp2->shared->nrefs != 2)
        TEST_ERROR

    if(NULL == (f3 = H5F_efc_open(f0, filename[3],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f3) < 0)
        FAIL_STACK_ERROR
    if(ftmp2->shared->nrefs != 2)
        TEST_ERROR

    if(NULL == (f4 = H5F_efc_open(f0, filename[4],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f4) < 0)
        FAIL_STACK_ERROR
    if(ftmp1->shared->nrefs != 2)
        TEST_ERROR
    if(ftmp2->shared->nrefs != 1)
        TEST_ERROR

    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(ftmp1->shared->nrefs != 2)
        TEST_ERROR
    if(NULL == (f2 = H5F_efc_open(f0, filename[2], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f2) < 0)
        FAIL_STACK_ERROR
    if(ftmp1->shared->nrefs != 1)
        TEST_ERROR
    if(ftmp2->shared->nrefs != 2)
        TEST_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(ftmp1->shared->nrefs != 2)
        TEST_ERROR
    if(ftmp2->shared->nrefs != 2)
        TEST_ERROR

    if(H5F_efc_release(f0->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(ftmp1->shared->nrefs != 1)
        TEST_ERROR
    if(ftmp2->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(ftmp1) < 0)
        FAIL_STACK_ERROR
    if(H5F_try_close(ftmp2) < 0)
        FAIL_STACK_ERROR


    /* Test 6: Verify that having a full EFC filled only with open files
     * prevents further files from being cached.  Open and hold open 3 files
     * through the EFC, then open the fourth and verify that it was not added to
     * the EFC. */
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp1 = H5F_open(filename[1], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(ftmp1->shared != f1->shared)
        TEST_ERROR
    if(ftmp1->shared->nrefs != 2)
        TEST_ERROR

    if(NULL == (f2 = H5F_efc_open(f0, filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp2 = H5F_open(filename[2], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(ftmp2->shared != f2->shared)
        TEST_ERROR
    if(ftmp2->shared->nrefs != 2)
        TEST_ERROR

    if(NULL == (f3 = H5F_efc_open(f0, filename[3],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp3 = H5F_open(filename[3], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(ftmp3->shared != f3->shared)
        TEST_ERROR
    if(ftmp3->shared->nrefs != 2)
        TEST_ERROR

    if(NULL == (f4 = H5F_efc_open(f0, filename[4],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f4) < 0)
        FAIL_STACK_ERROR
    if(NULL == (ftmp4 = H5F_open(filename[4], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(ftmp4->shared->nrefs != 1)
        TEST_ERROR

    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f2) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f3) < 0)
        FAIL_STACK_ERROR
    if(ftmp1->shared->nrefs != 2)
        TEST_ERROR
    if(ftmp2->shared->nrefs != 2)
        TEST_ERROR
    if(ftmp3->shared->nrefs != 2)
        TEST_ERROR
    if(H5F_efc_release(f0->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(ftmp1->shared->nrefs != 1)
        TEST_ERROR
    if(ftmp2->shared->nrefs != 1)
        TEST_ERROR
    if(ftmp3->shared->nrefs != 1)
        TEST_ERROR
    if(ftmp4->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(ftmp1) < 0)
        FAIL_STACK_ERROR
    if(H5F_try_close(ftmp2) < 0)
        FAIL_STACK_ERROR
    if(H5F_try_close(ftmp3) < 0)
        FAIL_STACK_ERROR
    if(H5F_try_close(ftmp4) < 0)
        FAIL_STACK_ERROR


    /* Test 7: Test multiple file opens.  Open a file twice, close it once, then
     * verify that it is not evicted by H5F_efc_release(). */
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp1 = H5F_open(filename[1], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(ftmp1->shared->nrefs != 2)
        TEST_ERROR
    if(H5F_efc_close(f0, f2) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_release(f0->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(ftmp1->shared->nrefs != 2)
        TEST_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(ftmp1->shared->nrefs != 2)
        TEST_ERROR
    if(H5F_efc_release(f0->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(ftmp1->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(ftmp1) < 0)
        FAIL_STACK_ERROR


    /* Close parent file */
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR

    PASSED();

    return 0;

error:
    return 1;
} /* test_single */


/*-------------------------------------------------------------------------
 * Function:    test_graph_nocycle
 *
 * Purpose:     Tests manipulations on a graph of files with external file
 *              caches.  The graph does not contain cycles.
 *
 * Return:      Success: 0
 *              Failure: Number of errors
 *
 * Programmer:  Neil Fortner
 *              January 4, 2011
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_graph_nocycle(void)
{
    H5F_t       *f0 = NULL;     /* Parent file containing EFC */
    H5F_t       *f1 = NULL;     /* Child file */
    H5F_t       *f2 = NULL;     /* Child file */
    H5F_t       *f3 = NULL;     /* Child file */
    H5F_t       *f4 = NULL;     /* Child file */
    H5F_t       *ftmp1 = NULL;  /* Temp file */
    H5F_t       *ftmp2 = NULL;  /* Temp file */
    H5F_t       *ftmp3 = NULL;  /* Temp file */
    H5F_t       *ftmp4 = NULL;  /* Temp file */

    TESTING("graph of EFCs without cycles");

    /* Set EFC size to 8.  Do this instead of H5F_efc_create() so we can pass
     * a file pointer to H5F_efc_open containing the EFC.  Set to a high number
     * because we don't test the EFC becoming too large in this test. */
    if(H5Pset_elink_file_cache_size(fapl_id, 8) < 0)
        TEST_ERROR


    /* Test 1: Simple 3 file chain.  Open file 1 through file 0, then open file
     * 2 through file 1.  Release file 0's EFC and verify that file 2 gets its
     * ref count reduced (implying file 1 was closed).  Do the same with the
     * opening order reversed. */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_efc_open(f1, filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, f2) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (ftmp2 = H5F_open(filename[2], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(ftmp2->shared->nrefs != 2)
        TEST_ERROR
    if(H5F_efc_release(f0->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(ftmp2->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(ftmp2) < 0)
        FAIL_STACK_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR

    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp1 = H5F_open(filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_efc_open(ftmp1, filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_try_close(ftmp1) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, f2) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (ftmp2 = H5F_open(filename[2], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(ftmp2->shared->nrefs != 2)
        TEST_ERROR
    if(H5F_efc_release(f0->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(ftmp2->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(ftmp2) < 0)
        FAIL_STACK_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR


    /* Test 2: 5 file chain.  The parent file has 2 child files, each of which
     * has their own child file.  Verifies that releasing the parent's EFC
     * closes all 4 children. */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR

    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_efc_open(f1, filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, f2) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (ftmp2 = H5F_open(filename[2], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(ftmp2->shared->nrefs != 2)
        TEST_ERROR

    if(NULL == (f3 = H5F_efc_open(f0, filename[3],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f4 = H5F_efc_open(f3, filename[4],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f3, f4) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f3) < 0)
        FAIL_STACK_ERROR
    if(NULL == (ftmp4 = H5F_open(filename[4], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(ftmp4->shared->nrefs != 2)
        TEST_ERROR

    if(H5F_efc_release(f0->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(ftmp2->shared->nrefs != 1)
        TEST_ERROR
    if(ftmp4->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(ftmp2) < 0)
        FAIL_STACK_ERROR
    if(H5F_try_close(ftmp4) < 0)
        FAIL_STACK_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR


    /* Test 3: Simple "inverted" tree.  Two parent files share a child file,
     * which has its own child file.  Verify that the child's child is not
     * closed until both parents' EFCs are released.  First release through one
     * parent, then reopen through that parent and release the other, then 
     * re-release the first parent. */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_open(filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_efc_open(f0, filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp2 = H5F_efc_open(f1, filename[2], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_efc_open(f2, filename[3],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, f3) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f2) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, ftmp2) < 0)
        FAIL_STACK_ERROR
    if(NULL == (ftmp3 = H5F_open(filename[3], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(ftmp3->shared->nrefs != 2)
        TEST_ERROR

    if(H5F_efc_release(f0->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(ftmp3->shared->nrefs != 2)
        TEST_ERROR

    if(NULL == (f2 = H5F_efc_open(f0, filename[2], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f2) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_release(f1->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(ftmp3->shared->nrefs != 2)
        TEST_ERROR

    if(H5F_efc_release(f0->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(ftmp3->shared->nrefs != 1)
        TEST_ERROR

    if(H5F_try_close(ftmp3) < 0)
        FAIL_STACK_ERROR
    if(H5F_try_close(f1) < 0)
        FAIL_STACK_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR


    /* Test 4: Simple "diamond" tree.  The parent file has two children, which
     * shared the same child.  Verify that releasing the parent file closes all
     * files. */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_efc_open(f0, filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_efc_open(f2, filename[3],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, f3) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_efc_open(f1, filename[3], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, f3) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f2) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (ftmp3 = H5F_open(filename[3], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(ftmp3->shared->nrefs != 3)
        TEST_ERROR

    if(H5F_efc_release(f0->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(ftmp3->shared->nrefs != 1)
        TEST_ERROR

    if(H5F_try_close(ftmp3) < 0)
        FAIL_STACK_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR


    /* Test 5: Dense 5 file graph.  f0 caches f1, f2, f3 and f4.  f1 and f2
     * each cache f3 and f4.  f3 caches f4.  Verify that releasing f0 closes all
     * files. */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_efc_open(f0, filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_efc_open(f0, filename[3],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f4 = H5F_efc_open(f0, filename[4],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f4) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f3) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_efc_open(f1, filename[3], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f4 = H5F_efc_open(f1, filename[4], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, f4) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, f3) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_efc_open(f2, filename[3], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f4 = H5F_efc_open(f2, filename[4], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, f4) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f2) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f4 = H5F_efc_open(f3, filename[4], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f3, f4) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, f3) < 0)
        FAIL_STACK_ERROR
    if(NULL == (ftmp4 = H5F_open(filename[4], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(ftmp4->shared->nrefs != 5)
        TEST_ERROR

    if(H5F_efc_release(f0->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(ftmp4->shared->nrefs != 1)
        TEST_ERROR
    if(NULL == (ftmp1 = H5F_open(filename[1], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(ftmp1->shared->nrefs != 1)
        TEST_ERROR
    if(NULL == (ftmp2 = H5F_open(filename[2], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(ftmp2->shared->nrefs != 1)
        TEST_ERROR
    if(NULL == (ftmp3 = H5F_open(filename[3], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(ftmp3->shared->nrefs != 1)
        TEST_ERROR

    if(H5F_try_close(ftmp1) < 0)
        FAIL_STACK_ERROR
    if(H5F_try_close(ftmp2) < 0)
        FAIL_STACK_ERROR
    if(H5F_try_close(ftmp3) < 0)
        FAIL_STACK_ERROR
    if(H5F_try_close(ftmp4) < 0)
        FAIL_STACK_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR


    PASSED();

    return 0;

error:
    return 1;
} /* test_graph_nocycle */


/*-------------------------------------------------------------------------
 * Function:    test_graph_cycle
 *
 * Purpose:     Tests manipulations on a graph of files with external file
 *              caches containing cycles.
 *
 * Return:      Success: 0
 *              Failure: Number of errors
 *
 * Programmer:  Neil Fortner
 *              January 6, 2011
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_graph_cycle(void)
{
    H5F_t       *f0 = NULL;     /* File */
    H5F_t       *f1 = NULL;     /* File */
    H5F_t       *f2 = NULL;     /* File */
    H5F_t       *f3 = NULL;     /* File */
    H5F_t       *f4 = NULL;     /* File */
    H5F_t       *f5 = NULL;     /* File */
    H5F_t       *ftmp0 = NULL;  /* Temp file */
    H5F_t       *ftmp1 = NULL;  /* Temp file */
    H5F_t       *ftmp2 = NULL;  /* Temp file */
    H5F_t       *ftmp3 = NULL;  /* Temp file */

    TESTING("graph of EFCs with cycles");

    /* Set EFC size to 8.  Do this instead of H5F_efc_create() so we can pass
     * a file pointer to H5F_efc_open containing the EFC.  Set to a high number
     * because we don't test the EFC becoming too large in this test. */
    if(H5Pset_elink_file_cache_size(fapl_id, 8) < 0)
        TEST_ERROR


    /* Test 1: File caches itself.  Verify that closing the file causes it to be
     * actually closed, and there is no other unexpected behavior. */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp0 = H5F_efc_open(f0, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR
    if(H5F_efc_release(f0->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR

    if(NULL == (ftmp0 = H5F_efc_open(f0, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR

    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR


    /* Test 2: Indirectly referenced file caches itself.  Same as above except
     * the file is part of another file's EFC. */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp1 = H5F_efc_open(f1, filename[1], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, ftmp1) < 0)
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 2)
        TEST_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_release(f0->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_open(filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f1) < 0)
        FAIL_STACK_ERROR

    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp1 = H5F_efc_open(f1, filename[1], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, ftmp1) < 0)
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 2)
        TEST_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_open(filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f1) < 0)
        FAIL_STACK_ERROR


    /* Test 3: Simple 2 file cycle */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp0 = H5F_efc_open(f1, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR
    if(H5F_efc_release(f0->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR

    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp0 = H5F_efc_open(f1, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR


    /* Test 4: Simple 2 file cycle (indirectly referenced) */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_efc_open(f1, filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp1 = H5F_efc_open(f2, filename[1], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, ftmp1) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, f2) < 0)
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 2)
        TEST_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_open(filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f1) < 0)
        FAIL_STACK_ERROR


    /* Test 5: Parallel double cycle */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp0 = H5F_efc_open(f1, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_efc_open(f0, filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp0 = H5F_efc_open(f2, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f2) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 3)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR


    /* Test 6: Parallel double cycle with release */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp0 = H5F_efc_open(f1, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_efc_open(f0, filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp0 = H5F_efc_open(f2, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f2) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 3)
        TEST_ERROR
    if(H5F_efc_release(f0->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR


    /* Test 7: Chained parallel double cycle */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp0 = H5F_efc_open(f1, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_efc_open(f1, filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp1 = H5F_efc_open(f2, filename[1], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, ftmp1) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, f2) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_efc_open(f1, filename[3],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp1 = H5F_efc_open(f3, filename[1], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f3, ftmp1) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, f3) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR


    /* Test 8: Chained parallel double cycle with release */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp0 = H5F_efc_open(f1, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_efc_open(f1, filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp1 = H5F_efc_open(f2, filename[1], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, ftmp1) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, f2) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_efc_open(f1, filename[3],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp1 = H5F_efc_open(f3, filename[1], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f3, ftmp1) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, f3) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR
    if(H5F_efc_release(f0->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR


    /* Test 9: Simple 2 file cycle, extra ID on root */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp0 = H5F_open(filename[0], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp1 = H5F_efc_open(f1, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, ftmp1) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(f0->shared != ftmp0->shared)
        TEST_ERROR
    if(f0->shared->nrefs != 3)
        TEST_ERROR
    if(H5F_try_close(ftmp0) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR


    /* Test 10: Simple 2 file cycle, extra ID on second file */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp1 = H5F_open(filename[1], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp0 = H5F_efc_open(f1, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR
    if(ftmp1->shared->nrefs != 2)
        TEST_ERROR

    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(ftmp1->shared->nrefs != 2)
        TEST_ERROR
    if(NULL == (f0 = H5F_open(filename[0], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR

    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(H5F_try_close(ftmp1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_open(filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f1) < 0)
        FAIL_STACK_ERROR


    /* Test 11: Parallel double cycle, extra ID on a child file */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp0 = H5F_efc_open(f1, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_efc_open(f0, filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp2 = H5F_open(filename[2], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp0 = H5F_efc_open(f2, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f2) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 3)
        TEST_ERROR
    if(ftmp2->shared->nrefs != 2)
        TEST_ERROR

    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(ftmp2->shared->nrefs != 2)
        TEST_ERROR
    if(NULL == (f0 = H5F_open(filename[0], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 3)
        TEST_ERROR

    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(H5F_try_close(ftmp2) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_open(filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_open(filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f2->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f2) < 0)
        FAIL_STACK_ERROR


    /* Test 12: Parallel double cycle, extra ID on a child file, with release */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp0 = H5F_efc_open(f1, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_efc_open(f0, filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp2 = H5F_open(filename[2], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp0 = H5F_efc_open(f2, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f2) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 3)
        TEST_ERROR
    if(ftmp2->shared->nrefs != 2)
        TEST_ERROR

    if(H5F_efc_release(f0->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR
    if(ftmp2->shared->nrefs != 1)
        TEST_ERROR

    if(H5F_efc_release(ftmp2->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR
    if(ftmp2->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(H5F_try_close(ftmp2) < 0)
        FAIL_STACK_ERROR


    /* Test 13: Chained parallel double cycle, extra ID on a child file */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp0 = H5F_efc_open(f1, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_efc_open(f1, filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp1 = H5F_efc_open(f2, filename[1], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, ftmp1) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, f2) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_efc_open(f1, filename[3],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp3 = H5F_open(filename[3], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp1 = H5F_efc_open(f3, filename[1], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f3, ftmp1) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, f3) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR
    if(ftmp3->shared->nrefs != 2)
        TEST_ERROR

    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(ftmp3->shared->nrefs != 2)
        TEST_ERROR
    if(NULL == (f0 = H5F_open(filename[0], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR

    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(H5F_try_close(ftmp3) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_open(filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_open(filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f2->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f2) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_open(filename[3],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f3->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f3) < 0)
        FAIL_STACK_ERROR


    /* Test 14: Chained parallel double cycle, extra ID on a child file, with
     * release */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp0 = H5F_efc_open(f1, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_efc_open(f1, filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp1 = H5F_efc_open(f2, filename[1], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, ftmp1) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, f2) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_efc_open(f1, filename[3],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp3 = H5F_open(filename[3], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp1 = H5F_efc_open(f3, filename[1], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f3, ftmp1) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, f3) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR
    if(ftmp3->shared->nrefs != 2)
        TEST_ERROR

    if(H5F_efc_release(f0->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR
    if(ftmp3->shared->nrefs != 2)
        TEST_ERROR

    if(H5F_efc_release(ftmp3->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR
    if(ftmp3->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(H5F_try_close(ftmp3) < 0)
        FAIL_STACK_ERROR


    /* Test 15: One local and one remote cycle */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp0 = H5F_efc_open(f1, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_efc_open(f0, filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_efc_open(f2, filename[3],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp2 = H5F_efc_open(f3, filename[2], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f3, ftmp2) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, f3) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f2) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR

    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_open(filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_open(filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f2->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f2) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_open(filename[3],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f3->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f3) < 0)
        FAIL_STACK_ERROR


    /* Test 16: One local and one remote cycle, with release */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp0 = H5F_efc_open(f1, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_efc_open(f0, filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_efc_open(f2, filename[3],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp2 = H5F_efc_open(f3, filename[2], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f3, ftmp2) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, f3) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f2) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR

    if(H5F_efc_release(f0->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_open(filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_open(filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f2->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f2) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_open(filename[3],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f3->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f3) < 0)
        FAIL_STACK_ERROR


    /* Test 17: One local and one remote cycle, remote cycle held open */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp0 = H5F_efc_open(f1, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_efc_open(f0, filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_efc_open(f2, filename[3],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp3 = H5F_open(filename[3], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp2 = H5F_efc_open(f3, filename[2], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f3, ftmp2) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, f3) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f2) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR

    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(ftmp3->shared->nrefs != 2)
        TEST_ERROR

    if(H5F_try_close(ftmp3) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_open(filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_open(filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f2->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f2) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_open(filename[3],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f3->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f3) < 0)
        FAIL_STACK_ERROR


    /* Test 18: One local and one remote cycle, remote cycle held open, with
     * release */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp0 = H5F_efc_open(f1, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_efc_open(f0, filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_efc_open(f2, filename[3],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp3 = H5F_open(filename[3], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp2 = H5F_efc_open(f3, filename[2], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f3, ftmp2) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, f3) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f2) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR

    if(H5F_efc_release(f0->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR
    if(ftmp3->shared->nrefs != 2)
        TEST_ERROR
    if(NULL == (f1 = H5F_open(filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_open(filename[2], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f2->shared->nrefs != 2)
        TEST_ERROR
    if(H5F_try_close(f2) < 0)
        FAIL_STACK_ERROR

    if(H5F_efc_release(ftmp3->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(ftmp3->shared->nrefs != 1)
        TEST_ERROR
    if(NULL == (f2 = H5F_open(filename[2], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f2->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f2) < 0)
        FAIL_STACK_ERROR

    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(H5F_try_close(ftmp3) < 0)
        FAIL_STACK_ERROR


    /* Test 19: "Diamond" shape with links moving from bottom (root) to top.
     * Also cycle between bottom (root) and top and cycles on the sides. */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_efc_open(f0, filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_efc_open(f1, filename[3],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, f3) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_efc_open(f2, filename[3], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, f3) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_efc_open(f0, filename[3], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp0 = H5F_efc_open(f3, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f3, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f3) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f4 = H5F_efc_open(f1, filename[4],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp1 = H5F_efc_open(f4, filename[1], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f4, ftmp1) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, f4) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f5 = H5F_efc_open(f2, filename[5],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp2 = H5F_efc_open(f5, filename[2], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f5, ftmp2) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, f5) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f2) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR

    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_open(filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_open(filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f2->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f2) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_open(filename[3],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f3->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f3) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f4 = H5F_open(filename[4],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f4->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f4) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f5 = H5F_open(filename[5],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f5->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f5) < 0)
        FAIL_STACK_ERROR


    /* Test 20: "Diamond" shape with links moving from bottom (root) to top.
     * Also cycle between bottom (root) and top, cycles on the sides, and
     * release the files instead of closing. */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_efc_open(f0, filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_efc_open(f1, filename[3],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, f3) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_efc_open(f2, filename[3], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, f3) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_efc_open(f0, filename[3], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp0 = H5F_efc_open(f3, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f3, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f3) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f4 = H5F_efc_open(f1, filename[4],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp1 = H5F_efc_open(f4, filename[1], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f4, ftmp1) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, f4) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f5 = H5F_efc_open(f2, filename[5],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp2 = H5F_efc_open(f5, filename[2], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f5, ftmp2) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, f5) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f2) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR

    if(H5F_efc_release(f0->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_open(filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_open(filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f2->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f2) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_open(filename[3],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f3->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f3) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f4 = H5F_open(filename[4],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f4->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f4) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f5 = H5F_open(filename[5],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f5->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f5) < 0)
        FAIL_STACK_ERROR


    /* Test 21: "Diamond" shape with links moving from bottom (root) to top.
     * Also cycle between bottom (root) and top, cycles on sides held open. */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_efc_open(f0, filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_efc_open(f1, filename[3],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, f3) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_efc_open(f2, filename[3], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, f3) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_efc_open(f0, filename[3], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp0 = H5F_efc_open(f3, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f3, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f3) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f4 = H5F_efc_open(f1, filename[4],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp1 = H5F_efc_open(f4, filename[1], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f4, ftmp1) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, f4) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f4 = H5F_open(filename[4], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f5 = H5F_efc_open(f2, filename[5],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp2 = H5F_efc_open(f5, filename[2], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f5, ftmp2) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, f5) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f5 = H5F_open(filename[5], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f2) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR
    if(f4->shared->nrefs != 2)
        TEST_ERROR
    if(f5->shared->nrefs != 2)
        TEST_ERROR

    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(f4->shared->nrefs != 2)
        TEST_ERROR
    if(f5->shared->nrefs != 2)
        TEST_ERROR
    if(NULL == (f0 = H5F_open(filename[0], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR

    if(H5F_try_close(f4) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR
    if(f5->shared->nrefs != 2)
        TEST_ERROR

    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(f5->shared->nrefs != 2)
        TEST_ERROR
    if(NULL == (f0 = H5F_open(filename[0], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR

    if(H5F_try_close(f5) < 0)
        FAIL_STACK_ERROR

    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_open(filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_open(filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f2->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f2) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_open(filename[3],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f3->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f3) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f4 = H5F_open(filename[4],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f4->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f4) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f5 = H5F_open(filename[5],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f5->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f5) < 0)
        FAIL_STACK_ERROR


    /* Test 22: "Diamond" shape with links moving from bottom (root) to top.
     * Also cycle between bottom (root) and top, cycles on sides held open.
     * Also release the files instead of closing. */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_efc_open(f0, filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_efc_open(f1, filename[3],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, f3) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_efc_open(f2, filename[3], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, f3) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_efc_open(f0, filename[3], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp0 = H5F_efc_open(f3, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f3, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f3) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f4 = H5F_efc_open(f1, filename[4],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp1 = H5F_efc_open(f4, filename[1], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f4, ftmp1) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, f4) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f4 = H5F_open(filename[4], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f5 = H5F_efc_open(f2, filename[5],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp2 = H5F_efc_open(f5, filename[2], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f5, ftmp2) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, f5) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f5 = H5F_open(filename[5], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f2) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR
    if(f4->shared->nrefs != 2)
        TEST_ERROR
    if(f5->shared->nrefs != 2)
        TEST_ERROR

    if(H5F_efc_release(f0->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR
    if(f4->shared->nrefs != 2)
        TEST_ERROR
    if(f5->shared->nrefs != 2)
        TEST_ERROR
    if(NULL == (f3 = H5F_open(filename[3], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f3->shared->nrefs != 3)
        TEST_ERROR
    if(H5F_try_close(f3) < 0)
        FAIL_STACK_ERROR

    if(H5F_efc_release(f4->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR
    if(f4->shared->nrefs != 1)
        TEST_ERROR
    if(f5->shared->nrefs != 2)
        TEST_ERROR
    if(NULL == (f3 = H5F_open(filename[3], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f3->shared->nrefs != 2)
        TEST_ERROR
    if(H5F_try_close(f3) < 0)
        FAIL_STACK_ERROR

    if(H5F_efc_release(f5->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR
    if(f4->shared->nrefs != 1)
        TEST_ERROR
    if(f5->shared->nrefs != 1)
        TEST_ERROR
    if(NULL == (f3 = H5F_open(filename[3], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f3->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f3) < 0)
        FAIL_STACK_ERROR

    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(H5F_try_close(f4) < 0)
        FAIL_STACK_ERROR
    if(H5F_try_close(f5) < 0)
        FAIL_STACK_ERROR


    /* Test 23: Dense "ball" of files.  4 files each cache all files (including
     * itself). */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp0 = H5F_efc_open(f0, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_efc_open(f0, filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_efc_open(f0, filename[3],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR

    if(NULL == (ftmp0 = H5F_efc_open(f1, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (ftmp1 = H5F_efc_open(f1, filename[1], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, ftmp1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (ftmp2 = H5F_efc_open(f1, filename[2], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, ftmp2) < 0)
        FAIL_STACK_ERROR
    if(NULL == (ftmp3 = H5F_efc_open(f1, filename[3], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, ftmp3) < 0)
        FAIL_STACK_ERROR

    if(NULL == (ftmp0 = H5F_efc_open(f2, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (ftmp1 = H5F_efc_open(f2, filename[1], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, ftmp1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (ftmp2 = H5F_efc_open(f2, filename[2], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, ftmp2) < 0)
        FAIL_STACK_ERROR
    if(NULL == (ftmp3 = H5F_efc_open(f2, filename[3], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, ftmp3) < 0)
        FAIL_STACK_ERROR

    if(NULL == (ftmp0 = H5F_efc_open(f3, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f3, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (ftmp1 = H5F_efc_open(f3, filename[1], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f3, ftmp1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (ftmp2 = H5F_efc_open(f3, filename[2], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f3, ftmp2) < 0)
        FAIL_STACK_ERROR
    if(NULL == (ftmp3 = H5F_efc_open(f3, filename[3], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f3, ftmp3) < 0)
        FAIL_STACK_ERROR

    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f2) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f3) < 0)
        FAIL_STACK_ERROR

    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_open(filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_open(filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f2->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f2) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_open(filename[3],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f3->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f3) < 0)
        FAIL_STACK_ERROR


    /* Test 24: Dense "ball" of files.  4 files each cache all files (including
     * itself).  Release the files instead of closing. */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp0 = H5F_efc_open(f0, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_efc_open(f0, filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_efc_open(f0, filename[3],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR

    if(NULL == (ftmp0 = H5F_efc_open(f1, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (ftmp1 = H5F_efc_open(f1, filename[1], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, ftmp1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (ftmp2 = H5F_efc_open(f1, filename[2], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, ftmp2) < 0)
        FAIL_STACK_ERROR
    if(NULL == (ftmp3 = H5F_efc_open(f1, filename[3], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, ftmp3) < 0)
        FAIL_STACK_ERROR

    if(NULL == (ftmp0 = H5F_efc_open(f2, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (ftmp1 = H5F_efc_open(f2, filename[1], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, ftmp1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (ftmp2 = H5F_efc_open(f2, filename[2], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, ftmp2) < 0)
        FAIL_STACK_ERROR
    if(NULL == (ftmp3 = H5F_efc_open(f2, filename[3], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, ftmp3) < 0)
        FAIL_STACK_ERROR

    if(NULL == (ftmp0 = H5F_efc_open(f3, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f3, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (ftmp1 = H5F_efc_open(f3, filename[1], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f3, ftmp1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (ftmp2 = H5F_efc_open(f3, filename[2], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f3, ftmp2) < 0)
        FAIL_STACK_ERROR
    if(NULL == (ftmp3 = H5F_efc_open(f3, filename[3], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f3, ftmp3) < 0)
        FAIL_STACK_ERROR

    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f2) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f3) < 0)
        FAIL_STACK_ERROR

    if(H5F_efc_release(f0->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_open(filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_open(filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f2->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f2) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_open(filename[3],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f3->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f3) < 0)
        FAIL_STACK_ERROR


    /* Test 25: File held open by EFC client interrupts cycle, with release */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp0 = H5F_efc_open(f1, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR
    if(f1->shared->nrefs != 1)
        TEST_ERROR

    if(H5F_efc_release(f0->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR
    if(f1->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR

    if(H5F_efc_release(f0->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR


    /* Test 26: File held open by EFC does not interrupt cycle, with release */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp0 = H5F_efc_open(f1, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_efc_open(f0, filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR
    if(f2->shared->nrefs != 1)
        TEST_ERROR

    if(H5F_efc_release(f0->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR
    if(f2->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_efc_close(f0, f2) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR


    /* Test 27: File held open by EFC client through non-parent file does not
     * interrupt cycle, but parent file does (no valid way around it) */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp0 = H5F_efc_open(f1, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_open(filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f2, filename[1], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_efc_open(f1, filename[3],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR
    if(f1->shared->nrefs != 2)
        TEST_ERROR
    if(f2->shared->nrefs != 1)
        TEST_ERROR
    if(f3->shared->nrefs != 1)
        TEST_ERROR

    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 2)
        TEST_ERROR
    if(f2->shared->nrefs != 1)
        TEST_ERROR
    if(f3->shared->nrefs != 1)
        TEST_ERROR
    if(NULL == (f0 = H5F_open(filename[0], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR

    if(H5F_efc_close(f1, f3) < 0)
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 2)
        TEST_ERROR
    if(f2->shared->nrefs != 1)
        TEST_ERROR
    if(NULL == (f0 = H5F_open(filename[0], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_open(filename[3], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f3->shared->nrefs != 2)
        TEST_ERROR
    if(H5F_try_close(f3) < 0)
        FAIL_STACK_ERROR

    if(H5F_efc_close(f2, f1) < 0)
        FAIL_STACK_ERROR
    if(f2->shared->nrefs != 1)
        TEST_ERROR
    if(NULL == (f0 = H5F_open(filename[0], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_open(filename[1], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 3)
        TEST_ERROR
    if(H5F_try_close(f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_open(filename[3], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f3->shared->nrefs != 2)
        TEST_ERROR
    if(H5F_try_close(f3) < 0)
        FAIL_STACK_ERROR

    if(H5F_try_close(f2) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f0 = H5F_open(filename[0], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_open(filename[1], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_open(filename[2], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f2->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f2) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_open(filename[3], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f3->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f3) < 0)
        FAIL_STACK_ERROR


    /* Test 28: File held open by EFC client through non-parent file does not
     * interrupt cycle, but parent file does (no valid way around it), with
     * release */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp0 = H5F_efc_open(f1, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_open(filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f2, filename[1], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_efc_open(f1, filename[3],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR
    if(f1->shared->nrefs != 2)
        TEST_ERROR
    if(f2->shared->nrefs != 1)
        TEST_ERROR
    if(f3->shared->nrefs != 1)
        TEST_ERROR

    if(H5F_efc_release(f0->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR
    if(f1->shared->nrefs != 1)
        TEST_ERROR
    if(f2->shared->nrefs != 1)
        TEST_ERROR
    if(f3->shared->nrefs != 1)
        TEST_ERROR

    if(H5F_efc_release(f2->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR
    if(f1->shared->nrefs != 1)
        TEST_ERROR
    if(f2->shared->nrefs != 1)
        TEST_ERROR
    if(f3->shared->nrefs != 1)
        TEST_ERROR

    if(H5F_efc_close(f1, f3) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f2, f1) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_release(f2->shared->efc) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR
    if(f2->shared->nrefs != 1)
        TEST_ERROR
    if(NULL == (f1 = H5F_open(filename[1], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f3 = H5F_open(filename[3], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f3->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f3) < 0)
        FAIL_STACK_ERROR

    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(H5F_try_close(f2) < 0)
        FAIL_STACK_ERROR


    /* Test 29: File without EFC interrupts cycle */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(H5Pset_elink_file_cache_size(fapl_id, 0) < 0)
        TEST_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(H5Pset_elink_file_cache_size(fapl_id, 8) < 0)
        TEST_ERROR
    if(NULL == (ftmp0 = H5F_efc_open(f1, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR
    if(H5F_efc_close(f1, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR

    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f0 = H5F_open(filename[0], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_open(filename[1], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f1) < 0)
        FAIL_STACK_ERROR


    /* Test 30: File without EFC does not interrupt cycle */
    if(NULL == (f0 = H5F_open(filename[0],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_efc_open(f0, filename[1],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(NULL == (ftmp0 = H5F_efc_open(f1, filename[0], H5F_ACC_RDWR, fcpl_id,
            fapl_id, dxpl_id)))
        FAIL_STACK_ERROR
    if(H5F_efc_close(f1, ftmp0) < 0)
        FAIL_STACK_ERROR
    if(H5F_efc_close(f0, f1) < 0)
        FAIL_STACK_ERROR
    if(H5Pset_elink_file_cache_size(fapl_id, 0) < 0)
        TEST_ERROR
    if(NULL == (f2 = H5F_efc_open(f1, filename[2],
            H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(H5Pset_elink_file_cache_size(fapl_id, 8) < 0)
        TEST_ERROR
    if(H5F_efc_close(f1, f2) < 0)
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 2)
        TEST_ERROR

    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f0 = H5F_open(filename[0], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f0->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f0) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f1 = H5F_open(filename[1], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f1->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f1) < 0)
        FAIL_STACK_ERROR
    if(NULL == (f2 = H5F_open(filename[2], H5F_ACC_RDWR, fcpl_id, fapl_id,
            dxpl_id)))
        FAIL_STACK_ERROR
    if(f2->shared->nrefs != 1)
        TEST_ERROR
    if(H5F_try_close(f2) < 0)
        FAIL_STACK_ERROR


    PASSED();

    return 0;

error:
    return 1;
} /* test_graph_cycle */


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Test the external file cache code
 *
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 *
 * Programmer:  Neil Fortner
 *              December 16, 2010
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    unsigned nerrors = 0;        /* track errors */

    /* Test Setup */
    puts("Testing the external file cache");

    /* Create property lists */
    fcpl_id = H5Pcreate(H5P_FILE_CREATE);
    fapl_id = h5_fileaccess();
    dxpl_id = H5Pcreate(H5P_DATASET_XFER);

    /* Patch filenames */
    h5_fixname(FILENAME[0], fapl_id, filename[0], sizeof(filename[0]));
    h5_fixname(FILENAME[1], fapl_id, filename[1], sizeof(filename[1]));
    h5_fixname(FILENAME[2], fapl_id, filename[2], sizeof(filename[2]));
    h5_fixname(FILENAME[3], fapl_id, filename[3], sizeof(filename[3]));
    h5_fixname(FILENAME[4], fapl_id, filename[4], sizeof(filename[4]));
    h5_fixname(FILENAME[5], fapl_id, filename[5], sizeof(filename[5]));

    /* Test Functions */
    nerrors += test_single();
    nerrors += test_graph_nocycle();
    nerrors += test_graph_cycle();

    /* Close property lists */
    if(H5Pclose(dxpl_id) < 0)
        TEST_ERROR
    if(H5Pclose(fcpl_id) < 0)
        TEST_ERROR

    /* Verify symbol table messages are cached */
    nerrors += (h5_verify_cached_stabs(FILENAME, fapl_id) < 0 ? 1 : 0);

    if(nerrors)
        goto error;

    puts("All external file cache tests passed.");

    h5_cleanup(FILENAME, fapl_id);

    return 0;

error:
    puts("*** TESTS FAILED ***");

    H5E_BEGIN_TRY {
        H5Pclose(fapl_id);
    } H5E_END_TRY

    return 1;
} /* end main() */

