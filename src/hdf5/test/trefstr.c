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
   FILE
       refstr.c
   Test HDF reference counted string routines.

   REMARKS

   DESIGN

   BUGS/LIMITATIONS

   EXPORTED ROUTINES

   AUTHOR
       Quincey Koziol

   MODIFICATION HISTORY
       12/17/02 - Started coding
 */

#include "testhdf5.h"
#include "H5FLprivate.h"
#include "H5RSprivate.h"

/* Declare extern the PQ free list for the wrapped strings */
H5FL_BLK_EXTERN(str_buf);

/****************************************************************
**
**  test_refstr_init(): Test basic H5RS (ref-counted strings) code.
**      Initialize data for RS testing
**
****************************************************************/
static void
test_refstr_init(void)
{
} /* end test_refstr_init() */

/****************************************************************
**
**  test_refstr_create(): Test basic H5RS (ref-counted strings) code.
**      Tests creating and closing ref-counted strings.
**
****************************************************************/
static void
test_refstr_create(void)
{
    H5RS_str_t *rs;     /* Ref-counted string created */
    unsigned count;     /* Reference count on string */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Creating & Closing Ref-Counted Strings\n"));

    /* Try creating a ref-counted string */
    rs=H5RS_create("foo");
    CHECK(rs, NULL, "H5RS_create");

    /* Get the reference count on the string */
    count=H5RS_get_count(rs);
    VERIFY(count, 1, "H5RS_get_count");

    /* Try closing a real ref-counted string */
    ret=H5RS_decr(rs);
    CHECK(ret, FAIL, "H5RS_decr");

} /* end test_refstr_create() */

/****************************************************************
**
**  test_refstr_count(): Test basic H5RS (ref-counted strings) code.
**      Tests reference counting on ref-counted strings.
**
****************************************************************/
static void
test_refstr_count(void)
{
    H5RS_str_t *rs;     /* Ref-counted string created */
    unsigned count;     /* Reference count on string */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Incrementing & Decrementing Ref-Counted Strings\n"));

    /* Try creating a ref-counted string */
    rs=H5RS_create("foo");
    CHECK(rs, NULL, "H5RS_create");

    /* Get the reference count on the string */
    count=H5RS_get_count(rs);
    VERIFY(count, 1, "H5RS_get_count");

    /* Increment reference count */
    ret=H5RS_incr(rs);
    CHECK(ret, FAIL, "H5RS_incr");

    /* Get the reference count on the string */
    count=H5RS_get_count(rs);
    VERIFY(count, 2, "H5RS_get_count");

    /* Decrement reference count for string */
    ret=H5RS_decr(rs);
    CHECK(ret, FAIL, "H5RS_decr");

    /* Get the reference count on the string */
    count=H5RS_get_count(rs);
    VERIFY(count, 1, "H5RS_get_count");

    /* Decrement reference count for string */
    ret=H5RS_decr(rs);
    CHECK(ret, FAIL, "H5RS_decr");

} /* end test_refstr_count() */

/****************************************************************
**
**  test_refstr_dup(): Test basic H5RS (ref-counted strings) code.
**      Tests duplicating ref-counted strings.
**
****************************************************************/
static void
test_refstr_dup(void)
{
    H5RS_str_t *rs1;    /* Ref-counted string created */
    H5RS_str_t *rs2;    /* Ref-counted string created */
    unsigned count;     /* Reference count on string */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Duplicating Ref-Counted Strings\n"));

    /* Try creating a ref-counted string */
    rs1=H5RS_create("foo");
    CHECK(rs1, NULL, "H5RS_create");

    /* Get the reference count on the string */
    count=H5RS_get_count(rs1);
    VERIFY(count, 1, "H5RS_get_count");

    /* Duplicate r-string */
    rs2=H5RS_dup(rs1);
    CHECK(rs2, NULL, "H5RS_dup");

    /* Get the reference count on the strings */
    count=H5RS_get_count(rs1);
    VERIFY(count, 2, "H5RS_get_count");
    count=H5RS_get_count(rs2);
    VERIFY(count, 2, "H5RS_get_count");

    /* Decrement reference count for string */
    ret=H5RS_decr(rs2);
    CHECK(ret, FAIL, "H5RS_decr");

    /* Get the reference count on the string */
    count=H5RS_get_count(rs1);
    VERIFY(count, 1, "H5RS_get_count");

    /* Decrement reference count for string */
    ret=H5RS_decr(rs1);
    CHECK(ret, FAIL, "H5RS_decr");

} /* end test_refstr_dup() */

/****************************************************************
**
**  test_refstr_cmp(): Test basic H5RS (ref-counted strings) code.
**      Tests comparing ref-counted strings.
**
****************************************************************/
static void
test_refstr_cmp(void)
{
    H5RS_str_t *rs1;    /* Ref-counted string created */
    H5RS_str_t *rs2;    /* Ref-counted string created */
    int cmp;            /* Comparison value */
    ssize_t len;        /* Length of string */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Comparing Ref-Counted Strings\n"));

    /* Create first reference counted string */
    rs1=H5RS_create("foo");
    CHECK(rs1, NULL, "H5RS_create");

    /* Create second reference counted string */
    rs2=H5RS_create("foo2");
    CHECK(rs2, NULL, "H5RS_create");

    /* Compare the strings in various ways */
    cmp=H5RS_cmp(rs1,rs1);
    VERIFY(cmp, 0, "H5RS_cmp");
    cmp=H5RS_cmp(rs2,rs2);
    VERIFY(cmp, 0, "H5RS_cmp");
    cmp=H5RS_cmp(rs1,rs2);
    if(cmp>=0)
        TestErrPrintf("%d: string comparison incorrect!\n",__LINE__);

    /* Check the lengths of the strings also */
    len=H5RS_len(rs1);
    VERIFY(len, 3, "H5RS_len");
    len=H5RS_len(rs2);
    VERIFY(len, 4, "H5RS_len");

    /* Decrement reference count for strings */
    ret=H5RS_decr(rs2);
    CHECK(ret, FAIL, "H5RS_decr");
    ret=H5RS_decr(rs1);
    CHECK(ret, FAIL, "H5RS_decr");

} /* end test_refstr_cmp() */

/****************************************************************
**
**  test_refstr_wrap(): Test basic H5RS (ref-counted strings) code.
**      Tests wrapping ref-counted strings around existing strings.
**
****************************************************************/
static void
test_refstr_wrap(void)
{
    H5RS_str_t *rs;     /* Ref-counted string created */
    const char *s;      /* Pointer to raw string in ref-counted string */
    char buf[16];       /* Buffer to wrap */
    int cmp;            /* Comparison value */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Wrapping Ref-Counted Strings\n"));

    /* Initialize buffer */
    HDstrcpy(buf,"foo");

    /* Wrap ref-counted string around existing buffer */
    rs=H5RS_wrap(buf);
    CHECK(rs, NULL, "H5RS_wrap");

    /* Get pointer to raw string in ref-counted string */
    s=H5RS_get_str(rs);
    CHECK(s, NULL, "H5RS_get_str");
    VERIFY(s, buf, "wrapping");
    cmp=HDstrcmp(s,buf);
    VERIFY(cmp, 0, "HDstrcmp");

    /* Increment reference count (should duplicate string) */
    ret=H5RS_incr(rs);
    CHECK(ret, FAIL, "H5RS_incr");

    /* Change the buffer initially wrapped */
    buf[0]='F';

    /* Get pointer to raw string in ref-counted string */
    s=H5RS_get_str(rs);
    CHECK(s, NULL, "H5RS_get_str");
    CHECK(s, buf, "wrapping");
    cmp=HDstrcmp(s,buf);
    if(cmp<=0)
        TestErrPrintf("%d: string comparison incorrect!\n",__LINE__);

    /* Decrement reference count for string */
    ret=H5RS_decr(rs);
    CHECK(ret, FAIL, "H5RS_decr");
    ret=H5RS_decr(rs);
    CHECK(ret, FAIL, "H5RS_decr");

} /* end test_refstr_wrap() */

/****************************************************************
**
**  test_refstr_own(): Test basic H5RS (ref-counted strings) code.
**      Tests transferring ownership of dynamically allocated strings
**      to ref-counted strings.
**
****************************************************************/
static void
test_refstr_own(void)
{
    H5RS_str_t *rs;     /* Ref-counted string created */
    char *s;            /* Pointer to string to transfer */
    const char *t;      /* Temporary pointers to string */
    int cmp;            /* Comparison value */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Transferring Ref-Counted Strings\n"));

    /* Initialize buffer */
    s = (char *)H5FL_BLK_MALLOC(str_buf,HDstrlen("foo") + 1);
    CHECK(s, NULL, "H5FL_BLK_MALLOC");    
    HDstrcpy(s, "foo");

    /* Transfer ownership of dynamically allocated string to ref-counted string */
    rs=H5RS_own(s);
    CHECK(rs, NULL, "H5RS_own");

    /* Get pointer to raw string in ref-counted string */
    t=H5RS_get_str(rs);
    CHECK(t, NULL, "H5RS_get_str");
    VERIFY(t, s, "transferring");
    cmp=HDstrcmp(s,t);
    VERIFY(cmp, 0, "HDstrcmp");

    /* Increment reference count (should NOT duplicate string) */
    ret=H5RS_incr(rs);
    CHECK(ret, FAIL, "H5RS_incr");

    /* Change the buffer initially wrapped */
    *s='F';

    /* Get pointer to raw string in ref-counted string */
    t=H5RS_get_str(rs);
    CHECK(t, NULL, "H5RS_get_str");
    VERIFY(t, s, "transferring");
    cmp=HDstrcmp(t,s);
    VERIFY(cmp, 0, "HDstrcmp");

    /* Decrement reference count for string */
    ret=H5RS_decr(rs);
    CHECK(ret, FAIL, "H5RS_decr");
    ret=H5RS_decr(rs);
    CHECK(ret, FAIL, "H5RS_decr");

} /* end test_refstr_own() */

/****************************************************************
**
**  test_refstr_finalize(): Test basic H5RS (ref-counted strings) code.
**      Wrap up data for ref-counted string testing
**
****************************************************************/
static void
test_refstr_finalize(void)
{
} /* end test_refstr_finalize() */

/****************************************************************
**
**  test_refstr(): Main H5RS testing routine.
**
****************************************************************/
void
test_refstr(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing Reference Counted Strings\n"));

    /* Initialize ref-counted strings testing data */
    test_refstr_init();

    /* Actual ref-counted strings tests */
    test_refstr_create();       /* Test ref-counted string creation */
    test_refstr_count();        /* Test ref-counted string counting */
    test_refstr_dup();          /* Test ref-counted string duplication */
    test_refstr_cmp();          /* Test ref-counted string comparison */
    test_refstr_wrap();         /* Test ref-counted string wrapping */
    test_refstr_own();          /* Test ref-counted string ownership transfer */

    /* Finalize ref-counted strings testing data */
    test_refstr_finalize();
}   /* end test_refstr() */

