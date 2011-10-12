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
   Test HDF Heap routines.

   REMARKS

   DESIGN

   BUGS/LIMITATIONS

   EXPORTED ROUTINES

   AUTHOR
       Quincey Koziol

   MODIFICATION HISTORY
       2/18/03 - Started coding
 */

#include <time.h>
#include <stdlib.h>

#include "testhdf5.h"
#include "H5HPprivate.h"

/* The number of elements in testing arrays */
#define NUM_ELEMS       1000

/* Objects for testing in heaps */
typedef struct test_obj {
    H5HP_info_t heap_info;      /* Information required for heap.  _MUST_ be first */
    int val;                    /* Actual information for object */
} test_obj;

/* Array of random element values */
static test_obj rand_num[NUM_ELEMS];

/* Array of random elements values, sorted in increasing order */
static test_obj inc_sort_num[NUM_ELEMS];

/* Array of random elements values, sorted in decreasing order */
static test_obj dec_sort_num[NUM_ELEMS];

static int tst_dec_sort(const void *_i1, const void *_i2)
{
    const test_obj *i1=(const test_obj *)_i1;
    const test_obj *i2=(const test_obj *)_i2;

    if(i1->val<i2->val)
        return(1);
    else if(i1->val>i2->val)
        return(-1);
    return(0);
}

static int tst_inc_sort(const void *_i1, const void *_i2)
{
    const test_obj *i1=(const test_obj *)_i1;
    const test_obj *i2=(const test_obj *)_i2;

    if(i1->val<i2->val)
        return(-1);
    else if(i1->val>i2->val)
        return(1);
    return(0);
}

/****************************************************************
**
**  test_heap_init(): Test H5HP (heap) code.
**      Initialize data for Heap testing
**
****************************************************************/
static void
test_heap_init(void)
{
    time_t curr_time;   /* Current time, for seeding random number generator */
    size_t u;           /* Local index variables */

    /* Create randomized set of numbers */
    curr_time=time(NULL);
    HDsrandom((unsigned)curr_time);
    for(u=0; u<NUM_ELEMS; u++)
        /* Generate random numbers from -1000 to 1000 */
        rand_num[u].val=(int)(HDrandom()%2001)-1001;

    /* Sort random numbers into increasing order */
    HDmemcpy(inc_sort_num,rand_num,sizeof(test_obj)*NUM_ELEMS);
    HDqsort(inc_sort_num, (size_t)NUM_ELEMS, sizeof(test_obj), tst_inc_sort);

    /* Sort random numbers into decreasing order */
    HDmemcpy(dec_sort_num,rand_num,sizeof(test_obj)*NUM_ELEMS);
    HDqsort(dec_sort_num, (size_t)NUM_ELEMS, sizeof(test_obj), tst_dec_sort);
} /* end test_tst_init() */

/****************************************************************
**
**  test_heap_create(): Test basic H5HP (heap) code.
**      Tests creating and closing heaps.
**
****************************************************************/
static void
test_heap_create(void)
{
    H5HP_t *heap;       /* Heap created */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(6, ("Testing Creating & Closing Heaps\n"));

    /* Try creating a maximum Heap */
    heap=H5HP_create(H5HP_MAX_HEAP);
    CHECK(heap, NULL, "H5HP_create");

    /* Try closing the heap */
    ret=H5HP_close(heap);
    CHECK(ret, FAIL, "H5HP_close");

    /* Try creating a minimum Heap */
    heap=H5HP_create(H5HP_MIN_HEAP);
    CHECK(heap, NULL, "H5HP_create");

    /* Try closing the heap */
    ret=H5HP_close(heap);
    CHECK(ret, FAIL, "H5HP_close");

} /* end test_heap_create() */

/****************************************************************
**
**  test_heap_insert_min(): Test H5HP (heap) code.
**      Tests basic inserting objects into minimum heaps.
**
****************************************************************/
static void
test_heap_insert_min(void)
{
    H5HP_t *heap;       /* Heap created */
    ssize_t num;        /* Number of elements in heap */
    int val;            /* Value of object on heap */
    test_obj obj1, obj2, obj3;  /* Test objects to insert */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Inserting Into Minimum Heaps\n"));

    /* Create a Heap */
    heap=H5HP_create(H5HP_MIN_HEAP);
    CHECK(heap, NULL, "H5HP_create");

    /* Check that the heap has no elements */
    num=H5HP_count(heap);
    VERIFY(num, 0, "H5HP_count");

    /* Insert an object into the heap */
    obj1.val=100;
    ret=H5HP_insert(heap,10,&obj1);
    CHECK(ret, FAIL, "H5HP_insert");

    /* Check that the heap has one element */
    num=H5HP_count(heap);
    VERIFY(num, 1, "H5HP_count");

    /* Check the minimum value on the heap */
    ret=H5HP_top(heap, &val);
    CHECK(ret, FAIL, "H5HP_top");
    VERIFY(val, 10, "H5HP_top");

    /* Insert another object into the heap, with value less than top element */
    obj2.val=50;
    ret=H5HP_insert(heap,5,&obj2);
    CHECK(ret, FAIL, "H5HP_insert");

    /* Check that the heap has two elements */
    num=H5HP_count(heap);
    VERIFY(num, 2, "H5HP_count");

    /* Check the minimum value on the heap */
    ret=H5HP_top(heap, &val);
    CHECK(ret, FAIL, "H5HP_top");
    VERIFY(val, 5, "H5HP_top");

    /* Insert third object into the heap, with value greater than top element */
    obj3.val=200;
    ret=H5HP_insert(heap,20,&obj3);
    CHECK(ret, FAIL, "H5HP_insert");

    /* Check that the heap has three elements */
    num=H5HP_count(heap);
    VERIFY(num, 3, "H5HP_count");

    /* Check the minimum value on the heap */
    ret=H5HP_top(heap, &val);
    CHECK(ret, FAIL, "H5HP_top");
    VERIFY(val, 5, "H5HP_top");

    /* Close the heap */
    ret=H5HP_close(heap);
    CHECK(ret, FAIL, "H5HP_close");

} /* end test_heap_insert_min() */

/****************************************************************
**
**  test_heap_insert(): Test H5HP (heap) code.
**      Tests basic inserting objects into maximum heaps.
**
****************************************************************/
static void
test_heap_insert_max(void)
{
    H5HP_t *heap;       /* Heap created */
    ssize_t num;        /* Number of elements in heap */
    int val;            /* Value of object on heap */
    test_obj obj1, obj2, obj3;  /* Test objects to insert */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Inserting Into Maximum Heaps\n"));

    /* Create a Heap */
    heap=H5HP_create(H5HP_MAX_HEAP);
    CHECK(heap, NULL, "H5HP_create");

    /* Check that the heap has no elements */
    num=H5HP_count(heap);
    VERIFY(num, 0, "H5HP_count");

    /* Insert an object into the heap */
    obj1.val=100;
    ret=H5HP_insert(heap,10,&obj1);
    CHECK(ret, FAIL, "H5HP_insert");

    /* Check that the heap has one element */
    num=H5HP_count(heap);
    VERIFY(num, 1, "H5HP_count");

    /* Check the maximum value on the heap */
    ret=H5HP_top(heap, &val);
    CHECK(ret, FAIL, "H5HP_top");
    VERIFY(val, 10, "H5HP_top");

    /* Insert another object into the heap, with value less than top element */
    obj2.val=50;
    ret=H5HP_insert(heap,5,&obj2);
    CHECK(ret, FAIL, "H5HP_insert");

    /* Check that the heap has two elements */
    num=H5HP_count(heap);
    VERIFY(num, 2, "H5HP_count");

    /* Check the maximum value on the heap */
    ret=H5HP_top(heap, &val);
    CHECK(ret, FAIL, "H5HP_top");
    VERIFY(val, 10, "H5HP_top");

    /* Insert third object into the heap, with value greater than top element */
    obj3.val=200;
    ret=H5HP_insert(heap,20,&obj3);
    CHECK(ret, FAIL, "H5HP_insert");

    /* Check that the heap has three elements */
    num=H5HP_count(heap);
    VERIFY(num, 3, "H5HP_count");

    /* Check the maximum value on the heap */
    ret=H5HP_top(heap, &val);
    CHECK(ret, FAIL, "H5HP_top");
    VERIFY(val, 20, "H5HP_top");

    /* Close the heap */
    ret=H5HP_close(heap);
    CHECK(ret, FAIL, "H5HP_close");

} /* end test_heap_insert_max() */

/****************************************************************
**
**  test_heap_insert(): Test H5HP (heap) code.
**      Tests basic inserting objects into heaps.
**
****************************************************************/
static void
test_heap_insert(void)
{
    /* Output message about test being performed */
    MESSAGE(6, ("Testing Inserting Into Heaps\n"));

    /* Test insertions into minimum & maximum heaps */
    test_heap_insert_max();
    test_heap_insert_min();
} /* end test_heap_insert() */

/****************************************************************
**
**  test_heap_insert_many_core (): Tests H5HP (heap) code.
**      "Core" routine called by test_heap_insert_many() routine.
**
****************************************************************/
static void
test_heap_insert_many_core(H5HP_type_t heap_type, test_obj *arr, size_t nelem, int top_val)
{
    H5HP_t *heap;       /* Heap created */
    ssize_t num;        /* Number of elements in heap */
    int val;            /* Value of object on heap */
    size_t u;           /* Local index variable */
    herr_t ret;         /* Generic return value */

    /* Create a Heap */
    heap=H5HP_create(heap_type);
    CHECK(heap, NULL, "H5HP_create");

    /* Check that the heap has no elements */
    num=H5HP_count(heap);
    VERIFY(num, 0, "H5HP_count");

    /* Insert the array elements into the heap */
    for(u=0; u<nelem; u++) {
        ret=H5HP_insert(heap,arr[u].val,&arr[u]);
        CHECK(ret, FAIL, "H5HP_insert");
    } /* end for */

    /* Check that the heap has correct number of elements */
    num=H5HP_count(heap);
    CHECK(num, FAIL, "H5HP_count");
    VERIFY((size_t)num, nelem, "H5HP_count");

    /* Check the maximum value on the heap */
    ret=H5HP_top(heap, &val);
    CHECK(ret, FAIL, "H5HP_top");
    VERIFY(val, top_val, "H5HP_top");

    /* Close the heap */
    ret=H5HP_close(heap);
    CHECK(ret, FAIL, "H5HP_close");
} /* end test_heap_insert_many_core() */

/****************************************************************
**
**  test_heap_insert_many (): Test H5HP (heap) code.
**      Tests inserting many objects into heaps.
**
****************************************************************/
static void
test_heap_insert_many(void)
{
    /* Output message about test being performed */
    MESSAGE(6, ("Testing Inserting Many Objects Into Heaps\n"));

    /* Test creating a heap from random elements */
    test_heap_insert_many_core(H5HP_MAX_HEAP, rand_num, (size_t)NUM_ELEMS, dec_sort_num[0].val);

    /* Test creating a heap from elements in increasing order */
    test_heap_insert_many_core(H5HP_MAX_HEAP, inc_sort_num, (size_t)NUM_ELEMS, dec_sort_num[0].val);

    /* Test creating a heap from elements in decreasing order */
    test_heap_insert_many_core(H5HP_MAX_HEAP, dec_sort_num, (size_t)NUM_ELEMS, dec_sort_num[0].val);

    /* Test creating a heap from random elements */
    test_heap_insert_many_core(H5HP_MIN_HEAP, rand_num, (size_t)NUM_ELEMS, inc_sort_num[0].val);

    /* Test creating a heap from elements in increasing order */
    test_heap_insert_many_core(H5HP_MIN_HEAP, inc_sort_num, (size_t)NUM_ELEMS, inc_sort_num[0].val);

    /* Test creating a heap from elements in decreasing order */
    test_heap_insert_many_core(H5HP_MIN_HEAP, dec_sort_num, (size_t)NUM_ELEMS, inc_sort_num[0].val);

} /* end test_heap_insert_many() */

/****************************************************************
**
**  test_heap_remove_min(): Test H5HP (heap) code.
**      Tests basic removal of objects from minimum heaps.
**
****************************************************************/
static void
test_heap_remove_min(void)
{
    H5HP_t *heap;       /* Heap created */
    ssize_t num;        /* Number of elements in heap */
    int val;            /* Value of object on heap */
    void *ptr;          /* Pointer for object on heap */
    test_obj obj1, obj2, obj3;  /* Test objects to insert */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Removing From Minimum Heaps\n"));

    /* Create a Heap */
    heap=H5HP_create(H5HP_MIN_HEAP);
    CHECK(heap, NULL, "H5HP_create");

    /* Check that the heap has no elements */
    num=H5HP_count(heap);
    VERIFY(num, 0, "H5HP_count");

    /* Try removing an object from an empty heap */
    ret=H5HP_remove(heap,&val,&ptr);
    VERIFY(ret, FAIL, "H5HP_remove");

    /* Insert an object into the heap */
    obj1.val=100;
    ret=H5HP_insert(heap,10,&obj1);
    CHECK(ret, FAIL, "H5HP_insert");

    /* Insert another object into the heap, with value less than top element */
    obj2.val=50;
    ret=H5HP_insert(heap,5,&obj2);
    CHECK(ret, FAIL, "H5HP_insert");

    /* Insert third object into the heap, with value greater than top element */
    obj3.val=200;
    ret=H5HP_insert(heap,20,&obj3);
    CHECK(ret, FAIL, "H5HP_insert");

    /* Remove first maximum value from heap */
    ret=H5HP_remove(heap,&val,&ptr);
    CHECK(ret, FAIL, "H5HP_remove");
    VERIFY(val, 5, "H5HP_remove");
    VERIFY(ptr, &obj2, "H5HP_remove");

    /* Remove second maximum value from heap */
    ret=H5HP_remove(heap,&val,&ptr);
    CHECK(ret, FAIL, "H5HP_remove");
    VERIFY(val, 10, "H5HP_remove");
    VERIFY(ptr, &obj1, "H5HP_remove");

    /* Remove third maximum value from heap */
    ret=H5HP_remove(heap,&val,&ptr);
    CHECK(ret, FAIL, "H5HP_remove");
    VERIFY(val, 20, "H5HP_remove");
    VERIFY(ptr, &obj3, "H5HP_remove");

    /* Try removing an object from an empty heap */
    ret=H5HP_remove(heap,&val,&ptr);
    VERIFY(ret, FAIL, "H5HP_remove");

    /* Close the heap */
    ret=H5HP_close(heap);
    CHECK(ret, FAIL, "H5HP_close");

} /* end test_heap_remove_min() */

/****************************************************************
**
**  test_heap_remove_max(): Test H5HP (heap) code.
**      Tests basic removal of objects from maximum heaps.
**
****************************************************************/
static void
test_heap_remove_max(void)
{
    H5HP_t *heap;       /* Heap created */
    ssize_t num;        /* Number of elements in heap */
    int val;            /* Value of object on heap */
    void *ptr;          /* Pointer for object on heap */
    test_obj obj1, obj2, obj3;  /* Test objects to insert */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Removing From Maximum Heaps\n"));

    /* Create a Heap */
    heap=H5HP_create(H5HP_MAX_HEAP);
    CHECK(heap, NULL, "H5HP_create");

    /* Check that the heap has no elements */
    num=H5HP_count(heap);
    VERIFY(num, 0, "H5HP_count");

    /* Try removing an object from an empty heap */
    ret=H5HP_remove(heap,&val,&ptr);
    VERIFY(ret, FAIL, "H5HP_remove");

    /* Insert an object into the heap */
    obj1.val=100;
    ret=H5HP_insert(heap,10,&obj1);
    CHECK(ret, FAIL, "H5HP_insert");

    /* Insert another object into the heap, with value less than top element */
    obj2.val=50;
    ret=H5HP_insert(heap,5,&obj2);
    CHECK(ret, FAIL, "H5HP_insert");

    /* Insert third object into the heap, with value greater than top element */
    obj3.val=200;
    ret=H5HP_insert(heap,20,&obj3);
    CHECK(ret, FAIL, "H5HP_insert");

    /* Remove first maximum value from heap */
    ret=H5HP_remove(heap,&val,&ptr);
    CHECK(ret, FAIL, "H5HP_remove");
    VERIFY(val, 20, "H5HP_remove");
    VERIFY(ptr, &obj3, "H5HP_remove");

    /* Remove second maximum value from heap */
    ret=H5HP_remove(heap,&val,&ptr);
    CHECK(ret, FAIL, "H5HP_remove");
    VERIFY(val, 10, "H5HP_remove");
    VERIFY(ptr, &obj1, "H5HP_remove");

    /* Remove third maximum value from heap */
    ret=H5HP_remove(heap,&val,&ptr);
    CHECK(ret, FAIL, "H5HP_remove");
    VERIFY(val, 5, "H5HP_remove");
    VERIFY(ptr, &obj2, "H5HP_remove");

    /* Try removing an object from an empty heap */
    ret=H5HP_remove(heap,&val,&ptr);
    VERIFY(ret, FAIL, "H5HP_remove");

    /* Close the heap */
    ret=H5HP_close(heap);
    CHECK(ret, FAIL, "H5HP_close");

} /* end test_heap_remove_max() */

/****************************************************************
**
**  test_heap_remove(): Test H5HP (heap) code.
**      Tests basic removal of objects from minimum & maximum heaps.
**
****************************************************************/
static void
test_heap_remove(void)
{
    /* Output message about test being performed */
    MESSAGE(6, ("Testing Removing From Heaps\n"));

    /* Test removals from minimum & maximum heaps */
    test_heap_remove_max();
    test_heap_remove_min();
} /* end test_heap_remove() */

/****************************************************************
**
**  test_heap_remove_many_core (): Tests H5HP (heap) code.
**      "Core" routine called by test_heap_remove_many() routine.
**
****************************************************************/
static void test_heap_remove_many_core(H5HP_type_t heap_type, test_obj *arr, size_t nelem)
{
    H5HP_t *heap;       /* Heap created */
    ssize_t num;        /* Number of elements in heap */
    int last_val;       /* Last value from the heap */
    int val;            /* Value of object on heap */
    test_obj *ptr;      /* Pointer for object on heap */
    size_t u;           /* Local index variable */
    herr_t ret;         /* Generic return value */

    /* Create a Heap */
    heap=H5HP_create(heap_type);
    CHECK(heap, NULL, "H5HP_create");

    /* Check that the heap has no elements */
    num=H5HP_count(heap);
    VERIFY(num, 0, "H5HP_count");

    /* Insert the array elements into the heap */
    for(u=0; u<nelem; u++) {
        ret=H5HP_insert(heap,arr[u].val,&arr[u]);
        CHECK(ret, FAIL, "H5HP_insert");
    } /* end for */

    /* Check that the heap has correct number of elements */
    num=H5HP_count(heap);
    CHECK(num, FAIL, "H5HP_count");
    VERIFY((size_t)num, nelem, "H5HP_count");

    /* Set an appropriate starting value for the "last" value from heap */
    if(heap_type==H5HP_MAX_HEAP)
        last_val=INT_MAX;
    else
        last_val=INT_MIN;

    /* Remove the objects from the heap */
    for(u=0; u<nelem; u++) {
        ret=H5HP_remove(heap,&val,(void **)&ptr);
        CHECK(ret, FAIL, "H5HP_remove");
        VERIFY(val, ptr->val, "H5HP_remove");

        /* Check that the value is correct, based on the heap type */
        if(heap_type==H5HP_MAX_HEAP) {
            if(val>last_val)
                TestErrPrintf("Error on line %d: incorrect value from heap=%d, last_val=%d\n",__LINE__,val,last_val);
        } /* end if */
        else {
            if(val<last_val)
                TestErrPrintf("Error on line %d: incorrect value from heap=%d, last_val=%d\n",__LINE__,val,last_val);
        } /* end else */

        /* Update last value */
        last_val=val;
    } /* end for */

    /* Check that the heap has no elements */
    num=H5HP_count(heap);
    VERIFY(num, 0, "H5HP_count");

/* Insert & remove again, to check that completely empty heaps can be added again */

    /* Set an appropriate starting value for the "last" value from heap */
    if(heap_type==H5HP_MAX_HEAP)
        last_val=INT_MAX;
    else
        last_val=INT_MIN;

    /* Insert the array elements into the heap */
    for(u=0; u<nelem; u++) {
        ret=H5HP_insert(heap,arr[u].val,&arr[u]);
        CHECK(ret, FAIL, "H5HP_insert");
    } /* end for */

    /* Check that the heap has correct number of elements */
    num=H5HP_count(heap);
    CHECK(num, FAIL, "H5HP_count");
    VERIFY((size_t)num, nelem, "H5HP_count");

    /* Remove the objects from the heap */
    for(u=0; u<nelem; u++) {
        ret=H5HP_remove(heap,&val,(void **)&ptr);
        CHECK(ret, FAIL, "H5HP_remove");
        VERIFY(val, ptr->val, "H5HP_remove");

        /* Check that the value is correct, based on the heap type */
        if(heap_type==H5HP_MAX_HEAP) {
            if(val>last_val)
                TestErrPrintf("Error on line %d: incorrect value from heap=%d, last_val=%d\n",__LINE__,val,last_val);
        } /* end if */
        else {
            if(val<last_val)
                TestErrPrintf("Error on line %d: incorrect value from heap=%d, last_val=%d\n",__LINE__,val,last_val);
        } /* end else */

        /* Update last value */
        last_val=val;
    } /* end for */

    /* Check that the heap has no elements */
    num=H5HP_count(heap);
    VERIFY(num, 0, "H5HP_count");

    /* Close the heap */
    ret=H5HP_close(heap);
    CHECK(ret, FAIL, "H5HP_close");
} /* end test_heap_remove_many_core() */

/****************************************************************
**
**  test_heap_remove_many (): Test H5HP (heap) code.
**      Tests removing many objects into heaps.
**
****************************************************************/
static void
test_heap_remove_many(void)
{
    /* Output message about test being performed */
    MESSAGE(6, ("Testing Removing Many Objects From Heaps\n"));

    /* Test removing objects from maximum heap with random elements */
    test_heap_remove_many_core(H5HP_MAX_HEAP, rand_num, (size_t)NUM_ELEMS);

    /* Test removing objects from maximum heap with elements already sorted in increasing order */
    test_heap_remove_many_core(H5HP_MAX_HEAP, inc_sort_num, (size_t)NUM_ELEMS);

    /* Test removing objects from maximum heap with elements already sorted in decreasing order */
    test_heap_remove_many_core(H5HP_MAX_HEAP, dec_sort_num, (size_t)NUM_ELEMS);

    /* Test removing objects from minimum heap with random elements */
    test_heap_remove_many_core(H5HP_MIN_HEAP, rand_num, (size_t)NUM_ELEMS);

    /* Test removing objects from minimum heap with elements already sorted in increasing order */
    test_heap_remove_many_core(H5HP_MIN_HEAP, inc_sort_num, (size_t)NUM_ELEMS);

    /* Test removing objects from minimum heap with elements already sorted in decreasing order */
    test_heap_remove_many_core(H5HP_MIN_HEAP, dec_sort_num, (size_t)NUM_ELEMS);

} /* end test_heap_remove_many() */

/****************************************************************
**
**  test_heap_change_min (): Test H5HP (heap) code.
**      Tests changing the priority of an object in a minimum heap
**
****************************************************************/
static void
test_heap_change_min(void)
{
    H5HP_t *heap;       /* Heap created */
    ssize_t num;        /* Number of elements in heap */
    int val;            /* Value of object on heap */
    test_obj obj1, obj2, obj3;  /* Test objects to insert */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Changing Priority of Objects in Minimum Heaps\n"));

    /* Create a Heap */
    heap=H5HP_create(H5HP_MIN_HEAP);
    CHECK(heap, NULL, "H5HP_create");

    /* Check that the heap has no elements */
    num=H5HP_count(heap);
    VERIFY(num, 0, "H5HP_count");

    /* Insert an object into the heap */
    obj1.val=100;
    ret=H5HP_insert(heap,10,&obj1);
    CHECK(ret, FAIL, "H5HP_insert");

    /* Insert another object into the heap, with value less than top element */
    obj2.val=50;
    ret=H5HP_insert(heap,5,&obj2);
    CHECK(ret, FAIL, "H5HP_insert");

    /* Insert third object into the heap, with value greater than top element */
    obj3.val=200;
    ret=H5HP_insert(heap,20,&obj3);
    CHECK(ret, FAIL, "H5HP_insert");

    /* Change priority of first object on heap in way which shouldn't affect heap order */
    ret=H5HP_change(heap,11,&obj1);
    CHECK(ret, FAIL, "H5HP_change");

    /* Check the minimum value on the heap */
    ret=H5HP_top(heap, &val);
    CHECK(ret, FAIL, "H5HP_top");
    VERIFY(val, 5, "H5HP_top");

    /* Change priority of first object on heap to be the top object on the heap */
    ret=H5HP_change(heap,3,&obj1);
    CHECK(ret, FAIL, "H5HP_change");

    /* Check the maximum value on the heap */
    ret=H5HP_top(heap, &val);
    CHECK(ret, FAIL, "H5HP_top");
    VERIFY(val, 3, "H5HP_top");

    /* Change priority of first object on heap to not be the top object on the heap */
    ret=H5HP_change(heap,10,&obj1);
    CHECK(ret, FAIL, "H5HP_change");

    /* Check the maximum value on the heap */
    ret=H5HP_top(heap, &val);
    CHECK(ret, FAIL, "H5HP_top");
    VERIFY(val, 5, "H5HP_top");

    /* Close the heap */
    ret=H5HP_close(heap);
    CHECK(ret, FAIL, "H5HP_close");

} /* end test_heap_change_min() */

/****************************************************************
**
**  test_heap_change_max (): Test H5HP (heap) code.
**      Tests changing the priority of an object in a maximumheap
**
****************************************************************/
static void
test_heap_change_max(void)
{
    H5HP_t *heap;       /* Heap created */
    ssize_t num;        /* Number of elements in heap */
    int val;            /* Value of object on heap */
    test_obj obj1, obj2, obj3;  /* Test objects to insert */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Changing Priority of Objects in Maximum Heaps\n"));

    /* Create a Heap */
    heap=H5HP_create(H5HP_MAX_HEAP);
    CHECK(heap, NULL, "H5HP_create");

    /* Check that the heap has no elements */
    num=H5HP_count(heap);
    VERIFY(num, 0, "H5HP_count");

    /* Insert an object into the heap */
    obj1.val=100;
    ret=H5HP_insert(heap,10,&obj1);
    CHECK(ret, FAIL, "H5HP_insert");

    /* Insert another object into the heap, with value less than top element */
    obj2.val=50;
    ret=H5HP_insert(heap,5,&obj2);
    CHECK(ret, FAIL, "H5HP_insert");

    /* Insert third object into the heap, with value greater than top element */
    obj3.val=200;
    ret=H5HP_insert(heap,20,&obj3);
    CHECK(ret, FAIL, "H5HP_insert");

    /* Change priority of first object on heap in way which shouldn't affect heap order */
    ret=H5HP_change(heap,11,&obj1);
    CHECK(ret, FAIL, "H5HP_change");

    /* Check the maximum value on the heap */
    ret=H5HP_top(heap, &val);
    CHECK(ret, FAIL, "H5HP_top");
    VERIFY(val, 20, "H5HP_top");

    /* Change priority of first object on heap to be the top object on the heap */
    ret=H5HP_change(heap,21,&obj1);
    CHECK(ret, FAIL, "H5HP_change");

    /* Check the maximum value on the heap */
    ret=H5HP_top(heap, &val);
    CHECK(ret, FAIL, "H5HP_top");
    VERIFY(val, 21, "H5HP_top");

    /* Change priority of first object on heap to not be the top object on the heap */
    ret=H5HP_change(heap,10,&obj1);
    CHECK(ret, FAIL, "H5HP_change");

    /* Check the maximum value on the heap */
    ret=H5HP_top(heap, &val);
    CHECK(ret, FAIL, "H5HP_top");
    VERIFY(val, 20, "H5HP_top");

    /* Close the heap */
    ret=H5HP_close(heap);
    CHECK(ret, FAIL, "H5HP_close");

} /* end test_heap_change() */

/****************************************************************
**
**  test_heap_change (): Test H5HP (heap) code.
**      Tests changing the priority of an object in maximum & minimum heaps
**
****************************************************************/
static void
test_heap_change(void)
{
    /* Output message about test being performed */
    MESSAGE(6, ("Testing Changing Priority of Objects in Heaps\n"));

    /* Test removals from minimum & maximum heaps */
    test_heap_change_max();
    test_heap_change_min();
} /* end test_heap_change() */

/****************************************************************
**
**  test_heap_incdec_min (): Test H5HP (heap) code.
**      Tests incrementing & decrementing priority of objects on
**      a minimum heap.
**
****************************************************************/
static void
test_heap_incdec_min(void)
{
    H5HP_t *heap;       /* Heap created */
    ssize_t num;        /* Number of elements in heap */
    int val;            /* Value of object on heap */
    test_obj obj1, obj2, obj3;  /* Test objects to insert */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Incrementing & Decrementing Priority of Objects in Minimum Heaps\n"));

    /* Create a Heap */
    heap=H5HP_create(H5HP_MIN_HEAP);
    CHECK(heap, NULL, "H5HP_create");

    /* Check that the heap has no elements */
    num=H5HP_count(heap);
    VERIFY(num, 0, "H5HP_count");

    /* Insert an object into the heap */
    obj1.val=100;
    ret=H5HP_insert(heap,6,&obj1);
    CHECK(ret, FAIL, "H5HP_insert");

    /* Insert another object into the heap, with value less than top element */
    obj2.val=50;
    ret=H5HP_insert(heap,5,&obj2);
    CHECK(ret, FAIL, "H5HP_insert");

    /* Insert third object into the heap, with value greater than top element */
    obj3.val=200;
    ret=H5HP_insert(heap,20,&obj3);
    CHECK(ret, FAIL, "H5HP_insert");

    /* Decrement object one's priority by two to put it on top of the heap */
    ret=H5HP_decr(heap, 2, &obj1);
    CHECK(ret, FAIL, "H5HP_change");

    /* Check the minimum value on the heap */
    ret=H5HP_top(heap, &val);
    CHECK(ret, FAIL, "H5HP_top");
    VERIFY(val, 4, "H5HP_top");

    /* Decrement object two's priority by two to put it back on top of the heap */
    ret=H5HP_decr(heap, 2, &obj2);
    CHECK(ret, FAIL, "H5HP_change");

    /* Check the minimum value on the heap */
    ret=H5HP_top(heap, &val);
    CHECK(ret, FAIL, "H5HP_top");
    VERIFY(val, 3, "H5HP_top");

    /* Increment object two's priority by two to return object one to the top */
    ret=H5HP_incr(heap,2,&obj2);
    CHECK(ret, FAIL, "H5HP_change");

    /* Check the minimum value on the heap */
    ret=H5HP_top(heap, &val);
    CHECK(ret, FAIL, "H5HP_top");
    VERIFY(val, 4, "H5HP_top");

    /* Close the heap */
    ret=H5HP_close(heap);
    CHECK(ret, FAIL, "H5HP_close");

} /* end test_heap_incdec_min() */

/****************************************************************
**
**  test_heap_incdec_max (): Test H5HP (heap) code.
**      Tests incrementing & decrementing priority of objects on
**      a maximum heap.
**
****************************************************************/
static void
test_heap_incdec_max(void)
{
    H5HP_t *heap;       /* Heap created */
    ssize_t num;        /* Number of elements in heap */
    int val;            /* Value of object on heap */
    test_obj obj1, obj2, obj3;  /* Test objects to insert */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Incrementing & Decrementing Priority of Objects in Maximum Heaps\n"));

    /* Create a Heap */
    heap=H5HP_create(H5HP_MAX_HEAP);
    CHECK(heap, NULL, "H5HP_create");

    /* Check that the heap has no elements */
    num=H5HP_count(heap);
    VERIFY(num, 0, "H5HP_count");

    /* Insert an object into the heap */
    obj1.val=100;
    ret=H5HP_insert(heap,19,&obj1);
    CHECK(ret, FAIL, "H5HP_insert");

    /* Insert another object into the heap, with value less than top element */
    obj2.val=50;
    ret=H5HP_insert(heap,5,&obj2);
    CHECK(ret, FAIL, "H5HP_insert");

    /* Insert third object into the heap, with value greater than top element */
    obj3.val=200;
    ret=H5HP_insert(heap,20,&obj3);
    CHECK(ret, FAIL, "H5HP_insert");

    /* Increment object one's priority by two to put it on top of the heap */
    ret=H5HP_incr(heap, 2, &obj1);
    CHECK(ret, FAIL, "H5HP_change");

    /* Check the maximum value on the heap */
    ret=H5HP_top(heap, &val);
    CHECK(ret, FAIL, "H5HP_top");
    VERIFY(val, 21, "H5HP_top");

    /* Increment object three's priority by two to put it back on top of the heap */
    ret=H5HP_incr(heap, 2, &obj3);
    CHECK(ret, FAIL, "H5HP_change");

    /* Check the maximum value on the heap */
    ret=H5HP_top(heap, &val);
    CHECK(ret, FAIL, "H5HP_top");
    VERIFY(val, 22, "H5HP_top");

    /* Decrement object three's priority by two to return object one to the top */
    ret=H5HP_decr(heap,2,&obj3);
    CHECK(ret, FAIL, "H5HP_change");

    /* Check the maximum value on the heap */
    ret=H5HP_top(heap, &val);
    CHECK(ret, FAIL, "H5HP_top");
    VERIFY(val, 21, "H5HP_top");

    /* Close the heap */
    ret=H5HP_close(heap);
    CHECK(ret, FAIL, "H5HP_close");

} /* end test_heap_incdec_max() */

/****************************************************************
**
**  test_heap_incdec (): Test H5HP (heap) code.
**      Tests incrementing & decrementing priority of objects on
**      maximum & minimum heaps.
**
****************************************************************/
static void
test_heap_incdec(void)
{
    /* Output message about test being performed */
    MESSAGE(6, ("Testing Incrementing & Decrementing Priority of Objects in Heaps\n"));

    /* Test increments & decrements in minimum & maximum heaps */
    test_heap_incdec_max();
    test_heap_incdec_min();
} /* end test_heap_incdec() */

/****************************************************************
**
**  test_heap(): Main H5HP testing routine.
**
****************************************************************/
void
test_heap(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing Heaps\n"));

    /* Initialize Heap testing data */
    test_heap_init();

    /* Actual Heap tests */
    test_heap_create();          /* Test Heap creation */
    test_heap_insert();          /* Test basic Heap insertion */
    test_heap_insert_many();     /* Test Heap insertion of many items */
    test_heap_remove();          /* Test basic Heap removal */
    test_heap_remove_many();     /* Test Heap removal of many items */
    test_heap_change();          /* Test changing priority of objects on Heap */
    test_heap_incdec();          /* Test incrementing & decrementing priority of objects on Heap */

}   /* end test_heap() */

