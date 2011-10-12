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
       tst.c
   Test HDF Ternary Search Tree (tst) routines.

   REMARKS

   DESIGN

   BUGS/LIMITATIONS

   EXPORTED ROUTINES

   AUTHOR
       Quincey Koziol

   MODIFICATION HISTORY
       12/9/02 - Started coding
 */

#include <time.h>
#include <stdlib.h>

#include "testhdf5.h"
#include "H5STprivate.h"

/* Test words to insert into s TST */
static const char *words[]={
    "We", "hold", "these", "truths", "to", "be", "self-evident,", "that",
    "all", "men", "are", "created", "equal,", "that", "they", "are", "endowed",
    "by", "their", "Creator", "with", "certain", "unalienable", "Rights,",
    "that", "among", "these", "are", "Life,", "Liberty", "and", "the",
    "pursuit", "of", "Happiness."
};
/* Number of words in test words set */
size_t num_words;

/* Number of unique words in test word set */
size_t num_uniq_words;
/* Unique words in test word set */
char **uniq_words;
/* Randomized order version of words in test word set */
char **rand_uniq_words;
/* Sorted order version of words in test word set */
char **sort_uniq_words;

static int tst_strcmp(const void *_s1, const void *_s2)
{
    return(HDstrcmp(*(const char * const *)_s1,*(const char * const *)_s2));
}

/****************************************************************
**
**  test_tst_init(): Test basic H5ST (ternary search tree) selection code.
**      Initialize data for TST testing
**
****************************************************************/
static void
test_tst_init(void)
{
    time_t curr_time;   /* Current time, for seeding random number generator */
    char *tmp_word;/* Temporary pointer to word in word set */
    size_t u,v,w;       /* Local index variables */

    /* Compute the number of words in the test set */
    num_words=sizeof(words)/sizeof(words[0]);

    /* Determine the number of unique words in test set */
    /* (Not particularly efficient, be careful if many words are added to set) */
    num_uniq_words=0;
    for(u=0; u<num_words; u++) {
        /* Assume word is unique */
        num_uniq_words++;
        for(v=0; v<u; v++)
            /* If word is already found in words looked at, decrement unique count */
            if(!HDstrcmp(words[u],words[v])) {
                num_uniq_words--;
                break;
            } /* end if */
    } /* end for */

    /* Allocate space for the array of unique words */
    uniq_words=HDmalloc(sizeof(char *)*num_uniq_words);

    /* Allocate space for the array of randomized order unique words also */
    rand_uniq_words=HDmalloc(sizeof(char *)*num_uniq_words);

    /* Allocate space for the array of sorted order unique words also */
    sort_uniq_words=HDmalloc(sizeof(char *)*num_uniq_words);

    /* Insert unique words from test set into unique word set */
    w=0;
    for(u=0; u<num_words; u++) {
        /* Assume word is unique */
        tmp_word=(char *)words[u];
        for(v=0; v<u; v++)
            /* If word is already found in words looked at, decrement unique count */
            if(!HDstrcmp(words[u],words[v])) {
                tmp_word=NULL;
                break;
            } /* end if */

        /* Check if word was actually unique */
        if(tmp_word!=NULL)
            uniq_words[w++]=tmp_word;
    } /* end for */

    /* Create randomized set of unique words */
    for(u=0; u<num_uniq_words; u++)
        rand_uniq_words[u]=uniq_words[u];
    curr_time=HDtime(NULL);
    HDsrandom((unsigned)curr_time);
    for(u=0; u<num_uniq_words; u++) {
        v=u+(HDrandom()%(num_uniq_words-u));
        if(u!=v) {
            tmp_word=rand_uniq_words[u];
            rand_uniq_words[u]=rand_uniq_words[v];
            rand_uniq_words[v]=tmp_word;
        } /* end if */
    } /* end for */

    /* Create sorted set of unique words */
    for(u=0; u<num_uniq_words; u++)
        sort_uniq_words[u]=uniq_words[u];
    HDqsort(sort_uniq_words,num_uniq_words,sizeof(char *),tst_strcmp);
} /* end test_tst_init() */

/****************************************************************
**
**  test_tst_create(): Test basic H5ST (ternary search tree) selection code.
**      Tests creating and closing TSTs.
**
****************************************************************/
static void
test_tst_create(void)
{
    H5ST_tree_t *tree;   /* TST created */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Creating & Closing TSTs\n"));

    /* Try closing a NULL tree */
    tree=NULL;
    ret=H5ST_close(tree);
    VERIFY(ret, FAIL, "H5ST_close");

    /* Try creating a TST */
    tree=H5ST_create();
    CHECK(tree, NULL, "H5ST_create");

    /* Try closing a real tree */
    ret=H5ST_close(tree);
    CHECK(ret, FAIL, "H5ST_close");

} /* end test_tst_create() */

/****************************************************************
**
**  test_tst_insert(): Test basic H5ST (ternary search tree) selection code.
**      Tests inserting key/value pairs into TST
**
****************************************************************/
static void
test_tst_insert(void)
{
    H5ST_tree_t *tree;  /* TST created */
    H5ST_ptr_t found;   /* Pointer to TST node found */
    void *obj;          /* Pointer to object located in TST */
    size_t u;           /* Local index counter */
    htri_t check;       /* Is string in TST? */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Inserting Values into TSTs\n"));

    /* Create the TST */
    tree=H5ST_create();
    CHECK(tree, NULL, "H5ST_create");

    /* Insert unique words into TST, in random order */
    for(u=0; u<num_uniq_words; u++) {
        ret=H5ST_insert(tree,rand_uniq_words[u],rand_uniq_words[u]);
        CHECK(ret, FAIL, "H5ST_insert");
    } /* end for */

    /* Verify that all words were inserted into TST properly */
    for(u=0; u<num_uniq_words; u++) {
        /* Check that the word is present */
        check=H5ST_search(tree,uniq_words[u]);
        VERIFY(check, TRUE, "H5ST_search");

        /* Check that the value "payloads" are correct */
        found=H5ST_find(tree,uniq_words[u]);
        CHECK(found, NULL, "H5ST_find");

        if(HDstrcmp((const char *)found->eqkid,uniq_words[u]))
            TestErrPrintf("%d: TST node values don't match!, found->eqkid=%s, uniq_words[%u]=%s\n",__LINE__,(char *)found->eqkid,(unsigned)u,uniq_words[u]);

        obj=H5ST_locate(tree,uniq_words[u]);
        CHECK(obj, NULL, "H5ST_locate");

        if(HDstrcmp((const char *)obj,uniq_words[u]))
            TestErrPrintf("%d: TST objects don't match!, obj=%s, uniq_words[%u]=%s\n",__LINE__,(char *)obj,(unsigned)u,uniq_words[u]);
    } /* end for */

    /* Verify that words not in the TST aren't found */
    check=H5ST_search(tree,"foo");
    VERIFY(check, FALSE, "H5ST_search");
    check=H5ST_search(tree,"bar");
    VERIFY(check, FALSE, "H5ST_search");
    check=H5ST_search(tree,"baz");
    VERIFY(check, FALSE, "H5ST_search");

    /* Close the TST */
    ret=H5ST_close(tree);
    CHECK(ret, FAIL, "H5ST_close");
} /* end test_tst_insert() */

/****************************************************************
**
**  test_tst_iterate(): Test basic H5ST (ternary search tree) code.
**      Tests iterating through key/value pairs in TST
**
****************************************************************/
static void
test_tst_iterate(void)
{
    H5ST_tree_t *tree;  /* TST created */
    H5ST_ptr_t found;   /* Pointer to TST node found */
    size_t u;           /* Local index counter */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Iterating Over TSTs\n"));

    /* Create the TST */
    tree=H5ST_create();
    CHECK(tree, NULL, "H5ST_create");

    /* Insert unique words into TST, in random order */
    for(u=0; u<num_uniq_words; u++) {
        ret=H5ST_insert(tree,rand_uniq_words[u],rand_uniq_words[u]);
        CHECK(ret, FAIL, "H5ST_insert");
    } /* end for */

    /* Use findfirst/findnext calls to iterate through TST */
    found=H5ST_findfirst(tree);
    CHECK(found, NULL, "H5ST_findfirst");
    u=0;
    do {
        /* Check that the strings in the TST are in the correct order */
        if(HDstrcmp((const char *)found->eqkid,sort_uniq_words[u]))
            TestErrPrintf("%d: TST node values don't match!, found->eqkid=%s, sort_uniq_words[%u]=%s\n",__LINE__,(char *)found->eqkid,(unsigned)u,sort_uniq_words[u]);

        /* Advance to next string in TST */
        found=H5ST_findnext(found);
        u++;
    } while(found!=NULL);

    /* Close the TST */
    ret=H5ST_close(tree);
    CHECK(ret, FAIL, "H5ST_close");
} /* end test_tst_iterate() */

/****************************************************************
**
**  test_tst_remove(): Test basic H5ST (ternary search tree) code.
**      Tests removing key/value pairs by string value in TST
**
****************************************************************/
static void
test_tst_remove(void)
{
    H5ST_tree_t *tree;  /* TST created */
    H5ST_ptr_t found;   /* Pointer to TST node found */
    void *obj;          /* Pointer to object removed from TST */
    htri_t check;       /* Is string in TST? */
    size_t u;           /* Local index counter */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Removing String Values from TSTs\n"));

    /* Create the TST */
    tree=H5ST_create();
    CHECK(tree, NULL, "H5ST_create");

    /* Insert unique words into TST, in random order */
    for(u=0; u<num_uniq_words; u++) {
        ret=H5ST_insert(tree,rand_uniq_words[u],rand_uniq_words[u]);
        CHECK(ret, FAIL, "H5ST_insert");
    } /* end for */

    /* Remove strings from TST in random order */
    for(u=0; u<num_uniq_words; u++) {
        obj=H5ST_remove(tree,rand_uniq_words[u]);
        CHECK(obj, NULL, "H5ST_remove");

        /* Check that the correct string was removed from TST */
        if(HDstrcmp((const char *)obj,rand_uniq_words[u]))
            TestErrPrintf("%d: TST node values don't match!, obj=%s, rand_uniq_words[%u]=%s\n",__LINE__,(char *)obj,(unsigned)u,rand_uniq_words[u]);

        /* Check that the string can't be found in the TST any longer */
        check=H5ST_search(tree,rand_uniq_words[u]);
        VERIFY(check, FALSE, "H5ST_search");
    } /* end for */

    /* Re-insert unique words into TST, in random order */
    for(u=0; u<num_uniq_words; u++) {
        ret=H5ST_insert(tree,rand_uniq_words[u],rand_uniq_words[u]);
        CHECK(ret, FAIL, "H5ST_insert");
    } /* end for */

    /* Remove TST nodes from TST in random order */
    for(u=0; u<num_uniq_words; u++) {
        /* Get the pointer to the node to delete */
        found=H5ST_find(tree,rand_uniq_words[u]);
        CHECK(found, NULL, "H5ST_find");

        /* Check that the correct object will be removed from TST */
        if(HDstrcmp((const char *)found->eqkid,rand_uniq_words[u]))
            TestErrPrintf("%d: TST node values don't match!, found->eqkid=%s, rand_uniq_words[%u]=%s\n",__LINE__,(char *)found->eqkid,(unsigned)u,rand_uniq_words[u]);

        /* Remove the node */
        ret=H5ST_delete(tree,found);
        CHECK(ret, FAIL, "H5ST_delete");

        /* Check that the string can't be found in the TST any longer */
        check=H5ST_search(tree,rand_uniq_words[u]);
        VERIFY(check, FALSE, "H5ST_search");
    } /* end for */

    /* Close the TST */
    ret=H5ST_close(tree);
    CHECK(ret, FAIL, "H5ST_close");
} /* end test_tst_remove() */

/****************************************************************
**
**  test_tst_finalize(): Test basic H5ST (ternary search tree) selection code.
**      Wrap up data for TST testing
**
****************************************************************/
static void
test_tst_finalize(void)
{
    /* Release memory for unordered, randomized and sorted order unique words */
    HDfree(uniq_words);
    HDfree(rand_uniq_words);
    HDfree(sort_uniq_words);
} /* end test_tst_finalize() */

/****************************************************************
**
**  test_tst(): Main H5ST selection testing routine.
**
****************************************************************/
void
test_tst(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing Ternary Search Trees\n"));

    /* Initialize TST testing data */
    test_tst_init();

    /* Actual TST tests */
    test_tst_create();          /* Test TST creation */
    test_tst_insert();          /* Test TST insertion */
    test_tst_iterate();         /* Test TST iteration */
    test_tst_remove();          /* Test TST deletion */

    /* Finalize TST testing data */
    test_tst_finalize();
}   /* end test_tst() */

