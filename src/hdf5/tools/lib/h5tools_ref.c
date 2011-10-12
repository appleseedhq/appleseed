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
#include <stdlib.h>
#include "h5tools_ref.h"
#include "H5private.h"
#include "H5SLprivate.h"
#include "h5tools.h"
#include "h5tools_utils.h"
#include "h5trav.h"


/*
 *  Table to look up a path name for an object
 *  reference.
 *
 *  This table stores mappings of reference -> path
 *  for all objects in the file that may be the target of
 *  an object reference.
 *
 *  The 'path' is an absolute path by which the object
 *  can be accessed.  When an object has > 1 such path,
 *  only one will be used in the table, with no particular
 *  method of selecting which one.
 */

typedef struct {
    haddr_t objno;      /* Object ID (i.e. address) */
    const char *path;   /* Object path */
} ref_path_node_t;

static H5SL_t *ref_path_table = NULL;   /* the "table" (implemented with a skip list) */
static hid_t thefile = (-1);

static int ref_path_table_put(const char *, haddr_t objno);

/*-------------------------------------------------------------------------
 * Function:    free_ref_path_info
 *
 * Purpose:     Free the key for a reference path table node
 *
 * Return:      Non-negative on success, negative on failure
 *
 * Programmer:  Quincey Koziol
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
free_ref_path_info(void *item, void UNUSED *key, void UNUSED *operator_data/*in,out*/)
{
    ref_path_node_t *node = (ref_path_node_t *)item;

    HDfree((void *)node->path);
    HDfree(node);

    return(0);
}

/*-------------------------------------------------------------------------
 * Function:    init_ref_path_cb
 *
 * Purpose:     Called by interator to create references for
 *              all objects and enter them in the table.
 *
 * Return:      Error status.
 *
 * Programmer:  REMcG
 *
 *-------------------------------------------------------------------------
 */
static herr_t
init_ref_path_cb(const char *obj_name, const H5O_info_t *oinfo,
    const char *already_seen, void UNUSED *_udata)
{
    /* Check if the object is already in the path table */
    if(NULL == already_seen) {
        /* Insert the object into the path table */
        ref_path_table_put(obj_name, oinfo->addr);
    } /* end if */

    return 0;
}

/*-------------------------------------------------------------------------
 * Function:    init_ref_path_table
 *
 * Purpose:     Initalize the reference path table
 *
 * Return:      Non-negative on success, negative on failure
 *
 * Programmer:  Quincey Koziol
 *
 *-------------------------------------------------------------------------
 */
static int
init_ref_path_table(void)
{
    /* Sanity check */
    HDassert(thefile > 0);

    /* Create skip list to store reference path information */
    if((ref_path_table = H5SL_create(H5SL_TYPE_HADDR))==NULL)
        return (-1);

    /* Iterate over objects in this file */
    if(h5trav_visit(thefile, "/", TRUE, TRUE, init_ref_path_cb, NULL, NULL) < 0) {
        error_msg("unable to construct reference path table\n");
        h5tools_setstatus(EXIT_FAILURE);
    } /* end if */

    return(0);
}

/*-------------------------------------------------------------------------
 * Function:    term_ref_path_table
 *
 * Purpose:     Terminate the reference path table
 *
 * Return:      Non-negative on success, negative on failure
 *
 * Programmer:  Quincey Koziol
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
term_ref_path_table(void)
{
    /* Destroy reference path table, freeing all memory */
    if(ref_path_table)
        H5SL_destroy(ref_path_table, free_ref_path_info, NULL);

    return(0);
}

/*-------------------------------------------------------------------------
 * Function:    ref_path_table_lookup
 *
 * Purpose:     Looks up a table entry given a path name.
 *              Used during construction of the table.
 *
 * Return:      The table entre (pte) or NULL if not in the
 *              table.
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
haddr_t
ref_path_table_lookup(const char *thepath)
{
    H5O_info_t  oi;

    /* Allow lookups on the root group, even though it doesn't have any link info */
    if(HDstrcmp(thepath, "/")) {
        H5L_info_t  li;

        /* Check for external link first, so we don't return the OID of an object in another file */
        if(H5Lget_info(thefile, thepath, &li, H5P_DEFAULT) < 0)
            return HADDR_UNDEF;

        /* UD links can't be followed, so they always "dangle" like soft links.  */
        if(li.type >= H5L_TYPE_UD_MIN)
            return HADDR_UNDEF;
    } /* end if */

    /* Get the object info now */
    /* (returns failure for dangling soft links) */
    if(H5Oget_info_by_name(thefile, thepath, &oi, H5P_DEFAULT) < 0)
        return HADDR_UNDEF;

    /* Return OID */
    return(oi.addr);
}

/*-------------------------------------------------------------------------
 * Function:    ref_path_table_put
 *
 * Purpose:     Enter the 'obj' with 'path' in the table (assumes its not
 *              already there)
 *
 *              Create an object reference, pte, and store them
 *              in the table.
 *
 *              NOTE: Takes ownership of the path name string passed in!
 *
 * Return:      Non-negative on success, negative on failure
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
ref_path_table_put(const char *path, haddr_t objno)
{
    ref_path_node_t *new_node;

    HDassert(ref_path_table);
    HDassert(path);

    if((new_node = HDmalloc(sizeof(ref_path_node_t))) == NULL)
        return(-1);

    new_node->objno = objno;
    new_node->path = HDstrdup(path);

    return(H5SL_insert(ref_path_table, new_node, &(new_node->objno)));
}

/*
 *  counter used to disambiguate multiple instances of same object.
 */
int xid = 1;

int get_next_xid(void) {
    return xid++;
}

/*
 *  This counter is used to create fake object ID's
 *  The idea is to set it to the largest possible offest, which
 *  minimizes the chance of collision with a real object id.
 *
 */
haddr_t fake_xid = HADDR_MAX;
haddr_t
get_fake_xid (void) {
    return (fake_xid--);
}

/*
 * for an object that does not have an object id (e.g., soft link),
 * create a table entry with a fake object id as the key.
 *
 * Assumes 'path' is for an object that is not in the table.
 *
 */

haddr_t
ref_path_table_gen_fake(const char *path)
{
    haddr_t fake_objno;

    /* Generate fake ID for string */
    fake_objno = get_fake_xid();

    /* Create ref path table, if it hasn't already been created */
    if(ref_path_table == NULL)
        init_ref_path_table();

    /* Insert "fake" object into table */
    ref_path_table_put(path, fake_objno);

    return(fake_objno);
}

/*-------------------------------------------------------------------------
 * Function:    lookup_ref_path
 *
 * Purpose:     Lookup the path to the object with refernce 'ref'.
 *
 * Return:      Return a path to the object, or NULL if not found.
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
const char *
lookup_ref_path(haddr_t ref)
{
    ref_path_node_t *node;

    /* Be safer for h5ls */
    if(thefile < 0)
        return(NULL);

    /* Create ref path table, if it hasn't already been created */
    if(ref_path_table == NULL)
        init_ref_path_table();

    node = H5SL_search(ref_path_table, &ref);

    return(node ? node->path : NULL);
}

/*-------------------------------------------------------------------------
 * Function:    fill_ref_path_table
 *
 * Purpose:     Called by interator to create references for
 *              all objects and enter them in the table.
 *
 * Return:      Error status.
 *
 * Programmer:  REMcG
 *
 *-------------------------------------------------------------------------
 */
herr_t
fill_ref_path_table(hid_t fid)
{
    /* Set file ID for later queries (XXX: this should be fixed) */
    thefile = fid;

    /* Defer creating the ref path table until it's needed */

    return 0;
}

