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

/* This program demonstrates how to create and use "external links" in
 * HDF5.
 *
 * External links point from one HDF5 file to an object (Group, Dataset, or
 * committed Datatype) in another file.
 */

#include "hdf5.h"
#include <string.h>

#define SOURCE_FILE "extlink_source.h5"
#define TARGET_FILE "extlink_target.h5"

#define PREFIX_SOURCE_FILE "extlink_prefix_source.h5"

#define SOFT_LINK_FILE "soft_link.h5"
#define SOFT_LINK_NAME "soft_link_to_group"
#define UD_SOFT_LINK_NAME "ud_soft_link"
#define TARGET_GROUP "target_group"

#define UD_SOFT_CLASS 65

#define HARD_LINK_FILE "hard_link.h5"
#define HARD_LINK_NAME "hard_link_to_group"
#define UD_HARD_LINK_NAME "ud_hard_link"

#define UD_HARD_CLASS 66

#define PLIST_LINK_PROP "plist_link_prop"
#define UD_PLIST_CLASS 66



/* Basic external link example
 *
 * Creates two files and uses an external link to access an object in the
 * second file from the first file.
 */
static void extlink_example(void)
{
    hid_t source_file_id, targ_file_id;
    hid_t group_id, group2_id;

    /* Create two files, a source and a target */
    source_file_id = H5Fcreate(SOURCE_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    targ_file_id = H5Fcreate(TARGET_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create a group in the target file for the external link to point to. */
    group_id = H5Gcreate2(targ_file_id, "target_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Close the group and the target file */
    H5Gclose(group_id);

    /* Create an external link in the source file pointing to the target group.
     * We could instead have created the external link first, then created the
     * group it points to; the order doesn't matter.
     */
    H5Lcreate_external(TARGET_FILE, "target_group", source_file_id, "ext_link", H5P_DEFAULT, H5P_DEFAULT);

    /* Now we can use the external link to create a new group inside the
     * target group (even though the target file is closed!).  The external
     * link works just like a soft link.
     */
    group_id = H5Gcreate2(source_file_id, "ext_link/new_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* The group is inside the target file and we can access it normally.
     * Here, group_id and group2_id point to the same group inside the
     * target file.
     */
    group2_id = H5Gopen2(targ_file_id, "target_group/new_group", H5P_DEFAULT);

    /* Don't forget to close the IDs we opened. */
    H5Gclose(group2_id);
    H5Gclose(group_id);

    H5Fclose(targ_file_id);
    H5Fclose(source_file_id);

    /* The link from the source file to the target file will work as long as
     * the target file can be found.  If the target file is moved, renamed,
     * or deleted in the filesystem, HDF5 won't be able to find it and the
     * external link will "dangle."
     */
}


/* External link prefix example
 *
 * Uses a group access property list to set a "prefix" for the filenames
 * accessed through an external link.
 *
 * Group access property lists inherit from link access property lists;
 * the external link prefix property is actually a property of LAPLs.
 *
 * This example requires a "red" directory and a "blue" directory to exist
 * where it is run (so to run this example on Unix, first mkdir red and mkdir
 * blue).
 */
static void extlink_prefix_example(void)
{
    hid_t source_file_id, red_file_id, blue_file_id;
    hid_t group_id, group2_id;
    hid_t gapl_id;

    /* Create three files, a source and two targets.  The targets will have
     * the same name, but one will be located in the red directory and one will
     * be located in the blue directory */
    source_file_id = H5Fcreate(PREFIX_SOURCE_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    red_file_id = H5Fcreate("red/prefix_target.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    blue_file_id = H5Fcreate("blue/prefix_target.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* This test needs a red and a blue directory in the filesystem. If they're not present,
     * trying to create the files above will fail.
     */
    if(red_file_id < 0 || blue_file_id < 0)
      printf("This test requires directories named 'red' and 'blue' to exist. Did you forget to create them?\n");

    /* Create an external link in the source file pointing to the root group of
     * a file named prefix_target.h5.  This file doesn't exist in the current
     * directory, but the files in the red and blue directories both have this
     * name.
     */
    H5Lcreate_external("prefix_target.h5", "/", source_file_id, "ext_link", H5P_DEFAULT, H5P_DEFAULT);

    /* If we tried to traverse the external link now, we would fail (since the
     * file it points to doesn't exist).  Instead, we'll create a group access
     * property list that will provide a prefix path to the external link.
     * Group access property lists inherit the properties of link access
     * property lists.
     */
    gapl_id = H5Pcreate(H5P_GROUP_ACCESS);
    H5Pset_elink_prefix(gapl_id, "red/");

    /* Now if we traverse the external link, HDF5 will look for an external
     * file named red/prefix_target.h5, which exists.
     * To pass the group access property list, we need to use H5Gopen2.
     */
    group_id = H5Gopen2(source_file_id, "ext_link", gapl_id);

    /* Now we can use the open group ID to create a new group inside the
     * "red" file.
     */
    group2_id = H5Gcreate2(group_id, "pink", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Close both groups. */
    H5Gclose(group2_id);
    H5Gclose(group_id);

    /* If we change the prefix, the same external link can find a file in the blue
     * directory.
     */
    H5Pset_elink_prefix(gapl_id, "blue/");
    group_id = H5Gopen2(source_file_id, "ext_link", gapl_id);
    group2_id = H5Gcreate2(group_id, "sky blue", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Close both groups. */
    H5Gclose(group2_id);
    H5Gclose(group_id);

    /* Each file has had a group created inside it using the same external link. */
    group_id = H5Gopen2(red_file_id, "pink", H5P_DEFAULT);
    group2_id = H5Gopen2(blue_file_id, "sky blue", H5P_DEFAULT);

    /* Clean up our open IDs */
    H5Gclose(group2_id);
    H5Gclose(group_id);
    H5Pclose(gapl_id);
    H5Fclose(blue_file_id);
    H5Fclose(red_file_id);
    H5Fclose(source_file_id);

    /* User-defined links can expand on the ability to pass in parameters
     * using an access property list; for instance, a user-defined link
     * might function like an external link but allow the full filename to be
     * passed in through the access property list.
     */
}


/* Soft Link example
 *
 * Create a new class of user-defined links that behave like HDF5's built-in
 * soft links.
 *
 * This isn't very useful by itself (HDF5's soft links already do the same
 * thing), but it can serve as an example for how to reference objects by
 * name.
 */

/* We need to define the callback function that the soft link will use.
 * It is defined after the example below.
 * To keep the example simple, these links don't have a query callback.
 * In general, link classes should always be query-able.
 * We might also have wanted to supply a creation callback that checks
 * that a path was supplied in the udata.
 */
static hid_t UD_soft_traverse(const char *link_name, hid_t cur_group,
    const void *udata, size_t udata_size, hid_t lapl_id);

static void soft_link_example(void)
{
    hid_t file_id;
    hid_t group_id;
    /* Define the link class that we'll use to register "user-defined soft
     * links" using the callbacks we defined above.
     * A link class can have NULL for any callback except its traverse
     * callback.
     */
    const H5L_class_t UD_soft_class[1] = {{
        H5L_LINK_CLASS_T_VERS,      /* Version number for this struct.
                                     * This field is always H5L_LINK_CLASS_T_VERS */
        (H5L_type_t)UD_SOFT_CLASS,  /* Link class id number. This can be any
                                     * value between H5L_TYPE_UD_MIN (64) and
                                     * H5L_TYPE_MAX (255). It should be a
                                     * value that isn't already being used by
                                     * another kind of link. We'll use 65. */
        "UD_soft_link",             /* Link class name for debugging  */
        NULL,                       /* Creation callback              */
        NULL,                       /* Move callback                  */
        NULL,                       /* Copy callback                  */
        UD_soft_traverse,           /* The actual traversal function  */
        NULL,                       /* Deletion callback              */
        NULL                        /* Query callback                 */
    }};


    /* First, create a file and an object within the file for the link to
     * point to.
     */
    file_id = H5Fcreate(SOFT_LINK_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    group_id = H5Gcreate2(file_id, TARGET_GROUP, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group_id);

    /* This is how we create a normal soft link to the group.
     */
    H5Lcreate_soft(TARGET_GROUP, file_id, SOFT_LINK_NAME, H5P_DEFAULT, H5P_DEFAULT);

    /* To do the same thing using a user-defined link, we first have to
     * register the link class we defined.
     */
    H5Lregister(UD_soft_class);

    /* Now create a user-defined link.  We give it the path to the group
     * as its udata.1
     */
    H5Lcreate_ud(file_id, UD_SOFT_LINK_NAME, (H5L_type_t)UD_SOFT_CLASS, TARGET_GROUP,
                 strlen(TARGET_GROUP) + 1, H5P_DEFAULT, H5P_DEFAULT);

    /* We can access the group through the UD soft link like we would through
     * a normal soft link. This link will still dangle if the object's
     * original name is changed or unlinked.
     */
    group_id = H5Gopen2(file_id, UD_SOFT_LINK_NAME, H5P_DEFAULT);

    /* The group is now open normally.  Don't forget to close it! */
    H5Gclose(group_id);

    H5Fclose(file_id);
}

/* UD_soft_traverse
 * The actual traversal function simply needs to open the correct object by
 * name and return its ID.
 */

static hid_t UD_soft_traverse(const char *link_name, hid_t cur_group,
    const void *udata, size_t udata_size, hid_t lapl_id)
{
    const char *target = (const char *) udata;
    hid_t ret_value;

    /* Pass the udata straight through to HDF5. If it's invalid, let HDF5
     * return an error.
     */
    ret_value = H5Oopen(cur_group, target, lapl_id);
    return ret_value;
}


/* Hard Link example
 *
 * Create a new class of user-defined links that behave like HDF5's built-in
 * hard links.
 *
 * This isn't very useful by itself (HDF5's hard links already do the same
 * thing), but it can serve as an example for how to reference objects by
 * address.
 */

/* We need to define the callback functions that the hard link will use.
 * These are defined after the example below.
 * To keep the example simple, these links don't have a query callback.
 * Generally, real link classes should always be query-able.
 */
static herr_t UD_hard_create(const char *link_name, hid_t loc_group,
    const void *udata, size_t udata_size, hid_t lcpl_id);
static herr_t UD_hard_delete(const char *link_name, hid_t loc_group,
    const void *udata, size_t udata_size);
static hid_t UD_hard_traverse(const char *link_name, hid_t cur_group,
    const void *udata, size_t udata_size, hid_t lapl_id);

static void hard_link_example(void)
{
    hid_t file_id;
    hid_t group_id;
    H5L_info_t li;
    /* Define the link class that we'll use to register "user-defined hard
     * links" using the callbacks we defined above.
     * A link class can have NULL for any callback except its traverse
     * callback.
     */
    const H5L_class_t UD_hard_class[1] = {{
        H5L_LINK_CLASS_T_VERS,      /* Version number for this struct.
                                     * This field is always H5L_LINK_CLASS_T_VERS */
        (H5L_type_t)UD_HARD_CLASS,  /* Link class id number. This can be any
                                     * value between H5L_TYPE_UD_MIN (64) and
                                     * H5L_TYPE_MAX (255). It should be a
                                     * value that isn't already being used by
                                     * another kind of link. We'll use 66. */
        "UD_hard_link",             /* Link class name for debugging  */
        UD_hard_create,             /* Creation callback              */
        NULL,                       /* Move callback                  */
        NULL,                       /* Copy callback                  */
        UD_hard_traverse,           /* The actual traversal function  */
        UD_hard_delete,             /* Deletion callback              */
        NULL                        /* Query callback                 */
    }};


    /* First, create a file and an object within the file for the link to
     * point to.
     */
    file_id = H5Fcreate(HARD_LINK_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    group_id = H5Gcreate2(file_id, TARGET_GROUP, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group_id);

    /* This is how we create a normal hard link to the group. This
     * creates a second "name" for the group.
     */
    H5Lcreate_hard(file_id, TARGET_GROUP, file_id, HARD_LINK_NAME, H5P_DEFAULT, H5P_DEFAULT);

    /* To do the same thing using a user-defined link, we first have to
     * register the link class we defined.
     */
    H5Lregister(UD_hard_class);

    /* Since hard links link by object address, we'll need to retrieve
     * the target group's address. We do this by calling H5Lget_info
     * on a hard link to the object.
     */
    H5Lget_info(file_id, TARGET_GROUP, &li, H5P_DEFAULT);

    /* Now create a user-defined link.  We give it the group's address
     * as its udata.
     */
    H5Lcreate_ud(file_id, UD_HARD_LINK_NAME, (H5L_type_t)UD_HARD_CLASS, &(li.u.address),
                 sizeof(li.u.address), H5P_DEFAULT, H5P_DEFAULT);

    /* The UD hard link has now incremented the group's reference count
     * like a normal hard link would.  This means that we can unlink the
     * other two links to that group and it won't be deleted until the
     * UD hard link is deleted.
     */
    H5Ldelete(file_id, TARGET_GROUP, H5P_DEFAULT);
    H5Ldelete(file_id, HARD_LINK_NAME, H5P_DEFAULT);

    /* The group is still accessible through the UD hard link. If this were
     * a soft link instead, the object would have been deleted when the last
     * hard link to it was unlinked. */
    group_id = H5Gopen2(file_id, UD_HARD_LINK_NAME, H5P_DEFAULT);

    /* The group is now open normally.  Don't forget to close it! */
    H5Gclose(group_id);

    /* Removing the user-defined hard link will delete the group. */
    H5Ldelete(file_id, UD_HARD_LINK_NAME, H5P_DEFAULT);

    H5Fclose(file_id);
}

/* Callbacks for User-defined hard links. */
/* UD_hard_create
 * The most important thing this callback does is to increment the reference
 * count on the target object. Without this step, the object could be
 * deleted while this link still pointed to it, resulting in possible data
 * corruption!
 * The create callback also checks the arguments used to create this link.
 * If this function returns a negative value, the call to H5Lcreate_ud()
 * will also return failure and the link will not be created.
 */
static herr_t UD_hard_create(const char *link_name, hid_t loc_group,
    const void *udata, size_t udata_size, hid_t lcpl_id)
{
    haddr_t addr;
    hid_t target_obj = -1;
    herr_t ret_value = 0;

    /* Make sure that the address passed in looks valid */
    if(udata_size != sizeof(haddr_t))
    {
      ret_value = -1;
      goto done;
    }

    addr = *((const haddr_t *) udata);

    /* Open the object this link points to so that we can increment
     * its reference count. This also ensures that the address passed
     * in points to a real object (although this check is not perfect!) */
    target_obj= H5Oopen_by_addr(loc_group, addr);
    if(target_obj < 0)
    {
      ret_value = -1;
      goto done;
    }

    /* Increment the reference count of the target object */
    if(H5Oincr_refcount(target_obj) < 0)
    {
      ret_value = -1;
      goto done;
    }

done:
    /* Close the target object if we opened it */
    if(target_obj >= 0)
        H5Oclose(target_obj);
    return ret_value;
}

/* UD_hard_delete
 * Since the creation function increments the object's reference count, it's
 * important to decrement it again when the link is deleted.
 */
static herr_t UD_hard_delete(const char *link_name, hid_t loc_group,
    const void *udata, size_t udata_size)
{
    haddr_t addr;
    hid_t target_obj = -1;
    herr_t ret_value = 0;

    /* Sanity check; we have already verified the udata's size in the creation
     * callback.
     */
    if(udata_size != sizeof(haddr_t))
    {
      ret_value = -1;
      goto done;
    }

    addr = *((const haddr_t *) udata);

    /* Open the object this link points to */
    target_obj= H5Oopen_by_addr(loc_group, addr);
    if(target_obj < 0)
    {
      ret_value = -1;
      goto done;
    }

    /* Decrement the reference count of the target object */
    if(H5Odecr_refcount(target_obj) < 0)
    {
      ret_value = -1;
      goto done;
    }

done:
    /* Close the target object if we opened it */
    if(target_obj >= 0)
        H5Oclose(target_obj);
    return ret_value;
}

/* UD_hard_traverse
 * The actual traversal function simply needs to open the correct object and
 * return its ID.
 */
static hid_t UD_hard_traverse(const char *link_name, hid_t cur_group,
    const void *udata, size_t udata_size, hid_t lapl_id)
{
    haddr_t       addr;
    hid_t         ret_value = -1;

    /* Sanity check; we have already verified the udata's size in the creation
     * callback.
     */
    if(udata_size != sizeof(haddr_t))
      return -1;

    addr = *((const haddr_t *) udata);

    /* Open the object by address. If H5Oopen_by_addr fails, ret_value will
     * be negative to indicate that the traversal function failed.
     */
    ret_value = H5Oopen_by_addr(cur_group, addr);

    return ret_value;
}



/* Plist example
 *
 * Create a new class of user-defined links that open objects within a file
 * based on a value passed in through a link access property list.
 *
 * Group, dataset, and datatype access property lists all inherit from link
 * access property lists, so they can be used instead of LAPLs.
 */

/* We need to define the callback functions that this link type will use.
 * These are defined after the example below.
 * These links have no udata, so they don't need a query function.
 */
static hid_t UD_plist_traverse(const char *link_name, hid_t cur_group,
    const void *udata, size_t udata_size, hid_t lapl_id);

static void plist_link_example(void)
{
    hid_t file_id;
    hid_t group_id, group2_id;
    hid_t gapl_id;
    char *path = NULL;

    /* Define the link class that we'll use to register "plist
     * links" using the callback we defined above.
     * A link class can have NULL for any callback except its traverse
     * callback.
     */
    const H5L_class_t UD_plist_class[1] = {{
        H5L_LINK_CLASS_T_VERS,      /* Version number for this struct.
                                     * This field is always H5L_LINK_CLASS_T_VERS */
        (H5L_type_t)UD_PLIST_CLASS, /* Link class id number. This can be any
                                     * value between H5L_TYPE_UD_MIN (64) and
                                     * H5L_TYPE_MAX (255). It should be a
                                     * value that isn't already being used by
                                     * another kind of link. We'll use 67. */
        "UD_plist_link",            /* Link class name for debugging  */
        NULL,                       /* Creation callback              */
        NULL,                       /* Move callback                  */
        NULL,                       /* Copy callback                  */
        UD_plist_traverse,          /* The actual traversal function  */
        NULL,                       /* Deletion callback              */
        NULL                        /* Query callback                 */
    }};


    /* First, create a file and two objects within the file for the link to
     * point to.
     */
    file_id = H5Fcreate(HARD_LINK_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    group_id = H5Gcreate2(file_id, "group_1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group_id);
    group_id = H5Gcreate2(file_id, "group_1/group_2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group_id);

    /* Register "plist links" and create one.  It has no udata at all. */
    H5Lregister(UD_plist_class);
    H5Lcreate_ud(file_id, "plist_link", (H5L_type_t)UD_PLIST_CLASS, NULL, 0,
                 H5P_DEFAULT, H5P_DEFAULT);

    /* Create a group access property list to pass in the target for the
     * plist link.
     */
    gapl_id = H5Pcreate(H5P_GROUP_ACCESS);

    /* There is no HDF5 API for setting the property that controls these
     * links, so we have to add the property manually
     */
    H5Pinsert2(gapl_id, PLIST_LINK_PROP, sizeof(const char *), &(path), NULL, NULL, NULL, NULL, NULL, NULL);

    /* Set the property to point to the first group. */
    path = "group_1";
    H5Pset(gapl_id, PLIST_LINK_PROP, &path);

    /* Open the first group through the plist link using the GAPL we just
     * created */
    group_id = H5Gopen2(file_id, "plist_link", gapl_id);

    /* If we change the value set on the property list, it will change where
     * the plist link points.
     */
    path = "group_1/group_2";
    H5Pset(gapl_id, PLIST_LINK_PROP, &path);
    group2_id = H5Gopen2(file_id, "plist_link", gapl_id);

    /* group_id points to group_1 and group2_id points to group_2, both opened
     * through the same link.
     * Using more than one of this type of link could quickly become confusing,
     * since they will all use the same property list; however, there is
     * nothing to prevent the links from changing the property list in their
     * traverse callbacks.
     */

    /* Clean up */
    H5Pclose(gapl_id);
    H5Gclose(group_id);
    H5Gclose(group2_id);
    H5Fclose(file_id);
}

/* Traversal callback for User-defined plist links. */
/* UD_plist_traverse
 * Open a path passed in through the property list.
 */
static hid_t UD_plist_traverse(const char *link_name, hid_t cur_group,
    const void *udata, size_t udata_size, hid_t lapl_id)
{
    char *        path;
    hid_t         ret_value = -1;

    /* If the link property isn't set or can't be found, traversal fails. */
    if(H5Pexist(lapl_id, PLIST_LINK_PROP) < 0)
        goto error;

    if(H5Pget(lapl_id, PLIST_LINK_PROP, &path) < 0)
        goto error;

    /* Open the object by address. If H5Oopen_by_addr fails, ret_value will
     * be negative to indicate that the traversal function failed.
     */
    ret_value = H5Oopen(cur_group, path, lapl_id);

    return ret_value;

error:
    return -1;
}



/* Main function
 *
 * Invokes the example functions.
 */
 int
main(void)
{
    printf("Testing basic external links.\n");
    extlink_example();

    printf("Testing external link prefixes.\n");
    extlink_prefix_example();

    printf("Testing user-defined soft links.\n");
    soft_link_example();

    printf("Testing user-defined hard links.\n");
    hard_link_example();

    printf("Testing user-defined property list links.\n");
    plist_link_example();

    return 0;
}


