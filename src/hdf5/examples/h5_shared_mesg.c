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
 *  This program illustrates the usage of HDF5's implicit message sharing
 *  feature, which can be used to save space when the same messages are
 *  used many times in a file.
 *
 *  This example creates a standard file using file creation property lists
 *  to control which messages are shared.  Messages that can be shared are
 *  datatypes, dataspaces, attributes, fill values, and filter pipelines.
 *
 */

#include <stdlib.h>

#include "hdf5.h"

#define NUM_DATASETS 40
const char* DSETNAME[] = {
    "dataset0",    "dataset1",
    "dataset2",    "dataset3",
    "dataset4",    "dataset5",
    "dataset6",    "dataset7",
    "dataset8",    "dataset9",
    "dataset10",   "dataset11",
    "dataset12",   "dataset13",
    "dataset14",   "dataset15",
    "dataset16",   "dataset17",
    "dataset18",   "dataset19",
    "dataset20",   "dataset21",
    "dataset22",   "dataset23",
    "dataset24",   "dataset25",
    "dataset26",   "dataset27",
    "dataset28",   "dataset29",
    "dataset30",   "dataset31",
    "dataset32",   "dataset33",
    "dataset34",   "dataset35",
    "dataset36",   "dataset37",
    "dataset38",   "dataset39",
    NULL
};

herr_t create_standard_file(const char *filename, hid_t fcpl);

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Enables shared messages using File Creation Property Lists
 *              and creates files using these settings.
 *
 *-------------------------------------------------------------------------
 */
int main(void)
{
    hid_t fcpl_id;
    herr_t ret;

    /* Create a file creation property list */
    fcpl_id = H5Pcreate(H5P_FILE_CREATE);
    if(fcpl_id < 0) goto error;

    /* The file creation property list is the default list right now.
     * Create a file using it (this is the same as creating a file with
     * H5P_DEFAULT).  Implicit shared messages will be disabled.
     */
    ret = create_standard_file("default_file.h5", fcpl_id);
    if(ret < 0) goto error;

    /* There are five kinds of messages that can be shared: datatypes,
     * dataspaces, attributes, fill values, and filter pipelines.
     * Shared messages are stored in up to five "indexes," where each
     * index can contain one or more types of message.  Using more indexes
     * will result in more overhead for sharing, but can also provide
     * more "tunability" and may affect caching performance.
     */
    /* To begin with, use only one index. */
    ret = H5Pset_shared_mesg_nindexes(fcpl_id, 1);
    if(ret < 0) goto error;

    /* Each index has a "minimum message size" for a message of that
     * type to be shared.  Since sharing a message creates some overhead,
     * this is to prevent this overhead for very small messages when little
     * space would be saved by sharing them anyway.
     * If the content of the file isn't known beforehand, it's probably best
     * to set the minimum size "high"; over 100 or 200 bytes.  If the content
     * of the file is known, this value can be used to trade space saved for
     * performance lost.  The smaller this value is, the more messages will
     * be shared, so the more overhead will be incurred.
     * This value is in bytes.  A shared message involves about 30 bytes of
     * overhead.  Note that even messages that are only written once will
     * require this overhead (since they "might" be shared in the future),
     * so setting the minimum size too low may result in a file actually growing
     * in size.
     * For this example case, we'll set the minimum sharing size to be small
     * since we know that every message the "standard" file uses will be
     * repeated many times.
     */
    /* The other property that each index has is the kinds of messages that
     * it holds.  For the simple case, we'll put every message that could be
     * shared in this single index.
     */
    ret = H5Pset_shared_mesg_index(fcpl_id, 0, H5O_SHMESG_ALL_FLAG, 40);
    if(ret < 0) goto error;

    /* The other property that can be set for shared messages is the
     * list/B-tree cutoff for the indexes.
     * Each shared message index beins life as a simple list of messages
     * and becomes a B-tree when "too many" messages are written to it.
     * This keeps the indexes simple when only a few messages are shared,
     * but allows them to scale for many messages.  If many messages are
     * deleted from the B-tree, it scales back down into a list.
     * A "resonable" setting for maximum list size and minimum btree size
     * depends on what kinds of messages will be stored in the file.
     * These numbers are the same for all indexes in a file.
     * We'll guess at some numbers, though we could just as easily have kept
     * the default values.  The first value is the maximum list size, the
     * second the minimum B-tree size.
     */
    ret = H5Pset_shared_mesg_phase_change(fcpl_id, 30, 20);
    if(ret < 0) goto error;

    /* Now create a file with this property list.  After the FCPL is used,
     * everything is automatic; messages will be shared and this will be
     * completely transparent to the user.  Even if the file is closed
     * and re-opened, this settings will be saved and applied to messages
     * written later.
     */
    ret = create_standard_file("one_index_file.h5", fcpl_id);
    if(ret < 0) goto error;

    /* Now try some variations on this.  The FCPL hasn't been closed, so
     * we don't need to re-create it.
     * For instance, if we set the index to only share very large
     * messages, none of the messages we write will qualify and the file
     * will be about the same size as a normal file (with just a little extra
     * overhead).
     */
    ret = H5Pset_shared_mesg_index(fcpl_id, 0, H5O_SHMESG_ALL_FLAG, 1000);
    if(ret < 0) goto error;

    ret = create_standard_file("only_huge_mesgs_file.h5", fcpl_id);
    if(ret < 0) goto error;


    /* Or, suppose we only wanted to shared dataspaces and
     * attributes (which might make sense if we were going to use committed
     * datatypes).  We could change the flags on the index:
     */
    ret = H5Pset_shared_mesg_index(fcpl_id, 0, H5O_SHMESG_SDSPACE_FLAG | H5O_SHMESG_ATTR_FLAG, 40);
    if(ret < 0) goto error;

    ret = create_standard_file("only_dspaces_and_attrs_file.h5", fcpl_id);
    if(ret < 0) goto error;


    /* We could create a second index and put attributes in it to separate them
     * from datatypes and dataspaces (and then run some performance metrics to
     * see whether this improved caching performance).
     */
    ret = H5Pset_shared_mesg_nindexes(fcpl_id, 2);
    if(ret < 0) goto error;
    ret = H5Pset_shared_mesg_index(fcpl_id, 0, H5O_SHMESG_DTYPE_FLAG | H5O_SHMESG_SDSPACE_FLAG, 40);
    if(ret < 0) goto error;
    ret = H5Pset_shared_mesg_index(fcpl_id, 1, H5O_SHMESG_ATTR_FLAG, 40);
    if(ret < 0) goto error;

    ret = create_standard_file("separate_indexes_file.h5", fcpl_id);
    if(ret < 0) goto error;

    /* We can try twiddling the "phase change" values and see what it does to
     * the file size.  Since there's only a few different messages (two
     * datatypes, two dataspaces, and one attribute), using smaller lists will
     * save some space.
     */
    ret = H5Pset_shared_mesg_nindexes(fcpl_id, 1);
    if(ret < 0) goto error;
    ret = H5Pset_shared_mesg_index(fcpl_id, 0, H5O_SHMESG_ALL_FLAG, 40);
    if(ret < 0) goto error;

    ret = H5Pset_shared_mesg_phase_change(fcpl_id, 5, 0);
    if(ret < 0) goto error;

    ret = create_standard_file("small_lists_file.h5", fcpl_id);
    if(ret < 0) goto error;

    /* Or we could create indexes that are never lists, but are created as
     * B-trees.  We do this by setting the "maximum list size" to zero.
     */
    ret = H5Pset_shared_mesg_phase_change(fcpl_id, 0, 0);
    if(ret < 0) goto error;

    ret = create_standard_file("btrees_file.h5", fcpl_id);
    if(ret < 0) goto error;


    /* Obviously there are a lot more permutations of these options possible.
     * Performance will often be a tradeoff of speed for space, but will
     * depend a great deal on the specific application.  If performance is
     * important, the best thing to do is to play with these settings to find
     * the ones that work best for you.
     * Please let The HDF Group (help@hdfgroup.org) know what you find!
     */


    /* Close the property list */
    ret = H5Pclose(fcpl_id);
    if(ret < 0) goto error;
    return 0;

error:
    return -1;
}

/*-------------------------------------------------------------------------
 * Function:    create_standard_file
 *
 * Purpose:     A helper functon for the example.  Creates an HDF5 file
 *              with many repeated messages using the file creation
 *              property list FCPL.
 *
 *              This function only uses datatypes, dataspaces, and
 *              attributes.  Fill values and filter pipelines can also
 *              be shared in the same way (i.e., by enabling sharing in
 *              the FCPL and writing the same message more than once).
 *-------------------------------------------------------------------------
 */
herr_t
create_standard_file(const char *filename, hid_t fcpl_id)
{
    hid_t file_id=-1;
    hid_t type_id=-1, temp_type_id=-1;
    hsize_t dims[] = {10, 9, 8, 7, 6, 5, 4, 3, 2, 1};
    hid_t space_id=-1;
    hid_t attr_type_id = -1;
    hid_t attr_space_id = -1;
    int attr_data[] = {1,2,3,4,5,6,7,8,9,0};
    hid_t dset_id=-1;
    hid_t attr_id=-1;
    int x;
    herr_t ret;

    /* Create the file */
    file_id = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl_id, H5P_DEFAULT);
    if(file_id < 0) goto error;

    /* Create the datatype we'll be using.  Generally, sharing messages
     * is most useful when the message is complex and takes more space on
     * disk, so this type will be an array type rather than an atomic type.
     * However, any type can be shared.
     */
    temp_type_id = H5Tarray_create2(H5T_NATIVE_INT, 2, dims);
    if(temp_type_id < 0) goto error;
    type_id  = H5Tarray_create2(temp_type_id, 2, dims);
    if(type_id < 0) goto error;
    ret = H5Tclose(temp_type_id);
    if(ret < 0) goto error;

    /* Create the dataspace we'll be using.
     * Again, create a more complex dataspace so that more space will
     * be saved when we share it.
     */
    space_id = H5Screate_simple(10, dims, dims);
    if(space_id < 0) goto error;

    /* Create a datatype and dataspace for the attributes we'll be creating.
     * The datatype will be a single integer, and each attribute will hold
     * 10 integers.
     */
    attr_type_id = H5Tcopy(H5T_NATIVE_INT);
    if(attr_type_id < 0) goto error;
    attr_space_id = H5Screate_simple(1, dims, dims);
    if(attr_space_id < 0) goto error;


    /* Begin using the messages many times.  Do this by creating datasets
     * that use this datatype, dataspace, and have this attribute.
     */
    for(x = 0; x < NUM_DATASETS; ++x) {
       /* Create a dataset */
       dset_id = H5Dcreate2(file_id, DSETNAME[x], type_id, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
       if(dset_id < 0) goto error;

       /* Create an attribute on the dataset */
       attr_id = H5Acreate2(dset_id, "attr_name", attr_type_id, attr_space_id, H5P_DEFAULT, H5P_DEFAULT);
       if(attr_id < 0) goto error;

       /* Write data to the attribute */
       ret = H5Awrite(attr_id, H5T_NATIVE_INT, attr_data);
       if(ret < 0) goto error;

       ret = H5Aclose(attr_id);
       if(ret < 0) goto error;
       ret = H5Dclose(dset_id);
       if(ret < 0) goto error;
    }

    /* Close all open IDs */
    ret = H5Tclose(attr_type_id);
    if(ret < 0) goto error;
    ret = H5Sclose(attr_space_id);
    if(ret < 0) goto error;
    ret = H5Tclose(type_id);
    if(ret < 0) goto error;
    ret = H5Sclose(space_id);
    if(ret < 0) goto error;
    ret = H5Fclose(file_id);
    if(ret < 0) goto error;

    return 0;

error:
    return -1;
}

