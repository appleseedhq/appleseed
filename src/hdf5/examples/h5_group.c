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
 * This program creates a group in the file and two datasets in the group.
 * Hard link to the group object is created and one of the datasets is accessed
 * under new name.
 * Iterator functions are used to find information about the objects
 * in the root group and in the created group.
 */


#include "hdf5.h"


#define H5FILE_NAME    "group.h5"
#define RANK    2

static herr_t file_info(hid_t loc_id, const char *name, const H5L_info_t *linfo,
    void *opdata);              /* Link iteration operator function */
static herr_t group_info(hid_t loc_id, const char *name, const H5L_info_t *linfo,
    void *opdata);              /* Link iteration operator function */
int
main(void)
{

    hid_t    file;
    hid_t    grp;
    hid_t    dataset, dataspace;
    hid_t    plist;

    herr_t   status;
    hsize_t  dims[2];
    hsize_t  cdims[2];

    int      idx_f, idx_g;

    /*
     * Create a file.
     */
    file = H5Fcreate(H5FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create a group in the file.
     */
    grp = H5Gcreate2(file, "/Data", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create dataset "Compressed Data" in the group using absolute
     * name. Dataset creation property list is modified to use
     * GZIP compression with the compression effort set to 6.
     * Note that compression can be used only when dataset is chunked.
     */
    dims[0] = 1000;
    dims[1] = 20;
    cdims[0] = 20;
    cdims[1] = 20;
    dataspace = H5Screate_simple(RANK, dims, NULL);
    plist     = H5Pcreate(H5P_DATASET_CREATE);
                H5Pset_chunk(plist, 2, cdims);
                H5Pset_deflate( plist, 6);
    dataset = H5Dcreate2(file, "/Data/Compressed_Data", H5T_NATIVE_INT,
                        dataspace, H5P_DEFAULT, plist, H5P_DEFAULT);
    /*
     * Close the first dataset .
     */
    H5Sclose(dataspace);
    H5Dclose(dataset);

    /*
     * Create the second dataset.
     */
    dims[0] = 500;
    dims[1] = 20;
    dataspace = H5Screate_simple(RANK, dims, NULL);
    dataset = H5Dcreate2(file, "/Data/Float_Data", H5T_NATIVE_FLOAT,
			dataspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     *Close the second dataset and file.
     */
    H5Sclose(dataspace);
    H5Dclose(dataset);
    H5Pclose(plist);
    H5Gclose(grp);
    H5Fclose(file);

    /*
     * Now reopen the file and group in the file.
     */
    file = H5Fopen(H5FILE_NAME, H5F_ACC_RDWR, H5P_DEFAULT);
    grp  = H5Gopen2(file, "Data", H5P_DEFAULT);

    /*
     * Access "Compressed_Data" dataset in the group.
     */
    dataset = H5Dopen2(grp, "Compressed_Data", H5P_DEFAULT);
    if( dataset < 0) printf(" Dataset 'Compressed-Data' is not found. \n");
    printf("\"/Data/Compressed_Data\" dataset is open \n");

    /*
     * Close the dataset.
     */
    status = H5Dclose(dataset);

    /*
     * Create hard link to the Data group.
     */
    status = H5Lcreate_hard(file, "Data", H5L_SAME_LOC, "Data_new", H5P_DEFAULT, H5P_DEFAULT);

    /*
     * We can access "Compressed_Data" dataset using created
     * hard link "Data_new".
     */
    dataset = H5Dopen2(file, "/Data_new/Compressed_Data", H5P_DEFAULT);
    if( dataset < 0) printf(" Dataset is not found. \n");
    printf("\"/Data_new/Compressed_Data\" dataset is open \n");

    /*
     * Close the dataset.
     */
    status = H5Dclose(dataset);


    /*
     * Use iterator to see the names of the objects in the root group.
     */
    idx_f = H5Literate(file, H5_INDEX_NAME, H5_ITER_INC, NULL, file_info, NULL);

    /*
     * Unlink  name "Data" and use iterator to see the names
     * of the objects in the file root direvtory.
     */
    if(H5Ldelete(file, "Data", H5P_DEFAULT) < 0)
      printf(" H5Ldelete failed \n");
    else
      printf("\"Data\" is unlinked \n");

    idx_f = H5Literate(file, H5_INDEX_NAME, H5_ITER_INC, NULL, file_info, NULL);

    /*
     * Use iterator to see the names of the objects in the group
     * /Data_new.
     */
    idx_g = H5Literate_by_name(grp, "/Data_new", H5_INDEX_NAME, H5_ITER_INC, NULL, group_info, NULL, H5P_DEFAULT);

    /*
     * Close the file.
     */

    H5Gclose(grp);
    H5Fclose(file);

    return 0;
}

/*
 * Operator function.
 */
static herr_t
file_info(hid_t loc_id, const char *name, const H5L_info_t *linfo, void *opdata)
{
    /* avoid compiler warnings */
    loc_id = loc_id;
    opdata = opdata;
    linfo = linfo;

    /*
     * Display group name. The name is passed to the function by
     * the Library. Some magic :-)
     */
    printf("\nName : %s\n", name);

    return 0;
}


/*
 * Operator function.
 */
static herr_t
group_info(hid_t loc_id, const char *name, const H5L_info_t *linfo, void *opdata)
{
    hid_t did;  /* dataset identifier  */
    hid_t tid;  /* datatype identifier */
    H5T_class_t t_class;
    hid_t pid;  /* data_property identifier */
    hsize_t chunk_dims_out[2];
    int  rank_chunk;

    /* avoid warnings */
    opdata = opdata;
    linfo = linfo;

    /*
     * Open the datasets using their names.
     */
    did = H5Dopen2(loc_id, name, H5P_DEFAULT);

    /*
     * Display dataset name.
     */
    printf("\nName : %s\n", name);

    /*
     * Display dataset information.
     */
    tid = H5Dget_type(did);  /* get datatype*/
    pid = H5Dget_create_plist(did); /* get creation property list */

    /*
     * Check if dataset is chunked.
     */
    if(H5D_CHUNKED == H5Pget_layout(pid)) {
        /*
         * get chunking information: rank and dimensions.
         */
        rank_chunk = H5Pget_chunk(pid, 2, chunk_dims_out);
        printf("chunk rank %d, dimensions %lu x %lu\n", rank_chunk,
            (unsigned long)(chunk_dims_out[0]),
            (unsigned long)(chunk_dims_out[1]));
    }
    else {
        t_class = H5Tget_class(tid);
        if(t_class < 0) {
            puts(" Invalid datatype.\n");
        }
        else {
            if(t_class == H5T_INTEGER)
                puts(" Datatype is 'H5T_NATIVE_INTEGER'.\n");
            if(t_class == H5T_FLOAT)
                puts(" Datatype is 'H5T_NATIVE_FLOAT'.\n");
            if(t_class == H5T_STRING)
                puts(" Datatype is 'H5T_NATIVE_STRING'.\n");
            if(t_class == H5T_BITFIELD)
                puts(" Datatype is 'H5T_NATIVE_BITFIELD'.\n");
            if(t_class == H5T_OPAQUE)
                puts(" Datatype is 'H5T_NATIVE_OPAQUE'.\n");
            if(t_class == H5T_COMPOUND)
                puts(" Datatype is 'H5T_NATIVE_COMPOUND'.\n");
        }
    }

    H5Dclose(did);
    H5Pclose(pid);
    H5Tclose(tid);
    return 0;
}

