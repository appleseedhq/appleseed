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
 * This program checks if group exists in a file and creates it including
 * all intermediate groups.
 */


#include "hdf5.h"


#define H5FILE_NAME    "interm_group.h5"
#define TRUE            1
#define FALSE           0

int
main(void)
{

    hid_t    file;
    hid_t    g1_id, g2_id, g3_id;
    hid_t    grp_crt_plist;
    H5G_info_t g2_info;
    char     name[3];

    herr_t   status;
    int      i;


    /*
     * Create a file using the default properties.
     */
    file = H5Fcreate(H5FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create a group in the file.
     */
    g1_id = H5Gcreate2(file, "/G1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    H5Gclose(g1_id);
    H5Fclose(file);

    /*
     * Now reopen the file and group in the file.
     */
    file = H5Fopen(H5FILE_NAME, H5F_ACC_RDWR, H5P_DEFAULT);

    /*
     * Check if group /G1 exists in the file.
     */
    if(H5Lexists(file, "/G1", H5P_DEFAULT) !=FALSE)
    printf("Group /G1 exists in the file\n");

    /*
     * Check that group G2/G3 exists in /G1 and if not create it using
     * intermediate group creation property.
     */
    g1_id = H5Gopen2(file, "/G1", H5P_DEFAULT);
/* Next commented call causes error stack to be printed out; the next one
 * works fine; is it a bug or a feature? EIP 04-25-07
*/
/*  if (H5Lexists(g1_id, "G2/G3", H5P_DEFAULT) !=TRUE) { */
    if (H5Lexists(g1_id, "G2", H5P_DEFAULT) !=TRUE) {

    grp_crt_plist = H5Pcreate(H5P_LINK_CREATE);

    /* Set flag for intermediate group creation */
    status = H5Pset_create_intermediate_group(grp_crt_plist, TRUE);
    g3_id = H5Gcreate2(g1_id, "G2/G3", grp_crt_plist, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(g3_id);
    }
    H5Gclose(g1_id);


    /* Now check if group /G1/G2 exists in the file, then open it and print
     * its members names
     */
    if (H5Lexists(file, "/G1/G2", H5P_DEFAULT)) {

    	g2_id = H5Gopen2(file, "/G1/G2", H5P_DEFAULT);
    	status = H5Gget_info(g2_id, &g2_info);
    	printf("Group /G1/G2 has %d member(s)\n", (int)g2_info.nlinks);

    	for (i=0; i < (int)g2_info.nlinks; i++) {
    	H5Lget_name_by_idx(g2_id, ".", H5_INDEX_NAME, H5_ITER_NATIVE, (hsize_t)i,
    	                   name, 3, H5P_DEFAULT);
    	printf("Object's name is %s\n", name);

    	}
    H5Gclose(g2_id);
    }
    H5Fclose(file);
    return 0;
}






