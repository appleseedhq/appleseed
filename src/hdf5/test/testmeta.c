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
 * This program illustrates assertion errors when linked
 * to HDF5 1.4.1-post2 or 1.4.2-pre3 debug library.
 *
 * If the assertion errors are ignored, the program eventially causes
 * an error in H5Gcreate2 when writing object 83381.
 *
 * When writing in single file mode, the assertion errors still occur
 * but the H5Gcreate2 error does not.
 */


#include "hdf5.h"

#define FILEN		"testmeta.h5"

#define MEMB_SIZE		100000000
#define CHUNK_SIZE		512

#define NDATAARRAYS		3
/*#define NPOINTS			2048*/
#define NPOINTS			20
#define NEXTARRAYS		10
#define NWATTRS			2
#define NDATAOBJECTS	100000

int main(void)
{
    hid_t	file_id, prop_id, memspace_id, type_id;
    hid_t	group_id;
    hid_t	dataset_id, dataspace_id;
    herr_t	status;
    hsize_t	dims[1];
    hsize_t	maxdims[1];
    float	data[NPOINTS];
    float	floatval;
    unsigned	numdataobj	= 0;
    unsigned	i, j;
    char	name[80];
    hsize_t	start[1]			= {0};
    hsize_t	stride[1]			= {1};
    hsize_t	count[1]			= {1};

    /* Create a file */
    file_id = H5Fcreate(FILEN, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create a dataset to hold the number of data objects */
    /* Create the data space */
    dataspace_id = H5Screate(H5S_SCALAR);

    /* Create dataset */
    dataset_id = H5Dcreate2(file_id, "/NumDataObj",
                                    H5T_NATIVE_UINT, dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Write value to NumDataObj dataset */
    status = H5Dwrite(dataset_id, H5T_NATIVE_UINT, H5S_ALL,
            H5S_ALL, H5P_DEFAULT, &numdataobj);

    /* Close the identifiers */
    status = H5Dclose(dataset_id);
    status = H5Sclose(dataspace_id);

    /* Create extendible arrays */
    /* Set up for extendible dataset */
    prop_id = H5Pcreate(H5P_DATASET_CREATE);
    dims[0] = CHUNK_SIZE;
    status = H5Pset_chunk(prop_id, 1, dims);

    /* Create dataspace */
    dims[0]=1;
    maxdims[0]=H5S_UNLIMITED;
    dataspace_id = H5Screate_simple(1, dims, maxdims);

    for(i=0; i<NEXTARRAYS; i++)
    {
        /* Create dataset */
        sprintf(name, "/ExtArray%06d", i);
        dataset_id = H5Dcreate2(file_id, name,
                H5T_NATIVE_FLOAT, dataspace_id, H5P_DEFAULT, prop_id, H5P_DEFAULT);

        /* Close the identifier */
        status = H5Dclose(dataset_id);
    }

    /* Close the identifiers */
    status = H5Sclose(dataspace_id);
    status = H5Pclose(prop_id);

    /* Create group to hold data object data arrays */
    group_id = H5Gcreate2(file_id, "/DataArray", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group_id);

    for(j=0; j<NDATAOBJECTS; j++)
    {
        printf("\rWriting Object #%d of %d", j+1, NDATAOBJECTS);
        fflush(stdout);

        floatval = (float)j;

        /* Create group to hold data arrays for this object */
        sprintf(name, "/DataArray/%06d", j);
        group_id = H5Gcreate2(file_id, name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        if(group_id < 0) {
            fprintf(stderr, "Failed to create DataArray group.\n");
            status = H5Fclose(file_id);
            return -1;
        }

        /* Loop over data arrays */
        for(i=0; i<NDATAARRAYS; i++)
        {
            /* Create dataspace */
            dims[0]=NPOINTS;
            maxdims[0]=NPOINTS;
            dataspace_id = H5Screate_simple(1 ,dims, maxdims);

            /* Create dataset */
            sprintf(name, "DataArray%06d", i);
            dataset_id = H5Dcreate2(group_id, name,
                    H5T_NATIVE_FLOAT, dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            if(dataset_id < 0) {
                fprintf(stderr, "Failed to create DataArray dataset.\n");
                status = H5Fclose(file_id);
                return -1;
            }

            /* Write the data array data */
            status = H5Dwrite(dataset_id, H5T_NATIVE_FLOAT, H5S_ALL,
                    H5S_ALL, H5P_DEFAULT, data);
            if(status < 0) {
                fprintf(stderr, "Failed to write DataArray dataset.\n");
                status = H5Fclose(file_id);
                return -1;
            }

            /* Close the identifiers */
            status = H5Dclose(dataset_id);
            status = H5Sclose(dataspace_id);
        }

        /* Open NumDataObj dataset */
        dataset_id = H5Dopen2(file_id, "/NumDataObj", H5P_DEFAULT);
        if(dataset_id < 0) {
            fprintf(stderr, "Failed to open NumDataObj dataset.\n");
            status = H5Fclose(file_id);
            return -1;
        }

        /* Write value to NumDataObj dataset */
        numdataobj = j + 1;
        status = H5Dwrite(dataset_id, H5T_NATIVE_UINT, H5S_ALL,
                H5S_ALL, H5P_DEFAULT, &numdataobj);
        if(status < 0) {
            fprintf(stderr, "Failed to write NumDataObj dataset.\n");
            status = H5Fclose(file_id);
            return -1;
        }

        /* Close identifiers */
        status = H5Dclose(dataset_id);
        status = H5Gclose(group_id);

        /* Extend attribute arrays */
        for(i = 0; i < NEXTARRAYS; i++) {
            /* Open extendable dataset */
            sprintf(name, "/ExtArray%06d", i);
            dataset_id = H5Dopen2(file_id, name, H5P_DEFAULT);
            if(dataset_id < 0) {
                fprintf(stderr, "Failed to open ExtArray dataset.\n");
                status = H5Fclose(file_id);
                return -1;
            } /* end if */

            /* Extend attribute dataset */
            dims[0] = (hsize_t)j + 1;
            status = H5Dset_extent(dataset_id, dims);
            if(status < 0) {
                fprintf(stderr, "Failed to extend DataArray dataset.\n");
                status = H5Fclose(file_id);
                return -1;
            } /* end if */

            /* Select element and write value to attribute dataset */
            dims[0] = 1;
            memspace_id = H5Screate_simple(1, dims, dims);
            dataspace_id = H5Dget_space(dataset_id);
            type_id = H5Dget_type(dataset_id);

            start[0] = 0;
            status = H5Sselect_hyperslab(memspace_id, H5S_SELECT_SET,
                    start, stride, count, NULL);
            start[0] = (hssize_t)j;
            status = H5Sselect_hyperslab(dataspace_id, H5S_SELECT_SET,
                    start, stride, count, NULL);
            status = H5Dwrite(dataset_id, type_id, memspace_id,
                    dataspace_id, H5P_DEFAULT, &floatval);
            if(status < 0)
            {
                fprintf(stderr, "Failed to write DataArray dataset.\n");
                status = H5Fclose(file_id);
                return -1;
            }

            /* Close identifiers */
            status = H5Tclose(type_id);
            status = H5Sclose(dataspace_id);
            status = H5Sclose(memspace_id);
            status = H5Dclose(dataset_id);
        }
    }


    /* Close the file */
    status = H5Fclose(file_id);

    printf("\n");

    return 0;
}

