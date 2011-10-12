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

#include "hdf5.h"
#include "hdf5_hl.h"
#include <stdlib.h>

/*-------------------------------------------------------------------------
 * Packet Table Fixed-Length Example
 *
 * Example program that creates a packet table and performs
 * writes and reads.
 *
 *-------------------------------------------------------------------------
 */

int main(void)
{
 hid_t          fid;        /* File identifier */
 hid_t          ptable;     /* Packet table identifier */

 herr_t         err;        /* Function return status */
 hsize_t        count;      /* Number of records in the table */

 int            x;          /* Loop variable */

    /* Buffers to hold data */
 int writeBuffer[5];
 int readBuffer[5];

   /* Initialize buffers */
 for(x=0; x<5; x++)
 {
     writeBuffer[x]=x;
     readBuffer[x] = -1;
 }

    /* Create a file using default properties */
 fid=H5Fcreate("packet_table_FLexample.h5",H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT);

    /* Create a fixed-length packet table within the file */
    /* This table's "packets" will be simple integers and it will use compression
     * level 5. */
 ptable = H5PTcreate_fl(fid, "Packet Test Dataset", H5T_NATIVE_INT, (hsize_t)100, 5);
 if(ptable == H5I_INVALID_HID)
     goto out;

    /* Write one packet to the packet table */
 err = H5PTappend(ptable, (hsize_t)1, &(writeBuffer[0]) );
 if(err < 0)
     goto out;

    /* Write several packets to the packet table */
 err = H5PTappend(ptable, (hsize_t)4, &(writeBuffer[1]) );
 if(err < 0)
     goto out;

    /* Get the number of packets in the packet table.  This should be five. */
 err = H5PTget_num_packets(ptable, &count);
 if(err < 0)
     goto out;

 printf("Number of packets in packet table after five appends: %d\n", (int)count);

    /* Initialize packet table's "current record" */
 err = H5PTcreate_index(ptable);
 if(err < 0)
     goto out;

    /* Iterate through packets, read each one back */
 for(x=0; x<5; x++)
 {
    err = H5PTget_next(ptable, (hsize_t)1, &(readBuffer[x]) );
    if(err < 0)
	goto out;

    printf("Packet %d's value is %d\n", x, readBuffer[x]);
 }

    /* Close the packet table */
 err = H5PTclose(ptable);
 if(err < 0)
     goto out;

    /* Close the file */
 H5Fclose(fid);

 return 0;

 out: /* An error has occurred.  Clean up and exit. */
    H5PTclose(ptable);
    H5Fclose(fid);
    return -1;
}

