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
 * Packet Table Variable-Length Example
 *
 * Example program that creates a variable-length packet table and performs
 * writes and reads.
 *
 *-------------------------------------------------------------------------
 */

int main(void)
{
#ifdef VLPT_REMOVED
 hid_t          fid;        /* File identifier */
 hid_t          ptable;     /* Packet table identifier */

 herr_t         err;        /* Function return status */
 hsize_t        count;      /* Number of records in the table */
 int            x;          /* Loop variable */

    /* Buffers to hold data */
 hvl_t writeBuffer[5];
 hvl_t readBuffer[5];

    /* This example has two different sizes of "record": longs and shorts */
 long longBuffer[5];
 short shortBuffer[5];

   /* Initialize buffers */
 for(x=0; x<5; x++)
 {
     longBuffer[x] = -x;
     shortBuffer[x] = x;
 }

    /* Fill the write buffer with a mix of longs and shorts */
    /* We need to supply the length of each record and a pointer to */
    /* the beginning of each record. */
 writeBuffer[0].len = sizeof(long);
 writeBuffer[0].p = &(longBuffer[0]);
 writeBuffer[1].len = sizeof(short);
 writeBuffer[1].p = &(shortBuffer[1]);
 writeBuffer[2].len = sizeof(long);
 writeBuffer[2].p = &(longBuffer[2]);
 writeBuffer[3].len = sizeof(long);
 writeBuffer[3].p = &(longBuffer[3]);
 writeBuffer[4].len = sizeof(short);
 writeBuffer[4].p = &(shortBuffer[4]);

    /* Create a file using default properties */
 fid=H5Fcreate("packet_table_VLexample.h5",H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT);

    /* Create a variable-length packet table within the file */
 ptable = H5PTcreate_vl(fid, "Packet Test Dataset", (hsize_t)1);
 if(ptable == H5I_INVALID_HID)
     goto out;

    /* Write the entire buffer to the packet table */
 err = H5PTappend(ptable, (hsize_t)5, writeBuffer );
 if(err < 0)
     goto out;

    /* Get the number of packets in the packet table.  This should be five. */
 err = H5PTget_num_packets(ptable, &count);
 if(err < 0)
     goto out;

 printf("Number of packets in packet table after five appends: %d\n", count);

    /* Read all five packets back */
 err = H5PTread_packets(ptable, (hsize_t)0, (hsize_t)5, readBuffer );
 if(err < 0)
    goto out;

 for(x=0; x<5; x++)
 {
    printf("Packet %d's length is %d\n", x, readBuffer[x].len);
    if(readBuffer[x].len == sizeof(long))
        printf("Packet %d's value is %d\n", x, *( (long *) readBuffer[x].p) );
    else
        printf("Packet %d's value is %d\n", x, *( (short *) readBuffer[x].p) );
 }

    /* Before we close the packet table, we must free the memory that */
    /* the pointers in readBuffer point to. */
 err = H5PTfree_vlen_readbuff(ptable, (hsize_t)5, readBuffer);
 if(err < 0)
     goto out;

    /* Close the packet table */
 err = H5PTclose(ptable);
 if(err < 0)
     goto out;

    /* Close the file */
 H5Fclose(fid);
#endif /* VLPT_REMOVED */

 return 0;

#ifdef VLPT_REMOVED
 out: /* An error has occurred.  Clean up and exit. */
    fprintf(stderr, "An error has occurred!\n");
    H5PTclose(ptable);
    H5Fclose(fid);
    return -1;
#endif /* VLPT_REMOVED */
}
