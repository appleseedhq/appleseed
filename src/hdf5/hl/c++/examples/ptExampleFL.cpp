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

#include "H5PacketTable.h"

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
    herr_t err;     /* Return value from function calls */
    hid_t fileID;   /* HDF5 identifier for file */
    hsize_t count;  /* Number of records in table */
    int x;          /* Temporary counter variable */

    /* Buffers to hold data */
    int readBuffer[5];
    int writeBuffer[5];

    /* Initialize buffers */
    for(x=0; x<5; x++)
    {
        writeBuffer[x]=x;
        readBuffer[x] = -1;
    }

    /* Create a new HDF5 file */
    fileID = H5Fcreate("PTcppexampleFL.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if(fileID <0)
        fprintf(stderr, "Couldn't create file.\n");

    /* Create a fixed-length packet table with compression level 5. */
    FL_PacketTable ptable(fileID, "/examplePacketTable", H5T_NATIVE_INT, 100, 5);
    if(! ptable.IsValid())
        fprintf(stderr, "Unable to create packet table.");

    /* Append five packets to the packet table, one at a time */
    for(x=0; x<5; x++)
    {
        err = ptable.AppendPacket( &(writeBuffer[x]) );
        if(err<0)
            fprintf(stderr, "Error adding record.");
    }

    /* Get the number of packets in the packet table.  This should be five. */
    count = ptable.GetPacketCount(err);
    if(err < 0)
        fprintf(stderr, "Error getting packet count.");

    printf("Number of packets in packet table after five appends: %d\n", count);

    /* Initialize packet table's "current record" */
    ptable.ResetIndex();

    /* Iterate through packets, read each one back */
    for(x=0; x<5; x++)
    {
        err = ptable.GetNextPacket( &(readBuffer[x]) );
        if(err < 0)
            fprintf(stderr, "Error reading record.");

        printf("Packet %d's value is %d.\n", x, readBuffer[x]);
    }

    /* The packet table will close automatically when its object goes */
    /* out of scope.  */

    err = H5Fclose(fileID);
    if( err < 0 )
        fprintf(stderr, "Failed to close file.\n");

    return 0;
}

