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

/* ptableTest.cpp */

#include "ptableTest.h"

using namespace H5;

#define TEST_FILE "packettest.h5"

/* Main test function */
int main(void)
{
    herr_t err;
    herr_t num_errors = 0;

    /* Create new HDF5 file */
    fileID = H5Fcreate(TEST_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if(fileID <0)
    {
        fprintf(stderr, "Couldn't create file.\n");
        num_errors = 1;
    }
    else {

            num_errors += BasicTest();

            num_errors += TestCompoundDatatype();

            num_errors += TestGetPacket();

            num_errors += TestGetNext();

            num_errors += TestCompress();

            num_errors += TestErrors();

            num_errors += SystemTest();

#ifdef VLPT_REMOVED
            num_errors += VariableLengthTest();
#endif /* VLPT_REMOVED */

        /* Terminate access to the file. */
        err = H5Fclose(fileID);
        if( err < 0 )
        {
            fprintf(stderr, "Failed to close file.\n");
            num_errors++;
        }

        /* Delete the file */
        remove(TEST_FILE);
    }

    if (num_errors == 0)
      /* ALL TESTS PASSED */
      return 0;
    else
      /* ERRORS */
      return -1;
}


int BasicTest()
{
    herr_t err;
    int myRecord;
    hsize_t count;
    int error;

    TESTING("basic funtionality")

    FL_PacketTable wrapper(fileID, "/basicTest", H5T_NATIVE_INT, 1);
    if(! wrapper.IsValid())
      goto out;

    /* Ensure initial count is zero */
    count = wrapper.GetPacketCount(error);
    if(count != 0 || error != 0)
      goto out;

    myRecord = 1;

    /* add some records test */
    err = wrapper.AppendPacket(&myRecord);
    if(err < 0)
        goto out;

    myRecord = 2;

    wrapper.AppendPacket(&myRecord);

    /* get number of records test */
    count = wrapper.GetPacketCount();
    if(count != 2)
      goto out;

    /* get records test */
    err = wrapper.GetPacket(0, &myRecord);
    if(err < 0)
      goto out;

    if(myRecord != 1)
      goto out;

    err = wrapper.GetPacket(1, &myRecord);
    if(err < 0)
      goto out;
    if(myRecord != 2)
      goto out;

    PASSED();
    return 0;

out:
    H5_FAILED();
    return 1;
}

int TestCompoundDatatype()
{
    hid_t dtypeID;
    hsize_t count;
    int error;

    TESTING("compound datatypes")

    /* Create compound datatype */
    typedef struct compoundType
    {
        short a, b, c;
        int e;
    } compoundType;

    dtypeID = H5Tcreate( H5T_COMPOUND, sizeof(compoundType));

    H5Tinsert(dtypeID, "abbey", HOFFSET( compoundType, a ), H5T_NATIVE_SHORT);
    H5Tinsert(dtypeID, "bert", HOFFSET( compoundType, b ), H5T_NATIVE_SHORT);
    H5Tinsert(dtypeID, "charlie", HOFFSET( compoundType, c ), H5T_NATIVE_SHORT);
    H5Tinsert(dtypeID, "ebert", HOFFSET( compoundType, e ), H5T_NATIVE_INT);

    /* Create packet table.  Explicitly specify no compression */
    FL_PacketTable wrapper(fileID, "/compoundTest", dtypeID, 1, -1);

    if(! wrapper.IsValid())
      goto out;

    compoundType first;
    first.a = 1;
    first.b = first.c = 3;
    first.e = 5;

    /* Write packet */
    wrapper.AppendPacket(&first);

    count = wrapper.GetPacketCount(error);
    if(count != 1)
      goto out;

    first.a = first.b = first.c = 0;
    first.e = 0;

    /* Read packet back */
    wrapper.GetPacket(0, &first);

    if(first.a != 1)
      goto out;
    if(first.e != 5)
      goto out;

    PASSED();
    return 0;

out:
    H5_FAILED();
    return 1;
}

int TestGetNext()
{
    int error;
    int record;
    int records[2];
    int i;

    TESTING("GetNextPacket")

    /* Create a dataset */
    FL_PacketTable wrapper(fileID, "/TestGetNext", H5T_NATIVE_INT, 500);

    if(! wrapper.IsValid())
      goto out;

    /* Append 5 records to the dataset */
    for(record = 1; record < 6; record++)
        wrapper.AppendPacket(&record);

    /* Ensure that we can interate through the records and get the right ones */
    for(i = 1; i < 6; i++)
    {
        wrapper.GetNextPacket(&record);
        if(record != i)
          goto out;
    }

    /* Reset the index and check that it worked */
    wrapper.ResetIndex();
    if(wrapper.GetIndex(error) != 0) goto out;
    if(error < 0) goto out;

    /* Ensure that we can interate through the records and get the right ones */
    for(i = 1; i < 6; i++)
    {
        error = wrapper.GetNextPacket(&record);
        if(record != i || error <0)
          goto out;
    }

    wrapper.SetIndex(1);
    if(wrapper.GetIndex(error) != 1) goto out;
    if(error < 0) goto out;

    /* Ensure we can get multiple records with our index pointer */
    wrapper.GetNextPackets(2, records);
    if(records[0] != 2 || records[1] != 3)
      goto out;

    /* Ensure our pointer was updated correctly */
    wrapper.GetNextPacket(&record);
    if(record != 4)
      goto out;

    PASSED();
    return 0;

out:
    H5_FAILED();
    return 1;
}

int TestCompress()
{

	unsigned int flags = 0;
    unsigned int config = 0;
    size_t cd_nelemts = 0;

    TESTING("compression")
#ifdef H5_HAVE_FILTER_DEFLATE
    try {
        /* Create packet table with compression. */
        FL_PacketTable wrapper(fileID, "/compressTest", H5T_NATIVE_CHAR, 100, 8);

        /* Create an HDF5 C++ file object */
        H5File file;
        file.setId(fileID);

        /* Make sure that the deflate filter is set by opening the packet table
         * as a dataset and getting its creation property list */
        DataSet dsetID = file.openDataSet("/compressTest");

        DSetCreatPropList dcplID = dsetID.getCreatePlist();

        dcplID.getFilterById(H5Z_FILTER_DEFLATE, flags, cd_nelemts, NULL, 0, NULL, config);
    } catch (Exception e) {
      H5_FAILED();
      return 1;
    }
    PASSED();
#else
    SKIPPED();
    puts("    deflate filter not enabled");
#endif /* H5_HAVE_FILTER_DEFLATE */
    return 0;
}

int TestGetPacket()
{
    int record;
    int theRecs[3];
    int i;
    TESTING("GetPacket")

    /* Create a dataset.  Explicitly specify no compression */
    FL_PacketTable wrapper(fileID, "/TestGetPacket", H5T_NATIVE_INT, 1, -1);

    if(! wrapper.IsValid())
      goto out;

    /* Append 5 records to the dataset */
    for(record = 1; record < 6; record++)
        wrapper.AppendPacket(&record);

    /* Ensure that the records were written properly */
    wrapper.GetPacket(1, &record);
    if(record != 2)
      goto out;

    /* Ensure that we can retrieve multiple records */
    wrapper.GetPackets(1, 3, theRecs);
    for(i = 0; i < 3; i++)
    {
        if(theRecs[i] != i+2)
          goto out;
    }

    PASSED();
    return 0;

out:
    H5_FAILED();
    return 1;
}

int TestErrors()
{
    TESTING("error conditions")

    /* Create a dataset */
    FL_PacketTable wrapper(fileID, "/TestErrors", H5T_NATIVE_INT, 1);

    if(! wrapper.IsValid())
      goto out;

    int record;
    int records[3];
    int error;

    /* Append 4 records to the dataset */
    for(record = 1; record < 5; record++)
        wrapper.AppendPacket(&record);

    /* Try to confuse functions with bad indexes */
    error = wrapper.GetPacket( (unsigned) -1, &record);
    if(error >= 0)
      goto out;
    error = wrapper.GetPacket(4, &record);
    if(error >= 0)
      goto out;
    error = wrapper.GetPacket((unsigned) -250, &record);
    if(error >= 0)
      goto out;
    error = wrapper.GetPacket(3000, &record);
    if(error >= 0)
      goto out;
    error = wrapper.GetPacket(1, &record);
    if(error < 0)
      goto out;

    error = wrapper.GetPackets((unsigned) -1, 1, records);
    if(error >= 0)
      goto out;
    error = wrapper.GetPackets(2, 4, records);
    if(error >= 0)
      goto out;
    error = wrapper.GetPackets((unsigned) -60, (unsigned) -62, records);
     if(error >= 0)
      goto out;
    error = wrapper.GetPackets(10, 12, records);
    if(error >= 0)
      goto out;
    error = wrapper.GetPackets(0, 2, records);
    if(error < 0)
      goto out;
    error = wrapper.GetPackets(2, 0, records);
    if(error >= 0)
      goto out;
    error = wrapper.GetPackets(1, 1, records);
    if(error < 0)
      goto out;
    error = wrapper.GetPackets(1, 3, records);
    if(error < 0)
      goto out;

    wrapper.ResetIndex();
    error = wrapper.SetIndex((unsigned) -1);
    if(error >= 0)
      goto out;
    if(wrapper.GetIndex(error) != 0) goto out;
    if(error < 0) goto out;
    error = wrapper.GetNextPacket(&record);
    if(error < 0)
      goto out;
    if(record != 1)
      goto out;
    if(wrapper.GetIndex(error) != 1) goto out;
    if(error < 0) goto out;
    error = wrapper.SetIndex(20);
    if(error >= 0)
      goto out;
    error = wrapper.GetNextPacket(&record);
    if(error < 0)
      goto out;
    if(record != 2)
      goto out;
    wrapper.SetIndex(3);
    error = wrapper.GetNextPacket(&record);
    if(error < 0)
      goto out;
    if(record != 4)
      goto out;
    if(wrapper.GetIndex(error) != 4) goto out;
    if(error < 0) goto out;
    error = wrapper.GetNextPacket(&record);
    if(error >= 0)
      goto out;

    wrapper.ResetIndex();
    error = wrapper.GetNextPackets(10, records);
    if(error >= 0)
      goto out;
    error = wrapper.GetNextPackets(0, records);
    if(error < 0)
      goto out;

    PASSED();
    return 0;

out:
    H5_FAILED();
    return 1;
}

int SystemTest()
{
    TESTING("multiple datatypes")

    hid_t dtypeID1, dtypeID2;
    hsize_t count;
    int error;

    /* Creating two inter-related datatypes.  Create two datasets and put
     * one datatype in each. */
    typedef struct compoundType
    {
        short a, b, c;
        int e;
    } compoundType;

    dtypeID1 = H5Tcreate( H5T_COMPOUND, sizeof(compoundType));

    H5Tinsert(dtypeID1, "abbey", HOFFSET( compoundType, a ), H5T_NATIVE_SHORT);
    H5Tinsert(dtypeID1, "bert", HOFFSET( compoundType, b ), H5T_NATIVE_SHORT);
    H5Tinsert(dtypeID1, "charlie", HOFFSET( compoundType, c ), H5T_NATIVE_SHORT);
    H5Tinsert(dtypeID1, "ebert", HOFFSET( compoundType, e ), H5T_NATIVE_INT);

    typedef struct cType2
    {
        char f;
        compoundType g;
    } cType2;

    dtypeID2 = H5Tcreate ( H5T_COMPOUND, sizeof(cType2));

    H5Tinsert(dtypeID2, "f", HOFFSET( cType2, f ), H5T_NATIVE_CHAR);
    H5Tinsert(dtypeID2, "g", HOFFSET( cType2, g ), dtypeID1);

    cType2 ct2[10];
    ct2[0].f = 'h';
    ct2[0].g.a = 9;
    ct2[0].g.b = -13;
    ct2[0].g.c = 0;
    ct2[0].g.e = 3000;

    /* Create the packet table datasets.  Make one of them compressed. */
    FL_PacketTable wrapper1(fileID, "/SystemTest1", dtypeID1, 1);
    FL_PacketTable wrapper2(fileID, "/SystemTest2", dtypeID2, 1, 5);

    if(! wrapper1.IsValid())
      goto out;
    if(! wrapper2.IsValid())
      goto out;

    /* Write and read packets, ensure that nothing is unusual */
    wrapper2.AppendPacket(ct2);

    count = wrapper1.GetPacketCount();
    if(count != 0)
      goto out;

    compoundType ct1[10];
    ct1[0].a = 31;
    ct1[0].b = 4607;
    ct1[0].c = -1002;
    ct1[0].e = 3;

    ct2[1].f = 'b';
    ct2[1].g = ct1[0];

    wrapper1.AppendPacket(ct1);
    wrapper2.AppendPacket(&ct2[1]);

    wrapper1.ResetIndex();
    wrapper1.GetNextPacket(&ct1[1]);
    wrapper2.GetPacket(1, &ct2[2]);
    if(wrapper1.GetIndex(error) != 1) goto out;
    if(error < 0) goto out;
    if(wrapper2.GetIndex(error) != 0) goto out;
    if(error < 0) goto out;

    if(ct1[1].b != ct2[2].g.b)
      goto out;

    PASSED();
    return 0;

out:
    H5_FAILED();
    return 1;
}

#ifdef VLPT_REMOVED
int VariableLengthTest(void)
{
    long test_long;
    short test_short;
    hvl_t read_buf;
    VL_PacketTable* test_VLPT;
    PacketTable* new_pt;

    TESTING("variable-length packet tables")

    /* Create a variable length table */
    test_VLPT = new VL_PacketTable(fileID, "/VariableLengthTest", 1);

    /* Verify that the creation succeeded */
    if(! test_VLPT->IsValid())
      goto out;

    /* Append some packets */
    test_short = 9;
    test_VLPT->AppendPacket(&test_short, sizeof(short));
    test_long = 16;
    test_VLPT->AppendPacket(&test_long, sizeof(long));

    /* Read them back and make sure they are correct */
    test_VLPT->GetNextPackets(1, &read_buf);

    if(read_buf.len != sizeof(short))
      goto out;
    if(*(short *)(read_buf.p) != test_short)
      goto out;

    /* Free the memory used by the read */
    test_VLPT->FreeReadbuff(1, &read_buf);

    /* Read the second record */
    test_VLPT->GetNextPackets(1, &read_buf);

    if(read_buf.len != sizeof(long))
      goto out;
    if(*(long *)(read_buf.p) != test_long)
      goto out;

    /* Free the memory used by the read */
    test_VLPT->FreeReadbuff(1, &read_buf);

    /* Close the packet table */
    delete test_VLPT;

    /* Reopen the packet table and verify that it is variable length */
    new_pt = new PacketTable(fileID, "/VariableLengthTest");

    /* Verify that the open succeeded */
    if(! new_pt->IsValid())
      goto out;

    if(new_pt->IsVariableLength() != 1)
      goto out;

    /* Close the packet table */
    delete new_pt;

    PASSED();
    return 0;

out:
    H5_FAILED();
    return 1;
}
#endif /* VLPT_REMOVED */
