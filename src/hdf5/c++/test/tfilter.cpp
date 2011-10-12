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

/*****************************************************************************
   FILE
   tfilter.cpp - HDF5 C++ testing various filters and their combination.

 ***************************************************************************/

#ifdef OLD_HEADER_FILENAME
#include <iostream.h>
#else
#include <iostream>
#endif
#include <string>

#ifndef H5_NO_NAMESPACE
#ifndef H5_NO_STD
    using std::cerr;
    using std::endl;
#endif  // H5_NO_STD
#endif

#include "H5Cpp.h"      // C++ API header file

#ifndef H5_NO_NAMESPACE
    using namespace H5;
#endif

#include "h5cpputil.h"  // C++ utilility header file

#define DSET_DIM1	  100
#define DSET_DIM2	  200
#define FILTER_CHUNK_DIM1 2
#define FILTER_CHUNK_DIM2 25

// will do this function later or use it as guideline - BMR - 2007/01/26
static herr_t test_filter_internal(hid_t fid, const char *name, hid_t dcpl,
		int if_fletcher32, int corrupted, hsize_t *dset_size)
{
    cerr << "do nothing right now" << endl;
    return(0);
}

/* Temporary filter IDs used for testing */
#define H5Z_FILTER_BOGUS        305
static size_t filter_bogus(unsigned int flags, size_t cd_nelmts,
    const unsigned int *cd_values, size_t nbytes, size_t *buf_size, void **buf);
/* This message derives from H5Z */
const H5Z_class2_t H5Z_BOGUS[1] = {{
    H5Z_CLASS_T_VERS,       /* H5Z_class_t version */
    H5Z_FILTER_BOGUS,           /* Filter id number             */
    1, 1,               /* Encoding and decoding enabled */
    "bogus",                    /* Filter name for debugging    */
    NULL,                       /* The "can apply" callback     */
    NULL,                       /* The "set local" callback     */
    (H5Z_func_t)filter_bogus,   /* The actual filter function   */
}};

/*-------------------------------------------------------------------------
 * Function:    filter_bogus
 *
 * Purpose:     A bogus compression method that doesn't do anything.
 *
 * Return:      Success:        Data chunk size
 *
 *              Failure:        0
 *
 * Programmer:  Robb Matzke
 *              Tuesday, April 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
filter_bogus(unsigned int UNUSED flags, size_t UNUSED cd_nelmts,
      const unsigned int UNUSED *cd_values, size_t nbytes,
      size_t UNUSED *buf_size, void UNUSED **buf)
{
    return nbytes;
}

/*-------------------------------------------------------------------------
 * Function:	test_null_filter
 *
 * Purpose:	Test null I/O filter by itself.
 *
 * Return:	None
 *
 * Programmer:	Binh-Minh Ribler (use C version, from dsets.c/test_filters)
 *		January, 2007
 *
 * Modifications:
 *		Note: H5Z interface is not implemented yet.
 *
 *-------------------------------------------------------------------------
 */

// Chunk dimensions
const hsize_t chunk_size[2] = {FILTER_CHUNK_DIM1, FILTER_CHUNK_DIM2};

static void test_null_filter()
{
    // Output message about test being performed
    SUBTEST("'Null' filter");
    try {
	//hsize_t  null_size;          // Size of dataset with null filter

	// Prepare dataset create property list
	DSetCreatPropList dsplist;
	dsplist.setChunk(2, chunk_size);

	if (H5Zregister (H5Z_BOGUS)<0)
            throw Exception("test_null_filter", "H5Zregister failed");

	// Set some pretent filter
	dsplist.setFilter(H5Z_FILTER_BOGUS);

	// this function is just a stub right now; will work on it later - BMR
	//if(test_filter_internal(file,DSET_BOGUS_NAME,dc,DISABLE_FLETCHER32,DATA_NOT_CORRUPTED,&null_size)<0)
        //  throw Exception("test_null_filter", "test_filter_internal failed");

	// Close objects.
	dsplist.close();
	PASSED();
    } // end of try

    // catch all other exceptions
    catch (Exception E)
    {
        issue_fail_msg("test_null_filter()", __LINE__, __FILE__, E.getCDetailMsg());
    }
}  // test_null_filter

/*-------------------------------------------------------------------------
 * Function:	test_szip_filter
 *
 * Purpose:	Test SZIP filter by itself.
 *
 * Return:	None
 *
 * Programmer:	Binh-Minh Ribler (partly from dsets.c/test_filters)
 *		January, 2007
 *
 * Modifications:
 *		Note: H5Z interface is not implemented yet.
 *
 *-------------------------------------------------------------------------
 */

const H5std_string      DSET_SZIP_NAME("szipped dataset");

void test_szip_filter(H5File& file1)
{
#ifdef H5_HAVE_FILTER_SZIP
    int      points[DSET_DIM1][DSET_DIM2], check[DSET_DIM1][DSET_DIM2];
    unsigned szip_options_mask=H5_SZIP_NN_OPTION_MASK;
    unsigned szip_pixels_per_block=4;

    // Output message about test being performed
    SUBTEST("szip filter (with encoder)");

    if ( h5_szip_can_encode() == 1) {
        char* tconv_buf = new char [1000];

        try {
            const hsize_t   size[2] = {DSET_DIM1, DSET_DIM2};

            // Create the data space
            DataSpace space1(2, size, NULL);

            // Create a small conversion buffer to test strip mining (?)
            DSetMemXferPropList xfer;
            xfer.setBuffer (1000, tconv_buf, NULL);

            // Prepare dataset create property list
            DSetCreatPropList dsplist;
            dsplist.setChunk(2, chunk_size);

            // Set up for szip compression
            dsplist.setSzip(szip_options_mask, szip_pixels_per_block);

            // Create a dataset with szip compression
            DataSpace space2 (2, size, NULL);
            DataSet dataset(file1.createDataSet (DSET_SZIP_NAME, PredType::NATIVE_INT, space2, dsplist));

            hsize_t i, j, n;
            for (i=n=0; i<size[0]; i++)
            {
                for (j=0; j<size[1]; j++)
                {
                    points[i][j] = (int)n++;
                }
            }

            // Write to the dataset then read back the values
            dataset.write ((void*)points, PredType::NATIVE_INT, DataSpace::ALL, DataSpace::ALL, xfer);
            dataset.read ((void*)check, PredType::NATIVE_INT, DataSpace::ALL, DataSpace::ALL, xfer);

            // Check that the values read are the same as the values written
            for (i = 0; i < size[0]; i++)
                for (j = 0; j < size[1]; j++)
                {
                    int status = check_values (i, j, points[i][j], check[i][j]);
                    if (status == -1)
                        throw Exception("test_szip_filter", "Failed in testing szip method");
                }
            dsplist.close();
            PASSED();
        } // end of try

        // catch all other exceptions
        catch (Exception E)
        {
            issue_fail_msg("test_szip_filter()", __LINE__, __FILE__, E.getCDetailMsg());
        }

        delete tconv_buf;
    } // if szip presents
    else {
	SKIPPED();
    }

#else /* H5_HAVE_FILTER_SZIP */
    SUBTEST("szip filter");
    SKIPPED();
    puts("    Szip filter not enabled");
#endif /* H5_HAVE_FILTER_SZIP */
}  // test_szip_filter


/****************************************************************
**
**  test_filters(): Main routine for testing filters.
**
****************************************************************/
const H5std_string      FILE1("tfilters.h5");
#ifdef __cplusplus
extern "C"
#endif
void test_filters()
{
    // Output message about test being performed
    //MESSAGE("Testing Various Filters\n");
    MESSAGE(5, ("Testing Various Filters\n"));

    hid_t       fapl_id;
    fapl_id = h5_fileaccess(); // in h5test.c, returns a file access template

    int         nerrors=0;      // keep track of number of failures occurr
    try
    {
        // Use the file access template id to create a file access prop. list
        FileAccPropList fapl(fapl_id);

        H5File file1(FILE1, H5F_ACC_TRUNC, FileCreatPropList::DEFAULT, fapl);

	// Test basic VL string datatype
	test_null_filter();
	test_szip_filter(file1);
    }
    catch (Exception E)
    {
        issue_fail_msg("test_filters()", __LINE__, __FILE__, E.getCDetailMsg());
    }
}   // test_filters()

/*-------------------------------------------------------------------------
 * Function:	cleanup_filters
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	Quincey Koziol
 *              September 10, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
#ifdef __cplusplus
extern "C"
#endif
void cleanup_filters()
{
    HDremove(FILE1.c_str());
}
