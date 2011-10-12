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
   testhdf5.cpp - HDF5 testing framework main file.

   REMARKS
   General test wrapper for HDF5 C++ library test programs

   DESIGN
   Each test function should be implemented as function having no
   parameters and returning void (i.e. no return value).  They should be put
   into the list of AddTest() calls in main() below.  Functions which depend
   on other functionality should be placed below the AddTest() call for the
   base functionality testing.

   EXTERNAL ROUTINES/VARIABLES:
	TestInit(...) -- Initialize testing framework
	TestInfo(...) -- Print test info
	AddTest(...)  -- Setup a test function and add it to the list of tests
	TestParseCmdLine(...) -- Parse command line arguments
	PerformTests() -- Perform requested testing
	GetTestSummary() -- Retrieve Summary request value
	TestSummary() -- Display test summary
	GetTestCleanup() -- Retrieve Cleanup request value
	TestCleanup() -- Clean up files from testing
	GetTestNumErrs() -- Retrieve the number of testing errors

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

#include "h5test.h"	// C test header file
#include "H5Cpp.h"	// C++ API header file

#ifndef H5_NO_NAMESPACE
    using namespace H5;
#endif  /* !H5_NO_NAMESPACE */

#include "h5cpputil.h"  // C++ utilility header file

int
main(int argc, char *argv[])
{
    /* Initialize testing framework */
    TestInit(argv[0], NULL, NULL);

    // testing file creation and opening in tfile.cpp
    AddTest("tfile", test_file, cleanup_file, "File I/O Operations", NULL);
    // testing dataset functionalities in dset.cpp
    AddTest("dsets", test_dset, cleanup_dsets, "Dataset I/O Operations", NULL);
    // testing dataspace functionalities in th5s.cpp
    AddTest("th5s",  test_h5s,  cleanup_h5s,  "Dataspaces", NULL);
    // testing attribute functionalities in tattr.cpp
    AddTest("tattr", test_attr, cleanup_attr,  "Attributes", NULL);
    // testing reference functionalities in trefer.cpp
    AddTest("trefer", test_reference, cleanup_reference,  "References", NULL);
    // testing variable-length strings in tvlstr.cpp
    AddTest("tvlstr", test_vlstrings, cleanup_vlstrings,  "Variable-Length Strings", NULL);
    AddTest("ttypes", test_types, cleanup_types,  "Generic Data Types", NULL);
    AddTest("tcompound", test_compound, cleanup_compound,  "Compound Data Types", NULL);
    AddTest("tfilter", test_filters, cleanup_filters,  "Various Filters", NULL);
    AddTest("tlinks", test_links, cleanup_links,  "Various Links", NULL);
/* Comment out tests that are not done yet. - BMR, Feb 2001
    AddTest("select", test_select, cleanup_select,  "Selections", NULL);
    AddTest("time", test_time, cleanup_time,  "Time Datatypes", NULL);
    AddTest("vltypes", test_vltypes, cleanup_vltypes,  "Variable-Length Datatypes", NULL);
    AddTest("iterate", test_iterate, cleanup_iterate,  "Group & Attribute Iteration", NULL);
    AddTest("array", test_array, cleanup_array,  "Array Datatypes", NULL);
    AddTest("genprop", test_genprop, cleanup_genprop,  "Generic Properties", NULL);
    AddTest("id", test_ids, NULL,  "User-Created Identifiers", NULL);

Comment out tests that are not done yet */

/* Tentative - BMR 2007/1/12
    AddTest("datatypes", test_dtypes, cleanup_dtypes,  "Data Types", NULL);
    AddTest("enum", test_enum, cleanup_enum,  "Enum Data Types", NULL);
*/
    /* Display testing information */
    TestInfo(argv[0]);

    /* Parse command line arguments */
    TestParseCmdLine(argc,argv);

    /* Perform requested testing */
    PerformTests();

    /* Display test summary, if requested */
    if (GetTestSummary())
        TestSummary();

    /* Clean up test files, if allowed */
    if (GetTestCleanup() && !getenv("HDF5_NOCLEANUP"))
        TestCleanup();

    return (GetTestNumErrs());
}
