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
   util.cpp - Utility routines for HDF5 C++ tests.

   EXTERNAL ROUTINES/VARIABLES:

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

#include "h5test.h"
#include "H5Cpp.h"

#ifndef H5_NO_NAMESPACE
    using namespace H5;
#endif

#include "h5cpputil.h"


/*-------------------------------------------------------------------------
 * Function:	test_report
 *
 * Purpose:	Prints out the number of errors for the tests indicated
 * 		by 'testname,' if there were any failures occurred.  If
 * 		no failure, test_report prints out the tests passed message.
 *
 * Return:	if any failure has occurred:	1
 *
 *		if no failure occurs:	0
 *
 * Programmer:	Binh-Minh Ribler (using C code segment for reporting tests)
 *		Friday, February 6, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int test_report( int nerrors, const H5std_string& testname )
{
   if (nerrors)
   {
      nerrors = MAX(1, nerrors);
	if (1 == nerrors)
	    cerr << "***** " << nerrors << testname
					<< " TEST FAILED! *****" << endl;
	else
	    cerr << "***** " << nerrors << testname
					<< " TESTS FAILED! *****" << endl;
      return 1;
   }
   else
   {
      cerr << "All" << testname << " tests passed." << endl;
      return 0;
   }
}

/*-------------------------------------------------------------------------
 * Function:	issue_fail_msg
 *
 * Purpose:	Displays that a function has failed with its location.
 *
 * Return:	None
 *
 * Programmer:	Binh-Minh Ribler (copied and modified macro CHECK from C)
 *		Monday, December 20, 2004
 *
 *-------------------------------------------------------------------------
 */
void issue_fail_msg(const char* where, int line, const char* file_name,
		    const char* message)
{
    //if (GetTestVerbosity()>=VERBO_HI)
    {
        cerr << ">>> FAILED in " << where << " at line " << line
             << " in " << file_name << " - " << message << endl << endl;
    }
}

/*-------------------------------------------------------------------------
 * Function:	check_values
 *
 * Purpose:	Checks a read value against the written value.  If they are
 *		different, the function will print out a message and the
 *		different values.  This function is made to reuse the code
 *		segment that is used in various places throughout
 *		the test code.  Where the C version of this code segment
 *		"goto error," this function will return -1.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Binh-Minh Ribler (using C code segment for checking values)
 *		Friday, February 6, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int check_values (hsize_t i, hsize_t j, int apoint, int acheck)
{
    if (apoint != acheck)
    {
	cerr << "    Read different values than written.\n" << endl;
	cerr << "    At index " << (unsigned long)i << "," <<
   	(unsigned long)j << endl;
	return -1;
    }
    return 0;
} // check_values

/*-------------------------------------------------------------------------
 * Function:	verify_val (const char*, const char*,...)
 *
 * Purpose:	Compares two character strings.  If they are
 *		different, the function will print out a message and the
 *		different values.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Binh-Minh Ribler
 *		May 2, 2010
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void verify_val(const char* x, const char* value, const char* where, int line, const char* file_name)
{
    if (GetTestVerbosity()>=VERBO_HI)
    {
        cerr << endl;
        cerr << "   Call to routine: " << where << " at line " << line
             << " in " << file_name <<  " had value " << x << endl;
    }
    if (strcmp(x, value) != 0)
    {
        cerr << endl;
        cerr << "*** UNEXPECTED VALUE from " << where << " should be "
             << value << ", but is " << x << " at line " << line
             << " in " << file_name << endl;
        IncTestNumErrs();
        throw TestFailedException(where, "");
    }
}

//--------------------------------------------------------------------------
// Function:    InvalidActionException default constructor
//--------------------------------------------------------------------------
InvalidActionException::InvalidActionException():Exception(){}

//--------------------------------------------------------------------------
// Function:    InvalidActionException overloaded constructor
//
// Purpose:	Creates an InvalidActionException with the name of the function,
//              which the failure should have occurred but didn't, and a
//		message explaining why it should fail.
// Parameters
//		func_name - IN: Name of the function where failure should occur
//		message   - IN: Message
//--------------------------------------------------------------------------
InvalidActionException::InvalidActionException(const H5std_string func_name, const H5std_string message) : Exception(func_name, message) {}

//--------------------------------------------------------------------------
// Function:    InvalidActionException destructor
//--------------------------------------------------------------------------
InvalidActionException::~InvalidActionException() {}

//--------------------------------------------------------------------------
// Function:    TestFailedException default constructor
//--------------------------------------------------------------------------
TestFailedException::TestFailedException():Exception(){}

//--------------------------------------------------------------------------
// Function:    TestFailedException overloaded constructor
//
// Purpose:	Creates an TestFailedException with the name of the function,
//              which the failure should have occurred but didn't, and a
//		message explaining why it should fail.
// Parameters
//		func_name - IN: Name of the function where failure should occur
//		message   - IN: Message
//--------------------------------------------------------------------------
TestFailedException::TestFailedException(const H5std_string func_name, const H5std_string message) : Exception(func_name, message) {}

//--------------------------------------------------------------------------
// Function:    TestFailedException destructor
//--------------------------------------------------------------------------
TestFailedException::~TestFailedException() {}

