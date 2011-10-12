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
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Friday, November 20, 1998
 *
 * Purpose:     Test support stuff.
 */
#ifndef _H5TEST_H
#define _H5TEST_H

#include "hdf5.h"
#include "H5private.h"

/*
 * Predefined test verbosity levels.
 *
 * Convention:
 *
 * The higher the verbosity value, the more information printed.
 * So, output for higher verbosity also include output of all lower
 * verbosity.
 *
 *  Value     Description
 *  0         None:   No informational message.
 *  1                 "All tests passed"
 *  2                 Header of overall test
 *  3         Default: header and results of individual test
 *  4
 *  5         Low:    Major category of tests.
 *  6
 *  7         Medium: Minor category of tests such as functions called.
 *  8
 *  9         High:   Highest level.  All information.
 */
#define VERBO_NONE 0     /* None    */
#define VERBO_DEF  3     /* Default */
#define VERBO_LO   5     /* Low     */
#define VERBO_MED  7     /* Medium  */
#define VERBO_HI   9     /* High    */

/*
 * Verbose queries
 * Only None needs an exact match.  The rest are at least as much.
 */

/* A macro version of HDGetTestVerbosity(). */
/* Should be used internally by the libtest.a only. */
#define HDGetTestVerbosity() (TestVerbosity)

#define VERBOSE_NONE	(HDGetTestVerbosity()==VERBO_NONE)
#define VERBOSE_DEF	(HDGetTestVerbosity()>=VERBO_DEF)
#define VERBOSE_LO	(HDGetTestVerbosity()>=VERBO_LO)
#define VERBOSE_MED	(HDGetTestVerbosity()>=VERBO_MED)
#define VERBOSE_HI	(HDGetTestVerbosity()>=VERBO_HI)

/*
 * Test controls definitions.
 */
#define SKIPTEST	1	/* Skip this test */
#define ONLYTEST	2	/* Do only this test */
#define BEGINTEST	3	/* Skip all tests before this test */

/*
 * This contains the filename prefix specificied as command line option for
 * the parallel test files.
 */
H5TEST_DLLVAR char *paraprefix;
#ifdef H5_HAVE_PARALLEL
extern MPI_Info h5_io_info_g;         /* MPI INFO object for IO */
#endif

/*
 * Print the current location on the standard output stream.
 */
#define AT() 		printf ("	 at %s:%d in %s()...\n",	      \
				__FILE__, __LINE__, __FUNCTION__);

/*
 * The name of the test is printed by saying TESTING("something") which will
 * result in the string `Testing something' being flushed to standard output.
 * If a test passes, fails, or is skipped then the PASSED(), H5_FAILED(), or
 * SKIPPED() macro should be called.  After H5_FAILED() or SKIPPED() the caller
 * should print additional information to stdout indented by at least four
 * spaces.  If the h5_errors() is used for automatic error handling then
 * the H5_FAILED() macro is invoked automatically when an API function fails.
 */
#define TESTING(WHAT)	{printf("Testing %-62s",WHAT); fflush(stdout);}
#define TESTING_2(WHAT)	{printf(" Testing %-62s",WHAT); fflush(stdout);}
#define PASSED()	{puts(" PASSED");fflush(stdout);}
#define H5_FAILED()	{puts("*FAILED*");fflush(stdout);}
#define H5_WARNING()	{puts("*WARNING*");fflush(stdout);}
#define SKIPPED()	{puts(" -SKIP-");fflush(stdout);}
#define TEST_ERROR      {H5_FAILED(); AT(); goto error;}
#define STACK_ERROR     {H5Eprint2(H5E_DEFAULT, stdout); goto error;}
#define FAIL_STACK_ERROR {H5_FAILED(); AT(); H5Eprint2(H5E_DEFAULT, stdout); \
    goto error;}
#define FAIL_PUTS_ERROR(s) {H5_FAILED(); AT(); puts(s); goto error;}

/*
 * Alarm definitions to wait up (terminate) a test that runs too long.
 */
#define H5_ALARM_SEC	1200	/* default is 20 minutes */
#define ALARM_ON	TestAlarmOn()
#define ALARM_OFF	HDalarm(0)

/*
 * The methods to compare the equality of floating-point values:
 *    1. XXX_ABS_EQUAL - check if the difference is smaller than the
 *       Epsilon value.  The Epsilon values, FLT_EPSILON, DBL_EPSILON,
 *       and LDBL_EPSILON, are defined by compiler in float.h.
 *    2. XXX_REL_EQUAL - check if the relative difference is smaller than a
 *       predefined value M.  See if two values are relatively equal.
 *       It's the test's responsibility not to pass in the value 0, which
 *       may cause the equation to fail.
 */
#define FLT_ABS_EQUAL(X,Y)	((float)fabs(X-Y)<FLT_EPSILON)
#define DBL_ABS_EQUAL(X,Y)	(fabs(X-Y)<DBL_EPSILON)
#define LDBL_ABS_EQUAL(X,Y)	(fabsl(X-Y)<LDBL_EPSILON)

#define FLT_REL_EQUAL(X,Y,M)    (fabsf((Y-X)/X<M)
#define DBL_REL_EQUAL(X,Y,M)    (fabs((Y-X)/X)<M)
#define LDBL_REL_EQUAL(X,Y,M)    (fabsl((Y-X)/X)<M)

#ifdef __cplusplus
extern "C" {
#endif

/* Generally useful testing routines */
H5TEST_DLL int h5_cleanup(const char *base_name[], hid_t fapl);
H5TEST_DLL char *h5_fixname(const char *base_name, hid_t fapl, char *fullname,
		 size_t size);
H5TEST_DLL hid_t h5_fileaccess(void);
H5TEST_DLL void h5_no_hwconv(void);
H5TEST_DLL const char *h5_rmprefix(const char *filename);
H5TEST_DLL void h5_reset(void);
H5TEST_DLL void h5_show_hostname(void);
H5TEST_DLL h5_stat_size_t h5_get_file_size(const char *filename, hid_t fapl);
H5TEST_DLL int print_func(const char *format, ...);
H5TEST_DLL int h5_make_local_copy(char *origfilename, char *local_copy_name);
H5TEST_DLL herr_t h5_verify_cached_stabs(const char *base_name[], hid_t fapl);

/* Routines for operating on the list of tests (for the "all in one" tests) */
H5TEST_DLL void TestUsage(void);
H5TEST_DLL void AddTest(const char *TheName, void (*TheCall) (void),
	     void (*Cleanup) (void), const char *TheDescr,
	     const void *Parameters);
H5TEST_DLL void TestInfo(const char *ProgName);
H5TEST_DLL void TestParseCmdLine(int argc, char *argv[]);
H5TEST_DLL void PerformTests(void);
H5TEST_DLL void TestSummary(void);
H5TEST_DLL void TestCleanup(void);
H5TEST_DLL void TestInit(const char *ProgName, void (*private_usage)(void), int (*private_parser)(int ac, char *av[]));
H5TEST_DLL int  GetTestVerbosity(void);
H5TEST_DLL int  SetTestVerbosity(int newval);
H5TEST_DLL int  GetTestSummary(void);
H5TEST_DLL int  GetTestCleanup(void);
H5TEST_DLL int  SetTestNoCleanup(void);
H5TEST_DLL int  GetTestExpress(void);
H5TEST_DLL int  SetTestExpress(int newval);
H5TEST_DLL void ParseTestVerbosity(char *argv);
H5TEST_DLL int  GetTestNumErrs(void);
H5TEST_DLL void  IncTestNumErrs(void);
H5TEST_DLL const void *GetTestParameters(void);
H5TEST_DLL int  TestErrPrintf(const char *format, ...);
H5TEST_DLL void SetTest(const char *testname, int action);
H5TEST_DLL void TestAlarmOn(void);
H5TEST_DLL void TestAlarmOff(void);
H5TEST_DLL void PrintErrorStackOn(void);
H5TEST_DLL void PrintErrorStackOff(void);

#ifdef H5_HAVE_FILTER_SZIP
H5TEST_DLL int h5_szip_can_encode(void);
#endif /* H5_HAVE_FILTER_SZIP */

#ifdef H5_HAVE_PARALLEL
H5TEST_DLL int h5_set_info_object(void);
H5TEST_DLL void h5_dump_info_object(MPI_Info info);
H5TEST_DLL char* getenv_all(MPI_Comm comm, int root, const char* name);
#endif

/* Extern global variables */
H5TEST_DLLVAR int TestVerbosity;

#ifdef __cplusplus
}
#endif
#endif
