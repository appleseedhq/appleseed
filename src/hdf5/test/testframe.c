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
 * Programmer:  Quincey Koziol <koziol@ncsa.uiuc.edu>
 *              Tuesday, January  6, 2004
 *
 * Purpose:	Provides support functions for the testing framework.
 *
 */

#include "testhdf5.h"

/*
 * Definitions for the testing structure.
 */
#define MAXNUMOFTESTS   50
#define MAXTESTNAME     16
#define MAXTESTDESC     64

typedef struct TestStruct {
	int    NumErrors;
	char   Description[MAXTESTDESC];
	int    SkipFlag;
	char   Name[MAXTESTNAME];
	void (*Call)(void);
	void (*Cleanup)(void);
	const void *Parameters;
} TestStruct;


/*
 * Variables used by testing framework.
 */
static int num_errs = 0;        /* Total number of errors during testing */
int TestVerbosity = VERBO_DEF;       /* Default Verbosity is Low */
static int Summary = 0;		/* Show test summary. Default is no. */
static int CleanUp = 1;		/* Do cleanup or not. Default is yes. */
static int TestExpress = -1;	/* Do TestExpress or not. -1 means not set yet. */
static H5E_auto_t	*PrintErrorStackFunc;
static void		**PrintErrorStackData;
static TestStruct Test[MAXNUMOFTESTS];
static int    Index = 0;
static const void *Test_parameters = NULL;
static const char *TestProgName = NULL;
static void (*TestPrivateUsage)(void) = NULL;
static int (*TestPrivateParser)(int ac, char *av[]) = NULL;


/*
 * Setup a test function and add it to the list of tests.
 *      It must have no parameters and returns void.
 * TheName--short test name.
 *    If the name starts with '-', do not run it by default.
 * TheCall--the test routine.
 * Cleanup--the cleanup routine for the test.
 * TheDescr--Long description of the test.
 * Parameters--pointer to extra parameters. Use NULL if none used.
 *    Since only the pointer is copied, the contents should not change.
 */
void
AddTest(const char *TheName, void (*TheCall) (void), void (*Cleanup) (void), const char *TheDescr, const void *Parameters)
{
    /* Sanity checking */
    if (Index >= MAXNUMOFTESTS) {
        printf("Too many tests added, increase MAXNUMOFTEST(%d).\n",
		MAXNUMOFTESTS);
        exit(-1);
    }                           /* end if */
    if (HDstrlen(TheDescr) >= MAXTESTDESC) {
        printf("Test description too long, increase MAXTESTDESC(%d).\n",
		MAXTESTDESC);
        exit(-1);
    } /* end if */
    if (HDstrlen(TheName) >= MAXTESTNAME) {
        printf("Test name too long, increase MAXTESTNAME(%d).\n",
		MAXTESTNAME);
        exit(-1);
    } /* end if */

    /* Set up test function */
    HDstrcpy(Test[Index].Description, TheDescr);
    if (*TheName != '-'){
	HDstrcpy(Test[Index].Name, TheName);
	Test[Index].SkipFlag = 0;
    }
    else {	/* skip test by default */
	HDstrcpy(Test[Index].Name, TheName+1);
	Test[Index].SkipFlag = 1;
    }
    Test[Index].Call = TheCall;
    Test[Index].Cleanup = Cleanup;
    Test[Index].NumErrors = -1;
    Test[Index].Parameters = Parameters;

    /* Increment test count */
    Index++;
}


/*
 * Initialize testing framework
 *
 * ProgName: Name of test program.
 * private_usage: Optional routine provided by test program to print the
 *      private portion of usage page.  Default to NULL which means none is
 *      provided.
 * private_parser: Optional routine provided by test program to parse the
 *      private options.  Default to NULL which means none is provided.
 *
 * Modifications:
 *     	Albert Cheng 2004/08/17
 *     	Added the ProgName, private_usage and private_parser arguments.
 */
void TestInit(const char *ProgName, void (*private_usage)(void), int (*private_parser)(int ac, char *av[]))
{
#if !(defined MAC || defined SYMANTEC_C)
    /* Un-buffer the stdout and stderr */
    setbuf(stderr, NULL);
    setbuf(stdout, NULL);
#endif

    /* Save error printing settings */
    H5Eget_auto2(H5E_DEFAULT, PrintErrorStackFunc, PrintErrorStackData);
    /*
     * Turn off automatic error reporting since we do it ourselves.  Besides,
     * half the functions this test calls are private, so automatic error
     * reporting wouldn't do much good since it's triggered at the API layer.
     */
    PrintErrorStackOff();

    /*
     * Record the program name and private routines if provided.
     */
    TestProgName = ProgName;
    if (NULL != private_usage)
	TestPrivateUsage = private_usage;
    if (NULL != private_parser)
	TestPrivateParser = private_parser;
}


/*
 * Print test usage.
 *	First print the common test options, then the extra options if provided.
 *
 * Modification:
 * 	2004/08/18 Albert Cheng.  Add TestPrivateUsage feature.
 */
void TestUsage(void)
{
	int i;

	print_func("Usage: %s [-v[erbose] (l[ow]|m[edium]|h[igh]|0-9)] %s\n",
	    TestProgName, (TestPrivateUsage ? "<extra options>" : ""));
	print_func("              [-[e]x[clude] name+] \n");
	print_func("              [-o[nly] name+] \n");
	print_func("              [-b[egin] name] \n");
	print_func("              [-s[ummary]]  \n");
	print_func("              [-c[leanoff]]  \n");
	print_func("              [-h[elp]]  \n");
	print_func("\n\n");
	print_func("verbose   controls the amount of information displayed\n");
	print_func("exclude   to exclude tests by name\n");
	print_func("only      to name tests which should be run\n");
	print_func("begin     start at the name of the test givin\n");
	print_func("summary   prints a summary of test results at the end\n");
	print_func("cleanoff  does not delete *.hdf files after execution of tests\n");
	print_func("help      print out this information\n");
	if (TestPrivateUsage){
	    print_func("\nExtra options\n");
	    TestPrivateUsage();
	}
	print_func("\n\n");
	print_func("This program currently tests the following: \n\n");
	print_func("%16s %s\n", "Name", "Description");
	print_func("%16s %s\n", "----", "-----------");

	for (i = 0; i < Index; i++)
		print_func("%16s %s\n", Test[i].Name, Test[i].Description);

	print_func("\n\n");
}


/*
 * Print test info.
 */
void TestInfo(const char *ProgName)
{
    unsigned major, minor, release;

    H5get_libversion(&major, &minor, &release);

    print_func("\nFor help use: %s -help\n",ProgName);
    print_func("Linked with hdf5 version %u.%u release %u\n", major, minor, release);
}


/*
 * Parse command line information.
 *      argc, argv: the usual command line argument count and strings
 *
 * Modification:
 * 	2004/08/18 Albert Cheng.  Add extra_parse feature.
 */
void TestParseCmdLine(int argc, char *argv[])
{
    int ret_code;

    while (argv++, --argc > 0){
	if ((HDstrcmp(*argv, "-verbose") == 0) ||
				(HDstrcmp(*argv, "-v") == 0)) {
	    if (argc > 0){
		--argc; ++argv;
		ParseTestVerbosity(*argv);
	    }else{
		TestUsage();
		exit(1);
	    }
	}
	else if (((HDstrcmp(*argv, "-exclude") == 0) ||
				    (HDstrcmp(*argv, "-x") == 0))) {
	    if (argc > 0){
		--argc; ++argv;
		SetTest(*argv, SKIPTEST);
	    }else{
		TestUsage();
		exit(1);
	    }
	}
	else if (((HDstrcmp(*argv, "-begin") == 0) ||
				    (HDstrcmp(*argv, "-b") == 0))) {
	    if (argc > 0){
		--argc; ++argv;
		SetTest(*argv, BEGINTEST);
	    }else{
		TestUsage();
		exit(1);
	    }
	}
	else if (((HDstrcmp(*argv, "-only") == 0) ||
				    (HDstrcmp(*argv, "-o") == 0))) {
	    if (argc > 0){
		int Loop;
		--argc; ++argv;
		/* Skip all tests, then activate only one. */
		for (Loop = 0; Loop < Index; Loop++)
		    Test[Loop].SkipFlag = 1;
		SetTest(*argv, ONLYTEST);
	    }else{
		TestUsage();
		exit(1);
	    }
	}
	else if ((HDstrcmp(*argv, "-summary") == 0) || (HDstrcmp(*argv, "-s") == 0))
            Summary = 1;
	else if ((HDstrcmp(*argv, "-help") == 0) || (HDstrcmp(*argv, "-h") == 0)) {
            TestUsage();
            exit(0);
        }
	else if ((HDstrcmp(*argv, "-cleanoff") == 0) || (HDstrcmp(*argv, "-c") == 0))
	    SetTestNoCleanup();
	else {
	    /* non-standard option.  Break out. */
	    break;
	}

    }

    /* Call extra parsing function if provided. */
    if (NULL != TestPrivateParser){
	ret_code=TestPrivateParser(argc+1, argv-1);
	if (ret_code != 0)
	    exit(-1);
    }
}


/*
 * Perform Tests.
 */
void PerformTests(void)
{
    int                     Loop;

    for (Loop = 0; Loop < Index; Loop++)
        if (Test[Loop].SkipFlag) {
            MESSAGE(2, ("Skipping -- %s (%s) \n", Test[Loop].Description, Test[Loop].Name));
        } else {
            MESSAGE(2, ("Testing  -- %s (%s) \n", Test[Loop].Description, Test[Loop].Name));
            MESSAGE(5, ("===============================================\n"));
            Test[Loop].NumErrors = num_errs;
	    Test_parameters = Test[Loop].Parameters;
	    ALARM_ON;
            Test[Loop].Call();
	    ALARM_OFF;
            Test[Loop].NumErrors = num_errs - Test[Loop].NumErrors;
            MESSAGE(5, ("===============================================\n"));
            MESSAGE(5, ("There were %d errors detected.\n\n", (int)Test[Loop].NumErrors));
        }

    Test_parameters = NULL;     /* clear it. */
    MESSAGE(2, ("\n\n"))

    if (num_errs)
        print_func("!!! %d Error(s) were detected !!!\n\n", (int) num_errs);
    else
        print_func("All tests were successful. \n\n");
}


/*
 * Display test summary.
 */
void TestSummary(void)
{
    int                     Loop;

    print_func("Summary of Test Results:\n");
    print_func("Name of Test     Errors Description of Test\n");
    print_func("---------------- ------ --------------------------------------\n");

    for (Loop = 0; Loop < Index; Loop++) {
        if (Test[Loop].NumErrors == -1)
            print_func("%16s %6s %s\n", Test[Loop].Name, "N/A", Test[Loop].Description);
        else
            print_func("%16s %6d %s\n", Test[Loop].Name, (int)Test[Loop].NumErrors, Test[Loop].Description);
    }

    print_func("\n\n");
}


/*
 * Cleanup files from testing
 */
void TestCleanup(void)
{
    int                     Loop;

    MESSAGE(2, ("\nCleaning Up temp files...\n\n"));

    /* call individual cleanup routines in each source module */
    for (Loop = 0; Loop < Index; Loop++)
        if (!Test[Loop].SkipFlag && Test[Loop].Cleanup!=NULL)
            Test[Loop].Cleanup();
}


/*
 * Retrieve the verbosity level for the testing framework
 */
int GetTestVerbosity(void)
{
    return(TestVerbosity);
}

/*
 * Set the verbosity level for the testing framework.
 * Return previous verbosity level.
 */
int SetTestVerbosity(int newval)
{
    int oldval;

    oldval = TestVerbosity;
    TestVerbosity = newval;
    return(oldval);
}

/*
 * Retrieve the TestExpress mode for the testing framework
 Values:
 0: Exhaustive run
    Tests should take as long as necessary
 1: Full run.  Default if HDF5TestExpress is not defined
    Tests should take no more than 30 minutes
 2: Quick run
    Tests should take no more than 10 minutes
 3: Smoke test.  Default if HDF5TestExpress is set to a value other than 0-3
    Tests should take less than 1 minute

 Design:
 If the environment variable $HDF5TestExpress is defined,
 then test programs should skip some tests so that they
 complete sooner.

 Terms:
 A "test" is a single executable, even if it contains multiple
 sub-tests.
 The standard system for test times is a Linux machine running in
 NFS space (to catch tests that involve a great deal of disk I/O).

 Implementation:
 I think this can be easily implemented in the test library (libh5test.a)
 so that all tests can just call it to check the status of $HDF5TestExpress.
 */
int GetTestExpress(void)
{
    char * env_val;

    /* set it here for now.  Should be done in something like h5test_init(). */
    if(TestExpress==-1)
    {
       env_val = getenv("HDF5TestExpress");

       if(env_val == NULL)
         SetTestExpress(1);
       else if(strcmp(env_val, "0") == 0)
         SetTestExpress(0);
       else if(strcmp(env_val, "1") == 0)
         SetTestExpress(1);
       else if(strcmp(env_val, "2") == 0)
         SetTestExpress(2);
       else
         SetTestExpress(3);
    }

    return(TestExpress);
}

/*
 * Set the TestExpress mode for the testing framework.
 * Return previous TestExpress mode.
 * Values: non-zero means TestExpress mode is on, 0 means off.
 */
int SetTestExpress(int newval)
{
    int oldval;

    oldval = TestExpress;
    TestExpress = newval;
    return(oldval);
}

/*
 * Retrieve Summary request value.
 *     0 means no summary, 1 means yes.
 */
int GetTestSummary(void)
{
    return(Summary);
}

/*
 * Retrieve Cleanup request value.
 *     0 means no Cleanup, 1 means yes.
 */
int GetTestCleanup(void)
{
    return(CleanUp);
}

/*
 * Set cleanup to no.
 * Return previous cleanup value.
 */
int SetTestNoCleanup(void)
{
    int oldval;

    oldval = CleanUp;
    CleanUp = 0;
    return(oldval);
}

/*
 * Parse an argument string for verbosity level and set it.
 */
void ParseTestVerbosity(char *argv)
{
    if (*argv == 'l')
	SetTestVerbosity(VERBO_LO);
    else if (*argv == 'm')
	SetTestVerbosity(VERBO_MED);
    else if (*argv == 'h')
	SetTestVerbosity(VERBO_HI);
    else
	SetTestVerbosity(atoi(argv));
}


/*
 * Retrieve the number of testing errors for the testing framework
 */
int GetTestNumErrs(void)
{
    return(num_errs);
}


/*
 * Increment the number of testing errors
 */
void IncTestNumErrs(void)
{
    num_errs++;
}


/*
 * Retrieve the current Test Parameters pointer.
 */
const void *GetTestParameters(void)
{
    return(Test_parameters);
}


/*
 * This routine is designed to provide equivalent functionality to 'printf'
 * and also increment the error count for the testing framework.
 */
int
TestErrPrintf(const char *format, ...)
{
    va_list arglist;
    int ret_value;

    /* Increment the error count */
    num_errs++;

    /* Print the requested information */
    va_start(arglist, format);
    ret_value = vprintf(format, arglist);
    va_end(arglist);

    /* Return the length of the string produced (like printf() does) */
    return ret_value;
}


/*
 * Set (control) which test will be tested.
 * SKIPTEST: skip this test
 * ONLYTEST: do only this test
 * BEGINETEST: skip all tests before this test
 *
 */
void SetTest(const char *testname, int action)
{
    int Loop;
    switch (action){
	case SKIPTEST:
	    for (Loop = 0; Loop < Index; Loop++)
		if (HDstrcmp(testname, Test[Loop].Name) == 0){
		    Test[Loop].SkipFlag = 1;
		    break;
		}
	    break;
	case BEGINTEST:
	    for (Loop = 0; Loop < Index; Loop++) {
		if (HDstrcmp(testname, Test[Loop].Name) != 0)
		    Test[Loop].SkipFlag = 1;
		else{
		    /* Found it. Set it to run.  Done. */
		    Test[Loop].SkipFlag = 0;
		    break;
		}
	    }
	    break;
	case ONLYTEST:
	    for (Loop = 0; Loop < Index; Loop++) {
		if (HDstrcmp(testname, Test[Loop].Name) != 0)
		    Test[Loop].SkipFlag = 1;
		else {
		    /* Found it. Set it to run. Break to skip the rest. */
		    Test[Loop].SkipFlag = 0;
		    break;
		}
	    }
	    /* skip the rest */
	    while (++Loop < Index)
		Test[Loop].SkipFlag = 1;
	    break;
	default:
	    /* error */
	    printf("*** ERROR: Unknown action (%d) for SetTest\n", action);
	    break;
    }
}


/*
 * Enable alarm on test execution, configurable by environment variable
 */
void TestAlarmOn(void)
{
    char * env_val = HDgetenv("HDF5_ALARM_SECONDS");    /* Alarm environment */
    unsigned long alarm_sec = H5_ALARM_SEC;     /* Number of seconds before alarm goes off */

    /* Get the alarm value from the environment variable, if set */
    if(env_val != NULL)
        alarm_sec = (unsigned)HDstrtoul(env_val, (char **)NULL, 10);

    /* Set the number of seconds before alarm goes off */
    HDalarm((unsigned)alarm_sec);
}


/*
 * Enable error stack printing when errors occur.
 */
void PrintErrorStackOn(void)
{
    H5Eset_auto2(H5E_DEFAULT, PrintErrorStackFunc, PrintErrorStackData);
}

/*
 * Disable error stack printing when errors occur.
 */
void PrintErrorStackOff(void)
{
    H5Eset_auto2(H5E_DEFAULT, NULL, NULL);
}
