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
 * FILE
 * ttsafe.c - HDF5 threadsafe testing framework main file.
 *
 * REMARKS
 * General test wrapper for HDF5 library thread safety test programs
 *
 * DESIGN
 * Each test function should be implemented as function having no
 * parameters and returning void (i.e. no return value).  They should be put
 * into the list of InitTest() calls in main() below.  Functions which depend
 * on other functionality should be placed below the InitTest() call for the
 * base functionality testing.
 * Each test module should include ttsafe.h and define a unique set of
 * names for test files they create.
 *
 * BUGS/LIMITATIONS
 *
 * EXPORTED ROUTINES/VARIABLES:
 *
 */

#include "h5test.h"

/* ANY new test needs to have a prototype in ttsafe.h */
#include "ttsafe.h"

#ifndef H5_HAVE_THREADSAFE
int main(void)
{
    printf("Test skipped because THREADSAFE not enabled\n");
    return 0;
}
#else

#define MAX_NUM_NAME 1000
#define NAME_OFFSET 6         /* offset for "name<num>" */

/* pre-condition: num must be a non-negative number */
static int num_digits(int num)
{
	int i;

	if (num == 0)
		return 1;

	for (i = 0; num > 0; i++)
		num = num / 10;

	return i;
}

/* Routine to generate attribute names for numeric values */
char *gen_name(int value)
{
	char *temp;
	int i, length;

	length = num_digits(MAX_NUM_NAME - 1);
	temp = (char *)HDmalloc((NAME_OFFSET + length + 1) * sizeof(char));
	temp = HDstrcpy(temp, "attrib");
	temp[NAME_OFFSET + length] = '\0';

	for (i = length - 1; i >= 0; i--) {
		temp[NAME_OFFSET + i] = (char)((int)'0' + value % 10);
		value = value / 10;
	}

	return temp;
}

int main(int argc, char *argv[])
{
        /* Initialize testing framework */
        TestInit(argv[0], NULL, NULL);

        /* Tests are generally arranged from least to most complexity... */
        AddTest("dcreate", tts_dcreate, cleanup_dcreate, "multi-dataset creation", NULL);
        AddTest("error", tts_error, cleanup_error, "per-thread error stacks", NULL);
#ifdef H5_HAVE_PTHREAD_H    
        /* Thread cancellability only supported with pthreads ... */
        AddTest("cancel", tts_cancel, cleanup_cancel, "thread cancellation safety test", NULL);
#endif /* H5_HAVE_PTHREAD_H */
        AddTest("acreate", tts_acreate, cleanup_acreate, "multi-attribute creation", NULL);

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

	return GetTestNumErrs();
} /* end main() */
#endif  /*H5_HAVE_THREADSAFE*/
