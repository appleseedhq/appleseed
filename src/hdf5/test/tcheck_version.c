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
 * This tests the h5check_version() function.
 *
 * The default is to call the h5check_version() with the version information
 * in the header file and should incur no warnings or abort.
 * Options provided to call it with incorrect versions to test
 * if it will indeed issue the warning message and aborts.  With environment
 * variable $HDF5_DISABLE_VERSION_CHECK sets to 1, it should issue warnings
 * but no abort.  If it is 2, no warning or abort.
 *
 * Programmer: Albert Cheng
 *             September 20, 2009
 * Modifications:
 *   Added abort signal intercept. AKC - 2009/10/16 -
 */

#include "h5test.h"

#define	progname	"tcheck_version"

/* prototypes */
void showhelp(void);
void parse(int ac, char **av);
void abort_intercept (int UNUSED sig);

/* global variables */
unsigned	major = H5_VERS_MAJOR;
unsigned	minor = H5_VERS_MINOR;
unsigned	release = H5_VERS_RELEASE;

void
showhelp(void)
{
    printf("Usage: " progname " [-h] [-t<vers>]\n");
    printf("\t-h\tShow this page and version information\n");
    printf("\t-t<vers>: Test by changing (adding 1 to) the <vers> to trigger\n");
    printf("\t\t  the warning. <vers> can be:\n");
    printf("\t\t\tM for Major version number (%d)\n", H5_VERS_MAJOR);
    printf("\t\t\tm for Minor version number (%d)\n", H5_VERS_MINOR);
    printf("\t\t\tr for Release number (%d)\n", H5_VERS_RELEASE);
}


void
parse(int ac, char **av)
{
    char *pt;

    while (--ac > 0){
	pt = *(++av);
	if (*pt != '-') {
	    fprintf(stderr, "Unknown option(%s). Aborted.\n", *av);
	    exit(1);
	}else{
	    switch(*(++pt)) {
		case 't': 	/* option -t */
		    switch(*(++pt)) {
			case 'M':
			    major++;
			    break;
			case 'm':
			    minor++;
			    break;
			case 'r':
			    release++;
			    break;
			default:
			    fprintf(stderr, "Unknown -v parameter (%s). Aborted.\n", *av);
			    exit(1);
		    }
		    break;
		case 'h':	/* help page */
		    showhelp();
		    exit(0);
		default:
		    fprintf(stderr, "Unknown option(%s). Aborted.\n", *av);
		    exit(1);
	    }
	}
    }
}

/* Handler for SIGABRT - catch the abort signal supposedly from check_version()
 * and exit(6).  Would have used 134 is the return code in Unix systems
 * but some systems (e.g., poe in AIX interprets exit(134) the same as
 * if the process has really been interrupted by the abort signal and prints
 * extra messages that confuse test script that is looking for matching output.
 * This handles the abort signal instead letting it interrupt the OS because
 * some systems may produce extra messages and/or produce core dump.
 * This tries to eliminate those side effects.
 */
void
abort_intercept (int UNUSED sig)
{
    HDexit(6);
}

int
main(int ac, char **av)
{
    parse(ac, av);
    HDsignal(SIGABRT, &abort_intercept);
    H5check_version(major, minor, release);
    HDsignal(SIGABRT, SIG_DFL);
    return 0;
}
