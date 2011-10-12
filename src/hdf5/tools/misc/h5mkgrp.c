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


#include "H5private.h"
#include "h5tools.h"
#include "h5tools_utils.h"
#include <string.h>
#include <stdlib.h>

/* Name of tool */
#define PROGRAMNAME "h5mkgrp"

/* Exit status for tools library routines */
int d_status = EXIT_SUCCESS;

/* command-line options: short and long-named parameters */
static const char *s_opts = "hlpvV";
static struct long_options l_opts[] = {
    { "help", no_arg, 'h' },
    { "latest", no_arg, 'l' },
    { "parents", no_arg, 'p' },
    { "verbose", no_arg, 'v' },
    { "version", no_arg, 'V' },
    { NULL, 0, '\0' }
};

/* Command line parameter settings */
typedef struct {
    char *fname;                /* File name to operate on */
    hbool_t latest;             /* Whether file should use latest format versions */
    hbool_t verbose;            /* Whether output should be verbose */
    hbool_t parents;            /* Whether to create intermediate groups */
    size_t ngroups;             /* Number of groups to create */
    char **groups;              /* Pointer to array of group names */
} param_t;


/*-------------------------------------------------------------------------
 * Function:    leave
 *
 * Purpose:     Shutdown MPI and/or HDF5 and call exit()
 *
 * Return:      Does not return
 *
 * Programmer: Quincey Koziol, 2/13/2007
 *
 *-------------------------------------------------------------------------
 */
static void
leave(int ret)
{
    h5tools_close();
    exit(ret);
} /* end leave() */


/*-------------------------------------------------------------------------
 * Function: usage
 *
 * Purpose: Prints a usage message on stderr and then returns.
 *
 * Return: void
 *
 * Programmer: Quincey Koziol, 2/13/2007
 *
 *-------------------------------------------------------------------------
 */
static void
usage(void)
{
    fprintf(stdout, "\
usage: h5mkgrp [OPTIONS] FILE GROUP...\n\
   OPTIONS\n\
      -h, --help         Print a usage message and exit\n\
      -l, --latest       Use latest version of file format to create groups\n\
      -p, --parents      No error if existing, make parent groups as needed\n\
      -v, --verbose      Print information about OBJECTS and OPTIONS\n\
      -V, --version      Print version number and exit\n");
} /* end usage() */


/*-------------------------------------------------------------------------
 * Function: parse_command_line
 *
 * Purpose: Parses command line and sets up global variable to control output
 *
 * Return:	Success: 0
 *		Failure: -1
 *
 * Programmer: Quincey Koziol, 2/13/2007
 *
 *-------------------------------------------------------------------------
 */
static int
parse_command_line(int argc, const char *argv[], param_t *params)
{
    int opt;            /* Option from command line */
    size_t curr_group;  /* Current group name to copy */

    /* Check for empty command line */
    if(argc == 1) {
        usage();
        leave(EXIT_SUCCESS);
    } /* end if */

    /* Parse command line options */
    while((opt = get_option(argc, argv, s_opts, l_opts)) != EOF) {
        switch((char)opt) {
            /* Display 'help' */
            case 'h':
                usage();
                leave(EXIT_SUCCESS);

            /* Create objects with the latest version of the format */
            case 'l':
                params->latest = TRUE;
                break;

            /* Create parent groups */
            case 'p':
                params->parents = TRUE;
                break;

            /* Verbose output */
            case 'v':
                params->verbose = TRUE;
                break;

            /* Display version */
            case 'V':
                print_version(h5tools_getprogname());
                leave(EXIT_SUCCESS);

            /* Bad command line argument */
            default:
                usage();
                leave(EXIT_FAILURE);
        } /* end switch */
    } /* end while */

    /* Check for file name to be processed */
    if(argc <= opt_ind) {
        error_msg("missing file name\n");
        usage();
        leave(EXIT_FAILURE);
    } /* end if */

    /* Retrieve file name */
    params->fname = HDstrdup(argv[opt_ind]);
    opt_ind++;

    /* Check for group(s) to be created */
    if(argc <= opt_ind) {
        error_msg("missing group name(s)\n");
        usage();
        leave(EXIT_FAILURE);
    } /* end if */

    /* Allocate space for the group name pointers */
    params->ngroups = (argc - opt_ind);
    params->groups = HDmalloc(params->ngroups * sizeof(char *));

    /* Retrieve the group names */
    curr_group = 0;
    while(opt_ind < argc) {
        params->groups[curr_group] = HDstrdup(argv[opt_ind]);
        curr_group++;
        opt_ind++;
    } /* end while */

#ifdef QAK
HDfprintf(stderr, "params->parents = %t\n", params->parents);
HDfprintf(stderr, "params->verbose = %t\n", params->verbose);
HDfprintf(stderr, "params->fname = '%s'\n", params->fname);
HDfprintf(stderr, "params->ngroups = %Zu\n", params->ngroups);
for(curr_group = 0; curr_group < params->ngroups; curr_group++)
    HDfprintf(stderr, "params->group[%Zu] = '%s'\n", curr_group, params->groups[curr_group]);
#endif /* QAK */

    return(0);
} /* parse_command_line() */


/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose: Create group(s) in an HDF5 file
 *
 * Programmer: Quincey Koziol, 2/13/2007
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, const char *argv[])
{
    param_t params;             /* Command line parameter settings */
    hid_t fid;                  /* HDF5 file ID */
    hid_t fapl_id;              /* File access property list ID */
    hid_t lcpl_id;              /* Link creation property list ID */
    size_t curr_group;          /* Current group to create */

    h5tools_setprogname(PROGRAMNAME);
    h5tools_setstatus(EXIT_SUCCESS);

    /* Disable the HDF5 library's error reporting */
    H5Eset_auto2(H5E_DEFAULT, NULL, NULL);

    /* Initialize h5tools lib */
    h5tools_init();

    /* Parse command line */
    HDmemset(&params, 0, sizeof(params));
    if(parse_command_line(argc, argv, &params) < 0) {
        error_msg("unable to parse command line arguments\n");
        leave(EXIT_FAILURE);
    } /* end if */

    /* Create file access property list */
    if((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0) {
        error_msg("Could not create file access property list\n");
        leave(EXIT_FAILURE);
    } /* end if */

    /* Check for creating groups with new format version */
    if(params.latest) {
        /* Set the "use the latest version of the format" bounds */
        if(H5Pset_libver_bounds(fapl_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0) {
            error_msg("Could not set property for using latest version of the format\n");
            leave(EXIT_FAILURE);
        } /* end if */

        /* Display some output if requested */
        if(params.verbose)
            printf("%s: Creating groups with latest version of the format\n", h5tools_getprogname());
    } /* end if */

    /* Attempt to open an existing HDF5 file first */
    fid = h5tools_fopen(params.fname, H5F_ACC_RDWR, fapl_id, NULL, NULL, 0);

    /* If we couldn't open an existing file, try creating file */
    /* (use "EXCL" instead of "TRUNC", so we don't blow away existing non-HDF5 file) */
    if(fid < 0)
        fid = H5Fcreate(params.fname, H5F_ACC_EXCL, H5P_DEFAULT, fapl_id);

    /* Test for error in opening file */
    if(fid < 0) {
        error_msg("Could not open output file '%s'\n", params.fname);
        leave(EXIT_FAILURE);
    } /* end if */

    /* Create link creation property list */
    if((lcpl_id = H5Pcreate(H5P_LINK_CREATE)) < 0) {
        error_msg("Could not create link creation property list\n");
        leave(EXIT_FAILURE);
    } /* end if */

    /* Check for creating intermediate groups */
    if(params.parents) {
        /* Set the intermediate group creation property */
        if(H5Pset_create_intermediate_group(lcpl_id, TRUE) < 0) {
            error_msg("Could not set property for creating parent groups\n");
            leave(EXIT_FAILURE);
        } /* end if */

        /* Display some output if requested */
        if(params.verbose)
            printf("%s: Creating parent groups\n", h5tools_getprogname());
    } /* end if */

    /* Loop over creating requested groups */
    for(curr_group = 0; curr_group < params.ngroups; curr_group++) {
        hid_t gid;              /* Group ID */

        /* Attempt to create a group */
        if((gid = H5Gcreate2(fid, params.groups[curr_group], lcpl_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
            error_msg("Could not create group '%s'\n", params.groups[curr_group]);
            leave(EXIT_FAILURE);
        } /* end if */

        /* Close the group */
        if(H5Gclose(gid) < 0) {
            error_msg("Could not close group '%s'??\n", params.groups[curr_group]);
            leave(EXIT_FAILURE);
        } /* end if */

        /* Display some output if requested */
        if(params.verbose)
            printf("%s: created group '%s'\n", h5tools_getprogname(), params.groups[curr_group]);
    } /* end for */

    /* Close link creation property list */
    if(H5Pclose(lcpl_id) < 0) {
        error_msg("Could not close link creation property list\n");
        leave(EXIT_FAILURE);
    } /* end if */

    /* Close file */
    if(H5Fclose(fid) < 0) {
        error_msg("Could not close output file '%s'??\n", params.fname);
        leave(EXIT_FAILURE);
    } /* end if */

    /* Close file access property list */
    if(H5Pclose(fapl_id) < 0) {
        error_msg("Could not close file access property list\n");
        leave(EXIT_FAILURE);
    } /* end if */

    /* Shut down h5tools lib */
    h5tools_close();

    return EXIT_SUCCESS;
} /* end main() */

