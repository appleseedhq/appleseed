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

#include "h5diff.h"
#include "ph5diff.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "h5diff_common.h"
#include "h5tools_utils.h"

/* Name of tool */
#define PROGRAMNAME "h5diff"

static void ph5diff_worker(int );

/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose: h5diff/ph5diff main program
 *
 * Return: An  exit status of 0 means no differences were found, 1 means some
 *   differences were found.
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 * Comments:
 *
 * Modifications: July 2004
 *  Introduced the four modes:
 *   Normal mode: print the number of differences found and where they occured
 *   Report mode: print the above plus the differences
 *   Verbose mode: print the above plus a list of objects and warnings
 *   Quiet mode: do not print output
 *
 * November 2004: Leon Arber (larber@uiuc.edu)
 *        Additions that allow h5diff to be run in parallel
 *
 * This function drives the diff process and will do a serial or parallel diff depending
 * on the value of the global variable g_Parallel (default is 0), set to 1 when the program
 * is run as "ph5diff"
 *-------------------------------------------------------------------------
 */

int main(int argc, const char *argv[])
{
    int        nID = 0;
    const char *fname1 = NULL;
    const char *fname2 = NULL;
    const char *objname1  = NULL;
    const char *objname2  = NULL;
    diff_opt_t options;

    h5tools_setprogname(PROGRAMNAME);
    h5tools_setstatus(EXIT_SUCCESS);

    outBuffOffset = 0;
    g_Parallel = 1;

    MPI_Init(&argc, (char***) &argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &nID);
    MPI_Comm_size(MPI_COMM_WORLD, &g_nTasks);

    if(g_nTasks == 1)
    {
        printf("Only 1 task available...doing serial diff\n");

        g_Parallel = 0;

        parse_command_line(argc, argv, &fname1, &fname2, &objname1, &objname2, &options);

        h5diff(fname1, fname2, objname1, objname2, &options);

        print_info(&options);
    }
    /* Parallel h5diff */
    else {

        /* Have the manager process the command-line */
        if(nID == 0)
        {
            parse_command_line(argc, argv, &fname1, &fname2, &objname1, &objname2, &options);

            h5diff(fname1, fname2, objname1, objname2, &options);

            MPI_Barrier(MPI_COMM_WORLD);

            print_info(&options);
            print_manager_output();
        }
        /* All other tasks become workers and wait for assignments. */
        else {
            ph5diff_worker(nID);

            MPI_Barrier(MPI_COMM_WORLD);
        } /* end else */

    } /* end else */

    MPI_Finalize();

    return 0;
}

/*-------------------------------------------------------------------------
 * Function: ph5diff_worker
 *
 * Purpose: worker process of ph5diff
 *
 * Return: none
 *
 * Programmer: Leon Arber
 * Date: January 2005
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
ph5diff_worker(int nID)
{
    struct diff_args args;
    hid_t file1_id, file2_id;
    char    filenames[2][MAX_FILENAME];
    char    out_data[PRINT_DATA_MAX_SIZE] = {0};
    struct diffs_found  diffs;
    int i;
    MPI_Status Status;

    outBuffOffset = 0;

    MPI_Recv(filenames, MAX_FILENAME*2, MPI_CHAR, 0, MPI_ANY_TAG, MPI_COMM_WORLD, &Status);
    if(Status.MPI_TAG == MPI_TAG_PARALLEL)
    {
        /* disable error reporting */
        H5E_BEGIN_TRY
        {
            /* Open the files */
            if ((file1_id = H5Fopen (filenames[0], H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
            {
            printf ("h5diff Task [%d]: <%s>: unable to open file\n", nID, filenames[0]);
            MPI_Abort(MPI_COMM_WORLD, 0);
            }
            if ((file2_id = H5Fopen (filenames[1], H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
            {
            printf ("h5diff Task [%d]: <%s>: unable to open file\n", nID, filenames[1]);
            MPI_Abort(MPI_COMM_WORLD, 0);
            }
            /* enable error reporting */
        }
        H5E_END_TRY;


        while(1)
        {
            MPI_Probe(0, MPI_ANY_TAG, MPI_COMM_WORLD, &Status);

            if(Status.MPI_TAG == MPI_TAG_ARGS)
            {
                /*Recv parameters for diff from manager task */
                MPI_Recv(&args, sizeof(args), MPI_BYTE, 0, MPI_TAG_ARGS, MPI_COMM_WORLD, &Status);
                /*Do the diff */
                diffs.nfound = diff(file1_id, args.name1, file2_id, args.name2, &(args.options), args.type);
                diffs.not_cmp = args.options.not_cmp;

                /*If print buffer has something in it, request print token.*/
                if(outBuffOffset>0)
                {
                    MPI_Send(NULL, 0, MPI_BYTE, 0, MPI_TAG_TOK_REQUEST, MPI_COMM_WORLD);
                    /*Wait for print token. */
                    MPI_Recv(NULL, 0, MPI_BYTE, 0, MPI_TAG_PRINT_TOK, MPI_COMM_WORLD, &Status);

                    /*When get token, send all of our output to the manager task and then return the token */
                    for(i=0; i<outBuffOffset; i+=PRINT_DATA_MAX_SIZE)
                        MPI_Send(outBuff+i, PRINT_DATA_MAX_SIZE, MPI_BYTE, 0, MPI_TAG_PRINT_DATA, MPI_COMM_WORLD);


                    /* An overflow file exists, so we send it's output to the manager too and then delete it */
                    if(overflow_file)
                    {
                        int tmp;

                        memset(out_data, 0, PRINT_DATA_MAX_SIZE);
                        i=0;

                        rewind(overflow_file);
                        while((tmp = getc(overflow_file)) >= 0)
                        {
                            *(out_data + i++) = (char)tmp;
                            if(i==PRINT_DATA_MAX_SIZE)
                            {
                                MPI_Send(out_data, PRINT_DATA_MAX_SIZE, MPI_BYTE, 0, MPI_TAG_PRINT_DATA, MPI_COMM_WORLD);
                                i=0;
                                memset(out_data, 0, PRINT_DATA_MAX_SIZE);
                            }
                        }

                        if(i>0)
                            MPI_Send(out_data, PRINT_DATA_MAX_SIZE, MPI_BYTE, 0, MPI_TAG_PRINT_DATA, MPI_COMM_WORLD);

                        fclose(overflow_file);
                        overflow_file = NULL;
                    }

                    fflush(stdout);
                    memset(outBuff, 0, OUTBUFF_SIZE);
                    outBuffOffset = 0;

                    MPI_Send(&diffs, sizeof(diffs), MPI_BYTE, 0, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD);
                }
                else
                    MPI_Send(&diffs, sizeof(diffs), MPI_BYTE, 0, MPI_TAG_DONE, MPI_COMM_WORLD);
            }
            else if(Status.MPI_TAG == MPI_TAG_END)
            {
                MPI_Recv(NULL, 0, MPI_BYTE, 0, MPI_TAG_END, MPI_COMM_WORLD, &Status);
            /*  printf("exiting..., task: %d\n", nID);
                fflush(stdout);*/
                break;
            }
            else
            {
                printf("ph5diff_worker: ERROR: invalid tag (%d) received\n", Status.MPI_TAG);
                MPI_Abort(MPI_COMM_WORLD, 0);
            }

        }
    }
}

