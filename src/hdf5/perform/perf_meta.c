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
 * Programmer:	Raymond Lu <slu@ncsa.uiuc.edu>
 *		Friday, Oct 3, 2004
 *
 * Purpose:	Tests performance of metadata
 */

#include "h5test.h"

#ifdef H5_HAVE_PARALLEL
#define MAINPROCESS	(!mpi_rank)	/* define process 0 as main process */
#endif /*H5_HAVE_PARALLEL*/

/* File_Access_type bits */
#define FACC_DEFAULT	0x0	/* serial as default */
#define FACC_MPIO	0x1	/* MPIO */
#define FACC_MPIPOSIX   0x8	/* MPIPOSIX */

/* Which test to run */
int RUN_TEST = 0x0;     /* all tests as default */
int TEST_1   = 0x1;     /* Test 1 */
int TEST_2   = 0x2;     /* Test 2 */
int TEST_3   = 0x4;     /* Test 3 */


const char *FILENAME[] = {
    "meta_perf_1",
    "meta_perf_2",
    "meta_perf_3",
    NULL
};

/* Default values for performance. Can be changed through command line options */
int 	NUM_DSETS = 16;
int 	NUM_ATTRS = 8;
int 	BATCH_ATTRS = 2;
hbool_t flush_dset = FALSE;
hbool_t flush_attr = FALSE;
int 	nerrors = 0;			/* errors count */
hid_t	fapl;

/* Data space IDs */
hid_t	space;
hid_t	small_space;

/* Performance data */
typedef struct p_time {
    double total;
    double avg;
    double max;
    double min;
    double start;
    char   func[32];
} p_time;

/*Test file access type for parallel.  MPIO as default */
int facc_type = FACC_DEFAULT;

double  retrieve_time(void);
void    perf(p_time *perf_t, double start_t, double end_t);
void    print_perf(p_time, p_time, p_time);


/*-------------------------------------------------------------------------
 * Function:	parse_options
 *
  Purpose:	Parse command line options
 *
 * Programmer:	Raymond Lu
 *		Friday, Oct 3, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
parse_options(int argc, char **argv)
{
    int t;

    /* Use default values */
    if(argc==1)
	return(0);

    while (--argc){
	if (**(++argv) != '-'){
	    break;
	}else{
	    switch(*(*argv+1)){
                case 'h':   /* Help page */
                            return(1);

		case 'd':   /* Number of datasets */
                            NUM_DSETS = atoi((*argv+1)+1);
			    if (NUM_DSETS < 0){
				nerrors++;
				return(1);
			    }
			    break;

		case 'a':   /* Number of attributes per dataset */
                            NUM_ATTRS = atoi((*argv+1)+1);
			    if (NUM_ATTRS < 0){
				nerrors++;
				return(1);
			    }
			    break;

		case 'n':   /* Number of attributes to be created in batch */
                            BATCH_ATTRS = atoi((*argv+1)+1);
			    if (BATCH_ATTRS < 0){
				nerrors++;
				return(1);
			    }
			    break;

		case 'p':   /* Use the MPI-POSIX driver access */
			    facc_type = FACC_MPIPOSIX;
			    break;

		case 'm':   /* Use the MPI-POSIX driver access */
			    facc_type = FACC_MPIO;
			    break;

                case 'f':   /* Call H5Fflush for each dataset or attribute */
                            if(!strcmp("a", (*argv+2)))
                                flush_attr = TRUE;
                            else if(!strcmp("d", (*argv+2)))
                                flush_dset = TRUE;
                            else {
                                nerrors++;
                                return(1);
                            }
                            break;

		case 't':   /* Which test to run */
                            t = atoi((*argv+1)+1);
			    if (t < 1 || t > 3){
				nerrors++;
				return(1);
			    }
                            if(t == 1)
                                RUN_TEST |= TEST_1;
                            else if(t == 2)
                                RUN_TEST |= TEST_2;
                            else
                                RUN_TEST |= TEST_3;

			    break;

 		default:    nerrors++;
			    return(1);
	    }
	}
    } /*while*/

    /* Check valid values */
#ifndef H5_HAVE_PARALLEL
    if(facc_type == FACC_MPIO || facc_type == FACC_MPIPOSIX)
    {
        nerrors++;
        return(1);
    }
#endif /*H5_HAVE_PARALLEL*/

    if(NUM_ATTRS && !BATCH_ATTRS)
        NUM_ATTRS = 0;

    if(!NUM_ATTRS && BATCH_ATTRS)
        BATCH_ATTRS = 0;

    if(!NUM_DSETS) {
        nerrors++;
        return(1);
    }

    if(NUM_ATTRS && BATCH_ATTRS) {
        if(BATCH_ATTRS > NUM_ATTRS || NUM_ATTRS % BATCH_ATTRS) {
	    nerrors++;
            return(1);
        }
    }

    return(0);
}


/*-------------------------------------------------------------------------
 * Function:	usage
 *
  Purpose:	Prints help page
 *
 * Programmer:	Raymond Lu
 *		Friday, Oct 3, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
usage(void)
{
    printf("Usage: perf_meta [-h] [-m] [-p] [-d<num_datasets>]"
           "[-a<num_attributes>]\n"
           "\t[-n<batch_attributes>] [-f<option>] [-t<test>]\n");
    printf("\t-h"
	"\t\t\thelp page.\n");
    printf("\t-m"
	"\t\t\tset MPIO as the file driver when parallel HDF5\n"
        "\t\t\t\tis enabled.  Either -m or -p has be to \n"
        "\t\t\t\tspecified when running parallel program.\n");
    printf("\t-p"
	"\t\t\tset MPI POSIX as the file driver when parallel \n"
	"\t\t\t\tHDF5 is enabled.  Either -m or -p has be to \n"
        "\t\t\t\tspecified when running parallel program.\n");
    printf("\t-d<num_datasets>"
	"\tset number of datasets for meta data \n"
        "\t\t\t\tperformance test\n");
    printf("\t-a<num_attributes>"
        "\tset number of attributes per dataset for meta \n"
        "\t\t\t\tdata performance test.\n");
    printf("\t-n<batch_attributes>"
	"\tset batch number of attributes for dataset \n"
        "\t\t\t\tfor meta data performance test.\n");
    printf("\t-f<option>"
	"\t\tflush data to disk after closing a dataset \n"
        "\t\t\t\tor attribute.  Valid options are \"d\" for \n"
        "\t\t\t\tdataset, \"a\" for attribute.  Disabled is \n"
        "\t\t\t\tthe default.\n");
    printf("\t-t<tests>"
	"\t\trun specific test.  Give only one number each \n"
        "\t\t\t\ttime. i.e. \"-t1 -t3\" will run test 1 and 3. \n"
        "\t\t\t\tDefault is all three tests.  The 3 tests are: \n\n"
        "\t\t\t\t1. Create <num_attributes> attributes for each \n"
        "\t\t\t\t   of <num_datasets> existing datasets.\n"
        "\t\t\t\t2. Create <num_attributes> attributes for each \n"
        "\t\t\t\t   of <num_datasets> new datasets.\n"
        "\t\t\t\t3. Create <batch_attributes> attributes for \n"
        "\t\t\t\t   each of <num_dataset> new datasets for \n"
        "\t\t\t\t   <num_attributes>/<batch_attributes> times.\n");
}


/*-------------------------------------------------------------------------
 * Function:	create_dspace
 *
 * Purpose:	Attempts to create data space.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		Friday, Oct 3, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
create_dspace(void)
{
    hsize_t	dims[2];
    hsize_t	small_dims[2];

    /* Create the data space */
    dims[0] = 256;
    dims[1] = 512;
    if((space = H5Screate_simple(2, dims, NULL)) < 0)
	    goto error;

    /* Create a small data space for attributes */
    small_dims[0] = 16;
    small_dims[1] = 8;
    if((small_space = H5Screate_simple(2, small_dims, NULL)) < 0)
	    goto error;

    return 0;

error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	create_dsets
 *
 * Purpose:	Attempts to create some datasets.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		Friday, Oct 3, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
create_dsets(hid_t file)
{
    hid_t	dataset;
    char	dset_name[32];
    int		i;

    /*
     * Create a dataset using the default dataset creation properties.
     */
    for(i = 0; i < NUM_DSETS; i++) {
	sprintf(dset_name, "dataset %d", i);
    	if((dataset = H5Dcreate2(file, dset_name, H5T_NATIVE_DOUBLE, space,
                H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto error;

    	if(H5Dclose(dataset) < 0)
            goto error;
    } /* end for */

    return 0;

error:
    return -1;

}


/*-------------------------------------------------------------------------
 * Function:	create_attrs_1
 *
 * Purpose:	Attempts to create all attributes for each existing dataset.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		Friday, Oct 3, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
create_attrs_1(void)
{
    hid_t	file, dataset, attr;
    char	filename[128];
    char	dset_name[64];
    char	attr_name[128];
    int		i, j;
    p_time      attr_t  = {0, 0, 0, 1000000, 0, ""};
    p_time      open_t  = {0, 0, 0, 1000000, 0, "H5Dopen2"};
    p_time      close_t = {0, 0, 0, 1000000, 0, ""};

#ifdef H5_HAVE_PARALLEL
    /* need the rank for printing data */
    int         mpi_rank;
    if(facc_type == FACC_MPIO || facc_type == FACC_MPIPOSIX)
        MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
#endif /*H5_HAVE_PARALLEL*/

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT,
	fapl)) < 0)
	goto error;

    if(create_dsets(file) < 0)
	goto error;

    /*
     * Create all(user specifies the number) attributes for each dataset
     */
    for(i = 0; i < NUM_DSETS; i++) {
	sprintf(dset_name, "dataset %d", i);
        open_t.start = retrieve_time();
	if((dataset = H5Dopen2(file, dset_name, H5P_DEFAULT)) < 0)
		goto error;
	perf(&open_t, open_t.start, retrieve_time());

	for(j = 0; j < NUM_ATTRS; j++) {
            sprintf(attr_name, "all attrs for each dset %d", j);
            attr_t.start = retrieve_time();
            if((attr = H5Acreate2(dataset, attr_name, H5T_NATIVE_DOUBLE,
                    small_space, H5P_DEFAULT, H5P_DEFAULT)) < 0)
                goto error;
            if(H5Aclose(attr) < 0)
                goto error;
            perf(&attr_t, attr_t.start, retrieve_time());
            if(flush_attr && H5Fflush(file, H5F_SCOPE_LOCAL) < 0)
                goto error;
    	} /* end for */

	close_t.start = retrieve_time();
    	if(H5Dclose(dataset) < 0)
            goto error;
	perf(&close_t, close_t.start, retrieve_time());
        if(flush_dset && H5Fflush(file,  H5F_SCOPE_LOCAL) < 0)
            goto error;
    } /* end for */

    if(facc_type == FACC_MPIO || facc_type == FACC_MPIPOSIX) {
#ifdef H5_HAVE_PARALLEL
        MPI_Barrier(MPI_COMM_WORLD);
#endif /*H5_HAVE_PARALLEL*/
    }

#ifdef H5_HAVE_PARALLEL
    if (facc_type == FACC_DEFAULT || (facc_type != FACC_DEFAULT && MAINPROCESS)) /* only process 0 reports */
#endif /*H5_HAVE_PARALLEL*/
    {
        /* Calculate the average time */
        open_t.avg  = open_t.total / NUM_DSETS;
        close_t.avg = close_t.total / NUM_DSETS;
        if(NUM_ATTRS)
            attr_t.avg  = attr_t.total / (NUM_ATTRS*NUM_DSETS);

        /* Print out the performance result */
        fprintf(stderr, "1.  Create %d attributes for each of %d existing datasets\n",
            NUM_ATTRS, NUM_DSETS);
        print_perf(open_t, close_t, attr_t);
    }

    if (H5Fclose(file) < 0) goto error;

    return 0;

error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	create_attrs_2
 *
 * Purpose:	Attempts to create all attributes for each new dataset.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		Friday, Oct 3, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
create_attrs_2(void)
{
    hid_t	file, dataset, attr;
    char	filename[128];
    char	dset_name[64];
    char	attr_name[128];
    int		i, j;
    p_time      attr_t  = {0, 0, 0, 1000000, 0, ""};
    p_time      create_t  = {0, 0, 0, 1000000, 0, "H5Dcreate2"};
    p_time      close_t = {0, 0, 0, 1000000, 0, ""};

#ifdef H5_HAVE_PARALLEL
    /* need the rank for printing data */
    int         mpi_rank;
    if(facc_type == FACC_MPIO || facc_type == FACC_MPIPOSIX)
        MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
#endif /*H5_HAVE_PARALLEL*/

    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);

    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
	goto error;

    /*
     * Create all(user specifies the number) attributes for each new dataset
     */
    for(i = 0; i < NUM_DSETS; i++) {
	sprintf(dset_name, "dataset %d", i);
        create_t.start = retrieve_time();
   	if((dataset = H5Dcreate2(file, dset_name, H5T_NATIVE_DOUBLE,
                space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto error;
	perf(&create_t, create_t.start, retrieve_time());

	for(j = 0; j < NUM_ATTRS; j++) {
            sprintf(attr_name, "all attrs for each dset %d", j);
            attr_t.start = retrieve_time();
            if((attr = H5Acreate2(dataset, attr_name, H5T_NATIVE_DOUBLE,
                    small_space, H5P_DEFAULT, H5P_DEFAULT)) < 0)
                goto error;
            if(H5Aclose(attr) < 0)
                goto error;
            perf(&attr_t, attr_t.start, retrieve_time());
            if(flush_attr && H5Fflush(file,  H5F_SCOPE_LOCAL) < 0)
                goto error;
	} /* end for */

	close_t.start = retrieve_time();
    	if(H5Dclose(dataset) < 0)
            goto error;
	perf(&close_t, close_t.start, retrieve_time());
        if(flush_dset && H5Fflush(file,  H5F_SCOPE_LOCAL) < 0)
            goto error;
    } /* end for */

    if(facc_type == FACC_MPIO || facc_type == FACC_MPIPOSIX) {
#ifdef H5_HAVE_PARALLEL
    MPI_Barrier(MPI_COMM_WORLD);
#endif /*H5_HAVE_PARALLEL*/
    }

#ifdef H5_HAVE_PARALLEL
    /* only process 0 reports if parallel */
    if (facc_type == FACC_DEFAULT || (facc_type != FACC_DEFAULT && MAINPROCESS))
#endif /*H5_HAVE_PARALLEL*/
    {
        /* Calculate the average time */
        create_t.avg = create_t.total / NUM_DSETS;
        close_t.avg  = close_t.total / NUM_DSETS;
        if(NUM_ATTRS)
            attr_t.avg = attr_t.total / (NUM_ATTRS*NUM_DSETS);

        /* Print out the performance result */
        fprintf(stderr, "2.  Create %d attributes for each of %d new datasets\n",
            NUM_ATTRS, NUM_DSETS);
        print_perf(create_t, close_t, attr_t);
    }

    if (H5Fclose(file) < 0) goto error;

    return 0;

error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	create_attrs_3
 *
 * Purpose:	Attempts to create some attributes for each dataset in a
 * 		loop.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		Friday, Oct 3, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
create_attrs_3(void)
{
    hid_t	file, dataset, attr;
    char	filename[128];
    char	dset_name[64];
    char	attr_name[128];
    int		loop_num;
    int		i, j, k;
    p_time      attr_t  = {0, 0, 0, 1000000, 0, ""};
    p_time      open_t  = {0, 0, 0, 1000000, 0, "H5Dopen2"};
    p_time      close_t = {0, 0, 0, 1000000, 0, ""};

#ifdef H5_HAVE_PARALLEL
    /* need the rank for printing data */
    int         mpi_rank;
    if(facc_type == FACC_MPIO || facc_type == FACC_MPIPOSIX)
        MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
#endif /*H5_HAVE_PARALLEL*/

    h5_fixname(FILENAME[2], fapl, filename, sizeof filename);

    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT,
	fapl)) < 0)
	goto error;

    if(create_dsets(file) < 0)
	goto error;

    /*
     * Create some(user specifies the number) attributes for each dataset
     * in a loop
     */
    loop_num = NUM_ATTRS/BATCH_ATTRS;

    for(i = 0; i < loop_num; i++) {
    	for(j = 0; j < NUM_DSETS; j++) {
            sprintf(dset_name, "dataset %d", j);
            open_t.start = retrieve_time();
            if((dataset = H5Dopen2(file, dset_name, H5P_DEFAULT)) < 0)
                goto error;
            perf(&open_t, open_t.start, retrieve_time());

            for(k = 0; k < BATCH_ATTRS; k++) {
                sprintf(attr_name, "some attrs for each dset %d %d", i, k);
                attr_t.start = retrieve_time();
                if((attr = H5Acreate2(dataset, attr_name, H5T_NATIVE_DOUBLE,
                        small_space, H5P_DEFAULT, H5P_DEFAULT)) < 0)
                    goto error;
                if(H5Aclose(attr) < 0)
                    goto error;
                perf(&attr_t, attr_t.start, retrieve_time());
                if(flush_attr && H5Fflush(file,  H5F_SCOPE_LOCAL) < 0)
                    goto error;
            } /* end for */

            close_t.start = retrieve_time();
            if(H5Dclose(dataset) < 0)
                goto error;
            perf(&close_t, close_t.start, retrieve_time());
            if(flush_dset && H5Fflush(file,  H5F_SCOPE_LOCAL) < 0)
                goto error;
    	} /* end for */
    } /* end for */

    if(facc_type == FACC_MPIO || facc_type == FACC_MPIPOSIX) {
#ifdef H5_HAVE_PARALLEL
        MPI_Barrier(MPI_COMM_WORLD);
#endif /*H5_HAVE_PARALLEL*/
    }

#ifdef H5_HAVE_PARALLEL
    /* only process 0 reports if parallel */
    if (facc_type == FACC_DEFAULT || (facc_type != FACC_DEFAULT && MAINPROCESS))
#endif /*H5_HAVE_PARALLEL*/
    {
        /* Calculate the average time */
        open_t.avg = open_t.total / (loop_num*NUM_DSETS);
        close_t.avg = close_t.total / (loop_num*NUM_DSETS);
        attr_t.avg = attr_t.total / (NUM_ATTRS*NUM_DSETS);

        /* Print out the performance result */
        fprintf(stderr, "3.  Create %d attributes for each of %d existing datasets for %d times\n",
            BATCH_ATTRS, NUM_DSETS, loop_num);
        print_perf(open_t, close_t, attr_t);
    }

    if (H5Fclose(file) < 0) goto error;

    return 0;

error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	retrieve_time
 *
 * Purpose:     Returns time in seconds, in a double number.
 *
 * Programmer:	Raymond Lu
 *		Friday, Oct 3, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
double retrieve_time(void)
{
#ifdef H5_HAVE_PARALLEL
    if(facc_type == FACC_DEFAULT) {
#endif /*H5_HAVE_PARALLEL*/
        struct timeval t;
        HDgettimeofday(&t, NULL);
        return ((double)t.tv_sec + (double)t.tv_usec / 1000000);
#ifdef H5_HAVE_PARALLEL
    } else {
        return MPI_Wtime();
    }
#endif /*H5_HAVE_PARALLEL*/
}


/*-------------------------------------------------------------------------
 * Function:	perf
 *
 * Purpose:	Calculate total time, maximal and minimal time of
 *              performance.
 *
 * Programmer:	Raymond Lu
 *		Friday, Oct 3, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void perf(p_time *perf_t, double start_t, double end_t)
{
	double t = end_t - start_t;

    if(facc_type == FACC_MPIO || facc_type == FACC_MPIPOSIX) {
#ifdef H5_HAVE_PARALLEL
        double reduced_t;
        double t_max, t_min;
        int    mpi_size, mpi_rank;

        MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
        MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
        MPI_Barrier(MPI_COMM_WORLD);

        MPI_Reduce(&t, &reduced_t, 1, MPI_DOUBLE, MPI_SUM, 0,
                MPI_COMM_WORLD);
        reduced_t /= mpi_size;

        MPI_Reduce(&t, &t_max, 1, MPI_DOUBLE, MPI_MAX, 0,
                MPI_COMM_WORLD);
        MPI_Reduce(&t, &t_min, 1, MPI_DOUBLE, MPI_MIN, 0,
                MPI_COMM_WORLD);

        if (MAINPROCESS) {
            perf_t->total += reduced_t;

	    if(t_max > perf_t->max)
		perf_t->max = t_max;
	    if(t_min < perf_t->min)
		perf_t->min = t_min;
        }
#endif /*H5_HAVE_PARALLEL*/
    } else {
	perf_t->total += t;

	if(t > perf_t->max)
		perf_t->max = t;
	if(t < perf_t->min)
		perf_t->min = t;
    }
}


/*-------------------------------------------------------------------------
 * Function:	print_perf
 *
 * Purpose:	Print out performance data.
 *
 * Programmer:	Raymond Lu
 *		Friday, Oct 3, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void print_perf(p_time open_t, p_time close_t, p_time attr_t)
{
    fprintf(stderr, "\t%s:\t\tavg=%.6fs;\tmax=%.6fs;\tmin=%.6fs\n",
		open_t.func, open_t.avg, open_t.max, open_t.min);
    fprintf(stderr, "\tH5Dclose:\t\tavg=%.6fs;\tmax=%.6fs;\tmin=%.6fs\n",
		close_t.avg, close_t.max, close_t.min);
    if(NUM_ATTRS)
        fprintf(stderr, "\tH5A(create & close):\tavg=%.6fs;\tmax=%.6fs;\tmin=%.6fs\n",
		attr_t.avg, attr_t.max, attr_t.min);
}


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Tests
 *
 * Return:	Success:	exit(0)
 *
 *		Failure:	exit(1)
 *
 * Programmer:	Raymond Lu
 *		Friday, Oct 3, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, char **argv)
{
#ifdef H5_HAVE_PARALLEL
    int mpi_size, mpi_rank;				/* mpi variables */
#endif /*H5_HAVE_PARALLEL*/

    if(parse_options(argc, argv) != 0) {
	   usage();
	   return 0;
    }

    if(facc_type == FACC_MPIO || facc_type == FACC_MPIPOSIX) {
#ifdef H5_HAVE_PARALLEL
        MPI_Init(&argc, &argv);
        MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
        MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
#endif /*H5_HAVE_PARALLEL*/
    }

#ifdef H5_HAVE_PARALLEL
    if (facc_type == FACC_DEFAULT || (facc_type != FACC_DEFAULT && MAINPROCESS))
#endif /*H5_HAVE_PARALLEL*/
        fprintf(stderr, "\t\tPerformance result of metadata for datasets and attributes\n\n");

    fapl = H5Pcreate (H5P_FILE_ACCESS);
    if(facc_type == FACC_MPIO || facc_type == FACC_MPIPOSIX) {
#ifdef H5_HAVE_PARALLEL
        if(facc_type == FACC_DEFAULT || facc_type == FACC_MPIO)
            H5Pset_fapl_mpio(fapl, MPI_COMM_WORLD, MPI_INFO_NULL);
        else if(facc_type == FACC_MPIPOSIX)
            H5Pset_fapl_mpiposix(fapl, MPI_COMM_WORLD, FALSE);
#endif /*H5_HAVE_PARALLEL*/
    }

    nerrors += create_dspace() < 0 	?1:0;

    if((RUN_TEST & TEST_1) || !RUN_TEST)
        nerrors += create_attrs_1() < 0 	?1:0;
    if((RUN_TEST & TEST_2) || !RUN_TEST)
        nerrors += create_attrs_2() < 0 	?1:0;
    if(((RUN_TEST & TEST_3) || !RUN_TEST) && BATCH_ATTRS && NUM_ATTRS)
        nerrors += create_attrs_3() < 0 	?1:0;

    if (H5Sclose(space) < 0) goto error;
    if (H5Sclose(small_space) < 0) goto error;

    h5_cleanup(FILENAME, fapl);

    if(facc_type == FACC_MPIO || facc_type == FACC_MPIPOSIX) {
#ifdef H5_HAVE_PARALLEL
        /* MPI_Finalize must be called AFTER H5close which may use MPI calls */
        MPI_Finalize();
#endif /*H5_HAVE_PARALLEL*/
    }

    if (nerrors) goto error;
#ifdef H5_HAVE_PARALLEL
    if (facc_type != FACC_DEFAULT && MAINPROCESS)
#endif /*H5_HAVE_PARALLEL*/
        printf("All metadata performance tests passed.\n");

    return 0;

 error:
    nerrors = MAX(1, nerrors);
#ifdef H5_HAVE_PARALLEL
    if (facc_type != FACC_DEFAULT && MAINPROCESS)
#endif /*H5_HAVE_PARALLEL*/
        printf("***** %d PERFORMANCE TEST%s FAILED! *****\n",
	   nerrors, 1 == nerrors ? "" : "S");

    return 1;
}

