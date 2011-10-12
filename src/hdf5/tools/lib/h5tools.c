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
 *              Thursday, July 23, 1998
 *
 * Purpose: A library for displaying the values of a dataset in a human
 *  readable format.
 */

#include <stdio.h>
#include <stdlib.h>

#include "h5tools.h"
#include "h5tools_ref.h"
#include "h5tools_utils.h"
#include "H5private.h"

#define SANITY_CHECK

#define ALIGN(A,Z)  ((((A) + (Z) - 1) / (Z)) * (Z))

/* global variables */
hid_t H5tools_ERR_CLS_g = -1;
hid_t H5E_tools_g = -1;
hid_t H5E_tools_min_id_g = -1;
int         compound_data;
FILE       *rawdatastream; /* should initialize to stdout but gcc moans about it */
int         bin_output;    /* binary output */
int         bin_form;      /* binary form */
int         region_output; /* region output */
#ifdef H5_HAVE_H5DUMP_PACKED_BITS
int         packed_bits_num; /* number of packed bits to display */
int         packed_data_offset; /* offset of packed bits to display */
unsigned long long packed_data_mask;  /* mask in which packed bits to display */
#endif

static h5tool_format_t h5tools_dataformat = {
0, /*raw */

"", /*fmt_raw */
"%d", /*fmt_int */
"%u", /*fmt_uint */
"%hhd", /*fmt_schar */
"%u", /*fmt_uchar */
"%d", /*fmt_short */
"%u", /*fmt_ushort */
"%ld", /*fmt_long */
"%lu", /*fmt_ulong */
NULL, /*fmt_llong */
NULL, /*fmt_ullong */
"%g", /*fmt_double */
"%g", /*fmt_float */

0, /*ascii */
0, /*str_locale */
0, /*str_repeat */

"[ ", /*arr_pre */
",", /*arr_sep */
" ]", /*arr_suf */
1, /*arr_linebreak */

"", /*cmpd_name */
",\n", /*cmpd_sep */
"{\n", /*cmpd_pre */
"}", /*cmpd_suf */
"\n", /*cmpd_end */

", ", /*vlen_sep */
"(", /*vlen_pre */
")", /*vlen_suf */
"", /*vlen_end */

"%s", /*elmt_fmt */
",", /*elmt_suf1 */
" ", /*elmt_suf2 */

"", /*idx_n_fmt */
"", /*idx_sep */
"", /*idx_fmt */

80, /*line_ncols *//*standard default columns */
0, /*line_per_line */
"", /*line_pre */
"%s", /*line_1st */
"%s", /*line_cont */
"", /*line_suf */
"", /*line_sep */
1, /*line_multi_new */
"   ", /*line_indent */

1, /*skip_first */

1, /*obj_hidefileno */
" "H5_PRINTF_HADDR_FMT, /*obj_format */

1, /*dset_hidefileno */
"DATASET %s ", /*dset_format */
"%s", /*dset_blockformat_pre */
"%s", /*dset_ptformat_pre */
"%s", /*dset_ptformat */
1, /*array indices */
1 /*escape non printable characters */
};

static const h5tools_dump_header_t h5tools_standardformat = {
"standardformat", /*name */
"HDF5", /*fileebgin */
"", /*fileend */
SUPER_BLOCK, /*bootblockbegin */
"", /*bootblockend */
H5_TOOLS_GROUP, /*groupbegin */
"", /*groupend */
H5_TOOLS_DATASET, /*datasetbegin */
"", /*datasetend */
ATTRIBUTE, /*attributebegin */
"", /*attributeend */
H5_TOOLS_DATATYPE, /*datatypebegin */
"", /*datatypeend */
DATASPACE, /*dataspacebegin */
"", /*dataspaceend */
DATA, /*databegin */
"", /*dataend */
SOFTLINK, /*softlinkbegin */
"", /*softlinkend */
EXTLINK, /*extlinkbegin */
"", /*extlinkend */
UDLINK, /*udlinkbegin */
"", /*udlinkend */
SUBSET, /*subsettingbegin */
"", /*subsettingend */
START, /*startbegin */
"", /*startend */
STRIDE, /*stridebegin */
"", /*strideend */
COUNT, /*countbegin */
"", /*countend */
BLOCK, /*blockbegin */
"", /*blockend */

"{", /*fileblockbegin */
"}", /*fileblockend */
"{", /*bootblockblockbegin */
"}", /*bootblockblockend */
"{", /*groupblockbegin */
"}", /*groupblockend */
"{", /*datasetblockbegin */
"}", /*datasetblockend */
"{", /*attributeblockbegin */
"}", /*attributeblockend */
"", /*datatypeblockbegin */
"", /*datatypeblockend */
"", /*dataspaceblockbegin */
"", /*dataspaceblockend */
"{", /*datablockbegin */
"}", /*datablockend */
"{", /*softlinkblockbegin */
"}", /*softlinkblockend */
"{", /*extlinkblockbegin */
"}", /*extlinkblockend */
"{", /*udlinkblockbegin */
"}", /*udlinkblockend */
"{", /*strblockbegin */
"}", /*strblockend */
"{", /*enumblockbegin */
"}", /*enumblockend */
"{", /*structblockbegin */
"}", /*structblockend */
"{", /*vlenblockbegin */
"}", /*vlenblockend */
"{", /*subsettingblockbegin */
"}", /*subsettingblockend */
"(", /*startblockbegin */
");", /*startblockend */
"(", /*strideblockbegin */
");", /*strideblockend */
"(", /*countblockbegin */
");", /*countblockend */
"(", /*blockblockbegin */
");", /*blockblockend */

"", /*dataspacedescriptionbegin */
"", /*dataspacedescriptionend */
"(", /*dataspacedimbegin */
")", /*dataspacedimend */
};

static const h5tools_dump_header_t * h5tools_dump_header_format;

/* local prototypes */
static int do_bin_output(FILE *stream, hid_t container, hsize_t nelmts, hid_t tid, void *_mem);
static int render_bin_output(FILE *stream, hid_t container, hid_t tid, void *_mem);
static int render_bin_output_region_data_blocks(hid_t region_id, FILE *stream,
    hid_t container, int ndims, hid_t type_id, hssize_t nblocks, hsize_t *ptdata);
static hbool_t render_bin_output_region_blocks(hid_t region_space, hid_t region_id,
                         FILE *stream, hid_t container);
static hbool_t render_bin_output_region_points(hid_t region_space, hid_t region_id,
                         FILE *stream, hid_t container);
static hbool_t h5tools_is_zero(const void *_mem, size_t size);

hbool_t h5tools_render_element(FILE *stream, const h5tool_format_t *info,
                h5tools_context_t *ctx/*in,out*/,
                h5tools_str_t *buffer/*string into which to render */,
                hsize_t *curr_pos/*total data element position*/,
                size_t ncols, hsize_t local_elmt_counter/*element counter*/,
                hsize_t elmt_counter);

hbool_t h5tools_render_region_element(FILE *stream, const h5tool_format_t *info,
                h5tools_context_t *ctx/*in,out*/,
                h5tools_str_t *buffer/*string into which to render */,
                hsize_t *curr_pos/*total data element position*/,
                size_t ncols, hsize_t *ptdata,
                hsize_t local_elmt_counter/*element counter*/,
                hsize_t elmt_counter);

static int h5tools_print_region_data_blocks(hid_t region_id,
        FILE *stream, const h5tool_format_t *info, h5tools_context_t ctx,
        h5tools_str_t *buffer/*string into which to render */, size_t ncols,
        int ndims, hid_t type_id, hssize_t nblocks, hsize_t *ptdata);

hbool_t h5tools_dump_region_data_points(hid_t region_space, hid_t region_id,
                FILE *stream, const h5tool_format_t *info,
                h5tools_context_t *ctx/*in,out*/,
                h5tools_str_t *buffer/*string into which to render */,
                hsize_t *curr_pos/*total data element position*/,
                size_t ncols, hsize_t region_elmt_counter/*element counter*/,
                hsize_t elmt_counter);

int h5tools_print_region_data_points(hid_t region_space, hid_t region_id,
        FILE *stream, const h5tool_format_t *info, h5tools_context_t ctx,
        h5tools_str_t *buffer, size_t ncols,
        int ndims, hid_t type_id, hssize_t npoints, hsize_t *ptdata);

hbool_t h5tools_dump_region_data_blocks(hid_t region_space, hid_t region_id,
                FILE *stream, const h5tool_format_t *info,
                h5tools_context_t *ctx/*in,out*/,
                h5tools_str_t *buffer/*string into which to render */,
                hsize_t *curr_pos/*total data element position*/,
                size_t ncols, hsize_t region_elmt_counter/*element counter*/,
                hsize_t elmt_counter);

/* module-scoped variables */
static int  h5tools_init_g;     /* if h5tools lib has been initialized */
#ifdef H5_HAVE_PARALLEL
static int  h5tools_mpi_init_g; /* if MPI_Init() has been called */
#endif /* H5_HAVE_PARALLEL */

/* Names of VFDs */
static const char *drivernames[]={
    "sec2",
    "family",
    "split",
    "multi",
#ifdef H5_HAVE_STREAM
    "stream",
#endif /* H5_HAVE_STREAM */
#ifdef H5_HAVE_PARALLEL
    "mpio",
    "mpiposix"
#endif /* H5_HAVE_PARALLEL */
};

/* This enum should match the entries in the above drivers_list since they
 * are indexes into the drivers_list array. */
enum {
    SEC2_IDX = 0
   ,FAMILY_IDX
   ,SPLIT_IDX
   ,MULTI_IDX
#ifdef H5_HAVE_STREAM
   ,STREAM_IDX
#endif /* H5_HAVE_STREAM */
#ifdef H5_HAVE_PARALLEL
   ,MPIO_IDX
   ,MPIPOSIX_IDX
#endif /* H5_HAVE_PARALLEL */
} driver_idx;
#define NUM_DRIVERS     (sizeof(drivernames) / sizeof(drivernames[0]))

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose:     Initialize the H5 Tools library
 * Description:
 *      This should be called before any other h5tools function is called.
 *      Effect of any h5tools function called before this has been called is
 *      undetermined.
 * Return:
 *      None
 * Programmer:
 *      Albert Cheng, 2000-10-31
 * Modifications:
 *-------------------------------------------------------------------------
 */
void
h5tools_init(void)
{
    char lib_str[256];

    if (!h5tools_init_g) {
        /* register the error class */
        sprintf(lib_str, "%d.%d.%d",H5_VERS_MAJOR, H5_VERS_MINOR, H5_VERS_RELEASE);

        H5TOOLS_INIT_ERROR()

        if (!rawdatastream)
            rawdatastream = stdout;

        h5tools_dump_header_format = &h5tools_standardformat;

        h5tools_init_g++;
    }
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose: Close the H5 Tools library
 * Description:
 *      Close or release resources such as files opened by the library. This
 *      should be called after all other h5tools functions have been called.
 *      Effect of any h5tools function called after this has been called is
 *      undetermined.
 * Return:
 *      None
 * Programmer:
 *      Albert Cheng, 2000-10-31
 * Modifications:
 *-------------------------------------------------------------------------
 */
void
h5tools_close(void)
{
    if (h5tools_init_g) {
        if (rawdatastream && rawdatastream != stdout) {
            if (fclose(rawdatastream))
                perror("closing rawdatastream");
            else
                rawdatastream = NULL;
        }

        /* Clean up the reference path table, if it's been used */
        term_ref_path_table();

        H5TOOLS_CLOSE_ERROR()

        /* Shut down the library */
        H5close();

        h5tools_init_g = 0;
    }
}

/*-------------------------------------------------------------------------
 * Audience:    Private
 * Chapter:     H5Tools Library
 * Purpose: Get a FAPL for a driver
 * Description:
 *      Get a FAPL for a given VFL driver name.
 * Return:
 *      None
 * Programmer:
 *      Quincey Koziol, 2004-02-04
 * Modifications:
 *      Pedro Vicente Nunes, Thursday, July 27, 2006
 *   Added error return conditions for the H5Pset_fapl calls
 *-------------------------------------------------------------------------
 */
static hid_t
h5tools_get_fapl(hid_t fapl, const char *driver, unsigned *drivernum)
{
    hid_t new_fapl; /* Copy of file access property list passed in, or new property list */

    /* Make a copy of the FAPL, for the file open call to use, eventually */
    if (fapl == H5P_DEFAULT) {
        if ((new_fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
            goto error;
    } /* end if */
    else {
        if ((new_fapl = H5Pcopy(fapl)) < 0)
            goto error;
    } /* end else */

    /* Determine which driver the user wants to open the file with. Try
     * that driver. If it can't open it, then fail. */
    if (!strcmp(driver, drivernames[SEC2_IDX])) {
        /* SEC2 driver */
        if (H5Pset_fapl_sec2(new_fapl) < 0)
            goto error;

        if (drivernum)
            *drivernum = SEC2_IDX;
    }
    else if (!strcmp(driver, drivernames[FAMILY_IDX])) {
        /* FAMILY Driver */

        /* Set member size to be 0 to indicate the current first member size
         * is the member size.
         */
        if (H5Pset_fapl_family(new_fapl, (hsize_t) 0, H5P_DEFAULT) < 0)
            goto error;

        if (drivernum)
            *drivernum = FAMILY_IDX;
    }
    else if (!strcmp(driver, drivernames[SPLIT_IDX])) {
        /* SPLIT Driver */
        if (H5Pset_fapl_split(new_fapl, "-m.h5", H5P_DEFAULT, "-r.h5", H5P_DEFAULT) < 0)
            goto error;

        if (drivernum)
            *drivernum = SPLIT_IDX;
    }
    else if (!strcmp(driver, drivernames[MULTI_IDX])) {
        /* MULTI Driver */
        if (H5Pset_fapl_multi(new_fapl, NULL, NULL, NULL, NULL, TRUE) < 0)
        goto error;

        if(drivernum)
        *drivernum = MULTI_IDX;
#ifdef H5_HAVE_STREAM
            }
            else if(!strcmp(driver, drivernames[STREAM_IDX])) {
                /* STREAM Driver */
                if(H5Pset_fapl_stream(new_fapl, NULL) < 0)
                goto error;

                if(drivernum)
                *drivernum = STREAM_IDX;
#endif /* H5_HAVE_STREAM */
#ifdef H5_HAVE_PARALLEL
            }
            else if(!strcmp(driver, drivernames[MPIO_IDX])) {
                /* MPI-I/O Driver */
                /* check if MPI has been initialized. */
                if(!h5tools_mpi_init_g)
                MPI_Initialized(&h5tools_mpi_init_g);
                if(h5tools_mpi_init_g) {
                    if(H5Pset_fapl_mpio(new_fapl, MPI_COMM_WORLD, MPI_INFO_NULL) < 0)
                goto error;

            if(drivernum)
                *drivernum = MPIO_IDX;
        } /* end if */
    }
    else if (!strcmp(driver, drivernames[MPIPOSIX_IDX])) {
        /* MPI-I/O Driver */
        /* check if MPI has been initialized. */
        if(!h5tools_mpi_init_g)
            MPI_Initialized(&h5tools_mpi_init_g);
        if(h5tools_mpi_init_g) {
            if(H5Pset_fapl_mpiposix(new_fapl, MPI_COMM_WORLD, TRUE) < 0)
                goto error;

            if(drivernum)
                *drivernum = MPIPOSIX_IDX;
        } /* end if */
#endif /* H5_HAVE_PARALLEL */
    }
    else {
        goto error;
    }

    return(new_fapl);

error:
    if(new_fapl != H5P_DEFAULT)
        H5Pclose(new_fapl);
    return -1;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose:     Open a file with various VFL drivers.
 * Description:
 *      Loop through the various types of VFL drivers trying to open FNAME.
 *      If the HDF5 library is version 1.2 or less, then we have only the SEC2
 *      driver to try out. If the HDF5 library is greater than version 1.2,
 *      then we have the FAMILY, SPLIT, and MULTI drivers to play with (and
 *      the STREAM driver if H5_HAVE_STREAM is defined, that is).
 *
 *      If DRIVER is non-NULL, then it will try to open the file with that
 *      driver first. We assume that the user knows what they are doing so, if
 *      we fail, then we won't try other file drivers.
 * Return:
 *      On success, returns a file id for the opened file. If DRIVERNAME is
 *      non-null then the first DRIVERNAME_SIZE-1 characters of the driver
 *      name are copied into the DRIVERNAME array and null terminated.
 *
 *      Otherwise, the function returns FAIL. If DRIVERNAME is non-null then
 *      the first byte is set to the null terminator.
 * Programmer:
 *      Lost in the mists of time.
 * Modifications:
 *      Robb Matzke, 2000-06-23
 *      We only have to initialize driver[] on the first call, thereby
 *      preventing memory leaks from repeated calls to H5Pcreate().
 *
 *      Robb Matzke, 2000-06-23
 *      Added DRIVERNAME_SIZE arg to prevent overflows when writing to
 *      DRIVERNAME.
 *
 *      Robb Matzke, 2000-06-23
 *      Added test to prevent coredump when the file could not be opened by
 *      any driver.
 *
 *      Robb Matzke, 2000-06-23
 *      Changed name from H5ToolsFopen() so it jives better with the names we
 *      already have at the top of this source file.
 *
 *      Thomas Radke, 2000-09-12
 *      Added Stream VFD to the driver[] array.
 *
 *      Bill Wendling, 2001-01-10
 *      Changed macro behavior so that if we have a version other than 1.2.x
 *      (i.e., > 1.2), then we do the drivers check.
 *
 *      Bill Wendling, 2001-07-30
 *      Added DRIVER parameter so that the user can specify "try this driver"
 *      instead of the default behaviour. If it fails to open the file with
 *      that driver, this will fail completely (i.e., we won't try the other
 *      drivers). We're assuming the user knows what they're doing. How UNIX
 *      of us.
 *-------------------------------------------------------------------------
 */
hid_t
h5tools_fopen(const char *fname, unsigned flags, hid_t fapl, const char *driver,
    char *drivername, size_t drivername_size)
{
    unsigned    drivernum;
    hid_t       fid = FAIL;
    hid_t       my_fapl = H5P_DEFAULT;

    if (driver && *driver) {
        /* Get the correct FAPL for the given driver */
        if ((my_fapl = h5tools_get_fapl(fapl, driver, &drivernum)) < 0)
            goto done;

        H5E_BEGIN_TRY {
            fid = H5Fopen(fname, flags, my_fapl);
        } H5E_END_TRY;

        if (fid == FAIL)
            goto done;

    }
    else {
        /* Try to open the file using each of the drivers */
        for (drivernum = 0; drivernum < NUM_DRIVERS; drivernum++) {
            /* Get the correct FAPL for the given driver */
            if((my_fapl = h5tools_get_fapl(fapl, drivernames[drivernum], NULL)) < 0)
                goto done;

            H5E_BEGIN_TRY {
                fid = H5Fopen(fname, flags, my_fapl);
            } H5E_END_TRY;

            if (fid != FAIL)
                break;
            else {
                /* Close the FAPL */
                H5Pclose(my_fapl);
                my_fapl = H5P_DEFAULT;
            } /* end else */
        }
    }

    /* Save the driver name */
    if (drivername && drivername_size) {
        if (fid != FAIL) {
            strncpy(drivername, drivernames[drivernum], drivername_size);
            drivername[drivername_size - 1] = '\0';
        }
        else {
            /*no file opened*/
            drivername[0] = '\0';
        }
    }

done:
    if(my_fapl != H5P_DEFAULT)
        H5Pclose(my_fapl);

    return fid;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose:     Count the number of columns in a string.
 * Description:
 *      Count the number of columns in a string. This is the number of
 *      characters in the string not counting line-control characters.
 * Return:
 *      On success, returns the width of the string. Otherwise this function
 *      returns 0.
 * Programmer:
 *       Robb Matzke, Tuesday, April 27, 1999
 * Modifications:
 *-------------------------------------------------------------------------
 */
static size_t
h5tools_ncols(const char *s)
{
    register size_t i;

    for (i = 0; *s; s++)
        if (*s >= ' ')
            i++;

    return i;
}

/*-------------------------------------------------------------------------
 * Function: h5tools_detect_vlen
 *
 * Purpose: Recursive check for any variable length data in given type.
 *
 * Return: 
 *    TRUE : type conatains any variable length data
 *    FALSE : type doesn't contain any variable length data
 *    Negative value: error occur
 * 
 * Programmer: Jonathan Kim  March 18, 2011
 *-------------------------------------------------------------------------
 */
htri_t
h5tools_detect_vlen(hid_t tid)
{
    htri_t ret;

    /* recursive detect any vlen data values in type (compound, array ...) */
    ret = H5Tdetect_class(tid, H5T_VLEN);
    if((ret == TRUE) || (ret < 0))
        goto done;

    /* recursive detect any vlen string in type (compound, array ...) */
    ret = h5tools_detect_vlen_str(tid);
    if((ret == TRUE) || (ret < 0))
        goto done;

done:
    return ret;
}

/*-------------------------------------------------------------------------
 * Function: h5tools_detect_vlen_str
 *
 * Purpose: Recursive check for variable length string of a datatype.
 *
 * Return: 
 *    TRUE : type conatains any variable length string
 *    FALSE : type doesn't contain any variable length string
 *    Negative value: error occur
 *
 *-------------------------------------------------------------------------
 */
htri_t
h5tools_detect_vlen_str(hid_t tid)
{
    H5T_class_t tclass = -1;
    htri_t ret = FALSE;

    ret = H5Tis_variable_str(tid);
    if((ret == TRUE) || (ret < 0))
        goto done;

    tclass = H5Tget_class(tid);
    if(tclass == H5T_ARRAY || tclass == H5T_VLEN) {
        hid_t btid = H5Tget_super(tid);

        if(btid < 0) {
            ret = (htri_t)btid;
            goto done;
        }
        ret = h5tools_detect_vlen_str(btid);
        if((ret == TRUE) || (ret < 0)) {
            H5Tclose(btid);
            goto done;
        }
    }
    else if(tclass == H5T_COMPOUND) {
        int i = 0;
        int n = H5Tget_nmembers(tid);

        if(n < 0) {
            n = ret;
            goto done;
        }

        for(i = 0; i < n; i++) {
            hid_t mtid = H5Tget_member_type(tid, i);

            ret = h5tools_detect_vlen_str(mtid);
            if((ret == TRUE) || (ret < 0)) {
                H5Tclose(mtid);
                goto done;
            }
            H5Tclose(mtid);
        }
    }

done:
    return ret;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose:     Emit a simple prefix to STREAM.
 * Description:
 *      If /ctx->need_prefix/ is set then terminate the current line (if
 *      applicable), calculate the prefix string, and display it at the start
 *      of a line.
 * Return:
 *      None
 * Programmer:
 *      Robb Matzke, Monday, April 26, 1999
 * Modifications:
 *      Robb Matzke, 1999-09-29
 * If a new prefix is printed then the current element number is set back
 * to zero.
 *      pvn, 2004-07-08
 * Added support for printing array indices:
 *  the indentation is printed before the prefix (printed one indentation
 *  level before)
 *-------------------------------------------------------------------------
 */
static void
h5tools_simple_prefix(FILE *stream, const h5tool_format_t *info,
                      h5tools_context_t *ctx, hsize_t elmtno, int secnum)
{
    h5tools_str_t prefix;
    h5tools_str_t str; /*temporary for indentation */
    size_t templength = 0;
    int i, indentlevel = 0;

    if (!ctx->need_prefix)
        return;

    memset(&prefix, 0, sizeof(h5tools_str_t));
    memset(&str, 0, sizeof(h5tools_str_t));

    /* Terminate previous line, if any */
    if (ctx->cur_column) {
        fputs(OPT(info->line_suf, ""), stream);
        putc('\n', stream);
        fputs(OPT(info->line_sep, ""), stream);
    }

    /* Calculate new prefix */
    h5tools_str_prefix(&prefix, info, elmtno, ctx->ndims, ctx);

    /* Write new prefix to output */
    if (ctx->indent_level >= 0) {
        indentlevel = ctx->indent_level;
    }
    else {
        /*
         * This is because sometimes we don't print out all the header
         * info for the data (like the tattr-2.ddl example). If that happens
         * the ctx->indent_level is negative so we need to skip the above and
         * just print out the default indent levels.
         */
        indentlevel = ctx->default_indent_level;
    }

    /* when printing array indices, print the indentation before the prefix
       the prefix is printed one indentation level before */
    if (info->pindex) {
        for (i = 0; i < indentlevel - 1; i++) {
            fputs(h5tools_str_fmt(&str, 0, info->line_indent), stream);
        }
    }

    if (elmtno == 0 && secnum == 0 && info->line_1st)
        fputs(h5tools_str_fmt(&prefix, 0, info->line_1st), stream);
    else if (secnum && info->line_cont)
        fputs(h5tools_str_fmt(&prefix, 0, info->line_cont), stream);
    else
        fputs(h5tools_str_fmt(&prefix, 0, info->line_pre), stream);

    templength = h5tools_str_len(&prefix);

    for (i = 0; i < indentlevel; i++) {
        /*we already made the indent for the array indices case */
        if (!info->pindex) {
            fputs(h5tools_str_fmt(&prefix, 0, info->line_indent), stream);
            templength += h5tools_str_len(&prefix);
        }
        else {
            /*we cannot count the prefix for the array indices case */
            templength += h5tools_str_len(&str);
        }
    }

    ctx->cur_column = ctx->prev_prefix_len = templength;
    ctx->cur_elmt = 0;
    ctx->need_prefix = 0;

    /* Free string */
    h5tools_str_close(&prefix);
    h5tools_str_close(&str);
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose:     Emit a simple prefix to STREAM.
 * Description:
 *      If /ctx->need_prefix/ is set then terminate the current line (if
 *      applicable), calculate the prefix string, and display it at the start
 *      of a line. Calls region specific function.
 * Return:
 *      None
 *-------------------------------------------------------------------------
 */
static void
h5tools_region_simple_prefix(FILE *stream, const h5tool_format_t *info,
        h5tools_context_t *ctx, hsize_t elmtno, hsize_t *ptdata, int secnum)
{
    h5tools_str_t prefix;
    h5tools_str_t str; /*temporary for indentation */
    size_t templength = 0;
    int i, indentlevel = 0;

    if (!ctx->need_prefix)
        return;

    memset(&prefix, 0, sizeof(h5tools_str_t));
    memset(&str, 0, sizeof(h5tools_str_t));

    /* Terminate previous line, if any */
    if (ctx->cur_column) {
        fputs(OPT(info->line_suf, ""), stream);
        putc('\n', stream);
        fputs(OPT(info->line_sep, ""), stream);
    }

    /* Calculate new prefix */
    h5tools_str_region_prefix(&prefix, info, elmtno, ptdata, ctx->ndims, 
            ctx->p_max_idx, ctx);

    /* Write new prefix to output */
    if (ctx->indent_level >= 0) {
        indentlevel = ctx->indent_level;
    }
    else {
        /*
         * This is because sometimes we don't print out all the header
         * info for the data (like the tattr-2.ddl example). If that happens
         * the ctx->indent_level is negative so we need to skip the above and
         * just print out the default indent levels.
         */
        indentlevel = ctx->default_indent_level;
    }

    /* when printing array indices, print the indentation before the prefix
       the prefix is printed one indentation level before */
    if (info->pindex) {
        for (i = 0; i < indentlevel - 1; i++) {
            fputs(h5tools_str_fmt(&str, 0, info->line_indent), stream);
        }
    }

    if (elmtno == 0 && secnum == 0 && info->line_1st)
        fputs(h5tools_str_fmt(&prefix, 0, info->line_1st), stream);
    else if (secnum && info->line_cont)
        fputs(h5tools_str_fmt(&prefix, 0, info->line_cont), stream);
    else
        fputs(h5tools_str_fmt(&prefix, 0, info->line_pre), stream);

    templength = h5tools_str_len(&prefix);

    for (i = 0; i < indentlevel; i++) {
        /*we already made the indent for the array indices case */
        if (!info->pindex) {
            fputs(h5tools_str_fmt(&prefix, 0, info->line_indent), stream);
            templength += h5tools_str_len(&prefix);
        }
        else {
            /*we cannot count the prefix for the array indices case */
            templength += h5tools_str_len(&str);
        }
    }

    ctx->cur_column = ctx->prev_prefix_len = templength;
    ctx->cur_elmt = 0;
    ctx->need_prefix = 0;

    /* Free string */
    h5tools_str_close(&prefix);
    h5tools_str_close(&str);
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose:     Prints NELMTS data elements to output STREAM.
 * Description:
 *      Prints some (NELMTS) data elements to output STREAM. The elements are
 *      stored in _MEM as type TYPE and are printed according to the format
 *      described in INFO. The CTX struct contains context information shared
 *      between calls to this function. The FLAGS is a bit field that
 *      indicates whether the data supplied in this call falls at the
 *      beginning or end of the total data to be printed (START_OF_DATA and
 *      END_OF_DATA).
 * Return:
 *      None
 * Programmer:
 *      Robb Matzke, Monday, April 26, 1999
 * Modifications:
 *  Robb Matzke, 1999-06-04
 * The `container' argument is the optional dataset for reference types.
 *
 *  Robb Matzke, 1999-09-29
 * Understands the `per_line' property which indicates that every Nth
 * element should begin a new line.
 *
 *      Robb Matzke, LLNL, 2003-06-05
 *      Do not dereference the memory for a variable-length string here.
 *      Deref in h5tools_str_sprint() instead so recursive types are
 *      handled correctly.
 *
 *      Pedro Vicente Nunes, The HDF Group, 2005-10-19
 *        pass to the prefix in h5tools_simple_prefix the total position
 *        instead of the current stripmine position i; this is necessary
 *        to print the array indices
 *        new field sm_pos in h5tools_context_t, the current stripmine element position
 *-------------------------------------------------------------------------
 */
void
h5tools_dump_simple_data(FILE *stream, const h5tool_format_t *info, hid_t container,
                         h5tools_context_t *ctx/*in,out*/, unsigned flags,
                         hsize_t nelmts, hid_t type, void *_mem)
{
    unsigned char *mem = (unsigned char*) _mem;
    hsize_t        i;         /*element counter  */
    size_t         size;      /*size of each datum  */
    hid_t          region_space;
    hid_t          region_id;
    hbool_t        dimension_break = TRUE;
    H5S_sel_type   region_type;
    size_t         ncols = 80; /*available output width */
    h5tools_str_t  buffer;    /*string into which to render */
    hsize_t        curr_pos;  /* total data element position   */
    hsize_t        elmt_counter = 0;/*counts the # elements printed.
                                     *I (ptl?) needed something that
                                     *isn't going to get reset when a new
                                     *line is formed. I'm going to use
                                     *this var to count elements and
                                     *break after we see a number equal
                                     *to the ctx->size_last_dim.   */

    /* binary dump */
    if (bin_output) {
        do_bin_output(rawdatastream, container, nelmts, type, _mem);
    } /* end if */
    else {
        /* setup */
        HDmemset(&buffer, 0, sizeof(h5tools_str_t));
        size = H5Tget_size(type);

        if (info->line_ncols > 0)
            ncols = info->line_ncols;

        /* pass to the prefix in h5tools_simple_prefix the total position
         * instead of the current stripmine position i; this is necessary
         * to print the array indices
         */
        curr_pos = ctx->sm_pos;

        for (i = 0; i < nelmts; i++, ctx->cur_elmt++, elmt_counter++) {
            void* memref = mem + i * size;
            if (region_output && H5Tequal(type, H5T_STD_REF_DSETREG)) {
                char ref_name[1024];

                /* region data */
                region_id = H5Rdereference(container, H5R_DATASET_REGION, memref);
                if (region_id >= 0) {
                    region_space = H5Rget_region(container, H5R_DATASET_REGION, memref);
                    if (region_space >= 0) {
                        if (h5tools_is_zero(memref, H5Tget_size(type))) {
                            h5tools_str_append(&buffer, "NULL");
                        }
                        else {
                            if(H5Rget_name(region_id, H5R_DATASET_REGION, memref, (char*) ref_name, 1024)<0)
                                HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Rget_name failed");

                            /* Render the region element begin */
                            h5tools_str_reset(&buffer);

                            h5tools_str_append(&buffer, info->dset_format, ref_name);

                            dimension_break = h5tools_render_element(stdout, info,
                                       ctx, &buffer, &curr_pos, ncols, i, elmt_counter);

                            region_type = H5Sget_select_type(region_space);
                            if(region_type==H5S_SEL_POINTS)
                                /* Print point information */
                                dimension_break = h5tools_dump_region_data_points(
                                                       region_space, region_id, stdout, info, ctx,
                                                       &buffer, &curr_pos, ncols, i, elmt_counter);
                            else if(region_type==H5S_SEL_HYPERSLABS)
                                /* Print block information */
                                dimension_break = h5tools_dump_region_data_blocks(
                                                       region_space, region_id, stdout, info, ctx,
                                                       &buffer, &curr_pos, ncols, i, elmt_counter);
                            else
                                HERROR(H5E_tools_g, H5E_tools_min_id_g, "invalid region type");
                            /* Render the region element end */

                        } /* end else to if (h5tools_is_zero(... */
                        if(H5Sclose(region_space) < 0)
                            HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Sclose failed");
                    } /* end if (region_space >= 0) */
                    else
                        HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Rget_region failed");
                    if(H5Dclose(region_id) < 0)
                        HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Dclose failed");

                } /* if (region_id >= 0) */
                else
                    HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Rdereference failed");

                ctx->need_prefix = TRUE;
            } /* end if (region_output... */
            else {
                /* Render the data element begin*/
                h5tools_str_reset(&buffer);
                h5tools_str_sprint(&buffer, info, container, type, memref, ctx);

                if (i + 1 < nelmts || (flags & END_OF_DATA) == 0)
                    h5tools_str_append(&buffer, "%s", OPT(info->elmt_suf1, ","));

                dimension_break = h5tools_render_element(stream, info, ctx, &buffer,
                                                           &curr_pos, ncols, i, elmt_counter);
                /* Render the data element end*/

            }
            if(FALSE==dimension_break)
                elmt_counter = 0;
        } /* end for (i = 0; i < nelmts... */

        h5tools_str_close(&buffer);
    }/* else bin */
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose:     Render an element to output STREAM.
 * Description:
 *      Prints the string buffer to the output STREAM. The string is
 *      printed according to the format described in INFO. The CTX struct
 *      contains context information shared between calls to this function.
 *
 * Return:
 *      False if a dimension end is reached, otherwise true
 *
 * In/Out:
 *      h5tools_context_t *ctx
 *      h5tools_str_t *buffer
 *      hsize_t *curr_pos
 *
 * Parameters Description:
 *      h5tools_str_t *buffer is the string into which to render
 *      hsize_t curr_pos is the total data element position
 *      size_t ncols
 *      hsize_t local_elmt_counter is the local element loop counter
 *      hsize_t elmt_count is the data element loop counter
 *-------------------------------------------------------------------------
 */
hbool_t
h5tools_render_element(FILE *stream, const h5tool_format_t *info,
        h5tools_context_t *ctx, h5tools_str_t *buffer, hsize_t *curr_pos,
        size_t ncols, hsize_t local_elmt_counter, hsize_t elmt_counter)
{
    hbool_t  dimension_break = TRUE;
    char    *s;
    char    *section; /*a section of output  */
    int      secnum; /*section sequence number */
    int      multiline; /*datum was multiline  */

    s = h5tools_str_fmt(buffer, 0, "%s");

    /*
     * If the element would split on multiple lines if printed at our
     * current location...
     */
    if (info->line_multi_new == 1 &&
        (ctx->cur_column + h5tools_ncols(s) +
         strlen(OPT(info->elmt_suf2, " ")) +
         strlen(OPT(info->line_suf, ""))) > ncols) {
        if (ctx->prev_multiline) {
            /*
             * ... and the previous element also occupied more than one
             * line, then start this element at the beginning of a line.
             */
            ctx->need_prefix = TRUE;
        }
        else if ((ctx->prev_prefix_len + h5tools_ncols(s) +
                  strlen(OPT(info->elmt_suf2, " ")) +
                  strlen(OPT(info->line_suf, ""))) <= ncols) {
            /*
             * ...but *could* fit on one line otherwise, then we
             * should end the current line and start this element on its
             * own line.
             */
            ctx->need_prefix = TRUE;
        }
    }

    /*
     * We need to break after each row of a dimension---> we should
     * break at the end of the each last dimension well that is the
     * way the dumper did it before
     */
    if (info->arr_linebreak && ctx->cur_elmt) {
        if (ctx->size_last_dim && (ctx->cur_elmt % ctx->size_last_dim) == 0)
            ctx->need_prefix = TRUE;

        if (elmt_counter == ctx->size_last_dim) {
            ctx->need_prefix = TRUE;
            dimension_break = FALSE;
        }
    }

    /*
     * If the previous element occupied multiple lines and this element
     * is too long to fit on a line then start this element at the
     * beginning of the line.
     */
    if (info->line_multi_new == 1 &&
            ctx->prev_multiline &&
            (ctx->cur_column +
            h5tools_ncols(s) +
            strlen(OPT(info->elmt_suf2, " ")) +
            strlen(OPT(info->line_suf, ""))) > ncols)
        ctx->need_prefix = TRUE;

    /*
     * If too many elements have already been printed then we need to
     * start a new line.
     */
    if (info->line_per_line > 0 && ctx->cur_elmt >= info->line_per_line)
        ctx->need_prefix = TRUE;

    /*
     * Each OPTIONAL_LINE_BREAK embedded in the rendered string can cause
     * the data to split across multiple lines.  We display the sections
     * one-at a time.
     */
    multiline = 0;
    for (secnum = 0, multiline = 0;
             (section = strtok(secnum ? NULL : s, OPTIONAL_LINE_BREAK));
             secnum++) {
        /*
         * If the current section plus possible suffix and end-of-line
         * information would cause the output to wrap then we need to
         * start a new line.
         */

        /*
         * check for displaying prefix for each section
         */
        if ( (ctx->cur_column + strlen(section) +
              strlen(OPT(info->elmt_suf2, " ")) +
              strlen(OPT(info->line_suf, ""))) > ncols)
            ctx->need_prefix = 1;

        /*
         * Print the prefix or separate the beginning of this element
         * from the previous element.
         */
        if (ctx->need_prefix) {
            if (secnum)
                multiline++;

            /* pass to the prefix in h5tools_simple_prefix the total
             * position instead of the current stripmine position i;
             * this is necessary to print the array indices
             */
            *curr_pos = ctx->sm_pos + local_elmt_counter;

            h5tools_simple_prefix(stream, info, ctx, *curr_pos, secnum);
        }
        else if ((local_elmt_counter || ctx->continuation) && secnum == 0) {
            fputs(OPT(info->elmt_suf2, " "), stream);
            ctx->cur_column += strlen(OPT(info->elmt_suf2, " "));
        }

        /* Print the section */
        fputs(section, stream);
        ctx->cur_column += strlen(section);
    }

    ctx->prev_multiline = multiline;
    return dimension_break;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose:     Render a region element to output STREAM.
 * Description:
 *      Prints the string buffer to the output STREAM. The string is
 *      printed according to the format described in INFO. The CTX struct
 *      contains context information shared between calls to this function.
 *
 * Return:
 *      False if a dimension end is reached, otherwise true
 *
 * In/Out:
 *      h5tools_context_t *ctx
 *      h5tools_str_t *buffer
 *      hsize_t *curr_pos
 *
 * Parameters Description:
 *      h5tools_str_t *buffer is the string into which to render
 *      hsize_t curr_pos is the total data element position
 *      size_t ncols
 *      hsize_t *ptdata
 *      hsize_t local_elmt_counter is the local element loop counter
 *      hsize_t elmt_count is the data element loop counter
 *-------------------------------------------------------------------------
 */
hbool_t
h5tools_render_region_element(FILE *stream, const h5tool_format_t *info,
        h5tools_context_t *ctx, h5tools_str_t *buffer, hsize_t *curr_pos,
        size_t ncols, hsize_t *ptdata, hsize_t local_elmt_counter, hsize_t elmt_counter)
{
    hbool_t  dimension_break = TRUE;
    char    *s;
    char    *section; /*a section of output  */
    int      secnum; /*section sequence number */
    int      multiline; /*datum was multiline  */

    s = h5tools_str_fmt(buffer, 0, "%s");

    /*
     * If the element would split on multiple lines if printed at our
     * current location...
     */
    if (info->line_multi_new == 1 &&
            (ctx->cur_column + h5tools_ncols(s) +
            strlen(OPT(info->elmt_suf2, " ")) +
            strlen(OPT(info->line_suf, ""))) > ncols) {
        if (ctx->prev_multiline) {
            /*
             * ... and the previous element also occupied more than one
             * line, then start this element at the beginning of a line.
             */
            ctx->need_prefix = TRUE;
        }
        else if ((ctx->prev_prefix_len + h5tools_ncols(s) +
                strlen(OPT(info->elmt_suf2, " ")) +
                strlen(OPT(info->line_suf, ""))) <= ncols) {
            /*
             * ...but *could* fit on one line otherwise, then we
             * should end the current line and start this element on its
             * own line.
             */
            ctx->need_prefix = TRUE;
        }
    }

    /*
     * We need to break after each row of a dimension---> we should
     * break at the end of the each last dimension well that is the
     * way the dumper did it before
     */
    if (info->arr_linebreak && ctx->cur_elmt) {
        if (ctx->size_last_dim && (ctx->cur_elmt % ctx->size_last_dim) == 0)
            ctx->need_prefix = TRUE;

        if (elmt_counter == ctx->size_last_dim) {
            ctx->need_prefix = TRUE;
            dimension_break = FALSE;
        }
    }

    /*
     * If the previous element occupied multiple lines and this element
     * is too long to fit on a line then start this element at the
     * beginning of the line.
     */
    if (info->line_multi_new == 1 &&
            ctx->prev_multiline &&
            (ctx->cur_column +
            h5tools_ncols(s) +
            strlen(OPT(info->elmt_suf2, " ")) +
            strlen(OPT(info->line_suf, ""))) > ncols)
        ctx->need_prefix = TRUE;

    /*
     * If too many elements have already been printed then we need to
     * start a new line.
     */
    if (info->line_per_line > 0 && ctx->cur_elmt >= info->line_per_line)
        ctx->need_prefix = TRUE;

    /*
     * Each OPTIONAL_LINE_BREAK embedded in the rendered string can cause
     * the data to split across multiple lines.  We display the sections
     * one-at a time.
     */
    multiline = 0;
    for (secnum = 0, multiline = 0; (section = strtok(secnum ? NULL : s,
            OPTIONAL_LINE_BREAK)); secnum++) {
        /*
         * If the current section plus possible suffix and end-of-line
         * information would cause the output to wrap then we need to
         * start a new line.
         */

        /*
         * Added the info->skip_first because the dumper does not want
         * this check to happen for the first line
         */
        if ((!info->skip_first || local_elmt_counter) &&
                (ctx->cur_column +
                strlen(section) +
                strlen(OPT(info->elmt_suf2, " ")) +
                strlen(OPT(info->line_suf, ""))) > ncols)
            ctx->need_prefix = 1;

        /*
         * Print the prefix or separate the beginning of this element
         * from the previous element.
         */
        if (ctx->need_prefix) {
            if (secnum)
                multiline++;

            /* pass to the prefix in h5tools_simple_prefix the total
             * position instead of the current stripmine position i;
             * this is necessary to print the array indices
             */
            *curr_pos = ctx->sm_pos + local_elmt_counter;

            h5tools_region_simple_prefix(stream, info, ctx, local_elmt_counter, ptdata, secnum);
        }
        else if ((local_elmt_counter || ctx->continuation) && secnum == 0) {
            fputs(OPT(info->elmt_suf2, " "), stream);
            ctx->cur_column += strlen(OPT(info->elmt_suf2, " "));
        }

        /* Print the section */
        fputs(section, stream);
        ctx->cur_column += strlen(section);
    }

    ctx->prev_multiline = multiline;
    return dimension_break;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose: Print the data values from a dataset referenced by region blocks.
 *
 * Description:
 *      This is a special case subfunction to print the data in a region reference of type blocks.
 *
 * Return:
 *      The function returns FAIL if there was an error, otherwise SUCEED
 *
 * Parameters Description:
 *      h5tools_str_t *buffer is the string into which to render
 *      size_t ncols
 *      int ndims is the number of dimensions of the region element
 *      hssize_t nblocks is the number of blocks in the region
 *-------------------------------------------------------------------------
 */
static int
h5tools_print_region_data_blocks(hid_t region_id,
        FILE *stream, const h5tool_format_t *info, h5tools_context_t ctx,
        h5tools_str_t *buffer/*string into which to render */, size_t ncols,
        int ndims, hid_t type_id, hssize_t nblocks, hsize_t *ptdata)
{
    hbool_t      dimension_break = TRUE;
    hsize_t     *dims1 = NULL;
    hsize_t     *start = NULL;
    hsize_t     *count = NULL;
    size_t       numelem;
    hsize_t      total_size[H5S_MAX_RANK];
    hsize_t      elmtno; /* elemnt index  */
    unsigned int region_flags; /* buffer extent flags */
    hsize_t      curr_pos;
    int          jndx;
    size_t       indx;
    int          type_size;
    hid_t        mem_space = -1;
    void        *region_buf = NULL;
    int          blkndx;
    hid_t        sid1 = -1;
    int          ret_value = SUCCEED;

    /* Get the dataspace of the dataset */
    if((sid1 = H5Dget_space(region_id)) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dget_space failed");

    /* Allocate space for the dimension array */
    if((dims1 = (hsize_t *) HDmalloc(sizeof(hsize_t) * ndims)) == NULL)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "Could not allocate buffer for dims");

    /* find the dimensions of each data space from the block coordinates */
    numelem = 1;
    for (jndx = 0; jndx < ndims; jndx++) {
        dims1[jndx] = ptdata[jndx + ndims] - ptdata[jndx] + 1;
        numelem = dims1[jndx] * numelem;
    }

    /* Create dataspace for reading buffer */
    if((mem_space = H5Screate_simple(ndims, dims1, NULL)) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Screate_simple failed");

    if((type_size = H5Tget_size(type_id)) == 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Tget_size failed");

    if((region_buf = HDmalloc(type_size * numelem)) == NULL)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "Could not allocate region buffer");

    /* Select (x , x , ..., x ) x (y , y , ..., y ) hyperslab for reading memory dataset */
    /*          1   2        n      1   2        n                                       */
    if((start = (hsize_t *) HDmalloc(sizeof(hsize_t) * ndims)) == NULL)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "Could not allocate buffer for start");

    if((count = (hsize_t *) HDmalloc(sizeof(hsize_t) * ndims)) == NULL)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "Could not allocate buffer for count");

    curr_pos = 0;
    for (blkndx = 0; blkndx < nblocks; blkndx++) {
        ctx.ndims = ndims;
        ctx.need_prefix = TRUE;
        ctx.cur_elmt = 0;
        for (jndx = 0; jndx < ndims; jndx++) {
            start[jndx] = ptdata[jndx + blkndx * ndims * 2];
            count[jndx] = dims1[jndx];
        }

        if(H5Sselect_hyperslab(sid1, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
            HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Sselect_hyperslab failed");

        if(H5Dread(region_id, type_id, mem_space, sid1, H5P_DEFAULT, region_buf) < 0)
            HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Dread failed");

        ctx.indent_level++;
        if(H5Sget_simple_extent_dims(mem_space, total_size, NULL) < 0)
            HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Sget_simple_extent_dims failed");

        /* assume entire data space to be printed */
        for (indx = 0; indx < (size_t) ctx.ndims; indx++)
            ctx.p_min_idx[indx] = start[indx];
        init_acc_pos(&ctx, total_size);

        /* print the data */
        region_flags = START_OF_DATA;
        if (blkndx == nblocks - 1)
            region_flags |= END_OF_DATA;

        for (indx = 0; indx < (size_t)ctx.ndims; indx++)
            ctx.p_max_idx[indx] = dims1[indx];

        curr_pos = 0;
        ctx.sm_pos = blkndx*2*ndims;
        ctx.size_last_dim = dims1[ndims-1];

        h5tools_region_simple_prefix(stream, info, &ctx, curr_pos, ptdata, 0);

        elmtno = 0;
        for (indx = 0; indx < numelem; indx++, elmtno++, ctx.cur_elmt++) {
            /* Render the region data element begin */
            h5tools_str_reset(buffer);

            h5tools_str_append(buffer, "%s", indx ? OPTIONAL_LINE_BREAK "" : "");
            h5tools_str_sprint(buffer, info, region_id, type_id,
                                ((char*)region_buf + indx * type_size), &ctx);

            if (indx + 1 < numelem || (region_flags & END_OF_DATA) == 0)
                h5tools_str_append(buffer, "%s", OPT(info->elmt_suf1, ","));

            dimension_break = h5tools_render_region_element(stream, info, &ctx, buffer, &curr_pos,
                                                                    ncols, ptdata, indx, elmtno);
            /* Render the region data element end */

            if(FALSE == dimension_break)
                elmtno = 0;
        } /* end for (indx = 0; indx < numelem; indx++, region_elmtno++, ctx.cur_elmt++) */

        ctx.indent_level--;
    } /* end for (blkndx = 0; blkndx < nblocks; blkndx++) */

 done:
    HDfree(start);
    HDfree(count);
    HDfree(region_buf);
    HDfree(dims1);

    if(H5Sclose(mem_space) < 0)
        HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Sclose failed");
    if(H5Sclose(sid1) < 0)
        HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Sclose failed");

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose: Print some values from a dataset referenced by region blocks.
 *
 * Description:
 *      This is a special case subfunction to dump a region reference using blocks.
 *
 * Return:
 *      The function returns False if the last dimension has been reached, otherwise True
 *
 * In/Out:
 *      h5tools_context_t *ctx
 *      hsize_t *curr_pos
 *
 * Parameters Description:
 *      h5tools_str_t *buffer is the string into which to render
 *      hsize_t curr_pos is the total data element position
 *      size_t ncols
 *      hsize_t region_elmt_counter is the region element loop counter
 *      hsize_t elmt_count is the data element loop counter
 *-------------------------------------------------------------------------
 */
hbool_t
h5tools_dump_region_data_blocks(hid_t region_space, hid_t region_id,
        FILE *stream, const h5tool_format_t *info,
        h5tools_context_t *ctx/*in,out*/,
        h5tools_str_t *buffer/*string into which to render */,
        hsize_t *curr_pos/*total data element position*/,
        size_t ncols, hsize_t region_elmt_counter/*element counter*/,
        hsize_t elmt_counter)
{
    HERR_INIT(hbool_t, TRUE)
    hbool_t      dimension_break = TRUE;
    hssize_t     nblocks;
    hsize_t      alloc_size;
    hsize_t     *ptdata = NULL;
    int          ndims;
    hid_t        dtype;
    hid_t        type_id;
    int          i;

    if((nblocks = H5Sget_select_hyper_nblocks(region_space)) <= 0)
        H5E_THROW(dimension_break, H5E_tools_min_id_g, "H5Sget_select_hyper_nblocks failed");

    /* Print block information */
    if((ndims = H5Sget_simple_extent_ndims(region_space)) < 0)
        H5E_THROW(dimension_break, H5E_tools_min_id_g, "H5Sget_simple_extent_ndims failed");

    /* Render the region { element begin */
    h5tools_str_reset(buffer);

    h5tools_str_append(buffer, "{");
    dimension_break = h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols, region_elmt_counter, elmt_counter);
    /* Render the region { element end */

    /* Render the region datatype info and indices element begin */
    h5tools_str_reset(buffer);

    ctx->indent_level++;
    ctx->need_prefix = TRUE;
    h5tools_str_append(buffer, "REGION_TYPE BLOCK  ");

    alloc_size = nblocks * ndims * 2 * sizeof(ptdata[0]);
    assert(alloc_size == (hsize_t) ((size_t) alloc_size)); /*check for overflow*/
    if((ptdata = (hsize_t*) malloc((size_t) alloc_size)) == NULL)
        HGOTO_ERROR(dimension_break, H5E_tools_min_id_g, "Could not allocate buffer for ptdata");

    H5_CHECK_OVERFLOW(nblocks, hssize_t, hsize_t);
    if(H5Sget_select_hyper_blocklist(region_space, (hsize_t) 0, (hsize_t) nblocks, ptdata) < 0)
        HGOTO_ERROR(dimension_break, H5E_tools_min_id_g, "H5Rget_select_hyper_blocklist failed");

    for (i = 0; i < nblocks; i++) {
        int j;

        h5tools_str_append(buffer, info->dset_blockformat_pre,
                            i ? "," OPTIONAL_LINE_BREAK " " : "", (unsigned long) i);

        /* Start coordinates and opposite corner */
        for (j = 0; j < ndims; j++)
            h5tools_str_append(buffer, "%s%lu", j ? "," : "(",
                                (unsigned long) ptdata[i * 2 * ndims + j]);

        for (j = 0; j < ndims; j++)
            h5tools_str_append(buffer, "%s%lu", j ? "," : ")-(",
                                (unsigned long) ptdata[i * 2 * ndims + j + ndims]);

        h5tools_str_append(buffer, ")");
    } /* end for (i = 0; i < nblocks; i++) */

    dimension_break = h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols, region_elmt_counter, elmt_counter);
    /* Render the region datatype info and indices element end */

    ctx->need_prefix = TRUE;

    if((dtype = H5Dget_type(region_id)) < 0)
        HGOTO_ERROR(dimension_break, H5E_tools_min_id_g, "H5Dget_type failed");
    if((type_id = H5Tget_native_type(dtype, H5T_DIR_DEFAULT)) < 0)
        HGOTO_ERROR(dimension_break, H5E_tools_min_id_g, "H5Tget_native_type failed");

    /* Render the datatype element begin */
    h5tools_str_reset(buffer);
    h5tools_str_append(buffer, "%s %s ",
                        h5tools_dump_header_format->datatypebegin,
                        h5tools_dump_header_format->datatypeblockbegin);

    h5tools_print_datatype(buffer, info, ctx, dtype);

    if (HDstrlen(h5tools_dump_header_format->datatypeblockend)) {
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->datatypeblockend);
        if (HDstrlen(h5tools_dump_header_format->datatypeend))
            h5tools_str_append(buffer, " ");
    }
    if (HDstrlen(h5tools_dump_header_format->datatypeend))
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->datatypeend);

    dimension_break = h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols, region_elmt_counter, elmt_counter);
    /* Render the datatype element end */

    ctx->need_prefix = TRUE;

    /* Render the dataspace element begin */
    h5tools_str_reset(buffer);

    ctx->need_prefix = TRUE;
    h5tools_str_append(buffer, "%s ", h5tools_dump_header_format->dataspacebegin);

    h5tools_print_dataspace(buffer, region_space);

    if (HDstrlen(h5tools_dump_header_format->dataspaceblockend)) {
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->dataspaceblockend);
        if (HDstrlen(h5tools_dump_header_format->dataspaceend))
            h5tools_str_append(buffer, " ");
    }
    if (HDstrlen(h5tools_dump_header_format->dataspaceend))
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->dataspaceblockend);

    dimension_break = h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols, region_elmt_counter, elmt_counter);
    /* Render the dataspace element end */

    ctx->need_prefix = TRUE;

    /* Render the databegin element begin */
    h5tools_str_reset(buffer);

    h5tools_str_append(buffer, "%s %s ",
                        h5tools_dump_header_format->databegin,
                        h5tools_dump_header_format->datablockbegin);

    dimension_break = h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols, region_elmt_counter, elmt_counter);
    /* Render the databegin element end */

    ctx->need_prefix = TRUE;

    h5tools_print_region_data_blocks(region_id, rawdatastream, info, *ctx,
        buffer, ncols, ndims, type_id, nblocks, ptdata);

 done:
    free(ptdata);

    if(H5Tclose(type_id) < 0)
        HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tclose failed");

    if(H5Tclose(dtype) < 0)
        HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tclose failed");

    ctx->need_prefix = TRUE;

    /* Render the dataend element begin */
    h5tools_str_reset(buffer);
    h5tools_str_append(buffer, "%s %s ",
                        h5tools_dump_header_format->dataend,
                        h5tools_dump_header_format->datablockend);
    dimension_break = h5tools_render_element(stream, info, ctx, buffer, curr_pos,
                                                ncols, region_elmt_counter, elmt_counter);
    /* Render the dataend element end */

    ctx->indent_level--;
    ctx->need_prefix = TRUE;

    /* Render the region } element begin */
    h5tools_str_reset(buffer);
    h5tools_str_append(buffer, "}");
    dimension_break = h5tools_render_element(stream, info, ctx, buffer, curr_pos,
                                                ncols, region_elmt_counter, elmt_counter);
    /* Render the region } element end */

    H5_LEAVE(dimension_break)

 CATCH
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose: Print the data values from a dataset referenced by region points.
 *
 * Description:
 *      This is a special case subfunction to print the data in a region reference of type points.
 *
 * Return:
 *      The function returns FAIL on error, otherwise SUCCEED
 *
 * Parameters Description:
 *      h5tools_str_t *buffer is the string into which to render
 *      size_t ncols
 *      int ndims is the number of dimensions of the region element
 *      hssize_t npoints is the number of points in the region
 *-------------------------------------------------------------------------
 */
int
h5tools_print_region_data_points(hid_t region_space, hid_t region_id,
        FILE *stream, const h5tool_format_t *info, h5tools_context_t ctx,
        h5tools_str_t *buffer, size_t ncols,
        int ndims, hid_t type_id, hssize_t npoints, hsize_t *ptdata)
{
    hbool_t  dimension_break = TRUE;
    hsize_t *dims1 = NULL;
    hsize_t  elmtno; /* elemnt index  */
    unsigned int region_flags; /* buffer extent flags */
    hsize_t  curr_pos;
    size_t   indx;
    int      jndx;
    int      type_size;
    hid_t    mem_space = -1;
    void    *region_buf = NULL;
    int      ret_value = SUCCEED;

    if((type_size = H5Tget_size(type_id)) == 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Tget_size failed");

    if((region_buf = HDmalloc(type_size * npoints)) == NULL)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "Could not allocate buffer for region");

    /* Allocate space for the dimension array */
    if((dims1 = (hsize_t *) HDmalloc(sizeof(hsize_t) * ndims)) == NULL)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "Could not allocate buffer for dims");

    dims1[0] = npoints;
    if((mem_space = H5Screate_simple(1, dims1, NULL)) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Screate_simple failed");

    if(H5Dread(region_id, type_id, mem_space, region_space, H5P_DEFAULT, region_buf) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dread failed");

    elmtno = 0;
    curr_pos = 0;
    for (jndx = 0; jndx < npoints; jndx++, elmtno++) {
        ctx.ndims = ndims;
        ctx.need_prefix = TRUE;
        ctx.cur_elmt = 0;    /* points are always 0 */

        /* Render the point element begin */
        h5tools_str_reset(buffer);

        ctx.indent_level++;

        /* assume entire data space to be printed */
        for (indx = 0; indx < (size_t) ctx.ndims; indx++)
            ctx.p_min_idx[indx] = 0;
        if(H5Sget_simple_extent_dims(region_space, ctx.p_max_idx, NULL) < 0)
            HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Sget_simple_extent_dims failed");

        if (ctx.ndims > 0) {
            ctx.size_last_dim = (int) (ctx.p_max_idx[ctx.ndims - 1]);
        }
        else
            ctx.size_last_dim = 0;

        if (ctx.ndims > 0)
            init_acc_pos(&ctx, ctx.p_max_idx);

        /* print the data */
        region_flags = START_OF_DATA;
        if (jndx == npoints - 1)
            region_flags |= END_OF_DATA;

        curr_pos = 0;    /* points requires constant 0 */
        ctx.sm_pos = jndx * ndims;

        h5tools_region_simple_prefix(stream, info, &ctx, curr_pos, ptdata, 0);

        h5tools_str_sprint(buffer, info, region_id, type_id,
                               ((char*)region_buf + jndx * type_size), &ctx);

        if (jndx + 1 < npoints || (region_flags & END_OF_DATA) == 0)
            h5tools_str_append(buffer, "%s", OPT(info->elmt_suf1, ","));

        dimension_break =
                h5tools_render_region_element(stream, info, &ctx, buffer, &curr_pos,
                                                ncols, ptdata, (hsize_t)0, elmtno);
        /* Render the point element end */

        ctx.indent_level--;
        if(FALSE == dimension_break)
            elmtno = 0;
    } /* end for (jndx = 0; jndx < npoints; jndx++, region_elmtno++) */

 done:
    HDfree(region_buf);
    HDfree(dims1);

    if(H5Sclose(mem_space) < 0)
        HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Sclose failed");

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose: Print some values from a dataset referenced by region points.
 *
 * Description:
 *      This is a special case subfunction to dump a region reference using points.
 *
 * Return:
 *      The function returns False if the last dimension has been reached, otherwise True
 *
 * In/Out:
 *      h5tools_context_t *ctx
 *      hsize_t *curr_pos
 *
 * Parameters Description:
 *      h5tools_str_t *buffer is the string into which to render
 *      hsize_t curr_pos is the total data element position
 *      size_t ncols
 *      hsize_t region_elmt_counter is the region element loop counter
 *      hsize_t elmt_count is the data element loop counter
 *-------------------------------------------------------------------------
 */
hbool_t
h5tools_dump_region_data_points(hid_t region_space, hid_t region_id,
        FILE *stream, const h5tool_format_t *info, h5tools_context_t *ctx,
        h5tools_str_t *buffer, hsize_t *curr_pos, size_t ncols, hsize_t region_elmt_counter,
        hsize_t elmt_counter) {
    HERR_INIT(hbool_t, TRUE)
    hbool_t  dimension_break = TRUE;
    hssize_t npoints;
    hsize_t  alloc_size;
    hsize_t *ptdata;
    int      ndims;
    hssize_t indx;
    hid_t    dtype;
    hid_t    type_id;

    if((npoints = H5Sget_select_elem_npoints(region_space)) <= 0)
        H5E_THROW(dimension_break, H5E_tools_min_id_g, "H5Sget_select_elem_npoints failed");

    /* Allocate space for the dimension array */
    if((ndims = H5Sget_simple_extent_ndims(region_space)) < 0)
        H5E_THROW(dimension_break, H5E_tools_min_id_g, "H5Sget_simple_extent_ndims failed");

    /* Render the region { element begin */
    h5tools_str_reset(buffer);

    h5tools_str_append(buffer, "{");
    dimension_break = h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols, region_elmt_counter, elmt_counter);
    /* Render the region { element end */

    /* Render the region datatype info and indices element begin */
    h5tools_str_reset(buffer);

    ctx->indent_level++;
    ctx->need_prefix = TRUE;
    h5tools_str_append(buffer, "REGION_TYPE POINT  ");

    alloc_size = npoints * ndims * sizeof(ptdata[0]);
    assert(alloc_size == (hsize_t) ((size_t) alloc_size)); /*check for overflow*/
    if(NULL == (ptdata = (hsize_t *)HDmalloc((size_t) alloc_size)))
        HGOTO_ERROR(dimension_break, H5E_tools_min_id_g, "Could not allocate buffer for ptdata");

    H5_CHECK_OVERFLOW(npoints, hssize_t, hsize_t);
    if(H5Sget_select_elem_pointlist(region_space, (hsize_t) 0, (hsize_t) npoints, ptdata) < 0)
        HGOTO_ERROR(dimension_break, H5E_tools_min_id_g, "H5Sget_select_elem_pointlist failed");

    for (indx = 0; indx < npoints; indx++) {
        int loop_indx;

        h5tools_str_append(buffer, info->dset_ptformat_pre,
                            indx ? "," OPTIONAL_LINE_BREAK " " : "", (unsigned long) indx);

        for (loop_indx = 0; loop_indx < ndims; loop_indx++)
            h5tools_str_append(buffer, "%s%lu", loop_indx ? "," : "(",
                                (unsigned long) (ptdata[indx * ndims + loop_indx]));

        h5tools_str_append(buffer, ")");
    } /* end for (indx = 0; indx < npoints; indx++) */

    dimension_break = h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols, region_elmt_counter, elmt_counter);
    /* Render the region datatype info and indices element end */

    ctx->need_prefix = TRUE;

    if((dtype = H5Dget_type(region_id)) < 0)
        HGOTO_ERROR(dimension_break, H5E_tools_min_id_g, "H5Dget_type failed");

    if((type_id = H5Tget_native_type(dtype, H5T_DIR_DEFAULT)) < 0)
        HGOTO_ERROR(dimension_break, H5E_tools_min_id_g, "H5Tget_native_type failed");

    /* Render the datatype element begin */
    h5tools_str_reset(buffer);
    h5tools_str_append(buffer, "%s %s ",
                        h5tools_dump_header_format->datatypebegin,
                        h5tools_dump_header_format->datatypeblockbegin);

    h5tools_print_datatype(buffer, info, ctx, dtype);

    if (HDstrlen(h5tools_dump_header_format->datatypeblockend)) {
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->datatypeblockend);
        if (HDstrlen(h5tools_dump_header_format->datatypeend))
            h5tools_str_append(buffer, " ");
    }
    if (HDstrlen(h5tools_dump_header_format->datatypeend))
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->datatypeend);

    dimension_break = h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols, region_elmt_counter, elmt_counter);
    /* Render the datatype element end */

    ctx->need_prefix = TRUE;

    /* Render the dataspace element begin */
    h5tools_str_reset(buffer);

    ctx->need_prefix = TRUE;
    h5tools_str_append(buffer, "%s ", h5tools_dump_header_format->dataspacebegin);

    h5tools_print_dataspace(buffer, region_space);

    if (HDstrlen(h5tools_dump_header_format->dataspaceblockend)) {
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->dataspaceblockend);
        if (HDstrlen(h5tools_dump_header_format->dataspaceend))
            h5tools_str_append(buffer, " ");
    }
    if (HDstrlen(h5tools_dump_header_format->dataspaceend))
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->dataspaceblockend);

    dimension_break = h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols, region_elmt_counter, elmt_counter);
    /* Render the dataspace element end */

    ctx->need_prefix = TRUE;

    /* Render the databegin element begin */
    h5tools_str_reset(buffer);

    h5tools_str_append(buffer, "%s %s ",
                        h5tools_dump_header_format->databegin,
                        h5tools_dump_header_format->datablockbegin);

    dimension_break = h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols, region_elmt_counter, elmt_counter);
    /* Render the databegin element end */

    ctx->need_prefix = TRUE;

    h5tools_print_region_data_points(region_space, region_id,
            rawdatastream, info, *ctx, buffer, ncols, ndims, type_id, npoints, ptdata);

 done:
    free(ptdata);

    if(H5Tclose(type_id) < 0)
        HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tclose failed");

    if(H5Tclose(dtype) < 0)
        HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tclose failed");

    ctx->need_prefix = TRUE;

    /* Render the dataend element begin */
    h5tools_str_reset(buffer);
    h5tools_str_append(buffer, "%s %s ",
                        h5tools_dump_header_format->dataend,
                        h5tools_dump_header_format->datablockend);
    dimension_break = h5tools_render_element(stream, info, ctx, buffer, curr_pos,
                                              ncols, region_elmt_counter, elmt_counter);
    /* Render the dataend element end*/

    ctx->indent_level--;
    ctx->need_prefix = TRUE;

    /* Render the region } element begin */
    h5tools_str_reset(buffer);
    h5tools_str_append(buffer, "}");
    dimension_break = h5tools_render_element(stream, info, ctx, buffer, curr_pos,
                                                ncols, region_elmt_counter, elmt_counter);
    /* Render the region } element end */

    H5_LEAVE(dimension_break)
CATCH
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose:     print out the data for a subset of a dataset.
 * Description:
 *
 *  Select a hyperslab from the dataset DSET using the parameters
 *   specified in SSET. Dump this out to STREAM.
 *
 *  Hyperslabs select "count" blocks of size "block", spaced "stride" elements
 *   from each other, starting at coordinate "start".
 *
 * Return:
 *      On success, return SUCCEED. Otherwise, the function returns FAIL.
 *
 * Algorithm
 *
 * The parameters from SSET are translated into temporary
 * variables so that 1 row is printed at a time (getting the coordinate indices
 * at each row).
 * We define the stride, count and block to be 1 in the row dimension to achieve
 * this and advance until all points are printed.
 *
 * The element position is obtained from the matrix according to:
 *       Given an index I(z,y,x) its position from the beginning of an array
 *       of sizes A(size_z, size_y,size_x) is given by
 *       Position of I(z,y,x) = index_z * size_y * size_x
 *                             + index_y * size_x
 *                             + index_x
 *
 *-------------------------------------------------------------------------
 */
static herr_t
h5tools_print_simple_subset(FILE *stream, const h5tool_format_t *info, h5tools_context_t *ctx,
                           hid_t dset, hid_t p_type, struct subset_t *sset,
                           hid_t f_space, hsize_t hyperslab_count,
                           hsize_t *temp_start,/* start inside offset count loop */
                           hsize_t *temp_count,/* count inside offset count loop  */
                           hsize_t *temp_block,/* block size used in loop  */
                           hsize_t *temp_stride,/* stride size used in loop  */
                           hsize_t *total_size,/* total size of dataset */
                           unsigned int row_dim/* index of row_counter dimension */)
{
    HERR_INIT(herr_t, SUCCEED)
    size_t            i;                       /* counters  */
    size_t            j;                       /* counters  */
    hsize_t           zero[1] = {0};           /* vector of zeros */
    unsigned int      flags;                   /* buffer extent flags */
    hsize_t           elmtno;                  /* elemnt index  */
    hsize_t           low[H5S_MAX_RANK];       /* low bound of hyperslab */
    hsize_t           high[H5S_MAX_RANK];      /* higher bound of hyperslab */
    size_t            p_type_nbytes;           /* size of memory type */
    hsize_t           sm_size[H5S_MAX_RANK];   /* stripmine size */
    hsize_t           sm_nbytes;               /* bytes per stripmine */
    hssize_t          ssm_nelmts;              /* elements per stripmine*/
    hsize_t           sm_nelmts;               /* elements per stripmine*/
    unsigned char    *sm_buf = NULL;           /* buffer for raw data */
    hid_t             sm_space = -1;           /* stripmine data space */
    hsize_t           size_row_block;          /* size for blocks along rows */
    hsize_t           row_counter = 0;

    /* VL data special information */
    unsigned int        vl_data = 0; /* contains VL datatypes */

    if ((size_t) ctx->ndims > NELMTS(sm_size))
        H5E_THROW(FAIL, H5E_tools_min_id_g, "ndims and sm_size comparision failed");

    if (ctx->ndims > 0)
        init_acc_pos(ctx, total_size);

    size_row_block = sset->block.data[row_dim];

    /* Check if we have VL data in the dataset's datatype */
    if (h5tools_detect_vlen_str(p_type) == TRUE)
        vl_data = TRUE;
    if (H5Tdetect_class(p_type, H5T_VLEN) == TRUE)
        vl_data = TRUE;

    /* display loop */
    for (; hyperslab_count > 0; temp_start[row_dim] += temp_stride[row_dim], hyperslab_count--) {
        /* jump rows if size of block exceeded
         cases where block > 1 only and stride > block */
        if (size_row_block > 1
                && row_counter == size_row_block
                && sset->stride.data[row_dim] > sset->block.data[row_dim]) {

            hsize_t increase_rows = sset->stride.data[row_dim] - sset->block.data[row_dim];
            temp_start[row_dim] += increase_rows;
            row_counter = 0;
        }

        row_counter++;

        /* calculate the potential number of elements we're going to print */
        if(H5Sselect_hyperslab(f_space, H5S_SELECT_SET, temp_start, temp_stride, temp_count, temp_block) < 0)
            H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Sselect_hyperslab failed");

        if((ssm_nelmts = H5Sget_select_npoints(f_space)) < 0)
            H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Sget_select_npoints failed");
        sm_nelmts = (hsize_t)ssm_nelmts;

        if (sm_nelmts > 0) {
            /*
             * determine the strip mine size and allocate a buffer. the strip mine is
             * a hyperslab whose size is manageable.
             */
            if((sm_nbytes = p_type_nbytes = H5Tget_size(p_type)) == 0)
                H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Tget_size failed");

            if (ctx->ndims > 0)
                for (i = ctx->ndims; i > 0; --i) {
                    hsize_t size = H5TOOLS_BUFSIZE / sm_nbytes;
                    if (size == 0) /* datum size > H5TOOLS_BUFSIZE */
                        size = 1;
                    sm_size[i - 1] = MIN(total_size[i - 1], size);
                    sm_nbytes *= sm_size[i - 1];
                    assert(sm_nbytes > 0);
                }

            assert(sm_nbytes == (hsize_t) ((size_t) sm_nbytes)); /*check for overflow*/
            if(NULL == (sm_buf = (unsigned char *)HDmalloc((size_t) sm_nelmts * p_type_nbytes)))
                H5E_THROW(FAIL, H5E_tools_min_id_g, "Could not allocate buffer for strip-mine");

            if((sm_space = H5Screate_simple(1, &sm_nelmts, NULL)) < 0)
                H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Screate_simple failed");

            if(H5Sselect_hyperslab(sm_space, H5S_SELECT_SET, zero, NULL, &sm_nelmts, NULL) < 0)
                H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Sselect_hyperslab failed");

            /* read the data */
            if(H5Dread(dset, p_type, sm_space, f_space, H5P_DEFAULT, sm_buf) < 0)
                H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Dread failed");

            /* print the data */
            flags = START_OF_DATA;

            if (hyperslab_count == 1)
                flags |= END_OF_DATA;

            for (i = 0; i < ctx->ndims; i++)
                ctx->p_max_idx[i] = ctx->p_min_idx[i] + MIN(total_size[i], sm_size[i]);

            /* print array indices. get the lower bound of the hyperslab and calulate
             the element position at the start of hyperslab */
            if(H5Sget_select_bounds(f_space, low, high) < 0)
                H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Sget_select_bounds failed");

            elmtno = 0;
            for (i = 0; i < (size_t) ctx->ndims - 1; i++) {
                hsize_t offset = 1; /* accumulation of the previous dimensions */
                for (j = i + 1; j < (size_t) ctx->ndims; j++)
                    offset *= total_size[j];
                elmtno += low[i] * offset;
            }
            elmtno += low[ctx->ndims - 1];

            /* initialize the current stripmine position; this is necessary to print the array
             indices */
            ctx->sm_pos = elmtno;

            h5tools_dump_simple_data(stream, info, dset, ctx, flags, sm_nelmts, p_type, sm_buf);

            /* Reclaim any VL memory, if necessary */
            if (vl_data)
                H5Dvlen_reclaim(p_type, sm_space, H5P_DEFAULT, sm_buf);

            if(H5Sclose(sm_space) < 0)
                H5E_THROW(H5E_tools_g, H5E_tools_min_id_g, "H5Sclose failed");
            if(sm_buf)
                HDfree(sm_buf);
            sm_buf = NULL;
        }
        else
            H5E_THROW(SUCCEED, H5E_tools_min_id_g, "nothing to print");
        /* we need to jump to next line and update the index */
        ctx->need_prefix = 1;

        ctx->continuation++;

    } /* hyperslab_count loop */

CATCH
    if(sm_buf)
        HDfree(sm_buf);

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose:     print out the data for a subset of a dataset.
 * Description:
 *
 *  Select a hyperslab from the dataset DSET using the parameters
 *   specified in SSET. Dump this out to STREAM.
 *
 *  Hyperslabs select "count" blocks of size "block", spaced "stride" elements
 *   from each other, starting at coordinate "start".
 *
 * Return:
 *      On success, return SUCCEED. Otherwise, the function returns FAIL.
 *
 * Algorithm
 *
 * The parameters from SSET are translated into temporary
 * variables so that 1 row is printed at a time (getting the coordinate indices
 * at each row).
 * We define the stride, count and block to be 1 in the row dimension to achieve
 * this and advance until all points are printed.
 *
 * The element position is obtained from the matrix according to:
 *       Given an index I(z,y,x) its position from the beginning of an array
 *       of sizes A(size_z, size_y,size_x) is given by
 *       Position of I(z,y,x) = index_z * size_y * size_x
 *                             + index_y * size_x
 *                             + index_x
 *
 *-------------------------------------------------------------------------
 */
static herr_t
h5tools_display_simple_subset(FILE *stream, const h5tool_format_t *info, h5tools_context_t *ctx,
                           hid_t dset, hid_t p_type, struct subset_t *sset,
                           hid_t f_space, hsize_t *total_size)
{
    size_t            i;                       /* counters  */
    hsize_t           n;                       /* counters  */
    hsize_t           count;                   /* hyperslab count */
    hsize_t           outer_count;             /* offset count */
    unsigned int      row_dim;                 /* index of row_counter dimension */
    int               current_outer_dim;       /* dimension for start */
    hsize_t           temp_start[H5S_MAX_RANK];/* temporary start inside offset count loop */
    hsize_t           max_start[H5S_MAX_RANK]; /* maximum start inside offset count loop */
    hsize_t           temp_count[H5S_MAX_RANK];/* temporary count inside offset count loop  */
    hsize_t           temp_block[H5S_MAX_RANK];/* temporary block size used in loop  */
    hsize_t           temp_stride[H5S_MAX_RANK];/* temporary stride size used in loop  */
    int               reset_dim;
    herr_t            ret_value = SUCCEED;

    if (ctx->ndims == 1)
        row_dim = 0;
    else
        row_dim = ctx->ndims - 2;

    /* get the offset count */
    outer_count = 1;
    if (ctx->ndims > 2)
        for (i = 0; i < (size_t) ctx->ndims - 2; i++) {
            /* consider block size */
            outer_count = outer_count * sset->count.data[i] * sset->block.data[i];

        }

    /* initialize temporary start, count and maximum start */
    for (i = 0; i < (size_t) ctx->ndims; i++) {
        temp_start[i] = sset->start.data[i];
        temp_count[i] = sset->count.data[i];
        temp_block[i] = sset->block.data[i];
        temp_stride[i] = sset->stride.data[i];
        max_start[i] = 0;
    }

    if (ctx->ndims > 2) {
        for (i = 0; i < (size_t) ctx->ndims - 2; i++) {
            max_start[i] = temp_start[i] + sset->count.data[i];
            temp_count[i] = 1;

        }
    }

    /* offset loop */
    for (n = 0; n < outer_count; n++) {
        /* number of read iterations in inner loop, read by rows, to match 2D display */
        if (ctx->ndims > 1) {

            /* count is the number of iterations to display all the rows,
             the block size count times */
            count = sset->count.data[row_dim] * sset->block.data[row_dim];

            /* always 1 row_counter at a time, that is a block of size 1, 1 time */
            temp_count[row_dim] = 1;
            temp_block[row_dim] = 1;

            /* advance 1 row_counter at a time  */
            if (sset->block.data[row_dim] > 1)
                temp_stride[row_dim] = 1;

        }
        /* for the 1D case */
        else {
            count = 1;
        }

        h5tools_print_simple_subset(stream, info, ctx, dset, p_type, sset,
                                   f_space, count, temp_start, temp_count,
                                   temp_block, temp_stride, total_size, row_dim);

        if (ctx->ndims > 2) {
            /* dimension for start */
            current_outer_dim = (ctx->ndims - 2) - 1;

            /* set start to original from current_outer_dim up */
            for (i = current_outer_dim + 1; i < ctx->ndims; i++) {
                temp_start[i] = sset->start.data[i];
            }

            /* increment start dimension */
            do {
                reset_dim = 0;
                temp_start[current_outer_dim]++;
                if (temp_start[current_outer_dim] >= max_start[current_outer_dim]) {
                    temp_start[current_outer_dim] = sset->start.data[current_outer_dim];

                    /* consider block */
                    if (sset->block.data[current_outer_dim] > 1)
                        temp_start[current_outer_dim]++;

                    current_outer_dim--;
                    reset_dim = 1;
                }
            } while (current_outer_dim >= 0 && reset_dim);

        } /* ctx.ndims > 1 */

    } /* outer_count */

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose:     Dump out a subset of a dataset.
 * Description:
 *
 *  Select a hyperslab from the dataset DSET using the parameters
 *   specified in SSET. Dump this out to STREAM.
 *
 *  Hyperslabs select "count" blocks of size "block", spaced "stride" elements
 *   from each other, starting at coordinate "start".
 *
 * Return:
 *      On success, return SUCCEED. Otherwise, the function returns FAIL.
 *
 * Original programmer:
 *      Bill Wendling, Wednesday, March 07, 2001
 *
 * Rewritten with modified algorithm by:
 *      Pedro Vicente, Wednesday, January 16, 2008, contributions from Quincey Koziol
 *
 * Algorithm
 *
 * In a inner loop, the parameters from SSET are translated into temporary
 * variables so that 1 row is printed at a time (getting the coordinate indices
 * at each row).
 * We define the stride, count and block to be 1 in the row dimension to achieve
 * this and advance until all points are printed.
 * An outer loop for cases where dimensionality is greater than 2D is made.
 * In each iteration, the 2D block is displayed in the inner loop. The remaining
 * slower dimensions above the first 2 are incremented one at a time in the outer loop
 *
 * The element position is obtained from the matrix according to:
 *       Given an index I(z,y,x) its position from the beginning of an array
 *       of sizes A(size_z, size_y,size_x) is given by
 *       Position of I(z,y,x) = index_z * size_y * size_x
 *                             + index_y * size_x
 *                             + index_x
 *
 *-------------------------------------------------------------------------
 */
static herr_t
h5tools_dump_simple_subset(FILE *stream, const h5tool_format_t *info, hid_t dset,
                           hid_t p_type, struct subset_t *sset, int indentlevel)
{
    HERR_INIT(herr_t, SUCCEED)
    int               sndims;
    hid_t             f_space = -1;            /* file data space */
    size_t            i;                       /* counters  */
    hsize_t           total_size[H5S_MAX_RANK];/* total size of dataset*/
    h5tools_context_t ctx;                     /* print context  */

    if((f_space = H5Dget_space(dset)) < 0)
        H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Dget_space failed");

    /*
     * check that everything looks okay. the dimensionality must not be too
     * great and the dimensionality of the items selected for printing must
     * match the dimensionality of the dataset.
     */
    memset(&ctx, 0, sizeof(ctx));
    ctx.indent_level = indentlevel;
    ctx.need_prefix = 1;
    if((sndims = H5Sget_simple_extent_ndims(f_space)) < 0)
        H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Sget_simple_extent_ndims failed");
    ctx.ndims = (unsigned)sndims;

    /* assume entire data space to be printed */
    if (ctx.ndims > 0)
        for (i = 0; i < (size_t) ctx.ndims; i++)
            ctx.p_min_idx[i] = 0;

    if(H5Sget_simple_extent_dims(f_space, total_size, NULL) < 0)
        H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Sget_simple_extent_dims failed");
    ctx.size_last_dim = total_size[ctx.ndims - 1];

    h5tools_display_simple_subset(stream, info, &ctx, dset, p_type, sset, f_space, total_size);

    /* Terminate the output */
    if (ctx.cur_column) {
        fputs(OPT(info->line_suf, ""), stdout);
        putc('\n', stdout); 
        fputs(OPT(info->line_sep, ""), stdout);
    }

CATCH
    if(f_space >= 0 && H5Sclose(f_space) < 0)
        H5E_THROW(H5E_tools_g, H5E_tools_min_id_g, "H5Sclose failed");

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose: Print some values from a dataset with a simple data space.
 * Description:
 *      This is a special case of h5tools_dump_dset(). This function only
 *      intended for dumping datasets -- it does strip mining and some other
 *      things which are unnecessary for smaller objects such as attributes
 *      (to print small objects like attributes simply read the attribute and
 *      call h5tools_dump_simple_mem()).
 * Return:
 *      On success, the function returns SUCCEED. Otherwise, the function
 *      returns FAIL.
 *-------------------------------------------------------------------------
 */
static int
h5tools_dump_simple_dset(FILE *stream, const h5tool_format_t *info,
                         hid_t dset, hid_t p_type, int indentlevel)
{
    hid_t               f_space;                  /* file data space */
    hsize_t             elmtno;                   /* counter  */
    size_t              i;                        /* counter  */
    int                 carry;                    /* counter carry value */
    hsize_t             zero[8];                  /* vector of zeros */
    unsigned int        flags;                    /* buffer extent flags */
    hsize_t             total_size[H5S_MAX_RANK]; /* total size of dataset*/

    /* Print info */
    h5tools_context_t   ctx;                      /* print context  */
    size_t              p_type_nbytes;            /* size of memory type */
    hsize_t             p_nelmts;                 /* total selected elmts */

    /* Stripmine info */
    hsize_t             sm_size[H5S_MAX_RANK];    /* stripmine size */
    hsize_t             sm_nbytes;                /* bytes per stripmine */
    hsize_t             sm_nelmts;                /* elements per stripmine*/
    unsigned char      *sm_buf = NULL;            /* buffer for raw data */
    hid_t               sm_space;                 /* stripmine data space */

    /* Hyperslab info */
    hsize_t             hs_offset[H5S_MAX_RANK];  /* starting offset */
    hsize_t             hs_size[H5S_MAX_RANK];    /* size this pass */
    hsize_t             hs_nelmts;                /* elements in request */

    /* VL data special information */
    unsigned int        vl_data = 0; /* contains VL datatypes */

    f_space = H5Dget_space(dset);

    if (f_space == FAIL)
        return FAIL;

    /*
     * Check that everything looks okay. The dimensionality must not be too
     * great and the dimensionality of the items selected for printing must
     * match the dimensionality of the dataset.
     */
    memset(&ctx, 0, sizeof(ctx));
    ctx.ndims = H5Sget_simple_extent_ndims(f_space);

    if ((size_t)ctx.ndims > NELMTS(sm_size)) {
        H5Sclose(f_space);
        return FAIL;
    }

    ctx.indent_level = indentlevel;
    ctx.need_prefix = 1;

    /* Assume entire data space to be printed */
    if (ctx.ndims > 0)
        for (i = 0; i < (size_t)ctx.ndims; i++)
            ctx.p_min_idx[i] = 0;

    H5Sget_simple_extent_dims(f_space, total_size, NULL);

    /* calculate the number of elements we're going to print */
    p_nelmts = 1;

    if (ctx.ndims > 0) {
        for (i = 0; i < ctx.ndims; i++)
            p_nelmts *= total_size[i];
        ctx.size_last_dim = (total_size[ctx.ndims - 1]);
    } /* end if */
    else
        ctx.size_last_dim = 0;

    if (p_nelmts == 0) {
        /* nothing to print */
        H5Sclose(f_space);
        return SUCCEED;
    }

    /* Check if we have VL data in the dataset's datatype */
    if (h5tools_detect_vlen_str(p_type) == TRUE)
        vl_data = TRUE;
    if (H5Tdetect_class(p_type, H5T_VLEN) == TRUE)
        vl_data = TRUE;
 
    /*
     * Determine the strip mine size and allocate a buffer. The strip mine is
     * a hyperslab whose size is manageable.
     */
    sm_nbytes = p_type_nbytes = H5Tget_size(p_type);

    if (ctx.ndims > 0) {
        for (i = ctx.ndims; i > 0; --i) {
            hsize_t size = H5TOOLS_BUFSIZE / sm_nbytes;
            if ( size == 0) /* datum size > H5TOOLS_BUFSIZE */
                size = 1;
            sm_size[i - 1] = MIN(total_size[i - 1], size);
            sm_nbytes *= sm_size[i - 1];
            assert(sm_nbytes > 0);
        }
    }

    assert(sm_nbytes == (hsize_t)((size_t)sm_nbytes)); /*check for overflow*/
    sm_buf = (unsigned char *)HDmalloc((size_t)sm_nbytes);

    sm_nelmts = sm_nbytes / p_type_nbytes;
    sm_space = H5Screate_simple(1, &sm_nelmts, NULL);

    if (ctx.ndims > 0)
        init_acc_pos(&ctx, total_size);

    /* The stripmine loop */
    memset(hs_offset, 0, sizeof hs_offset);
    memset(zero, 0, sizeof zero);

    for (elmtno = 0; elmtno < p_nelmts; elmtno += hs_nelmts) {
        /* Calculate the hyperslab size */
        if (ctx.ndims > 0) {
            for (i = 0, hs_nelmts = 1; i < ctx.ndims; i++) {
                hs_size[i] = MIN(total_size[i] - hs_offset[i], sm_size[i]);
                ctx.p_max_idx[i] = ctx.p_min_idx[i] + hs_size[i];
                hs_nelmts *= hs_size[i];
            }

            H5Sselect_hyperslab(f_space, H5S_SELECT_SET, hs_offset, NULL, hs_size, NULL);
            H5Sselect_hyperslab(sm_space, H5S_SELECT_SET, zero, NULL, &hs_nelmts, NULL);
        }
        else {
            H5Sselect_all(f_space);
            H5Sselect_all(sm_space);
            hs_nelmts = 1;
        }

        /* Read the data */
        if (H5Dread(dset, p_type, sm_space, f_space, H5P_DEFAULT, sm_buf) < 0) {
            H5Sclose(f_space);
            H5Sclose(sm_space);
            free(sm_buf);
            return FAIL;
        }

        /* Print the data */
        flags = (elmtno == 0) ? START_OF_DATA : 0;
        flags |= ((elmtno + hs_nelmts) >= p_nelmts) ? END_OF_DATA : 0;

        /* initialize the current stripmine position; this is necessary to print the array
         indices */
        ctx.sm_pos = elmtno;

        h5tools_dump_simple_data(stream, info, dset, &ctx, flags, hs_nelmts, p_type, sm_buf);

        /* Reclaim any VL memory, if necessary */
        if (vl_data)
            H5Dvlen_reclaim(p_type, sm_space, H5P_DEFAULT, sm_buf);

        /* Calculate the next hyperslab offset */
        for (i = ctx.ndims, carry = 1; i > 0 && carry; --i) {
            ctx.p_min_idx[i - 1] = ctx.p_max_idx[i - 1];
            hs_offset[i - 1] += hs_size[i - 1];

            if (hs_offset[i - 1] == total_size[i - 1])
                hs_offset[i - 1] = 0;
            else
                carry = 0;
        }

        ctx.continuation++;
    }

    /* Terminate the output */
    if (ctx.cur_column) {
        fputs(OPT(info->line_suf, ""), stream);
        putc('\n', stream);
        fputs(OPT(info->line_sep, ""), stream);
    }

    H5Sclose(sm_space);
    H5Sclose(f_space);

    HDfree(sm_buf);

    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function: h5tools_dump_simple_mem
 *
 * Purpose: Print some values from memory with a simple data space.
 *  This is a special case of h5tools_dump_mem().
 *
 * Return: Success:    SUCCEED
 *         Failure:    FAIL
 *
 *-------------------------------------------------------------------------
 */
static int
h5tools_dump_simple_mem(FILE *stream, const h5tool_format_t *info, hid_t obj_id,
                        hid_t type, hid_t space, void *mem, int indentlevel)
{
    int                 i; /*counters  */
    hsize_t        nelmts; /*total selected elmts */
    h5tools_context_t ctx; /*printing context */

    /*
     * Check that everything looks okay.  The dimensionality must not be too
     * great and the dimensionality of the items selected for printing must
     * match the dimensionality of the dataset.
     */
    memset(&ctx, 0, sizeof(ctx));
    ctx.ndims = H5Sget_simple_extent_ndims(space);

    if ((size_t) ctx.ndims > NELMTS(ctx.p_min_idx))
        return FAIL;

    ctx.indent_level = indentlevel;
    ctx.need_prefix = 1;

    /* Assume entire data space to be printed */
    for (i = 0; i < ctx.ndims; i++)
        ctx.p_min_idx[i] = 0;

    H5Sget_simple_extent_dims(space, ctx.p_max_idx, NULL);

    for (i = 0, nelmts = 1; ctx.ndims != 0 && i < ctx.ndims; i++)
        nelmts *= ctx.p_max_idx[i] - ctx.p_min_idx[i];

    if (nelmts == 0)
        return SUCCEED; /*nothing to print*/
    if (ctx.ndims > 0) {
        assert(ctx.p_max_idx[ctx.ndims - 1] == (hsize_t) ((int) ctx.p_max_idx[ctx.ndims - 1]));
        ctx.size_last_dim = (int) (ctx.p_max_idx[ctx.ndims - 1]);
    } /* end if */
    else
        ctx.size_last_dim = 0;

    if (ctx.ndims > 0)
        init_acc_pos(&ctx, ctx.p_max_idx);

    /* Print it */
    h5tools_dump_simple_data(stream, info, obj_id, &ctx, START_OF_DATA | END_OF_DATA, nelmts, type, mem);

    /* Terminate the output */
    if (ctx.cur_column) {
        fputs(OPT(info->line_suf, ""), stream);
        putc('\n', stream);
        fputs(OPT(info->line_sep, ""), stream);
    }

    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function: h5tools_dump_dset
 *
 * Purpose: Print some values from a dataset DSET to the file STREAM
 *  after converting all types to P_TYPE (which should be a
 *  native type).  If P_TYPE is a negative value then it will be
 *  computed from the dataset type using only native types.
 *
 * Note: This function is intended only for datasets since it does
 *  some things like strip mining which are unnecessary for
 *  smaller objects such as attributes. The easiest way to print
 *  small objects is to read the object into memory and call
 *  h5tools_dump_mem().
 *
 * Return: Success:    SUCCEED
 *         Failure:    FAIL
 *
 * Modifications:
 *   Robb Matzke, 1999-06-07
 *      If info->raw is set then the memory datatype will be the same
 *      as the file datatype.
 *
 *  Bill Wendling, 2001-02-27
 *      Renamed to ``h5tools_dump_dset'' and added the subsetting
 *      parameter.
 *
 *-------------------------------------------------------------------------
 */
int
h5tools_dump_dset(FILE *stream, const h5tool_format_t *info, hid_t dset,
                  hid_t _p_type, struct subset_t *sset, int indentlevel)
{
    hid_t     f_space;
    hid_t     p_type = _p_type;
    hid_t     f_type;
    H5S_class_t space_type;
    int       status = FAIL;
    h5tool_format_t info_dflt;
    /* Use default values */
    if (!stream)
        stream = stdout;

    if (!info) {
        memset(&info_dflt, 0, sizeof info_dflt);
        info = &info_dflt;
    }

    if (p_type < 0) {
        f_type = H5Dget_type(dset);

        if (info->raw || bin_form == 1)
            p_type = H5Tcopy(f_type);
        else if (bin_form == 2)
            p_type = h5tools_get_little_endian_type(f_type);
        else if (bin_form == 3)
            p_type = h5tools_get_big_endian_type(f_type);
        else
            p_type = h5tools_get_native_type(f_type);

        H5Tclose(f_type);

        if (p_type < 0)
            goto done;
    }

    /* Check the data space */
    f_space = H5Dget_space(dset);

    space_type = H5Sget_simple_extent_type(f_space);

    /* Print the data */
    if (space_type == H5S_SIMPLE || space_type == H5S_SCALAR) {
        if(!sset)
            status = h5tools_dump_simple_dset(rawdatastream, info, dset, p_type, indentlevel);
        else
            status = h5tools_dump_simple_subset(rawdatastream, info, dset, p_type, sset, indentlevel);
    }
    else
        /* space is H5S_NULL */
        status = SUCCEED;

    /* Close the dataspace */
    H5Sclose(f_space);

done:
    if (p_type != _p_type)
        H5Tclose(p_type);

    return status;
}

/*-------------------------------------------------------------------------
 * Function: h5tools_dump_mem
 *
 * Purpose: Displays the data contained in MEM. MEM must have the
 *  specified data TYPE and SPACE.  Currently only simple data
 *  spaces are allowed and only the `all' selection.
 *
 * Return: Success:    SUCCEED
 *         Failure:    FAIL
 *
 *-------------------------------------------------------------------------
 */
int
h5tools_dump_mem(FILE *stream, const h5tool_format_t *info, hid_t obj_id, hid_t type,
                 hid_t space, void *mem, int indentlevel)
{
    HERR_INIT(int, SUCCEED)
    h5tool_format_t    info_dflt;

    /* Use default values */
    if (!stream)
        stream = stdout;

    if (!info) {
        memset(&info_dflt, 0, sizeof(info_dflt));
        info = &info_dflt;
    }

    /* Check the data space */
    if (H5Sis_simple(space) <= 0)
        H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Sis_simple failed")

     H5_LEAVE(h5tools_dump_simple_mem(stream, info, obj_id, type, space, mem, indentlevel))

CATCH
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    print_datatype
 *
 * Purpose:     print the datatype.
 *
 * Return:      void
 *
 * In/Out:      h5tools_str_t *buffer
 *              h5tools_context_t *ctx
 *
 *-------------------------------------------------------------------------
 */
int
h5tools_print_datatype(h5tools_str_t *buffer, const h5tool_format_t *info,
        h5tools_context_t *ctx, hid_t type)
{
    HERR_INIT(int, SUCCEED)
    char        *mname;
    hid_t        mtype, str_type;
    int          snmembers;
    unsigned     nmembers;
    int          sndims;
    unsigned     i;
    size_t       size = 0;
    hsize_t      dims[H5TOOLS_DUMP_MAX_RANK];
    H5T_str_t    str_pad;
    H5T_cset_t   cset;
    H5T_order_t  order;
    H5T_class_t  type_class;
    hid_t        super;
    hid_t        tmp_type;
    htri_t       is_vlstr = FALSE;
    const char  *order_s = NULL; /* byte order string */
    H5T_sign_t   sign;           /* sign scheme value */
    const char  *sign_s = NULL;  /* sign scheme string */

    if((type_class = H5Tget_class(type)) < 0)
        H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Tget_class failed");
    switch (type_class) {
    case H5T_INTEGER:
        if (H5Tequal(type, H5T_STD_I8BE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_I8BE");
        }
        else if (H5Tequal(type, H5T_STD_I8LE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_I8LE");
        }
        else if (H5Tequal(type, H5T_STD_I16BE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_I16BE");
        }
        else if (H5Tequal(type, H5T_STD_I16LE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_I16LE");
        }
        else if (H5Tequal(type, H5T_STD_I32BE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_I32BE");
        }
        else if (H5Tequal(type, H5T_STD_I32LE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_I32LE");
        }
        else if (H5Tequal(type, H5T_STD_I64BE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_I64BE");
        }
        else if (H5Tequal(type, H5T_STD_I64LE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_I64LE");
        }
        else if (H5Tequal(type, H5T_STD_U8BE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_U8BE");
        }
        else if (H5Tequal(type, H5T_STD_U8LE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_U8LE");
        }
        else if (H5Tequal(type, H5T_STD_U16BE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_U16BE");
        }
        else if (H5Tequal(type, H5T_STD_U16LE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_U16LE");
        }
        else if (H5Tequal(type, H5T_STD_U32BE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_U32BE");
        }
        else if (H5Tequal(type, H5T_STD_U32LE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_U32LE");
        }
        else if (H5Tequal(type, H5T_STD_U64BE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_U64BE");
        }
        else if (H5Tequal(type, H5T_STD_U64LE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_U64LE");
        }
        else if (H5Tequal(type, H5T_NATIVE_SCHAR) == TRUE) {
            h5tools_str_append(buffer, "H5T_NATIVE_SCHAR");
        }
        else if (H5Tequal(type, H5T_NATIVE_UCHAR) == TRUE) {
            h5tools_str_append(buffer, "H5T_NATIVE_UCHAR");
        }
        else if (H5Tequal(type, H5T_NATIVE_SHORT) == TRUE) {
            h5tools_str_append(buffer, "H5T_NATIVE_SHORT");
        }
        else if (H5Tequal(type, H5T_NATIVE_USHORT) == TRUE) {
            h5tools_str_append(buffer, "H5T_NATIVE_USHORT");
        }
        else if (H5Tequal(type, H5T_NATIVE_INT) == TRUE) {
            h5tools_str_append(buffer, "H5T_NATIVE_INT");
        }
        else if (H5Tequal(type, H5T_NATIVE_UINT) == TRUE) {
            h5tools_str_append(buffer, "H5T_NATIVE_UINT");
        }
        else if (H5Tequal(type, H5T_NATIVE_LONG) == TRUE) {
            h5tools_str_append(buffer, "H5T_NATIVE_LONG");
        }
        else if (H5Tequal(type, H5T_NATIVE_ULONG) == TRUE) {
            h5tools_str_append(buffer, "H5T_NATIVE_ULONG");
        }
        else if (H5Tequal(type, H5T_NATIVE_LLONG) == TRUE) {
            h5tools_str_append(buffer, "H5T_NATIVE_LLONG");
        }
        else if (H5Tequal(type, H5T_NATIVE_ULLONG) == TRUE) {
            h5tools_str_append(buffer, "H5T_NATIVE_ULLONG");
        }
        else {

            /* byte order */
            if (H5Tget_size(type) > 1) {
                order = H5Tget_order(type);
                if (H5T_ORDER_LE == order) {
                    order_s = " little-endian";
                }
                else if (H5T_ORDER_BE == order) {
                    order_s = " big-endian";
                }
                else if (H5T_ORDER_VAX == order) {
                    order_s = " mixed-endian";
                }
                else {
                    order_s = " unknown-byte-order";
                }
            }
            else {
                order_s = "";
            }

            /* sign */
            if ((sign = H5Tget_sign(type)) >= 0) {
                if (H5T_SGN_NONE == sign) {
                    sign_s = " unsigned";
                }
                else if (H5T_SGN_2 == sign) {
                    sign_s = "";
                }
                else {
                    sign_s = " unknown-sign";
                }
            }
            else {
                sign_s = " unknown-sign";
            }

            /* print size, order, and sign  */
            h5tools_str_append(buffer, "%lu-bit%s%s integer",
                                (unsigned long) (8 * H5Tget_size(type)), order_s, sign_s);
        }
        break;

    case H5T_FLOAT:
        if (H5Tequal(type, H5T_IEEE_F32BE) == TRUE) {
            h5tools_str_append(buffer, "H5T_IEEE_F32BE");
        }
        else if (H5Tequal(type, H5T_IEEE_F32LE) == TRUE) {
            h5tools_str_append(buffer, "H5T_IEEE_F32LE");
        }
        else if (H5Tequal(type, H5T_IEEE_F64BE) == TRUE) {
            h5tools_str_append(buffer, "H5T_IEEE_F64BE");
        }
        else if (H5Tequal(type, H5T_IEEE_F64LE) == TRUE) {
            h5tools_str_append(buffer, "H5T_IEEE_F64LE");
        }
        else if (H5Tequal(type, H5T_VAX_F32) == TRUE) {
            h5tools_str_append(buffer, "H5T_VAX_F32");
        }
        else if (H5Tequal(type, H5T_VAX_F64) == TRUE) {
            h5tools_str_append(buffer, "H5T_VAX_F64");
        }
        else if (H5Tequal(type, H5T_NATIVE_FLOAT) == TRUE) {
            h5tools_str_append(buffer, "H5T_NATIVE_FLOAT");
        }
        else if (H5Tequal(type, H5T_NATIVE_DOUBLE) == TRUE) {
            h5tools_str_append(buffer, "H5T_NATIVE_DOUBLE");
#if H5_SIZEOF_LONG_DOUBLE !=0
        }
        else if (H5Tequal(type, H5T_NATIVE_LDOUBLE) == TRUE) {
            h5tools_str_append(buffer, "H5T_NATIVE_LDOUBLE");
#endif
        }
        else {

            /* byte order */
            if (H5Tget_size(type) > 1) {
                order = H5Tget_order(type);
                if (H5T_ORDER_LE == order) {
                    order_s = " little-endian";
                }
                else if (H5T_ORDER_BE == order) {
                    order_s = " big-endian";
                }
                else if (H5T_ORDER_VAX == order) {
                    order_s = " mixed-endian";
                }
                else {
                    order_s = " unknown-byte-order";
                }
            }
            else {
                order_s = "";
            }

            /* print size and byte order */
            h5tools_str_append(buffer, "%lu-bit%s floating-point",
                                (unsigned long) (8 * H5Tget_size(type)), order_s);

        }
        break;

    case H5T_TIME:
        h5tools_str_append(buffer, "H5T_TIME: not yet implemented");
        break;

    case H5T_STRING:
        /* Make a copy of type in memory in case when TYPE is on disk, the size
         * will be bigger than in memory.  This makes it easier to compare
         * types in memory. */
        tmp_type = H5Tcopy(type);
        size = H5Tget_size(tmp_type);
        str_pad = H5Tget_strpad(tmp_type);
        cset = H5Tget_cset(tmp_type);
        is_vlstr = H5Tis_variable_str(tmp_type);

        h5tools_str_append(buffer, "H5T_STRING %s\n", h5tools_dump_header_format->strblockbegin);
        ctx->indent_level++;

        if (is_vlstr)
            h5tools_str_append(buffer, "%s H5T_VARIABLE;\n", STRSIZE);
        else
            h5tools_str_append(buffer, "%s %d;\n", STRSIZE, (int) size);

        h5tools_str_append(buffer, "%s ", STRPAD);
        if (str_pad == H5T_STR_NULLTERM)
            h5tools_str_append(buffer, "H5T_STR_NULLTERM;\n");
        else if (str_pad == H5T_STR_NULLPAD)
            h5tools_str_append(buffer, "H5T_STR_NULLPAD;\n");
        else if (str_pad == H5T_STR_SPACEPAD)
            h5tools_str_append(buffer, "H5T_STR_SPACEPAD;\n");
        else
            h5tools_str_append(buffer, "H5T_STR_ERROR;\n");

        h5tools_str_append(buffer, "%s ", CSET);

        if (cset == H5T_CSET_ASCII)
            h5tools_str_append(buffer, "H5T_CSET_ASCII;\n");
        else
            h5tools_str_append(buffer, "unknown_cset;\n");

        str_type = H5Tcopy(H5T_C_S1);
        if (is_vlstr)
            H5Tset_size(str_type, H5T_VARIABLE);
        else
            H5Tset_size(str_type, size);
        H5Tset_cset(str_type, cset);
        H5Tset_strpad(str_type, str_pad);

        h5tools_str_append(buffer, "%s ", CTYPE);

        /* Check C variable-length string first. Are the two types equal? */
        if (H5Tequal(tmp_type, str_type)) {
            h5tools_str_append(buffer, "H5T_C_S1;\n");
            goto done;
        }

        /* Change the endianness and see if they're equal. */
        order = H5Tget_order(tmp_type);
        if (order == H5T_ORDER_LE)
            H5Tset_order(str_type, H5T_ORDER_LE);
        else if (order == H5T_ORDER_BE)
            H5Tset_order(str_type, H5T_ORDER_BE);

        if (H5Tequal(tmp_type, str_type)) {
            h5tools_str_append(buffer, "H5T_C_S1;\n");
            goto done;
        }

        /* If not equal to C variable-length string, check Fortran type. */
        if(H5Tclose(str_type) < 0)
            HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tclose failed");
        str_type = H5Tcopy(H5T_FORTRAN_S1);

        H5Tset_cset(str_type, cset);
        H5Tset_size(str_type, size);
        H5Tset_strpad(str_type, str_pad);

        /* Are the two types equal? */
        if (H5Tequal(tmp_type, str_type)) {
            h5tools_str_append(buffer, "H5T_FORTRAN_S1;\n");
            goto done;
        }

        /* Change the endianness and see if they're equal. */
        order = H5Tget_order(tmp_type);
        if (order == H5T_ORDER_LE)
            H5Tset_order(str_type, H5T_ORDER_LE);
        else if (order == H5T_ORDER_BE)
            H5Tset_order(str_type, H5T_ORDER_BE);

        if (H5Tequal(tmp_type, str_type)) {
            h5tools_str_append(buffer, "H5T_FORTRAN_S1;\n");
            goto done;
        }

        /* Type doesn't match any of above. */
        h5tools_str_append(buffer, "unknown_one_character_type;\n ");

  done:
        if(H5Tclose(str_type) < 0)
            HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tclose failed");
        if(H5Tclose(tmp_type) < 0)
            HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tclose failed");

        ctx->indent_level--;
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->strblockend);
        break;

    case H5T_BITFIELD:
        if (H5Tequal(type, H5T_STD_B8BE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_B8BE");
        }
        else if (H5Tequal(type, H5T_STD_B8LE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_B8LE");
        }
        else if (H5Tequal(type, H5T_STD_B16BE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_B16BE");
        }
        else if (H5Tequal(type, H5T_STD_B16LE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_B16LE");
        }
        else if (H5Tequal(type, H5T_STD_B32BE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_B32BE");
        }
        else if (H5Tequal(type, H5T_STD_B32LE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_B32LE");
        }
        else if (H5Tequal(type, H5T_STD_B64BE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_B64BE");
        }
        else if (H5Tequal(type, H5T_STD_B64LE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_B64LE");
        }
        else {
            h5tools_str_append(buffer, "undefined bitfield");
        }
        break;

    case H5T_OPAQUE:
        h5tools_str_append(buffer, "\n");
        h5tools_str_append(buffer, "H5T_OPAQUE;\n");
        h5tools_str_append(buffer, "OPAQUE_TAG \"%s\";\n", H5Tget_tag(type));
        break;

    case H5T_COMPOUND:
        if((snmembers = H5Tget_nmembers(type)) < 0)
            H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Tget_nmembers failed");
        nmembers = (unsigned)snmembers;

        h5tools_str_append(buffer, "H5T_COMPOUND %s\n", h5tools_dump_header_format->structblockbegin);

        for (i = 0; i < nmembers; i++) {
            mname = H5Tget_member_name(type, i);
            if((mtype = H5Tget_member_type(type, i))>=0) {
                if (H5Tget_class(mtype) == H5T_COMPOUND)
                    ctx->indent_level++;

                h5tools_print_datatype(buffer, info, ctx, mtype);

                if (H5Tget_class(mtype) == H5T_COMPOUND)
                    ctx->indent_level--;

                h5tools_str_append(buffer, " \"%s\";\n", mname);
                if(H5Tclose(mtype) < 0)
                    HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tclose failed");
            }
            else
                HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tget_member_type failed");
            free(mname);
        }

        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->structblockend);
        break;

    case H5T_REFERENCE:
        h5tools_str_append(buffer, "H5T_REFERENCE");
        if(region_output) {
            if (H5Tequal(type, H5T_STD_REF_DSETREG) == TRUE) {
                h5tools_str_append(buffer, " { H5T_STD_REF_DSETREG }");
            }
            else {
                h5tools_str_append(buffer, " { H5T_STD_REF_OBJECT }");
            }
        }
        break;

    case H5T_ENUM:
        if((super = H5Tget_super(type)) < 0)
            H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Tget_nmembers failed");

        h5tools_str_append(buffer, "H5T_ENUM %s\n", h5tools_dump_header_format->enumblockbegin);
        ctx->indent_level++;

        h5tools_print_datatype(buffer, info, ctx, super);
        if(H5Tclose(super) < 0)
            HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tclose failed");

        h5tools_str_append(buffer, ";\n");
        h5tools_print_enum(buffer, type);

        ctx->indent_level--;
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->enumblockend);

        break;

    case H5T_VLEN:
        if((super = H5Tget_super(type)) < 0)
            H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Tget_nmembers failed");

        h5tools_str_append(buffer, "H5T_VLEN %s ", h5tools_dump_header_format->vlenblockbegin);

        h5tools_print_datatype(buffer, info, ctx, super);
        if(H5Tclose(super) < 0)
            HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tclose failed");

        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->vlenblockend);

        break;

    case H5T_ARRAY:
        h5tools_str_append(buffer, "H5T_ARRAY { ");

        /* Get array information */
        if((sndims = H5Tget_array_ndims(type)) >= 0) {
            unsigned     ndims = (unsigned)sndims;

            if(H5Tget_array_dims2(type, dims) >= 0) {
                /* Print array dimensions */
                for (i = 0; i < ndims; i++)
                    h5tools_str_append(buffer, "[%d]", (int) dims[i]);

                h5tools_str_append(buffer, " ");
            }
            else
                HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tget_array_dims2 failed");
        }
        else
            HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tget_array_ndims failed");

        /* Get array base type */
        if((super = H5Tget_super(type)) >= 0) {
            /* Print base type */
            h5tools_print_datatype(buffer, info, ctx, super);
            /* Close array base type */
            if(H5Tclose(super) < 0)
                HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tclose failed");
        }
        else
            HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tget_super failed");

        h5tools_str_append(buffer, " }");

        break;

    default:
        h5tools_str_append(buffer, "unknown datatype");
        break;
    }

CATCH
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    print_dataspace
 *
 * Purpose:     print the dataspace.
 *
 * Return:      void
 *
 * In/Out:      h5tools_str_t *buffer
 *              h5tools_context_t *ctx
 *
 *-------------------------------------------------------------------------
 */
int
h5tools_print_dataspace(h5tools_str_t *buffer, hid_t space)
{
    HERR_INIT(int, SUCCEED)
    hsize_t     size[H5TOOLS_DUMP_MAX_RANK];
    hsize_t     maxsize[H5TOOLS_DUMP_MAX_RANK];
    int         ndims = -1;
    H5S_class_t space_type = -1;
    int         i;

    if((ndims = H5Sget_simple_extent_dims(space, size, maxsize)) < 0)
        H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Sget_simple_extent_dims failed");

    if((space_type = H5Sget_simple_extent_type(space)) < 0)
        H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Sget_simple_extent_type failed");

    switch(space_type) {
    case H5S_SCALAR:
        /* scalar dataspace */
        h5tools_str_append(buffer, "%s %s", h5tools_dump_header_format->dataspacedescriptionbegin, S_SCALAR);
        break;

    case H5S_SIMPLE:
        /* simple dataspace */
        h5tools_str_append(buffer, "%s %s { %s %" H5_PRINTF_LL_WIDTH "u",
                            h5tools_dump_header_format->dataspacedescriptionbegin, S_SIMPLE,
                            h5tools_dump_header_format->dataspacedimbegin, size[0]);

        for(i = 1; i < ndims; i++)
            h5tools_str_append(buffer, ", %" H5_PRINTF_LL_WIDTH "u", size[i]);

        h5tools_str_append(buffer, " %s / ", h5tools_dump_header_format->dataspacedimend);

        if(maxsize[0] == H5S_UNLIMITED)
            h5tools_str_append(buffer, "%s %s",
                                h5tools_dump_header_format->dataspacedimbegin, "H5S_UNLIMITED");
        else
            h5tools_str_append(buffer, "%s %" H5_PRINTF_LL_WIDTH "u",
                                h5tools_dump_header_format->dataspacedimbegin, maxsize[0]);

        for(i = 1; i < ndims; i++)
            if(maxsize[i] == H5S_UNLIMITED)
                h5tools_str_append(buffer, ", %s", "H5S_UNLIMITED");
            else
                h5tools_str_append(buffer, ", %" H5_PRINTF_LL_WIDTH "u", maxsize[i]);

        h5tools_str_append(buffer, " %s }", h5tools_dump_header_format->dataspacedimend);
        break;

    case H5S_NULL:
        /* null dataspace */
        h5tools_str_append(buffer, "%s %s", h5tools_dump_header_format->dataspacedescriptionbegin, S_NULL);
        break;

    case H5S_NO_CLASS:
    default:
        h5tools_str_append(buffer, "%s unknown dataspace %s\n", BEGIN, END);
        break;
    } /* end switch */

CATCH
    return ret_value;
}


/*-------------------------------------------------------------------------
 * Function:    print_enum
 *
 * Purpose:     prints the enum data
 *
 * Return:      void
 *
 * In/Out:      h5tools_str_t *buffer
 *              h5tools_context_t *ctx
 *
 *-----------------------------------------------------------------------*/
int
h5tools_print_enum(h5tools_str_t *buffer, hid_t type)
{
    HERR_INIT(int, SUCCEED)
    char         **name = NULL;  /*member names                   */
    unsigned char *value = NULL; /*value array                    */
    unsigned char *copy = NULL;  /*a pointer to value array       */
    unsigned       nmembs = 0;   /*number of members              */
    int            nchars;       /*number of output characters    */
    hid_t          super = -1;   /*enum base integer type         */
    hid_t          native = -1;  /*native integer datatype        */
    H5T_sign_t     sign_type;    /*sign of value type             */
    size_t         type_size;    /*value type size                */
    size_t         dst_size;     /*destination value type size    */
    int            snmembs;
    unsigned       i;

    if((snmembs = H5Tget_nmembers(type)) < 0)
        H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Tget_nmembers failed");
    nmembs = (unsigned)snmembs;
    assert(nmembs > 0);

    if((super = H5Tget_super(type)) < 0)
        H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Tget_super failed");

    if((type_size = H5Tget_size(type)) <= 0)
        H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Tget_size(type) failed");

    /*
     * Determine what datatype to use for the native values.  To simplify
     * things we entertain three possibilities:
     *  1. long long -- the largest native signed integer
     *  2. unsigned long long -- the largest native unsigned integer
     *  3. raw format
     */
    if(type_size <= sizeof(long long)) {
        dst_size = sizeof(long long);

        if((sign_type = H5Tget_sign(type))<0)
            H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Tget_sign failed");
        if(H5T_SGN_NONE == sign_type)
            native = H5T_NATIVE_ULLONG;
        else
            native = H5T_NATIVE_LLONG;
    } /* end if */
    else
        dst_size = type_size;

    /* Get the names and raw values of all members */
    if(NULL == (name = (char **)HDcalloc(nmembs, sizeof(char *))))
        H5E_THROW(FAIL, H5E_tools_min_id_g, "Could not allocate buffer for member name");
    if(NULL == (value = (unsigned char *)HDcalloc(nmembs, MAX(type_size, dst_size))))
        H5E_THROW(FAIL, H5E_tools_min_id_g, "Could not allocate buffer for member value");

    for (i = 0; i < nmembs; i++) {
        name[i] = H5Tget_member_name(type, i);
        if(H5Tget_member_value(type, i, value + i * type_size) < 0)
            H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Tget_member_value failed");
    }

    /* Convert values to native datatype */
    if (native > 0)
        if(H5Tconvert(super, native, nmembs, value, NULL, H5P_DEFAULT) < 0)
            H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Tconvert failed");

    /*
     * Sort members by increasing value
     *    ***not implemented yet***
     */

    /* Print members */
    for (i = 0; i < nmembs; i++) {
        h5tools_str_append(buffer, "\"%s\"", name[i]);
        nchars = strlen(name[i]);
        h5tools_str_append(buffer, "%*s   ", MAX(0, 16 - nchars), "");

        if (native < 0) {
            size_t j;

            h5tools_str_append(buffer, "0x");

            for (j = 0; j < dst_size; j++)
                h5tools_str_append(buffer, "%02x", value[i * dst_size + j]);
        }
        else if (H5T_SGN_NONE == H5Tget_sign(native)) {
            /*On SGI Altix(cobalt), wrong values were printed out with "value+i*dst_size"
             *strangely, unless use another pointer "copy".*/
            copy = value + i * dst_size;
            h5tools_str_append(buffer, "%" H5_PRINTF_LL_WIDTH "u", *((unsigned long long *) ((void *) copy)));
        }
        else {
            /*On SGI Altix(cobalt), wrong values were printed out with "value+i*dst_size"
             *strangely, unless use another pointer "copy".*/
            copy = value + i * dst_size;
            h5tools_str_append(buffer, "%" H5_PRINTF_LL_WIDTH "d", *((long long *) ((void *) copy)));
        }

        h5tools_str_append(buffer, ";\n");
    }

CATCH
    if(name) {
        /* Release resources */
        for(i = 0; i < nmembs; i++)
            if(name[i])
                free(name[i]);
        free(name);
    } /* end if */

    if(value)
        free(value);

    if(super >= 0 && H5Tclose(super) < 0)
        H5E_THROW(FAIL, H5E_tools_min_id_g, "Could not close datatype's super class");

    if(0 == nmembs)
        h5tools_str_append(buffer, "\n<empty>");

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    dump_datatype
 *
 * Purpose:     Dump the datatype. Datatype can be HDF5 predefined
 *              atomic datatype or committed/transient datatype.
 *
 * Return:      void
 *
 * In/Out:      h5tools_context_t *ctx
 *-------------------------------------------------------------------------
 */
void
h5tools_dump_datatype(FILE *stream, const h5tool_format_t *info,
        h5tools_context_t *ctx, hid_t type)
{
    size_t        ncols = 80;      /* available output width        */
    h5tools_str_t buffer;          /* string into which to render   */
    hsize_t       curr_pos;        /* total data element position   */
    hsize_t       elmt_counter = 0;/* counts the # elements printed.*/

    /* setup */
    HDmemset(&buffer, 0, sizeof(h5tools_str_t));

    if (info->line_ncols > 0)
        ncols = info->line_ncols;

    /* pass to the prefix in h5tools_simple_prefix the total position
     * instead of the current stripmine position i; this is necessary
     * to print the array indices
     */
    curr_pos = ctx->sm_pos;

    h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);
    /* Render the element */
    h5tools_str_reset(&buffer);

    ctx->indent_level++;
    h5tools_str_append(&buffer, "%s %s ",
                        h5tools_dump_header_format->datatypebegin,
                        h5tools_dump_header_format->datatypeblockbegin);

    h5tools_print_datatype(&buffer, info, ctx, type);

    if (HDstrlen(h5tools_dump_header_format->datatypeblockend)) {
        h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->datatypeblockend);
        if (HDstrlen(h5tools_dump_header_format->datatypeend))
            h5tools_str_append(&buffer, " ");
    }
    if (HDstrlen(h5tools_dump_header_format->datatypeend))
        h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->datatypeend);
    h5tools_str_append(&buffer, "\n");

    curr_pos = h5tools_render_element(stream, info, ctx, &buffer, &curr_pos,
                                       ncols, elmt_counter, 0);

    ctx->need_prefix = TRUE;
    ctx->indent_level--;
}

/*-------------------------------------------------------------------------
 * Function:    init_acc_pos
 *
 * Purpose:     initialize accumulator and matrix position
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
void
init_acc_pos(h5tools_context_t *ctx, hsize_t *dims)
{
    int i;

    assert(ctx->ndims);

    ctx->acc[ctx->ndims - 1] = 1;
    for (i = (ctx->ndims - 2); i >= 0; i--) {
        ctx->acc[i] = ctx->acc[i + 1] * dims[i + 1];
    }
    for (i = 0; i < ctx->ndims; i++)
        ctx->pos[i] = 0;
}

/*-------------------------------------------------------------------------
 * Function: do_bin_output
 *
 * Purpose: Dump memory buffer to a binary file stream
 *
 * Return: Success:    SUCCEED
 *         Failure:    FAIL
 *-------------------------------------------------------------------------
 */
static
int do_bin_output(FILE *stream, hid_t container, hsize_t nelmts, hid_t tid, void *_mem)
{
    HERR_INIT(int, SUCCEED)
    unsigned char *mem  = (unsigned char*)_mem;
    size_t         size; /* datum size */
    hsize_t        i;    /* element counter  */

    if((size = H5Tget_size(tid)) == 0)
        H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Tget_size failed");

    for (i = 0; i < nelmts; i++) {
        if (render_bin_output(stream, container, tid, mem + i * size) < 0) {
            printf("\nError in writing binary stream\n");
            return FAIL;
       }
    }

CATCH
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: render_bin_output
 *
 * Purpose: Write one element of memory buffer to a binary file stream
 *
 * Return: Success:    SUCCEED
 *         Failure:    FAIL
 *-------------------------------------------------------------------------
 */
static int
render_bin_output(FILE *stream, hid_t container, hid_t tid, void *_mem)
{
    HERR_INIT(int, SUCCEED)
    unsigned char     *mem  = (unsigned char*)_mem;
    size_t             size;   /* datum size */
    float              tempfloat;
    double             tempdouble;
    unsigned long long tempullong;
    long long          templlong;
    unsigned long      tempulong;
    long               templong;
    unsigned int       tempuint;
    int                tempint;
    unsigned short     tempushort;
    short              tempshort;
    unsigned char      tempuchar;
    char               tempschar;
#if H5_SIZEOF_LONG_DOUBLE !=0
    long double        templdouble;
#endif
#ifdef DEBUG_H5DUMP_BIN
    static char fmt_llong[8], fmt_ullong[8];
    if (!fmt_llong[0]) {
        sprintf(fmt_llong, "%%%sd", H5_PRINTF_LL_WIDTH);
        sprintf(fmt_ullong, "%%%su", H5_PRINTF_LL_WIDTH);
    }
#endif

    if((size = H5Tget_size(tid)) == 0)
        H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Tget_size failed");

    if (H5Tequal(tid, H5T_NATIVE_FLOAT)) {
        memcpy(&tempfloat, mem, sizeof(float));
#ifdef DEBUG_H5DUMP_BIN
        fprintf(stream, "%g ", tempfloat);
#else
        if (1 != fwrite(&tempfloat, size, 1, stream))
            H5E_THROW(FAIL, H5E_tools_min_id_g, "fwrite failed");
#endif
    }
    else if (H5Tequal(tid, H5T_NATIVE_DOUBLE)) {
        memcpy(&tempdouble, mem, sizeof(double));
#ifdef DEBUG_H5DUMP_BIN
        fprintf(stream, "%g ", tempdouble);
#else
        if (1 != fwrite(&tempdouble, size, 1, stream))
            H5E_THROW(FAIL, H5E_tools_min_id_g, "fwrite failed");
#endif
    }
#if H5_SIZEOF_LONG_DOUBLE !=0
    else if (H5Tequal(tid, H5T_NATIVE_LDOUBLE)) {
        memcpy(&templdouble, mem, sizeof(long double));
#ifdef DEBUG_H5DUMP_BIN
        fprintf(stream, "%Lf ", templdouble);
#else
        if (1 != fwrite(&templdouble, size, 1, stream))
            H5E_THROW(FAIL, H5E_tools_min_id_g, "fwrite failed");
#endif
    }
#endif
    else if (H5T_STRING == H5Tget_class(tid)) {
        unsigned int i;
        H5T_str_t    pad;
        char        *s;

        pad = H5Tget_strpad(tid);

        if (H5Tis_variable_str(tid)) {
            s = *(char**) mem;
            if (s != NULL)
                size = HDstrlen(s);
        }
        else {
            s = (char *) mem;
            if((size = H5Tget_size(tid)) == 0)
                H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Tget_size failed");
        }
        for (i = 0; i < size && (s[i] || pad != H5T_STR_NULLTERM); i++) {
            memcpy(&tempuchar, &s[i], sizeof(unsigned char));
#ifdef DEBUG_H5DUMP_BIN
            fprintf(stream, "%d", tempuchar);
#else
            if (1 != fwrite(&tempuchar, size, 1, stream))
                H5E_THROW(FAIL, H5E_tools_min_id_g, "fwrite failed");
#endif
        } /* i */
    }
    else if (H5Tequal(tid, H5T_NATIVE_INT)) {
        memcpy(&tempint, mem, sizeof(int));
#ifdef DEBUG_H5DUMP_BIN
        fprintf(stream, "%d ", tempint);
#else
        if (1 != fwrite(&tempint, size, 1, stream))
            H5E_THROW(FAIL, H5E_tools_min_id_g, "fwrite failed");
#endif
    }
    else if (H5Tequal(tid, H5T_NATIVE_UINT)) {
        memcpy(&tempuint, mem, sizeof(unsigned int));
#ifdef DEBUG_H5DUMP_BIN
        fprintf(stream, "%u ", tempuint);
#else
        if (1 != fwrite(&tempuint, size, 1, stream))
            H5E_THROW(FAIL, H5E_tools_min_id_g, "fwrite failed");
#endif
    }
    else if (H5Tequal(tid, H5T_NATIVE_SCHAR)) {
        memcpy(&tempschar, mem, sizeof(char));
#ifdef DEBUG_H5DUMP_BIN
        fprintf(stream, "%d ", tempschar);
#else
        if (1 != fwrite(&tempschar, size, 1, stream))
            H5E_THROW(FAIL, H5E_tools_min_id_g, "fwrite failed");
#endif
    }
    else if (H5Tequal(tid, H5T_NATIVE_UCHAR)) {
        memcpy(&tempuchar, mem, sizeof(unsigned char));
#ifdef DEBUG_H5DUMP_BIN
        fprintf(stream, "%u ", tempuchar);
#else
        if (1 != fwrite(&tempuchar, size, 1, stream))
            H5E_THROW(FAIL, H5E_tools_min_id_g, "fwrite failed");
#endif
    }
    else if (H5Tequal(tid, H5T_NATIVE_SHORT)) {
        memcpy(&tempshort, mem, sizeof(short));
#ifdef DEBUG_H5DUMP_BIN
        fprintf(stream, "%d ", tempshort);
#else
        if (1 != fwrite(&tempshort, size, 1, stream))
            H5E_THROW(FAIL, H5E_tools_min_id_g, "fwrite failed");
#endif
    }
    else if (H5Tequal(tid, H5T_NATIVE_USHORT)) {
        memcpy(&tempushort, mem, sizeof(unsigned short));
#ifdef DEBUG_H5DUMP_BIN
        fprintf(stream, "%u ", tempushort);
#else
        if (1 != fwrite(&tempushort, size, 1, stream))
            H5E_THROW(FAIL, H5E_tools_min_id_g, "fwrite failed");
#endif
    }
    else if (H5Tequal(tid, H5T_NATIVE_LONG)) {
        memcpy(&templong, mem, sizeof(long));
#ifdef DEBUG_H5DUMP_BIN
        fprintf(stream, "%ld ", templong);
#else
        if (1 != fwrite(&templong, size, 1, stream))
            H5E_THROW(FAIL, H5E_tools_min_id_g, "fwrite failed");
#endif
    }
    else if (H5Tequal(tid, H5T_NATIVE_ULONG)) {
        memcpy(&tempulong, mem, sizeof(unsigned long));
#ifdef DEBUG_H5DUMP_BIN
        fprintf(stream, "%lu ", tempulong);
#else
        if (1 != fwrite(&tempulong, size, 1, stream))
            H5E_THROW(FAIL, H5E_tools_min_id_g, "fwrite failed");
#endif
    }
    else if (H5Tequal(tid, H5T_NATIVE_LLONG)) {
        memcpy(&templlong, mem, sizeof(long long));
#ifdef DEBUG_H5DUMP_BIN
        fprintf(stream, fmt_llong, templlong);
#else
        if (1 != fwrite(&templlong, size, 1, stream))
            H5E_THROW(FAIL, H5E_tools_min_id_g, "fwrite failed");
#endif
    }
    else if (H5Tequal(tid, H5T_NATIVE_ULLONG)) {
        memcpy(&tempullong, mem, sizeof(unsigned long long));
#ifdef DEBUG_H5DUMP_BIN
        fprintf(stream, fmt_ullong, tempullong);
#else
        if (1 != fwrite(&tempullong, size, 1, stream))
            H5E_THROW(FAIL, H5E_tools_min_id_g, "fwrite failed");
#endif
    }
    else if (H5Tequal(tid, H5T_NATIVE_HSSIZE)) {
        if (sizeof(hssize_t) == sizeof(int)) {
            memcpy(&tempint, mem, sizeof(int));
#ifdef DEBUG_H5DUMP_BIN
            fprintf(stream, "%d ", tempint);
#else
            if (1 != fwrite(&tempint, size, 1, stream))
                H5E_THROW(FAIL, H5E_tools_min_id_g, "fwrite failed");
#endif
        }
        else if (sizeof(hssize_t) == sizeof(long)) {
            memcpy(&templong, mem, sizeof(long));
#ifdef DEBUG_H5DUMP_BIN
            fprintf(stream, "%ld ", templong);
#else
            if (1 != fwrite(&templong, size, 1, stream))
                H5E_THROW(FAIL, H5E_tools_min_id_g, "fwrite failed");
#endif
        }
        else {
            memcpy(&templlong, mem, sizeof(long long));
#ifdef DEBUG_H5DUMP_BIN
            fprintf(stream, fmt_llong, templlong);
#else
            if (1 != fwrite(&templlong, size, 1, stream))
                H5E_THROW(FAIL, H5E_tools_min_id_g, "fwrite failed");
#endif
        }
    }
    else if (H5Tequal(tid, H5T_NATIVE_HSIZE)) {
        if (sizeof(hsize_t) == sizeof(int)) {
            memcpy(&tempuint, mem, sizeof(unsigned int));
#ifdef DEBUG_H5DUMP_BIN
            fprintf(stream, "%u ", tempuint);
#else
            if (1 != fwrite(&tempuint, size, 1, stream))
                H5E_THROW(FAIL, H5E_tools_min_id_g, "fwrite failed");
#endif
        }
        else if (sizeof(hsize_t) == sizeof(long)) {
            memcpy(&tempulong, mem, sizeof(unsigned long));
#ifdef DEBUG_H5DUMP_BIN
            fprintf(stream, "%lu ", tempulong);
#else
            if (1 != fwrite(&tempulong, size, 1, stream))
                H5E_THROW(FAIL, H5E_tools_min_id_g, "fwrite failed");
#endif
        }
        else {
            memcpy(&tempullong, mem, sizeof(unsigned long long));
#ifdef DEBUG_H5DUMP_BIN
            fprintf(stream, fmt_ullong, tempullong);
#else
            if (1 != fwrite(&tempullong, size, 1, stream))
                H5E_THROW(FAIL, H5E_tools_min_id_g, "fwrite failed");
#endif
        }
    }
    else if (H5Tget_class(tid) == H5T_COMPOUND) {
        unsigned j;
        hid_t    memb;
        unsigned nmembs;
        size_t   offset;

        nmembs = H5Tget_nmembers(tid);

        for (j = 0; j < nmembs; j++) {
            offset = H5Tget_member_offset(tid, j);
            memb   = H5Tget_member_type(tid, j);

            if (render_bin_output(stream, container, memb, mem + offset) < 0)
                return FAIL;

            H5Tclose(memb);
        }
    }
    else if (H5Tget_class(tid) == H5T_ENUM) {
        unsigned int i;
        if (1 == size) {
#ifdef DEBUG_H5DUMP_BIN
            fprintf(stream, "0x%02x", mem[0]);
#else
            if (1 != fwrite(&mem[0], size, 1, stream))
                H5E_THROW(FAIL, H5E_tools_min_id_g, "fwrite failed");
#endif
        }
        else {
            for (i = 0; i < size; i++) {
#ifdef DEBUG_H5DUMP_BIN
                fprintf(stream, "%s%02x", i?":":"", mem[i]);
#else
                if (1 != fwrite(&mem[i], sizeof(char), 1, stream))
                    H5E_THROW(FAIL, H5E_tools_min_id_g, "fwrite failed");
#endif
            } /*i*/
        }/*else 1 */
    }
    else if (H5Tget_class(tid) == H5T_ARRAY) {
        int     k, ndims;
        hsize_t i, dims[H5S_MAX_RANK], temp_nelmts, nelmts;
        hid_t   memb;

        /* get the array's base datatype for each element */
        memb = H5Tget_super(tid);
        size = H5Tget_size(memb);
        ndims = H5Tget_array_ndims(tid);
        H5Tget_array_dims2(tid, dims);
        assert(ndims >= 1 && ndims <= H5S_MAX_RANK);

        /* calculate the number of array elements */
        for (k = 0, nelmts = 1; k < ndims; k++) {
            temp_nelmts = nelmts;
            temp_nelmts *= dims[k];
            nelmts = (size_t) temp_nelmts;
        }

        /* dump the array element */
        for (i = 0; i < nelmts; i++) {
            if (render_bin_output(stream, container, memb, mem + i * size) < 0)
                H5E_THROW(FAIL, H5E_tools_min_id_g, "render_bin_output failed");
        }

        H5Tclose(memb);
    }
    else if (H5Tget_class(tid) == H5T_VLEN) {
        unsigned int i;
        hsize_t      nelmts;
        hid_t        memb;

        /* get the VL sequences's base datatype for each element */
        memb = H5Tget_super(tid);
        size = H5Tget_size(memb);

        /* Get the number of sequence elements */
        nelmts = ((hvl_t *) mem)->len;

        for (i = 0; i < nelmts; i++) {
            /* dump the array element */
            if (render_bin_output(stream, container, memb, ((char *) (((hvl_t *) mem)->p)) + i * size) < 0)
                H5E_THROW(FAIL, H5E_tools_min_id_g, "render_bin_output failed");
        }
        H5Tclose(memb);
    }
    else if (H5Tequal(tid, H5T_STD_REF_DSETREG)) {
        if (region_output) {
            /* region data */
            hid_t   region_id, region_space;
            H5S_sel_type region_type;

            region_id = H5Rdereference(container, H5R_DATASET_REGION, mem);
            if (region_id >= 0) {
                region_space = H5Rget_region(container, H5R_DATASET_REGION, mem);
                if (region_space >= 0) {
                    region_type = H5Sget_select_type(region_space);
                    if(region_type == H5S_SEL_POINTS)
                        render_bin_output_region_points(region_space, region_id, stream, container);
                    else
                        render_bin_output_region_blocks(region_space, region_id, stream, container);
                    H5Sclose(region_space);
                } /* end if (region_space >= 0) */
                H5Dclose(region_id);
            } /* end if (region_id >= 0) */
        } /* end if (region_output... */
    }
    else if (H5Tequal(tid, H5T_STD_REF_OBJ)) {
    }
    else {
        size_t i;
        if (1 == size) {
#ifdef DEBUG_H5DUMP_BIN
            fprintf(stream, "0x%02x", mem[0]);
#else
            if (1 != fwrite(&mem[0], size, 1, stream))
                H5E_THROW(FAIL, H5E_tools_min_id_g, "fwrite failed");
#endif
        }
        else {
            for (i = 0; i < size; i++) {
#ifdef DEBUG_H5DUMP_BIN
                fprintf(stream, "%s%02x", i?":":"", mem[i]);
#else
                if (1 != fwrite(&mem[i], sizeof(char), 1, stream))
                    H5E_THROW(FAIL, H5E_tools_min_id_g, "fwrite failed");
#endif
            } /*i*/
        }/*else 1 */
    }

CATCH
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose: Print the data values from a dataset referenced by region blocks.
 *
 * Description:
 *      This is a special case subfunction to print the data in a region reference of type blocks.
 *
 * Return:
 *      The function returns FAIL if there was an error, otherwise SUCEED
 *
 *-------------------------------------------------------------------------
 */
static int
render_bin_output_region_data_blocks(hid_t region_id, FILE *stream,
    hid_t container, int ndims, hid_t type_id, hssize_t nblocks, hsize_t *ptdata)
{
    hsize_t     *dims1 = NULL;
    hsize_t     *start = NULL;
    hsize_t     *count = NULL;
    size_t       numelem;
    hsize_t      total_size[H5S_MAX_RANK];
    int          jndx;
    int          type_size;
    hid_t        mem_space = -1;
    void        *region_buf = NULL;
    int          blkndx;
    hid_t        sid1 = -1;
    int          ret_value = SUCCEED;

    /* Get the dataspace of the dataset */
    if((sid1 = H5Dget_space(region_id)) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dget_space failed");

    /* Allocate space for the dimension array */
    if((dims1 = (hsize_t *) HDmalloc(sizeof(hsize_t) * ndims)) == NULL)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "Could not allocate buffer for dims");

    /* find the dimensions of each data space from the block coordinates */
    numelem = 1;
    for (jndx = 0; jndx < ndims; jndx++) {
        dims1[jndx] = ptdata[jndx + ndims] - ptdata[jndx] + 1;
        numelem = dims1[jndx] * numelem;
    }

    /* Create dataspace for reading buffer */
    if((mem_space = H5Screate_simple(ndims, dims1, NULL)) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Screate_simple failed");

    if((type_size = H5Tget_size(type_id)) == 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Tget_size failed");

    if((region_buf = HDmalloc(type_size * numelem)) == NULL)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "Could not allocate region buffer");

    /* Select (x , x , ..., x ) x (y , y , ..., y ) hyperslab for reading memory dataset */
    /*          1   2        n      1   2        n                                       */
    if((start = (hsize_t *) HDmalloc(sizeof(hsize_t) * ndims)) == NULL)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "Could not allocate buffer for start");

    if((count = (hsize_t *) HDmalloc(sizeof(hsize_t) * ndims)) == NULL)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "Could not allocate buffer for count");

    for (blkndx = 0; blkndx < nblocks; blkndx++) {
        for (jndx = 0; jndx < ndims; jndx++) {
            start[jndx] = ptdata[jndx + blkndx * ndims * 2];
            count[jndx] = dims1[jndx];
        }

        if(H5Sselect_hyperslab(sid1, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
            HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Sselect_hyperslab failed");

        if(H5Dread(region_id, type_id, mem_space, sid1, H5P_DEFAULT, region_buf) < 0)
            HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Dread failed");

        if(H5Sget_simple_extent_dims(mem_space, total_size, NULL) < 0)
            HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Sget_simple_extent_dims failed");

        for (jndx = 0; jndx < numelem; jndx++) {

            render_bin_output(stream, container, type_id,
                                ((char*)region_buf + jndx * type_size));
            /* Render the region data element end */
        } /* end for (jndx = 0; jndx < numelem; jndx++) */
    } /* end for (blkndx = 0; blkndx < nblocks; blkndx++) */

 done:
    HDfree(start);
    HDfree(count);
    HDfree(region_buf);
    HDfree(dims1);

    if(H5Sclose(mem_space) < 0)
        HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Sclose failed");
    if(H5Sclose(sid1) < 0)
        HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Sclose failed");

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose: Print some values from a dataset referenced by region blocks.
 *
 * Description:
 *      This is a special case subfunction to dump a region reference using blocks.
 *
 * Return:
 *      The function returns False if ERROR, otherwise True
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
render_bin_output_region_blocks(hid_t region_space, hid_t region_id,
        FILE *stream, hid_t container)
{
    HERR_INIT(hbool_t, TRUE)
    hssize_t     nblocks;
    hsize_t      alloc_size;
    hsize_t     *ptdata = NULL;
    int          ndims;
    hid_t        dtype;
    hid_t        type_id;

    if((nblocks = H5Sget_select_hyper_nblocks(region_space)) <= 0)
        H5E_THROW(FALSE, H5E_tools_min_id_g, "H5Sget_select_hyper_nblocks failed");

    /* Print block information */
    if((ndims = H5Sget_simple_extent_ndims(region_space)) < 0)
        H5E_THROW(FALSE, H5E_tools_min_id_g, "H5Sget_simple_extent_ndims failed");

    alloc_size = nblocks * ndims * 2 * sizeof(ptdata[0]);
    assert(alloc_size == (hsize_t) ((size_t) alloc_size)); /*check for overflow*/
    if((ptdata = (hsize_t*) malloc((size_t) alloc_size)) == NULL)
        HGOTO_ERROR(FALSE, H5E_tools_min_id_g, "Could not allocate buffer for ptdata");

    H5_CHECK_OVERFLOW(nblocks, hssize_t, hsize_t);
    if(H5Sget_select_hyper_blocklist(region_space, (hsize_t) 0, (hsize_t) nblocks, ptdata) < 0)
        HGOTO_ERROR(FALSE, H5E_tools_min_id_g, "H5Rget_select_hyper_blocklist failed");

    if((dtype = H5Dget_type(region_id)) < 0)
        HGOTO_ERROR(FALSE, H5E_tools_min_id_g, "H5Dget_type failed");
    if((type_id = H5Tget_native_type(dtype, H5T_DIR_DEFAULT)) < 0)
        HGOTO_ERROR(FALSE, H5E_tools_min_id_g, "H5Tget_native_type failed");

    render_bin_output_region_data_blocks(region_id, stream, container, ndims,
            type_id, nblocks, ptdata);

 done:
    free(ptdata);

    if(H5Tclose(type_id) < 0)
        HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tclose failed");

    if(H5Tclose(dtype) < 0)
        HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tclose failed");

    H5_LEAVE(TRUE)

 CATCH
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose: Print the data values from a dataset referenced by region points.
 *
 * Description:
 *      This is a special case subfunction to print the data in a region reference of type points.
 *
 * Return:
 *      The function returns FAIL on error, otherwise SUCCEED
 *
 * Parameters Description:
 *      h5tools_str_t *buffer is the string into which to render
 *      size_t ncols
 *      int ndims is the number of dimensions of the region element
 *      hssize_t npoints is the number of points in the region
 *-------------------------------------------------------------------------
 */
int
render_bin_output_region_data_points(hid_t region_space, hid_t region_id,
        FILE *stream, hid_t container,
        int ndims, hid_t type_id, hssize_t npoints, hsize_t *ptdata)
{
    hsize_t *dims1 = NULL;
    int      jndx;
    int      type_size;
    hid_t    mem_space = -1;
    void    *region_buf = NULL;
    int      ret_value = SUCCEED;

    if((type_size = H5Tget_size(type_id)) == 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Tget_size failed");

    if((region_buf = HDmalloc(type_size * npoints)) == NULL)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "Could not allocate buffer for region");

    /* Allocate space for the dimension array */
    if((dims1 = (hsize_t *) HDmalloc(sizeof(hsize_t) * ndims)) == NULL)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "Could not allocate buffer for dims");

    dims1[0] = npoints;
    if((mem_space = H5Screate_simple(1, dims1, NULL)) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Screate_simple failed");

    if(H5Dread(region_id, type_id, mem_space, region_space, H5P_DEFAULT, region_buf) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dread failed");
    for (jndx = 0; jndx < npoints; jndx++) {
        if(H5Sget_simple_extent_dims(region_space, dims1, NULL) < 0)
            HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Sget_simple_extent_dims failed");

        render_bin_output(stream, container, type_id,
                               ((char*)region_buf + jndx * type_size));
    } /* end for (jndx = 0; jndx < npoints; jndx++) */

 done:
    HDfree(region_buf);
    HDfree(dims1);

    if(H5Sclose(mem_space) < 0)
        HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Sclose failed");

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose: Print some values from a dataset referenced by region points.
 *
 * Description:
 *      This is a special case subfunction to dump a region reference using points.
 *
 * Return:
 *      The function returns False if the last dimension has been reached, otherwise True
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
render_bin_output_region_points(hid_t region_space, hid_t region_id,
        FILE *stream, hid_t container)
{
    HERR_INIT(hbool_t, TRUE)
    hssize_t npoints;
    hsize_t  alloc_size;
    hsize_t *ptdata;
    int      ndims;
    hid_t    dtype;
    hid_t    type_id;

    if((npoints = H5Sget_select_elem_npoints(region_space)) <= 0)
        H5E_THROW(FALSE, H5E_tools_min_id_g, "H5Sget_select_elem_npoints failed");

    /* Allocate space for the dimension array */
    if((ndims = H5Sget_simple_extent_ndims(region_space)) < 0)
        H5E_THROW(FALSE, H5E_tools_min_id_g, "H5Sget_simple_extent_ndims failed");

    alloc_size = npoints * ndims * sizeof(ptdata[0]);
    assert(alloc_size == (hsize_t) ((size_t) alloc_size)); /*check for overflow*/
    if(NULL == (ptdata = (hsize_t *)HDmalloc((size_t) alloc_size)))
        HGOTO_ERROR(FALSE, H5E_tools_min_id_g, "Could not allocate buffer for ptdata");

    H5_CHECK_OVERFLOW(npoints, hssize_t, hsize_t);
    if(H5Sget_select_elem_pointlist(region_space, (hsize_t) 0, (hsize_t) npoints, ptdata) < 0)
        HGOTO_ERROR(FALSE, H5E_tools_min_id_g, "H5Sget_select_elem_pointlist failed");

    if((dtype = H5Dget_type(region_id)) < 0)
        HGOTO_ERROR(FALSE, H5E_tools_min_id_g, "H5Dget_type failed");

    if((type_id = H5Tget_native_type(dtype, H5T_DIR_DEFAULT)) < 0)
        HGOTO_ERROR(FALSE, H5E_tools_min_id_g, "H5Tget_native_type failed");

    render_bin_output_region_data_points(region_space, region_id,
            stream, container, ndims, type_id, npoints, ptdata);

 done:
    free(ptdata);

    if(H5Tclose(type_id) < 0)
        HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tclose failed");

    if(H5Tclose(dtype) < 0)
        HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tclose failed");

    H5_LEAVE(ret_value)
CATCH
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_is_zero
 *
 * Purpose: Determines if memory is initialized to all zero bytes.
 *
 * Return:  TRUE if all bytes are zero; FALSE otherwise
 *-------------------------------------------------------------------------
 */
static
hbool_t h5tools_is_zero(const void *_mem, size_t size)
{
    const unsigned char *mem = (const unsigned char *) _mem;

    while (size-- > 0)
        if (mem[size])
            return FALSE;

    return TRUE;
}

