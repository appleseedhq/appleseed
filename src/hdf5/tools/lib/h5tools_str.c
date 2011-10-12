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
 * Programmer:  Bill Wendling <wendling@ncsa.uiuc.edu>
 *              Monday, 19. February 2001
 *
 * Purpose: These are string functions for us to use and abuse.
 */
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "H5private.h"
#include "h5tools.h"            /*for h5tool_format_t structure    */
#include "h5tools_ref.h"
#include "h5tools_str.h"        /*function prototypes       */

/*
 * If REPEAT_VERBOSE is defined then character strings will be printed so
 * that repeated character sequences like "AAAAAAAAAA" are displayed as
 *
 *  'A' repeates 9 times
 *
 * Otherwise the format is more Perl-like
 *
 *  'A'*10
 *
 */
#define REPEAT_VERBOSE

/* Variable length string datatype */
#define STR_INIT_LEN    4096    /*initial length            */

static char    *h5tools_escape(char *s, size_t size);
static hbool_t  h5tools_str_is_zero(const void *_mem, size_t size);
static void     h5tools_print_char(h5tools_str_t *str, const h5tool_format_t *info, char ch);

/*-------------------------------------------------------------------------
 * Function:    h5tools_str_close
 *
 * Purpose: Closes a string by releasing it's memory and setting the size
 *      information to zero.
 *
 * Return:  void
 *
 * Programmer:  Robb Matzke
 *              Monday, April 26, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
h5tools_str_close(h5tools_str_t *str)
{
    if (str && str->nalloc) {
        free(str->s);
        memset(str, 0, sizeof(h5tools_str_t));
    }
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_str_len
 *
 * Purpose: Returns the length of the string, not counting the null
 *      terminator.
 *
 * Return:  Success:    Length of string
 *
 *      Failure:    0
 *
 * Programmer:  Robb Matzke
 *              Monday, April 26, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
size_t
h5tools_str_len(h5tools_str_t *str)
{
    return str->len;
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_str_append
 *
 * Purpose: Formats variable arguments according to printf() format
 *      string and appends the result to variable length string STR.
 *
 * Return:  Success:    Pointer to buffer containing result.
 *
 *      Failure:    NULL
 *
 * Programmer:  Robb Matzke
 *              Monday, April 26, 1999
 *
 * Modifications:
 *
 *              Major change:  need to check results of vsnprintf to
 *              handle errors, empty format, and overflows.
 *
 * Programmer:  REMcG Matzke
 *              June 16, 2004
 *
 *-------------------------------------------------------------------------
 */
char *
h5tools_str_append(h5tools_str_t *str/*in,out*/, const char *fmt, ...)
{
    va_list ap;
    hbool_t isReallocated = FALSE;

    /* Make sure we have some memory into which to print */
    if (!str->s || str->nalloc <= 0) {
        str->nalloc = STR_INIT_LEN;
        str->s = malloc(str->nalloc);
        assert(str->s);
        str->s[0] = '\0';
        str->len = 0;
    }

    if (strlen(fmt) == 0) {
        /* nothing to print */
        return str->s;
    }

    /* Format the arguments and append to the value already in `str' */
    while (1) {
        /* How many bytes available for new value, counting the new NUL */
        size_t avail = str->nalloc - str->len;
        int nchars = -1;

        va_start(ap, fmt);
        nchars = HDvsnprintf(str->s + str->len, avail, fmt, ap);
        va_end(ap);

        /* Note: HDvsnprintf() behaves differently on Windows as Unix, when 
         * buffer is smaller than source string. On Unix, this function 
         * returns length of the source string and copy string upto the 
         * buffer size with NULL at the end of the buffer. However on 
         * Windows with the same condition, this function returns -1 and 
         * doesn't add NULL at the end of the buffer.
         * Because of this different return results, isReallocated variable
         * is used to handle when HDvsnprintf() returns -1 on Windows due
         * to lack of buffer size, so try one more time after realloc more
         * buffer size before return NULL. 
         */
        if (nchars < 0 && isReallocated == TRUE) {
            /* failure, such as bad format */
            return NULL;
        }

        if (nchars < 0 || (size_t) nchars >= avail || (0 == nchars && (strcmp(fmt, "%s")))) {
            /* Truncation return value as documented by C99, or zero return value with either of the
             * following conditions, each of which indicates that the proper C99 return value probably
             *  should have been positive when the format string is
             *  something other than "%s"
             * Alocate at least twice as much space and try again.
             */
            size_t newsize = MAX(str->len + nchars + 1, 2 * str->nalloc);
            assert(newsize > str->nalloc); /*overflow*/
            str->s = realloc(str->s, newsize);
            assert(str->s);
            str->nalloc = newsize;
            isReallocated = TRUE;
        }
        else {
            /* Success */
            str->len += nchars;
            break;
        }
    }
    return str->s;
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_str_reset
 *
 * Purpose: Reset the string to the empty value. If no memory is
 *      allocated yet then initialize the h5tools_str_t struct.
 *
 * Return:  Success:    Ptr to the buffer which contains a null
 *              character as the first element.
 *
 *      Failure:    NULL
 *
 * Programmer:  Robb Matzke
 *              Monday, April 26, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
char *
h5tools_str_reset(h5tools_str_t *str/*in,out*/)
{
    if (!str->s || str->nalloc <= 0) {
        str->nalloc = STR_INIT_LEN;
        str->s = malloc(str->nalloc);
        assert(str->s);
    }

    str->s[0] = '\0';
    str->len = 0;
    return str->s;
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_str_trunc
 *
 * Purpose: Truncate a string to be at most SIZE characters.
 *
 * Return:  Success:    Pointer to the string
 *
 *      Failure:    NULL
 *
 * Programmer:  Robb Matzke
 *              Monday, April 26, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
char *
h5tools_str_trunc(h5tools_str_t *str/*in,out*/, size_t size)
{
    if (size < str->len) {
        str->len = size;
        str->s[size] = '\0';
    }

    return str->s;
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_str_fmt
 *
 * Purpose: Reformat a string contents beginning at character START
 *      according to printf format FMT. FMT should contain no format
 *      specifiers except possibly the `%s' variety. For example, if
 *      the input string is `hello' and the format is "<<%s>>" then
 *      the output value will be "<<hello>>".
 *
 * Return:  Success:    A pointer to the resulting string.
 *
 *      Failure:    NULL
 *
 * Programmer:  Robb Matzke
 *              Monday, April 26, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
char *
h5tools_str_fmt(h5tools_str_t *str/*in,out*/, size_t start, const char *fmt)
{
    char _temp[1024], *temp = _temp;

    /* If the format string is simply "%s" then don't bother doing anything */
    if (!strcmp(fmt, "%s"))
        return str->s;

    /*
     * Save the input value if there is a `%' anywhere in FMT.  Otherwise
     * don't bother because we don't need a temporary copy.
     */
    if (strchr(fmt, '%')) {
        if (str->len - start + 1 > sizeof(_temp)) {
            temp = malloc(str->len - start + 1);
            assert(temp);
        }

        strcpy(temp, str->s + start);
    }

    /* Reset the output string and append a formatted version */
    h5tools_str_trunc(str, start);
    h5tools_str_append(str, fmt, temp);

    /* Free the temp buffer if we allocated one */
    if (temp != _temp)
        free(temp);

    return str->s;
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_str_prefix
 *
 * Purpose: Renders the line prefix value into string STR.
 *
 * Return:  Success:    Pointer to the prefix.
 *
 *      Failure:    NULL
 *
 * Programmer:  Robb Matzke
 *              Thursday, July 23, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
char *
h5tools_str_prefix(h5tools_str_t *str/*in,out*/, const h5tool_format_t *info,
    hsize_t elmtno, unsigned ndims, h5tools_context_t *ctx)
{
    size_t i = 0;
    hsize_t curr_pos = elmtno;

    h5tools_str_reset(str);

    if (ndims > 0) {
        /*
         * Calculate the number of elements represented by a unit change in a
         * certain index position.
         */
        for (i = 0; i < (size_t) ndims; i++) {
            ctx->pos[i] = curr_pos / ctx->acc[i];
            curr_pos -= ctx->acc[i] * ctx->pos[i];
        }
        assert(curr_pos == 0);

        /* Print the index values */
        for (i = 0; i < (size_t) ndims; i++) {
            if (i)
                h5tools_str_append(str, "%s", OPT(info->idx_sep, ","));

            h5tools_str_append(str, OPT(info->idx_n_fmt, HSIZE_T_FORMAT),
                    (hsize_t) ctx->pos[i]);

        }
    }
    else {
        /* Scalar */
        h5tools_str_append(str, OPT(info->idx_n_fmt, HSIZE_T_FORMAT), (hsize_t) 0);
    }

    /* Add prefix and suffix to the index */
    return h5tools_str_fmt(str, 0, OPT(info->idx_fmt, "%s: "));
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_str_region_prefix
 *
 * Purpose: Renders the line prefix value into string STR. Region reference specific.
 *
 * Return:  Success:    Pointer to the prefix.
 *          Failure:    NULL
 *
 * In/Out:
 *      h5tools_context_t *ctx
 *      h5tools_str_t     *str
 *-------------------------------------------------------------------------
 */
char *
h5tools_str_region_prefix(h5tools_str_t *str, const h5tool_format_t *info,
        hsize_t elmtno, hsize_t *ptdata, unsigned ndims, hsize_t max_idx[],
        h5tools_context_t *ctx)
{
    hsize_t p_prod[H5S_MAX_RANK];
    size_t i = 0;
    hsize_t curr_pos = elmtno;

    h5tools_str_reset(str);

    if (ndims > 0) {
        /*
         * Calculate the number of elements represented by a unit change in a
         * certain index position.
         */
        for (i = ndims - 1, p_prod[ndims - 1] = 1; i > 0; --i)
            p_prod[i - 1] = (max_idx[i]) * p_prod[i];

        for (i = 0; i < (size_t) ndims; i++) {
            ctx->pos[i] = curr_pos / p_prod[i];
            curr_pos -= p_prod[i] * ctx->pos[i];
            ctx->pos[i] += (unsigned long) ptdata[ctx->sm_pos+i];
        }

        /* Print the index values */
        for (i = 0; i < (size_t) ndims; i++) {
            if (i)
                h5tools_str_append(str, "%s", OPT(info->idx_sep, ","));

            h5tools_str_append(str, OPT(info->idx_n_fmt, HSIZE_T_FORMAT), (hsize_t) ctx->pos[i]);

        }
    } /* if (ndims > 0) */
    else {
        /* Scalar */
        h5tools_str_append(str, OPT(info->idx_n_fmt, HSIZE_T_FORMAT), (hsize_t) 0);
    }

    /* Add prefix and suffix to the index */
    return h5tools_str_fmt(str, 0, OPT(info->idx_fmt, "%s: "));
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_str_dump_region_blocks
 *
 * Purpose: Prints information about a dataspace region by appending
 *          the information to the specified string.
 *
 * Return:  none
 *
 * In/Out:
 *      h5tools_context_t *ctx
 *      h5tools_str_t     *str
 *-------------------------------------------------------------------------
 */
void
h5tools_str_dump_region_blocks(h5tools_str_t *str, hid_t region,
        const h5tool_format_t *info)
{
    hssize_t   nblocks;
    hsize_t    alloc_size;
    hsize_t   *ptdata;
    int        ndims = H5Sget_simple_extent_ndims(region);

    /*
     * This function fails if the region does not have blocks.
     */
    H5E_BEGIN_TRY {
        nblocks = H5Sget_select_hyper_nblocks(region);
    } H5E_END_TRY;

    /* Print block information */
    if (nblocks > 0) {
        int i;

        alloc_size = nblocks * ndims * 2 * sizeof(ptdata[0]);
        assert(alloc_size == (hsize_t) ((size_t) alloc_size)); /*check for overflow*/
        ptdata = (hsize_t *)malloc((size_t) alloc_size);
        H5_CHECK_OVERFLOW(nblocks, hssize_t, hsize_t);
        H5Sget_select_hyper_blocklist(region, (hsize_t)0, (hsize_t)nblocks, ptdata);

        for (i = 0; i < nblocks; i++) {
            int j;

            h5tools_str_append(str, info->dset_blockformat_pre, i ? "," OPTIONAL_LINE_BREAK " " : "",
                               (unsigned long)i);

            /* Start coordinates and opposite corner */
            for (j = 0; j < ndims; j++)
                h5tools_str_append(str, "%s%lu", j ? "," : "(",
                                    (unsigned long) ptdata[i * 2 * ndims + j]);

            for (j = 0; j < ndims; j++)
                h5tools_str_append(str, "%s%lu", j ? "," : ")-(",
                                    (unsigned long) ptdata[i * 2 * ndims + j + ndims]);

            h5tools_str_append(str, ")");
        }

        free(ptdata);
    } /* end if (nblocks > 0) */
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_str_dump_region_points
 *
 * Purpose: Prints information about a dataspace region by appending
 *          the information to the specified string.
 *
 * Return:  none
 *
 * In/Out:
 *      h5tools_context_t *ctx
 *      h5tools_str_t     *str
 *-------------------------------------------------------------------------
 */
void
h5tools_str_dump_region_points(h5tools_str_t *str, hid_t region,
        const h5tool_format_t *info)
{
    hssize_t   npoints;
    hsize_t    alloc_size;
    hsize_t   *ptdata;
    int        ndims = H5Sget_simple_extent_ndims(region);

    /*
     * This function fails if the region does not have points.
     */
    H5E_BEGIN_TRY {
        npoints = H5Sget_select_elem_npoints(region);
    } H5E_END_TRY;

    /* Print point information */
    if (npoints > 0) {
        int i;

        alloc_size = npoints * ndims * sizeof(ptdata[0]);
        assert(alloc_size == (hsize_t) ((size_t) alloc_size)); /*check for overflow*/
        ptdata = (hsize_t *)malloc((size_t) alloc_size);
        H5_CHECK_OVERFLOW(npoints, hssize_t, hsize_t);
        H5Sget_select_elem_pointlist(region, (hsize_t)0, (hsize_t)npoints, ptdata);

        for (i = 0; i < npoints; i++) {
            int j;

            h5tools_str_append(str, info->dset_ptformat_pre, i ? "," OPTIONAL_LINE_BREAK " " : "",
                               (unsigned long)i);

            for (j = 0; j < ndims; j++)
                h5tools_str_append(str, "%s%lu", j ? "," : "(",
                                  (unsigned long) (ptdata[i * ndims + j]));

            h5tools_str_append(str, ")");
        }

        free(ptdata);
    } /* end if (npoints > 0) */
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_print_char
 *
 * Purpose: Shove a character into the STR.
 *
 * Return:  Nothing
 *
 *-------------------------------------------------------------------------
 */
static void
h5tools_print_char(h5tools_str_t *str, const h5tool_format_t *info, char ch)
{
    if (info->str_locale == ESCAPE_HTML) {
        if (ch <= ' ' || ch > '~')
            h5tools_str_append(str, "%%%02x", ch);
        else
            h5tools_str_append(str, "%c", ch);
    }
    else {
        switch (ch) {
        case '"':
            if (!info->do_escape)
                h5tools_str_append(str, "\"");
            else
                h5tools_str_append(str, "\\\"");
            break;
        case '\\':
            if (!info->do_escape)
                h5tools_str_append(str, "\\");
            else
                h5tools_str_append(str, "\\\\");
            break;
        case '\b':
            if (!info->do_escape)
                h5tools_str_append(str, "\b");
            else
                h5tools_str_append(str, "\\b");
            break;
        case '\f':
            if (!info->do_escape)
                h5tools_str_append(str, "\f");
            else
                h5tools_str_append(str, "\\f");
            break;
        case '\n':
            if (!info->do_escape) {
                h5tools_str_append(str, "\n");
                h5tools_str_append(str, "           ");
            }
            else
                h5tools_str_append(str, "\\n");
            break;
        case '\r':
            if (!info->do_escape) {
                h5tools_str_append(str, "\r");
                h5tools_str_append(str, "           ");
            }
            else
                h5tools_str_append(str, "\\r");
            break;
        case '\t':
            if (!info->do_escape)
                h5tools_str_append(str, "\t");
            else
                h5tools_str_append(str, "\\t");
            break;
        default:
            if (isprint(ch))
                h5tools_str_append(str, "%c", ch);
            else
                h5tools_str_append(str, "\\%03o", ch);

            break;
        }
    }
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_str_sprint
 *
 * Purpose: Renders the value pointed to by VP of type TYPE into variable
 *      length string STR.
 *
 * Return:  A pointer to memory containing the result or NULL on error.
 *
 * Programmer:  Robb Matzke
 *              Thursday, July 23, 1998
 *
 * Modifications:
 *      Robb Matzke, 1999-04-26
 *      Made this function safe from overflow problems by allowing it
 *      to reallocate the output string.
 *
 *      Robb Matzke, 1999-06-04
 *      Added support for object references. The new `container'
 *      argument is the dataset where the reference came from.
 *
 *      Robb Matzke, 1999-06-07
 *      Added support for printing raw data. If info->raw is non-zero
 *      then data is printed in hexadecimal format.
 *
 *  Robb Matzke, 2003-01-10
 *  Binary output format is dd:dd:... instead of 0xdddd... so it
 *  doesn't look like a hexadecimal integer, and thus users will
 *  be less likely to complain that HDF5 didn't properly byte
 *  swap their data during type conversion.
 *
 *  Robb Matzke, LLNL, 2003-06-05
 *  If TYPE is a variable length string then the pointer to
 *  the value to pring (VP) is a pointer to a `char*'.
 *
 *  PVN, 28 March 2006
 *  added H5T_NATIVE_LDOUBLE case
 *-------------------------------------------------------------------------
 */
char *
h5tools_str_sprint(h5tools_str_t *str, const h5tool_format_t *info, hid_t container,
                   hid_t type, void *vp, h5tools_context_t *ctx)
{
    size_t         n, offset, size=0, nelmts, start;
    char          *name;
    unsigned char *ucp_vp = (unsigned char *)vp;
    char          *cp_vp = (char *)vp;
    hid_t          memb, obj;
    unsigned       nmembs;
    static char    fmt_llong[8], fmt_ullong[8];
    H5T_str_t      pad;

    /*
     * some tempvars to store the value before we append it to the string to
     * get rid of the memory alignment problem
     */
    unsigned long long tempullong;
    long long          templlong;
    unsigned long      tempulong;
    long               templong;
    unsigned int       tempuint;
    int                tempint;

    /* Build default formats for long long types */
    if (!fmt_llong[0]) {
        sprintf(fmt_llong, "%%%sd", H5_PRINTF_LL_WIDTH);
        sprintf(fmt_ullong, "%%%su", H5_PRINTF_LL_WIDTH);
    }

    /* Append value depending on data type */
    start = h5tools_str_len(str);

    if (info->raw) {
        size_t i;

        n = H5Tget_size(type);
        if (1 == n) {
            h5tools_str_append(str, OPT(info->fmt_raw, "0x%02x"), ucp_vp[0]);
        }
        else {
            for (i = 0; i < n; i++) {
                if (i)
                    h5tools_str_append(str, ":");
                h5tools_str_append(str, OPT(info->fmt_raw, "%02x"), ucp_vp[i]);
            }
        }
    }
    else if (H5Tequal(type, H5T_NATIVE_FLOAT)) {
        float tempfloat;

        HDmemcpy(&tempfloat, vp, sizeof(float));
        h5tools_str_append(str, OPT(info->fmt_float, "%g"), tempfloat);
    }
    else if (H5Tequal(type, H5T_NATIVE_DOUBLE)) {
        double tempdouble;

        HDmemcpy(&tempdouble, vp, sizeof(double));
        h5tools_str_append(str, OPT(info->fmt_double, "%g"), tempdouble);
#if H5_SIZEOF_LONG_DOUBLE !=0
    }
    else if (H5Tequal(type, H5T_NATIVE_LDOUBLE)) {
        long double templdouble;

        HDmemcpy(&templdouble, vp, sizeof(long double));
        h5tools_str_append(str, "%Lf", templdouble);
#endif
    }
    else if (info->ascii && (H5Tequal(type, H5T_NATIVE_SCHAR) ||
                             H5Tequal(type, H5T_NATIVE_UCHAR))) {
        h5tools_print_char(str, info, (char) (*ucp_vp));
    }
    else if (H5T_STRING == H5Tget_class(type)) {
        unsigned int i;
        char quote = '\0';
        char *s;

        quote = '\0';
        if (H5Tis_variable_str(type)) {
            /* cp_vp is the pointer into the struct where a `char*' is stored. So we have
             * to dereference the pointer to get the `char*' to pass to HDstrlen(). */
            s = *(char**) cp_vp;
            if (s != NULL)
                size = HDstrlen(s);
        }
        else {
            s = cp_vp;
            size = H5Tget_size(type);
        }
        pad = H5Tget_strpad(type);

        /* Check for NULL pointer for string */
        if (s == NULL) {
            h5tools_str_append(str, "NULL");
        }
        else {
            for (i = 0; i < size && (s[i] || pad != H5T_STR_NULLTERM); i++) {
                int j = 1;

                /*
                 * Count how many times the next character repeats. If the
                 * threshold is zero then that means it can repeat any number
                 * of times.
                 */
                if (info->str_repeat > 0)
                    while (i + j < size && s[i] == s[i + j])
                        j++;

                /*
                 * Print the opening quote.  If the repeat count is high enough to
                 * warrant printing the number of repeats instead of enumerating
                 * the characters, then make sure the character to be repeated is
                 * in it's own quote.
                 */
                if (info->str_repeat > 0 && j > info->str_repeat) {
                    if (quote)
                        h5tools_str_append(str, "%c", quote);

                    quote = '\'';
                    h5tools_str_append(str, "%s%c", i ? " " : "", quote);
                }
                else if (!quote) {
                    quote = '"';
                    h5tools_str_append(str, "%s%c", i ? " " : "", quote);
                }

                /* Print the character */
                h5tools_print_char(str, info, s[i]);

                /* Print the repeat count */
                if (info->str_repeat && j > info->str_repeat) {
#ifdef REPEAT_VERBOSE
                    h5tools_str_append(str, "%c repeats %d times", quote, j - 1);
#else
                    h5tools_str_append(str, "%c*%d", quote, j - 1);
#endif  /* REPEAT_VERBOSE */
                    quote = '\0';
                    i += j - 1;
                }

            }

            if (quote)
                h5tools_str_append(str, "%c", quote);

            if (i == 0)
                /*empty string*/
                h5tools_str_append(str, "\"\"");
        } /* end else */
    }
    else if (H5Tequal(type, H5T_NATIVE_INT)) {
        HDmemcpy(&tempint, vp, sizeof(int));
#ifdef H5_HAVE_H5DUMP_PACKED_BITS
        if(packed_bits_num) {
            tempint = (tempint >> packed_data_offset) & packed_data_mask;
        }
#endif
        h5tools_str_append(str, OPT(info->fmt_int, "%d"), tempint);
    }
    else if (H5Tequal(type, H5T_NATIVE_UINT)) {
        HDmemcpy(&tempuint, vp, sizeof(unsigned int));
#ifdef H5_HAVE_H5DUMP_PACKED_BITS
        if(packed_bits_num) {
            tempuint = (tempuint >> packed_data_offset) & packed_data_mask;
        }
#endif
        h5tools_str_append(str, OPT(info->fmt_uint, "%u"), tempuint);
    }
    else if (H5Tequal(type, H5T_NATIVE_SCHAR)) {
        char               tempchar;
        HDmemcpy(&tempchar, cp_vp, sizeof(char));
#ifdef H5_HAVE_H5DUMP_PACKED_BITS
        if(packed_bits_num) {
            tempchar = (tempchar >> packed_data_offset) & packed_data_mask;
        }
#endif
        h5tools_str_append(str, OPT(info->fmt_schar, "%hhd"), tempchar);
    }
    else if (H5Tequal(type, H5T_NATIVE_UCHAR)) {
        unsigned char      tempuchar;
        HDmemcpy(&tempuchar, ucp_vp, sizeof(unsigned char));
#ifdef H5_HAVE_H5DUMP_PACKED_BITS
        if(packed_bits_num) {
            tempuchar = (tempuchar >> packed_data_offset) & packed_data_mask;
        }
#endif
        h5tools_str_append(str, OPT(info->fmt_uchar, "%u"), tempuchar);
    }
    else if (H5Tequal(type, H5T_NATIVE_SHORT)) {
        short tempshort;

        HDmemcpy(&tempshort, vp, sizeof(short));
#ifdef H5_HAVE_H5DUMP_PACKED_BITS
        if(packed_bits_num) {
            tempshort = (tempshort >> packed_data_offset) & packed_data_mask;
        }
#endif
        h5tools_str_append(str, OPT(info->fmt_short, "%d"), tempshort);
    }
    else if (H5Tequal(type, H5T_NATIVE_USHORT)) {
        unsigned short tempushort;

        HDmemcpy(&tempushort, vp, sizeof(unsigned short));
#ifdef H5_HAVE_H5DUMP_PACKED_BITS
        if(packed_bits_num) {
            tempushort = (tempushort >> packed_data_offset) & packed_data_mask;
        }
#endif
        h5tools_str_append(str, OPT(info->fmt_ushort, "%u"), tempushort);
    }
    else if (H5Tequal(type, H5T_NATIVE_LONG)) {
        HDmemcpy(&templong, vp, sizeof(long));
#ifdef H5_HAVE_H5DUMP_PACKED_BITS
        if(packed_bits_num) {
            templong = (templong >> packed_data_offset) & packed_data_mask;
        }
#endif
        h5tools_str_append(str, OPT(info->fmt_long, "%ld"), templong);
    }
    else if (H5Tequal(type, H5T_NATIVE_ULONG)) {
        HDmemcpy(&tempulong, vp, sizeof(unsigned long));
#ifdef H5_HAVE_H5DUMP_PACKED_BITS
        if(packed_bits_num) {
            tempulong = (tempulong >> packed_data_offset) & packed_data_mask;
        }
#endif
        h5tools_str_append(str, OPT(info->fmt_ulong, "%lu"), tempulong);
    }
    else if (H5Tequal(type, H5T_NATIVE_LLONG)) {
        HDmemcpy(&templlong, vp, sizeof(long long));
#ifdef H5_HAVE_H5DUMP_PACKED_BITS
        if(packed_bits_num) {
            templlong = (templlong >> packed_data_offset) & packed_data_mask;
        }
#endif
        h5tools_str_append(str, OPT(info->fmt_llong, fmt_llong), templlong);
    }
    else if (H5Tequal(type, H5T_NATIVE_ULLONG)) {
        HDmemcpy(&tempullong, vp, sizeof(unsigned long long));
#ifdef H5_HAVE_H5DUMP_PACKED_BITS
        if(packed_bits_num) {
            tempullong = (tempullong >> packed_data_offset) & packed_data_mask;
        }
#endif
        h5tools_str_append(str, OPT(info->fmt_ullong, fmt_ullong), tempullong);
    }
    else if (H5Tequal(type, H5T_NATIVE_HSSIZE)) {
        if (sizeof(hssize_t) == sizeof(int)) {
            memcpy(&tempint, vp, sizeof(int));
            h5tools_str_append(str, OPT(info->fmt_int, "%d"), tempint);
        }
        else if (sizeof(hssize_t) == sizeof(long)) {
            memcpy(&templong, vp, sizeof(long));
            h5tools_str_append(str, OPT(info->fmt_long, "%ld"), templong);
        }
        else {
            memcpy(&templlong, vp, sizeof(long long));
            h5tools_str_append(str, OPT(info->fmt_llong, fmt_llong), templlong);
        }
    }
    else if (H5Tequal(type, H5T_NATIVE_HSIZE)) {
        if (sizeof(hsize_t) == sizeof(int)) {
            memcpy(&tempuint, vp, sizeof(unsigned int));
            h5tools_str_append(str, OPT(info->fmt_uint, "%u"), tempuint);
        }
        else if (sizeof(hsize_t) == sizeof(long)) {
            memcpy(&tempulong, vp, sizeof(long));
            h5tools_str_append(str, OPT(info->fmt_ulong, "%lu"), tempulong);
        }
        else {
            memcpy(&tempullong, vp, sizeof(unsigned long long));
            h5tools_str_append(str, OPT(info->fmt_ullong, fmt_ullong), tempullong);
        }
    }
    else if (H5Tget_class(type) == H5T_COMPOUND) {
        unsigned j;

        nmembs = H5Tget_nmembers(type);
        h5tools_str_append(str, "%s", OPT(info->cmpd_pre, "{"));

        for (j = 0; j < nmembs; j++) {
            if (j)
                h5tools_str_append(str, "%s", OPT(info->cmpd_sep, ", " OPTIONAL_LINE_BREAK));

            /* RPM 2000-10-31
             * If the previous character is a line-feed (which is true when
             * h5dump is running) then insert some white space for
             * indentation. Be warned that column number calculations will be
             * incorrect and that object indices at the beginning of the line
             * will be missing (h5dump doesn't display them anyway).  */
            if (ctx->indent_level >= 0 && str->len && str->s[str->len - 1] == '\n') {
                int x;

                h5tools_str_append(str, OPT(info->line_pre, ""), "");

                for (x = 0; x < ctx->indent_level + 1; x++)
                    h5tools_str_append(str, "%s", OPT(info->line_indent, ""));
            }

            /* The name */
            name = H5Tget_member_name(type, j);
            h5tools_str_append(str, OPT(info->cmpd_name, ""), name);
            free(name);

            /* The value */
            offset = H5Tget_member_offset(type, j);
            memb = H5Tget_member_type(type, j);

            ctx->indent_level++;
            h5tools_str_sprint(str, info, container, memb, cp_vp + offset, ctx);
            ctx->indent_level--;

            H5Tclose(memb);
        }

        /* RPM 2000-10-31
         * If the previous character is a line feed (which is true when
         * h5dump is running) then insert some white space for indentation.
         * Be warned that column number calculations will be incorrect and
         * that object indices at the beginning of the line will be missing
         * (h5dump doesn't display them anyway). */
        h5tools_str_append(str, "%s", OPT(info->cmpd_end, ""));

        if (ctx->indent_level >= 0 && str->len && str->s[str->len - 1] == '\n') {
            int x;

            h5tools_str_append(str, OPT(info->line_pre, ""), "");

            for (x = 0; x < ctx->indent_level; x++)
                h5tools_str_append(str, "%s", OPT(info->line_indent, ""));
        }

        h5tools_str_append(str, "%s", OPT(info->cmpd_suf, "}"));
    }
    else if (H5Tget_class(type) == H5T_ENUM) {
        char enum_name[1024];

        if (H5Tenum_nameof(type, vp, enum_name, sizeof enum_name) >= 0) {
            h5tools_str_append(str, h5tools_escape(enum_name, sizeof(enum_name)));
        }
        else {
            size_t i;
            n = H5Tget_size(type);
            if (1 == n) {
                h5tools_str_append(str, "0x%02x", ucp_vp[0]);
            }
            else {
                for (i = 0; i < n; i++)
                    h5tools_str_append(str, "%s%02x", i ? ":" : "", ucp_vp[i]);
            }
        }
    }
    else if (H5Tequal(type, H5T_STD_REF_DSETREG)) {
        if (h5tools_str_is_zero(vp, H5Tget_size(type))) {
            h5tools_str_append(str, "NULL");
        }
        else {
            h5tools_str_sprint_region(str, info, container, vp);
        }
    }
    else if (H5Tequal(type, H5T_STD_REF_OBJ)) {
        /*
         * Object references -- show the type and OID of the referenced
         * object.
         */
        if (h5tools_str_is_zero(vp, H5Tget_size(type))) {
            h5tools_str_append(str, "NULL");
        }
        else {
            H5O_info_t oi;
            const char *path;

            obj = H5Rdereference(container, H5R_OBJECT, vp);
            H5Oget_info(obj, &oi);

            /* Print object type and close object */
            switch (oi.type) {
            case H5O_TYPE_GROUP:
                h5tools_str_append(str, H5_TOOLS_GROUP);
                break;

            case H5O_TYPE_DATASET:
                h5tools_str_append(str, H5_TOOLS_DATASET);
                break;

            case H5O_TYPE_NAMED_DATATYPE:
                h5tools_str_append(str, H5_TOOLS_DATATYPE);
                break;

            default:
                h5tools_str_append(str, "%u-", (unsigned) oi.type);
                break;
            } /* end switch */
            H5Oclose(obj);

            /* Print OID */
            if (info->obj_hidefileno)
                h5tools_str_append(str, info->obj_format, oi.addr);
            else
                h5tools_str_append(str, info->obj_format, oi.fileno, oi.addr);

            /* Print name */
            path = lookup_ref_path(*(haddr_t *) vp);
            if (path) {
                h5tools_str_append(str, " ");
                h5tools_str_append(str, path);
                h5tools_str_append(str, " ");
            } /* end if */
        } /* end else */
    }
    else if (H5Tget_class(type) == H5T_ARRAY) {
        int k, ndims;
        hsize_t i, dims[H5S_MAX_RANK], temp_nelmts;

        /* Get the array's base datatype for each element */
        memb = H5Tget_super(type);
        size = H5Tget_size(memb);
        ndims = H5Tget_array_ndims(type);
        H5Tget_array_dims2(type, dims);
        assert(ndims >= 1 && ndims <= H5S_MAX_RANK);

        /* Calculate the number of array elements */
        for (k = 0, nelmts = 1; k < ndims; k++) {
            temp_nelmts = nelmts;
            temp_nelmts *= dims[k];
            assert(temp_nelmts == (hsize_t) ((size_t) temp_nelmts));
            nelmts = (size_t) temp_nelmts;
        }
        /* Print the opening bracket */
        h5tools_str_append(str, "%s", OPT(info->arr_pre, "["));

        for (i = 0; i < nelmts; i++) {
            if (i)
                h5tools_str_append(str, "%s", OPT(info->arr_sep, "," OPTIONAL_LINE_BREAK));

            if (info->arr_linebreak && i && i % dims[ndims - 1] == 0) {
                int x;

                h5tools_str_append(str, "%s", "\n");

                /* need to indent some more here*/
                if (ctx->indent_level >= 0)
                    if (!info->pindex)
                        h5tools_str_append(str, "%s", OPT(info->line_pre, ""));

                for (x = 0; x < ctx->indent_level + 1; x++)
                    h5tools_str_append(str, "%s", OPT(info->line_indent, ""));
            } /* end if */
            else if (i && info->arr_sep)
                h5tools_str_append(str, " ");

            ctx->indent_level++;

            /* Dump the array element */
            h5tools_str_sprint(str, info, container, memb, cp_vp + i * size, ctx);

            ctx->indent_level--;
        } /* end for */

        /* Print the closing bracket */
        h5tools_str_append(str, "%s", OPT(info->arr_suf, "]"));
        H5Tclose(memb);
    }
    else if (H5Tget_class(type) == H5T_VLEN) {
        unsigned int i;

        /* Get the VL sequences's base datatype for each element */
        memb = H5Tget_super(type);
        size = H5Tget_size(memb);

        /* Print the opening bracket */
        h5tools_str_append(str, "%s", OPT(info->vlen_pre, "("));

        /* Get the number of sequence elements */
        nelmts = ((hvl_t *) cp_vp)->len;

        for (i = 0; i < nelmts; i++) {
            if (i)
                h5tools_str_append(str, "%s", OPT(info->vlen_sep, "," OPTIONAL_LINE_BREAK));

#ifdef LATER
            /* Need to fix so VL data breaks at correct location on end of line -QAK */
            if (info->arr_linebreak && h5tools_str_len(str)>=info->line_ncols) {
                int x;

                h5tools_str_append(str, "%s", "\n");

                /* need to indent some more here */
                if (ctx->indent_level >= 0)
                    h5tools_str_append(str, "%s", OPT(info->line_pre, ""));

                for (x = 0; x < ctx->indent_level + 1; x++)
                    h5tools_str_append(str,"%s",OPT(info->line_indent,""));
            } /* end if */
#endif /* LATER */

            ctx->indent_level++;

            /* Dump the array element */
            h5tools_str_sprint(str, info, container, memb,
                    ((char *) (((hvl_t *) cp_vp)->p)) + i * size, ctx);

            ctx->indent_level--;
        } /* end for */

        h5tools_str_append(str, "%s", OPT(info->vlen_suf, ")"));
        H5Tclose(memb);
    }
    else {
        /* All other types get printed as hexadecimal */
        size_t i;
        n = H5Tget_size(type);
        if (1 == n) {
            h5tools_str_append(str, "0x%02x", ucp_vp[0]);
        }
        else {
            for (i = 0; i < n; i++)
                h5tools_str_append(str, "%s%02x", i ? ":" : "", ucp_vp[i]);
        }
    }

    return h5tools_str_fmt(str, start, OPT(info->elmt_fmt, "%s"));
}


/*-------------------------------------------------------------------------
 * Function:    h5tools_str_sprint_region
 *
 * Purpose: Dataset region reference -- show the type and data of the referenced object.
 *
 * Return:  Nothing
 *-------------------------------------------------------------------------
 */
void
h5tools_str_sprint_region(h5tools_str_t *str, const h5tool_format_t *info,
        hid_t container, void *vp)
{
    hid_t   obj, region;
    char    ref_name[1024];
    H5S_sel_type region_type;

    obj = H5Rdereference(container, H5R_DATASET_REGION, vp);
    if (obj >= 0) {
        region = H5Rget_region(container, H5R_DATASET_REGION, vp);
        if (region >= 0) {
            H5Rget_name(obj, H5R_DATASET_REGION, vp, (char*) ref_name, 1024);

            h5tools_str_append(str, info->dset_format, ref_name);

            h5tools_str_append(str, "{");

            region_type = H5Sget_select_type(region);
            if(region_type==H5S_SEL_POINTS)
                h5tools_str_dump_region_points(str, region, info);
            else
                h5tools_str_dump_region_blocks(str, region, info);

            h5tools_str_append(str, "}");

            H5Sclose(region);
        } /* end if (region >= 0) */
        H5Dclose(obj);
    } /* end if (obj >= 0) */
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_escape
 *
 * Purpose: Changes all "funny" characters in S into standard C escape
 *      sequences.
 *
 * Return:  Success:    S
 *
 *      Failure:    NULL if the buffer would overflow. The
 *              buffer has as many left-to-right escapes as
 *              possible before overflow would have happened.
 *
 * Programmer:  Robb Matzke
 *              Monday, April 26, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static char *
h5tools_escape(char *s/*in,out*/, size_t size)
{
    register size_t i;
    size_t n = strlen(s);
    const char *escape;
    char octal[8];

    for (i = 0; i < n; i++) {
        switch (s[i]) {
        case '\'':
            escape = "\\\'";
            break;
        case '\"':
            escape = "\\\"";
            break;
        case '\\':
            escape = "\\\\";
            break;
        case '\?':
            escape = "\\\?";
            break;
        case '\a':
            escape = "\\a";
            break;
        case '\b':
            escape = "\\b";
            break;
        case '\f':
            escape = "\\f";
            break;
        case '\n':
            escape = "\\n";
            break;
        case '\r':
            escape = "\\r";
            break;
        case '\t':
            escape = "\\t";
            break;
        case '\v':
            escape = "\\v";
            break;
        default:
            if (!isprint(s[i])) {
                sprintf(octal, "\\%03o", (unsigned char) s[i]);
                escape = octal;
            }
            else {
                escape = NULL;
            }

            break;
        }

        if (escape) {
            size_t esc_size = strlen(escape);

            if (n + esc_size + 1 > size)
                /*would overflow*/
                return NULL;

            memmove(s + i + esc_size, s + i + 1, n - i); /*make room*/
            memcpy(s + i, escape, esc_size); /*insert*/
            n += esc_size - 1; /* adjust total string size */
            i += esc_size; /* adjust string position */
        }
    }

    return s;
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_str_is_zero
 *
 * Purpose: Determines if memory is initialized to all zero bytes.
 *
 * Return:  TRUE if all bytes are zero; FALSE otherwise
 *
 * Programmer:  Robb Matzke
 *              Monday, June  7, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
h5tools_str_is_zero(const void *_mem, size_t size)
{
    const unsigned char *mem = (const unsigned char *) _mem;

    while (size-- > 0)
        if (mem[size])
            return FALSE;

    return TRUE;
}
