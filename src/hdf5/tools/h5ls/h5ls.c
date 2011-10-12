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
 *              Monday, March 23, 1998
 */


/*
 * We include the private header file so we can get to the uniform
 * programming environment it declares.  Other than that, h5ls only calls
 * HDF5 API functions (except for H5G_basename())
 */
#include "H5private.h"
#include "h5tools.h"
#include "h5tools_utils.h"
#include "h5trav.h"

/* Name of tool */
#define PROGRAMNAME "h5ls"

#define NAME_BUF_SIZE   2048

/* Struct to pass through to visitors */
typedef struct {
    const char *fname;                  /* Filename */
    hid_t fid;                          /* File ID */
    hid_t gid;                          /* Group ID */
    hbool_t symlink_target;                 /* Whether this is the target of an symbolic link */
    symlink_trav_t *symlink_list;       /* List of visited symbolic links */
    int base_len;                       /* Length of base path name, if not root */
    int name_start;                     /* # of leading characters to strip off path names on output */
}iter_t;

/* Command-line switches */
static int      verbose_g = 0;            /* lots of extra output */
static int      width_g = 80;             /* output width in characters */
static hbool_t  address_g = FALSE;        /* print raw data addresses */
static hbool_t  data_g = FALSE;           /* display dataset values? */
static hbool_t  label_g = FALSE;          /* label compound values? */
static hbool_t  string_g = FALSE;         /* print 1-byte numbers as ASCII? */
static hbool_t  fullname_g = FALSE;       /* print full path names */
static hbool_t  recursive_g = FALSE;      /* recursive descent listing */
static hbool_t  follow_symlink_g = FALSE; /* follow symbolic links */
static hbool_t  no_dangling_link_g = FALSE; /* treat dangling link is error */
static hbool_t  follow_elink_g = FALSE;   /* follow external links */
static hbool_t  grp_literal_g = FALSE;    /* list group, not contents */
static hbool_t  hexdump_g = FALSE;        /* show data as raw hexadecimal */
static hbool_t  show_errors_g = FALSE;    /* print HDF5 error messages */
static hbool_t  simple_output_g = FALSE;  /* make output more machine-readable */
static hbool_t  show_file_name_g = FALSE; /* show file name for full names */
static hbool_t  no_line_wrap_g = FALSE;   /* show data content without line wrap */
static hbool_t  display_root_g = FALSE;   /* show root group in output? */

/* Information about how to display each type of object */
static struct dispatch_t {
    const char *name;
    hid_t (*open)(hid_t loc, const char *name, hid_t apl_id);
    herr_t (*close)(hid_t obj);
    herr_t (*list1)(hid_t obj);
    herr_t (*list2)(hid_t obj, const char *name);
} dispatch_g[H5O_TYPE_NTYPES];

#define DISPATCH(TYPE, NAME, LIST1, LIST2) {         \
    dispatch_g[TYPE].name = (NAME);             \
    dispatch_g[TYPE].list1 = (LIST1);           \
    dispatch_g[TYPE].list2 = (LIST2);           \
}

static void display_type(hid_t type, int ind);
static herr_t visit_obj(hid_t file, const char *oname, iter_t *iter);


/*-------------------------------------------------------------------------
 * Function: usage
 *
 * Purpose: Prints a usage message on stderr and then returns.
 *
 * Return: void
 *
 * Programmer: Robb Matzke
 *              Thursday, July 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
usage (void)
{
    fprintf(stderr, "\
usage: %s [OPTIONS] [OBJECTS...]\n\
  OPTIONS\n\
   -h, -?, --help  Print a usage message and exit\n\
   -a, --address   Print raw data address.  If dataset is contiguous, address\n\
                   is offset in file of beginning of raw data. If chunked,\n\
                   returned list of addresses indicates offset of each chunk.\n\
                   Must be used with -v, --verbose option.\n\
                   Provides no information for non-dataset objects.\n\
   -d, --data      Print the values of datasets\n\
   --enable-error-stack\n\
                   Prints messages from the HDF5 error stack as they occur.\n\
   --follow-symlinks\n\
                   Follow symbolic links (soft links and external links)\n\
                   to display target object information.\n\
                   Without this option, h5ls identifies a symbolic link\n\
                   as a soft link or external link and prints the value\n\
                   assigned to the symbolic link; it does not provide any\n\
                   information regarding the target object or determine\n\
                   whether the link is a dangling link.\n\
   --no-dangling-links\n\
                   Must be used with --follow-symlinks option;\n\
                   otherwise, h5ls shows error message and returns an exit\n\
                   code of 1. \n\
                   Check for any symbolic links (soft links or external links)\n\
                   that do not resolve to an existing object (dataset, group,\n\
                   or named datatype).\n\
                   If any dangling link is found, this situation is treated\n\
                   as an error and h5ls returns an exit code of 1.\n\
   -f, --full      Print full path names instead of base names\n\
   -g, --group     Show information about a group, not its contents\n\
   -l, --label     Label members of compound datasets\n\
   -r, --recursive List all groups recursively, avoiding cycles\n\
   -s, --string    Print 1-byte integer datasets as ASCII\n\
   -S, --simple    Use a machine-readable output format\n\
   -wN, --width=N  Set the number of columns of output\n\
   -v, --verbose   Generate more verbose output\n\
   -V, --version   Print version number and exit\n\
   --vfd=DRIVER    Use the specified virtual file driver\n\
   -x, --hexdump   Show raw data in hexadecimal format\n\
\n\
  OBJECTS\n\
    Each object consists of an HDF5 file name optionally followed by a\n\
    slash and an object name within the file (if no object is specified\n\
    within the file then the contents of the root group are displayed).\n\
    The file name may include a printf(3C) integer format such as\n\
    \"%%05d\" to open a file family.\n\
\n\
  Deprecated Options\n\
    The following options have been deprecated in HDF5. While they remain\n\
    available, they have been superseded as indicated and may be removed\n\
    from HDF5 in the future. Use the indicated replacement option in all\n\
    new work; where possible, existing scripts, et cetera, should also be\n\
    updated to use the replacement option.\n\
\n\
   -E or --external   Follow external links.\n\
                      Replaced by --follow-symlinks.\n\
   -e, --errors       Show all HDF5 error reporting\n\
                      Replaced by --enable-error-stack.\n",
     h5tools_getprogname());
}


/*-------------------------------------------------------------------------
 * Function: display_string
 *
 * Purpose: Print a string value by escaping unusual characters. If
 *  STREAM is null then we only count how large the output would
 *  be.
 *
 * Return: Number of characters printed.
 *
 * Programmer: Robb Matzke
 *              Thursday, November  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
display_string(FILE *stream, const char *s, hbool_t escape_spaces)
{
    int  nprint=0;

    for (/*void*/; s && *s; s++) {
        switch (*s) {
            case '"':
                if (stream) fprintf(stream, "\\\"");
                nprint += 2;
                break;
            case '\\':
                if (stream) fprintf(stream, "\\\\");
                nprint += 2;
                break;
            case '\b':
                if (stream) fprintf(stream, "\\b");
                nprint += 2;
                break;
            case '\f':
                if (stream) fprintf(stream, "\\f");
                nprint += 2;
                break;
            case '\n':
                if (stream) fprintf(stream, "\\n");
                nprint += 2;
                break;
            case '\r':
                if (stream) fprintf(stream, "\\r");
                nprint += 2;
                break;
            case '\t':
                if (stream) fprintf(stream, "\\t");
                nprint += 2;
                break;
            case ' ':
                if (escape_spaces) {
                    if (stream) fprintf(stream, "\\ ");
                    nprint += 2;
                } else {
                    if (stream) fprintf(stream, " ");
                    nprint++;
                }
                break;
            default:
                if (isprint((int)*s)) {
                    if (stream) putc(*s, stream);
                    nprint++;
                } else {
                    if (stream) {
                        fprintf(stream, "\\%03o", *((const unsigned char*)s));
                    }
                    nprint += 4;
                }
                break;
        }
    }
    return nprint;
}


/*-------------------------------------------------------------------------
 * Function: display_obj_name
 *
 * Purpose: Print an object name and another string.
 *
 * Return: Success: TRUE
 *
 *  Failure: FALSE, nothing printed
 *
 * Programmer: Quincey Koziol
 *              Tuesday, November  6, 2007
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
display_obj_name(FILE *stream, const iter_t *iter, const char *oname,
    const char *s)
{
    static char fullname[NAME_BUF_SIZE];     /* Buffer for file and/or object name */
    const char *name = fullname;                /* Pointer to buffer for printing */
    int  n;

    if(show_file_name_g)
        sprintf(fullname, "%s/%s", iter->fname, oname + iter->name_start);
    else
        name = oname + iter->name_start;

    /* Print the object name, either full name or base name */
    if(fullname_g)
        n = display_string(stream, name, TRUE);
    else {
        const char *last_sep;   /* The location of the last group separator */

        /* Find the last component of the path name */
        if(NULL == (last_sep = HDstrrchr(name, '/')))
            last_sep = name;
        else {
            last_sep++;
        } /* end else */
        n = display_string(stream, last_sep, TRUE);
    } /* end else */
    printf("%*s ", MAX(0, (24 - n)), s);

    return TRUE;
}


/*-------------------------------------------------------------------------
 * Function: display_native_type
 *
 * Purpose: Prints the name of a native C data type.
 *
 * Return: Success: TRUE
 *
 *  Failure: FALSE, nothing printed.
 *
 * Programmer: Robb Matzke
 *              Thursday, November  5, 1998
 *
 * Modifications:
 *   Robb Matzke, 1999-06-11
 *  Added the C9x types, but we still prefer to display the types
 *  from the C language itself (like `int' vs. `int32_t').
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
display_native_type(hid_t type, int UNUSED ind)
{
    if (H5Tequal(type, H5T_NATIVE_SCHAR)==TRUE) {
        printf("native signed char");
    } else if (H5Tequal(type, H5T_NATIVE_UCHAR)==TRUE) {
        printf("native unsigned char");
    } else if (H5Tequal(type, H5T_NATIVE_INT)==TRUE) {
        printf("native int");
    } else if (H5Tequal(type, H5T_NATIVE_UINT)==TRUE) {
        printf("native unsigned int");
    } else if (H5Tequal(type, H5T_NATIVE_SHORT)==TRUE) {
        printf("native short");
    } else if (H5Tequal(type, H5T_NATIVE_USHORT)==TRUE) {
        printf("native unsigned short");
    } else if (H5Tequal(type, H5T_NATIVE_LONG)==TRUE) {
        printf("native long");
    } else if (H5Tequal(type, H5T_NATIVE_ULONG)==TRUE) {
        printf("native unsigned long");
    } else if (H5Tequal(type, H5T_NATIVE_LLONG)==TRUE) {
        printf("native long long");
    } else if (H5Tequal(type, H5T_NATIVE_ULLONG)==TRUE) {
        printf("native unsigned long long");
    } else if (H5Tequal(type, H5T_NATIVE_FLOAT)==TRUE) {
        printf("native float");
    } else if (H5Tequal(type, H5T_NATIVE_DOUBLE)==TRUE) {
        printf("native double");
#if H5_SIZEOF_LONG_DOUBLE !=0
    } else if (H5Tequal(type, H5T_NATIVE_LDOUBLE)==TRUE) {
        printf("native long double");
#endif
    } else if (H5Tequal(type, H5T_NATIVE_INT8)==TRUE) {
        printf("native int8_t");
    } else if (H5Tequal(type, H5T_NATIVE_UINT8)==TRUE) {
        printf("native uint8_t");
    } else if (H5Tequal(type, H5T_NATIVE_INT16)==TRUE) {
        printf("native int16_t");
    } else if (H5Tequal(type, H5T_NATIVE_UINT16)==TRUE) {
        printf("native uint16_t");
    } else if (H5Tequal(type, H5T_NATIVE_INT32)==TRUE) {
        printf("native int32_t");
    } else if (H5Tequal(type, H5T_NATIVE_UINT32)==TRUE) {
        printf("native uint32_t");
    } else if (H5Tequal(type, H5T_NATIVE_INT64)==TRUE) {
        printf("native int64_t");
    } else if (H5Tequal(type, H5T_NATIVE_UINT64)==TRUE) {
        printf("native uint64_t");
    } else if (H5Tequal(type, H5T_NATIVE_INT_LEAST8)==TRUE) {
        printf("native int_least8_t");
    } else if (H5Tequal(type, H5T_NATIVE_UINT_LEAST8)==TRUE) {
        printf("native uint_least8_t");
    } else if (H5Tequal(type, H5T_NATIVE_INT_LEAST16)==TRUE) {
        printf("native int_least16_t");
    } else if (H5Tequal(type, H5T_NATIVE_UINT_LEAST16)==TRUE) {
        printf("native uint_least16_t");
    } else if (H5Tequal(type, H5T_NATIVE_INT_LEAST32)==TRUE) {
        printf("native int_least32_t");
    } else if (H5Tequal(type, H5T_NATIVE_UINT_LEAST32)==TRUE) {
        printf("native uint_least32_t");
    } else if (H5Tequal(type, H5T_NATIVE_INT_LEAST64)==TRUE) {
        printf("native int_least64_t");
    } else if (H5Tequal(type, H5T_NATIVE_UINT_LEAST64)==TRUE) {
        printf("native uint_least64_t");
    } else if (H5Tequal(type, H5T_NATIVE_INT_FAST8)==TRUE) {
        printf("native int_fast8_t");
    } else if (H5Tequal(type, H5T_NATIVE_UINT_FAST8)==TRUE) {
        printf("native uint_fast8_t");
    } else if (H5Tequal(type, H5T_NATIVE_INT_FAST16)==TRUE) {
        printf("native int_fast16_t");
    } else if (H5Tequal(type, H5T_NATIVE_UINT_FAST16)==TRUE) {
        printf("native uint_fast16_t");
    } else if (H5Tequal(type, H5T_NATIVE_INT_FAST32)==TRUE) {
        printf("native int_fast32_t");
    } else if (H5Tequal(type, H5T_NATIVE_UINT_FAST32)==TRUE) {
        printf("native uint_fast32_t");
    } else if (H5Tequal(type, H5T_NATIVE_INT_FAST64)==TRUE) {
        printf("native int_fast64_t");
    } else if (H5Tequal(type, H5T_NATIVE_UINT_FAST64)==TRUE) {
        printf("native uint_fast64_t");
    } else if (H5Tequal(type, H5T_NATIVE_B8)==TRUE) {
        printf("native 8-bit field");
    } else if (H5Tequal(type, H5T_NATIVE_B16)==TRUE) {
        printf("native 16-bit field");
    } else if (H5Tequal(type, H5T_NATIVE_B32)==TRUE) {
        printf("native 32-bit field");
    } else if (H5Tequal(type, H5T_NATIVE_B64)==TRUE) {
        printf("native 64-bit field");
    } else if (H5Tequal(type, H5T_NATIVE_HSIZE)==TRUE) {
        printf("native hsize_t");
    } else if (H5Tequal(type, H5T_NATIVE_HSSIZE)==TRUE) {
        printf("native hssize_t");
    } else if (H5Tequal(type, H5T_NATIVE_HERR)==TRUE) {
        printf("native herr_t");
    } else if (H5Tequal(type, H5T_NATIVE_HBOOL)==TRUE) {
        printf("native hbool_t");
    } else {
        return FALSE;
    }
    return TRUE;
}


/*-------------------------------------------------------------------------
 * Function: display_ieee_type
 *
 * Purpose: Print the name of an IEEE floating-point data type.
 *
 * Return: Success: TRUE
 *
 *  Failure: FALSE, nothing printed
 *
 * Programmer: Robb Matzke
 *              Thursday, November  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
display_ieee_type(hid_t type, int UNUSED ind)
{
    if (H5Tequal(type, H5T_IEEE_F32BE)==TRUE) {
        printf("IEEE 32-bit big-endian float");
    } else if (H5Tequal(type, H5T_IEEE_F32LE)==TRUE) {
        printf("IEEE 32-bit little-endian float");
    } else if (H5Tequal(type, H5T_IEEE_F64BE)==TRUE) {
        printf("IEEE 64-bit big-endian float");
    } else if (H5Tequal(type, H5T_IEEE_F64LE)==TRUE) {
        printf("IEEE 64-bit little-endian float");
    } else {
        return FALSE;
    }
    return TRUE;
}


/*-------------------------------------------------------------------------
 * Function: display_precision
 *
 * Purpose: Prints information on the next line about precision and
 *  padding if the precision is less than the total data type
 *  size.
 *
 * Return: void
 *
 * Programmer: Robb Matzke
 *              Thursday, November  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
display_precision(hid_t type, int ind)
{
    size_t      prec;           /* precision */
    H5T_pad_t   plsb, pmsb;     /* lsb and msb padding */
    const char  *plsb_s=NULL;   /* lsb padding string */
    const char  *pmsb_s=NULL;   /* msb padding string */
    size_t      nbits;          /* number of bits */

    /* If the precision is less than the total size then show the precision
     * and offset on the following line.  Also display the padding
     * information. */
    if (8*H5Tget_size(type)!=(prec=H5Tget_precision(type))) {
        printf("\n%*s(%lu bit%s of precision beginning at bit %lu)",
            ind, "", (unsigned long)prec, 1==prec?"":"s",
            (unsigned long)H5Tget_offset(type));

        H5Tget_pad(type, &plsb, &pmsb);
        if (H5Tget_offset(type)>0) {
            switch (plsb) {
                case H5T_PAD_ZERO:
                    plsb_s = "zero";
                    break;
                case H5T_PAD_ONE:
                    plsb_s = "one";
                    break;
                case H5T_PAD_BACKGROUND:
                    plsb_s = "bkg";
                    break;
                case H5T_PAD_ERROR:
                case H5T_NPAD:
                    plsb_s = "unknown";
                    break;
                default:
                    ;
                    break;
            }
        }
        if (H5Tget_offset(type)+prec<8*H5Tget_size(type)) {
            switch (pmsb) {
                case H5T_PAD_ZERO:
                    pmsb_s = "zero";
                    break;
                case H5T_PAD_ONE:
                    pmsb_s = "one";
                    break;
                case H5T_PAD_BACKGROUND:
                    pmsb_s = "bkg";
                    break;
                case H5T_PAD_ERROR:
                case H5T_NPAD:
                    pmsb_s = "unknown";
                    break;
                default:
                    ;
                    break;
            }
        }
        if (plsb_s || pmsb_s) {
            printf("\n%*s(", ind, "");
            if (plsb_s) {
                nbits = H5Tget_offset(type);
                printf("%lu %s bit%s at bit 0",
                        (unsigned long)nbits, plsb_s, 1==nbits?"":"s");
            }
            if (plsb_s && pmsb_s) printf(", ");
            if (pmsb_s) {
                nbits = 8*H5Tget_size(type)-(H5Tget_offset(type)+prec);
                printf("%lu %s bit%s at bit %lu",
                        (unsigned long)nbits, pmsb_s, 1==nbits?"":"s",
                        (unsigned long)(8*H5Tget_size(type)-nbits));
            }
            printf(")");
        }
    }
}


/*-------------------------------------------------------------------------
 * Function: display_int_type
 *
 * Purpose: Print the name of an integer data type.  Common information
 *  like number of bits, byte order, and sign scheme appear on
 *  the first line. Additional information might appear in
 *  parentheses on the following lines.
 *
 * Return: Success: TRUE
 *
 *  Failure: FALSE, nothing printed
 *
 * Programmer: Robb Matzke
 *              Thursday, November  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
display_int_type(hid_t type, int ind)
{
    H5T_order_t order;          /* byte order value */
    const char  *order_s=NULL;  /* byte order string */
    H5T_sign_t  sign;           /* sign scheme value */
    const char  *sign_s=NULL;   /* sign scheme string */

    if (H5T_INTEGER!=H5Tget_class(type)) return FALSE;

    /* Byte order */
    if (H5Tget_size(type)>1) {
        order = H5Tget_order(type);
        if (H5T_ORDER_LE==order) {
            order_s = " little-endian";
        } else if (H5T_ORDER_BE==order) {
            order_s = " big-endian";
        } else if (H5T_ORDER_VAX==order) {
            order_s = " mixed-endian";
        } else {
            order_s = " unknown-byte-order";
        }
    } else {
        order_s = "";
    }

    /* Sign */
    if ((sign=H5Tget_sign(type))>=0) {
        if (H5T_SGN_NONE==sign) {
            sign_s = " unsigned";
        } else if (H5T_SGN_2==sign) {
            sign_s = "";
        } else {
            sign_s = " unknown-sign";
        }
    } else {
        sign_s = " unknown-sign";
    }

    /* Print size, order, and sign on first line, precision and padding
     * information on the subsequent lines */
    printf("%lu-bit%s%s integer",
            (unsigned long)(8*H5Tget_size(type)), order_s, sign_s);
    display_precision(type, ind);
    return TRUE;
}


/*-------------------------------------------------------------------------
 * Function: display_float_type
 *
 * Purpose: Print info about a floating point data type.
 *
 * Return: Success: TRUE
 *
 *  Failure: FALSE, nothing printed
 *
 * Programmer: Robb Matzke
 *              Thursday, November  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
display_float_type(hid_t type, int ind)
{
    H5T_order_t order;          /* byte order value */
    const char  *order_s=NULL;  /* byte order string */
    size_t      spos;           /* sign bit position */
    size_t      esize, epos;    /* exponent size and position */
    size_t      msize, mpos;    /* significand size and position */
    size_t      ebias;          /* exponent bias */
    H5T_norm_t  norm;           /* significand normalization */
    const char  *norm_s=NULL;   /* normalization string */
    H5T_pad_t   pad;            /* internal padding value */
    const char  *pad_s=NULL;    /* internal padding string */

    if (H5T_FLOAT!=H5Tget_class(type)) return FALSE;

    /* Byte order */
    if (H5Tget_size(type)>1) {
        order = H5Tget_order(type);
        if (H5T_ORDER_LE==order) {
            order_s = " little-endian";
        } else if (H5T_ORDER_BE==order) {
            order_s = " big-endian";
        } else if (H5T_ORDER_VAX==order) {
            order_s = " mixed-endian";
        } else {
            order_s = " unknown-byte-order";
        }
    } else {
        order_s = "";
    }

    /* Print size and byte order on first line, precision and padding on
     * subsequent lines. */
    printf("%lu-bit%s floating-point",
        (unsigned long)(8*H5Tget_size(type)), order_s);
    display_precision(type, ind);

    /* Print sizes, locations, and other information about each field */
    H5Tget_fields (type, &spos, &epos, &esize, &mpos, &msize);
    ebias = H5Tget_ebias(type);
    norm = H5Tget_norm(type);
    switch (norm) {
        case H5T_NORM_IMPLIED:
            norm_s = ", msb implied";
            break;
        case H5T_NORM_MSBSET:
            norm_s = ", msb always set";
            break;
        case H5T_NORM_NONE:
            norm_s = ", no normalization";
            break;
        case H5T_NORM_ERROR:
            norm_s = ", unknown normalization";
            break;
        default:
            ;
        break;
    }
    printf("\n%*s(significant for %lu bit%s at bit %lu%s)", ind, "",
            (unsigned long)msize, 1==msize?"":"s", (unsigned long)mpos,
            norm_s);
    printf("\n%*s(exponent for %lu bit%s at bit %lu, bias is 0x%lx)",
            ind, "", (unsigned long)esize, 1==esize?"":"s",
            (unsigned long)epos, (unsigned long)ebias);
    printf("\n%*s(sign bit at %lu)", ind, "", (unsigned long)spos);

    /* Display internal padding */
    if (1+esize+msize<H5Tget_precision(type)) {
        pad = H5Tget_inpad(type);
        switch (pad) {
            case H5T_PAD_ZERO:
                pad_s = "zero";
                break;
            case H5T_PAD_ONE:
                pad_s = "one";
                break;
            case H5T_PAD_BACKGROUND:
                pad_s = "bkg";
                break;
            case H5T_PAD_ERROR:
            case H5T_NPAD:
                pad_s = "unknown";
                break;
            default:
                ;
            break;
        }
        printf("\n%*s(internal padding bits are %s)", ind, "", pad_s);
    }
    return TRUE;
}


/*-------------------------------------------------------------------------
 * Function: display_cmpd_type
 *
 * Purpose: Print info about a compound data type.
 *
 * Return: Success: TRUE
 *
 *  Failure: FALSE, nothing printed
 *
 * Programmer: Robb Matzke
 *              Thursday, November  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
display_cmpd_type(hid_t type, int ind)
{
    char        *name=NULL;     /* member name */
    size_t      size;           /* total size of type in bytes */
    hid_t       subtype;        /* member data type */
    unsigned    nmembs;         /* number of members */
    int         n;              /* miscellaneous counters */
    unsigned    i;              /* miscellaneous counters */

    if (H5T_COMPOUND!=H5Tget_class(type)) return FALSE;
    printf("struct {");
    nmembs=H5Tget_nmembers(type);
    for (i=0; i<nmembs; i++) {

        /* Name and offset */
        name = H5Tget_member_name(type, i);
        printf("\n%*s\"", ind+4, "");
        n = display_string(stdout, name, FALSE);
        printf("\"%*s +%-4lu ", MAX(0, 16-n), "",
               (unsigned long)H5Tget_member_offset(type, i));
        free(name);

        /* Member's type */
        subtype = H5Tget_member_type(type, i);
        display_type(subtype, ind+4);
        H5Tclose(subtype);
    }
    size = H5Tget_size(type);
    printf("\n%*s} %lu byte%s",
    ind, "", (unsigned long)size, 1==size?"":"s");
    return TRUE;
}


/*-------------------------------------------------------------------------
 * Function: display_enum_type
 *
 * Purpose: Print info about an enumeration data type.
 *
 * Return: Success: TRUE
 *
 *  Failure: FALSE, nothing printed
 *
 * Programmer: Robb Matzke
 *              Wednesday, December 23, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
display_enum_type(hid_t type, int ind)
{
    char        **name=NULL;    /* member names */
    unsigned char *value=NULL;  /* value array */
    unsigned char *copy = NULL; /* a pointer to value array */
    unsigned    nmembs;         /* number of members */
    int         nchars;         /* number of output characters */
    hid_t       super;          /* enum base integer type */
    hid_t       native=-1;      /* native integer data type */
    size_t      dst_size;       /* destination value type size */
    unsigned    i;              /* miscellaneous counters */
    size_t j;

    if (H5T_ENUM!=H5Tget_class(type)) return FALSE;
    nmembs = H5Tget_nmembers(type);
    assert(nmembs>0);
    super = H5Tget_super(type);
    printf("enum ");
    display_type(super, ind+4);
    printf(" {");

    /* Determine what data type to use for the native values.  To simplify
     * things we entertain three possibilities:
     *  1. long long -- the largest native signed integer
     * 2. unsigned long long -- the largest native unsigned integer
     *     3. raw format */
    if (H5Tget_size(type)<=sizeof(long long)) {
        dst_size = sizeof(long long);
        if (H5T_SGN_NONE==H5Tget_sign(type)) {
            native = H5T_NATIVE_ULLONG;
        } else {
            native = H5T_NATIVE_LLONG;
        }
    } else {
        dst_size = H5Tget_size(type);
    }

    /* Get the names and raw values of all members */
    name = calloc(nmembs, sizeof(char*));
    value = (unsigned char *)calloc(nmembs, MAX(H5Tget_size(type), dst_size));
    for (i=0; i<nmembs; i++) {
        name[i] = H5Tget_member_name(type, i);
        H5Tget_member_value(type, i, value+i*H5Tget_size(type));
    }

    /* Convert values to native data type */
    if (native>0) H5Tconvert(super, native, nmembs, value, NULL, H5P_DEFAULT);

    /* Sort members by increasing value */
    /*not implemented yet*/

    /* Print members */
    for (i=0; i<nmembs; i++) {
        printf("\n%*s", ind+4, "");
        nchars = display_string(stdout, name[i], TRUE);
        printf("%*s = ", MAX(0, 16-nchars), "");

        if (native<0) {
            printf("0x");
            for (j=0; j<dst_size; j++)
                printf("%02x", value[i*dst_size+j]);
        } else if (H5T_SGN_NONE==H5Tget_sign(native)) {
 	    /*On SGI Altix(cobalt), wrong values were printed out with "value+i*dst_size"
 	     *strangely, unless use another pointer "copy".*/
 	    copy = value+i*dst_size;
            HDfprintf(stdout,"%"H5_PRINTF_LL_WIDTH"u",
            *((unsigned long long*)((void*)copy)));
        } else {
 	    /*On SGI Altix(cobalt), wrong values were printed out with "value+i*dst_size"
 	     *strangely, unless use another pointer "copy".*/
 	    copy = value+i*dst_size;
            HDfprintf(stdout,"%"H5_PRINTF_LL_WIDTH"d",
            *((long long*)((void*)copy)));
        }
    }

    /* Release resources */
    for (i=0; i<nmembs; i++) free(name[i]);
    free(name);
    free(value);
    H5Tclose(super);

    if (0==nmembs) printf("\n%*s <empty>", ind+4, "");
    printf("\n%*s}", ind, "");
    return TRUE;
}


/*-------------------------------------------------------------------------
 * Function: display_string_type
 *
 * Purpose: Print information about a string data type.
 *
 * Return: Success: TRUE
 *
 *  Failure: FALSE, nothing printed
 *
 * Programmer: Robb Matzke
 *              Thursday, November  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
display_string_type(hid_t type, int UNUSED ind)
{
    H5T_str_t  pad;
    const char  *pad_s=NULL;
    H5T_cset_t  cset;
    const char  *cset_s=NULL;

    if (H5T_STRING!=H5Tget_class(type)) return FALSE;

    /* Padding */
    pad = H5Tget_strpad(type);
    switch (pad) {
        case H5T_STR_NULLTERM:
            pad_s = "null-terminated";
            break;
        case H5T_STR_NULLPAD:
            pad_s = "null-padded";
            break;
        case H5T_STR_SPACEPAD:
            pad_s = "space-padded";
            break;
        case H5T_STR_RESERVED_3:
        case H5T_STR_RESERVED_4:
        case H5T_STR_RESERVED_5:
        case H5T_STR_RESERVED_6:
        case H5T_STR_RESERVED_7:
        case H5T_STR_RESERVED_8:
        case H5T_STR_RESERVED_9:
        case H5T_STR_RESERVED_10:
        case H5T_STR_RESERVED_11:
        case H5T_STR_RESERVED_12:
        case H5T_STR_RESERVED_13:
        case H5T_STR_RESERVED_14:
        case H5T_STR_RESERVED_15:
        case H5T_STR_ERROR:
            pad_s = "unknown-format";
            break;
        default:
            ;
        break;
    }

    /* Character set */
    cset = H5Tget_cset(type);
    switch (cset) {
        case H5T_CSET_ASCII:
            cset_s = "ASCII";
            break;
        case H5T_CSET_UTF8:
            cset_s = "UTF-8";
            break;
        case H5T_CSET_RESERVED_2:
        case H5T_CSET_RESERVED_3:
        case H5T_CSET_RESERVED_4:
        case H5T_CSET_RESERVED_5:
        case H5T_CSET_RESERVED_6:
        case H5T_CSET_RESERVED_7:
        case H5T_CSET_RESERVED_8:
        case H5T_CSET_RESERVED_9:
        case H5T_CSET_RESERVED_10:
        case H5T_CSET_RESERVED_11:
        case H5T_CSET_RESERVED_12:
        case H5T_CSET_RESERVED_13:
        case H5T_CSET_RESERVED_14:
        case H5T_CSET_RESERVED_15:
        case H5T_CSET_ERROR:
            cset_s = "unknown-character-set";
            break;
        default:
            ;
        break;
    }

    if (H5Tis_variable_str(type)) {
        printf("variable-length");
    } else {
        printf("%lu-byte", (unsigned long)H5Tget_size(type));
    }
    printf(" %s %s string", pad_s, cset_s);
    return TRUE;
}


/*-------------------------------------------------------------------------
 * Function: display_reference_type
 *
 * Purpose: Prints information about a reference data type.
 *
 * Return: Success: TRUE
 *
 *  Failure: FALSE, nothing printed
 *
 * Programmer: Robb Matzke
 *              Thursday, November  5, 1998
 *
 * Modifications:
 *   Robb Matzke, 1999-06-04
 *  Knows about object and dataset region references.
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
display_reference_type(hid_t type, int UNUSED ind)
{
    if (H5T_REFERENCE!=H5Tget_class(type)) return FALSE;

    if (H5Tequal(type, H5T_STD_REF_OBJ)==TRUE) {
        printf("object reference");
    } else if (H5Tequal(type, H5T_STD_REF_DSETREG)==TRUE) {
        printf("dataset region reference");
    } else {
        printf("%lu-byte unknown reference",
                (unsigned long)H5Tget_size(type));
    }

    return TRUE;
}


/*-------------------------------------------------------------------------
 * Function: display_opaque_type
 *
 * Purpose: Prints information about an opaque data type.
 *
 * Return: Success: TRUE
 *
 *  Failure: FALSE, nothing printed
 *
 * Programmer: Robb Matzke
 *              Monday, June  7, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
display_opaque_type(hid_t type, int ind)
{
    char *tag;
    size_t size;

    if (H5T_OPAQUE!=H5Tget_class(type)) return FALSE;

    size = H5Tget_size(type);
    printf("%lu-byte opaque type", (unsigned long)size);
    if ((tag=H5Tget_tag(type))) {
        printf("\n%*s(tag = \"", ind, "");
        display_string(stdout, tag, FALSE);
        printf("\")");
        free(tag);
    }
    return TRUE;
}


/*-------------------------------------------------------------------------
 * Function:    display_vlen_type
 *
 * Purpose:     Print information about a variable-length type
 *
 * Return:      Success:        TRUE
 *
 *              Failure:        FALSE
 *
 * Programmer:  Robb Matzke
 *              Friday, December  1, 2000
 *
 * Modifications:
 *-------------------------------------------------------------------------
 */
static hbool_t
display_vlen_type(hid_t type, int ind)
{
    hid_t       super;

    if (H5T_VLEN!=H5Tget_class(type)) return FALSE;

    printf("variable length of\n%*s", ind+4, "");
    super = H5Tget_super(type);
    display_type(super, ind+4);
    H5Tclose(super);
    return TRUE;
}


/*---------------------------------------------------------------------------
 * Purpose:     Print information about an array type
 *
 * Return:      Success:        TRUE
 *
 *              Failure:        FALSE
 *
 * Programmer:  Robb Matzke
 *              Thursday, January 31, 2002
 *
 * Modifications:
 *---------------------------------------------------------------------------
 */
static hbool_t
display_array_type(hid_t type, int ind)
{
    hid_t       super;
    int         ndims, i;
    hsize_t     *dims=NULL;

    if (H5T_ARRAY!=H5Tget_class(type)) return FALSE;
    ndims = H5Tget_array_ndims(type);
    if (ndims) {
        dims = (hsize_t *)malloc(ndims*sizeof(dims[0]));
        H5Tget_array_dims2(type, dims);

        /* Print dimensions */
        for (i=0; i<ndims; i++)
            HDfprintf(stdout, "%s%Hu" , i?",":"[", dims[i]);
        putchar(']');

        free(dims);
    } else {
        fputs(" [SCALAR]", stdout);
    }


    /* Print parent type */
    putchar(' ');
    super = H5Tget_super(type);
    display_type(super, ind+4);
    H5Tclose(super);
    return TRUE;
}


/*-------------------------------------------------------------------------
 * Function: display_bitfield_type
 *
 * Purpose: Print information about a bitfield type.
 *
 * Return: Success: TRUE
 *
 *  Failure: FALSE, nothing printed
 *
 * Programmer: Pedro Vicente
 *              Tuesday, May  20, 2003
 *
 * Modifications:
 *              Robb Matzke, LLNL 2003-06-05
 *              Generalized Pedro's original if/then/else.  Also display
 *              precision/offset information.
 *-------------------------------------------------------------------------
 */
static hbool_t
display_bitfield_type(hid_t type, int ind)
{
    H5T_order_t order;          /* byte order value */
    const char  *order_s=NULL;  /* byte order string */

    if (H5T_BITFIELD!=H5Tget_class(type)) return FALSE;
    if (H5Tget_size(type)>1) {
        order = H5Tget_order(type);
        if (H5T_ORDER_LE==order) {
            order_s = " little-endian";
        } else if (H5T_ORDER_BE==order) {
            order_s = " big-endian";
        } else if (H5T_ORDER_VAX==order) {
            order_s = " mixed-endian";
        } else {
            order_s = "unknown-byte-order";
        }
    } else {
        order_s = "";
    }

    printf("%lu-bit%s bitfield",
           (unsigned long)(8*H5Tget_size(type)), order_s);
    display_precision(type, ind);
    return TRUE;
}


/*-------------------------------------------------------------------------
 * Function: display_type
 *
 * Purpose: Prints a data type definition.  The definition is printed
 *  without any leading space or trailing line-feed (although
 *  there might be line-feeds inside the type definition).  The
 *  first line is assumed to have IND characters before it on
 *  the same line (printed by the caller).
 *
 * Return: void
 *
 * Programmer: Robb Matzke
 *              Thursday, November  5, 1998
 *
 * Modifications:
 *   Robb Matzke, 1999-06-11
 *  Prints the OID of shared data types.
 *
 *-------------------------------------------------------------------------
 */
static void
display_type(hid_t type, int ind)
{
    H5T_class_t  data_class = H5Tget_class(type);

    /* Bad data type */
    if (type<0) {
        printf("<ERROR>");
        return;
    }

    /* Shared? If so then print the type's OID */
    if(H5Tcommitted(type)) {
        H5O_info_t  oi;

        if(H5Oget_info(type, &oi) >= 0)
            printf("shared-%lu:"H5_PRINTF_HADDR_FMT" ",
                    oi.fileno, oi.addr);
        else
            printf("shared ");
    } /* end if */

    /* Print the type */
    if((!simple_output_g && display_native_type(type, ind)) ||
            display_ieee_type(type, ind) ||
            display_int_type(type, ind) ||
            display_float_type(type, ind) ||
            display_cmpd_type(type, ind) ||
            display_enum_type(type, ind) ||
            display_string_type(type, ind) ||
            display_reference_type(type, ind) ||
            display_vlen_type(type, ind) ||
            display_array_type(type, ind) ||
            display_opaque_type(type, ind) ||
            display_bitfield_type(type, ind))
        return;

    /* Unknown type */
    printf("%lu-byte class-%u unknown",
            (unsigned long)H5Tget_size(type), (unsigned)data_class);
}


/*-------------------------------------------------------------------------
 * Function: dump_dataset_values
 *
 * Purpose: Prints all values of a dataset.
 *
 * Return: void
 *
 * Programmer: Robb Matzke
 *              Tuesday, July 21, 1998
 *
 * Modifications:
 *  Robb Matzke, 1999-09-27
 *  Understands the simple_output_g switch which causes data to
 *  be displayed in a more machine-readable format.
 *-------------------------------------------------------------------------
 */
static void
dump_dataset_values(hid_t dset)
{
    hid_t  f_type = H5Dget_type(dset);
    size_t  size = H5Tget_size(f_type);
    h5tool_format_t  info;
    char  string_prefix[64];
    static char         fmt_double[16], fmt_float[16];

    /* Set to all default values and then override */
    memset(&info, 0, sizeof info);

    if (simple_output_g) {
        info.idx_fmt = "";
        info.line_ncols = 65535; /*something big*/
        info.line_per_line = 1;
        info.line_multi_new = 0;
        info.line_pre  = "        ";
        info.line_cont = "         ";

        info.arr_pre = "";
        info.arr_suf = "";
        info.arr_sep = " ";

        if (label_g)
        {
            info.cmpd_pre = "{";
            info.cmpd_suf = "}";
        }
        else
        {
            info.cmpd_pre = "";
            info.cmpd_suf = "";
        }
        info.cmpd_sep = " ";

        if (label_g) info.cmpd_name = "%s=";

        info.elmt_suf1 = " ";
        info.str_locale = ESCAPE_HTML;

    } else {
        info.idx_fmt = "(%s)";
        if (no_line_wrap_g) {
            info.line_ncols = 65535;
            info.line_per_line = 1;
        }
        else
            info.line_ncols = width_g;
        info.line_multi_new = 1;
        if (label_g) info.cmpd_name = "%s=";
        info.line_pre  = "        %s ";
        info.line_cont = "        %s  ";
        info.str_repeat = 8;
    }

    /* Floating point types should display full precision */
    sprintf(fmt_float, "%%1.%dg", FLT_DIG);
    info.fmt_float = fmt_float;
    sprintf(fmt_double, "%%1.%dg", DBL_DIG);
    info.fmt_double = fmt_double;

    info.dset_format =  "DSET-%s ";
    info.dset_hidefileno = 0;

    info.obj_format = "-%lu:"H5_PRINTF_HADDR_FMT;
    info.obj_hidefileno = 0;

    info.dset_blockformat_pre = "%sBlk%lu: ";
    info.dset_ptformat_pre = "%sPt%lu: ";

    info.line_indent = "";

    if (hexdump_g) {
        /* Print all data in hexadecimal format if the `-x' or `--hexdump'
         * command line switch was given. */
        info.raw = TRUE;
    } else if (string_g && 1==size && H5T_INTEGER==H5Tget_class(f_type)) {
        /* Print 1-byte integer data as an ASCI character string instead of
         * integers if the `-s' or `--string' command-line option was given. */
        info.ascii = TRUE;
        info.elmt_suf1 = "";
        info.elmt_suf2 = "";
        strcpy(string_prefix, info.line_pre);
        strcat(string_prefix, "\"");
        info.line_pre = string_prefix;
        info.line_suf = "\"";
    }

    /* Print all the values. */
    printf("    Data:\n");
    if (h5tools_dump_dset(stdout, &info, dset, -1, NULL, -1) < 0)
        printf("        Unable to print data.\n");

    H5Tclose(f_type);
}


/*-------------------------------------------------------------------------
 * Function: list_attr
 *
 * Purpose: Prints information about attributes.
 *
 * Return: Success: 0
 *
 *  Failure: -1
 *
 * Programmer: Robb Matzke
 *              Friday, June  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
list_attr(hid_t obj, const char *attr_name, const H5A_info_t UNUSED *ainfo,
    void UNUSED *op_data)
{
    hid_t attr, space, type, p_type;
    hsize_t size[H5S_MAX_RANK], nelmts = 1;
    int  ndims, i, n;
    size_t need;
    hsize_t     temp_need;
    void *buf;
    h5tool_format_t info;
    H5S_class_t space_type;

    printf("    Attribute: ");
    n = display_string(stdout, attr_name, TRUE);
    printf("%*s", MAX(0, (9 - n)), "");

    if((attr = H5Aopen(obj, attr_name, H5P_DEFAULT))) {
        space = H5Aget_space(attr);
        type = H5Aget_type(attr);

        /* Data space */
        ndims = H5Sget_simple_extent_dims(space, size, NULL);
        space_type = H5Sget_simple_extent_type(space);
        switch(space_type) {
            case H5S_SCALAR:
                /* scalar dataspace */
                puts(" scalar");
                break;

            case H5S_SIMPLE:
                /* simple dataspace */
                printf(" {");
                for (i=0; i<ndims; i++) {
                    HDfprintf(stdout, "%s%Hu", i?", ":"", size[i]);
                    nelmts *= size[i];
                }
                puts("}");
                break;

            case H5S_NULL:
                /* null dataspace */
                puts(" null");
                break;

            default:
                /* Unknown dataspace type */
                puts(" unknown");
                break;
        } /* end switch */

        /* Data type */
        printf("        Type:      ");
        display_type(type, 15);
        putchar('\n');

        /* Data */
        memset(&info, 0, sizeof info);
        info.line_multi_new = 1;
        if(nelmts < 5) {
            info.idx_fmt = "";
            info.line_1st  = "        Data:  ";
            info.line_pre  = "               ";
            info.line_cont = "                ";
            info.str_repeat = 8;

        } else {
            printf("        Data:\n");
            info.idx_fmt = "(%s)";
            info.line_pre  = "            %s ";
            info.line_cont = "            %s  ";
            info.str_repeat = 8;
        }

        info.line_ncols = width_g;
        if(label_g)
            info.cmpd_name = "%s=";
        if(string_g && 1==H5Tget_size(type) &&
                H5T_INTEGER==H5Tget_class(type)) {
            info.ascii = TRUE;
            info.elmt_suf1 = "";
            info.elmt_suf2 = "";
            info.idx_fmt  = "(%s)";
            info.line_pre = "            %s \"";
            info.line_suf = "\"";
        } /* end if */

        /* values of type reference */
        info.obj_format = "-%lu:"H5_PRINTF_HADDR_FMT;
        info.obj_hidefileno = 0;
        if(hexdump_g)
           p_type = H5Tcopy(type);
        else
           p_type = h5tools_get_native_type(type);

        if(p_type >= 0) {
            /* VL data special information */
            unsigned int        vl_data = 0; /* contains VL datatypes */

            /* Check if we have VL data in the dataset's datatype */
            if (h5tools_detect_vlen(p_type) == TRUE)
                vl_data = TRUE;

            temp_need= nelmts * MAX(H5Tget_size(type), H5Tget_size(p_type));
            assert(temp_need == (hsize_t)((size_t)temp_need));
            need = (size_t)temp_need;
            buf = malloc(need);
            assert(buf);
            if(H5Aread(attr, p_type, buf) >= 0)
               h5tools_dump_mem(stdout, &info, attr, p_type, space, buf, -1);

            /* Reclaim any VL memory, if necessary */
            if (vl_data)
                H5Dvlen_reclaim(p_type, space, H5P_DEFAULT, buf);

            free(buf);
            H5Tclose(p_type);
        } /* end if */

        H5Sclose(space);
        H5Tclose(type);
        H5Aclose(attr);
    } else
        putchar('\n');

    return 0;
}


/*-------------------------------------------------------------------------
 * Function: dataset_list1
 *
 * Purpose: List information about a dataset which should appear on the
 *  same line as the dataset name.  This information will precede
 *  information which is applicable to all objects which will be
 *  printed by the caller.
 *
 * Return: Success: 0
 *
 *  Failure: -1
 *
 * Programmer: Robb Matzke
 *              Thursday, August 27, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
dataset_list1(hid_t dset)
{
    hsize_t     cur_size[H5S_MAX_RANK];   /* current dataset dimensions */
    hsize_t     max_size[H5S_MAX_RANK];   /* maximum dataset dimensions */
    hid_t       space;          /* data space                 */
    int         ndims;          /* dimensionality             */
    H5S_class_t space_type;     /* type of dataspace          */
    int   i;

    /* Information that goes on the same row as the name.  The name has
     * already been printed. */
    space = H5Dget_space(dset);
    space_type = H5Sget_simple_extent_type(space);
    ndims = H5Sget_simple_extent_dims(space, cur_size, max_size);
    printf (" {");
    for (i=0; i<ndims; i++) {
        HDfprintf (stdout, "%s%Hu", i?", ":"", cur_size[i]);
        if (max_size[i]==H5S_UNLIMITED) {
            HDfprintf (stdout, "/%s", "Inf");
        } else if (max_size[i]!=cur_size[i] || verbose_g>0) {
            HDfprintf(stdout, "/%Hu", max_size[i]);
        }
    }
    if (space_type==H5S_SCALAR) printf("SCALAR");
    else if (space_type==H5S_NULL) printf("NULL");
    putchar('}');
    H5Sclose (space);

    return 0;
}


/*-------------------------------------------------------------------------
 * Function: dataset_list2
 *
 * Purpose: List information about a dataset which should appear after
 *  information which is general to all objects.
 *
 * Return: Success: 0
 *
 *  Failure: -1
 *
 * Programmer: Robb Matzke
 *              Thursday, August 27, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
dataset_list2(hid_t dset, const char UNUSED *name)
{
    hid_t       dcpl;           /* dataset creation property list */
    hid_t       type;           /* data type of dataset */
    hid_t       space;          /* data space of dataset */
    int         nf;             /* number of filters */
    unsigned    filt_flags;     /* filter flags */
    H5Z_filter_t filt_id;       /* filter identification number */
    unsigned    cd_values[20];  /* filter client data values */
    size_t      cd_nelmts;      /* filter client number of values */
    size_t      cd_num;         /* filter client data counter */
    char        f_name[256];    /* filter/file name */
    char        s[64];          /* temporary string buffer */
    off_t       f_offset;       /* offset in external file */
    hsize_t     f_size;         /* bytes used in external file */
    hsize_t     total, used;    /* total size or offset */
    int         ndims;          /* dimensionality */
    int         n, max_len;     /* max extern file name length */
    double      utilization;    /* percent utilization of storage */
    H5T_class_t tclass;         /* datatype class identifier */
    int         i;

    if(verbose_g > 0) {
        dcpl = H5Dget_create_plist(dset);
        space = H5Dget_space(dset);
        type = H5Dget_type(dset);

        /* Print information about chunked storage */
        if (H5D_CHUNKED==H5Pget_layout(dcpl)) {
            hsize_t     chsize[64];     /* chunk size in elements */

            ndims = H5Pget_chunk(dcpl, NELMTS(chsize), chsize/*out*/);
            printf("    %-10s {", "Chunks:");
            total = H5Tget_size(type);
            for (i=0; i<ndims; i++) {
                printf("%s%lu", i?", ":"", (unsigned long)(chsize[i]));
                total *= chsize[i];
            }
            printf("} %lu bytes\n", (unsigned long)total);
        }

        /* Print total raw storage size */
        total = H5Sget_simple_extent_npoints(space) * H5Tget_size(type);
        used = H5Dget_storage_size(dset);
        tclass = H5Tget_class(type);
        printf("    %-10s ", "Storage:");
        switch (tclass)
        {

        case H5T_VLEN:
            printf("information not available");
            break;

        case H5T_REFERENCE:
            if ( H5Tequal(type, H5T_STD_REF_DSETREG))
            {
                printf("information not available");
            }
            break;

        default:
            printf("%lu logical byte%s, %lu allocated byte%s",
                (unsigned long)total, 1==total?"":"s",
                (unsigned long)used, 1==used?"":"s");
            if (used>0)
            {
                utilization = (total*100.0)/used;
                printf(", %1.2f%% utilization", utilization);
            }

        }


        putchar('\n');

        /* Print information about external strorage */
        if((nf = H5Pget_external_count(dcpl)) > 0) {
            for(i = 0, max_len = 0; i < nf; i++) {
                H5Pget_external(dcpl, (unsigned)i, sizeof(f_name), f_name, NULL, NULL);
                n = display_string(NULL, f_name, TRUE);
                max_len = MAX(max_len, n);
            } /* end for */
            printf("    %-10s %d external file%s\n",
                    "Extern:", nf, 1==nf?"":"s");
            printf("        %4s %10s %10s %10s %s\n",
                    "ID", "DSet-Addr", "File-Addr", "Bytes", "File");
            printf("        %4s %10s %10s %10s ",
                    "----", "----------", "----------", "----------");
            for (i=0; i<max_len; i++) putchar('-');
            putchar('\n');
            for (i=0, total=0; i<nf; i++) {
                if (H5Pget_external(dcpl, (unsigned)i, sizeof(f_name), f_name, &f_offset, &f_size)<0) {
                    HDfprintf(stdout,
                            "        #%03d %10Hu %10s %10s ***ERROR*** %s\n",
                            i, total, "", "",
                            i+1<nf?"Following addresses are incorrect":"");
                } else if (H5S_UNLIMITED==f_size) {
                    HDfprintf(stdout, "        #%03d %10Hu %10Hu %10s ",
                            i, total, (hsize_t)f_offset, "INF");
                    display_string(stdout, f_name, TRUE);
                } else {
                    HDfprintf(stdout, "        #%03d %10Hu %10Hu %10Hu ",
                            i, total, (hsize_t)f_offset, f_size);
                    display_string(stdout, f_name, TRUE);
                }
                putchar('\n');
                total += f_size;
            }
            printf("        %4s %10s %10s %10s ",
                    "----", "----------", "----------", "----------");
            for (i=0; i<max_len; i++)
                putchar('-');
            putchar('\n');
        } /* end if */

        /* Print information about raw data filters */
        if((nf = H5Pget_nfilters(dcpl)) > 0) {
            for(i = 0; i < nf; i++) {
                cd_nelmts = NELMTS(cd_values);
                filt_id = H5Pget_filter2(dcpl, (unsigned)i, &filt_flags, &cd_nelmts,
                        cd_values, sizeof(f_name), f_name, NULL);
                f_name[sizeof(f_name) - 1] = '\0';
                sprintf(s, "Filter-%d:", i);
                printf("    %-10s %s-%u %s {", s,
                        (f_name[0] ? f_name : "method"),
                        (unsigned)filt_id,
                        ((filt_flags & H5Z_FLAG_OPTIONAL) ? "OPT" : ""));
                for(cd_num = 0; cd_num < cd_nelmts; cd_num++)
                    printf("%s%u", (cd_num ? ", " : ""), cd_values[cd_num]);
                printf("}\n");
            } /* end for */
        } /* end if */

        /* Print data type */
        printf("    %-10s ", "Type:");
        display_type(type, 15);
        printf("\n");

        /* Print address information */
        if(address_g)
            H5Ddebug(dset);

        /* Close stuff */
        H5Tclose(type);
        H5Sclose(space);
        H5Pclose(dcpl);
    } /* end if */

    if(data_g)
        dump_dataset_values(dset);

    return 0;
} /* end dataset_list2() */


/*-------------------------------------------------------------------------
 * Function: datatype_list2
 *
 * Purpose: List information about a datatype which should appear after
 *  information which is general to all objects.
 *
 * Return: Success: 0
 *
 *  Failure: -1
 *
 * Programmer: Robb Matzke
 *              Thursday, November  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
datatype_list2(hid_t type, const char UNUSED *name)
{
    if (verbose_g>0) {
        printf("    %-10s ", "Type:");
        display_type(type, 15);
        printf("\n");
    }
    return 0;
}


/*-------------------------------------------------------------------------
 * Function: list_obj
 *
 * Purpose: Prints information about an object
 *
 * Return: Success: 0
 *
 *  Failure: -1
 *
 * Programmer: Quincey Koziol
 *              Tuesday, November 6, 2007
 *
 *-------------------------------------------------------------------------
 */
static herr_t
list_obj(const char *name, const H5O_info_t *oinfo, const char *first_seen, void *_iter)
{
    H5O_type_t obj_type = oinfo->type;          /* Type of the object */
    iter_t *iter = (iter_t*)_iter;

    /* Print the link's name, either full name or base name */
    if(!iter->symlink_target)
        display_obj_name(stdout, iter, name, "");

    /* Check object information */
    if(oinfo->type < 0 || oinfo->type >= H5O_TYPE_NTYPES) {
        printf("Unknown type(%d)", (int)oinfo->type);
        obj_type = H5O_TYPE_UNKNOWN;
    }
    if(iter->symlink_target)
        fputc('{', stdout);
    if(obj_type >= 0 && dispatch_g[obj_type].name)
        fputs(dispatch_g[obj_type].name, stdout);

    /* Check if we've seen this object before */
    if(first_seen) {
        printf(", same as ");
        display_string(stdout, first_seen, TRUE);
        if(!iter->symlink_target)
            printf("\n");
    } /* end if */
    else {
        hid_t obj = (-1);               /* ID of object opened */

        /* Open the object.  Not all objects can be opened.  If this is the case
         * then return right away.
         */
        if(obj_type >= 0 && (obj = H5Oopen(iter->fid, name, H5P_DEFAULT)) < 0) {
            printf(" *ERROR*\n");
            goto done;
        } /* end if */

        /* List the first line of information for the object. */
        if(obj_type >= 0 && dispatch_g[obj_type].list1)
            (dispatch_g[obj_type].list1)(obj);
        if(!iter->symlink_target || (verbose_g > 0))
            putchar('\n');

        /* Show detailed information about the object, beginning with information
         * which is common to all objects. */
        if(verbose_g > 0) {
            size_t buf_size = 0;
            char* comment = NULL;
            ssize_t cmt_bufsize = -1;

            /* Display attributes */
            if(obj_type >= 0)
                H5Aiterate2(obj, H5_INDEX_NAME, H5_ITER_INC, NULL, list_attr, NULL);

            /* Object location & reference count */
            printf("    %-10s %lu:"H5_PRINTF_HADDR_FMT"\n", "Location:", oinfo->fileno, oinfo->addr);
            printf("    %-10s %u\n", "Links:", (unsigned)oinfo->rc);

            /* Modification time */
            if(oinfo->mtime > 0) {
                char buf[256];
                struct tm *tm;

                if(simple_output_g)
                    tm = HDgmtime(&(oinfo->mtime));
                else
                    tm = HDlocaltime(&(oinfo->mtime));
                if(tm) {
                    HDstrftime(buf, sizeof(buf), "%Y-%m-%d %H:%M:%S %Z", tm);
                    printf("    %-10s %s\n", "Modified:", buf);
                } /* end if */
            } /* end if */

            /* Object comment */
            cmt_bufsize = H5Oget_comment(obj, comment, buf_size);

            /* if the actual length of the comment is longer than cmt_bufsize, then call
             * H5Oget_comment again with the correct value.
             * If the call to H5Oget_comment returned an error, skip this block */
            if (cmt_bufsize > 0) {
                comment = (char *)HDmalloc((size_t)cmt_bufsize); /* new_size including null terminator */
                if(comment) {
                    cmt_bufsize = H5Oget_comment(obj, comment, cmt_bufsize);
                    if(cmt_bufsize > 0) {
                        printf("    %-10s \"", "Comment:");
                        display_string(stdout, comment, FALSE);
                        puts("\"");
                    } /* end if */
                    HDfree(comment);
                }
            }
        } /* end if */

        /* Detailed list for object */
        if(obj_type >= 0 && dispatch_g[obj_type].list2)
            (dispatch_g[obj_type].list2)(obj, name);

        /* Close the object. */
        if(obj_type >= 0)
            H5Oclose(obj);
    } /* end else */

done:
    if(iter->symlink_target) {
        fputs("}\n", stdout);
        iter->symlink_target = FALSE;
    }
    return 0;
} /* end list_obj() */



/*-------------------------------------------------------------------------
 * Function: list_lnk
 *
 * Purpose: Prints information about a link
 *
 * Return: Success: 0
 *
 *  Failure: -1
 *
 * Programmer: Quincey Koziol
 *              Thursday, November 8, 2007
 *
 *-------------------------------------------------------------------------
 */
static herr_t
list_lnk(const char *name, const H5L_info_t *linfo, void *_iter)
{
    char *buf=NULL;
    iter_t *iter = (iter_t*)_iter;
    int ret;
    h5tool_link_info_t lnk_info;

    /* init linkinfo struct */
    memset(&lnk_info, 0, sizeof(h5tool_link_info_t));

    /* if verbose, make H5tools_get_symlink_info() display more */
    if (verbose_g)
        lnk_info.opt.msg_mode=1;

    /* Print the link's name, either full name or base name */
    display_obj_name(stdout, iter, name, "");

    switch(linfo->type) {
        case H5L_TYPE_SOFT:
            ret = H5tools_get_symlink_info(iter->fid, name, &lnk_info, follow_symlink_g);
            /* lnk_info.trg_path is malloced in H5tools_get_symlink_info()
             * so it will be freed via buf later */
            buf = lnk_info.trg_path;
            /* error */
            if (ret < 0)
                goto done;
            /* no dangling link option given and detect dangling link */
            else if (no_dangling_link_g && ret == 0)
                iter->symlink_list->dangle_link = TRUE;

            HDfputs("Soft Link {", stdout);
            HDfputs(buf, stdout);
            HDfputc('}', stdout);
            if(follow_symlink_g)
            {
                hbool_t orig_grp_literal = grp_literal_g;
                HDfputc(' ', stdout);

                /* Check if we have already seen this softlink */
                if(symlink_is_visited(iter->symlink_list, linfo->type, NULL, buf)) 
                {
                    HDfputs("{Already Visited}\n", stdout);
                    goto done;
                }

                /* Add this link to the list of seen softlinks */
                if(symlink_visit_add(iter->symlink_list, linfo->type, NULL, buf) < 0) 
                    goto done;

                /* Adjust user data to specify that we are operating on the
                 * target of an soft link */
                iter->symlink_target = TRUE;

                /* Prevent recursive listing of soft link target if
                 * recursive_g is off */
                if(!recursive_g)
                    grp_literal_g = TRUE;
                /* Recurse through the soft link */
                if(visit_obj(iter->fid, name, iter) < 0) 
                {
                    grp_literal_g = orig_grp_literal;
                    goto done;
                }

                grp_literal_g = orig_grp_literal;
            }
            else
                HDfputc('\n', stdout);

            break;

        case H5L_TYPE_EXTERNAL:
            {
            const char *filename;
            const char *path;
            hbool_t follow_link = follow_symlink_g || follow_elink_g;

            ret = H5tools_get_symlink_info(iter->fid, name, &lnk_info, follow_link);
            /* lnk_info.trg_path is malloced in H5tools_get_symlink_info()
             * so it will be freed via buf later */
            buf = lnk_info.trg_path;
            /* error */
            if (ret < 0)
                goto done;
            /* no dangling link option given and detect dangling link */
            else if (no_dangling_link_g && ret == 0)
                iter->symlink_list->dangle_link = TRUE;

            if(H5Lunpack_elink_val(buf, linfo->u.val_size, NULL, &filename, &path) < 0) 
                goto done;

            HDfputs("External Link {", stdout);
            HDfputs(filename, stdout);
            HDfputc('/', stdout);
            if(*path != '/')
                HDfputc('/', stdout);
            HDfputs(path, stdout);
            HDfputc('}', stdout);

            /* Recurse through the external link */
            /* keep the follow_elink_g for backward compatibility with -E */
            if(follow_link)
            {
                hbool_t orig_grp_literal = grp_literal_g;
                HDfputc(' ', stdout);

                /* Check if we have already seen this elink */
                if(symlink_is_visited(iter->symlink_list, linfo->type, filename, path)) 
                {
                    HDfputs("{Already Visited}\n", stdout);
                    goto done;
                }

                /* Add this link to the list of seen elinks */
                if(symlink_visit_add(iter->symlink_list, linfo->type, filename, path) < 0) 
                {
                    goto done;
                }

                /* Adjust user data to specify that we are operating on the
                 * target of an external link */
                iter->symlink_target = TRUE;

                /* Prevent recursive listing of external link target if
                 * recursive_g is off */
                if(!recursive_g)
                    grp_literal_g = TRUE;

                /* Recurse through the external link */
                if(visit_obj(iter->fid, name, iter) < 0) {
                    grp_literal_g = orig_grp_literal;
                    goto done;
                }

                grp_literal_g = orig_grp_literal;
            }
            else
                HDfputc('\n', stdout);

            }
            break;

        default:
            HDfputs("UD Link {cannot follow UD links}\n", stdout);
            break;
    } /* end switch */

done:
    if (buf)
        HDfree(buf);
    return 0;
} /* end list_lnk() */


/*-------------------------------------------------------------------------
 * Function: visit_obj
 *
 * Purpose: Begins iteration on an object
 *
 * Return: 
 *  Success: 0
 *  Failure: -1
 *
 * Programmer: Neil Fortner
 *              Wednesday, August 21, 2008
 *             Mostly copied from main()
 *
 *-------------------------------------------------------------------------
 */
static herr_t
visit_obj(hid_t file, const char *oname, iter_t *iter)
{
    H5O_info_t oi;              /* Information for object */

    /* Retrieve info for object to list */
    if(H5Oget_info_by_name(file, oname, &oi, H5P_DEFAULT) < 0) {
        if(iter->symlink_target) {
            HDfputs("{**NOT FOUND**}\n", stdout);
            iter->symlink_target = FALSE;
        }
        else
            display_obj_name(stdout, iter, oname, "**NOT FOUND**");
        return -1;
    } /* end if */

    /* Check for group iteration */
    if(H5O_TYPE_GROUP == oi.type && !grp_literal_g) {
        /* Get ID for group */
        if(!iter->symlink_target && (iter->gid = H5Gopen2(file, oname, H5P_DEFAULT)) < 0) {
            fprintf(stderr, "%s: unable to open '%s' as group\n", iter->fname, oname);
            return 0;   /* Previously "continue", when this code was in main().
                         * We don't "continue" here in order to close the file
                         * and free the file name properly. */
        } /* end if */

        /* Delay specifying the name start point so the original object name is
         * displayed if it is a link or non-group object */
        iter->name_start = iter->base_len;

        /* Specified name is a group. List the complete contents of the group. */
        h5trav_visit(file, oname, (hbool_t) (display_root_g || iter->symlink_target), recursive_g, list_obj, list_lnk, iter);

        /* Close group */
        if(!iter->symlink_target)
            H5Gclose(iter->gid);
    } /* end if */
    else {
        /* Use file ID for root group ID */
        iter->gid = file;

        /* Specified name is a non-group object -- list that object */
        list_obj(oname, &oi, NULL, iter);
    } /* end else */

    return 0;
}


/*-------------------------------------------------------------------------
 * Function: get_width
 *
 * Purpose: Figure out how wide the screen is.  This is highly
 *  unportable, but the user can always override the width we
 *  detect by giving a command-line option. These code snippets
 *  were borrowed from the GNU less(1).
 *
 * Return: Success: Number of columns.
 *
 *  Failure: Some default number of columms.
 *
 * Programmer: Robb Matzke
 *              Friday, November  6, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
get_width(void)
{
    int  width = 80;  /*the default   */
    char *s;

    /* Try to get it from the COLUMNS environment variable first since it's
     * value is sometimes wrong. */
    if ((s=getenv("COLUMNS")) && *s && isdigit((int)*s))
        width = (int)strtol(s, NULL, 0);

#if defined(H5_HAVE_STRUCT_VIDEOCONFIG) && defined(H5_HAVE__GETVIDEOCONFIG)
    {
        /* Microsoft C */
        struct videoconfig w;
        _getvideoconfig(&w);
        width = w.numtextcols;
    }
#elif defined(H5_HAVE_STRUCT_TEXT_INFO) && defined(H5_HAVE_GETTEXTINFO)
    {
        /* Borland C or DJGPPC */
        struct text_info w;
        gettextinfo(&w);
        width = w.screenwidth;
    }
#elif defined(H5_HAVE_GETCONSOLESCREENBUFFERINFO)
    {
        /* Win32 C */
        CONSOLE_SCREEN_BUFFER_INFO scr;
        GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &scr);
        width = scr.srWindow.Right - scr.srWindow.Left + 1;
    }
#elif defined(H5_HAVE__SCRSIZE)
    {
        /* OS/2 */
        int w[2];
        _scrsize(w);
        width = w[0];
    }
#elif defined(H5_HAVE_TIOCGWINSZ) && defined(H5_HAVE_IOCTL)
    {
        /* Unix with ioctl(TIOCGWINSZ) */
        struct winsize w;
        if (ioctl(2, TIOCGWINSZ, &w)>=0 && w.ws_col>0)
            width = w.ws_col;
    }
#elif defined(H5_HAVE_TIOCGETD) && defined(H5_HAVE_IOCTL)
    {
        /* Unix with ioctl(TIOCGETD) */
        struct uwdata w;
        if (ioctl(2, WIOCGETD, &w)>=0 && w.uw_width>0)
            width = w.uw_width / w.uw_hs;
    }
#endif

    /* Set to at least 1 */
    if (width<1) width = 1;
    return width;
}

/*-------------------------------------------------------------------------
 * Function: is_valid_args
 *
 * Purpose: check if command line arguments are valid
 *
 * Return: 
 *  Success: TRUE (1)
 *  Failure: FALSE (0)
 *
 * Programmer:
 *  Jonathan Kim  (06/15/2010)
 *
 *-------------------------------------------------------------------------*/
static hbool_t 
is_valid_args(void)
{
    herr_t ret = TRUE;

    if(recursive_g && grp_literal_g) 
    {
        fprintf(stderr, "Error: 'recursive' option not compatible with 'group info' option!\n\n");
        ret = FALSE;
        goto out;
    }

    if(no_dangling_link_g && !follow_symlink_g) 
    {
        fprintf(stderr, "Error: --no-dangling-links must be used along with --follow-symlinks option!\n\n");
        ret = FALSE;
        goto out;
    }

out:
    return ret;
}


/*-------------------------------------------------------------------------
 * Function: leave
 *
 * Purpose: Close HDF5 and MPI and call exit()
 *
 * Return: Does not return
 *
 * Programmer: Quincey Koziol
 *              Saturday, January 31, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
leave(int ret)
{
    h5tools_close();

    exit(ret);
}


/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose: Opens a file and lists the specified group
 *
 * Return: Success: 0
 *
 *  Failure: 1
 *
 * Programmer: Robb Matzke
 *              Monday, March 23, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, const char *argv[])
{
    hid_t file = -1;
    char *fname = NULL, *oname = NULL, *x;
    const char *s = NULL;
    char *rest;
    int  argno;
    static char root_name[] = "/";
    char        drivername[50];
    const char *preferred_driver = NULL;
    int err_exit = 0;

    h5tools_setprogname(PROGRAMNAME);
    h5tools_setstatus(EXIT_SUCCESS);

    /* Initialize h5tools lib */
    h5tools_init();

    /* Build object display table */
    DISPATCH(H5O_TYPE_GROUP, "Group", NULL, NULL);
    DISPATCH(H5O_TYPE_DATASET, "Dataset", dataset_list1, dataset_list2);
    DISPATCH(H5O_TYPE_NAMED_DATATYPE, "Type", NULL, datatype_list2);

    /* Default output width */
    width_g = get_width();

    /* Switches come before non-switch arguments */
    for(argno = 1; argno < argc && '-' == argv[argno][0]; argno++) {
        if(!HDstrcmp(argv[argno], "--")) {
            /* Last switch */
            argno++;
            break;
        } else if(!HDstrcmp(argv[argno], "--help")) {
            usage();
            leave(EXIT_SUCCESS);
        } else if(!HDstrcmp(argv[argno], "--address")) {
            address_g = TRUE;
        } else if(!HDstrcmp(argv[argno], "--data")) {
            data_g = TRUE;
        } else if(!HDstrcmp(argv[argno], "--enable-error-stack")) {
            show_errors_g = TRUE;
        /* deprecated --errors */
        } else if(!HDstrcmp(argv[argno], "--errors")) {
            show_errors_g = TRUE;
        } else if(!HDstrcmp(argv[argno], "--follow-symlinks")) {
            follow_symlink_g = TRUE;
        } else if(!HDstrcmp(argv[argno], "--no-dangling-links")) {
            no_dangling_link_g = TRUE;
        } else if(!HDstrcmp(argv[argno], "--external")) {
            follow_elink_g = TRUE;
        } else if(!HDstrcmp(argv[argno], "--full")) {
            fullname_g = TRUE;
        } else if(!HDstrcmp(argv[argno], "--group")) {
            grp_literal_g = TRUE;
        } else if(!HDstrcmp(argv[argno], "--label")) {
            label_g = TRUE;
        } else if(!HDstrcmp(argv[argno], "--recursive")) {
            recursive_g = TRUE;
            fullname_g = TRUE;
        } else if(!HDstrcmp(argv[argno], "--simple")) {
            simple_output_g = TRUE;
        } else if(!HDstrcmp(argv[argno], "--string")) {
            string_g = TRUE;
        } else if(!HDstrncmp(argv[argno], "--vfd=", 6)) {
            preferred_driver = argv[argno]+6;
        } else if(!HDstrncmp(argv[argno], "--width=", 8)) {
            width_g = (int)HDstrtol(argv[argno]+8, &rest, 0);

            if(0 == width_g)
                no_line_wrap_g = TRUE;
            else if(width_g < 0 || *rest) {
                usage();
                leave(EXIT_FAILURE);
            }
        } else if(!HDstrcmp(argv[argno], "--width")) {
            if((argno + 1) >= argc) {
                usage();
                leave(EXIT_FAILURE);
            } else {
                s = argv[++argno];
            }
            width_g = (int)HDstrtol(s, &rest, 0);
            if(width_g <= 0 || *rest) {
                usage();
                leave(EXIT_FAILURE);
            }
        } else if(!HDstrcmp(argv[argno], "--verbose")) {
            verbose_g++;
        } else if(!HDstrcmp(argv[argno], "--version")) {
            print_version(h5tools_getprogname());
            leave(EXIT_SUCCESS);
        } else if(!HDstrcmp(argv[argno], "--hexdump")) {
            hexdump_g = TRUE;
        } else if(!HDstrncmp(argv[argno], "-w", 2)) {
            if(argv[argno][2]) {
                s = argv[argno] + 2;
            } else if((argno + 1) >= argc) {
                usage();
                leave(EXIT_FAILURE);
            } else {
                s = argv[++argno];
            }
            width_g = (int)HDstrtol(s, &rest, 0);

            if(0 == width_g)
                no_line_wrap_g = TRUE;
            else if(width_g < 0 || *rest) {
                usage();
                leave(EXIT_FAILURE);
            }
        } else if('-'!=argv[argno][1]) {
            /* Single-letter switches */
            for(s = argv[argno] + 1; *s; s++) {
                switch(*s) {
                    case '?':
                    case 'h': /* --help */
                        usage();
                        leave(EXIT_SUCCESS);

                    case 'a': /* --address */
                        address_g = TRUE;
                        break;

                    case 'd': /* --data */
                        data_g = TRUE;
                        break;

                    /* deprecated -e */
                    case 'e': /* --errors */
                        show_errors_g = TRUE;
                        break;

                    case 'E': /* --external */
                        follow_elink_g = TRUE;
                        break;

                    case 'f': /* --full */
                        fullname_g = TRUE;
                        break;

                    case 'g': /* --group */
                        grp_literal_g = TRUE;
                        break;

                    case 'l': /* --label */
                        label_g = TRUE;
                        break;

                    case 'r': /* --recursive */
                        recursive_g = TRUE;
                        fullname_g = TRUE;
                        break;

                    case 'S': /* --simple */
                        simple_output_g = TRUE;
                        break;

                    case 's': /* --string */
                        string_g = TRUE;
                        break;

                    case 'v': /* --verbose */
                        verbose_g++;
                        break;

                    case 'V': /* --version */
                        print_version(h5tools_getprogname());
                        leave(EXIT_SUCCESS);

                    case 'x': /* --hexdump */
                        hexdump_g = TRUE;
                        break;

                    default:
                        usage();
                        leave(EXIT_FAILURE);
                } /* end switch */
            } /* end for */
        } else {
            usage();
            leave(EXIT_FAILURE);
        }
    } /* end for */

    /* If no arguments remain then print a usage message (instead of doing
     * absolutely nothing ;-) */
    if(argno >= argc) {
        usage();
        leave(EXIT_FAILURE);
    } /* end if */

    /* Check for conflicting arguments */
    if (!is_valid_args())
    {
        usage();
        leave(EXIT_FAILURE);
    }

    /* Turn off HDF5's automatic error printing unless you're debugging h5ls */
    if(!show_errors_g)
        H5Eset_auto2(H5E_DEFAULT, NULL, NULL);


    /* Each remaining argument is an hdf5 file followed by an optional slash
     * and object name.
     *
     * Example: ../dir1/foo/bar/baz
     *          \_________/\______/
     *             file       obj
     *
     * The dichotomy is determined by calling H5Fopen() repeatedly until it
     * succeeds. The first call uses the entire name and each subsequent call
     * chops off the last component. If we reach the beginning of the name
     * then there must have been something wrong with the file (perhaps it
     * doesn't exist). */
    show_file_name_g = (argc-argno > 1); /*show file names if more than one*/
    while(argno < argc) {
        H5L_info_t li;
        iter_t iter;
        symlink_trav_t symlink_list;
        size_t u;

        fname = HDstrdup(argv[argno++]);
        oname = NULL;
        file = -1;

        while(fname && *fname) {
            file = h5tools_fopen(fname, H5F_ACC_RDONLY, H5P_DEFAULT, preferred_driver, drivername, sizeof drivername);

            if(file >= 0) {
                if(verbose_g)
                    printf("Opened \"%s\" with %s driver.\n", fname, drivername);
                break; /*success*/
            } /* end if */

            /* Shorten the file name; lengthen the object name */
            x = oname;
            oname = HDstrrchr(fname, '/');
            if(x)
                *x = '/';
            if(!oname)
                break;
            *oname = '\0';
        } /* end while */

        if(file < 0) {
            fprintf(stderr, "%s: unable to open file\n", argv[argno-1]);
            HDfree(fname);
            err_exit = 1;
            continue;
        } /* end if */
        if(oname) {
            /* Always use absolute paths to avoid confusion, keep track of where
             * to begin path name output */
            *oname = '/';
            iter.base_len = HDstrlen(oname);
            iter.base_len -= oname[iter.base_len-1] == '/';
            x = oname;
            if(NULL == (oname = HDstrdup(oname))) {
                fprintf(stderr, "memory allocation failed\n");
                leave(EXIT_FAILURE);
            }
            *x = '\0';
            /* Delay specifying the name start point so the original object name
             * is displayed if it is a link or non-group object */
            iter.name_start = 1;
        }
        if(!oname || !*oname) {
            oname = root_name;
            if(recursive_g)
                display_root_g = TRUE;
            iter.base_len = 0;
            iter.name_start = 0;
            /* Use x to remember if we have allocated space in oname */
            x = NULL;
        } /* end if */

        /* Remember the file information for later */
        iter.fname = fname;
        iter.fid = file;
        iter.gid = -1;
        iter.symlink_target = FALSE;
        iter.symlink_list = &symlink_list;
        iter.symlink_list->dangle_link = FALSE;

        /* Initialize list of visited symbolic links */
        symlink_list.nused = symlink_list.nalloc = 0;
        symlink_list.objs = NULL;

        /* Check for root group as object name */
        if(HDstrcmp(oname, root_name)) {
            /* Check the type of link given */
            if(H5Lget_info(file, oname, &li, H5P_DEFAULT) < 0) {
                display_obj_name(stdout, &iter, oname, "**NOT FOUND**");
                leave(EXIT_FAILURE);
            } /* end if */
        } /* end if */
        else
            li.type = H5L_TYPE_HARD;

        /* Open the object and display it's information */
        if(li.type == H5L_TYPE_HARD) {
            if(visit_obj(file, oname, &iter) < 0)
                leave(EXIT_FAILURE);
        } /* end if(li.type == H5L_TYPE_HARD) */
        else {
            /* Specified name is not for object -- list that link */
            /* Use file ID for root group ID */
            iter.gid = file;
            list_lnk(oname, &li, &iter);
        }
        H5Fclose(file);
        HDfree(fname);
        if(x)
            HDfree(oname);

        for(u=0; u < symlink_list.nused; u++) 
        {
            if (symlink_list.objs[u].type == H5L_TYPE_EXTERNAL)
                HDfree(symlink_list.objs[u].file);

            HDfree(symlink_list.objs[u].path);
        }
        HDfree(symlink_list.objs);
        
        /* if no-dangling-links option specified and dangling link found */
        if (no_dangling_link_g && iter.symlink_list->dangle_link)
            err_exit = 1;
    } /* end while */

    if (err_exit)
        leave(EXIT_FAILURE);
    else
        leave(EXIT_SUCCESS);
} /* end main() */

