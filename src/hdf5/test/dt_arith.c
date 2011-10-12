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
 *              Tuesday, December  9, 1997
 *
 * Purpose:     Tests the data type interface (H5T)
 */

#include <math.h>
#include <time.h>
#include "h5test.h"

/* Number of elements in each random test */
#define NTESTELEM	10000

/* Epsilon for floating-point comparisons */
#define FP_EPSILON 0.000001

/*
 * Offset from alinged memory returned by malloc().  This can be used to test
 * that type conversions handle non-aligned buffers correctly.
 */
#define ALIGNMENT	1

/*
 * Define if you want to test alignment code on a machine that doesn't
 * normally require alignment. When set, all native data types must be aligned
 * on a byte boundary equal to the data size.
 */
#define TEST_ALIGNMENT

/* Alignment test stuff */
#ifdef TEST_ALIGNMENT
#define H5T_PACKAGE
#include "H5Tpkg.h"
#endif
#define SET_ALIGNMENT(TYPE,VAL) \
    H5T_NATIVE_##TYPE##_ALIGN_g=MAX(H5T_NATIVE_##TYPE##_ALIGN_g, VAL)

const char *FILENAME[] = {
    "dt_arith1",
    "dt_arith2",
    NULL
};

/*
 * Count up or down depending on whether the machine is big endian, little
 * endian, or VAX (OpenVMS).  If local variable `endian' is H5T_ORDER_BE then
 * the result will be I, otherwise the result will be Z-(I+1).  VAX is printed
 * as little endian.
 */
#define ENDIAN(Z,I,E)	(H5T_ORDER_BE==E?(I):(Z)-((I)+1))

typedef enum dtype_t {
    INT_SCHAR, INT_UCHAR, INT_SHORT, INT_USHORT, INT_INT, INT_UINT,
    INT_LONG, INT_ULONG, INT_LLONG, INT_ULLONG, FLT_FLOAT, FLT_DOUBLE,
#if H5_SIZEOF_LONG_DOUBLE !=0
    FLT_LDOUBLE,
#endif
    OTHER
} dtype_t;

/* Skip overflow tests if non-zero */
static int skip_overflow_tests_g = 0;

/*
 * Although we check whether a floating point overflow generates a SIGFPE and
 * turn off overflow tests in that case, it might still be possible for an
 * overflow condition to occur.  Once a SIGFPE is raised the program cannot
 * be allowed to continue (cf. Posix signals) so in order to recover from a
 * SIGFPE we run tests that might generate one in a child process.
 */
#if defined(H5_HAVE_FORK) && defined(H5_HAVE_WAITPID)
#define HANDLE_SIGFPE
#endif

/* OpenVMS doesn't have this feature.  Make sure to disable it*/
#ifdef H5_VMS
#undef HANDLE_SIGFPE
#endif

/*
 * Decide what values of floating-point number we want to test.  They are
 * 1 - normalized; 2 - denormalized; 3 - special.
 */
#define TEST_NOOP       0
#define TEST_NORMAL     1
#define TEST_DENORM     2
#define TEST_SPECIAL    3


/* Don't use hardware conversions if set */
static int without_hardware_g = 0;

/* Allocates memory aligned on a certain boundary. */
#define aligned_malloc(Z)	((void*)((char*)HDmalloc(ALIGNMENT+Z)+ALIGNMENT))
#define aligned_free(M)		HDfree((char*)(M)-ALIGNMENT)

/* Initialize source buffer of integer for integer->integer and integer->floating-point conversion test.
 * This algorithm is mainly to avoid any casting and comparison between source and destination types
 * for compiler, because we're testing conversions. */
#define INIT_INTEGER(TYPE, SRC_MAX, SRC_MIN, SRC_SIZE, DST_SIZE, SRC_PREC, BUF, SAVED, NELMTS)  \
{                                                                                               \
    unsigned char *buf_p, *saved_p;                                                             \
    unsigned int n;                                                                             \
    TYPE value1 = 1;                                                                            \
    TYPE value2 = 0;                                                                            \
                                                                                                \
    /* Allocate buffers */                                                                      \
    NELMTS=(SRC_PREC-1)*3+1;                                                                    \
    BUF = (unsigned char*)aligned_malloc(NELMTS*MAX(SRC_SIZE, DST_SIZE));                       \
    SAVED = (unsigned char*)aligned_malloc(NELMTS*MAX(SRC_SIZE, DST_SIZE));                     \
    HDmemset(BUF, 0, NELMTS*MAX(SRC_SIZE, DST_SIZE));                                           \
    HDmemset(SAVED, 0, NELMTS*MAX(SRC_SIZE, DST_SIZE));                                         \
                                                                                                \
    buf_p = BUF;                                                                                \
    saved_p = SAVED;                                                                            \
                                                                                                \
    /*positive values, ascending order. VALUE1 starts from 00000001, to 00000010, until 10000000*/ \
    /*VALUE2 ascends from 00000000, to 00000011, 00000111,...,  until 11111111.*/               \
    for(n=0; n<SRC_PREC-1; n++) {                                                               \
        if(value1<=SRC_MAX && value1>=SRC_MIN) {                                                \
            memcpy(buf_p, &value1, SRC_SIZE);                                                   \
            memcpy(saved_p, &value1, SRC_SIZE);                                                 \
            buf_p += SRC_SIZE;                                                                  \
            saved_p += SRC_SIZE;                                                                \
        }                                                                                       \
        if(value2<=SRC_MAX && value2>=SRC_MIN) {                                                \
            memcpy(buf_p, &value2, SRC_SIZE);                                                   \
            memcpy(saved_p, &value2, SRC_SIZE);                                                 \
            buf_p += SRC_SIZE;                                                                  \
            saved_p += SRC_SIZE;                                                                \
        }                                                                                       \
                                                                                                \
        value1 <<= 1;                                                                           \
        value2 = (value1 - 1) | value1;                                                         \
    }                                                                                           \
                                                                                                \
    /* negative values for signed; descending positive values for unsigned */                   \
    /* VALUE2 descends from 11111111 to 11111110, 11111100, ..., until 10000000. */             \
    for(n=0; n<SRC_PREC; n++) {                                                                 \
        if(value2<=SRC_MAX && value2>=SRC_MIN) {                                                \
            memcpy(buf_p, &value2, SRC_SIZE);                                                   \
            memcpy(saved_p, &value2, SRC_SIZE);                                                 \
            buf_p += SRC_SIZE;                                                                  \
            saved_p += SRC_SIZE;                                                                \
        }                                                                                       \
        value2 <<= 1;                                                                           \
    }                                                                                           \
}

/* Change a buffer's byte order from big endian to little endian.  It's mainly for library's
 * bit operations which handle only little endian order.
 */
#define CHANGE_ORDER(EBUF, EORDER, ESIZE)                                                       \
{                                                                                               \
    unsigned int m;                                                                             \
    if (H5T_ORDER_BE==EORDER) {                                                                 \
        unsigned char mediator;                                                                 \
        size_t half_size = ESIZE/2;                                                             \
        for (m=0; m<half_size; m++) {                                                           \
            mediator = EBUF[ESIZE-(m+1)];                                                       \
            EBUF[ESIZE-(m+1)] = EBUF[m];                                                        \
            EBUF[m] = mediator;                                                                 \
        }                                                                                       \
    } else if (H5T_ORDER_VAX==EORDER) {                                                         \
        unsigned char mediator1, mediator2;                                                     \
        for (m = 0; m < ESIZE; m += 4) {                                                        \
            mediator1 = EBUF[m];                                                                \
            mediator2 = EBUF[m+1];                                                              \
                                                                                                \
            EBUF[m] = EBUF[(ESIZE-2)-m];                                                        \
            EBUF[m+1] = EBUF[(ESIZE-1)-m];                                                      \
                                                                                                \
            EBUF[(ESIZE-2)-m] = mediator1;                                                      \
            EBUF[(ESIZE-1)-m] = mediator2;                                                      \
        }                                                                                       \
    }                                                                                           \
}

/* Allocate buffer and initialize it with floating-point normalized values.
 * It's for conversion test of floating-point as the source.
 */
#define INIT_FP_NORM(TYPE, SRC_MAX, SRC_MIN, SRC_MAX_10_EXP, SRC_MIN_10_EXP, SRC_SIZE,          \
                DST_SIZE, BUF, SAVED, NELMTS)                                                   \
{                                                                                               \
    unsigned char *buf_p, *saved_p;                                                             \
    size_t num_norm, factor, n;                                                                 \
    TYPE value1, value2;                                                                        \
    TYPE multiply;                                                                              \
                                                                                                \
    /*Determine the number of normalized values and increment pace.  The values start from      \
     *minimal normalized value and are multiplied by MULTIPLY each step until reach to maximal  \
     *normalized value.*/                                                                       \
    if(SRC_MAX_10_EXP<100) { /*for float*/                                                      \
        factor = 0;                                                                             \
        multiply = 10;                                                                          \
    } else if(SRC_MAX_10_EXP>=100 && SRC_MAX_10_EXP<400) { /*for double*/                       \
        factor = 2;                                                                             \
        multiply = 10000;                                                                       \
    } else { /*for long double*/                                                                \
        factor = 3;                                                                             \
        multiply = 100000000;                                                                   \
    }                                                                                           \
                                                                                                \
    /*The number of values if multiplied by 10 for each step.*/                                 \
    num_norm =  (SRC_MAX_10_EXP - SRC_MIN_10_EXP);                                              \
    /*Reduce the number of values by 2^factor. MULTIPLY=10^(2^factor). Using this algorithm     \
     *instead of arithmatic operation to avoid any conversion*/                                 \
    num_norm >>= factor;                                                                        \
                                                                                                \
    /*Total number of values*/                                                                  \
    NELMTS  = 2 *                      /*both positive and negative*/                           \
                (num_norm +            /*number of normalized values*/                          \
                 1);                   /*maximal normalized value*/                             \
                                                                                                \
    /* Allocate buffers */                                                                      \
    BUF = (unsigned char*)aligned_malloc(NELMTS*MAX(SRC_SIZE, DST_SIZE));                       \
    SAVED = (unsigned char*)aligned_malloc(NELMTS*MAX(SRC_SIZE, DST_SIZE));                     \
    HDmemset(BUF, 0, NELMTS*MAX(SRC_SIZE, DST_SIZE));                                           \
    HDmemset(SAVED, 0, NELMTS*MAX(SRC_SIZE, DST_SIZE));                                         \
                                                                                                \
    buf_p = BUF;                                                                                \
    saved_p = SAVED;                                                                            \
                                                                                                \
    /*Normalized values*/                                                                       \
    value1 = SRC_MIN;                                                                           \
    value2 = -SRC_MIN;                                                                          \
    for(n=0; n<num_norm; n++) {                                                                 \
        if(value1<SRC_MAX) { /*positive*/                                                       \
            memcpy(buf_p, &value1, SRC_SIZE);                                                   \
            memcpy(saved_p, &value1, SRC_SIZE);                                                 \
            value1 *= multiply;                                                                 \
            buf_p += SRC_SIZE;                                                                  \
            saved_p += SRC_SIZE;                                                                \
        }                                                                                       \
        if(value2>-SRC_MAX) { /*negative*/                                                      \
            memcpy(buf_p, &value2, SRC_SIZE);                                                   \
            memcpy(saved_p, &value2, SRC_SIZE);                                                 \
            value2 *= multiply;                                                                 \
            buf_p += SRC_SIZE;                                                                  \
            saved_p += SRC_SIZE;                                                                \
        }                                                                                       \
    }                                                                                           \
                                                                                                \
    value1 = SRC_MAX;                              /*maximal value*/                            \
    memcpy(buf_p, &value1, SRC_SIZE);                                                           \
    memcpy(saved_p, &value1, SRC_SIZE);                                                         \
    buf_p += SRC_SIZE;                                                                          \
    saved_p += SRC_SIZE;                                                                        \
                                                                                                \
    value2 = -SRC_MAX;                             /*negative value*/                           \
    memcpy(buf_p, &value2, SRC_SIZE);                                                           \
    memcpy(saved_p, &value2, SRC_SIZE);                                                         \
    buf_p += SRC_SIZE;                                                                          \
    saved_p += SRC_SIZE;                                                                        \
}

/* Allocate buffer and initialize it with floating-point denormalized values.
 * It's for conversion test of floating-point as the source.
 */
#define INIT_FP_DENORM(TYPE, SRC_MANT_DIG, SRC_SIZE, SRC_PREC, SRC_ORDR, DST_SIZE,              \
                       BUF, SAVED, NELMTS)                                                      \
{                                                                                               \
    unsigned char *buf_p, *saved_p;                                                             \
    unsigned char *tmp1, *tmp2;                                                                 \
    size_t n;                                                                                   \
                                                                                                \
    /*Total number of values*/                                                                  \
    NELMTS  = 2 *                      /*both positive and negative*/                           \
                (SRC_MANT_DIG - 1);    /*number of denormalized values*/                        \
                                                                                                \
    /* Allocate buffers */                                                                      \
    BUF = (unsigned char*)aligned_malloc(NELMTS*MAX(SRC_SIZE, DST_SIZE));                       \
    SAVED = (unsigned char*)aligned_malloc(NELMTS*MAX(SRC_SIZE, DST_SIZE));                     \
    HDmemset(BUF, 0, NELMTS*MAX(SRC_SIZE, DST_SIZE));                                           \
    HDmemset(SAVED, 0, NELMTS*MAX(SRC_SIZE, DST_SIZE));                                         \
                                                                                                \
    tmp1 = (unsigned char*)calloc((size_t)1, (size_t)SRC_SIZE);                                                 \
    tmp2 = (unsigned char*)calloc((size_t)1, (size_t)SRC_SIZE);                                                 \
                                                                                                \
    buf_p = BUF;                                                                                \
    saved_p = SAVED;                                                                            \
                                                                                                \
    /*Denormalized values. Exponent is 0. Let mantissa starts from 00000001, 00000011,          \
     *00000111,..., until 11111111.*/                                                           \
    memset(tmp1, 0, SRC_SIZE);                                                                  \
    memset(tmp2, 0, SRC_SIZE);                                                                  \
    H5T_bit_set (tmp2, SRC_PREC-1, (size_t)1, TRUE);       /*the negative value*/                       \
    for(n=0; n<SRC_MANT_DIG-1; n++) {                                                           \
        H5T_bit_set (tmp1, n, (size_t)1, TRUE);            /*turn on 1 bit each time*/                  \
        CHANGE_ORDER(tmp1, SRC_ORDR, SRC_SIZE);    /*change order for big endian*/              \
        memcpy(buf_p, tmp1, SRC_SIZE);                                                          \
        memcpy(saved_p, tmp1, SRC_SIZE);                                                        \
        CHANGE_ORDER(tmp1, SRC_ORDR, SRC_SIZE);    /*change back the order for bit operation*/  \
        buf_p += SRC_SIZE;                                                                      \
        saved_p += SRC_SIZE;                                                                    \
                                                                                                \
        /*negative values*/                                                                     \
        H5T_bit_set (tmp2, n, (size_t)1, TRUE);                                                         \
        CHANGE_ORDER(tmp2, SRC_ORDR, SRC_SIZE);                                                 \
        memcpy(buf_p, tmp2, SRC_SIZE);                                                          \
        memcpy(saved_p, tmp2, SRC_SIZE);                                                        \
        CHANGE_ORDER(tmp2, SRC_ORDR, SRC_SIZE);                                                 \
        buf_p += SRC_SIZE;                                                                      \
        saved_p += SRC_SIZE;                                                                    \
    }                                                                                           \
    free(tmp1);                                                                                 \
    free(tmp2);                                                                                 \
}

/* Allocate buffer and initialize it with floating-point special values, +/-0, +/-infinity,
 * +/-QNaN, +/-SNaN.  It's for conversion test of floating-point as the source.
 */
#define INIT_FP_SPECIAL(SRC_SIZE, SRC_PREC, SRC_ORDR, SRC_MANT_DIG, DST_SIZE,                   \
            BUF, SAVED, NELMTS)                                                                 \
{                                                                                               \
    unsigned char *buf_p;                                                                       \
    unsigned char *value;                                                                       \
    int n;                                                                                      \
                                                                                                \
    /*Total number of values*/                                                                  \
    NELMTS  = 2 *                      /*both positive and negative*/                           \
                4;                     /*infinity, SNaN, QNaN      */                           \
                                                                                                \
    /* Allocate buffers */                                                                      \
    BUF = (unsigned char*)aligned_malloc(NELMTS*MAX(SRC_SIZE, DST_SIZE));                       \
    SAVED = (unsigned char*)aligned_malloc( NELMTS*MAX(SRC_SIZE, DST_SIZE));                    \
    HDmemset(BUF, 0, NELMTS*MAX(SRC_SIZE, DST_SIZE));                                           \
    HDmemset(SAVED, 0, NELMTS*MAX(SRC_SIZE, DST_SIZE));                                         \
    value = (unsigned char*)calloc(SRC_SIZE, sizeof(unsigned char));                            \
                                                                                                \
    buf_p = BUF;                                                                                \
                                                                                                \
    /* +0 */                                                                                    \
    H5T_bit_set(value, (size_t)0, SRC_PREC, FALSE);                                                     \
    memcpy(buf_p, value, SRC_SIZE*sizeof(unsigned char));                                       \
    buf_p += SRC_SIZE;                                                                          \
                                                                                                \
    for(n=0; n<2; n++) {                                                                        \
        if(n==1) {                                                                              \
            memset(value, 0, SRC_SIZE*sizeof(unsigned char));                                   \
            /* -0 */                                                                            \
            H5T_bit_set(value, (size_t)(SRC_PREC - 1), (size_t)1, TRUE);                                            \
            CHANGE_ORDER(value, SRC_ORDR, SRC_SIZE);/*change order for big endian*/             \
            HDmemcpy(buf_p, value, SRC_SIZE*sizeof(unsigned char));                               \
            CHANGE_ORDER(value, SRC_ORDR, SRC_SIZE);/*change back the order for bit operation*/ \
            buf_p += SRC_SIZE;                                                                  \
        }                                                                                       \
                                                                                                \
        /* +/-infinity */                                                                       \
        H5T_bit_set(value, (size_t)(SRC_MANT_DIG - 1), SRC_PREC-SRC_MANT_DIG, TRUE);                        \
        CHANGE_ORDER(value, SRC_ORDR, SRC_SIZE);    /*change order for big endian*/             \
        HDmemcpy(buf_p, value, SRC_SIZE*sizeof(unsigned char));                                   \
        CHANGE_ORDER(value, SRC_ORDR, SRC_SIZE);    /*change back the order for bit operation*/ \
        buf_p += SRC_SIZE;                                                                      \
                                                                                                \
        /* +/-SNaN */                                                                           \
        H5T_bit_set(value, (size_t)0, (size_t)1, TRUE);                                                         \
        CHANGE_ORDER(value, SRC_ORDR, SRC_SIZE);    /*change order for big endian*/             \
        HDmemcpy(buf_p, value, SRC_SIZE * sizeof(unsigned char));                                   \
        CHANGE_ORDER(value, SRC_ORDR, SRC_SIZE);    /*change back the order for bit operation*/ \
        buf_p += SRC_SIZE;                                                                      \
                                                                                                \
        /* +/-QNaN */                                                                           \
        H5T_bit_set(value, (size_t)(SRC_MANT_DIG - 2), (size_t)1, TRUE);                                            \
        CHANGE_ORDER(value, SRC_ORDR, SRC_SIZE);    /*change order for big endian*/             \
        HDmemcpy(buf_p, value, SRC_SIZE*sizeof(unsigned char));                                   \
        CHANGE_ORDER(value, SRC_ORDR, SRC_SIZE);    /*change back the order for bit operation*/ \
        buf_p += SRC_SIZE;                                                                      \
    }                                                                                           \
                                                                                                \
    HDmemcpy(SAVED, BUF, NELMTS*MAX(SRC_SIZE, DST_SIZE));                                         \
    free(value);                                                                                \
}

void some_dummy_func(float x);
static hbool_t overflows(unsigned char *origin_bits, hid_t src_id, size_t dst_num_bits);
static int my_isnan(dtype_t type, void *val);
static int my_isinf(int endian, unsigned char *val, size_t size,
        size_t mpos, size_t msize, size_t epos, size_t esize);

/*-------------------------------------------------------------------------
 * Function:	fpe_handler
 *
 * Purpose:	Exit with 255
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Monday, July  6, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
fpe_handler(int UNUSED signo)
{
    SKIPPED();
    HDputs("    Test skipped due to SIGFPE.");
#ifndef HANDLE_SIGFPE
    HDputs("    Remaining tests could not be run.");
    HDputs("    Please turn off SIGFPE on overflows and try again.");
#endif
    HDexit(255);
}


/*-------------------------------------------------------------------------
 * Function:	reset_hdf5
 *
 * Purpose:	Reset the hdf5 library.  This causes statistics to be printed
 *		and counters to be reset.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Monday, November 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
reset_hdf5(void)
{
    h5_reset();

    if (without_hardware_g) h5_no_hwconv();
#ifdef TEST_ALIGNMENT
    SET_ALIGNMENT(SCHAR,   H5_SIZEOF_CHAR);
    SET_ALIGNMENT(UCHAR,   H5_SIZEOF_CHAR);
    SET_ALIGNMENT(SHORT,   H5_SIZEOF_SHORT);
    SET_ALIGNMENT(USHORT,  H5_SIZEOF_SHORT);
    SET_ALIGNMENT(INT,     H5_SIZEOF_INT);
    SET_ALIGNMENT(UINT,    H5_SIZEOF_INT);
    SET_ALIGNMENT(LONG,    H5_SIZEOF_LONG);
    SET_ALIGNMENT(ULONG,   H5_SIZEOF_LONG);
    SET_ALIGNMENT(LLONG,   H5_SIZEOF_LONG_LONG);
    SET_ALIGNMENT(ULLONG,  H5_SIZEOF_LONG_LONG);
    SET_ALIGNMENT(FLOAT,   H5_SIZEOF_FLOAT);
    SET_ALIGNMENT(DOUBLE,  H5_SIZEOF_DOUBLE);
#if H5_SIZEOF_LONG_DOUBLE !=0
    SET_ALIGNMENT(LDOUBLE, H5_SIZEOF_LONG_DOUBLE);
#endif
#endif

}


/*-------------------------------------------------------------------------
 * Function:	except_func
 *
 * Purpose:	Gets called for all data type conversion exceptions.
 *
 * Return:	H5T_CONV_ABORT:	        -1
 *
 *              H5T_CONV_UNHANDLED      0
 *
 *              H5T_CONV_HANDLED        1
 *
 * Programmer:	Raymond Lu
 *              April 19, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static H5T_conv_ret_t
except_func(H5T_conv_except_t except_type, hid_t UNUSED src_id, hid_t UNUSED dst_id, void UNUSED *src_buf,
		 void *dst_buf, void *user_data)
{
    H5T_conv_ret_t      ret = H5T_CONV_HANDLED;

    if(except_type == H5T_CONV_EXCEPT_RANGE_HI)
        /*only test integer case*/
        *(int*)dst_buf = *(int*)user_data;
    else if(except_type == H5T_CONV_EXCEPT_RANGE_LOW)
        /*only test integer case*/
        *(int*)dst_buf = *(int*)user_data;
    else if(except_type == H5T_CONV_EXCEPT_TRUNCATE)
        ret = H5T_CONV_UNHANDLED;
    else if(except_type == H5T_CONV_EXCEPT_PRECISION)
        ret = H5T_CONV_UNHANDLED;
    else if(except_type == H5T_CONV_EXCEPT_PINF)
        /*only test integer case*/
        *(int*)dst_buf = *(int*)user_data;
    else if(except_type == H5T_CONV_EXCEPT_NINF)
        /*only test integer case*/
        *(int*)dst_buf = *(int*)user_data;
    else if(except_type == H5T_CONV_EXCEPT_NAN)
        /*only test integer case*/
        *(int*)dst_buf = *(int*)user_data;

    return ret;
}


/*-------------------------------------------------------------------------
 * Function:	some_dummy_func
 *
 * Purpose:	A dummy function to help check for overflow.
 *
 * Note:	DO NOT DECLARE THIS FUNCTION STATIC OR THE COMPILER MIGHT
 *		PROMOTE ARGUMENT `x' TO DOUBLE AND DEFEAT THE OVERFLOW
 *		CHECKING.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Tuesday, July 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
some_dummy_func(float x)
{
    char	s[128];
    sprintf(s, "%g", x);
}


/*-------------------------------------------------------------------------
 * Function:	generates_sigfpe
 *
 * Purpose:	Determines if SIGFPE is generated from overflows.  We must be
 *		able to fork() and waitpid() in order for this test to work
 *		properly.  Sets skip_overflow_tests_g to non-zero if they
 *		would generate SIGBUS, zero otherwise.
 *
 * Programmer:	Robb Matzke
 *              Tuesday, July 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
generates_sigfpe(void)
{
#if defined(H5_HAVE_FORK) && defined(H5_HAVE_WAITPID)
    pid_t	pid;
    int		status;
    size_t	i, j;
    double	d;
    unsigned char *dp = (unsigned char*)&d;
    float	f;

    HDfflush(stdout);
    HDfflush(stderr);
    if ((pid=fork()) < 0) {
	HDperror("fork");
	HDexit(1);
    } else if (0==pid) {
	for (i=0; i<2000; i++) {
	    for (j=0; j<sizeof(double); j++) dp[j] = HDrand();
	    f = (float)d;
	    some_dummy_func((float)f);
	}
	HDexit(0);
    }

    while (pid!=waitpid(pid, &status, 0))
        /*void*/;
    if (WIFEXITED(status) && 0==WEXITSTATUS(status)) {
	HDputs("Floating-point overflow cases will be tested.");
	skip_overflow_tests_g = FALSE;
    } else if (WIFSIGNALED(status) && SIGFPE==WTERMSIG(status)) {
	HDputs("Floating-point overflow cases cannot be safely tested.");
	skip_overflow_tests_g = TRUE;
	/* delete the core dump file that SIGFPE may have created */
	HDunlink("core");
    }
#else
    HDputs("Cannot determine if floating-point overflows generate a SIGFPE;");
    HDputs("assuming yes.");
    HDputs("Overflow cases will not be tested.");
    skip_overflow_tests_g = TRUE;
#endif
}


/*-------------------------------------------------------------------------
 * Function:    test_hard_query
 *
 * Purpose:     Tests H5Tcompiler_conv() for querying whether a conversion is
 *              a hard one.
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 * Programmer:  Raymond Lu
 *              Friday, Sept 2, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_hard_query(void)
{
    TESTING("query functions of compiler conversion");

    /* Verify the conversion from int to float is a hard conversion. */
    if(H5Tcompiler_conv(H5T_NATIVE_INT, H5T_NATIVE_FLOAT) != TRUE) {
        H5_FAILED();
        printf("Can't query conversion function\n");
        goto error;
    }

    /* Unregister the hard conversion from int to float.  Verify the conversion
     * is a soft conversion. */
    H5Tunregister(H5T_PERS_HARD, NULL, H5T_NATIVE_INT, H5T_NATIVE_FLOAT, H5T_conv_int_float);
    if(H5Tcompiler_conv(H5T_NATIVE_INT, H5T_NATIVE_FLOAT) != FALSE) {
        H5_FAILED();
        printf("Can't query conversion function\n");
        goto error;
    }

    /* Register the hard conversion from int to float.  Verify the conversion
     * is a hard conversion. */
    H5Tregister(H5T_PERS_HARD, "int_flt", H5T_NATIVE_INT, H5T_NATIVE_FLOAT, H5T_conv_int_float);
    if(H5Tcompiler_conv(H5T_NATIVE_INT, H5T_NATIVE_FLOAT) != TRUE) {
        H5_FAILED();
        printf("Can't query conversion function\n");
        goto error;
    }

    PASSED();
    reset_hdf5();

    return 0;

 error:
    reset_hdf5();
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	expt_handle
 *
 * Purpose:	Gets called from test_particular_fp_integer() for data type
 *              conversion exceptions.
 *
 * Return:	H5T_CONV_HANDLED        1
 *
 * Programmer:	Raymond Lu
 *              Sept 7, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static H5T_conv_ret_t
expt_handle(H5T_conv_except_t except_type, hid_t UNUSED src_id, hid_t UNUSED dst_id, void UNUSED *src_buf,
		 void *dst_buf, void *user_data)
{
    signed char         fill_value1 = 7;
    int                 fill_value2 = 13;

    if(except_type == H5T_CONV_EXCEPT_RANGE_HI || except_type == H5T_CONV_EXCEPT_RANGE_LOW ||
            except_type == H5T_CONV_EXCEPT_TRUNCATE) {
        if(*(hbool_t*)user_data)
            *(signed char*)dst_buf = fill_value1;
        else
            *(int*)dst_buf = fill_value2;
    } /* end if */

    return H5T_CONV_HANDLED;
}


/*-------------------------------------------------------------------------
 * Function:    test_particular_fp_integer
 *
 * Purpose:     Tests hard conversions from floating numbers to integers in
 *              a special situation when the source is "float" and assigned
 *              the value of "INT_MAX".  A compiler may do roundup making
 *              this value "INT_MAX+1".  When this float value is casted to
 *              int, overflow happens.  This test makes sure the library
 *              returns exception in this situation.
 *
 *              Also verifies the library handles conversion from double to
 *              signed char correctly when the value of double is SCHAR_MAX.
 *              The test makes sure the signed char doesn't overflow.
 *
 *              This test is mainly for netCDF's request.
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 * Programmer:  Raymond Lu
 *              Sept 7, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int test_particular_fp_integer(void)
{
    hid_t       dxpl_id;
    hbool_t     flag;
    double      src_d = (double)SCHAR_MAX;
    signed char dst_c;
    unsigned char *buf1 = NULL, *buf2 = NULL;
    unsigned char *saved_buf1 = NULL, *saved_buf2 = NULL;
    size_t      src_size1, src_size2;
    size_t      dst_size1, dst_size2;
    float       src_f = (float)INT_MAX;
    int         dst_i;
    int         fill_value = 13;
    int		endian;			/*endianess	        */
    unsigned int        fails_this_test = 0;
    size_t      j;

    TESTING("hard particular floating number -> integer conversions");

    if((dxpl_id = H5Pcreate(H5P_DATASET_XFER)) < 0) {
        H5_FAILED();
        printf("Can't create data transfer property list\n");
        goto error;
    }

    /* Test conversion from double (the value is SCHAR_MAX) to signed char. */
    endian = H5Tget_order(H5T_NATIVE_DOUBLE);
    src_size1 = H5Tget_size(H5T_NATIVE_DOUBLE);
    dst_size1 = H5Tget_size(H5T_NATIVE_SCHAR);
    buf1 = (unsigned char*)calloc((size_t)1, (size_t)MAX(src_size1, dst_size1));
    saved_buf1 = (unsigned char*)calloc((size_t)1, (size_t)MAX(src_size1, dst_size1));

    memcpy(buf1, &src_d, src_size1);
    memcpy(saved_buf1, &src_d, src_size1);

    /* Register exception handling function and signal the destination is "signed char". */
    flag = 1;
    if(H5Pset_type_conv_cb(dxpl_id, expt_handle, &flag) < 0) {
        H5_FAILED();
        printf("Can't register conversion callback\n");
        goto error;
    }

    /* Do conversion */
    if(H5Tconvert(H5T_NATIVE_DOUBLE, H5T_NATIVE_SCHAR, (size_t)1, buf1, NULL, dxpl_id) < 0) {
        H5_FAILED();
        printf("Can't convert data\n");
        goto error;
    }

    memcpy(&dst_c, buf1, dst_size1);

    /* Print errors */
    if(dst_c != SCHAR_MAX) {
        double x;
        signed char   y;

        if(0 == fails_this_test++)
            H5_FAILED();

        printf("    test double to signed char:\n");
        printf("        src = ");
        for (j=0; j<src_size1; j++)
            printf(" %02x", saved_buf1[ENDIAN(src_size1, j, endian)]);

        HDmemcpy(&x, saved_buf1, src_size1);
        printf(" %29.20e\n", x);

        printf("        dst = ");
        for (j=0; j<dst_size1; j++)
            printf(" %02x", buf1[ENDIAN(dst_size1, j, endian)]);

        HDmemcpy(&y, buf1, dst_size1);
        printf(" %29d\n", y);
    }

    /* Test conversion from float (the value is INT_MAX) to int. */
    src_size2 = H5Tget_size(H5T_NATIVE_FLOAT);
    dst_size2 = H5Tget_size(H5T_NATIVE_INT);
    buf2 = (unsigned char*)calloc((size_t)1, (size_t)MAX(src_size2, dst_size2));
    saved_buf2 = (unsigned char*)calloc((size_t)1, (size_t)MAX(src_size2, dst_size2));
    HDmemcpy(buf2, &src_f, src_size2);
    HDmemcpy(saved_buf2, &src_f, src_size2);

    /* signal exception handling function that the destination is "int". */
    flag = 0;

    /* Do conversion */
    if(H5Tconvert(H5T_NATIVE_FLOAT, H5T_NATIVE_INT, (size_t)1, buf2, NULL, dxpl_id) < 0) {
        H5_FAILED();
        printf("Can't convert data\n");
        goto error;
    }

    memcpy(&dst_i, buf2, dst_size2);

    /* Print errors */
    if(dst_i != fill_value) {
        float x;
        int   y;

        if(0 == fails_this_test++)
            H5_FAILED();

        printf("    test float to int:\n");
        printf("        src = ");
        for (j=0; j<src_size2; j++)
            printf(" %02x", saved_buf2[ENDIAN(src_size2, j, endian)]);

        HDmemcpy(&x, saved_buf2, src_size2);
        printf(" %29.20e\n", x);

        printf("        dst = ");
        for (j=0; j<dst_size2; j++)
            printf(" %02x", buf2[ENDIAN(dst_size2, j, endian)]);

        HDmemcpy(&y, buf2, dst_size2);
        printf(" %29d\n", y);
    }

    if(fails_this_test)
        goto error;

    if(H5Pclose(dxpl_id) < 0) {
        H5_FAILED();
        printf("Can't close property list\n");
        goto error;
    }

    if(buf1)
        free(buf1);
    if(buf2)
        free(buf2);
    if(saved_buf1)
        free(saved_buf1);
    if(saved_buf2)
        free(saved_buf2);

    PASSED();
    return 0;

error:
    HDfflush(stdout);
    H5E_BEGIN_TRY {
        H5Pclose(dxpl_id);
    } H5E_END_TRY;
    if(buf1)
        free(buf1);
    if(buf2)
        free(buf2);
    if(saved_buf1)
        free(saved_buf1);
    if(saved_buf2)
        free(saved_buf2);

    reset_hdf5(); /*print statistics*/
    return MAX((int)fails_this_test, 1);
}


/*-------------------------------------------------------------------------
 * Function:    test_derived_flt
 *
 * Purpose:     Tests user-define and query functions of floating-point types.
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 * Programmer:  Raymond Lu
 *              Thursday, Jan 6, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_derived_flt(void)
{
    hid_t       file=-1, tid1=-1, tid2=-1;
    hid_t       dxpl_id=-1;
    char        filename[1024];
    size_t      spos, epos, esize, mpos, msize, size;
    size_t      src_size, dst_size;
    unsigned char        *buf=NULL, *saved_buf=NULL;
    int         *aligned=NULL;
    int		endian;			/*endianess	        */
    size_t      nelmts = NTESTELEM;
    unsigned int        fails_this_test = 0;
    const size_t	max_fails=40;	/*max number of failures*/
    char	str[256];		/*message string	*/
    unsigned int         i, j;

    TESTING("user-define and query functions of floating-point types");

    /* Create File */
    h5_fixname(FILENAME[0], H5P_DEFAULT, filename, sizeof filename);
    if((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("Can't create file\n");
        goto error;
    }

    if((dxpl_id = H5Pcreate(H5P_DATASET_XFER)) < 0) {
        H5_FAILED();
        printf("Can't create data transfer property list\n");
        goto error;
    }

    if((tid1 = H5Tcopy(H5T_IEEE_F64LE)) < 0) {
        H5_FAILED();
        printf("Can't copy data type\n");
        goto error;
    }

    if((tid2 = H5Tcopy(H5T_IEEE_F32LE)) < 0) {
        H5_FAILED();
        printf("Can't copy data type\n");
        goto error;
    }

    /*------------------------------------------------------------------------
     *                   1st floating-point type
     * size=7 byte, precision=42 bits, offset=3 bits, mantissa size=31 bits,
     * mantissa position=3, exponent size=10 bits, exponent position=34,
     * exponent bias=511.  It can be illustrated in little-endian order as
     *
     *          6       5       4       3       2       1       0
     *    ???????? ???SEEEE EEEEEEMM MMMMMMMM MMMMMMMM MMMMMMMM MMMMM???
     *
     * To create a new floating-point type, the following properties must be
     * set in the order of
     *   set fields -> set offset -> set precision -> set size.
     * All these properties must be set before the type can function. Other
     * properties can be set anytime.  Derived type size cannot be expanded
     * bigger than original size but can be decreased.  There should be no
     * holes among the significant bits.  Exponent bias usually is set
     * 2^(n-1)-1, where n is the exponent size.
     *-----------------------------------------------------------------------*/
    if(H5Tset_fields(tid1, (size_t)44, (size_t)34, (size_t)10, (size_t)3, (size_t)31) < 0) {
        H5_FAILED();
        printf("Can't set fields\n");
        goto error;
    }
    if(H5Tset_offset(tid1, (size_t)3) < 0) {
        H5_FAILED();
        printf("Can't set offset\n");
        goto error;
    }
    if(H5Tset_precision(tid1, (size_t)42) < 0) {
        H5_FAILED();
        printf("Can't set precision 1\n");
        goto error;
    }
    if(H5Tset_size(tid1, (size_t)7) < 0) {
        H5_FAILED();
        printf("Can't set size\n");
        goto error;
    }

    if(H5Tset_ebias(tid1, (size_t)511) < 0) {
        H5_FAILED();
        printf("Can't set exponent bias\n");
        goto error;
    }
    if(H5Tset_pad(tid1, H5T_PAD_ZERO, H5T_PAD_ZERO) < 0) {
        H5_FAILED();
        printf("Can't set padding\n");
        goto error;
    }

    if(H5Tcommit2(file, "new float type 1", tid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Can't set inpad\n");
        goto error;
    }
    if(H5Tclose(tid1) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    }

    if((tid1 = H5Topen2(file, "new float type 1", H5P_DEFAULT)) < 0)
        FAIL_PUTS_ERROR("Can't open datatype")
    if(H5Tget_fields(tid1, &spos, &epos, &esize, &mpos, &msize) < 0) {
        H5_FAILED();
        printf("Can't get fields\n");
        goto error;
    }
    if(spos != 44 || epos != 34 || esize != 10 || mpos != 3 || msize != 31) {
        H5_FAILED();
        printf("Wrong field values\n");
        goto error;
    }

    if(H5Tget_precision(tid1) != 42) {
        H5_FAILED();
        printf("Can't get precision or wrong precision\n");
        goto error;
    }
    if(H5Tget_offset(tid1)!=3) {
        H5_FAILED();
        printf("Can't get offset or wrong offset\n");
        goto error;
    }
    if((size = H5Tget_size(tid1))!=7) {
        H5_FAILED();
        printf("Can't get size or wrong size\n");
        goto error;
    }
    if(H5Tget_ebias(tid1)!=511) {
        H5_FAILED();
        printf("Can't get exponent bias or wrong bias\n");
        goto error;
    }

    /* Convert data from native integer to the 1st derived floating-point type.
     * Then convert data from the floating-point type back to native integer.
     * Compare the final data with the original data.
     */
    src_size = H5Tget_size(H5T_NATIVE_INT);
    endian = H5Tget_order(H5T_NATIVE_INT);
    buf = (unsigned char*)malloc(nelmts * (MAX(src_size, size)));
    saved_buf = (unsigned char*)malloc(nelmts * src_size);
    HDmemset(buf, 0, nelmts * MAX(src_size, size));
    HDmemset(saved_buf, 0, nelmts * src_size);
    aligned = (int*)calloc((size_t)1, src_size);

    for(i = 0; i < nelmts * src_size; i++)
        buf[i] = saved_buf[i] = HDrand();

    /* Convert data from native integer to derived floating-point type.
     * The mantissa is big enough to retain the integer's precision. */
    if(H5Tconvert(H5T_NATIVE_INT, tid1, nelmts, buf, NULL, dxpl_id) < 0) {
        H5_FAILED();
        printf("Can't convert data\n");
        goto error;
    }
    /* Convert data from the derived floating-point type back to native integer. */
    if(H5Tconvert(tid1, H5T_NATIVE_INT, nelmts, buf, NULL, dxpl_id) < 0) {
        H5_FAILED();
        printf("Can't convert data\n");
        goto error;
    }

    /* Are the values still the same?*/
    for(i=0; i<nelmts; i++) {
        for(j=0; j<src_size; j++)
            if(buf[i*src_size+j]!=saved_buf[i*src_size+j])
               break;
        if(j==src_size)
           continue; /*no error*/

        /* Print errors */
        if (0==fails_this_test++) {
	    sprintf(str, "\nTesting random sw derived floating-point -> derived floating-point conversions");
	    printf("%-70s", str);
	    HDfflush(stdout);
            H5_FAILED();
        }
        printf("    test %u elmt %u: \n", 1, (unsigned)i);

        printf("        src = ");
        for (j=0; j<src_size; j++)
            printf(" %02x", saved_buf[i*src_size+ENDIAN(src_size, j, endian)]);

        HDmemcpy(aligned, saved_buf+i*sizeof(int), sizeof(int));
        printf(" %29d\n", *aligned);

        printf("        dst = ");
        for (j=0; j<src_size; j++)
            printf(" %02x", buf[i*src_size+ENDIAN(src_size, j, endian)]);

        HDmemcpy(aligned, buf+i*sizeof(int), sizeof(int));
        printf(" %29d\n", *aligned);

        if (fails_this_test>=max_fails) {
            HDputs("    maximum failures reached, aborting test...");
            goto error;
        }
    }

    fails_this_test = 0;
    if(buf) free(buf);
    if(saved_buf) free(saved_buf);
    if(aligned) free(aligned);
    buf = NULL;
    saved_buf = NULL;
    aligned = NULL;

    /*--------------------------------------------------------------------------
     *                   2nd floating-point type
     * size=3 byte, precision=24 bits, offset=0 bits, mantissa size=16 bits,
     * mantissa position=0, exponent size=7 bits, exponent position=16, exponent
     * bias=63. It can be illustrated in little-endian order as
     *
     *          2       1       0
     *    SEEEEEEE MMMMMMMM MMMMMMMM
     *--------------------------------------------------------------------------*/
    if(H5Tset_fields(tid2, (size_t)23, (size_t)16, (size_t)7, (size_t)0, (size_t)16) < 0) {
        H5_FAILED();
        printf("Can't set fields\n");
        goto error;
    }
    if(H5Tset_offset(tid2, (size_t)0) < 0) {
        H5_FAILED();
        printf("Can't set offset\n");
        goto error;
    }
    if(H5Tset_precision(tid2, (size_t)24) < 0) {
        H5_FAILED();
        printf("Can't set precision 2\n");
        goto error;
    }
    if(H5Tset_size(tid2, (size_t)3) < 0) {
        H5_FAILED();
        printf("Can't set size\n");
        goto error;
    }
    if(H5Tset_ebias(tid2, (size_t)63) < 0) {
        H5_FAILED();
        printf("Can't set size\n");
        goto error;
    }
    if(H5Tset_pad(tid2, H5T_PAD_ZERO, H5T_PAD_ZERO) < 0) {
        H5_FAILED();
        printf("Can't set padding\n");
        goto error;
    }

    if(H5Tcommit2(file, "new float type 2", tid2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Can't set inpad\n");
        goto error;
    }
    if(H5Tclose(tid2) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    }

    if((tid2 = H5Topen2(file, "new float type 2", H5P_DEFAULT)) < 0)
        FAIL_PUTS_ERROR("Can't open datatype")
    if(H5Tget_fields(tid2, &spos, &epos, &esize, &mpos, &msize) < 0) {
        H5_FAILED();
        printf("Can't get fields\n");
        goto error;
    }
    if(spos != 23 || epos != 16 || esize != 7 || mpos != 0 || msize != 16) {
        H5_FAILED();
        printf("Wrong field values\n");
        goto error;
    }

    if(H5Tget_precision(tid2) != 24) {
        H5_FAILED();
        printf("Can't get precision or wrong precision\n");
        goto error;
    }
    if(H5Tget_offset(tid2)!=0) {
        H5_FAILED();
        printf("Can't get offset or wrong offset\n");
        goto error;
    }
    if((size = H5Tget_size(tid2))!=3) {
        H5_FAILED();
        printf("Can't get size or wrong size\n");
        goto error;
    }
    if(H5Tget_ebias(tid2)!=63) {
        H5_FAILED();
        printf("Can't get exponent bias or wrong bias\n");
        goto error;
    }

    /* Convert data from the 2nd to the 1st derived floating-point type.
     * Then convert data from the 1st type back to the 2nd type.
     * Compare the final data with the original data.
     */
    src_size = H5Tget_size(tid2);
    dst_size = H5Tget_size(tid1);
    endian = H5Tget_order(tid2);
    buf = (unsigned char*)malloc(nelmts*(MAX(src_size, dst_size)));
    saved_buf = (unsigned char*)malloc(nelmts*src_size);
    HDmemset(buf, 0, nelmts*MAX(src_size, dst_size));
    HDmemset(saved_buf, 0, nelmts*src_size);

    for(i=0; i<nelmts*src_size; i++)
        buf[i] = saved_buf[i] = HDrand();

    /* Convert data from the 2nd to the 1st derived floating-point type.
     * The mantissa and exponent of the 2nd type are big enough to retain
     * the precision and exponent power. */
    if(H5Tconvert(tid2, tid1, nelmts, buf, NULL, dxpl_id) < 0) {
        H5_FAILED();
        printf("Can't convert data\n");
        goto error;
    }
    /* Convert data from the 1st back to the 2nd derived floating-point type. */
    if(H5Tconvert(tid1, tid2, nelmts, buf, NULL, dxpl_id) < 0) {
        H5_FAILED();
        printf("Can't convert data\n");
        goto error;
    }

    /* Are the values still the same?*/
    for(i=0; i<nelmts; i++) {
        for(j=0; j<src_size; j++)
            if(buf[i*src_size+j]!=saved_buf[i*src_size+j])
               break;
        if(j==src_size)
           continue; /*no error*/

        /* If original value is NaN(exponent bits are all ones, 11..11),
         * the library simply sets all mantissa bits to ones.  So don't
         * compare values in this case.
         */
        if((buf[i*src_size+2]==0x7f && saved_buf[i*src_size+2]==0x7f) ||
            (buf[i*src_size+2]==0xff && saved_buf[i*src_size+2]==0xff))
            continue;

        /* Print errors */
        if (0==fails_this_test++) {
	    sprintf(str, "\nTesting random sw derived floating-point -> derived floating-point conversions");
	    printf("%-70s", str);
	    HDfflush(stdout);
            H5_FAILED();
        }
        printf("    test %u elmt %u: \n", 1, (unsigned)i);

        printf("        src = ");
        for (j=0; j<src_size; j++)
            printf(" %02x", saved_buf[i*src_size+ENDIAN(src_size, j, endian)]);
        printf("\n");

        printf("        dst = ");
        for (j=0; j<src_size; j++)
            printf(" %02x", buf[i*src_size+ENDIAN(src_size, j, endian)]);
        printf("\n");

        if (fails_this_test>=max_fails) {
            HDputs("    maximum failures reached, aborting test...");
            goto error;
        }
    }

    if (buf) free(buf);
    if (saved_buf) free(saved_buf);

    if(H5Tclose(tid1) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    }

    if(H5Tclose(tid2) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    }

    if(H5Pclose(dxpl_id) < 0) {
        H5_FAILED();
        printf("Can't close property list\n");
        goto error;
    }

    if(H5Fclose(file) < 0) {
        H5_FAILED();
        printf("Can't close file\n");
        goto error;
    } /* end if */

    PASSED();
    reset_hdf5();	/*print statistics*/

    return 0;

 error:
    if (buf) free(buf);
    if (saved_buf) free(saved_buf);
    if (aligned) free(aligned);
    HDfflush(stdout);
    H5E_BEGIN_TRY {
        H5Tclose (tid1);
        H5Tclose (tid2);
        H5Pclose (dxpl_id);
        H5Fclose (file);
    } H5E_END_TRY;
    reset_hdf5(); /*print statistics*/
    return MAX((int)fails_this_test, 1);
}


/*-------------------------------------------------------------------------
 * Function:    test_derived_integer
 *
 * Purpose:     Tests user-define and query functions of integer types.
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 * Programmer:  Raymond Lu
 *              Saturday, Jan 29, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_derived_integer(void)
{
    hid_t       file=-1, tid1=-1, tid2=-1;
    hid_t       dxpl_id=-1;
    char        filename[1024];
    size_t      src_size, dst_size;
    unsigned char        *buf=NULL, *saved_buf=NULL;
    int		endian;			/*endianess	        */
    size_t      nelmts = NTESTELEM;
    unsigned int        fails_this_test = 0;
    const size_t	max_fails=40;	/*max number of failures*/
    char	str[256];		/*message string	*/
    unsigned int         i, j;

    TESTING("user-define and query functions of integer types");

    /* Create File */
    h5_fixname(FILENAME[1], H5P_DEFAULT, filename, sizeof filename);
    if((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("Can't create file\n");
        goto error;
    }

    if((dxpl_id = H5Pcreate(H5P_DATASET_XFER)) < 0) {
        H5_FAILED();
        printf("Can't create data transfer property list\n");
        goto error;
    }

    if((tid1 = H5Tcopy(H5T_STD_I32LE)) < 0) {
        H5_FAILED();
        printf("Can't copy data type\n");
        goto error;
    }

    if((tid2 = H5Tcopy(H5T_STD_U64LE)) < 0) {
        H5_FAILED();
        printf("Can't copy data type\n");
        goto error;
    }

    /*--------------------------------------------------------------------------
     *                   1st integer type
     * size=3 byte, precision=24 bits, offset=0 bits, order=big endian.
     * It can be illustrated in big-endian order as
     *
     *          0       1       2
     *    SIIIIIII IIIIIIII IIIIIIII
     *
     * There's no specific order for these functions to define the attributes
     * of a new integer type, H5Tset_precision, H5Tset_offset, H5Tset_size,
     * H5Tset_order, H5Tset_pad, H5Tset_sign.
     *--------------------------------------------------------------------------*/
    if(H5Tset_offset(tid1, (size_t)0) < 0) {
        H5_FAILED();
        printf("Can't set offset\n");
        goto error;
    }

    if(H5Tset_size(tid1, (size_t)3) < 0) {
        H5_FAILED();
        printf("Can't set size\n");
        goto error;
    }

    if(H5Tset_precision(tid1, (size_t)24) < 0) {
        H5_FAILED();
        printf("Can't set precision\n");
        goto error;
    }

    if(H5Tset_order(tid1, H5T_ORDER_BE) < 0) {
        H5_FAILED();
        printf("Can't set order\n");
        goto error;
    }

    if(H5Tcommit2(file, "new integer type 1", tid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Can't commit data type\n");
        goto error;
    }

    if(H5Tclose(tid1) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    }

    if((tid1 = H5Topen2(file, "new integer type 1", H5P_DEFAULT)) < 0)
        FAIL_PUTS_ERROR("Can't open datatype")
    if(H5Tget_precision(tid1) != 24) {
        H5_FAILED();
        printf("Can't get precision or wrong precision\n");
        goto error;
    }
    if(H5Tget_offset(tid1) != 0) {
        H5_FAILED();
        printf("Can't get offset or wrong offset\n");
        goto error;
    }
    if(H5Tget_size(tid1) != 3) {
        H5_FAILED();
        printf("Can't get size or wrong size\n");
        goto error;
    }
    if(H5Tget_order(tid1)!=H5T_ORDER_BE) {
        H5_FAILED();
        printf("Can't get order or wrong order\n");
        goto error;
    }

    /*--------------------------------------------------------------------------
     *                   2nd integer type
     * size=8 byte, precision=48 bits, offset=10 bits, order=little endian.
     * It can be illustrated in little-endian order as
     *
     *          7       6       5       4       3       2       1       0
     *   ??????SI IIIIIIII IIIIIIII IIIIIIII IIIIIIII IIIIIIII IIIIII?? ????????
     *--------------------------------------------------------------------------*/
    if(H5Tset_precision(tid2, (size_t)48) < 0) {
        H5_FAILED();
        printf("Can't set precision\n");
        goto error;
    }

    if(H5Tset_offset(tid2, (size_t)10) < 0) {
        H5_FAILED();
        printf("Can't set offset\n");
        goto error;
    }

    if(H5Tset_sign(tid2, H5T_SGN_2) < 0) {
        H5_FAILED();
        printf("Can't set offset\n");
        goto error;
    }

    if(H5Tcommit2(file, "new integer type 2", tid2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Can't commit data type\n");
        goto error;
    }

    if(H5Tclose(tid2) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    }

    if((tid2 = H5Topen2(file, "new integer type 2", H5P_DEFAULT)) < 0)
        FAIL_PUTS_ERROR("Can't open datatype")
    if(H5Tget_precision(tid2) != 48) {
        H5_FAILED();
        printf("Can't get precision or wrong precision\n");
        goto error;
    }
    if(H5Tget_offset(tid2) != 10) {
        H5_FAILED();
        printf("Can't get offset or wrong offset\n");
        goto error;
    }
    if(H5Tget_size(tid2) != 8) {
        H5_FAILED();
        printf("Can't get size or wrong size\n");
        goto error;
    }
    if(H5Tget_sign(tid2)!=H5T_SGN_2) {
        H5_FAILED();
        printf("Can't get sign or wrong sign\n");
        goto error;
    }

    /* Convert data from the 1st to the 2nd derived integer type.
     * Then convert data from the 2nd type back to the 1st type.
     * Compare the final data with the original data.
     */
    src_size = H5Tget_size(tid1);
    dst_size = H5Tget_size(tid2);
    endian = H5Tget_order(tid1);
    buf = (unsigned char*)HDmalloc(nelmts*(MAX(src_size, dst_size)));
    saved_buf = (unsigned char*)HDmalloc(nelmts*src_size);
    HDmemset(buf, 0, nelmts*MAX(src_size, dst_size));
    HDmemset(saved_buf, 0, nelmts*src_size);

    for(i=0; i<nelmts*src_size; i++)
        buf[i] = saved_buf[i] = HDrand();

    /* Convert data from the 1st to the 2nd derived integer type.
     * The precision of the 2nd type are big enough to retain
     * the 1st type's precision. */
    if(H5Tconvert(tid1, tid2, nelmts, buf, NULL, dxpl_id) < 0) {
        H5_FAILED();
        printf("Can't convert data\n");
        goto error;
    }
    /* Convert data from the 2nd back to the 1st derived integer type. */
    if(H5Tconvert(tid2, tid1, nelmts, buf, NULL, dxpl_id) < 0) {
        H5_FAILED();
        printf("Can't convert data\n");
        goto error;
    }

    /* Are the values still the same?*/
    for(i=0; i<nelmts; i++) {
        for(j=0; j<src_size; j++)
            if(buf[i*src_size+j]!=saved_buf[i*src_size+j])
               break;
        if(j==src_size)
           continue; /*no error*/

        /* Print errors */
        if (0==fails_this_test++) {
	    sprintf(str, "\nTesting random sw derived integer -> derived integer conversions");
	    printf("%-70s", str);
	    HDfflush(stdout);
            H5_FAILED();
        }
        printf("    test %u elmt %u: \n", 1, (unsigned)i);

        printf("        src = ");
        for (j=0; j<src_size; j++)
            printf(" %02x", saved_buf[i*src_size+ENDIAN(src_size, j, endian)]);
        printf("\n");

        printf("        dst = ");
        for (j=0; j<src_size; j++)
            printf(" %02x", buf[i*src_size+ENDIAN(src_size, j, endian)]);
        printf("\n");

        if (fails_this_test>=max_fails) {
            HDputs("    maximum failures reached, aborting test...");
            goto error;
        }
    }

    if(H5Tclose(tid1) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    }

    if(H5Tclose(tid2) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    }

    if(H5Pclose(dxpl_id) < 0) {
        H5_FAILED();
        printf("Can't close property list\n");
        goto error;
    }

    if(H5Fclose(file) < 0) {
        H5_FAILED();
        printf("Can't close file\n");
        goto error;
    } /* end if */

    free(buf);
    free(saved_buf);

    PASSED();
    reset_hdf5();	/*print statistics*/

    return 0;

 error:
    if (buf) free(buf);
    if (saved_buf) free(saved_buf);
    HDfflush(stdout);
    H5E_BEGIN_TRY {
        H5Tclose (tid1);
        H5Tclose (tid2);
        H5Pclose (dxpl_id);
        H5Fclose (file);
    } H5E_END_TRY;
    reset_hdf5(); /*print statistics*/
    return MAX((int)fails_this_test, 1);
}


/*-------------------------------------------------------------------------
 * Function:	test_conv_int_1
 *
 * Purpose:	Test conversion of integer values from SRC to DST.
 *		These types should be any combination of:
 *
 * 			H5T_NATIVE_SCHAR	H5T_NATIVE_UCHAR
 *			H5T_NATIVE_SHORT	H5T_NATIVE_USHORT
 *			H5T_NATIVE_INT		H5T_NATIVE_UINT
 *			H5T_NATIVE_LONG		H5T_NATIVE_ULONG
 *			H5T_NATIVE_LLONG	H5T_NATIVE_ULLONG
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Monday, November 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_int_1(const char *name, hid_t src, hid_t dst)
{
    size_t	        nelmts=0;		/*num values per test	*/
    const size_t	max_fails=8;		/*max number of failures*/
    size_t		fails_all_tests=0;	/*number of failures	*/
    size_t		fails_this_test;	/*fails for this test	*/
    char		str[256];		/*hello string		*/
    dtype_t		src_type, dst_type;	/*data types		*/
    const char		*src_type_name=NULL;	/*source type name	*/
    const char		*dst_type_name=NULL;	/*destination type name	*/
    int			endian;			/*machine endianess	*/
    size_t		src_size, dst_size;	/*type sizes		*/
    unsigned char	*buf=NULL;		/*buffer for conversion	*/
    unsigned char	*saved=NULL;		/*original values	*/
    size_t		j, k;			/*counters		*/
    unsigned char	*hw=NULL;		/*hardware conv result	*/
    unsigned char	src_bits[32];		/*src value in LE order	*/
    unsigned char	dst_bits[32];		/*dest value in LE order*/
    size_t		src_nbits;		/*source length in bits	*/
    size_t		dst_nbits;		/*dst length in bits	*/
    H5T_sign_t          src_sign;               /*source sign type      */
    H5T_sign_t          dst_sign;               /*dst sign type         */
    void		*aligned=NULL;		/*aligned temp buffer	*/
    signed char		hw_char;
    unsigned char	hw_uchar;
    short		hw_short;
    unsigned short	hw_ushort;
    int			hw_int;
    unsigned		hw_uint;
    long		hw_long;
    unsigned long	hw_ulong;
    long long		hw_llong;
    unsigned long long	hw_ullong;

    /* What are the names of the source and destination types */
    if (H5Tequal(src, H5T_NATIVE_SCHAR)) {
	src_type_name = "signed char";
	src_type = INT_SCHAR;
    } else if (H5Tequal(src, H5T_NATIVE_UCHAR)) {
	src_type_name = "unsigned char";
	src_type = INT_UCHAR;
    } else if (H5Tequal(src, H5T_NATIVE_SHORT)) {
	src_type_name = "short";
	src_type = INT_SHORT;
    } else if (H5Tequal(src, H5T_NATIVE_USHORT)) {
	src_type_name = "unsigned short";
	src_type = INT_USHORT;
    } else if (H5Tequal(src, H5T_NATIVE_INT)) {
	src_type_name = "int";
	src_type = INT_INT;
    } else if (H5Tequal(src, H5T_NATIVE_UINT)) {
	src_type_name = "unsigned int";
	src_type = INT_UINT;
    } else if (H5Tequal(src, H5T_NATIVE_LONG)) {
	src_type_name = "long";
	src_type = INT_LONG;
    } else if (H5Tequal(src, H5T_NATIVE_ULONG)) {
	src_type_name = "unsigned long";
	src_type = INT_ULONG;
    } else if (H5Tequal(src, H5T_NATIVE_LLONG)) {
	src_type_name = "long long";
	src_type = INT_LLONG;
    } else if (H5Tequal(src, H5T_NATIVE_ULLONG)) {
	src_type_name = "unsigned long long";
	src_type = INT_ULLONG;
    } else {
	src_type_name = "UNKNOWN";
	src_type = OTHER;
    }

    if (H5Tequal(dst, H5T_NATIVE_SCHAR)) {
	dst_type_name = "signed char";
	dst_type = INT_SCHAR;
    } else if (H5Tequal(dst, H5T_NATIVE_UCHAR)) {
	dst_type_name = "unsigned char";
	dst_type = INT_UCHAR;
    } else if (H5Tequal(dst, H5T_NATIVE_SHORT)) {
	dst_type_name = "short";
	dst_type = INT_SHORT;
    } else if (H5Tequal(dst, H5T_NATIVE_USHORT)) {
	dst_type_name = "unsigned short";
	dst_type = INT_USHORT;
    } else if (H5Tequal(dst, H5T_NATIVE_INT)) {
	dst_type_name = "int";
	dst_type = INT_INT;
    } else if (H5Tequal(dst, H5T_NATIVE_UINT)) {
	dst_type_name = "unsigned int";
	dst_type = INT_UINT;
    } else if (H5Tequal(dst, H5T_NATIVE_LONG)) {
	dst_type_name = "long";
	dst_type = INT_LONG;
    } else if (H5Tequal(dst, H5T_NATIVE_ULONG)) {
	dst_type_name = "unsigned long";
	dst_type = INT_ULONG;
    } else if (H5Tequal(dst, H5T_NATIVE_LLONG)) {
	dst_type_name = "long long";
	dst_type = INT_LLONG;
    } else if (H5Tequal(dst, H5T_NATIVE_ULLONG)) {
	dst_type_name = "unsigned long long";
	dst_type = INT_ULLONG;
    } else {
	dst_type_name = "UNKNOWN";
	dst_type = OTHER;
    }

    /* Sanity checks */
    if (OTHER==src_type || OTHER==dst_type) {
	sprintf(str, "Testing %s %s -> %s conversions",
		name, src_type_name, dst_type_name);
	printf("%-70s", str);
	H5_FAILED();
	HDputs("    Unknown data type.");
	goto error;
    } else {
        sprintf(str, "Testing %s %s -> %s conversions",
            name, src_type_name, dst_type_name);
        printf("%-70s", str);
        HDfflush(stdout);
        fails_this_test=0;
    }

    /* Some information about datatypes */
    endian = H5Tget_order(H5T_NATIVE_INT);
    src_size = H5Tget_size(src);
    dst_size = H5Tget_size(dst);
    src_nbits = H5Tget_precision(src); /* not 8*src_size, esp on J90 - QAK */
    dst_nbits = H5Tget_precision(dst); /* not 8*dst_size, esp on J90 - QAK */
    src_sign = H5Tget_sign(src);
    dst_sign = H5Tget_sign(dst);
    aligned = HDcalloc((size_t)1, sizeof(long long));

    /* Allocate and initialize the source buffer through macro INIT_INTEGER.  The BUF
     * will be used for the conversion while the SAVED buffer will be
     * used for the comparison later.
     */
    if(src_type == INT_SCHAR) {
        INIT_INTEGER(signed char, SCHAR_MAX, SCHAR_MIN, src_size, dst_size, src_nbits, buf, saved, nelmts);
    } else if(src_type == INT_UCHAR) {
        INIT_INTEGER(unsigned char, UCHAR_MAX, 0, src_size, dst_size, src_nbits, buf, saved, nelmts);
    } else if(src_type == INT_SHORT) {
        INIT_INTEGER(short, SHRT_MAX, SHRT_MIN, src_size, dst_size, src_nbits, buf, saved, nelmts);
    } else if(src_type == INT_USHORT) {
        INIT_INTEGER(unsigned short, USHRT_MAX, 0, src_size, dst_size, src_nbits, buf, saved, nelmts);
    } else if(src_type == INT_INT) {
        INIT_INTEGER(int, INT_MAX, INT_MIN, src_size, dst_size, src_nbits, buf, saved, nelmts);
    } else if(src_type == INT_UINT) {
        INIT_INTEGER(unsigned int, UINT_MAX, 0, src_size, dst_size, src_nbits, buf, saved, nelmts);
    } else if(src_type == INT_LONG) {
        INIT_INTEGER(long, LONG_MAX, LONG_MIN, src_size, dst_size, src_nbits, buf, saved, nelmts);
    } else if(src_type == INT_ULONG) {
        INIT_INTEGER(unsigned long, ULONG_MAX, 0, src_size, dst_size, src_nbits, buf, saved, nelmts);
    } else if(src_type == INT_LLONG) {
        INIT_INTEGER(long long, LLONG_MAX, LLONG_MIN, src_size, dst_size, src_nbits, buf, saved, nelmts);
    } else if(src_type == INT_ULLONG) {
        INIT_INTEGER(unsigned long long, ULLONG_MAX, 0, src_size, dst_size, src_nbits, buf, saved, nelmts);
    } else
        goto error;

    /* Perform the conversion */
    if (H5Tconvert(src, dst, nelmts, buf, NULL, H5P_DEFAULT) < 0)
        goto error;

    /* Check the results from the library against hardware */
    for (j=0; j<nelmts; j++) {
        if (INT_SCHAR==dst_type) {
            hw = (unsigned char*)&hw_char;
            switch (src_type) {
            case INT_SCHAR:
                HDmemcpy(aligned, saved+j*sizeof(signed char), sizeof(signed char));
                hw_char = (signed char)(*((signed char*)aligned));
                break;
            case INT_UCHAR:
                HDmemcpy(aligned, saved+j*sizeof(unsigned char), sizeof(unsigned char));
                hw_char = (signed char)(*((unsigned char*)aligned));
                break;
            case INT_SHORT:
                HDmemcpy(aligned, saved+j*sizeof(short), sizeof(short));
                hw_char = (signed char)(*((short*)aligned));
                break;
            case INT_USHORT:
                HDmemcpy(aligned, saved+j*sizeof(unsigned short), sizeof(unsigned short));
                hw_char = (signed char)(*((unsigned short*)aligned));
                break;
            case INT_INT:
                HDmemcpy(aligned, saved+j*sizeof(int), sizeof(int));
                hw_char = (signed char)(*((int*)aligned));
                break;
            case INT_UINT:
                HDmemcpy(aligned, saved+j*sizeof(unsigned), sizeof(unsigned));
                hw_char = (signed char)(*((unsigned*)aligned));
                break;
            case INT_LONG:
                HDmemcpy(aligned, saved+j*sizeof(long), sizeof(long));
                hw_char = (signed char)(*((long*)aligned));
                break;
            case INT_ULONG:
                HDmemcpy(aligned, saved+j*sizeof(unsigned long), sizeof(unsigned long));
                hw_char = (signed char)(*((unsigned long*)aligned));
                break;
            case INT_LLONG:
                HDmemcpy(aligned, saved+j*sizeof(long long), sizeof(long long));
                hw_char = (signed char)(*((long long*)aligned));
                break;
            case INT_ULLONG:
                HDmemcpy(aligned, saved+j*sizeof(unsigned long long), sizeof(unsigned long long));
                hw_char = (signed char)(*((unsigned long long*)aligned));
                break;
            case FLT_FLOAT:
            case FLT_DOUBLE:
            case FLT_LDOUBLE:
            case OTHER:
            default:
                break;
            }
        } else if (INT_UCHAR==dst_type) {
            hw = (unsigned char*)&hw_uchar;
            switch (src_type) {
            case INT_SCHAR:
                HDmemcpy(aligned, saved+j*sizeof(signed char), sizeof(signed char));
                hw_uchar = (unsigned char)(*((signed char*)aligned));
                break;
            case INT_UCHAR:
                HDmemcpy(aligned, saved+j*sizeof(unsigned char), sizeof(unsigned char));
                hw_uchar = (unsigned char)(*((unsigned char*)aligned));
                break;
            case INT_SHORT:
                HDmemcpy(aligned, saved+j*sizeof(short), sizeof(short));
                hw_uchar = (unsigned char)(*((short*)aligned));
                break;
            case INT_USHORT:
                HDmemcpy(aligned, saved+j*sizeof(unsigned short), sizeof(unsigned short));
                hw_uchar = (unsigned char)(*((unsigned short*)aligned));
                break;
            case INT_INT:
                HDmemcpy(aligned, saved+j*sizeof(int), sizeof(int));
                hw_uchar = (unsigned char)(*((int*)aligned));
                break;
            case INT_UINT:
                HDmemcpy(aligned, saved+j*sizeof(unsigned), sizeof(unsigned));
                hw_uchar = (unsigned char)(*((unsigned*)aligned));
                break;
            case INT_LONG:
                HDmemcpy(aligned, saved+j*sizeof(long), sizeof(long));
                hw_uchar = (unsigned char)(*((long*)aligned));
                break;
            case INT_ULONG:
                HDmemcpy(aligned, saved+j*sizeof(unsigned long), sizeof(unsigned long));
                hw_uchar = (unsigned char)(*((unsigned long*)aligned));
                break;
            case INT_LLONG:
                HDmemcpy(aligned, saved+j*sizeof(long long), sizeof(long long));
                hw_uchar = (unsigned char)(*((long long*)aligned));
                break;
            case INT_ULLONG:
                HDmemcpy(aligned, saved+j*sizeof(unsigned long long), sizeof(unsigned long long));
                hw_uchar = (unsigned char)(*((unsigned long long*)aligned));
                break;
            case FLT_FLOAT:
            case FLT_DOUBLE:
            case FLT_LDOUBLE:
            case OTHER:
            default:
                break;
            }
        } else if (INT_SHORT==dst_type) {
            hw = (unsigned char*)&hw_short;
            switch (src_type) {
            case INT_SCHAR:
                HDmemcpy(aligned, saved+j*sizeof(char), sizeof(char));
                hw_short = (short)(*((char*)aligned));
                break;
            case INT_UCHAR:
                HDmemcpy(aligned, saved+j*sizeof(unsigned char), sizeof(unsigned char));
                hw_short = (short)(*((unsigned char*)aligned));
                break;
            case INT_SHORT:
                HDmemcpy(aligned, saved+j*sizeof(short), sizeof(short));
                hw_short = (short)(*((short*)aligned));
                break;
            case INT_USHORT:
                HDmemcpy(aligned, saved+j*sizeof(unsigned short), sizeof(unsigned short));
                hw_short = (short)(*((unsigned short*)aligned));
                break;
            case INT_INT:
                HDmemcpy(aligned, saved+j*sizeof(int), sizeof(int));
                hw_short = (short)(*((int*)aligned));
                break;
            case INT_UINT:
                HDmemcpy(aligned, saved+j*sizeof(unsigned), sizeof(unsigned));
                hw_short = (short)(*((unsigned*)aligned));
                break;
            case INT_LONG:
                HDmemcpy(aligned, saved+j*sizeof(long), sizeof(long));
                hw_short = (short)(*((long*)aligned));
                break;
            case INT_ULONG:
                HDmemcpy(aligned, saved+j*sizeof(unsigned long), sizeof(unsigned long));
                hw_short = (short)(*((unsigned long*)aligned));
                break;
            case INT_LLONG:
                HDmemcpy(aligned, saved+j*sizeof(long long), sizeof(long long));
                hw_short = (short)(*((long long*)aligned));
                break;
            case INT_ULLONG:
                HDmemcpy(aligned, saved+j*sizeof(unsigned long long), sizeof(unsigned long long));
                hw_short = (short)(*((unsigned long long*)aligned));
                break;

            case FLT_FLOAT:
            case FLT_DOUBLE:
            case FLT_LDOUBLE:
            case OTHER:
            default:
                break;
            }
        } else if (INT_USHORT==dst_type) {
            hw = (unsigned char*)&hw_ushort;
            switch (src_type) {
            case INT_SCHAR:
                HDmemcpy(aligned, saved+j*sizeof(signed char), sizeof(signed char));
                hw_ushort = (unsigned short)(*((signed char*)aligned));
                break;
            case INT_UCHAR:
                HDmemcpy(aligned, saved+j*sizeof(unsigned char), sizeof(unsigned char));
                hw_ushort = (unsigned short)(*((unsigned char*)aligned));
                break;
            case INT_SHORT:
                HDmemcpy(aligned, saved+j*sizeof(short), sizeof(short));
                hw_ushort = (unsigned short)(*((short*)aligned));
                break;
            case INT_USHORT:
                HDmemcpy(aligned, saved+j*sizeof(unsigned short), sizeof(unsigned short));
                hw_ushort = (unsigned short)(*((unsigned short*)aligned));
                break;
            case INT_INT:
                HDmemcpy(aligned, saved+j*sizeof(int), sizeof(int));
                hw_ushort = (unsigned short)(*((int*)aligned));
                break;
            case INT_UINT:
                HDmemcpy(aligned, saved+j*sizeof(unsigned), sizeof(unsigned));
                hw_ushort = (unsigned short)(*((unsigned*)aligned));
                break;
            case INT_LONG:
                HDmemcpy(aligned, saved+j*sizeof(long), sizeof(long));
                hw_ushort = (unsigned short)(*((long*)aligned));
                break;
            case INT_ULONG:
                HDmemcpy(aligned, saved+j*sizeof(unsigned long), sizeof(unsigned long));
                hw_ushort = (unsigned short)(*((unsigned long*)aligned));
                break;
            case INT_LLONG:
                HDmemcpy(aligned, saved+j*sizeof(long long), sizeof(long long));
                hw_ushort = (unsigned short)(*((long long*)aligned));
                break;
            case INT_ULLONG:
                HDmemcpy(aligned, saved+j*sizeof(unsigned long long), sizeof(unsigned long long));
                hw_ushort = (unsigned short)(*((unsigned long long*)aligned));
                break;
            case FLT_FLOAT:
            case FLT_DOUBLE:
            case FLT_LDOUBLE:
            case OTHER:
            default:
                break;
            }
        } else if (INT_INT==dst_type) {
            hw = (unsigned char*)&hw_int;
            switch (src_type) {
            case INT_SCHAR:
                HDmemcpy(aligned, saved+j*sizeof(signed char), sizeof(signed char));
                hw_int = (int)(*((signed char*)aligned));
                break;
            case INT_UCHAR:
                HDmemcpy(aligned, saved+j*sizeof(unsigned char), sizeof(unsigned char));
                hw_int = (int)(*((unsigned char*)aligned));
                break;
            case INT_SHORT:
                HDmemcpy(aligned, saved+j*sizeof(short), sizeof(short));
                hw_int = (int)(*((short*)aligned));
                break;
            case INT_USHORT:
                HDmemcpy(aligned, saved+j*sizeof(unsigned short), sizeof(unsigned short));
                hw_int = (int)(*((unsigned short*)aligned));
                break;
            case INT_INT:
                HDmemcpy(aligned, saved+j*sizeof(int), sizeof(int));
                hw_int = (int)(*((int*)aligned));
                break;
            case INT_UINT:
                HDmemcpy(aligned, saved+j*sizeof(unsigned), sizeof(unsigned));
                hw_int = (int)(*((unsigned*)aligned));
                break;
            case INT_LONG:
                HDmemcpy(aligned, saved+j*sizeof(long), sizeof(long));
                hw_int = (int)(*((long*)aligned));
                break;
            case INT_ULONG:
                HDmemcpy(aligned, saved+j*sizeof(unsigned long), sizeof(unsigned long));
                hw_int = (int)(*((unsigned long*)aligned));
                break;
            case INT_LLONG:
                HDmemcpy(aligned, saved+j*sizeof(long long), sizeof(long long));
                hw_int = (int)(*((long long*)aligned));
                break;
            case INT_ULLONG:
                HDmemcpy(aligned, saved+j*sizeof(unsigned long long), sizeof(unsigned long long));
                hw_int = (int)(*((unsigned long long*)aligned));
                break;
            case FLT_FLOAT:
            case FLT_DOUBLE:
            case FLT_LDOUBLE:
            case OTHER:
            default:
                break;
            }
        } else if (INT_UINT==dst_type) {
            hw = (unsigned char*)&hw_uint;
            switch (src_type) {
            case INT_SCHAR:
                HDmemcpy(aligned, saved+j*sizeof(signed char), sizeof(signed char));
                hw_uint = (unsigned int)(*((signed char*)aligned));
                break;
            case INT_UCHAR:
                HDmemcpy(aligned, saved+j*sizeof(unsigned char), sizeof(unsigned char));
                hw_uint = (unsigned int)(*((unsigned char*)aligned));
                break;
            case INT_SHORT:
                HDmemcpy(aligned, saved+j*sizeof(short), sizeof(short));
                hw_uint = (unsigned int)(*((short*)aligned));
                break;
            case INT_USHORT:
                HDmemcpy(aligned, saved+j*sizeof(unsigned short), sizeof(unsigned short));
                hw_uint = (unsigned int)(*((unsigned short*)aligned));
                break;
            case INT_INT:
                HDmemcpy(aligned, saved+j*sizeof(int), sizeof(int));
                hw_uint = (unsigned int)(*((int*)aligned));
                break;
            case INT_UINT:
                HDmemcpy(aligned, saved+j*sizeof(unsigned), sizeof(unsigned));
                hw_uint = (unsigned int)(*((unsigned*)aligned));
                break;
            case INT_LONG:
                HDmemcpy(aligned, saved+j*sizeof(long), sizeof(long));
                hw_uint = (unsigned int)(*((long*)aligned));
                break;
            case INT_ULONG:
                HDmemcpy(aligned, saved+j*sizeof(unsigned long), sizeof(unsigned long));
                hw_uint = (unsigned int)(*((unsigned long*)aligned));
                break;
            case INT_LLONG:
                HDmemcpy(aligned, saved+j*sizeof(long long), sizeof(long long));
                hw_uint = (unsigned int)(*((long long*)aligned));
                break;
            case INT_ULLONG:
                HDmemcpy(aligned, saved+j*sizeof(unsigned long long), sizeof(unsigned long long));
                hw_uint = (unsigned int)(*((unsigned long long*)aligned));
                break;
            case FLT_FLOAT:
            case FLT_DOUBLE:
            case FLT_LDOUBLE:
            case OTHER:
            default:
                break;
            }
        } else if (INT_LONG==dst_type) {
            hw = (unsigned char*)&hw_long;
            switch (src_type) {
            case INT_SCHAR:
                HDmemcpy(aligned, saved+j*sizeof(signed char), sizeof(signed char));
                hw_long = (long int)(*((signed char*)aligned));
                break;
            case INT_UCHAR:
                HDmemcpy(aligned, saved+j*sizeof(unsigned char), sizeof(unsigned char));
                hw_long = (long int)(*((unsigned char*)aligned));
                break;
            case INT_SHORT:
                HDmemcpy(aligned, saved+j*sizeof(short), sizeof(short));
                hw_long = (long int)(*((short*)aligned));
                break;
            case INT_USHORT:
                HDmemcpy(aligned, saved+j*sizeof(unsigned short), sizeof(unsigned short));
                hw_long = (long int)(*((unsigned short*)aligned));
                break;
            case INT_INT:
                HDmemcpy(aligned, saved+j*sizeof(int), sizeof(int));
                hw_long = (long int)(*((int*)aligned));
                break;
            case INT_UINT:
                HDmemcpy(aligned, saved+j*sizeof(unsigned), sizeof(unsigned));
                hw_long = (long int)(*((unsigned*)aligned));
                break;
            case INT_LONG:
                HDmemcpy(aligned, saved+j*sizeof(long), sizeof(long));
                hw_long = (long int)(*((long*)aligned));
                break;
            case INT_ULONG:
                HDmemcpy(aligned, saved+j*sizeof(unsigned long), sizeof(unsigned long));
                hw_long = (long int)(*((unsigned long*)aligned));
                break;
            case INT_LLONG:
                HDmemcpy(aligned, saved+j*sizeof(long long), sizeof(long long));
                hw_long = (long int)(*((long long*)aligned));
                break;
            case INT_ULLONG:
                HDmemcpy(aligned, saved+j*sizeof(unsigned long long), sizeof(unsigned long long));
                hw_long = (long int)(*((unsigned long long*)aligned));
                break;
            case FLT_FLOAT:
            case FLT_DOUBLE:
            case FLT_LDOUBLE:
            case OTHER:
            default:
                break;
            }
        } else if (INT_ULONG==dst_type) {
            hw = (unsigned char*)&hw_ulong;
            switch (src_type) {
            case INT_SCHAR:
                HDmemcpy(aligned, saved+j*sizeof(signed char), sizeof(signed char));
                hw_ulong = (unsigned long)(*((signed char*)aligned));
                break;
            case INT_UCHAR:
                HDmemcpy(aligned, saved+j*sizeof(unsigned char), sizeof(unsigned char));
                hw_ulong = (unsigned long)(*((unsigned char*)aligned));
                break;
            case INT_SHORT:
                HDmemcpy(aligned, saved+j*sizeof(short), sizeof(short));
                hw_ulong = (unsigned long)(*((short*)aligned));
                break;
            case INT_USHORT:
                HDmemcpy(aligned, saved+j*sizeof(unsigned short), sizeof(unsigned short));
                hw_ulong = (unsigned long)(*((unsigned short*)aligned));
                break;
            case INT_INT:
                HDmemcpy(aligned, saved+j*sizeof(int), sizeof(int));
                hw_ulong = (unsigned long)(*((int*)aligned));
                break;
            case INT_UINT:
                HDmemcpy(aligned, saved+j*sizeof(unsigned), sizeof(unsigned));
                hw_ulong = (unsigned long)(*((unsigned*)aligned));
                break;
            case INT_LONG:
                HDmemcpy(aligned, saved+j*sizeof(long), sizeof(long));
                hw_ulong = (unsigned long)(*((long*)aligned));
                break;
            case INT_ULONG:
                HDmemcpy(aligned, saved+j*sizeof(unsigned long), sizeof(unsigned long));
                hw_ulong = (unsigned long)(*((unsigned long*)aligned));
                break;
            case INT_LLONG:
                HDmemcpy(aligned, saved+j*sizeof(long long), sizeof(long long));
                hw_ulong = (unsigned long)(*((long long*)aligned));
                break;
            case INT_ULLONG:
                HDmemcpy(aligned, saved+j*sizeof(unsigned long long), sizeof(unsigned long long));
                hw_ulong = (unsigned long)(*((unsigned long long*)aligned));
                break;
            case FLT_FLOAT:
            case FLT_DOUBLE:
            case FLT_LDOUBLE:
            case OTHER:
            default:
                break;
            }
        } else if (INT_LLONG==dst_type) {
            hw = (unsigned char*)&hw_llong;
            switch (src_type) {
            case INT_SCHAR:
                HDmemcpy(aligned, saved+j*sizeof(char), sizeof(char));
                hw_llong = (long long)(*((char*)aligned));
                break;
            case INT_UCHAR:
                HDmemcpy(aligned, saved+j*sizeof(unsigned char), sizeof(unsigned char));
                hw_llong = (long long)(*((unsigned char*)aligned));
                break;
            case INT_SHORT:
                HDmemcpy(aligned, saved+j*sizeof(short), sizeof(short));
                hw_llong = (long long)(*((short*)aligned));
                break;
            case INT_USHORT:
                HDmemcpy(aligned, saved+j*sizeof(unsigned short), sizeof(unsigned short));
                hw_llong = (long long)(*((unsigned short*)aligned));
                break;
            case INT_INT:
                HDmemcpy(aligned, saved+j*sizeof(int), sizeof(int));
                hw_llong = (long long)(*((int*)aligned));
                break;
            case INT_UINT:
                HDmemcpy(aligned, saved+j*sizeof(unsigned), sizeof(unsigned));
                hw_llong = (long long)(*((unsigned*)aligned));
                break;
            case INT_LONG:
                HDmemcpy(aligned, saved+j*sizeof(long), sizeof(long));
                hw_llong = (long long)(*((long*)aligned));
                break;
            case INT_ULONG:
                HDmemcpy(aligned, saved+j*sizeof(unsigned long), sizeof(unsigned long));
                hw_llong = (long long)(*((unsigned long*)aligned));
                break;
            case INT_LLONG:
                HDmemcpy(aligned, saved+j*sizeof(long long), sizeof(long long));
                hw_llong = (long long)(*((long long*)aligned));
                break;
            case INT_ULLONG:
                HDmemcpy(aligned, saved+j*sizeof(unsigned long long), sizeof(unsigned long long));
                hw_llong = (long long)(*((unsigned long long*)aligned));
                break;
            case FLT_FLOAT:
            case FLT_DOUBLE:
            case FLT_LDOUBLE:
            case OTHER:
            default:
                break;
            }
        } else if (INT_ULLONG==dst_type) {
            hw = (unsigned char*)&hw_ullong;
            switch (src_type) {
            case INT_SCHAR:
                HDmemcpy(aligned, saved+j*sizeof(signed char), sizeof(signed char));
                hw_ullong = (unsigned long long)(*((signed char*)aligned));
                break;
            case INT_UCHAR:
                HDmemcpy(aligned, saved+j*sizeof(unsigned char), sizeof(unsigned char));
                hw_ullong = (unsigned long long)(*((unsigned char*)aligned));
                break;
            case INT_SHORT:
                HDmemcpy(aligned, saved+j*sizeof(short), sizeof(short));
                hw_ullong = (unsigned long long)(*((short*)aligned));
                break;
            case INT_USHORT:
                HDmemcpy(aligned, saved+j*sizeof(unsigned short), sizeof(unsigned short));
                hw_ullong = (unsigned long long)(*((unsigned short*)aligned));
                break;
            case INT_INT:
                HDmemcpy(aligned, saved+j*sizeof(int), sizeof(int));
                hw_ullong = (unsigned long long)(*((int*)aligned));
                break;
            case INT_UINT:
                HDmemcpy(aligned, saved+j*sizeof(unsigned), sizeof(unsigned));
                hw_ullong = (unsigned long long)(*((unsigned*)aligned));
                break;
            case INT_LONG:
                HDmemcpy(aligned, saved+j*sizeof(long), sizeof(long));
                hw_ullong = (unsigned long long)(*((long*)aligned));
                break;
            case INT_ULONG:
                HDmemcpy(aligned, saved+j*sizeof(unsigned long), sizeof(unsigned long));
                hw_ullong = (unsigned long long)(*((unsigned long*)aligned));
                break;
            case INT_LLONG:
                HDmemcpy(aligned, saved+j*sizeof(long long), sizeof(long long));
                hw_ullong = (unsigned long long)(*((long long*)aligned));
                break;
            case INT_ULLONG:
                HDmemcpy(aligned, saved+j*sizeof(unsigned long long), sizeof(unsigned long long));
                hw_ullong = (unsigned long long)(*((unsigned long long*)aligned));
                break;
            case FLT_FLOAT:
            case FLT_DOUBLE:
            case FLT_LDOUBLE:
            case OTHER:
            default:
                break;
            }
        }

        /* Make certain that there isn't some weird number of destination bits */
        assert(dst_nbits%8==0);

        /* Are the two results the same? */
        for (k=(dst_size-(dst_nbits/8)); k<dst_size; k++)
            if (buf[j*dst_size+k]!=hw[k])
                break;
        if (k==dst_size)
            continue; /*no error*/

        /*
         * Convert the source and destination values to little endian
         * order so we can use the HDF5 bit vector operations to test
         * certain things.  These routines have already been tested by
         * the `bittests' program.
         */
        for (k=0; k<src_size; k++)
            src_bits[src_size-(k+1)] = saved[j*src_size+ENDIAN(src_size, k, endian)];

        for (k=0; k<dst_size; k++)
            dst_bits[dst_size-(k+1)] = buf[j*dst_size+ENDIAN(dst_size, k, endian)];

        /*
         * Hardware usually doesn't handle overflows too gracefully. The
         * hardware conversion result during overflows is usually garbage
         * so we must handle those cases differetly when checking results.
         */
        if (H5T_SGN_2==src_sign && H5T_SGN_2==dst_sign) {
            if (src_nbits>dst_nbits) {
                if(0==H5T_bit_get_d(src_bits, src_nbits-1, (size_t)1) &&
                        H5T_bit_find(src_bits, dst_nbits-1, (src_nbits-dst_nbits),
                            H5T_BIT_MSB, 1)>=0) {
                    /*
                     * Source is positive and the magnitude is too large for
                     * the destination.  The destination should be set to the
                     * maximum possible value: 0x7f...f
                     */
                    if (0==H5T_bit_get_d(dst_bits, dst_nbits-1, (size_t)1) &&
                            H5T_bit_find(dst_bits, (size_t)0, dst_nbits-1, H5T_BIT_LSB, 0) < 0)
                        continue; /*no error*/
                } else if (1==H5T_bit_get_d(src_bits, src_nbits-1, (size_t)1) &&
                       H5T_bit_find(src_bits, (size_t)0, src_nbits-1, H5T_BIT_MSB,
                            0)+1>=(ssize_t)dst_nbits) {
                    /*
                     * Source is negative but the magnitude is too large for
                     * the destination. The destination should be set to the
                     * smallest possible value: 0x80...0
                     */
                    if (1==H5T_bit_get_d(dst_bits, dst_nbits-1, (size_t)1) &&
                            H5T_bit_find(dst_bits, (size_t)0, dst_nbits-1, H5T_BIT_LSB, 1) < 0)
                        continue; /*no error*/
                }
            } else if(src_nbits<dst_nbits) {
                /* Source is smaller than the destination */
                if(0==H5T_bit_get_d(src_bits, src_nbits-1, (size_t)1)) {
                    /*
                     * Source is positive, so the excess bits in the
                     * destination should be set to 0's.
                     */
                    if (0==H5T_bit_get_d(dst_bits, src_nbits-1, (size_t)1) &&
                            H5T_bit_find(dst_bits, src_nbits, dst_nbits-src_nbits, H5T_BIT_LSB, 1) < 0)
                        continue; /*no error*/
                } else {
                    /*
                     * Source is negative, so the excess bits in the
                     * destination should be set to 1's.
                     */
                    if (1==H5T_bit_get_d(dst_bits, src_nbits-1, (size_t)1) &&
                            H5T_bit_find(dst_bits, src_nbits, dst_nbits-src_nbits, H5T_BIT_LSB, 0) < 0)
                        continue; /*no error*/
                }
            }
        } else if (H5T_SGN_2==src_sign && H5T_SGN_NONE==dst_sign) {
            if (H5T_bit_get_d(src_bits, src_nbits-1, (size_t)1)) {
                /*
                 * The source is negative so the result should be zero.
                 * The source is negative if the most significant bit is
                 * set.  The destination is zero if all bits are zero.
                 */
                if (H5T_bit_find(dst_bits, (size_t)0, dst_nbits, H5T_BIT_LSB, 1) < 0)
                    continue; /*no error*/
            } else if (src_nbits>dst_nbits &&
                   H5T_bit_find(src_bits, dst_nbits-1,
                        src_nbits-dst_nbits, H5T_BIT_LSB, 1)>=0) {
                /*
                 * The source is a value with a magnitude too large for
                 * the destination.  The destination should be the
                 * largest possible value: 0xff...f
                 */
                if (H5T_bit_find(dst_bits, (size_t)0, dst_nbits, H5T_BIT_LSB, 0) < 0)
                    continue; /*no error*/
            }
        } else if (H5T_SGN_NONE==src_sign && H5T_SGN_2==dst_sign) {
            if (src_nbits>=dst_nbits &&
                    H5T_bit_find(src_bits, dst_nbits-1, (src_nbits-dst_nbits)+1,
                        H5T_BIT_LSB, 1)>=0) {
                /*
                 * The source value has a magnitude that is larger than
                 * the destination can handle.  The destination should be
                 * set to the largest possible positive value: 0x7f...f
                 */
                if (0==H5T_bit_get_d(dst_bits, dst_nbits-1, (size_t)1) &&
                        H5T_bit_find(dst_bits, (size_t)0, dst_nbits-1, H5T_BIT_LSB, 0) < 0)
                    continue; /*no error*/
            }
        } else {
            if (src_nbits>dst_nbits &&
                    H5T_bit_find(src_bits, dst_nbits, src_nbits-dst_nbits,
                         H5T_BIT_LSB, 1)>=0) {
                /*
                 * The unsigned source has a value which is too large for
                 * the unsigned destination.  The destination should be
                 * set to the largest possible value: 0xff...f
                 */
                if (H5T_bit_find(dst_bits, (size_t)0, dst_nbits, H5T_BIT_LSB, 0) < 0)
                    continue; /*no error*/
            }
        }

        /* Print errors */
        if (0==fails_this_test++)
            H5_FAILED();
        printf("    elmt %u\n", (unsigned)j);

        printf("        src = ");
        for (k=0; k<src_size; k++)
            printf(" %02x", saved[j*src_size+ENDIAN(src_size, k, endian)]);
        printf("%*s", (int)(3*MAX(0, (ssize_t)dst_size-(ssize_t)src_size)), "");
        switch (src_type) {
            case INT_SCHAR:
                HDmemcpy(aligned, saved+j*sizeof(signed char), sizeof(signed char));
                printf(" %29d\n", (int)*((signed char*)aligned));
                break;
            case INT_UCHAR:
                HDmemcpy(aligned, saved+j*sizeof(unsigned char), sizeof(unsigned char));
                printf(" %29u\n", (unsigned)*((unsigned char*)aligned));
                break;
            case INT_SHORT:
                HDmemcpy(aligned, saved+j*sizeof(short), sizeof(short));
                printf(" %29hd\n", *((short*)aligned));
                break;
            case INT_USHORT:
                HDmemcpy(aligned, saved+j*sizeof(unsigned short), sizeof(unsigned short));
                printf(" %29hu\n", *((unsigned short*)aligned));
                break;
            case INT_INT:
                HDmemcpy(aligned, saved+j*sizeof(int), sizeof(int));
                printf(" %29d\n", *((int*)aligned));
                break;
            case INT_UINT:
                HDmemcpy(aligned, saved+j*sizeof(unsigned), sizeof(unsigned));
                printf(" %29u\n", *((unsigned*)aligned));
                break;
            case INT_LONG:
                HDmemcpy(aligned, saved+j*sizeof(long), sizeof(long));
                printf(" %29ld\n", *((long*)aligned));
                break;
            case INT_ULONG:
                HDmemcpy(aligned, saved+j*sizeof(unsigned long), sizeof(unsigned long));
                printf(" %29lu\n", *((unsigned long*)aligned));
                break;
            case INT_LLONG:
                HDmemcpy(aligned, saved+j*sizeof(long long), sizeof(long long));
                HDfprintf(stdout," %29"H5_PRINTF_LL_WIDTH"d\n", *((long long*)aligned));
                break;
            case INT_ULLONG:
                HDmemcpy(aligned, saved+j*sizeof(unsigned long long), sizeof(unsigned long long));
                HDfprintf(stdout," %29"H5_PRINTF_LL_WIDTH"u\n", *((unsigned long long*)aligned));
                break;
            case FLT_FLOAT:
            case FLT_DOUBLE:
            case FLT_LDOUBLE:
            case OTHER:
            default:
                break;
        }

        printf("        dst = ");
        for (k=0; k<dst_size; k++)
            printf(" %02x", buf[j*dst_size+ENDIAN(dst_size, k, endian)]);
        printf("%*s", (int)(3*MAX(0, (ssize_t)src_size-(ssize_t)dst_size)), "");
        switch (dst_type) {
            case INT_SCHAR:
                HDmemcpy(aligned, buf+j*sizeof(signed char), sizeof(signed char));
                printf(" %29d\n", (int)*((signed char*)aligned));
                break;
            case INT_UCHAR:
                HDmemcpy(aligned, buf+j*sizeof(unsigned char), sizeof(unsigned char));
                printf(" %29u\n", (unsigned)*((unsigned char*)aligned));
                break;
            case INT_SHORT:
                HDmemcpy(aligned, buf+j*sizeof(short), sizeof(short));
                printf(" %29hd\n", *((short*)aligned));
                break;
            case INT_USHORT:
                HDmemcpy(aligned, buf+j*sizeof(unsigned short), sizeof(unsigned short));
                printf(" %29hu\n", *((unsigned short*)aligned));
                break;
            case INT_INT:
                HDmemcpy(aligned, buf+j*sizeof(int), sizeof(int));
                printf(" %29d\n", *((int*)aligned));
                break;
            case INT_UINT:
                HDmemcpy(aligned, buf+j*sizeof(unsigned), sizeof(unsigned));
                printf(" %29u\n", *((unsigned*)aligned));
                break;
            case INT_LONG:
                HDmemcpy(aligned, buf+j*sizeof(long), sizeof(long));
                printf(" %29ld\n", *((long*)aligned));
                break;
            case INT_ULONG:
                HDmemcpy(aligned, buf+j*sizeof(unsigned long), sizeof(unsigned long));
                printf(" %29lu\n", *((unsigned long*)aligned));
                break;
            case INT_LLONG:
                HDmemcpy(aligned, buf+j*sizeof(long long), sizeof(long long));
                HDfprintf(stdout," %29"H5_PRINTF_LL_WIDTH"d\n", *((long long*)aligned));
                break;
            case INT_ULLONG:
                HDmemcpy(aligned, buf+j*sizeof(long long), sizeof(unsigned long long));
                HDfprintf(stdout," %29"H5_PRINTF_LL_WIDTH"u\n", *((unsigned long long*)aligned));
                break;
            case FLT_FLOAT:
            case FLT_DOUBLE:
            case FLT_LDOUBLE:
            case OTHER:
            default:
                break;
        }

        printf("        ans = ");
        for (k=0; k<dst_size; k++)
            printf(" %02x", hw[ENDIAN(dst_size, k, endian)]);
        printf("%*s", (int)(3*MAX(0, (ssize_t)src_size-(ssize_t)dst_size)), "");
        switch (dst_type) {
            case INT_SCHAR:
                printf(" %29d\n", (int)*((signed char*)hw));
                break;
            case INT_UCHAR:
                printf(" %29u\n", (unsigned)*((unsigned char*)hw));
                break;
            case INT_SHORT:
                printf(" %29hd\n", *((short*)hw));
                break;
            case INT_USHORT:
                printf(" %29hu\n", *((unsigned short*)hw));
                break;
            case INT_INT:
                printf(" %29d\n", *((int*)hw));
                break;
            case INT_UINT:
                printf(" %29u\n", *((unsigned*)hw));
                break;
            case INT_LONG:
                printf(" %29ld\n", *((long*)hw));
                break;
            case INT_ULONG:
                printf(" %29lu\n", *((unsigned long*)hw));
                break;
            case INT_LLONG:
                HDfprintf(stdout," %29"H5_PRINTF_LL_WIDTH"d\n", *((long long*)hw));
                break;
            case INT_ULLONG:
                HDfprintf(stdout," %29"H5_PRINTF_LL_WIDTH"u\n", *((unsigned long long*)hw));
                break;
            case FLT_FLOAT:
            case FLT_DOUBLE:
            case FLT_LDOUBLE:
            case OTHER:
            default:
                break;
        }

        if (++fails_all_tests>=max_fails) {
            HDputs("    maximum failures reached, aborting test...");
            HDputs("    (dst is library's conversion output. ans is compiler's conversion output.)");
            goto done;
        }
    }
    PASSED();

done:
    if (buf) aligned_free(buf);
    if (saved) aligned_free(saved);
    if (aligned) HDfree(aligned);
    HDfflush(stdout);
    reset_hdf5();	/*print statistics*/
    return (int)fails_all_tests;

error:
    if (buf) aligned_free(buf);
    if (saved) aligned_free(saved);
    if (aligned) HDfree(aligned);
    HDfflush(stdout);
    reset_hdf5();	/*print statistics*/
    return MAX((int)fails_all_tests, 1);
}


/*-------------------------------------------------------------------------
 * Function:	test_conv_int_2
 *
 * Purpose:	Tests overlap calculates in H5T_conv_i_i(), which should be
 *		the same as for H5T_conv_f_f() and H5T_conv_s_s().
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Friday, April 30, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_int_2(void)
{
    int		i, j;
    hid_t	src_type, dst_type;
    char	buf[32*100];

    printf("%-70s", "Testing overlap calculations");
    HDfflush(stdout);

    HDmemset(buf, 0, sizeof buf);
    for (i=1; i<=32; i++) {
	for (j=1; j<=32; j++) {

	    /* Source type */
	    src_type = H5Tcopy(H5T_NATIVE_CHAR);
	    H5Tset_size(src_type, (size_t)i);

	    /* Destination type */
	    dst_type = H5Tcopy(H5T_NATIVE_CHAR);
	    H5Tset_size(dst_type, (size_t)j);

	    /*
	     * Conversion. If overlap calculations aren't right then an
	     * assertion will fail in H5T_conv_i_i()
	     */
	    H5Tconvert(src_type, dst_type, (size_t)100, buf, NULL, H5P_DEFAULT);
	    H5Tclose(src_type);
	    H5Tclose(dst_type);
	}
    }
    PASSED();
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	my_isnan
 *
 * Purpose:	Determines whether VAL points to NaN.
 *
 * Return:	TRUE or FALSE
 *
 * Programmer:	Robb Matzke
 *              Monday, July  6, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
my_isnan(dtype_t type, void *val)
{
    int retval = 0;
    char s[256];

    if (FLT_FLOAT==type) {
	float x;
	HDmemcpy(&x, val, sizeof(float));
	retval = (x!=x);
    } else if (FLT_DOUBLE==type) {
	double x;
	HDmemcpy(&x, val, sizeof(double));
	retval = (x!=x);
#if H5_SIZEOF_LONG_DOUBLE!=H5_SIZEOF_DOUBLE && H5_SIZEOF_LONG_DOUBLE!=0
    } else if (FLT_LDOUBLE==type) {
	long double x;
	HDmemcpy(&x, val, sizeof(long double));
	retval = (x!=x);
#endif
    } else {
	return 0;
    }

    /*
     * Sometimes NaN==NaN (e.g., DEC Alpha) so we try to print it and see if
     * the result contains a NaN string.
     */
    if (!retval) {
	if (FLT_FLOAT==type) {
	    float x;
	    HDmemcpy(&x, val, sizeof(float));
	    sprintf(s, "%g", x);
	} else if (FLT_DOUBLE==type) {
	    double x;
	    HDmemcpy(&x, val, sizeof(double));
	    sprintf(s, "%g", x);
#if H5_SIZEOF_LONG_DOUBLE!=H5_SIZEOF_DOUBLE && H5_SIZEOF_LONG_DOUBLE!=0
	} else if (FLT_LDOUBLE==type) {
	    long double x;
	    HDmemcpy(&x, val, sizeof(long double));
	    sprintf(s, "%Lg", x);
#endif
	} else {
	    return 0;
	}
	 if (HDstrstr(s, "NaN") || HDstrstr(s, "NAN") || HDstrstr(s, "nan"))
	    retval = 1;
    }

#ifdef H5_VMS
    /* For "float" and "double" on OpenVMS/Alpha, NaN is
     * actually a valid value of maximal value.*/
    if(!retval) {
	if (FLT_FLOAT==type) {
	    float x;
	    HDmemcpy(&x, val, sizeof(float));
            retval = (x==FLT_MAX || x==-FLT_MAX);
	} else if (FLT_DOUBLE==type) {
 	    double x;
	    HDmemcpy(&x, val, sizeof(double));
            retval = (x==DBL_MAX || x==-DBL_MAX);
	} else {
	    return 0;
	}
    }
#endif /*H5_VMS*/

    return retval;
}


/*-------------------------------------------------------------------------
 * Function:	my_isinf
 *
 * Purpose:	Determines whether VAL points to +/-infinity.
 *
 * Return:	TRUE or FALSE
 *
 * Programmer:	Raymond Lu
 *              Monday, June 20, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
my_isinf(int endian, unsigned char *val, size_t size,
        size_t mpos, size_t msize, size_t epos, size_t esize)
{
    unsigned char *bits;
    int retval = 0;
    size_t i;

    bits = (unsigned char*)calloc((size_t)1, size);

#ifdef H5_VMS
    if(H5T_ORDER_VAX==endian) {
        for (i = 0; i < size; i += 4) {
            bits[i] = val[(size-2)-i];
            bits[i+1] = val[(size-1)-i];

            bits[(size-2)-i] = val[i];
            bits[(size-1)-i] = val[i+1];
        }
    } else {
        for (i=0; i<size; i++)
            bits[size-(i+1)] = *(val + ENDIAN(size,i,endian));
    }
#else /*H5_VMS*/
    for (i=0; i<size; i++)
        bits[size-(i+1)] = *(val + ENDIAN(size, i, endian));
#endif /*H5_VMS*/

    if(H5T_bit_find(bits, mpos, msize, H5T_BIT_LSB, 1) < 0 &&
            H5T_bit_find(bits, epos, esize, H5T_BIT_LSB, 0) < 0)
        retval = 1;

    free(bits);

    return retval;
}


/*-------------------------------------------------------------------------
 * Function:	test_conv_flt_1
 *
 * Purpose:	Test conversion of floating point values from SRC to
 *		DST.  These types should be H5T_NATIVE_FLOAT,
 *		H5T_NATIVE_DOUBLE, or H5T_NATIVE_LDOUBLE.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Tuesday, June 23, 1998
 *
 * Modifications:
 *	     Albert Cheng, Apr 16, 2004
 *	     Check for underflow condition. If the src number is
 *	     smaller than the dst MIN float number, consider it okay
 *	     if the converted sw and hw dst are both less than or
 *	     equal to the dst MIN float number.
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_flt_1 (const char *name, int run_test, hid_t src, hid_t dst)
{
    dtype_t		src_type, dst_type;	/*data types		*/
    size_t	        nelmts=0;		/*num values per test	*/
    const size_t	max_fails=8;		/*max number of failures*/
    size_t		fails_all_tests=0;	/*number of failures	*/
    size_t		fails_this_test;	/*fails for this test	*/
    const char		*src_type_name = NULL;	/*source type name	*/
    const char		*dst_type_name = NULL;	/*destination type name	*/
    size_t		src_size, dst_size;	/*type sizes		*/
    unsigned char	*buf = NULL;		/*buffer for conversion	*/
    unsigned char	*saved = NULL;		/*original values	*/
    char		str[256];		/*hello string		*/
    void		*aligned=NULL;		/*aligned buffer	*/
    float		hw_f;			/*hardware-converted 	*/
    double		hw_d;			/*hardware-converted	*/
#if H5_SIZEOF_LONG_DOUBLE!=H5_SIZEOF_DOUBLE
    long double		hw_ld;			/*hardware-converted	*/
#endif
    unsigned char	*hw=NULL;		/*ptr to hardware-conv'd*/
    int			underflow;		/*underflow occurred	*/
    int			overflow;		/*overflow occurred	*/
#ifdef H5_VMS
    int			maximal;		/*maximal value occurred, for VMS only.	*/
#endif /* H5_VMS */
    int 		uflow=0;		/*underflow debug counters*/
    size_t		j, k;			/*counters		*/
    int			sendian;		/* source type endianess */
    int			dendian;		/* Destination type endianess */
    size_t		dst_ebias;		/* Destination type's exponent bias */
    size_t		src_epos;		/* Source type's exponent position */
    size_t		src_esize;		/* Source type's exponent size */
    size_t		dst_epos;		/* Destination type's exponent position */
    size_t		dst_esize;		/* Destination type's exponent size */
    size_t		dst_mpos;		/* Destination type's mantissa position */
    size_t		dst_msize;		/* Destination type's mantissa size */
    size_t		src_nbits;		/* source length in bits */
    size_t		dst_nbits;		/* dst length in bits */

#ifdef HANDLE_SIGFPE
    pid_t		child_pid;		/*process ID of child	*/
    int			status;			/*child exit status	*/

    /*
     * Some systems generage SIGFPE during floating point overflow and we
     * cannot assume that we can continue from such a signal.  Therefore, we
     * fork here and let the child run the test and return the number of
     * failures with the exit status.
     */
    HDfflush(stdout);
    HDfflush(stderr);
    if ((child_pid=fork()) < 0) {
	HDperror("fork");
	return 1;
    } else if (child_pid>0) {
	while (child_pid!=waitpid(child_pid, &status, 0)) /*void*/;
	if (WIFEXITED(status) && 255==WEXITSTATUS(status)) {
	    return 0; /*child exit after catching SIGFPE*/
	} else if (WIFEXITED(status)) {
	    return WEXITSTATUS(status);
	} else if (WIFSIGNALED(status)) {
	    sprintf(str, "   Child caught signal %d.", WTERMSIG(status));
	    HDputs(str);
	    return 1; /*child exit after catching non-SIGFPE signal */
	} else {
	    HDputs("   Child didn't exit normally.");
	    return 1;
	}
    }
#endif

    /*
     * The remainder of this function is executed only by the child if
     * HANDLE_SIGFPE is defined.
     */
#ifndef H5_VMS
    HDsignal(SIGFPE,fpe_handler);
#endif

    /* What are the names of the source and destination types */
    if (H5Tequal(src, H5T_NATIVE_FLOAT)) {
	src_type_name = "float";
	src_type = FLT_FLOAT;
    } else if (H5Tequal(src, H5T_NATIVE_DOUBLE)) {
	src_type_name = "double";
	src_type = FLT_DOUBLE;
#if H5_SIZEOF_LONG_DOUBLE!=H5_SIZEOF_DOUBLE && H5_SIZEOF_LONG_DOUBLE!=0
    } else if (H5Tequal(src, H5T_NATIVE_LDOUBLE)) {
	src_type_name = "long double";
	src_type = FLT_LDOUBLE;
#endif
    } else {
	src_type_name = "UNKNOWN";
	src_type = OTHER;
    }

    if (H5Tequal(dst, H5T_NATIVE_FLOAT)) {
	dst_type_name = "float";
	dst_type = FLT_FLOAT;
    } else if (H5Tequal(dst, H5T_NATIVE_DOUBLE)) {
	dst_type_name = "double";
	dst_type = FLT_DOUBLE;
#if H5_SIZEOF_LONG_DOUBLE!=H5_SIZEOF_DOUBLE && H5_SIZEOF_LONG_DOUBLE!=0
    } else if (H5Tequal(dst, H5T_NATIVE_LDOUBLE)) {
	dst_type_name = "long double";
	dst_type = FLT_LDOUBLE;
#endif
    } else {
	dst_type_name = "UNKNOWN";
	dst_type = OTHER;
    }

    /* Sanity checks */
    if(sizeof(float)==sizeof(double))
        HDputs("Sizeof(float)==sizeof(double) - some tests may not be sensible.");
    if (OTHER==src_type || OTHER==dst_type) {
        if(!strcmp(name, "noop"))
	    sprintf(str, "Testing %s %s -> %s conversions",
		name, src_type_name, dst_type_name);
        else if(run_test==TEST_SPECIAL)
	    sprintf(str, "Testing %s special %s -> %s conversions",
		name, src_type_name, dst_type_name);
        else if(run_test==TEST_NORMAL)
	    sprintf(str, "Testing %s normalized %s -> %s conversions",
		name, src_type_name, dst_type_name);
        else if(run_test==TEST_DENORM)
	    sprintf(str, "Testing %s denormalized %s -> %s conversions",
		name, src_type_name, dst_type_name);

	printf("%-70s", str);
	H5_FAILED();
	HDputs("    Unknown data type.");
	goto error;
    } else {
        if(!strcmp(name, "noop"))
            sprintf(str, "Testing %s %s -> %s conversions",
                name, src_type_name, dst_type_name);
        else if(run_test==TEST_SPECIAL)
            sprintf(str, "Testing %s special %s -> %s conversions",
                name, src_type_name, dst_type_name);
        else if(run_test==TEST_NORMAL)
            sprintf(str, "Testing %s normalized %s -> %s conversions",
                name, src_type_name, dst_type_name);
        else if(run_test==TEST_DENORM)
            sprintf(str, "Testing %s denormalized %s -> %s conversions",
                name, src_type_name, dst_type_name);

        printf("%-70s", str);
        HDfflush(stdout);
        fails_this_test = 0;
    }

    /* Get "interesting" values */
    src_size = H5Tget_size(src);
    dst_size = H5Tget_size(dst);
    src_nbits = H5Tget_precision(src); /* not 8*src_size, esp on J90 - QAK */
    dst_nbits = H5Tget_precision(dst); /* not 8*dst_size, esp on J90 - QAK */
    dst_ebias=H5Tget_ebias(dst);
    H5Tget_fields(src,NULL,&src_epos,&src_esize,NULL,NULL);
    H5Tget_fields(dst,NULL,&dst_epos,&dst_esize,&dst_mpos,&dst_msize);
    sendian = H5Tget_order(src);
    dendian = H5Tget_order(dst);

    /* Allocate buffers */
    aligned = HDcalloc((size_t)1, MAX(sizeof(long double), sizeof(double)));

    /* Allocate and initialize the source buffer through macro INIT_FP_NORM or INIT_FP_SPECIAL.
     * The BUF will be used for the conversion while the SAVED buffer will be used for
     * the comparison later.  INIT_FP_NORM will fill in the buffer with regular values like
     * normalized and denormalized values; INIT_FP_SPECIAL will fill with special values
     * like infinity, NaN.
     */
    switch (run_test) {
        case TEST_NOOP:
        case TEST_NORMAL:
#ifdef H5_VMS
            if(src_type == FLT_FLOAT) {
                INIT_FP_NORM(float, FLT_MAX, FLT_MIN, FLT_MAX_10_EXP, FLT_MIN_10_EXP,
                        src_size, dst_size, buf, saved, nelmts);
            } else if(src_type == FLT_DOUBLE && dst_type == FLT_FLOAT) {
                /*Temporary solution for VMS.  Cap double values between maximal and minimal
                 *destination values because VMS return exception when overflows or underflows.
                 *Same below.*/
                INIT_FP_NORM(double, FLT_MAX, FLT_MIN, FLT_MAX_10_EXP, FLT_MIN_10_EXP,
                        src_size, dst_size, buf, saved, nelmts);
            } else if(src_type == FLT_DOUBLE) {
                INIT_FP_NORM(double, DBL_MAX, DBL_MIN, DBL_MAX_10_EXP, DBL_MIN_10_EXP,
                        src_size, dst_size, buf, saved, nelmts);
#if H5_SIZEOF_LONG_DOUBLE!=H5_SIZEOF_DOUBLE && H5_SIZEOF_LONG_DOUBLE!=0
            } else if(src_type == FLT_LDOUBLE && dst_type == FLT_FLOAT) {
                INIT_FP_NORM(long double, FLT_MAX, FLT_MIN, FLT_MAX_10_EXP, FLT_MIN_10_EXP,
                        src_size, dst_size, buf, saved, nelmts);
            } else if(src_type == FLT_LDOUBLE && dst_type == FLT_DOUBLE) {
                INIT_FP_NORM(long double, DBL_MAX, DBL_MIN, DBL_MAX_10_EXP, DBL_MIN_10_EXP,
                        src_size, dst_size, buf, saved, nelmts);
            } else if(src_type == FLT_LDOUBLE) {
                INIT_FP_NORM(long double, LDBL_MAX, LDBL_MIN, LDBL_MAX_10_EXP, LDBL_MIN_10_EXP,
                        src_size, dst_size, buf, saved, nelmts);
#endif
            } else
                goto error;
#else /*H5_VMS*/
            if(src_type == FLT_FLOAT) {
                INIT_FP_NORM(float, FLT_MAX, FLT_MIN, FLT_MAX_10_EXP, FLT_MIN_10_EXP,
                        src_size, dst_size, buf, saved, nelmts);
            } else if(src_type == FLT_DOUBLE) {
                INIT_FP_NORM(double, DBL_MAX, DBL_MIN, DBL_MAX_10_EXP, DBL_MIN_10_EXP,
                        src_size, dst_size, buf, saved, nelmts);
#if H5_SIZEOF_LONG_DOUBLE!=H5_SIZEOF_DOUBLE && H5_SIZEOF_LONG_DOUBLE!=0
            } else if(src_type == FLT_LDOUBLE) {
                INIT_FP_NORM(long double, LDBL_MAX, LDBL_MIN, LDBL_MAX_10_EXP, LDBL_MIN_10_EXP,
                        src_size, dst_size, buf, saved, nelmts);
#endif
            } else
                goto error;
#endif /*H5_VMS*/

            break;
        case TEST_DENORM:
            if(src_type == FLT_FLOAT) {
                INIT_FP_DENORM(float, FLT_MANT_DIG, src_size, src_nbits, sendian, dst_size,
                        buf, saved, nelmts);
            } else if(src_type == FLT_DOUBLE) {
                INIT_FP_DENORM(double, DBL_MANT_DIG, src_size, src_nbits, sendian, dst_size,
                        buf, saved, nelmts);
#if H5_SIZEOF_LONG_DOUBLE!=H5_SIZEOF_DOUBLE && H5_SIZEOF_LONG_DOUBLE!=0
            } else if(src_type == FLT_LDOUBLE) {
                INIT_FP_DENORM(long double, LDBL_MANT_DIG, src_size, src_nbits, sendian, dst_size,
                        buf, saved, nelmts);
#endif
            } else
                goto error;

            break;

        case TEST_SPECIAL:
            if(src_type == FLT_FLOAT) {
                INIT_FP_SPECIAL(src_size, src_nbits, sendian, FLT_MANT_DIG, dst_size,
                        buf, saved, nelmts);
            } else if(src_type == FLT_DOUBLE) {
                 INIT_FP_SPECIAL(src_size, src_nbits, sendian, DBL_MANT_DIG, dst_size,
                        buf, saved, nelmts);
#if H5_SIZEOF_LONG_DOUBLE!=H5_SIZEOF_DOUBLE && H5_SIZEOF_LONG_DOUBLE!=0
            } else if(src_type == FLT_LDOUBLE) {
                 INIT_FP_SPECIAL(src_size, src_nbits, sendian, LDBL_MANT_DIG, dst_size,
                        buf, saved, nelmts);
#endif
            } else
                goto error;

            break;
        default:
            goto error;
    }

    /* Perform the conversion in software */
    if (H5Tconvert(src, dst, nelmts, buf, NULL, H5P_DEFAULT) < 0)
        goto error;

    /* Check the software results against the hardware */
    for (j=0; j<nelmts; j++) {
        underflow = 0;
        hw_f = 911.0;
        hw_d = 911.0;
#if H5_SIZEOF_LONG_DOUBLE!=H5_SIZEOF_DOUBLE
        hw_ld = 911.0;
#endif

        /* The hardware conversion */
        /* Check for underflow when src is a "larger" float than dst.*/
        if (FLT_FLOAT==src_type) {
            HDmemcpy(aligned, saved+j*sizeof(float), sizeof(float));
            if (FLT_FLOAT==dst_type) {
                hw_f = *((float*)aligned);
                hw = (unsigned char*)&hw_f;
            } else if (FLT_DOUBLE==dst_type) {
                hw_d = *((float*)aligned);
                hw = (unsigned char*)&hw_d;
#if H5_SIZEOF_LONG_DOUBLE!=H5_SIZEOF_DOUBLE
            } else {
                hw_ld = *((float*)aligned);
                hw = (unsigned char*)&hw_ld;
#endif
            }
        } else if (FLT_DOUBLE==src_type) {
            HDmemcpy(aligned, saved+j*sizeof(double), sizeof(double));
            if (FLT_FLOAT==dst_type) {
                hw_f = (float)(*((double*)aligned));
                hw = (unsigned char*)&hw_f;
                underflow = HDfabs(*((double*)aligned)) < FLT_MIN;
                overflow = HDfabs(*((double*)aligned)) > FLT_MAX;
#ifdef H5_VMS
                maximal = HDfabs(*((double*)aligned)) == FLT_MAX;
#endif
            } else if (FLT_DOUBLE==dst_type) {
                hw_d = *((double*)aligned);
                hw = (unsigned char*)&hw_d;
#if H5_SIZEOF_LONG_DOUBLE!=H5_SIZEOF_DOUBLE
            } else {
                hw_ld = *((double*)aligned);
                hw = (unsigned char*)&hw_ld;
#endif
            }
#if H5_SIZEOF_LONG_DOUBLE!=H5_SIZEOF_DOUBLE
        } else {
            HDmemcpy(aligned, saved+j*sizeof(long double), sizeof(long double));
            if (FLT_FLOAT==dst_type) {
                hw_f = *((long double*)aligned);
                hw = (unsigned char*)&hw_f;
                underflow = HDfabsl(*((long double*)aligned)) < FLT_MIN;
                overflow = HDfabsl(*((long double*)aligned)) > FLT_MAX;
#ifdef H5_VMS
                maximal = HDfabs(*((long double*)aligned)) == FLT_MAX;
#endif
            } else if (FLT_DOUBLE==dst_type) {
                hw_d = *((long double*)aligned);
                hw = (unsigned char*)&hw_d;
                underflow = HDfabsl(*((long double*)aligned)) < DBL_MIN;
                overflow = HDfabsl(*((long double*)aligned)) > DBL_MAX;
#ifdef H5_VMS
                maximal = HDfabs(*((long double*)aligned)) == DBL_MAX;
#endif
            } else {
                hw_ld = *((long double*)aligned);
                hw = (unsigned char*)&hw_ld;
            }
#endif
        }
        if (underflow){
            uflow++;
        }

        /* For Intel machines, the size of "long double" is 12 bytes, precision
         * is 80 bits; for Intel IA64 and AMD processors, the size of "long double"
         * is 16 bytes, precision is 80 bits.  During hardware conversion, the
         * last few unused bytes may have garbage in them.  Clean them out with
         * 0s before compare the values.
         */
#if H5_SIZEOF_LONG_DOUBLE !=0
        if(sendian==H5T_ORDER_LE && dst_type==FLT_LDOUBLE) {
            unsigned int q;
            for(q=dst_nbits/8; q<dst_size; q++) {
                buf[j*dst_size+q] = 0x00;
                hw[q] = 0x00;
            }
        }
#endif

        /* Are the two results the same? */
        for (k=(dst_size-(dst_nbits/8)); k<dst_size; k++)
            if (buf[j*dst_size+k]!=hw[k])
                break;
        if (k==dst_size)
            continue; /*no error*/

#ifdef H5_VMS
        /* For "float" and "double" on OpenVMS/Alpha, NaN is
         * a valid value of maximal value.*/
        if (FLT_FLOAT==src_type &&
                my_isnan(src_type, saved+j*sizeof(float))) {
            continue;
        } else if (FLT_DOUBLE==src_type &&
                my_isnan(src_type, saved+j*sizeof(double))) {
            continue;
        }
#endif /*H5_VMS*/

        /*
         * Assume same if both results are NaN.  There are many NaN bit
         * patterns and the software doesn't attemt to emulate the
         * hardware in this regard.  Instead, software uses a single bit
         * pattern for NaN by setting the significand to all ones.
         */
        if (FLT_FLOAT==dst_type &&
                my_isnan(dst_type, buf+j*sizeof(float)) &&
                my_isnan(dst_type, hw)) {
            continue;
        } else if (FLT_DOUBLE==dst_type &&
                my_isnan(dst_type, buf+j*sizeof(double)) &&
                my_isnan(dst_type, hw)) {
            continue;
#if H5_SIZEOF_LONG_DOUBLE!=H5_SIZEOF_DOUBLE && H5_SIZEOF_LONG_DOUBLE!=0
        } else if (FLT_LDOUBLE==dst_type &&
                my_isnan(dst_type, buf+j*sizeof(long double)) &&
                my_isnan(dst_type, hw)) {
            continue;
#endif
        }

        /*
         * Assume same if hardware result is NaN.  This is because the
         * hardware conversions on some machines return NaN instead of
         * overflowing to +Inf or -Inf or underflowing to +0 or -0.
         */
        if (my_isnan(dst_type, hw))
            continue;

        /*
         * Instead of matching down to the bit, just make sure the
         * exponents are the same and the mantissa is the same to a
         * certain precision.  This is needed on machines that don't
         * round as expected.
         * If the src number is smaller than the dst MIN float number,
         * consider it okay if the converted sw and hw dst are both
         * less than or equal to the dst MIN float number.
         * If overflow happens when the src value is greater than
         * the maximum dst value, the library assign INFINITY to dst.
         * This might be different from what the compiler does, i.e.
         * the SGI compiler assigns the dst's maximal value.
         */
        {
            double		check_mant[2];
            int		check_expo[2];

            if (FLT_FLOAT==dst_type) {
                float x;
                HDmemcpy(&x, &buf[j*dst_size], sizeof(float));
                if (underflow &&
                        HDfabsf(x) <= FLT_MIN && HDfabsf(hw_f) <= FLT_MIN)
                    continue;	/* all underflowed, no error */
                if (overflow && my_isinf(dendian, buf+j*sizeof(float),
                        dst_size, dst_mpos, dst_msize, dst_epos, dst_esize))
                    continue;	/* all overflowed, no error */
#ifdef H5_VMS
                if (maximal && my_isinf(dendian, buf+j*sizeof(float),
                        dst_size, dst_mpos, dst_msize, dst_epos, dst_esize))
                    continue;	/* maximal value, no error */
#endif /*H5_VMS*/
                check_mant[0] = HDfrexpf(x, check_expo+0);
                check_mant[1] = HDfrexpf(hw_f, check_expo+1);
            } else if (FLT_DOUBLE==dst_type) {
                double x;
                HDmemcpy(&x, &buf[j*dst_size], sizeof(double));
                if (underflow &&
                        HDfabs(x) <= DBL_MIN && HDfabs(hw_d) <= DBL_MIN)
                    continue;	/* all underflowed, no error */
                if (overflow && my_isinf(dendian, buf+j*sizeof(double),
                        dst_size, dst_mpos, dst_msize, dst_epos, dst_esize))
                    continue;	/* all overflowed, no error */
#ifdef H5_VMS
                if (maximal && my_isinf(dendian, buf+j*sizeof(double),
                        dst_size, dst_mpos, dst_msize, dst_epos, dst_esize))
                    continue;	/* maximal value, no error */
#endif /*H5_VMS*/
                check_mant[0] = HDfrexp(x, check_expo+0);
                check_mant[1] = HDfrexp(hw_d, check_expo+1);
#if H5_SIZEOF_LONG_DOUBLE !=0 && (H5_SIZEOF_LONG_DOUBLE!=H5_SIZEOF_DOUBLE)
            } else {
                long double x;
                HDmemcpy(&x, &buf[j*dst_size], sizeof(long double));
                /* dst is largest float, no need to check underflow. */
                check_mant[0] = HDfrexpl(x, check_expo+0);
                check_mant[1] = HDfrexpl(hw_ld, check_expo+1);
#endif
            }
#ifdef H5_CONVERT_DENORMAL_FLOAT
            /* Special check for denormalized values */
            if(check_expo[0]<(-(int)dst_ebias) || check_expo[1]<(-(int)dst_ebias)) {
                int expo_diff=check_expo[0]-check_expo[1];
                int valid_bits=(int)((dst_ebias+dst_msize)+MIN(check_expo[0],check_expo[1]))-1;
                double epsilon=1.0;

                /* Re-scale the mantissas based on any exponent difference */
                if(expo_diff!=0)
                    check_mant[0] = HDldexp(check_mant[0],expo_diff);

                /* Compute the proper epsilon */
                epsilon=HDldexp(epsilon,-valid_bits);

                /* Check for "close enough" fit with scaled epsilon value */
                if (HDfabs(check_mant[0]-check_mant[1])<=epsilon)
                    continue;
            } /* end if */
            else {
                if (check_expo[0]==check_expo[1] &&
                        HDfabs(check_mant[0]-check_mant[1])<FP_EPSILON)
                    continue;
            } /* end else */
#else /* H5_CONVERT_DENORMAL_FLOAT */
            {
            hssize_t	expo;			/*exponent			*/
            uint8_t tmp[32];

            assert(src_size<=sizeof(tmp));
            if(sendian==H5T_ORDER_LE)
                HDmemcpy(tmp,&saved[j*src_size],src_size);
            else if(sendian==H5T_ORDER_BE)
                for (k=0; k<src_size; k++)
                    tmp[k]=saved[j*src_size+(src_size-(k+1))];
            else {
                for (k = 0; k < src_size; k += 4) {
                    tmp[k] = saved[j*src_size+(src_size-2)-k];
                    tmp[k+1] = saved[j*src_size+(src_size-1)-k];

                    tmp[(src_size-2)-k] = saved[j*src_size+k];
                    tmp[(src_size-1)-k] = saved[j*src_size+k+1];
                }
            }

            expo = H5T_bit_get_d(tmp, src_epos, src_esize);
            if(expo==0)
                continue;   /* Denormalized floating-point value detected */
            else {
                assert(dst_size<=sizeof(tmp));
                if(sendian==H5T_ORDER_LE)
                    HDmemcpy(tmp,&buf[j*dst_size],dst_size);
                else if(sendian==H5T_ORDER_BE)
                    for (k=0; k<dst_size; k++)
                        tmp[k]=buf[j*dst_size+(dst_size-(k+1))];
                else {
                    for (k = 0; k < src_size; k += 4) {
                        tmp[k] = buf[j*dst_size+(dst_size-2)-k];
                        tmp[k+1] = buf[j*dst_size+(dst_size-1)-k];

                        tmp[(dst_size-2)-k] = buf[j*dst_size+k];
                        tmp[(dst_size-1)-k] = buf[j*dst_size+k+1];
                    }
                }

                expo = H5T_bit_get_d(tmp, dst_epos, dst_esize);
                if(expo==0)
                    continue;   /* Denormalized floating-point value detected */
                else {
                    if (check_expo[0]==check_expo[1] &&
                            HDfabs(check_mant[0]-check_mant[1])<FP_EPSILON)
                        continue;
                } /* end else */
            } /* end else */
            }
#endif /* H5_CONVERT_DENORMAL_FLOAT */
        }

        if (0==fails_this_test++) {
            if(run_test==TEST_NOOP || run_test==TEST_NORMAL) {
                H5_FAILED();
            } else if(run_test==TEST_DENORM || run_test==TEST_SPECIAL) {
                H5_WARNING();
            }
        }
        printf("    elmt %u\n", (unsigned)j);

        printf("        src =");
        for (k=0; k<src_size; k++)
            printf(" %02x", saved[j*src_size+ENDIAN(src_size,k,sendian)]);
        printf("%*s", (int)(3*MAX(0, (ssize_t)dst_size-(ssize_t)src_size)), "");
        if (FLT_FLOAT==src_type) {
            float x;
            HDmemcpy(&x, &saved[j*src_size], sizeof(float));
            printf(" %29.20e\n", x);
        } else if (FLT_DOUBLE==src_type) {
            double x;
            HDmemcpy(&x, &saved[j*src_size], sizeof(double));
            printf(" %29.20e\n", x);
#if H5_SIZEOF_LONG_DOUBLE!=H5_SIZEOF_DOUBLE
        } else {
            long double x;
            HDmemcpy(&x, &saved[j*src_size], sizeof(long double));
            HDfprintf(stdout," %29.20Le\n", x);
#endif
        }

        printf("        dst =");
        for (k=0; k<dst_size; k++)
            printf(" %02x", buf[j*dst_size+ENDIAN(dst_size,k,dendian)]);
        printf("%*s", (int)(3*MAX(0, (ssize_t)src_size-(ssize_t)dst_size)), "");
        if (FLT_FLOAT==dst_type) {
            float x;
            HDmemcpy(&x, &buf[j*dst_size], sizeof(float));
            printf(" %29.20e\n", x);
        } else if (FLT_DOUBLE==dst_type) {
            double x;
            HDmemcpy(&x, &buf[j*dst_size], sizeof(double));
            printf(" %29.20e\n", x);
#if H5_SIZEOF_LONG_DOUBLE!=H5_SIZEOF_DOUBLE
        } else {
            long double x;
            HDmemcpy(&x, &buf[j*dst_size], sizeof(long double));
            HDfprintf(stdout," %29.20Le\n", x);
#endif
        }

        printf("        ans =");
        for (k=0; k<dst_size; k++)
            printf(" %02x", hw[ENDIAN(dst_size,k,dendian)]);
        printf("%*s", (int)(3*MAX(0, (ssize_t)src_size-(ssize_t)dst_size)), "");
        if (FLT_FLOAT==dst_type)
            printf(" %29.20e\n", hw_f);
        else if (FLT_DOUBLE==dst_type)
            printf(" %29.20e\n", hw_d);
#if H5_SIZEOF_LONG_DOUBLE!=H5_SIZEOF_DOUBLE
        else
            HDfprintf(stdout," %29.20Le\n", hw_ld);
#endif

        /* If the source is normalized values, print out error message; if it is
         * denormalized or special values, print out warning message.*/
        if (++fails_all_tests>=max_fails) {
            if(run_test==TEST_NORMAL)
                HDputs("    maximum failures reached, aborting test...");
            else if(run_test==TEST_DENORM || run_test==TEST_SPECIAL)
                HDputs("    maximum warnings reached, aborting test...");
            HDputs("    (dst is library's conversion output. ans is compiler's conversion output.)");

            goto done;
        }
    }

    if(!fails_all_tests)
        PASSED();

done:
    if (buf) aligned_free(buf);
    if (saved) aligned_free(saved);
    if (aligned) HDfree(aligned);
    HDfflush(stdout);
#ifdef HANDLE_SIGFPE
    if(run_test==TEST_NOOP || run_test==TEST_NORMAL)
        HDexit(MIN((int)fails_all_tests, 254));
    else if(run_test==TEST_DENORM || run_test==TEST_SPECIAL)
        HDexit(0);
#else
    reset_hdf5();

    /* If the source is normalized values, treat the failures as error;
     * if it is denormalized or special values, treat the failure as warning.*/
    if(run_test==TEST_NOOP || run_test==TEST_NORMAL)
        return (int)fails_all_tests;
    else if(run_test==TEST_DENORM || run_test==TEST_SPECIAL)
        return 0;
#endif

error:
    if (buf) aligned_free(buf);
    if (saved) aligned_free(saved);
    if (aligned) HDfree(aligned);
    HDfflush(stdout);
#ifdef HANDLE_SIGFPE
    if(run_test==TEST_NOOP || run_test==TEST_NORMAL)
        HDexit(MIN(MAX((int)fails_all_tests, 1), 254));
    else if(run_test==TEST_DENORM || run_test==TEST_SPECIAL)
        HDexit(1);
#else
    reset_hdf5();
    if(run_test==TEST_NOOP || run_test==TEST_NORMAL)
        return MAX((int)fails_all_tests, 1);
    else if(run_test==TEST_DENORM || run_test==TEST_SPECIAL)
        return 1;
#endif
}


/*-------------------------------------------------------------------------
 * Function:	test_conv_int_fp
 *
 * Purpose:	Test conversion between integer and float values
 *              from SRC to DST.  These types should be any combination of:
 *
 * 			H5T_NATIVE_SCHAR	H5T_NATIVE_FLOAT
 *			H5T_NATIVE_SHORT	H5T_NATIVE_DOUBLE
 *			H5T_NATIVE_INT		H5T_NATIVE_LDOUBLE
 *			H5T_NATIVE_LONG
 *			H5T_NATIVE_LLONG
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Raymond Lu
 *              Thursday, November 6, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_int_fp(const char *name, int run_test, hid_t src, hid_t dst)
{
    hid_t               dxpl_id;                /*dataset transfer property list*/
    int                 fill_value=9;           /*fill value for conversion exception*/
    H5T_conv_except_func_t   op;                /*returned callback function for conversion exception*/
    void                *user_data;             /*returned pointer to user data passed in to the callback*/
    hbool_t             except_set = FALSE;     /*whether user's exception handling is set*/
    size_t	        nelmts=0;	        /*num values per test	*/
    const size_t	max_fails=40;		/*max number of failures*/
    size_t		fails_all_tests=0;	/*number of failures	*/
    size_t		fails_this_test;	/*fails for this test	*/
    char		str[256];		/*hello string		*/
    dtype_t		src_type;	        /*data types		*/
    dtype_t		dst_type;	        /*data types		*/
    const char		*src_type_name=NULL;	/*source type name	*/
    const char		*dst_type_name=NULL;	/*destination type name	*/
    int			sendian;		/*source endianess	*/
    int			dendian;		/*destination endianess	*/
    size_t		src_size, dst_size;	/*type sizes		*/
    unsigned char	*buf=NULL;		/*buffer for conversion	*/
    unsigned char	*saved=NULL;		/*original values	*/
    size_t		j, k;			/*counters		*/
    unsigned char 	*hw=NULL;		/*hardware conv result	*/
    unsigned char	src_bits[32];		/*src value in LE order	*/
    unsigned char	dst_bits[32];		/*dest value in LE order*/
    size_t		src_nbits;		/*source length in bits	*/
    size_t		dst_nbits;		/*dst length in bits	*/
    void		*aligned=NULL;		/*aligned temp buffer	*/
    float		hw_float=0;
    double	        hw_double=0;
    long double		hw_ldouble=0;
    signed char		hw_schar=0;
    unsigned char	hw_uchar=0;
    short		hw_short=0;
    unsigned short	hw_ushort=0;
    int			hw_int=0;
    unsigned		hw_uint=0;
    long		hw_long=0;
    unsigned long	hw_ulong=0;
    long long		hw_llong=0;
    unsigned long long	hw_ullong=0;

    /* What is the name of the source type */
    if (H5Tequal(src, H5T_NATIVE_SCHAR)) {
	src_type_name = "signed char";
	src_type = INT_SCHAR;
    } else if (H5Tequal(src, H5T_NATIVE_UCHAR)) {
	src_type_name = "unsigned char";
	src_type = INT_UCHAR;
    } else if (H5Tequal(src, H5T_NATIVE_SHORT)) {
	src_type_name = "short";
	src_type = INT_SHORT;
    } else if (H5Tequal(src, H5T_NATIVE_USHORT)) {
	src_type_name = "unsigned short";
	src_type = INT_USHORT;
    } else if (H5Tequal(src, H5T_NATIVE_INT)) {
	src_type_name = "int";
	src_type = INT_INT;
    } else if (H5Tequal(src, H5T_NATIVE_UINT)) {
	src_type_name = "unsigned int";
	src_type = INT_UINT;
    } else if (H5Tequal(src, H5T_NATIVE_LONG)) {
	src_type_name = "long";
	src_type = INT_LONG;
    } else if (H5Tequal(src, H5T_NATIVE_ULONG)) {
	src_type_name = "unsigned long";
	src_type = INT_ULONG;
    } else if (H5Tequal(src, H5T_NATIVE_LLONG)) {
	src_type_name = "long long";
	src_type = INT_LLONG;
    } else if (H5Tequal(src, H5T_NATIVE_ULLONG)) {
	src_type_name = "unsigned long long";
	src_type = INT_ULLONG;
    } else if (H5Tequal(src, H5T_NATIVE_FLOAT)) {
	src_type_name = "float";
	src_type = FLT_FLOAT;
    } else if (H5Tequal(src, H5T_NATIVE_DOUBLE)) {
	src_type_name = "double";
	src_type = FLT_DOUBLE;
#if H5_SIZEOF_LONG_DOUBLE!=H5_SIZEOF_DOUBLE && H5_SIZEOF_LONG_DOUBLE!=0
    } else if (H5Tequal(src, H5T_NATIVE_LDOUBLE)) {
	src_type_name = "long double";
	src_type = FLT_LDOUBLE;
#endif
    } else {
	src_type_name = "UNKNOWN";
	src_type = OTHER;
    }

    /* What is the name of the destination type */
    if (H5Tequal(dst, H5T_NATIVE_SCHAR)) {
	dst_type_name = "signed char";
	dst_type = INT_SCHAR;
    } else if (H5Tequal(dst, H5T_NATIVE_UCHAR)) {
	dst_type_name = "unsigned char";
	dst_type = INT_UCHAR;
    } else if (H5Tequal(dst, H5T_NATIVE_SHORT)) {
	dst_type_name = "short";
	dst_type = INT_SHORT;
    } else if (H5Tequal(dst, H5T_NATIVE_USHORT)) {
	dst_type_name = "unsigned short";
	dst_type = INT_USHORT;
    } else if (H5Tequal(dst, H5T_NATIVE_INT)) {
	dst_type_name = "int";
	dst_type = INT_INT;
    } else if (H5Tequal(dst, H5T_NATIVE_UINT)) {
	dst_type_name = "unsigned int";
	dst_type = INT_UINT;
    } else if (H5Tequal(dst, H5T_NATIVE_LONG)) {
	dst_type_name = "long";
	dst_type = INT_LONG;
    } else if (H5Tequal(dst, H5T_NATIVE_ULONG)) {
	dst_type_name = "unsigned long";
	dst_type = INT_ULONG;
    } else if (H5Tequal(dst, H5T_NATIVE_LLONG)) {
	dst_type_name = "long long";
	dst_type = INT_LLONG;
    } else if (H5Tequal(dst, H5T_NATIVE_ULLONG)) {
	dst_type_name = "unsigned long long";
	dst_type = INT_ULLONG;
    } else if (H5Tequal(dst, H5T_NATIVE_FLOAT)) {
	dst_type_name = "float";
	dst_type = FLT_FLOAT;
    } else if (H5Tequal(dst, H5T_NATIVE_DOUBLE)) {
	dst_type_name = "double";
	dst_type = FLT_DOUBLE;
#if H5_SIZEOF_LONG_DOUBLE!=H5_SIZEOF_DOUBLE && H5_SIZEOF_LONG_DOUBLE!=0
    } else if (H5Tequal(dst, H5T_NATIVE_LDOUBLE)) {
	dst_type_name = "long double";
	dst_type = FLT_LDOUBLE;
#endif
    } else {
	dst_type_name = "UNKNOWN";
	dst_type = OTHER;
    }

    /* Sanity checks */
    if (OTHER==src_type || OTHER==dst_type) {
	sprintf(str, "Testing %s %s -> %s conversions",
		name, src_type_name, dst_type_name);
	printf("%-70s", str);
	H5_FAILED();
	HDputs("    Unknown data type.");
	goto error;
    }

    if ((INT_SCHAR==src_type || INT_UCHAR==src_type || INT_SHORT==src_type ||
        INT_USHORT==src_type || INT_INT==src_type || INT_UINT==src_type ||
        INT_LONG==src_type || INT_ULONG==src_type || INT_LLONG==src_type ||
        INT_ULLONG==src_type) &&
        (FLT_FLOAT!=dst_type && FLT_DOUBLE!=dst_type
#if H5_SIZEOF_LONG_DOUBLE !=0
        && FLT_LDOUBLE!=dst_type
#endif
        )) {
	sprintf(str, "Testing %s %s -> %s conversions",
	    name, src_type_name, dst_type_name);
	printf("%-70s", str);
	H5_FAILED();
	HDputs("    1. Not an integer-float conversion.");
	goto error;
    }

    if ((FLT_FLOAT==src_type || FLT_DOUBLE==src_type
#if H5_SIZEOF_LONG_DOUBLE !=0
        || FLT_LDOUBLE==src_type
#endif
        )
        && (INT_SCHAR!=dst_type && INT_UCHAR!=dst_type && INT_SHORT!=dst_type
        && INT_USHORT!=dst_type && INT_INT!=dst_type && INT_UINT!=dst_type
        && INT_LONG!=dst_type && INT_ULONG!=dst_type && INT_LLONG!=dst_type
        && INT_ULLONG!=dst_type)) {
 	sprintf(str, "Testing %s %s -> %s conversions",
	    name, src_type_name, dst_type_name);
	printf("%-70s", str);
	H5_FAILED();
	HDputs("    2. Not a float-integer conversion.");
	goto error;
    }

    if (INT_SCHAR==src_type || INT_UCHAR==src_type || INT_SHORT==src_type ||
        INT_USHORT==src_type || INT_INT==src_type || INT_UINT==src_type ||
        INT_LONG==src_type || INT_ULONG==src_type || INT_LLONG==src_type ||
        INT_ULLONG==src_type) {
        sprintf(str, "Testing %s %s -> %s conversions",
            name, src_type_name, dst_type_name);
        printf("%-70s", str);
        HDfflush(stdout);
        fails_this_test=0;
    } else {
        if(run_test==TEST_NORMAL)
            sprintf(str, "Testing %s normalized %s -> %s conversions",
                    name, src_type_name, dst_type_name);
        else if(run_test==TEST_DENORM)
            sprintf(str, "Testing %s denormalized %s -> %s conversions",
                    name, src_type_name, dst_type_name);
        else
            sprintf(str, "Testing %s special %s -> %s conversions",
                    name, src_type_name, dst_type_name);
        printf("%-70s", str);
        HDfflush(stdout);
        fails_this_test=0;
    }

    /* Some information about datatypes */
    sendian = H5Tget_order(src);
    dendian = H5Tget_order(dst);
    src_size = H5Tget_size(src);
    dst_size = H5Tget_size(dst);
    src_nbits = H5Tget_precision(src); /* not 8*src_size, esp on J90 - QAK */
    dst_nbits = H5Tget_precision(dst); /* not 8*dst_size, esp on J90 - QAK */
    aligned = HDcalloc((size_t)1, MAX(sizeof(long double), sizeof(long long)));
#ifdef SHOW_OVERFLOWS
    noverflows_g = 0;
#endif

    /* This is for some Linux systems where long double has the size
     * 12 bytes but precision is 10 bytes.  The 2 unused bytes may
     * have garbage causing wrong value comparison.
     */
    HDmemset(&hw_ldouble, 0, sizeof(long double));

    /* Create a dataset transfer property list and datatype conversion
     * exception handler function and pass in fill value.  This is mainly
     * for NetCDF compatibility, which requests fill in fill value when
     * conversion exception happens.  We only test (unsigned) int - float
     * and float - (unsigned) int conversions, which should cover more cases.
     */
    if((dxpl_id = H5Pcreate(H5P_DATASET_XFER)) < 0)
        goto error;

    if((src_type == INT_INT && dst_type == FLT_FLOAT) ||
            (src_type == INT_UINT && dst_type == FLT_FLOAT) ||
            (src_type == FLT_FLOAT && dst_type == INT_UINT) ||
            (src_type == FLT_FLOAT && dst_type == INT_INT)) {
        if(H5Pset_type_conv_cb(dxpl_id, except_func, &fill_value) < 0)
            goto error;
        else
            except_set = TRUE;

        if(H5Pget_type_conv_cb(dxpl_id, &op, &user_data) < 0)
            goto error;

        if(op != except_func || *(int*)user_data != fill_value)
            goto error;
    }

    /* Allocate and initialize the source buffer through macro INIT_INTEGER if the source is integer,
     * INIT_FP_NORM if floating-point.  The BUF will be used for the conversion while the SAVED buffer will be
     * used for the comparison later.
     */
    if(src_type == INT_SCHAR) {
        INIT_INTEGER(signed char, SCHAR_MAX, SCHAR_MIN, src_size, dst_size, src_nbits, buf, saved, nelmts);
    } else if(src_type == INT_UCHAR) {
        INIT_INTEGER(unsigned char, UCHAR_MAX, 0, src_size, dst_size, src_nbits, buf, saved, nelmts);
    } else if(src_type == INT_SHORT) {
        INIT_INTEGER(short, SHRT_MAX, SHRT_MIN, src_size, dst_size, src_nbits, buf, saved, nelmts);
    } else if(src_type == INT_USHORT) {
        INIT_INTEGER(unsigned short, USHRT_MAX, 0, src_size, dst_size, src_nbits, buf, saved, nelmts);
    } else if(src_type == INT_INT) {
        INIT_INTEGER(int, INT_MAX, INT_MIN, src_size, dst_size, src_nbits, buf, saved, nelmts);
    } else if(src_type == INT_UINT) {
        INIT_INTEGER(unsigned int, UINT_MAX, 0, src_size, dst_size, src_nbits, buf, saved, nelmts);
    } else if(src_type == INT_LONG) {
        INIT_INTEGER(long, LONG_MAX, LONG_MIN, src_size, dst_size, src_nbits, buf, saved, nelmts);
    } else if(src_type == INT_ULONG) {
        INIT_INTEGER(unsigned long, ULONG_MAX, 0, src_size, dst_size, src_nbits, buf, saved, nelmts);
    } else if(src_type == INT_LLONG) {
        INIT_INTEGER(long long, LLONG_MAX, LLONG_MIN, src_size, dst_size, src_nbits, buf, saved, nelmts);
    } else if(src_type == INT_ULLONG) {
        INIT_INTEGER(unsigned long long, ULLONG_MAX, 0, src_size, dst_size, src_nbits, buf, saved, nelmts);
    } else if(src_type == FLT_FLOAT) {
        if(run_test==TEST_NORMAL) {
            INIT_FP_NORM(float, FLT_MAX, FLT_MIN, FLT_MAX_10_EXP, FLT_MIN_10_EXP,
                    src_size, dst_size, buf, saved, nelmts);
        } else if(run_test==TEST_DENORM) {
            INIT_FP_DENORM(float, FLT_MANT_DIG, src_size, src_nbits, sendian, dst_size,
                    buf, saved, nelmts);
        } else {
            INIT_FP_SPECIAL(src_size, src_nbits, sendian, FLT_MANT_DIG, dst_size, buf, saved, nelmts);
        }
    } else if(src_type == FLT_DOUBLE) {
        if(run_test==TEST_NORMAL) {
            INIT_FP_NORM(double, DBL_MAX, DBL_MIN, DBL_MAX_10_EXP, DBL_MIN_10_EXP,
                    src_size, dst_size, buf, saved, nelmts);
        } else if(run_test==TEST_DENORM) {
            INIT_FP_DENORM(double, DBL_MANT_DIG, src_size, src_nbits, sendian, dst_size,
                    buf, saved, nelmts);
        } else {
            INIT_FP_SPECIAL(src_size, src_nbits, sendian, DBL_MANT_DIG, dst_size, buf, saved, nelmts);
        }
#if H5_SIZEOF_LONG_DOUBLE!=H5_SIZEOF_DOUBLE && H5_SIZEOF_LONG_DOUBLE!=0
    } else if(src_type == FLT_LDOUBLE) {
        if(run_test==TEST_NORMAL) {
            INIT_FP_NORM(long double, LDBL_MAX, LDBL_MIN, LDBL_MAX_10_EXP, LDBL_MIN_10_EXP,
                    src_size, dst_size, buf, saved, nelmts);
        } else if(run_test==TEST_DENORM) {
            INIT_FP_DENORM(long double, LDBL_MANT_DIG, src_size, src_nbits, sendian, dst_size,
                    buf, saved, nelmts);
        } else {
            INIT_FP_SPECIAL(src_size, src_nbits, sendian, LDBL_MANT_DIG, dst_size, buf, saved, nelmts);
        }
#endif
    } else
        goto error;

    /* Perform the conversion */
    if(H5Tconvert(src, dst, nelmts, buf, NULL, dxpl_id) < 0)
        goto error;

    /* Check the results from the library against hardware */
    for (j=0; j<nelmts; j++) {
        if(FLT_FLOAT==src_type || FLT_DOUBLE==src_type
#if H5_SIZEOF_LONG_DOUBLE !=0
            || FLT_LDOUBLE==src_type
#endif
            )
            if(my_isnan(src_type, saved+j*src_size))
                continue;

        if (FLT_FLOAT==dst_type) {
            hw = (unsigned char*)&hw_float;
            switch (src_type) {
            case INT_SCHAR:
                HDmemcpy(aligned, saved+j*sizeof(signed char), sizeof(signed char));
                hw_float = (float)(*((signed char*)aligned));
                break;
            case INT_UCHAR:
                HDmemcpy(aligned, saved+j*sizeof(unsigned char), sizeof(unsigned char));
                hw_float = (float)(*((unsigned char*)aligned));
                break;
            case INT_SHORT:
                HDmemcpy(aligned, saved+j*sizeof(short), sizeof(short));
                hw_float = (float)(*((short*)aligned));
                break;
            case INT_USHORT:
                HDmemcpy(aligned, saved+j*sizeof(unsigned short), sizeof(unsigned short));
                hw_float = (float)(*((unsigned short*)aligned));
                break;
            case INT_INT:
                HDmemcpy(aligned, saved+j*sizeof(int), sizeof(int));
                hw_float = (float)(*((int*)aligned));
                break;
            case INT_UINT:
                HDmemcpy(aligned, saved+j*sizeof(unsigned), sizeof(unsigned));
                hw_float = (float)(*((unsigned*)aligned));
                break;
            case INT_LONG:
                HDmemcpy(aligned, saved+j*sizeof(long), sizeof(long));
                hw_float = (float)(*((long*)aligned));
                break;
            case INT_ULONG:
                HDmemcpy(aligned, saved+j*sizeof(unsigned long), sizeof(unsigned long));
                hw_float = (float)(*((unsigned long*)aligned));
                break;
            case INT_LLONG:
                HDmemcpy(aligned, saved+j*sizeof(long long), sizeof(long long));
                hw_float = (float)(*((long long*)aligned));
                break;
#ifdef H5_ULLONG_TO_FP_CAST_WORKS
            case INT_ULLONG:
                HDmemcpy(aligned, saved+j*sizeof(unsigned long long), sizeof(unsigned long long));
                hw_float = (float)(*((unsigned long long*)aligned));
                break;
#endif /* H5_ULLONG_TO_FP_CAST_WORKS */
            case FLT_FLOAT:
            case FLT_DOUBLE:
            case FLT_LDOUBLE:
            case OTHER:
            default:
                break;
            }
        } else if (FLT_DOUBLE==dst_type) {
            hw = (unsigned char*)&hw_double;
            switch (src_type) {
            case INT_SCHAR:
                HDmemcpy(aligned, saved+j*sizeof(signed char), sizeof(signed char));
                hw_double = (double)(*((signed char*)aligned));
                break;
            case INT_UCHAR:
                HDmemcpy(aligned, saved+j*sizeof(unsigned char), sizeof(unsigned char));
                hw_double = (double)(*((unsigned char*)aligned));
                break;
            case INT_SHORT:
                HDmemcpy(aligned, saved+j*sizeof(short), sizeof(short));
                hw_double = (double)(*((short*)aligned));
                break;
            case INT_USHORT:
                HDmemcpy(aligned, saved+j*sizeof(unsigned short), sizeof(unsigned short));
                hw_double = (double)(*((unsigned short*)aligned));
                break;
            case INT_INT:
                HDmemcpy(aligned, saved+j*sizeof(int), sizeof(int));
                hw_double = (double)(*((int*)aligned));
                break;
            case INT_UINT:
                HDmemcpy(aligned, saved+j*sizeof(unsigned), sizeof(unsigned));
                hw_double = (double)(*((unsigned*)aligned));
                break;
            case INT_LONG:
                HDmemcpy(aligned, saved+j*sizeof(long), sizeof(long));
                hw_double = (double)(*((long*)aligned));
                break;
            case INT_ULONG:
                HDmemcpy(aligned, saved+j*sizeof(unsigned long), sizeof(unsigned long));
                hw_double = (double)(*((unsigned long*)aligned));
                break;
            case INT_LLONG:
                HDmemcpy(aligned, saved+j*sizeof(long long), sizeof(long long));
                hw_double = (double)(*((long long*)aligned));
                break;
#ifdef H5_ULLONG_TO_FP_CAST_WORKS
            case INT_ULLONG:
                HDmemcpy(aligned, saved+j*sizeof(unsigned long long), sizeof(unsigned long long));
                hw_double = (double)(*((unsigned long long*)aligned));
                break;
#endif /* H5_ULLONG_TO_FP_CAST_WORKS */
            case FLT_FLOAT:
            case FLT_DOUBLE:
            case FLT_LDOUBLE:
            case OTHER:
            default:
                break;
            }
#if H5_SIZEOF_LONG_DOUBLE !=0
        } else if (FLT_LDOUBLE==dst_type) {
            hw = (unsigned char*)&hw_ldouble;
            switch (src_type) {
            case INT_SCHAR:
                HDmemcpy(aligned, saved+j*sizeof(signed char), sizeof(signed char));
                hw_ldouble = (long double)(*((signed char*)aligned));
                break;
            case INT_UCHAR:
                HDmemcpy(aligned, saved+j*sizeof(unsigned char), sizeof(unsigned char));
                hw_ldouble = (long double)(*((unsigned char*)aligned));
                break;
            case INT_SHORT:
                HDmemcpy(aligned, saved+j*sizeof(short), sizeof(short));
                hw_ldouble = (long double)(*((short*)aligned));
                break;
            case INT_USHORT:
                HDmemcpy(aligned, saved+j*sizeof(unsigned short), sizeof(unsigned short));
                hw_ldouble = (long double)(*((unsigned short*)aligned));
                break;
            case INT_INT:
                HDmemcpy(aligned, saved+j*sizeof(int), sizeof(int));
                hw_ldouble = (long double)(*((int*)aligned));
                break;
            case INT_UINT:
                HDmemcpy(aligned, saved+j*sizeof(unsigned), sizeof(unsigned));
                hw_ldouble = (long double)(*((unsigned*)aligned));
                break;
            case INT_LONG:
                HDmemcpy(aligned, saved+j*sizeof(long), sizeof(long));
                hw_ldouble = (long double)(*((long*)aligned));
                break;
            case INT_ULONG:
                HDmemcpy(aligned, saved+j*sizeof(unsigned long), sizeof(unsigned long));
                hw_ldouble = (long double)(*((unsigned long*)aligned));
                break;
            case INT_LLONG:
                HDmemcpy(aligned, saved+j*sizeof(long long), sizeof(long long));
                hw_ldouble = (long double)(*((long long*)aligned));
                break;
#ifdef H5_ULLONG_TO_FP_CAST_WORKS
            case INT_ULLONG:
                HDmemcpy(aligned, saved+j*sizeof(unsigned long long), sizeof(unsigned long long));
                hw_ldouble = (long double)(*((unsigned long long*)aligned));
                break;
#endif /* H5_ULLONG_TO_FP_CAST_WORKS */
            case FLT_FLOAT:
            case FLT_DOUBLE:
            case FLT_LDOUBLE:
            case OTHER:
            default:
                break;
            }
#endif
        } else if (INT_SCHAR==dst_type) {
            hw = (unsigned char*)&hw_schar;
            switch (src_type) {
            case FLT_FLOAT:
                HDmemcpy(aligned, saved+j*sizeof(float), sizeof(float));
                hw_schar = (signed char)(*((float*)aligned));
                break;
            case FLT_DOUBLE:
                HDmemcpy(aligned, saved+j*sizeof(double), sizeof(double));
                hw_schar = (signed char)(*((double*)aligned));
                break;
#if H5_SIZEOF_LONG_DOUBLE !=0
            case FLT_LDOUBLE:
                HDmemcpy(aligned, saved+j*sizeof(long double), sizeof(long double));
                hw_schar = (signed char)(*((long double*)aligned));
                break;
#endif
            case INT_SCHAR:
            case INT_UCHAR:
            case INT_SHORT:
            case INT_USHORT:
            case INT_INT:
            case INT_UINT:
            case INT_LONG:
            case INT_ULONG:
            case INT_LLONG:
            case INT_ULLONG:
            case OTHER:
            default:
                break;
            }
        } else if (INT_UCHAR==dst_type) {
            hw = (unsigned char*)&hw_uchar;
            switch (src_type) {
            case FLT_FLOAT:
                HDmemcpy(aligned, saved+j*sizeof(float), sizeof(float));
                hw_uchar = (unsigned char)(*((float*)aligned));
                break;
            case FLT_DOUBLE:
                HDmemcpy(aligned, saved+j*sizeof(double), sizeof(double));
                hw_uchar = (unsigned char)(*((double*)aligned));
                break;
#if H5_SIZEOF_LONG_DOUBLE !=0
            case FLT_LDOUBLE:
                HDmemcpy(aligned, saved+j*sizeof(long double), sizeof(long double));
                hw_uchar = (unsigned char)(*((long double*)aligned));
                break;
#endif
            case INT_SCHAR:
            case INT_UCHAR:
            case INT_SHORT:
            case INT_USHORT:
            case INT_INT:
            case INT_UINT:
            case INT_LONG:
            case INT_ULONG:
            case INT_LLONG:
            case INT_ULLONG:
            case OTHER:
            default:
                break;
            }
        } else if (INT_SHORT==dst_type) {
            hw = (unsigned char*)&hw_short;
            switch (src_type) {
            case FLT_FLOAT:
                HDmemcpy(aligned, saved+j*sizeof(float), sizeof(float));
                hw_short = (short)(*((float*)aligned));
                break;
            case FLT_DOUBLE:
                HDmemcpy(aligned, saved+j*sizeof(double), sizeof(double));
                hw_short = (short)(*((double*)aligned));
                break;
#if H5_SIZEOF_LONG_DOUBLE !=0
            case FLT_LDOUBLE:
                HDmemcpy(aligned, saved+j*sizeof(long double), sizeof(long double));
                hw_short = (short)(*((long double*)aligned));
                break;
#endif
            case INT_SCHAR:
            case INT_UCHAR:
            case INT_SHORT:
            case INT_USHORT:
            case INT_INT:
            case INT_UINT:
            case INT_LONG:
            case INT_ULONG:
            case INT_LLONG:
            case INT_ULLONG:
            case OTHER:
            default:
                break;
            }
        } else if (INT_USHORT==dst_type) {
            hw = (unsigned char*)&hw_ushort;
            switch (src_type) {
            case FLT_FLOAT:
                HDmemcpy(aligned, saved+j*sizeof(float), sizeof(float));
                hw_ushort = (unsigned short)(*((float*)aligned));
                break;
            case FLT_DOUBLE:
                HDmemcpy(aligned, saved+j*sizeof(double), sizeof(double));
                hw_ushort = (unsigned short)(*((double*)aligned));
                break;
#if H5_SIZEOF_LONG_DOUBLE !=0
            case FLT_LDOUBLE:
                HDmemcpy(aligned, saved+j*sizeof(long double), sizeof(long double));
                hw_ushort = (unsigned short)(*((long double*)aligned));
                break;
#endif
            case INT_SCHAR:
            case INT_UCHAR:
            case INT_SHORT:
            case INT_USHORT:
            case INT_INT:
            case INT_UINT:
            case INT_LONG:
            case INT_ULONG:
            case INT_LLONG:
            case INT_ULLONG:
            case OTHER:
            default:
                break;
            }
        } else if (INT_INT==dst_type) {
            hw = (unsigned char*)&hw_int;
            switch (src_type) {
            case FLT_FLOAT:
                HDmemcpy(aligned, saved+j*sizeof(float), sizeof(float));
                hw_int = (int)(*((float*)aligned));
                break;
            case FLT_DOUBLE:
                HDmemcpy(aligned, saved+j*sizeof(double), sizeof(double));
                hw_int = (int)(*((double*)aligned));
                break;
#if H5_SIZEOF_LONG_DOUBLE !=0
            case FLT_LDOUBLE:
                HDmemcpy(aligned, saved+j*sizeof(long double), sizeof(long double));
                hw_int = (int)(*((long double*)aligned));
                break;
#endif
            case INT_SCHAR:
            case INT_UCHAR:
            case INT_SHORT:
            case INT_USHORT:
            case INT_INT:
            case INT_UINT:
            case INT_LONG:
            case INT_ULONG:
            case INT_LLONG:
            case INT_ULLONG:
            case OTHER:
            default:
                break;
            }
        } else if (INT_UINT==dst_type) {
            hw = (unsigned char*)&hw_uint;
            switch (src_type) {
            case FLT_FLOAT:
                HDmemcpy(aligned, saved+j*sizeof(float), sizeof(float));
                hw_uint = (unsigned int)(*((float*)aligned));
                break;
            case FLT_DOUBLE:
                HDmemcpy(aligned, saved+j*sizeof(double), sizeof(double));
                hw_uint = (unsigned int)(*((double*)aligned));
                break;
#if H5_SIZEOF_LONG_DOUBLE !=0
            case FLT_LDOUBLE:
                HDmemcpy(aligned, saved+j*sizeof(long double), sizeof(long double));
                hw_uint = (unsigned int)(*((long double*)aligned));
                break;
#endif
            case INT_SCHAR:
            case INT_UCHAR:
            case INT_SHORT:
            case INT_USHORT:
            case INT_INT:
            case INT_UINT:
            case INT_LONG:
            case INT_ULONG:
            case INT_LLONG:
            case INT_ULLONG:
            case OTHER:
            default:
                break;
            }
        } else if (INT_LONG==dst_type) {
            hw = (unsigned char*)&hw_long;
            switch (src_type) {
            case FLT_FLOAT:
                HDmemcpy(aligned, saved+j*sizeof(float), sizeof(float));
                hw_long = (long)(*((float*)aligned));
                break;
            case FLT_DOUBLE:
                HDmemcpy(aligned, saved+j*sizeof(double), sizeof(double));
                hw_long = (long)(*((double*)aligned));
                break;
#if H5_SIZEOF_LONG_DOUBLE !=0
            case FLT_LDOUBLE:
                HDmemcpy(aligned, saved+j*sizeof(long double), sizeof(long double));
                hw_long = (long)(*((long double*)aligned));
                break;
#endif
            case INT_SCHAR:
            case INT_UCHAR:
            case INT_SHORT:
            case INT_USHORT:
            case INT_INT:
            case INT_UINT:
            case INT_LONG:
            case INT_ULONG:
            case INT_LLONG:
            case INT_ULLONG:
            case OTHER:
            default:
                break;
            }
        } else if (INT_ULONG==dst_type) {
            hw = (unsigned char*)&hw_ulong;
            switch (src_type) {
            case FLT_FLOAT:
                HDmemcpy(aligned, saved+j*sizeof(float), sizeof(float));
                hw_ulong = (unsigned long)(*((float*)aligned));
                break;
            case FLT_DOUBLE:
                HDmemcpy(aligned, saved+j*sizeof(double), sizeof(double));
                hw_ulong = (unsigned long)(*((double*)aligned));
                break;
#if H5_SIZEOF_LONG_DOUBLE !=0
            case FLT_LDOUBLE:
                HDmemcpy(aligned, saved+j*sizeof(long double), sizeof(long double));
                hw_ulong = (unsigned long)(*((long double*)aligned));
                break;
#endif
            case INT_SCHAR:
            case INT_UCHAR:
            case INT_SHORT:
            case INT_USHORT:
            case INT_INT:
            case INT_UINT:
            case INT_LONG:
            case INT_ULONG:
            case INT_LLONG:
            case INT_ULLONG:
            case OTHER:
            default:
                break;
            }
        } else if (INT_LLONG==dst_type) {
            hw = (unsigned char*)&hw_llong;
            switch (src_type) {
            case FLT_FLOAT:
                HDmemcpy(aligned, saved+j*sizeof(float), sizeof(float));
                hw_llong = (long long)(*((float*)aligned));
                break;
            case FLT_DOUBLE:
                HDmemcpy(aligned, saved+j*sizeof(double), sizeof(double));
                hw_llong = (long long)(*((double*)aligned));
                break;
#if H5_SIZEOF_LONG_DOUBLE !=0
            case FLT_LDOUBLE:
                HDmemcpy(aligned, saved+j*sizeof(long double), sizeof(long double));
                hw_llong = (long long)(*((long double*)aligned));
                break;
#endif
            case INT_SCHAR:
            case INT_UCHAR:
            case INT_SHORT:
            case INT_USHORT:
            case INT_INT:
            case INT_UINT:
            case INT_LONG:
            case INT_ULONG:
            case INT_LLONG:
            case INT_ULLONG:
            case OTHER:
            default:
                break;
            }
        } else if (INT_ULLONG==dst_type) {
            hw = (unsigned char*)&hw_ullong;
            switch (src_type) {
            case FLT_FLOAT:
                HDmemcpy(aligned, saved+j*sizeof(float), sizeof(float));
                hw_ullong = (unsigned long long)(*((float*)aligned));
                break;
            case FLT_DOUBLE:
                HDmemcpy(aligned, saved+j*sizeof(double), sizeof(double));
                hw_ullong = (unsigned long long)(*((double*)aligned));
                break;
#if H5_SIZEOF_LONG_DOUBLE !=0
            case FLT_LDOUBLE:
                HDmemcpy(aligned, saved+j*sizeof(long double), sizeof(long double));
                hw_ullong = (unsigned long long)(*((long double*)aligned));
                break;
#endif
            case INT_SCHAR:
            case INT_UCHAR:
            case INT_SHORT:
            case INT_USHORT:
            case INT_INT:
            case INT_UINT:
            case INT_LONG:
            case INT_ULONG:
            case INT_LLONG:
            case INT_ULLONG:
            case OTHER:
            default:
                break;
            }
        }

        /* Make certain that there isn't some weird number of destination bits */
        assert(dst_nbits%8==0);

        /* For Intel machines, the size of "long double" is 12 bytes, precision
         * is 80 bits; for AMD processors, the size of "long double" is 16 bytes,
         * precision is 80 bits.  During hardware conversion, the last few unused
         * bytes may have garbage in them.  Clean them out with 0s before compare
         * the values.
         */
#if H5_SIZEOF_LONG_DOUBLE !=0
        if(dendian==H5T_ORDER_LE && dst_type==FLT_LDOUBLE) {
            unsigned int q;
            for(q=dst_nbits/8; q<dst_size; q++) {
                buf[j*dst_size+q] = 0x00;
            }
        }
#endif

        /* Are the two results the same? */
        for (k=(dst_size-(dst_nbits/8)); k<dst_size; k++)
            if (buf[j*dst_size+k]!=hw[k])
                break;
        if (k==dst_size)
            continue; /*no error*/

        /*
         * Convert the source and destination values to little endian
         * order so we can use the HDF5 bit vector operations to test
         * certain things.  These routines have already been tested by
         * the `bittests' program.
         */

        if ((FLT_FLOAT==src_type || FLT_DOUBLE==src_type) && sendian==H5T_ORDER_VAX) {
            for (k = 0; k < src_size; k += 2) {
                src_bits[k] = saved[j*src_size + (src_size - 2) - k];
                src_bits[k + 1] = saved[j*src_size + (src_size - 1) - k];
            }
        } else {
            for (k=0; k<src_size; k++)
                src_bits[src_size-(k+1)] = saved[j*src_size+ENDIAN(src_size, k, sendian)];
        }

        for (k=0; k<dst_size; k++)
            dst_bits[dst_size-(k+1)] = buf[j*dst_size+ENDIAN(dst_size, k, dendian)];

        /*          Test library's default overflow handling:
         * Hardware usually doesn't handle overflows too gracefully. The
         * hardware conversion result during overflows is usually garbage
         * so we must handle those cases differetly when checking results.
         *
         *          Test user's exception handler when overflows:
         * Try to follow the except_func callback function to check if the
         * desired value was set.
         */
        if ((FLT_FLOAT==src_type || FLT_DOUBLE==src_type
#if H5_SIZEOF_LONG_DOUBLE !=0
                    || FLT_LDOUBLE==src_type
#endif
                    )
                && (INT_SCHAR==dst_type || INT_SHORT==dst_type || INT_INT==dst_type
                || INT_LONG==dst_type || INT_LLONG==dst_type)) {
            if(0==H5T_bit_get_d(src_bits, src_nbits-1, (size_t)1) &&
                    overflows(src_bits, src, dst_nbits-1)) {
                /*
                 * Source is positive and the magnitude is too large for
                 * the destination.  The destination should be set to the
                 * maximum possible value: 0x7f...f
                 */
                if(!except_set) {
                    if (0==H5T_bit_get_d(dst_bits, dst_nbits-1, (size_t)1) &&
                            H5T_bit_find(dst_bits, (size_t)0, dst_nbits-1, H5T_BIT_LSB, 0) < 0)
                        continue; /*no error*/
                } else {
                    /* fill_value is small so we know only the 1st byte is set */
                    if (dst_bits[0] == fill_value)
                        continue; /*no error*/
                }
            } else if (1==H5T_bit_get_d(src_bits, src_nbits-1, (size_t)1) &&
                    overflows(src_bits, src, dst_nbits-1)) {
                /*
                 * Source is negative but the magnitude is too large for
                 * the destination. The destination should be set to the
                 * smallest possible value: 0x80...0
                 */
                if(!except_set) {
                    if (1==H5T_bit_get_d(dst_bits, dst_nbits-1, (size_t)1) &&
                            H5T_bit_find(dst_bits, (size_t)0, dst_nbits-1, H5T_BIT_LSB, 1) < 0)
                        continue; /*no error*/
                } else {
                    if (dst_bits[0] == fill_value)
                        continue; /*no error*/
                }
            }
        }

        if ((FLT_FLOAT==src_type || FLT_DOUBLE==src_type
#if H5_SIZEOF_LONG_DOUBLE !=0
                    || FLT_LDOUBLE==src_type
#endif
                )
                && (INT_UCHAR==dst_type || INT_USHORT==dst_type || INT_UINT==dst_type
                || INT_ULONG==dst_type || INT_ULLONG==dst_type)) {
            if (H5T_bit_get_d(src_bits, src_nbits-1, (size_t)1)) {
                /*
                 * The source is negative so the result should be zero.
                 * The source is negative if the most significant bit is
                 * set.  The destination is zero if all bits are zero.
                 */
                if(!except_set) {
                    if (H5T_bit_find(dst_bits, (size_t)0, dst_nbits, H5T_BIT_LSB, 1) < 0)
                        continue; /*no error*/
                } else {
                    if (dst_bits[0] == fill_value)
                        continue; /*no error*/
                }
            } else if (overflows(src_bits, src, dst_nbits)) {
                /*
                 * The source is a value with a magnitude too large for
                 * the destination.  The destination should be the
                 * largest possible value: 0xff...f
                 */
                if(!except_set) {
                    if (H5T_bit_find(dst_bits, (size_t)0, dst_nbits, H5T_BIT_LSB, 0) < 0)
                        continue; /*no error*/
                } else {
                    if (dst_bits[0] == fill_value)
                        continue; /*no error*/
                }
            }
        }
/* On some machines (notably the SGI and Solaris 64-bit machines) unsigned long
* values are not converted to float or double values correctly, they are
* consistently off by the lowest bit being rounded oppositely to our
* software conversion routines output.  So, on those machines, we allow
* the converted value to be +/- 1 from the machine's value.  -QAK
*/
#ifndef H5_SW_ULONG_TO_FP_BOTTOM_BIT_WORKS
        if(dst_size==sizeof(unsigned)) {
            unsigned tmp_s, tmp_h;
            HDmemcpy(&tmp_s,&buf[j*dst_size],sizeof(unsigned));
            HDmemcpy(&tmp_h,&hw[0],sizeof(unsigned));
            if((tmp_s+1)==tmp_h || (tmp_s-1)==tmp_h)
                continue; /*no error*/
        } /* end if */
        else if (dst_size==sizeof(unsigned long)) {
            unsigned long tmp_s, tmp_h;
            HDmemcpy(&tmp_s,&buf[j*dst_size],sizeof(unsigned long));
            HDmemcpy(&tmp_h,&hw[0],sizeof(unsigned long));
            if((tmp_s+1)==tmp_h || (tmp_s-1)==tmp_h)
                continue; /*no error*/
        } /* end if */
        else if (dst_size==sizeof(unsigned long long)) {
            unsigned long long tmp_s, tmp_h;
            HDmemcpy(&tmp_s,&buf[j*dst_size],sizeof(unsigned long long));
            HDmemcpy(&tmp_h,&hw[0],sizeof(unsigned long long));
            if((tmp_s+1)==tmp_h || (tmp_s-1)==tmp_h)
                continue; /*no error*/
        } /* end if */
#endif /* end H5_ULONG_FP_BOTTOM_BIT_WORKS */

/* For PGI compiler on Linux, during conversion from 'float' or 'double' to
* 'unsigned long long', round-up happens when the fraction of float-point
* value is greater than 0.5. So we allow the converted value to be off by 1.
*/
#ifndef H5_FP_TO_ULLONG_BOTTOM_BIT_WORKS
        if((src_type==FLT_FLOAT || src_type==FLT_DOUBLE) && dst_type==INT_ULLONG) {
            unsigned long long tmp_s, tmp_h;
            HDmemcpy(&tmp_s,&buf[j*dst_size],sizeof(unsigned long long));
            HDmemcpy(&tmp_h,&hw[0],sizeof(unsigned long long));
            if((tmp_s+1)==tmp_h)
                continue; /*no error*/
        }
#endif /*end H5_FP_TO_ULLONG_BOTTOM_BIT_WORKS*/

/* For GNU compilers on FreeBSD(sleipnir), during conversion from 'unsigned long long'
* to 'long double', the last 2 bytes of mantissa are lost.  But this loss seems
* acceptable.  We allow it to go through instead of fail it.  Sometimes, there's roundup
* to the 3rd last byte of mantissa.  So we only try to compare all but the last 3 bytes.
*/
#ifndef H5_ULLONG_TO_LDOUBLE_PRECISION
#if H5_SIZEOF_LONG_DOUBLE !=0
        if(src_type==INT_ULLONG && dst_type==FLT_LDOUBLE) {
            long double tmp_s, tmp_h;
            HDmemcpy(&tmp_s,&buf[j*dst_size],sizeof(long double));
            HDmemcpy(&tmp_h,&hw[0],sizeof(long double));
            /*Don't compare the last 3 bytes of mantissa*/
            if(!HDmemcmp(&tmp_s+4, &tmp_h+4, sizeof(long double)-4))
                continue; /*no error*/
        }
#endif
#endif /*end H5_ULLONG_TO_LDOUBLE_PRECISION*/

        /* Print errors */
        if (0==fails_this_test++) {
            if(run_test==TEST_NORMAL) {
                H5_FAILED();
            } else if(run_test==TEST_DENORM || run_test==TEST_SPECIAL) {
                H5_WARNING();
            }
        }
        printf("    elmt %u: \n", (unsigned)j);

        printf("        src = ");
        for (k=0; k<src_size; k++)
            printf(" %02x", saved[j*src_size+ENDIAN(src_size, k, sendian)]);
        printf("%*s", (int)(3*MAX(0, (ssize_t)dst_size-(ssize_t)src_size)), "");
        switch (src_type) {
            case INT_SCHAR:
                HDmemcpy(aligned, saved+j*sizeof(signed char), sizeof(signed char));
                printf(" %29d\n", (int)*((signed char*)aligned));
                break;
            case INT_UCHAR:
                HDmemcpy(aligned, saved+j*sizeof(unsigned char), sizeof(unsigned char));
                printf(" %29u\n", (unsigned)*((unsigned char*)aligned));
                break;
            case INT_SHORT:
                HDmemcpy(aligned, saved+j*sizeof(short), sizeof(short));
                printf(" %29hd\n", *((short*)aligned));
                break;
            case INT_USHORT:
                HDmemcpy(aligned, saved+j*sizeof(unsigned short), sizeof(unsigned short));
                printf(" %29hu\n", *((unsigned short*)aligned));
                break;
            case INT_INT:
                HDmemcpy(aligned, saved+j*sizeof(int), sizeof(int));
                printf(" %29d\n", *((int*)aligned));
                break;
            case INT_UINT:
                HDmemcpy(aligned, saved+j*sizeof(unsigned), sizeof(unsigned));
                printf(" %29u\n", *((unsigned*)aligned));
                break;
            case INT_LONG:
                HDmemcpy(aligned, saved+j*sizeof(long), sizeof(long));
                printf(" %29ld\n", *((long*)aligned));
                break;
            case INT_ULONG:
                HDmemcpy(aligned, saved+j*sizeof(unsigned long), sizeof(unsigned long));
                printf(" %29lu\n", *((unsigned long*)aligned));
                break;
            case INT_LLONG:
                HDmemcpy(aligned, saved+j*sizeof(long long), sizeof(long long));
                HDfprintf(stdout," %29"H5_PRINTF_LL_WIDTH"d\n", *((long long*)aligned));
                break;
            case INT_ULLONG:
                HDmemcpy(aligned, saved+j*sizeof(unsigned long long), sizeof(unsigned long long));
                HDfprintf(stdout," %29"H5_PRINTF_LL_WIDTH"u\n", *((unsigned long long*)aligned));
                break;
            case FLT_FLOAT:
                HDmemcpy(aligned, saved+j*sizeof(float), sizeof(float));
                printf(" %29f\n", *((float*)aligned));
                break;
            case FLT_DOUBLE:
                HDmemcpy(aligned, saved+j*sizeof(double), sizeof(double));
                printf(" %29f\n", *((double*)aligned));
                break;
#if H5_SIZEOF_LONG_DOUBLE !=0
            case FLT_LDOUBLE:
                HDmemcpy(aligned, saved+j*sizeof(long double), sizeof(long double));
                printf(" %29Lf\n", *((long double*)aligned));
                break;
#endif
            case OTHER:
                break;
        }

        printf("        dst = ");
        for (k=0; k<dst_size; k++)
            printf(" %02x", buf[j*dst_size+ENDIAN(dst_size, k, dendian)]);
        printf("%*s", (int)(3*MAX(0, (ssize_t)src_size-(ssize_t)dst_size)), "");
        switch (dst_type) {
            case INT_SCHAR:
                HDmemcpy(aligned, buf+j*sizeof(signed char), sizeof(signed char));
                printf(" %29d\n", (int)*((signed char*)aligned));
                break;
            case INT_UCHAR:
                HDmemcpy(aligned, buf+j*sizeof(unsigned char), sizeof(unsigned char));
                printf(" %29u\n", (unsigned)*((unsigned char*)aligned));
                break;
            case INT_SHORT:
                HDmemcpy(aligned, buf+j*sizeof(short), sizeof(short));
                printf(" %29hd\n", *((short*)aligned));
                break;
            case INT_USHORT:
                HDmemcpy(aligned, buf+j*sizeof(unsigned short), sizeof(unsigned short));
                printf(" %29hu\n", *((unsigned short*)aligned));
                break;
            case INT_INT:
                HDmemcpy(aligned, buf+j*sizeof(int), sizeof(int));
                printf(" %29d\n", *((int*)aligned));
                break;
            case INT_UINT:
                HDmemcpy(aligned, buf+j*sizeof(unsigned), sizeof(unsigned));
                printf(" %29u\n", *((unsigned*)aligned));
                break;
            case INT_LONG:
                HDmemcpy(aligned, buf+j*sizeof(long), sizeof(long));
                printf(" %29ld\n", *((long*)aligned));
                break;
            case INT_ULONG:
                HDmemcpy(aligned, buf+j*sizeof(unsigned long), sizeof(unsigned long));
                printf(" %29lu\n", *((unsigned long*)aligned));
                break;
            case INT_LLONG:
                HDmemcpy(aligned, buf+j*sizeof(long long), sizeof(long long));
                HDfprintf(stdout," %29"H5_PRINTF_LL_WIDTH"d\n", *((long long*)aligned));
                break;
            case INT_ULLONG:
                HDmemcpy(aligned, buf+j*sizeof(unsigned long long), sizeof(unsigned long long));
                HDfprintf(stdout," %29"H5_PRINTF_LL_WIDTH"u\n", *((unsigned long long*)aligned));
                break;
            case FLT_FLOAT:
                HDmemcpy(aligned, buf+j*sizeof(float), sizeof(float));
                printf(" %29f\n", *((float*)aligned));
                break;
            case FLT_DOUBLE:
                HDmemcpy(aligned, buf+j*sizeof(double), sizeof(double));
                printf(" %29f\n", *((double*)aligned));
                break;
#if H5_SIZEOF_LONG_DOUBLE !=0
            case FLT_LDOUBLE:
                HDmemcpy(aligned, buf+j*sizeof(long double), sizeof(long double));
                printf(" %29Lf\n", *((long double*)aligned));
                break;
#endif
            case OTHER:
                break;
        }

        printf("        ans = ");
        for (k=0; k<dst_size; k++)
            printf(" %02x", hw[ENDIAN(dst_size, k, dendian)]);
        printf("%*s", (int)(3*MAX(0, (ssize_t)src_size-(ssize_t)dst_size)), "");
        switch (dst_type) {
            case INT_SCHAR:
                printf(" %29d\n", (int)*((signed char*)hw));
                break;
            case INT_UCHAR:
                printf(" %29u\n", (unsigned)*((unsigned char*)hw));
                break;
            case INT_SHORT:
                printf(" %29hd\n", *((short*)hw));
                break;
            case INT_USHORT:
                printf(" %29hu\n", *((unsigned short*)hw));
                break;
            case INT_INT:
                printf(" %29d\n", *((int*)hw));
                break;
            case INT_UINT:
                printf(" %29u\n", *((unsigned int*)hw));
                break;
            case INT_LONG:
                printf(" %29ld\n", *((long*)hw));
                break;
            case INT_ULONG:
                printf(" %29lu\n", *((unsigned long*)hw));
                break;
            case INT_LLONG:
                printf(" %29"H5_PRINTF_LL_WIDTH"d\n", *((long long*)hw));
                break;
            case INT_ULLONG:
                printf(" %29"H5_PRINTF_LL_WIDTH"u\n", *((unsigned long long*)hw));
                break;
            case FLT_FLOAT:
                printf(" %29f\n", *((float*)hw));
                break;
            case FLT_DOUBLE:
                printf(" %29f\n", *((double*)hw));
                break;
#if H5_SIZEOF_LONG_DOUBLE !=0
            case FLT_LDOUBLE:
                printf(" %29Lf\n", *((long double*)hw));
                break;
#endif
            case OTHER:
                break;
        }

        /* If the source is normalized values, print out error message; if it is
         * denormalized or special values, print out warning message.*/
        if (++fails_all_tests>=max_fails) {
            if(run_test==TEST_NORMAL)
                HDputs("    maximum failures reached, aborting test...");
            else if(run_test==TEST_DENORM || run_test==TEST_SPECIAL)
                HDputs("    maximum warnings reached, aborting test...");
            HDputs("    (dst is library's conversion output. ans is compiler's conversion output.)");

            goto done;
        }
    }

    if(!fails_all_tests)
        PASSED();

 done:
    if (buf) aligned_free(buf);
    if (saved) aligned_free(saved);
    if (aligned) HDfree(aligned);
    HDfflush(stdout);
    reset_hdf5();	/*print statistics*/

    /* If the source is normalized floating values, treat the failures as error;
     * if it is denormalized or special floating values, treat the failure as warning.*/
    if(run_test==TEST_NORMAL)
        return (int)fails_all_tests;
    else if(run_test==TEST_DENORM || run_test==TEST_SPECIAL)
        return 0;

 error:
    if (buf) aligned_free(buf);
    if (saved) aligned_free(saved);
    if (aligned) HDfree(aligned);
    HDfflush(stdout);
    reset_hdf5();	/*print statistics*/

    if(run_test==TEST_NORMAL)
        return MAX((int)fails_all_tests, 1);
    else {
        HDassert(run_test==TEST_DENORM || run_test==TEST_SPECIAL);
        return 1;
    }
}


/*-------------------------------------------------------------------------
 * Function:	overflows
 *
 * Purpose:	When convert from float or double to any integer type,
 *              check if overflow occurs.
 *
 *
 * Return:	TRUE:           overflow happens
 *
 *              FALSE:          no overflow
 *
 * Programmer:	Raymond Lu
 *              Monday, Nov 17, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
overflows(unsigned char *origin_bits, hid_t src_id, size_t dst_num_bits)
{
    hbool_t     ret_value=FALSE;
    hsize_t     expt;
    size_t      mant_digits=0, expt_digits=0, bias=0;
    size_t      epos, mpos;
    size_t      src_prec=0;             /*source type precision in bits*/
    H5T_norm_t  norm;
    ssize_t     indx;
    unsigned char        bits[32], mant_bits[32];

    HDmemset(bits, 0, (size_t)32);
    HDmemset(mant_bits, 0, (size_t)32);

    /*
     * Sometimes, type size isn't equal to the precision like Linux's "long
     * double", where size is 96 bits and precision is 80 bits.
     */

    src_prec = H5Tget_precision(src_id);
    H5Tget_fields(src_id, NULL, &epos, &expt_digits, &mpos, &mant_digits);
    bias = H5Tget_ebias(src_id);
    norm = H5Tget_norm(src_id);

    HDmemcpy(bits, origin_bits, src_prec/8+1);

    /*Check for special cases: +Inf, -Inf*/
    if (H5T_bit_find (bits, mpos, mant_digits, H5T_BIT_LSB, TRUE) < 0) {
        if (H5T_bit_find (bits, epos, expt_digits, H5T_BIT_LSB, FALSE) < 0) {
            ret_value=TRUE;
            goto done;
        }
    } else if (H5T_NORM_NONE==norm && H5T_bit_find (bits, mpos, mant_digits-1,
        H5T_BIT_LSB, TRUE) < 0 && H5T_bit_find (bits, epos, expt_digits,
        H5T_BIT_LSB, FALSE) < 0) {
        /*This is a special case for the source of no implied mantissa bit.
         *If the exponent bits are all 1s and only the 1st bit of mantissa
         *is set to 1.  It's infinity. The Intel-Linux "long double" is this case.*/
            ret_value=TRUE;
            goto done;
    }

    /* get exponent */
    expt = H5T_bit_get_d(bits, mant_digits, expt_digits) - bias;

    if(expt>=(dst_num_bits-1)) {
       ret_value=TRUE;
       goto done;
    }

    /* get significand */
    H5T_bit_copy (mant_bits, (size_t)0, bits, (size_t)0, mant_digits);


    /* restore implicit bit if normalization is implied*/
    if(norm == H5T_NORM_IMPLIED) {
        H5T_bit_inc(mant_bits, mant_digits, (size_t)1);
        mant_digits++;
    }

    /* shift significand */
    H5T_bit_shift (mant_bits, (ssize_t)(expt-expt_digits), (size_t)0, (size_t)(32 * 8));

    indx = H5T_bit_find(mant_bits, (size_t)0, (size_t)(32 * 8), H5T_BIT_MSB, 1);

    if((size_t)indx>=dst_num_bits)
        ret_value=TRUE;

done:
    return ret_value;
}


/*-------------------------------------------------------------------------
 * Function:	run_integer_tests
 *
 * Purpose:	Runs all integer tests.
 *
 * Return:	Number of errors
 *
 * Programmer:	Robb Matzke
 *              Tuesday, November 24, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
run_integer_tests(const char *name)
{
    int		nerrors = 0;

    nerrors += test_conv_int_1(name, H5T_NATIVE_SCHAR, H5T_NATIVE_UCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SCHAR, H5T_NATIVE_SHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SCHAR, H5T_NATIVE_USHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SCHAR, H5T_NATIVE_INT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SCHAR, H5T_NATIVE_UINT);
#if H5_SIZEOF_LONG!=H5_SIZEOF_INT
    nerrors += test_conv_int_1(name, H5T_NATIVE_SCHAR, H5T_NATIVE_LONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SCHAR, H5T_NATIVE_ULONG);
#endif
#if H5_SIZEOF_LONG_LONG!=H5_SIZEOF_LONG
    nerrors += test_conv_int_1(name, H5T_NATIVE_SCHAR, H5T_NATIVE_LLONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SCHAR, H5T_NATIVE_ULLONG);
#endif

    nerrors += test_conv_int_1(name, H5T_NATIVE_UCHAR, H5T_NATIVE_SCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UCHAR, H5T_NATIVE_SHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UCHAR, H5T_NATIVE_USHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UCHAR, H5T_NATIVE_INT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UCHAR, H5T_NATIVE_UINT);
#if H5_SIZEOF_LONG!=H5_SIZEOF_INT
    nerrors += test_conv_int_1(name, H5T_NATIVE_UCHAR, H5T_NATIVE_LONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UCHAR, H5T_NATIVE_ULONG);
#endif
#if H5_SIZEOF_LONG_LONG!=H5_SIZEOF_LONG
    nerrors += test_conv_int_1(name, H5T_NATIVE_UCHAR, H5T_NATIVE_LLONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UCHAR, H5T_NATIVE_ULLONG);
#endif

    nerrors += test_conv_int_1(name, H5T_NATIVE_SHORT, H5T_NATIVE_SCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SHORT, H5T_NATIVE_UCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SHORT, H5T_NATIVE_USHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SHORT, H5T_NATIVE_INT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SHORT, H5T_NATIVE_UINT);
#if H5_SIZEOF_LONG!=H5_SIZEOF_INT
    nerrors += test_conv_int_1(name, H5T_NATIVE_SHORT, H5T_NATIVE_LONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SHORT, H5T_NATIVE_ULONG);
#endif
#if H5_SIZEOF_LONG_LONG!=H5_SIZEOF_LONG
    nerrors += test_conv_int_1(name, H5T_NATIVE_SHORT, H5T_NATIVE_LLONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SHORT, H5T_NATIVE_ULLONG);
#endif

    nerrors += test_conv_int_1(name, H5T_NATIVE_USHORT, H5T_NATIVE_SCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_USHORT, H5T_NATIVE_UCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_USHORT, H5T_NATIVE_SHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_USHORT, H5T_NATIVE_INT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_USHORT, H5T_NATIVE_UINT);
#if H5_SIZEOF_LONG!=H5_SIZEOF_INT
    nerrors += test_conv_int_1(name, H5T_NATIVE_USHORT, H5T_NATIVE_LONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_USHORT, H5T_NATIVE_ULONG);
#endif
#if H5_SIZEOF_LONG_LONG!=H5_SIZEOF_LONG
    nerrors += test_conv_int_1(name, H5T_NATIVE_USHORT, H5T_NATIVE_LLONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_USHORT, H5T_NATIVE_ULLONG);
#endif

    nerrors += test_conv_int_1(name, H5T_NATIVE_INT, H5T_NATIVE_SCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_INT, H5T_NATIVE_UCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_INT, H5T_NATIVE_SHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_INT, H5T_NATIVE_USHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_INT, H5T_NATIVE_UINT);
#if H5_SIZEOF_LONG!=H5_SIZEOF_INT
    nerrors += test_conv_int_1(name, H5T_NATIVE_INT, H5T_NATIVE_LONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_INT, H5T_NATIVE_ULONG);
#endif
#if H5_SIZEOF_LONG_LONG!=H5_SIZEOF_LONG
    nerrors += test_conv_int_1(name, H5T_NATIVE_INT, H5T_NATIVE_LLONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_INT, H5T_NATIVE_ULLONG);
#endif

    nerrors += test_conv_int_1(name, H5T_NATIVE_UINT, H5T_NATIVE_SCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UINT, H5T_NATIVE_UCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UINT, H5T_NATIVE_SHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UINT, H5T_NATIVE_USHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UINT, H5T_NATIVE_INT);
#if H5_SIZEOF_LONG!=H5_SIZEOF_INT
    nerrors += test_conv_int_1(name, H5T_NATIVE_UINT, H5T_NATIVE_LONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UINT, H5T_NATIVE_ULONG);
#endif
#if H5_SIZEOF_LONG_LONG!=H5_SIZEOF_LONG
    nerrors += test_conv_int_1(name, H5T_NATIVE_UINT, H5T_NATIVE_LLONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UINT, H5T_NATIVE_ULLONG);
#endif

#if H5_SIZEOF_LONG!=H5_SIZEOF_INT
    nerrors += test_conv_int_1(name, H5T_NATIVE_LONG, H5T_NATIVE_SCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LONG, H5T_NATIVE_UCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LONG, H5T_NATIVE_SHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LONG, H5T_NATIVE_USHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LONG, H5T_NATIVE_INT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LONG, H5T_NATIVE_UINT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LONG, H5T_NATIVE_ULONG);
#if H5_SIZEOF_LONG_LONG!=H5_SIZEOF_LONG
    nerrors += test_conv_int_1(name, H5T_NATIVE_LONG, H5T_NATIVE_LLONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LONG, H5T_NATIVE_ULLONG);
#endif
#endif

#if H5_SIZEOF_LONG!=H5_SIZEOF_INT
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULONG, H5T_NATIVE_SCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULONG, H5T_NATIVE_UCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULONG, H5T_NATIVE_SHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULONG, H5T_NATIVE_USHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULONG, H5T_NATIVE_INT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULONG, H5T_NATIVE_UINT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULONG, H5T_NATIVE_LONG);
#if H5_SIZEOF_LONG_LONG!=H5_SIZEOF_LONG
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULONG, H5T_NATIVE_LLONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULONG, H5T_NATIVE_ULLONG);
#endif
#endif

#if H5_SIZEOF_LONG_LONG!=H5_SIZEOF_LONG
    nerrors += test_conv_int_1(name, H5T_NATIVE_LLONG, H5T_NATIVE_SCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LLONG, H5T_NATIVE_UCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LLONG, H5T_NATIVE_SHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LLONG, H5T_NATIVE_USHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LLONG, H5T_NATIVE_INT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LLONG, H5T_NATIVE_UINT);
#if H5_SIZEOF_LONG!=H5_SIZEOF_INT
    nerrors += test_conv_int_1(name, H5T_NATIVE_LLONG, H5T_NATIVE_LONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LLONG, H5T_NATIVE_ULONG);
#endif
    nerrors += test_conv_int_1(name, H5T_NATIVE_LLONG, H5T_NATIVE_ULLONG);
#endif

#if H5_SIZEOF_LONG_LONG!=H5_SIZEOF_LONG
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULLONG, H5T_NATIVE_SCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULLONG, H5T_NATIVE_UCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULLONG, H5T_NATIVE_SHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULLONG, H5T_NATIVE_USHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULLONG, H5T_NATIVE_INT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULLONG, H5T_NATIVE_UINT);
#if H5_SIZEOF_LONG!=H5_SIZEOF_INT
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULLONG, H5T_NATIVE_LONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULLONG, H5T_NATIVE_ULONG);
#endif
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULLONG, H5T_NATIVE_LLONG);
#endif

    return nerrors;
}


/*-------------------------------------------------------------------------
 * Function:	run_fp_tests
 *
 * Purpose:	Runs all floating-point tests.
 *
 * Return:	Number of errors
 *
 * Programmer:	Raymond Lu
 *              Tuesday, March 22, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
run_fp_tests(const char *name)
{
    int		nerrors = 0;

    if(!strcmp(name, "noop")) {
        nerrors += test_conv_flt_1("noop", TEST_NOOP, H5T_NATIVE_FLOAT, H5T_NATIVE_FLOAT);
        nerrors += test_conv_flt_1("noop", TEST_NOOP, H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE);
#if H5_SIZEOF_LONG_DOUBLE !=0
        nerrors += test_conv_flt_1("noop", TEST_NOOP, H5T_NATIVE_LDOUBLE, H5T_NATIVE_LDOUBLE);
#endif
        goto done;
    }

    /*Test normalized values.  TEST_NORMAL indicates normalized values.*/
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_FLOAT, H5T_NATIVE_DOUBLE);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_DOUBLE, H5T_NATIVE_FLOAT);
#if H5_SIZEOF_LONG_DOUBLE!=H5_SIZEOF_DOUBLE && H5_SIZEOF_LONG_DOUBLE !=0
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_FLOAT, H5T_NATIVE_LDOUBLE);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_DOUBLE, H5T_NATIVE_LDOUBLE);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_LDOUBLE, H5T_NATIVE_FLOAT);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_LDOUBLE, H5T_NATIVE_DOUBLE);
#endif

#ifndef H5_VMS
    /*Test denormalized values.  TEST_DENORM indicates denormalized values.*/
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_FLOAT, H5T_NATIVE_DOUBLE);
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_DOUBLE, H5T_NATIVE_FLOAT);
#if H5_SIZEOF_LONG_DOUBLE!=H5_SIZEOF_DOUBLE && H5_SIZEOF_LONG_DOUBLE!=0
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_FLOAT, H5T_NATIVE_LDOUBLE);
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_DOUBLE, H5T_NATIVE_LDOUBLE);
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_LDOUBLE, H5T_NATIVE_FLOAT);
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_LDOUBLE, H5T_NATIVE_DOUBLE);
#endif

    /*Test special values, +/-0, +/-infinity, +/-QNaN, +/-SNaN.*/
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_FLOAT, H5T_NATIVE_DOUBLE);
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_DOUBLE, H5T_NATIVE_FLOAT);
#if H5_SIZEOF_LONG_DOUBLE!=H5_SIZEOF_DOUBLE && H5_SIZEOF_LONG_DOUBLE!=0
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_FLOAT, H5T_NATIVE_LDOUBLE);
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_DOUBLE, H5T_NATIVE_LDOUBLE);
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_LDOUBLE, H5T_NATIVE_FLOAT);
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_LDOUBLE, H5T_NATIVE_DOUBLE);
#endif
#endif /*H5_VMS*/

done:
    return nerrors;
}


/*-------------------------------------------------------------------------
 * Function:	run_int_fp_conv
 *
 * Purpose:	Runs all integer-float tests.
 *
 * Return:	Number of errors
 *
 * Programmer:	Raymond Lu
 *              Monday, November 10, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
run_int_fp_conv(const char *name)
{
    int		nerrors = 0;

    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_SCHAR, H5T_NATIVE_FLOAT);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_SCHAR, H5T_NATIVE_DOUBLE);

    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_UCHAR, H5T_NATIVE_FLOAT);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_UCHAR, H5T_NATIVE_DOUBLE);

    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_SHORT, H5T_NATIVE_FLOAT);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_SHORT, H5T_NATIVE_DOUBLE);

    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_USHORT, H5T_NATIVE_FLOAT);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_USHORT, H5T_NATIVE_DOUBLE);

    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_INT, H5T_NATIVE_FLOAT);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_INT, H5T_NATIVE_DOUBLE);

    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_UINT, H5T_NATIVE_FLOAT);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_UINT, H5T_NATIVE_DOUBLE);

#if H5_SIZEOF_LONG!=H5_SIZEOF_INT
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_LONG, H5T_NATIVE_FLOAT);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_LONG, H5T_NATIVE_DOUBLE);

#if H5_ULONG_TO_FLOAT_ACCURATE
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_ULONG, H5T_NATIVE_FLOAT);
#endif
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_ULONG, H5T_NATIVE_DOUBLE);
#endif

#if H5_SIZEOF_LONG_LONG!=H5_SIZEOF_LONG
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_LLONG, H5T_NATIVE_FLOAT);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_LLONG, H5T_NATIVE_DOUBLE);

#ifdef H5_ULLONG_TO_FP_CAST_WORKS
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_ULLONG, H5T_NATIVE_FLOAT);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_ULLONG, H5T_NATIVE_DOUBLE);
#else /* H5_ULLONG_TO_FP_CAST_WORKS */
    {
        char		str[256];		/*hello string		*/

        sprintf(str, "Testing %s %s -> %s conversions",
                name, "unsigned long long", "float");
        printf("%-70s", str);
        SKIPPED();
        HDputs("    Test skipped due to compiler not handling conversion.");

        sprintf(str, "Testing %s %s -> %s conversions",
                name, "unsigned long long", "double");
        printf("%-70s", str);
        SKIPPED();
        HDputs("    Test skipped due to compiler not handling conversion.");
    }
#endif /* H5_ULLONG_TO_FP_CAST_WORKS */
#endif

#if H5_INTEGER_TO_LDOUBLE_ACCURATE
#if H5_SIZEOF_LONG_DOUBLE!=H5_SIZEOF_DOUBLE
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_SCHAR, H5T_NATIVE_LDOUBLE);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_UCHAR, H5T_NATIVE_LDOUBLE);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_SHORT, H5T_NATIVE_LDOUBLE);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_USHORT, H5T_NATIVE_LDOUBLE);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_INT, H5T_NATIVE_LDOUBLE);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_UINT, H5T_NATIVE_LDOUBLE);
#if H5_SIZEOF_LONG!=H5_SIZEOF_INT
#ifndef H5_LONG_TO_LDOUBLE_SPECIAL
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_LONG, H5T_NATIVE_LDOUBLE);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_ULONG, H5T_NATIVE_LDOUBLE);
#else
        {
            char		str[256];		/*string		*/

            sprintf(str, "Testing %s %s -> %s conversions",
                    name, "(unsigned) long", "long double");
            printf("%-70s", str);
            SKIPPED();
#if H5_SIZEOF_LONG_DOUBLE!=0
            HDputs("    Test skipped due to the special algorithm of hardware conversion.");
#else
            HDputs("    Test skipped due to disabled long double.");
#endif
        }
#endif
#endif /* H5_SIZEOF_LONG!=H5_SIZEOF_INT */
#if H5_SIZEOF_LONG_LONG!=H5_SIZEOF_LONG
#if H5_LLONG_TO_LDOUBLE_CORRECT
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_LLONG, H5T_NATIVE_LDOUBLE);
#else /* H5_LLONG_TO_LDOUBLE_CORRECT */
    {
        char		str[256];		/*hello string		*/

        sprintf(str, "Testing %s %s -> %s conversions",
                name, "long long", "long double");
        printf("%-70s", str);
        SKIPPED();
        HDputs("    Test skipped due to compiler error in handling conversion.");
    }
#endif /* H5_LLONG_TO_LDOUBLE_CORRECT */
#if !H5_CYGWIN_ULLONG_TO_LDOUBLE_ROUND_PROBLEM && H5_ULLONG_TO_FP_CAST_WORKS && H5_ULLONG_TO_LDOUBLE_PRECISION && H5_LLONG_TO_LDOUBLE_CORRECT
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_ULLONG, H5T_NATIVE_LDOUBLE);
#else /* !H5_CYGWIN_ULLONG_TO_LDOUBLE_ROUND_PROBLEM && H5_ULLONG_TO_FP_CAST_WORKS && H5_ULLONG_TO_LDOUBLE_PRECISION && H5_LLONG_TO_LDOUBLE_CORRECT */
    {
        char		str[256];		/*hello string		*/

        sprintf(str, "Testing %s %s -> %s conversions",
                name, "unsigned long long", "long double");
        printf("%-70s", str);
        SKIPPED();
        HDputs("    Test skipped due to compiler not handling conversion.");
    }
#endif /* !H5_CYGWIN_ULLONG_TO_LDOUBLE_ROUND_PROBLEM && H5_ULLONG_TO_FP_CAST_WORKS && H5_ULLONG_TO_LDOUBLE_PRECISION && H5_LLONG_TO_LDOUBLE_CORRECT */
#endif
#endif
#else /*H5_INTEGER_TO_LDOUBLE_ACCURATE*/
    {
        char		str[256];		/*string		*/

        sprintf(str, "Testing %s %s -> %s conversions",
                name, "all integers", "long double");
        printf("%-70s", str);
        SKIPPED();
#if H5_SIZEOF_LONG_DOUBLE !=0
        HDputs("    Test skipped due to hardware conversion error.");
#else
        HDputs("    Test skipped due to disabled long double.");
#endif

    }
#endif /*H5_INTEGER_TO_LDOUBLE_ACCURATE*/

    return nerrors;
}


/*-------------------------------------------------------------------------
 * Function:	run_fp_int_conv
 *
 * Purpose:	Runs all float-integer tests.
 *
 * Return:	Number of errors
 *
 * Programmer:	Raymond Lu
 *              Monday, November 10, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
run_fp_int_conv(const char *name)
{
    int		nerrors = 0;
#ifdef H5_FP_TO_INTEGER_OVERFLOW_WORKS
    int         test_values;

#ifdef H5_VMS
    test_values = TEST_NORMAL;
#else
    for(test_values = TEST_NORMAL; test_values <= TEST_SPECIAL; test_values++) {
#endif /*H5_VMS*/

        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT, H5T_NATIVE_SCHAR);
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE, H5T_NATIVE_SCHAR);

        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT, H5T_NATIVE_UCHAR);
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE, H5T_NATIVE_UCHAR);

        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT, H5T_NATIVE_SHORT);
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE, H5T_NATIVE_SHORT);

        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT, H5T_NATIVE_USHORT);
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE, H5T_NATIVE_USHORT);

        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT, H5T_NATIVE_INT);
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE, H5T_NATIVE_INT);

        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT, H5T_NATIVE_UINT);
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE, H5T_NATIVE_UINT);

#if H5_SIZEOF_LONG!=H5_SIZEOF_INT
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT, H5T_NATIVE_LONG);
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE, H5T_NATIVE_LONG);

        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT, H5T_NATIVE_ULONG);
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE, H5T_NATIVE_ULONG);
#endif

#if H5_SIZEOF_LONG_LONG!=H5_SIZEOF_LONG
        if(!strcmp(name, "hw")) { /* Hardware conversion */
            /* Windows .NET 2003 doesn't work for hardware conversion of this case.
             * .NET should define this macro H5_HW_FP_TO_LLONG_NOT_WORKS. */
#ifndef H5_HW_FP_TO_LLONG_NOT_WORKS
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT, H5T_NATIVE_LLONG);
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE, H5T_NATIVE_LLONG);
#endif /*H5_HW_FP_TO_LLONG_NOT_WORKS*/
        } else {  /* Software conversion */
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT, H5T_NATIVE_LLONG);
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE, H5T_NATIVE_LLONG);
        }
#ifdef H5_FP_TO_ULLONG_RIGHT_MAXIMUM
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT, H5T_NATIVE_ULLONG);
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE, H5T_NATIVE_ULLONG);
#else /*H5_FP_TO_ULLONG_RIGHT_MAXIMUM*/
        {
            char		str[256];		/*hello string		*/

            sprintf(str, "Testing %s %s -> %s conversions",
                    name, "float", "unsigned long long");
            printf("%-70s", str);
            SKIPPED();
            HDputs("    Test skipped due to hardware conversion error.");

            sprintf(str, "Testing %s %s -> %s conversions",
                    name, "double", "unsigned long long");
            printf("%-70s", str);
            SKIPPED();
            HDputs("    Test skipped due to hardware conversion error.");
        }
#endif /*H5_FP_TO_ULLONG_RIGHT_MAXIMUM*/
#endif

#if H5_LDOUBLE_TO_INTEGER_WORKS && H5_LDOUBLE_TO_INTEGER_ACCURATE
#if H5_SIZEOF_LONG_DOUBLE!=H5_SIZEOF_DOUBLE
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE, H5T_NATIVE_SCHAR);
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE, H5T_NATIVE_UCHAR);
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE, H5T_NATIVE_SHORT);
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE, H5T_NATIVE_USHORT);
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE, H5T_NATIVE_INT);
#if H5_LDOUBLE_TO_UINT_ACCURATE
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE, H5T_NATIVE_UINT);
#else /*H5_LDOUBLE_TO_UINT_ACCURATE*/
        {
            char		str[256];		/*string		*/

            sprintf(str, "Testing %s %s -> %s conversions",
                    name, "long double", "unsigned int");
            printf("%-70s", str);
            SKIPPED();
#if H5_SIZEOF_LONG_DOUBLE!=0
            HDputs("    Test skipped due to hardware conversion error.");
#else
            HDputs("    Test skipped due to disabled long double.");
#endif
        }
#endif /*H5_LDOUBLE_TO_UINT_ACCURATE*/
#if H5_SIZEOF_LONG!=H5_SIZEOF_INT && H5_SIZEOF_LONG_DOUBLE!=0
#ifndef H5_LDOUBLE_TO_LONG_SPECIAL
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE, H5T_NATIVE_LONG);
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE, H5T_NATIVE_ULONG);
#else
        {
            char		str[256];		/*string		*/

            sprintf(str, "Testing %s %s -> %s conversions",
                    name, "long double", "(unsigned) long");
            printf("%-70s", str);
            SKIPPED();
#if H5_SIZEOF_LONG_DOUBLE!=0
            HDputs("    Test skipped due to the special algorithm of hardware conversion.");
#else
            HDputs("    Test skipped due to disabled long double.");
#endif
        }
#endif
#endif /*H5_SIZEOF_LONG!=H5_SIZEOF_INT && H5_SIZEOF_LONG_DOUBLE!=0 */

#if H5_SIZEOF_LONG_LONG!=H5_SIZEOF_LONG && H5_SIZEOF_LONG_DOUBLE!=0
#ifdef H5_LDOUBLE_TO_LLONG_ACCURATE
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE, H5T_NATIVE_LLONG);
#else /*H5_LDOUBLE_TO_LLONG_ACCURATE*/
        {
            char		str[256];		/*string		*/

            sprintf(str, "Testing %s %s -> %s conversions",
                    name, "long double", "long long");
            printf("%-70s", str);
            SKIPPED();
#if H5_SIZEOF_LONG_DOUBLE!=0
            HDputs("    Test skipped due to hardware conversion error.");
#else
            HDputs("    Test skipped due to disabled long double.");
#endif
        }
#endif /*H5_LDOUBLE_TO_LLONG_ACCURATE*/
#if defined(H5_FP_TO_ULLONG_RIGHT_MAXIMUM) && defined(H5_LDOUBLE_TO_LLONG_ACCURATE)
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE, H5T_NATIVE_ULLONG);
#else /*H5_FP_TO_ULLONG_RIGHT_MAXIMUM && H5_LDOUBLE_TO_LLONG_ACCURATE*/
        {
            char		str[256];		/*string		*/

            sprintf(str, "Testing %s %s -> %s conversions",
                    name, "long double", "unsigned long long");
            printf("%-70s", str);
            SKIPPED();
#if H5_SIZEOF_LONG_DOUBLE!=0
            HDputs("    Test skipped due to hardware conversion error.");
#else
            HDputs("    Test skipped due to disabled long double.");
#endif
        }
#endif /*H5_FP_TO_ULLONG_RIGHT_MAXIMUM && H5_LDOUBLE_TO_LLONG_ACCURATE*/
#endif
#endif
#else /*H5_LDOUBLE_TO_INTEGER_WORKS && H5_LDOUBLE_TO_INTEGER_ACCURATE*/
        {
            char		str[256];		/*hello string		*/

            sprintf(str, "Testing %s %s -> %s conversions",
                    name, "long double", "all integers");
            printf("%-70s", str);
            SKIPPED();
#if H5_SIZEOF_LONG_DOUBLE!=0
            HDputs("    Test skipped due to hardware conversion error.");
#else
            HDputs("    Test skipped due to disabled long double.");
#endif
        }
#endif /*H5_LDOUBLE_TO_INTEGER_WORKS && H5_LDOUBLE_TO_INTEGER_ACCURATE*/
#ifndef H5_VMS
    } /* end for */
#endif /* H5_VMS */
#else /* H5_FP_TO_INTEGER_OVERFLOW_WORKS */
/* For Cray X1, the compiler generates floating exception when the
 * conversion overflows.  So disable all of the conversions from
 * floating-point numbers to integers.
 */
    char		str[256];		/*string		*/

    sprintf(str, "Testing %s %s -> %s conversions",
            name, "all floating-point numbers", "all integers");
    printf("%-70s", str);
    SKIPPED();
#if H5_SIZEOF_LONG_DOUBLE!=0
    HDputs("    Test skipped due to hardware conversion error.");
#else
    HDputs("    Test skipped due to disbaled long double.");
#endif
#endif /* H5_FP_TO_INTEGER_OVERFLOW_WORKS */

    return nerrors;
}


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Test the data type(integer and floating-point number).
 *
 * Return:      Success:
 *
 *              Failure:
 *
 * Programmer:  Robb Matzke
 *              Tuesday, December 9, 1997
 *
 * Modifications:
 *              Raymond Lu
 *              Monday, April 4, 2005
 *              These tests were split from dtypes.c because dtypes.c
 *              has grown too big.
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    unsigned long	nerrors = 0;

    /* Set the random # seed */
    HDsrandom((unsigned)HDtime(NULL));

    reset_hdf5();

    if (ALIGNMENT)
	printf("Testing non-aligned conversions (ALIGNMENT=%d)....\n", ALIGNMENT);

    /* Do the tests */

    /* Test H5Tcompiler_conv() for querying hard conversion. */
    nerrors += test_hard_query();

    /* Test user-define, query functions and software conversion
     * for user-defined floating-point types */
    nerrors += test_derived_flt();

    /* Test user-define, query functions and software conversion
     * for user-defined integer types */
    nerrors += test_derived_integer();

#ifndef H5_VMS
    /* Does floating point overflow generate a SIGFPE? */
    generates_sigfpe();
#endif

    /* Test degenerate cases */
    nerrors += run_fp_tests("noop");

    /* Test hardware floating-point conversion functions */
    nerrors += run_fp_tests("hard");

    /* Test hardware integer conversion functions */
    nerrors += run_integer_tests("hard");

    /* Test hardware integer-float conversion functions */
    nerrors += run_int_fp_conv("hard");

    /* Test hardware float-integer conversion functions */
    nerrors += run_fp_int_conv("hard");

    /* Test a few special values for hardware float-integer conversions */
    nerrors += test_particular_fp_integer();

    /*----------------------------------------------------------------------
     * Software tests
     *----------------------------------------------------------------------
     */
    without_hardware_g = TRUE;
    reset_hdf5();

    /* Test software floating-point conversion functions */
    nerrors += run_fp_tests("soft");

    /* Test software integer conversion functions */
    nerrors += test_conv_int_2();
    nerrors += run_integer_tests("soft");

    /* Test software float-integer conversion functions */
    nerrors += run_fp_int_conv("soft");

    /* Test software integer-float conversion functions */
    nerrors += run_int_fp_conv("soft");

    reset_hdf5();

    if (nerrors) {
        printf("***** %lu FAILURE%s! *****\n",
               nerrors, 1==nerrors?"":"S");
        HDexit(1);
    }
    printf("All data type tests passed.\n");
    return 0;
}
