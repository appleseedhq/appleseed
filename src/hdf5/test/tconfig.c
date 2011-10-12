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

/***********************************************************
*
* Test program:  tconfig
*
* Test the definitions in the H5config.h as much as possible
*
*************************************************************/

#include "hdf5.h"
#include "testhdf5.h"

/* macros definitions */
/* verify C int type: verify the size of signed and unsigned int type
 * with the macro size.
 */
#define vrfy_cint_type(ctype, uctype, ctype_macro) \
    /* check signed type size */ \
    vrfy_macrosize(ctype, ctype_macro, #ctype_macro);\
    /* check unsigned type size */ \
    vrfy_macrosize(uctype, ctype_macro, #ctype_macro);

/* verify C type sizes: verify the sizeof type with the macro size. */
#define vrfy_ctype(type, macro) \
    vrfy_macrosize(type, macro, #macro);

/* verify if the sizeof(type) matches size defined in macro. */
/* Needs this extra step so that we can print the macro name. */
#define vrfy_macrosize(type, macro, macroname) \
    if (sizeof(type) != macro) \
	TestErrPrintf("Error: sizeof(%s) is %d but %s is %d\n", \
	    #type, sizeof(type), macroname, macro);

/* local routine prototypes */
void test_config_ctypes(void);
void test_exit_definitions(void);


/*-------------------------------------------------------------------------
 * Function:	test_configure
 *
 * Purpose:	Main configure definitions testing routine
 *
 * Return:	none (error is fed back via global variable num_errs)
 *
 * Programmer:	Albert Cheng
 *              September 25, 2001
 *
 * Modifications:
 *              Raymond Lu
 *              16 Dec 2009
 *              On Boeing's OpenVMS, the value of EXIT_FAILURE is 268435458.
 *              (The test is in test_exit_definitions.)  Their document says
 *              it's supposed to be 2.  I commented it out for further
 *              consideration.
 *-------------------------------------------------------------------------
 */
void
test_configure(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing configure definitions\n"));
    test_config_ctypes();
#ifndef H5_VMS
    test_exit_definitions();
#endif
}


/*-------------------------------------------------------------------------
 * Function:	cleanup_configure
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	Albert Cheng
 *              September 25, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_configure(void)
{
    /* no file to clean */
}


/*-------------------------------------------------------------------------
 * Function:	test_config_ctypes
 *
 * Purpose:	test C language data type sizes
 *
 * Return:	none (error is fed back via global variable num_errs)
 *
 * Programmer:	Albert Cheng
 *              September 25, 2001
 *
 * Modifications:
 *	Albert Cheng, 2004/10/14
 *	Verified both signed and unsigned int types.
 *
 *-------------------------------------------------------------------------
 */
void
test_config_ctypes(void)
{
    /* standard C89 basic types */
    /* char, signed char,  unsigned char are three distinct types. */
    vrfy_ctype(char, H5_SIZEOF_CHAR);
    vrfy_cint_type(signed char, unsigned char, H5_SIZEOF_CHAR);
    vrfy_cint_type(int, unsigned int, H5_SIZEOF_INT);
    vrfy_cint_type(short, unsigned short, H5_SIZEOF_SHORT);
    vrfy_cint_type(long, unsigned long, H5_SIZEOF_LONG);
    vrfy_ctype(float, H5_SIZEOF_FLOAT);
    vrfy_ctype(double, H5_SIZEOF_DOUBLE);
#if H5_SIZEOF_LONG_DOUBLE >0
    vrfy_ctype(long double, H5_SIZEOF_LONG_DOUBLE);
#endif

    /* standard C99 basic types */
#if H5_SIZEOF_LONG_LONG > 0
    vrfy_cint_type(long long, unsigned long long, H5_SIZEOF_LONG_LONG);
#endif

#if H5_SIZEOF_INT8_T > 0
    vrfy_cint_type(int8_t, uint8_t, H5_SIZEOF_INT8_T);
#endif

#if H5_SIZEOF_INT16_T > 0
    vrfy_cint_type(int16_t, uint16_t, H5_SIZEOF_INT16_T);
#endif

#if H5_SIZEOF_INT32_T > 0
    vrfy_cint_type(int32_t, uint32_t, H5_SIZEOF_INT32_T);
#endif

#if H5_SIZEOF_INT64_T > 0
    vrfy_cint_type(int64_t, uint64_t, H5_SIZEOF_INT64_T);
#endif

    /* Some vendors have different sizes for the signed and unsigned */
    /* fast8_t.  Need to check them individually. */
#if H5_SIZEOF_INT_FAST8_T > 0
    vrfy_ctype(int_fast8_t, H5_SIZEOF_INT_FAST8_T);
#endif

#if H5_SIZEOF_UINT_FAST8_T > 0
    vrfy_ctype(uint_fast8_t, H5_SIZEOF_UINT_FAST8_T);
#endif

#if H5_SIZEOF_INT_FAST16_T > 0
    vrfy_cint_type(int_fast16_t, uint_fast16_t, H5_SIZEOF_INT_FAST16_T);
#endif

#if H5_SIZEOF_INT_FAST32_T > 0
    vrfy_cint_type(int_fast32_t, uint_fast32_t, H5_SIZEOF_INT_FAST32_T);
#endif

#if H5_SIZEOF_INT_FAST64_T > 0
    vrfy_cint_type(int_fast64_t, uint_fast64_t, H5_SIZEOF_INT_FAST64_T);
#endif

#if H5_SIZEOF_INT_LEAST8_T > 0
    vrfy_cint_type(int_least8_t, uint_least8_t, H5_SIZEOF_INT_LEAST8_T);
#endif

#if H5_SIZEOF_INT_LEAST16_T > 0
    vrfy_cint_type(int_least16_t, uint_least16_t, H5_SIZEOF_INT_LEAST16_T);
#endif

#if H5_SIZEOF_INT_LEAST32_T > 0
    vrfy_cint_type(int_least32_t, uint_least32_t, H5_SIZEOF_INT_LEAST32_T);
#endif

#if H5_SIZEOF_INT_LEAST64_T > 0
    vrfy_cint_type(int_least64_t, uint_least64_t, H5_SIZEOF_INT_LEAST64_T);
#endif

    /* pseudo standard basic types */
#if H5_SIZEOF___INT64 > 0
    vrfy_cint_type(__int64, unsigned __int64, H5_SIZEOF___INT64);
#endif

#if H5_SIZEOF_OFF_T > 0
    vrfy_ctype(off_t, H5_SIZEOF_OFF_T);
#endif

#if H5_SIZEOF_SIZE_T > 0
    vrfy_ctype(size_t, H5_SIZEOF_SIZE_T);
#endif

#if H5_SIZEOF_SSIZE_T > 0
    vrfy_ctype(ssize_t, H5_SIZEOF_SSIZE_T);
#endif

}


/*-------------------------------------------------------------------------
 * Function:	test_exit_definitions
 *
 * Purpose:	test the exit macros values
 *
 * Return:	none (error is fed back via global variable num_errs)
 *
 * Programmer:	Albert Cheng
 *              October 12, 2009
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
test_exit_definitions(void)
{
    /* Verify the EXIT_SUCCESS and EXIT_FAILURE are 0 and 1 respectively. */
    /* This should be true for POSIX compliant systems. */
    if (EXIT_SUCCESS != 0) \
	TestErrPrintf("Error: EXIT_SUCCESS is %d, should be %d\n", \
	    EXIT_SUCCESS, 0);
    if (EXIT_FAILURE != 1) \
	TestErrPrintf("Error: EXIT_FAILURE is %d, should be %d\n", \
	    EXIT_FAILURE, 1);
}
