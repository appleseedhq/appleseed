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
 *              Tuesday, December 22, 1998
 */
#include "h5test.h"
#define CPTR(VAR,CONST)	((VAR)=(CONST),&(VAR))

const char *FILENAME[] = {
    "enum1",
    NULL
};

typedef enum {
    E1_RED,
    E1_GREEN,
    E1_BLUE,
    E1_WHITE,
    E1_BLACK
} c_e1;


/*-------------------------------------------------------------------------
 * Function:	test_named
 *
 * Purpose:	Create an enumeration data type and store it in the file.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Wednesday, December 23, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_named(hid_t file)
{
    hid_t	type = -1, cwg = -1;
    c_e1	val;
    signed char	val8;

    TESTING("named enumeration types");
    if((cwg = H5Gcreate2(file, "test_named", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* A native integer */
    if((type = H5Tcreate(H5T_ENUM, sizeof(c_e1))) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(type, "RED",   CPTR(val, E1_RED  )) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(type, "GREEN", CPTR(val, E1_GREEN)) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(type, "BLUE",  CPTR(val, E1_BLUE )) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(type, "WHITE", CPTR(val, E1_WHITE)) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(type, "BLACK", CPTR(val, E1_BLACK)) < 0) FAIL_STACK_ERROR
    if(H5Tcommit2(cwg, "e1_a", type, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Tclose(type) < 0) FAIL_STACK_ERROR

    /* A smaller type */
    if((type = H5Tcreate(H5T_ENUM, (size_t)1)) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(type, "RED",   CPTR(val8, E1_RED  )) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(type, "GREEN", CPTR(val8, E1_GREEN)) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(type, "BLUE",  CPTR(val8, E1_BLUE )) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(type, "WHITE", CPTR(val8, E1_WHITE)) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(type, "BLACK", CPTR(val8, E1_BLACK)) < 0) FAIL_STACK_ERROR
    if(H5Tcommit2(cwg, "e1_b", type, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Tclose(type) < 0) FAIL_STACK_ERROR

    /* A non-native type */
    if(H5T_ORDER_BE == H5Tget_order(H5T_NATIVE_INT)) {
	if ((type = H5Tenum_create(H5T_STD_U8LE)) < 0) FAIL_STACK_ERROR
    } else {
	if ((type = H5Tenum_create(H5T_STD_U8BE)) < 0) FAIL_STACK_ERROR
    }
    if(H5Tenum_insert(type, "RED",   CPTR(val8, E1_RED  )) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(type, "GREEN", CPTR(val8, E1_GREEN)) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(type, "BLUE",  CPTR(val8, E1_BLUE )) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(type, "WHITE", CPTR(val8, E1_WHITE)) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(type, "BLACK", CPTR(val8, E1_BLACK)) < 0) FAIL_STACK_ERROR
    if(H5Tcommit2(cwg, "e1_c", type, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Tclose(type) < 0) FAIL_STACK_ERROR

    if(H5Gclose(cwg) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Tclose(type);
	H5Gclose(cwg);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_noconv
 *
 * Purpose:	Tests creation of datasets when no conversion is present.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Monday, January  4, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_noconv(hid_t file)
{
    hid_t	cwg=-1, type=-1, space=-1, dset=-1;
    c_e1	val;
    static c_e1	data1[]={E1_RED,   E1_GREEN, E1_BLUE,  E1_GREEN, E1_WHITE,
			 E1_WHITE, E1_BLACK, E1_GREEN, E1_BLUE,  E1_RED,
			 E1_RED,   E1_BLUE,  E1_GREEN, E1_BLACK, E1_WHITE,
			 E1_RED,   E1_WHITE, E1_GREEN, E1_GREEN, E1_BLUE};
    c_e1	data2[NELMTS(data1)];
    hsize_t	ds_size[1]={NELMTS(data1)};
    size_t	i;

    TESTING("no-conversion datasets");

    if((cwg = H5Gcreate2(file, "test_noconv", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    if((type = H5Tcreate(H5T_ENUM, sizeof(c_e1))) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(type, "RED",   CPTR(val, E1_RED  )) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(type, "GREEN", CPTR(val, E1_GREEN)) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(type, "BLUE",  CPTR(val, E1_BLUE )) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(type, "WHITE", CPTR(val, E1_WHITE)) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(type, "BLACK", CPTR(val, E1_BLACK)) < 0) FAIL_STACK_ERROR

    if((space = H5Screate_simple(1, ds_size, NULL)) < 0) FAIL_STACK_ERROR
    if((dset = H5Dcreate2(cwg, "color_table", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Dwrite(dset, type, space, space, H5P_DEFAULT, data1) < 0) FAIL_STACK_ERROR
    if(H5Dread(dset, type, space, space, H5P_DEFAULT, data2) < 0) FAIL_STACK_ERROR

    for(i = 0; i < (size_t)ds_size[0]; i++)
	if(data1[i] != data2[i]) {
	    H5_FAILED();
	    printf("    data1[%lu]=%d, data2[%lu]=%d (should be same)\n",
		   (unsigned long)i, (int)(data1[i]),
		   (unsigned long)i, (int)(data2[i]));
	    goto error;
	} /* end if */

    if(H5Dclose(dset) < 0) FAIL_STACK_ERROR
    if(H5Sclose(space) < 0) FAIL_STACK_ERROR
    if(H5Tclose(type) < 0) FAIL_STACK_ERROR
    if(H5Gclose(cwg) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Dclose(dset);
	H5Sclose(space);
	H5Tclose(type);
	H5Gclose(cwg);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_tr1
 *
 * Purpose:	Writes enumerated data to a dataset which requires
 *		translation. Both memory and file data types use native
 *		integers but the file type has a different mapping between
 *		the integers and symbols.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Monday, January  4, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_tr1(hid_t file)
{
    hid_t	cwg = -1, m_type = -1, f_type = -1, space = -1, dset = -1;
    hsize_t	ds_size[1] = {10};
    size_t	i;
    c_e1	eval;
    int		ival;
    static c_e1	data1[10] = {E1_RED,   E1_GREEN, E1_BLUE,  E1_GREEN, E1_WHITE,
			   E1_WHITE, E1_BLACK, E1_GREEN, E1_BLUE,  E1_RED};
    c_e1	data2[10];

    TESTING("O(1) conversions");

    if((cwg = H5Gcreate2(file, "test_tr1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    if((m_type = H5Tcreate(H5T_ENUM, sizeof(c_e1))) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(m_type, "RED",   CPTR(eval, E1_RED  )) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(m_type, "GREEN", CPTR(eval, E1_GREEN)) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(m_type, "BLUE",  CPTR(eval, E1_BLUE )) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(m_type, "WHITE", CPTR(eval, E1_WHITE)) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(m_type, "BLACK", CPTR(eval, E1_BLACK)) < 0) FAIL_STACK_ERROR


    if((f_type = H5Tcreate(H5T_ENUM, sizeof(c_e1))) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(f_type, "RED",   CPTR(ival, 105)) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(f_type, "GREEN", CPTR(ival, 104)) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(f_type, "BLUE",  CPTR(ival, 103)) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(f_type, "WHITE", CPTR(ival, 102)) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(f_type, "BLACK", CPTR(ival, 101)) < 0) FAIL_STACK_ERROR

    if((space = H5Screate_simple(1, ds_size, NULL)) < 0) FAIL_STACK_ERROR
    if((dset = H5Dcreate2(cwg, "color_table", f_type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Dwrite(dset, m_type, space, space, H5P_DEFAULT, data1) < 0) FAIL_STACK_ERROR
    if(H5Dread(dset, m_type, space, space, H5P_DEFAULT, data2) < 0) FAIL_STACK_ERROR

    for(i = 0; i < (size_t)ds_size[0]; i++)
        if(data1[i] != data2[i]) {
            H5_FAILED();
            printf("    data1[%lu]=%d, data2[%lu]=%d (should be same)\n",
                    (unsigned long)i, (int)(data1[i]),
                    (unsigned long)i, (int)(data2[i]));
            goto error;
        }

    if(H5Dclose(dset) < 0) FAIL_STACK_ERROR
    if(H5Sclose(space) < 0) FAIL_STACK_ERROR
    if(H5Tclose(m_type) < 0) FAIL_STACK_ERROR
    if(H5Tclose(f_type) < 0) FAIL_STACK_ERROR
    if(H5Gclose(cwg) < 0) FAIL_STACK_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
        H5Sclose(space);
        H5Tclose(m_type);
        H5Tclose(f_type);
        H5Gclose(cwg);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_tr2
 *
 * Purpose:	Tests conversions that use the O(log N) lookup function.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Tuesday, January  5, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_tr2(hid_t file)
{
    hid_t	cwg = -1, m_type = -1, f_type = -1, space = -1, dset = -1;
    hsize_t	ds_size[1] = {10};
    size_t	i;
    c_e1	val1;
    int		val2;
    static c_e1	data1[10] = {E1_RED,   E1_GREEN, E1_BLUE,  E1_GREEN, E1_WHITE,
			   E1_WHITE, E1_BLACK, E1_GREEN, E1_BLUE,  E1_RED};
    c_e1	data2[10];

    TESTING("O(log N) converions");

    if((cwg = H5Gcreate2(file, "test_tr2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    if((m_type = H5Tcreate(H5T_ENUM, sizeof(c_e1))) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(m_type, "RED",   CPTR(val1, E1_RED  )) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(m_type, "GREEN", CPTR(val1, E1_GREEN)) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(m_type, "BLUE",  CPTR(val1, E1_BLUE )) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(m_type, "WHITE", CPTR(val1, E1_WHITE)) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(m_type, "BLACK", CPTR(val1, E1_BLACK)) < 0) FAIL_STACK_ERROR

    if((f_type = H5Tcreate(H5T_ENUM, sizeof(int))) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(f_type, "RED",   CPTR(val2, 1050)) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(f_type, "GREEN", CPTR(val2, 1040)) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(f_type, "BLUE",  CPTR(val2, 1030)) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(f_type, "WHITE", CPTR(val2, 1020)) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(f_type, "BLACK", CPTR(val2, 1010)) < 0) FAIL_STACK_ERROR

    if((space = H5Screate_simple(1, ds_size, NULL)) < 0) FAIL_STACK_ERROR
    if((dset = H5Dcreate2(cwg, "color_table", f_type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Dwrite(dset, m_type, space, space, H5P_DEFAULT, data1) < 0) FAIL_STACK_ERROR
    if(H5Dread(dset, m_type, space, space, H5P_DEFAULT, data2) < 0) FAIL_STACK_ERROR

    for(i = 0; i < (size_t)ds_size[0]; i++)
        if(data1[i] != data2[i]) {
            H5_FAILED();
            printf("    data1[%lu]=%d, data2[%lu]=%d (should be same)\n",
                    (unsigned long)i, (int)(data1[i]),
                    (unsigned long)i, (int)(data2[i]));
            goto error;
        }

    if(H5Dclose(dset) < 0) FAIL_STACK_ERROR
    if(H5Sclose(space) < 0) FAIL_STACK_ERROR
    if(H5Tclose(m_type) < 0) FAIL_STACK_ERROR
    if(H5Tclose(f_type) < 0) FAIL_STACK_ERROR
    if(H5Gclose(cwg) < 0) FAIL_STACK_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
        H5Sclose(space);
        H5Tclose(m_type);
        H5Tclose(f_type);
        H5Gclose(cwg);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_value_dsnt_exist
 *
 * Purpose:	Create an enumeration datatype with "gaps in values"
 *              and then request a name of non-existing value within
 *              an existing range by calling H5Tenum_nameof function.
 *              Function should fail instead of succeeding and returning
 *              a name of one of the existing values.
 *              Request a value by supplying non-existing name by calling
 *              H5Tenum_nameof function. Function should fail.
 *
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Elena Pourmal
 *              Wednesday, June 7, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_value_dsnt_exist(void)
{

    hid_t       datatype_id=(-1);  /* identifiers */
    int val;
    char nam[100];
    size_t size = 100;
    TESTING("for non-existing name and value");
    /* Turn off error reporting since we expect failure in this test */

    if (H5Eset_auto2(H5E_DEFAULT, NULL, NULL) < 0) goto error;

    if ((datatype_id = H5Tenum_create(H5T_NATIVE_INT))< 0) goto error;

    /* These calls should fail, since no memebrs exist yet */
    if (H5Tenum_valueof(datatype_id, "SAX", &val) >= 0) goto error;
    val = 3;
    if (H5Tenum_nameof(datatype_id, &val, nam, size) >= 0) goto error;

    val = 2;
    if (H5Tenum_insert(datatype_id, "TWO", (int *)&val) < 0) goto error;
    val = 6;
    if (H5Tenum_insert(datatype_id, "SIX", (int *)&val) < 0) goto error;
    val = 10;
    if (H5Tenum_insert(datatype_id, "TEN", (int *)&val) < 0) goto error;

    /* This call should fail since we did not create a member with value = 3*/
    val = 3;
    if (H5Tenum_nameof(datatype_id, &val, nam, size) >= 0) goto error;

    /* This call should fail since we did not create a member with value = 11*/
    val = 11;
    if (H5Tenum_nameof(datatype_id, &val, nam, size) >= 0) goto error;

    /* This call should fail since we did not create a member with value = 0*/
    val = 0;
    if (H5Tenum_nameof(datatype_id, &val, nam, size) >= 0) goto error;

    /* This call should fail since we do not have SAX name in the type */
    if (H5Tenum_valueof(datatype_id, "SAX", &val) >= 0) goto error;

    /* This call should fail since we do not have TEEN name in the type */
    if (H5Tenum_valueof(datatype_id, "TEEN", &val) >= 0) goto error;

    /* This call should fail since we do not have A name in the type */
    if (H5Tenum_valueof(datatype_id, "A", &val) >= 0) goto error;

    if (H5Tclose(datatype_id) < 0) goto error;
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Tclose(datatype_id);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_funcs
 *
 * Purpose:	Create an enumeration data type and test some functions
 *              that are or aren't supposed to work with it.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Raymond Lu
 *              Tuesday, April 4, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_funcs(void)
{
    hid_t	type=-1;
    c_e1	val;
    size_t      size;
    H5T_pad_t   inpad;
    H5T_cset_t  cset;
    herr_t      ret;

    TESTING("some functions with enumeration types");

    /* A native integer */
    if((type = H5Tcreate(H5T_ENUM, sizeof(c_e1))) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(type, "RED",   CPTR(val, E1_RED  )) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(type, "GREEN", CPTR(val, E1_GREEN)) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(type, "BLUE",  CPTR(val, E1_BLUE )) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(type, "WHITE", CPTR(val, E1_WHITE)) < 0) FAIL_STACK_ERROR
    if(H5Tenum_insert(type, "BLACK", CPTR(val, E1_BLACK)) < 0) FAIL_STACK_ERROR

    if(H5Tget_precision(type) == 0) FAIL_STACK_ERROR
    if(H5Tget_size(type) == 0) FAIL_STACK_ERROR
    if(H5Tget_offset(type) < 0) FAIL_STACK_ERROR
    if(H5Tget_sign(type) < 0) FAIL_STACK_ERROR
    if(H5Tget_super(type) < 0) FAIL_STACK_ERROR

    H5E_BEGIN_TRY {
        ret=H5Tset_pad(type, H5T_PAD_ZERO, H5T_PAD_ONE);
    } H5E_END_TRY;
    if (ret>=0) {
        H5_FAILED();
        printf("Operation not allowed for this type.\n");
        goto error;
    } /* end if */

    H5E_BEGIN_TRY {
        size=H5Tget_ebias(type);
    } H5E_END_TRY;
    if (size>0) {
        H5_FAILED();
        printf("Operation not allowed for this type.\n");
        goto error;
    } /* end if */

    H5E_BEGIN_TRY {
        inpad=H5Tget_inpad(type);
    } H5E_END_TRY;
    if (inpad>-1) {
        H5_FAILED();
        printf("Operation not allowed for this type.\n");
        goto error;
    } /* end if */

    H5E_BEGIN_TRY {
        cset=H5Tget_cset(type);
    } H5E_END_TRY;
    if (cset>-1) {
        H5_FAILED();
        printf("Operation not allowed for this type.\n");
        goto error;
    } /* end if */

    size = 16;
    H5E_BEGIN_TRY {
        ret=H5Tset_offset(type, (size_t)size);
    } H5E_END_TRY;
    if (ret>=0) {
        H5_FAILED();
        printf("Operation not allowed for this type.\n");
        goto error;
    } /* end if */

    H5E_BEGIN_TRY {
        ret=H5Tset_order(type, H5T_ORDER_BE);
    } H5E_END_TRY;
    if (ret>=0) {
        H5_FAILED();
        printf("Operation not allowed for this type.\n");
        goto error;
    } /* end if */

    if (H5Tclose(type)<0) goto error;

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Tclose(type);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:
 *
 * Return:	Success:
 *
 *		Failure:
 *
 * Programmer:	Robb Matzke
 *              Tuesday, December 22, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t	fapl=-1, file=-1;
    char	name[1024];
    int		nerrors=0;

    h5_reset();
    fapl = h5_fileaccess();

    /* Create the file */
    h5_fixname(FILENAME[0], fapl, name, sizeof name);
    if ((file=H5Fcreate(name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) goto error;

    /* Tests */
    nerrors += test_named(file);
    nerrors += test_noconv(file);
    nerrors += test_tr1(file);
    nerrors += test_tr2(file);
    nerrors += test_value_dsnt_exist();
    nerrors += test_funcs();

    H5Fclose(file);

    /* Verify symbol table messages are cached */
    nerrors += (h5_verify_cached_stabs(FILENAME, fapl) < 0 ? 1 : 0);

    if (nerrors) goto error;
    puts("All enum tests passed.");
    h5_cleanup(FILENAME, fapl);
    return 0;

 error:
    puts("*** ENUM TESTS FAILED ***");
    return 1;
}
