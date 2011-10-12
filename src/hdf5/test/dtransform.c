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

#include "h5test.h"

#define ROWS    12
#define COLS    18
#define FLOAT_TOL 0.0001

static int init_test(hid_t file_id);
static int test_copy(const hid_t dxpl_id_c_to_f_copy, const hid_t dxpl_id_polynomial_copy);
static int test_trivial(const hid_t dxpl_id_simple);
static int test_poly(const hid_t dxpl_id_polynomial);
static int test_set(void);
static int test_getset(const hid_t dxpl_id_simple);

/* These are needed for multiple tests, so are declared here globally and are init'ed in init_test */
hid_t dset_id_int = -1;
hid_t dset_id_float = -1;
hid_t dset_id_int_chunk = -1;
hid_t dset_id_float_chunk = -1;


const float windchillFfloat[ROWS][COLS] =
    {   {36.0, 31.0, 25.0, 19.0, 13.0, 7.0, 1.0, -5.0, -11.0, -16.0, -22.0, -28.0, -34.0, -40.0, -46.0, -52.0, -57.0, -63.0 },
	{34.0, 27.0, 21.0, 15.0, 9.0, 3.0, -4.0, -10.0, -16.0, -22.0, -28.0, -35.0, -41.0, -47.0, -53.0, -59.0, -66.0, -72.0 } ,
	{32.0, 25.0, 19.0, 13.0, 6.0, 0.0, -7.0, -13.0, -19.0, -26.0, -32.0, -39.0, -45.0, -51.0, -58.0, -64.0, -71.0, -77.0 },
	{30.0, 24.0, 17.0, 11.0, 4.0, -2.0, -9.0, -15.0, -22.0, -29.0, -35.0, -42.0, -48.0, -55.0, -61.0, -68.0, -74.0, -81.0 },
	{29.0, 23.0, 16.0, 9.0, 3.0, -4.0, -11.0, -17.0, -24.0, -31.0, -37.0, -44.0, -51.0, -58.0, -64.0, -71.0, -78.0, -84.0 },
	{28.0, 22.0, 15.0, 8.0, 1.0, -5.0, -12.0, -19.0, -26.0, -33.0, -39.0, -46.0, -53.0, -60.0, -67.0, -73.0, -80.0, -87.0 },
	{28.0, 21.0, 14.0, 7.0, 0.0, -7.0, -14.0, -21.0, -27.0, -34.0, -41.0, -48.0, -55.0, -62.0, -69.0, -76.0, -82.0, -89.0 },
	{27.0, 20.0, 13.0, 6.0, -1.0, -8.0, -15.0, -22.0, -29.0, -36.0, -43.0, -50.0, -57.0, -64.0, -71.0, -78.0, -84.0, -91.0 },
	{26.0, 19.0, 12.0, 5.0, -2.0, -9.0, -16.0, -23.0, -30.0, -37.0, -44.0, -51.0, -58.0, -65.0, -72.0, -79.0, -86.0, -93.0 },
	{26.0, 19.0, 12.0, 4.0, -3.0, -10.0, -17.0, -24.0, -31.0, -38.0, -45.0, -52.0, -60.0, -67.0, -74.0, -81.0, -88.0, -95.0},
	{25.0, 18.0, 11.0, 4.0, -3.0, -11.0, -18.0, -25.0, -32.0, -39.0, -46.0, -54.0, -61.0, -68.0, -75.0, -82.0, -89.0, -97.0},
	{25.0, 17.0, 10.0, 3.0, -4.0, -11.0, -19.0, -26.0, -33.0, -40.0, -48.0, -55.0, -62.0, -69.0, -76.0, -84.0, -91.0, -98.0}
    };

const int transformData[ROWS][COLS] =
    {   {36, 31, 25, 19, 13, 7, 1, 5, 11, 16, 22, 28, 34, 40, 46, 52, 57, 63 },
        {34, 27, 21, 15, 9, 3, 4, 10, 16, 22, 28, 35, 41, 47, 53, 59, 66, 0 } ,
        {32, 25, 19, 13, 6, 0, 7, 13, 19, 26, 32, 39, 45, 51, 58, 64, 71, 5 },
        {30, 24, 17, 11, 4, 2, 9, 15, 22, 29, 35, 42, 48, 55, 61, 68, 2, 9 },
        {29, 23, 16, 9, 3, 4, 11, 17, 24, 31, 37, 44, 51, 58, 64, 71, 6, 12 },
        {28, 22, 15, 8, 1, 5, 12, 19, 26, 33, 39, 46, 53, 60, 67, 1, 8, 15 },
        {28, 21, 14, 7, 0, 7, 14, 21, 27, 34, 41, 48, 55, 62, 69, 4, 10, 17 },
        {27, 20, 13, 6, 1, 8, 15, 22, 29, 36, 43, 50, 57, 64, 71, 6, 12, 19 },
        {26, 19, 12, 5, 2, 9, 16, 23, 30, 37, 44, 51, 58, 65, 0, 7, 14, 21 },
        {26, 19, 12, 4, 3, 10, 17, 24, 31, 38, 45, 52, 60, 67, 2, 9, 16, 23},
        {25, 18, 11, 4, 3, 11, 18, 25, 32, 39, 46, 54, 61, 68, 3, 10, 17, 25},
        {25, 17, 10, 3, 4, 11, 19, 26, 33, 40, 48, 55, 62, 69, 4, 12, 19, 26}
    };

#define UCOMPARE(TYPE,VAR1,VAR2,TOL)			\
{							\
    size_t i,j;						\
							\
    for(i=0; i<ROWS; i++)				\
        for(j=0; j<COLS; j++)				\
        {							\
            if(!( (((VAR1)[i][j] >= (TYPE)((VAR2)[i][j])) && ( ((VAR1)[i][j] - TOL) < (TYPE)((VAR2)[i][j]))) || (  ((VAR1)[i][j] <= (TYPE)((VAR2)[i][j])) && ( ((VAR1)[i][j] + TOL) > (TYPE)((VAR2)[i][j])))))	\
            {						\
                H5_FAILED();				\
                fprintf(stderr, "    ERROR: Conversion failed to match computed data\n");	\
                goto error;					\
            }						\
        }							\
    PASSED();						\
}

#define COMPARE(TYPE,VAR1,VAR2,TOL)			\
{							\
    size_t i,j;						\
							\
    for(i=0; i<ROWS; i++)				\
        for(j=0; j<COLS; j++)				\
        {							\
            if( !(((VAR1)[i][j] <= ((TYPE)(VAR2)[i][j] + TOL)) && ((VAR1)[i][j] >= ((TYPE)(VAR2)[i][j] - TOL))) )	\
            {						\
                H5_FAILED();				\
                fprintf(stderr, "    ERROR: Conversion failed to match computed data\n");	\
                goto error;					\
            }						\
        }							\
    PASSED();						\
}

#define TEST_TYPE_CONTIG(XFORM, TYPE, HDF_TYPE, TEST_STR, COMPARE_DATA, SIGNED)	\
{										\
    TYPE array[ROWS][COLS];							\
    const char* f_to_c = "(5/9.0)*(x-32)";					\
    /* utrans is a transform for unsigned types: no negative numbers involved and results are < 255 to fit into uchar */ \
    const char* utrans = "((x+100)/4)*3";					\
										\
    hid_t dataspace, dxpl_id_f_to_c, dxpl_id_utrans, dset, dset_nn, dt_nn;	\
    H5T_order_t order;                                                          \
    hsize_t dim[2] = {ROWS, COLS};						\
										\
    if((dataspace = H5Screate_simple(2, dim, NULL)) < 0) TEST_ERROR;		\
    if((dset = H5Dcreate2(file_id, "/transformtest_"TEST_STR, HDF_TYPE, dataspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR; \
                                                                                \
    if((dt_nn = H5Tcopy(HDF_TYPE)) < 0) TEST_ERROR                              \
    if((order = H5Tget_order(dt_nn)) == H5T_ORDER_ERROR) TEST_ERROR             \
    if(H5Tset_order(dt_nn, order == H5T_ORDER_LE ? H5T_ORDER_BE : H5T_ORDER_LE) < 0) TEST_ERROR \
    if((dset_nn = H5Dcreate2(file_id, "/nonnative_transformtest_"TEST_STR, dt_nn, dataspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR	\
    if(H5Tclose(dt_nn) < 0) TEST_ERROR                                          \
										\
    if(SIGNED)									\
    {										\
	if((dxpl_id_f_to_c = H5Pcreate(H5P_DATASET_XFER)) < 0) TEST_ERROR;	\
	if(H5Pset_data_transform(dxpl_id_f_to_c, f_to_c) < 0) TEST_ERROR;	\
	if(H5Dwrite(dset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, dxpl_id_f_to_c, windchillFfloat) < 0) TEST_ERROR;	\
	if(H5Dwrite(dset_nn, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, dxpl_id_f_to_c, windchillFfloat) < 0) TEST_ERROR;	\
	if(H5Pclose(dxpl_id_f_to_c) < 0) TEST_ERROR;			\
    }										\
    else									\
    {										\
	if((dxpl_id_utrans = H5Pcreate(H5P_DATASET_XFER)) < 0) TEST_ERROR;	\
	if(H5Pset_data_transform(dxpl_id_utrans, utrans) < 0) TEST_ERROR;	\
	if(H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl_id_utrans, transformData) < 0) TEST_ERROR;	\
	if(H5Dwrite(dset_nn, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl_id_utrans, transformData) < 0) TEST_ERROR;	\
	if(H5Pclose(dxpl_id_utrans) < 0) TEST_ERROR;			\
    }										\
										\
										\
    TESTING("contiguous, no data type conversion ("TEST_STR"->"TEST_STR")")	\
										\
    if(H5Dread(dset, HDF_TYPE, H5S_ALL, H5S_ALL, XFORM, array) < 0) TEST_ERROR;	\
    if(SIGNED)									\
        COMPARE(TYPE, array, COMPARE_DATA, 2)					\
    else									\
        UCOMPARE(TYPE, array, COMPARE_DATA, 4)					\
                                                                                \
    TESTING("contiguous, byte order conversion ("TEST_STR"->"TEST_STR")")	\
										\
    if(H5Dread(dset_nn, HDF_TYPE, H5S_ALL, H5S_ALL, XFORM, array) < 0) TEST_ERROR;	\
    if(SIGNED)									\
        COMPARE(TYPE, array, COMPARE_DATA, 2)					\
    else									\
        UCOMPARE(TYPE, array, COMPARE_DATA, 4)					\
										\
    if(SIGNED)									\
    {    									\
        TESTING("contiguous, with type conversion (float->"TEST_STR")")	\
										\
	if(H5Dread(dset_id_float, HDF_TYPE, H5S_ALL, H5S_ALL, XFORM, array) < 0) TEST_ERROR;	\
	COMPARE(TYPE, array, COMPARE_DATA, 2)				\
    }										\
										\
   if(H5Dclose(dset) < 0) TEST_ERROR;					\
   if(H5Sclose(dataspace) < 0) TEST_ERROR;				\
}

#define TEST_TYPE_CHUNK(XFORM, TYPE, HDF_TYPE, TEST_STR, COMPARE_DATA, SIGNED)	\
{										\
    TYPE array[ROWS][COLS];							\
    const char* f_to_c = "(5/9.0)*(x-32)";					\
    /* utrans is a transform for unsigned types: no negative numbers involved and results are < 255 to fit into uchar */ \
    const char* utrans = "((x+100)/4)*3";					\
										\
    hid_t dataspace, dxpl_id_f_to_c, dxpl_id_utrans, cparms, memspace, dset_chunk, filespace;	\
    hsize_t dim[2] = {ROWS, COLS};						\
    hsize_t offset[2] = {0, 0};							\
										\
										\
    if((dataspace = H5Screate_simple(2, dim, NULL)) < 0) TEST_ERROR;		\
										\
    if((cparms = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR;		\
    if(H5Pset_chunk(cparms, 2, dim) < 0) TEST_ERROR;				\
										\
    if((dset_chunk = H5Dcreate2(file_id, "/transformtest_chunk_"TEST_STR, HDF_TYPE, dataspace, H5P_DEFAULT, cparms, H5P_DEFAULT)) < 0) TEST_ERROR;	\
    if((filespace = H5Dget_space(dset_chunk)) < 0) TEST_ERROR			\
    if((memspace = H5Screate_simple(2, dim, NULL)) < 0) TEST_ERROR		\
    if(H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, NULL, dim, NULL) < 0) TEST_ERROR;			\
    										\
    if(SIGNED)									\
    {										\
	if((dxpl_id_f_to_c = H5Pcreate(H5P_DATASET_XFER)) < 0) TEST_ERROR;	\
	if(H5Pset_data_transform(dxpl_id_f_to_c, f_to_c) < 0) TEST_ERROR;	\
	if(H5Dwrite(dset_chunk, H5T_NATIVE_FLOAT, dataspace, filespace, dxpl_id_f_to_c, windchillFfloat) < 0) TEST_ERROR;	\
	if(H5Pclose(dxpl_id_f_to_c) < 0) TEST_ERROR;				\
    }										\
    else									\
    {										\
	if((dxpl_id_utrans = H5Pcreate(H5P_DATASET_XFER)) < 0) TEST_ERROR;	\
	if(H5Pset_data_transform(dxpl_id_utrans, utrans) < 0) TEST_ERROR;	\
	if(H5Dwrite(dset_chunk, H5T_NATIVE_INT, dataspace, filespace, dxpl_id_utrans, transformData) < 0) TEST_ERROR;	\
	if(H5Pclose(dxpl_id_utrans) < 0) TEST_ERROR;				\
    }										\
										\
										\
    TESTING("chunked, no data type conversion ("TEST_STR"->"TEST_STR")")	\
										\
    if(H5Dread(dset_chunk, HDF_TYPE, memspace, filespace, XFORM, array) < 0) TEST_ERROR;	\
    if(SIGNED)									\
        COMPARE(TYPE, array, COMPARE_DATA, 2)					\
    else									\
        UCOMPARE(TYPE, array, COMPARE_DATA, 4)					\
										\
    if(SIGNED)									\
    {    									\
        TESTING("chunked, with type conversion (float->"TEST_STR")")		\
										\
	if(H5Dread(dset_id_float_chunk, HDF_TYPE, memspace, filespace, XFORM, array) < 0) TEST_ERROR;	\
	COMPARE(TYPE, array, COMPARE_DATA, 2)					\
    }										\
										\
										\
   if(H5Pclose(cparms) < 0) TEST_ERROR;						\
   if(H5Dclose(dset_chunk) < 0) TEST_ERROR;					\
   if(H5Sclose(dataspace) < 0) TEST_ERROR;					\
   if(H5Sclose(memspace) < 0) TEST_ERROR;					\
}

#define INVALID_SET_TEST(TRANSFORM)			\
{							\
    if(H5Pset_data_transform(dxpl_id, TRANSFORM) < 0)	\
    {							\
	PASSED();					\
    }							\
    else						\
    {							\
	H5_FAILED();					\
	fprintf(stderr, "    ERROR: Data transform allowed invalid TRANSFORM transform to be set\n");	\
	goto error;					\
    }							\
}

int main(void)
{
    hid_t dxpl_id_c_to_f = -1;
    hid_t dxpl_id_c_to_f_copy = 1;
    hid_t dxpl_id_simple = -1; 
    hid_t dxpl_id_polynomial = -1;
    hid_t dxpl_id_polynomial_copy = -1;
    hid_t dxpl_id_utrans_inv = -1;
    hid_t file_id = -1;

    const char* c_to_f = "(9/5.0)*x + 32";
    const char* simple = "(4/2) * ( (2 + 4)/(5 - 2.5))"; /* this equals 4.8 */
    const char* polynomial = "(2+x)* ((x-8)/2)";
    /* inverses the utrans transform in init_test to get back original array */
    const char* utrans_inv = "(x/3)*4 - 100";

    if((file_id = H5Fcreate("dtransform.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;

    if((dxpl_id_c_to_f = H5Pcreate(H5P_DATASET_XFER)) < 0) TEST_ERROR;
    if((dxpl_id_simple = H5Pcreate(H5P_DATASET_XFER)) < 0) TEST_ERROR;
    if((dxpl_id_utrans_inv = H5Pcreate(H5P_DATASET_XFER)) < 0) TEST_ERROR;
    if((dxpl_id_polynomial =  H5Pcreate(H5P_DATASET_XFER)) < 0) TEST_ERROR;
    if(H5Pset_data_transform(dxpl_id_c_to_f, c_to_f) < 0) TEST_ERROR;
    if(H5Pset_data_transform(dxpl_id_polynomial, polynomial) < 0) TEST_ERROR;
    if(H5Pset_data_transform(dxpl_id_simple, simple) < 0) TEST_ERROR;
    if(H5Pset_data_transform(dxpl_id_utrans_inv, utrans_inv) < 0) TEST_ERROR;
    if((dxpl_id_polynomial_copy = H5Pcopy(dxpl_id_polynomial)) < 0) TEST_ERROR;
    if((dxpl_id_c_to_f_copy = H5Pcopy(dxpl_id_c_to_f)) < 0) TEST_ERROR;

    /* Run all the tests */

    if(init_test(file_id) < 0) TEST_ERROR;
    if(test_set() < 0) TEST_ERROR;
    TEST_TYPE_CONTIG(dxpl_id_utrans_inv, char, H5T_NATIVE_CHAR, "char", transformData, 0);
    TEST_TYPE_CONTIG(dxpl_id_utrans_inv, unsigned char, H5T_NATIVE_UCHAR, "uchar", transformData, 0);
    TEST_TYPE_CONTIG(dxpl_id_c_to_f, signed char, H5T_NATIVE_SCHAR, "schar", windchillFfloat, 1);
    TEST_TYPE_CONTIG(dxpl_id_c_to_f, short, H5T_NATIVE_SHORT, "short", windchillFfloat, 1);
    TEST_TYPE_CONTIG(dxpl_id_utrans_inv, unsigned short, H5T_NATIVE_USHORT, "ushort", transformData, 0);
    TEST_TYPE_CONTIG(dxpl_id_c_to_f, int, H5T_NATIVE_INT, "int", windchillFfloat, 1);
    TEST_TYPE_CONTIG(dxpl_id_utrans_inv, unsigned int, H5T_NATIVE_UINT, "uint", transformData, 0);
    TEST_TYPE_CONTIG(dxpl_id_c_to_f, long, H5T_NATIVE_LONG, "long", windchillFfloat, 1);
    TEST_TYPE_CONTIG(dxpl_id_utrans_inv, unsigned long, H5T_NATIVE_ULONG, "ulong", transformData, 0);

#ifdef H5_LLONG_TO_FP_CAST_WORKS
    TEST_TYPE_CONTIG(dxpl_id_c_to_f, long long, H5T_NATIVE_LLONG, "llong", windchillFfloat, 1);
#else
    TESTING("contiguous, with type conversion (float->llong)")
    SKIPPED()
#endif

#ifdef H5_ULLONG_TO_FP_CAST_WORKS
    TEST_TYPE_CONTIG(dxpl_id_utrans_inv, unsigned long long, H5T_NATIVE_ULLONG, "ullong", transformData, 0);
#else
    TESTING("contiguous, with type conversion (float->ullong)")
    SKIPPED()
#endif
    TEST_TYPE_CONTIG(dxpl_id_c_to_f, float, H5T_NATIVE_FLOAT, "float", windchillFfloat, 1);
    TEST_TYPE_CONTIG(dxpl_id_c_to_f, double, H5T_NATIVE_DOUBLE, "double", windchillFfloat, 1);
#if H5_SIZEOF_LONG_DOUBLE!=0
    TEST_TYPE_CONTIG(dxpl_id_c_to_f, long double, H5T_NATIVE_LDOUBLE, "ldouble", windchillFfloat, 1);
#endif

    TEST_TYPE_CHUNK(dxpl_id_utrans_inv, char, H5T_NATIVE_CHAR, "char", transformData, 0);
    TEST_TYPE_CHUNK(dxpl_id_utrans_inv, unsigned char, H5T_NATIVE_UCHAR, "uchar", transformData, 0);
    TEST_TYPE_CHUNK(dxpl_id_c_to_f, signed char, H5T_NATIVE_SCHAR, "schar", windchillFfloat, 1);
    TEST_TYPE_CHUNK(dxpl_id_c_to_f, short, H5T_NATIVE_SHORT, "short", windchillFfloat, 1);
    TEST_TYPE_CHUNK(dxpl_id_utrans_inv, unsigned short, H5T_NATIVE_USHORT, "ushort", transformData, 0);
    TEST_TYPE_CHUNK(dxpl_id_c_to_f, int, H5T_NATIVE_INT, "int", windchillFfloat, 1);
    TEST_TYPE_CHUNK(dxpl_id_utrans_inv, unsigned int, H5T_NATIVE_UINT, "uint", transformData, 0);
    TEST_TYPE_CHUNK(dxpl_id_c_to_f, long, H5T_NATIVE_LONG, "long", windchillFfloat, 1);
    TEST_TYPE_CHUNK(dxpl_id_utrans_inv, unsigned long, H5T_NATIVE_ULONG, "ulong", transformData, 0);
#ifdef H5_LLONG_TO_FP_CAST_WORKS
    TEST_TYPE_CHUNK(dxpl_id_c_to_f, long long, H5T_NATIVE_LLONG, "llong", windchillFfloat, 1);
#else
    TESTING("chunked, with type conversion (float->llong)")
    SKIPPED()
#endif

#ifdef H5_ULLONG_TO_FP_CAST_WORKS
    TEST_TYPE_CHUNK(dxpl_id_utrans_inv, unsigned long long, H5T_NATIVE_ULLONG, "ullong", transformData, 0);
#else
    TESTING("chunked, with type conversion (float->ullong)")
    SKIPPED()
#endif
    TEST_TYPE_CHUNK(dxpl_id_c_to_f, float, H5T_NATIVE_FLOAT, "float", windchillFfloat, 1);
    TEST_TYPE_CHUNK(dxpl_id_c_to_f, double, H5T_NATIVE_DOUBLE, "double", windchillFfloat, 1);
#if H5_SIZEOF_LONG_DOUBLE!=0
    TEST_TYPE_CHUNK(dxpl_id_c_to_f, long double, H5T_NATIVE_LDOUBLE, "ldouble", windchillFfloat, 1);
#endif

    if(test_copy(dxpl_id_c_to_f_copy, dxpl_id_polynomial_copy) < 0) TEST_ERROR;
    if(test_trivial(dxpl_id_simple) < 0) TEST_ERROR;
    if(test_poly(dxpl_id_polynomial) < 0) TEST_ERROR;
    if(test_getset(dxpl_id_c_to_f) < 0) TEST_ERROR;

    /* Close the objects we opened/created */
    if(H5Dclose(dset_id_int) < 0) TEST_ERROR;
    if(H5Dclose(dset_id_int_chunk) < 0) TEST_ERROR;
    if(H5Dclose(dset_id_float) < 0) TEST_ERROR;
    if(H5Dclose(dset_id_float_chunk) < 0) TEST_ERROR;
    if(H5Fclose(file_id) < 0) TEST_ERROR;
    if(H5Pclose(dxpl_id_c_to_f) < 0) TEST_ERROR;
    if(H5Pclose(dxpl_id_c_to_f_copy) < 0) TEST_ERROR;
    if(H5Pclose(dxpl_id_polynomial) < 0) TEST_ERROR;
    if(H5Pclose(dxpl_id_polynomial_copy) < 0) TEST_ERROR;
    if(H5Pclose(dxpl_id_simple) < 0) TEST_ERROR;
    if(H5Pclose(dxpl_id_utrans_inv) < 0) TEST_ERROR;


   return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset_id_int);
        H5Dclose(dset_id_int_chunk);
        H5Dclose(dset_id_float);
        H5Dclose(dset_id_float_chunk);
        H5Fclose(file_id);
        H5Pclose(dxpl_id_c_to_f);
        H5Pclose(dxpl_id_c_to_f_copy);
        H5Pclose(dxpl_id_polynomial);
        H5Pclose(dxpl_id_polynomial_copy);
        H5Pclose(dxpl_id_simple);
        H5Pclose(dxpl_id_utrans_inv);
    } H5E_END_TRY
   return -1;
}

static int
init_test(hid_t file_id)
{
    const char* f_to_c = "(5/9.0)*(x-32)";
    /* utrans is a transform for unsigned types: no negative numbers involved and results are < 255 to fit into uchar */
    const char* utrans = "((x+100)/4)*3";

    hid_t   dataspace = -1;
    hid_t   dxpl_id_f_to_c = -1;
    hid_t   dxpl_id_utrans = -1;
    hid_t   cparms = -1;
    hid_t   filespace = -1;
    hsize_t dim[2] = { ROWS, COLS };
    hsize_t offset[2] = { 0, 0 };

    if((dxpl_id_f_to_c = H5Pcreate(H5P_DATASET_XFER)) < 0)
        TEST_ERROR
    if((dxpl_id_utrans = H5Pcreate(H5P_DATASET_XFER)) < 0)
        TEST_ERROR

    if(H5Pset_data_transform(dxpl_id_f_to_c, f_to_c) < 0)
        TEST_ERROR
    if(H5Pset_data_transform(dxpl_id_utrans, utrans) < 0)
        TEST_ERROR

    cparms = H5Pcreate(H5P_DATASET_CREATE);
    if(H5Pset_chunk(cparms, 2, dim) < 0)
        TEST_ERROR

    if((dataspace = H5Screate_simple(2, dim, NULL)) < 0)
        TEST_ERROR

    TESTING("Intializing test...")

    if((dset_id_int = H5Dcreate2(file_id, "/default_int", H5T_NATIVE_INT,
            dataspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Dwrite(dset_id_int, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL,
            dxpl_id_f_to_c, windchillFfloat) < 0)
        TEST_ERROR

    if((dset_id_float = H5Dcreate2(file_id, "/default_float",
            H5T_NATIVE_FLOAT, dataspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Dwrite(dset_id_float, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL,
            dxpl_id_f_to_c, windchillFfloat) < 0)
        TEST_ERROR

    if((dset_id_int_chunk = H5Dcreate2(file_id, "/default_chunk_int",
            H5T_NATIVE_INT, dataspace, H5P_DEFAULT, cparms, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((filespace = H5Dget_space(dset_id_int_chunk)) < 0)
        TEST_ERROR
    if(H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, NULL, dim, NULL) < 0)
        TEST_ERROR
    if(H5Dwrite(dset_id_int_chunk, H5T_NATIVE_FLOAT, dataspace, filespace,
            dxpl_id_f_to_c, windchillFfloat) < 0)
        TEST_ERROR

    if((dset_id_float_chunk = H5Dcreate2(file_id, "/default_chunk_float",
            H5T_NATIVE_FLOAT, dataspace, H5P_DEFAULT, cparms, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Dwrite(dset_id_float_chunk, H5T_NATIVE_FLOAT, dataspace, filespace,
            dxpl_id_f_to_c, windchillFfloat) < 0)
        TEST_ERROR

    if(H5Pclose(cparms) < 0)
        TEST_ERROR
    if(H5Pclose(dxpl_id_f_to_c) < 0)
        TEST_ERROR
    if(H5Pclose(dxpl_id_utrans) < 0)
        TEST_ERROR
    if(H5Sclose(dataspace) < 0)
        TEST_ERROR
    if(H5Sclose(filespace) < 0)
        TEST_ERROR

    PASSED();

    return 0;
    
error: 
    H5E_BEGIN_TRY {
        H5Pclose(cparms);
        H5Pclose(dxpl_id_f_to_c);
        H5Pclose(dxpl_id_utrans);
        H5Sclose(dataspace);
        H5Sclose(filespace);
    } H5E_END_TRY

    return -1;
}

static int
test_poly(const hid_t dxpl_id_polynomial)
{
    float polyflres[ROWS][COLS];
    int polyintread[ROWS][COLS];
    float polyflread[ROWS][COLS];
    int windchillC;
    int row, col;

    for(row = 0; row < ROWS; row++)
        for(col = 0; col < COLS; col++) {
            windchillC = (int) ((5.0 / 9.0) * (windchillFfloat[row][col] - 32));
            polyflres[row][col] = (float) ((2.0 + windchillC) * ((windchillC - 8.0) / 2.0));
        }

    TESTING("data transform, polynomial transform (int->float)")
    if(H5Dread(dset_id_int, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL,
            dxpl_id_polynomial, polyflread) < 0)
        TEST_ERROR
        
    COMPARE(float, polyflread, polyflres, 2.0)

    for(row = 0; row < ROWS; row++)
        for(col = 0; col < COLS; col++) {
            windchillC = (int) ((5.0 / 9.0) * (windchillFfloat[row][col] - 32));
            polyflres[row][col] = (float) ((2 + windchillC) * ((windchillC - 8) / 2));
        }

    TESTING("data transform, polynomial transform (float->int)")
    if(H5Dread(dset_id_float, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
            dxpl_id_polynomial, polyintread) < 0)
        TEST_ERROR
        
    COMPARE(int, polyintread, polyflres, 4)

    return 0;

error: 
     return -1;
}

static int
test_copy(const hid_t dxpl_id_c_to_f_copy, const hid_t dxpl_id_polynomial_copy)
{
    int windchillC;
    float polyflres[ROWS][COLS];
    int polyintread[ROWS][COLS];
    int windchillFintread[ROWS][COLS];
    int row, col;

    for(row = 0; row < ROWS; row++)
        for(col = 0; col < COLS; col++) {
            windchillC = (int) ((5.0 / 9.0) * (windchillFfloat[row][col] - 32));
            polyflres[row][col] = (float) ((2 + windchillC) * ((windchillC - 8) / 2));
        }

    TESTING("data transform, linear transform w/ copied property")
    if(H5Dread(dset_id_float, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
            dxpl_id_c_to_f_copy, windchillFintread) < 0)
        TEST_ERROR
        
    COMPARE(int, windchillFintread, windchillFfloat, 2)

    TESTING("data transform, polynomial transform w/ copied property")
    if(H5Dread(dset_id_float, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
            dxpl_id_polynomial_copy, polyintread) < 0)
        TEST_ERROR
        
    COMPARE(int, polyintread, polyflres, 2)

    return 0;

error: 
    return -1;
}

static int
test_trivial(const hid_t dxpl_id_simple)
{
    float windchillFfloatread[ROWS][COLS];
    int windchillFintread[ROWS][COLS];
    int row, col;

    TESTING("data transform, trivial transform, without type conversion")
    if(H5Dread(dset_id_float, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL,
            dxpl_id_simple, windchillFfloatread) < 0)
        TEST_ERROR
    for(row = 0; row < ROWS; row++)
        for(col = 0; col < COLS; col++) {
            if((windchillFfloatread[row][col] - 4.8) > FLOAT_TOL) 
                FAIL_PUTS_ERROR("    ERROR: Conversion failed to match computed data\n");
        }

    PASSED()

    TESTING("data transform, trivial transform, with type conversion")
    if(H5Dread(dset_id_float, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
            dxpl_id_simple, windchillFintread) < 0)
        TEST_ERROR
    for(row = 0; row < ROWS; row++)
        for(col = 0; col < COLS; col++) {
            if(windchillFintread[row][col] != 4) 
                FAIL_PUTS_ERROR("    ERROR: Conversion failed to match computed data\n")
        }

    PASSED()

    return 0;
error:
    return -1;
}

static int
test_getset(const hid_t dxpl_id_c_to_f)
{
    int         row;
    int         col;
    float       windchillFfloatread[ROWS][COLS];
    const char *simple = "(4/2) * ( (2 + 4)/(5 - 2.5))"; /* this equals 4.8 */
    const char *c_to_f = "(9/5.0)*x + 32";
    char       *ptrgetTest = NULL;

    TESTING("H5Pget_data_transform")

    if(NULL == (ptrgetTest = (char *)HDmalloc(HDstrlen(simple) + 1)))
        TEST_ERROR

    if(H5Pget_data_transform(dxpl_id_c_to_f, ptrgetTest, HDstrlen(c_to_f) + 1) < 0)
        TEST_ERROR
    if(HDstrcmp(c_to_f, ptrgetTest) != 0)
        FAIL_PUTS_ERROR("    ERROR: Data transform failed to match what was set\n")

    PASSED()
    
    HDfree(ptrgetTest);
    ptrgetTest = NULL;

    TESTING("data transform, read after reseting of transform property")

    if(H5Pset_data_transform(dxpl_id_c_to_f, simple) < 0)
        TEST_ERROR

    if(H5Dread(dset_id_float, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL,
                dxpl_id_c_to_f, windchillFfloatread) < 0)
        TEST_ERROR
        
    for(row = 0; row < ROWS; row++)
        for(col = 0; col < COLS; col++) {
            if((windchillFfloatread[row][col] - 4.8) > FLOAT_TOL) 
                FAIL_PUTS_ERROR("    ERROR: Conversion failed to match computed data\n")
        }

    PASSED()

    TESTING("H5Pget_data_transform, after resetting transform property")

    if(NULL == (ptrgetTest = (char *)HDcalloc((size_t)1, HDstrlen(simple) + 1)))
        TEST_ERROR
    if(H5Pget_data_transform(dxpl_id_c_to_f, ptrgetTest, HDstrlen(simple) + 1) < 0)
        TEST_ERROR
    if(HDstrcmp(simple, ptrgetTest) != 0) 
        FAIL_PUTS_ERROR("    ERROR: Data transform failed to match what was set\n")

    PASSED()

    HDfree(ptrgetTest);
    ptrgetTest = NULL;

    return 0;

error:
    if(ptrgetTest)
        HDfree(ptrgetTest);

    return -1;
}

static int
test_set(void)
{
    hid_t dxpl_id = -1;
    H5E_auto2_t func;
    const char *str = "(9/5.0)*x + 32";
    char *ptrgetTest = NULL;

    TESTING("H5Pget_data_transform (get before set)")
    
    if(NULL == (ptrgetTest = (char *)HDmalloc(HDstrlen(str) + 1)))
        TEST_ERROR

    if((dxpl_id = H5Pcreate(H5P_DATASET_XFER)) < 0) 
        TEST_ERROR

    /* Test get before set */
    H5Eget_auto2(H5E_DEFAULT, &func, NULL);

    H5Eset_auto2(H5E_DEFAULT, NULL, NULL);

    if(H5Pget_data_transform(dxpl_id, ptrgetTest, HDstrlen(str) + 1) < 0)
        PASSED()
    else 
        FAIL_PUTS_ERROR("    ERROR: Data transform get before set succeeded (it shouldn't have)\n");

    HDfree(ptrgetTest);
    ptrgetTest = NULL;

    TESTING("H5Pset_data_transform (set with NULL transform)");
    INVALID_SET_TEST(NULL);

    TESTING("H5Pset_data_transform (set with invalid transform 1)")
    INVALID_SET_TEST("\0");

    TESTING("H5Pset_data_transform (set with invalid transform 2)")
    INVALID_SET_TEST("     ");

    TESTING("H5Pset_data_transform (set with invalid transform 3)")
    INVALID_SET_TEST("x+");

    TESTING("H5Pset_data_transform (set with invalid transform 4)")
    INVALID_SET_TEST("(x+5");

    TESTING("H5Pset_data_transform (set with invalid transform 5)")
    INVALID_SET_TEST("+");

    TESTING("H5Pset_data_transform (set with invalid transform 6)")
    INVALID_SET_TEST("(9/5)*x + x**2");

    TESTING("H5Pset_data_transform (set with invalid transform 7)")
    INVALID_SET_TEST("(9/5)x");

    TESTING("H5Pset_data_transform (set with invalid transform 8)")
    INVALID_SET_TEST("(9/5)*x + x^2");

    H5Eset_auto2(H5E_DEFAULT, func, NULL);

    if(H5Pclose(dxpl_id) < 0)
        TEST_ERROR

    return 0;

error:
    if(ptrgetTest)
        HDfree(ptrgetTest);
    H5E_BEGIN_TRY {
        H5Pclose(dxpl_id);
    } H5E_END_TRY

    return -1;
}

