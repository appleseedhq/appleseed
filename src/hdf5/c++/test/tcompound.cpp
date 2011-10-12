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

/*****************************************************************************
   FILE
   tcompound.cpp - HDF5 C++ testing the compound data type functionality

 ***************************************************************************/

#ifdef OLD_HEADER_FILENAME
#include <iostream.h>
#else
#include <iostream>
#endif
#include <string>

#ifndef H5_NO_NAMESPACE
#ifndef H5_NO_STD
    using std::cerr;
    using std::endl;
#endif  // H5_NO_STD
#endif

#include "H5Cpp.h"      // C++ API header file

#ifndef H5_NO_NAMESPACE
    using namespace H5;
#endif

#include "h5cpputil.h"  // C++ utilility header file

/* Number of elements in each test */
#define NTESTELEM	100000

typedef struct complex_t {
    double                  re;
    double                  im;
} complex_t;


/*-------------------------------------------------------------------------
 * Function:    test_compound_1
 *
 * Purpose:     Tests various things about compound data types.
 *
 * Return:      None
 *
 * Programmer:  Binh-Minh Ribler (using C version)
 *              January, 2007
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void test_compound_1()
{
    // Output message about test being performed
    SUBTEST("Compound Data Types");
    try {
	// Create an empty compound datatype
	CompType complex_type(sizeof(complex_t));

	// Add a couple of fields
	complex_type.insertMember("real", HOFFSET(complex_t, re), PredType::NATIVE_DOUBLE);
	complex_type.insertMember("imaginary", HOFFSET(complex_t, im), PredType::NATIVE_DOUBLE);
	PASSED();
    }   // end of try block

    catch (Exception E) {
cerr << "test_compound_1 in catch" << endl;
        issue_fail_msg(E.getCFuncName(), __LINE__, __FILE__, E.getCDetailMsg());
    }
}   // test_compound_1()


/*-------------------------------------------------------------------------
 * Function:	test_compound_2
 *
 * Purpose:	Tests a compound type conversion where the source and
 *		destination are the same except for the order of the
 *		elements.
 *
 * Return:	None
 *
 * Programmer:	Binh-Minh Ribler (use C version)
 *              January, 2007
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void test_compound_2()
{
    typedef struct {
	int a, b, c[4], d, e;
    } src_typ_t;
    typedef struct {
	int e, d, c[4], b, a;
    } dst_typ_t;

    src_typ_t	  *s_ptr;
    dst_typ_t	  *d_ptr;
    const int	   nelmts = NTESTELEM;
    const hsize_t  four = 4;
    int		   i;
    unsigned char *buf = NULL, *orig = NULL, *bkg = NULL;
    ArrayType *array_dt = NULL;

    // Output message about test being performed
    SUBTEST("Compound Element Reordering");
    try {
	// Sizes should be the same, but be careful just in case
	buf = (unsigned char*)malloc(nelmts * MAX(sizeof(src_typ_t), sizeof(dst_typ_t)));
	bkg = (unsigned char*)malloc(nelmts * sizeof(dst_typ_t));
	orig = (unsigned char*)malloc(nelmts * sizeof(src_typ_t));
	for (i=0; i<nelmts; i++) {
	    s_ptr = ((src_typ_t*)orig) + i;
	    s_ptr->a    = i*8+0;
	    s_ptr->b    = i*8+1;
	    s_ptr->c[0] = i*8+2;
	    s_ptr->c[1] = i*8+3;
	    s_ptr->c[2] = i*8+4;
	    s_ptr->c[3] = i*8+5;
	    s_ptr->d    = i*8+6;
	    s_ptr->e    = i*8+7;
	}
	memcpy(buf, orig, nelmts*sizeof(src_typ_t));

	// Build hdf5 datatypes
	array_dt = new ArrayType(PredType::NATIVE_INT, 1, &four);

	// Create an empty compound datatype
	CompType st(sizeof(src_typ_t));
	st.insertMember("a", HOFFSET(src_typ_t, a), PredType::NATIVE_INT);
	st.insertMember("b", HOFFSET(src_typ_t, b), PredType::NATIVE_INT);
	st.insertMember("c", HOFFSET(src_typ_t, c), *array_dt);
	st.insertMember("d", HOFFSET(src_typ_t, d), PredType::NATIVE_INT);
	st.insertMember("e", HOFFSET(src_typ_t, e), PredType::NATIVE_INT);
	array_dt->close();
        delete array_dt;

	array_dt = new ArrayType(PredType::NATIVE_INT, 1, &four);

	// Create an empty compound datatype
	CompType dt(sizeof(dst_typ_t));
	dt.insertMember("a", HOFFSET(dst_typ_t, a), PredType::NATIVE_INT);
	dt.insertMember("b", HOFFSET(dst_typ_t, b), PredType::NATIVE_INT);
	dt.insertMember("c", HOFFSET(dst_typ_t, c), *array_dt);
	dt.insertMember("d", HOFFSET(dst_typ_t, d), PredType::NATIVE_INT);
	dt.insertMember("e", HOFFSET(dst_typ_t, e), PredType::NATIVE_INT);
	array_dt->close();

	// Perform the conversion
	st.convert(dt, (size_t)nelmts, buf, bkg);

	// Compare results
	for (i=0; i<nelmts; i++) {
	    s_ptr = ((src_typ_t*)orig) + i;
	    d_ptr = ((dst_typ_t*)buf)  + i;
	    if (s_ptr->a    != d_ptr->a    ||
		s_ptr->b    != d_ptr->b    ||
		s_ptr->c[0] != d_ptr->c[0] ||
		s_ptr->c[1] != d_ptr->c[1] ||
		s_ptr->c[2] != d_ptr->c[2] ||
		s_ptr->c[3] != d_ptr->c[3] ||
		s_ptr->d    != d_ptr->d    ||
		s_ptr->e    != d_ptr->e) {
		H5_FAILED();
		cerr << "    i=" << i << endl;
		cerr << "    src={a=" << s_ptr->a << ", b=" << s_ptr->b
		     << "c=[" << s_ptr->c[0] << "," << s_ptr->c[1] << ","
		     << s_ptr->c[2] << "," << s_ptr->c[3] << ", d="
		     << s_ptr->d << ", e=" << s_ptr->e << "}" << endl;
		cerr << "    dst={a=" << s_ptr->a << ", b=" << s_ptr->b
		     << "c=[" << s_ptr->c[0] << "," << s_ptr->c[1] << ","
		     << s_ptr->c[2] << "," << s_ptr->c[3] << ", d="
		     << s_ptr->d << ", e=" << s_ptr->e << "}" << endl;
	    }
    	}
	// Release resources
	free(buf);
	free(bkg);
	free(orig);
	s_ptr = NULL;
	d_ptr = NULL;
	st.close();
	dt.close();
	PASSED();
    }   // end of try block

    catch (Exception E) {
cerr << "test_compound_2 in catch" << endl;
        issue_fail_msg(E.getCFuncName(), __LINE__, __FILE__, E.getCDetailMsg());
    }

    if(array_dt)
        delete array_dt;
}   // test_compound_2()


/*-------------------------------------------------------------------------
 * Function:	test_compound_3
 *
 * Purpose:	Tests compound conversions where the source and destination
 *		are the same except the destination is missing a couple
 *		members which appear in the source.
 *
 * Return:	None
 *
 * Programmer:	Binh-Minh Ribler (use C version)
 *              January, 2007
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void test_compound_3()
{
    typedef struct {
	int a, b, c[4], d, e;
    } src_typ_t;
    typedef struct {
	int a,    c[4],    e;
    } dst_typ_t;

    src_typ_t	  *s_ptr;
    dst_typ_t 	  *d_ptr;
    int		   i;
    const int	   nelmts = NTESTELEM;
    const hsize_t  four = 4;
    unsigned char *buf = NULL, *orig = NULL, *bkg = NULL;
    ArrayType* array_dt = NULL;

    // Output message about test being performed
    SUBTEST("Compound Datatype Subset Conversions");
    try {
	/* Initialize */
	buf = (unsigned char*)malloc(nelmts * MAX(sizeof(src_typ_t), sizeof(dst_typ_t)));
	bkg = (unsigned char*)malloc(nelmts * sizeof(dst_typ_t));
	orig = (unsigned char*)malloc(nelmts * sizeof(src_typ_t));
	for (i=0; i<nelmts; i++) {
	    s_ptr = ((src_typ_t*)orig) + i;
	    s_ptr->a    = i*8+0;
	    s_ptr->b    = i*8+1;
	    s_ptr->c[0] = i*8+2;
	    s_ptr->c[1] = i*8+3;
	    s_ptr->c[2] = i*8+4;
	    s_ptr->c[3] = i*8+5;
	    s_ptr->d    = i*8+6;
	    s_ptr->e    = i*8+7;
	}
	memcpy(buf, orig, nelmts*sizeof(src_typ_t));

	/* Build hdf5 datatypes */
	array_dt = new ArrayType(PredType::NATIVE_INT, 1, &four);

	// Create an empty compound datatype
	CompType st(sizeof(src_typ_t));
	st.insertMember("a", HOFFSET(src_typ_t, a), PredType::NATIVE_INT);
	st.insertMember("b", HOFFSET(src_typ_t, b), PredType::NATIVE_INT);
	st.insertMember("c", HOFFSET(src_typ_t, c), *array_dt);
	st.insertMember("d", HOFFSET(src_typ_t, d), PredType::NATIVE_INT);
	st.insertMember("e", HOFFSET(src_typ_t, e), PredType::NATIVE_INT);
	array_dt->close();
        delete array_dt;

	array_dt = new ArrayType(PredType::NATIVE_INT, 1, &four);

	// Create an empty compound datatype
	CompType dt(sizeof(dst_typ_t));
	dt.insertMember("a", HOFFSET(dst_typ_t, a), PredType::NATIVE_INT);
	dt.insertMember("c", HOFFSET(dst_typ_t, c), *array_dt);
	dt.insertMember("e", HOFFSET(dst_typ_t, e), PredType::NATIVE_INT);
	array_dt->close();

	/* Perform the conversion */
	st.convert(dt, (size_t)nelmts, buf, bkg);

	/* Compare results */
	for (i=0; i<nelmts; i++) {
	    s_ptr = ((src_typ_t*)orig) + i;
	    d_ptr = ((dst_typ_t*)buf)  + i;
	    if (s_ptr->a    != d_ptr->a    ||
		s_ptr->c[0] != d_ptr->c[0] ||
		s_ptr->c[1] != d_ptr->c[1] ||
		s_ptr->c[2] != d_ptr->c[2] ||
		s_ptr->c[3] != d_ptr->c[3] ||
		s_ptr->e    != d_ptr->e) {
		H5_FAILED();
		cerr << "    i=" << i << endl;
		cerr << "    src={a=" << s_ptr->a << ", b=" << s_ptr->b
		     << ", c=[" << s_ptr->c[0] << "," << s_ptr->c[1] << ","
		     << s_ptr->c[2] << "," << s_ptr->c[3] << "], d="
		     << s_ptr->d << ", e=" << s_ptr->e << "}" << endl;
		cerr << "    dst={a=" << d_ptr->a
		     << ", c=[" << d_ptr->c[0] << "," << d_ptr->c[1] << ","
		     << d_ptr->c[2] << "," << d_ptr->c[3] << "], e="
		     << d_ptr->e << "}" << endl;
	    } // if
	} // for

	/* Release resources */
	free(buf);
	free(bkg);
	free(orig);
	s_ptr = NULL;
	d_ptr = NULL;
	st.close();
	dt.close();
	PASSED();
    }   // end of try block

    catch (Exception E) {
cerr << "test_compound_3 in catch" << endl;
	issue_fail_msg(E.getCFuncName(), __LINE__, __FILE__, E.getCDetailMsg());
    }

    if(array_dt)
        delete array_dt;
}   // test_compound_3()


/*-------------------------------------------------------------------------
 * Function:	test_compound_4
 *
 * Purpose:	Tests compound conversions when the destination has the same
 *		fields as the source but one or more of the fields are
 *		smaller.
 *
 * Return:	None
 *
 * Programmer:	Binh-Minh Ribler (use C version)
 *		January, 2007
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void test_compound_4()
{

    typedef struct {
	int a, b, c[4], d, e;
    } src_typ_t;

    typedef struct {
	short b;
	int a, c[4];
	short d;
	int e;
    } dst_typ_t;

    src_typ_t	  *s_ptr;
    dst_typ_t	  *d_ptr;
    int		   i;
    const int	   nelmts = NTESTELEM;
    const hsize_t  four = 4;
    unsigned char *buf = NULL, *orig = NULL, *bkg = NULL;
    ArrayType* array_dt = NULL;

    // Output message about test being performed
    SUBTEST("Compound Element Shrinking & Reordering");
    try {
	/* Sizes should be the same, but be careful just in case */
	buf = (unsigned char*)malloc(nelmts * MAX(sizeof(src_typ_t), sizeof(dst_typ_t)));
	bkg = (unsigned char*)malloc(nelmts * sizeof(dst_typ_t));
	orig = (unsigned char*)malloc(nelmts * sizeof(src_typ_t));
	for (i=0; i<nelmts; i++) {
	    s_ptr = ((src_typ_t*)orig) + i;
	    s_ptr->a    = i*8+0;
	    s_ptr->b    = (i*8+1) & 0x7fff;
	    s_ptr->c[0] = i*8+2;
	    s_ptr->c[1] = i*8+3;
	    s_ptr->c[2] = i*8+4;
	    s_ptr->c[3] = i*8+5;
	    s_ptr->d	    = (i*8+6) & 0x7fff;
	    s_ptr->e    = i*8+7;
	}
	memcpy(buf, orig, nelmts*sizeof(src_typ_t));

	/* Build hdf5 datatypes */
	array_dt = new ArrayType(PredType::NATIVE_INT, 1, &four);

	// Create an empty compound datatype
	CompType st(sizeof(src_typ_t));
	st.insertMember("a", HOFFSET(src_typ_t, a), PredType::NATIVE_INT);
	st.insertMember("b", HOFFSET(src_typ_t, b), PredType::NATIVE_INT);
	st.insertMember("c", HOFFSET(src_typ_t, c), *array_dt);
	st.insertMember("d", HOFFSET(src_typ_t, d), PredType::NATIVE_INT);
	st.insertMember("e", HOFFSET(src_typ_t, e), PredType::NATIVE_INT);
	array_dt->close();
        delete array_dt;

	array_dt = new ArrayType(PredType::NATIVE_INT, 1, &four);

	// Create an empty compound datatype
	CompType dt(sizeof(dst_typ_t));
	dt.insertMember("a", HOFFSET(dst_typ_t, a), PredType::NATIVE_INT);
	dt.insertMember("b", HOFFSET(dst_typ_t, b), PredType::NATIVE_SHORT);
	dt.insertMember("c", HOFFSET(dst_typ_t, c), *array_dt);
	dt.insertMember("d", HOFFSET(dst_typ_t, d), PredType::NATIVE_SHORT);
	dt.insertMember("e", HOFFSET(dst_typ_t, e), PredType::NATIVE_INT);
	array_dt->close();

	/* Perform the conversion */
	st.convert(dt, (size_t)nelmts, buf, bkg);

	/* Compare results */
	for (i=0; i<nelmts; i++) {
	    s_ptr = ((src_typ_t*)orig) + i;
	    d_ptr = ((dst_typ_t*)buf)  + i;
	    if (s_ptr->a    != d_ptr->a    ||
		s_ptr->b    != d_ptr->b    ||
		s_ptr->c[0] != d_ptr->c[0] ||
		s_ptr->c[1] != d_ptr->c[1] ||
		s_ptr->c[2] != d_ptr->c[2] ||
		s_ptr->c[3] != d_ptr->c[3] ||
		s_ptr->d    != d_ptr->d    ||
		s_ptr->e    != d_ptr->e)
	    {
		H5_FAILED();
		cerr << "    i=" << i << endl;
		cerr << "    src={a=" << s_ptr->a << ", b=" << s_ptr->b
		     << "c=[" << s_ptr->c[0] << "," << s_ptr->c[1] << ","
		     << s_ptr->c[2] << "," << s_ptr->c[3] << ", d="
		     << s_ptr->d << ", e=" << s_ptr->e << "}" << endl;
		cerr << "    dst={a=" << d_ptr->a << ", b=" << d_ptr->b
		     << "c=[" << d_ptr->c[0] << "," << d_ptr->c[1] << ","
		     << d_ptr->c[2] << "," << d_ptr->c[3] << ", d="
		     << d_ptr->d << ", e=" << d_ptr->e << "}" << endl;
	    } // if
	} // for

	/* Release resources */
	free(buf);
	free(bkg);
	free(orig);
	s_ptr = NULL;
	d_ptr = NULL;
	st.close();
	dt.close();
	PASSED();
    }   // end of try block

    catch (Exception E) {
cerr << "test_compound_4 in catch" << endl;
        issue_fail_msg(E.getCFuncName(), __LINE__, __FILE__, E.getCDetailMsg());
    }

    if(array_dt)
        delete array_dt;
}   // test_compound_4()


/*-------------------------------------------------------------------------
 * Function:	test_compound_5
 *
 * Purpose:	Many versions of HDF5 have a bug in the optimized compound
 *              datatype conversion function, H5T_conv_struct_opt(), which
 *              is triggered when the top-level type contains a struct
 *              which must undergo a conversion.
 *
 * Return:	None
 *
 * Programmer:	Binh-Minh Ribler (use C version)
 *		January, 2007
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void test_compound_5()
{
    typedef struct {
        char    name[16];
        short   tdim;
        short   coll_ids[4];
    } src_typ_t;

    typedef struct {
        char    name[16];
        short   tdim;
        int     coll_ids[4];
    } dst_typ_t;

    hsize_t      dims[1] = {4};
    src_typ_t  src[2] = {{"one", 102, {104, 105, 106, 107}},
                          {"two", 202, {204, 205, 206, 207}}};
    dst_typ_t  *dst;
    void        *buf = calloc(2, sizeof(dst_typ_t));
    void        *bkg = calloc(2, sizeof(dst_typ_t));
    ArrayType* array_dt = NULL;

    // Output message about test being performed
    SUBTEST("Optimized Struct Converter");
    try {

	/* Build datatypes */
	array_dt = new ArrayType(PredType::NATIVE_SHORT, 1, dims);
	CompType short_array(4*sizeof(short));
	short_array.insertMember("_", 0, *array_dt);
	array_dt->close();
        delete array_dt;

	CompType int_array(4*sizeof(int));
	array_dt = new ArrayType(PredType::NATIVE_INT, 1, dims);
	int_array.insertMember("_", 0, *array_dt);
	array_dt->close();

	StrType strg(PredType::C_S1, 16);
	CompType src_type(sizeof(src_typ_t));
	src_type.insertMember("name", HOFFSET(src_typ_t, name), strg);
	src_type.insertMember("tdim", HOFFSET(src_typ_t, tdim), PredType::NATIVE_SHORT);
	src_type.insertMember("coll_ids", HOFFSET(src_typ_t, coll_ids), short_array);

	CompType dst_type(sizeof(dst_typ_t));
	dst_type.insertMember("name", HOFFSET(dst_typ_t, name), strg);
	dst_type.insertMember("tdim", HOFFSET(dst_typ_t, tdim), PredType::NATIVE_SHORT);
	dst_type.insertMember("coll_ids", HOFFSET(dst_typ_t, coll_ids), int_array);

	/* Convert data */
	memcpy(buf, src, sizeof(src));
	src_type.convert(dst_type, (size_t)2, buf, bkg);
	dst = (dst_typ_t*)buf;

	/* Cleanup */
	src_type.close();
	dst_type.close();
	strg.close();
	short_array.close();
	int_array.close();

	/* Check results */
	if (memcmp(src[1].name, dst[1].name, sizeof(src[1].name)) ||
	    src[1].tdim!=dst[1].tdim ||
	    src[1].coll_ids[0]!=dst[1].coll_ids[0] ||
	    src[1].coll_ids[1]!=dst[1].coll_ids[1] ||
	    src[1].coll_ids[2]!=dst[1].coll_ids[2] ||
	    src[1].coll_ids[3]!=dst[1].coll_ids[3])
	{ H5_FAILED(); }

	/* Free memory buffers */
	free(buf);
	free(bkg);
	dst = NULL;
	PASSED();
    }   // end of try block

    catch (Exception E) {
cerr << "test_compound_5 in catch" << endl;
        issue_fail_msg(E.getCFuncName(), __LINE__, __FILE__, E.getCDetailMsg());
    }

    if(array_dt)
        delete array_dt;
}   // test_compound_5()


/*-------------------------------------------------------------------------
 * Function:	test_compound_6
 *
 * Purpose:	Tests compound conversions when the destination has the same
 *		fields as the source but one or more of the fields are
 *		larger.
 *
 * Return:	None
 *
 * Programmer:	Binh-Minh Ribler (use C version)
 *		January, 2007
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void test_compound_6()
{
    typedef struct {
	short b;
	short d;
    } src_typ_t;

    typedef struct {
	long b;
	long d;
    } dst_typ_t;

    src_typ_t	  *s_ptr;
    dst_typ_t	  *d_ptr;
    int		   i;
    const int	   nelmts = NTESTELEM;
    unsigned char *buf=NULL, *orig=NULL, *bkg=NULL;

    // Output message about test being performed
    SUBTEST("Compound Element Growing");
    try {
	/* Sizes should be the same, but be careful just in case */
	buf = (unsigned char*)malloc(nelmts * MAX(sizeof(src_typ_t), sizeof(dst_typ_t)));
	bkg = (unsigned char*)malloc(nelmts * sizeof(dst_typ_t));
	orig = (unsigned char*)malloc(nelmts * sizeof(src_typ_t));
	for (i=0; i<nelmts; i++) {
	    s_ptr = ((src_typ_t*)orig) + i;
	    s_ptr->b    = (i*8+1) & 0x7fff;
	    s_ptr->d    = (i*8+6) & 0x7fff;
	}
	memcpy(buf, orig, nelmts*sizeof(src_typ_t));

	/* Build hdf5 datatypes */
	CompType st(sizeof(src_typ_t));
	st.insertMember("b", HOFFSET(src_typ_t, b), PredType::NATIVE_SHORT);
	st.insertMember("d", HOFFSET(src_typ_t, d), PredType::NATIVE_SHORT);

	CompType dt(sizeof(dst_typ_t));
	dt.insertMember("b", HOFFSET(dst_typ_t, b), PredType::NATIVE_LONG);
	dt.insertMember("d", HOFFSET(dst_typ_t, d), PredType::NATIVE_LONG);

	/* Perform the conversion */
	st.convert(dt, (size_t)nelmts, buf, bkg);

	/* Compare results */
	for (i=0; i<nelmts; i++) {
	    s_ptr = ((src_typ_t*)orig) + i;
	    d_ptr = ((dst_typ_t*)buf)  + i;
	    if (s_ptr->b    != d_ptr->b    ||
		s_ptr->d    != d_ptr->d)
	    {
		H5_FAILED();
		cerr << "    i=" << i << endl;
		cerr << "    src={b=" << s_ptr->b << ", d=" << s_ptr->d
		     << "}" << endl;
		cerr << "    dst={b=" << d_ptr->b << ", d=" << d_ptr->d
		     << "}" << endl;
	    } // if
	} // for

	/* Release resources */
	free(buf);
	free(bkg);
	free(orig);
	s_ptr = NULL;
	d_ptr = NULL;
	st.close();
	dt.close();
	PASSED();
    }   // end of try block

    catch (Exception E) {
cerr << "test_compound_6 in catch" << endl;
        issue_fail_msg(E.getCFuncName(), __LINE__, __FILE__, E.getCDetailMsg());
    }
}   // test_compound_6()

/*-------------------------------------------------------------------------
 * Function:	test_compound_7
 *
 * Purpose:	Tests inserting fields into compound datatypes when the field
 *		overlaps the end of the compound datatype.
 *
 * Return:	None
 *
 * Programmer:	Binh-Minh Ribler (use C version)
 *		January, 2007
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void test_compound_7()
{
    typedef struct {
	int a;
	float b;
	long c;
    } s1_typ_t;

    typedef struct {
	int a;
	float b;
	long c;
	double d;
    } s2_typ_t;

    // Output message about test being performed
    SUBTEST("Compound Element Insertion");
    try {
	CompType tid1(sizeof(s1_typ_t));

	tid1.insertMember("a", HOFFSET(s1_typ_t,a),PredType::NATIVE_INT);
	tid1.insertMember("b", HOFFSET(s1_typ_t,b),PredType::NATIVE_FLOAT);
	tid1.insertMember("c", HOFFSET(s1_typ_t,c),PredType::NATIVE_LONG);

	size_t type_size = tid1.getSize();
	verify_val(type_size, sizeof(s1_typ_t), "DataType::getSize", __LINE__, __FILE__);

	CompType tid2;
	tid2.copy(tid1);

	type_size = tid2.getSize();
	verify_val_noteq(type_size, sizeof(s2_typ_t), "DataType::getSize", __LINE__, __FILE__);

	/* Should not be able to insert field past end of compound datatype */
	try {
	    tid2.insertMember("d", HOFFSET(s2_typ_t, d), PredType::NATIVE_DOUBLE);
	    // Should FAIL but didn't, so throw an invalid action exception
	    throw InvalidActionException("CompType::insertMember", "Attempted to insert field past end of compound data type.");
	} catch (DataTypeIException err) {}

	/* Release resources */
	tid1.close();
	tid2.close();
	PASSED();
    }   // end of try block

    catch (Exception E) {
cerr << "test_compound_7 in catch" << endl;
        issue_fail_msg(E.getCFuncName(), __LINE__, __FILE__, E.getCDetailMsg());
    }
}   // test_compound_7()


/*-------------------------------------------------------------------------
 * Function:	test_compound
 *
 * Purpose:	Main compound datatype testing routine
 *
 * Return:	None
 *
 * Programmer:	Binh-Minh Ribler
 *		January 2007
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
#ifdef __cplusplus
extern "C"
#endif
void test_compound()
{
    // Output message about test being performed
    //MESSAGE("Testing Compound Data Type operations\n");
    MESSAGE(5, ("Testing Compound Data Type operations\n"));

    test_compound_1();	// various things about compound data types
    test_compound_2();	// compound element reordering
    test_compound_3();	// compound datatype subset conversions
    test_compound_4();	// compound element shrinking & reordering
    test_compound_5();	// optimized struct converter
    test_compound_6();	// compound element growing
    test_compound_7();	// compound element insertion
}   // test_compound()


/*-------------------------------------------------------------------------
 * Function:	cleanup_compound
 *
 * Purpose:	Cleanup temporary test files - nothing at this time.
 *
 * Return:	none
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
#ifdef __cplusplus
extern "C"
#endif
void cleanup_compound()
{
}   // cleanup_file
