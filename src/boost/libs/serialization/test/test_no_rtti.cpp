/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
// test_no_rtti.cpp

// (C) Copyright 2002 Robert Ramey - http://www.rrsd.com . 
// Use, modification and distribution is subject to the Boost Software
// License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

// note: this program tests the inter-operability of different
// extended typeinfo systems.  In this example, one class is
// identified using the default RTTI while the other uses a custom
// system based on the export key.
// 
// As this program uses RTTI for one of the types, the test will fail
// on a system for which RTTI is not enabled or not existent.

#include <cstddef>
#include <fstream>

#include <boost/config.hpp>
#include <cstdio> // remove
#if defined(BOOST_NO_STDC_NAMESPACE)
namespace std{ 
    using ::remove;
}
#endif

#include <boost/static_assert.hpp>
#include <boost/type_traits/is_same.hpp>

#include <boost/archive/archive_exception.hpp>
#include "test_tools.hpp"
#include <boost/preprocessor/stringize.hpp>
#include BOOST_PP_STRINGIZE(BOOST_ARCHIVE_TEST)

#include <boost/serialization/type_info_implementation.hpp>
#include <boost/serialization/extended_type_info_no_rtti.hpp>

#include "polymorphic_base.hpp"
#include "polymorphic_derived1.hpp"
#include "polymorphic_derived2.hpp"

// save derived polymorphic class
void save_derived(const char *testfile)
{
    test_ostream os(testfile, TEST_STREAM_FLAGS);
    test_oarchive oa(os);

    polymorphic_derived1 *rd1 = new polymorphic_derived1;
    polymorphic_derived2 *rd2 = new polymorphic_derived2;

    oa << BOOST_SERIALIZATION_NVP(rd1);
    oa << BOOST_SERIALIZATION_NVP(rd2);

    // the above opereration registers the derived classes as a side
    // effect.  Hence, instances can now be correctly serialized through
    // a base class pointer.
    polymorphic_base *rb1 =  rd1;
    polymorphic_base *rb2 =  rd2;
    oa << BOOST_SERIALIZATION_NVP(rb1);
    oa << BOOST_SERIALIZATION_NVP(rb2);

    delete rd1;
    delete rd2;
}

// save derived polymorphic class
void load_derived(const char *testfile)
{
    test_istream is(testfile, TEST_STREAM_FLAGS);
    test_iarchive ia(is);

    polymorphic_derived1 *rd1 = NULL;
    polymorphic_derived2 *rd2 = NULL;

    ia >> BOOST_SERIALIZATION_NVP(rd1);

    BOOST_CHECK_MESSAGE(
        boost::serialization::type_info_implementation<
            polymorphic_derived1
        >::type::get_const_instance()
        == 
        * boost::serialization::type_info_implementation<
            polymorphic_derived1
        >::type::get_const_instance().get_derived_extended_type_info(*rd1)
        ,
        "restored pointer d1 not of correct type"
    );

    ia >> BOOST_SERIALIZATION_NVP(rd2);

    BOOST_CHECK_MESSAGE(
        boost::serialization::type_info_implementation<
            polymorphic_derived2
        >::type::get_const_instance()
        == 
        * boost::serialization::type_info_implementation<
            polymorphic_derived2
        >::type::get_const_instance().get_derived_extended_type_info(*rd2)
        ,
        "restored pointer d2 not of correct type"
    );

    polymorphic_base *rb1 = NULL;
    polymorphic_base *rb2 = NULL;

    // the above opereration registers the derived classes as a side
    // effect.  Hence, instances can now be correctly serialized through
    // a base class pointer.
    ia >> BOOST_SERIALIZATION_NVP(rb1);

    BOOST_CHECK_MESSAGE(
        rb1 == dynamic_cast<polymorphic_base *>(rd1),
        "serialized pointers not correctly restored"
    );

    BOOST_CHECK_MESSAGE(
        boost::serialization::type_info_implementation<
            polymorphic_derived1
        >::type::get_const_instance()
        == 
        * boost::serialization::type_info_implementation<
            polymorphic_base
        >::type::get_const_instance().get_derived_extended_type_info(*rb1)
        ,
        "restored pointer b1 not of correct type"
    );

    ia >> BOOST_SERIALIZATION_NVP(rb2);

    BOOST_CHECK_MESSAGE(
        rb2 ==  dynamic_cast<polymorphic_base *>(rd2),
        "serialized pointers not correctly restored"
    );

    BOOST_CHECK_MESSAGE(
        boost::serialization::type_info_implementation<
            polymorphic_derived2
        >::type::get_const_instance()
        == 
        * boost::serialization::type_info_implementation<
            polymorphic_base
        >::type::get_const_instance().get_derived_extended_type_info(*rb2)
        ,
        "restored pointer b2 not of correct type"
    );

    delete rb1;
    delete rb2;
}

int
test_main( int /* argc */, char* /* argv */[] )
{
    const char * testfile = boost::archive::tmpnam(NULL);
    
    BOOST_REQUIRE(NULL != testfile);

    save_derived(testfile);
    load_derived(testfile);

    std::remove(testfile);
    return EXIT_SUCCESS;
}

// EOF
