/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
// test_plugin.cpp

// (C) Copyright 2002 Robert Ramey - http://www.rrsd.com . 
// Use, modification and distribution is subject to the Boost Software
// License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

// should pass compilation and execution

// Note this test creates, serializes, and destroys
// a class instance while knowing nothing more than its
// exported class ID (GUID) and a base class from which
// it is derived.  This is referred to as a "plugin"
// since the same program could, without recompilation, 
// manipulate any number of derived types - even those
// which have not been yet been created.

#include <fstream>

#include <cstdio> // remove
#include <boost/config.hpp>
#if defined(BOOST_NO_STDC_NAMESPACE)
namespace std{ 
    using ::remove;
}
#endif

#include <boost/archive/archive_exception.hpp>
#include "test_tools.hpp"
#include "test_decl.hpp"

#define DLL_DECL IMPORT_DECL
#include "base.hpp"
#undef  DLL_DECL

#include <boost/serialization/base_object.hpp>
#include <boost/serialization/export.hpp>
#include <boost/serialization/type_info_implementation.hpp>
#include <boost/serialization/access.hpp>
#include <boost/serialization/void_cast.hpp>
#include <boost/serialization/extended_type_info.hpp>

class polymorphic_derived1 : public polymorphic_base
{
    friend class boost::serialization::access;
    template<class Archive>
    void serialize(Archive &ar, const unsigned int /* file_version */){
        ar & BOOST_SERIALIZATION_BASE_OBJECT_NVP(polymorphic_base);
    }
public:
    virtual ~polymorphic_derived1(){}
};

BOOST_CLASS_EXPORT(polymorphic_derived1)

// MWerks users can do this to make their code work
BOOST_SERIALIZATION_MWERKS_BASE_AND_DERIVED(polymorphic_base, polymorphic_derived1)

// save exported polymorphic class
void save_exported(const char *testfile)
{
    test_ostream os(testfile, TEST_STREAM_FLAGS);
    test_oarchive oa(os, TEST_ARCHIVE_FLAGS);

    polymorphic_base *rb1 = new polymorphic_derived1;

    // get the eti record for the exported type "polymorphic_derived2"
    boost::serialization::extended_type_info const * const d2_eti =
        boost::serialization::extended_type_info::find(
            "polymorphic_derived2"
        );
    assert(NULL != d2_eti);

    // create a new instance of the type referred to by this record.
    // in this example, we happen to know that the class constructor
    // takes no arguments.
    void const * const rd2 = d2_eti->construct();
    assert(NULL != rd2);

    // transform the pointer to a pointer to the base class
    polymorphic_base const * const rb2 
        = static_cast<polymorphic_base const *>(
            boost::serialization::void_upcast(
                * d2_eti,
                boost::serialization::type_info_implementation<polymorphic_base>
                    ::type::get_const_instance(),
                rd2
            )
        );

    // export will permit correct serialization
    // through a pointer to a base class
    oa << BOOST_SERIALIZATION_NVP(rb1);
    oa << BOOST_SERIALIZATION_NVP(rb2);

    // don't need these any more - don't leak memory
    delete rb1;
    // note delete original handle - not runtime cast one !!!
    d2_eti->destroy(rd2);
}

// save exported polymorphic class
void load_exported(const char *testfile)
{
    test_istream is(testfile, TEST_STREAM_FLAGS);
    test_iarchive ia(is, TEST_ARCHIVE_FLAGS);

    polymorphic_base *rb1 = NULL;
    polymorphic_base *rb2 = NULL;

    // export will permit correct serialization
    // through a pointer to a base class
    ia >> BOOST_SERIALIZATION_NVP(rb1);

    BOOST_CHECK_MESSAGE(
        boost::serialization::type_info_implementation<polymorphic_derived1>
            ::type::get_const_instance()
        == 
        * boost::serialization::type_info_implementation<polymorphic_base>
            ::type::get_const_instance().get_derived_extended_type_info(*rb1),
        "restored pointer b1 not of correct type"
    );
    ia >> BOOST_SERIALIZATION_NVP(rb2);

    // get the eti record for the exported type "polymorphic_derived2"
    boost::serialization::extended_type_info const * const d2_eti =
        boost::serialization::extended_type_info::find(
            "polymorphic_derived2"
        );
    assert(NULL != d2_eti);

    BOOST_CHECK_MESSAGE(
        * d2_eti
        == 
        * boost::serialization::type_info_implementation<polymorphic_base>
            ::type::get_const_instance().get_derived_extended_type_info(*rb2),
        "restored pointer b2 not of correct type"
    );

    delete rb1;
    delete rb2;
}

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

int
test_main( int /* argc */, char* /* argv */[] )
{
    const char * testfile = boost::archive::tmpnam(NULL);
    BOOST_REQUIRE(NULL != testfile);

    HINSTANCE hDLL;               // Handle to DLL
    hDLL = LoadLibrary("derived2.dll");
    save_exported(testfile);
    load_exported(testfile);
    FreeLibrary(hDLL);

    std::remove(testfile);
    return EXIT_SUCCESS;
}

// EOF
