/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
// dll_derived2.cpp

// (C) Copyright 2002 Robert Ramey - http://www.rrsd.com . 
// Use, modification and distribution is subject to the Boost Software
// License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

// Build a dll which contains the serialization for a class A
// used in testing distribution of serialization code in DLLS
#include "test_decl.hpp"

#define DLL_DECL IMPORT_DECL
#include "base.hpp"
#undef  DLL_DECL

#define DLL_DECL EXPORT_DECL(BOOST_PP_EMPTY())
#include "derived2.hpp"
#undef  DLL_DECL

#include <boost/serialization/nvp.hpp>
template<class Archive>
void polymorphic_derived2::serialize(
    Archive &ar, 
    const unsigned int /* file_version */
){
    ar & BOOST_SERIALIZATION_BASE_OBJECT_NVP(polymorphic_base);
}

// instantiate code for text archives
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>

// instantiate code for polymorphic archives
#include <boost/archive/polymorphic_oarchive.hpp>
#include <boost/archive/polymorphic_iarchive.hpp>

// note: export has to be AFTER #includes for all archive classes

#include <boost/serialization/factory.hpp>
BOOST_SERIALIZATION_FACTORY_0(polymorphic_derived2)
BOOST_CLASS_EXPORT(polymorphic_derived2)
