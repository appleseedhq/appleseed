/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
// extended_type_info_no_rtti.cpp: specific implementation of type info
// that is NOT based on typeid

// (C) Copyright 2002 Robert Ramey - http://www.rrsd.com . 
// Use, modification and distribution is subject to the Boost Software
// License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org for updates, documentation, and revision history.

#include <cstring>
#include <cstddef> // NULL

#include <boost/config.hpp>
#if defined(BOOST_NO_STDC_NAMESPACE)
namespace std{ using ::strcmp; }
#endif

#define BOOST_SERIALIZATION_SOURCE
#include <boost/serialization/extended_type_info_no_rtti.hpp>

#define EXTENDED_TYPE_INFO_NO_RTTI_KEY 2

namespace boost { 
namespace serialization { 
namespace detail { 

BOOST_SERIALIZATION_DECL(BOOST_PP_EMPTY())  
extended_type_info_no_rtti_0::extended_type_info_no_rtti_0() :
    extended_type_info(EXTENDED_TYPE_INFO_NO_RTTI_KEY)
{}

BOOST_SERIALIZATION_DECL(bool)
extended_type_info_no_rtti_0::is_less_than(
    const boost::serialization::extended_type_info &rhs) const 
{
    const char * l = m_key;
    const char * r = rhs.get_key();
    assert(NULL != l);
    assert(NULL != r);
    return std::strcmp(l, r) < 0;
}

BOOST_SERIALIZATION_DECL(bool)
extended_type_info_no_rtti_0::is_equal(
    const boost::serialization::extended_type_info &rhs) const 
{
    const char * l = m_key;
    const char * r = rhs.get_key();
    if(l == r)
        return true;
    if(NULL == l)
        return false;
    if(NULL == r)
        return false;
    return 0 == std::strcmp(l, r);
}

BOOST_SERIALIZATION_DECL(BOOST_PP_EMPTY())  
extended_type_info_no_rtti_0::~extended_type_info_no_rtti_0()
{}

} // namespece detail
} // namespace serialization
} // namespace boost
