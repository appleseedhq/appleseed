/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
// extended_type_info.cpp: implementation for portable version of type_info

// (C) Copyright 2002 Robert Ramey - http://www.rrsd.com . 
// Use, modification and distribution is subject to the Boost Software
// License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org for updates, documentation, and revision history.

#if (defined _MSC_VER) && (_MSC_VER == 1200)
#  pragma warning (disable : 4786) // too long name, harmless warning
#endif

#include <algorithm>
#include <set>
#include <utility>
#include <cassert>
#include <cstddef> // NULL

#include <boost/config.hpp> // msvc needs this to suppress warning

#include <cstring>
#if defined(BOOST_NO_STDC_NAMESPACE)
namespace std{ using ::strcmp; }
#endif

#include <boost/detail/no_exceptions_support.hpp>
#include <boost/serialization/singleton.hpp>
#include <boost/serialization/force_include.hpp>

#define BOOST_SERIALIZATION_SOURCE
#include <boost/serialization/extended_type_info.hpp>

namespace boost { 
namespace serialization {
namespace detail {

struct key_compare
{
    bool
    operator()(
        const extended_type_info * lhs, 
        const extended_type_info * rhs
    ) const {
        const char * l = lhs->get_key();
        assert(NULL != l);
        const char * r = rhs->get_key();
        assert(NULL != r);
        // performance shortcut
        // shortcut to exploit string pooling
        if(l == r)
            return false;
        // for exported types, use the string key so that
        // multiple instances in different translation units
        // can be matched up
        return std::strcmp(l, r) < 0;
    }
};

typedef std::multiset<const extended_type_info *, key_compare> ktmap;

class extended_type_info_arg : public extended_type_info
{
public:
    extended_type_info_arg(const char * key) :
        extended_type_info()
    {
        m_key = key;
    }
    ~extended_type_info_arg(){
        m_key = NULL;
    }
};

} // namespace detail

BOOST_SERIALIZATION_DECL(void)  
extended_type_info::key_register(const char *key) {
    assert(NULL != key);
    m_key = key;
    singleton<detail::ktmap>::get_mutable_instance().insert(this);
}

BOOST_SERIALIZATION_DECL(void)  
extended_type_info::key_unregister() {
    assert(NULL != m_key);
    if(! singleton<detail::ktmap>::is_destroyed()){
        detail::ktmap & x = singleton<detail::ktmap>::get_mutable_instance();
        detail::ktmap::iterator start = x.lower_bound(this);
        detail::ktmap::iterator end = x.upper_bound(this);
        assert(start != end);

        // remove entry in map which corresponds to this type
        for(;start != end; ++start){
            if(this == *start){
                x.erase(start);
                break;
            }
        }
    }
    m_key = NULL;
}

BOOST_SERIALIZATION_DECL(const extended_type_info *) 
extended_type_info::find(const char *key) {
    assert(NULL != key);
    const detail::ktmap & k = singleton<detail::ktmap>::get_const_instance();
    const detail::extended_type_info_arg eti_key(key);
    const detail::ktmap::const_iterator it = k.find(& eti_key);
    if(k.end() == it)
        return NULL;
    return *(it);
}

BOOST_SERIALIZATION_DECL(BOOST_PP_EMPTY())
extended_type_info::extended_type_info(
    const unsigned int type_info_key
) :
    m_type_info_key(type_info_key),
    m_key(NULL)
{
}

BOOST_SERIALIZATION_DECL(BOOST_PP_EMPTY()) 
extended_type_info::~extended_type_info(){
    if(NULL == m_key)
        return;
    key_unregister();
}

BOOST_SERIALIZATION_DECL(bool)  
extended_type_info::operator<(const extended_type_info &rhs) const {
    if(m_type_info_key == rhs.m_type_info_key){
        return is_less_than(rhs);
    }
    if(m_type_info_key < rhs.m_type_info_key)
        return true;
    return false;
}

BOOST_SERIALIZATION_DECL(bool)
extended_type_info::operator==(const extended_type_info &rhs) const {
    // short cut for a common cases
    if(this == & rhs)
        return true;
    if(m_type_info_key != rhs.m_type_info_key){
        return false;
    }
    return is_equal(rhs);
}

} // namespace serialization
} // namespace boost
