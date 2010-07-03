/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
// extended_type_info_typeid.cpp: specific implementation of type info
// that is based on typeid

// (C) Copyright 2002 Robert Ramey - http://www.rrsd.com . 
// Use, modification and distribution is subject to the Boost Software
// License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org for updates, documentation, and revision history.

#include <algorithm>
#include <set>
#include <cassert>
#include <typeinfo>
#include <cstddef> // NULL

#include <boost/detail/no_exceptions_support.hpp>

#include <boost/serialization/singleton.hpp>

#define BOOST_SERIALIZATION_SOURCE
#include <boost/serialization/extended_type_info_typeid.hpp>

namespace boost { 
namespace serialization { 
namespace detail {

#define EXTENDED_TYPE_INFO_TYPE_KEY 1

struct type_compare
{
    bool
    operator()(
        const extended_type_info_typeid_0 * lhs,
        const extended_type_info_typeid_0 * rhs
    ) const {
        return lhs->is_less_than(*rhs);
    }
};

typedef std::multiset<
    const extended_type_info_typeid_0 *,
    type_compare
> tkmap;
    
BOOST_SERIALIZATION_DECL(bool) 
extended_type_info_typeid_0::is_less_than(
    const boost::serialization::extended_type_info & rhs
) const {
    return static_cast<bool>(m_ti->before(
        *(static_cast<const extended_type_info_typeid_0 &>(rhs).m_ti)
    ));
}

BOOST_SERIALIZATION_DECL(bool) 
extended_type_info_typeid_0::is_equal(
    const boost::serialization::extended_type_info & rhs
) const {
    return static_cast<bool>(
        * m_ti 
        == *(static_cast<const extended_type_info_typeid_0 &>(rhs).m_ti)
    );
}

BOOST_SERIALIZATION_DECL(BOOST_PP_EMPTY())
extended_type_info_typeid_0::extended_type_info_typeid_0() :
    extended_type_info(EXTENDED_TYPE_INFO_TYPE_KEY),
    m_ti(NULL)
{}

BOOST_SERIALIZATION_DECL(BOOST_PP_EMPTY())
extended_type_info_typeid_0::~extended_type_info_typeid_0()
{}

BOOST_SERIALIZATION_DECL(void) 
extended_type_info_typeid_0::type_register(const std::type_info & ti){
    m_ti = & ti;
    singleton<tkmap>::get_mutable_instance().insert(this);
}

BOOST_SERIALIZATION_DECL(void) 
extended_type_info_typeid_0::type_unregister()
{
    if(NULL != m_ti){
        if(! singleton<tkmap>::is_destroyed()){
            tkmap & x = singleton<tkmap>::get_mutable_instance();
            tkmap::iterator start = x.lower_bound(this);
            tkmap::iterator end = x.upper_bound(this);
            assert(start != end);

            // remove entry in map which corresponds to this type
            do{
            if(this == *start)
            	x.erase(start++);
     	    else
                ++start;
            }while(start != end);
        }
    }
    m_ti = NULL;
}

// this derivation is used for creating search arguments
class extended_type_info_typeid_arg : 
    public extended_type_info_typeid_0
{
private:
public:
    extended_type_info_typeid_arg(const std::type_info & ti){ 
        // note absense of self register and key as this is used only as
        // search argument given a type_info reference and is not to 
        // be added to the map.
        m_ti = & ti;
    }
    ~extended_type_info_typeid_arg(){
        m_ti = NULL;
    }
};

BOOST_SERIALIZATION_DECL(const extended_type_info *)
extended_type_info_typeid_0::get_extended_type_info(
    const std::type_info & ti
) const {
    detail::extended_type_info_typeid_arg etia(ti);
    const tkmap & t = singleton<tkmap>::get_const_instance();
    const tkmap::const_iterator it = t.find(& etia);
    if(t.end() == it)
        return NULL;
    return *(it);
}

} // namespace detail
} // namespace serialization
} // namespace boost
