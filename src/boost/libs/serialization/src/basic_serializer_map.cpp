/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
// serializer_map.cpp:

// (C) Copyright 2002 Robert Ramey - http://www.rrsd.com . 
// Use, modification and distribution is subject to the Boost Software
// License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org for updates, documentation, and revision history.

#if (defined _MSC_VER) && (_MSC_VER == 1200)
#  pragma warning (disable : 4786) // too long name, harmless warning
#endif

#include <stdlib.h> // for NULL

#define BOOST_ARCHIVE_SOURCE
#include <boost/archive/detail/basic_serializer.hpp>
#include <boost/archive/detail/basic_serializer_map.hpp>

namespace boost {
    namespace serialization {
        class extended_type_info;
    }
namespace archive {
namespace detail {

#if 0
BOOST_ARCHIVE_DECL(bool) 
type_info_pointer_compare::operator()(
    const basic_serializer * lhs, const basic_serializer * rhs
) const {
    return *lhs < *rhs;
}

class basic_serializer_arg : public basic_serializer {
public:
    basic_serializer_arg(const serialization::extended_type_info & eti) :
        basic_serializer(eti)
    {}
};
#endif

} // namespace detail
} // namespace archive
} // namespace boost

#if 0
BOOST_ARCHIVE_DECL(BOOST_PP_EMPTY())
basic_serializer_map::basic_serializer_map(bool & deleted) :
    m_deleted(deleted)
{
    m_deleted = false;
}

BOOST_ARCHIVE_DECL(const basic_serializer *) 
basic_serializer_map::tfind(
    const boost::serialization::extended_type_info & eti
) const {
    const basic_serializer_arg bs(eti);
    map_type::const_iterator it;
    boost::serialization::singleton<basic_serializer_map>::lease l;
    it = l->m_map.find(& bs);
    if(it == l->m_map.end())
        return NULL;
    return *it;
}

BOOST_ARCHIVE_DECL(BOOST_PP_EMPTY())
basic_serializer_map::~basic_serializer_map(){
    m_deleted = true;
}

BOOST_ARCHIVE_DECL(bool) 
basic_serializer_map::insert(const basic_serializer * bs){
    boost::serialization::singleton<basic_serializer_map>::lease l;
    return l->m_map.insert(bs).second;
}

BOOST_ARCHIVE_DECL(void) 
basic_serializer_map::erase(basic_serializer * bs){
    boost::serialization::singleton<basic_serializer_map>::lease l;
    l->m_map.erase(bs);
}
#endif
