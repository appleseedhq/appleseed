/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
// binary_wiarchive.cpp:

// (C) Copyright 2002 Robert Ramey - http://www.rrsd.com . 
// Use, modification and distribution is subject to the Boost Software
// License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org for updates, documentation, and revision history.

#include <boost/config.hpp>

#ifdef BOOST_NO_STD_WSTREAMBUF
#error "wide char i/o not supported on this platform"
#else

#define BOOST_WARCHIVE_SOURCE
#include <boost/archive/binary_wiarchive.hpp>

#include <boost/archive/impl/basic_binary_iprimitive.ipp>
#include <boost/archive/impl/basic_binary_iarchive.ipp>
#include <boost/archive/impl/archive_pointer_iserializer.ipp>

namespace boost {
namespace archive {

// explicitly instantiate for this type of text stream
template class basic_binary_iprimitive<
    naked_binary_wiarchive,
    wchar_t, 
    std::char_traits<wchar_t> 
>;
template class basic_binary_iarchive<naked_binary_wiarchive> ;
template class binary_iarchive_impl<
    naked_binary_wiarchive, 
    wchar_t, 
    std::char_traits<wchar_t> 
>;
template class detail::archive_pointer_iserializer<naked_binary_wiarchive> ;

// explicitly instantiate for this type of text stream
template class basic_binary_iprimitive<
    binary_wiarchive,
    wchar_t, 
    std::char_traits<wchar_t> 
>;
template class basic_binary_iarchive<binary_wiarchive> ;
template class binary_iarchive_impl<
    binary_wiarchive, 
    wchar_t, 
    std::char_traits<wchar_t> 
>;
template class detail::archive_pointer_iserializer<binary_wiarchive> ;

} // namespace archive
} // namespace boost

#endif  // BOOST_NO_STD_WSTREAMBUF

