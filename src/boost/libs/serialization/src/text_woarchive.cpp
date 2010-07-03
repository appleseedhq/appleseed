/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
// text_woarchive.cpp:

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
#include <boost/archive/text_woarchive.hpp>

#include <boost/archive/impl/basic_text_oarchive.ipp>
#include <boost/archive/impl/text_woarchive_impl.ipp>
#include <boost/archive/impl/archive_pointer_oserializer.ipp>

namespace boost {
namespace archive {

template class basic_text_oarchive<text_woarchive> ;
template class text_woarchive_impl<text_woarchive> ;
template class detail::archive_pointer_oserializer<text_woarchive> ;

} // namespace archive
} // namespace boost

#endif // BOOST_NO_STD_WSTREAMBUF
