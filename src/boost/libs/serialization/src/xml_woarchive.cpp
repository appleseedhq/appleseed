/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
// xml_woarchive.cpp:

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
#include <boost/archive/xml_woarchive.hpp>

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
// implemenations of functions specific to wide char archives

#include <boost/archive/impl/basic_xml_oarchive.ipp>
#include <boost/archive/impl/xml_woarchive_impl.ipp>
#include <boost/archive/impl/archive_pointer_oserializer.ipp>

namespace boost {
namespace archive {

#if 0
BOOST_WARCHIVE_DECL(std::wostream &)
operator<<(std::wostream &os, const char *t){
    for(;;){
        wchar_t wc;
        int result = std::mbtowc(&wc, t, 10 /* max number */);
        if(0 < result)
            os.put(wc);
        else
        if(0 == result)
            break;
        else
            boost::serialization::throw_exception(
                iterators::dataflow_exception(
                    iterators::dataflow_exception::invalid_conversion
                )
            );
    }
    return os;
}

BOOST_WARCHIVE_DECL(std::wostream &)
operator<<(std::wostream &os, const char t){
    wchar_t wc;
    std::mbtowc(&wc, &t, 1);
    os.put(wc);
    return os;
}
#endif

template class basic_xml_oarchive<xml_woarchive> ;
template class xml_woarchive_impl<xml_woarchive> ;
template class detail::archive_pointer_oserializer<xml_woarchive> ;

} // namespace archive
} // namespace boost

#endif // BOOST_NO_STD_WSTREAMBUF
