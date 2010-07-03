/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
// basic_archive.cpp:

// (C) Copyright 2002 Robert Ramey - http://www.rrsd.com . 
// Use, modification and distribution is subject to the Boost Software
// License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org for updates, documentation, and revision history.

//////////////////////////////////////////////////////////////////////
//
//  objects are stored as
//
//      class_id*   // -1 for a null pointer
//      if a new class id
//      [
//          exported key - class name*
//          tracking level - always/never
//          class version
//      ]
//
//      if tracking
//      [
//          object_id
//      ]
//          
//      [   // if a new object id
//          data...
//      ]
//
//  * required only for pointers - optional for objects

#define BOOST_ARCHIVE_SOURCE
#include <boost/archive/basic_archive.hpp>

namespace boost {
namespace archive {

///////////////////////////////////////////////////////////////////////
// constants used in archive signature
//This should never ever change. note that is not an std::string
// string.
BOOST_ARCHIVE_DECL(const char *) 
BOOST_ARCHIVE_SIGNATURE(){
    return "serialization::archive";
}

// this should change if the capabilities are added to the library
// such that archives can be created which can't be read by previous
// versions of this library
// 1 - initial version
// 2 - made address tracking optional
// 3 - numerous changes - can't guarentee compatibility with previous versions
// 4 - Boost 1.34
//     added register_version to properly support versioning for collections 
// 5 - Boost 1.36
//     changed serialization of collections: adding version even for primitive
//     types caused backwards compatibility breaking change in 1.35

BOOST_ARCHIVE_DECL(unsigned char)
BOOST_ARCHIVE_VERSION(){
    return 5;
}

} // namespace archive
} // namespace boost
