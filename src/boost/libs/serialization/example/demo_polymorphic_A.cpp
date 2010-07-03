/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
// demo_polymorphic_A.cpp

// (C) Copyright 2002 Robert Ramey - http://www.rrsd.com . 
// Use, modification and distribution is subject to the Boost Software
// License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)


#include <boost/archive/polymorphic_iarchive.hpp>
#include <boost/archive/polymorphic_oarchive.hpp>

#include "demo_polymorphic_A.hpp"

// now we can define the serialization for class A
void A::serialize(
    boost::archive::polymorphic_iarchive & ar, 
    const unsigned int file_version
){
    ar & data;
}
void A::serialize(
    boost::archive::polymorphic_oarchive & ar, 
    const unsigned int file_version
){
    ar & data;
}
