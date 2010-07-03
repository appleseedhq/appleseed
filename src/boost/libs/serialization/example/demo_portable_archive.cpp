/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
// demo_portable_archive.cpp
//
// (C) Copyright 2002-4 Robert Ramey - http://www.rrsd.com .
// Use, modification and distribution is subject to the Boost Software
// License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

// should pass compilation and execution
#include <sstream>

#include "portable_binary_oarchive.hpp"
#include "portable_binary_iarchive.hpp"

#include <cstdlib>
#include <boost/config.hpp>
#if defined(BOOST_NO_STDC_NAMESPACE)
namespace std{ using ::rand; }
#endif

class A
{
    friend class boost::serialization::access;
    char c;
    int i;
    int i2; // special tricky case to check sign extension
    unsigned int ui;
    long l;
    unsigned long ul;
    template<class Archive>
    void serialize(Archive & ar, const unsigned int /* version */){
        ar & c & i & i2 & ui & l & ul ;
    }
public:
    bool operator==(const A & rhs) const {
        return
            c == rhs.c
            && i == rhs.i
            && i2 == rhs.i2
            && ui == rhs.ui 
            && l == rhs.l 
            && ul == rhs.ul
        ;
    }
    A() :
        c(std::rand()),
        i(std::rand()),
        i2(0x80),
        ui(std::rand()),
        l(std::rand() * std::rand()),
        ul(std::rand())
    {}
};

int main( int /* argc */, char* /* argv */[] )
{
    const A a;
    A a1;

    std::stringstream ss;
    {   
        portable_binary_oarchive pboa(ss);
        pboa << a;
    }
    {
        portable_binary_iarchive pbia(ss);
        pbia >> a1;
    }
    if(! (a == a1))
        return 1;

    ss.clear();
    {   
        portable_binary_oarchive pboa(ss, endian_big);
        pboa << a;
    }
    {
        portable_binary_iarchive pbia(ss, endian_big);
        pbia >> a1;
    }
    if(! (a == a1))
        return 1;

    ss.clear();
    {   
        portable_binary_oarchive pboa(ss, endian_big);
        pboa << a;
    }
    {
        portable_binary_iarchive pbia(ss, endian_big);
        pbia >> a1;
    }

    return !(a == a1);
}


