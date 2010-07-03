#ifndef BOOST_SERIALIZATION_TEST_BASE_HPP
#define BOOST_SERIALIZATION_TEST_BASE_HPP

// MS compatible compilers support #pragma once
#if defined(_MSC_VER) && (_MSC_VER >= 1020)
# pragma once
#endif

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
// base.hpp    simple class test

// (C) Copyright 2002 Robert Ramey - http://www.rrsd.com . 
// Use, modification and distribution is subject to the Boost Software
// License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org for updates, documentation, and revision history.

#include <boost/serialization/access.hpp>
#include <boost/serialization/assume_abstract.hpp>

#ifndef DLL_DECL
#define DLL_DECL
#endif

class DLL_DECL polymorphic_base
{
    friend class boost::serialization::access;
    template<class Archive>
    void serialize(Archive & /* ar */, const unsigned int /* file_version */);
public:
    virtual ~polymorphic_base(){};
};

BOOST_SERIALIZATION_ASSUME_ABSTRACT(polymorphic_base)

#endif // BOOST_SERIALIZATION_TEST_BASE_HPP
