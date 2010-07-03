#ifndef BOOST_SERIALIZATION_TEST_DERIVED2_HPP
#define BOOST_SERIALIZATION_TEST_DERIVED2_HPP

// MS compatible compilers support #pragma once
#if defined(_MSC_VER) && (_MSC_VER >= 1020)
# pragma once
#endif

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
// derived2.hpp    simple class test

// (C) Copyright 2002 Robert Ramey - http://www.rrsd.com . 
// Use, modification and distribution is subject to the Boost Software
// License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org for updates, documentation, and revision history.

#include <boost/serialization/export.hpp>
#include <boost/serialization/access.hpp>

#include "base.hpp"

#ifndef DLL_DECL
#define DLL_DECL
#endif

class DLL_DECL polymorphic_derived2 : public polymorphic_base
{
    friend class boost::serialization::access;
    template<class Archive>
    void serialize(Archive &ar, const unsigned int /* file_version */);
public:
    ~polymorphic_derived2(){}
};

// MWerks users can do this to make their code work
BOOST_SERIALIZATION_MWERKS_BASE_AND_DERIVED(polymorphic_base, polymorphic_derived2)

#endif // BOOST_SERIALIZATION_TEST_DERIVED2_HPP
