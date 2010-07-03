//  Copyright (C) 2007 Douglas Gregor
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org/libs/config for most recent version.

//  MACRO:         BOOST_HAS_CONCEPTS
//  TITLE:         concepts
//  DESCRIPTION:   The compiler supports C++0x concepts

namespace boost_has_concepts {

concept C<typename T> { }

concept_map C<int> { }

int test()
{
   return 0;
}

}
