
// Copyright 2006-2009 Daniel James.
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#define HASH_NAMESPACE boost

// Simple test to make sure BOOST_HASH_NO_EXTENSIONS does disable extensions
// (or at least one of them).
#define BOOST_HASH_NO_EXTENSIONS

#include <boost/functional/hash.hpp>
#include <boost/functional/hash.hpp>

int main()
{
    HASH_NAMESPACE::hash< int[10] > hasher;
    return 0;
}
