
// Copyright 2005-2009 Daniel James.
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#if defined(TEST_STD)
#  define TEST_STD_INCLUDES
#  define HASH_NAMESPACE std::tr1
#else
#  define HASH_NAMESPACE boost
#  if !defined(BOOST_HASH_NO_EXTENSIONS)
#    define TEST_EXTENSIONS
#  endif
#endif
