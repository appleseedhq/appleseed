
//  (C) Copyright John Maddock 2002. 
//  Use, modification and distribution are subject to the 
//  Boost Software License, Version 1.0. (See accompanying file 
//  LICENSE_1_0.txt or copy at http://www.tt.org/LICENSE_1_0.txt)

#include "test.hpp"
#include "check_type.hpp"
#ifdef TEST_STD
#  include <type_traits>
#else
#  include <boost/type_traits/add_pointer.hpp>
#endif

BOOST_DECL_TRANSFORM_TEST(add_pointer_test_5, ::tt::add_pointer, const &, const*)
BOOST_DECL_TRANSFORM_TEST(add_pointer_test_6, ::tt::add_pointer, &, *)
BOOST_DECL_TRANSFORM_TEST(add_pointer_test_8, ::tt::add_pointer, const [2], const (*)[2])
BOOST_DECL_TRANSFORM_TEST(add_pointer_test_9, ::tt::add_pointer, const &, const*)
BOOST_DECL_TRANSFORM_TEST(add_pointer_test_12, ::tt::add_pointer, const[2][3], const (*)[2][3])
BOOST_DECL_TRANSFORM_TEST(add_pointer_test_13, ::tt::add_pointer, (&)[2], (*)[2])


TT_TEST_BEGIN(tricky_add_pointer_test)

   add_pointer_test_5();
   add_pointer_test_6();
   add_pointer_test_8();
   add_pointer_test_9();
   add_pointer_test_12();
   add_pointer_test_13();

TT_TEST_END








