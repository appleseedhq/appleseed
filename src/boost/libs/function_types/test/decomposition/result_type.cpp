
// (C) Copyright Tobias Schwinger
//
// Use modification and distribution are subject to the boost Software License,
// Version 1.0. (See http://www.boost.org/LICENSE_1_0.txt).

//------------------------------------------------------------------------------

#include <boost/mpl/assert.hpp>
#include <boost/type_traits/is_same.hpp>

#include <boost/function_types/result_type.hpp>


namespace ft = boost::function_types;

class C; 
typedef C func();
typedef C (*func_ptr)();
typedef C (&func_ref)();
typedef C (C::*mem_func_ptr)();
typedef C (C::*c_mem_func_ptr)() const;
typedef C (C::*v_mem_func_ptr)() volatile;
typedef C (C::*cv_mem_func_ptr)() const volatile;
typedef int C::* mem_ptr;
typedef int const C::* c_mem_ptr;

BOOST_MPL_ASSERT((
  boost::is_same<ft::result_type<func>::type,C>
));

BOOST_MPL_ASSERT((
  boost::is_same<ft::result_type<func_ptr>::type,C>
));

BOOST_MPL_ASSERT((
  boost::is_same<ft::result_type<func_ref>::type,C>
));

BOOST_MPL_ASSERT((
  boost::is_same<ft::result_type<mem_func_ptr>::type,C>
));

BOOST_MPL_ASSERT((
  boost::is_same<ft::result_type<c_mem_func_ptr>::type,C>
));

BOOST_MPL_ASSERT((
  boost::is_same<ft::result_type<v_mem_func_ptr>::type,C>
));

BOOST_MPL_ASSERT((
  boost::is_same<ft::result_type<cv_mem_func_ptr>::type,C>
));

BOOST_MPL_ASSERT((
  boost::is_same<ft::result_type<mem_ptr>::type,int&>
));

BOOST_MPL_ASSERT((
  boost::is_same<ft::result_type<c_mem_ptr>::type,int const&>
));

