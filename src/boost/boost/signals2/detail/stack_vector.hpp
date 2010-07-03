/*
  A non-copyable vector which first allocates from the stack, before falling
  back on usual std::allocator behavior.

*/
// Copyright Frank Mori Hess 2008.
// Distributed under the Boost Software License, Version
// 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

// See http://www.boost.org/libs/signals2 for library home page.

#ifndef BOOST_SIGNALS2_STACK_VECTOR_HPP
#define BOOST_SIGNALS2_STACK_VECTOR_HPP

#include <boost/noncopyable.hpp>
#include <boost/signals2/detail/stack_allocator.hpp>
#include <vector>

namespace boost
{
  namespace signals2
  {
    namespace detail
    {
      template<typename T, std::size_t NumStackElements>
        class stack_vector:
        public std::vector<T, stack_allocator<T, NumStackElements> >,
        public boost::noncopyable
      {
        typedef std::vector<T, stack_allocator<T, NumStackElements> > base_vector_type;
      public:
        static const std::size_t num_stack_elements = NumStackElements;
        stack_vector(): base_vector_type(stack_allocator<T, num_stack_elements>(&_storage))
        {
          base_vector_type::reserve(num_stack_elements);
        }
      private:
        stack_storage<T, num_stack_elements> _storage;
      };
      template<typename T, std::size_t NumStackElements>
        const std::size_t stack_vector<T, NumStackElements>::num_stack_elements;

    } // namespace detail
  } // namespace signals2
} // namespace boost

#endif  // BOOST_SIGNALS2_STACK_VECTOR_HPP
