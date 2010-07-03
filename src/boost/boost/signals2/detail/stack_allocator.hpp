/*
  An allocator which first allocates from the stack, before falling
  back on usual std::allocator behavior.  Used by signals2 to
  optimize the vector of tracked shared_ptr created during signal
  invocation.

  Example usage:

  static const std::size_t n = 10;
  stack_storage<T, n> storage;
  stack_allocator<T, n> a(&storage);
  std::vector<T, stack_allocator<T, n> > v(a);

*/
// Copyright Frank Mori Hess 2008.
// Distributed under the Boost Software License, Version
// 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

// See http://www.boost.org/libs/signals2 for library home page.

#ifndef BOOST_SIGNALS2_STACK_ALLOCATOR_HPP
#define BOOST_SIGNALS2_STACK_ALLOCATOR_HPP

#include <memory>
#include <boost/noncopyable.hpp>
#include <boost/type_traits/aligned_storage.hpp>
#include <boost/type_traits/alignment_of.hpp>

namespace boost
{
  namespace signals2
  {
    namespace detail
    {
      template<typename T, std::size_t n_stack_elements>
        class stack_storage: public boost::noncopyable
      {
      public:
        typedef typename boost::aligned_storage<sizeof(T), boost::alignment_of<T>::value>::type storage_type;
        stack_storage(): is_reserved(false)
        {
        }
        storage_type array[n_stack_elements];
        bool is_reserved;
      };
      template<typename T, std::size_t n_stack_elements>
        class stack_allocator: public std::allocator<T>
      {
        typedef std::allocator<T> base_class;
      public:
        template<typename U>
          struct rebind
        {
          typedef stack_allocator<U, n_stack_elements> other;
        };
        stack_allocator(stack_storage<T, n_stack_elements> *storage = 0):
          _storage(storage)
        {
        }
        typename base_class::pointer allocate(typename base_class::size_type n_elements,
          std::allocator<void>::const_pointer hint = 0)
        {
          if(_storage && _storage->is_reserved == false &&
            n_elements <= n_stack_elements)
          {
            _storage->is_reserved = true;
            return reinterpret_cast<typename base_class::pointer>(&_storage->array[0]);
          }
          return base_class::allocate(n_elements, hint);
        }
        void deallocate(typename base_class::pointer p, typename base_class::size_type n)
        {
          if(_storage &&
            p == reinterpret_cast<typename base_class::pointer>(&_storage->array[0]))
          {
            _storage->is_reserved = false;
          }else
          {
            base_class::deallocate(p, n);
          }
        }
      private:
        stack_storage<T, n_stack_elements> *_storage;
      };
    } // namespace detail
  } // namespace signals2
} // namespace boost

#endif  // BOOST_SIGNALS2_STACK_ALLOCATOR_HPP
