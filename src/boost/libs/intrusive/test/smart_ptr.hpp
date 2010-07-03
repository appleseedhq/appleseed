//////////////////////////////////////////////////////////////////////////////
//
// (C) Copyright Ion Gaztanaga 2006. Distributed under the Boost
// Software License, Version 1.0. (See accompanying file
// LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
// See http://www.boost.org/libs/intrusive for documentation.
//
//////////////////////////////////////////////////////////////////////////////

#ifndef BOOST_INTRUSIVE_SMART_PTR_HPP
#define BOOST_INTRUSIVE_SMART_PTR_HPP

#include <boost/iterator.hpp>
#include <boost/intrusive/pointer_plus_bits.hpp>

#if (defined _MSC_VER) && (_MSC_VER >= 1200)
#  pragma once
#endif

namespace boost{
namespace intrusive{

namespace detail {

struct static_cast_tag {};
struct const_cast_tag {};
struct dynamic_cast_tag {};
struct reinterpret_cast_tag {};

}  //namespace detail {

//Empty class
struct empty_type{};

template<class T>
struct random_it 
: public boost::iterator<std::random_access_iterator_tag, 
                         T, std::ptrdiff_t, T*, T&> 
{
   typedef const T*           const_pointer;
   typedef const T&           const_reference;
};

template<> struct random_it<void>
{
   typedef const void *       const_pointer;
   typedef empty_type&        reference;
   typedef const empty_type&  const_reference;
   typedef empty_type         difference_type;
   typedef empty_type         iterator_category;
};

template<> struct random_it<const void>
{
   typedef const void *       const_pointer;
   typedef const empty_type & reference;
   typedef const empty_type & const_reference;
   typedef empty_type         difference_type;
   typedef empty_type         iterator_category;
};

template<> struct random_it<volatile void>
{
   typedef const volatile void * const_pointer;
   typedef empty_type&           reference;
   typedef const empty_type&     const_reference;
   typedef empty_type            difference_type;
   typedef empty_type            iterator_category;
};

template<> struct random_it<const volatile void>
{
   typedef const volatile void *    const_pointer;
   typedef const empty_type &       reference;
   typedef const empty_type &       const_reference;
   typedef empty_type               difference_type;
   typedef empty_type               iterator_category;
};

}  //namespace intrusive {
}  //namespace boost {


namespace boost {
namespace intrusive {

template <class PointedType>
class smart_ptr
{
   typedef random_it<PointedType> random_it_t;
   typedef smart_ptr<PointedType>                           self_t;
   typedef typename random_it_t::const_pointer              const_pointer_t;
   typedef typename random_it_t::const_reference            const_reference_t;

   void unspecified_bool_type_func() const {}
   typedef void (self_t::*unspecified_bool_type)() const;

   public:
   typedef PointedType *                           pointer;
   typedef typename random_it_t::reference         reference;
   typedef PointedType                             value_type;
   typedef typename random_it_t::difference_type   difference_type;
   typedef typename random_it_t::iterator_category iterator_category;

   PointedType *m_ptr;

   public:   //Public Functions

   //!Constructor from raw pointer (allows "0" pointer conversion). Never throws.
   explicit smart_ptr(pointer ptr = 0)
      :  m_ptr(ptr)
   {}

   //!Constructor from other pointer. Never throws.
   template <class T>
   smart_ptr(T *ptr) 
      :  m_ptr(ptr)
   {}

   //!Constructor from other smart_ptr 
   smart_ptr(const smart_ptr& ptr)
      :  m_ptr(ptr.m_ptr)
   {}

   //!Constructor from other smart_ptr. If pointers of pointee types are 
   //!convertible, offset_ptrs will be convertibles. Never throws.
   template<class T2>
   smart_ptr(const smart_ptr<T2> &ptr) 
      :  m_ptr(ptr.m_ptr)
   {}

   //!Emulates static_cast operator. Never throws.
   template<class Y>
   smart_ptr(const smart_ptr<Y> & r, detail::static_cast_tag)
      :  m_ptr(static_cast<PointedType*>(r.get()))
   {}

   //!Emulates const_cast operator. Never throws.
   template<class Y>
   smart_ptr(const smart_ptr<Y> & r, detail::const_cast_tag)
      :  m_ptr(const_cast<PointedType*>(r.get()))
   {}

   //!Emulates dynamic_cast operator. Never throws.
   template<class Y>
   smart_ptr(const smart_ptr<Y> & r, detail::dynamic_cast_tag)
      :  m_ptr(dynamic_cast<PointedType*>(r.get()))
   {}

   //!Emulates reinterpret_cast operator. Never throws.
   template<class Y>
   smart_ptr(const smart_ptr<Y> & r, detail::reinterpret_cast_tag)
      :  m_ptr(reinterpret_cast<PointedType*>(r.get()))
   {}

   //!Obtains raw pointer from offset. Never throws.
   pointer get() const
   {  return m_ptr;   }

   //!Pointer-like -> operator. It can return 0 pointer. Never throws.
   pointer operator->() const           
   {  return this->get(); }

   //!Dereferencing operator, if it is a null smart_ptr behavior 
   //!   is undefined. Never throws.
   reference operator* () const           
   {  return *(this->get());   }

   //!Indexing operator. Never throws.
   reference operator[](std::ptrdiff_t idx) const   
   {  return this->get()[idx];  }

   //!Assignment from pointer (saves extra conversion). Never throws.
   smart_ptr& operator= (pointer from)
   {  m_ptr = from;  return *this;  }

   //!Assignment from other smart_ptr. Never throws.
   smart_ptr& operator= (const smart_ptr & pt)
   {  m_ptr = pt.m_ptr;  return *this;  }

   //!Assignment from related smart_ptr. If pointers of pointee types 
   //!   are assignable, offset_ptrs will be assignable. Never throws.
   template <class T2>
   smart_ptr& operator= (const smart_ptr<T2> & pt)
   {  m_ptr = pt.m_ptr;  return *this;  }
 
   //!smart_ptr + std::ptrdiff_t. Never throws.
   smart_ptr operator+ (std::ptrdiff_t offset) const   
   {  return smart_ptr(this->get()+offset);   }

   //!smart_ptr - std::ptrdiff_t. Never throws.
   smart_ptr operator- (std::ptrdiff_t offset) const   
   {  return smart_ptr(this->get()-offset);   }

   //!smart_ptr += std::ptrdiff_t. Never throws.
   smart_ptr &operator+= (std::ptrdiff_t offset)
   {  m_ptr += offset;   return *this;  }

   //!smart_ptr -= std::ptrdiff_t. Never throws.
   smart_ptr &operator-= (std::ptrdiff_t offset)
   {  m_ptr -= offset;  return *this;  }

   //!++smart_ptr. Never throws.
   smart_ptr& operator++ (void) 
   {  ++m_ptr;   return *this;  }

   //!smart_ptr++. Never throws.
   smart_ptr operator++ (int)
   {  smart_ptr temp(*this); ++*this; return temp; }

   //!--smart_ptr. Never throws.
   smart_ptr& operator-- (void) 
   {  --m_ptr;   return *this;  }

   //!smart_ptr--. Never throws.
   smart_ptr operator-- (int)
   {  smart_ptr temp(*this); --*this; return temp; }

   //!safe bool conversion operator. Never throws.
   operator unspecified_bool_type() const  
   {  return this->get()? &self_t::unspecified_bool_type_func : 0;   }

   //!Not operator. Not needed in theory, but improves portability. 
   //!Never throws.
   bool operator! () const
   {  return this->get() == 0;   }
/*
   friend void swap (smart_ptr &pt, smart_ptr &pt2)
   {  
      value_type *ptr = pt.get();
      pt = pt2;
      pt2 = ptr;
   }
*/
};

//!smart_ptr<T1> == smart_ptr<T2>. Never throws.
template<class T1, class T2>
inline bool operator== (const smart_ptr<T1> &pt1, 
                        const smart_ptr<T2> &pt2)
{  return pt1.get() == pt2.get();  }

//!smart_ptr<T1> != smart_ptr<T2>. Never throws.
template<class T1, class T2>
inline bool operator!= (const smart_ptr<T1> &pt1, 
                        const smart_ptr<T2> &pt2)
{  return pt1.get() != pt2.get();  }

//!smart_ptr<T1> < smart_ptr<T2>. Never throws.
template<class T1, class T2>
inline bool operator< (const smart_ptr<T1> &pt1, 
                       const smart_ptr<T2> &pt2)
{  return pt1.get() < pt2.get();  }

//!smart_ptr<T1> <= smart_ptr<T2>. Never throws.
template<class T1, class T2>
inline bool operator<= (const smart_ptr<T1> &pt1, 
                        const smart_ptr<T2> &pt2)
{  return pt1.get() <= pt2.get();  }

//!smart_ptr<T1> > smart_ptr<T2>. Never throws.
template<class T1, class T2>
inline bool operator> (const smart_ptr<T1> &pt1, 
                       const smart_ptr<T2> &pt2)
{  return pt1.get() > pt2.get();  }

//!smart_ptr<T1> >= smart_ptr<T2>. Never throws.
template<class T1, class T2>
inline bool operator>= (const smart_ptr<T1> &pt1, 
                        const smart_ptr<T2> &pt2)
{  return pt1.get() >= pt2.get();  }

//!operator<< 
template<class E, class T, class Y> 
inline std::basic_ostream<E, T> & operator<< 
   (std::basic_ostream<E, T> & os, smart_ptr<Y> const & p)
{  return os << p.get();   }

//!operator>> 
template<class E, class T, class Y> 
inline std::basic_istream<E, T> & operator>> 
   (std::basic_istream<E, T> & os, smart_ptr<Y> & p)
{  Y * tmp; return os >> tmp; p = tmp;   }

//!std::ptrdiff_t + smart_ptr  
template<class T>
inline smart_ptr<T> operator+(std::ptrdiff_t diff, const smart_ptr<T>& right)
{  return right + diff;  }

//!smart_ptr - smart_ptr  
template<class T, class T2>
inline std::ptrdiff_t operator- (const smart_ptr<T> &pt, const smart_ptr<T2> &pt2)
{  return pt.get()- pt2.get();   }

//!swap specialization 
template<class T>
inline void swap (smart_ptr<T> &pt, 
                  smart_ptr<T> &pt2)
{  
   typename smart_ptr<T>::value_type *ptr = pt.get();
   pt = pt2;
   pt2 = ptr;
}

//!detail::get_pointer() enables boost::mem_fn to recognize smart_ptr.
//!Never throws.
template<class T>
inline T* get_pointer(const smart_ptr<T>  & p)
{  return p.get();   }

//!Simulation of static_cast between pointers. Never throws.
template<class T, class U> 
inline smart_ptr<T> 
   static_pointer_cast(smart_ptr<U> const & r)
{  
   return smart_ptr<T>(r, detail::static_cast_tag());  
}

//!Simulation of const_cast between pointers. Never throws.
template<class T, class U> 
inline smart_ptr<T>const_pointer_cast(smart_ptr<U> const & r)
{  
   return smart_ptr<T>(r, detail::const_cast_tag());  
}

//!Simulation of dynamic_cast between pointers. Never throws.
template<class T, class U> 
inline smart_ptr<T> 
   dynamic_pointer_cast(smart_ptr<U> const & r)
{  
   return smart_ptr<T>
            (r, detail::dynamic_cast_tag());  
}

//!Simulation of reinterpret_cast between pointers. Never throws.
template<class T, class U> 
inline smart_ptr<T>
   reinterpret_pointer_cast(smart_ptr<U> const & r)
{  
   return smart_ptr<T>(r, detail::reinterpret_cast_tag());  
}

}  //namespace intrusive {
}  //namespace boost {

namespace boost{

//This is to support embedding a bit in the pointer
//for intrusive containers, saving space
namespace intrusive {

template<std::size_t Alignment>
struct max_pointer_plus_bits<smart_ptr<void>, Alignment>
{
   static const std::size_t value = max_pointer_plus_bits<void*, Alignment>::value;
};

template<class T, std::size_t NumBits>
struct pointer_plus_bits<smart_ptr<T>, NumBits>
{
   typedef smart_ptr<T>         pointer;

   static pointer get_pointer(const pointer &n)
   {  return pointer_plus_bits<T*, NumBits>::get_pointer(n.get());  }

   static void set_pointer(pointer &n, pointer p)
   {
      T *raw_n = n.get();
      pointer_plus_bits<T*, NumBits>::set_pointer(raw_n, p.get());
      n = raw_n;
   }

   static std::size_t get_bits(const pointer &n)
   {  return pointer_plus_bits<T*, NumBits>::get_bits(n.get());  }

   static void set_bits(pointer &n, std::size_t c)
   {
      T *raw_n = n.get();
      pointer_plus_bits<T*, NumBits>::set_bits(raw_n, c);
      n = raw_n;
   }
};

}  //namespace intrusive
}  //namespace boost{

#endif //#ifndef BOOST_INTRUSIVE_SMART_PTR_HPP
