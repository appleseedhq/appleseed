///////////////////////////////////////////////////////////////////////////
//
// (C) Copyright Ion Gaztanaga 2006. Distributed under the Boost
// Software License, Version 1.0. (See accompanying file
// LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
// See http://www.boost.org/libs/interprocess for documentation.
//
///////////////////////////////////////////////////////////////////////////

#ifndef BOOST_INTERPROCESS_TEST_MOVABLE_INT_HEADER
#define BOOST_INTERPROCESS_TEST_MOVABLE_INT_HEADER

#include <boost/interprocess/detail/config_begin.hpp>
#include <boost/interprocess/detail/workaround.hpp>
#include <boost/interprocess/detail/move.hpp>

namespace boost {
namespace interprocess {
namespace test {

class movable_int
{
   movable_int(movable_int&);
   movable_int &operator= (movable_int&);

   public:
   BOOST_INTERPROCESS_ENABLE_MOVE_EMULATION(movable_int)

   movable_int()
      :  m_int(0)
   {}

   explicit movable_int(int a)
      :  m_int(a)
   {}

   movable_int(BOOST_INTERPROCESS_RV_REF(movable_int) mmi)
      :  m_int(mmi.m_int)
   {  mmi.m_int = 0; }

   movable_int & operator= (BOOST_INTERPROCESS_RV_REF(movable_int) mmi)
   {  this->m_int = mmi.m_int;   mmi.m_int = 0;  return *this;  }

   movable_int & operator= (int i)
   {  this->m_int = i;  return *this;  }

   bool operator ==(const movable_int &mi) const
   {  return this->m_int == mi.m_int;   }

   bool operator !=(const movable_int &mi) const
   {  return this->m_int != mi.m_int;   }

   bool operator <(const movable_int &mi) const
   {  return this->m_int < mi.m_int;   }

   bool operator <=(const movable_int &mi) const
   {  return this->m_int <= mi.m_int;   }

   bool operator >=(const movable_int &mi) const
   {  return this->m_int >= mi.m_int;   }

   bool operator >(const movable_int &mi) const
   {  return this->m_int > mi.m_int;   }

   int get_int() const
   {  return m_int;  }

   private:
   int m_int;
};

template<class E, class T> 
std::basic_ostream<E, T> & operator<< 
   (std::basic_ostream<E, T> & os, movable_int const & p)

{
    os << p.get_int();
    return os;
}

class movable_and_copyable_int
{
   public:
   BOOST_INTERPROCESS_ENABLE_MOVE_EMULATION(movable_and_copyable_int)

   movable_and_copyable_int()
      :  m_int(0)
   {}

   explicit movable_and_copyable_int(int a)
      :  m_int(a)
   {}

   movable_and_copyable_int(const movable_and_copyable_int& mmi)
      :  m_int(mmi.m_int)
   {}
   
   movable_and_copyable_int &operator= (const movable_and_copyable_int& mi)
   {  this->m_int = mi.m_int;    return *this;  }

   movable_and_copyable_int(BOOST_INTERPROCESS_RV_REF(movable_and_copyable_int) mmi)
      :  m_int(mmi.m_int)
   {  mmi.m_int = 0; }

   movable_and_copyable_int & operator= (BOOST_INTERPROCESS_RV_REF(movable_and_copyable_int) mmi)
   {  this->m_int = mmi.m_int;   mmi.m_int = 0;    return *this;  }

   movable_and_copyable_int & operator= (int i)
   {  this->m_int = i;  return *this;  }

   bool operator ==(const movable_and_copyable_int &mi) const
   {  return this->m_int == mi.m_int;   }

   bool operator !=(const movable_and_copyable_int &mi) const
   {  return this->m_int != mi.m_int;   }

   bool operator <(const movable_and_copyable_int &mi) const
   {  return this->m_int < mi.m_int;   }

   bool operator <=(const movable_and_copyable_int &mi) const
   {  return this->m_int <= mi.m_int;   }

   bool operator >=(const movable_and_copyable_int &mi) const
   {  return this->m_int >= mi.m_int;   }

   bool operator >(const movable_and_copyable_int &mi) const
   {  return this->m_int > mi.m_int;   }

   int get_int() const
   {  return m_int;  }

   private:
   int m_int;
};

template<class E, class T> 
std::basic_ostream<E, T> & operator<< 
   (std::basic_ostream<E, T> & os, movable_and_copyable_int const & p)

{
    os << p.get_int();
    return os;
}

}  //namespace test {
}  //namespace interprocess {
}  //namespace boost {

#include <boost/interprocess/detail/config_end.hpp>

#endif   //#ifndef BOOST_INTERPROCESS_TEST_MOVABLE_INT_HEADER
