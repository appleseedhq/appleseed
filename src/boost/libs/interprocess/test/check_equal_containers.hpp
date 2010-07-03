//////////////////////////////////////////////////////////////////////////////
//
// (C) Copyright Ion Gaztanaga 2006. Distributed under the Boost
// Software License, Version 1.0. (See accompanying file
// LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
// See http://www.boost.org/libs/interprocess for documentation.
//
//////////////////////////////////////////////////////////////////////////////

#ifndef BOOST_INTERPROCESS_TEST_CHECK_EQUAL_CONTAINERS_HPP
#define BOOST_INTERPROCESS_TEST_CHECK_EQUAL_CONTAINERS_HPP

#include <boost/interprocess/detail/config_begin.hpp>
#include <functional>
#include <iostream>
#include <algorithm>

namespace boost{
namespace interprocess{
namespace test{

//Function to check if both containers are equal
template<class MyShmCont
        ,class MyStdCont>
bool CheckEqualContainers(MyShmCont *shmcont, MyStdCont *stdcont)
{
   if(shmcont->size() != stdcont->size())
      return false;

   typedef typename MyShmCont::value_type value_type;

   typename MyShmCont::iterator itshm(shmcont->begin()), itshmend(shmcont->end());
   typename MyStdCont::iterator itstd(stdcont->begin());
   typename MyStdCont::size_type dist = (typename MyStdCont::size_type)std::distance(itshm, itshmend);
   if(dist != shmcont->size()){ 
      return false;
   }
   std::size_t i = 0;
   for(; itshm != itshmend; ++itshm, ++itstd, ++i){
      value_type val(*itstd);
      const value_type &v = *itshm;
      if(v != val)
         return false;
   }
   return true;
}

template<class MyShmCont
        ,class MyStdCont>
bool CheckEqualPairContainers(MyShmCont *shmcont, MyStdCont *stdcont)
{
   if(shmcont->size() != stdcont->size())
      return false;

   typedef typename MyShmCont::key_type      key_type;
   typedef typename MyShmCont::mapped_type   mapped_type;

   typename MyShmCont::iterator itshm(shmcont->begin()), itshmend(shmcont->end());
   typename MyStdCont::iterator itstd(stdcont->begin());
   for(; itshm != itshmend; ++itshm, ++itstd){
      if(itshm->first != key_type(itstd->first))
         return false;

      if(itshm->second != mapped_type(itstd->second))
         return false;
   }
   return true;
}
}  //namespace test{
}  //namespace interprocess{
}  //namespace boost{

#include <boost/interprocess/detail/config_end.hpp>

#endif //#ifndef BOOST_INTERPROCESS_TEST_CHECK_EQUAL_CONTAINERS_HPP
