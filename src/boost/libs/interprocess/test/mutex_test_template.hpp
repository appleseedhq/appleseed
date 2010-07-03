//////////////////////////////////////////////////////////////////////////////
//
// (C) Copyright Ion Gaztanaga 2004-2007. Distributed under the Boost
// Software License, Version 1.0. (See accompanying file
// LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
// See http://www.boost.org/libs/interprocess for documentation.
//
//////////////////////////////////////////////////////////////////////////////
// Copyright (C) 2001-2003
// William E. Kempf
//
// Permission to use, copy, modify, distribute and sell this software
// and its documentation for any purpose is hereby granted without fee,
// provided that the above copyright notice appear in all copies and
// that both that copyright notice and this permission notice appear
// in supporting documentation.  William E. Kempf makes no representations
// about the suitability of this software for any purpose.
// It is provided "as is" without express or implied warranty.

#ifndef BOOST_INTERPROCESS_TEST_MUTEX_TEST_TEMPLATE_HEADER
#define BOOST_INTERPROCESS_TEST_MUTEX_TEST_TEMPLATE_HEADER

#include <boost/interprocess/detail/config_begin.hpp>
#include <boost/interprocess/exceptions.hpp>
#include "boost_interprocess_check.hpp"
#include "util.hpp"
#include <boost/thread/thread.hpp>
#include <iostream>

namespace boost { namespace interprocess { namespace test {

template <typename M>
struct test_lock
{
   typedef M mutex_type;
   typedef boost::interprocess::scoped_lock<mutex_type> lock_type;


   void operator()()
   {
      mutex_type interprocess_mutex;

      // Test the lock's constructors.
      {
         lock_type lock(interprocess_mutex, boost::interprocess::defer_lock);
         BOOST_INTERPROCES_CHECK(!lock);
      }
      lock_type lock(interprocess_mutex);
      BOOST_INTERPROCES_CHECK(lock ? true : false);

      // Test the lock and unlock methods.
      lock.unlock();
      BOOST_INTERPROCES_CHECK(!lock);
      lock.lock();
      BOOST_INTERPROCES_CHECK(lock ? true : false);
   }
};

template <typename M>
struct test_trylock
{
   typedef M mutex_type;
   typedef boost::interprocess::scoped_lock<mutex_type> try_to_lock_type;

   void operator()()
   {
      mutex_type interprocess_mutex;

      // Test the lock's constructors.
      {
         try_to_lock_type lock(interprocess_mutex, boost::interprocess::try_to_lock);
         BOOST_INTERPROCES_CHECK(lock ? true : false);
      }
      {
         try_to_lock_type lock(interprocess_mutex, boost::interprocess::defer_lock);
         BOOST_INTERPROCES_CHECK(!lock);
      }
      try_to_lock_type lock(interprocess_mutex);
      BOOST_INTERPROCES_CHECK(lock ? true : false);

      // Test the lock, unlock and trylock methods.
      lock.unlock();
      BOOST_INTERPROCES_CHECK(!lock);
      lock.lock();
      BOOST_INTERPROCES_CHECK(lock ? true : false);
      lock.unlock();
      BOOST_INTERPROCES_CHECK(!lock);
      BOOST_INTERPROCES_CHECK(lock.try_lock());
      BOOST_INTERPROCES_CHECK(lock ? true : false);
   }
};

template <typename M>
struct test_timedlock
{
   typedef M mutex_type;
   typedef boost::interprocess::scoped_lock<mutex_type> timed_lock_type;

   void operator()()
   {
      mutex_type interprocess_mutex;

      // Test the lock's constructors.
      {
         // Construct and initialize an ptime for a fast time out.
         boost::posix_time::ptime pt = delay(1*BaseSeconds, 0);

         timed_lock_type lock(interprocess_mutex, pt);
         BOOST_INTERPROCES_CHECK(lock ? true : false);
      }
      {
         timed_lock_type lock(interprocess_mutex, boost::interprocess::defer_lock);
         BOOST_INTERPROCES_CHECK(!lock);
      }
      timed_lock_type lock(interprocess_mutex);
      BOOST_INTERPROCES_CHECK(lock ? true : false);

      // Test the lock, unlock and timedlock methods.
      lock.unlock();
      BOOST_INTERPROCES_CHECK(!lock);
      lock.lock();
      BOOST_INTERPROCES_CHECK(lock ? true : false);
      lock.unlock();
      BOOST_INTERPROCES_CHECK(!lock);
      boost::posix_time::ptime pt = delay(3*BaseSeconds, 0);
      BOOST_INTERPROCES_CHECK(lock.timed_lock(pt));
      BOOST_INTERPROCES_CHECK(lock ? true : false);
   }
};

template <typename M>
struct test_recursive_lock
{
   typedef M mutex_type;
   typedef boost::interprocess::scoped_lock<mutex_type> lock_type;

   void operator()()
   {
      mutex_type mx;
      {
         lock_type lock1(mx);
         lock_type lock2(mx);
      }
      {
         lock_type lock1(mx, defer_lock);
         lock_type lock2(mx, defer_lock);
      }
      {
         lock_type lock1(mx, try_to_lock);
         lock_type lock2(mx, try_to_lock);
      }
      {
         //This should always lock
         boost::posix_time::ptime pt = delay(2*BaseSeconds);
         lock_type lock1(mx, pt);
         lock_type lock2(mx, pt);
      }
   }
};

// plain_exclusive exercises the "infinite" lock for each
//   read_write_mutex type.

template<typename M>
void lock_and_sleep(void *arg, M &sm)
{
   data<M> *pdata = static_cast<data<M>*>(arg);
   boost::interprocess::scoped_lock<M> l(sm);
   if(pdata->m_secs){
      boost::thread::sleep(xsecs(pdata->m_secs));
   }
   else{
      boost::thread::sleep(xsecs(2*BaseSeconds));
   }

   ++shared_val;
   pdata->m_value = shared_val;
}

template<typename M>
void try_lock_and_sleep(void *arg, M &sm)
{
   data<M> *pdata = static_cast<data<M>*>(arg);
   boost::interprocess::scoped_lock<M> l(sm, boost::interprocess::defer_lock);
   if (l.try_lock()){
      boost::thread::sleep(xsecs(2*BaseSeconds));
      ++shared_val;
      pdata->m_value = shared_val;
   }
}

template<typename M>
void timed_lock_and_sleep(void *arg, M &sm)
{
   data<M> *pdata = static_cast<data<M>*>(arg);
   boost::posix_time::ptime pt(delay(pdata->m_secs));
   boost::interprocess::scoped_lock<M> 
      l (sm, boost::interprocess::defer_lock);
   if (l.timed_lock(pt)){
      boost::thread::sleep(xsecs(2*BaseSeconds));
      ++shared_val;
      pdata->m_value = shared_val;
   }
}

template<bool SameObject, typename M>
void test_mutex_lock()
{
   shared_val = 0;
   
   M m1, m2;
   M *pm1, *pm2;

   if(SameObject){
      pm1 = pm2 = &m1;
   }
   else{
      pm1 = &m1;
      pm2 = &m2;
   }

   data<M> d1(1);
   data<M> d2(2);

   // Locker one launches, holds the lock for 2*BaseSeconds seconds.
   boost::thread tm1(thread_adapter<M>(&lock_and_sleep, &d1, *pm1));

   //Wait 1*BaseSeconds
   boost::thread::sleep(xsecs(1*BaseSeconds));

   // Locker two launches, but it won't hold the lock for 2*BaseSeconds seconds.
   boost::thread tm2(thread_adapter<M>(&lock_and_sleep, &d2, *pm2));

   //Wait completion
   tm1.join();
   tm2.join();

   assert(d1.m_value == 1);
   assert(d2.m_value == 2);
}

template<bool SameObject, typename M>
void test_mutex_try_lock()
{
   shared_val = 0;

   M m1, m2;
   M *pm1, *pm2;

   if(SameObject){
      pm1 = pm2 = &m1;
   }
   else{
      pm1 = &m1;
      pm2 = &m2;
   }

   data<M> d1(1);
   data<M> d2(2);

   // Locker one launches, holds the lock for 2*BaseSeconds seconds.
   boost::thread tm1(thread_adapter<M>(&try_lock_and_sleep, &d1, *pm1));

   //Wait 1*BaseSeconds
   boost::thread::sleep(xsecs(1*BaseSeconds));

   // Locker two launches, but it should fail acquiring the lock
   boost::thread tm2(thread_adapter<M>(&try_lock_and_sleep, &d2, *pm2));

   //Wait completion
   tm1.join();
   tm2.join();
   //Only the first should succeed locking
   assert(d1.m_value == 1);
   assert(d2.m_value == -1);
}

template<bool SameObject, typename M>
void test_mutex_timed_lock()

{
   shared_val = 0;

   M m1, m2;
   M *pm1, *pm2;

   if(SameObject){
      pm1 = pm2 = &m1;
   }
   else{
      pm1 = &m1;
      pm2 = &m2;
   }

   data<M> d1(1, 2*BaseSeconds);
   data<M> d2(2, 2*BaseSeconds);

   // Locker one launches, holds the lock for 2*BaseSeconds seconds.
   boost::thread tm1(thread_adapter<M>(&timed_lock_and_sleep, &d1, *pm1));

   //Wait 1*BaseSeconds
   boost::thread::sleep(xsecs(1*BaseSeconds));

   // Locker two launches, holds the lock for 2*BaseSeconds seconds.
   boost::thread tm2(thread_adapter<M>(&timed_lock_and_sleep, &d2, *pm2));

   //Wait completion
   tm1.join();
   tm2.join();

   //Both should succeed locking
   assert(d1.m_value == 1);
   assert(d2.m_value == 2);
}

template <typename M>
inline void test_all_lock()
{
   //Now generic interprocess_mutex tests
   std::cout << "test_lock<" << typeid(M).name() << ">" << std::endl;
   test_lock<M>()();
   std::cout << "test_trylock<" << typeid(M).name() << ">" << std::endl;
   test_trylock<M>()();
   std::cout << "test_timedlock<" << typeid(M).name() << ">" << std::endl;
   test_timedlock<M>()();
} 

template <typename M>
inline void test_all_recursive_lock()
{
   //Now generic interprocess_mutex tests
   std::cout << "test_recursive_lock<" << typeid(M).name() << ">" << std::endl;
   test_recursive_lock<M>()();
}

template<bool SameObject, typename M>
void test_all_mutex()
{
   std::cout << "test_mutex_lock<" << typeid(M).name() << ">" << std::endl;
   test_mutex_lock<SameObject, M>();
   std::cout << "test_mutex_try_lock<" << typeid(M).name() << ">" << std::endl;
   test_mutex_try_lock<SameObject, M>();
   std::cout << "test_mutex_timed_lock<" << typeid(M).name() << ">" << std::endl;
   test_mutex_timed_lock<SameObject, M>();
}

}}}   //namespace boost { namespace interprocess { namespace test {

#include <boost/interprocess/detail/config_end.hpp>

#endif   //BOOST_INTERPROCESS_TEST_MUTEX_TEST_TEMPLATE_HEADER
