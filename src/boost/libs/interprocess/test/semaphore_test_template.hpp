//////////////////////////////////////////////////////////////////////////////
//
// (C) Copyright Ion Gaztanaga 2004-2007. Distributed under the Boost
// Software License, Version 1.0. (See accompanying file
// LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
// See http://www.boost.org/libs/interprocess for documentation.
//
//////////////////////////////////////////////////////////////////////////////

#ifndef BOOST_INTERPROCESS_TEST_SEMAPHORE_TEST_TEMPLATE_HEADER
#define BOOST_INTERPROCESS_TEST_SEMAPHORE_TEST_TEMPLATE_HEADER

#include <boost/interprocess/detail/config_begin.hpp>
#include <boost/interprocess/exceptions.hpp>
#include "boost_interprocess_check.hpp"
#include "util.hpp"
#include <boost/thread/thread.hpp>
#include <boost/thread/xtime.hpp>
#include <iostream>

namespace boost { namespace interprocess { namespace test {

template <typename P>
struct test_wait
{
   void operator()()
   {/*
      mutex_type interprocess_mutex;
      boost::interprocess::interprocess_condition interprocess_condition;

      // Test the lock's constructors.
      {
         wait_type lock(interprocess_mutex, boost::interprocess::defer_lock);
         BOOST_INTERPROCES_CHECK(!lock);
      }
      wait_type lock(interprocess_mutex);
      BOOST_INTERPROCES_CHECK(lock ? true : false);

      // Test the lock and unlock methods.
      lock.unlock();
      BOOST_INTERPROCES_CHECK(!lock);
      lock.lock();
      BOOST_INTERPROCES_CHECK(lock ? true : false);*/
   }
};

template <typename P>
struct test_try_wait
{
   void operator()()
   {/*
      mutex_type interprocess_mutex;
      boost::interprocess::interprocess_condition interprocess_condition;

      // Test the lock's constructors.
      {
         try_to_wait_type lock(interprocess_mutex, boost::interprocess::try_to_lock);
         BOOST_INTERPROCES_CHECK(lock ? true : false);
      }
      {
         try_to_wait_type lock(interprocess_mutex, boost::interprocess::defer_lock);
         BOOST_INTERPROCES_CHECK(!lock);
      }
      try_to_wait_type lock(interprocess_mutex);
      BOOST_INTERPROCES_CHECK(lock ? true : false);

      // Test the lock, unlock and trylock methods.
      lock.unlock();
      BOOST_INTERPROCES_CHECK(!lock);
      lock.lock();
      BOOST_INTERPROCES_CHECK(lock ? true : false);
      lock.unlock();
      BOOST_INTERPROCES_CHECK(!lock);
      BOOST_INTERPROCES_CHECK(lock.try_lock());
      BOOST_INTERPROCES_CHECK(lock ? true : false);*/
   }
};

template <typename P>
struct test_timed_wait
{
   void operator()()
   {/*
      mutex_type interprocess_mutex;
      boost::interprocess::interprocess_condition interprocess_condition;

      // Test the lock's constructors.
      {
         // Construct and initialize an xtime for a fast time out.
         boost::posix_time::ptime pt = delay(100, 0);

         timed_wait_type lock(interprocess_mutex, pt);
         BOOST_INTERPROCES_CHECK(lock ? true : false);
      }
      {
         timed_wait_type lock(interprocess_mutex, boost::interprocess::defer_lock);
         BOOST_INTERPROCES_CHECK(!lock);
      }
      timed_wait_type lock(interprocess_mutex);
      BOOST_INTERPROCES_CHECK(lock ? true : false);

      // Test the lock, unlock and timedlock methods.
      lock.unlock();
      BOOST_INTERPROCES_CHECK(!lock);
      lock.lock();
      BOOST_INTERPROCES_CHECK(lock ? true : false);
      lock.unlock();
      BOOST_INTERPROCES_CHECK(!lock);
      boost::posix_time::ptime pt = delay(10, 0);
      BOOST_INTERPROCES_CHECK(lock.timed_lock(pt));
      BOOST_INTERPROCES_CHECK(lock ? true : false);*/
   }
};

template <typename P>
struct test_recursive_lock
{
   void operator()()
   {/*
      mutex_type mx;
      {
         wait_type lock1(mx);
         wait_type lock2(mx);
      }
      {
         wait_type lock1(mx, defer_lock);
         wait_type lock2(mx, defer_lock);
      }
      {
         wait_type lock1(mx, try_to_lock);
         wait_type lock2(mx, try_to_lock);
      }
      {
         //This should always lock
         boost::posix_time::ptime pt = delay(3);
         wait_type lock1(mx, pt);
         wait_type lock2(mx, pt);
      }*/
   }
};

// plain_exclusive exercises the "infinite" lock for each
//   read_write_mutex type.

template<typename P>
void wait_and_sleep(void *arg, P &sm)
{
   data<P> *pdata = static_cast<data<P>*>(arg);
   boost::interprocess::scoped_lock<P> l(sm);
   boost::thread::sleep(xsecs(3*BaseSeconds));
   ++shared_val;
   pdata->m_value = shared_val;
}

template<typename P>
void try_wait_and_sleep(void *arg, P &sm)
{
   data<P> *pdata = static_cast<data<P>*>(arg);
   boost::interprocess::scoped_lock<P> l(sm, boost::interprocess::defer_lock);
   if (l.try_lock()){
      boost::thread::sleep(xsecs(3*BaseSeconds));
      ++shared_val;
      pdata->m_value = shared_val;
   }
}

template<typename P>
void timed_wait_and_sleep(void *arg, P &sm)
{
   data<P> *pdata = static_cast<data<P>*>(arg);
   boost::posix_time::ptime pt(delay(pdata->m_secs));
   boost::interprocess::scoped_lock<P> 
      l (sm, boost::interprocess::defer_lock);
   if (l.timed_lock(pt)){
      boost::thread::sleep(xsecs(3*BaseSeconds));
      ++shared_val;
      pdata->m_value = shared_val;
   }
}

template<typename P>
void test_mutex_lock(P &sm)
{
   shared_val = 0;

   data<P> m1(1,sm);
   data<P> m2(2,sm);

   // Locker one launches, holds the lock for 3*BaseSeconds seconds.
   boost::thread tm1(thread_adapter<P>(&wait_and_sleep, &m1, sm));

   //Wait 1*BaseSeconds
   boost::thread::sleep(xsecs(1*BaseSeconds));

   // Locker two launches, holds the lock for 3*BaseSeconds seconds.
   boost::thread tm2(thread_adapter<P>(&wait_and_sleep, &m2, sm));

   //Wait completion
   tm1.join();
   tm2.join();

   assert(m1.m_value == 1);
   assert(m2.m_value == 2);
}

template<typename P>
void test_mutex_try_lock(P &sm)
{
   shared_val = 0;

   data<P> m1(1,sm);
   data<P> m2(2,sm);

   // Locker one launches, holds the lock for 3*BaseSeconds seconds.
   boost::thread tm1(thread_adapter<P>(&try_wait_and_sleep, &m1, sm));

   //Wait 1*BaseSeconds
   boost::thread::sleep(xsecs(1*BaseSeconds));

   // Locker two launches, holds the lock for 3*BaseSeconds seconds.
   boost::thread tm2(thread_adapter<P>(&try_wait_and_sleep, &m2, sm));

   //Wait completion
   tm1.join();
   tm2.join();
   //Only the first should succeed locking
   assert(m1.m_value == 1);
   assert(m2.m_value == -1);
}

template<typename P>
void test_mutex_timed_lock(P &sm)

{
   {
      shared_val = 0;

      data<P> m1(1, sm, 3);
      data<P> m2(2, sm, 3);

      // Locker one launches, holds the lock for 3*BaseSeconds seconds.
      boost::thread tm1(thread_adapter<P>(&timed_wait_and_sleep, &m1, sm));

      //Wait 1*BaseSeconds
      boost::thread::sleep(xsecs(1*BaseSeconds));

      // Locker two launches, holds the lock for 3*BaseSeconds seconds.
      boost::thread tm2(thread_adapter<P>(&timed_wait_and_sleep, &m2, sm));

      //Wait completion
      tm1.join();
      tm2.join();

      //Both should succeed locking
      assert(m1.m_value == 1);
      assert(m2.m_value == 2);
   }
   {
      shared_val = 0;

      data<P> m1(1, sm, 3);
      data<P> m2(2, sm, 3);

      // Locker one launches, holds the lock for 3*BaseSeconds seconds.
      boost::thread tm1(thread_adapter<P>(&timed_wait_and_sleep, &m1, sm));

      //Wait 1*BaseSeconds
      boost::thread::sleep(xsecs(1*BaseSeconds));

      // Locker two launches, holds the lock for 3*BaseSeconds seconds.
      boost::thread tm2(thread_adapter<P>(&timed_wait_and_sleep, &m2, sm));

      //Wait completion
      tm1.join();
      tm2.join();

      //Both should succeed locking
      assert(m1.m_value == 1);
      assert(m2.m_value == 2);
   }
}

template <typename P>
inline void test_all_lock()
{
   //Now generic interprocess_mutex tests
   std::cout << "test_wait<" << typeid(P).name() << ">" << std::endl;
   test_wait<P>()();
   std::cout << "test_try_wait<" << typeid(P).name() << ">" << std::endl;
   test_try_wait<P>()();
   std::cout << "test_timed_wait<" << typeid(P).name() << ">" << std::endl;
   test_timed_wait<P>()();
} 

template <typename P>
inline void test_all_recursive_lock()
{
   //Now generic interprocess_mutex tests
   std::cout << "test_recursive_lock<" << typeid(P).name() << ">" << std::endl;
   test_recursive_lock<P>()();
}

template<typename P>
void test_all_mutex()
{
   P mut;
   std::cout << "test_mutex_lock<" << typeid(P).name() << ">" << std::endl;
   test_mutex_lock(mut);
   std::cout << "test_mutex_try_lock<" << typeid(P).name() << ">" << std::endl;
   test_mutex_try_lock(mut);
   std::cout << "test_mutex_timed_lock<" << typeid(P).name() << ">" << std::endl;
   test_mutex_timed_lock(mut);
}

}}}   //namespace boost { namespace interprocess { namespace test {

#include <boost/interprocess/detail/config_end.hpp>

#endif   //BOOST_INTERPROCESS_TEST_SEMAPHORE_TEST_TEMPLATE_HEADER
