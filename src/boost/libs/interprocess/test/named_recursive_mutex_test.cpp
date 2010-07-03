//////////////////////////////////////////////////////////////////////////////
//
// (C) Copyright Ion Gaztanaga 2004-2007. Distributed under the Boost
// Software License, Version 1.0. (See accompanying file
// LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
// See http://www.boost.org/libs/interprocess for documentation.
//
//////////////////////////////////////////////////////////////////////////////

#include <boost/interprocess/detail/config_begin.hpp>
#include <boost/interprocess/sync/named_recursive_mutex.hpp>
#include <boost/interprocess/sync/scoped_lock.hpp>
#include <boost/date_time/posix_time/posix_time_types.hpp>
#include "mutex_test_template.hpp"
#include "named_creation_template.hpp"
#include <string>
#include "get_process_id_name.hpp"

using namespace boost::interprocess;

struct mutex_deleter
{
   ~mutex_deleter()
   {  named_recursive_mutex::remove(test::get_process_id_name()); }
};

//This wrapper is necessary to have a default constructor
//in generic mutex_test_template functions
class named_recursive_mutex_lock_test_wrapper
   : public named_recursive_mutex
{
   public:
   named_recursive_mutex_lock_test_wrapper()
      :  named_recursive_mutex(open_or_create, test::get_process_id_name())
   {  ++count_;   }

   ~named_recursive_mutex_lock_test_wrapper()
   {
      if(--count_){
         detail::interprocess_tester::
            dont_close_on_destruction(static_cast<named_recursive_mutex&>(*this));
      }
   }

   static int count_;
};

int named_recursive_mutex_lock_test_wrapper::count_ = 0;

//This wrapper is necessary to have a common constructor
//in generic named_creation_template functions
class named_mutex_creation_test_wrapper
   : public mutex_deleter, public named_recursive_mutex
{
   public:
   named_mutex_creation_test_wrapper(create_only_t)
      :  named_recursive_mutex(create_only, test::get_process_id_name())
   {  ++count_;   }

   named_mutex_creation_test_wrapper(open_only_t)
      :  named_recursive_mutex(open_only, test::get_process_id_name())
   {  ++count_;   }

   named_mutex_creation_test_wrapper(open_or_create_t)
      :  named_recursive_mutex(open_or_create, test::get_process_id_name())
   {  ++count_;   }

   ~named_mutex_creation_test_wrapper()
   {
      if(--count_){
         detail::interprocess_tester::
            dont_close_on_destruction(static_cast<named_recursive_mutex&>(*this));
      }
   }

   static int count_;
};

int named_mutex_creation_test_wrapper::count_ = 0;

int main ()
{
   try{
      named_recursive_mutex::remove(test::get_process_id_name());
      test::test_named_creation<named_mutex_creation_test_wrapper>();
      test::test_all_lock<named_recursive_mutex_lock_test_wrapper>();
      test::test_all_mutex<false, named_recursive_mutex_lock_test_wrapper>();
      test::test_all_recursive_lock<named_recursive_mutex_lock_test_wrapper>();
   }
   catch(std::exception &ex){
      named_recursive_mutex::remove(test::get_process_id_name());
      std::cout << ex.what() << std::endl;
      return 1;
   }
   named_recursive_mutex::remove(test::get_process_id_name());
   return 0;
}

#include <boost/interprocess/detail/config_end.hpp>

