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
#include <boost/interprocess/sync/named_condition.hpp>
#include <boost/interprocess/sync/named_mutex.hpp>
#include <boost/interprocess/sync/scoped_lock.hpp>
#include <boost/date_time/posix_time/posix_time_types.hpp>
#include <boost/lexical_cast.hpp>
#include "condition_test_template.hpp"
#include "named_creation_template.hpp"
#include <string>
#include "get_process_id_name.hpp"

using namespace boost::interprocess;

struct condition_deleter
{
   std::string name;

   ~condition_deleter()
   {  
      if(name.empty())
         named_condition::remove(test::add_to_process_id_name("named_condition"));
      else
         named_condition::remove(name.c_str()); 
   }
};

//This wrapper is necessary to have a default constructor
//in generic mutex_test_template functions
class named_condition_test_wrapper
   : public condition_deleter, public named_condition
{
   public:

   named_condition_test_wrapper()
      :  named_condition(open_or_create, 
             (test::add_to_process_id_name("test_cond") + boost::lexical_cast<std::string>(count)).c_str())
   {
      condition_deleter::name += test::add_to_process_id_name("test_cond");
      condition_deleter::name += boost::lexical_cast<std::string>(count);
      ++count;
   }

   ~named_condition_test_wrapper()
   {  --count; }

   static int count;
};

int named_condition_test_wrapper::count = 0;

//This wrapper is necessary to have a common constructor
//in generic named_creation_template functions
class named_condition_creation_test_wrapper
   : public condition_deleter, public named_condition
{
   public:
   named_condition_creation_test_wrapper(create_only_t)
      :  named_condition(create_only, test::add_to_process_id_name("named_condition"))
   {  ++count_;   }

   named_condition_creation_test_wrapper(open_only_t)
      :  named_condition(open_only, test::add_to_process_id_name("named_condition"))
   {  ++count_;   }

   named_condition_creation_test_wrapper(open_or_create_t)
      :  named_condition(open_or_create, test::add_to_process_id_name("named_condition"))
   {  ++count_;   }

   ~named_condition_creation_test_wrapper()   {
      if(--count_){
         detail::interprocess_tester::
            dont_close_on_destruction(static_cast<named_condition&>(*this));
      }
   }
   static int count_;
};

int named_condition_creation_test_wrapper::count_ = 0;

struct mutex_deleter
{
   std::string name;

   ~mutex_deleter()
   {  
      if(name.empty())
         named_mutex::remove(test::add_to_process_id_name("named_mutex"));
      else
         named_mutex::remove(name.c_str()); 
   }
};

//This wrapper is necessary to have a default constructor
//in generic mutex_test_template functions
class named_mutex_test_wrapper
   : public mutex_deleter, public named_mutex
{
   public:
   named_mutex_test_wrapper()
      :  named_mutex(open_or_create, 
             (test::add_to_process_id_name("test_mutex") + boost::lexical_cast<std::string>(count)).c_str())
   {
      mutex_deleter::name += test::add_to_process_id_name("test_mutex");
      mutex_deleter::name += boost::lexical_cast<std::string>(count);
      ++count;
   }

   ~named_mutex_test_wrapper()
   {  --count; }

   static int count;
};

int named_mutex_test_wrapper::count = 0;

int main ()
{
   try{
      //Remove previous mutexes and conditions
      named_mutex::remove(test::add_to_process_id_name("test_mutex0"));
      named_condition::remove(test::add_to_process_id_name("test_cond0"));
      named_condition::remove(test::add_to_process_id_name("test_cond1"));
      named_condition::remove(test::add_to_process_id_name("named_condition"));
      named_mutex::remove(test::add_to_process_id_name("named_mutex"));

      test::test_named_creation<named_condition_creation_test_wrapper>();
      test::do_test_condition<named_condition_test_wrapper
                             ,named_mutex_test_wrapper>();
   }
   catch(std::exception &ex){
      std::cout << ex.what() << std::endl;
      return 1;
   }
   named_mutex::remove(test::add_to_process_id_name("test_mutex0"));
   named_condition::remove(test::add_to_process_id_name("test_cond0"));
   named_condition::remove(test::add_to_process_id_name("test_cond1"));
   named_condition::remove(test::add_to_process_id_name("named_condition"));
   named_mutex::remove(test::add_to_process_id_name("named_mutex"));
   return 0;
}

#include <boost/interprocess/detail/config_end.hpp>
