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
#include <boost/interprocess/allocators/allocator.hpp>
#include <boost/interprocess/containers/vector.hpp>
#include <boost/interprocess/detail/file_wrapper.hpp>
#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/detail/managed_open_or_create_impl.hpp>
#include "named_creation_template.hpp"
#include <cstdio>
#include <cstring>
#include <string>
#include "get_process_id_name.hpp"

using namespace boost::interprocess;

static const std::size_t FileSize = 1000;
static const char *      FileName = test::get_process_id_name();

struct file_destroyer
{
   ~file_destroyer()
   {
      //The last destructor will destroy the file
      file_mapping::remove(FileName);  
   }
};

//This wrapper is necessary to have a common constructor
//in generic named_creation_template functions
class mapped_file_creation_test_wrapper
   : public file_destroyer
   , public boost::interprocess::detail::managed_open_or_create_impl
      <boost::interprocess::detail::file_wrapper>
{
   typedef boost::interprocess::detail::managed_open_or_create_impl
      <boost::interprocess::detail::file_wrapper> mapped_file;
   public:
   mapped_file_creation_test_wrapper(boost::interprocess::create_only_t)
      :  mapped_file(boost::interprocess::create_only, FileName, FileSize)
   {}

   mapped_file_creation_test_wrapper(boost::interprocess::open_only_t)
      :  mapped_file(boost::interprocess::open_only, FileName)
   {}

   mapped_file_creation_test_wrapper(boost::interprocess::open_or_create_t)
      :  mapped_file(boost::interprocess::open_or_create, FileName, FileSize)
   {}
};

int main ()
{
   typedef boost::interprocess::detail::managed_open_or_create_impl
      <boost::interprocess::detail::file_wrapper> mapped_file;
   file_mapping::remove(FileName);
   test::test_named_creation<mapped_file_creation_test_wrapper>();

   //Create and get name, size and address
   {  
      mapped_file file1(create_only, FileName, FileSize);

      //Compare name
      if(std::strcmp(file1.get_name(), FileName) != 0){
         return 1;
      }

      //Overwrite all memory
      std::memset(file1.get_user_address(), 0, file1.get_user_size());

      //Now test move semantics
      mapped_file move_ctor(boost::interprocess::move(file1));
      mapped_file move_assign;
      move_assign = boost::interprocess::move(move_ctor);
   }
   file_mapping::remove(FileName);
   return 0;
}

#include <boost/interprocess/detail/config_end.hpp>
