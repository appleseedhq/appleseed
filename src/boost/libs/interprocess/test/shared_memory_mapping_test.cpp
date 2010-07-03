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
#include <fstream>
#include <iostream>
#include <boost/interprocess/shared_memory_object.hpp>
#include <boost/interprocess/mapped_region.hpp>
#include <boost/interprocess/anonymous_shared_memory.hpp>
#include <string>
#include "get_process_id_name.hpp"

using namespace boost::interprocess;

shared_memory_object get_shared_memory_mapping()
{
   shared_memory_object sh;
   return shared_memory_object(boost::interprocess::move(sh));
}

int main ()
{
   try{
      const std::size_t FileSize = 99999*2;
      {
         //Remove shared memory
         shared_memory_object::remove(test::get_process_id_name());

         //Create shared memory and file mapping
         shared_memory_object mapping(create_only, test::get_process_id_name(), read_write);
         mapping.truncate(FileSize);
      }

      {
         //Create a file mapping
         shared_memory_object mapping(open_only, test::get_process_id_name(), read_write);

         //Create two mapped regions, one half of the file each
         mapped_region region (mapping
                              ,read_write
                              ,0
                              ,FileSize/2
                              ,0);

         mapped_region region2(mapping
                              ,read_write
                              ,FileSize/2
                              ,FileSize - FileSize/2
                              ,0);

         //Fill two regions with a pattern   
         unsigned char *filler = static_cast<unsigned char*>(region.get_address());
         for(std::size_t i = 0
            ;i < FileSize/2
            ;++i){
            *filler++ = static_cast<unsigned char>(i);
         }

         filler = static_cast<unsigned char*>(region2.get_address());
         for(std::size_t i = FileSize/2
            ;i < FileSize
            ;++i){
            *filler++ = static_cast<unsigned char>(i);
         }
      }

      //See if the pattern is correct in the file using two mapped regions
      {
         //Create a file mapping
         shared_memory_object mapping(open_only, test::get_process_id_name(), read_write);
         mapped_region region(mapping, read_write, 0, FileSize/2, 0);
         mapped_region region2(mapping, read_write, FileSize/2, FileSize - FileSize/2, 0);

         unsigned char *checker = static_cast<unsigned char*>(region.get_address());
         //Check pattern
         for(std::size_t i = 0
            ;i < FileSize/2
            ;++i){
            if(*checker++ != static_cast<unsigned char>(i)){
               return 1;
            }
         }

         //Check second half
         checker = static_cast<unsigned char *>(region2.get_address());

         //Check pattern
         for(std::size_t i = FileSize/2
            ;i < FileSize
            ;++i){
            if(*checker++ != static_cast<unsigned char>(i)){
               return 1;
            }
         }
      }

      //Now check the pattern mapping a single read only mapped_region
      {
         //Create a file mapping
         shared_memory_object mapping(open_only, test::get_process_id_name(), read_only);

         //Create a single regions, mapping all the file
         mapped_region region (mapping
                              ,read_only);

         //Check pattern
         unsigned char *pattern = static_cast<unsigned char*>(region.get_address());
         for(std::size_t i = 0
            ;i < FileSize
            ;++i, ++pattern){
            if(*pattern != static_cast<unsigned char>(i)){
               return 1;
            }
         }
      }
      {
         //Now check anonymous mapping
         mapped_region region(anonymous_shared_memory(FileSize));

         //Write pattern
         unsigned char *pattern = static_cast<unsigned char*>(region.get_address());
         for(std::size_t i = 0
            ;i < FileSize
            ;++i, ++pattern){
            *pattern = static_cast<unsigned char>(i);
         }

         //Check pattern
         pattern = static_cast<unsigned char*>(region.get_address());
         for(std::size_t i = 0
            ;i < FileSize
            ;++i, ++pattern){
            if(*pattern != static_cast<unsigned char>(i)){
               return 1;
            }
         }
      }
      {
         //Now test move semantics
         shared_memory_object mapping(open_only, test::get_process_id_name(), read_write);
         shared_memory_object move_ctor(boost::interprocess::move(mapping));
         shared_memory_object move_assign;
         move_assign = boost::interprocess::move(move_ctor);
         shared_memory_object ret(get_shared_memory_mapping());
      }
   }
   catch(std::exception &exc){
      shared_memory_object::remove(test::get_process_id_name());
      std::cout << "Unhandled exception: " << exc.what() << std::endl;
      return 1;
   }
   shared_memory_object::remove(test::get_process_id_name());
   return 0;
}
