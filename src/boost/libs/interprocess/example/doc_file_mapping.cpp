//////////////////////////////////////////////////////////////////////////////
//
// (C) Copyright Ion Gaztanaga 2006-2007. Distributed under the Boost
// Software License, Version 1.0. (See accompanying file
// LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
// See http://www.boost.org/libs/interprocess for documentation.
//
//////////////////////////////////////////////////////////////////////////////
#include <boost/interprocess/detail/config_begin.hpp>
//[doc_file_mapping
#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/mapped_region.hpp>
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <cstring>
#include <cstddef>
#include <cstdlib>
//<-
#include "../test/get_process_id_name.hpp"
//->

int main(int argc, char *argv[])
{
   using namespace boost::interprocess;
   const std::size_t FileSize = 10000;
   if(argc == 1){ //Parent process executes this
      {  //Create a file
         std::filebuf fbuf;
      //<-
      #if 1
         fbuf.open(test::get_process_id_name(), std::ios_base::in | std::ios_base::out 
                              | std::ios_base::trunc | std::ios_base::binary); 
      #else
      //->
         fbuf.open("file.bin", std::ios_base::in | std::ios_base::out 
                              | std::ios_base::trunc | std::ios_base::binary); 
      //<-
      #endif
      //->
         //Set the size
         fbuf.pubseekoff(FileSize-1, std::ios_base::beg);
         fbuf.sputc(0);
      }
      //Remove file on exit
      struct file_remove 
      {
      //<-
      #if 1
         ~file_remove (){  file_mapping::remove(test::get_process_id_name()); }
      #else
      //->
         ~file_remove (){  file_mapping::remove("file.bin"); }
      //<-
      #endif
      //->
      } destroy_on_exit;

      //Create a file mapping
      //<-
      #if 1
      file_mapping m_file(test::get_process_id_name(), read_write);
      #else
      //->
      file_mapping m_file("file.bin", read_write);
      //<-
      #endif
      //->

      //Map the whole file with read-write permissions in this process
      mapped_region region(m_file, read_write);

      //Get the address of the mapped region
      void * addr       = region.get_address();
      std::size_t size  = region.get_size();

      //Write all the memory to 1
      std::memset(addr, 1, size);

      //Launch child process
      std::string s(argv[0]); s += " child ";
      //<-
      s += test::get_process_id_name();
      //->
      if(0 != std::system(s.c_str()))
         return 1;
   }
   else{  //Child process executes this
      {  //Open the file mapping and map it as read-only
         //<-
         #if 1
         file_mapping m_file(argv[2], read_only);
         #else
         //->
         file_mapping m_file("file.bin", read_only);
         //<-
         #endif
         //->

         mapped_region region(m_file, read_only);

         //Get the address of the mapped region
         void * addr       = region.get_address();
         std::size_t size  = region.get_size();

         //Check that memory was initialized to 1
         const char *mem = static_cast<char*>(addr);
         for(std::size_t i = 0; i < size; ++i)
            if(*mem++ != 1)
               return 1;   //Error checking memory
      }
      {  //Now test it reading the file
         std::filebuf fbuf;
         //<-
         #if 1
         fbuf.open(argv[2], std::ios_base::in | std::ios_base::binary); 
         #else
         //->
         fbuf.open("file.bin", std::ios_base::in | std::ios_base::binary); 
         //<-
         #endif
         //->

         //Read it to memory
         std::vector<char> vect(FileSize, 0);
         fbuf.sgetn(&vect[0], std::streamsize(vect.size()));

         //Check that memory was initialized to 1
         const char *mem = static_cast<char*>(&vect[0]);
         for(std::size_t i = 0; i < FileSize; ++i)
            if(*mem++ != 1)
               return 1;   //Error checking memory
      }
   }

   return 0;
}
//]
#include <boost/interprocess/detail/config_end.hpp>
