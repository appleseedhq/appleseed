/*
 *
 * Copyright (c) 2003 Dr John Maddock
 * Use, modification and distribution is subject to the 
 * Boost Software License, Version 1.0. (See accompanying file 
 * LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 *
 * This file implements the following:
 *    void bcp_implementation::copy_path(const fs::path& p)
 *    void bcp_implementation::create_path(const fs::path& p)
 */

#include "bcp_imp.hpp"
#include <boost/filesystem/operations.hpp>
#include <fstream>
#include <iterator>
#include <algorithm>
#include <iostream>

void bcp_implementation::copy_path(const fs::path& p)
{
   assert(!fs::is_directory(m_boost_path / p));
   if(fs::exists(m_dest_path / p))
   {
      std::cout << "Copying (and overwriting) file: " << p.string() << "\n";
     fs::remove(m_dest_path / p);
   }
   else
      std::cout << "Copying file: " << p.string() << "\n";
   //
   // create the path to the new file if it doesn't already exist:
   //
   create_path(p.branch_path());
   //
   // do text based copy if requested:
   //
   if(m_unix_lines && !is_binary_file(p))
   {
      std::ifstream is((m_boost_path / p).native_file_string().c_str());
      std::istreambuf_iterator<char> isi(is);
      std::istreambuf_iterator<char> end;

      std::ofstream os((m_dest_path / p).native_file_string().c_str(), std::ios_base::binary | std::ios_base::out);
      std::ostreambuf_iterator<char> osi(os);

      std::copy(isi, end, osi);
   }
   else
   {
      // binary copy:
      fs::copy_file(m_boost_path / p, m_dest_path / p);
   }
}

void bcp_implementation::create_path(const fs::path& p)
{
   if(!fs::exists(m_dest_path / p))
   {
      // recurse then create the path:
      create_path(p.branch_path());
      fs::create_directory(m_dest_path / p);
   }
}


