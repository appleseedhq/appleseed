//  (C) Copyright John Maddock 2003. 
//  Use, modification and distribution are subject to the 
//  Boost Software License, Version 1.0. (See accompanying file 
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org/libs/config for the most recent version.

#include "link_test.hpp"

int main()
{
   return check_options(dyn_link, dyn_rtl, has_threads, debug, stl_debug) ? 0 : -1;
}


