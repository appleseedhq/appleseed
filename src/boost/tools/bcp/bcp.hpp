/*
 *
 * Copyright (c) 2003 Dr John Maddock
 * Use, modification and distribution is subject to the 
 * Boost Software License, Version 1.0. (See accompanying file 
 * LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 *
 */

#include <boost/shared_ptr.hpp>

class bcp_application;
typedef boost::shared_ptr<bcp_application> pbcp_application;

class bcp_application
{
public:
   virtual ~bcp_application();

   virtual void enable_list_mode() = 0;
   virtual void enable_summary_list_mode() = 0;
   virtual void enable_cvs_mode() = 0;
   virtual void enable_svn_mode() = 0;
   virtual void enable_unix_lines() = 0;
   virtual void enable_scan_mode() = 0;
   virtual void enable_license_mode() = 0;
   virtual void enable_bsl_convert_mode() = 0;
   virtual void enable_bsl_summary_mode() = 0;
   virtual void set_boost_path(const char* p) = 0;
   virtual void set_destination(const char* p) = 0;
   virtual void add_module(const char* p) = 0;

   virtual int run() = 0;

   static pbcp_application create();
};


