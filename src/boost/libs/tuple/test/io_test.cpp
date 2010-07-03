// Copyright (C) 1999, 2000 Jaakko Jarvi (jaakko.jarvi@cs.utu.fi)
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

// For more information, see http://www.boost.org

// -- io_test.cpp -----------------------------------------------
//
// Testing the I/O facilities of tuples

#define BOOST_INCLUDE_MAIN  // for testing, include rather than link
#include "boost/test/test_tools.hpp"    // see "Header Implementation Option"

#include "boost/tuple/tuple_io.hpp"
#include "boost/tuple/tuple_comparison.hpp"

#include <fstream>
#include <iterator>
#include <algorithm>
#include <string>

#if defined BOOST_NO_STRINGSTREAM
#include <strstream>
#else
#include <sstream>
#endif

using namespace std;
using namespace boost;

#if defined BOOST_NO_STRINGSTREAM
typedef ostrstream useThisOStringStream;
typedef istrstream useThisIStringStream;
#else
typedef ostringstream useThisOStringStream;
typedef istringstream useThisIStringStream;
#endif

int test_main(int argc, char * argv[] ) {
   (void)argc;
   (void)argv;
   using boost::tuples::set_close;
   using boost::tuples::set_open;
   using boost::tuples::set_delimiter;
   
  useThisOStringStream os1;

  // Set format [a, b, c] for os1
  os1 << set_open('[');
  os1 << set_close(']');
  os1 << set_delimiter(',');
  os1 << make_tuple(1, 2, 3);
  BOOST_CHECK (os1.str() == std::string("[1,2,3]") );

  {
  useThisOStringStream os2;
  // Set format (a:b:c) for os2; 
  os2 << set_open('(');
  os2 << set_close(')');
  os2 << set_delimiter(':');
#if !defined (BOOST_NO_TEMPLATE_PARTIAL_SPECIALIZATION)
  os2 << make_tuple("TUPU", "HUPU", "LUPU", 4.5);
  BOOST_CHECK (os2.str() == std::string("(TUPU:HUPU:LUPU:4.5)") );
#endif
  }

  // The format is still [a, b, c] for os1
  os1 << make_tuple(1, 2, 3);
  BOOST_CHECK (os1.str() == std::string("[1,2,3][1,2,3]") );

  ofstream tmp("temp.tmp");

#if !defined (BOOST_NO_TEMPLATE_PARTIAL_SPECIALIZATION)
  tmp << make_tuple("One", "Two", 3);
#endif   
  tmp << set_delimiter(':');
  tmp << make_tuple(1000, 2000, 3000) << endl;

  tmp.close();
  
  // When teading tuples from a stream, manipulators must be set correctly:
  ifstream tmp3("temp.tmp");
  tuple<string, string, int> j;

#if !defined (BOOST_NO_TEMPLATE_PARTIAL_SPECIALIZATION)
  tmp3 >> j; 
  BOOST_CHECK (tmp3.good() ); 
#endif
   
  tmp3 >> set_delimiter(':');
  tuple<int, int, int> i;
  tmp3 >> i; 
  BOOST_CHECK (tmp3.good() ); 
   
  tmp3.close(); 


  // reading tuple<int, int, int> in format (a b c); 
  useThisIStringStream is("(100 200 300)"); 
   
  tuple<int, int, int> ti; 
  BOOST_CHECK(bool(is >> ti));
  BOOST_CHECK(ti == make_tuple(100, 200, 300));
   

  // Note that strings are problematic:
  // writing a tuple on a stream and reading it back doesn't work in
  // general. If this is wanted, some kind of a parseable string class
  // should be used.
  
  return 0;
}

