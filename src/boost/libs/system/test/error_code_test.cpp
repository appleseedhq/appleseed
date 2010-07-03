//  error_code_test.cpp  -----------------------------------------------------//

//  Copyright Beman Dawes 2006

//  Distributed under the Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

//  See library home page at http://www.boost.org/libs/system

//----------------------------------------------------------------------------// 

#include <boost/config/warning_disable.hpp>

#include <boost/test/minimal.hpp>
#include <boost/system/error_code.hpp>
#include <boost/system/cygwin_error.hpp>
#include <boost/system/linux_error.hpp>
#include <boost/system/windows_error.hpp>
#include <iostream>
#include <sstream>
#include <string>
#include <cstring>
#include <functional>
#include <boost/cerrno.hpp>

//  Although using directives are not the best programming practice, testing
//  with a boost::system using directive increases use scenario coverage.
using namespace boost::system;

# if defined( BOOST_WINDOWS_API )
#   include "winerror.h"
#   define BOOST_ACCESS_ERROR_MACRO ERROR_ACCESS_DENIED
# elif defined( BOOST_POSIX_API )
#   define BOOST_ACCESS_ERROR_MACRO EACCES
# else
#   error "Only supported for POSIX and Windows"
# endif

namespace
{
  void check_ostream( error_code ec, const char * expected )
  {
    std::stringstream ss;
    std::string s;

    ss << ec;
    ss >> s;
    BOOST_CHECK( s == expected );
  }
}

//  test_main  ---------------------------------------------------------------//

// TODO: add hash_value tests

int test_main( int, char ** )
{

  std::cout << "General tests...\n";
  // unit tests:

  BOOST_CHECK( posix_category == posix_category );
  BOOST_CHECK( system_category == system_category );
  BOOST_CHECK( posix_category != system_category );
  BOOST_CHECK( system_category != posix_category );

  if ( std::less<const error_category*>()( &posix_category, &system_category ) )
  {
    BOOST_CHECK( posix_category < system_category );
    BOOST_CHECK( !(system_category < posix_category) );
  }
  else
  {
    BOOST_CHECK( system_category < posix_category );
    BOOST_CHECK( !(posix_category < system_category) );
  }


  error_code ec;
  error_condition dec;
  BOOST_CHECK( !ec );
  BOOST_CHECK( ec.value() == 0 );
  dec = ec.default_error_condition();
  BOOST_CHECK( dec.value() == 0 );
  BOOST_CHECK( dec.category() == posix_category );
  BOOST_CHECK( ec == posix::success );
  BOOST_CHECK( ec.category() == system_category );
  BOOST_CHECK( std::strcmp( ec.category().name(), "system") == 0 );
  BOOST_CHECK( !(ec < error_code( 0, system_category )) );
  BOOST_CHECK( !(error_code( 0, system_category ) < ec) );
  BOOST_CHECK( ec < error_code( 1, system_category ) );
  BOOST_CHECK( !(error_code( 1, system_category ) < ec) );

  error_code ec_0_system( 0, system_category );
  BOOST_CHECK( !ec_0_system );
  BOOST_CHECK( ec_0_system.value() == 0 );
  dec = ec_0_system.default_error_condition();
  BOOST_CHECK( dec.value() == 0 );
  BOOST_CHECK( dec.category() == posix_category );
  BOOST_CHECK( ec_0_system == posix::success );
  BOOST_CHECK( ec_0_system.category() == system_category );
  BOOST_CHECK( std::strcmp( ec_0_system.category().name(), "system") == 0 );
  check_ostream( ec_0_system, "system:0" );

  BOOST_CHECK( ec_0_system == ec );

  error_code ec_1_system( 1, system_category );
  BOOST_CHECK( ec_1_system );
  BOOST_CHECK( ec_1_system.value() == 1 );
  BOOST_CHECK( ec_1_system.value() != 0 );
  BOOST_CHECK( ec != ec_1_system );
  BOOST_CHECK( ec_0_system != ec_1_system );
  check_ostream( ec_1_system, "system:1" );

  ec = error_code( BOOST_ACCESS_ERROR_MACRO, system_category );
  BOOST_CHECK( ec );
  BOOST_CHECK( ec.value() == BOOST_ACCESS_ERROR_MACRO );
  dec = ec.default_error_condition();
  BOOST_CHECK( dec.value() == static_cast<int>(posix::permission_denied) );
  BOOST_CHECK( dec.category() == posix_category );
  BOOST_CHECK( dec == error_condition( posix::permission_denied, posix_category ) );
  BOOST_CHECK( dec == posix::permission_denied );
  BOOST_CHECK( posix::permission_denied == dec );
  BOOST_CHECK( ec == posix::permission_denied );
  BOOST_CHECK( ec.category() == system_category );
  BOOST_CHECK( std::strcmp( ec.category().name(), "system") == 0 );

  // test the explicit make_error_code conversion for posix
  ec = make_error_code( posix::bad_message );
  BOOST_CHECK( ec );
  BOOST_CHECK( ec == posix::bad_message );
  BOOST_CHECK( posix::bad_message == ec );
  BOOST_CHECK( ec != posix::permission_denied );
  BOOST_CHECK( posix::permission_denied != ec );
  BOOST_CHECK( ec.category() == posix_category );

  // test the deprecated predefined error_category synonyms
  BOOST_CHECK( &system_category == &native_ecat );
  BOOST_CHECK( &posix_category == &errno_ecat );
  BOOST_CHECK( system_category == native_ecat );
  BOOST_CHECK( posix_category == errno_ecat );

  // test error_code and error_condition message();
  // see Boost.Filesystem operations_test for code specific message() tests
  ec = error_code( -1, system_category );
  std::cout << "error_code message for -1 is \"" << ec.message() << "\"\n";
#if defined(BOOST_WINDOWS_API)
  // Borland appends newline, so just check text
  BOOST_CHECK( ec.message().substr(0,13) == "Unknown error" );
#elif  defined(linux) || defined(__linux) || defined(__linux__)
  // Linux appends value to message as unsigned, so it varies with # of bits
  BOOST_CHECK( ec.message().substr(0,13) == "Unknown error" );
#elif defined(__hpux)
  BOOST_CHECK( ec.message() == "" );
#elif defined(__osf__)
  BOOST_CHECK( ec.message() == "Error -1 occurred." );
#elif defined(__vms)
  BOOST_CHECK( ec.message() == "error -1" );
#endif

  ec = error_code( BOOST_ACCESS_ERROR_MACRO, system_category );
  BOOST_CHECK( ec.message() != "" );
  BOOST_CHECK( ec.message().substr( 0, 13) != "Unknown error" );

  dec = error_condition( -1, posix_category );
  std::cout << "error_condition message for -1 is \"" << dec.message() << "\"\n";
#if defined(BOOST_WINDOWS_API)
  // Borland appends newline, so just check text
  BOOST_CHECK( dec.message().substr(0,13) == "Unknown error" );
#elif  defined(linux) || defined(__linux) || defined(__linux__)
  // Linux appends value to message as unsigned, so it varies with # of bits
  BOOST_CHECK( dec.message().substr(0,13) == "Unknown error" );
#elif defined(__hpux)
  BOOST_CHECK( dec.message() == "" );
#elif defined(__osf__)
  BOOST_CHECK( dec.message() == "Error -1 occurred." );
#elif defined(__vms)
  BOOST_CHECK( dec.message() == "error -1" );
#endif

  dec = error_condition( BOOST_ACCESS_ERROR_MACRO, posix_category );
  BOOST_CHECK( dec.message() != "" );
  BOOST_CHECK( dec.message().substr( 0, 13) != "Unknown error" );

#ifdef BOOST_WINDOWS_API
  std::cout << "Windows tests...\n";
  // these tests probe the Windows posix decoder
  //   test the first entry in the decoder table:
  ec = error_code( ERROR_ACCESS_DENIED, system_category );
  BOOST_CHECK( ec.value() == ERROR_ACCESS_DENIED );
  BOOST_CHECK( ec == posix::permission_denied );
  BOOST_CHECK( ec.default_error_condition().value() == posix::permission_denied );
  BOOST_CHECK( ec.default_error_condition().category() == posix_category );

  //   test the second entry in the decoder table:
  ec = error_code( ERROR_ALREADY_EXISTS, system_category );
  BOOST_CHECK( ec.value() == ERROR_ALREADY_EXISTS );
  BOOST_CHECK( ec == posix::file_exists );
  BOOST_CHECK( ec.default_error_condition().value() == posix::file_exists );
  BOOST_CHECK( ec.default_error_condition().category() == posix_category );

  //   test the third entry in the decoder table:
  ec = error_code( ERROR_BAD_UNIT, system_category );
  BOOST_CHECK( ec.value() == ERROR_BAD_UNIT );
  BOOST_CHECK( ec == posix::no_such_device );
  BOOST_CHECK( ec.default_error_condition().value() == posix::no_such_device );
  BOOST_CHECK( ec.default_error_condition().category() == posix_category );

  //   test the last non-Winsock entry in the decoder table:
  ec = error_code( ERROR_WRITE_PROTECT, system_category );
  BOOST_CHECK( ec.value() == ERROR_WRITE_PROTECT );
  BOOST_CHECK( ec == posix::permission_denied );
  BOOST_CHECK( ec.default_error_condition().value() == posix::permission_denied );
  BOOST_CHECK( ec.default_error_condition().category() == posix_category );

  //   test the last Winsock entry in the decoder table:
  ec = error_code( WSAEWOULDBLOCK, system_category );
  BOOST_CHECK( ec.value() == WSAEWOULDBLOCK );
  BOOST_CHECK( ec == posix::operation_would_block );
  BOOST_CHECK( ec.default_error_condition().value() == posix::operation_would_block );
  BOOST_CHECK( ec.default_error_condition().category() == posix_category );

  //   test not-in-table condition:
  ec = error_code( 1234567890, system_category );
  BOOST_CHECK( ec.value() == 1234567890 );
  BOOST_CHECK( ec.default_error_condition().value() == 1234567890 );
  BOOST_CHECK( ec.default_error_condition().category() == system_category );

#else // POSIX

  std::cout << "POSIX tests...\n";
  ec = error_code( EACCES, system_category );
  BOOST_CHECK( ec == error_code( posix::permission_denied, system_category ) );
  BOOST_CHECK( error_code( posix::permission_denied, system_category ) == ec );
  BOOST_CHECK( ec == posix::permission_denied );
  BOOST_CHECK( posix::permission_denied == ec );
  BOOST_CHECK( ec.default_error_condition().value() == posix::permission_denied );
  BOOST_CHECK( ec.default_error_condition().category() == posix_category );

# ifdef __CYGWIN__

  std::cout << "Cygwin tests...\n";
  ec = cygwin_error::no_package;
  BOOST_CHECK( ec == cygwin_error::no_package );
  BOOST_CHECK( ec == error_code( ENOPKG, system_category ) );
  BOOST_CHECK( ec == error_code( cygwin_error::no_package, system_category ) );
  BOOST_CHECK( ec.default_error_condition().category() == system_category );

# elif defined(linux) || defined(__linux) || defined(__linux__)

  std::cout << "Linux tests...\n";
  ec = linux_error::dot_dot_error;
  BOOST_CHECK( ec == linux_error::dot_dot_error );
  BOOST_CHECK( ec == error_code( EDOTDOT, system_category ) );
  BOOST_CHECK( ec == error_code( linux_error::dot_dot_error, system_category ) );
  BOOST_CHECK( ec.default_error_condition().category() == system_category );

# endif

#endif
  
  return 0;
}


