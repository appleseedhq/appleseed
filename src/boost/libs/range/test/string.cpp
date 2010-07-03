// Boost.Range library
//
//  Copyright Thorsten Ottosen 2003-2004. Use, modification and
//  distribution is subject to the Boost Software License, Version
//  1.0. (See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// For more information, see http://www.boost.org/libs/range/
//

//#define _MSL_USING_NAMESPACE 1

#include <boost/detail/workaround.hpp>

#if BOOST_WORKAROUND(__BORLANDC__, BOOST_TESTED_AT(0x564))
#  pragma warn -8091 // supress warning in Boost.Test
#  pragma warn -8057 // unused argument argc/argv in Boost.Test
#endif

#include <boost/range/as_array.hpp>
#include <boost/range/as_literal.hpp>
#include <boost/range/functions.hpp>
#include <boost/range/metafunctions.hpp>
#include <boost/static_assert.hpp>
#include <boost/type_traits.hpp>
#include <boost/test/test_tools.hpp>
#include <boost/config.hpp>
#include <vector>
#include <fstream>
#include <algorithm>

template< class T >
inline BOOST_DEDUCED_TYPENAME boost::range_iterator<T>::type
str_begin( T& r )
{
    return boost::begin( boost::as_literal(r) ); 
}

template< class T >
inline BOOST_DEDUCED_TYPENAME boost::range_iterator<T>::type
str_end( T& r )
{
    return boost::end( boost::as_literal(r) ); 
}

template< class T >
inline BOOST_DEDUCED_TYPENAME boost::range_size<T>::type
str_size( const T& r )
{
    return boost::size( boost::as_literal(r) ); 
}

template< class T >
inline bool
str_empty( T& r )
{
    return boost::empty( boost::as_literal(r) ); 
}

template< typename Container, typename T >
BOOST_DEDUCED_TYPENAME boost::range_iterator<Container>::type
find( Container& c,  T value )
{
    return std::find( str_begin(c), str_end(c), 
                      value );
}

template< typename Container, typename T >
BOOST_DEDUCED_TYPENAME boost::range_iterator<const Container>::type
find( const Container& c, T value )
{
    return std::find( str_begin(c), str_end(c), 
                      value );
}

template< typename Container, typename T >
BOOST_DEDUCED_TYPENAME boost::range_iterator<Container>::type
find_mutable( Container& c,  T value )
{ 
    str_size( c );
    return std::find( str_begin(c), str_end(c), 
                      value );
}

template< typename Container, typename T >
BOOST_DEDUCED_TYPENAME boost::range_iterator<const Container>::type
find_const( const Container& c, T value )
{
    str_size( c );
    return std::find( str_begin(c), str_end(c), 
                      value );
}


std::vector<char> 
check_rvalue_return()
{
    return std::vector<char>( 10, 'm' ); 
}

using namespace boost;


void check_char()
{
    typedef char*                  char_iterator_t;
    typedef char                   char_array_t[10];
    const char*      char_s            = "a string";
    char             my_string[]       = "another string";
    const char       my_const_string[] = "another string";
    const unsigned   my_string_length  = 14;
    char*            char_s2           = "a string";
    
    BOOST_STATIC_ASSERT(( is_same<  range_value<char_iterator_t>::type, 
                                    detail::iterator_traits<char_iterator_t>::value_type>::value ));
    BOOST_STATIC_ASSERT(( is_same<  range_iterator<char_iterator_t>::type, char_iterator_t >::value ));

    BOOST_STATIC_ASSERT(( is_same<  range_difference<char_iterator_t>::type,                           
                                    ::std::ptrdiff_t >::value ));
    BOOST_STATIC_ASSERT(( is_same<  range_size<char_iterator_t>::type, std::size_t >::value ));
    BOOST_STATIC_ASSERT(( is_same<  range_iterator<char_iterator_t>::type, char_iterator_t >::value ));
    BOOST_STATIC_ASSERT(( is_same<  range_iterator<const char*>::type, const char* >::value ));
    
    BOOST_STATIC_ASSERT(( is_same< range_value<char_array_t>::type, 
                                    char>::value ));
    BOOST_STATIC_ASSERT(( is_same< range_iterator<char_array_t>::type, char* >::value ));
    BOOST_STATIC_ASSERT(( is_same< range_iterator<const char_array_t>::type, const char* >::value ));
    BOOST_STATIC_ASSERT(( is_same< range_difference<char_array_t>::type,                           
                                    ::std::ptrdiff_t >::value ));
    BOOST_STATIC_ASSERT(( is_same< range_size<char_array_t>::type, std::size_t >::value ));
    BOOST_STATIC_ASSERT(( is_same< range_iterator<char_array_t>::type, char* >::value ));
    BOOST_STATIC_ASSERT(( is_same< range_iterator<const char_array_t>::type, const char* >::value ));

    BOOST_CHECK_EQUAL( str_begin( char_s ), char_s );
    std::size_t sz = str_size(char_s);
    const char* str_end1 = str_begin( char_s ) + sz; 
    BOOST_CHECK_EQUAL( str_end( char_s ), str_end1 );
    BOOST_CHECK_EQUAL( str_empty( char_s ), (char_s == 0 || char_s[0] == char()) );
    BOOST_CHECK_EQUAL( sz, std::char_traits<char>::length( char_s ) );

    BOOST_CHECK_EQUAL( str_begin( my_string ), my_string );
    range_iterator<char_array_t>::type str_end2 = str_begin( my_string ) + str_size(my_string);
    range_iterator<char_array_t>::type str_end3 = str_end(my_string);
    BOOST_CHECK_EQUAL( str_end3, str_end2 );
    BOOST_CHECK_EQUAL( str_empty( my_string ), (my_string == 0 || my_string[0] == char()) );
    BOOST_CHECK_EQUAL( str_size( my_string ), my_string_length );
    BOOST_CHECK_EQUAL( str_size( my_string ), std::char_traits<char>::length( my_string ) );

    char to_search = 'n';
    BOOST_CHECK( find_mutable( char_s, to_search ) != str_end( char_s ) );
    BOOST_CHECK( find_const( char_s, to_search ) != str_end(char_s) );

    BOOST_CHECK( find_mutable( my_string, to_search ) != str_end(my_string) );
    BOOST_CHECK( find_const( my_string, to_search ) != str_end(my_string) );

    BOOST_CHECK( find_mutable( char_s2, to_search ) != str_end(char_s) );   
    BOOST_CHECK( find_const( char_s2, to_search ) != str_end(char_s2) );

    BOOST_CHECK( find_const( as_array( my_string ), to_search ) != str_end(my_string) );
    BOOST_CHECK( find_const( as_array( my_const_string ), to_search ) != str_end(my_string) );

    //
    // Test that as_literal() always scan for null terminator
    //
    char an_array[] = "foo\0bar";
    BOOST_CHECK_EQUAL( str_begin( an_array ), an_array );
    BOOST_CHECK_EQUAL( str_end( an_array ), an_array + 3 );
    BOOST_CHECK_EQUAL( str_size( an_array ), 3 );

    const char a_const_array[] = "foobar\0doh";
    BOOST_CHECK_EQUAL( str_begin( a_const_array ), a_const_array );
    BOOST_CHECK_EQUAL( str_end( a_const_array ), a_const_array + 6 );
    BOOST_CHECK_EQUAL( str_size( a_const_array ), 6 );

}



void check_string()
{
    check_char();
    
#ifndef BOOST_NO_STD_WSTRING
    typedef wchar_t*               wchar_iterator_t;          
    const wchar_t*  char_ws      = L"a wide string";
    wchar_t         my_wstring[] = L"another wide string";
    wchar_t*        char_ws2     = L"a wide string";
            
    BOOST_STATIC_ASSERT(( is_same< range_value<wchar_iterator_t>::type, 
                                   detail::iterator_traits<wchar_iterator_t>::value_type>::value ));
    BOOST_STATIC_ASSERT(( is_same< range_iterator<wchar_iterator_t>::type, wchar_iterator_t >::value ));
    BOOST_STATIC_ASSERT(( is_same< range_iterator<const wchar_t*>::type, const wchar_t* >::value ));
    BOOST_STATIC_ASSERT(( is_same< range_difference<wchar_iterator_t>::type,                           
                                   detail::iterator_traits<wchar_iterator_t>::difference_type >::value ));
    BOOST_STATIC_ASSERT(( is_same< range_size<wchar_iterator_t>::type, std::size_t >::value ));
    BOOST_STATIC_ASSERT(( is_same< range_iterator<wchar_iterator_t>::type, wchar_iterator_t >::value ));
    BOOST_STATIC_ASSERT(( is_same< range_iterator<const wchar_t*>::type, const wchar_t* >::value ));
    
    std::size_t sz = str_size( char_ws );
    BOOST_CHECK_EQUAL( str_begin( char_ws ), char_ws );
    BOOST_CHECK_EQUAL( str_end(char_ws), (str_begin( char_ws ) + sz) );
    BOOST_CHECK_EQUAL( str_empty( char_ws ), (char_ws == 0 || char_ws[0] == wchar_t()) );
    BOOST_CHECK_EQUAL( sz, std::char_traits<wchar_t>::length( char_ws ) );

    wchar_t to_search = L'n';
    BOOST_CHECK( find( char_ws, to_search ) != str_end(char_ws) );    

#if BOOST_WORKAROUND(_MSC_VER, BOOST_TESTED_AT(1300))

    BOOST_CHECK( find( my_wstring, to_search ) != str_end(my_wstring) );

#endif  
#endif
    
    find( check_rvalue_return(), 'n' );

}

#include <boost/test/unit_test.hpp>
using boost::unit_test::test_suite;


test_suite* init_unit_test_suite( int argc, char* argv[] )
{
    test_suite* test = BOOST_TEST_SUITE( "Range Test Suite" );

    test->add( BOOST_TEST_CASE( &check_string ) );

    return test;
}






