//  Copyright (c) 2007-2009 Hartmut Kaiser
//  Copyright (c) Christopher Diggins 2005 
//
//  Distributed under the Boost Software License, Version 1.0. (See accompanying 
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// #define BOOST_SPIRIT_ANY_IMPLICIT_CASTING

#include <cassert>
#include <stdexcept>
#include <typeinfo>
#include <iostream>
#include <vector>
#include <complex>
#include <string>

#include <boost/detail/lightweight_test.hpp>
#include <boost/spirit/home/support/detail/hold_any.hpp>

using namespace std; 
using namespace boost::spirit;

///////////////////////////////////////////////////////////////////////////////
bool output_any (hold_any const& a, std::string expected) 
{
    std::ostringstream o;
    if (a.type() == typeid(int)) 
    {
        o << any_cast<int>(a);
    }
    else if (a.type() == typeid(char)) 
    {
        o << any_cast<char>(a);
    }
    else if (a.type() == typeid(double)) 
    {
        o << any_cast<double>(a);
    }
    else if (a.type() == typeid(std::complex<int>)) 
    {
        o << any_cast<std::complex<int> >(a);
    }
    else 
    {
        o << "unexpected type: " << a.type().name();
        return false;
    }
    return o.str() == expected;
}

template <typename T>
bool output_any_direct (T const& v, std::string expected) 
{
    std::ostringstream o;
    o << v;
    return o.str() == expected;
}

void simple_any_test() 
{
    BOOST_TEST(output_any(42, "42"));
    BOOST_TEST(output_any('q', "q"));
    BOOST_TEST(output_any(3.14, "3.14"));
    BOOST_TEST(output_any(std::complex<int>(1, 2), "(1,2)"));

    int n = 42; BOOST_TEST(output_any(n, "42"));
    char c = 'q'; BOOST_TEST(output_any(c, "q"));
    double d = 3.14; BOOST_TEST(output_any(d, "3.14"));
    std::complex<int> x(1, 2); BOOST_TEST(output_any(x, "(1,2)"));
  
    hold_any a;
    BOOST_TEST(output_any(a = n, "42"));
    BOOST_TEST(output_any(a = c, "q"));
    BOOST_TEST(output_any(a = d, "3.14"));  
    BOOST_TEST(output_any(a = x, "(1,2)"));
    BOOST_TEST(output_any(a = 13, "13"));

#ifdef BOOST_SPIRIT_ANY_IMPLICIT_CASTING
    BOOST_TEST(output_any_direct(n = hold_any(42), "42"));
    BOOST_TEST(output_any_direct(c = hold_any('q'), "q"));
    BOOST_TEST(output_any_direct(d = hold_any(3.14), "3.14"));  
    BOOST_TEST(output_any_direct(x = std::complex<int>(hold_any(std::complex<int>(1, 2))), "(1,2)"));
#endif
  
    BOOST_TEST(output_any_direct(hold_any(42), "42"));
    BOOST_TEST(output_any_direct(hold_any('q'), "q"));
    BOOST_TEST(output_any_direct(hold_any(3.14), "3.14"));  
    BOOST_TEST(output_any_direct(hold_any(std::complex<int>(1, 2)), "(1,2)"));

    BOOST_TEST(!a.empty());
    a = 0;
    BOOST_TEST(!a.empty());
    a.reset();
    BOOST_TEST(a.empty());
  
    try {
        any_cast<int>(a);
        BOOST_TEST(false);
    }
    catch (boost::spirit::bad_any_cast const&) {
        BOOST_TEST(true);
    }
    catch (...) {
        BOOST_TEST(false);
    }
}

void test2(hold_any const& x, hold_any const& y)
{
    BOOST_TEST(x.type() != y.type());
    BOOST_TEST(x.type().name() != y.type().name());
}

///////////////////////////////////////////////////////////////////////////////
int state; 

///////////////////////////////////////////////////////////////////////////////
struct small_object 
{
    small_object() {}
    small_object(small_object const&) { state = 1; }
    ~small_object() { state = 2; }
};

inline std::istream& 
operator>> (std::istream& i, small_object&)
{
    return i;
}

inline std::ostream& 
operator<< (std::ostream& o, small_object const&)
{
    return o;
}

///////////////////////////////////////////////////////////////////////////////
struct large_object 
{
    large_object() {}
    large_object(large_object const&) { state = 3; }
    ~large_object() { state = 4; }
    
    int data0;
    int data1; 
    int data2; 
    int data3;
};

inline std::istream& 
operator>> (std::istream& i, large_object&)
{
    return i;
}

inline std::ostream& 
operator<< (std::ostream& o, large_object const&)
{
    return o;
}

void constructor_test() 
{
    small_object lfb;  
    large_object bfb;  
    hold_any a;
    state = 0;
    
    a = lfb;  BOOST_TEST(1 == state); state = 0;
    a = 42;   BOOST_TEST(2 == state); state = 0;
    a = bfb;  BOOST_TEST(3 == state); state = 0;
    a = 42;   BOOST_TEST(4 == state); state = 0;

    // test assignment of large objects
    a = bfb;
    a = bfb;
}

int main()
{  
    test2(std::string("10"), std::complex<double>(20));

    constructor_test();  
    simple_any_test();

    hold_any non_const(10);
    BOOST_TEST(any_cast<int>(non_const) == 10);
    *any_cast<int>(&non_const) = 15;
    BOOST_TEST(any_cast<int>(non_const) == 15);

    hold_any const const_(10);
    BOOST_TEST(any_cast<int>(const_) == *any_cast<int>(&const_));

    hold_any a = 42, b = 'q';
    swap(a, b);
    BOOST_TEST(any_cast<int>(b) == 42);
    BOOST_TEST(any_cast<char>(a) == 'q');
  
    return boost::report_errors();
}

