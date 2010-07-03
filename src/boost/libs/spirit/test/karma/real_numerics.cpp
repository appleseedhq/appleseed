//  Copyright (c) 2001-2009 Hartmut Kaiser
// 
//  Distributed under the Boost Software License, Version 1.0. (See accompanying 
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

//#define KARMA_FAIL_COMPILATION

#include <boost/version.hpp>
#include <boost/config/warning_disable.hpp>
#include <boost/detail/lightweight_test.hpp>
#include <boost/math/concepts/real_concept.hpp>

#include <boost/spirit/include/karma_char.hpp>
#include <boost/spirit/include/karma_numeric.hpp>
#include <boost/spirit/include/karma_generate.hpp>
#include <boost/spirit/include/karma_directive.hpp>

#include <limits>
#include "test.hpp"

using namespace spirit_test;

///////////////////////////////////////////////////////////////////////////////
//  policy for real_generator, which forces the scientific notation
template <typename T>
struct scientific_policy : boost::spirit::karma::real_generator_policies<T>
{
    //  we want the numbers always to be in scientific format
    typedef boost::spirit::karma::real_generator_policies<T> base_type;
    static int floatfield(T) { return base_type::scientific; }
};

///////////////////////////////////////////////////////////////////////////////
//  policy for real_generator, which forces the fixed notation
template <typename T>
struct fixed_policy : boost::spirit::karma::real_generator_policies<T>
{
    typedef boost::spirit::karma::real_generator_policies<T> base_type;

    //  we want the numbers always to be in scientific format
    static int floatfield(T) { return base_type::fixed; }
};

///////////////////////////////////////////////////////////////////////////////
//  policy for real_generator, which forces to output trailing zeros in the 
//  fractional part
template <typename T>
struct trailing_zeros_policy 
  : boost::spirit::karma::real_generator_policies<T>   // 4 digits
{
    //  we want the numbers always to contain trailing zeros up to 4 digits in 
    //  the fractional part
    static bool const trailing_zeros = true;
    
    //  we want to generate up to 4 fractional digits 
    static unsigned int precision(T) { return 4; }
};

///////////////////////////////////////////////////////////////////////////////
//  policy for real_generator, which forces the sign to be generated
template <typename T>
struct signed_policy 
  : boost::spirit::karma::real_generator_policies<T>
{
    // we want to always have a sign generated
    static bool const force_sign = true;
};

// support for using real_concept with a Karma generator has been implemented 
// in Boost versions > 1.36 only
#if BOOST_VERSION > 103600
///////////////////////////////////////////////////////////////////////////////
//  We need to specialize is_real_lit_tag to allow to use a real_concept as a
//  literal below
namespace boost { namespace spirit 
{
    template <typename Domain>
    struct is_real_lit_tag<boost::math::concepts::real_concept, Domain> 
      : boost::mpl::true_ {};
}}
#endif

///////////////////////////////////////////////////////////////////////////////
int
main()
{
    using namespace boost::spirit;
    
    {
        ///////////////////////////////////////////////////////////////////////
        // use the default real_policies
        BOOST_TEST(test("0.0", double_, 0.0));
        BOOST_TEST(test("1.0", double_, 1.0));
        BOOST_TEST(test("1.0", double_, 1.0001));
        BOOST_TEST(test("1.001", double_, 1.001));
        BOOST_TEST(test("1.01", double_, 1.010));
        BOOST_TEST(test("1.1", double_, 1.100));
        
        BOOST_TEST(test("1.234e-04", double_, 0.00012345));
        BOOST_TEST(test("0.001", double_, 0.0012345));
        BOOST_TEST(test("0.012", double_, 0.012345));
        BOOST_TEST(test("0.123", double_, 0.12345));
        BOOST_TEST(test("1.234", double_, 1.2345));
        BOOST_TEST(test("12.346", double_, 12.346));
        BOOST_TEST(test("123.46", double_, 123.46));
        BOOST_TEST(test("1234.5", double_, 1234.5));
        BOOST_TEST(test("12342.0", double_, 12342.));
        BOOST_TEST(test("1.234e05", double_, 123420.));
        
        BOOST_TEST(test("-1.0", double_, -1.0));
        BOOST_TEST(test("-1.234", double_, -1.2345));
        BOOST_TEST(test("-1.235", double_, -1.2346));
        BOOST_TEST(test("-1234.2", double_, -1234.2));
        
        BOOST_TEST(test("1.0", double_(1.0)));
        BOOST_TEST(test("1.0", double_(1.0001)));
        BOOST_TEST(test("1.001", double_(1.001)));
        BOOST_TEST(test("1.01", double_(1.010)));
        BOOST_TEST(test("1.1", double_(1.100)));
        
        BOOST_TEST(test("1.234e-04", double_(0.00012345)));
        BOOST_TEST(test("0.001", double_(0.0012345)));
        BOOST_TEST(test("0.012", double_(0.012345)));
        BOOST_TEST(test("0.123", double_(0.12345)));
        BOOST_TEST(test("1.234", double_(1.2345)));
        BOOST_TEST(test("12.346", double_(12.346)));
        BOOST_TEST(test("123.46", double_(123.46)));
        BOOST_TEST(test("1234.5", double_(1234.5)));
        BOOST_TEST(test("12342.0", double_(12342.)));
        BOOST_TEST(test("1.234e05", double_(123420.)));
    }
    
    {
        ///////////////////////////////////////////////////////////////////////
        // test NaN and Inf
        BOOST_TEST(test("nan", double_, std::numeric_limits<double>::quiet_NaN()));
        BOOST_TEST(test("-nan", double_, -std::numeric_limits<double>::quiet_NaN()));
        BOOST_TEST(test("inf", double_, std::numeric_limits<double>::infinity()));
        BOOST_TEST(test("-inf", double_, -std::numeric_limits<double>::infinity()));

        typedef karma::real_spec<double, signed_policy<double> > signed_type;
        signed_type const signed_ = signed_type();

        BOOST_TEST(test("+nan", signed_, std::numeric_limits<double>::quiet_NaN()));
        BOOST_TEST(test("-nan", signed_, -std::numeric_limits<double>::quiet_NaN()));
        BOOST_TEST(test("+inf", signed_, std::numeric_limits<double>::infinity()));
        BOOST_TEST(test("-inf", signed_, -std::numeric_limits<double>::infinity()));
        BOOST_TEST(test(" 0.0", signed_, 0.0));
        
        BOOST_TEST(test("+nan", signed_(std::numeric_limits<double>::quiet_NaN())));
        BOOST_TEST(test("-nan", signed_(-std::numeric_limits<double>::quiet_NaN())));
        BOOST_TEST(test("+inf", signed_(std::numeric_limits<double>::infinity())));
        BOOST_TEST(test("-inf", signed_(-std::numeric_limits<double>::infinity())));
        BOOST_TEST(test(" 0.0", signed_(0.0)));
    }
    
    {
        ///////////////////////////////////////////////////////////////////////
        typedef karma::real_spec<double, trailing_zeros_policy<double> > 
            trailing_zeros_type;
        trailing_zeros_type const trail_zeros = trailing_zeros_type();

        BOOST_TEST(test("0.0000", trail_zeros, 0.0));
        BOOST_TEST(test("1.0000", trail_zeros, 1.0));
        BOOST_TEST(test("1.0001", trail_zeros, 1.0001));
        BOOST_TEST(test("1.0010", trail_zeros, 1.001));
        BOOST_TEST(test("1.0100", trail_zeros, 1.010));
        BOOST_TEST(test("1.1000", trail_zeros, 1.100));
        
        BOOST_TEST(test("1.2345e-04", trail_zeros, 0.00012345));
        BOOST_TEST(test("0.0012", trail_zeros, 0.0012345));
        BOOST_TEST(test("0.0123", trail_zeros, 0.012345));
        BOOST_TEST(test("0.1235", trail_zeros, 0.12345));
        BOOST_TEST(test("1.2345", trail_zeros, 1.2345));
        BOOST_TEST(test("12.3460", trail_zeros, 12.346));
        BOOST_TEST(test("123.4600", trail_zeros, 123.46));
        BOOST_TEST(test("1234.5000", trail_zeros, 1234.5));
        BOOST_TEST(test("12342.0000", trail_zeros, 12342.));
        BOOST_TEST(test("1.2342e05", trail_zeros, 123420.));
        
        BOOST_TEST(test("-1.0000", trail_zeros, -1.0));
        BOOST_TEST(test("-1.2345", trail_zeros, -1.2345));
        BOOST_TEST(test("-1.2346", trail_zeros, -1.2346));
        BOOST_TEST(test("-1234.2000", trail_zeros, -1234.2));
        
        BOOST_TEST(test("1.0000", trail_zeros(1.0)));
        BOOST_TEST(test("1.0001", trail_zeros(1.0001)));
        BOOST_TEST(test("1.0010", trail_zeros(1.001)));
        BOOST_TEST(test("1.0100", trail_zeros(1.010)));
        BOOST_TEST(test("1.1000", trail_zeros(1.100)));
        
        BOOST_TEST(test("1.2345e-04", trail_zeros(0.00012345)));
        BOOST_TEST(test("0.0012", trail_zeros(0.0012345)));
        BOOST_TEST(test("0.0123", trail_zeros(0.012345)));
        BOOST_TEST(test("0.1235", trail_zeros(0.12345)));
        BOOST_TEST(test("1.2345", trail_zeros(1.2345)));
        BOOST_TEST(test("12.3460", trail_zeros(12.346)));
        BOOST_TEST(test("123.4600", trail_zeros(123.46)));
        BOOST_TEST(test("1234.5000", trail_zeros(1234.5)));
        BOOST_TEST(test("12342.0000", trail_zeros(12342.)));
        BOOST_TEST(test("1.2342e05", trail_zeros(123420.)));
    }
    
    {
        ///////////////////////////////////////////////////////////////////////
        BOOST_TEST(test_delimited("0.0 ", double_, 0.0, char_(' ')));
        BOOST_TEST(test_delimited("1.0 ", double_, 1.0, char_(' ')));
        BOOST_TEST(test_delimited("1.0 ", double_, 1.0001, char_(' ')));
        BOOST_TEST(test_delimited("1.001 ", double_, 1.001, char_(' ')));
        BOOST_TEST(test_delimited("1.01 ", double_, 1.010, char_(' ')));
        BOOST_TEST(test_delimited("1.1 ", double_, 1.100, char_(' ')));
        
        BOOST_TEST(test_delimited("1.234e-04 ", double_, 0.00012345, char_(' ')));
        BOOST_TEST(test_delimited("0.001 ", double_, 0.0012345, char_(' ')));
        BOOST_TEST(test_delimited("0.012 ", double_, 0.012345, char_(' ')));
        BOOST_TEST(test_delimited("0.123 ", double_, 0.12345, char_(' ')));
        BOOST_TEST(test_delimited("1.234 ", double_, 1.2345, char_(' ')));
        BOOST_TEST(test_delimited("12.346 ", double_, 12.346, char_(' ')));
        BOOST_TEST(test_delimited("123.46 ", double_, 123.46, char_(' ')));
        BOOST_TEST(test_delimited("1234.5 ", double_, 1234.5, char_(' ')));
        BOOST_TEST(test_delimited("12342.0 ", double_, 12342., char_(' ')));
        BOOST_TEST(test_delimited("1.234e05 ", double_, 123420., char_(' ')));
        
        BOOST_TEST(test_delimited("-1.0 ", double_, -1.0, char_(' ')));
        BOOST_TEST(test_delimited("-1.234 ", double_, -1.2345, char_(' ')));
        BOOST_TEST(test_delimited("-1.235 ", double_, -1.2346, char_(' ')));
        BOOST_TEST(test_delimited("-1234.2 ", double_, -1234.2, char_(' ')));
        
        BOOST_TEST(test_delimited("1.0 ", double_(1.0), char_(' ')));
        BOOST_TEST(test_delimited("1.0 ", double_(1.0001), char_(' ')));
        BOOST_TEST(test_delimited("1.001 ", double_(1.001), char_(' ')));
        BOOST_TEST(test_delimited("1.01 ", double_(1.010), char_(' ')));
        BOOST_TEST(test_delimited("1.1 ", double_(1.100), char_(' ')));
        
        BOOST_TEST(test_delimited("1.234e-04 ", double_(0.00012345), char_(' ')));
        BOOST_TEST(test_delimited("0.001 ", double_(0.0012345), char_(' ')));
        BOOST_TEST(test_delimited("0.012 ", double_(0.012345), char_(' ')));
        BOOST_TEST(test_delimited("0.123 ", double_(0.12345), char_(' ')));
        BOOST_TEST(test_delimited("1.234 ", double_(1.2345), char_(' ')));
        BOOST_TEST(test_delimited("12.346 ", double_(12.346), char_(' ')));
        BOOST_TEST(test_delimited("123.46 ", double_(123.46), char_(' ')));
        BOOST_TEST(test_delimited("1234.5 ", double_(1234.5), char_(' ')));
        BOOST_TEST(test_delimited("12342.0 ", double_(12342.), char_(' ')));
        BOOST_TEST(test_delimited("1.234e05 ", double_(123420.), char_(' ')));
    }

    {
        ///////////////////////////////////////////////////////////////////////
        // test NaN and Inf
        BOOST_TEST(test_delimited("nan ", double_, 
            std::numeric_limits<double>::quiet_NaN(), char_(' ')));
        BOOST_TEST(test_delimited("-nan ", double_, 
            -std::numeric_limits<double>::quiet_NaN(), char_(' ')));
        BOOST_TEST(test_delimited("inf ", double_, 
            std::numeric_limits<double>::infinity(), char_(' ')));
        BOOST_TEST(test_delimited("-inf ", double_, 
            -std::numeric_limits<double>::infinity(), char_(' ')));

        typedef karma::real_spec<double, signed_policy<double> > signed_type;
        signed_type const signed_ = signed_type();

        BOOST_TEST(test_delimited("+nan ", signed_, 
            std::numeric_limits<double>::quiet_NaN(), char_(' ')));
        BOOST_TEST(test_delimited("-nan ", signed_, 
            -std::numeric_limits<double>::quiet_NaN(), char_(' ')));
        BOOST_TEST(test_delimited("+inf ", signed_, 
            std::numeric_limits<double>::infinity(), char_(' ')));
        BOOST_TEST(test_delimited("-inf ", signed_, 
            -std::numeric_limits<double>::infinity(), char_(' ')));
        BOOST_TEST(test_delimited(" 0.0 ", signed_, 0.0, char_(' ')));
    }

    {
        using namespace boost::spirit::ascii;
        
        ///////////////////////////////////////////////////////////////////////
        typedef karma::real_spec<double, scientific_policy<double> > 
            science_type;
        science_type const science = science_type();
        
        BOOST_TEST(test("0.0e00", science, 0.0));
        BOOST_TEST(test("1.0e00", science, 1.0));
        
        BOOST_TEST(test("1.234e-05", science, 0.000012345));
        BOOST_TEST(test("1.234e-04", science, 0.00012345));
        BOOST_TEST(test("1.234e-03", science, 0.0012345));
        BOOST_TEST(test("1.234e-02", science, 0.012345));
        BOOST_TEST(test("1.235e-01", science, 0.12345));     // note the rounding error!
        BOOST_TEST(test("1.234e00", science, 1.2345));
        BOOST_TEST(test("1.235e01", science, 12.346));
        BOOST_TEST(test("1.235e02", science, 123.46));
        BOOST_TEST(test("1.234e03", science, 1234.5));
        BOOST_TEST(test("1.234e04", science, 12342.));
        BOOST_TEST(test("1.234e05", science, 123420.));

        BOOST_TEST(test("-1.234e-05", science, -0.000012345));
        BOOST_TEST(test("-1.234e-04", science, -0.00012345));
        BOOST_TEST(test("-1.234e-03", science, -0.0012345));
        BOOST_TEST(test("-1.234e-02", science, -0.012345));
        BOOST_TEST(test("-1.235e-01", science, -0.12345));   // note the rounding error!
        BOOST_TEST(test("-1.234e00", science, -1.2345));
        BOOST_TEST(test("-1.235e01", science, -12.346));
        BOOST_TEST(test("-1.235e02", science, -123.46));
        BOOST_TEST(test("-1.234e03", science, -1234.5));
        BOOST_TEST(test("-1.234e04", science, -12342.));
        BOOST_TEST(test("-1.234e05", science, -123420.));

        BOOST_TEST(test("1.234E-05", upper[science], 0.000012345));
        BOOST_TEST(test("1.234E-04", upper[science], 0.00012345));
        BOOST_TEST(test("1.234E-03", upper[science], 0.0012345));
        BOOST_TEST(test("1.234E-02", upper[science], 0.012345));
        BOOST_TEST(test("1.235E-01", upper[science], 0.12345));     // note the rounding error!
        BOOST_TEST(test("1.234E00", upper[science], 1.2345));
        BOOST_TEST(test("1.235E01", upper[science], 12.346));
        BOOST_TEST(test("1.235E02", upper[science], 123.46));
        BOOST_TEST(test("1.234E03", upper[science], 1234.5));
        BOOST_TEST(test("1.234E04", upper[science], 12342.));
        BOOST_TEST(test("1.234E05", upper[science], 123420.));

        BOOST_TEST(test("-1.234E-05", upper[science], -0.000012345));
        BOOST_TEST(test("-1.234E-04", upper[science], -0.00012345));
        BOOST_TEST(test("-1.234E-03", upper[science], -0.0012345));
        BOOST_TEST(test("-1.234E-02", upper[science], -0.012345));
        BOOST_TEST(test("-1.235E-01", upper[science], -0.12345));   // note the rounding error!
        BOOST_TEST(test("-1.234E00", upper[science], -1.2345));
        BOOST_TEST(test("-1.235E01", upper[science], -12.346));
        BOOST_TEST(test("-1.235E02", upper[science], -123.46));
        BOOST_TEST(test("-1.234E03", upper[science], -1234.5));
        BOOST_TEST(test("-1.234E04", upper[science], -12342.));
        BOOST_TEST(test("-1.234E05", upper[science], -123420.));
    }
    
    {
        using namespace boost::spirit::ascii;
        
        ///////////////////////////////////////////////////////////////////////
        typedef karma::real_spec<double, fixed_policy<double> > fixed_type;
        fixed_type const fixed = fixed_type();
        
        BOOST_TEST(test("0.0", fixed, 0.0));
        BOOST_TEST(test("1.0", fixed, 1.0));
        
        BOOST_TEST(test("0.0", fixed, 0.000012345));
        BOOST_TEST(test("0.0", fixed, 0.00012345));
        BOOST_TEST(test("0.001", fixed, 0.0012345));
        BOOST_TEST(test("0.012", fixed, 0.012345));
        BOOST_TEST(test("0.123", fixed, 0.12345));
        BOOST_TEST(test("1.234", fixed, 1.2345));
        BOOST_TEST(test("12.345", fixed, 12.345));
        BOOST_TEST(test("123.45", fixed, 123.45));
        BOOST_TEST(test("1234.5", fixed, 1234.5));
        BOOST_TEST(test("12342.0", fixed, 12342.));
        BOOST_TEST(test("123420.0", fixed, 123420.));
        BOOST_TEST(test("123420000000000000000.0", fixed, 1.23420e20));

        BOOST_TEST(test("0.0", fixed, -0.000012345));
        BOOST_TEST(test("0.0", fixed, -0.00012345));
        BOOST_TEST(test("-0.001", fixed, -0.0012345));
        BOOST_TEST(test("-0.012", fixed, -0.012345));
        BOOST_TEST(test("-0.123", fixed, -0.12345));
        BOOST_TEST(test("-1.234", fixed, -1.2345));
        BOOST_TEST(test("-12.346", fixed, -12.346));
        BOOST_TEST(test("-123.46", fixed, -123.46));
        BOOST_TEST(test("-1234.5", fixed, -1234.5));
        BOOST_TEST(test("-12342.0", fixed, -12342.));
        BOOST_TEST(test("-123420.0", fixed, -123420.));
        BOOST_TEST(test("-123420000000000000000.0", fixed, -1.23420e20));
    }

    {
        using namespace boost::spirit::ascii;
        
        ///////////////////////////////////////////////////////////////////////
        // test NaN and Inf
        BOOST_TEST(test("NAN", upper[double_], 
            std::numeric_limits<double>::quiet_NaN()));
        BOOST_TEST(test("-NAN", upper[double_], 
            -std::numeric_limits<double>::quiet_NaN()));
        BOOST_TEST(test("INF", upper[double_], 
            std::numeric_limits<double>::infinity()));
        BOOST_TEST(test("-INF", upper[double_], 
            -std::numeric_limits<double>::infinity()));

        typedef karma::real_spec<double, signed_policy<double> > signed_type;
        signed_type const signed_ = signed_type();

        BOOST_TEST(test("+NAN", upper[signed_], 
            std::numeric_limits<double>::quiet_NaN()));
        BOOST_TEST(test("-NAN", upper[signed_], 
            -std::numeric_limits<double>::quiet_NaN()));
        BOOST_TEST(test("+INF", upper[signed_], 
            std::numeric_limits<double>::infinity()));
        BOOST_TEST(test("-INF", upper[signed_], 
            -std::numeric_limits<double>::infinity()));
        BOOST_TEST(test(" 0.0", upper[signed_], 0.0));
    }

// support for using real_concept with a Karma generator has been implemented 
// in Boost versions > 1.36 only
#if BOOST_VERSION > 103600
    {
        using boost::math::concepts::real_concept;
        typedef karma::real_spec<real_concept> custom_type;
        custom_type const custom = custom_type();

        BOOST_TEST(test("0.0", custom, real_concept(0.0)));
        BOOST_TEST(test("1.0", custom, real_concept(1.0)));
        BOOST_TEST(test("1.0", custom, real_concept(1.0001)));
        BOOST_TEST(test("1.001", custom, real_concept(1.001)));
        BOOST_TEST(test("1.01", custom, real_concept(1.010)));
        BOOST_TEST(test("1.1", custom, real_concept(1.100)));
        
        BOOST_TEST(test("1.234e-04", custom, real_concept(0.00012345)));
        BOOST_TEST(test("0.001", custom, real_concept(0.0012345)));
        BOOST_TEST(test("0.012", custom, real_concept(0.012345)));
        BOOST_TEST(test("0.123", custom, real_concept(0.12345)));
        BOOST_TEST(test("1.234", custom, real_concept(1.2345)));
        BOOST_TEST(test("12.346", custom, real_concept(12.346)));
        BOOST_TEST(test("123.46", custom, real_concept(123.46)));
        BOOST_TEST(test("1234.5", custom, real_concept(1234.5)));
        BOOST_TEST(test("12342.0", custom, real_concept(12342.)));
        BOOST_TEST(test("1.234e05", custom, real_concept(123420.)));
        
        BOOST_TEST(test("-1.0", custom, real_concept(-1.0)));
        BOOST_TEST(test("-1.234", custom, real_concept(-1.2345)));
        BOOST_TEST(test("-1.235", custom, real_concept(-1.2346)));
        BOOST_TEST(test("-1234.2", custom, real_concept(-1234.2)));
        
        BOOST_TEST(test("1.0", custom(real_concept(1.0))));
        BOOST_TEST(test("1.0", custom(real_concept(1.0001))));
        BOOST_TEST(test("1.001", custom(real_concept(1.001))));
        BOOST_TEST(test("1.01", custom(real_concept(1.010))));
        BOOST_TEST(test("1.1", custom(real_concept(1.100))));
        
        BOOST_TEST(test("1.234e-04", custom(real_concept(0.00012345))));
        BOOST_TEST(test("0.001", custom(real_concept(0.0012345))));
        BOOST_TEST(test("0.012", custom(real_concept(0.012345))));
        BOOST_TEST(test("0.123", custom(real_concept(0.12345))));
        BOOST_TEST(test("1.234", custom(real_concept(1.2345))));
        BOOST_TEST(test("12.346", custom(real_concept(12.346))));
        BOOST_TEST(test("123.46", custom(real_concept(123.46))));
        BOOST_TEST(test("1234.5", custom(real_concept(1234.5))));
        BOOST_TEST(test("12342.0", custom(real_concept(12342.))));
        BOOST_TEST(test("1.234e05", custom(real_concept(123420.))));
    }
#endif

    return boost::report_errors();
}
