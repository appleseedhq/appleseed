/*=============================================================================
    Copyright (c) 2001-2007 Joel de Guzman
    Copyright (c) 2001-2009 Hartmut Kaiser

    Use, modification and distribution is subject to the Boost Software
    License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
    http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include <climits>
#include <boost/math/concepts/real_concept.hpp>
#include <boost/detail/lightweight_test.hpp>
#include <boost/spirit/include/qi_char.hpp>
#include <boost/spirit/include/qi_numeric.hpp>
#include <boost/spirit/include/qi_operator.hpp>
#include <boost/spirit/home/support/detail/math/fpclassify.hpp>
#include <boost/spirit/home/support/detail/math/signbit.hpp>

#include "test.hpp"
using namespace spirit_test;

///////////////////////////////////////////////////////////////////////////////
//  These policies can be used to parse thousand separated
//  numbers with at most 2 decimal digits after the decimal
//  point. e.g. 123,456,789.01
///////////////////////////////////////////////////////////////////////////////
template <typename T>
struct ts_real_policies : boost::spirit::qi::ureal_policies<T>
{
    //  2 decimal places Max
    template <typename Iterator, typename Attribute>
    static bool
    parse_frac_n(Iterator& first, Iterator const& last, Attribute& attr)
    {
        return boost::spirit::qi::
            extract_uint<T, 10, 1, 2, true>::call(first, last, attr);
    }

    //  No exponent
    template <typename Iterator>
    static bool
    parse_exp(Iterator&, Iterator const&)
    {
        return false;
    }

    //  No exponent
    template <typename Iterator, typename Attribute>
    static bool
    parse_exp_n(Iterator&, Iterator const&, Attribute&)
    {
        return false;
    }

    //  Thousands separated numbers
    template <typename Iterator, typename Attribute>
    static bool
    parse_n(Iterator& first, Iterator const& last, Attribute& attr)
    {
        using namespace boost::spirit::qi;
        using namespace boost::spirit;

        uint_spec<unsigned, 10, 1, 3> uint3;
        uint_spec<unsigned, 10, 3, 3> uint3_3;

        T result = 0;
        if (parse(first, last, uint3, result))
        {
            bool hit = false;
            T n;
            Iterator save = first;

            while (parse(first, last, ',') && parse(first, last, uint3_3, n))
            {
                result = result * 1000 + n;
                save = first;
                hit = true;
            }

            first = save;
            if (hit)
                attr = result;
            return hit;
        }
        return false;
    }
};

template <typename T>
struct no_trailing_dot_policy : boost::spirit::qi::real_policies<T>
{
    static bool const allow_trailing_dot = false;
};

template <typename T>
struct no_leading_dot_policy : boost::spirit::qi::real_policies<T>
{
    static bool const allow_leading_dot = false;
};

template <typename T>
bool
compare(T n, double expected)
{
    double const eps = 0.00001;
    T delta = n - expected;
    return (delta >= -eps) && (delta <= eps);
}

int
main()
{
    ///////////////////////////////////////////////////////////////////////////////
    //  thousand separated numbers
    ///////////////////////////////////////////////////////////////////////////////
    {
        using boost::spirit::qi::uint_spec;
        using boost::spirit::qi::parse;

        uint_spec<unsigned, 10, 1, 3> uint3;
        uint_spec<unsigned, 10, 3, 3> uint3_3;

    #define r (uint3 >> *(',' >> uint3_3))

        BOOST_TEST(test("1,234,567,890", r));
        BOOST_TEST(test("12,345,678,900", r));
        BOOST_TEST(test("123,456,789,000", r));
        BOOST_TEST(!test("1000,234,567,890", r));
        BOOST_TEST(!test("1,234,56,890", r));
        BOOST_TEST(!test("1,66", r));
    }

    ///////////////////////////////////////////////////////////////////////////////
    //  unsigned real number tests
    ///////////////////////////////////////////////////////////////////////////////
    {
        using boost::spirit::qi::real_spec;
        using boost::spirit::qi::parse;
        using boost::spirit::qi::ureal_policies;

        real_spec<double, ureal_policies<double> > udouble;
        double  d;

        BOOST_TEST(test("1234", udouble));
        BOOST_TEST(test_attr("1234", udouble, d) && compare(d, 1234));

        BOOST_TEST(test("1.2e3", udouble));
        BOOST_TEST(test_attr("1.2e3", udouble, d) && compare(d, 1.2e3));

        BOOST_TEST(test("1.2e-3", udouble));
        BOOST_TEST(test_attr("1.2e-3", udouble, d) && compare(d, 1.2e-3));

        BOOST_TEST(test("1.e2", udouble));
        BOOST_TEST(test_attr("1.e2", udouble, d) && compare(d, 1.e2));

        BOOST_TEST(test("1.", udouble));
        BOOST_TEST(test_attr("1.", udouble, d) && compare(d, 1.));

        BOOST_TEST(test(".2e3", udouble));
        BOOST_TEST(test_attr(".2e3", udouble, d) && compare(d, .2e3));

        BOOST_TEST(test("2e3", udouble));
        BOOST_TEST(test_attr("2e3", udouble, d) && compare(d, 2e3));

        BOOST_TEST(test("2", udouble));
        BOOST_TEST(test_attr("2", udouble, d) && compare(d, 2));

        using boost::math::fpclassify;
        BOOST_TEST(test("inf", udouble));
        BOOST_TEST(test("infinity", udouble));
        BOOST_TEST(test("INF", udouble));
        BOOST_TEST(test("INFINITY", udouble));
        BOOST_TEST(test_attr("inf", udouble, d) && FP_INFINITE == fpclassify(d));
        BOOST_TEST(test_attr("INF", udouble, d) && FP_INFINITE == fpclassify(d));
        BOOST_TEST(test_attr("infinity", udouble, d) && FP_INFINITE == fpclassify(d));
        BOOST_TEST(test_attr("INFINITY", udouble, d) && FP_INFINITE == fpclassify(d));

        BOOST_TEST(test("nan", udouble));
        BOOST_TEST(test_attr("nan", udouble, d) && FP_NAN == fpclassify(d));
        BOOST_TEST(test("NAN", udouble));
        BOOST_TEST(test_attr("NAN", udouble, d) && FP_NAN == fpclassify(d));

        BOOST_TEST(test("nan(...)", udouble));
        BOOST_TEST(test_attr("nan(...)", udouble, d) && FP_NAN == fpclassify(d));
        BOOST_TEST(test("NAN(...)", udouble));
        BOOST_TEST(test_attr("NAN(...)", udouble, d) && FP_NAN == fpclassify(d));

        BOOST_TEST(!test("e3", udouble));
        BOOST_TEST(!test_attr("e3", udouble, d));

        BOOST_TEST(!test("-1.2e3", udouble));
        BOOST_TEST(!test_attr("-1.2e3", udouble, d));

        BOOST_TEST(!test("+1.2e3", udouble));
        BOOST_TEST(!test_attr("+1.2e3", udouble, d));

        BOOST_TEST(!test("1.2e", udouble));
        BOOST_TEST(!test_attr("1.2e", udouble, d));

        BOOST_TEST(!test("-.3", udouble));
        BOOST_TEST(!test_attr("-.3", udouble, d));
    }

///////////////////////////////////////////////////////////////////////////////
//  signed real number tests
///////////////////////////////////////////////////////////////////////////////
    {
        using boost::spirit::double_;
        using boost::spirit::qi::parse;
        double  d;

        BOOST_TEST(test("-1234", double_));
        BOOST_TEST(test_attr("-1234", double_, d) && compare(d, -1234));

        BOOST_TEST(test("-1.2e3", double_));
        BOOST_TEST(test_attr("-1.2e3", double_, d) && compare(d, -1.2e3));

        BOOST_TEST(test("+1.2e3", double_));
        BOOST_TEST(test_attr("+1.2e3", double_, d) && compare(d, 1.2e3));

        BOOST_TEST(test("-0.1", double_));
        BOOST_TEST(test_attr("-0.1", double_, d) && compare(d, -0.1));

        BOOST_TEST(test("-1.2e-3", double_));
        BOOST_TEST(test_attr("-1.2e-3", double_, d) && compare(d, -1.2e-3));

        BOOST_TEST(test("-1.e2", double_));
        BOOST_TEST(test_attr("-1.e2", double_, d) && compare(d, -1.e2));

        BOOST_TEST(test("-.2e3", double_));
        BOOST_TEST(test_attr("-.2e3", double_, d) && compare(d, -.2e3));

        BOOST_TEST(test("-2e3", double_));
        BOOST_TEST(test_attr("-2e3", double_, d) && compare(d, -2e3));

        BOOST_TEST(!test("-e3", double_));
        BOOST_TEST(!test_attr("-e3", double_, d));

        BOOST_TEST(!test("-1.2e", double_));
        BOOST_TEST(!test_attr("-1.2e", double_, d));

        using boost::spirit::math::fpclassify;
        using boost::spirit::math::signbit;
        BOOST_TEST(test("-inf", double_));
        BOOST_TEST(test("-infinity", double_));
        BOOST_TEST(test_attr("-inf", double_, d) &&
            FP_INFINITE == fpclassify(d) && signbit(d));
        BOOST_TEST(test_attr("-infinity", double_, d) &&
            FP_INFINITE == fpclassify(d) && signbit(d));
        BOOST_TEST(test("-INF", double_));
        BOOST_TEST(test("-INFINITY", double_));
        BOOST_TEST(test_attr("-INF", double_, d) &&
            FP_INFINITE == fpclassify(d) && signbit(d));
        BOOST_TEST(test_attr("-INFINITY", double_, d) &&
            FP_INFINITE == fpclassify(d) && signbit(d));

        BOOST_TEST(test("-nan", double_));
        BOOST_TEST(test_attr("-nan", double_, d) &&
            FP_NAN == fpclassify(d) && signbit(d));
        BOOST_TEST(test("-NAN", double_));
        BOOST_TEST(test_attr("-NAN", double_, d) &&
            FP_NAN == fpclassify(d) && signbit(d));

        BOOST_TEST(test("-nan(...)", double_));
        BOOST_TEST(test_attr("-nan(...)", double_, d) &&
            FP_NAN == fpclassify(d) && signbit(d));
        BOOST_TEST(test("-NAN(...)", double_));
        BOOST_TEST(test_attr("-NAN(...)", double_, d) &&
            FP_NAN == fpclassify(d) && signbit(d));
    }

    ///////////////////////////////////////////////////////////////////////////////
    //  strict real number tests
    ///////////////////////////////////////////////////////////////////////////////
    {
        using boost::spirit::qi::real_spec;
        using boost::spirit::qi::parse;
        using boost::spirit::qi::strict_ureal_policies;
        using boost::spirit::qi::strict_real_policies;

        real_spec<double, strict_ureal_policies<double> > strict_udouble;
        real_spec<double, strict_real_policies<double> > strict_double;
        double  d;

        BOOST_TEST(!test("1234", strict_udouble));
        BOOST_TEST(!test_attr("1234", strict_udouble, d));

        BOOST_TEST(test("1.2", strict_udouble));
        BOOST_TEST(test_attr("1.2", strict_udouble, d) && compare(d, 1.2));

        BOOST_TEST(!test("-1234", strict_double));
        BOOST_TEST(!test_attr("-1234", strict_double, d));

        BOOST_TEST(test("123.", strict_double));
        BOOST_TEST(test_attr("123.", strict_double, d) && compare(d, 123));

        BOOST_TEST(test("3.E6", strict_double));
        BOOST_TEST(test_attr("3.E6", strict_double, d) && compare(d, 3e6));

        real_spec<double, no_trailing_dot_policy<double> > notrdot_real;
        real_spec<double, no_leading_dot_policy<double> > nolddot_real;

        BOOST_TEST(!test("1234.", notrdot_real));          //  Bad trailing dot
        BOOST_TEST(!test(".1234", nolddot_real));          //  Bad leading dot
    }

    ///////////////////////////////////////////////////////////////////////////
    //  Special thousands separated numbers
    ///////////////////////////////////////////////////////////////////////////
    {
        using boost::spirit::qi::real_spec;
        using boost::spirit::qi::parse;
        real_spec<double, ts_real_policies<double> > ts_real;
        double  d;

        BOOST_TEST(test("123,456,789.01", ts_real));
        BOOST_TEST(test_attr("123,456,789.01", ts_real, d) && compare(d, 123456789.01));

        BOOST_TEST(test("12,345,678.90", ts_real));
        BOOST_TEST(test_attr("12,345,678.90", ts_real, d) && compare(d, 12345678.90));

        BOOST_TEST(test("1,234,567.89", ts_real));
        BOOST_TEST(test_attr("1,234,567.89", ts_real, d) && compare(d, 1234567.89));

        BOOST_TEST(!test("1234,567,890", ts_real));
        BOOST_TEST(!test("1,234,5678,9", ts_real));
        BOOST_TEST(!test("1,234,567.89e6", ts_real));
        BOOST_TEST(!test("1,66", ts_real));
    }

    ///////////////////////////////////////////////////////////////////////////
    //  Custom data type
    ///////////////////////////////////////////////////////////////////////////
    {
        using boost::math::concepts::real_concept;
        using boost::spirit::qi::real_spec;
        using boost::spirit::qi::real_policies;
        using boost::spirit::qi::parse;
        
        real_spec<real_concept, real_policies<real_concept> > custom_real;
        real_concept d;
        
        BOOST_TEST(test("-1234", custom_real));
        BOOST_TEST(test_attr("-1234", custom_real, d) && compare(d, -1234));

        BOOST_TEST(test("-1.2e3", custom_real));
        BOOST_TEST(test_attr("-1.2e3", custom_real, d) && compare(d, -1.2e3));

        BOOST_TEST(test("+1.2e3", custom_real));
        BOOST_TEST(test_attr("+1.2e3", custom_real, d) && compare(d, 1.2e3));

        BOOST_TEST(test("-0.1", custom_real));
        BOOST_TEST(test_attr("-0.1", custom_real, d) && compare(d, -0.1));

        BOOST_TEST(test("-1.2e-3", custom_real));
        BOOST_TEST(test_attr("-1.2e-3", custom_real, d) && compare(d, -1.2e-3));

        BOOST_TEST(test("-1.e2", custom_real));
        BOOST_TEST(test_attr("-1.e2", custom_real, d) && compare(d, -1.e2));

        BOOST_TEST(test("-.2e3", custom_real));
        BOOST_TEST(test_attr("-.2e3", custom_real, d) && compare(d, -.2e3));

        BOOST_TEST(test("-2e3", custom_real));
        BOOST_TEST(test_attr("-2e3", custom_real, d) && compare(d, -2e3));

        BOOST_TEST(!test("-e3", custom_real));
        BOOST_TEST(!test_attr("-e3", custom_real, d));

        BOOST_TEST(!test("-1.2e", custom_real));
        BOOST_TEST(!test_attr("-1.2e", custom_real, d));
    }
    
    return boost::report_errors();
}
