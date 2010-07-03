//  Copyright (c) 2001-2009 Hartmut Kaiser
// 
//  Distributed under the Boost Software License, Version 1.0. (See accompanying 
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

//  This examples demonstrate how to write functor based generators for special
//  purposes. 

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/karma.hpp>
#include <boost/spirit/include/karma_stream.hpp>

#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_bind.hpp>

#include <iostream>
#include <string>
#include <vector>
#include <list>
#include <algorithm>
#include <cstdlib> 

using namespace boost::spirit;

///////////////////////////////////////////////////////////////////////////////
//  The functor generator 'counter' can be used for output annotation with some
//  item counting information.
///////////////////////////////////////////////////////////////////////////////
struct counter_impl : boost::spirit::karma::functor_base
{
    template <typename OutputIterator, typename Context, typename Parameter>
    bool operator()(Parameter const&, Context& ctx, OutputIterator& sink) const
    {
        return karma::generate(sink, int_ << ": ", counter++);
    }

    counter_impl(int& counter_) 
    : counter(counter_) {}

    int& counter;
};

inline boost::spirit::result_of::as_generator<counter_impl>::type
counter(int& counter_)
{
    using namespace boost::spirit::karma;
    return as_generator(counter_impl(counter_));
}

///////////////////////////////////////////////////////////////////////////////
//  The functor generator 'confix' allows a simple syntax for generating 
//  output wrapped inside a pair of a prefix and a suffix.
///////////////////////////////////////////////////////////////////////////////
template <typename Expr>
struct confix_impl : public boost::spirit::karma::functor_base
{
    template <typename Context>
    struct apply
    {
        typedef boost::spirit::hold_any type;
    };

    template <typename OutputIterator, typename Context, typename Parameter>
    bool operator()(Parameter const& v, Context& ctx, OutputIterator& sink) const
    {
        return karma::generate(sink, open << xpr << close, v);
    }

    confix_impl(char const* open_, char const* close_, Expr const& xpr_) 
    : open(open_), close(close_), xpr(xpr_) {}

    std::string open;
    std::string close;
    Expr xpr;
};

template <typename Expr>
inline typename boost::spirit::result_of::as_generator<confix_impl<Expr> >::type
confix(Expr const& xpr_, char const* open_ = "", char const* close_ = "")
{
    using namespace boost::spirit::karma;
    return as_generator(confix_impl<Expr>(open_, close_, xpr_));
}

///////////////////////////////////////////////////////////////////////////////
//  The functor generator 'list' allows a simple syntax for generating 
//  list formatted output.
//
//  This example uses phoenix::bind to allow to omit the second argument from 
//  the operator() and to allow to switch the remaining two arguments.
///////////////////////////////////////////////////////////////////////////////
template <typename Expr>
struct list_impl : boost::spirit::karma::functor_base
{
    // this function will be called to generate the output
    template <typename OutputIterator, typename Parameter>
    bool operator()(OutputIterator& sink, Parameter const& v) const
    {
        return karma::generate(sink, xpr % delim, v);
    }

    list_impl(Expr const& xpr_, char const* delim_) 
    : xpr(xpr_), delim(delim_) {}

    Expr xpr;
    std::string delim;
};

//  Supply the expected parameter type explicitly
struct list_impl_mf
{
    // the expected parameter type of a functor has to be defined using a
    // embedded apply metafunction
    template <typename Context>
    struct apply
    {
        typedef boost::spirit::hold_any type;
    };
};

template <typename Expr>
inline list_impl<Expr>
list(Expr const& xpr, char const* delim)
{
    return list_impl<Expr>(xpr, delim);
}

///////////////////////////////////////////////////////////////////////////////
int main()
{
    using namespace boost::phoenix;
    using namespace boost::phoenix::arg_names;

    ///////////////////////////////////////////////////////////////////////////
    //  Output the given containers in list format
    //  We use a special functor generator here to annotate the output with 
    //  a integer counting the entries.
    ///////////////////////////////////////////////////////////////////////////
    std::vector<int> v (8);
    std::generate(v.begin(), v.end(), std::rand); // randomly fill the vector

    int counter1 = 1;
    std::cout << 
        karma::format(
            (counter(counter1) << int_) % ", ",   // format description
            v                                     // data
        ) << std::endl;

    //  Here we initialize the counter to 100
    int counter2 = 100;
    std::cout << 
        karma::format(
            '[' << (
                (counter(counter2) << int_) % ", "
             ) << ']',                            // format description
            v                                     // data
        ) << std::endl;

    ///////////////////////////////////////////////////////////////////////////
    //  list
    //  The output format description used below adds special item formatting
    ///////////////////////////////////////////////////////////////////////////
    std::list<std::string> names;
    names.push_back("Spirit");
    names.push_back("Qi");
    names.push_back("Karma");

    // specifying a prefix item suffix scheme directly
    std::cout << 
        karma::format(
            ('{' << stream << '}') % ", ",        // format description
            names                                 // data
        ) << std::endl;

    //  The confix generator nicely wraps the given expression with prefix and 
    //  suffix strings
    std::cout << 
        karma::format(
            confix(stream % ", ", "[", "]"),      // format description
            names                                 // data
        ) << std::endl;

    ///////////////////////////////////////////////////////////////////////////
    //  Output the given container as a list
    //  We use a separate metafunction list_impl_mf to specify the expected 
    //  parameter type of this functor generator. 
    //  We use phoenix::bind to allow to omit the 2nd argument from the functor
    //  function operator and to change the sequence of the remaining two 
    //  arguments.
    ///////////////////////////////////////////////////////////////////////////
    std::string str("Hello world!");
    std::cout << 
        karma::format(
            karma::as_generator_mf<list_impl_mf>(bind(list(stream, ", "), _3, _1)),
            str
        ) << std::endl;
    
    return 0;
}
