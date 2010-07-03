/*=============================================================================
    Copyright (c) 2002 2004 2006 Joel de Guzman
    http://spirit.sourceforge.net/

    Use, modification and distribution is subject to the Boost Software
    License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
    http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#if !defined(BOOST_SPIRIT_QUICKBOOK_TEMPLATE_STACK_HPP)
#define BOOST_SPIRIT_QUICKBOOK_TEMPLATE_STACK_HPP

#include <string>
#include <deque>
#include <vector>
#include <boost/tuple/tuple.hpp>
#include <boost/assert.hpp>
#include <boost/spirit/include/classic_position_iterator.hpp>
#include <boost/spirit/include/classic_functor_parser.hpp>
#include <boost/spirit/include/classic_symbols.hpp>
#include <boost/next_prior.hpp>

namespace quickbook
{
    //  template symbols are stored as follows:
    //
    //      template name
    //      template param name[0]
    //      template param name[1]
    //      ...
    //      template param name[N]
    //      template body

    typedef boost::tuple<
            std::vector<std::string>
          , boost::spirit::classic::file_position>
    template_symbol;

    typedef boost::spirit::classic::symbols<template_symbol> template_symbols;

    struct template_stack
    {
        typedef std::deque<template_symbols> deque;

        struct parser
        {
            typedef boost::spirit::classic::nil_t result_t;

            parser(template_stack& ts)
                : ts(ts) {}

            template <typename Scanner>
            std::ptrdiff_t
            operator()(Scanner const& scan, result_t) const
            {
                // search all scopes for the longest matching symbol.
                typename Scanner::iterator_t f = scan.first;
                std::ptrdiff_t len = -1;
                for (template_stack::deque::const_iterator i = ts.scopes.begin();
                    i != ts.scopes.end(); ++i)
                {
                    boost::spirit::classic::match<> m = i->parse(scan);
                    if (m.length() > len)
                        len = m.length();
                    scan.first = f;
                }
                if (len >= 0)
                    scan.first = boost::next(f, len);
                return len;
            }

            template_stack& ts;
        };

        template_stack();
        template_symbol* find(std::string const& symbol) const;
        template_symbol* find_top_scope(std::string const& symbol) const;
        template_symbols const& top() const;
        void add(std::string const& symbol, template_symbol const& ts);
        void push();
        void pop();

        boost::spirit::classic::functor_parser<parser> scope;

    private:

        friend struct parser;
        deque scopes;
    };
}

#endif // BOOST_SPIRIT_QUICKBOOK_TEMPLATE_STACK_HPP

