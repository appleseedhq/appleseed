//  Copyright (c) 2001-2007 Joel de Guzman
//  Copyright (c) 2001-2009 Hartmut Kaiser
//
//  Distributed under the Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#if !defined(SPIRIT_KARMA_OPTIONAL_MARCH_31_2007_0852AM)
#define SPIRIT_KARMA_OPTIONAL_MARCH_31_2007_0852AM

#if defined(_MSC_VER) && (_MSC_VER >= 1020)
#pragma once      // MS compatible compilers support #pragma once
#endif

#include <boost/spirit/home/support/component.hpp>
#include <boost/spirit/home/support/attribute_of.hpp>
#include <boost/spirit/home/support/attribute_transform.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/optional.hpp>

namespace boost { namespace spirit { namespace karma
{
    namespace detail
    {
        template <typename Parameter>
        inline bool
        optional_is_valid(boost::optional<Parameter> const& opt)
        {
            return opt;
        }

        template <typename Parameter>
        inline bool
        optional_is_valid(Parameter const& opt)
        {
            return true;
        }

        inline bool
        optional_is_valid(unused_type)
        {
            return true;
        }

        template <typename Parameter>
        inline Parameter const&
        optional_get(boost::optional<Parameter> const& opt)
        {
            return get(opt);
        }

        template <typename Parameter>
        inline Parameter const&
        optional_get(Parameter const& opt)
        {
            return opt;
        }

        inline unused_type
        optional_get(unused_type)
        {
            return unused;
        }
    }

    struct optional
    {
        template <typename T>
        struct build_attribute_container
        {
            typedef boost::optional<T> type;
        };

        template <typename Component, typename Context, typename Iterator>
        struct attribute :
            build_container<optional, Component, Iterator, Context>
        {
        };

        template <typename Component, typename OutputIterator,
            typename Context, typename Delimiter, typename Parameter>
        static bool
        generate(Component const& component, OutputIterator& sink,
            Context& ctx, Delimiter const& d, Parameter const& param)
        {
            typedef typename
                result_of::subject<Component>::type::director
            director;

            if (detail::optional_is_valid(param))
            {
                director::generate(subject(component), sink, ctx, d,
                    detail::optional_get(param));
            }
            return true;
        }

        template <typename Component, typename Context>
        static std::string what(Component const& component, Context const& ctx)
        {
            std::string result = "optional[";

            typedef typename
                spirit::result_of::subject<Component>::type::director
            director;

            result += director::what(spirit::subject(component), ctx);
            result += "]";
            return result;
        }
    };

}}}

#endif
