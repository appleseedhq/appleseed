//Copyright (c) 2006-2009 Emil Dotchevski and Reverge Studios, Inc.

//Distributed under the Boost Software License, Version 1.0. (See accompanying
//file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef UUID_0552D49838DD11DD90146B8956D89593
#define UUID_0552D49838DD11DD90146B8956D89593

#include <boost/config.hpp>
#include <boost/exception/get_error_info.hpp>
#include <boost/utility/enable_if.hpp>
#include <exception>
#include <sstream>
#include <string>

namespace
boost
    {
    namespace
    exception_detail
        {
        template <class T>
        struct
        enable_boost_exception_overload
            {
            struct yes { char q[100]; };
            typedef char no;
            static yes check(exception const *);
            static no check(...);
            enum e { value=sizeof(check((T*)0))==sizeof(yes) };
            };

        template <class T>
        struct
        enable_std_exception_overload
            {
            struct yes { char q[100]; };
            typedef char no;
            static yes check(std::exception const *);
            static no check(...);
            enum e { value = !enable_boost_exception_overload<T>::value && sizeof(check((T*)0))==sizeof(yes) };
            };

#ifndef BOOST_NO_RTTI
        template <class T>
        inline
        std::string
        dynamic_exception_type( T const & x )
            {
            return std::string("Dynamic exception type: ") + BOOST_EXCEPTION_DYNAMIC_TYPEID(x).name();
            }
#endif

        inline
        char const *
        get_diagnostic_information( exception const & x )
            {
            if( error_info_container * c=x.data_.get() )
#ifndef BOOST_NO_EXCEPTIONS
                try
                    {
#endif
                    return c->diagnostic_information();
#ifndef BOOST_NO_EXCEPTIONS
                    }
                catch(...)
                    {
                    }
#endif
            return 0;
            }

        inline
        std::string
        boost_diagnostic_information( exception const & x )
            {
            std::ostringstream tmp;
            if( char const * const * f=get_error_info<throw_file>(x) )
                {
                tmp << *f;
                if( int const * l=get_error_info<throw_line>(x) )
                    tmp << '(' << *l << "): ";
                }
            tmp << "Throw in function ";
            if( char const * const * fn=get_error_info<throw_function>(x) )
                tmp << *fn;
            else
                tmp << "(unknown)";
            tmp << std::endl;
#ifndef BOOST_NO_RTTI
            tmp << dynamic_exception_type(x) << std::endl;
            if( std::exception const * e=dynamic_cast<std::exception const *>(&x) )
                tmp << "std::exception::what: " << e->what() << std::endl;
#endif
            if( char const * s=exception_detail::get_diagnostic_information(x) )
                if( *s )
                    tmp << s;
            return tmp.str();
            }

        inline
        std::string
        std_diagnostic_information( std::exception const & x )
            {
            std::ostringstream tmp;
#ifndef BOOST_NO_RTTI
            if( exception const * e=dynamic_cast<exception const *>(&x) )
                return boost_diagnostic_information(*e);
            tmp << dynamic_exception_type(x) << std::endl;
#endif
            tmp << "std::exception::what: " << x.what() << std::endl;
            return tmp.str();
            }
        }

    template <class T>
    inline
    typename enable_if<exception_detail::enable_boost_exception_overload<T>,std::string>::type
    diagnostic_information( T const & e )
        {
        return exception_detail::boost_diagnostic_information(e);
        }

    template <class T>
    inline
    typename enable_if<exception_detail::enable_std_exception_overload<T>,std::string>::type
    diagnostic_information( T const & e )
        {
        return exception_detail::std_diagnostic_information(e);
        }
    }

#ifndef BOOST_NO_EXCEPTIONS
#include <boost/exception/current_exception_cast.hpp>
namespace
boost
    {
    inline
    std::string
    current_exception_diagnostic_information()
        {
        if( boost::exception const * e=current_exception_cast<boost::exception const>() )
            return diagnostic_information(*e);
        else if( std::exception const * e=current_exception_cast<std::exception const>() )
            return diagnostic_information(*e);
        else
            return "No diagnostic information available.";
        }
	}
#endif

#endif
