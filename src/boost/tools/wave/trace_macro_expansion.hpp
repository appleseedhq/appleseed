/*=============================================================================
    Boost.Wave: A Standard compliant C++ preprocessor library
    http://www.boost.org/

    Copyright (c) 2001-2009 Hartmut Kaiser. Distributed under the Boost
    Software License, Version 1.0. (See accompanying file
    LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/

#if !defined(TRACE_MACRO_EXPANSION_HPP_D8469318_8407_4B9D_A19F_13CA60C1661F_INCLUDED)
#define TRACE_MACRO_EXPANSION_HPP_D8469318_8407_4B9D_A19F_13CA60C1661F_INCLUDED

#include <cstdio>
#include <cstdlib>
#include <ctime>

#include <ostream>
#include <string>
#include <stack>

#include <boost/assert.hpp>
#include <boost/config.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/convenience.hpp>

#include <boost/wave/token_ids.hpp>
#include <boost/wave/util/macro_helpers.hpp>
#include <boost/wave/util/filesystem_compatibility.hpp>
#include <boost/wave/preprocessing_hooks.hpp>
#include <boost/wave/whitespace_handling.hpp>
#include <boost/wave/language_support.hpp>

#include "stop_watch.hpp"

#ifdef BOOST_NO_STRINGSTREAM
#include <strstream>
#define BOOST_WAVE_OSSTREAM std::ostrstream
std::string BOOST_WAVE_GETSTRING(std::ostrstream& ss)
{
    ss << std::ends;
    std::string rval = ss.str();
    ss.freeze(false);
    return rval;
}
#else
#include <sstream>
#define BOOST_WAVE_GETSTRING(ss) ss.str()
#define BOOST_WAVE_OSSTREAM std::ostringstream
#endif

//  trace_flags:  enable single tracing functionality
enum trace_flags {
    trace_nothing = 0,      // disable tracing
    trace_macros = 1,       // enable macro tracing
    trace_includes = 2      // enable include file tracing
};

///////////////////////////////////////////////////////////////////////////////
//
//  Special error thrown whenever the #pragma wave system() directive is 
//  disabled
//
///////////////////////////////////////////////////////////////////////////////
class bad_pragma_exception :
    public boost::wave::preprocess_exception
{
public:
    enum error_code {
        pragma_system_not_enabled = boost::wave::preprocess_exception::last_error_number + 1,
        pragma_mismatched_push_pop,
    };
    
    bad_pragma_exception(char const *what_, error_code code, int line_, 
        int column_, char const *filename_) throw() 
    :   boost::wave::preprocess_exception(what_, 
            (boost::wave::preprocess_exception::error_code)code, line_, 
            column_, filename_)
    {
    }
    ~bad_pragma_exception() throw() {}
    
    
    virtual char const *what() const throw()
    {
        return "boost::wave::bad_pragma_exception";
    }
    virtual bool is_recoverable() const throw()
    {
        return true;
    }
    virtual int get_severity() const throw()
    {
        return boost::wave::util::severity_remark;
    }
    
    static char const *error_text(int code)
    {
        switch(code) {
        case pragma_system_not_enabled:
            return "the directive '#pragma wave system()' was not enabled, use the "
                   "-x command line argument to enable the execution of";
                   
        case pragma_mismatched_push_pop:
            return "unbalanced #pragma push/pop in input file(s) for option";
        }
        return "Unknown exception";
    }
    static boost::wave::util::severity severity_level(int code)
    {
        switch(code) {
        case pragma_system_not_enabled:
            return boost::wave::util::severity_remark;
    
        case pragma_mismatched_push_pop:
            return boost::wave::util::severity_error;
        }
        return boost::wave::util::severity_fatal;
    }
    static char const *severity_text(int code)
    {
        return boost::wave::util::get_severity(boost::wave::util::severity_remark);
    }
};

///////////////////////////////////////////////////////////////////////////////
//  
//  The trace_macro_expansion policy is used to trace the macro expansion of
//  macros whenever it is requested from inside the input stream to preprocess
//  through the '#pragma wave_option(trace: enable)' directive. The macro 
//  tracing is disabled with the help of a '#pragma wave_option(trace: disable)'
//  directive.
//
//  This policy type is used as a template parameter to the boost::wave::context<>
//  object.
//
///////////////////////////////////////////////////////////////////////////////
template <typename TokenT>
class trace_macro_expansion
:   public boost::wave::context_policies::eat_whitespace<TokenT>
{
    typedef boost::wave::context_policies::eat_whitespace<TokenT> base_type;
    
public:
    trace_macro_expansion(bool preserve_whitespace_, 
            std::ofstream &output_, std::ostream &tracestrm_, 
            std::ostream &includestrm_, trace_flags flags_, 
            bool enable_system_command_, bool& generate_output_,
            std::string const& default_outfile_)
    :   outputstrm(output_), tracestrm(tracestrm_), includestrm(includestrm_), 
        level(0), flags(flags_), logging_flags(trace_nothing), 
        enable_system_command(enable_system_command_),
        preserve_whitespace(preserve_whitespace_),
        generate_output(generate_output_),
        default_outfile(default_outfile_)
    {
        using namespace std;    // some systems have time in namespace std
        time(&started_at);
    }
    ~trace_macro_expansion()
    {
    }
    
    ///////////////////////////////////////////////////////////////////////////
    //  
    //  The function 'expanding_function_like_macro' is called whenever a 
    //  function-like macro is to be expanded.
    //
    //  The parameter 'ctx' is a reference to the context object used for 
    //  instantiating the preprocessing iterators by the user.
    //
    //  The parameter 'macrodef' marks the position, where the macro to expand 
    //  is defined.
    //
    //  The parameter 'formal_args' holds the formal arguments used during the
    //  definition of the macro.
    //
    //  The parameter 'definition' holds the macro definition for the macro to 
    //  trace.
    //
    //  The parameter 'macro_call' marks the position, where this macro invoked.
    //
    //  The parameter 'arguments' holds the macro arguments used during the 
    //  invocation of the macro
    //
    //  The parameters 'seqstart' and 'seqend' point into the input token 
    //  stream allowing to access the whole token sequence comprising the macro
    //  invocation (starting with the opening parenthesis and ending after the
    //  closing one).
    //
    ///////////////////////////////////////////////////////////////////////////
#if BOOST_WAVE_USE_DEPRECIATED_PREPROCESSING_HOOKS != 0
    // old signature
    template <typename ContainerT>
    void expanding_function_like_macro(
        TokenT const &macrodef, std::vector<TokenT> const &formal_args, 
        ContainerT const &definition,
        TokenT const &macrocall, std::vector<ContainerT> const &arguments) 
    {
        if (!enabled_macro_tracing()) 
            return;
#else
    // new signature
    template <typename ContextT, typename ContainerT, typename IteratorT>
    bool 
    expanding_function_like_macro(ContextT const& ctx,
        TokenT const &macrodef, std::vector<TokenT> const &formal_args, 
        ContainerT const &definition,
        TokenT const &macrocall, std::vector<ContainerT> const &arguments,
        IteratorT const& seqstart, IteratorT const& seqend) 
    {
        if (!enabled_macro_tracing()) 
            return false;
#endif
        if (0 == get_level()) {
        // output header line
        BOOST_WAVE_OSSTREAM stream;

            stream 
                << macrocall.get_position() << ": "
                << macrocall.get_value() << "(";

        // argument list
            for (typename ContainerT::size_type i = 0; i < arguments.size(); ++i) {
                stream << boost::wave::util::impl::as_string(arguments[i]);
                if (i < arguments.size()-1)
                    stream << ", ";
            }
            stream << ")" << std::endl; 
            output(BOOST_WAVE_GETSTRING(stream));
            increment_level();
        }        
        
    // output definition reference
        {
        BOOST_WAVE_OSSTREAM stream;

            stream 
                << macrodef.get_position() << ": see macro definition: "
                << macrodef.get_value() << "(";

        // formal argument list
            for (typename std::vector<TokenT>::size_type i = 0; 
                i < formal_args.size(); ++i) 
            {
                stream << formal_args[i].get_value();
                if (i < formal_args.size()-1)
                    stream << ", ";
            }
            stream << ")" << std::endl; 
            output(BOOST_WAVE_GETSTRING(stream));
        }

        if (formal_args.size() > 0) {
        // map formal and real arguments
            open_trace_body("invoked with\n");
            for (typename std::vector<TokenT>::size_type j = 0; 
                j < formal_args.size(); ++j) 
            {
                using namespace boost::wave;

                BOOST_WAVE_OSSTREAM stream;
                stream << formal_args[j].get_value() << " = ";
#if BOOST_WAVE_SUPPORT_VARIADICS_PLACEMARKERS != 0
                if (T_ELLIPSIS == token_id(formal_args[j])) {
                // ellipsis
                    for (typename ContainerT::size_type k = j; 
                        k < arguments.size(); ++k) 
                    {
                        stream << boost::wave::util::impl::as_string(arguments[k]);
                        if (k < arguments.size()-1)
                            stream << ", ";
                    }
                } 
                else 
#endif
                {
                    stream << boost::wave::util::impl::as_string(arguments[j]);
                }
                stream << std::endl;
                output(BOOST_WAVE_GETSTRING(stream));
            }
            close_trace_body();
        }
        open_trace_body();
        
#if BOOST_WAVE_USE_DEPRECIATED_PREPROCESSING_HOOKS == 0
        return false;
#endif
    }

    ///////////////////////////////////////////////////////////////////////////
    //  
    //  The function 'expanding_object_like_macro' is called whenever a 
    //  object-like macro is to be expanded .
    //
    //  The parameter 'ctx' is a reference to the context object used for 
    //  instantiating the preprocessing iterators by the user.
    //
    //  The parameter 'macrodef' marks the position, where the macro to expand 
    //  is defined.
    //
    //  The definition 'definition' holds the macro definition for the macro to 
    //  trace.
    //
    //  The parameter 'macrocall' marks the position, where this macro invoked.
    //
    ///////////////////////////////////////////////////////////////////////////
#if BOOST_WAVE_USE_DEPRECIATED_PREPROCESSING_HOOKS != 0
    // old signature
    template <typename ContainerT>
    void expanding_object_like_macro(TokenT const &macrodef, 
        ContainerT const &definition, TokenT const &macrocall)
    {
        if (!enabled_macro_tracing()) 
            return;
#else
    // new signature
    template <typename ContextT, typename ContainerT>
    bool 
    expanding_object_like_macro(ContextT const& ctx,
        TokenT const &macrodef, ContainerT const &definition, 
        TokenT const &macrocall)
    {
        if (!enabled_macro_tracing()) 
            return false;
#endif
        if (0 == get_level()) {
        // output header line
        BOOST_WAVE_OSSTREAM stream;

            stream 
                << macrocall.get_position() << ": "
                << macrocall.get_value() << std::endl;
            output(BOOST_WAVE_GETSTRING(stream));
            increment_level();
        }
        
    // output definition reference
        {
        BOOST_WAVE_OSSTREAM stream;

            stream 
                << macrodef.get_position() << ": see macro definition: "
                << macrodef.get_value() << std::endl;
            output(BOOST_WAVE_GETSTRING(stream));
        }
        open_trace_body();

#if BOOST_WAVE_USE_DEPRECIATED_PREPROCESSING_HOOKS == 0
        return false;
#endif
    }
    
    ///////////////////////////////////////////////////////////////////////////
    //  
    //  The function 'expanded_macro' is called whenever the expansion of a 
    //  macro is finished but before the rescanning process starts.
    //
    //  The parameter 'ctx' is a reference to the context object used for 
    //  instantiating the preprocessing iterators by the user.
    //
    //  The parameter 'result' contains the token sequence generated as the 
    //  result of the macro expansion.
    //
    ///////////////////////////////////////////////////////////////////////////
#if BOOST_WAVE_USE_DEPRECIATED_PREPROCESSING_HOOKS != 0
    // old signature
    template <typename ContainerT>
    void expanded_macro(ContainerT const &result)
#else
    // new signature
    template <typename ContextT, typename ContainerT>
    void expanded_macro(ContextT const& ctx,ContainerT const &result)
#endif
    {
        if (!enabled_macro_tracing()) return;
        
        BOOST_WAVE_OSSTREAM stream;
        stream << boost::wave::util::impl::as_string(result) << std::endl;
        output(BOOST_WAVE_GETSTRING(stream));

        open_trace_body("rescanning\n");
    }

    ///////////////////////////////////////////////////////////////////////////
    //  
    //  The function 'rescanned_macro' is called whenever the rescanning of a 
    //  macro is finished.
    //
    //  The parameter 'ctx' is a reference to the context object used for 
    //  instantiating the preprocessing iterators by the user.
    //
    //  The parameter 'result' contains the token sequence generated as the 
    //  result of the rescanning.
    //
    ///////////////////////////////////////////////////////////////////////////
#if BOOST_WAVE_USE_DEPRECIATED_PREPROCESSING_HOOKS != 0
    // old signature
    template <typename ContainerT>
    void rescanned_macro(ContainerT const &result)
#else
    // new signature
    template <typename ContextT, typename ContainerT>
    void rescanned_macro(ContextT const& ctx,ContainerT const &result)
#endif
    {
        if (!enabled_macro_tracing() || get_level() == 0) 
            return;

        BOOST_WAVE_OSSTREAM stream;
        stream << boost::wave::util::impl::as_string(result) << std::endl;
        output(BOOST_WAVE_GETSTRING(stream));
        close_trace_body();
        close_trace_body();
        
        if (1 == get_level())
            decrement_level();
    }

    ///////////////////////////////////////////////////////////////////////////
    //  
    //  The function 'interpret_pragma' is called whenever a #pragma command 
    //  directive is found which isn't known to the core Wave library, where
    //  command is the value defined as the BOOST_WAVE_PRAGMA_KEYWORD constant
    //  which defaults to "wave".
    //
    //  The parameter 'ctx' is a reference to the context object used for 
    //  instantiating the preprocessing iterators by the user.
    //
    //  The parameter 'pending' may be used to push tokens back into the input 
    //  stream, which are to be used as the replacement text for the whole 
    //  #pragma directive.
    //
    //  The parameter 'option' contains the name of the interpreted pragma.
    //
    //  The parameter 'values' holds the values of the parameter provided to 
    //  the pragma operator.
    //
    //  The parameter 'act_token' contains the actual #pragma token, which may 
    //  be used for error output.
    //
    //  If the return value is 'false', the whole #pragma directive is 
    //  interpreted as unknown and a corresponding error message is issued. A
    //  return value of 'true' signs a successful interpretation of the given 
    //  #pragma.
    //
    ///////////////////////////////////////////////////////////////////////////
    template <typename ContextT, typename ContainerT>
    bool 
    interpret_pragma(ContextT &ctx, ContainerT &pending, 
        typename ContextT::token_type const &option, ContainerT const &valuetokens, 
        typename ContextT::token_type const &act_token)
    {
        typedef typename ContextT::token_type token_type;

        ContainerT values(valuetokens);
        boost::wave::util::impl::trim_sequence(values);    // trim whitespace

        if (option.get_value() == "timer") {
        // #pragma wave timer(value)
            if (0 == values.size()) {
            // no value means '1'
                using namespace boost::wave;
                timer(token_type(T_INTLIT, "1", act_token.get_position()));
            }
            else {
                timer(values.front());
            }
            return true;
        }
        if (option.get_value() == "trace") {
        // enable/disable tracing option
            return interpret_pragma_trace(ctx, values, act_token);
        }
        if (option.get_value() == "system") {
            if (!enable_system_command) {
            // if the #pragma wave system() directive is not enabled, throw
            // a corresponding error (actually its a remark),
                BOOST_WAVE_THROW_CTX(ctx, bad_pragma_exception, 
                    pragma_system_not_enabled,
                    boost::wave::util::impl::as_string(values).c_str(), 
                    act_token.get_position());
                return false;
            }
            
        // try to spawn the given argument as a system command and return the
        // std::cout of this process as the replacement of this _Pragma
            return interpret_pragma_system(ctx, pending, values, act_token);
        }
        if (option.get_value() == "stop") {
        // stop the execution and output the argument
            BOOST_WAVE_THROW_CTX(ctx, preprocess_exception, error_directive,
                boost::wave::util::impl::as_string(values).c_str(), 
                act_token.get_position());
            return false;
        }
        if (option.get_value() == "option") {
        // handle different options 
            return interpret_pragma_option(ctx, values, act_token);
        }
        return false;
    }
        
    ///////////////////////////////////////////////////////////////////////////
    //  
    //  The function 'opened_include_file' is called whenever a file referred 
    //  by an #include directive was successfully located and opened.
    //
    //  The parameter 'ctx' is a reference to the context object used for 
    //  instantiating the preprocessing iterators by the user.
    //
    //  The parameter 'filename' contains the file system path of the 
    //  opened file (this is relative to the directory of the currently 
    //  processed file or a absolute path depending on the paths given as the
    //  include search paths).
    //
    //  The include_depth parameter contains the current include file depth.
    //
    //  The is_system_include parameter denotes, whether the given file was 
    //  found as a result of a #include <...> directive.
    //  
    ///////////////////////////////////////////////////////////////////////////
#if BOOST_WAVE_USE_DEPRECIATED_PREPROCESSING_HOOKS != 0
    // old signature
    void 
    opened_include_file(std::string const &relname, std::string const &absname, 
        std::size_t include_depth, bool is_system_include) 
    {
#else
    // new signature
    template <typename ContextT>
    void 
    opened_include_file(ContextT const& ctx, std::string const &relname, 
        std::string const &absname, bool is_system_include) 
    {
        std::size_t include_depth = ctx.get_max_include_nesting_depth();
#endif
        if (enabled_include_tracing()) {
            // print indented filename
            for (std::size_t i = 0; i < include_depth; ++i)
                includestrm << " ";
                
            if (is_system_include)
                includestrm << "<" << relname << "> (" << absname << ")";
            else
                includestrm << "\"" << relname << "\" (" << absname << ")";

            includestrm << std::endl;
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    //
    //  The function 'may_skip_whitespace' will be called by the 
    //  library whenever a token is about to be returned to the calling 
    //  application. 
    //
    //  The parameter 'ctx' is a reference to the context object used for 
    //  instantiating the preprocessing iterators by the user.
    //
    //  The 'token' parameter holds a reference to the current token. The policy 
    //  is free to change this token if needed.
    //
    //  The 'skipped_newline' parameter holds a reference to a boolean value 
    //  which should be set to true by the policy function whenever a newline 
    //  is going to be skipped. 
    //
    //  If the return value is true, the given token is skipped and the 
    //  preprocessing continues to the next token. If the return value is 
    //  false, the given token is returned to the calling application. 
    //
    //  ATTENTION!
    //  Caution has to be used, because by returning true the policy function 
    //  is able to force skipping even significant tokens, not only whitespace. 
    //
    ///////////////////////////////////////////////////////////////////////////
    template <typename ContextT>
    bool may_skip_whitespace(ContextT const &ctx, TokenT &token, 
        bool &skipped_newline)
    {
        return this->base_type::may_skip_whitespace(ctx, token, skipped_newline) ?
            !preserve_whitespace : false;
    }

    ///////////////////////////////////////////////////////////////////////////
    //
    //  The function 'throw_exception' will be called by the library whenever a
    //  preprocessing exception occurs.
    //
    //  The parameter 'ctx' is a reference to the context object used for 
    //  instantiating the preprocessing iterators by the user.
    //
    //  The parameter 'e' is the exception object containing detailed error 
    //  information.
    //
    //  The default behavior is to call the function boost::throw_exception.
    //  
    ///////////////////////////////////////////////////////////////////////////
    template <typename ContextT>
    void
    throw_exception(ContextT const& ctx, boost::wave::preprocess_exception const& e)
    {
#if BOOST_WAVE_SUPPORT_MS_EXTENSIONS != 0
        if (!is_import_directive_error(e))
            boost::throw_exception(e);
#else
        boost::throw_exception(e);
#endif
    }
    using base_type::throw_exception; 
    
protected:
#if BOOST_WAVE_SUPPORT_MS_EXTENSIONS != 0
    ///////////////////////////////////////////////////////////////////////////
    //  Avoid throwing an error from a #import directive
    bool is_import_directive_error(boost::wave::preprocess_exception const& e)
    {
        using namespace boost::wave;
        if (e.get_errorcode() != preprocess_exception::ill_formed_directive)
            return false;
            
        // the error string is formatted as 'severity: error: directive'
        std::string error(e.description());
        std::string::size_type p = error.find_last_of(":");
        return p != std::string::npos && error.substr(p+2) == "import";
    }
#endif

    ///////////////////////////////////////////////////////////////////////////
    //  Interpret the different Wave specific pragma directives/operators
    template <typename ContextT, typename ContainerT>
    bool 
    interpret_pragma_trace(ContextT& ctx, ContainerT const &values, 
        typename ContextT::token_type const &act_token)
    {
        typedef typename ContextT::token_type token_type;
        typedef typename token_type::string_type string_type;

    bool valid_option = false;

        if (1 == values.size()) {
        token_type const &value = values.front();
        
            if (value.get_value() == "enable" ||
                value.get_value() == "on" || 
                value.get_value() == "1") 
            {
            // #pragma wave trace(enable)
                enable_tracing(static_cast<trace_flags>(
                    tracing_enabled() | trace_macros));
                valid_option = true;
            }
            else if (value.get_value() == "disable" ||
                value.get_value() == "off" || 
                value.get_value() == "0") 
            {
            // #pragma wave trace(disable)
                enable_tracing(static_cast<trace_flags>(
                    tracing_enabled() & ~trace_macros));
                valid_option = true;
            }
        }
        if (!valid_option) {
        // unknown option value
        string_type option_str ("trace");

            if (values.size() > 0) {
                option_str += "(";
                option_str += boost::wave::util::impl::as_string(values);
                option_str += ")";
            }
            BOOST_WAVE_THROW_CTX(ctx, preprocess_exception, 
                ill_formed_pragma_option, option_str.c_str(), 
                act_token.get_position());
            return false;
        }
        return true;
    }

    ///////////////////////////////////////////////////////////////////////////
    //  interpret the pragma wave option(preserve: [0|1|2|push|pop]) directive
    template <typename ContextT>
    static bool 
    interpret_pragma_option_preserve_set(int mode, bool &preserve_whitespace, 
        ContextT &ctx)
    {
        switch(mode) {
        case 0:   
            preserve_whitespace = false;
            ctx.set_language(
                enable_preserve_comments(ctx.get_language(), false),
                false);
            break;

        case 2:
            preserve_whitespace = true;
              /* fall through */
        case 1:
            ctx.set_language(
                enable_preserve_comments(ctx.get_language()),
                false);
            break;

        default:
              return false;
        }
        return true;
    }

    template <typename ContextT, typename IteratorT>
    bool 
    interpret_pragma_option_preserve(ContextT &ctx, IteratorT &it,
        IteratorT end, typename ContextT::token_type const &act_token)
    {
        using namespace boost::wave;

        token_id id = util::impl::skip_whitespace(it, end);
        if (T_COLON == id)
            id = util::impl::skip_whitespace(it, end);
        
        // implement push/pop
        if (T_IDENTIFIER == id) {
            if ((*it).get_value() == "push") {
            // push current preserve option onto the internal option stack
                if (preserve_whitespace) {
                    if (need_preserve_comments(ctx.get_language()))
                        preserve_options.push(2);
                    else
                        preserve_options.push(1);
                }
                else {
                    preserve_options.push(0);
                }                    
                return true;
            }
            else if ((*it).get_value() == "pop") {
            // test for mismatched push/pop #pragmas
                if (preserve_options.empty()) {
                    BOOST_WAVE_THROW_CTX(ctx, bad_pragma_exception, 
                        pragma_mismatched_push_pop, "preserve", 
                        act_token.get_position());
                }
                
            // pop output preserve from the internal option stack
                bool result = interpret_pragma_option_preserve_set(
                    preserve_options.top(), preserve_whitespace, ctx);
                line_options.pop();
                return result;
            }
            return false;
        }

        if (T_PP_NUMBER != id) 
            return false;
            
        using namespace std;    // some platforms have atoi in namespace std
        return interpret_pragma_option_preserve_set(
            atoi((*it).get_value().c_str()), preserve_whitespace, ctx);
    }
    
    //  interpret the pragma wave option(line: [0|1|push|pop]) directive
    template <typename ContextT, typename IteratorT>
    bool 
    interpret_pragma_option_line(ContextT &ctx, IteratorT &it,
        IteratorT end, typename ContextT::token_type const &act_token)
    {
        using namespace boost::wave;

        token_id id = util::impl::skip_whitespace(it, end);
        if (T_COLON == id)
            id = util::impl::skip_whitespace(it, end);
        
        // implement push/pop
        if (T_IDENTIFIER == id) {
            if ((*it).get_value() == "push") {
            // push current line option onto the internal option stack
                line_options.push(need_emit_line_directives(ctx.get_language()));
                return true;
            }
            else if ((*it).get_value() == "pop") {
            // test for mismatched push/pop #pragmas
                if (line_options.empty()) {
                    BOOST_WAVE_THROW_CTX(ctx, bad_pragma_exception, 
                        pragma_mismatched_push_pop, "line", 
                        act_token.get_position());
                }
                
            // pop output line from the internal option stack
                ctx.set_language(
                    enable_emit_line_directives(ctx.get_language(), line_options.top()),
                    false);
                line_options.pop();
                return true;
            }
            return false;
        }

        if (T_PP_NUMBER != id) 
            return false;
            
        using namespace std;    // some platforms have atoi in namespace std
        int emit_lines = atoi((*it).get_value().c_str());
        if (0 == emit_lines || 1 == emit_lines) {
            // set the new emit #line directive mode
            ctx.set_language(
                enable_emit_line_directives(ctx.get_language(), emit_lines),
                false);
            return true;
        }
        return false;
    }

    //  interpret the pragma wave option(output: ["filename"|null|default|push|pop]) 
    //  directive
    template <typename ContextT>
    bool 
    interpret_pragma_option_output_open(boost::filesystem::path &fpath, 
        ContextT& ctx, typename ContextT::token_type const &act_token)
    {
        namespace fs = boost::filesystem;
        
        // ensure all directories for this file do exist
        fs::create_directories(boost::wave::util::branch_path(fpath));

        // figure out, whether the file to open was last accessed by us
        std::ios::openmode mode = std::ios::out;
        if (fs::exists(fpath) && fs::last_write_time(fpath) >= started_at)
            mode = (std::ios::openmode)(std::ios::out | std::ios::app);

        // close the current file
        if (outputstrm.is_open())
            outputstrm.close();

        // open the new file
        outputstrm.open(fpath.string().c_str(), mode);
        if (!outputstrm.is_open()) { 
            BOOST_WAVE_THROW_CTX(ctx, preprocess_exception, 
                could_not_open_output_file,
                fpath.string().c_str(), act_token.get_position());
            return false;
        }
        generate_output = true;
        current_outfile = fpath;
        return true;        
    }
    
    bool interpret_pragma_option_output_close(bool generate)
    {
        if (outputstrm.is_open())
            outputstrm.close();
        current_outfile = boost::filesystem::path();
        generate_output = generate;
        return true;
    }

    template <typename ContextT, typename IteratorT>
    bool 
    interpret_pragma_option_output(ContextT &ctx, IteratorT &it,
        IteratorT end, typename ContextT::token_type const &act_token)
    {
        using namespace boost::wave;
        namespace fs = boost::filesystem;
        
        typedef typename ContextT::token_type token_type;
        typedef typename token_type::string_type string_type;

        token_id id = util::impl::skip_whitespace(it, end);
        if (T_COLON == id)
            id = util::impl::skip_whitespace(it, end);
        
        bool result = false;
        if (T_STRINGLIT == id) {
            namespace fs = boost::filesystem;
            
            string_type fname ((*it).get_value());
            fs::path fpath (boost::wave::util::create_path(
                util::impl::unescape_lit(fname.substr(1, fname.size()-2)).c_str()));
            fpath = fs::complete(fpath, ctx.get_current_directory());
            result = interpret_pragma_option_output_open(fpath, ctx, act_token);
        }
        else if (T_IDENTIFIER == id) {
            if ((*it).get_value() == "null") {
            // suppress all output from this point on
                result = interpret_pragma_option_output_close(false);
            }        
            else if ((*it).get_value() == "push") {
            // initialize the current_outfile, if appropriate
                if (output_options.empty() && current_outfile.empty() &&
                    !default_outfile.empty() && default_outfile != "-")
                {
                    current_outfile = fs::complete(default_outfile, 
                        ctx.get_current_directory());
                }

            // push current output option onto the internal option stack
                output_options.push(
                    output_option_type(generate_output, current_outfile));
                result = true;
            }
            else if ((*it).get_value() == "pop") {
            // test for mismatched push/pop #pragmas
                if (output_options.empty()) {
                    BOOST_WAVE_THROW_CTX(ctx, bad_pragma_exception, 
                        pragma_mismatched_push_pop, "output", 
                        act_token.get_position());
                    return false;
                }
                
            // pop output option from the internal option stack
                output_option_type const& opts = output_options.top();
                generate_output = opts.first;
                current_outfile = opts.second;
                if (!current_outfile.empty()) {
                // re-open the last file
                    result = interpret_pragma_option_output_open(current_outfile, 
                        ctx, act_token);
                }
                else {
                // either no output or generate to std::cout
                    result = interpret_pragma_option_output_close(generate_output);
                }
                output_options.pop();
            }
        }
        else if (T_DEFAULT == id) {
        // re-open the default output given on command line
            if (!default_outfile.empty()) {
                if (default_outfile == "-") {
                // the output was suppressed on the command line
                    result = interpret_pragma_option_output_close(false);
                }
                else {
                // there was a file name on the command line
                    fs::path fpath(boost::wave::util::create_path(default_outfile));
                    result = interpret_pragma_option_output_open(fpath, ctx, 
                        act_token);
                }
            }
            else {
            // generate the output to std::cout
                result = interpret_pragma_option_output_close(true);
            }
        }
        return result;      
    }

    ///////////////////////////////////////////////////////////////////////////
    //  interpret the pragma wave option() directives
    template <typename ContextT, typename ContainerT>
    bool 
    interpret_pragma_option(ContextT &ctx, ContainerT const &values, 
        typename ContextT::token_type const &act_token)
    {
        using namespace boost::wave;
        
        typedef typename ContextT::token_type token_type;
        typedef typename token_type::string_type string_type;
        typedef typename ContainerT::const_iterator const_iterator;
        
        const_iterator end = values.end();
        for (const_iterator it = values.begin(); it != end; /**/) {
        bool valid_option = false;
            
            token_type const &value = *it;
            if (value.get_value() == "preserve") {
            // #pragma wave option(preserve: [0|1|2|push|pop])
                valid_option = interpret_pragma_option_preserve(ctx, it, end, 
                    act_token);
            }
            else if (value.get_value() == "line") {
            // #pragma wave option(line: [0|1|push|pop])
                valid_option = interpret_pragma_option_line(ctx, it, end, 
                    act_token);
            }
            else if (value.get_value() == "output") {
            // #pragma wave option(output: ["filename"|null|default|push|pop])
                valid_option = interpret_pragma_option_output(ctx, it, end, 
                    act_token);
            }

            if (!valid_option) {
            // unknown option value
            string_type option_str ("option");

                if (values.size() > 0) {
                    option_str += "(";
                    option_str += util::impl::as_string(values);
                    option_str += ")";
                }
                BOOST_WAVE_THROW_CTX(ctx, preprocess_exception, 
                    ill_formed_pragma_option,
                    option_str.c_str(), act_token.get_position());
                return false;
            }
            
            token_id id = util::impl::skip_whitespace(it, end);
            if (id == T_COMMA)
                util::impl::skip_whitespace(it, end);
        }
        return true;
    }

    ///////////////////////////////////////////////////////////////////////////
    // interpret the #pragma wave system() directive
    template <typename ContextT, typename ContainerT>
    bool
    interpret_pragma_system(ContextT& ctx, ContainerT &pending, 
        ContainerT const &values, 
        typename ContextT::token_type const &act_token)
    {
        typedef typename ContextT::token_type token_type;
        typedef typename token_type::string_type string_type;

        if (0 == values.size()) return false;   // ill_formed_pragma_option
        
    string_type stdout_file(std::tmpnam(0));
    string_type stderr_file(std::tmpnam(0));
    string_type system_str(boost::wave::util::impl::as_string(values));
    string_type native_cmd(system_str);

        system_str += " >" + stdout_file + " 2>" + stderr_file;
        if (0 != std::system(system_str.c_str())) {
        // unable to spawn the command
        string_type error_str("unable to spawn command: ");
        
            error_str += native_cmd;
            BOOST_WAVE_THROW_CTX(ctx, preprocess_exception, 
                ill_formed_pragma_option,
                error_str.c_str(), act_token.get_position());
            return false;
        }
        
    // rescan the content of the stdout_file and insert it as the 
    // _Pragma replacement
        typedef typename ContextT::lexer_type lexer_type;
        typedef typename ContextT::input_policy_type input_policy_type;
        typedef boost::wave::iteration_context<
                ContextT, lexer_type, input_policy_type> 
            iteration_context_type;

    iteration_context_type iter_ctx(ctx, stdout_file.c_str(), 
        act_token.get_position(), ctx.get_language());
    ContainerT pragma;

        for (/**/; iter_ctx.first != iter_ctx.last; ++iter_ctx.first) 
            pragma.push_back(*iter_ctx.first);

    // prepend the newly generated token sequence to the 'pending' container
        pending.splice(pending.begin(), pragma);

    // erase the created tempfiles
        std::remove(stdout_file.c_str());
        std::remove(stderr_file.c_str());
        return true;
    }

    ///////////////////////////////////////////////////////////////////////////
    //  The function enable_tracing is called, whenever the status of the 
    //  tracing was changed.
    //  The parameter 'enable' is to be used as the new tracing status.
    void enable_tracing(trace_flags flags) 
        { logging_flags = flags; }

    //  The function tracing_enabled should return the current tracing status.
    trace_flags tracing_enabled() 
        { return logging_flags; }

    //  Helper functions for generating the trace output
    void open_trace_body(char const *label = 0)
    {
        if (label)
            output(label);
        output("[\n");
        increment_level();
    }
    void close_trace_body()
    {
        if (get_level() > 0) {
            decrement_level();
            output("]\n");
            tracestrm << std::flush;      // flush the stream buffer
        }
    }

    template <typename StringT>
    void output(StringT const &outstr) const
    {
        indent(get_level());
        tracestrm << outstr;          // output the given string
    }

    void indent(int level) const
    {
        for (int i = 0; i < level; ++i)
            tracestrm << "  ";        // indent
    }

    int increment_level() { return ++level; }
    int decrement_level() { BOOST_ASSERT(level > 0); return --level; }
    int get_level() const { return level; }
    
    bool enabled_macro_tracing() const 
    { 
        return (flags & trace_macros) && (logging_flags & trace_macros); 
    }
    bool enabled_include_tracing() const 
    { 
        return (flags & trace_includes); 
    }
    
    void timer(TokenT const &value)
    {
        if (value.get_value() == "0" || value.get_value() == "restart") {
        // restart the timer
            elapsed_time.restart();
        }
        else if (value.get_value() == "1") {
        // print out the current elapsed time
            std::cerr 
                << value.get_position() << ": " 
                << elapsed_time.format_elapsed_time()
                << std::endl;
        }
        else if (value.get_value() == "suspend") {
        // suspend the timer
            elapsed_time.suspend();
        }
        else if (value.get_value() == "resume") {
        // resume the timer
            elapsed_time.resume();
        }
    }

private:
    std::ofstream &outputstrm;      // main output stream
    std::ostream &tracestrm;        // trace output stream
    std::ostream &includestrm;      // included list output stream
    int level;                      // indentation level
    trace_flags flags;              // enabled globally
    trace_flags logging_flags;      // enabled by a #pragma
    bool enable_system_command;     // enable #pragma wave system() command
    bool preserve_whitespace;       // enable whitespace preservation
    bool& generate_output;          // allow generated tokens to be streamed to output
    std::string const& default_outfile;         // name of the output file given on command line
    boost::filesystem::path current_outfile;    // name of the current output file 
    
    stop_watch elapsed_time;        // trace timings
    std::time_t started_at;         // time, this process was started at
    
    typedef std::pair<bool, boost::filesystem::path> output_option_type;
    std::stack<output_option_type> output_options;  // output option stack
    std::stack<int> line_options;       // line option stack
    std::stack<int> preserve_options;   // preserve option stack
};

#undef BOOST_WAVE_GETSTRING
#undef BOOST_WAVE_OSSTREAM

#endif // !defined(TRACE_MACRO_EXPANSION_HPP_D8469318_8407_4B9D_A19F_13CA60C1661F_INCLUDED)
