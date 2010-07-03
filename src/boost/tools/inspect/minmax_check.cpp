//  minmax_check implementation  --------------------------------------------//

//  Copyright Beman Dawes   2002.
//  Copyright Gennaro Prota 2006.
//
//  Distributed under the Boost Software License, Version 1.0.
//  (See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)


#include <algorithm>

#include "minmax_check.hpp"
#include "boost/regex.hpp"
#include "boost/lexical_cast.hpp"

namespace
{
  boost::regex minmax_regex(
    "("
    "^\\s*#\\s*undef\\s*" // # undef
    "\\b(min|max)\\b"     // followed by min or max, whole word
    ")"
    "|"                   // or
    "("
    "\\b(min|max)\\b" // min or max, whole word
    "\\s*\\("         // followed by 0 or more spaces and an opening paren
    ")"
    , boost::regex::normal);

} // unnamed namespace

namespace boost
{
  namespace inspect
  {

    //  minmax_check constructor  -------------------------------------------//

    minmax_check::minmax_check()
      : m_errors(0)
    {
      // C/C++ source code...
      register_signature( ".c" );
      register_signature( ".cpp" );
      register_signature( ".cxx" );
      register_signature( ".h" );
      register_signature( ".hpp" );
      register_signature( ".hxx" );
      register_signature( ".inc" );
      register_signature( ".ipp" );
    }

    //  inspect ( C++ source files )  ---------------------------------------//

    void minmax_check::inspect(
      const string & library_name,
      const path & full_path,      // example: c:/foo/boost/filesystem/path.hpp
      const string & contents)     // contents of file to be inspected
    {
      if (contents.find( "boostinspect:" "nominmax" ) != string::npos) return;

      boost::sregex_iterator cur(contents.begin(), contents.end(), minmax_regex), end;

      for( ; cur != end; ++cur /*, ++m_errors*/ )
      {
        // ~ experimental: try to filter out apparent
        // ~ min/max guideline violations in one-line comments
        //
        // This is just a quick hack to avoid impacting the
        // overall application design. To be on the safe side,
        // we only aim at one-line comments; the comment must be
        // the only non-blank content of the line, and no quote
        // character or "*/" shall appear within it. Otherwise we
        // give up filtering, at the cost of possible false positives.
        //
        const string one_line_comment_line ( "^\\s*//" );

        string::const_iterator it = contents.begin();
        string::const_iterator match_it = (*cur)[0].first;

        string::const_iterator line_start = it;

        string::size_type line_number = 1;
        for ( ; it != match_it; ++it) {
            if (string::traits_type::eq(*it, '\n')) {
                ++line_number;
                line_start = it + 1; // could be end()
            }
        }

        string::const_iterator past_line_content =
            std::find(it, contents.end(), '\n');

        // one-line comment spanning the whole line
        // with no quotes and no "*/" pair
        //
        match_results<string::const_iterator> m;
        const string whole_line( line_start, past_line_content );
        const bool filter_out =
               regex_search( line_start, past_line_content
               , m, boost::regex(one_line_comment_line) )
                 && string::npos == whole_line.find('\"')
                 && string::npos == whole_line.find("*/")
               ;

        if (!filter_out) {

                ++m_errors;
                error( library_name, full_path, string(name())
                    + " violation of Boost min/max guidelines on line "
                    + boost::lexical_cast<string>( line_number ) );
        }

      }
    }

  } // namespace inspect
} // namespace boost

