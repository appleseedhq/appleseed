//  link_check implementation  -----------------------------------------------//

//  Copyright Beman Dawes 2002.
//
//  Distributed under the Boost Software License, Version 1.0.
//  (See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)

#include "link_check.hpp"
#include "boost/regex.hpp"
#include "boost/filesystem/operations.hpp"
#include <cstdlib>

namespace fs = boost::filesystem;

namespace
{
  boost::regex html_url_regex(
    "<\\s*[^>]*\\s+(?:HREF|SRC)" // HREF or SRC
    "\\s*=\\s*(['\"])(.*?)\\1",
    boost::regbase::normal | boost::regbase::icase);
  boost::regex css_url_regex(
    "(\\@import\\s*[\"']|url\\s*\\(\\s*[\"']?)([^\"')]*)",
    boost::regbase::normal | boost::regbase::icase);

  // Regular expression for parsing URLS from:
  // http://tools.ietf.org/html/rfc3986#appendix-B
  boost::regex url_decompose_regex(
    "^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?$",
    boost::regbase::normal);

  // Decode html escapsed ampersands, returns an empty string if there's an error.
  std::string decode_ampersands(std::string const& url_path) {
    std::string::size_type pos = 0, next;
    std::string result;
    result.reserve(url_path.length());

    while((next = url_path.find('&', pos)) != std::string::npos) {
      result.append(url_path, pos, next - pos);
      pos = next;
      if(url_path.substr(pos, 5) == "&amp;") {
        result += '&'; pos += 5;
      }
      else {
        result += '&'; pos += 1;
      }
      break;
    }

    result.append(url_path, pos, url_path.length());

    return result;
  }

  // Decode percent encoded characters, returns an empty string if there's an error.
  std::string decode_percents(std::string const& url_path) {
    std::string::size_type pos = 0, next;
    std::string result;
    result.reserve(url_path.length());

    while((next = url_path.find('%', pos)) != std::string::npos) {
      result.append(url_path, pos, next - pos);
      pos = next;
      switch(url_path[pos]) {
        case '%': {
          if(url_path.length() - next < 3) return "";
          char hex[3] = { url_path[next + 1], url_path[next + 2], '\0' };
          char* end_ptr;
          result += (char) std::strtol(hex, &end_ptr, 16);
          if(*end_ptr) return "";
          pos = next + 3;
          break;
        }
      }
    }

    result.append(url_path, pos, url_path.length());

    return result;
  }

  bool is_css(const path & p) {
      return p.extension() == ".css";
  }

} // unnamed namespace

namespace boost
{
  namespace inspect
  {

//  link_check constructor  --------------------------------------------------//

   link_check::link_check()
     : m_broken_errors(0), m_unlinked_errors(0), m_invalid_errors(0),
       m_bookmark_errors(0)
   {
       // HTML signatures are already registered by the base class,
       // 'hypertext_inspector' 
       register_signature(".css");
   }

//  inspect (all)  -----------------------------------------------------------//

   void link_check::inspect(
      const string & /*library_name*/,
      const path & full_path )
    {
      // keep track of paths already encountered to reduce disk activity
      if ( !fs::is_directory( full_path ) )
        m_paths[ relative_to( full_path, fs::initial_path() ) ] |= m_present;
    }

//  inspect ( .htm, .html, .shtml, .css )  -----------------------------------//

   void link_check::inspect(
      const string & library_name,
      const path & full_path,   // example: c:/foo/boost/filesystem/path.hpp
      const string & contents )     // contents of file to be inspected
    {
      if (contents.find( "boostinspect:" "nounlinked" ) != string::npos)
          m_paths[ relative_to( full_path, fs::initial_path() ) ] |= m_nounlinked_errors;

      bool no_link_errors =
          (contents.find( "boostinspect:" "nolink" ) != string::npos);

      string::const_iterator start( contents.begin() );
      string::const_iterator end( contents.end() );
      boost::match_results< string::const_iterator > what;
      boost::match_flag_type flags = boost::match_default;

      boost::regex const& url_regex =
          is_css(full_path) ? css_url_regex : html_url_regex;

      while( boost::regex_search( start, end, what, url_regex, flags) )
      {
        // what[0] contains the whole string iterators.
        // what[2] contains the URL iterators.
        do_url( string( what[2].first, what[2].second ),
          library_name, full_path, no_link_errors );

        start = what[0].second; // update search position
        flags |= boost::match_prev_avail; // update flags
        flags |= boost::match_not_bob;
      }
    }

//  do_url  ------------------------------------------------------------------//

    void link_check::do_url( const string & url, const string & library_name,
      const path & source_path, bool no_link_errors )
        // precondition: source_path.is_complete()
    {
      if(!no_link_errors && url.empty()) {
        ++m_invalid_errors;
        error( library_name, source_path, string(name()) + " empty URL." );
        return;
      }

      // Decode ampersand encoded characters.
      string decoded_url = is_css(source_path) ? url : decode_ampersands(url);
      if(decoded_url.empty()) {
        if(!no_link_errors) {
          ++m_invalid_errors;
          error( library_name, source_path, string(name()) + " invalid URL (invalid ampersand encodings): " + url );
        }
        return;
      }
    
      boost::smatch m;
      if(!boost::regex_match(decoded_url, m, url_decompose_regex)) {
        if(!no_link_errors) {
          ++m_invalid_errors;
          error( library_name, source_path, string(name()) + " invalid URL: " + decoded_url );
        }
        return;
      }

      bool scheme_matched = m[2].matched,
        authority_matched = m[4].matched,
        //query_matched = m[7].matched,
        fragment_matched = m[9].matched;

      std::string scheme(m[2]),
        authority(m[4]),
        url_path(m[5]),
        //query(m[7]),
        fragment(m[9]);

      // Protocol checks
      if(scheme_matched) {
        if(scheme == "http" || scheme == "https") {
          // All http links should have a hostname. Generally if they don't
          // it's by mistake. If they shouldn't, then a protocol isn't
          // required.
          if(!authority_matched) {
            if(!no_link_errors) {
              ++m_invalid_errors;
              error( library_name, source_path, string(name()) + " no hostname: " + decoded_url );
            }
          }

          return;
        }
        else if(scheme == "file") {
          if(!no_link_errors) {
            ++m_invalid_errors;
            error( library_name, source_path, string(name()) + " invalid URL (hardwired file): " + decoded_url );
          }
        }
        else if(scheme == "mailto" || scheme == "ftp" || scheme == "news" || scheme == "javascript") {
          if ( !no_link_errors && is_css(source_path) ) {
            ++m_invalid_errors;
            error( library_name, source_path, string(name()) + " invalid protocol for css: " + decoded_url );
          }
        }
        else {
          if(!no_link_errors) {
            ++m_invalid_errors;
            error( library_name, source_path, string(name()) + " unknown protocol: " + decoded_url );
          }
        }

        return;
      }

      // Hostname without protocol.
      if(authority_matched) {
        if(!no_link_errors) {
          ++m_invalid_errors;
          error( library_name, source_path, string(name()) + " invalid URL (hostname without protocol): " + decoded_url );
        }
      }

      // Check the fragment identifier
      if ( fragment_matched ) {
        if ( is_css(source_path) ) {
            if ( !no_link_errors ) {
              ++m_invalid_errors;
              error( library_name, source_path, string(name()) + " fragment link in CSS: " + decoded_url );
            }
        }
        else {
          if ( !no_link_errors && fragment.find( '#' ) != string::npos )
          {
            ++m_bookmark_errors;
            error( library_name, source_path, string(name()) + " invalid bookmark: " + decoded_url );
          }
        }

        // No more to do if it's just a fragment identifier
        if(url_path.empty()) return;
      }

      // Detect characters banned by RFC2396:
      if ( !no_link_errors && decoded_url.find_first_of( " <>\"{}|\\^[]'" ) != string::npos )
      {
        ++m_invalid_errors;
        error( library_name, source_path, string(name()) + " invalid character in URL: " + decoded_url );
      }

      // Check that we actually have a path.
      if(url_path.empty()) {
        if(!no_link_errors) {
          ++m_invalid_errors;
          error( library_name, source_path, string(name()) + " invalid URL (empty path in relative url): " + decoded_url );
        }
      }

      // Decode percent and ampersand encoded characters.
      string decoded_path = decode_percents(url_path);
      if(decoded_path.empty()) {
        if(!no_link_errors) {
          ++m_invalid_errors;
          error( library_name, source_path, string(name()) + " invalid URL (invalid character encodings): " + decoded_url );
        }
        return;
      }

      // strip url of references to current dir
      if ( decoded_path[0]=='.' && decoded_path[1]=='/' ) decoded_path.erase( 0, 2 );

      // url is relative source_path.branch()
      // convert to target_path, which is_complete()
      path target_path;
      try { target_path = source_path.branch_path() /= path( decoded_path, fs::no_check ); }
      catch ( const fs::filesystem_error & )
      {
        if(!no_link_errors) {
          ++m_invalid_errors;
          error( library_name, source_path, string(name()) + " invalid URL (error resolving path): " + decoded_url );
        }
        return;
      }

      // create a m_paths entry if necessary
      std::pair< const string, int > entry(
        relative_to( target_path, fs::initial_path() ), 0 );
      m_path_map::iterator itr( m_paths.find( entry.first ) );
      if ( itr == m_paths.end() )
      {
        if ( fs::exists( target_path ) ) entry.second = m_present;
        itr = m_paths.insert( entry ).first;
      }

      // itr now points to the m_paths entry
      itr->second |= m_linked_to;

      // if target isn't present, the link is broken
      if ( !no_link_errors && (itr->second & m_present) == 0 )
      {
        ++m_broken_errors;
        error( library_name, source_path, string(name()) + " broken link: " + decoded_url );
      }
    }

//  close  -------------------------------------------------------------------//

   void link_check::close()
   {
     for ( m_path_map::const_iterator itr = m_paths.begin();
       itr != m_paths.end(); ++itr )
     {
// std::clog << itr->first << " " << itr->second << "\n";
       if ( (itr->second & m_linked_to) != m_linked_to
         && (itr->second & m_nounlinked_errors) != m_nounlinked_errors
         && (itr->first.rfind( ".html" ) == itr->first.size()-5
          || itr->first.rfind( ".htm" ) == itr->first.size()-4
          || itr->first.rfind( ".css" ) == itr->first.size()-4)
         // because they may be redirectors, it is OK if these are unlinked:
         && itr->first.rfind( "index.html" ) == string::npos
         && itr->first.rfind( "index.htm" ) == string::npos )
       {
         ++m_unlinked_errors;
         path full_path( fs::initial_path() / path(itr->first, fs::no_check) );
         error( impute_library( full_path ), full_path, string(name()) + " unlinked file" );
       }
     }
   }

  } // namespace inspect
} // namespace boost

