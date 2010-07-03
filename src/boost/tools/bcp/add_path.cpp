/*
 *
 * Copyright (c) 2003 Dr John Maddock
 * Use, modification and distribution is subject to the 
 * Boost Software License, Version 1.0. (See accompanying file 
 * LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 *
 * This file implements the following:
 *    void bcp_implementation::add_path(const fs::path& p)
 *    void bcp_implementation::add_directory(const fs::path& p)
 *    void bcp_implementation::add_file(const fs::path& p)
 *    void bcp_implementation::add_dependent_lib(const std::string& libname)
 */

#include "bcp_imp.hpp"
#include "fileview.hpp"
#include <boost/regex.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/exception.hpp>
#include <iostream>


void bcp_implementation::add_path(const fs::path& p)
{
   fs::path normalized_path = p;
   normalized_path.normalize();
   if(fs::exists(m_boost_path / normalized_path))
   {
      if(fs::is_directory(m_boost_path / normalized_path))
         add_directory(normalized_path);
      else
         add_file(normalized_path);
   }
   else
   {
      std::cerr << "CAUTION: dependent file " << p.string() << " does not exist." << std::endl;
   }
}

void bcp_implementation::add_directory(const fs::path& p)
{
   //
   // Don't add files created by build system:
   //
   if((p.leaf() == "bin") || (p.leaf() == "bin-stage"))
      return; 
   //
   // Don't add version control directories:
   //
   if((p.leaf() == "CVS") || (p.leaf() == ".svn"))
      return; 
   //
   // don't add directories not under version control:
   //
   if(m_cvs_mode && !fs::exists(m_boost_path / p / "CVS/Entries"))
      return;
   if(m_svn_mode && !fs::exists(m_boost_path / p / ".svn/entries"))
      return;
   //
   // enermerate files and directories:
   //
   fs::directory_iterator i(m_boost_path / p);
   fs::directory_iterator j;
   while(i != j)
   {
      //
      // we need to convert *i back into
      // a relative path, what follows is a hack:
      //
      std::string s(i->string());
      if(m_boost_path.string().size())
         s.erase(0, m_boost_path.string().size() + 1);
      if(!m_dependencies.count(fs::path(s))) 
         m_dependencies[fs::path(s)] = p; // set up dependency tree
      add_path(fs::path(s));
      ++i;
   }
}

void bcp_implementation::add_file(const fs::path& p)
{
   //
   // if the file does not exist in cvs then don't do anything with it:
   //
   if((m_cvs_mode || m_svn_mode) && (m_cvs_paths.find(p) == m_cvs_paths.end()))
      return;
   //
   // if we've already seen the file return:
   //
   if(m_copy_paths.find(p) != m_copy_paths.end())
      return;
   //
   // add the file to our list:
   //
   m_copy_paths.insert(p);
   //
   // if this is a source file, scan for dependencies:
   //
   if(is_source_file(p))
   {
      add_file_dependencies(p, false);
   }
   //
   // if this is a html file, scan for dependencies:
   //
   if(is_html_file(p))
   {
      static const boost::regex e(
         "<(?:img[^>]*src=|link[^>]*href=)(\"[^\"]+\"|\\w+)[^>]*>"
         );

      fileview view(m_boost_path / p);
      boost::regex_token_iterator<const char*> i(view.begin(), view.end(), e, 1);
      boost::regex_token_iterator<const char*> j;
      while(i != j)
      {
         //
         // extract the dependent name:
         //
         std::string s(*i);
         if(s[0] == '\"')
         {
            // remove quotes:
            assert(s.size() > 2);
            s.erase(0, 1);
            s.erase(s.size() - 1);
         }
         //
         // if the name starts with ./ remove it
         // or we'll get an error:
         if(s.compare(0, 2, "./") == 0)
            s.erase(0, 2);
         if(s.find(':') == std::string::npos)
         {
            // only concatonate if it's a relative path
            // rather than a URL:
            fs::path dep(p.branch_path() / s);
            if(!m_dependencies.count(dep)) 
               m_dependencies[dep] = p; // set up dependency tree
            add_path(dep);
         }
         ++i;
      }
   }
   //
   // now scan for "special" dependencies:
   // anything that we can't automatically detect...
   //
static const std::pair<fs::path, fs::path>
   specials[] = {
      std::pair<fs::path, fs::path>("boost/config.hpp", "boost/config"),
      std::pair<fs::path, fs::path>("tools/build/allyourbase.jam", "Jamrules"),
      std::pair<fs::path, fs::path>("tools/build/allyourbase.jam", "project-root.jam"),
      std::pair<fs::path, fs::path>("tools/build/allyourbase.jam", "boost-build.jam"),
      std::pair<fs::path, fs::path>("tools/build/v1/allyourbase.jam", "Jamrules"),
      std::pair<fs::path, fs::path>("tools/build/v1/allyourbase.jam", "project-root.jam"),
      std::pair<fs::path, fs::path>("tools/build/v1/allyourbase.jam", "boost-build.jam"),
      std::pair<fs::path, fs::path>("tools/build/v2/boost-build.jam", "Jamrules"),
      std::pair<fs::path, fs::path>("tools/build/v2/boost-build.jam", "project-root.jam"),
      std::pair<fs::path, fs::path>("tools/build/v2/boost-build.jam", "boost-build.jam"),
      std::pair<fs::path, fs::path>("tools/build/v2/boost-build.jam", "Jamfile.v2"),
      std::pair<fs::path, fs::path>("boost/preprocessor/iterate.hpp", "boost/preprocessor/iteration"),
      std::pair<fs::path, fs::path>("boost/preprocessor/slot/slot.hpp", "boost/preprocessor/slot/detail"),
      std::pair<fs::path, fs::path>("boost/function.hpp", "boost/function/detail"),
      std::pair<fs::path, fs::path>("boost/regex/config.hpp", "boost/regex/user.hpp"),
      std::pair<fs::path, fs::path>("boost/signals/signal_template.hpp", "boost/function"),
      std::pair<fs::path, fs::path>("boost/mpl/list.hpp", "boost/mpl/list"),
      std::pair<fs::path, fs::path>("boost/mpl/list_c.hpp", "boost/mpl/list"),
      std::pair<fs::path, fs::path>("boost/mpl/vector.hpp", "boost/mpl/vector"),
      std::pair<fs::path, fs::path>("boost/mpl/deque.hpp", "boost/mpl/vector"),
      std::pair<fs::path, fs::path>("boost/mpl/vector_c.hpp", "boost/mpl/vector"),
      std::pair<fs::path, fs::path>("boost/mpl/map.hpp", "boost/mpl/map"),
      std::pair<fs::path, fs::path>("boost/mpl/set.hpp", "boost/mpl/set"),
      std::pair<fs::path, fs::path>("boost/mpl/set_c.hpp", "boost/mpl/set"),
      std::pair<fs::path, fs::path>("boost/mpl/aux_/include_preprocessed.hpp", "boost/mpl/aux_/preprocessed"),
      std::pair<fs::path, fs::path>("boost/mpl/vector/aux_/include_preprocessed.hpp", "boost/mpl/vector/aux_/preprocessed"),
      std::pair<fs::path, fs::path>("boost/mpl/set/aux_/include_preprocessed.hpp", "boost/mpl/set/aux_/preprocessed"),
      std::pair<fs::path, fs::path>("boost/mpl/map/aux_/include_preprocessed.hpp", "boost/mpl/map/aux_/preprocessed"),
      std::pair<fs::path, fs::path>("boost/mpl/list/aux_/include_preprocessed.hpp", "boost/mpl/list/aux_/preprocessed"),
      std::pair<fs::path, fs::path>("libs/graph/src/python/visitor.hpp", "libs/graph/src/python"),
   };

   for(unsigned int n = 0; n < (sizeof(specials)/sizeof(specials[0])); ++n)
   {
      if(0 == compare_paths(specials[n].first, p))
      {
         if(!m_dependencies.count(specials[n].second)) 
            m_dependencies[specials[n].second] = p; // set up dependency tree
         add_path(specials[n].second);
      }
   }

}

void bcp_implementation::add_dependent_lib(const std::string& libname, const fs::path& p)
{
   //
   // if the boost library libname has source associated with it
   // then add the source to our list:
   //
   if(fs::exists(m_boost_path / "libs" / libname / "src"))
   {
      if(!m_dependencies.count(fs::path("libs") / libname / "src")) 
         m_dependencies[fs::path("libs") / libname / "src"] = p; // set up dependency tree
      add_path(fs::path("libs") / libname / "src");
      if(fs::exists(m_boost_path / "libs" / libname / "build"))
      {
         if(!m_dependencies.count(fs::path("libs") / libname / "build")) 
            m_dependencies[fs::path("libs") / libname / "build"] = p; // set up dependency tree
         add_path(fs::path("libs") / libname / "build");
      }
   }
}

void bcp_implementation::add_file_dependencies(const fs::path& p, bool scanfile)
{
   static const boost::regex e(
      "^[[:blank:]]*(?://@bcp[[:blank:]]+([^\\n]*)\n)?#[[:blank:]]*include[[:blank:]]*[\"<]([^\">]+)[\">]"
      );

   if(!m_dependencies.count(p)) 
      m_dependencies[p] = p; // set terminal dependency

   fileview view;
   if(scanfile)
      view.open(p);
   else
      view.open(m_boost_path / p);
   if(m_license_mode && !scanfile)
      scan_license(p, view);
   const int subs[] = { 1, 2 };
   boost::regex_token_iterator<const char*> i(view.begin(), view.end(), e, subs);
   boost::regex_token_iterator<const char*> j;
   while(i != j)
   {
      //
      // *i contains the name of the include file,
      // check first for a file of that name in the
      // same directory as the file we are scanning,
      // and if that fails, then check the boost-root:
      //
      fs::path include_file;
      try{
         std::string discart_message = *i;
         ++i;
         if(discart_message.size())
         {
            // The include is optional and should be discarded:
            std::cout << "Optional functionality won't be copied: " << discart_message << std::endl;
            std::cout << "Add the file " << *i << " to the list of dependencies to extract to copy this functionality." << std::endl;
            ++i;
            continue;
         }
         include_file = i->str();
      }
      catch(const fs::filesystem_error&)
      {
         std::cerr << "Can't parse filename " << *i << " included by file " << p.string() << std::endl;
         ++i;
         continue;
      }
      fs::path test_file(m_boost_path / p.branch_path() / include_file);
      if(fs::exists(test_file) && !fs::is_directory(test_file) && (p.branch_path().string() != "boost"))
      {
         if(!m_dependencies.count(p.branch_path() / include_file)) 
            m_dependencies[p.branch_path() / include_file] = p;
         add_path(p.branch_path() / include_file);
      }
      else if(fs::exists(m_boost_path / include_file))
      {
         if(!m_dependencies.count(include_file)) 
            m_dependencies[include_file] = p;
         add_path(include_file);
      }
      ++i;
   }
   //
   // Now we need to scan for Boost.Preprocessor includes that
   // are included via preprocessor iteration:
   //
   boost::regex ppfiles("^[[:blank:]]*#[[:blank:]]*define[[:blank:]]+(?:BOOST_PP_FILENAME|BOOST_PP_ITERATION_PARAMS|BOOST_PP_INDIRECT_SELF)[^\\n]+?[\"<]([^\">]+)[\">]");
   i = boost::regex_token_iterator<const char*>(view.begin(), view.end(), ppfiles, 1);
   while(i != j)
   {
      //
      // *i contains the name of the include file,
      // check first for a file of that name in the
      // same directory as the file we are scanning,
      // and if that fails, then check the boost-root:
      //
      fs::path include_file;
      try{
         include_file = i->str();
      }
      catch(const fs::filesystem_error&)
      {
         std::cerr << "Can't parse filename " << *i << " included by file " << p.string() << std::endl;
         ++i;
         continue;
      }
      fs::path test_file(m_boost_path / p.branch_path() / include_file);
      if(fs::exists(test_file) && !fs::is_directory(test_file) && (p.branch_path().string() != "boost"))
      {
         if(!m_dependencies.count(p.branch_path() / include_file)) 
            m_dependencies[p.branch_path() / include_file] = p;
         add_path(p.branch_path() / include_file);
      }
      else if(fs::exists(m_boost_path / include_file))
      {
         if(!m_dependencies.count(include_file)) 
            m_dependencies[include_file] = p;
         add_path(include_file);
      }
      else
      {
         std::cerr << "CAUTION: Boost.Preprocessor iterated file " << include_file.string() << " does not exist." << std::endl;
      }
      ++i;
   }

   //
   // Scan for any #include MACRO includes that we don't recognise.
   //
   // Begin by declaring all of the macros that get #included that 
   // we know about and are correctly handled as special cases:
   //
   static const std::string known_macros[] = {
      "BOOST_USER_CONFIG",
      "BOOST_COMPILER_CONFIG",
      "BOOST_STDLIB_CONFIG",
      "BOOST_PLATFORM_CONFIG",
      "BOOST_PP_FILENAME_1",
      "BOOST_PP_ITERATION_PARAMS_1",
      "BOOST_PP_FILENAME_2",
      "BOOST_PP_ITERATION_PARAMS_2",
      "BOOST_PP_FILENAME_3",
      "BOOST_PP_ITERATION_PARAMS_3",
      "BOOST_PP_FILENAME_4",
      "BOOST_PP_ITERATION_PARAMS_4",
      "BOOST_PP_FILENAME_5",
      "BOOST_PP_ITERATION_PARAMS_5",
      "BOOST_PP_INDIRECT_SELF",
      "BOOST_PP_INCLUDE_SELF()",
      "BOOST_PP_ITERATE",
      "BOOST_PP_LOCAL_ITERATE",
      "BOOST_PP_ITERATE()",
      "BOOST_PP_LOCAL_ITERATE()",
      "BOOST_PP_ASSIGN_SLOT(1)",
      "BOOST_PP_ASSIGN_SLOT(2)",
      "BOOST_PP_ASSIGN_SLOT(3)",
      "BOOST_PP_ASSIGN_SLOT(4)",
      "BOOST_PP_ASSIGN_SLOT(5)",
      "BOOST_ABI_PREFIX",
      "BOOST_ABI_SUFFIX",
      "BOOST_PP_STRINGIZE(boost/mpl/aux_/preprocessed/AUX_PREPROCESSED_HEADER)",
      "BOOST_PP_STRINGIZE(boost/mpl/list/AUX778076_HEADER)",
      "BOOST_PP_STRINGIZE(boost/mpl/list/AUX778076_LIST_C_HEADER)",
      "BOOST_PP_STRINGIZE(boost/mpl/list/AUX778076_LIST_HEADER)",
      "BOOST_PP_STRINGIZE(boost/mpl/map/aux_/preprocessed/AUX778076_HEADER)",
      "BOOST_PP_STRINGIZE(boost/mpl/map/AUX778076_MAP_HEADER)",
      "BOOST_PP_STRINGIZE(boost/mpl/set/aux_/preprocessed/AUX778076_HEADER)",
      "BOOST_PP_STRINGIZE(boost/mpl/set/AUX778076_SET_HEADER)",
      "BOOST_PP_STRINGIZE(boost/mpl/set/AUX778076_SET_C_HEADER)",
      "BOOST_PP_STRINGIZE(boost/mpl/vector/AUX778076_VECTOR_HEADER)",
      "BOOST_PP_STRINGIZE(boost/mpl/vector/aux_/preprocessed/AUX778076_HEADER)",
      "BOOST_PP_STRINGIZE(boost/mpl/vector/AUX778076_DEQUE_HEADER)",
      "BOOST_PP_STRINGIZE(boost/mpl/vector/AUX778076_VECTOR_C_HEADER)",
      "BOOST_REGEX_USER_CONFIG",
      "BGL_PYTHON_EVENTS_HEADER",
   };

   boost::regex indirect_includes("^[[:blank:]]*#[[:blank:]]*include[[:blank:]]+([^\"<][^\n]*?)[[:blank:]]*$");
   i = boost::regex_token_iterator<const char*>(view.begin(), view.end(), indirect_includes, 1);
   while(i != j)
   {
      const std::string* known_macros_end = known_macros + sizeof(known_macros)/sizeof(known_macros[0]);
      if(known_macros_end == std::find(known_macros, known_macros_end, i->str()))
      {
         std::cerr << "CAUTION: don't know how to trace depenencies through macro: " << *i << " in file: " << p.string() << std::endl;
      }
      ++i;
   }
   //
   // if the file contains a cpp_main / unit_test_main / test_main
   // it is dependent upon Boost.test even if it doesn't
   // include any of the Boost.test headers directly.
   //
   static const boost::regex m("^\\s*int\\s+(?:cpp_main|test_main|unit_test_main)");
   boost::cmatch what;
   if(boost::regex_search(view.begin(), view.end(), what, m))
   {
      add_dependent_lib("test", p);
   }
   //
   // grab the name of the library to which the header belongs, 
   // and if that library has source then add the source to our
   // list:
   //
   // this regex catches boost/libname.hpp or boost/libname/whatever:
   //
   static const boost::regex lib1("boost/([^\\./]+)(?!detail).*");
   boost::smatch swhat;
   if(boost::regex_match(p.string(), swhat, lib1))
   {
      add_dependent_lib(swhat.str(1), p);
   }
   //
   // and this one catches boost/x/y/whatever (for example numeric/ublas):
   //
   static const boost::regex lib2("boost/([^/]+/[^/]+)/(?!detail).*");
   if(boost::regex_match(p.string(), swhat, lib2))
   {
      add_dependent_lib(swhat.str(1), p);
   }
}
