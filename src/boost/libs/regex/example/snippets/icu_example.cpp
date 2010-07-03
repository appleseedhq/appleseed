/*
 *
 * Copyright (c) 2004
 * John Maddock
 *
 * Use, modification and distribution are subject to the 
 * Boost Software License, Version 1.0. (See accompanying file 
 * LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 *
 */

 /*
  *   LOCATION:    see http://www.boost.org for most recent version.
  *   FILE         mfc_example.cpp
  *   VERSION      see <boost/version.hpp>
  *   DESCRIPTION: examples of using Boost.Regex with MFC and ATL string types.
  */

#include <boost/regex/config.hpp>

#ifdef BOOST_HAS_ICU

#include <boost/regex/icu.hpp>
#include <iostream>
#include <assert.h>

//
// Find out if *password* meets our password requirements,
// as defined by the regular expression *requirements*.
//
bool is_valid_password(const UnicodeString& password, const UnicodeString& requirements)
{
   return boost::u32regex_match(password, boost::make_u32regex(requirements));
}

//
// Extract filename part of a path from a UTF-8 encoded std::string and return the result
// as another std::string:
//
std::string get_filename(const std::string& path)
{
   boost::u32regex r = boost::make_u32regex("(?:\\A|.*\\\\)([^\\\\]+)");
   boost::smatch what;
   if(boost::u32regex_match(path, what, r))
   {
      // extract $1 as a std::string:
      return what.str(1);
   }
   else
   {
      throw std::runtime_error("Invalid pathname");
   }
}

UnicodeString extract_greek(const UnicodeString& text)
{
   // searches through some UTF-16 encoded text for a block encoded in Greek,
   // this expression is imperfect, but the best we can do for now - searching
   // for specific scripts is actually pretty hard to do right.
   boost::u32regex r = boost::make_u32regex(L"[\\x{370}-\\x{3FF}](?:[^[:L*:]]|[\\x{370}-\\x{3FF}])*");
   boost::u16match what;
   if(boost::u32regex_search(text, what, r))
   {
      // extract $0 as a UnicodeString:
      return UnicodeString(what[0].first, what.length(0));
   }
   else
   {
      throw std::runtime_error("No Greek found!");
   }
}

void enumerate_currencies(const std::string& text)
{
   // enumerate and print all the currency symbols, along
   // with any associated numeric values:
   const char* re = 
      "([[:Sc:]][[:Cf:][:Cc:][:Z*:]]*)?"
      "([[:Nd:]]+(?:[[:Po:]][[:Nd:]]+)?)?"
      "(?(1)"
         "|(?(2)"
            "[[:Cf:][:Cc:][:Z*:]]*"
         ")"
         "[[:Sc:]]"
      ")";
   boost::u32regex r = boost::make_u32regex(re);
   boost::u32regex_iterator<std::string::const_iterator> i(boost::make_u32regex_iterator(text, r)), j;
   while(i != j)
   {
      std::cout << (*i)[0] << std::endl;
      ++i;
   }
}

void enumerate_currencies2(const std::string& text)
{
   // enumerate and print all the currency symbols, along
   // with any associated numeric values:
   const char* re = 
      "([[:Sc:]][[:Cf:][:Cc:][:Z*:]]*)?"
      "([[:Nd:]]+(?:[[:Po:]][[:Nd:]]+)?)?"
      "(?(1)"
         "|(?(2)"
            "[[:Cf:][:Cc:][:Z*:]]*"
         ")"
         "[[:Sc:]]"
      ")";
   boost::u32regex r = boost::make_u32regex(re);
   boost::u32regex_token_iterator<std::string::const_iterator> 
      i(boost::make_u32regex_token_iterator(text, r, 1)), j;
   while(i != j)
   {
      std::cout << *i << std::endl;
      ++i;
   }
}


//
// Take a credit card number as a string of digits, 
// and reformat it as a human readable string with "-"
// separating each group of four digit;, 
// note that we're mixing a UTF-32 regex, with a UTF-16
// string and a UTF-8 format specifier, and it still all 
// just works:
//
const boost::u32regex e = boost::make_u32regex("\\A(\\d{3,4})[- ]?(\\d{4})[- ]?(\\d{4})[- ]?(\\d{4})\\z");
const char* human_format = "$1-$2-$3-$4";

UnicodeString human_readable_card_number(const UnicodeString& s)
{
   return boost::u32regex_replace(s, e, human_format);
}


int main()
{
   // password checks using u32regex_match:
   UnicodeString pwd = "abcDEF---";
   UnicodeString pwd_check = "(?=.*[[:lower:]])(?=.*[[:upper:]])(?=.*[[:punct:]]).{6,}";
   bool b = is_valid_password(pwd, pwd_check);
   assert(b);
   pwd = "abcD-";
   b = is_valid_password(pwd, pwd_check);
   assert(!b);
   // filename extraction with u32regex_match:
   std::string file = "abc.hpp";
   file = get_filename(file);
   assert(file == "abc.hpp");
   file = "c:\\a\\b\\c\\d.h";
   file = get_filename(file);
   assert(file == "d.h");

   // Greek text extraction with u32regex_search:
   UnicodeString text = L"Some where in \x0391\x039D\x0395\x0398\x0391 2004";
   UnicodeString greek = extract_greek(text);
   assert(greek == L"\x0391\x039D\x0395\x0398\x0391 2004");

   // extract currency symbols with associated value, use iterator interface:
   std::string text2 = " $100.23 or \xC2\xA3""198.12 "; // \xC2\xA3 is the pound sign encoded in UTF-8
   enumerate_currencies(text2);
   enumerate_currencies2(text2);

   UnicodeString credit_card_number = "1234567887654321";
   credit_card_number = human_readable_card_number(credit_card_number);
   assert(credit_card_number == "1234-5678-8765-4321");
   return 0;
}

#else

#include <iostream>

int main()
{
   std::cout << "<NOTE>ICU support not enabled, feature unavailable</NOTE>";
   return 0;
}


#endif

