//////////////////////////////////////////////////////////////////////////////
//
// (C) Copyright Ion Gaztanaga 2007. Distributed under the Boost
// Software License, Version 1.0. (See accompanying file
// LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
// See http://www.boost.org/libs/interprocess for documentation.
//
//////////////////////////////////////////////////////////////////////////////

#include <boost/interprocess/detail/config_begin.hpp>
#include <boost/interprocess/offset_ptr.hpp>
#include <boost/interprocess/detail/type_traits.hpp>

using namespace boost::interprocess;

bool test_types_and_convertions()
{
   typedef offset_ptr<int>                pint_t;
   typedef offset_ptr<const int>          pcint_t;
   typedef offset_ptr<volatile int>       pvint_t;
   typedef offset_ptr<const volatile int> pcvint_t;

   if(!detail::is_same<pint_t::value_type, int>::value)
      return false;
   if(!detail::is_same<pcint_t::value_type, const int>::value)
      return false;
   if(!detail::is_same<pvint_t::value_type, volatile int>::value)
      return false;
   if(!detail::is_same<pcvint_t::value_type, const volatile int>::value)
      return false;
   int dummy_int = 9;

   {  pint_t pint(&dummy_int);   pcint_t  pcint(pint);
      if(pcint.get()  != &dummy_int)   return false;  }
   {  pint_t pint(&dummy_int);   pvint_t  pvint(pint);
      if(pvint.get()  != &dummy_int)   return false;  }
   {  pint_t pint(&dummy_int);   pcvint_t  pcvint(pint);
      if(pcvint.get()  != &dummy_int)  return false;  }
   {  pcint_t pcint(&dummy_int); pcvint_t  pcvint(pcint);
         if(pcvint.get()  != &dummy_int)  return false;  }
   {  pvint_t pvint(&dummy_int); pcvint_t  pcvint(pvint);
      if(pcvint.get()  != &dummy_int)  return false;  }

   pint_t   pint(0);
   pcint_t  pcint(0);
   pvint_t  pvint(0);
   pcvint_t pcvint(0);
   
   pint     = &dummy_int;
   pcint    = &dummy_int;
   pvint    = &dummy_int;
   pcvint   = &dummy_int;

   {   pcint  = pint;   if(pcint.get() != &dummy_int)   return false;  }
   {   pvint  = pint;   if(pvint.get() != &dummy_int)   return false;  }
   {   pcvint = pint;   if(pcvint.get() != &dummy_int)  return false;  }
   {   pcvint = pcint;  if(pcvint.get() != &dummy_int)  return false;  }
   {   pcvint = pvint;  if(pcvint.get() != &dummy_int)  return false;  }

   if(!pint)
      return false;

   pint = 0;
   if(pint)
      return false;

   return true;
}

bool test_arithmetic()
{
   typedef offset_ptr<int> pint_t;
   const int NumValues = 5;
   int values[NumValues];
   
   //Initialize p
   pint_t p = values;
   if(p.get() != values)
      return false;

   //Initialize p + NumValues
   pint_t pe = &values[NumValues];
   if(pe == p)
      return false;
   if(pe.get() != &values[NumValues])
      return false;

   //ptr - ptr
   if((pe - p) != NumValues)
      return false;
   //ptr - integer
   if((pe - NumValues) != p)
      return false;
   //ptr + integer
   if((p + NumValues) != pe)
      return false;
   //integer + ptr
   if((NumValues + p) != pe)
      return false;
   //indexing
   if(pint_t(&p[NumValues])   != pe)
      return false;
   if(pint_t(&pe[-NumValues]) != p)
      return false;

   //ptr -= integer
   pint_t p0 = pe;
   p0-= NumValues;
   if(p != p0)
      return false;
   //ptr += integer
   pint_t penew = p0;
   penew += NumValues;
   if(penew != pe)
      return false;

   //++ptr
   penew = p0;
   for(int j = 0; j != NumValues; ++j, ++penew);
   if(penew != pe)
      return false;
   //--ptr
   p0 = pe;
   for(int j = 0; j != NumValues; ++j, --p0);
   if(p != p0)
      return false;
   //ptr++
   penew = p0;
   for(int j = 0; j != NumValues; ++j){
      pint_t p = penew;
      if(p != penew++)
         return false;
   }
   //ptr--
   p0 = pe;
   for(int j = 0; j != NumValues; ++j){
      pint_t p = p0;
      if(p != p0--)
         return false;
   }

   return true;
}

bool test_comparison()
{
   typedef offset_ptr<int> pint_t;
   const int NumValues = 5;
   int values[NumValues];

   //Initialize p
   pint_t p = values;
   if(p.get() != values)
      return false;

   //Initialize p + NumValues
   pint_t pe = &values[NumValues];
   if(pe == p)
      return false;
   if(pe.get() != &values[NumValues])
      return false;

   //operators
   if(p == pe)
      return false;
   if(p != p)
      return false;
   if(!(p < pe))
      return false;
   if(!(p <= pe))
      return false;
   if(!(pe > p))
      return false;
   if(!(pe >= p))
      return false;

   return true;
}

int main()
{
   if(!test_types_and_convertions())
      return 1;
   if(!test_arithmetic())
      return 1;
   if(!test_comparison())
      return 1;
   return 0;
}

#include <boost/interprocess/detail/config_end.hpp>
