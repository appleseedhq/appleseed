/*****************************************************************************/
/*                                                                           */
/*  Header: zorder_type_traits.hpp                                           */
/*                                                                           */
/*  Accompanies STANN Version 0.71 B                                         */
/*  Dec 07, 2009                                                             */
/*                                                                           */
/*  Copyright 2007, 2008                                                     */
/*  Michael Connor and Piyush Kumar                                          */
/*  Florida State University                                                 */
/*  Tallahassee FL, 32306-4532                                               */
/*                                                                           */
/*****************************************************************************/





#ifndef __STANN_ZORDER_TYPE_TRAITS__
#define __STANN_ZORDER_TYPE_TRAITS__

#include <sep_float.hpp>

/*! \file
  \brief Contains type traits used by the zorder_lt class
*/

struct zorder_t {};
struct zorder_f {};

/*! zorder traits class
  \brief Contains traits used by the zorder_lt class

  This is the unspecialzied version of this class,
  it must be specialized for each type that will be used
  for coordinates.
  Note: some of these are deprecated and will be removed

*/
template<typename TYPE>
class zorder_traits
{
public:
  /*! Check Type
    \brief Checks if type is valid
    
    This function checks if the type traits specialization exists.
    Prints an error and exits if this
    type does not have defined traits.  Checked at
    creation of a zorder_lt object
  */
  static void check_type()
  {
    //cerr << "Error: Type traits not defined." << endl;
    //cerr << "Please ensure the appropriate type is added to zorder_traits.hpp" << endl;
    //cerr << "and contact author for update." << endl;
    //exit(1);
  }
  /*! is_signed trait
    \brief set to zorder_t if the type is signed.
  */
  typedef zorder_t     is_signed;
  /*! is_integral trait
    \brief set to zorder_t if the type is integral
  */
  typedef zorder_t     is_integral;
  /*! is_seperated trait
    \brief set to zorder_t if type is an instance of sep_float<>
  */
  typedef zorder_t     is_seperated;
  /*! unsigned_type trait
    \brief Deprecated, should be removed
  */
  typedef int          unsigned_type;
  /*! signed_type trait
    \brief Deprecated, should be removed
  */
  typedef unsigned int signed_type;
};

template<>
class zorder_traits <int>
{
public:
  static void check_type(){};
  typedef zorder_t     is_signed;
  typedef zorder_t     is_integral;
  typedef zorder_f     is_seperated;
  typedef unsigned int unsigned_type;
  typedef int          signed_type;
};

template<>
class zorder_traits <unsigned int>
{
public:
  static void check_type(){};
  typedef zorder_f     is_signed;
  typedef zorder_t     is_integral;
  typedef zorder_f     is_seperated;
  typedef unsigned int unsigned_type;
  typedef int          signed_type;
};

template<>
class zorder_traits <char>
{
public:
  static void check_type(){};
  typedef zorder_t      is_signed;
  typedef zorder_t      is_integral;
  typedef zorder_f      is_seperated;
  typedef unsigned char unsigned_type;
  typedef char          signed_type;
};

template<>
class zorder_traits <unsigned char>
{
public:
  static void check_type(){};
  typedef zorder_f      is_signed;
  typedef zorder_t      is_integral;
  typedef zorder_f      is_seperated;
  typedef unsigned char unsigned_type;
  typedef char          signed_type;
};

template<>
class zorder_traits <short>
{
public:
  static void check_type(){};
  typedef zorder_t       is_signed;
  typedef zorder_t       is_integral;
  typedef zorder_f       is_seperated;
  typedef unsigned short unsigned_type;
  typedef short          signed_type;
};

template<>
class zorder_traits <unsigned short>
{
public:
  static void check_type(){};
  typedef zorder_f       is_signed;
  typedef zorder_t       is_integral;
  typedef zorder_f       is_seperated;
  typedef unsigned short unsigned_type;
  typedef unsigned short signed_type;
};

template<>
class zorder_traits <long>
{
public:
  static void check_type(){};
  typedef zorder_t     is_signed;
  typedef zorder_t     is_integral;
  typedef zorder_f     is_seperated;
  typedef long unsigned int unsigned_type;
  typedef long int          signed_type;
};

template<>
class zorder_traits <unsigned long>
{
public:
  static void check_type(){};
  typedef zorder_f     is_signed;
  typedef zorder_t     is_integral;
  typedef zorder_f     is_seperated;
  typedef long unsigned int unsigned_type;
  typedef long unsigned int signed_type;
};

template<>
class zorder_traits <float>
{
public:
  static void check_type(){};
  typedef zorder_t       is_signed;
  typedef zorder_f       is_integral;
  typedef zorder_f       is_seperated;
  typedef float          unsigned_type;
  typedef float          signed_type;
};

template<>
class zorder_traits <double>
{
public:
  static void check_type(){};
  typedef zorder_t       is_signed;
  typedef zorder_f       is_integral;
  typedef zorder_f       is_seperated;
  typedef double         unsigned_type;
  typedef double         signed_type;
};

template<>
class zorder_traits <long double>
{
public:
  static void check_type(){};
  typedef zorder_t       is_signed;
  typedef zorder_f       is_integral;
  typedef zorder_f       is_seperated;
  typedef long double    unsigned_type;
  typedef long double    signed_type;
};

template<>
class zorder_traits <sep_float<float> >
{
public:
  static void check_type(){};
  typedef zorder_t       is_signed;
  typedef zorder_f       is_integral;
  typedef zorder_t       is_seperated;
};

template<>
class zorder_traits <sep_float<double> >
{
public:
  static void check_type(){};
  typedef zorder_t       is_signed;
  typedef zorder_f       is_integral;
  typedef zorder_t       is_seperated;
};

template<>
class zorder_traits <sep_float<long double> >
{
public:
  static void check_type(){};
  typedef zorder_t       is_signed;
  typedef zorder_f       is_integral;
  typedef zorder_t       is_seperated;
};

#endif
