//-*****************************************************************************
//
// Copyright (c) 2009-2011,
//  Sony Pictures Imageworks Inc. and
//  Industrial Light & Magic, a division of Lucasfilm Entertainment Company Ltd.
//
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
// *       Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
// *       Redistributions in binary form must reproduce the above
// copyright notice, this list of conditions and the following disclaimer
// in the documentation and/or other materials provided with the
// distribution.
// *       Neither the name of Industrial Light & Magic nor the names of
// its contributors may be used to endorse or promote products derived
// from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//-*****************************************************************************

//-*****************************************************************************
//! \file Alembic/Util/Exception.h
//! \brief Header file containing class definition for class
//!     \ref Alembic::Util::Exception
//-*****************************************************************************

#ifndef _Alembic_Util_Exception_h_
#define _Alembic_Util_Exception_h_

#include <Alembic/Util/Foundation.h>

//! \brief Alembic namespace
//! ...
namespace Alembic {
namespace Util {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
//! \brief Base class for all exceptions in the Alembic libraries. Derived
//!     from both std::exception and std::string, publicly
//!     It is mostly commonly thrown using the macros
class Exception : public std::string, public std::exception
{
public:
    //! \brief default constructor creates exception with
    //! empty message string
    Exception() throw() : std::string( "" ), std::exception() {}

    //! \brief Creates exception with an explicit message string.
    //! ...
    explicit Exception( const std::string &str ) throw()
      : std::string( str ), std::exception() {}

    //! \brief Copies exception.
    //! ...
    Exception( const Exception &exc ) throw()
      : std::string( exc.c_str() ), std::exception() {}

    //! \brief Destructor is empty, but virtual to support polymorphic
    //! destruction of data in any derived classes.
    virtual ~Exception() throw() {}

    //! \brief Inherited from std::exception, this returns a non-modifiable
    //! character string describing the nature of the exception
    virtual const char *what() const throw() { return c_str(); }
};

//-*****************************************************************************
//! \brief convenient macro which may be used with std::iostream syntax
//! \details Same as \ref ALEMBIC_THROW

#define ABC_THROW( TEXT )                             \
do                                                    \
{                                                     \
    std::stringstream sstr;                           \
    sstr << TEXT;                                     \
    Alembic::Util::Exception exc( sstr.str() );       \
    throw( exc );                                     \
}                                                     \
while( 0 )

//-*****************************************************************************
//! \brief convenient macro which may be used with std::iostream syntax
//! \details Same as \ref ABC_THROW
#define ALEMBIC_THROW( TEXT ) ABC_THROW( TEXT )

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace Util
} // End namespace Alembic

#endif
