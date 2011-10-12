//-*****************************************************************************
//
// Copyright (c) 2009-2011,
//  Sony Pictures Imageworks, Inc. and
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
// *       Neither the name of Sony Pictures Imageworks, nor
// Industrial Light & Magic nor the names of their contributors may be used
// to endorse or promote products derived from this software without specific
// prior written permission.
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

#ifndef _Alembic_AbcCoreAbstract_CompoundPropertyReader_h_
#define _Alembic_AbcCoreAbstract_CompoundPropertyReader_h_

#include <Alembic/AbcCoreAbstract/Foundation.h>
#include <Alembic/AbcCoreAbstract/BasePropertyReader.h>
#include <Alembic/AbcCoreAbstract/ForwardDeclarations.h>
#include <Alembic/AbcCoreAbstract/PropertyHeader.h>

namespace Alembic {
namespace AbcCoreAbstract {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
//! A CompoundProperty is a group of other properties, possibly Simple
//! or possibly Compound. Every object has at one of these.
class CompoundPropertyReader : public BasePropertyReader
{
public:
    //! Virtual destructor
    //! ...
    virtual ~CompoundPropertyReader();

    //-*************************************************************************
    // NEW FUNCTIONS
    //-*************************************************************************

    //! Returns the number of properties read from the file
    //! ...
    virtual size_t getNumProperties() = 0;

    //! Return the header of a property by index.
    //! This will throw an exception on out-of-range access.
    virtual const PropertyHeader & getPropertyHeader( size_t i ) = 0;

    //! Return the header of a property name.
    //! This will return a NULL pointer if no header by that name is found.
    virtual const PropertyHeader *
    getPropertyHeader( const std::string &iName ) = 0;

    //! Get a Scalar Property by name..
    //! It will return an empty pointer if the property is not scalar or
    //! is not found.
    virtual ScalarPropertyReaderPtr
    getScalarProperty( const std::string &iName ) = 0;
    
    //! Get a Array Property by name..
    //! It will return an empty pointer if the property is not array or
    //! is not found.
    virtual ArrayPropertyReaderPtr
    getArrayProperty( const std::string &iName ) = 0;
    
    //! Get a Compound Property by name..
    //! It will return an empty pointer if the property is not compound or
    //! is not found.
    virtual CompoundPropertyReaderPtr
    getCompoundProperty( const std::string &iName ) = 0;
    
    //! Get a base property by name.
    //! That property can be safely upcast.
    //! This is a convenience function that uses getPropertyHeader and
    //! the various named "get" functions here.
    BasePropertyReaderPtr getProperty( const std::string &iName );

    //! Get a Scalar Property by index.
    //! It will return an empty pointer if the property is not scalar or
    //! is not found.
    //! This is convenience function that uses the above functions
    //! to get the answer.
    ScalarPropertyReaderPtr
    getScalarProperty( size_t i );
    
    //! Get a Array Property by index.
    //! It will return an empty pointer if the property is not array or
    //! is not found.
    //! This is convenience function that uses the above functions
    //! to get the answer.
    ArrayPropertyReaderPtr
    getArrayProperty( size_t i );
    
    //! Get a Compound Property by index.
    //! It will return an empty pointer if the property is not compound or
    //! is not found.
    //! This is convenience function that uses the above functions
    //! to get the answer.
    CompoundPropertyReaderPtr
    getCompoundProperty( size_t i );

    //! Get a base property by index.
    //! It is an error to call with out-of-range indices.
    //! This is a convenience function that uses getPropertyHeader and
    //! the various named "get" functions here.
    BasePropertyReaderPtr getProperty( size_t i );

};

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcCoreAbstract
} // End namespace Alembic

#endif
