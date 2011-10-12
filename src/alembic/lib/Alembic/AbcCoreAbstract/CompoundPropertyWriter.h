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

#ifndef _Alembic_AbcCoreAbstract_CompoundPropertyWriter_h_
#define _Alembic_AbcCoreAbstract_CompoundPropertyWriter_h_

#include <Alembic/AbcCoreAbstract/Foundation.h>
#include <Alembic/AbcCoreAbstract/BasePropertyWriter.h>

namespace Alembic {
namespace AbcCoreAbstract {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
//! A CompoundProperty is a group of other properties, possibly Simple
//! or possibly Compound. Every object has at one of these.
class CompoundPropertyWriter : public BasePropertyWriter
{
public:
    //! Virtual destructor
    //! ...
    virtual ~CompoundPropertyWriter();

    //-*************************************************************************
    // NEW FUNCTIONS
    //-*************************************************************************

    //! Returns the number of properties that have been created thus far.
    //! May change as more are created.
    virtual size_t getNumProperties() = 0;

    //! Return the header of a property that has already been added.
    //! Property is selected by index.
    //! This will throw an exception on out-of-range access.
    virtual const PropertyHeader & getPropertyHeader( size_t i ) = 0;

    //! Return the header of a property that has already been added,
    //! found by name. A typical use of this would be for an application
    //! that wants to incrementally add properties, and wishes to query
    //! whether a property of a given name has already been added, before
    //! attempting to add a new one.
    //! This will return NULL if no property of the given name has
    //! been added.
    virtual const PropertyHeader *
    getPropertyHeader( const std::string &iName ) = 0;

    //! It is an error to request for a property by index out of range.
    //! This returns a property that has ALREADY BEEN ADDED.
    //! This will throw an exception on out-of-range access.
    //! There is a possibility it could return a NULL pointer, if the
    //! added property has been closed (deleted).
    //! This is just a convenience function which calls getPropertyHeader
    //! and then getProperty.
    BasePropertyWriterPtr getProperty( size_t i );

    //! Returns an ALREADY ADDED PROPERTY by name. If it can't find
    //! one by name, it returns an empty pointer. This can also happen
    //! if the property was added, but has been closed (deleted).
    virtual BasePropertyWriterPtr getProperty( const std::string & iName ) = 0;

    //! Create and return the requested scalar property.
    //! If a property already exists with the same name, throws
    //! an exception. An exception will also be thrown if the DataType, 
    //! or time sampling index is illegal.
    virtual ScalarPropertyWriterPtr
    createScalarProperty( const std::string & iName,
        const MetaData & iMetaData,
        const DataType & iDataType,
        uint32_t iTimeSamplingIndex ) = 0;

    //! Create and return the requested array property.
    //! If a property already exists with the same name, throws.
    //! an exception. An exception will also be thrown if the DataType, 
    //! or time sampling index is illegal.
    virtual ArrayPropertyWriterPtr
    createArrayProperty( const std::string & iName,
        const MetaData & iMetaData,
        const DataType & iDataType,
        uint32_t iTimeSamplingIndex ) = 0;
    
    //! Create and return the requested compound property.
    //! If a property already exists with the same name, throws
    //! an exception.
    virtual CompoundPropertyWriterPtr
    createCompoundProperty( const std::string & iName,
        const MetaData & iMetaData ) = 0;

};

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcCoreAbstract
} // End namespace Alembic

#endif

