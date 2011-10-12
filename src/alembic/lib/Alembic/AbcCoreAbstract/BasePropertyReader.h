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

#ifndef _Alembic_AbcCoreAbstract_BasePropertyReader_h_
#define _Alembic_AbcCoreAbstract_BasePropertyReader_h_

#include <Alembic/AbcCoreAbstract/Foundation.h>
#include <Alembic/AbcCoreAbstract/ForwardDeclarations.h>
#include <Alembic/AbcCoreAbstract/PropertyHeader.h>

namespace Alembic {
namespace AbcCoreAbstract {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
//! Base Property Reader.
class BasePropertyReader
    : private boost::noncopyable
{
public:
    //! Virtual destructor
    //! ...
    virtual ~BasePropertyReader();

    //-*************************************************************************
    // NEW FUNCTIONS
    //-*************************************************************************

    //! Properties are created with a collection of metadata that is stored
    //! in a lightweight structure called PropertyHeader.
    //! This returns a constant reference to the PropertyHeader which
    //! was given upon creation.
    virtual const PropertyHeader & getHeader() const = 0;

    //! All properties have a name, which is unique amongst its siblings
    //! in the compund property they all live in. This is the name that
    //! was given when the property was created, and is part of the property
    //! header.
    const std::string &getName() const
    { return getHeader().getName(); }

    //! There are three types of abstract properties.
    //! They are Scalar, Array, and Compound properties. This function
    //! returns an enum PropertyType which indicates which property
    //! type is returned. This is simply a convenience function which
    //! returns data from the PropertyHeader.
    PropertyType getPropertyType() const
    { return getHeader().getPropertyType(); }

    //! Convenience to return whether the property is scalar.
    //! Same as getPropertyType() == kScalarProperty
    bool isScalar() const { return getPropertyType() == kScalarProperty; }

    //! Convenience to return whether the property is array.
    //! Same as getPropertyType() == kArrayProperty
    bool isArray() const { return getPropertyType() == kArrayProperty; }

    //! Convenience to return whether the property is compound.
    //! Same as getPropertyType() == kCompoundProperty
    bool isCompound() const { return getPropertyType() == kCompoundProperty; }

    //! Convenience to return whether the property is simple (non-compound)
    //! Same as getPropertyType() != kCompoundProperty
    bool isSimple() const { return !isCompound(); }

    //! All properties have MetaData. This just returns the
    //! MetaData portion of the header that was used in creation.
    const MetaData &getMetaData() const
    { return getHeader().getMetaData(); }

    //! Non-compound properties have a DataType. It is an error
    //! to call this function for CompoundProperties, and an exception will
    //! be thrown. This is a convenience function which just returns the
    //! DataType from the header that was used in creation.
    const DataType &getDataType() const
    { return getHeader().getDataType(); }

    //! Non-compound properties have a TimeSampling. It is an error
    //! to call this function for CompoundProperties, and an exception will
    //! be thrown. This is a convenience function which just returns the
    //! TimeSampling from the header that was used in creation.
    TimeSamplingPtr getTimeSampling() const
    { return getHeader().getTimeSampling(); }
    
    //! All properties have an object that owns them, and in order to
    //! ensure the object stays alive as long as the properties do, they
    //! retain a shared pointer to their object.
    virtual ObjectReaderPtr getObject() = 0;

    //! Most properties live in a compound property. (Except for
    //! the top-compound property in any object)
    //! This returns a pointer to the parent compound property.
    virtual CompoundPropertyReaderPtr getParent() = 0;

    //! Up-cast this base property to a ScalarProperty, if such an
    //! upcast is valid. This can be checked with the \ref isScalar()
    //! function. If the upcast is not valid, an empty pointer will
    //! be returned. This default implementation returns an empty
    //! pointer.
    virtual ScalarPropertyReaderPtr asScalarPtr();
    
    //! Up-cast this base property to an ArrayProperty, if such an
    //! upcast is valid. This can be checked with the \ref isArray()
    //! function. If the upcast is not valid, an empty pointer will
    //! be returned. This default implementation returns an empty
    //! pointer.
    virtual ArrayPropertyReaderPtr asArrayPtr();
    
    //! Up-cast this base property to a CompoundProperty, if such an
    //! upcast is valid. This can be checked with the \ref isCompound()
    //! function. If the upcast is not valid, an empty pointer will
    //! be returned. This default implementation returns an empty
    //! pointer.
    virtual CompoundPropertyReaderPtr asCompoundPtr();
};

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcCoreAbstract
} // End namespace Alembic

#endif
