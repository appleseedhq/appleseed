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
// *       Neither the name of Sony Pictures Imageworks, nor
// Industrial Light & Magic, nor the names of their contributors may be used
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

#ifndef _Alembic_AbcCoreAbstract_PropertyHeader_h_
#define _Alembic_AbcCoreAbstract_PropertyHeader_h_

#include <Alembic/AbcCoreAbstract/Foundation.h>
#include <Alembic/AbcCoreAbstract/MetaData.h>
#include <Alembic/AbcCoreAbstract/DataType.h>
#include <Alembic/AbcCoreAbstract/TimeSampling.h>

namespace Alembic {
namespace AbcCoreAbstract {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
//! In Alembic, Objects may have three distinct types of abstract properties.
//! Any fancy type-specific properties are ultimately and instance of one
//! of these three types of properties, identified here by an enum.
//! "Simple" properties are simply "non-compound" properties -
//! the SimpleProperty classes are simply common base classes for
//! Scalar and Array Properties.
enum PropertyType
{

    //! Compound Properties are groups of other properties, with their own
    //! unique name and set of MetaData. All objects have a single root
    //! compound property as the base of their property description.
    kCompoundProperty = 0,

    //! Scalar Properties represent Rank-0 properties, which contain a
    //! single element value for any given time sample.
    kScalarProperty = 1,

    //! Array Properties represent Rank-N properties, which contain an
    //! array of values for any given time sample. Array properties may have
    //! any rank of 1 or higher, but will most often be ranks 1, 2, 3.
    kArrayProperty = 2
};

//-*****************************************************************************
//! The PropertyHeader is a collection of MetaData which helps define a
//! Property. It also acts as a key for getting an instance of a property
//! from a CompoundProperty.
class PropertyHeader
{
public:
    //! Default constructor creates an invalid property.
    //! The propertyType is set to Scalar, but the dataType will be set
    //! to its default value of kUnknownPOD[1]
    PropertyHeader()
      : m_name(),
        m_propertyType( kScalarProperty ),
        m_metaData(),
        m_dataType(),
        m_timeSampling() {}

    //! Construct a compound property header.
    //! Just give a name and metadata, the rest is redundant or unused.
    explicit PropertyHeader( const std::string &iName,
                             const MetaData &iMetaData )
      : m_name( iName ),
        m_propertyType( kCompoundProperty ),
        m_metaData( iMetaData ),
        m_dataType(),
        m_timeSampling() {}

    //! Construct a simple property header.
    //! Use this for array or scalar properties.
    PropertyHeader( const std::string &iName,
                    PropertyType iPropType,
                    const MetaData &iMetaData,
                    const DataType &iDataType,
                    const TimeSamplingPtr & iTsamp )
      : m_name( iName ),
        m_propertyType( iPropType ),
        m_metaData( iMetaData ),
        m_dataType( iDataType ),
        m_timeSampling( iTsamp ) {}

    //! Copy constructor
    //! ...
    PropertyHeader( const PropertyHeader &iCopy )
      : m_name( iCopy.m_name ),
        m_propertyType( iCopy.m_propertyType ),
        m_metaData( iCopy.m_metaData ),
        m_dataType( iCopy.m_dataType ),
        m_timeSampling( iCopy.m_timeSampling ) {}

    //! Assignment operator
    //! ...
    PropertyHeader& operator=( const PropertyHeader &iCopy )
    {
        m_name = iCopy.m_name;
        m_propertyType = iCopy.m_propertyType;
        m_metaData = iCopy.m_metaData;
        m_dataType = iCopy.m_dataType;
        m_timeSampling = iCopy.m_timeSampling;
        return *this;
    }

    //! All properties have a name, which is unique amongst its siblings.
    //! ...
    const std::string &getName() const { return m_name; }
    
    void setName( const std::string &iName ) { m_name = iName; }

    //! All properties have a type, which is the enum defined above.
    //! ...
    PropertyType getPropertyType() const { return m_propertyType; }
    
    void setPropertyType( PropertyType iPtyp ) { m_propertyType = iPtyp; }

    //! Convenience to return whether the property is scalar.
    //! Same as getPropertyType() == kScalarProperty
    bool isScalar() const { return m_propertyType == kScalarProperty; }

    //! Convenience to return whether the property is array.
    //! Same as getPropertyType() == kArrayProperty
    bool isArray() const { return m_propertyType == kArrayProperty; }

    //! Convenience to return whether the property is compound.
    //! Same as getPropertyType() == kCompoundProperty
    bool isCompound() const { return m_propertyType == kCompoundProperty; }

    //! Convenience to return whether the property is simple (non-compound)
    //! Same as getPropertyType() != kCompoundProperty
    bool isSimple() const { return !isCompound(); }

    //! All properties have metadata.
    //! ...
    const MetaData &getMetaData() const { return m_metaData; }
    
    void setMetaData( const MetaData &iMetaData ) { m_metaData = iMetaData; }

    //! Non-compound properties have a data type.
    //! If this is called for a Compound Property (basically, one which
    //! returns kCompoundProperty from getType() above)
    //! it will throw an exception.
    const DataType &getDataType() const { return m_dataType; }
    
    void setDataType( const DataType &iDataType ) { m_dataType = iDataType; }

    //! Non-compound properties have time sampling
    //! If this is called for a Compound Property (basically, one which
    //! returns kCompoundProperty from getType() above)
    //! it will throw an exception.
    TimeSamplingPtr getTimeSampling() const
    { return m_timeSampling; }
    
    void setTimeSampling( const TimeSamplingPtr &iTsamp )
    { m_timeSampling = iTsamp; }

private:
    std::string m_name;
    PropertyType m_propertyType;
    MetaData m_metaData;
    DataType m_dataType;
    TimeSamplingPtr m_timeSampling;
};

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcCoreAbstract
} // End namespace Alembic

#endif
