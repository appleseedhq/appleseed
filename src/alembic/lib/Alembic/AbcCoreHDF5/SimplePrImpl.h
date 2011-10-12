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

#ifndef _Alembic_AbcCoreHDF5_SimplePrImpl_h_
#define _Alembic_AbcCoreHDF5_SimplePrImpl_h_

#include <Alembic/AbcCoreHDF5/Foundation.h>

#include <Alembic/AbcCoreHDF5/OrImpl.h>
#include <Alembic/AbcCoreHDF5/ArImpl.h>
#include <Alembic/AbcCoreHDF5/ReadUtil.h>
#include <Alembic/AbcCoreHDF5/DataTypeRegistry.h>
#include <Alembic/AbcCoreHDF5/HDF5Util.h>

namespace Alembic {
namespace AbcCoreHDF5 {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
// This templated base class implements the common logic behind both the
// scalar and array property readers. The only way these two readers differ
// is the type of sample they read and the way those samples are returned
// and managed. Scalar samples are simply copied by value, compared by value,
// and stored by value. Array samples are returned from an optional sample
// cache.
//
// There is a bit of template hoosafudgery going on here, which is always
// troublesome in foundation libraries, because it is hard to read and
// hard to decipher errors. I have kept it to a reasonable minimum, but the
// value of a single code instance is high enough that it's worth a bit of
// obfuscation.
//
// The IMPL class is assumed to have the following functions:
// void readSample( H5G &iGroup,
//                  const std::string &iSampleName,
//                  index_t iSampleIndex,
//                  SAMPLE oSample );
//
//-*****************************************************************************
template <class ABSTRACT, class IMPL, class SAMPLE>
class SimplePrImpl : public ABSTRACT
{
protected:
    SimplePrImpl( AbcA::CompoundPropertyReaderPtr iParent,
                  hid_t iParentGroup,
                  PropertyHeaderPtr iHeader,
                  uint32_t iNumSamples,
                  uint32_t iFirstChangedIndex,
                  uint32_t iLastChangedIndex );

public:
    //-*************************************************************************
    // ABSTRACT API
    //-*************************************************************************
    virtual ~SimplePrImpl();

    virtual const AbcA::PropertyHeader &getHeader() const;

    virtual AbcA::ObjectReaderPtr getObject();

    virtual AbcA::CompoundPropertyReaderPtr getParent();

    virtual size_t getNumSamples();

    virtual bool isConstant();

    virtual void getSample( index_t iSampleIndex,
                            SAMPLE oSample );

    virtual std::pair<index_t, chrono_t> getFloorIndex( chrono_t iTime );

    virtual std::pair<index_t, chrono_t> getCeilIndex( chrono_t iTime );

    virtual std::pair<index_t, chrono_t> getNearIndex( chrono_t iTime );

    virtual bool getKey( index_t iSampleIndex, AbcA::ArraySampleKey & oKey );

protected:

    index_t verifySampleIndex( index_t iSampleIndex );

    // Parent compound property writer. It must exist.
    AbcA::CompoundPropertyReaderPtr m_parent;

    // The HDF5 Group associated with the parent property reader.
    hid_t m_parentGroup;

    // We don't hold a pointer to the object, but instead
    // get it from the compound property reader.

    // The Header
    PropertyHeaderPtr m_header;

    // Data Types.
    hid_t m_fileDataType;
    bool m_cleanFileDataType;
    hid_t m_nativeDataType;
    bool m_cleanNativeDataType;

    // The number of samples that were written. This may be greater
    // than the number of samples that were stored, because we don't
    // repeat head or tail samples that repeat
    uint32_t m_numSamples;

    // The first sample index that is different from sample 0
    uint32_t m_firstChangedIndex;

    // The last sample index that needed to be written out, if the last sample
    // repeats m_numSamples will be greater than this.
    uint32_t m_lastChangedIndex;

    // The simple properties only store samples after the first
    // sample in a sub group. Therefore, there may not actually be
    // a group associated with this property.
    hid_t m_samplesIGroup;
};

//-*****************************************************************************
//-*****************************************************************************
//-*****************************************************************************
// IMPLEMENTATION
//-*****************************************************************************
//-*****************************************************************************
//-*****************************************************************************

//-*****************************************************************************
template <class ABSTRACT, class IMPL, class SAMPLE>
SimplePrImpl<ABSTRACT,IMPL,SAMPLE>::SimplePrImpl
(
    AbcA::CompoundPropertyReaderPtr iParent,
    hid_t iParentGroup,
    PropertyHeaderPtr iHeader,
    uint32_t iNumSamples,
    uint32_t iFirstChangedIndex,
    uint32_t iLastChangedIndex
)
  : m_parent( iParent )
  , m_parentGroup( iParentGroup )
  , m_header( iHeader )
  , m_fileDataType( -1 )
  , m_cleanFileDataType( false )
  , m_nativeDataType( -1 )
  , m_cleanNativeDataType( false )
  , m_numSamples( iNumSamples )
  , m_firstChangedIndex( iFirstChangedIndex )
  , m_lastChangedIndex( iLastChangedIndex )
  , m_samplesIGroup( -1 )
{
    // Validate all inputs.
    ABCA_ASSERT( m_parent, "Invalid parent" );
    ABCA_ASSERT( m_parentGroup >= 0, "Invalid parent group" );
    ABCA_ASSERT( m_header, "Invalid header" );
    ABCA_ASSERT( m_header->getPropertyType() != AbcA::kCompoundProperty,
                 "Tried to create a simple property with a compound header" );

    // Get data types
    PlainOldDataType POD = m_header->getDataType().getPod();
    if ( POD != kStringPOD && POD != kWstringPOD )
    {
        m_fileDataType = GetFileH5T( m_header->getDataType(),
                                     m_cleanFileDataType );
        m_nativeDataType = GetNativeH5T( m_header->getDataType(),
                                         m_cleanNativeDataType );
    }

    // Get our name.
    const std::string &myName = m_header->getName();

    // Validate the first and last changed index
    ABCA_ASSERT( m_firstChangedIndex <= m_numSamples &&
                 m_lastChangedIndex <= m_numSamples &&
                 m_firstChangedIndex <= m_lastChangedIndex,
                 "Corrupt sampling information for property: " << myName
                 << " first change index: " << m_firstChangedIndex
                 << " last change index: " << m_lastChangedIndex
                 << " total number of samples: " << m_numSamples );
}

//-*****************************************************************************
// Destructor is at the end, so that this file has a logical ordering that
// matches the order of operations (create, get samples, destroy)
//-*****************************************************************************

//-*****************************************************************************
template <class ABSTRACT, class IMPL, class SAMPLE>
const AbcA::PropertyHeader &
SimplePrImpl<ABSTRACT,IMPL,SAMPLE>::getHeader() const
{
    ABCA_ASSERT( m_header, "Invalid header" );
    return *m_header;
}

//-*****************************************************************************
template <class ABSTRACT, class IMPL, class SAMPLE>
AbcA::ObjectReaderPtr
SimplePrImpl<ABSTRACT,IMPL,SAMPLE>::getObject()
{
    ABCA_ASSERT( m_parent, "Invalid parent" );
    return m_parent->getObject();
}

//-*****************************************************************************
template <class ABSTRACT, class IMPL, class SAMPLE>
AbcA::CompoundPropertyReaderPtr
SimplePrImpl<ABSTRACT,IMPL,SAMPLE>::getParent()
{
    ABCA_ASSERT( m_parent, "Invalid parent" );
    return m_parent;
}

//-*****************************************************************************
template <class ABSTRACT, class IMPL, class SAMPLE>
size_t SimplePrImpl<ABSTRACT,IMPL,SAMPLE>::getNumSamples()
{
    return ( size_t )m_numSamples;
}

//-*****************************************************************************
template <class ABSTRACT, class IMPL, class SAMPLE>
bool SimplePrImpl<ABSTRACT,IMPL,SAMPLE>::isConstant()
{
    // No first change means no changes at all
    return ( m_firstChangedIndex == 0 );
}

//-*****************************************************************************
template <class ABSTRACT, class IMPL, class SAMPLE>
index_t SimplePrImpl<ABSTRACT,IMPL,SAMPLE>::verifySampleIndex( index_t iIndex )
{
    // Verify sample index
    ABCA_ASSERT( iIndex >= 0 &&
                 iIndex < m_numSamples,
                 "Invalid sample index: " << iIndex
                 << ", should be between 0 and " << m_numSamples-1 );

    // greater than the last index that had a change?  read it from there
    if ( iIndex > m_lastChangedIndex )
    {
        iIndex = m_lastChangedIndex;
    }
    // less than the first change?  map to 0
    else if ( iIndex < m_firstChangedIndex )
    {
        iIndex = 0;
    }

    return iIndex;
}

//-*****************************************************************************
template <class ABSTRACT, class IMPL, class SAMPLE>
std::pair<index_t, chrono_t>
SimplePrImpl<ABSTRACT,IMPL,SAMPLE>::getFloorIndex( chrono_t iTime )
{
    return m_header->getTimeSampling()->getFloorIndex( iTime, m_numSamples );
}

//-*****************************************************************************
template <class ABSTRACT, class IMPL, class SAMPLE>
std::pair<index_t, chrono_t>
SimplePrImpl<ABSTRACT,IMPL,SAMPLE>::getCeilIndex( chrono_t iTime )
{
    return m_header->getTimeSampling()->getCeilIndex( iTime, m_numSamples );
}

//-*****************************************************************************
template <class ABSTRACT, class IMPL, class SAMPLE>
std::pair<index_t, chrono_t>
SimplePrImpl<ABSTRACT,IMPL,SAMPLE>::getNearIndex( chrono_t iTime )
{
    return m_header->getTimeSampling()->getNearIndex( iTime, m_numSamples );
}

//-*****************************************************************************
template <class ABSTRACT, class IMPL, class SAMPLE>
void
SimplePrImpl<ABSTRACT,IMPL,SAMPLE>::getSample( index_t iSampleIndex,
                                               SAMPLE oSample )
{

    iSampleIndex = verifySampleIndex( iSampleIndex );

    // Get our name.
    const std::string &myName = m_header->getName();

    if ( iSampleIndex == 0 )
    {
        // Read the sample from the parent group.
        // Sample 0 is always on the parent group, with
        // our name + ".smp0" as the name of it.
        std::string sample0Name = getSampleName( myName, 0 );
        if ( m_header->getPropertyType() == AbcA::kScalarProperty )
        {
            ABCA_ASSERT( H5Aexists( m_parentGroup, sample0Name.c_str() ),
                         "Invalid property: " << myName
                         << ", missing smp0" );
        }
        else
        {
            ABCA_ASSERT( DatasetExists( m_parentGroup, sample0Name ),
                         "Invalid property: " << myName
                         << ", missing smp1" );
        }

        static_cast<IMPL *>( this )->readSample( m_parentGroup,
                                                 sample0Name,
                                                 iSampleIndex,
                                                 oSample );
    }
    else
    {
        // Create the subsequent samples group.
        if ( m_samplesIGroup < 0 )
        {
            std::string samplesIName = myName + ".smpi";
            ABCA_ASSERT( GroupExists( m_parentGroup,
                                      samplesIName ),
                         "Invalid property: " << myName
                         << ", missing smpi" );

            m_samplesIGroup = H5Gopen2( m_parentGroup,
                                        samplesIName.c_str(),
                                        H5P_DEFAULT );
            ABCA_ASSERT( m_samplesIGroup >= 0,
                         "Invalid property: " << myName
                         << ", invalid smpi group" );
        }

        // Read the sample.
        std::string sampleName = getSampleName( myName, iSampleIndex );
        static_cast<IMPL *>( this )->readSample( m_samplesIGroup,
                                                 sampleName,
                                                 iSampleIndex,
                                                 oSample );
    }
}

//-*****************************************************************************
template <class ABSTRACT, class IMPL, class SAMPLE>
bool
SimplePrImpl<ABSTRACT,IMPL,SAMPLE>::getKey( index_t iSampleIndex,
                                            AbcA::ArraySampleKey & oKey )
{
    iSampleIndex = verifySampleIndex( iSampleIndex );

    // Get our name.
    const std::string &myName = m_header->getName();

    if ( iSampleIndex == 0 )
    {
        // Read the sample from the parent group.
        // Sample 0 is always on the parent group, with
        // our name + ".smp0" as the name of it.
        std::string sample0Name = getSampleName( myName, 0 );
        if ( m_header->getPropertyType() == AbcA::kScalarProperty )
        {
            ABCA_ASSERT( H5Aexists( m_parentGroup, sample0Name.c_str() ),
                         "Invalid property: " << myName
                         << ", missing smp0" );
        }
        else
        {
            ABCA_ASSERT( DatasetExists( m_parentGroup, sample0Name ),
                         "Invalid property: " << myName
                         << ", missing smp1" );
        }

        return static_cast<IMPL *>( this )->readKey( m_parentGroup,
                                                     sample0Name,
                                                     oKey );
    }
    else
    {
        // Create the subsequent samples group.
        if ( m_samplesIGroup < 0 )
        {
            std::string samplesIName = myName + ".smpi";
            ABCA_ASSERT( GroupExists( m_parentGroup,
                                      samplesIName ),
                         "Invalid property: " << myName
                         << ", missing smpi" );

            m_samplesIGroup = H5Gopen2( m_parentGroup,
                                        samplesIName.c_str(),
                                        H5P_DEFAULT );
            ABCA_ASSERT( m_samplesIGroup >= 0,
                         "Invalid property: " << myName
                         << ", invalid smpi group" );
        }

        // Read the sample.
        std::string sampleName = getSampleName( myName, iSampleIndex );
        return static_cast<IMPL *>( this )->readKey( m_samplesIGroup,
                                                     sampleName,
                                                     oKey );
    }
}

//-*****************************************************************************
template <class ABSTRACT, class IMPL, class SAMPLE>
SimplePrImpl<ABSTRACT,IMPL,SAMPLE>::~SimplePrImpl()
{
    // Clean up our samples group, if necessary.
    if ( m_samplesIGroup >= 0 )
    {
        H5Gclose( m_samplesIGroup );
        m_samplesIGroup = -1;
    }

    if ( m_fileDataType >= 0 && m_cleanFileDataType )
    {
        H5Tclose( m_fileDataType );
        m_fileDataType = -1;
    }

    if ( m_nativeDataType >= 0 && m_cleanNativeDataType )
    {
        H5Tclose( m_nativeDataType );
        m_nativeDataType = -1;
    }
}

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcCoreHDF5
} // End namespace Alembic

#endif
