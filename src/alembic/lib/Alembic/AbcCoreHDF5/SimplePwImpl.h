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

#ifndef _Alembic_AbcCoreHDF5_SimplePwImpl_h_
#define _Alembic_AbcCoreHDF5_SimplePwImpl_h_

#include <Alembic/AbcCoreHDF5/Foundation.h>
#include <Alembic/AbcCoreHDF5/WriteUtil.h>
#include <Alembic/AbcCoreHDF5/DataTypeRegistry.h>
#include <Alembic/AbcCoreHDF5/HDF5Util.h>

namespace Alembic {
namespace AbcCoreHDF5 {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
// This templated base class implements the common logic behind both the
// scalar and array property writers. The only way these two writers differ
// is the type of sample they write and the way those samples are stored
// and copied.  Scalar samples are simply copied by value, compared by value,
// and stored by value. Array samples are assumed to be "big", and are
// first condensed into a "key" (using a checksum), which is compared
// to other "keys" of previously written samples.  However, the primary logic
// of when to compare, when to copy, when to write, and how to manage time
// values is common across both classes, and represents the primary complexity.
// To minimize redundancy, errors, and code size, we create a common base class.
//
// It is assumed that the IMPL class will provide a mechanism for constructing
// a KEY from a SAMPLE.
// 
// The IMPL class is assumed to have the following functions:
// KEY  computeSampleKey( SAMPLE iSamp ) const;
// bool sameAsPreviousSample( SAMPLE iSamp, const KEY &iKey ) const;
// void copyPreviousSample( index_t iSampleIndex );
// void writeSample( index_t iSampleIndex, SAMPLE iSamp, const KEY &iKey );
//
//-*****************************************************************************
template <class ABSTRACT, class IMPL, class SAMPLE, class KEY>
class SimplePwImpl : public ABSTRACT
{
protected:
    SimplePwImpl( AbcA::CompoundPropertyWriterPtr iParent,
                  hid_t iParentGroup,
                  const std::string & iName,
                  const AbcA::MetaData & iMetaData,
                  const AbcA::DataType & iDataType,
                  uint32_t iTimeSamplingIndex,
                  AbcA::PropertyType iPropType );

public:
    virtual ~SimplePwImpl();
    
    //-*************************************************************************
    // FROM ABSTRACT
    //-*************************************************************************
    
    virtual const AbcA::PropertyHeader & getHeader() const;

    virtual AbcA::ObjectWriterPtr getObject();

    virtual AbcA::CompoundPropertyWriterPtr getParent();

private:
    hid_t getSampleIGroup();

public:
    // Scalar/Array API
    virtual void setSample( SAMPLE iSamp );

    virtual void setFromPreviousSample( );

    virtual size_t getNumSamples();

    virtual void setTimeSamplingIndex( uint32_t iIndex );

protected:
    // The parent compound property writer.
    AbcA::CompoundPropertyWriterPtr m_parent;
    
    // The parent group. We need to keep this around because we
    // don't create our group until we need to. This is guaranteed to
    // exist because our parent (or object) is guaranteed to exist.
    hid_t m_parentGroup;

    // The header which defines this property.
    PropertyHeaderPtr m_header;

    // The DataTypes for this property.
    hid_t m_fileDataType;
    bool m_cleanFileDataType;
    hid_t m_nativeDataType;
    bool m_cleanNativeDataType;

    // The group corresponding to this property.
    // It may never be created or written.
    hid_t m_sampleIGroup;

    // Index of the next sample to write
    uint32_t m_nextSampleIndex;

    // Index representing the first sample that is different from sample 0
    uint32_t m_firstChangedIndex;

    // Index representing the last sample in which a change has occured
    // There is no need to repeat samples if they are the same between this
    // index and m_nextSampleIndex
    uint32_t m_lastChangedIndex;

    // Index representing which TimeSampling from the ArchiveWriter to use.
    uint32_t m_timeSamplingIndex;
};

//-*****************************************************************************
//-*****************************************************************************
//-*****************************************************************************
// IMPLEMENTATION
//-*****************************************************************************
//-*****************************************************************************
//-*****************************************************************************

//-*****************************************************************************
template <class ABSTRACT, class IMPL, class SAMPLE, class KEY>
SimplePwImpl<ABSTRACT,IMPL,SAMPLE,KEY>::SimplePwImpl
(
  AbcA::CompoundPropertyWriterPtr iParent,
  hid_t iParentGroup,
  const std::string & iName,
  const AbcA::MetaData & iMetaData,
  const AbcA::DataType & iDataType,
  uint32_t iTimeSamplingIndex,
  AbcA::PropertyType iPropType
)
  : m_parent( iParent )
  , m_parentGroup( iParentGroup )
  , m_fileDataType( -1 )
  , m_cleanFileDataType( false )
  , m_nativeDataType( -1 )
  , m_cleanNativeDataType( false )
  , m_sampleIGroup( -1 )
  , m_nextSampleIndex( 0 )
  , m_firstChangedIndex( 0 )
  , m_lastChangedIndex( 0 )
  , m_timeSamplingIndex(iTimeSamplingIndex)
{
    // Check the validity of all inputs.
    ABCA_ASSERT( m_parent, "Invalid parent" );

    // will assert if TimeSamplingPtr not found
    AbcA::TimeSamplingPtr ts =
        m_parent->getObject()->getArchive()->getTimeSampling(
            m_timeSamplingIndex );

    m_header = PropertyHeaderPtr( new AbcA::PropertyHeader( iName, iPropType,
        iMetaData, iDataType, ts ) );

    ABCA_ASSERT( m_header, "Invalid property header" );
    ABCA_ASSERT( m_parentGroup >= 0, "Invalid parent group" );
    ABCA_ASSERT( m_header->getDataType().getExtent() > 0,
        "Invalid DatatType extent");
 
    // Get data types
    PlainOldDataType POD = m_header->getDataType().getPod();
    if ( POD != kStringPOD && POD != kWstringPOD )
    {
        m_fileDataType = GetFileH5T( m_header->getDataType(),
                                     m_cleanFileDataType );
        m_nativeDataType = GetNativeH5T( m_header->getDataType(),
                                         m_cleanNativeDataType );
        
        ABCA_ASSERT( m_fileDataType >= 0, "Couldn't get file datatype" );
        ABCA_ASSERT( m_nativeDataType >= 0, "Couldn't get native datatype" );
    }

    WriteMetaData( m_parentGroup, m_header->getName() + ".meta",
        m_header->getMetaData() );
}

//-*****************************************************************************
// Destructor is at the end, so that this file has a logical ordering that
// matches the order of operations (create, set samples, destroy)
//-*****************************************************************************

//-*****************************************************************************
template <class ABSTRACT, class IMPL, class SAMPLE, class KEY>
const AbcA::PropertyHeader &
SimplePwImpl<ABSTRACT,IMPL,SAMPLE,KEY>::getHeader() const
{
    ABCA_ASSERT( m_header, "Invalid header" );
    return *m_header;
}

//-*****************************************************************************
template <class ABSTRACT, class IMPL, class SAMPLE, class KEY>
AbcA::ObjectWriterPtr
SimplePwImpl<ABSTRACT,IMPL,SAMPLE,KEY>::getObject()
{
    ABCA_ASSERT( m_parent, "Invalid parent" );
    return m_parent->getObject();
}

//-*****************************************************************************
template <class ABSTRACT, class IMPL, class SAMPLE, class KEY>
AbcA::CompoundPropertyWriterPtr
SimplePwImpl<ABSTRACT,IMPL,SAMPLE,KEY>::getParent()
{
    ABCA_ASSERT( m_parent, "Invalid parent" );
    return m_parent;
}

//-*****************************************************************************
template <class ABSTRACT, class IMPL, class SAMPLE, class KEY>
hid_t SimplePwImpl<ABSTRACT,IMPL,SAMPLE,KEY>::getSampleIGroup()
{
    if ( m_sampleIGroup >= 0 )
    {
        return m_sampleIGroup;
    }

    ABCA_ASSERT( m_parentGroup >= 0, "invalid parent group" );
    ABCA_ASSERT( m_nextSampleIndex > 0,
                 "can't create sampleI group before numSamples > 1" );

    const std::string groupName = m_header->getName() + ".smpi";
    
    hid_t copl = CreationOrderPlist();
    PlistCloser plistCloser( copl );
    
    m_sampleIGroup = H5Gcreate2( m_parentGroup,
                                 groupName.c_str(),
                                 H5P_DEFAULT,
                                 copl,
                                 H5P_DEFAULT );
    ABCA_ASSERT( m_sampleIGroup >= 0,
                 "Could not create simple samples group named: "
                 << groupName );
    
    return m_sampleIGroup;
}

//-*****************************************************************************
template <class ABSTRACT, class IMPL, class SAMPLE, class KEY>
void SimplePwImpl<ABSTRACT,IMPL,SAMPLE,KEY>::setSample
( SAMPLE iSamp )
{
    // Make sure we aren't writing more samples than we have times for
    // This applies to acyclic sampling only
    ABCA_ASSERT(
        !m_header->getTimeSampling()->getTimeSamplingType().isAcyclic() ||
        m_header->getTimeSampling()->getNumStoredTimes() > 
        m_nextSampleIndex,
        "Can not write more samples than we have times for when using "
        "Acyclic sampling." );

    // The Key helps us analyze the sample.
    KEY key = static_cast<IMPL*>(this)->computeSampleKey( iSamp );

    // We need to write the sample
    if ( m_nextSampleIndex == 0  ||
        !(static_cast<IMPL*>(this)->sameAsPreviousSample( iSamp, key )) )
    {
        const std::string &myName = m_header->getName();

        // we only need to repeat samples if this is not the first change
        if (m_firstChangedIndex != 0)
        {
            // copy the samples from after the last change to the latest index
            for ( index_t smpI = m_lastChangedIndex + 1;
                smpI < m_nextSampleIndex; ++smpI )
            {
                assert( smpI > 0 );
                static_cast<IMPL*>(this)->copyPreviousSample(
                    getSampleIGroup(),
                    getSampleName( myName, smpI ),
                    smpI );
            }
        }
        else
        {
            m_firstChangedIndex = m_nextSampleIndex;
        }

        // Write this sample, which will update its internal
        // cache of what the previously written sample was.
        static_cast<IMPL*>(this)->writeSample(
            m_nextSampleIndex == 0 ? m_parentGroup : getSampleIGroup(),
            getSampleName( myName, m_nextSampleIndex ),
            m_nextSampleIndex, iSamp, key );

        // this index is now the last change
        m_lastChangedIndex = m_nextSampleIndex;
    }


    m_nextSampleIndex ++;
}

//-*****************************************************************************
template <class ABSTRACT, class IMPL, class SAMPLE, class KEY>
void SimplePwImpl<ABSTRACT,IMPL,SAMPLE,KEY>::setFromPreviousSample
()
{

    // Make sure we aren't writing more samples than we have times for
    // This applies to acyclic sampling only
    ABCA_ASSERT(
        !m_header->getTimeSampling()->getTimeSamplingType().isAcyclic() ||
        m_header->getTimeSampling()->getNumStoredTimes() >
        m_nextSampleIndex,
        "Can not set more samples than we have times for when using "
        "Acyclic sampling." );

    ABCA_ASSERT( m_nextSampleIndex > 0,
        "Can't set from previous sample before any samples have been written" );

    m_nextSampleIndex ++;
}

//-*****************************************************************************
template <class ABSTRACT, class IMPL, class SAMPLE, class KEY>
size_t SimplePwImpl<ABSTRACT,IMPL,SAMPLE,KEY>::getNumSamples()
{
    return ( size_t )m_nextSampleIndex;
}

template <class ABSTRACT, class IMPL, class SAMPLE, class KEY>
void SimplePwImpl<ABSTRACT,IMPL,SAMPLE,KEY>::setTimeSamplingIndex
( uint32_t iIndex )
{
    // will assert if TimeSamplingPtr not found
    AbcA::TimeSamplingPtr ts =
        m_parent->getObject()->getArchive()->getTimeSampling(
            iIndex );

    ABCA_ASSERT( !ts->getTimeSamplingType().isAcyclic() ||
        ts->getNumStoredTimes() > m_nextSampleIndex,
        "Already have written more samples than we have times for when using "
        "Acyclic sampling." );

    m_header->setTimeSampling(ts);
    m_timeSamplingIndex = iIndex;
}

//-*****************************************************************************
template <class ABSTRACT, class IMPL, class SAMPLE, class KEY>
SimplePwImpl<ABSTRACT,IMPL,SAMPLE,KEY>::~SimplePwImpl()
{
    // Wrap the whole thing in a try block, so as to prevent
    // exceptions from being thrown out of a destructor.
    try
    {
        if ( m_fileDataType >= 0 && m_cleanFileDataType )
        { H5Tclose( m_fileDataType ); }
        if ( m_nativeDataType >= 0 && m_cleanNativeDataType )
        { H5Tclose( m_nativeDataType ); }

        // Check validity of the group.
        ABCA_ASSERT( m_parentGroup >= 0, "Invalid parent group" );

        // Close the sampleIGroup if it was open
        if ( m_sampleIGroup >= 0 )
        {
            // this should never have been openened, if a change was never
            // detected.
            ABCA_ASSERT( m_firstChangedIndex > 0, "Corrupt SimplePwImpl" );
            H5Gclose( m_sampleIGroup );
            m_sampleIGroup = -1;
        }
    }
    catch ( std::exception & exc )
    {
        std::cerr << "AbcCoreHDF5::SimplePwImpl<A,I,K>::"
                  << "~SimplePwImpl(): EXCEPTION: "
                  << exc.what() << std::endl;
    }
    catch ( ... )
    {
        std::cerr << "AbcCoreHDF5::SimplePwImpl<A,I,K>::~SimplePwImpl(): "
                  << "UNKNOWN EXCEPTION: " << std::endl;
    }

    m_parentGroup = -1;
    m_sampleIGroup = -1;
    m_fileDataType = -1;
    m_nativeDataType = -1;
}

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcCoreHDF5
} // End namespace Alembic

#endif
