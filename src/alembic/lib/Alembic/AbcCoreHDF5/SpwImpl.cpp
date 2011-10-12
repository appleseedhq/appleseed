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

#include <Alembic/AbcCoreHDF5/SpwImpl.h>
#include <Alembic/AbcCoreHDF5/WriteUtil.h>
#include <Alembic/AbcCoreHDF5/StringWriteUtil.h>
#include <Alembic/AbcCoreHDF5/DataTypeRegistry.h>
#include <Alembic/AbcCoreHDF5/HDF5Util.h>

namespace Alembic {
namespace AbcCoreHDF5 {
namespace ALEMBIC_VERSION_NS {

//-*************************************************************************
SpwImpl::SpwImpl( AbcA::CompoundPropertyWriterPtr iParent,
                  hid_t iParentGroup,
                  const std::string & iName,
                  const AbcA::MetaData & iMetaData,
                  const AbcA::DataType & iDataType,
                  uint32_t iTimeSamplingIndex )
  : SimplePwImpl<AbcA::ScalarPropertyWriter,
                 SpwImpl,
                 const void *,
                 ScalarSampleKey>( iParent,
                                   iParentGroup,
                                   iName,
                                   iMetaData,
                                   iDataType,
                                   iTimeSamplingIndex,
                                   AbcA::kScalarProperty )
  , m_previousSample( iDataType )
{
    if ( m_header->getPropertyType() != AbcA::kScalarProperty )
    {
        ABCA_THROW( "Attempted to create a ScalarPropertyWriter from a "
                    "non-scalar property type" );
    }
}

//-*****************************************************************************
SpwImpl::~SpwImpl()
{
    WritePropertyInfo( m_parentGroup, m_header->getName(),
        m_header->getPropertyType(), m_header->getDataType(), true,
        m_timeSamplingIndex, m_nextSampleIndex, m_firstChangedIndex,
        m_lastChangedIndex );
}

//-*****************************************************************************
AbcA::ScalarPropertyWriterPtr SpwImpl::asScalarPtr()
{
    return shared_from_this();
}

//-*****************************************************************************
void SpwImpl::copyPreviousSample( hid_t iGroup,
                                  const std::string &iSampleName,
                                  index_t iSampleIndex )
{
    assert( iGroup >= 0 );
    assert( m_previousSample.getData() );
    
    // Write the sample.
    const AbcA::DataType &dtype = m_header->getDataType();
    uint8_t extent = dtype.getExtent();

    if ( dtype.getPod() == kStringPOD )
    {
        const std::string *strings
            = reinterpret_cast<const std::string *>(
                m_previousSample.getData() );
        
        if ( extent == 1 )
        {
            WriteString( iGroup, iSampleName, *strings );
        }
        else
        {
            WriteStrings( iGroup, iSampleName, dtype.getExtent(), strings );
        }
    }
    else if ( dtype.getPod() == kWstringPOD )
    {
        const std::wstring *wstrings
            = reinterpret_cast<const std::wstring *>(
                m_previousSample.getData() );
        
        if ( extent == 1 )
        {
            WriteWstring( iGroup, iSampleName, *wstrings );
        }
        else
        {
            WriteWstrings( iGroup, iSampleName, dtype.getExtent(), wstrings );
        }
    }
    else
    {
        assert( m_fileDataType >= 0 );
        assert( m_nativeDataType >= 0 );
        if (extent == 1)
        {
            WriteScalar( iGroup, iSampleName,
                         m_fileDataType,
                         m_nativeDataType,
                         m_previousSample.getData() );
        }
        else
        {
            WriteSmallArray( iGroup, iSampleName,
                         m_fileDataType,
                         m_nativeDataType,
                         extent,
                         m_previousSample.getData() );
        }
    }
}

//-*****************************************************************************
void SpwImpl::writeSample( hid_t iGroup,
                           const std::string &iSampleName,
                           index_t iSampleIndex,
                           const void *iSamp,
                           const ScalarSampleKey &iKey )
{
    assert( iGroup >= 0 );
    assert( iSamp );

    // Copy sample into previous bytes.
    m_previousSample.copyFrom( iSamp );

    // And now just copy previous sample.
    copyPreviousSample( iGroup, iSampleName, iSampleIndex );
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace AbcCoreHDF5
} // End namespace Alembic
