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

#include <Alembic/AbcCoreHDF5/ApwImpl.h>
#include <Alembic/AbcCoreHDF5/WriteUtil.h>
#include <Alembic/AbcCoreHDF5/StringWriteUtil.h>

namespace Alembic {
namespace AbcCoreHDF5 {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
ApwImpl::ApwImpl( AbcA::CompoundPropertyWriterPtr iParent,
                  hid_t iParentGroup,
                  const std::string & iName,
                  const AbcA::MetaData & iMetaData,
                  const AbcA::DataType & iDataType,
                  uint32_t iTimeSamplingIndex )
  : SimplePwImpl<AbcA::ArrayPropertyWriter,
                 ApwImpl,
                 const AbcA::ArraySample &,
                 AbcA::ArraySample::Key>( iParent,
                                          iParentGroup,
                                          iName,
                                          iMetaData,
                                          iDataType,
                                          iTimeSamplingIndex,
                                          AbcA::kArrayProperty )
{
    if ( m_header->getPropertyType() != AbcA::kArrayProperty )
    {
        ABCA_THROW( "Attempted to create a ArrayPropertyWriter from a "
                    "non-array property type" );
    }

    m_isScalarLike = true;

    // The WrittenArraySampleID is invalid by default.
    assert( !m_previousWrittenArraySampleID );
}


//-*****************************************************************************
ApwImpl::~ApwImpl()
{
    WritePropertyInfo( m_parentGroup, m_header->getName(),
        m_header->getPropertyType(), m_header->getDataType(), m_isScalarLike,
        m_timeSamplingIndex, m_nextSampleIndex, m_firstChangedIndex,
        m_lastChangedIndex );
}


//-*****************************************************************************
AbcA::ArrayPropertyWriterPtr ApwImpl::asArrayPtr()
{
    return shared_from_this();
}

//-*****************************************************************************
void ApwImpl::writeSample( hid_t iGroup,
                           const std::string &iSampleName,
                           index_t iSampleIndex,
                           const AbcA::ArraySample & iSamp,
                           const AbcA::ArraySample::Key &iKey )
{
    AbcA::ArchiveWriterPtr awp =
        this->getObject()->getArchive();

    ABCA_ASSERT(iSamp.getDataType() == m_header->getDataType(),
        "DataType on ArraySample iSamp: " << iSamp.getDataType() <<
        ", does not match the DataType of the Array property: " <<
        m_header->getDataType());

    // if we haven't written this already, m_isScalarLike will be true
    if (m_isScalarLike && iSamp.getDimensions().numPoints() != 1)
    {
        m_isScalarLike = false;
    }

    // Write the sample.
    // This distinguishes between string, wstring, and regular arrays.
    m_previousWrittenArraySampleID =
        WriteArray( GetWrittenArraySampleMap( awp ),
                    iGroup, iSampleName,
                    iSamp, iKey,
                    m_fileDataType,
                    m_nativeDataType,
                    awp->getCompressionHint() );
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace AbcCoreHDF5
} // End namespace Alembic
