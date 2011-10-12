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

#include <Alembic/AbcCoreHDF5/SprImpl.h>
#include <Alembic/AbcCoreHDF5/ReadUtil.h>
#include <Alembic/AbcCoreHDF5/StringReadUtil.h>

namespace Alembic {
namespace AbcCoreHDF5 {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
AbcA::ScalarPropertyReaderPtr SprImpl::asScalarPtr()
{
    return shared_from_this();
}

//-*****************************************************************************
void SprImpl::readSample( hid_t iGroup,
                          const std::string &iSampleName,
                          index_t iSampleIndex,
                          void *oSampleBytes )
{
    assert( iGroup >= 0 );
    assert( oSampleBytes );

    const AbcA::DataType &dtype = m_header->getDataType();
    uint8_t extent = dtype.getExtent();
    if ( dtype.getPod() == kStringPOD )
    {
        std::string *strings
            = reinterpret_cast<std::string*>( oSampleBytes );
        ABCA_ASSERT( strings != NULL,
                     "Invalid data buffer in scalar read sample" );
        
        if ( extent == 1 )
        {
            ReadString( iGroup, iSampleName, *strings );
        }
        else
        {
            ReadStrings( iGroup, iSampleName, dtype.getExtent(), strings );
        }
    }
    else if ( dtype.getPod() == kWstringPOD )
    {
        std::wstring *wstrings
            = reinterpret_cast<std::wstring*>( oSampleBytes );
        ABCA_ASSERT( wstrings != NULL,
                     "Invalid data buffer in scalar read sample" );
        
        if ( extent == 1 )
        {
            ReadWstring( iGroup, iSampleName, *wstrings );
        }
        else
        {
            ReadWstrings( iGroup, iSampleName, dtype.getExtent(), wstrings );
        }
            
        ReadWstrings( iGroup, iSampleName, dtype.getExtent(), wstrings );
    }
    else
    {
        assert( m_fileDataType >= 0 );
        assert( m_nativeDataType >= 0 );

        if ( extent == 1 )
        {
            ReadScalar( iGroup, iSampleName, m_fileDataType, m_nativeDataType,
                oSampleBytes );
        }
        else
        {
            size_t readElements = 0;
            ReadSmallArray( iGroup, iSampleName, m_fileDataType,
                m_nativeDataType, extent, readElements, oSampleBytes );
        }
    }
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace AbcCoreHDF5
} // End namespace Alembic

