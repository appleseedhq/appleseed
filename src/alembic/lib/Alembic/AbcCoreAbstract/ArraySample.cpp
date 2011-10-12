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

#include <Alembic/AbcCoreAbstract/ArraySample.h>
#include <Alembic/Util/Murmur3.h>

namespace Alembic {
namespace AbcCoreAbstract {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
ArraySample::Key ArraySample::getKey() const
{

    // Depending on data type, loop over everything.
    size_t numPoints = m_dimensions.numPoints();
    size_t numPods = m_dataType.getExtent() * numPoints;
    size_t numBytes = m_dataType.getNumBytes() * numPoints;

    ArraySample::Key k;
    k.numBytes = numBytes;
    k.origPOD = m_dataType.getPod();
    k.readPOD = k.origPOD;

    switch ( m_dataType.getPod() )
    {
    case kBooleanPOD:
    case kUint8POD:
    case kInt8POD:
    case kUint16POD:
    case kInt16POD:
    case kUint32POD:
    case kInt32POD:
    case kUint64POD:
    case kInt64POD:
    case kFloat16POD:
    case kFloat32POD:
    case kFloat64POD:
    {
        MurmurHash3_x64_128( m_data, numBytes, PODNumBytes(m_dataType.getPod()),
            k.digest.words );
    }
    break;

    case kStringPOD:
    {
        std::vector <int8_t> v;
        for ( size_t j = 0; j < numPods; ++j )
        {
            const std::string &str =
                static_cast<const std::string*>( m_data )[j];

            size_t strLen = str.length();
            for ( size_t k = 0; k < strLen; ++k )
            {
                v.push_back(str[k]);
            }

            // append a 0 for the NULL seperator character
            v.push_back(0);
        }

        int8_t * vptr = NULL;
        if ( !v.empty() )
            vptr = &(v.front());

        MurmurHash3_x64_128( vptr, v.size(), sizeof(int8_t), k.digest.words );
    }
    break;

    case kWstringPOD:
    {
        std::vector <int32_t> v;
        for ( size_t j = 0; j < numPods; ++j )
        {
            const std::wstring &wstr =
                static_cast<const std::wstring*>( m_data )[j];

            std::vector <int32_t> v(wstr.length());
            size_t wlen = wstr.length();
            for (size_t k = 0; k < wlen; ++k)
            {
                v[k] = wstr[k];
            }

            // append a 0 for the NULL seperator character
            v.push_back(0);
        }

        int32_t * vptr = NULL;
        if ( !v.empty() )
            vptr = &(v.front());

        MurmurHash3_x64_128( vptr, v.size(), sizeof(int32_t), k.digest.words );
    }
    break;

    default:
        ABCA_THROW( "Can't calculate key for: " << m_dataType ); break;

    }

    return k;
}

//-*****************************************************************************
ArraySamplePtr AllocateArraySample( const DataType &iDtype,
                                    const Dimensions &iDims )
{
    switch ( iDtype.getPod() )
    {
    case kBooleanPOD:
        return TAllocateArraySample<bool_t>( iDtype.getExtent(), iDims );

    case kUint8POD:
        return TAllocateArraySample<uint8_t>( iDtype.getExtent(), iDims );
    case kInt8POD:
        return TAllocateArraySample<int8_t>( iDtype.getExtent(), iDims );

    case kUint16POD:
        return TAllocateArraySample<uint16_t>( iDtype.getExtent(), iDims );
    case kInt16POD:
        return TAllocateArraySample<int16_t>( iDtype.getExtent(), iDims );

    case kUint32POD:
        return TAllocateArraySample<uint32_t>( iDtype.getExtent(), iDims );
    case kInt32POD:
        return TAllocateArraySample<int32_t>( iDtype.getExtent(), iDims );

    case kUint64POD:
        return TAllocateArraySample<uint64_t>( iDtype.getExtent(), iDims );
    case kInt64POD:
        return TAllocateArraySample<int64_t>( iDtype.getExtent(), iDims );

    case kFloat16POD:
        return TAllocateArraySample<float16_t>( iDtype.getExtent(), iDims );
    case kFloat32POD:
        return TAllocateArraySample<float32_t>( iDtype.getExtent(), iDims );
    case kFloat64POD:
        return TAllocateArraySample<float64_t>( iDtype.getExtent(), iDims );

    case kStringPOD:
        return TAllocateArraySample<string>( iDtype.getExtent(), iDims );
    case kWstringPOD:
        return TAllocateArraySample<wstring>( iDtype.getExtent(), iDims );

    default:
        return ArraySamplePtr();
    }
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace AbcCoreAbstract
} // End namespace Alembic
