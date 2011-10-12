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

#include <Alembic/AbcCoreAbstract/ScalarSample.h>
#include <typeinfo>

namespace Alembic {
namespace AbcCoreAbstract {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
template <class T>
static inline bool equalWithRelAbsError( const T &iA, const T &iB,
                                         double iEpsilon )
{
    return iA == iB;
}

//-*****************************************************************************
template <>
inline bool equalWithRelAbsError<float16_t>( const float16_t &iA,
                                             const float16_t &iB,
                                             double iEpsilon )
{
    return fabs( ( double )( iA - iB ) ) < iEpsilon;
}

//-*****************************************************************************
template <>
inline bool equalWithRelAbsError<float32_t>( const float32_t &iA,
                                             const float32_t &iB,
                                             double iEpsilon )
{
    return fabs( ( double )( iA - iB ) ) < iEpsilon;
}

//-*****************************************************************************
template <>
inline bool equalWithRelAbsError<float64_t>( const float64_t &iA,
                                             const float64_t &iB,
                                             double iEpsilon )
{
    return fabs( ( double )( iA - iB ) ) < iEpsilon;
}

//-*****************************************************************************
template <class T>
class TypedScalarSampleData : public ScalarSample::Data
{
public:
    TypedScalarSampleData( size_t N )
      : m_data( N )
    {
        for ( size_t i = 0; i < N; ++i )
        {
            m_data[i] = PODTraitsFromType<T>::default_value();
        }
    }

    virtual void setToDefault()
    {
        for ( size_t i = 0, N = m_data.size(); i < N; ++i )
        {
            m_data[i] = PODTraitsFromType<T>::default_value();
        }
    }

    // THIS IS NOT MULTITHREAD SAFE, but currently is only
    // used during write operations.
    virtual void copyFrom( const void *iData )
    {
        const T *iDataT = reinterpret_cast<const T *>( iData );
        for ( size_t i = 0, N = m_data.size(); i < N; ++i )
        {
            m_data[i] = iDataT[i];
        }
    }

    virtual bool equalTo( const void *iData ) const
    {
        const T *iDataT = reinterpret_cast<const T *>( iData );
        for ( size_t i = 0, N = m_data.size(); i < N; ++i )
        {
            if ( m_data[i] != iDataT[i] ) { return false; }
        }
        return true;
    }

    virtual bool equalEpsilon( const void *iData, double iEpsilon ) const
    {
        const T *iDataT = reinterpret_cast<const T *>( iData );
        for ( size_t i = 0, N = m_data.size(); i < N; ++i )
        {
            if ( !equalWithRelAbsError( m_data[i], iDataT[i], iEpsilon ) )
            {
                return false;
            }
        }
        return true;
    }

    virtual bool lessThan( const void *iData ) const
    {
        const T *iDataT = reinterpret_cast<const T *>( iData );
        for ( size_t i = 0, N = m_data.size(); i < N; ++i )
        {
            if ( m_data[i] < iDataT[i] ) { return true; }
            else if ( m_data[i] > iDataT[i] ) { return false; }
        }
        return false;
    }

    virtual const void *getData() const
    {
        return reinterpret_cast<const void *>( &m_data.front() );
    }

protected:
    std::vector<T> m_data;
};


//-*****************************************************************************
//-*****************************************************************************
//-*****************************************************************************
ScalarSample::ScalarSample( const DataType &iDataType )
  : m_dataType( iDataType )
{
    size_t N = m_dataType.getExtent();
    ABCA_ASSERT( m_dataType.getPod() != kUnknownPOD && N > 0,
                 "Degenerate data type in scalar sample" );
    switch ( m_dataType.getPod() )
    {
    case kBooleanPOD:
        m_data.reset( new TypedScalarSampleData<bool_t>( N ) ); break;
    case kUint8POD:
        m_data.reset( new TypedScalarSampleData<uint8_t>( N ) ); break;
    case kInt8POD:
        m_data.reset( new TypedScalarSampleData<int8_t>( N ) ); break;
    case kUint16POD:
        m_data.reset( new TypedScalarSampleData<uint16_t>( N ) ); break;
    case kInt16POD:
        m_data.reset( new TypedScalarSampleData<int16_t>( N ) ); break;
    case kUint32POD:
        m_data.reset( new TypedScalarSampleData<uint32_t>( N ) ); break;
    case kInt32POD:
        m_data.reset( new TypedScalarSampleData<int32_t>( N ) ); break;
    case kUint64POD:
        m_data.reset( new TypedScalarSampleData<uint64_t>( N ) ); break;
    case kInt64POD:
        m_data.reset( new TypedScalarSampleData<int64_t>( N ) ); break;
    case kFloat16POD:
        m_data.reset( new TypedScalarSampleData<float16_t>( N ) ); break;
    case kFloat32POD:
        m_data.reset( new TypedScalarSampleData<float32_t>( N ) ); break;
    case kFloat64POD:
        m_data.reset( new TypedScalarSampleData<float64_t>( N ) ); break;
    case kStringPOD:
        m_data.reset( new TypedScalarSampleData<string>( N ) ); break;
    case kWstringPOD:
        m_data.reset( new TypedScalarSampleData<wstring>( N ) ); break;
    default:
        ABCA_THROW( "Unknown datatype in ScalarSample: " << m_dataType );
        break;
    }
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace AbcCoreAbstract
} // End namespace Alembic
