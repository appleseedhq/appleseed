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
// *       Neither the name of Industrial Light & Magic nor the names of
// its contributors may be used to endorse or promote products derived
// from this software without specific prior written permission.
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

#ifndef _Alembic_Util_Dimensions_h_
#define _Alembic_Util_Dimensions_h_

#include <Alembic/Util/Foundation.h>
#include <Alembic/Util/PlainOldDataType.h>

namespace Alembic {
namespace Util {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
template <class T>
class BaseDimensions
{
private:
    typedef std::vector<T> SizeVec;
    SizeVec m_vector;

public:
    // Default is for a rank-0 dimension.
    BaseDimensions()
      : m_vector()
    {}

    // When you specify a single thing, you're specifying a rank-1
    // dimension of a certain size.
    explicit BaseDimensions( const T& t )
      : m_vector( 1, t )
    {}

    BaseDimensions( const BaseDimensions &copy )
      : m_vector( copy.m_vector )
    {}

    template <class Y>
    BaseDimensions( const BaseDimensions<Y> &copy )
    {
        m_vector.resize( copy.rank() );
        for ( size_t i = 0; i < copy.rank(); ++i )
        {
            Y val = copy[i];
            m_vector[i] = static_cast<T>( val );
        }
    }

    BaseDimensions& operator=( const BaseDimensions &copy )
    {
        m_vector = copy.m_vector;
        return *this;
    }

    template <class Y>
    BaseDimensions& operator=( const BaseDimensions<Y> &copy )
    {
        m_vector.resize( copy.rank() );
        for ( size_t i = 0; i < copy.rank(); ++i )
        {
            Y val = copy[i];
            m_vector[i] = static_cast<T>( val );
        }
        return *this;
    }

    size_t rank() const { return m_vector.size(); }
    void setRank( size_t r )
    {
        size_t oldSize = m_vector.size();
        m_vector.resize( r );
        for ( size_t s = oldSize; s < r; ++s )
        {
            m_vector[s] = ( T )0;
        }
    }

    T &operator[]( size_t i )
    { return m_vector[i]; }

    const T &operator[]( size_t i ) const
    { return m_vector[i]; }

    T *rootPtr() { return ( T * )( &( m_vector.front() ) ); }
    const T *rootPtr() const
    { return ( const T * )( &( m_vector.front() ) ); }

    size_t numPoints() const
    {
        if ( m_vector.size() == 0 ) { return 0; }
        else
        {
            size_t npoints = 1;
            for ( size_t i = 0 ; i < m_vector.size() ; i++ )
            {
                npoints *= (size_t)m_vector[i];
            }
            return npoints;
        }
    }
};

//-*****************************************************************************
template <class T, class Y>
bool operator==( const BaseDimensions<T> &a, const BaseDimensions<Y> &b )
{
    size_t aRank = a.rank();
    size_t bRank = b.rank();
    if ( aRank != bRank ) { return false; }

    if ( sizeof( Y ) > sizeof( T ) )
    {
        for ( size_t d = 0; d < aRank; ++d )
        {
            if ( static_cast<Y>( a[d] ) !=
                 static_cast<Y>( b[d] ) ) { return false; }
        }
    }
    else
    {
        for ( size_t d = 0; d < aRank; ++d )
        {
            if ( static_cast<T>( a[d] ) !=
                 static_cast<T>( b[d] ) ) { return false; }
        }
    }

    return true;
}

//-*****************************************************************************
template <class T, class Y>
inline bool operator!=( const BaseDimensions<T> &a,
                        const BaseDimensions<Y> &b )
{
    return !( a == b );
}

//-*****************************************************************************
template <class T>
std::ostream &operator<<( std::ostream &ostr, const BaseDimensions<T> &a )
{
    ostr << "{";
    for ( size_t i = 0; i < a.rank(); ++i )
    {
        ostr << a[i];
        if ( i != a.rank()-1 )
        {
            ostr << ", ";
        }
    }
    ostr << "}";
    return ostr;
}

//-*****************************************************************************
typedef BaseDimensions<size_t> Dimensions;

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace Util
} // End namespace Alembic

#endif
