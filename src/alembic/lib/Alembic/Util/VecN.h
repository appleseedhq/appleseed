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

#ifndef _Alembic_Util_VecN_h_
#define _Alembic_Util_VecN_h_

#include <Alembic/Util/Foundation.h>
#include <Alembic/Util/PlainOldDataType.h>

namespace Alembic {
namespace Util {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
//! The VecN class is explicitly designed to handle "POD" types, which are
//! defined by the Alembic::Util library in the PlainOldDataType.h file.
//! This is enforced by the compiler based on the use of the
//! PODTraitsFromType<T> usage in this class.
template <class T, std::size_t N>
class VecN
    : public boost::array<T,N>
    , boost::addable< VecN<T,N>
    , boost::subtractable< VecN<T,N>
    , boost::multipliable< VecN<T,N>
    , boost::dividable< VecN<T,N>
      > > > >
{
public:
    typedef VecN<T,N> this_type;
    typedef PODTraitsFromType<T> pod_traits_type;
    
    VecN()
    {
        std::fill( this->begin(), this->end(),
                   pod_traits_type::default_value() );
    }
    
    explicit VecN( const T &iVal )
    {
        std::fill( this->begin(), this->end(), iVal );
    }

    explicit VecN( const T iVals[] )
    {
        std::copy( iVals, iVals + N, this->begin() );
    }

    //! Explicitly declaring copy constructor, despite behavior being default.
    //! This helps disambiguate the templated "other value type" copy
    //! constructor below.
    VecN( const VecN<T,N> &iCopy )
      : boost::array<T,N>( iCopy ) {}

    //! "other value type" copy constructor.
    //! ...
    template <class S>
    VecN( const VecN<S,N> &iCvtCopy )
    {
        std::copy( iCvtCopy.begin(), iCvtCopy.end(),
                   this->begin() );
    }

    //! Explicitly declaring assignment operator, despite behavior
    //! being default. This helps disambiguate the templated "other value type"
    //! assignment operator below.
    VecN& operator=( const VecN<T,N> &iCopy )
    {
        boost::array<T,N>::operator=( iCopy );
        return *this;
    }

    //! "other value type" assignment operator.
    //! ...
    template <class S>
    VecN& operator=( const VecN<S,N> &iCopy )
    {
        boost::array<T,N>::operator=( iCopy );
        return *this;
    }
    
    VecN& operator=( const T iVals[] )
    {
        std::copy( iVals, iVals + N, this->begin() );
        return *this;
    }

    VecN& operator+=( const VecN<T,N> &iRhs )
    {
        for ( std::size_t i = 0; i < N; ++i )
        {
            this->elems[i] += iRhs.elems[i];
        }
        return *this;
    }

    VecN& operator-=( const VecN<T,N> &iRhs )
    {
        for ( std::size_t i = 0; i < N; ++i )
        {
            this->elems[i] -= iRhs.elems[i];
        }
        return *this;
    }

    VecN& operator*=( const VecN<T,N> &iRhs )
    {
        for ( std::size_t i = 0; i < N; ++i )
        {
            this->elems[i] *= iRhs.elems[i];
        }
        return *this;
    }

    VecN& operator/=( const VecN<T,N> &iRhs )
    {
        for ( std::size_t i = 0; i < N; ++i )
        {
            // Kinda dangerous, but whatever.
            this->elems[i] /= iRhs.elems[i];
        }
        return *this;
    }
};

//-*****************************************************************************
template <class T, std::size_t N>
inline std::ostream &operator<<( std::ostream &ostr, const VecN<T,N> &iVecN )
{
    ostr << "{";
    for ( std::size_t i = 0; i < N; ++i )
    {
        ostr << iVecN[i];
        if ( i < N-1 ) { ostr << ","; }
    }
    ostr << "}";
    return ostr;
}

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace Util
} // End namespace Alembic

#endif
