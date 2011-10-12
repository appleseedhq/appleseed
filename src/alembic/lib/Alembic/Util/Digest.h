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

#ifndef _Alembic_Util_Digest_h_
#define _Alembic_Util_Digest_h_

#include <Alembic/Util/Foundation.h>
#include <boost/format.hpp>
#include <boost/cstdint.hpp>

namespace Alembic {
namespace Util {
namespace ALEMBIC_VERSION_NS {

using boost::uint8_t;
using boost::uint64_t;

//-*************************************************************************
// Digest class. This is a 128 bit hash key produced by our hashing algorithm.
// It is totally ordered, by way of the boost::totally_ordered
// operator template.
struct Digest : public boost::totally_ordered<Digest>
{
    union
    {
        uint8_t d[16];
        uint64_t words[2];
    };

    Digest() { words[0] = words[1] = 0; }
    Digest( const Digest &copy )
    {
        words[0] = copy.words[0];
        words[1] = copy.words[1];
    }

    Digest &operator=( const Digest &copy )
    {
        words[0] = copy.words[0];
        words[1] = copy.words[1];
        return *this;
    }

    uint8_t& operator[]( size_t i ) { return d[i]; }
    uint8_t operator[]( size_t i ) const { return d[i]; }

    void print( std::ostream &ostr ) const
    {
        for ( int i = 0; i < 16; ++i )
        {
            ostr << ( boost::format( "%02x" ) % ( int )(d[i]) );
        }
    }

    std::string str() const
    {
        std::stringstream sstr;
        print( sstr );
        return sstr.str();
    }

    //-*************************************************************************
    // ORDERING AND COMPARISON OPERATORS
    //-*************************************************************************
    bool operator==( const Digest &iRhs ) const
    {
        return ( ( words[0] == iRhs.words[0] ) &&
                 ( words[1] == iRhs.words[1] ) );
    }

    bool operator<( const Digest &iRhs ) const
    {
        return ( words[0] < iRhs.words[0] ? true :
                 ( words[0] > iRhs.words[0] ? false :
                   ( words[1] < iRhs.words[1] ) ) );
    }
};

//-*****************************************************************************
inline std::ostream &operator<<( std::ostream &ostr, const Digest &a )
{
    a.print( ostr );
    return ostr;
}

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace Util
} // End namespace Alembic

#endif
