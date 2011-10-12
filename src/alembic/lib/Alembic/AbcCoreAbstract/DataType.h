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

#ifndef _Alembic_AbcCoreAbstract_DataType_h_
#define _Alembic_AbcCoreAbstract_DataType_h_

#include <Alembic/AbcCoreAbstract/Foundation.h>

namespace Alembic {
namespace AbcCoreAbstract {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
//! The DataType class is a description of how an element of a sample in a
//! Scalar or an Array property is stored. It does not contain an interpretation
//! this is left to the metadata of the properties themselves.
class DataType : boost::totally_ordered<DataType>
{
public:
    //! Default constructor
    //! Sets the DataType to an unknown DataType with extent 0.
    //! This is obviously an invalid storage description, and is used in
    //! cases where we need to indicated that a DataType could not be
    //! determined.
    DataType()
      : m_pod( kUnknownPOD ), m_extent( 0 ) {}

    //! Explicit constructor.
    //! Takes a pod and an extent.
    //! By default the extent is 1.
    //! For String and Wstring types, the extent _must_ be 1.
    explicit DataType( PlainOldDataType iPod, uint8_t iExtent = 1 )
      : m_pod( iPod ), m_extent( iExtent ) {}

    //! Default copy constructor used.
    //! Default assignment operator used.

    //! Return the PlainOldDataType enum
    //! ...
    PlainOldDataType getPod() const { return m_pod; }

    //! Set the PlainOldDataType
    //! ...
    void setPod( PlainOldDataType iPod ) { m_pod = iPod; }

    //! Return the 8-bit extent
    //! ...
    uint8_t getExtent() const { return m_extent; }

    //! Set the 8-bit extent
    //! ...
    void setExtent( uint8_t iExtent ) { m_extent = iExtent; }

    //! Returns the number of bytes occupied by a single datum. (element)
    //! The assumption that each element has a fixed size in memory is a
    //! core assumption in Alembic.
    //!
    //! String DataTypes are a troublesome problem. A single string datum
    //! does not have a fixed number of bytes associated with it. So we
    //! are returning, here, the size of the std::string and std::wstring
    //! datatypes, respectively.
    size_t getNumBytes() const
    {
        return PODNumBytes( m_pod ) * ( size_t )m_extent;
    }

    //! Equality operator
    //! ...
    bool operator==( const DataType &b ) const
    {
        return ( ( m_pod == b.m_pod ) &&
                 ( m_extent == b.m_extent ) );
    }

    //-*************************************************************************
    //! Returns whether one datatype is lexigraphically "less" than
    //! another - this has meaning only so that DataType instances can
    //! be meaningfully sorted.
    bool operator<( const DataType &b ) const
    {
        if ( m_pod < b.m_pod ) { return true; }
        else if ( m_pod > b.m_pod ) { return false; }
        else { return ( m_extent < b.m_extent ); }
    }

private:
    //! An Enum indicating which PlainOldDataType is our
    //! super-storage-class.
    PlainOldDataType m_pod;

    //! An 8-bit extent indicating the cardinality of
    //! a single element of data.
    uint8_t m_extent;
};

//-*****************************************************************************
//! Outputs DataType to a std::ostream
//! Makes use of PlainOldDataType's string conversion functions
inline std::ostream &operator<<( std::ostream &ostr, const DataType &a )
{
    ostr << PODName( a.getPod() );
    if ( a.getExtent() > 1 )
    {
        ostr << "[" << ( size_t )a.getExtent() << "]";
    }
    return ostr;
}

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcCoreAbstract
} // End namespace Alembic

#endif
