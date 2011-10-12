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

#ifndef _Alembic_AbcCoreAbstract_ArraySample_h_
#define _Alembic_AbcCoreAbstract_ArraySample_h_

#include <Alembic/AbcCoreAbstract/Foundation.h>
#include <Alembic/AbcCoreAbstract/ArraySampleKey.h>
#include <Alembic/AbcCoreAbstract/DataType.h>

namespace Alembic {
namespace AbcCoreAbstract {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
//! The ArraySample class is a reference to a block of memory corresponding
//! to an array of instances of DataTypes. The array may be multi-rank, with
//! different sizes in each dimension, but with its data ultimately stored
//! contiguously. The class is basically just a bundle of a memory address,
//! stored as a void*, a DataType, and a Dimension.
//!
//! The ArraySample itself does not pretend to own the data referred to
//! memory address "data". It is just a reference. For data retention mgmt,
//! see the note on \ref ArraySamplePtr below.
class ArraySample
{
public:
    typedef ArraySample this_type;
    typedef ArraySampleKey Key;
    typedef Key key_type;

    //! Default constructor creates NULL bytes with degenerate dimensions.
    //! ...
    ArraySample()
      : m_data( NULL )
      , m_dataType()
      , m_dimensions() {}

    //! Explicit constructor takes bytes and dims by reference
    //! and creates its own reference to them.
    ArraySample( const void * iData,
                 const DataType &iDataType,
                 const Dimensions & iDims )
      : m_data( iData )
      , m_dataType( iDataType )
      , m_dimensions( iDims ) {}

    //! Using Default Copy Constructor
    //! Using Default Assignment Operator

    //! Return the data as a raw pointer
    //! ...
    const void* getData() const { return m_data; }

    //! Return the datatype.
    //! ...
    const DataType &getDataType() const { return m_dataType; }

    //! Return the dimensions
    //! ...
    const Dimensions& getDimensions() const { return m_dimensions; }

    //! Return the "size", which is getDimensions().numPoints()
    //! ...
    size_t size() const { return m_dimensions.numPoints(); }

    //! Compute the Key.
    //! This is a calculation.
    Key getKey() const;

    //! Return if it is valid.
    //! An empty ArraySample is valid.
    //! however, an ArraySample that is empty and has a scalar
    //! dimension is invalid. This is how we discriminate between
    //! setting a sample of length zero (useful in particle systems)
    //! vs. indicating an invalid sample (NULL).
    bool valid() const
    {
        return ( m_dataType.getPod() != kUnknownPOD ) &&
            !( m_data == NULL && m_dimensions.rank() < 1 );
    }

    //! Reset the array sample to an empty, invalid state.
    //! Basically the same as calling *this = ArraySample();
    void reset()
    {
        m_data = NULL;
        m_dataType = DataType();
        m_dimensions = Dimensions();
    }

private:
    const void *m_data;
    DataType m_dataType;
    Dimensions m_dimensions;
};

//-*****************************************************************************
//! The ArraySamplePtr can be used not only to share this ArraySample, but
//! also to manage the data referred to by the memory address in the pointer,
//! by way of a custom deleter. In this manner, ArraySample and ArraySamplePtr
//! can be used both as a reference to data and as an explicit ownership of
//! data. This greatly reduces the redundancy of this library's code.
typedef boost::shared_ptr<ArraySample> ArraySamplePtr;

//-*****************************************************************************
//! When creating an actual buffer for reading an array sample into,
//! we need to allocate an array of some number of bytes, and then delete
//! it with a special deleter. This function will return an array sample
//! that is managed in this way.
//! Dimensions tells us how many instances of the DataType to create
//! DataType tells us what the instance is - and this works for
//! pretty much every case, including std::string and std::wstring.
ArraySamplePtr AllocateArraySample( const DataType &iDtype,
                                    const Dimensions &iDims );

//-*****************************************************************************
//-*****************************************************************************
//-*****************************************************************************
// HELPER STUFF!
//-*****************************************************************************
//-*****************************************************************************
//-*****************************************************************************

//-*****************************************************************************
template <class T>
struct TArrayDeleter
{
    void operator()( void *memory ) const
    {
        ArraySample *arraySample = static_cast<ArraySample*>( memory );
        if ( arraySample )
        {
            T *data = reinterpret_cast<T*>(
                const_cast<void*>( arraySample->getData() ) );

            // Delete[] NULL is okay.
            delete[] data;
        }
        delete arraySample;
    }
};

//-*****************************************************************************
template <class T>
ArraySamplePtr TAllocateArraySample( size_t iDataTypeExtent,
                                     const Dimensions &iDims )
{
    DataType dtype( PODTraitsFromType<T>::pod_enum, iDataTypeExtent );
    size_t numPODs = iDims.numPoints() * iDataTypeExtent;
    if ( numPODs > 0 )
    {
        T *data = new T[numPODs];
        ArraySamplePtr ret(
            new ArraySample( reinterpret_cast<const void *>( data ),
                             dtype, iDims ),
            TArrayDeleter<T>() );
        return ret;
    }
    else
    {
        ArraySamplePtr ret(
            new ArraySample( ( const void * )NULL,
                             dtype, iDims ) );
        return ret;
    }
}

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcCoreAbstract
} // End namespace Alembic

#endif
