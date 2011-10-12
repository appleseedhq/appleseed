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

#ifndef _Alembic_AbcCoreAbstract_ReadArraySampleCache_h_
#define _Alembic_AbcCoreAbstract_ReadArraySampleCache_h_

#include <Alembic/AbcCoreAbstract/Foundation.h>
#include <Alembic/AbcCoreAbstract/ArraySample.h>

namespace Alembic {
namespace AbcCoreAbstract {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
//! A ReadArraySampleID is a bundle that contains a pointer to the
//! \ref ArraySample itself, along with the sample's key.
class ReadArraySampleID
{
public:
    //! By convention, we define the typedef this_type. This
    //! is used by the unspecified-bool-type cast below.
    typedef ReadArraySampleID this_type;
    
    //! Default constructor creates empty ID
    //! ...
    ReadArraySampleID() {}

    //! Explicit constructor creates ID with key and sample
    ReadArraySampleID( const ArraySample::Key &iSampleKey,
                       ArraySamplePtr iSample )
      : m_sampleKey( iSampleKey ),
        m_sample( iSample ) {}

    //! Default copy constructor
    //! Default assignment operator.

    //! Return the sample key
    //! ...
    const ArraySample::Key &getKey() const
    { return m_sampleKey; }

    //! Return the sample itself.
    //! ...
    ArraySamplePtr getSample() const
    { return m_sample; }

    //! Return whether or not this read sample is valid
    //! ...
    bool valid() const
    {
        return ( m_sample && m_sample->valid() );
    }

    //! Unspecified bool type cast
    //! ...
    ALEMBIC_OPERATOR_BOOL( valid() );

private:
    ArraySample::Key m_sampleKey;
    ArraySamplePtr m_sample;
};


//-*****************************************************************************
//-*****************************************************************************
//-*****************************************************************************
// READ ARRAY SAMPLE CACHE
//-*****************************************************************************
//-*****************************************************************************
//-*****************************************************************************

//-*****************************************************************************
//! Alembic caches array samples based on a Murmur3 128bit checksum key.
//! This is an abstract interface to these caches, which can be implemented
//! in any number of ways.
class ReadArraySampleCache
    : private boost::noncopyable
    , public boost::enable_shared_from_this<ReadArraySampleCache>
{
public:
    //! Virtual destructor
    //! ...
    virtual ~ReadArraySampleCache();

    //! If it finds the entry, return a valid pointer to it which
    //! is expected to lock the entry in the cache until the pointer
    //! is dereferenced.
    virtual ReadArraySampleID find( const ArraySample::Key &iKey ) = 0;

    //! Store an entry given an explicit set of storage.
    //! The magnificent flexibility of the shared_ptr class makes
    //! it possible for an ArraySamplePtr to contain its own destructor
    //! as a custom deleter, and thus we can use ArraySamplePtrs for
    //! both reference and ownership, depending on the deleter.
    //! In this case, it is assumed that iSamp represents
    //! "owned" data, rather than a reference.  The data will not
    //! be copied, but rather this sample will be stored directly
    //! using the passed shared_ptr.
    virtual ReadArraySampleID store( const ArraySample::Key &iKey,
                                     ArraySamplePtr iSamp ) = 0;
};

//-*****************************************************************************
typedef boost::shared_ptr<ReadArraySampleCache> ReadArraySampleCachePtr;

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcCoreAbstract
} // End namespace Alembic

#endif
