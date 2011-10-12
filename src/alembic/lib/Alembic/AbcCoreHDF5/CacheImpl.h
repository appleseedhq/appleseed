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

#ifndef _Alembic_AbcCoreHDF5_CacheImpl_h_
#define _Alembic_AbcCoreHDF5_CacheImpl_h_

#include <Alembic/AbcCoreHDF5/Foundation.h>

namespace Alembic {
namespace AbcCoreHDF5 {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
typedef boost::weak_ptr<AbcA::ArraySample> ArraySampleWeakPtr;

//-*****************************************************************************
class CacheImpl;
typedef boost::shared_ptr<CacheImpl> CacheImplPtr;
typedef boost::weak_ptr<CacheImpl> CacheImplWeakPtr;

//-*****************************************************************************
//! This class is underimplemented. It ought to allow limits on storage.
//! Todo!
//! THIS CLASS IS NOT MULTITHREAD SAFE
class CacheImpl : public AbcA::ReadArraySampleCache
{
public:
    //-*************************************************************************
    // PUBLIC INTERFACE
    //-*************************************************************************
    CacheImpl();
    
    virtual ~CacheImpl();

    virtual AbcA::ReadArraySampleID
    find( const AbcA::ArraySample::Key &iKey );
    
    virtual AbcA::ReadArraySampleID
    store( const AbcA::ArraySample::Key &iKey,
           AbcA::ArraySamplePtr iBytes );

private:
    //-*************************************************************************
    // INTERNAL STORAGE
    // Using the unordered map (hash map)
    //-*************************************************************************
    struct Record
    {
        Record(){}
        Record( AbcA::ArraySamplePtr iGivenPtr,
                AbcA::ArraySamplePtr iDeleterPtr )
          : given( iGivenPtr ),
            weakDeleter( iDeleterPtr )
        {
            ABCA_ASSERT( iGivenPtr && iDeleterPtr,
                         "Cannot record null records in CacheImpl" );
            ABCA_ASSERT( iGivenPtr.get() == iDeleterPtr.get(),
                         "Given Ptr must match contents of DeleterPtr" );
        }
        
        // This is the original, given Array Sample Ptr.
        AbcA::ArraySamplePtr given;

        // This is the one we've created which corresponds
        // to this record. It has the same pointer as above,
        // but has a special deleter that will instead tell this
        // class to erase this record.
        // This is how we facilitate cache management.
        // Also: I LOVE SMART PTRS
        // We don't store it directly because we want the destructor
        // to get called whenever we're not using this in the world anymore.
        ArraySampleWeakPtr weakDeleter;
    };

public:
    class RecordDeleter;

private:
    friend class RecordDeleter;
    AbcA::ArraySamplePtr lock( const AbcA::ArraySample::Key &iKey,
                               AbcA::ArraySamplePtr iSamp );
    void unlock( const AbcA::ArraySample::Key &iKey );

public:
    class RecordDeleter
    {
    private:
        friend class CacheImpl;
        RecordDeleter( const AbcA::ArraySample::Key &iKey,
                       CacheImplPtr iCache )
          : m_key( iKey ),
            m_cache( iCache ) {}

    public:
        void operator()( AbcA::ArraySample *iPtr )
        {
            CacheImplPtr cachePtr = m_cache.lock();
            if ( cachePtr )
            {
                cachePtr->unlock( m_key );
            }
        }

    private:
        AbcA::ArraySample::Key m_key;
        CacheImplWeakPtr m_cache;
    };

private:
    typedef AbcA::UnorderedMapUtil<Record>::umap_type Map;
    typedef AbcA::UnorderedMapUtil<AbcA::ArraySamplePtr>::umap_type
    UnlockedMap;

    Map m_lockedMap;
    UnlockedMap m_unlockedMap;
};

//-*****************************************************************************
AbcA::ReadArraySampleCachePtr MakeCacheImplPtr();

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcCoreHDF5
} // End namespace Alembic

#endif
