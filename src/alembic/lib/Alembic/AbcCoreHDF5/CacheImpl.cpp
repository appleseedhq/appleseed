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

#include <Alembic/AbcCoreHDF5/CacheImpl.h>

namespace Alembic {
namespace AbcCoreHDF5 {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
CacheImpl::CacheImpl()
{
    // Nothing!
}

//-*****************************************************************************
CacheImpl::~CacheImpl()
{
    // Nothing!
}

//-*****************************************************************************
AbcA::ReadArraySampleID
CacheImpl::find( const AbcA::ArraySample::Key &iKey )
{
    // Check the locked map! If we have already locked it, just return
    // it locked!
    Map::iterator foundIter = m_lockedMap.find( iKey );
    if ( foundIter != m_lockedMap.end() )
    {
        AbcA::ArraySamplePtr deleterPtr =
            (*foundIter).second.weakDeleter.lock();
        assert( deleterPtr );

        return AbcA::ReadArraySampleID( iKey, deleterPtr );
    }

    // If we get here, we're not in the locked map.
    // Check the unlocked one.
    UnlockedMap::iterator uFoundIter = m_unlockedMap.find( iKey );
    if ( uFoundIter != m_unlockedMap.end() )
    {
        AbcA::ArraySamplePtr givenSampPtr = (*uFoundIter).second;
        assert( givenSampPtr );

        AbcA::ArraySamplePtr deleterPtr = lock( iKey, givenSampPtr );
        assert( deleterPtr );
        assert( givenSampPtr.get() == deleterPtr.get() );

        // Remove it from the unlocked map.
        m_unlockedMap.erase( uFoundIter );

        return AbcA::ReadArraySampleID( iKey, deleterPtr );
    }

    // Didn't find it, return null sample ID
    return AbcA::ReadArraySampleID();
}

//-*****************************************************************************
AbcA::ReadArraySampleID
CacheImpl::store( const AbcA::ArraySample::Key &iKey,
                  AbcA::ArraySamplePtr iSamp )
{
    ABCA_ASSERT( iSamp, "Cannot store a null sample" );

    // Check to see if we already have it.
    AbcA::ReadArraySampleID foundID = find( iKey );
    if ( foundID )
    {
        return foundID;
    }

    // Lock it.
    AbcA::ArraySamplePtr deleterPtr = lock( iKey, iSamp );
    assert( deleterPtr );

    return AbcA::ReadArraySampleID( iKey, deleterPtr );
}

//-*****************************************************************************
AbcA::ArraySamplePtr
CacheImpl::lock( const AbcA::ArraySample::Key &iKey,
                 AbcA::ArraySamplePtr iGivenPtr )
{
    assert( iGivenPtr );

    // Lock it by creating a cache-managing deleter.
    // This RecordDeleter simply tells this cache instance to nuke
    // us.
    RecordDeleter deleter( iKey,
                           boost::dynamic_pointer_cast<CacheImpl,
                           AbcA::ReadArraySampleCache>( shared_from_this() ) );
    AbcA::ArraySamplePtr deleterPtr( iGivenPtr.get(), deleter );

    // Add it to the locked map
    Record record( iGivenPtr, deleterPtr );
    m_lockedMap[iKey] = record;

    return deleterPtr;
}

//-*****************************************************************************
void CacheImpl::unlock( const AbcA::ArraySample::Key &iKey )
{
    Map::iterator foundIter = m_lockedMap.find( iKey );
    if ( foundIter != m_lockedMap.end() )
    {
        AbcA::ArraySamplePtr givenPtr = (*foundIter).second.given;
        assert( givenPtr );
        m_unlockedMap[iKey] = givenPtr;
        m_lockedMap.erase( foundIter );
    }
}

//-*****************************************************************************
AbcA::ReadArraySampleCachePtr MakeCacheImplPtr()
{
    return boost::make_shared<CacheImpl>();
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace AbcCoreHDF5
} // End namespace Alembic
