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

#include <Alembic/Abc/IArchive.h>
#include <Alembic/Abc/IObject.h>

namespace Alembic {
namespace Abc {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
IArchive::~IArchive()
{
    // Nothing - just here as a support entry point for debugging
}

//-*****************************************************************************
std::string IArchive::getName() const
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN(
        "IArchive::getName()" );

    return m_archive->getName();

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw,
    // so return a NO-OP value
    return "";
}

//-*****************************************************************************
IObject IArchive::getTop()
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "IArchive::getTop()" );

    return IObject( m_archive->getTop(), kWrapExisting );

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw, so here is a default behavior.
    return IObject();
}

//-*****************************************************************************
AbcA::ReadArraySampleCachePtr IArchive::getReadArraySampleCachePtr()
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "IArchive::getReadArraySampleCachePtr" );

    return m_archive->getReadArraySampleCachePtr();

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw,
    // so return a NO-OP value.
    return AbcA::ReadArraySampleCachePtr();
}

//-*****************************************************************************
AbcA::TimeSamplingPtr IArchive::getTimeSampling( uint32_t iIndex )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "IArchive:::getTimeSampling" );

    return m_archive->getTimeSampling( iIndex );

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw, so here is a default behavior.
    return AbcA::TimeSamplingPtr();
}

//-*****************************************************************************
uint32_t IArchive::getNumTimeSamplings( )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "IArchive:::getNumTimeSampling" );

    return m_archive->getNumTimeSamplings( );

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw, so here is a default behavior.
    return 0;
}

//-*****************************************************************************
int32_t IArchive::getArchiveVersion( )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "IArchive:::getArchiveVersion" );

    return m_archive->getArchiveVersion( );

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw, so here is a default behavior.
    return 0;
}

//-*****************************************************************************
void IArchive::setReadArraySampleCachePtr( AbcA::ReadArraySampleCachePtr iPtr )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "IArchive::setReadArraySampleCachePtr" );

    m_archive->setReadArraySampleCachePtr( iPtr );

    ALEMBIC_ABC_SAFE_CALL_END();
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace Abc
} // End namespace Alembic
