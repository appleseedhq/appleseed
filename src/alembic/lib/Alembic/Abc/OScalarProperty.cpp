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

#include <Alembic/Abc/OScalarProperty.h>

namespace Alembic {
namespace Abc {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
OScalarProperty::~OScalarProperty()
{
    // Nothing for now.
    // Mostly here in case we need to add reference-counting debug code.
}

//-*****************************************************************************
size_t OScalarProperty::getNumSamples()
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OScalarProperty::getNumSamples()" );

    return m_property->getNumSamples();

    ALEMBIC_ABC_SAFE_CALL_END();

    return 0;
}

//-*****************************************************************************
void OScalarProperty::set( const void *iSamp )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OScalarProperty::set()" );

    m_property->setSample( iSamp );

    ALEMBIC_ABC_SAFE_CALL_END();
}

//-*****************************************************************************
void OScalarProperty::setFromPrevious()
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OScalarProperty::setFromPrevious()" );

    m_property->setFromPreviousSample();

    ALEMBIC_ABC_SAFE_CALL_END();
}

//-*****************************************************************************
void OScalarProperty::setTimeSampling( uint32_t iIndex )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OScalarProperty::setTimeSampling(uint32_t)" );

    m_property->setTimeSamplingIndex(iIndex);

    ALEMBIC_ABC_SAFE_CALL_END();
}

//-*****************************************************************************
void OScalarProperty::setTimeSampling(  AbcA::TimeSamplingPtr iTime )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OScalarProperty::setTimeSampling()" );

    uint32_t tsIndex =
        m_property->getParent()->getObject()->getArchive()->addTimeSampling(
            *iTime);

    m_property->setTimeSamplingIndex(tsIndex);

    ALEMBIC_ABC_SAFE_CALL_END();
}

//-*****************************************************************************
OCompoundProperty OScalarProperty::getParent()
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OScalarProperty::getParent()" );

    return OCompoundProperty( m_property->getParent(),
                              kWrapExisting,
                              getErrorHandlerPolicy() );

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw. Have a default.
    return OCompoundProperty();
}

//-*****************************************************************************
void OScalarProperty::init( AbcA::CompoundPropertyWriterPtr iParent,
                            const std::string &iName,
                            const AbcA::DataType &iDataType,

                            ErrorHandler::Policy iParentPolicy,
                            const Argument &iArg0,
                            const Argument &iArg1,
                            const Argument &iArg2 )
{
    Arguments args( iParentPolicy );
    iArg0.setInto( args );
    iArg1.setInto( args );
    iArg2.setInto( args );

    getErrorHandler().setPolicy( args.getErrorHandlerPolicy() );

    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OScalarProperty::init()" );

    AbcA::TimeSamplingPtr tsPtr = args.getTimeSampling();
    uint32_t tsIndex = args.getTimeSamplingIndex();

    // if we specified a valid TimeSamplingPtr, use it to determine the index
    // otherwise we'll use the index, which defaults to the intrinsic 0 index
    if (tsPtr)
    {
        tsIndex = iParent->getObject()->getArchive()->addTimeSampling(*tsPtr);
    }

    m_property = iParent->createScalarProperty( iName, args.getMetaData(), 
        iDataType, tsIndex );

    ALEMBIC_ABC_SAFE_CALL_END_RESET();
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace Abc
} // End namespace Alembic
