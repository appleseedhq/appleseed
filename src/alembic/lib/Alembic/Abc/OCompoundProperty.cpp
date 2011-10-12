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

#include <Alembic/Abc/OCompoundProperty.h>

namespace Alembic {
namespace Abc {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
OCompoundProperty::~OCompoundProperty()
{
    // Here for debug support
}

//-*****************************************************************************
size_t OCompoundProperty::getNumProperties()
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OCompoundProperty::getNumProperties()" );

    return m_property->getNumProperties();

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw, have a default.
    return 0;
}

//-*****************************************************************************
const AbcA::PropertyHeader &OCompoundProperty::getPropertyHeader( size_t iIdx )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OCompoundProperty::getPropertyHeader()" );

    return m_property->getPropertyHeader( iIdx );

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw, have a default.
    static const AbcA::PropertyHeader hd;
    return hd;
}

//-*****************************************************************************
const AbcA::PropertyHeader *
OCompoundProperty::getPropertyHeader( const std::string &iName )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OCompoundProperty::getPropertyHeader()" );

    return m_property->getPropertyHeader( iName );

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw, have a default.
    return NULL;
}

//-*****************************************************************************
OBaseProperty
OCompoundProperty::getProperty( size_t i )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OCompoundProperty::getProperty( i )" );

    return OBaseProperty( m_property->getProperty( i ),
                          kWrapExisting,
                          getErrorHandlerPolicy() );

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw, have a default.
    return OBaseProperty();
}

//-*****************************************************************************
OBaseProperty
OCompoundProperty::getProperty( const std::string &iName )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OCompoundProperty::getProperty( name )" );

    return OBaseProperty( m_property->getProperty( iName ),
                          kWrapExisting,
                          getErrorHandlerPolicy() );

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw, have a default.
    return OBaseProperty();
}

//-*****************************************************************************
OCompoundProperty OCompoundProperty::getParent()
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OCompoundProperty::getParent()" );

    return OCompoundProperty( m_property->getParent(),
                              kWrapExisting,
                              getErrorHandlerPolicy() );

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw. Have a default.
    return OCompoundProperty();
}

//-*****************************************************************************
void OCompoundProperty::init( AbcA::CompoundPropertyWriterPtr iParent,
                              const std::string &iName,
                              ErrorHandler::Policy iParentPolicy,
                              const Argument &iArg0,
                              const Argument &iArg1 )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OCompoundProperty::init()" );

    ABCA_ASSERT( iParent, "invalid parent" );

    Arguments args( iParentPolicy );
    iArg0.setInto( args );
    iArg1.setInto( args );

    getErrorHandler().setPolicy( args.getErrorHandlerPolicy() );

    m_property = iParent->createCompoundProperty( iName, args.getMetaData() );

    ALEMBIC_ABC_SAFE_CALL_END_RESET();
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace Abc
} // End namespace Alembic
