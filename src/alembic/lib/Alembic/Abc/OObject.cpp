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

#include <Alembic/Abc/OObject.h>
#include <Alembic/Abc/OArchive.h>
#include <Alembic/Abc/OCompoundProperty.h>

namespace Alembic {
namespace Abc {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
OObject::~OObject()
{
    // Nothing for now.
    // Mostly here in case we need to add reference-counting debug code.
    //std::cout << "OObject::~OObject() name: "
    //          << m_object->getName()
    //          << std::endl
    //          << "\tUse count of writer ptr: "
    //          << m_object.use_count() << std::endl;
}

//-*****************************************************************************
const AbcA::ObjectHeader &OObject::getHeader() const
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OObject::getHeader()" );

    return m_object->getHeader();

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw, so have a default behavior.
    static const AbcA::ObjectHeader ohd;
    return ohd;
};

//-*****************************************************************************
OArchive OObject::getArchive()
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OObject::getArchive()" );

    return OArchive( m_object->getArchive(),
                     kWrapExisting,
                     getErrorHandlerPolicy() );

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw. Have a default.
    return OArchive();
}

//-*****************************************************************************
OObject OObject::getParent()
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OObject::getParent()" );

    return OObject( m_object->getParent(),
                    kWrapExisting,
                    getErrorHandlerPolicy() );

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw. Have a default.
    return OObject();
}

//-*****************************************************************************
size_t OObject::getNumChildren()
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OObject::getNumChildren()" );

    return m_object->getNumChildren();

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw, have a default.
    return 0;
}

//-*****************************************************************************
const AbcA::ObjectHeader &OObject::getChildHeader( size_t iIdx )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OObject::getChildHeader()" );

    return m_object->getChildHeader( iIdx );

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw, have a default.
    static const AbcA::ObjectHeader hd;
    return hd;
}

//-*****************************************************************************
const AbcA::ObjectHeader *OObject::getChildHeader( const std::string &iName )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OObject::getChildHeader()" );

    return m_object->getChildHeader( iName );

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw, have a default.
    return NULL;
}

//-*****************************************************************************
OObject OObject::getChild( size_t iIdx )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OObject::getChild( idx )" );

    return OObject( m_object->getChild( iIdx ),
                    kWrapExisting,
                    getErrorHandlerPolicy() );

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw, have a default.
    return OObject();
}

//-*****************************************************************************
OObject OObject::getChild( const std::string &iName )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OObject::getChild( name )" );

    return OObject( m_object->getChild( iName ),
                    kWrapExisting,
                    getErrorHandlerPolicy() );

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw, have a default.
    return OObject();
}

//-*****************************************************************************
OCompoundProperty OObject::getProperties()
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OObject::getProperties()" );

    return OCompoundProperty( m_object->getProperties(), kWrapExisting );

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw, have a default.
    return OCompoundProperty();
}

//-*****************************************************************************
void OObject::init( AbcA::ObjectWriterPtr iParent,
                    const std::string &iName,

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

    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OObject::init()" );

    AbcA::ObjectHeader ohdr( iName, args.getMetaData() );
    m_object = iParent->createChild( ohdr );

    ALEMBIC_ABC_SAFE_CALL_END_RESET();
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace Abc
} // End namespace Alembic
