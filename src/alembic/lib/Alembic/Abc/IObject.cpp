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

#include <Alembic/Abc/IObject.h>
#include <Alembic/Abc/IArchive.h>
#include <Alembic/Abc/ICompoundProperty.h>

namespace Alembic {
namespace Abc {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
// Nothing at the moment, this is just here as a debug entry point for
// tracking down problems with reference counting.
IObject::~IObject()
{
    // Nothing for now.
    // Mostly here in case we need to add reference-counting debug code.
    //std::cout << "IObject::~IObject() name: "
    //          << m_object->getName()
    //          << std::endl
    //          << "\tUse count of writer ptr: "
    //          << m_object.use_count() << std::endl;
}


//-*****************************************************************************
const AbcA::ObjectHeader &IObject::getHeader() const
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "IObject::getHeader()" );

    return m_object->getHeader();

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw, so have a default behavior.
    static const AbcA::ObjectHeader ohd;
    return ohd;
};

//-*****************************************************************************
IArchive IObject::getArchive()
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "IObject::getArchive()" );

    return IArchive( m_object->getArchive(),
                     kWrapExisting,
                     getErrorHandlerPolicy() );

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw. Have a default.
    return IArchive();
}

//-*****************************************************************************
IObject IObject::getParent()
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "IObject::getParent()" );

    return IObject( m_object->getParent(),
                    kWrapExisting,
                    getErrorHandlerPolicy() );

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw. Have a default.
    return IObject();
}

//-*****************************************************************************
size_t IObject::getNumChildren() const
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "IObject::getNumChildren()" );

    return m_object->getNumChildren();

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw, have a default.
    return 0;
}

//-*****************************************************************************
const AbcA::ObjectHeader &IObject::getChildHeader( size_t iIdx ) const
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "IObject::getChildHeader()" );

    return m_object->getChildHeader( iIdx );

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw, have a default.
    static const AbcA::ObjectHeader hd;
    return hd;
}

//-*****************************************************************************
const AbcA::ObjectHeader *IObject::getChildHeader( const std::string &iName ) const
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "IObject::getChildHeader( name )" );

    return m_object->getChildHeader( iName );

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw, have a default.
    return NULL;
}

//-*****************************************************************************
IObject IObject::getChild( size_t iChildIndex ) const
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "IObject::getChild()" );

    return IObject( m_object->getChild( iChildIndex ),
                    kWrapExisting,
                    getErrorHandlerPolicy() );

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw, return something in case.
    return IObject();
}

//-*****************************************************************************
IObject IObject::getChild( const std::string &iChildName ) const
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "IObject::getChild()" );

    return IObject( m_object->getChild( iChildName ),
                    kWrapExisting,
                    getErrorHandlerPolicy() );

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw, return something in case.
    return IObject();
}

//-*****************************************************************************
ICompoundProperty IObject::getProperties()
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "IObject::getProperties()" );

    return ICompoundProperty( m_object->getProperties(), kWrapExisting );

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw, have a default.
    return ICompoundProperty();
}


//-*****************************************************************************
void IObject::init( AbcA::ObjectReaderPtr iParent,
                    const std::string &iName,
                    ErrorHandler::Policy iParentPolicy,
                    ErrorHandler::Policy iChildPolicy )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "IObject::init()" );

    Arguments args( iParentPolicy );
    Argument cpcyArg( iChildPolicy );
    cpcyArg.setInto( args );

    getErrorHandler().setPolicy( args.getErrorHandlerPolicy() );

    m_object = iParent->getChild( iName );

    ALEMBIC_ABC_SAFE_CALL_END();
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace Abc
} // End namespace Alembic
