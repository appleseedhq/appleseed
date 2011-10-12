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

#include <Alembic/AbcCoreHDF5/BaseOrImpl.h>
#include <Alembic/AbcCoreHDF5/OrImpl.h>
#include <Alembic/AbcCoreHDF5/ArImpl.h>
#include <Alembic/AbcCoreHDF5/TopCprImpl.h>
#include <Alembic/AbcCoreHDF5/ReadUtil.h>
#include <Alembic/AbcCoreHDF5/HDF5Util.h>

namespace Alembic {
namespace AbcCoreHDF5 {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
//-*****************************************************************************
// OBJECT SUB_OBJECT VISITATION
//-*****************************************************************************
//-*****************************************************************************

//-*****************************************************************************
class ObjectGroupVisitor
{
private:
    friend class BaseOrImpl;
    ObjectGroupVisitor( BaseOrImpl &iParent )
      : m_parent( iParent ) {}

public:
    void createProtoObject( hid_t iGroup, const char *iName )
    {
        m_parent.createProtoObject( iGroup, iName );
    }

private:
    BaseOrImpl &m_parent;
};

//-*****************************************************************************
static herr_t VisitAllLinksCB( hid_t iGroup,
                               const char *iName,
                               const H5L_info_t *iLinfo,
                               void *iOpData )
{
    ObjectGroupVisitor *visitor = ( ObjectGroupVisitor * )iOpData;

    // ".prop" is special
    if ( std::string( iName ) != ".prop" )
    {
        // Create a proto object.
        visitor->createProtoObject( iGroup, iName );
    }

    // Keep iterating!
    return 0;
}

//-*****************************************************************************
//-*****************************************************************************
// OBJECT READER IMPLEMENTATION
//-*****************************************************************************
//-*****************************************************************************

//-*****************************************************************************
// Reading as a child of a parent.
BaseOrImpl::BaseOrImpl( ProtoObjectReaderPtr iProto )
  : m_proto( iProto )
  , m_properties( NULL )
{
    // Check validity of all inputs.
    ABCA_ASSERT( m_proto, "Invalid proto" );
    ABCA_ASSERT( m_proto->getGroup() >= 0, "Invalid group" );

    // Archive left NULL, will be set by OrImpl,
    // TopOrImpl handles archive differently.

    ObjectGroupVisitor visitor( *this );

    hid_t id = m_proto->getGroup();

    herr_t status = H5Literate( id,
                                H5_INDEX_CRT_ORDER,
                                H5_ITER_INC,
                                NULL,
                                VisitAllLinksCB,
                                ( void * )&visitor );

    ABCA_ASSERT( status >= 0,
                 "BaseOrImpl::init(): H5Literate failed" );
}

//-*****************************************************************************
void BaseOrImpl::createProtoObject( hid_t iGroup, const std::string &iName )
{
    // We are called from ctor via VisitAllLinksCB(), so
    // we are multithread safe from changes to m_children,
    // and m_protoObjects.
    ABCA_ASSERT( m_children.count( iName ) == 0,
                 "Creating multiple children named: " << iName );

    Child child;
    child.proto = MakeProtoObjectReaderPtr(
        m_proto->getGroup(),
        m_proto->getHeader().getFullName(),
        iName );

    m_protoObjects.push_back( child.proto );
    m_children[iName] = child;
}

//-*****************************************************************************
const AbcA::ObjectHeader &BaseOrImpl::getHeader() const
{
    // ProtoObjectReader created by ctor and then doesn't change,
    // so multithread safe.
    ABCA_ASSERT( m_proto, "Invalid protoObjectReader in getHeader()" );

    return m_proto->getHeader();
}

//-*****************************************************************************
AbcA::ArchiveReaderPtr BaseOrImpl::getArchive()
{
    // OrImpl ctor sets m_archive, so multithread safe.
    ABCA_ASSERT( m_archive, "Invalid archive in BaseOrImpl::getArchive()" );

    return m_archive;
}

//-*****************************************************************************
AbcA::CompoundPropertyReaderPtr BaseOrImpl::getProperties()
{
    // Create compound property reader for properties.
    // Top properties share meta data with object.
    boost::mutex::scoped_lock l(m_propertiesMutex);

    if ( !m_properties )
    {
        m_properties = new TopCprImpl( *this,
                                       m_proto->getGroup(),
                                       m_proto->getHeader().getMetaData() );
    }

    AbcA::CompoundPropertyReaderPtr ret( m_properties,
                                         Alembic::Util::NullDeleter() );
    return ret;
}

//-*****************************************************************************
size_t BaseOrImpl::getNumChildren()
{
    // m_protoObjects filled by ctor via VisitAllLinksCB via createProtoObject
    // so multithread safe.
    return m_protoObjects.size();
}

//-*****************************************************************************
const AbcA::ObjectHeader & BaseOrImpl::getChildHeader( size_t i )
{
    // m_protoObjects filled by ctor via VisitAllLinksCB via createProtoObject
    // so multithread safe.
    if ( i >= m_protoObjects.size() )
    {
        ABCA_THROW( "Out of range index in OrImpl::getChildHeader: "
                     << i );
    }

    return m_protoObjects[i]->getHeader();
}

//-*****************************************************************************
const AbcA::ObjectHeader *
BaseOrImpl::getChildHeader( const std::string &iName )
{
    // m_children filled by ctor via VisitAllLinksCB via createProtoObject ,
    // so multithread safe.
    ChildrenMap::iterator fiter = m_children.find( iName );
    if ( fiter == m_children.end() )
    {
        return NULL;
    }

    return &((*fiter).second.proto->getHeader());
}

//-*****************************************************************************
AbcA::ObjectReaderPtr
BaseOrImpl::getChild( const std::string &iName )
{
    // m_children filled by ctor via VisitAllLinksCB via createProtoObject ,
    // so multithread safe.
    ChildrenMap::iterator fiter = m_children.find( iName );
    if ( fiter == m_children.end() )
    {
        return AbcA::ObjectReaderPtr();
    }

    Child &child = (*fiter).second;

    AbcA::ObjectReaderPtr optr = child.made.lock();
    if ( ! optr )
    {
        // Make a new one.
        optr.reset ( new OrImpl( asObjectPtr(), child.proto ) );
        child.made = optr;
    }
    return optr;
}

//-*****************************************************************************
BaseOrImpl::~BaseOrImpl()
{
    // delete NULL okay
    delete m_properties;

    // Proto will delete group.
    // Rest will take care of themselves
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace AbcCoreHDF5
} // End namespace Alembic
