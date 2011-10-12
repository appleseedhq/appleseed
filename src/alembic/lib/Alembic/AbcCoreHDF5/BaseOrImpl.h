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

#ifndef _Alembic_AbcCoreHDF5_BaseOrImpl_h_
#define _Alembic_AbcCoreHDF5_BaseOrImpl_h_

#include <Alembic/AbcCoreHDF5/Foundation.h>
#include <Alembic/AbcCoreHDF5/ProtoObjectReader.h>
#include <boost/thread/mutex.hpp>

namespace Alembic {
namespace AbcCoreHDF5 {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
class TopCprImpl;

//-*****************************************************************************
class BaseOrImpl : public AbcA::ObjectReader
{
protected:
    friend class ObjectGroupVisitor;

    BaseOrImpl( ProtoObjectReaderPtr iProto );

public:
    // Not really public
    void createProtoObject( hid_t iGroup, const std::string &iName );

    virtual ~BaseOrImpl();

    //-*************************************************************************
    // ABSTRACT
    //-*************************************************************************
    virtual const AbcA::ObjectHeader &getHeader() const;

    virtual AbcA::ArchiveReaderPtr getArchive();

    virtual AbcA::CompoundPropertyReaderPtr getProperties();

    //-*************************************************************************
    // CHILDREN
    //-*************************************************************************

    virtual size_t getNumChildren();

    virtual const AbcA::ObjectHeader & getChildHeader( size_t i );

    virtual const AbcA::ObjectHeader *
    getChildHeader( const std::string &iName );

    virtual AbcA::ObjectReaderPtr getChild( const std::string &iName );

protected:
    typedef std::vector<ProtoObjectReaderPtr> ProtoObjects;

    struct Child
    {
        ProtoObjectReaderPtr proto;
        WeakOrPtr made;
    };

    typedef std::map<std::string,Child> ChildrenMap;

    // The archive
    // This will be NULL for TopOrImpl, to avoid circular references.
    // The archive is returned via other means in that case.
    AbcA::ArchiveReaderPtr m_archive;

    // The proto.
    ProtoObjectReaderPtr m_proto;

    // The properties
    // We own these.
    // They are a special kind of compound property which only weak links
    // back up to us, so no pointer cycles.
    TopCprImpl *m_properties;
    boost::mutex    m_propertiesMutex;

    // The children
    ProtoObjects m_protoObjects;
    ChildrenMap m_children;
private:
    // We aren't copyable
    BaseOrImpl( const BaseOrImpl & input);
    const BaseOrImpl & operator=(const BaseOrImpl & rhs);
};

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcCoreHDF5
} // End namespace Alembic

#endif
