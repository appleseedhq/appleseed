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

#ifndef _Alembic_AbcCoreHDF5_BaseOwImpl_h_
#define _Alembic_AbcCoreHDF5_BaseOwImpl_h_

#include <Alembic/AbcCoreHDF5/Foundation.h>

namespace Alembic {
namespace AbcCoreHDF5 {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
// Forwards
class TopCpwImpl;

//-*****************************************************************************
class BaseOwImpl : public AbcA::ObjectWriter
{
protected:
    BaseOwImpl( hid_t iParentGroup,
                const std::string &iName,
                const AbcA::MetaData &iMetaData );
    
public:
    virtual ~BaseOwImpl();

    //-*************************************************************************
    // FROM ABSTRACT
    //-*************************************************************************

    virtual AbcA::ArchiveWriterPtr getArchive();

    virtual AbcA::CompoundPropertyWriterPtr getProperties();

    //-*************************************************************************
    // CHILDREN
    //-*************************************************************************

    virtual size_t getNumChildren();

    virtual const AbcA::ObjectHeader & getChildHeader( size_t i );

    virtual const AbcA::ObjectHeader *
    getChildHeader( const std::string &iName );

    virtual AbcA::ObjectWriterPtr getChild( const std::string &iName );

    virtual AbcA::ObjectWriterPtr createChild(
        const AbcA::ObjectHeader &iHeader );

protected:
    void setArchive( AbcA::ArchiveWriterPtr iArchive ) { m_archive = iArchive; }

private:
    typedef std::vector<ObjectHeaderPtr> ChildHeaders;
    typedef std::map<std::string,WeakOwPtr> MadeChildren;

    // The archive ptr.
    // This will be NULL with TopOwImpl, to avoid a circular reference.
    AbcA::ArchiveWriterPtr m_archive;

    // The group corresponding to this property.
    hid_t m_group;

    // The properties
    // We own these.
    TopCpwImpl *m_properties;

    // The children
    ChildHeaders m_childHeaders;
    MadeChildren m_madeChildren;
};

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcCoreHDF5
} // End namespace Alembic

#endif
