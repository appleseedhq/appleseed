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

#ifndef _Alembic_AbcCoreHDF5_BaseCpwImpl_h_
#define _Alembic_AbcCoreHDF5_BaseCpwImpl_h_

#include <Alembic/AbcCoreHDF5/Foundation.h>

namespace Alembic {
namespace AbcCoreHDF5 {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
class BaseCpwImpl : public AbcA::CompoundPropertyWriter
{
protected:
    // First constructor is for building the top-level compound property
    // of an ObjectWriter. This one has meta data already written, so we
    // don't have to write it here.
    BaseCpwImpl( hid_t iParentGroup );

public:
    virtual ~BaseCpwImpl();

    // FROM ABSTRACT
    virtual AbcA::ObjectWriterPtr getObject();

    // COMPOUND API
    virtual size_t getNumProperties();

    virtual const AbcA::PropertyHeader & getPropertyHeader( size_t i );

    virtual const AbcA::PropertyHeader *
    getPropertyHeader( const std::string &iName );

    virtual AbcA::BasePropertyWriterPtr
    getProperty( const std::string & iName );

private:
    hid_t getGroup();

public:
    virtual AbcA::ScalarPropertyWriterPtr
    createScalarProperty( const std::string & iName,
        const AbcA::MetaData & iMetaData,
        const AbcA::DataType & iDataType,
        uint32_t iTimeSamplingIndex );

    virtual AbcA::ArrayPropertyWriterPtr
    createArrayProperty( const std::string & iName,
        const AbcA::MetaData & iMetaData,
        const AbcA::DataType & iDataType,
        uint32_t iTimeSamplingIndex );

    virtual AbcA::CompoundPropertyWriterPtr
    createCompoundProperty( const std::string & iName,
        const AbcA::MetaData & iMetaData );

protected:
    // The object we belong to. For TopCpwImpls, this will be NULL
    // to avoid circular references.
    AbcA::ObjectWriterPtr m_object;
    
    // The parent group. We need to keep this around because we
    // don't create our group until we need to. This is guaranteed to
    // exist because our parent (or object) is guaranteed to exist.
    hid_t m_parentGroup;

    // The group corresponding to this property.
    // It may never be created or written.
    hid_t m_group;
    
    typedef std::map<std::string,WeakBpwPtr> MadeProperties;

    PropertyHeaderPtrs m_propertyHeaders;
    MadeProperties m_madeProperties;
};

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcCoreHDF5
} // End namespace Alembic

#endif

