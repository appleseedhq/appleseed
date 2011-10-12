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

#ifndef _Alembic_AbcCoreHDF5_BaseCprImpl_h_
#define _Alembic_AbcCoreHDF5_BaseCprImpl_h_

#include <Alembic/AbcCoreHDF5/Foundation.h>
#include <boost/thread/mutex.hpp>

namespace Alembic {
namespace AbcCoreHDF5 {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
class BaseCprImpl : public AbcA::CompoundPropertyReader
{
public:
    // For construction from an object reader
    BaseCprImpl( hid_t iParentGroup,
                 const std::string &iName );

public:
    virtual ~BaseCprImpl();

    // ABSTRACT
    virtual AbcA::ObjectReaderPtr getObject();

    // COMPOUND API
    virtual size_t getNumProperties();

    virtual const AbcA::PropertyHeader & getPropertyHeader( size_t i );

    virtual const AbcA::PropertyHeader *
    getPropertyHeader( const std::string &iName );

    virtual AbcA::ScalarPropertyReaderPtr
    getScalarProperty( const std::string &iName );

    virtual AbcA::ArrayPropertyReaderPtr
    getArrayProperty( const std::string &iName );

    virtual AbcA::CompoundPropertyReaderPtr
    getCompoundProperty( const std::string &iName );

protected:
    // My Object
    AbcA::ObjectReaderPtr m_object;

    // My group.
    hid_t m_group;

    // Property Headers and Made Property Pointers.
    struct SubProperty
    {
        PropertyHeaderPtr header;

        // extra data that doesn't quite fit into the property header
        // but is stuff we only want to read once
        uint32_t numSamples;
        uint32_t firstChangedIndex;
        uint32_t lastChangedIndex;
        bool isScalarLike;

        WeakBprPtr made;
        std::string name;
    };

    typedef std::map<std::string, size_t> SubPropertiesMap;
    typedef std::vector<SubProperty> SubPropertyVec;

    // Allocated mutexes, one per SubProperty
    boost::mutex * m_subPropertyMutexes;
    SubPropertyVec m_propertyHeaders;
    SubPropertiesMap m_subProperties;
private:
    // We aren't copyable
    BaseCprImpl();
    BaseCprImpl( const BaseCprImpl &input );
    const BaseCprImpl& operator=( const BaseCprImpl &rhs );
};

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcCoreHDF5
} // End namespace Alembic

#endif
