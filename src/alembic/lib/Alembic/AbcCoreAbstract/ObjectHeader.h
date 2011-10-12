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

#ifndef _Alembic_AbcCoreAbstract_ObjectHeader_h_
#define _Alembic_AbcCoreAbstract_ObjectHeader_h_

#include <Alembic/AbcCoreAbstract/Foundation.h>
#include <Alembic/AbcCoreAbstract/MetaData.h>

namespace Alembic {
namespace AbcCoreAbstract {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
//! The ObjectHeader is a collection of MetaData which helps define a
//! Property. It also acts as a key for getting an instance of a property
//! from a CompoundProperty.
class ObjectHeader
{
public:
    //! Default constructor creates an unspecified object header.
    //! ...
    ObjectHeader()
      : m_name()
      , m_fullName()
      , m_metaData() {}

    //! Explicit constructor, ignoring full name.
    //! ...
    ObjectHeader( const std::string &iName,
                  const MetaData &iMetaData )
      : m_name( iName )
      , m_fullName( "" )
      , m_metaData( iMetaData ) {}

    //! Explicit constructor with full name.
    //! ...
    ObjectHeader( const std::string &iName,
                  const std::string &iFullName,
                  const MetaData &iMetaData )
      : m_name( iName )
      , m_fullName( iFullName )
      , m_metaData( iMetaData ) {}

    //! Default Copy constructor
    //! Default Assignment operator

    //! All objects have a name, which is unique amongst its siblings.
    //! ...
    const std::string &getName() const { return m_name; }
    void setName ( const std::string &iName ) { m_name = iName; }

    //! All objects have a full name, which is unique in the whole file.
    //! ...
    const std::string &getFullName() const { return m_fullName; }
    void setFullName( const std::string &iFullName ) { m_fullName = iFullName; }

    //! All objects have metadata.
    //! It is manipulated directly.
    const MetaData &getMetaData() const { return m_metaData; }
    MetaData &getMetaData() { return m_metaData; }

private:
    std::string m_name;
    std::string m_fullName;
    MetaData m_metaData;
};

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcCoreAbstract
} // End namespace Alembic

#endif
