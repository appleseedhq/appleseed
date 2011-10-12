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

#include <Alembic/AbcCoreHDF5/TopCpwImpl.h>
#include <Alembic/AbcCoreHDF5/BaseOwImpl.h>
#include <Alembic/AbcCoreHDF5/WriteUtil.h>

namespace Alembic {
namespace AbcCoreHDF5 {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
// With the compound property writer as an input.
TopCpwImpl::TopCpwImpl( BaseOwImpl &iObject,
                        hid_t iParentGroup,
                        const AbcA::MetaData &iMetaData )
  : BaseCpwImpl( iParentGroup )
  , m_objectRef( iObject )
  , m_header( ".prop", iMetaData )
{
    // Write just the meta data.
    WriteMetaData( iParentGroup, ".prop.meta", iMetaData );
}

//-*****************************************************************************
// Destructor is at the end, so that this file has a logical ordering that
// matches the order of operations (create, set samples, destroy)
//-*****************************************************************************

//-*****************************************************************************
const AbcA::PropertyHeader &TopCpwImpl::getHeader() const
{
    return m_header;
}

//-*****************************************************************************
AbcA::ObjectWriterPtr TopCpwImpl::getObject()
{
    return m_objectRef.asObjectPtr();
}

//-*****************************************************************************
AbcA::CompoundPropertyWriterPtr TopCpwImpl::getParent()
{
    // I have no parent
    return AbcA::CompoundPropertyWriterPtr();
}

//-*****************************************************************************
AbcA::CompoundPropertyWriterPtr TopCpwImpl::asCompoundPtr()
{
    return m_objectRef.getProperties();
}

//-*****************************************************************************
TopCpwImpl::~TopCpwImpl()
{
    // Nothing!
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace AbcCoreHDF5
} // End namespace Alembic

