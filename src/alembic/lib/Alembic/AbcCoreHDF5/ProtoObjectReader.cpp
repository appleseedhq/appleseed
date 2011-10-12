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

#include <Alembic/AbcCoreHDF5/ProtoObjectReader.h>
#include <Alembic/AbcCoreHDF5/ReadUtil.h>

namespace Alembic {
namespace AbcCoreHDF5 {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
ProtoObjectReader::ProtoObjectReader( hid_t iParent,
                                      const std::string &iParentFullPathName,
                                      const std::string &iName )
{
    // Validate.
    ABCA_ASSERT( iParent >= 0,
                 "Invalid group passed into ProtoObjectReader ctor" );

    // Open the HDF5 group corresponding to this object.
    m_group = H5Gopen2( iParent, iName.c_str(), H5P_DEFAULT );
    ABCA_ASSERT( m_group >= 0,
                 "Could not open object group named: "
                 << iName << " in parent: " << iParentFullPathName );

    // Read the meta data.
    // Metadata is always named ".prop.meta" for objects,
    // as it is shared with the underlying property.
    AbcA::MetaData mdata;
    ReadMetaData( m_group, ".prop.meta", mdata );

    std::string fullChildName;

    if ( iParentFullPathName == "/" )
    {
        fullChildName = "/" + iName;
    }
    else
    {
        fullChildName = iParentFullPathName + "/" + iName;
    }

    if ( fullChildName == "/ABC" ) { fullChildName = "/"; }

    m_header = AbcA::ObjectHeader( iName,
                                   fullChildName,
                                   mdata );
}

//-*****************************************************************************
ProtoObjectReader::~ProtoObjectReader()
{
    if ( m_group >= 0 )
    {
        H5Gclose( m_group );
        m_group = -1;
    }
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace AbcCoreHDF5
} // End namespace Alembic
