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

#include <Alembic/AbcCoreHDF5/ArImpl.h>
#include <Alembic/AbcCoreHDF5/TopOrImpl.h>
#include <Alembic/AbcCoreHDF5/ReadUtil.h>
#include <Alembic/AbcCoreHDF5/HDF5Util.h>

namespace Alembic {
namespace AbcCoreHDF5 {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
ArImpl::ArImpl( const std::string &iFileName,
                AbcA::ReadArraySampleCachePtr iCache )
  : m_fileName( iFileName )
  , m_file( -1 )
  , m_readArraySampleCache( iCache )
{
    // OPEN THE FILE!
    htri_t exi = H5Fis_hdf5( m_fileName.c_str() );
    ABCA_ASSERT( exi == 1, "Nonexistent File: " << m_fileName );

    m_file = H5Fopen( m_fileName.c_str(), H5F_ACC_RDONLY, H5P_DEFAULT );
    ABCA_ASSERT( m_file >= 0,
                 "Could not open file: " << m_fileName );

    // get the version using HDF5 native calls
    int version = -INT_MAX;
    if (H5Aexists(m_file, "abc_version"))
    {
        H5LTget_attribute_int(m_file, ".", "abc_version", &version);
    }
    ABCA_ASSERT(version == ALEMBIC_HDF5_FILE_VERSION,
        "Unsupported file version detected.");

    // if it isn't there, it's pre 1.0
    int fileVersion = 9999;
    if (H5Aexists( m_file, "abc_release_version" ))
    {
        H5LTget_attribute_int( m_file, ".", "abc_release_version",
                               &fileVersion);
    }
    m_archiveVersion = fileVersion;

    // Read the top object
    m_top = new TopOrImpl( *this, m_file );

    ReadTimeSamples( m_file, m_timeSamples );
}

//-*****************************************************************************
const std::string &ArImpl::getName() const
{
    return m_fileName;
}

//-*****************************************************************************
const AbcA::MetaData &ArImpl::getMetaData() const
{
    ABCA_ASSERT( m_top, "Invalid top object" );
    return m_top->getMetaData();
}

//-*****************************************************************************
AbcA::ObjectReaderPtr ArImpl::getTop()
{
    ABCA_ASSERT( m_top, "Invalid top object" );
    AbcA::ObjectReaderPtr ret( m_top,
                               Alembic::Util::NullDeleter() );
    return ret;
}

//-*****************************************************************************
AbcA::TimeSamplingPtr ArImpl::getTimeSampling( uint32_t iIndex )
{
    ABCA_ASSERT( iIndex < m_timeSamples.size(),
        "Invalid index provided to getTimeSampling." );

    return m_timeSamples[iIndex];
}

//-*****************************************************************************
AbcA::ArchiveReaderPtr ArImpl::asArchivePtr()
{
    return shared_from_this();
}

//-*****************************************************************************
ArImpl::~ArImpl()
{
    delete m_top;
    m_top = NULL;

    if ( m_file >= 0 )
    {
        int dsetCount = H5Fget_obj_count( m_file,
            H5F_OBJ_LOCAL | H5F_OBJ_DATASET);
        int grpCount = H5Fget_obj_count( m_file,
            H5F_OBJ_LOCAL | H5F_OBJ_GROUP );
        int dtypCount = H5Fget_obj_count( m_file,
            H5F_OBJ_LOCAL | H5F_OBJ_DATATYPE );
        int attrCount = H5Fget_obj_count( m_file,
            H5F_OBJ_LOCAL | H5F_OBJ_ATTR );

        int objCount = dsetCount + grpCount + dtypCount + attrCount;

        if ( objCount != 0 )
        {
            std::string excStr =
                ( boost::format(
                      "Open HDF5 handles detected during reading:\n"
                      "DataSets: %d, Groups: %d, "
                      "DataTypes: %d, Attributes: %d" )
                  % dsetCount
                  % grpCount
                  % dtypCount
                  % attrCount ).str();

            m_file = -1;
            ABCA_THROW( excStr );
        }

        H5Fclose( m_file );
        m_file = -1;
    }
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace AbcCoreHDF5
} // End namespace Alembic
