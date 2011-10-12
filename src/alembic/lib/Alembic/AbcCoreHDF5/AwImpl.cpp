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

#include <Alembic/AbcCoreHDF5/AwImpl.h>
#include <Alembic/AbcCoreHDF5/TopOwImpl.h>
#include <Alembic/AbcCoreHDF5/WriteUtil.h>
#include <Alembic/AbcCoreHDF5/HDF5Util.h>

namespace Alembic {
namespace AbcCoreHDF5 {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
AwImpl::AwImpl( const std::string &iFileName,
                const AbcA::MetaData &iMetaData )
  : m_fileName( iFileName )
  , m_metaData( iMetaData )
  , m_file( -1 )
{

    // add default time sampling
    AbcA::TimeSamplingPtr ts( new AbcA::TimeSampling() );
    m_timeSamples.push_back(ts);

    // OPEN THE FILE!
    hid_t faid = H5Pcreate( H5P_FILE_ACCESS );
    if ( faid < 0 )
    {
        ABCA_THROW( "Could not create property access for fopen" );
    }
    H5Pset_libver_bounds( faid, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST );

    m_file = H5Fcreate( m_fileName.c_str(),
                        H5F_ACC_TRUNC, H5P_DEFAULT,
                        faid );

    H5Pclose( faid );

    if ( m_file < 0 )
    {
        ABCA_THROW( "Could not open file: " << m_fileName );
    }

    // set the version using HDF5 native calls
    // This expresses the AbcCoreHDF5 version - how properties,
    // are stored within HDF5, etc.
    int version = ALEMBIC_HDF5_FILE_VERSION;
    H5LTset_attribute_int(m_file, ".", "abc_version", &version, 1);

    // This is the Alembic library version XXYYZZ
    // Where XX is the major version, YY is the minor version
    // and ZZ is the patch version
    int libraryVersion = ALEMBIC_LIBRARY_VERSION;
    H5LTset_attribute_int(m_file, ".", "abc_release_version", 
        &libraryVersion, 1);

    m_metaData.set("_ai_AlembicVersion", AbcA::GetLibraryVersion());

    // Create top explicitly.
    m_top = new TopOwImpl( *this, m_file, m_metaData );
}

//-*****************************************************************************
const std::string &AwImpl::getName() const
{
    return m_fileName;
}

//-*****************************************************************************
const AbcA::MetaData &AwImpl::getMetaData() const
{
    return m_metaData;
}

//-*****************************************************************************
AbcA::ArchiveWriterPtr AwImpl::asArchivePtr()
{
    return shared_from_this();
}

//-*****************************************************************************
AbcA::ObjectWriterPtr AwImpl::getTop()
{
    assert( m_top );

    AbcA::ObjectWriterPtr ret( m_top,
                               Alembic::Util::NullDeleter() );
    return ret;
}

//-*****************************************************************************
uint32_t AwImpl::addTimeSampling( const AbcA::TimeSampling & iTs )
{
    index_t numTS = m_timeSamples.size();
    for (index_t i = 0; i < numTS; ++i)
    {
        if (iTs == *(m_timeSamples[i]))
            return i;
    }

    // we've got a new TimeSampling, write it and add it to our vector
    AbcA::TimeSamplingPtr ts( new AbcA::TimeSampling(iTs) );
    m_timeSamples.push_back(ts);

    index_t latestSample = m_timeSamples.size() - 1;

    std::stringstream strm;
    strm << latestSample;
    std::string name = strm.str();

    WriteTimeSampling(m_file, name, *ts);
    return latestSample;
}

//-*****************************************************************************
AbcA::TimeSamplingPtr AwImpl::getTimeSampling( uint32_t iIndex )
{
    ABCA_ASSERT( iIndex < m_timeSamples.size(),
        "Invalid index provided to getTimeSampling." );

    return m_timeSamples[iIndex];
}

//-*****************************************************************************
AwImpl::~AwImpl()
{
    delete m_top;
    m_top = NULL;

    // empty out the map so any dataset IDs will be freed up
    m_writtenArraySampleMap.m_map.clear();

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
                      "Open HDF5 handles detected during writing:\n"
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
