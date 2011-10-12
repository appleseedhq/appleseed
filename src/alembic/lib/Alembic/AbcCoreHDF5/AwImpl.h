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

#ifndef _Alembic_AbcCoreHDF5_AwImpl_h_
#define _Alembic_AbcCoreHDF5_AwImpl_h_

#include <Alembic/AbcCoreHDF5/Foundation.h>
#include <Alembic/AbcCoreHDF5/WrittenArraySampleMap.h>
#include <Alembic/AbcCoreHDF5/DataTypeRegistry.h>

namespace Alembic {
namespace AbcCoreHDF5 {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
class TopOwImpl;

//-*****************************************************************************
class AwImpl : public AbcA::ArchiveWriter
             , public boost::enable_shared_from_this<AwImpl>
{
private:
    friend struct WriteArchive;

    AwImpl( const std::string &iFileName,
            const AbcA::MetaData &iMetaData );

public:
    virtual ~AwImpl();

    //-*************************************************************************
    // ABSTRACT FUNCTIONS
    //-*************************************************************************
    virtual const std::string &getName() const;

    virtual const AbcA::MetaData &getMetaData() const;

    virtual AbcA::ObjectWriterPtr getTop();

    virtual AbcA::ArchiveWriterPtr asArchivePtr();

    //-*************************************************************************
    // GLOBAL FILE CONTEXT STUFF.
    //-*************************************************************************
    WrittenArraySampleMap &getWrittenArraySampleMap()
    {
        return m_writtenArraySampleMap;
    }

    virtual uint32_t addTimeSampling( const AbcA::TimeSampling & iTs );

    virtual AbcA::TimeSamplingPtr getTimeSampling( uint32_t iIndex );

    virtual uint32_t getNumTimeSamplings() { return m_timeSamples.size(); }

private:
    std::string m_fileName;
    AbcA::MetaData m_metaData;
    hid_t m_file;

    // This won't create a circular reference because the
    // TopObjectWriter we create is special and doesn't like back up
    // like a normal object writer would.
    TopOwImpl *m_top;

    std::vector < AbcA::TimeSamplingPtr > m_timeSamples;

    WrittenArraySampleMap m_writtenArraySampleMap;
};

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcCoreHDF5
} // End namespace Alembic

#endif
