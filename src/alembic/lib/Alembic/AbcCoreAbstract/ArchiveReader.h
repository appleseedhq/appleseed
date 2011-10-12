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

#ifndef _Alembic_AbcCoreAbstract_ArchiveReader_h_
#define _Alembic_AbcCoreAbstract_ArchiveReader_h_

#include <Alembic/AbcCoreAbstract/Foundation.h>
#include <Alembic/AbcCoreAbstract/ForwardDeclarations.h>
#include <Alembic/AbcCoreAbstract/ReadArraySampleCache.h>

namespace Alembic {
namespace AbcCoreAbstract {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
//! In order for an implementation to concretely provide a starting point
//! for using the abstract API, a single "explicitly linked" function to
//! start from is necessary. That function will return, in the case of reading,
//! an ArchiveReaderPtr. This typedef provides a signature that such a function
//! should adhere to. This idiom is a workaround for the lack of virtual
//! constructors in C++, and to avoid creating another class (such as
//! "Implementation" or something similar)
//! This is provided here as an illustration. It does not need to be
//! derived from explicitly.
namespace IllustrationOnly {
struct ArchiveReaderConstructor
{
    // Create whatever cache is the default caching mechanism associated
    // with the archive reader implementation
    ArchiveReaderPtr operator()( const std::string &iFileName );

    // Use the explicitly supplied cache implementation.
    ArchiveReaderPtr operator()( const std::string &iFileName,
                                 ReadArraySampleCachePtr iCachePtr );
};
} // End namespace IllustrationOnly

//-*****************************************************************************
//! The Archive is "the file". It has a single object, it's top object.
//! It has no properties, but does have metadata.
class ArchiveReader
    : private boost::noncopyable
{
public:
    //! Virtual destructor
    //! ...
    virtual ~ArchiveReader();

    //-*************************************************************************
    // NEW FUNCTIONS
    //-*************************************************************************
    
    //! Return the archive (file) name. This is the name of the file
    //! which the root reader is associated with.
    virtual const std::string &getName() const = 0;
    
    //! The meta data of the archive is the same as the meta data
    //! of the top-level object reader.
    virtual const MetaData &getMetaData() const = 0;

    //! Get (or open) a pointer to the top object reader
    //! corresponding to this archive.
    virtual ObjectReaderPtr getTop() = 0;

    //! Get the read array sample cache. It may be a NULL pointer.
    //! Caches can be shared amongst separate archives, and caching
    //! will is disabled if a NULL cache is returned here.
    virtual ReadArraySampleCachePtr getReadArraySampleCachePtr() = 0;

    //! Set the read array sample cache. It may also be a NULL pointer.
    //! Caches can be shared amongst separate archives, and caching
    //! will be disabled if a NULL cache is passed here.
    virtual void setReadArraySampleCachePtr( ReadArraySampleCachePtr iPtr ) = 0;

    //! Returns the TimeSampling at a given index.
    virtual TimeSamplingPtr getTimeSampling( uint32_t iIndex ) = 0;

    //! Returns the total number of TimeSamplingPtrs in the Archive
    //! TimeSampling pool.
    virtual uint32_t getNumTimeSamplings() = 0;

    //! Returns the Alembic library numeric version (see Foundation.h)
    //! of this archive file.
    virtual int32_t getArchiveVersion() = 0;

    //! Return self
    //! ...
    virtual ArchiveReaderPtr asArchivePtr() = 0;
};

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcCoreAbstract
} // End namespace Alembic

#endif

