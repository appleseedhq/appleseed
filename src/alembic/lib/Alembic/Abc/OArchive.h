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

#ifndef _Alembic_Abc_OArchive_h_
#define _Alembic_Abc_OArchive_h_

#include <Alembic/Abc/Foundation.h>
#include <Alembic/Abc/Base.h>
#include <Alembic/Abc/Argument.h>

namespace Alembic {
namespace Abc {
namespace ALEMBIC_VERSION_NS {

class OObject;

//-*****************************************************************************
class OArchive : public Base
{
public:
    //! By convention, we always define "this_type" in every Abc
    //! class. This convention is relied upon by the unspecified-bool-type
    //! conversion.
    typedef OArchive this_type;

    //-*************************************************************************
    // CONSTRUCTION, DESTRUCTION, ASSIGNMENT
    //-*************************************************************************

    //! The default constructor creates an empty OArchive function set.
    //! ...
    OArchive() {}

    //! The explicit constructor creates an archive with the given
    //! file name. Additional arguments that may be passed are the
    //! error handling policy and meta data.
    template <class ARCHIVE_CTOR>
    OArchive(
        //! We need to pass in a constructor which provides
        //! an explicit link to the concrete implementation of
        //! AbcCoreAbstract that we're using.
        ARCHIVE_CTOR iCtor,

        //! File name
        const std::string &iFileName,

        //! Optionally could be the error handling policy, or the meta data.
        const Argument &iArg0 = Argument(),

        //! Optionally could be the error handling policy, or the meta data.
        const Argument &iArg1 = Argument() );

    //! This attaches an OArchive wrapper around an existing
    //! ArchiveWriterPtr, with an optional error handling policy.
    OArchive(

        //! The pointer
        //! ...
        AbcA::ArchiveWriterPtr iPtr,

        //! Wrap existing. Here cosmetically, for consistency.
        //! ...
        WrapExistingFlag iWrap,

        //! Optional error handling policy
        //! ...
        ErrorHandler::Policy iPolicy = ErrorHandler::kThrowPolicy )
      : m_archive( iPtr )
    {
        // Set the error handling policy.
        getErrorHandler().setPolicy( iPolicy );
    }

    //! Destructor
    //! ...
    ~OArchive();

    //! Default copy constructor
    //! Default assignment operator

    //-*************************************************************************
    // ARCHIVE WRITER FUNCTIONALITY
    //-*************************************************************************

    //! Returns the file name.
    //! It is an error to do so with an invalid object.
    std::string getName() const;

    //! This returns the single top-level OObject that exists
    //! automatically as part of the archive.
    OObject getTop();

    //! Get the compression applied to array properties.
    //! The value will be -1 for uncompressed, and 0-9 for weak through
    //! strong gzip compression.
    int8_t getCompressionHint() const;

    //! Set the compression applied to array properties.
    //! Value of -1 means uncompressed, and values of 0-9 indicate increasingly
    //! compressed data, at the expense of time.
    void setCompressionHint( int8_t iCh );

    //! Adds the TimeSampling to the Archive TimeSampling pool.
    //! If the TimeSampling already exists in the pool, the index for the match
    //! should be returned.
    //! index 0 is automatically reserved for uniform time sampling with a start
    //! time of 0 and time per cycle of 1 (aka identity sampling)
    uint32_t addTimeSampling( const AbcA::TimeSampling & iTs );

    //! Returns the TimeSampling at a given index.
    //! index 0 is automatically reserved for uniform time sampling with a start
    //! time of 0 and time per cycle of 1 (aka identity sampling)
    AbcA::TimeSamplingPtr getTimeSampling( uint32_t iIndex );

    //! Returns the total number of TimeSamplingPtrs in the Archive
    //! TimeSampling pool.
    uint32_t getNumTimeSamplings();

    //-*************************************************************************
    // ABC BASE MECHANISMS
    // These functions are used by Abc to deal with errors, rewrapping,
    // and so on.
    //-*************************************************************************

    //! getPtr, as usual, returns a shared ptr to the
    //! underlying AbcCoreAbstract object, in this case the
    //! ArchiveWriterPtr.
    AbcA::ArchiveWriterPtr getPtr() { return m_archive; }

    //! Reset returns this function set to an empty, default
    //! state.
    void reset() { m_archive.reset(); Base::reset(); }

    //! Valid returns whether this function set is
    //! valid.
    bool valid() const
    {
        return ( Base::valid() && m_archive );
    }

    //! The unspecified-bool-type operator casts the object to "true"
    //! if it is valid, and "false" otherwise.
    ALEMBIC_OPERATOR_BOOL( valid() );

private:
    AbcA::ArchiveWriterPtr m_archive;
};

//-*****************************************************************************
inline AbcA::ArchiveWriterPtr GetArchiveWriterPtr( OArchive &iArch )
{
    return iArch.getPtr();
}

//-*****************************************************************************
//-*****************************************************************************
template <class ARCHIVE_CTOR>
OArchive::OArchive( ARCHIVE_CTOR iCtor,
                    const std::string &iFileName,
                    const Argument &iArg0,
                    const Argument &iArg1 )
{
    // Create arguments
    Arguments args( ErrorHandler::kThrowPolicy );
    iArg0.setInto( args );
    iArg1.setInto( args );

    // Set the error handling policy.
    getErrorHandler().setPolicy( args.getErrorHandlerPolicy() );

    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OArchive::OArchive( iFileName )" );

    m_archive = iCtor( iFileName, args.getMetaData() );

    ALEMBIC_ABC_SAFE_CALL_END_RESET();
}

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace Abc
} // End namespace Alembic

#endif
