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

#ifndef _Alembic_Abc_ArchiveInfo_h_
#define _Alembic_Abc_ArchiveInfo_h_

#include <Alembic/Abc/Foundation.h>
#include <Alembic/Abc/IArchive.h>
#include <Alembic/Abc/OArchive.h>
#include <time.h>

namespace Alembic {
namespace Abc {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
// Alembic archive information:
// Useful utilities that readers and writers can use to provide
// useful annotations into the alembic file.

//-*****************************************************************************
// Some MetaData key constants
static const char * kApplicationNameKey = "_ai_Application";
static const char * kDateWrittenKey = "_ai_DateWritten";
static const char * kUserDescriptionKey = "_ai_Description";

//-*****************************************************************************
template <class ARCHIVE_CTOR>
OArchive CreateArchiveWithInfo(
    //! We need to pass in a constructor which provides
    //! an explicit link to the concrete implementation of
    //! AbcCoreAbstract that we're using.
    ARCHIVE_CTOR iCtor,

    //! File name
    const std::string &iFileName,

    //! Application specific information about what is writing the file
    const std::string & iApplicationWriter,

    //! Extra information, could be arguments to the tool that is
    //! writing the file.
    const std::string & iUserDescription,

    //! Optional meta data or error handling policy
    const Argument &iArg0 = Argument(),

    //! Optional meta data or error handling policy
    const Argument &iArg1 = Argument() );

//-*****************************************************************************
void
GetArchiveInfo(
    //! The Archive whose meta data will be inspected
    IArchive & iArchive,

    //! Application specific information about what wrote the file
    std::string & oApplicationWriter,

    //! What version of Alembic wrote the file (and when it was built)
    std::string & oAlembicVersion,

    //! Numeric version of the Alembic API that wrote the file
    Util::uint32_t & oAlembicApiVersion,

    //! The data that the file was originally written
    std::string & oDateWritten,

    //! Extra information, could be arguments to the tool that wrote the file.
    std::string & oUserDescription );

//-*****************************************************************************
template <class ARCHIVE_CTOR>
OArchive CreateArchiveWithInfo(
    ARCHIVE_CTOR iCtor,
    const std::string &iFileName,
    const std::string &iApplicationWriter,
    const std::string &iUserDescription,
    const Argument &iArg0,
    const Argument &iArg1 )
{
    AbcA::MetaData md = GetMetaData( iArg0, iArg1 );
    ErrorHandler::Policy policy = GetErrorHandlerPolicyFromArgs( iArg0, iArg1 );

    if ( iApplicationWriter != "" )
    {
        md.set( kApplicationNameKey, iApplicationWriter );
    }

    time_t rawtimeNow;
    time( &rawtimeNow );
    char dateBuf [128];
#if defined _WIN32 || defined _WIN64
    ctime_s( dateBuf, 128, &rawtimeNow);
#else
    ctime_r( &rawtimeNow, dateBuf );
#endif

    std::size_t bufLen = strlen( dateBuf );
    if ( bufLen > 0 && dateBuf[bufLen - 1] == '\n' )
    {
        dateBuf[bufLen - 1] = '\0';
    }
    md.set( kDateWrittenKey, dateBuf );

    if ( iUserDescription != "" )
    {
        md.set( kUserDescriptionKey, iUserDescription );
    }

    return OArchive( iCtor, iFileName, md, policy );

}


} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace Abc
} // End namespace Alembic

#endif
