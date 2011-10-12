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

#include <Alembic/Abc/ErrorHandler.h>

namespace Alembic {
namespace Abc {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
void ErrorHandler::operator()( std::exception &iExc,
                               const std::string &iMsg )
{
    handleIt( ( iMsg + "\nERROR: EXCEPTION:\n" ) + iExc.what()
              /*+ "\n" + boost::diagnostic_information( iExc )*/ );
}

//-*****************************************************************************
void ErrorHandler::operator()( const std::string &iErr,
                               const std::string &iMsg )
{
    handleIt( ( iMsg + "\nERROR:\n" ) + iErr
              /*+ "\n" */ );
}

//-*****************************************************************************
void ErrorHandler::operator()( ErrorHandler::UnknownExceptionFlag kFlag,
                               const std::string &iMsg )
{
    handleIt( iMsg + "\nERROR: UNKNOWN EXCEPTION\n" );
}

//-*****************************************************************************
void ErrorHandler::handleIt( const std::string &iMsg )
{
    switch ( m_policy )
    {
    case kNoisyNoopPolicy:
        std::cerr << iMsg << std::endl;
        // Intentionally passing through to next
    case kQuietNoopPolicy:
        m_errorLog.append( iMsg );
        m_errorLog.append( "\n" );
        return;

    default:
    case kThrowPolicy:
        ABCA_THROW( iMsg );
        return;
    }
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace Abc
} // End namespace Alembic
