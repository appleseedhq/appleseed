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

#ifndef _Alembic_Abc_ErrorHandler_h_
#define _Alembic_Abc_ErrorHandler_h_

#include <Alembic/Abc/Foundation.h>

namespace Alembic {
namespace Abc {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
class ErrorHandler
{
public:
    enum Policy
    {
        kQuietNoopPolicy,
        kNoisyNoopPolicy,
        kThrowPolicy
    };

    enum UnknownExceptionFlag
    {
        kUnknownException
    };

    ErrorHandler()
      : m_policy( kThrowPolicy )
      , m_errorLog( "" ) {}

    ErrorHandler( Policy iPolicy )
      : m_policy( iPolicy )
      , m_errorLog( "" ) {}

    //! Default copy constructor
    //! Default assignment operator

    void operator()( std::exception &iExc,
                     const std::string &iCtx = "" );

    void operator()( const std::string &iErrMsg,
                     const std::string &iCtx = "" );

    void operator()( UnknownExceptionFlag iUef,
                     const std::string &iCtx = "" );

    Policy getPolicy() const { return m_policy; }
    void setPolicy( Policy iPolicy ) { m_policy = iPolicy; }

    const std::string getErrorLog() const { return m_errorLog; }

    bool valid() const { return ( m_errorLog == "" ); }

    void clear() { m_errorLog = ""; }

    class Context
    {
    public:
        Context( ErrorHandler &iEhnd, const std::string &iCtxMsg )
          : m_handler( iEhnd ),
            m_message( iCtxMsg ) {}

        void operator()( std::exception &iExc )
        {
            m_handler( iExc, m_message );
        }

        void operator()( const std::string &iMsg )
        {
            m_handler( iMsg, m_message );
        }

        void operator()( UnknownExceptionFlag iUef )
        {
            m_handler( iUef, m_message );
        }

    private:
        const Context& operator= (const Context&);
        ErrorHandler &m_handler;
        std::string m_message;
    };

private:
    void handleIt( const std::string &iErr );

    Policy m_policy;
    std::string m_errorLog;
};

//-*****************************************************************************

//-*****************************************************************************
inline ErrorHandler::Policy
GetErrorHandlerPolicy( AbcA::ArchiveWriterPtr iClass )
{ return ErrorHandler::kThrowPolicy; }

inline ErrorHandler::Policy
GetErrorHandlerPolicy( AbcA::ObjectWriterPtr iClass )
{ return ErrorHandler::kThrowPolicy; }

inline ErrorHandler::Policy
GetErrorHandlerPolicy( AbcA::CompoundPropertyWriterPtr iClass )
{ return ErrorHandler::kThrowPolicy; }

inline ErrorHandler::Policy
GetErrorHandlerPolicy( AbcA::ScalarPropertyWriterPtr iClass )
{ return ErrorHandler::kThrowPolicy; }

inline ErrorHandler::Policy
GetErrorHandlerPolicy( AbcA::ArrayPropertyWriterPtr iClass )
{ return ErrorHandler::kThrowPolicy; }

//-*****************************************************************************
inline ErrorHandler::Policy
GetErrorHandlerPolicy( AbcA::ArchiveReaderPtr iClass )
{ return ErrorHandler::kThrowPolicy; }

inline ErrorHandler::Policy
GetErrorHandlerPolicy( AbcA::ObjectReaderPtr iClass )
{ return ErrorHandler::kThrowPolicy; }

inline ErrorHandler::Policy
GetErrorHandlerPolicy( AbcA::CompoundPropertyReaderPtr iClass )
{ return ErrorHandler::kThrowPolicy; }

inline ErrorHandler::Policy
GetErrorHandlerPolicy( AbcA::ScalarPropertyReaderPtr iClass )
{ return ErrorHandler::kThrowPolicy; }

inline ErrorHandler::Policy
GetErrorHandlerPolicy( AbcA::ArrayPropertyReaderPtr iClass )
{ return ErrorHandler::kThrowPolicy; }

//-*****************************************************************************
#define ALEMBIC_ABC_SAFE_CALL_BEGIN( CONTEXT )              \
do                                                              \
{                                                               \
    ::Alembic::Abc::ErrorHandler::Context                   \
        __err( this->getErrorHandler(), ( CONTEXT ) );          \
    try                                                         \
    {

//-*****************************************************************************
#define ALEMBIC_ABC_SAFE_CALL_END_RESET()                   \
    }                                                           \
    catch ( std::exception &exc )                               \
    {                                                           \
        this->reset();                                          \
        __err( exc );                                           \
    }                                                           \
    catch ( ... )                                               \
    {                                                           \
        this->reset();                                          \
        __err( ::Alembic::Abc::                             \
             ErrorHandler::kUnknownException );                 \
    }                                                           \
}                                                               \
while( 0 )

//-*****************************************************************************
#define ALEMBIC_ABC_SAFE_CALL_END()                         \
    }                                                           \
    catch ( std::exception &exc )                               \
    {                                                           \
        __err( exc );                                           \
    }                                                           \
    catch ( ... )                                               \
    {                                                           \
        __err( ::Alembic::Abc::                             \
             ErrorHandler::kUnknownException );                 \
    }                                                           \
}                                                               \
while( 0 )

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace Abc
} // End namespace Alembic

#endif
