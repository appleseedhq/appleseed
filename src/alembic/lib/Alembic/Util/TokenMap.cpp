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
// *       Neither the name of Industrial Light & Magic nor the names of
// its contributors may be used to endorse or promote products derived
// from this software without specific prior written permission.
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

//-*****************************************************************************
//! \file Alembic/Util/TokenMap.cpp
//! \brief The body file containing the class implementation for
//!     the \ref Alembic::Util::TokenMap class
//-*****************************************************************************

#include <Alembic/Util/TokenMap.h>

namespace Alembic {
namespace Util {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
void TokenMap::set( const std::string &config,
                    char pairSep,
                    char assignSep )
{
    std::size_t lastPair = 0;
    while(1)
    {
        std::size_t curPair = config.find(pairSep, lastPair);
        std::size_t curAssign = config.find(assignSep, lastPair);

        if (curAssign != std::string::npos)
        {
            std::size_t endPos = std::string::npos;
            if (curPair != endPos)
            {
                endPos = curPair - curAssign - 1;
            }

            m_map[config.substr(lastPair, curAssign - lastPair)] =
                config.substr(curAssign + 1, endPos);
        }

        if (curPair == std::string::npos)
        {
            return;
        }

        lastPair = curPair + 1;
    }
}

//-*****************************************************************************
void TokenMap::setUnique( const std::string &config,
                          char pairSep,
                          char assignSep,
                          bool quiet )
{
    std::size_t lastPair = 0;
    while(1)
    {
        std::size_t curPair = config.find(pairSep, lastPair);
        std::size_t curAssign = config.find(assignSep, lastPair);

        if (curAssign != std::string::npos)
        {
            std::size_t endPos = std::string::npos;
            if (curPair != endPos)
            {
                endPos = curPair - curAssign - 1;
            }

            std::string keyStr = config.substr(lastPair, curAssign - lastPair);

            if ( m_map.count( keyStr ) > 0 )
            {
                if ( !quiet )
                {
                    ALEMBIC_THROW( "TokenMap::setUnique: token: "
                                   << keyStr << " is not unique." );
                }
            }
            else
            {
                m_map[keyStr] = config.substr(curAssign + 1, endPos);
            }
        }

        if (curPair == std::string::npos)
        {
            return;
        }

        lastPair = curPair + 1;
    }
}

//-*****************************************************************************
std::string TokenMap::get( char pairSep,
                           char assignSep,
                           bool check ) const
{
    char buf[2] = { 0, 0 };
    
    buf[0] = pairSep;
    std::string pairSepStr( ( const char * )buf );
    
    buf[0] = assignSep;
    std::string assignSepStr( ( const char * )buf );

    std::stringstream output;

    bool start = true;
    
    for ( const_iterator iter = m_map.begin();
          iter != m_map.end(); ++iter )
    {
        std::string token = (*iter).first;
        std::string value = (*iter).second;

        if ( check &&
             ( token.find( pairSep ) != std::string::npos ||
               token.find( assignSep ) != std::string::npos ||
               value.find( pairSep ) != std::string::npos ||
               value.find( assignSep ) != std::string::npos ) )
        {
            ALEMBIC_THROW( "TokenMap::get: Token-Value pair: "
                           << token << ", " << value
                           << " contains separator characters: "
                           << pairSepStr << " or "
                           << assignSepStr );
        }

        if ( !start )
        {
            output << pairSepStr;
        }

        output << token << assignSepStr << value;

        start = false;
    }

    return output.str();
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace Util
} // End namespace Alembic


