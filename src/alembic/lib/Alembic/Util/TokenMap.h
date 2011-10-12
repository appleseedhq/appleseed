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
//! \file Alembic/Util/TokenMap.h
//! \brief The header file containing the class definition for
//!     the \ref Alembic::Util::TokenMap class
//-*****************************************************************************
#ifndef _Alembic_Util_TokenMap_h_
#define _Alembic_Util_TokenMap_h_

#include <Alembic/Util/Foundation.h>
#include <Alembic/Util/Exception.h>

namespace Alembic {
namespace Util {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
// TOKEN MAP
//
//! \brief A wrapper around std::map that serializes and deserializes the map
//!        into a doubly-tokenized string, usually of the form
//!        token=value;token=value;token=value;
//-*****************************************************************************
class TokenMap
{
public:
    //-*************************************************************************
    // TYPEDEFS
    //-*************************************************************************
    //! The map_type is std::map<std::string,std::string>
    //! ...
    typedef std::map<std::string,std::string> map_type;
    
    //! key_type promoted from map_type::key_type, which is std::string
    //! ...
    typedef map_type::key_type key_type;
    
    //! data_type promoted from map_type::data_type, which is std::string
    //! ...
    // CJH: Not defined? typedef map_type::data_type data_type;
    typedef std::string data_type;
    
    //! value_type promoted from map_type::value_type, which is
    //! std::pair<std::string, std::string>
    typedef map_type::value_type value_type;

    //! iterator promoted from map_type::iterator
    //! ...
    typedef map_type::iterator iterator;
    
    //! const_iterator promoted from map_type::iterator
    //! ...
    typedef map_type::const_iterator const_iterator;
    
    //! reverse_iterator promoted from map_type::reverse_iterator
    //! ...
    typedef map_type::reverse_iterator reverse_iterator;
    
    //! const_reverse_iterator promoted from map_type::const_reverse_iterator
    //! ...
    typedef map_type::const_reverse_iterator const_reverse_iterator;

    //! reference promoted from map_type::reference
    //! ...
    typedef map_type::reference reference;

    //! const_reference promoted from map_type::const_reference
    //! ...
    typedef map_type::const_reference const_reference; 

    //-*************************************************************************
    // CONSTRUCTORS
    //-*************************************************************************
    //! \brief Default constructor
    //!     Map is initialized with no entries. Values can be added using
    //!     The \ref set and \ref setUnique member functions
    TokenMap() {}

    //-*************************************************************************
    //! \brief Explicit constructor
    //!     Map is initialized from given string, using the delimiter scheme
    //!     as presented. If the 'unique' bool is 'true', it will use
    //!     the \ref setUnique function, obeying the 'quiet' bool accordingly.
    //!     Otherwise it will use the \ref set function.
    explicit TokenMap( const std::string &config,
                       char pairSeparator = ';',
                       char assignSeparator = '=',
                       bool unique = false,
                       bool quiet = true )
    {
        if ( unique )
        {
            setUnique( config, pairSeparator, assignSeparator, quiet );
        }
        else
        {
            set( config, pairSeparator, assignSeparator );
        }
    }

    //-*************************************************************************
    //! Using default copy constructor
    //! ...

    //-*************************************************************************
    //! Using default assignment operator.
    //! ...

    //-*************************************************************************
    // SET
    //! \brief This function sets the token/value pairs in the map by
    //!     deserializing them from a doubly-delimited string.
    //!
    //! \details The delimiter scheme defaults to:
    //!     "token=value;token=value;token=value" but may be overridden
    //!     using the optional separator arguments. Values are added to the
    //!     map one-by-one, overwriting any values that were there before.
    //!     To avoid overwriting, use the \ref setUnique function, which can
    //!     silently or rigidly deal with conflicts
    void set( const std::string &config,
              char pairSeparator = ';',
              char assignSeparator = '=' );

    //-*************************************************************************
    // SET UNIQUE
    //! \brief This function sets only unique (not already stored) token/value
    //!     pairs by deserializing them from a doubly-delimited string.
    //!
    //! \details The delimiter scheme and rules are the same as \ref set ,
    //!     the main difference here is that the class will not overwrite
    //!     values that already exist. If the function is called with
    //!     the default value of 'true' for the 'quiet' parameter, it will
    //!     simply not write those values. Otherwise, it will throw a
    //!     \ref Alembic::Util::Exception
    void setUnique( const std::string &config,
                    char pairSeparator = ';',
                    char assignSeparator = '=',
                    bool quiet = true );


    //-*************************************************************************
    // GET
    //! \brief This function turns the map back into a doubly-tokenized string.
    //!
    //! \details The passed delimiters are used to delimit the string, and
    //!     they have default values. Checking is optionally performed
    //!     (based on the 'check' bool) to make sure neither the tokens nor
    //!     values contain the delimiter characters, and an
    //!     \ref Alembic::Util::Exception is thrown if a conflict is detected.
    std::string get( char pairSeparator = ';',
                     char assignSeparator = '=',
                     bool check = false ) const;

    //-*************************************************************************
    // CLEAR THE MAP
    //-*************************************************************************
    void clear() { m_map.clear(); }

    //-*************************************************************************
    // INDIVIDUAL TOKEN ACCESS
    //-*************************************************************************

    //! \brief This function returns the number of pairs.
    //!        ...
    size_t size() const { return m_map.size(); }
    
    //! \brief This function returns whether the map contains an entry for
    //!     a particular token.
    bool tokenExists( const std::string &token ) const
    {
        return ( m_map.count( token ) > 0 );
    }

    //! \brief This function returns the string value associated with a
    //!     particular token, or the empty string "" if the map does not
    //!     contain this token-value pair.
    std::string value( const std::string &token ) const
    {
        const_iterator fiter = m_map.find( token );
        if ( fiter != m_map.end() )
        {
            return (*fiter).second;
        }
        else
        {
            return "";
        }
    }

    //! \brief This function is a shorthand for \ref value
    //!     It will not return a modifiable entry. To modify,
    //!     \ref setValue must be used.
    std::string operator[]( const std::string &token ) const
    {
        return value( token );
    }

    //! \brief This function sets the value of a token. It will either
    //!     add a new token-value pair if the map does not already contain
    //!     this token, or it will overwrite the value for this token if
    //!     it already exists. You can use the \ref tokenExists function
    //!     to manage uniqueness guarantees.
    void setValue( const std::string &token,
                   const std::string &value )
    {
        m_map[token] = value;
    }

    //-*************************************************************************
    // ITERATION
    //-*************************************************************************

    //! \brief same as std::map begin
    //!     Returns an \ref iterator corresponding to the beginning of the map
    //!     or the end of the map if the map is empty.
    iterator begin() { return m_map.begin(); }

    //! \brief same as std::map begin const
    //!     Returns a \ref const_iterator corresponding to the beginning of the
    //!     map or the end of the map if the map is empty.
    const_iterator begin() const { return m_map.begin(); }

    //! \brief same as std::map end
    //!     Returns an \ref iterator corresponding to the end of the map.
    iterator end() { return m_map.end(); }

    //! \brief same as std::map end const
    //!     Returns an \ref const_iterator corresponding to the end of the map.
    const_iterator end() const { return m_map.end(); }

    //-*************************************************************************
    // REVERSE ITERATION
    //-*************************************************************************

    //! \brief same as std::map rbegin
    //!     Returns an \ref reverse_iterator corresponding to the
    //!     reverse_beginning of the map or the reverse_end of the map
    //!     if the map is empty.
    reverse_iterator rbegin() { return m_map.rbegin(); }

    //! \brief same as std::map rbegin const
    //!     Returns a \ref const_reverse_iterator corresponding to the beginning
    //!     of the map or the end of the map if the map is empty.
    const_reverse_iterator rbegin() const { return m_map.rbegin(); }

    //! \brief same as std::map rend
    //!     Returns an \ref reverse_iterator corresponding to the
    //!     reverse end of the map.
    reverse_iterator rend() { return m_map.rend(); }

    //! \brief same as std::map rend const
    //!     Returns an \ref const_reverse_iterator corresponding to the end
    //!     of the map.
    const_reverse_iterator rend() const { return m_map.rend(); }

    //-*************************************************************************
    // COMPARISON
    //-*************************************************************************

    //! Return an exact match
    //! ...
    bool exactMatch( const TokenMap &iOther ) const
    {
        return m_map == iOther.m_map;
    }

protected:
    map_type m_map;
};

//-*****************************************************************************
inline bool operator==( const TokenMap &iA, const TokenMap &iB )
{
    return iA.exactMatch( iB );
}

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace Util
} // End namespace Alembic

#endif
