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

#ifndef _Alembic_AbcCoreAbstract_MetaData_h_
#define _Alembic_AbcCoreAbstract_MetaData_h_

#include <Alembic/AbcCoreAbstract/Foundation.h>

namespace Alembic {
namespace AbcCoreAbstract {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
//! The MetaData class lies at the core of Alembic's notion of
//! "Object and Property Identity". It is a refinement of the idea of
//! Protocol (for Objects) and Interpretation (for Properties) in OpenGTO.
//! It is, essentially, an UNORDERED, UNIQUE DICTIONARY of strings.
//! It turns itself into a regular string for serialization and deserialization.
//! This is not a virtual class, nor is it intended to be used as a base
//! for derivation. It is explicitly declared and implemented as part of
//! the AbcCoreAbstract library.
//! It is composed (not inherited) from \ref Alembic::Util::TokenMap.
//! In order to not have duplicated (and possibly conflicting) policy
//! implementation, we present this class here as a MOSTLY-WRITE-ONCE interface,
//! with selective exception throwing behavior for failed writes.
class MetaData
{   
public:
    //-*************************************************************************
    // TYPEDEFS
    //-*************************************************************************
    
    //! Our internals are handled by a TokenMap, which we expose
    //! through these typedefs.
    typedef Alembic::Util::TokenMap token_map_type;

    //! Key type.
    //! Keys are unique within each MetaData instance.
    typedef token_map_type::key_type key_type;

    //! Data type.
    //! Data is associated with a key, with each key being unique.
    typedef token_map_type::data_type data_type;

    //! Value-type
    //! This is what the MetaData class "contains", when viewed
    //! as a standard container.
    typedef token_map_type::value_type value_type;

    //! Const reference type
    //! This is what the iterators dereference to.
    typedef token_map_type::const_reference const_reference;

    //! const_iterator typedef
    //! this dereferences to a const \ref value_type reference.
    typedef token_map_type::const_iterator const_iterator;

    //! const_reverse_iterator typedef
    //! this dereferences to a const \ref value_type instance.
    typedef token_map_type::const_reverse_iterator const_reverse_iterator;

    //-*************************************************************************
    // CONSTRUCTION
    //-*************************************************************************

    //! Default constructor creates an empty dictionary.
    //! ...
    MetaData() {}

    //! Copy constructor copies another MetaData.
    //! ...
    MetaData( const MetaData &iCopy ) : m_tokenMap( iCopy.m_tokenMap ) {}

    //! Assignment operator copies the contents of another
    //! MetaData instance.
    MetaData& operator=( const MetaData &iCopy )
    {
        m_tokenMap = iCopy.m_tokenMap;
        return *this;
    }

    //-*************************************************************************
    // SERIALIZATION/DESERIALIZATION
    //-*************************************************************************

    //! Deserialization will replace the contents of this class with the
    //! parsed contents of a string. It will just clear the contents first.
    //! It will throw an exception if the string is mal-formed.
    //! \internal For library implementation internal use.
    void deserialize( const std::string &iFrom )
    {
        m_tokenMap.clear();
        m_tokenMap.setUnique( iFrom, ';', '=', true );
    }

    //! Serialization will convert the contents of this MetaData into a
    //! single string.
    //! \internal For library implementation internal use.
    std::string serialize() const
    {
        return m_tokenMap.get( ';', '=', true );
    }

    //-*************************************************************************
    // SIZE
    //-*************************************************************************
    size_t size() const { return m_tokenMap.size(); }
    
    //-*************************************************************************
    // ITERATION
    //-*************************************************************************

    //! Returns a \ref const_iterator corresponding to the beginning of the
    //! MetaData or the end of the MetaData if empty.
    const_iterator begin() const { return m_tokenMap.begin(); }

    //! Returns a \ref const_iterator corresponding to the end of the
    //! MetaData.
    const_iterator end() const { return m_tokenMap.end(); }

    //! Returns a \ref const_reverse_iterator corresponding to the beginning
    //! of the MetaData or the end of the MetaData if empty.
    const_reverse_iterator rbegin() const { return m_tokenMap.rbegin(); }

    //! Returns an \ref const_reverse_iterator corresponding to the end
    //! of the MetaData.
    const_reverse_iterator rend() const { return m_tokenMap.rend(); }

    //-*************************************************************************
    // ACCESS/ASSIGNMENT
    //-*************************************************************************

    //! set lets you set a key/data pair.
    //! This will silently overwrite an existing value.
    void set( const std::string &iKey, const std::string &iData )
    {
        m_tokenMap.setValue( iKey, iData );
    }

    //! setUnique lets you set a key/data pair,
    //! but throws an exception if you attempt to change the value
    //! of an existing field. It is fine if you set the same value.
    //! \remarks Not the most efficient implementation at the moment.
    void setUnique( const std::string &iKey, const std::string &iData )
    {
        std::string found = m_tokenMap.value( iKey );
        if ( found == "" )
        {
            m_tokenMap.setValue( iKey, iData );
        }
        else if ( found != iData )
        {
            ABCA_THROW( "Key: " << iKey << " already exists in MetaData" );
        }
    }

    //! get returns the value, or an empty string if it is not set.
    //! ...
    std::string get( const std::string &iKey ) const
    {
        return m_tokenMap.value( iKey );
    }

    //! getRequired returns the value, and throws an exception if it is
    //! not found.
    std::string getRequired( const std::string &iKey ) const
    {
        std::string ret = m_tokenMap.value( iKey );
        if ( ret == "" )
        {
            ABCA_THROW( "Key: " << iKey << " did not exist in MetaData" );
        }
        return ret;
    }

    //! append appends the given MetaData to this class. Duplicates are
    //! overwritten.
    void append( const MetaData &iMetaData )
    {
        for ( const_iterator iter = iMetaData.begin();
              iter != iMetaData.end(); ++iter )
        {
            set( (*iter).first, (*iter).second );
        }
    }

    //! append appends the given MetaData to this class. Duplicate values
    //! will cause an exception to be thrown.
    void appendUnique( const MetaData &iMetaData )
    {
        for ( const_iterator iter = iMetaData.begin();
              iter != iMetaData.end(); ++iter )
        {
            setUnique( (*iter).first, (*iter).second );
        }
    }

    //-*************************************************************************
    // MATCHING
    // Simple matching for now, we'll save regex stuff for later.
    //-*************************************************************************

    //! The matches function returns true if each of the fields in the passed
    //! iMetaData are found in this instance and have the same values.
    //! it returns false otherwise.
    //! This is not the same as "equals", because this MetaData may contain
    //! fields that are not included in the passed iMetaData.
    //! This should be the default "matching" function.
    bool matches( const MetaData &iMetaData ) const
    {
        for ( const_iterator iter = iMetaData.begin();
              iter != iMetaData.end(); ++iter )
        {
            if ( get( (*iter).first ) != (*iter).second )
            {
                return false;
            }
        }
        return true;
    }

    //! The matchesExisting function returns true if, for each of the fields
    //! in the passed iMetaData, we have either no entry, or the same entry.
    bool matchesOverlap( const MetaData &iMetaData ) const
    {
        for ( const_iterator iter = iMetaData.begin();
              iter != iMetaData.end(); ++iter )
        {
            std::string found = get( (*iter).first );
            if ( found != "" && found != (*iter).second )
            {
                return false;
            }
        }
        return true;
    }

    //! the matchesExactly function returns true if we're exactly equal in
    //! every field. This is a rarely useful concept with MetaData.
    //! It is for this reason that we explicitly do not overload the == operator.
    bool matchesExactly( const MetaData &iMetaData ) const
    {
        return m_tokenMap.exactMatch( iMetaData.m_tokenMap );
    }
    
private:
    Alembic::Util::TokenMap m_tokenMap;
};

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcCoreAbstract
} // End namespace Alembic
 
#endif
