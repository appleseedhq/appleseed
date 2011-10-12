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

#include <Alembic/AbcCoreHDF5/BaseCpwImpl.h>
#include <Alembic/AbcCoreHDF5/WriteUtil.h>
#include <Alembic/AbcCoreHDF5/SpwImpl.h>
#include <Alembic/AbcCoreHDF5/ApwImpl.h>
#include <Alembic/AbcCoreHDF5/CpwImpl.h>

namespace Alembic {
namespace AbcCoreHDF5 {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
// With the object as an input.
BaseCpwImpl::BaseCpwImpl( hid_t iParentGroup )
  : m_parentGroup( iParentGroup )
  , m_group( -1 )
{
    // Check the validity of all inputs.
    ABCA_ASSERT( m_parentGroup >= 0, "Invalid parent group" );

    // The object ptr is left alone. The TopCpwImpl will leave it
    // alone, the CpwImpl will set it explicitly.
}

//-*****************************************************************************
// Destructor is at the end, so that this file has a logical ordering that
// matches the order of operations (create, set samples, destroy)
//-*****************************************************************************

//-*****************************************************************************
AbcA::ObjectWriterPtr BaseCpwImpl::getObject()
{
    ABCA_ASSERT( m_object, "Invalid object in BaseCpwImpl::getObject()" );
    return m_object;
}

//-*****************************************************************************
size_t BaseCpwImpl::getNumProperties()
{
    return m_propertyHeaders.size();
}

//-*****************************************************************************
const AbcA::PropertyHeader &
BaseCpwImpl::getPropertyHeader( size_t i )
{
    if ( i > m_propertyHeaders.size() )
    {
        ABCA_THROW( "Out of range index in " <<
                    "CpwImpl::getPropertyHeader: " << i );
    }

    PropertyHeaderPtr ptr = m_propertyHeaders[i];
    ABCA_ASSERT( ptr, "Invalid property header ptr in CpwImpl" );

    return *ptr;
}

//-*****************************************************************************
const AbcA::PropertyHeader *
BaseCpwImpl::getPropertyHeader( const std::string &iName )
{
    for ( PropertyHeaderPtrs::iterator piter = m_propertyHeaders.begin();
          piter != m_propertyHeaders.end(); ++piter )
    {
        if ( (*piter)->getName() == iName )
        {
            return (*piter).get();
        }
    }
    return NULL;
}

//-*****************************************************************************
AbcA::BasePropertyWriterPtr
BaseCpwImpl::getProperty( const std::string &iName )
{
    MadeProperties::iterator fiter = m_madeProperties.find( iName );
    if ( fiter == m_madeProperties.end() )
    {
        return AbcA::BasePropertyWriterPtr();
    }

    WeakBpwPtr wptr = (*fiter).second;

    return wptr.lock();
}

//-*****************************************************************************
hid_t BaseCpwImpl::getGroup()
{
    // If we've already made it, return it!
    if ( m_group >= 0 )
    {
        return m_group;
    }

    ABCA_ASSERT( m_parentGroup >= 0, "invalid parent group" );

    // Create the HDF5 group corresponding to this property.
    const std::string groupName = getName();

    hid_t copl = CreationOrderPlist();
    m_group = H5Gcreate2( m_parentGroup, groupName.c_str(),
                          H5P_DEFAULT, copl, H5P_DEFAULT );
    H5Pclose( copl );

    ABCA_ASSERT( m_group >= 0,
                 "Could not create compound property group named: "
                 << groupName );

    return m_group;
}

//-*****************************************************************************
AbcA::ScalarPropertyWriterPtr
BaseCpwImpl::createScalarProperty( const std::string & iName,
        const AbcA::MetaData & iMetaData,
        const AbcA::DataType & iDataType,
        uint32_t iTimeSamplingIndex )
{
    if ( m_madeProperties.count( iName ) )
    {
        ABCA_THROW( "Already have a property named: " << iName );
    }

    hid_t myGroup = getGroup();

    AbcA::ScalarPropertyWriterPtr
        ret( new SpwImpl( asCompoundPtr(), myGroup, iName, iMetaData,
            iDataType, iTimeSamplingIndex ) );

    PropertyHeaderPtr headerPtr( new AbcA::PropertyHeader( ret->getHeader() ) );
    m_propertyHeaders.push_back( headerPtr );
    m_madeProperties[iName] = WeakBpwPtr( ret );

    return ret;
}

//-*****************************************************************************
AbcA::ArrayPropertyWriterPtr
BaseCpwImpl::createArrayProperty( const std::string & iName,
        const AbcA::MetaData & iMetaData,
        const AbcA::DataType & iDataType,
        uint32_t iTimeSamplingIndex )
{

    if ( m_madeProperties.count( iName ) )
    {
        ABCA_THROW( "Already have a property named: " << iName );
    }

    hid_t myGroup = getGroup();

    AbcA::ArrayPropertyWriterPtr
        ret( new ApwImpl( asCompoundPtr(), myGroup, iName, iMetaData,
            iDataType, iTimeSamplingIndex ) );

    PropertyHeaderPtr headerPtr( new AbcA::PropertyHeader( ret->getHeader() ) );
    m_propertyHeaders.push_back( headerPtr );
    m_madeProperties[iName] = WeakBpwPtr( ret );

    return ret;
}

//-*****************************************************************************
AbcA::CompoundPropertyWriterPtr
BaseCpwImpl::createCompoundProperty( const std::string & iName,
        const AbcA::MetaData & iMetaData )
{

    if ( m_madeProperties.count( iName ) )
    {
        ABCA_THROW( "Already have a property named: " << iName );
    }

    hid_t myGroup = getGroup();

    AbcA::CompoundPropertyWriterPtr
        ret( new CpwImpl( this->asCompoundPtr(), myGroup, iName, iMetaData ) );

    PropertyHeaderPtr headerPtr( new AbcA::PropertyHeader( ret->getHeader() ) );
    m_propertyHeaders.push_back( headerPtr );
    m_madeProperties[iName] = WeakBpwPtr( ret );

    return ret;
}

//-*****************************************************************************
BaseCpwImpl::~BaseCpwImpl()
{
    if ( m_group >= 0 )
    {
        H5Gclose( m_group );
        m_group = -1;
    }
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace AbcCoreHDF5
} // End namespace Alembic
