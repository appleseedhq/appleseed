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

#ifndef _Alembic_AbcCoreAbstract_ScalarSample_h_
#define _Alembic_AbcCoreAbstract_ScalarSample_h_

#include <Alembic/AbcCoreAbstract/Foundation.h>
#include <Alembic/AbcCoreAbstract/DataType.h>

namespace Alembic {
namespace AbcCoreAbstract {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
//! ScalarSample is purely a helper class for implementations.
//! It is not a required object of exchange within Alembic, and you wouldn't
//! want this to be the carrier of data with scalars because you'd be
//! carrying around the extra 2 bytes of "DataType" data every time you
//! passed data, which is wasteful and unnecessary.
//!
//! However, since the Scalar Readers and Writers will always be obligated
//! to compare samples to previously written samples and copy sample values,
//! this class will be helpful in that regard.
//!
//! Plus - and this is just a hunch - I suspect that as Alembic evolves,
//! there will be a need for this extra bit of encapsulation at the abstract
//! level, which is why I'm putting it here.
class ScalarSample
    : public boost::totally_ordered<ScalarSample>
{
public:
    //-*************************************************************************
    // Data
    //-*************************************************************************
    class Data
    {
    public:
        virtual ~Data() {}

        virtual void setToDefault() = 0;
        virtual void copyFrom( const void *iData ) = 0;
        virtual bool equalTo( const void *iData ) const = 0;
        virtual bool equalEpsilon( const void *iData,
                                   double iEpsilon ) const = 0;
        virtual bool lessThan( const void *iData ) const = 0;
        virtual const void *getData() const = 0;
    };
    
    //! Construct from given data type and data. Data will be
    //! copied. If given data is NULL, internal data will be
    //! set to default value.
    explicit ScalarSample( const DataType &iDataType );

    //! Assignment to just data.
    //! Will assume data is of the same type as described
    //! internally by our data type.
    //! is invalid to set to NULL here.
    void copyFrom( const void *iData )
    {
        m_data->copyFrom( iData );
    }

    //-*************************************************************************
    // Data Access
    //-*************************************************************************

    //! Returns the datatype.
    //! ...
    const DataType &getDataType() const { return m_dataType; }

    //! Returns the memory address corresponding to the data
    //! ...
    const void *getData() const { return m_data->getData(); }

    //-*************************************************************************
    // Comparison and Equality
    //-*************************************************************************
    
    //! Assuming the passed memory address points to data of the
    //! same type as us, are they equal? An element-by-element
    //! comparison is done.
    bool operator==( const void *iRhs ) const
    {
        return m_data->equalTo( iRhs );
    }

    //! Are the data types exactly equal?
    //! This will do an element-by-element comparison.
    bool operator==( const ScalarSample &iRhs ) const
    {
        return ( iRhs.getDataType() == m_dataType ) &&
            m_data->equalTo( iRhs.getData() );
    }

    //! Are the data types equal with some precision. This only applies
    //! to floating point types, but will just ignore the relAbsError
    //! for the non-floating-point types.
    bool equalWithRelAbsError( const void *iRhs,
                               double iRelAbsError ) const
    {
        return m_data->equalEpsilon( iRhs, iRelAbsError );
    }

    //! Same as precision-bound equality operator above.
    //! ...
    bool equalWithRelAbsError( const ScalarSample &iRhs,
                               double iRelAbsError ) const
    {
        return ( iRhs.getDataType() == m_dataType ) &&
            m_data->equalEpsilon( iRhs.getData(), iRelAbsError );
    }

    //! Sorting operator. Compares element by element, with
    //! first elements having precedence over later ones.
    bool operator<( const void *iRhs ) const
    {
        return m_data->lessThan( iRhs );
    }

    //! Sorting operator. Compares element by element, with
    //! first elements having precedence over later ones.
    bool operator<( const ScalarSample &iRhs ) const
    {
        if ( m_dataType < iRhs.getDataType() )
        {
            return true;
        }
        else if ( m_dataType > iRhs.getDataType() )
        {
            return false;
        }
        else
        {
            return m_data->lessThan( iRhs.getData() );
        }
    }

    //-*************************************************************************
    // Useful functions
    //-*************************************************************************

    //! Sets to to default values for whichever POD types are contained.
    //! ...
    void setToDefault()
    {
        m_data->setToDefault();
    }

private:
    DataType m_dataType;
    boost::scoped_ptr<Data> m_data;
};

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcCoreAbstract
} // End namespace Alembic

#endif
