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

#ifndef _Alembic_AbcGeom_OXform_h_
#define _Alembic_AbcGeom_OXform_h_

#include <Alembic/AbcGeom/Foundation.h>
#include <Alembic/AbcGeom/SchemaInfoDeclarations.h>

#include <Alembic/AbcGeom/XformSample.h>

namespace Alembic {
namespace AbcGeom {
namespace ALEMBIC_VERSION_NS {

//! The default value for determining whether a property is actually
//! different from the default.
static const double kXFORM_DELTA_TOLERANCE = 1.0e-12;

//-*****************************************************************************
class OXformSchema : public Abc::OSchema<XformSchemaInfo>
{
    //-*************************************************************************
    // XFORM SCHEMA
    //-*************************************************************************
public:

    //! By convention we always define this_type in AbcGeom classes.
    //! Used by unspecified-bool-type conversion below
    typedef Abc::OSchema<XformSchemaInfo> super_type;
    typedef OXformSchema this_type;
    typedef XformSample sample_type;

    //-*************************************************************************
    // CONSTRUCTION, DESTRUCTION, ASSIGNMENT
    //-*************************************************************************

    //! The default constructor creates an empty OPolyMeshSchema
    //! ...
    OXformSchema() {}

    //! This templated, primary constructor creates a new xform writer.
    //! The first argument is any Abc (or AbcCoreAbstract) object
    //! which can intrusively be converted to an CompoundPropertyWriterPtr
    //! to use as a parent, from which the error handler policy for
    //! inheritance is also derived.  The remaining optional arguments
    //! can be used to override the ErrorHandlerPolicy, to specify
    //! MetaData, and to set TimeSampling.
    template <class CPROP_PTR>
    OXformSchema( CPROP_PTR iParent,
                  const std::string &iName,
                  const Abc::Argument &iArg0 = Abc::Argument(),
                  const Abc::Argument &iArg1 = Abc::Argument(),
                  const Abc::Argument &iArg2 = Abc::Argument() )
      : Abc::OSchema<XformSchemaInfo>( iParent, iName,
                                       iArg0, iArg1, iArg2 )
    {
        // Meta data and error handling are eaten up by
        // the super type, so all that's left is time sampling.
        AbcA::TimeSamplingPtr tsPtr =
            Abc::GetTimeSampling( iArg0, iArg1, iArg2 );

        AbcA::index_t tsIndex =
            Abc::GetTimeSamplingIndex( iArg0, iArg1, iArg2 );

        if ( tsPtr )
        {
            tsIndex = iParent->getObject()->getArchive()->
                addTimeSampling( *tsPtr );
        }

        init( tsIndex );
    }

    //! This constructor does the same as the above, but uses the default
    //! name from the XformSchemaInfo struct.
    template <class CPROP_PTR>
    explicit OXformSchema( CPROP_PTR iParent,
                           const Abc::Argument &iArg0 = Abc::Argument(),
                           const Abc::Argument &iArg1 = Abc::Argument(),
                           const Abc::Argument &iArg2 = Abc::Argument() )
      : Abc::OSchema<XformSchemaInfo>( iParent,
                                       iArg0, iArg1, iArg2 )
    {
        // Meta data and error handling are eaten up by
        // the super type, so all that's left is time sampling.
        AbcA::TimeSamplingPtr tsPtr =
            Abc::GetTimeSampling( iArg0, iArg1, iArg2 );

        AbcA::index_t tsIndex =
            Abc::GetTimeSamplingIndex( iArg0, iArg1, iArg2 );

        if ( tsPtr )
        {
            tsIndex = iParent->getObject()->getArchive()->
                addTimeSampling( *tsPtr );
        }

        init( tsIndex );
    }

    //! Explicit copy constructor to work around MSVC bug
    OXformSchema( const OXformSchema &iCopy )
        : Abc::OSchema<XformSchemaInfo>()
    { *this = iCopy; }

    //! Default assignment operator used.

    //-*************************************************************************
    // SCHEMA STUFF
    //-*************************************************************************

    AbcA::TimeSamplingPtr getTimeSampling() const
    {
        return m_inheritsProperty.getTimeSampling();
    }

    //-*************************************************************************
    // SAMPLE STUFF
    //-*************************************************************************

    //! Get number of samples written so far.
    //! ...
    size_t getNumSamples() const;

    //! Set an animated sample.  On first call to set, the sample is modified,
    //! so it can't be const.
    void set( XformSample &ioSamp );

    //! Set from previous sample. Will hold the animated channels.
    void setFromPrevious();

    void setTimeSampling( uint32_t iIndex );
    void setTimeSampling( AbcA::TimeSamplingPtr iTime );

    Abc::OCompoundProperty getArbGeomParams();
    Abc::OCompoundProperty getUserProperties();

    //-*************************************************************************
    // ABC BASE MECHANISMS
    // These functions are used by Abc to deal with errors, rewrapping,
    // and so on.
    //-*************************************************************************

    //! Reset returns this function set to an empty, default
    //! state.
    void reset()
    {
        m_childBoundsProperty.reset();
        m_inheritsProperty.reset();
        m_opsPWPtr.reset();
        m_valsPWPtr.reset();
        m_protoSample.reset();
        m_animChannelsProperty.reset();

        m_staticChans.clear();
        m_staticChans.resize( 0 );

        m_arbGeomParams.reset();
        m_userProperties.reset();

        super_type::reset();
    }

    //! Valid returns whether this function set is valid.
    bool valid() const
    {
        return ( m_opsPWPtr && super_type::valid() );
    }

    //! unspecified-bool-type operator overload.
    //! ...
    ALEMBIC_OVERRIDE_OPERATOR_BOOL( this_type::valid() );


private:
    void init( const AbcA::index_t iTSIndex );

    std::size_t m_numChannels;
    std::size_t m_numOps;

    // should we store are channel values in an ArrayProperty,
    // or in a ScalarProperty with some Dimension > 0 and < MAX_SCALAR_CHANS
    bool m_useArrayProp;

    AbcA::DataType m_arrayValuesDataType;
    Alembic::Util::Dimensions m_arraySampleDimensions;

    void setChannelValues( const std::vector<double> &iVals );

protected:

    Abc::OBox3dProperty m_childBoundsProperty;

    AbcA::ScalarPropertyWriterPtr m_opsPWPtr;

    AbcA::BasePropertyWriterPtr m_valsPWPtr;

    Abc::OBoolProperty m_inheritsProperty;

    Abc::OBoolProperty m_isNotConstantIdentityProperty;

    Abc::OUInt32ArrayProperty m_animChannelsProperty;

    // ensure that our sample's topology is unchanging between
    // calls to set; see usage in OXformSchema::set()
    XformSample m_protoSample;

    std::vector<bool> m_staticChans;

    bool m_isIdentity;

    Abc::OCompoundProperty m_arbGeomParams;

    Abc::OCompoundProperty m_userProperties;
};

//-*****************************************************************************
// SCHEMA OBJECT
//-*****************************************************************************
typedef Abc::OSchemaObject<OXformSchema> OXform;

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcGeom
} // End namespace Alembic

#endif
