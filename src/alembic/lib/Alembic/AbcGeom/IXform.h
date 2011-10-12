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

#ifndef _Alembic_AbcGeom_IXform_h_
#define _Alembic_AbcGeom_IXform_h_

#include <Alembic/AbcGeom/Foundation.h>
#include <Alembic/AbcGeom/SchemaInfoDeclarations.h>

#include <Alembic/AbcGeom/XformSample.h>

namespace Alembic {
namespace AbcGeom {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
class IXformSchema : public Abc::ISchema<XformSchemaInfo>
{
    //-*************************************************************************
    // XFORM SCHEMA
    //-*************************************************************************
public:

    //! By convention we always define this_type in AbcGeom classes.
    //! Used by unspecified-bool-type conversion below
    typedef Abc::ISchema<XformSchemaInfo> super_type;
    typedef IXformSchema this_type;
    typedef XformSample sample_type;

    //-*************************************************************************
    // CONSTRUCTION, DESTRUCTION, ASSIGNMENT
    //-*************************************************************************

    //! The default constructor creates an empty OPolyMeshSchema
    //! ...
    IXformSchema() {}

    //! This templated, primary constructor creates a new xform writer.
    //! The first argument is any Abc (or AbcCoreAbstract) object
    //! which can intrusively be converted to an CompoundPropertyWriterPtr
    //! to use as a parent, from which the error handler policy for
    //! inheritance is also derived.  The remaining optional arguments
    //! can be used to override the ErrorHandlerPolicy, to specify
    //! MetaData, and to set TimeSamplingType.
    template <class CPROP_PTR>
    IXformSchema( CPROP_PTR iParent,
                  const std::string &iName,
                  const Abc::Argument &iArg0 = Abc::Argument(),
                  const Abc::Argument &iArg1 = Abc::Argument() )
      : Abc::ISchema<XformSchemaInfo>( iParent, iName,
                                       iArg0, iArg1 )
    {
        // Meta data and error handling are eaten up by
        // the super type, so all that's left is SchemaInterpMatching.
        init( Abc::GetSchemaInterpMatching( iArg0, iArg1 ) );
    }

    //! This constructor does the same as the above, but uses the default
    //! name from the XformSchemaInfo struct.
    template <class CPROP_PTR>
    explicit IXformSchema( CPROP_PTR iParent,

                           const Abc::Argument &iArg0 = Abc::Argument(),
                           const Abc::Argument &iArg1 = Abc::Argument() )
      : Abc::ISchema<XformSchemaInfo>( iParent, iArg0, iArg1 )
    {
        init( Abc::GetSchemaInterpMatching( iArg0, iArg1 ) );
    }

    //! Wrap an existing IXform object
    template <class CPROP_PTR>
    explicit IXformSchema( CPROP_PTR iThis,
                           Abc::WrapExistingFlag iFlag,

                           const Abc::Argument &iArg0 = Abc::Argument(),
                           const Abc::Argument &iArg1 = Abc::Argument() )
      : Abc::ISchema<XformSchemaInfo>( iThis, iFlag, iArg0, iArg1 )
    {
        init( Abc::GetSchemaInterpMatching( iArg0, iArg1 ) );
    }

    //! explicit copy constructor to work around Windows compiler bug
    IXformSchema( const IXformSchema &iCopy )
        : Abc::ISchema<XformSchemaInfo>()
    {
        *this = iCopy;
    }

    AbcA::TimeSamplingPtr getTimeSampling() const;

    bool isConstant() const { return m_isConstant; }

    //! is this xform both constant and identity?
    bool isConstantIdentity() const { return m_isConstantIdentity; }

    size_t getNumSamples() const;

    //! fill the supplied sample reference with values
    void get( XformSample &oSamp,
              const Abc::ISampleSelector &iSS = Abc::ISampleSelector() ) const;

    XformSample getValue( const Abc::ISampleSelector &iSS =
                          Abc::ISampleSelector() ) const;

    Abc::IBox3dProperty getChildBoundsProperty() const { return m_childBoundsProperty; }

    // lightweight get to avoid constructing a sample
    // see XformSample.h for explanation of this property
    bool getInheritsXforms( const Abc::ISampleSelector &iSS =
                            Abc::ISampleSelector() ) const;

    size_t getNumOps() const { return m_sample.getNumOps(); }

    //! Reset returns this function set to an empty, default
    //! state.
    void reset()
    {
        m_childBoundsProperty.reset();
        m_sample = XformSample();
        m_inheritsProperty.reset();
        m_isConstant = true;
        m_isConstantIdentity = true;

        m_arbGeomParams.reset();
        m_userProperties.reset();

        super_type::reset();
    }

    //! Valid returns whether this function set is valid.
    bool valid() const
    {
        return ( super_type::valid() );
    }

    ICompoundProperty getArbGeomParams() const { return m_arbGeomParams; }

    ICompoundProperty getUserProperties() const { return m_userProperties; }

    //! unspecified-bool-type operator overload.
    //! ...
    ALEMBIC_OVERRIDE_OPERATOR_BOOL( this_type::valid() );


protected:
    Abc::IBox3dProperty m_childBoundsProperty;

    AbcA::BasePropertyReaderPtr m_valsProperty;

    Abc::IBoolProperty m_inheritsProperty;

    Abc::ICompoundProperty m_arbGeomParams;
    Abc::ICompoundProperty m_userProperties;

    bool m_isConstant;

    bool m_isConstantIdentity;

    XformSample m_sample;

private:
    void init( Abc::SchemaInterpMatching iMatching );

    // is m_vals an ArrayProperty, or a ScalarProperty?
    bool m_useArrayProp;

    // fills m_valVec with data
    void getChannelValues( const AbcA::index_t iSampleIndex,
                           XformSample & oSamp ) const;
};

//-*****************************************************************************
// SCHEMA OBJECT
//-*****************************************************************************
typedef Abc::ISchemaObject<IXformSchema> IXform;

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcGeom
} // End namespace Alembic

#endif
