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

#ifndef _Alembic_AbcGeom_OCamera_h_
#define _Alembic_AbcGeom_OCamera_h_

#include <Alembic/AbcGeom/Foundation.h>
#include <Alembic/AbcGeom/SchemaInfoDeclarations.h>
#include <Alembic/AbcGeom/CameraSample.h>

namespace Alembic {
namespace AbcGeom {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
class OCameraSchema : public Abc::OSchema<CameraSchemaInfo>
{
    //-*************************************************************************
    // CAMERA SCHEMA
    //-*************************************************************************
public:
    //! By convention we always define this_type in AbcGeom classes.
    //! Used by unspecified-bool-type conversion below
    typedef OCameraSchema this_type;

    //-*************************************************************************
    // CONSTRUCTION, DESTRUCTION, ASSIGNMENT
    //-*************************************************************************

    //! The default constructor creates an empty OCameraMeshSchema
    //! ...
    OCameraSchema() {}

    //! This templated, primary constructor creates a new camera writer.
    //! The first argument is any Abc (or AbcCoreAbstract) object
    //! which can intrusively be converted to an CompoundPropertyWriterPtr
    //! to use as a parent, from which the error handler policy for
    //! inheritance is also derived.  The remaining optional arguments
    //! can be used to override the ErrorHandlerPolicy, to specify
    //! MetaData, and to set TimeSampling.
    template <class CPROP_PTR>
    OCameraSchema( CPROP_PTR iParent,
                     const std::string &iName,

                     const Abc::Argument &iArg0 = Abc::Argument(),
                     const Abc::Argument &iArg1 = Abc::Argument(),
                     const Abc::Argument &iArg2 = Abc::Argument() )
      : Abc::OSchema<CameraSchemaInfo>( iParent, iName,
                                            iArg0, iArg1, iArg2 )
    {

        AbcA::TimeSamplingPtr tsPtr =
            Abc::GetTimeSampling( iArg0, iArg1, iArg2 );
        uint32_t tsIndex =
            Abc::GetTimeSamplingIndex( iArg0, iArg1, iArg2 );

        // if we specified a valid TimeSamplingPtr, use it to determine the
        // index otherwise we'll use the index, which defaults to the intrinsic
        // 0 index
        if (tsPtr)
        {
            tsIndex = iParent->getObject()->getArchive(
                )->addTimeSampling(*tsPtr);
        }

        // Meta data and error handling are eaten up by
        // the super type, so all that's left is time sampling.
        init( tsIndex );
    }

    template <class CPROP_PTR>
    explicit OCameraSchema( CPROP_PTR iParent,
                              const Abc::Argument &iArg0 = Abc::Argument(),
                              const Abc::Argument &iArg1 = Abc::Argument(),
                              const Abc::Argument &iArg2 = Abc::Argument() )
      : Abc::OSchema<CameraSchemaInfo>( iParent,
                                            iArg0, iArg1, iArg2 )
    {

        AbcA::TimeSamplingPtr tsPtr =
            Abc::GetTimeSampling( iArg0, iArg1, iArg2 );
        uint32_t tsIndex =
            Abc::GetTimeSamplingIndex( iArg0, iArg1, iArg2 );

        // if we specified a valid TimeSamplingPtr, use it to determine the
        // index otherwise we'll use the index, which defaults to the intrinsic
        // 0 index
        if (tsPtr)
        {
            tsIndex = iParent->getObject()->getArchive(
                )->addTimeSampling(*tsPtr);
        }

        // Meta data and error handling are eaten up by
        // the super type, so all that's left is time sampling.
        init( tsIndex );
    }

    //! Copy constructor.
    OCameraSchema(const OCameraSchema& iCopy)
        : Abc::OSchema<CameraSchemaInfo>()
    {
        *this = iCopy;
    }

    //! Default assignment operator used.

    //-*************************************************************************
    // SCHEMA STUFF
    //-*************************************************************************

    //! Return the time sampling.
    AbcA::TimeSamplingPtr getTimeSampling() const
    { return m_coreProperties.getTimeSampling(); }

    //-*************************************************************************
    // SAMPLE STUFF
    //-*************************************************************************

    //! Get number of samples written so far.
    //! ...
    size_t getNumSamples()
    { return m_coreProperties.getNumSamples(); }

    //! Set a sample
    void set( const CameraSample &iSamp );

    //! Set from previous sample.
    void setFromPrevious();

    void setTimeSampling( uint32_t iIndex );
    void setTimeSampling( AbcA::TimeSamplingPtr iTime );

    Abc::OCompoundProperty getUserProperties();
    Abc::OCompoundProperty getArbGeomParams();

    //-*************************************************************************
    // ABC BASE MECHANISMS
    // These functions are used by Abc to deal with errors, rewrapping,
    // and so on.
    //-*************************************************************************

    //! Reset returns this function set to an empty, default
    //! state.
    void reset()
    {
        m_coreProperties.reset();
        m_childBoundsProperty.reset();
        m_userProperties.reset();
        m_arbGeomParams.reset();

        Abc::OSchema<CameraSchemaInfo>::reset();
    }

    //! Returns whether this function set is valid.
    bool valid() const
    {
        return ( Abc::OSchema<CameraSchemaInfo>::valid() &&
                 m_coreProperties.valid() );
    }

    //! unspecified-bool-type operator overload.
    //! ...
    ALEMBIC_OVERRIDE_OPERATOR_BOOL( OCameraSchema::valid() );

protected:
    void init( uint32_t iTsIdx );

    Abc::OScalarProperty m_coreProperties;

    Abc::OBox3dProperty m_childBoundsProperty;

    Abc::OCompoundProperty m_userProperties;
    Abc::OCompoundProperty m_arbGeomParams;

    Abc::ODoubleArrayProperty m_bigFilmBackChannelsProperty;

    Abc::OScalarProperty m_smallFilmBackChannelsProperty;

private:
    CameraSample m_initialSample;

};

//-*****************************************************************************
// SCHEMA OBJECT
//-*****************************************************************************
typedef Abc::OSchemaObject<OCameraSchema> OCamera;

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcGeom
} // End namespace Alembic

#endif
