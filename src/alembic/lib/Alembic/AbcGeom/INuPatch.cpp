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

#include <Alembic/AbcGeom/INuPatch.h>

namespace Alembic {
namespace AbcGeom {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
bool INuPatchSchema::trimCurveTopologyIsConstant()
{
    return hasTrimCurve() && m_trimNumLoopsProperty.isConstant() &&
        m_trimNumVerticesProperty.isConstant() &&
        m_trimNumCurvesProperty.isConstant() && m_trimOrderProperty.isConstant() &&
        m_trimKnotProperty.isConstant() && m_trimMinProperty.isConstant() &&
        m_trimMaxProperty.isConstant() && m_trimUProperty.isConstant() &&
        m_trimVProperty.isConstant() && m_trimWProperty.isConstant();
}

//-*****************************************************************************
bool INuPatchSchema::trimCurveTopologyIsHomogenous()
{
    return hasTrimCurve() && m_trimNumLoopsProperty.isConstant() &&
        m_trimNumVerticesProperty.isConstant() &&
        m_trimNumCurvesProperty.isConstant() && m_trimOrderProperty.isConstant() &&
        m_trimMinProperty.isConstant() && m_trimMaxProperty.isConstant();
}

//-*****************************************************************************
MeshTopologyVariance INuPatchSchema::getTopologyVariance()
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "INuPatch::getTopologyVariance()" );

    // check for constant topology.
    // if the surface has trim curves, we must also check those of tology
    // variance.
    if ( m_positionsProperty.isConstant() && m_uOrderProperty.isConstant() &&
         m_vOrderProperty.isConstant() && m_uKnotProperty.isConstant() &&
         m_vKnotProperty.isConstant() )
    {
        if ( this -> hasTrimCurve() )
        {
            if ( this -> trimCurveTopologyIsConstant() )
            {
                return kConstantTopology;
            }
            else if ( this -> trimCurveTopologyIsHomogenous() )
            {
                return kHomogenousTopology;
            }
            else
            {
                return kHeterogenousTopology;
            }
        }
        else if ( m_positionWeightsProperty && 
                 !m_positionWeightsProperty.isConstant() )
        {
            return kHomogenousTopology;
        }
        else
        {
            return kConstantTopology;
        }
    }
    else if ( m_numUProperty.isConstant() && m_numVProperty.isConstant() &&
              m_uOrderProperty.isConstant() && m_vOrderProperty.isConstant() &&
              m_uKnotProperty.isConstant() && m_vKnotProperty.isConstant() )
    {

        if ( this -> hasTrimCurve() )
        {
            if ( this -> trimCurveTopologyIsHomogenous() )
            {
                return kHomogenousTopology;
            }
            else
            {
                return kHeterogenousTopology;
            }
        }
        else
        {
            return kHomogenousTopology;
        }
    }


    ALEMBIC_ABC_SAFE_CALL_END();

    return kHeterogenousTopology;
}

//-*****************************************************************************
bool INuPatchSchema::hasTrimProps()
{
    return this->getPropertyHeader( "trim_nloops" ) != NULL &&
           this->getPropertyHeader( "trim_n" ) != NULL &&
           this->getPropertyHeader( "trim_order" ) != NULL &&
           this->getPropertyHeader( "trim_knot" ) != NULL &&
           this->getPropertyHeader( "trim_min" ) != NULL &&
           this->getPropertyHeader( "trim_max" ) != NULL &&
           this->getPropertyHeader( "trim_u" ) != NULL &&
           this->getPropertyHeader( "trim_v" ) != NULL &&
           this->getPropertyHeader( "trim_w" ) != NULL;
}

//-*****************************************************************************
void INuPatchSchema::get( sample_type &oSample,
                          const Abc::ISampleSelector &iSS )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "INuPatch::get()" );

    m_positionsProperty.get( oSample.m_positions, iSS );
    m_numUProperty.get( oSample.m_numU, iSS );
    m_numVProperty.get( oSample.m_numV, iSS );
    m_uOrderProperty.get( oSample.m_uOrder, iSS );
    m_vOrderProperty.get( oSample.m_vOrder, iSS );
    m_uKnotProperty.get( oSample.m_uKnot, iSS );
    m_vKnotProperty.get( oSample.m_vKnot, iSS );

    if ( m_selfBoundsProperty )
    {
        m_selfBoundsProperty.get( oSample.m_selfBounds, iSS );
    }

    if ( m_childBoundsProperty && m_childBoundsProperty.getNumSamples() > 0 )
    {
        m_childBoundsProperty.get( oSample.m_childBounds, iSS );
    }

    if ( m_positionWeightsProperty )
    {
        m_positionWeightsProperty.get( oSample.m_positionWeights, iSS );
    }

    // handle trim curves
    if ( this->hasTrimCurve() )
    {
        m_trimNumLoopsProperty.get( oSample.m_trimNumLoops, iSS );
        m_trimNumCurvesProperty.get( oSample.m_trimNumCurves, iSS );
        m_trimNumVerticesProperty.get( oSample.m_trimNumVertices, iSS );
        m_trimOrderProperty.get( oSample.m_trimOrder, iSS );
        m_trimKnotProperty.get( oSample.m_trimKnot, iSS );
        m_trimMinProperty.get( oSample.m_trimMin, iSS );
        m_trimMaxProperty.get( oSample.m_trimMax, iSS );
        m_trimUProperty.get( oSample.m_trimU, iSS );
        m_trimVProperty.get( oSample.m_trimV, iSS );
        m_trimWProperty.get( oSample.m_trimW, iSS );
    }

    ALEMBIC_ABC_SAFE_CALL_END();
}

//-*****************************************************************************
void INuPatchSchema::init( const Abc::Argument &iArg0,
                           const Abc::Argument &iArg1 )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "INuPatchSchema::init()" );

    Abc::Arguments args;
    iArg0.setInto( args );
    iArg1.setInto( args );

    m_hasTrimCurve = false;

    AbcA::CompoundPropertyReaderPtr _this = this->getPtr();

    // required properties

    // no matching so we pick up old assets written as V3f
    m_positionsProperty = Abc::IP3fArrayProperty( _this, "P", kNoMatching );

    m_numUProperty = Abc::IInt32Property( _this, "nu",
                                  args.getSchemaInterpMatching() );

    m_numVProperty = Abc::IInt32Property( _this, "nv",
                                  args.getSchemaInterpMatching() );

    m_uOrderProperty = Abc::IInt32Property( _this, "uOrder",
                                    args.getSchemaInterpMatching() );

    m_vOrderProperty = Abc::IInt32Property( _this, "vOrder",
                                    args.getSchemaInterpMatching() );

    m_uKnotProperty = Abc::IFloatArrayProperty( _this, "uKnot",
                                        args.getSchemaInterpMatching() );

    m_vKnotProperty = Abc::IFloatArrayProperty( _this, "vKnot",
                                        args.getSchemaInterpMatching() );

    // optional properties
    // none of the things below here are guaranteed to exist

    if ( this->getPropertyHeader( "w" ) != NULL )
    {
        m_positionWeightsProperty = Abc::IFloatArrayProperty( _this, "w",
                                        args.getSchemaInterpMatching() );
    }

    if ( this->getPropertyHeader( "N" ) != NULL )
    {
        m_normalsParam = IN3fGeomParam( _this, "N", iArg0, iArg1 );
    }

    if ( this->getPropertyHeader( "uv" ) != NULL )
    {
        m_uvsParam = IV2fGeomParam( _this, "uv", iArg0, iArg1 );
    }

    if ( this->hasTrimProps() )
    {
        m_trimNumLoopsProperty = Abc::IInt32Property( _this, "trim_nloops",
                                              args.getErrorHandlerPolicy() );
        m_trimNumCurvesProperty = Abc::IInt32ArrayProperty(
            _this, "trim_ncurves",
            args.getErrorHandlerPolicy() );
        m_trimNumVerticesProperty = Abc::IInt32ArrayProperty(
            _this, "trim_n",
            args.getErrorHandlerPolicy() );
        m_trimOrderProperty = Abc::IInt32ArrayProperty( _this, "trim_order",
                                                args.getErrorHandlerPolicy() );
        m_trimKnotProperty = Abc::IFloatArrayProperty( _this, "trim_knot",
                                               args.getErrorHandlerPolicy() );
        m_trimMinProperty = Abc::IFloatArrayProperty( _this, "trim_min",
                                              args.getErrorHandlerPolicy() );
        m_trimMaxProperty = Abc::IFloatArrayProperty( _this, "trim_max",
                                              args.getErrorHandlerPolicy() );
        m_trimUProperty = Abc::IFloatArrayProperty( _this, "trim_u",
                                            args.getErrorHandlerPolicy() );
        m_trimVProperty = Abc::IFloatArrayProperty( _this, "trim_v",
                                            args.getErrorHandlerPolicy() );
        m_trimWProperty = Abc::IFloatArrayProperty( _this, "trim_w",
                                            args.getErrorHandlerPolicy() );

        m_hasTrimCurve = m_trimNumLoopsProperty.getNumSamples() > 0;
    }

    ALEMBIC_ABC_SAFE_CALL_END_RESET();
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace AbcGeom
} // End namespace Alembic
