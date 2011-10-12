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

#include <Alembic/AbcGeom/CameraSample.h>

namespace Alembic {
namespace AbcGeom {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
CameraSample::CameraSample ( double iTop, double iBottom, double iLeft,
    double iRight )
{
    reset();
    m_lensSqueezeRatio = (iRight - iLeft) * 0.5;
    m_horizontalFilmOffset = (iLeft + m_lensSqueezeRatio) *
        m_horizontalAperture / (2.0 * m_lensSqueezeRatio);

    m_verticalAperture = (iTop - iBottom) * 0.5 * m_horizontalAperture /
        m_lensSqueezeRatio;

    m_verticalFilmOffset = ( (m_lensSqueezeRatio * m_verticalAperture) /
        m_horizontalAperture + iBottom ) * m_horizontalAperture * 0.5;
}

//-*****************************************************************************
double CameraSample::getCoreValue( std::size_t iIndex ) const
{
    switch ( iIndex )
    {
        case 0:
            return m_focalLength;
        break;

        case 1:
            return m_horizontalAperture;
        break;

        case 2:
            return m_horizontalFilmOffset;
        break;

        case 3:
            return m_verticalAperture;
        break;

        case 4:
            return m_verticalFilmOffset;
        break;

        case 5:
            return m_lensSqueezeRatio;
        break;

        case 6:
            return m_overscanLeft;
        break;

        case 7:
            return m_overscanRight;
        break;

        case 8:
            return m_overscanTop;
        break;

        case 9:
            return m_overscanBottom;
        break;

        case 10:
            return m_fStop;
        break;

        case 11:
            return m_focusDistance;
        break;

        case 12:
            return m_shutterOpen;
        break;

        case 13:
            return m_shutterClose;
        break;

        case 14:
            return m_nearClippingPlane;
        break;

        case 15:
            return m_farClippingPlane;
        break;

        default:
            ABCA_THROW( "Invalid index specified, must be 0-15 not: "
                << iIndex );
        break;
    }
    // For compiler warning
    return 0.0;
}

//-*****************************************************************************
double CameraSample::getFieldOfView () const
{
    // * 10.0 since horizontal film aperture is in cm
    return  2.0 * RadiansToDegrees( atan( m_horizontalAperture * 10.0 /
        (2.0 * m_focalLength ) ) );
}

//-*****************************************************************************
void CameraSample::getScreenWindow( double & oTop, double & oBottom,
    double & oLeft, double & oRight )
{
    double offsetX = 2.0 * m_horizontalFilmOffset * m_lensSqueezeRatio /
        m_horizontalAperture;

    oLeft = -( 1.0 + m_overscanLeft ) * m_lensSqueezeRatio + offsetX;

    oRight = ( 1.0 + m_overscanRight ) * m_lensSqueezeRatio + offsetX;

    double aperY = m_lensSqueezeRatio * m_verticalAperture /
        m_horizontalAperture;
    double offsetY = 2.0 * m_verticalFilmOffset / m_horizontalAperture;

    oBottom = -( 1.0 + m_overscanBottom ) * aperY + offsetY;
    oTop = ( 1.0 + m_overscanTop ) * aperY + offsetY;

    Abc::V2d lt ( oLeft, oTop );
    Abc::V2d rb ( oRight, oBottom );
    Abc::M33d mat = getFilmBackMatrix();

    Abc::V2d flt, frb;
    mat.multVecMatrix( lt, flt );
    mat.multVecMatrix( rb, frb );
    oLeft = flt.x;
    oTop = flt.y;
    oRight = frb.x;
    oBottom = frb.y;
}

//-*****************************************************************************
std::size_t CameraSample::addOp( FilmBackXformOp iOp )
{
    m_ops.push_back( iOp );
    return m_ops.size() - 1;
}

//-*****************************************************************************
FilmBackXformOp CameraSample::getOp( std::size_t iIndex ) const
{
    ABCA_ASSERT( iIndex < m_ops.size(),
        "Invalid index in CameraSample: " << iIndex );

    return m_ops[iIndex];
}

//-*****************************************************************************
FilmBackXformOp & CameraSample::operator[]( const std::size_t &iIndex )
{
    ABCA_ASSERT( iIndex < m_ops.size(),
        "Invalid index in CameraSample: " << iIndex );

    return m_ops[iIndex];
}

//-*****************************************************************************
const FilmBackXformOp & CameraSample::operator[]
    ( const std::size_t &iIndex ) const
{
    ABCA_ASSERT( iIndex < m_ops.size(),
        "Invalid index in CameraSample: " << iIndex );

    return m_ops[iIndex];
}

//-*****************************************************************************
Abc::M33d CameraSample::getFilmBackMatrix () const
{
    Abc::M33d ret;
    ret.makeIdentity();

    for ( size_t i = 0 ; i < m_ops.size() ; ++i )
    {
        Abc::M33d m;
        m.makeIdentity();

        FilmBackXformOp op = m_ops[i];

        if ( op.isMatrixOp() )
        {
            m = op.getMatrix();
        }
        else if ( op.isScaleOp() )
        {
            m.setScale ( op.getScale() );
        }
        else if ( op.isTranslateOp() )
        {
            m.setTranslation( op.getTranslate() );
        }

        ret = m * ret;
    }

    return ret;
}

//-*****************************************************************************
std::size_t CameraSample::getNumOps() const
{
    return m_ops.size();
}

//-*****************************************************************************
std::size_t CameraSample::getNumOpChannels() const
{
    std::size_t ret = 0;
    for ( size_t i = 0 ; i < m_ops.size() ; ++i )
    {
        ret += m_ops[i].getNumChannels();
    }

    return ret;
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace AbcGeom
} // End namespace Alembic
