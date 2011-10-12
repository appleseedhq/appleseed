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

#include <Alembic/AbcGeom/FilmBackXformOp.h>

namespace Alembic {
namespace AbcGeom {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
FilmBackXformOp::FilmBackXformOp()
  : m_type( kTranslateFilmBackOperation )
  , m_channels( 2, 0 )
{
}

//-*****************************************************************************
FilmBackXformOp::FilmBackXformOp( const FilmBackXformOperationType iType,
                  const std::string & iHint )
    : m_type( iType )
    , m_hint( iHint )
{

    // initialize them all to identity

    switch ( m_type )
    {
        case kScaleFilmBackOperation:
        {
            m_channels = std::vector<double>( 2, 1.0 );
        }
        break;

        case kTranslateFilmBackOperation:
        {
            m_channels = std::vector<double>( 2, 0.0 );
        }
        break;

        case kMatrixFilmBackOperation:
        {
            m_channels = std::vector<double>( 9, 0.0 );
            m_channels[0] = 1.0;
            m_channels[4] = 1.0;
            m_channels[8] = 1.0;
        }
        break;
    }
}

//-*****************************************************************************
FilmBackXformOp::FilmBackXformOp( const std::string & iTypeAndHint )
{
    if ( iTypeAndHint.empty() )
    {
        m_type = kTranslateFilmBackOperation;
        m_channels = std::vector<double> ( 2, 0.0 );
    }
    else
    {
        if ( iTypeAndHint[0] == 'm' )
        {
            m_type = kMatrixFilmBackOperation;
            m_hint = iTypeAndHint.substr(1);
            m_channels = std::vector<double>( 9, 0.0 );
            m_channels[0] = 1.0;
            m_channels[4] = 1.0;
            m_channels[8] = 1.0;
        }
        else if ( iTypeAndHint[0] == 's' )
        {
            m_type = kScaleFilmBackOperation;
            m_hint = iTypeAndHint.substr(1);
            m_channels = std::vector<double>( 2, 1.0 );
        }
        else
        {
            m_type = kTranslateFilmBackOperation;
            m_hint = iTypeAndHint.substr(1);
            m_channels = std::vector<double>( 2, 0.0 );
        }
    }
}

//-*****************************************************************************
FilmBackXformOperationType FilmBackXformOp::getType() const
{
    return m_type;
}

//-*****************************************************************************
std::string FilmBackXformOp::getHint() const
{
    return m_hint;
}

//-*****************************************************************************
std::string FilmBackXformOp::getTypeAndHint() const
{
    switch ( m_type )
    {
        case kScaleFilmBackOperation:
        {
            return "s" + m_hint;
        }
        break;

        case kTranslateFilmBackOperation:
        {
            return "t" + m_hint;
        }
        break;

        case kMatrixFilmBackOperation:
        {
            return "m" + m_hint;
        }
        break;
    }

    return "";
}

//-*****************************************************************************
std::size_t FilmBackXformOp::getNumChannels() const
{
    return m_channels.size();
}

//-*****************************************************************************
double FilmBackXformOp::getChannelValue( std::size_t iIndex ) const
{
    if ( iIndex <= m_channels.size() )
        return m_channels[iIndex];
    return 0.0;
}

//-*****************************************************************************
void FilmBackXformOp::setChannelValue( std::size_t iIndex, double iVal )
{
    if ( iIndex < m_channels.size() )
        m_channels[iIndex] = iVal;
}

//-*****************************************************************************
bool FilmBackXformOp::isTranslateOp() const
{
    return m_type == kTranslateFilmBackOperation;
}

//-*****************************************************************************
bool FilmBackXformOp::isScaleOp() const
{
    return m_type == kScaleFilmBackOperation;
}

//-*****************************************************************************
bool FilmBackXformOp::isMatrixOp() const
{
    return m_type == kMatrixFilmBackOperation;
}

//-*****************************************************************************
void FilmBackXformOp::setTranslate( const Abc::V2d &iTrans )
{
    ABCA_ASSERT( m_type == kTranslateFilmBackOperation,
                 "Meaningless to set translate on non-translate op." );

    m_channels[0] = iTrans.x;
    m_channels[1] = iTrans.y;
}

//-*****************************************************************************
void FilmBackXformOp::setScale( const Abc::V2d &iScale )
{
    ABCA_ASSERT( m_type == kScaleFilmBackOperation,
                 "Meaningless to set scale on non-scale op." );

    m_channels[0] = iScale.x;
    m_channels[1] = iScale.y;
}

//-*****************************************************************************
void FilmBackXformOp::setMatrix( const Abc::M33d &iMatrix )
{
    ABCA_ASSERT( m_type == kMatrixFilmBackOperation,
                 "Cannot set non-matrix op from Abc::M33d" );

    m_channels[0] = iMatrix.x[0][0];
    m_channels[1] = iMatrix.x[0][1];
    m_channels[2] = iMatrix.x[0][2];
    m_channels[3] = iMatrix.x[1][0];
    m_channels[4] = iMatrix.x[1][1];
    m_channels[5] = iMatrix.x[1][2];
    m_channels[6] = iMatrix.x[2][0];
    m_channels[7] = iMatrix.x[2][1];
    m_channels[8] = iMatrix.x[2][2];

}

//-*****************************************************************************
Abc::V2d FilmBackXformOp::getTranslate() const
{
    ABCA_ASSERT( m_type == kTranslateFilmBackOperation,
                 "Meaningless to get translate vector from non-translate op." );

    return Abc::V2d(m_channels[0], m_channels[1]);
}

//-*****************************************************************************
Abc::V2d FilmBackXformOp::getScale() const
{
    ABCA_ASSERT( m_type == kScaleFilmBackOperation,
                 "Meaningless to get scaling vector from non-scale op." );

    return Abc::V2d(m_channels[0], m_channels[1]);
}

//-*****************************************************************************
Abc::M33d FilmBackXformOp::getMatrix() const
{
    ABCA_ASSERT( m_type == kMatrixFilmBackOperation,
                 "Can't get matrix from non-matrix op." );

    Abc::M33d ret;

    ret.x[0][0] = m_channels[0];
    ret.x[0][1] = m_channels[1];
    ret.x[0][2] = m_channels[2];
    ret.x[1][0] = m_channels[3];
    ret.x[1][1] = m_channels[4];
    ret.x[1][2] = m_channels[5];
    ret.x[2][0] = m_channels[6];
    ret.x[2][1] = m_channels[7];
    ret.x[2][2] = m_channels[8];

    return ret;
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace AbcGeom
} // End namespace Alembic
