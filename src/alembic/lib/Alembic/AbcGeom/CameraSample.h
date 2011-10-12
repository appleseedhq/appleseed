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

#ifndef _Alembic_AbcGeom_CameraSample_h_
#define _Alembic_AbcGeom_CameraSample_h_

#include <Alembic/AbcGeom/Foundation.h>
#include <Alembic/AbcGeom/FilmBackXformOp.h>

namespace Alembic {
namespace AbcGeom {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
class CameraSample
{
public:

    //! Creates a default sample with a bunch of defaults set
    CameraSample() { reset(); }

    //! Create a default sample and set the defaults so that they
    //! calculate the provided screen window.
    CameraSample( double iTop, double iBottom, double iLeft, double iRight );

    void getScreenWindow( double & oTop, double & oBottom, double & oLeft,
        double & oRight );

    //! get the camera focal length in millimeters.
    double getFocalLength() const { return m_focalLength; }

    //! set the camera focal length in millimeters.
    void  setFocalLength( double iVal ) { m_focalLength = iVal; }

    //! get the horizontal camera film back in centimeters
    double getHorizontalAperture() const { return m_horizontalAperture; }

    //! set the horizontal camera film back in centimeters
    void setHorizontalAperture( double iVal ) { m_horizontalAperture = iVal; }

    //! get the horizontal film back offset in centimeters
    double getHorizontalFilmOffset() const { return m_horizontalFilmOffset; }

    //! set the horizontal film back offset in centimeters
    void setHorizontalFilmOffset( double iVal )
    { m_horizontalFilmOffset = iVal; }

    //! get the vertical camera film back in centimeters
    double getVerticalAperture() const { return m_verticalAperture; }

    //! set the vertical camera film back in centimeters
    void setVerticalAperture( double iVal ) { m_verticalAperture = iVal; }

    //! get the vertical film back offset in centimeters
    double getVerticalFilmOffset() const { return m_verticalFilmOffset; }

    //! set the vertical film back offset in centimeters
    void setVerticalFilmOffset( double iVal ) { m_verticalFilmOffset = iVal; }

    //! get the amount the camera's lens compresses the image horizontally
    //! (width / height aspect ratio)
    double getLensSqueezeRatio() const { return m_lensSqueezeRatio; }

    //! set the amount the camera's lens compresses the image horizontally
    //! (width / height aspect ratio)
    void setLensSqueezeRatio( double iVal )
    { m_lensSqueezeRatio = iVal; }

    //! get over scan fractional percentage for the left part of the
    //! screen window
    double getOverScanLeft() { return m_overscanLeft; }

    //! set over scan fractional percentage for the left part of the
    //! screen window
    void setOverScanLeft( double iVal ) { m_overscanLeft = iVal; }

    //! get over scan fractional percentage for the right part of the
    //! screen window
    double getOverScanRight() const { return m_overscanRight; }

    //! set over scan fractional percentage for the right part of the
    //! screen window
    void setOverScanRight( double iVal ) { m_overscanRight = iVal; }

    //! get over scan fractional percentage for the top part of the
    //! screen window
    double getOverScanTop() const { return m_overscanTop; }

    //! get over scan fractional percentage for the top part of the
    //! screen window
    void setOverScanTop( double iVal ) { m_overscanTop = iVal; }

    //! get over scan fractional percentage for the bottom part of the
    //! screen window
    double getOverScanBottom() const { return m_overscanBottom; }

    //! set over scan fractional percentage for the bottom part of the
    //! screen window
    void setOverScanBottom( double iVal ) { m_overscanBottom = iVal; }

    //! get the f-stop (focal length divided by "effective" lens diameter)
    double getFStop() const { return m_fStop; }

    //! set the f-stop (focal length divided by "effective" lens diameter)
    void setFStop( double iVal ) { m_fStop = iVal; }

    //! get the distance from the camera to the object being focused on
    //! in centimeters
    double getFocusDistance() const { return m_focusDistance; }

    //! set the distance from the camera to the object being focused on
    //! in centimeters
    void setFocusDistance( double iVal ) { m_focusDistance = iVal; }

    //! get the frame relative shutter open time in seconds.
    double getShutterOpen() const { return m_shutterOpen; }

    //! set the frame relative shutter open time in seconds.
    void setShutterOpen( double iVal ) { m_shutterOpen = iVal; }

    //! get the frame relative shutter close time in seconds.
    double getShutterClose() const { return m_shutterClose; }

    //! set the frame relative shutter open time in seconds.
    void setShutterClose( double iVal ) { m_shutterClose = iVal; }

    //! get the distance from the camera to the near clipping plane in
    //! centimeters
    double getNearClippingPlane() const { return m_nearClippingPlane; }

    //! set the distance from the camera to the near clipping plane in
    //! centimeters
    void setNearClippingPlane( double iVal ) { m_nearClippingPlane = iVal; }

    //! get the distance from the camera to the far clipping plane in
    //! centimeters
    double getFarClippingPlane() const { return m_farClippingPlane; }

    //! set the distance from the camera to the near clipping plane in
    //! centimeters
    void setFarClippingPlane( double iVal ) { m_farClippingPlane = iVal; }

    Abc::Box3d getChildBounds() const { return m_childBounds; }
    void setChildBounds( const Abc::Box3d & iBounds )
    { m_childBounds = iBounds; }


    // helper function for getting one of the 16 core, non film back xform op
    // related values
    double getCoreValue( std::size_t iIndex ) const;

    // calculated the field of view in degrees
    double getFieldOfView() const;;

    // add an op and return the index of the op in its op-stack
    std::size_t addOp( FilmBackXformOp iOp );

    FilmBackXformOp getOp( std::size_t iIndex ) const;
    FilmBackXformOp &operator[]( const std::size_t &iIndex );
    const FilmBackXformOp &operator[]( const std::size_t &iIndex ) const;

    //! Returns the concatenated 3x3 film back matrix
    Abc::M33d getFilmBackMatrix () const;

    std::size_t getNumOps() const;
    std::size_t getNumOpChannels() const;

    void reset()
    {
        // in mm
        m_focalLength = 35.0;

        // in cm
        m_horizontalAperture = 3.6;
        m_horizontalFilmOffset = 0.0;
        m_verticalAperture = 2.4;
        m_verticalFilmOffset = 0.0;

        // width/height lens aspect ratio
        m_lensSqueezeRatio = 1.0;

        m_overscanLeft = 0.0;
        m_overscanRight = 0.0;
        m_overscanTop = 0.0;
        m_overscanBottom = 0.0;

        // optical property of the lens, focal length divided by 
        // "effective" lens diameter
        m_fStop = 5.6;

        m_focusDistance = 5.0;
        m_shutterOpen = 0.0;
        m_shutterClose = 0.020833333333333332;

        m_nearClippingPlane = 0.1;
        m_farClippingPlane = 100000.0;

        m_childBounds.makeEmpty();
        m_ops.clear();
    }

private:
    double m_focalLength;
    double m_horizontalAperture;
    double m_horizontalFilmOffset;
    double m_verticalAperture;
    double m_verticalFilmOffset;
    double m_lensSqueezeRatio;

    double m_overscanLeft;
    double m_overscanRight;
    double m_overscanTop;
    double m_overscanBottom;

    double m_fStop;
    double m_focusDistance;
    double m_shutterOpen;
    double m_shutterClose;

    double m_nearClippingPlane;
    double m_farClippingPlane;

    Abc::Box3d m_childBounds;

    std::vector<FilmBackXformOp> m_ops;
};

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcGeom
} // End namespace Alembic

#endif
