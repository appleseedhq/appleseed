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

#ifndef _Alembic_AbcGeom_GeometryScope_h_
#define _Alembic_AbcGeom_GeometryScope_h_

#include <Alembic/AbcGeom/Foundation.h>

namespace Alembic {
namespace AbcGeom {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
//! "GeometryScope" is the name we use for what is called Primitive Variable
//! Class in the Renderman Interface. "Primitive Variable Class" is a bit
//! non-descriptive in a geometry caching context, so we use the somewhat
//! more meaningful (but still admittedly vague) "GeometryScope".
//! Below is a table reproduced from PRMan Application Note #22
//! describing what each of these labels means for different types
//! of geometry.
//-*****************************************************************************

//-*****************************************************************************
//! From Prman Application Note #22
//! For every geometric prim, there is 1 value for "constant" geometry scope.
//! To save space in my little table here, I'll leave out the "constant" column
//! but just imagine it with a big happy column of '1's.
//!
//!-----------------------------------------------------------------------------
//! QUADRIC & POLYGON SURFACE PRIMITIVES                                       |
//! Surface Primitive     | Uniform | Varying | Vertex  | Facevarying          |
//!-----------------------|---------|---------|---------|----------------------|
//! Quadrics              | 1       | 4       | 4       | 4                    |
//! Polygon               | 1       | nverts  | nverts  | nverts               |
//! GeneralPolygon        | 1       | nverts  | nverts  | nverts               |
//! PointsPolygons        | npolys  | nverts  | nverts  | sum(nvertices_i)     |
//! PointsGeneralPolygons | npolys  | nverts  | nverts  | sum(nvertices_i)     |
//! Points                | 1       | npoints | npoints | npoints              |
//! SubdivisionMesh       | nfaces  | nverts  | nverts  | sum(nvertices_i)     |
//!-----------------------------------------------------------------------------
//!
//!-----------------------------------------------------------------------------
//! PARAMETRIC SURFACE PRIMITIVES                                              |
//! Surface Primitive  | Uniform         | Varying and      | Vertex           |
//!                    |                 |  Facevarying     |                  |
//!--------------------|-----------------|------------------|------------------|
//! Patch bilinear     | 1               | 4                | 4                |
//! Patch bicubic      | 1               | 4                | 16               |
//! PatchMesh bilinear | (nu-unowrap)*   | nu*nv            | nu*nv            |
//!                    |   (nv-vnowrap)  |                  |                  |
//! PatchMesh bicubic  | usegs * vsegs   | (usegs+unowrap)* | nu*nv            |
//!                    |                 |  (vsegs+vnowrap) |                  |
//! NuPatch            | (nu-uorder+1)*  | (nu-uorder+2)*   | nu*nv            |
//!                    |   (nv-vorder+1) |  (nv-vorder+2)   |                  |
//! Curves linear      | sum(nsegs_i     | sum(nvertices_i) | sum(nvertices_i) |
//!                    |     -nowrap)    |                  |                  |
//! Curves cubic       | sum(nsegs_i)    | sum(nsegs_i      | sum(nvertices_i) |
//!                    |                 |     +nowrap)     |                  |
//! Blobby             |  1              | nleaf            | nleaf            |
//!-----------------------------------------------------------------------------

//-*****************************************************************************
enum GeometryScope
{
    kConstantScope = 0,
    kUniformScope = 1,
    kVaryingScope = 2,
    kVertexScope = 3,
    kFacevaryingScope = 4,

    kUnknownScope = 127
};

//-*****************************************************************************
//! These functions will set or get the GeometryScope out of
//! an object, by looking at its metadata.
//-*****************************************************************************

//-*****************************************************************************
inline void SetGeometryScope( AbcA::MetaData &ioMetaData, GeometryScope iScope )
{
    switch ( iScope )
    {
    case kConstantScope: ioMetaData.set( "geoScope", "con" ); return;
    case kUniformScope: ioMetaData.set( "geoScope", "uni" ); return;
    case kVaryingScope: ioMetaData.set( "geoScope", "var" ); return;
    case kVertexScope: ioMetaData.set( "geoScope", "vtx" ); return;
    case kFacevaryingScope: ioMetaData.set( "geoScope", "fvr" ); return;
    case kUnknownScope: return;
    default: return;
    }
}

//-*****************************************************************************
inline GeometryScope GetGeometryScope( const AbcA::MetaData &iMetaData )
{
    const std::string val = iMetaData.get( "geoScope" );
    if ( val == "con" ) { return kConstantScope; }
    else if ( val == "uni" ) { return kUniformScope; }
    else if ( val == "var" ) { return kVaryingScope; }
    else if ( val == "vtx" ) { return kVertexScope; }
    else if ( val == "fvr" ) { return kFacevaryingScope; }
    else { return kUnknownScope; }
}

//-*****************************************************************************
//! These functions below are just implementations of the statements
//! in the table above.
//-*****************************************************************************

//! Works for any quadric.
//! ....
size_t GeometryScopeNumValuesQuadrics( GeometryScope iScope );

//! Works for GeneralPolygon & Polygon
//! ...
size_t GeometryScopeNumValuesPolygon( GeometryScope iScope,
                                      size_t iNumVerts );

//! Works for PointsPolygons & PointsGeneralPolygons
//! ...
size_t GeometryScopeNumValuesPointsPolygons( GeometryScope iScope,
                                             size_t iNumPolys,
                                             size_t iNumVerts,
                                             size_t iSumOfCounts );

//! Works for Points
//! ...
size_t GeometryScopeNumValuesPoints( GeometryScope iScope,
                                     size_t iNumPoints );

//! Works for Subds, but not hierarchical.
//! ...
size_t GeometryScopeNumValuesSubdivisionMesh( GeometryScope iScope,
                                              size_t iNumFaces,
                                              size_t iNumVerts,
                                              size_t iSumOfCounts );

//! Works for Bilinear Patch
//! ...
size_t GeometryScopeNumValuesBilinearPatch( GeometryScope iScope );

//! Works for Bicubic Patch
//! ...
size_t GeometryScopeNumValuesBicubicPatch( GeometryScope iScope );

//! Works for BilinearPatchMesh
//! ...
size_t GeometryScopeNumValuesBilinearPatchMesh( GeometryScope iScope,
                                                size_t iNu, bool iUNoWrap,
                                                size_t iNv, bool iVNoWrap );

//! Works for BicubicPatchMesh
//! Usegs = Nu-3.
//! Vsegs = Nv-3.
size_t GeometryScopeNumValuesBicubicPatchMesh( GeometryScope iScope,
                                               size_t iNu, bool iUNoWrap,
                                               size_t iNv, bool iVNoWrap );

//! Works for Nurbs patch
//! ...
size_t GeometryScopeNumValuesNuPatch( GeometryScope iScope,
                                      size_t iNu, size_t iUorder,
                                      size_t iNv, size_t iVorder );

//! Works for Linear Curves
//! ...
size_t GeometryScopeNumValuesLinearCurves( GeometryScope iScope,
                                           size_t iNumCurves, bool iNoWrap,
                                           size_t iSumOfCounts );
//! Works for Bicubic Curves
//! ...
size_t GeometryScopeNumValuesCubicCurves( GeometryScope iScope,
                                          size_t iNumCurves, bool iNoWrap,
                                          size_t iSumOfCounts );



} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcGeom
} // End namespace Alembic



#endif
