##-*****************************************************************************
##
## Copyright (c) 2009-2011,
##  Sony Pictures Imageworks Inc. and
##  Industrial Light & Magic, a division of Lucasfilm Entertainment Company Ltd.
##
## All rights reserved.
##
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions are
## met:
## *       Redistributions of source code must retain the above copyright
## notice, this list of conditions and the following disclaimer.
## *       Redistributions in binary form must reproduce the above
## copyright notice, this list of conditions and the following disclaimer
## in the documentation and/or other materials provided with the
## distribution.
## *       Neither the name of Industrial Light & Magic nor the names of
## its contributors may be used to endorse or promote products derived
## from this software without specific prior written permission.
##
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
## "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
## LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
## A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
## OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
## SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
## LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
## DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
## THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
## (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
## OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
##
##-*****************************************************************************

# C++ files for this project
SET( CXX_FILES

  ArchiveBounds.cpp

  GeometryScope.cpp

  FilmBackXformOp.cpp
  CameraSample.cpp
  ICamera.cpp
  OCamera.cpp

  Basis.cpp
  ICurves.cpp
  OCurves.cpp

  OFaceSet.cpp
  IFaceSet.cpp

  ONuPatch.cpp
  INuPatch.cpp

  OPoints.cpp
  IPoints.cpp

  OPolyMesh.cpp
  IPolyMesh.cpp

  OSubD.cpp
  ISubD.cpp

  Visibility.cpp

  XformOp.cpp
  XformSample.cpp
  IXform.cpp
  OXform.cpp
)

SET( H_FILES

  All.h

  Foundation.h

  ArchiveBounds.h

  IGeomBase.h
  OGeomBase.h

  GeometryScope.h

  SchemaInfoDeclarations.h

  FilmBackXformOp.h
  CameraSample.h
  ICamera.h
  OCamera.h

  Basis.h
  CurveType.h
  ICurves.h
  OCurves.h

  FaceSetExclusivity.h
  OFaceSet.h
  IFaceSet.h

  ONuPatch.h
  INuPatch.h

  OGeomParam.h
  IGeomParam.h

  OPoints.h
  IPoints.h

  OPolyMesh.h
  IPolyMesh.h

  OSubD.h
  ISubD.h

  Visibility.h

  XformOp.h
  XformSample.h
  IXform.h
  OXform.h
)

SET( SOURCE_FILES ${CXX_FILES} ${H_FILES} )

ADD_LIBRARY( AlembicAbcGeom ${SOURCE_FILES} )

INSTALL( TARGETS AlembicAbcGeom
         LIBRARY DESTINATION lib
         ARCHIVE DESTINATION lib/static )

INSTALL( FILES ${H_FILES}
         DESTINATION include/Alembic/AbcGeom
         PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )

ADD_SUBDIRECTORY( Tests )
