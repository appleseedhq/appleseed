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
  ArchiveInfo.cpp
  ErrorHandler.cpp

  IArchive.cpp
  IArrayProperty.cpp
  ICompoundProperty.cpp
  IObject.cpp
  ISampleSelector.cpp
  IScalarProperty.cpp

  OArchive.cpp
  OArrayProperty.cpp
  OCompoundProperty.cpp
  OObject.cpp
  OScalarProperty.cpp
)

SET( H_FILES 
  All.h
  Base.h
  ErrorHandler.h
  Foundation.h
  Argument.h
  ArchiveInfo.h

  IArchive.h
  IArrayProperty.h
  IBaseProperty.h
  ICompoundProperty.h
  IObject.h
  ISampleSelector.h
  IScalarProperty.h
  ISchema.h
  ISchemaObject.h
  ITypedArrayProperty.h
  ITypedScalarProperty.h

  OArchive.h
  OArrayProperty.h
  OBaseProperty.h
  OCompoundProperty.h
  OObject.h
  OScalarProperty.h
  OSchema.h
  OSchemaObject.h
  OTypedArrayProperty.h
  OTypedScalarProperty.h

  TypedArraySample.h
  TypedPropertyTraits.h
)

SET( SOURCE_FILES ${CXX_FILES} ${H_FILES} )

ADD_LIBRARY( AlembicAbc ${SOURCE_FILES} )

INSTALL( TARGETS AlembicAbc
         LIBRARY DESTINATION lib
         ARCHIVE DESTINATION lib/static )
 
INSTALL( FILES ${H_FILES}
         DESTINATION include/Alembic/Abc
         PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )

ADD_SUBDIRECTORY( Tests )
