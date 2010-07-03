# Microsoft Developer Studio Generated NMAKE File, Based on xerces-com.dsp
!IF "$(CFG)" == ""
CFG=xml4com - Win32 Debug
!MESSAGE No configuration specified. Defaulting to xml4com - Win32 Debug.
!ENDIF

!IF "$(CFG)" != "xml4com - Win32 Debug" && "$(CFG)" != "xml4com - Win32 Release MinDependency"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE
!MESSAGE NMAKE /f "xerces-com.mak" CFG="xml4com - Win32 Debug"
!MESSAGE
!MESSAGE Possible choices for configuration are:
!MESSAGE
!MESSAGE "xml4com - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "xml4com - Win32 Release MinDependency" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE
!ERROR An invalid configuration is specified.
!ENDIF

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE
NULL=nul
!ENDIF

!IF  "$(CFG)" == "xml4com - Win32 Debug"

OUTDIR=.\..\..\..\..\..\Build\Win32\xml4com\Debug
INTDIR=.\..\..\..\..\..\Build\Win32\xml4com\Debug
# Begin Custom Macros
OutDir=.\..\..\..\..\..\Build\Win32\xml4com\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0"

ALL : "$(OUTDIR)\xerces-com.dll" "..\..\..\..\..\Build\Win32\xml4com\xml4com.tlb" "..\..\..\..\..\src\xercesc\com\xml4com.h" "..\..\..\..\..\src\xercesc\com\xml4com_i.c" ".\..\..\..\..\..\Build\Win32\xml4com\Debug\regsvr32.trg"

!ELSE

ALL : "XercesLib - Win32 Debug" "$(OUTDIR)\xerces-com.dll" "..\..\..\..\..\Build\Win32\xml4com\xml4com.tlb" "..\..\..\..\..\src\xercesc\com\xml4com.h" "..\..\..\..\..\src\xercesc\com\xml4com_i.c" ".\..\..\..\..\..\Build\Win32\xml4com\Debug\regsvr32.trg"

!ENDIF

!IF "$(RECURSE)" == "1"
CLEAN :"XercesLib - Win32 DebugCLEAN"
!ELSE
CLEAN :
!ENDIF
	-@erase "$(INTDIR)\BindStatusCallback.obj"
	-@erase "$(INTDIR)\StdAfx.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\xerces-com.pch"
	-@erase "$(INTDIR)\xml4com.obj"
	-@erase "$(INTDIR)\xml4com.res"
	-@erase "$(INTDIR)\XMLDOMAttribute.obj"
	-@erase "$(INTDIR)\XMLDOMDocument.obj"
	-@erase "$(INTDIR)\XMLDOMDocumentType.obj"
	-@erase "$(INTDIR)\XMLDOMElement.obj"
	-@erase "$(INTDIR)\XMLDOMEntity.obj"
	-@erase "$(INTDIR)\XMLDOMImplementation.obj"
	-@erase "$(INTDIR)\XMLDOMNamedNodeMap.obj"
	-@erase "$(INTDIR)\XMLDOMNodeList.obj"
	-@erase "$(INTDIR)\XMLDOMNotation.obj"
	-@erase "$(INTDIR)\XMLDOMParseError.obj"
	-@erase "$(INTDIR)\XMLDOMProcessingInstruction.obj"
	-@erase "$(INTDIR)\XMLDOMUtil.obj"
	-@erase "$(INTDIR)\XMLDOMXMLDecl.obj"
	-@erase "$(INTDIR)\XMLHTTPRequest.obj"
	-@erase "$(OUTDIR)\xerces-com.dll"
	-@erase "$(OUTDIR)\xerces-com.exp"
	-@erase "$(OUTDIR)\xerces-com.ilk"
	-@erase "$(OUTDIR)\xerces-com.lib"
	-@erase "$(OUTDIR)\xerces-com.pdb"
	-@erase "..\..\..\..\..\Build\Win32\xml4com\xml4com.tlb"
	-@erase "..\..\..\..\..\src\xercesc\com\xml4com.h"
	-@erase "..\..\..\..\..\src\xercesc\com\xml4com_i.c"
	-@erase ".\..\..\..\..\..\Build\Win32\xml4com\Debug\regsvr32.trg"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=$(CPP)
CPP_PROJ=/MTd /W3 /Gm /GX /ZI /Od /I "..\..\..\..\..\src" /I "..\..\..\..\..\src\xercesc\com" /D "_DEBUG" /D "_MBCS" /D "_ATL_DEBUG_INTERFACES" /D "_ATL_DEBUG_QI" /D "_ATL_DEBUG_REFCOUNT" /D "WIN32" /D "_WINDOWS" /D "_USRDLL" /D "DEVENV_VCPP" /D "XML_SINGLEDLL" /D "XML_USE_WIN32_TRANSCODER" /D "XML_USE_WIN32_MSGLOADER" /Fp"$(INTDIR)\xerces-com.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $<
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $<
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $<
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $<
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $<
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $<
<<

MTL=midl.exe
MTL_PROJ=
RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\xml4com.res" /d "_DEBUG"
BSC32=bscmake.exe
BSC32_FLAGS=/o"$(OUTDIR)\xerces-com.bsc"
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wininet.lib shlwapi.lib xerces-c_2D.lib /subsystem:windows /dll /incremental:yes /pdb:"$(OUTDIR)\xerces-com.pdb" /debug /machine:I386 /def:"..\..\..\..\..\src\xercesc\com\xml4com.def" /out:"$(OUTDIR)\xerces-com.dll" /implib:"$(OUTDIR)\xerces-com.lib" /pdbtype:sept /libpath:"..\..\..\..\..\Build\Win32\VC6\Debug"
DEF_FILE= \
	"..\..\..\..\..\src\xercesc\com\xml4com.def"
LINK32_OBJS= \
	"$(INTDIR)\BindStatusCallback.obj" \
	"$(INTDIR)\StdAfx.obj" \
	"$(INTDIR)\xml4com.obj" \
	"$(INTDIR)\XMLDOMAttribute.obj" \
	"$(INTDIR)\XMLDOMDocument.obj" \
	"$(INTDIR)\XMLDOMDocumentType.obj" \
	"$(INTDIR)\XMLDOMElement.obj" \
	"$(INTDIR)\XMLDOMEntity.obj" \
	"$(INTDIR)\XMLDOMImplementation.obj" \
	"$(INTDIR)\XMLDOMNamedNodeMap.obj" \
	"$(INTDIR)\XMLDOMNodeList.obj" \
	"$(INTDIR)\XMLDOMNotation.obj" \
	"$(INTDIR)\XMLDOMParseError.obj" \
	"$(INTDIR)\XMLDOMProcessingInstruction.obj" \
	"$(INTDIR)\XMLDOMUtil.obj" \
	"$(INTDIR)\XMLDOMXMLDecl.obj" \
	"$(INTDIR)\XMLHTTPRequest.obj" \
	"$(INTDIR)\xml4com.res" \
	"..\..\..\..\..\Build\Win32\VC6\Debug\xerces-c_2D.lib"

"$(OUTDIR)\xerces-com.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

OutDir=.\..\..\..\..\..\Build\Win32\xml4com\Debug
TargetDir=\Xerces-Testing\Build\Win32\xml4com\Debug
TargetPath=\Xerces-Testing\Build\Win32\xml4com\Debug\xerces-com.dll
InputPath=\Xerces-Testing\Build\Win32\xml4com\Debug\xerces-com.dll
SOURCE="$(InputPath)"

"$(OUTDIR)\regsvr32.trg" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	<<tempfile.bat
	@echo off
	echo copy ..\..\..\..\..\Build\Win32\VC6\Debug\xerces-c_2_3_0D.dll "$(TargetDir)"
	copy ..\..\..\..\..\Build\Win32\VC6\Debug\xerces-c_2_3_0D.dll "$(TargetDir)"
	regsvr32 /s /c "$(TargetPath)"
	echo regsvr32 exec. time > "$(OutDir)\regsvr32.trg"
<<
	

!ELSEIF  "$(CFG)" == "xml4com - Win32 Release MinDependency"

OUTDIR=.\..\..\..\..\..\Build\Win32\xml4com\ReleaseMinDependency
INTDIR=.\..\..\..\..\..\Build\Win32\xml4com\ReleaseMinDependency
# Begin Custom Macros
OutDir=.\..\..\..\..\..\Build\Win32\xml4com\ReleaseMinDependency
# End Custom Macros

!IF "$(RECURSE)" == "0"

ALL : "$(OUTDIR)\xerces-com.dll" "..\..\..\..\..\Build\Win32\xml4com\xml4com.tlb" "..\..\..\..\..\src\xercesc\com\xml4com.h" "..\..\..\..\..\src\xercesc\com\xml4com_i.c" ".\..\..\..\..\..\Build\Win32\xml4com\ReleaseMinDependency\regsvr32.trg" "\Xerces-Testing\Build\Win32\xml4com\ReleaseMinDependency\xerces-c_2_3_0.dll"

!ELSE

ALL : "$(OUTDIR)\xerces-com.dll" "..\..\..\..\..\Build\Win32\xml4com\xml4com.tlb" "..\..\..\..\..\src\xercesc\com\xml4com.h" "..\..\..\..\..\src\xercesc\com\xml4com_i.c" ".\..\..\..\..\..\Build\Win32\xml4com\ReleaseMinDependency\regsvr32.trg" "\Xerces-Testing\Build\Win32\xml4com\ReleaseMinDependency\xerces-c_2_3_0.dll"

!ENDIF

!IF "$(RECURSE)" == "1"
CLEAN :
!ELSE
CLEAN :
!ENDIF
	-@erase "$(INTDIR)\BindStatusCallback.obj"
	-@erase "$(INTDIR)\StdAfx.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\xerces-com.pch"
	-@erase "$(INTDIR)\xml4com.obj"
	-@erase "$(INTDIR)\xml4com.res"
	-@erase "$(INTDIR)\XMLDOMAttribute.obj"
	-@erase "$(INTDIR)\XMLDOMDocument.obj"
	-@erase "$(INTDIR)\XMLDOMDocumentType.obj"
	-@erase "$(INTDIR)\XMLDOMElement.obj"
	-@erase "$(INTDIR)\XMLDOMEntity.obj"
	-@erase "$(INTDIR)\XMLDOMImplementation.obj"
	-@erase "$(INTDIR)\XMLDOMNamedNodeMap.obj"
	-@erase "$(INTDIR)\XMLDOMNodeList.obj"
	-@erase "$(INTDIR)\XMLDOMNotation.obj"
	-@erase "$(INTDIR)\XMLDOMParseError.obj"
	-@erase "$(INTDIR)\XMLDOMProcessingInstruction.obj"
	-@erase "$(INTDIR)\XMLDOMUtil.obj"
	-@erase "$(INTDIR)\XMLDOMXMLDecl.obj"
	-@erase "$(INTDIR)\XMLHTTPRequest.obj"
	-@erase "$(OUTDIR)\xerces-com.dll"
	-@erase "$(OUTDIR)\xerces-com.exp"
	-@erase "$(OUTDIR)\xerces-com.lib"
	-@erase "..\..\..\..\..\Build\Win32\xml4com\xml4com.tlb"
	-@erase "..\..\..\..\..\src\xercesc\com\xml4com.h"
	-@erase "..\..\..\..\..\src\xercesc\com\xml4com_i.c"
	-@erase ".\..\..\..\..\..\Build\Win32\xml4com\ReleaseMinDependency\regsvr32.trg"
	-@erase "\Xerces-Testing\Build\Win32\xml4com\ReleaseMinDependency\xerces-c_2_3_0.dll"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=$(CPP)
CPP_PROJ=/MT /W3 /GX /O1 /I "..\..\..\..\..\src" /I "..\..\..\..\..\src\xercesc\com" /D "NDEBUG" /D "_MBCS" /D "_ATL_STATIC_REGISTRY" /D "WIN32" /D "_WINDOWS" /D "_USRDLL" /D "DEVENV_VCPP" /D "XML_SINGLEDLL" /D "XML_USE_WIN32_TRANSCODER" /D "XML_USE_WIN32_MSGLOADER" /Fp"$(INTDIR)\xerces-com.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $<
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $<
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $<
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $<
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $<
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $<
<<

MTL=midl.exe
MTL_PROJ=
RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\xml4com.res" /d "NDEBUG"
BSC32=bscmake.exe
BSC32_FLAGS=/o"$(OUTDIR)\xerces-com.bsc"
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wininet.lib shlwapi.lib xerces-c_2.lib /subsystem:windows /dll /incremental:no /pdb:"$(OUTDIR)\xerces-com.pdb" /machine:I386 /def:"..\..\..\..\..\src\xercesc\com\xml4com.def" /out:"$(OUTDIR)\xerces-com.dll" /implib:"$(OUTDIR)\xerces-com.lib" /libpath:"..\..\..\..\..\Build\Win32\VC6\Release"
DEF_FILE= \
	"..\..\..\..\..\src\xercesc\com\xml4com.def"
LINK32_OBJS= \
	"$(INTDIR)\BindStatusCallback.obj" \
	"$(INTDIR)\StdAfx.obj" \
	"$(INTDIR)\xml4com.obj" \
	"$(INTDIR)\XMLDOMAttribute.obj" \
	"$(INTDIR)\XMLDOMDocument.obj" \
	"$(INTDIR)\XMLDOMDocumentType.obj" \
	"$(INTDIR)\XMLDOMElement.obj" \
	"$(INTDIR)\XMLDOMEntity.obj" \
	"$(INTDIR)\XMLDOMImplementation.obj" \
	"$(INTDIR)\XMLDOMNamedNodeMap.obj" \
	"$(INTDIR)\XMLDOMNodeList.obj" \
	"$(INTDIR)\XMLDOMNotation.obj" \
	"$(INTDIR)\XMLDOMParseError.obj" \
	"$(INTDIR)\XMLDOMProcessingInstruction.obj" \
	"$(INTDIR)\XMLDOMUtil.obj" \
	"$(INTDIR)\XMLDOMXMLDecl.obj" \
	"$(INTDIR)\XMLHTTPRequest.obj" \
	"$(INTDIR)\xml4com.res"

"$(OUTDIR)\xerces-com.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

OutDir=.\..\..\..\..\..\Build\Win32\xml4com\ReleaseMinDependency
TargetDir=\Xerces-Testing\Build\Win32\xml4com\ReleaseMinDependency
TargetPath=\Xerces-Testing\Build\Win32\xml4com\ReleaseMinDependency\xerces-com.dll
InputPath=\Xerces-Testing\Build\Win32\xml4com\ReleaseMinDependency\xerces-com.dll
SOURCE="$(InputPath)"

"$(OUTDIR)\xerces-c_2_2_0.dll"	"$(OUTDIR)\regsvr32.trg" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	<<tempfile.bat
	@echo off
	echo copy ..\..\..\..\..\Build\Win32\VC6\Release\xerces-c_2_3_0.dll $(TargetDir)
	copy ..\..\..\..\..\Build\Win32\VC6\Release\xerces-c_2_3_0.dll $(TargetDir)
	regsvr32 /s /c "$(TargetPath)"
	echo regsvr32 exec. time > "$(OutDir)\regsvr32.trg"
<<
	

!ENDIF


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("xerces-com.dep")
!INCLUDE "xerces-com.dep"
!ELSE
!MESSAGE Warning: cannot find "xerces-com.dep"
!ENDIF
!ENDIF


!IF "$(CFG)" == "xml4com - Win32 Debug" || "$(CFG)" == "xml4com - Win32 Release MinDependency"
SOURCE=..\..\..\..\..\src\xercesc\com\BindStatusCallback.cpp

"$(INTDIR)\BindStatusCallback.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\xerces-com.pch"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\com\StdAfx.cpp

!IF  "$(CFG)" == "xml4com - Win32 Debug"

CPP_SWITCHES=/MTd /W3 /Gm /GX /ZI /Od /I "..\..\..\..\..\src" /I "..\..\..\..\..\src\xercesc\com" /D "_DEBUG" /D "_MBCS" /D "_ATL_DEBUG_INTERFACES" /D "_ATL_DEBUG_QI" /D "_ATL_DEBUG_REFCOUNT" /D "WIN32" /D "_WINDOWS" /D "_USRDLL" /D "DEVENV_VCPP" /D "XML_SINGLEDLL" /D "XML_USE_WIN32_TRANSCODER" /D "XML_USE_WIN32_MSGLOADER" /Fp"$(INTDIR)\xerces-com.pch" /Yc"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c

"$(INTDIR)\StdAfx.obj"	"$(INTDIR)\xerces-com.pch" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "xml4com - Win32 Release MinDependency"

CPP_SWITCHES=/MT /W3 /GX /O1 /I "..\..\..\..\..\src" /I "..\..\..\..\..\src\xercesc\com" /D "NDEBUG" /D "_MBCS" /D "_ATL_STATIC_REGISTRY" /D "WIN32" /D "_WINDOWS" /D "_USRDLL" /D "DEVENV_VCPP" /D "XML_SINGLEDLL" /D "XML_USE_WIN32_TRANSCODER" /D "XML_USE_WIN32_MSGLOADER" /Fp"$(INTDIR)\xerces-com.pch" /Yc"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c

"$(INTDIR)\StdAfx.obj"	"$(INTDIR)\xerces-com.pch" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF

SOURCE=..\..\..\..\..\src\xercesc\com\xml4com.cpp

"$(INTDIR)\xml4com.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\xerces-com.pch"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\com\xml4com.idl

!IF  "$(CFG)" == "xml4com - Win32 Debug"

MTL_SWITCHES=/I "..\..\..\..\..\src\xercesc\com" /tlb "..\..\..\..\..\Build\Win32\xml4com\xml4com.tlb" /h "..\..\..\..\..\src\xercesc\com\xml4com.h" /iid "..\..\..\..\..\src\xercesc\com\xml4com_i.c"

"..\..\..\..\..\Build\Win32\xml4com\xml4com.tlb"	"..\..\..\..\..\src\xercesc\com\xml4com.h"	"..\..\..\..\..\src\xercesc\com\xml4com_i.c" : $(SOURCE) "$(INTDIR)"
	$(MTL) @<<
  $(MTL_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "xml4com - Win32 Release MinDependency"

MTL_SWITCHES=/I "..\..\..\..\..\src\xercesc\com" /tlb "..\..\..\..\..\Build\Win32\xml4com\xml4com.tlb" /h "..\..\..\..\..\src\xercesc\com\xml4com.h" /iid "..\..\..\..\..\src\xercesc\com\xml4com_i.c"

"..\..\..\..\..\Build\Win32\xml4com\xml4com.tlb"	"..\..\..\..\..\src\xercesc\com\xml4com.h"	"..\..\..\..\..\src\xercesc\com\xml4com_i.c" : $(SOURCE) "$(INTDIR)"
	$(MTL) @<<
  $(MTL_SWITCHES) $(SOURCE)
<<


!ENDIF

SOURCE=..\..\..\..\..\src\xercesc\com\xml4com.rc

!IF  "$(CFG)" == "xml4com - Win32 Debug"


"$(INTDIR)\xml4com.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) /l 0x409 /fo"$(INTDIR)\xml4com.res" /i "\Xerces-Testing\src\xercesc\com" /d "_DEBUG" $(SOURCE)


!ELSEIF  "$(CFG)" == "xml4com - Win32 Release MinDependency"


"$(INTDIR)\xml4com.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) /l 0x409 /fo"$(INTDIR)\xml4com.res" /i "\Xerces-Testing\src\xercesc\com" /d "NDEBUG" $(SOURCE)


!ENDIF

SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMAttribute.cpp

"$(INTDIR)\XMLDOMAttribute.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\xerces-com.pch"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMDocument.cpp

"$(INTDIR)\XMLDOMDocument.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\xerces-com.pch"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMDocumentType.cpp

"$(INTDIR)\XMLDOMDocumentType.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\xerces-com.pch"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMElement.cpp

"$(INTDIR)\XMLDOMElement.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\xerces-com.pch"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMEntity.cpp

"$(INTDIR)\XMLDOMEntity.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\xerces-com.pch"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMImplementation.cpp

"$(INTDIR)\XMLDOMImplementation.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\xerces-com.pch"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMNamedNodeMap.cpp

"$(INTDIR)\XMLDOMNamedNodeMap.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\xerces-com.pch"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMNodeList.cpp

"$(INTDIR)\XMLDOMNodeList.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\xerces-com.pch"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMNotation.cpp

"$(INTDIR)\XMLDOMNotation.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\xerces-com.pch"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMParseError.cpp

"$(INTDIR)\XMLDOMParseError.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\xerces-com.pch"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMProcessingInstruction.cpp

"$(INTDIR)\XMLDOMProcessingInstruction.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\xerces-com.pch"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMUtil.cpp

"$(INTDIR)\XMLDOMUtil.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\xerces-com.pch"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMXMLDecl.cpp

"$(INTDIR)\XMLDOMXMLDecl.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\xerces-com.pch"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\com\XMLHTTPRequest.cpp

"$(INTDIR)\XMLHTTPRequest.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\xerces-com.pch"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!IF  "$(CFG)" == "xml4com - Win32 Debug"

"XercesLib - Win32 Debug" :
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win32 Debug"
   cd "..\xerces-com"

"XercesLib - Win32 DebugCLEAN" :
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win32 Debug" RECURSE=1 CLEAN
   cd "..\xerces-com"

!ELSEIF  "$(CFG)" == "xml4com - Win32 Release MinDependency"

!ENDIF


!ENDIF

