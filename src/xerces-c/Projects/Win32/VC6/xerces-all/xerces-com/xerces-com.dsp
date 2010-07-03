# Microsoft Developer Studio Project File - Name="xml4com" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=xml4com - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "xerces-com.mak".
!MESSAGE 
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

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "xml4com - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\..\..\..\Build\Win32\xml4com\Debug"
# PROP Intermediate_Dir "..\..\..\..\..\Build\Win32\xml4com\Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /MTd /W3 /Gm /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /Yu"stdafx.h" /FD /GZ /c
# ADD CPP /MTd /W3 /Gm /GX /ZI /Od /I "..\..\..\..\..\src" /I "..\..\..\..\..\src\xercesc\com" /D "_DEBUG" /D "_MBCS" /D "_ATL_DEBUG_INTERFACES" /D "_ATL_DEBUG_QI" /D "_ATL_DEBUG_REFCOUNT" /D "WIN32" /D "_WINDOWS" /D "_USRDLL" /D "DEVENV_VCPP" /D "XML_SINGLEDLL" /D "XML_USE_WIN32_TRANSCODER" /D "XML_USE_WIN32_MSGLOADER" /Yu"stdafx.h" /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wininet.lib shlwapi.lib xerces-c_2D.lib /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept /libpath:"..\..\..\..\..\Build\Win32\VC6\Debug"
# Begin Custom Build - Performing registration
OutDir=.\..\..\..\..\..\Build\Win32\xml4com\Debug
TargetDir=\Xerces\xml-xerces\c\Build\Win32\xml4com\Debug
TargetPath=\Xerces\xml-xerces\c\Build\Win32\xml4com\Debug\xerces-com.dll
InputPath=\Xerces\xml-xerces\c\Build\Win32\xml4com\Debug\xerces-com.dll
SOURCE="$(InputPath)"

BuildCmds= \
	echo copy ..\..\..\..\..\Build\Win32\VC6\Debug\xerces-c_2_8D.dll "$(TargetDir)" \
	copy ..\..\..\..\..\Build\Win32\VC6\Debug\xerces-c_2_8D.dll "$(TargetDir)" \
	regsvr32 /s /c "$(TargetPath)" \
	echo regsvr32 exec. time > "$(OutDir)\regsvr32.trg" \
	

"$(TargetDir)\xerces-c_2_8D.dll" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"$(OutDir)\regsvr32.trg" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ELSEIF  "$(CFG)" == "xml4com - Win32 Release MinDependency"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "ReleaseMinDependency"
# PROP BASE Intermediate_Dir "ReleaseMinDependency"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\..\..\..\Build\Win32\xml4com\ReleaseMinDependency"
# PROP Intermediate_Dir "..\..\..\..\..\Build\Win32\xml4com\ReleaseMinDependency"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /MT /W3 /O1 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "_ATL_STATIC_REGISTRY" /D "_ATL_MIN_CRT" /Yu"stdafx.h" /FD /c
# ADD CPP /MT /W3 /GX /O1 /I "..\..\..\..\..\src" /I "..\..\..\..\..\src\xercesc\com" /D "NDEBUG" /D "_MBCS" /D "_ATL_STATIC_REGISTRY" /D "WIN32" /D "_WINDOWS" /D "_USRDLL" /D "DEVENV_VCPP" /D "XML_SINGLEDLL" /D "XML_USE_WIN32_TRANSCODER" /D "XML_USE_WIN32_MSGLOADER" /Yu"stdafx.h" /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /subsystem:windows /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wininet.lib shlwapi.lib xerces-c_2.lib /subsystem:windows /dll /machine:I386 /libpath:"..\..\..\..\..\Build\Win32\VC6\Release"
# Begin Custom Build - Performing registration
OutDir=.\..\..\..\..\..\Build\Win32\xml4com\ReleaseMinDependency
TargetDir=\Xerces\xml-xerces\c\Build\Win32\xml4com\ReleaseMinDependency
TargetPath=\Xerces\xml-xerces\c\Build\Win32\xml4com\ReleaseMinDependency\xerces-com.dll
InputPath=\Xerces\xml-xerces\c\Build\Win32\xml4com\ReleaseMinDependency\xerces-com.dll
SOURCE="$(InputPath)"

BuildCmds= \
	echo copy ..\..\..\..\..\Build\Win32\VC6\Release\xerces-c_2_8.dll $(TargetDir) \
	copy ..\..\..\..\..\Build\Win32\VC6\Release\xerces-c_2_8.dll $(TargetDir) \
	regsvr32 /s /c "$(TargetPath)" \
	echo regsvr32 exec. time > "$(OutDir)\regsvr32.trg" \
	

"$(TargetDir)\xerces-c_2_8.dll" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"$(OutDir)\regsvr32.trg" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ENDIF 

# Begin Target

# Name "xml4com - Win32 Debug"
# Name "xml4com - Win32 Release MinDependency"
# Begin Group "com"

# PROP Default_Filter "h;hpp;hxx;hm;inl;cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\BindStatusCallback.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\IXMLDOMCharacterDataImpl.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\IXMLDOMNodeImpl.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\IXMLDOMNodeImpl.inl
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\IXMLDOMTextImpl.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\NodeContainerImpl.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\Resource.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\StdAfx.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\xml4com.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\xml4comCP.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMAttribute.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMCDATASection.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMComment.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMDocument.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMDocumentFragment.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMDocumentType.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMElement.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMEntity.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMEntityReference.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMImplementation.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMNamedNodeMap.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMNodeList.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMNotation.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMParseError.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMProcessingInstruction.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMText.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMUtil.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMXMLDecl.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLHTTPRequest.h
# End Source File
# End Group
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\BindStatusCallback.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\StdAfx.cpp
# ADD CPP /Yc"stdafx.h"
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\xml4com.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\xml4com.def
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\xml4com.idl
# ADD MTL /I "..\..\..\..\..\src\xercesc\com" /tlb "..\..\..\..\..\Build\Win32\xml4com\xml4com.tlb" /h "..\..\..\..\..\src\xercesc\com\xml4com.h" /iid "..\..\..\..\..\src\xercesc\com\xml4com_i.c"
# SUBTRACT MTL /Oicf
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\xml4com.rc
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMAttribute.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMDocument.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMDocumentType.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMElement.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMEntity.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMImplementation.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMNamedNodeMap.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMNodeList.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMNotation.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMParseError.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMProcessingInstruction.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMUtil.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLDOMXMLDecl.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\XMLHTTPRequest.cpp
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\xmldocument.rgs
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\com\xmlhttprequest.rgs
# End Source File
# End Group
# End Group
# End Target
# End Project
