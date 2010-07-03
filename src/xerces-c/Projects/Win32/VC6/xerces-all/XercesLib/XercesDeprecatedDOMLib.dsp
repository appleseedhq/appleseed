# Microsoft Developer Studio Project File - Name="XercesDeprecatedDOMLib" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=XercesDeprecatedDOMLib - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "XercesDeprecatedDOMLib.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "XercesDeprecatedDOMLib.mak" CFG="XercesDeprecatedDOMLib - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "XercesDeprecatedDOMLib - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "XercesDeprecatedDOMLib - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "XercesDeprecatedDOMLib - Win64 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "XercesDeprecatedDOMLib - Win64 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "XercesDeprecatedDOMLib - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\..\..\..\Build\Win32\VC6\Release"
# PROP Intermediate_Dir "..\..\..\..\..\Build\Win32\VC6\Release\obj"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /G6 /MD /Za /W3 /GX /O2 /Ob2 /I "..\..\..\..\..\src" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "PLATFORM_WIN32" /D "_CRTDBG_MAP_ALLOC" /D "PROJ_DEPRECATED_DOM" /FD /c
# SUBTRACT CPP /Fr
# ADD BASE MTL /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /subsystem:windows /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib advapi32.lib ws2_32.lib xerces-c_2.lib /base:"0x12000000" /subsystem:windows /dll /map /machine:I386 /out:"..\..\..\..\..\Build\Win32\VC6\Release\xerces-depdom_2_8.dll" /implib:"..\..\..\..\..\Build\Win32\VC6\Release\xerces-depdom_2.lib" /libpath:"..\..\..\..\..\Build\Win32\VC6\Release"
# SUBTRACT LINK32 /pdb:none /incremental:yes

!ELSEIF  "$(CFG)" == "XercesDeprecatedDOMLib - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\..\..\..\Build\Win32\VC6\Debug"
# PROP Intermediate_Dir "..\..\..\..\..\Build\Win32\VC6\Debug\obj"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /G6 /MDd /Za /W3 /Gm /GX /Zi /Od /I "..\..\..\..\..\src" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "PLATFORM_WIN32" /D "_CRTDBG_MAP_ALLOC" /D "PROJ_DEPRECATED_DOM" /FD /c
# SUBTRACT CPP /Fr /YX
# ADD BASE MTL /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib advapi32.lib ws2_32.lib xerces-c_2D.lib /base:"0x12000000" /subsystem:windows /dll /debug /machine:I386 /out:"..\..\..\..\..\Build\Win32\VC6\Debug/xerces-depdom_2_8D.dll" /implib:"..\..\..\..\..\Build\Win32\VC6\Debug/xerces-depdom_2D.lib" /pdbtype:sept /libpath:"..\..\..\..\..\Build\Win32\VC6\Debug"
# SUBTRACT LINK32 /pdb:none

!ELSEIF  "$(CFG)" == "XercesDeprecatedDOMLib - Win64 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\..\..\..\Build\Win64\VC6\Debug"
# PROP Intermediate_Dir "..\..\..\..\..\Build\Win64\VC6\Debug\obj"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /G6 /MDd /Za /W3 /Gm /GX /Zi /Od /I "..\..\..\..\..\src" /D "_CRTDBG_MAP_ALLOC" /D "PROJ_SAX2" /D "_DEBUG" /D "PROJ_XMLPARSER" /D "PROJ_XMLUTIL" /D "PROJ_PARSERS" /D "PROJ_SAX4C" /D "PROJ_DOM" /D "PROJ_VALIDATORS" /D "XML_SINGLEDLL" /D "WIN32" /D "_WINDOWS" /D "XML_USE_WIN32_TRANSCODER" /D "XML_USE_WIN32_MSGLOADER" /D "XML_USE_NETACCESSOR_WINSOCK" /FD /c
# SUBTRACT BASE CPP /Fr /YX
# ADD CPP /MDd /W3 /Gm /GX /Zi /Od /I "..\..\..\..\..\src" /D "WIN64" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "PLATFORM_WIN32" /D "_CRTDBG_MAP_ALLOC" /D "PROJ_DEPRECATED_DOM" /FD /c
# ADD BASE MTL /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
LINK32=link.exe
# ADD BASE LINK32 /machine:IX86
# SUBTRACT BASE LINK32 /pdb:none
# ADD LINK32 kernel32.lib user32.lib advapi32.lib ws2_32.lib xerces-c_2D.lib /base:"0x12000000" /subsystem:windows /dll /debug /machine:IX86 /out:"..\..\..\..\..\Build\Win64\VC6\Debug/xerces-depdom_2_8D.dll" /implib:"..\..\..\..\..\Build\Win64\VC6\Debug/xerces-depdom_2D.lib" /pdbtype:sept /libpath:"..\..\..\..\..\Build\Win64\VC6\Debug" /machine:IA64
# SUBTRACT LINK32 /pdb:none

!ELSEIF  "$(CFG)" == "XercesDeprecatedDOMLib - Win64 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\..\..\..\Build\Win64\VC6\Release"
# PROP Intermediate_Dir "..\..\..\..\..\Build\Win64\VC6\Release\obj"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /G6 /MD /Za /W3 /GX /O2 /I "..\..\..\..\..\src" /D "NDEBUG" /D "PLATFORM_WIN32" /D "_CRTDBG_MAP_ALLOC" /D "PROJ_XMLPARSER" /D "PROJ_XMLUTIL" /D "PROJ_PARSERS" /D "PROJ_SAX4C" /D "PROJ_SAX2" /D "PROJ_DOM" /D "PROJ_VALIDATORS" /D "XML_SINGLEDLL" /D "WIN32" /D "_WINDOWS" /D "XML_USE_WIN32_TRANSCODER" /D "XML_USE_WIN32_MSGLOADER" /D "XML_USE_NETACCESSOR_WINSOCK" /FD /c
# ADD CPP /MD /W3 /GX /O2 /I "..\..\..\..\..\src" /D "WIN64" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "PLATFORM_WIN32" /D "_CRTDBG_MAP_ALLOC" /D "PROJ_DEPRECATED_DOM" /FD /c
# ADD BASE MTL /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
LINK32=link.exe
# ADD BASE LINK32 /machine:IX86
# SUBTRACT BASE LINK32 /pdb:none /incremental:yes
# ADD LINK32 kernel32.lib user32.lib advapi32.lib ws2_32.lib xerces-c_2.lib /base:"0x12000000" /subsystem:windows /dll /incremental:yes /map /machine:IX86 /out:"..\..\..\..\..\Build\Win64\VC6\Release\xerces-depdom_2_8.dll" /implib:"..\..\..\..\..\Build\Win64\VC6\Release\xerces-depdom_2.lib" /libpath:"..\..\..\..\..\Build\Win64\VC6\Release" /machine:IA64
# SUBTRACT LINK32 /pdb:none

!ENDIF 

# Begin Target

# Name "XercesDeprecatedDOMLib - Win32 Release"
# Name "XercesDeprecatedDOMLib - Win32 Debug"
# Name "XercesDeprecatedDOMLib - Win64 Debug"
# Name "XercesDeprecatedDOMLib - Win64 Release"
# Begin Group "dom"

# PROP Default_Filter ""
# Begin Group "deprecated"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\AttrImpl.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\AttrImpl.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\AttrMapImpl.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\AttrMapImpl.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\AttrNSImpl.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\AttrNSImpl.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\CDATASectionImpl.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\CDATASectionImpl.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\CharacterDataImpl.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\CharacterDataImpl.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\ChildNode.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\ChildNode.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\CommentImpl.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\CommentImpl.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DeepNodeListImpl.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DeepNodeListImpl.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DocumentFragmentImpl.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DocumentFragmentImpl.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DocumentImpl.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DocumentImpl.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DocumentTypeImpl.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DocumentTypeImpl.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_Attr.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_Attr.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_CDATASection.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_CDATASection.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_CharacterData.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_CharacterData.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_Comment.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_Comment.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_Document.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_Document.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_DocumentFragment.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_DocumentFragment.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_DocumentType.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_DocumentType.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_DOMException.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_DOMException.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_DOMImplementation.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_DOMImplementation.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_Element.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_Element.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_Entity.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_Entity.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_EntityReference.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_EntityReference.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_NamedNodeMap.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_NamedNodeMap.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_Node.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_Node.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_NodeFilter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_NodeFilter.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_NodeIterator.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_NodeIterator.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_NodeList.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_NodeList.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_Notation.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_Notation.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_ProcessingInstruction.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_ProcessingInstruction.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_Range.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_Range.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_RangeException.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_RangeException.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_Text.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_Text.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_TreeWalker.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_TreeWalker.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_XMLDecl.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_XMLDecl.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DomMemDebug.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DomMemDebug.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOMParser.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOMParser.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOMString.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOMString.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOMStringImpl.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DStringPool.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DStringPool.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\ElementDefinitionImpl.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\ElementDefinitionImpl.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\ElementImpl.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\ElementImpl.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\ElementNSImpl.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\ElementNSImpl.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\EntityImpl.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\EntityImpl.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\EntityReferenceImpl.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\EntityReferenceImpl.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\Makefile.in
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\MemDebug.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\NamedNodeMapImpl.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\NamedNodeMapImpl.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\NameNodeFilter.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\NodeIDMap.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\NodeIDMap.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\NodeImpl.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\NodeImpl.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\NodeIteratorImpl.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\NodeIteratorImpl.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\NodeListImpl.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\NodeListImpl.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\NodeVector.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\NodeVector.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\NotationImpl.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\NotationImpl.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\ParentNode.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\ParentNode.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\ProcessingInstructionImpl.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\ProcessingInstructionImpl.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\RangeImpl.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\RangeImpl.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\RefCountedImpl.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\RefCountedImpl.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\TextImpl.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\TextImpl.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\TreeWalkerImpl.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\TreeWalkerImpl.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\XMLDeclImpl.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\XMLDeclImpl.hpp
# End Source File
# End Group
# End Group
# End Target
# End Project
