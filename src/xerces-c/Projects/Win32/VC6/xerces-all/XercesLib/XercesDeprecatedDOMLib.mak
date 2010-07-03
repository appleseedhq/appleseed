# Microsoft Developer Studio Generated NMAKE File, Based on XercesDeprecatedDOMLib.dsp
!IF "$(CFG)" == ""
CFG=XercesDeprecatedDOMLib - Win32 Debug
!MESSAGE No configuration specified. Defaulting to XercesDeprecatedDOMLib - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "XercesDeprecatedDOMLib - Win32 Release" && "$(CFG)" != "XercesDeprecatedDOMLib - Win32 Debug" && "$(CFG)" != "XercesDeprecatedDOMLib - Win64 Debug" && "$(CFG)" != "XercesDeprecatedDOMLib - Win64 Release"
!MESSAGE Invalid configuration "$(CFG)" specified.
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
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "XercesDeprecatedDOMLib - Win32 Release"

OUTDIR=.\..\..\..\..\..\Build\Win32\VC6\Release
INTDIR=.\..\..\..\..\..\Build\Win32\VC6\Release\obj
# Begin Custom Macros
OutDir=.\..\..\..\..\..\Build\Win32\VC6\Release
# End Custom Macros

ALL : "$(OUTDIR)\xerces-depdom_2_8.dll"


CLEAN :
	-@erase "$(INTDIR)\AttrImpl.obj"
	-@erase "$(INTDIR)\AttrMapImpl.obj"
	-@erase "$(INTDIR)\AttrNSImpl.obj"
	-@erase "$(INTDIR)\CDATASectionImpl.obj"
	-@erase "$(INTDIR)\CharacterDataImpl.obj"
	-@erase "$(INTDIR)\ChildNode.obj"
	-@erase "$(INTDIR)\CommentImpl.obj"
	-@erase "$(INTDIR)\DeepNodeListImpl.obj"
	-@erase "$(INTDIR)\DocumentFragmentImpl.obj"
	-@erase "$(INTDIR)\DocumentImpl.obj"
	-@erase "$(INTDIR)\DocumentTypeImpl.obj"
	-@erase "$(INTDIR)\DOM_Attr.obj"
	-@erase "$(INTDIR)\DOM_CDATASection.obj"
	-@erase "$(INTDIR)\DOM_CharacterData.obj"
	-@erase "$(INTDIR)\DOM_Comment.obj"
	-@erase "$(INTDIR)\DOM_Document.obj"
	-@erase "$(INTDIR)\DOM_DocumentFragment.obj"
	-@erase "$(INTDIR)\DOM_DocumentType.obj"
	-@erase "$(INTDIR)\DOM_DOMException.obj"
	-@erase "$(INTDIR)\DOM_DOMImplementation.obj"
	-@erase "$(INTDIR)\DOM_Element.obj"
	-@erase "$(INTDIR)\DOM_Entity.obj"
	-@erase "$(INTDIR)\DOM_EntityReference.obj"
	-@erase "$(INTDIR)\DOM_NamedNodeMap.obj"
	-@erase "$(INTDIR)\DOM_Node.obj"
	-@erase "$(INTDIR)\DOM_NodeFilter.obj"
	-@erase "$(INTDIR)\DOM_NodeIterator.obj"
	-@erase "$(INTDIR)\DOM_NodeList.obj"
	-@erase "$(INTDIR)\DOM_Notation.obj"
	-@erase "$(INTDIR)\DOM_ProcessingInstruction.obj"
	-@erase "$(INTDIR)\DOM_Range.obj"
	-@erase "$(INTDIR)\DOM_RangeException.obj"
	-@erase "$(INTDIR)\DOM_Text.obj"
	-@erase "$(INTDIR)\DOM_TreeWalker.obj"
	-@erase "$(INTDIR)\DOM_XMLDecl.obj"
	-@erase "$(INTDIR)\DomMemDebug.obj"
	-@erase "$(INTDIR)\DOMParser.obj"
	-@erase "$(INTDIR)\DOMString.obj"
	-@erase "$(INTDIR)\DStringPool.obj"
	-@erase "$(INTDIR)\ElementDefinitionImpl.obj"
	-@erase "$(INTDIR)\ElementImpl.obj"
	-@erase "$(INTDIR)\ElementNSImpl.obj"
	-@erase "$(INTDIR)\EntityImpl.obj"
	-@erase "$(INTDIR)\EntityReferenceImpl.obj"
	-@erase "$(INTDIR)\NamedNodeMapImpl.obj"
	-@erase "$(INTDIR)\NodeIDMap.obj"
	-@erase "$(INTDIR)\NodeImpl.obj"
	-@erase "$(INTDIR)\NodeIteratorImpl.obj"
	-@erase "$(INTDIR)\NodeListImpl.obj"
	-@erase "$(INTDIR)\NodeVector.obj"
	-@erase "$(INTDIR)\NotationImpl.obj"
	-@erase "$(INTDIR)\ParentNode.obj"
	-@erase "$(INTDIR)\ProcessingInstructionImpl.obj"
	-@erase "$(INTDIR)\RangeImpl.obj"
	-@erase "$(INTDIR)\RefCountedImpl.obj"
	-@erase "$(INTDIR)\TextImpl.obj"
	-@erase "$(INTDIR)\TreeWalkerImpl.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\XMLDeclImpl.obj"
	-@erase "$(OUTDIR)\obj\xerces-depdom_2_8.map"
	-@erase "$(OUTDIR)\xerces-depdom_2.exp"
	-@erase "$(OUTDIR)\xerces-depdom_2.lib"
	-@erase "$(OUTDIR)\xerces-depdom_2_8.dll"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=$(CPP)
CPP_PROJ=/G6 /MD /Za /W3 /GX /O2 /Ob2 /I "..\..\..\..\..\src" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "PLATFORM_WIN32" /D "_CRTDBG_MAP_ALLOC" /D "PROJ_DEPRECATED_DOM" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
MTL_PROJ=/D "NDEBUG" /mktyplib203 /o "NUL" /win32 
RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/o"$(OUTDIR)\XercesDeprecatedDOMLib.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib advapi32.lib ws2_32.lib xerces-c_2.lib /base:"0x12000000" /subsystem:windows /dll /incremental:no /pdb:"$(OUTDIR)\xerces-depdom_2_8.pdb" /map:"$(INTDIR)\xerces-depdom_2_8.map" /machine:I386 /out:"$(OUTDIR)\xerces-depdom_2_8.dll" /implib:"$(OUTDIR)\xerces-depdom_2.lib" /libpath:"..\..\..\..\..\Build\Win32\VC6\Release"
LINK32_OBJS= \
	"$(INTDIR)\AttrImpl.obj" \
	"$(INTDIR)\AttrMapImpl.obj" \
	"$(INTDIR)\AttrNSImpl.obj" \
	"$(INTDIR)\CDATASectionImpl.obj" \
	"$(INTDIR)\CharacterDataImpl.obj" \
	"$(INTDIR)\ChildNode.obj" \
	"$(INTDIR)\CommentImpl.obj" \
	"$(INTDIR)\DeepNodeListImpl.obj" \
	"$(INTDIR)\DocumentFragmentImpl.obj" \
	"$(INTDIR)\DocumentImpl.obj" \
	"$(INTDIR)\DocumentTypeImpl.obj" \
	"$(INTDIR)\DOM_Attr.obj" \
	"$(INTDIR)\DOM_CDATASection.obj" \
	"$(INTDIR)\DOM_CharacterData.obj" \
	"$(INTDIR)\DOM_Comment.obj" \
	"$(INTDIR)\DOM_Document.obj" \
	"$(INTDIR)\DOM_DocumentFragment.obj" \
	"$(INTDIR)\DOM_DocumentType.obj" \
	"$(INTDIR)\DOM_DOMException.obj" \
	"$(INTDIR)\DOM_DOMImplementation.obj" \
	"$(INTDIR)\DOM_Element.obj" \
	"$(INTDIR)\DOM_Entity.obj" \
	"$(INTDIR)\DOM_EntityReference.obj" \
	"$(INTDIR)\DOM_NamedNodeMap.obj" \
	"$(INTDIR)\DOM_Node.obj" \
	"$(INTDIR)\DOM_NodeFilter.obj" \
	"$(INTDIR)\DOM_NodeIterator.obj" \
	"$(INTDIR)\DOM_NodeList.obj" \
	"$(INTDIR)\DOM_Notation.obj" \
	"$(INTDIR)\DOM_ProcessingInstruction.obj" \
	"$(INTDIR)\DOM_Range.obj" \
	"$(INTDIR)\DOM_RangeException.obj" \
	"$(INTDIR)\DOM_Text.obj" \
	"$(INTDIR)\DOM_TreeWalker.obj" \
	"$(INTDIR)\DOM_XMLDecl.obj" \
	"$(INTDIR)\DomMemDebug.obj" \
	"$(INTDIR)\DOMParser.obj" \
	"$(INTDIR)\DOMString.obj" \
	"$(INTDIR)\DStringPool.obj" \
	"$(INTDIR)\ElementDefinitionImpl.obj" \
	"$(INTDIR)\ElementImpl.obj" \
	"$(INTDIR)\ElementNSImpl.obj" \
	"$(INTDIR)\EntityImpl.obj" \
	"$(INTDIR)\EntityReferenceImpl.obj" \
	"$(INTDIR)\NamedNodeMapImpl.obj" \
	"$(INTDIR)\NodeIDMap.obj" \
	"$(INTDIR)\NodeImpl.obj" \
	"$(INTDIR)\NodeIteratorImpl.obj" \
	"$(INTDIR)\NodeListImpl.obj" \
	"$(INTDIR)\NodeVector.obj" \
	"$(INTDIR)\NotationImpl.obj" \
	"$(INTDIR)\ParentNode.obj" \
	"$(INTDIR)\ProcessingInstructionImpl.obj" \
	"$(INTDIR)\RangeImpl.obj" \
	"$(INTDIR)\RefCountedImpl.obj" \
	"$(INTDIR)\TextImpl.obj" \
	"$(INTDIR)\TreeWalkerImpl.obj" \
	"$(INTDIR)\XMLDeclImpl.obj"

"$(OUTDIR)\xerces-depdom_2_8.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "XercesDeprecatedDOMLib - Win32 Debug"

OUTDIR=.\..\..\..\..\..\Build\Win32\VC6\Debug
INTDIR=.\..\..\..\..\..\Build\Win32\VC6\Debug\obj
# Begin Custom Macros
OutDir=.\..\..\..\..\..\Build\Win32\VC6\Debug
# End Custom Macros

ALL : "$(OUTDIR)\xerces-depdom_2_8D.dll"


CLEAN :
	-@erase "$(INTDIR)\AttrImpl.obj"
	-@erase "$(INTDIR)\AttrMapImpl.obj"
	-@erase "$(INTDIR)\AttrNSImpl.obj"
	-@erase "$(INTDIR)\CDATASectionImpl.obj"
	-@erase "$(INTDIR)\CharacterDataImpl.obj"
	-@erase "$(INTDIR)\ChildNode.obj"
	-@erase "$(INTDIR)\CommentImpl.obj"
	-@erase "$(INTDIR)\DeepNodeListImpl.obj"
	-@erase "$(INTDIR)\DocumentFragmentImpl.obj"
	-@erase "$(INTDIR)\DocumentImpl.obj"
	-@erase "$(INTDIR)\DocumentTypeImpl.obj"
	-@erase "$(INTDIR)\DOM_Attr.obj"
	-@erase "$(INTDIR)\DOM_CDATASection.obj"
	-@erase "$(INTDIR)\DOM_CharacterData.obj"
	-@erase "$(INTDIR)\DOM_Comment.obj"
	-@erase "$(INTDIR)\DOM_Document.obj"
	-@erase "$(INTDIR)\DOM_DocumentFragment.obj"
	-@erase "$(INTDIR)\DOM_DocumentType.obj"
	-@erase "$(INTDIR)\DOM_DOMException.obj"
	-@erase "$(INTDIR)\DOM_DOMImplementation.obj"
	-@erase "$(INTDIR)\DOM_Element.obj"
	-@erase "$(INTDIR)\DOM_Entity.obj"
	-@erase "$(INTDIR)\DOM_EntityReference.obj"
	-@erase "$(INTDIR)\DOM_NamedNodeMap.obj"
	-@erase "$(INTDIR)\DOM_Node.obj"
	-@erase "$(INTDIR)\DOM_NodeFilter.obj"
	-@erase "$(INTDIR)\DOM_NodeIterator.obj"
	-@erase "$(INTDIR)\DOM_NodeList.obj"
	-@erase "$(INTDIR)\DOM_Notation.obj"
	-@erase "$(INTDIR)\DOM_ProcessingInstruction.obj"
	-@erase "$(INTDIR)\DOM_Range.obj"
	-@erase "$(INTDIR)\DOM_RangeException.obj"
	-@erase "$(INTDIR)\DOM_Text.obj"
	-@erase "$(INTDIR)\DOM_TreeWalker.obj"
	-@erase "$(INTDIR)\DOM_XMLDecl.obj"
	-@erase "$(INTDIR)\DomMemDebug.obj"
	-@erase "$(INTDIR)\DOMParser.obj"
	-@erase "$(INTDIR)\DOMString.obj"
	-@erase "$(INTDIR)\DStringPool.obj"
	-@erase "$(INTDIR)\ElementDefinitionImpl.obj"
	-@erase "$(INTDIR)\ElementImpl.obj"
	-@erase "$(INTDIR)\ElementNSImpl.obj"
	-@erase "$(INTDIR)\EntityImpl.obj"
	-@erase "$(INTDIR)\EntityReferenceImpl.obj"
	-@erase "$(INTDIR)\NamedNodeMapImpl.obj"
	-@erase "$(INTDIR)\NodeIDMap.obj"
	-@erase "$(INTDIR)\NodeImpl.obj"
	-@erase "$(INTDIR)\NodeIteratorImpl.obj"
	-@erase "$(INTDIR)\NodeListImpl.obj"
	-@erase "$(INTDIR)\NodeVector.obj"
	-@erase "$(INTDIR)\NotationImpl.obj"
	-@erase "$(INTDIR)\ParentNode.obj"
	-@erase "$(INTDIR)\ProcessingInstructionImpl.obj"
	-@erase "$(INTDIR)\RangeImpl.obj"
	-@erase "$(INTDIR)\RefCountedImpl.obj"
	-@erase "$(INTDIR)\TextImpl.obj"
	-@erase "$(INTDIR)\TreeWalkerImpl.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\XMLDeclImpl.obj"
	-@erase "$(OUTDIR)\xerces-depdom_2_8D.dll"
	-@erase "$(OUTDIR)\xerces-depdom_2_8D.ilk"
	-@erase "$(OUTDIR)\xerces-depdom_2_8D.pdb"
	-@erase "$(OUTDIR)\xerces-depdom_2D.exp"
	-@erase "$(OUTDIR)\xerces-depdom_2D.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=$(CPP)
CPP_PROJ=/G6 /MDd /Za /W3 /Gm /GX /Zi /Od /I "..\..\..\..\..\src" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "PLATFORM_WIN32" /D "_CRTDBG_MAP_ALLOC" /D "PROJ_DEPRECATED_DOM" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
MTL_PROJ=/D "_DEBUG" /mktyplib203 /o "NUL" /win32 
RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/o"$(OUTDIR)\XercesDeprecatedDOMLib.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib advapi32.lib ws2_32.lib xerces-c_2D.lib /base:"0x12000000" /subsystem:windows /dll /incremental:yes /pdb:"$(OUTDIR)\xerces-depdom_2_8D.pdb" /debug /machine:I386 /out:"$(OUTDIR)\xerces-depdom_2_8D.dll" /implib:"$(OUTDIR)\xerces-depdom_2D.lib" /pdbtype:sept /libpath:"..\..\..\..\..\Build\Win32\VC6\Debug"
LINK32_OBJS= \
	"$(INTDIR)\AttrImpl.obj" \
	"$(INTDIR)\AttrMapImpl.obj" \
	"$(INTDIR)\AttrNSImpl.obj" \
	"$(INTDIR)\CDATASectionImpl.obj" \
	"$(INTDIR)\CharacterDataImpl.obj" \
	"$(INTDIR)\ChildNode.obj" \
	"$(INTDIR)\CommentImpl.obj" \
	"$(INTDIR)\DeepNodeListImpl.obj" \
	"$(INTDIR)\DocumentFragmentImpl.obj" \
	"$(INTDIR)\DocumentImpl.obj" \
	"$(INTDIR)\DocumentTypeImpl.obj" \
	"$(INTDIR)\DOM_Attr.obj" \
	"$(INTDIR)\DOM_CDATASection.obj" \
	"$(INTDIR)\DOM_CharacterData.obj" \
	"$(INTDIR)\DOM_Comment.obj" \
	"$(INTDIR)\DOM_Document.obj" \
	"$(INTDIR)\DOM_DocumentFragment.obj" \
	"$(INTDIR)\DOM_DocumentType.obj" \
	"$(INTDIR)\DOM_DOMException.obj" \
	"$(INTDIR)\DOM_DOMImplementation.obj" \
	"$(INTDIR)\DOM_Element.obj" \
	"$(INTDIR)\DOM_Entity.obj" \
	"$(INTDIR)\DOM_EntityReference.obj" \
	"$(INTDIR)\DOM_NamedNodeMap.obj" \
	"$(INTDIR)\DOM_Node.obj" \
	"$(INTDIR)\DOM_NodeFilter.obj" \
	"$(INTDIR)\DOM_NodeIterator.obj" \
	"$(INTDIR)\DOM_NodeList.obj" \
	"$(INTDIR)\DOM_Notation.obj" \
	"$(INTDIR)\DOM_ProcessingInstruction.obj" \
	"$(INTDIR)\DOM_Range.obj" \
	"$(INTDIR)\DOM_RangeException.obj" \
	"$(INTDIR)\DOM_Text.obj" \
	"$(INTDIR)\DOM_TreeWalker.obj" \
	"$(INTDIR)\DOM_XMLDecl.obj" \
	"$(INTDIR)\DomMemDebug.obj" \
	"$(INTDIR)\DOMParser.obj" \
	"$(INTDIR)\DOMString.obj" \
	"$(INTDIR)\DStringPool.obj" \
	"$(INTDIR)\ElementDefinitionImpl.obj" \
	"$(INTDIR)\ElementImpl.obj" \
	"$(INTDIR)\ElementNSImpl.obj" \
	"$(INTDIR)\EntityImpl.obj" \
	"$(INTDIR)\EntityReferenceImpl.obj" \
	"$(INTDIR)\NamedNodeMapImpl.obj" \
	"$(INTDIR)\NodeIDMap.obj" \
	"$(INTDIR)\NodeImpl.obj" \
	"$(INTDIR)\NodeIteratorImpl.obj" \
	"$(INTDIR)\NodeListImpl.obj" \
	"$(INTDIR)\NodeVector.obj" \
	"$(INTDIR)\NotationImpl.obj" \
	"$(INTDIR)\ParentNode.obj" \
	"$(INTDIR)\ProcessingInstructionImpl.obj" \
	"$(INTDIR)\RangeImpl.obj" \
	"$(INTDIR)\RefCountedImpl.obj" \
	"$(INTDIR)\TextImpl.obj" \
	"$(INTDIR)\TreeWalkerImpl.obj" \
	"$(INTDIR)\XMLDeclImpl.obj"

"$(OUTDIR)\xerces-depdom_2_8D.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "XercesDeprecatedDOMLib - Win64 Debug"

OUTDIR=.\..\..\..\..\..\Build\Win64\VC6\Debug
INTDIR=.\..\..\..\..\..\Build\Win64\VC6\Debug\obj
# Begin Custom Macros
OutDir=.\..\..\..\..\..\Build\Win64\VC6\Debug
# End Custom Macros

ALL : "$(OUTDIR)\xerces-depdom_2_8D.dll"


CLEAN :
	-@erase "$(INTDIR)\AttrImpl.obj"
	-@erase "$(INTDIR)\AttrMapImpl.obj"
	-@erase "$(INTDIR)\AttrNSImpl.obj"
	-@erase "$(INTDIR)\CDATASectionImpl.obj"
	-@erase "$(INTDIR)\CharacterDataImpl.obj"
	-@erase "$(INTDIR)\ChildNode.obj"
	-@erase "$(INTDIR)\CommentImpl.obj"
	-@erase "$(INTDIR)\DeepNodeListImpl.obj"
	-@erase "$(INTDIR)\DocumentFragmentImpl.obj"
	-@erase "$(INTDIR)\DocumentImpl.obj"
	-@erase "$(INTDIR)\DocumentTypeImpl.obj"
	-@erase "$(INTDIR)\DOM_Attr.obj"
	-@erase "$(INTDIR)\DOM_CDATASection.obj"
	-@erase "$(INTDIR)\DOM_CharacterData.obj"
	-@erase "$(INTDIR)\DOM_Comment.obj"
	-@erase "$(INTDIR)\DOM_Document.obj"
	-@erase "$(INTDIR)\DOM_DocumentFragment.obj"
	-@erase "$(INTDIR)\DOM_DocumentType.obj"
	-@erase "$(INTDIR)\DOM_DOMException.obj"
	-@erase "$(INTDIR)\DOM_DOMImplementation.obj"
	-@erase "$(INTDIR)\DOM_Element.obj"
	-@erase "$(INTDIR)\DOM_Entity.obj"
	-@erase "$(INTDIR)\DOM_EntityReference.obj"
	-@erase "$(INTDIR)\DOM_NamedNodeMap.obj"
	-@erase "$(INTDIR)\DOM_Node.obj"
	-@erase "$(INTDIR)\DOM_NodeFilter.obj"
	-@erase "$(INTDIR)\DOM_NodeIterator.obj"
	-@erase "$(INTDIR)\DOM_NodeList.obj"
	-@erase "$(INTDIR)\DOM_Notation.obj"
	-@erase "$(INTDIR)\DOM_ProcessingInstruction.obj"
	-@erase "$(INTDIR)\DOM_Range.obj"
	-@erase "$(INTDIR)\DOM_RangeException.obj"
	-@erase "$(INTDIR)\DOM_Text.obj"
	-@erase "$(INTDIR)\DOM_TreeWalker.obj"
	-@erase "$(INTDIR)\DOM_XMLDecl.obj"
	-@erase "$(INTDIR)\DomMemDebug.obj"
	-@erase "$(INTDIR)\DOMParser.obj"
	-@erase "$(INTDIR)\DOMString.obj"
	-@erase "$(INTDIR)\DStringPool.obj"
	-@erase "$(INTDIR)\ElementDefinitionImpl.obj"
	-@erase "$(INTDIR)\ElementImpl.obj"
	-@erase "$(INTDIR)\ElementNSImpl.obj"
	-@erase "$(INTDIR)\EntityImpl.obj"
	-@erase "$(INTDIR)\EntityReferenceImpl.obj"
	-@erase "$(INTDIR)\NamedNodeMapImpl.obj"
	-@erase "$(INTDIR)\NodeIDMap.obj"
	-@erase "$(INTDIR)\NodeImpl.obj"
	-@erase "$(INTDIR)\NodeIteratorImpl.obj"
	-@erase "$(INTDIR)\NodeListImpl.obj"
	-@erase "$(INTDIR)\NodeVector.obj"
	-@erase "$(INTDIR)\NotationImpl.obj"
	-@erase "$(INTDIR)\ParentNode.obj"
	-@erase "$(INTDIR)\ProcessingInstructionImpl.obj"
	-@erase "$(INTDIR)\RangeImpl.obj"
	-@erase "$(INTDIR)\RefCountedImpl.obj"
	-@erase "$(INTDIR)\TextImpl.obj"
	-@erase "$(INTDIR)\TreeWalkerImpl.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\XMLDeclImpl.obj"
	-@erase "$(OUTDIR)\xerces-depdom_2_8D.dll"
	-@erase "$(OUTDIR)\xerces-depdom_2_8D.ilk"
	-@erase "$(OUTDIR)\xerces-depdom_2_8D.pdb"
	-@erase "$(OUTDIR)\xerces-depdom_2D.exp"
	-@erase "$(OUTDIR)\xerces-depdom_2D.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=$(CPP)
CPP_PROJ=/MDd /W3 /Gm /GX /Zi /Od /I "..\..\..\..\..\src" /D "WIN64" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "PLATFORM_WIN32" /D "_CRTDBG_MAP_ALLOC" /D "PROJ_DEPRECATED_DOM" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
MTL_PROJ=/D "_DEBUG" /mktyplib203 /o "NUL" /win32 
RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/o"$(OUTDIR)\XercesDeprecatedDOMLib.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib advapi32.lib ws2_32.lib xerces-c_2D.lib /base:"0x12000000" /subsystem:windows /dll /incremental:yes /pdb:"$(OUTDIR)\xerces-depdom_2_8D.pdb" /debug /machine:IX86 /out:"$(OUTDIR)\xerces-depdom_2_8D.dll" /implib:"$(OUTDIR)\xerces-depdom_2D.lib" /pdbtype:sept /libpath:"..\..\..\..\..\Build\Win64\VC6\Debug" /machine:IA64
LINK32_OBJS= \
	"$(INTDIR)\AttrImpl.obj" \
	"$(INTDIR)\AttrMapImpl.obj" \
	"$(INTDIR)\AttrNSImpl.obj" \
	"$(INTDIR)\CDATASectionImpl.obj" \
	"$(INTDIR)\CharacterDataImpl.obj" \
	"$(INTDIR)\ChildNode.obj" \
	"$(INTDIR)\CommentImpl.obj" \
	"$(INTDIR)\DeepNodeListImpl.obj" \
	"$(INTDIR)\DocumentFragmentImpl.obj" \
	"$(INTDIR)\DocumentImpl.obj" \
	"$(INTDIR)\DocumentTypeImpl.obj" \
	"$(INTDIR)\DOM_Attr.obj" \
	"$(INTDIR)\DOM_CDATASection.obj" \
	"$(INTDIR)\DOM_CharacterData.obj" \
	"$(INTDIR)\DOM_Comment.obj" \
	"$(INTDIR)\DOM_Document.obj" \
	"$(INTDIR)\DOM_DocumentFragment.obj" \
	"$(INTDIR)\DOM_DocumentType.obj" \
	"$(INTDIR)\DOM_DOMException.obj" \
	"$(INTDIR)\DOM_DOMImplementation.obj" \
	"$(INTDIR)\DOM_Element.obj" \
	"$(INTDIR)\DOM_Entity.obj" \
	"$(INTDIR)\DOM_EntityReference.obj" \
	"$(INTDIR)\DOM_NamedNodeMap.obj" \
	"$(INTDIR)\DOM_Node.obj" \
	"$(INTDIR)\DOM_NodeFilter.obj" \
	"$(INTDIR)\DOM_NodeIterator.obj" \
	"$(INTDIR)\DOM_NodeList.obj" \
	"$(INTDIR)\DOM_Notation.obj" \
	"$(INTDIR)\DOM_ProcessingInstruction.obj" \
	"$(INTDIR)\DOM_Range.obj" \
	"$(INTDIR)\DOM_RangeException.obj" \
	"$(INTDIR)\DOM_Text.obj" \
	"$(INTDIR)\DOM_TreeWalker.obj" \
	"$(INTDIR)\DOM_XMLDecl.obj" \
	"$(INTDIR)\DomMemDebug.obj" \
	"$(INTDIR)\DOMParser.obj" \
	"$(INTDIR)\DOMString.obj" \
	"$(INTDIR)\DStringPool.obj" \
	"$(INTDIR)\ElementDefinitionImpl.obj" \
	"$(INTDIR)\ElementImpl.obj" \
	"$(INTDIR)\ElementNSImpl.obj" \
	"$(INTDIR)\EntityImpl.obj" \
	"$(INTDIR)\EntityReferenceImpl.obj" \
	"$(INTDIR)\NamedNodeMapImpl.obj" \
	"$(INTDIR)\NodeIDMap.obj" \
	"$(INTDIR)\NodeImpl.obj" \
	"$(INTDIR)\NodeIteratorImpl.obj" \
	"$(INTDIR)\NodeListImpl.obj" \
	"$(INTDIR)\NodeVector.obj" \
	"$(INTDIR)\NotationImpl.obj" \
	"$(INTDIR)\ParentNode.obj" \
	"$(INTDIR)\ProcessingInstructionImpl.obj" \
	"$(INTDIR)\RangeImpl.obj" \
	"$(INTDIR)\RefCountedImpl.obj" \
	"$(INTDIR)\TextImpl.obj" \
	"$(INTDIR)\TreeWalkerImpl.obj" \
	"$(INTDIR)\XMLDeclImpl.obj"

"$(OUTDIR)\xerces-depdom_2_8D.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "XercesDeprecatedDOMLib - Win64 Release"

OUTDIR=.\..\..\..\..\..\Build\Win64\VC6\Release
INTDIR=.\..\..\..\..\..\Build\Win64\VC6\Release\obj
# Begin Custom Macros
OutDir=.\..\..\..\..\..\Build\Win64\VC6\Release
# End Custom Macros

ALL : "$(OUTDIR)\xerces-depdom_2_8.dll"


CLEAN :
	-@erase "$(INTDIR)\AttrImpl.obj"
	-@erase "$(INTDIR)\AttrMapImpl.obj"
	-@erase "$(INTDIR)\AttrNSImpl.obj"
	-@erase "$(INTDIR)\CDATASectionImpl.obj"
	-@erase "$(INTDIR)\CharacterDataImpl.obj"
	-@erase "$(INTDIR)\ChildNode.obj"
	-@erase "$(INTDIR)\CommentImpl.obj"
	-@erase "$(INTDIR)\DeepNodeListImpl.obj"
	-@erase "$(INTDIR)\DocumentFragmentImpl.obj"
	-@erase "$(INTDIR)\DocumentImpl.obj"
	-@erase "$(INTDIR)\DocumentTypeImpl.obj"
	-@erase "$(INTDIR)\DOM_Attr.obj"
	-@erase "$(INTDIR)\DOM_CDATASection.obj"
	-@erase "$(INTDIR)\DOM_CharacterData.obj"
	-@erase "$(INTDIR)\DOM_Comment.obj"
	-@erase "$(INTDIR)\DOM_Document.obj"
	-@erase "$(INTDIR)\DOM_DocumentFragment.obj"
	-@erase "$(INTDIR)\DOM_DocumentType.obj"
	-@erase "$(INTDIR)\DOM_DOMException.obj"
	-@erase "$(INTDIR)\DOM_DOMImplementation.obj"
	-@erase "$(INTDIR)\DOM_Element.obj"
	-@erase "$(INTDIR)\DOM_Entity.obj"
	-@erase "$(INTDIR)\DOM_EntityReference.obj"
	-@erase "$(INTDIR)\DOM_NamedNodeMap.obj"
	-@erase "$(INTDIR)\DOM_Node.obj"
	-@erase "$(INTDIR)\DOM_NodeFilter.obj"
	-@erase "$(INTDIR)\DOM_NodeIterator.obj"
	-@erase "$(INTDIR)\DOM_NodeList.obj"
	-@erase "$(INTDIR)\DOM_Notation.obj"
	-@erase "$(INTDIR)\DOM_ProcessingInstruction.obj"
	-@erase "$(INTDIR)\DOM_Range.obj"
	-@erase "$(INTDIR)\DOM_RangeException.obj"
	-@erase "$(INTDIR)\DOM_Text.obj"
	-@erase "$(INTDIR)\DOM_TreeWalker.obj"
	-@erase "$(INTDIR)\DOM_XMLDecl.obj"
	-@erase "$(INTDIR)\DomMemDebug.obj"
	-@erase "$(INTDIR)\DOMParser.obj"
	-@erase "$(INTDIR)\DOMString.obj"
	-@erase "$(INTDIR)\DStringPool.obj"
	-@erase "$(INTDIR)\ElementDefinitionImpl.obj"
	-@erase "$(INTDIR)\ElementImpl.obj"
	-@erase "$(INTDIR)\ElementNSImpl.obj"
	-@erase "$(INTDIR)\EntityImpl.obj"
	-@erase "$(INTDIR)\EntityReferenceImpl.obj"
	-@erase "$(INTDIR)\NamedNodeMapImpl.obj"
	-@erase "$(INTDIR)\NodeIDMap.obj"
	-@erase "$(INTDIR)\NodeImpl.obj"
	-@erase "$(INTDIR)\NodeIteratorImpl.obj"
	-@erase "$(INTDIR)\NodeListImpl.obj"
	-@erase "$(INTDIR)\NodeVector.obj"
	-@erase "$(INTDIR)\NotationImpl.obj"
	-@erase "$(INTDIR)\ParentNode.obj"
	-@erase "$(INTDIR)\ProcessingInstructionImpl.obj"
	-@erase "$(INTDIR)\RangeImpl.obj"
	-@erase "$(INTDIR)\RefCountedImpl.obj"
	-@erase "$(INTDIR)\TextImpl.obj"
	-@erase "$(INTDIR)\TreeWalkerImpl.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\XMLDeclImpl.obj"
	-@erase "$(OUTDIR)\obj\xerces-depdom_2_8.map"
	-@erase "$(OUTDIR)\xerces-depdom_2.exp"
	-@erase "$(OUTDIR)\xerces-depdom_2.lib"
	-@erase "$(OUTDIR)\xerces-depdom_2_8.dll"
	-@erase "$(OUTDIR)\xerces-depdom_2_8.ilk"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=$(CPP)
CPP_PROJ=/MD /W3 /GX /O2 /I "..\..\..\..\..\src" /D "WIN64" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "PLATFORM_WIN32" /D "_CRTDBG_MAP_ALLOC" /D "PROJ_DEPRECATED_DOM" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
MTL_PROJ=/D "NDEBUG" /mktyplib203 /o "NUL" /win32 
RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/o"$(OUTDIR)\XercesDeprecatedDOMLib.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib advapi32.lib ws2_32.lib xerces-c_2.lib /base:"0x12000000" /subsystem:windows /dll /incremental:yes /pdb:"$(OUTDIR)\xerces-depdom_2_8.pdb" /map:"$(INTDIR)\xerces-depdom_2_8.map" /machine:IX86 /out:"$(OUTDIR)\xerces-depdom_2_8.dll" /implib:"$(OUTDIR)\xerces-depdom_2.lib" /libpath:"..\..\..\..\..\Build\Win64\VC6\Release" /machine:IA64
LINK32_OBJS= \
	"$(INTDIR)\AttrImpl.obj" \
	"$(INTDIR)\AttrMapImpl.obj" \
	"$(INTDIR)\AttrNSImpl.obj" \
	"$(INTDIR)\CDATASectionImpl.obj" \
	"$(INTDIR)\CharacterDataImpl.obj" \
	"$(INTDIR)\ChildNode.obj" \
	"$(INTDIR)\CommentImpl.obj" \
	"$(INTDIR)\DeepNodeListImpl.obj" \
	"$(INTDIR)\DocumentFragmentImpl.obj" \
	"$(INTDIR)\DocumentImpl.obj" \
	"$(INTDIR)\DocumentTypeImpl.obj" \
	"$(INTDIR)\DOM_Attr.obj" \
	"$(INTDIR)\DOM_CDATASection.obj" \
	"$(INTDIR)\DOM_CharacterData.obj" \
	"$(INTDIR)\DOM_Comment.obj" \
	"$(INTDIR)\DOM_Document.obj" \
	"$(INTDIR)\DOM_DocumentFragment.obj" \
	"$(INTDIR)\DOM_DocumentType.obj" \
	"$(INTDIR)\DOM_DOMException.obj" \
	"$(INTDIR)\DOM_DOMImplementation.obj" \
	"$(INTDIR)\DOM_Element.obj" \
	"$(INTDIR)\DOM_Entity.obj" \
	"$(INTDIR)\DOM_EntityReference.obj" \
	"$(INTDIR)\DOM_NamedNodeMap.obj" \
	"$(INTDIR)\DOM_Node.obj" \
	"$(INTDIR)\DOM_NodeFilter.obj" \
	"$(INTDIR)\DOM_NodeIterator.obj" \
	"$(INTDIR)\DOM_NodeList.obj" \
	"$(INTDIR)\DOM_Notation.obj" \
	"$(INTDIR)\DOM_ProcessingInstruction.obj" \
	"$(INTDIR)\DOM_Range.obj" \
	"$(INTDIR)\DOM_RangeException.obj" \
	"$(INTDIR)\DOM_Text.obj" \
	"$(INTDIR)\DOM_TreeWalker.obj" \
	"$(INTDIR)\DOM_XMLDecl.obj" \
	"$(INTDIR)\DomMemDebug.obj" \
	"$(INTDIR)\DOMParser.obj" \
	"$(INTDIR)\DOMString.obj" \
	"$(INTDIR)\DStringPool.obj" \
	"$(INTDIR)\ElementDefinitionImpl.obj" \
	"$(INTDIR)\ElementImpl.obj" \
	"$(INTDIR)\ElementNSImpl.obj" \
	"$(INTDIR)\EntityImpl.obj" \
	"$(INTDIR)\EntityReferenceImpl.obj" \
	"$(INTDIR)\NamedNodeMapImpl.obj" \
	"$(INTDIR)\NodeIDMap.obj" \
	"$(INTDIR)\NodeImpl.obj" \
	"$(INTDIR)\NodeIteratorImpl.obj" \
	"$(INTDIR)\NodeListImpl.obj" \
	"$(INTDIR)\NodeVector.obj" \
	"$(INTDIR)\NotationImpl.obj" \
	"$(INTDIR)\ParentNode.obj" \
	"$(INTDIR)\ProcessingInstructionImpl.obj" \
	"$(INTDIR)\RangeImpl.obj" \
	"$(INTDIR)\RefCountedImpl.obj" \
	"$(INTDIR)\TextImpl.obj" \
	"$(INTDIR)\TreeWalkerImpl.obj" \
	"$(INTDIR)\XMLDeclImpl.obj"

"$(OUTDIR)\xerces-depdom_2_8.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("XercesDeprecatedDOMLib.dep")
!INCLUDE "XercesDeprecatedDOMLib.dep"
!ELSE 
!MESSAGE Warning: cannot find "XercesDeprecatedDOMLib.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "XercesDeprecatedDOMLib - Win32 Release" || "$(CFG)" == "XercesDeprecatedDOMLib - Win32 Debug" || "$(CFG)" == "XercesDeprecatedDOMLib - Win64 Debug" || "$(CFG)" == "XercesDeprecatedDOMLib - Win64 Release"
SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\AttrImpl.cpp

"$(INTDIR)\AttrImpl.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\AttrMapImpl.cpp

"$(INTDIR)\AttrMapImpl.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\AttrNSImpl.cpp

"$(INTDIR)\AttrNSImpl.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\CDATASectionImpl.cpp

"$(INTDIR)\CDATASectionImpl.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\CharacterDataImpl.cpp

"$(INTDIR)\CharacterDataImpl.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\ChildNode.cpp

"$(INTDIR)\ChildNode.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\CommentImpl.cpp

"$(INTDIR)\CommentImpl.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DeepNodeListImpl.cpp

"$(INTDIR)\DeepNodeListImpl.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DocumentFragmentImpl.cpp

"$(INTDIR)\DocumentFragmentImpl.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DocumentImpl.cpp

"$(INTDIR)\DocumentImpl.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DocumentTypeImpl.cpp

"$(INTDIR)\DocumentTypeImpl.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_Attr.cpp

"$(INTDIR)\DOM_Attr.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_CDATASection.cpp

"$(INTDIR)\DOM_CDATASection.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_CharacterData.cpp

"$(INTDIR)\DOM_CharacterData.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_Comment.cpp

"$(INTDIR)\DOM_Comment.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_Document.cpp

"$(INTDIR)\DOM_Document.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_DocumentFragment.cpp

"$(INTDIR)\DOM_DocumentFragment.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_DocumentType.cpp

"$(INTDIR)\DOM_DocumentType.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_DOMException.cpp

"$(INTDIR)\DOM_DOMException.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_DOMImplementation.cpp

"$(INTDIR)\DOM_DOMImplementation.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_Element.cpp

"$(INTDIR)\DOM_Element.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_Entity.cpp

"$(INTDIR)\DOM_Entity.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_EntityReference.cpp

"$(INTDIR)\DOM_EntityReference.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_NamedNodeMap.cpp

"$(INTDIR)\DOM_NamedNodeMap.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_Node.cpp

"$(INTDIR)\DOM_Node.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_NodeFilter.cpp

"$(INTDIR)\DOM_NodeFilter.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_NodeIterator.cpp

"$(INTDIR)\DOM_NodeIterator.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_NodeList.cpp

"$(INTDIR)\DOM_NodeList.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_Notation.cpp

"$(INTDIR)\DOM_Notation.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_ProcessingInstruction.cpp

"$(INTDIR)\DOM_ProcessingInstruction.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_Range.cpp

"$(INTDIR)\DOM_Range.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_RangeException.cpp

"$(INTDIR)\DOM_RangeException.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_Text.cpp

"$(INTDIR)\DOM_Text.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_TreeWalker.cpp

"$(INTDIR)\DOM_TreeWalker.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOM_XMLDecl.cpp

"$(INTDIR)\DOM_XMLDecl.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DomMemDebug.cpp

"$(INTDIR)\DomMemDebug.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOMParser.cpp

"$(INTDIR)\DOMParser.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DOMString.cpp

"$(INTDIR)\DOMString.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\DStringPool.cpp

"$(INTDIR)\DStringPool.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\ElementDefinitionImpl.cpp

"$(INTDIR)\ElementDefinitionImpl.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\ElementImpl.cpp

"$(INTDIR)\ElementImpl.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\ElementNSImpl.cpp

"$(INTDIR)\ElementNSImpl.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\EntityImpl.cpp

"$(INTDIR)\EntityImpl.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\EntityReferenceImpl.cpp

"$(INTDIR)\EntityReferenceImpl.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\NamedNodeMapImpl.cpp

"$(INTDIR)\NamedNodeMapImpl.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\NodeIDMap.cpp

"$(INTDIR)\NodeIDMap.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\NodeImpl.cpp

"$(INTDIR)\NodeImpl.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\NodeIteratorImpl.cpp

"$(INTDIR)\NodeIteratorImpl.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\NodeListImpl.cpp

"$(INTDIR)\NodeListImpl.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\NodeVector.cpp

"$(INTDIR)\NodeVector.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\NotationImpl.cpp

"$(INTDIR)\NotationImpl.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\ParentNode.cpp

"$(INTDIR)\ParentNode.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\ProcessingInstructionImpl.cpp

"$(INTDIR)\ProcessingInstructionImpl.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\RangeImpl.cpp

"$(INTDIR)\RangeImpl.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\RefCountedImpl.cpp

"$(INTDIR)\RefCountedImpl.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\TextImpl.cpp

"$(INTDIR)\TextImpl.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\TreeWalkerImpl.cpp

"$(INTDIR)\TreeWalkerImpl.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\src\xercesc\dom\deprecated\XMLDeclImpl.cpp

"$(INTDIR)\XMLDeclImpl.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)



!ENDIF 

