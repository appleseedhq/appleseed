# Microsoft Developer Studio Generated NMAKE File, Based on all.dsp
!IF "$(CFG)" == ""
CFG=all - Win32 Debug
!MESSAGE No configuration specified. Defaulting to all - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "all - Win32 Release" && "$(CFG)" != "all - Win32 Debug" && "$(CFG)" != "all - Win64 Debug" && "$(CFG)" != "all - Win64 Release"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "all.mak" CFG="all - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "all - Win32 Release" (based on "Win32 (x86) Generic Project")
!MESSAGE "all - Win32 Debug" (based on "Win32 (x86) Generic Project")
!MESSAGE "all - Win64 Debug" (based on "Win32 (x86) Generic Project")
!MESSAGE "all - Win64 Release" (based on "Win32 (x86) Generic Project")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

OUTDIR=.\Release
INTDIR=.\Release

!IF "$(RECURSE)" == "0" 

ALL : 

!ELSE 

ALL : "XSValueTest - Win64 Release" "XercesDeprecatedDOMLib - Win64 Release" "SCMPrint - Win64 Release" "PSVIWriter - Win64 Release" "DOMNormalizerTest - Win64 Release" "DOMTypeInfoTest - Win64 Release" "DeprecatedDOMCount - Win64 Release" "DOMTraversalTest - Win64 Release" "XercesLib - Win64 Release" "MemHandlerTest - Win64 Release" "XSerializerTest - Win64 Release" "ThreadTest - Win64 Release" "StdInParse - Win64 Release" "SEnumVal - Win64 Release" "SAXPrint - Win64 Release" "SAXCount - Win64 Release" "SAX2Print - Win64 Release" "SAX2Count - Win64 Release" "Redirect - Win64 Release" "RangeTest - Win64 Release" "PParse - Win64 Release" "MemParse - Win64 Release" "InitTermTest - Win64 Release" "EnumVal - Win64 Release" "EncodingTest - Win64 Release" "DOMTest - Win64 Release" "DOMPrint - Win64 Release" "DOMMemTest - Win64 Release" "DOMCount - Win64 Release" "CreateDOMDocument - Win64 Release" 

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"CreateDOMDocument - Win64 ReleaseCLEAN" "DOMCount - Win64 ReleaseCLEAN" "DOMMemTest - Win64 ReleaseCLEAN" "DOMPrint - Win64 ReleaseCLEAN" "DOMTest - Win64 ReleaseCLEAN" "EncodingTest - Win64 ReleaseCLEAN" "EnumVal - Win64 ReleaseCLEAN" "InitTermTest - Win64 ReleaseCLEAN" "MemParse - Win64 ReleaseCLEAN" "PParse - Win64 ReleaseCLEAN" "RangeTest - Win64 ReleaseCLEAN" "Redirect - Win64 ReleaseCLEAN" "SAX2Count - Win64 ReleaseCLEAN" "SAX2Print - Win64 ReleaseCLEAN" "SAXCount - Win64 ReleaseCLEAN" "SAXPrint - Win64 ReleaseCLEAN" "SEnumVal - Win64 ReleaseCLEAN" "StdInParse - Win64 ReleaseCLEAN" "ThreadTest - Win64 ReleaseCLEAN" "XSerializerTest - Win64 ReleaseCLEAN" "MemHandlerTest - Win64 ReleaseCLEAN" "XercesLib - Win64 ReleaseCLEAN" "DOMTraversalTest - Win64 ReleaseCLEAN" "DeprecatedDOMCount - Win64 ReleaseCLEAN" "DOMTypeInfoTest - Win64 ReleaseCLEAN" "DOMNormalizerTest - Win64 ReleaseCLEAN" "PSVIWriter - Win64 ReleaseCLEAN" "SCMPrint - Win64 ReleaseCLEAN" "XercesDeprecatedDOMLib - Win64 ReleaseCLEAN" "XSValueTest - Win64 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase 

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

!IF  "$(CFG)" == "all - Win32 Release"

MTL=midl.exe
MTL_PROJ=

!ELSEIF  "$(CFG)" == "all - Win32 Debug"

MTL=midl.exe
MTL_PROJ=

!ELSEIF  "$(CFG)" == "all - Win64 Debug"

MTL=midl.exe
MTL_PROJ=

!ELSEIF  "$(CFG)" == "all - Win64 Release"

MTL=midl.exe
MTL_PROJ=

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("all.dep")
!INCLUDE "all.dep"
!ELSE 
!MESSAGE Warning: cannot find "all.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "all - Win32 Release" || "$(CFG)" == "all - Win32 Debug" || "$(CFG)" == "all - Win64 Debug" || "$(CFG)" == "all - Win64 Release"

!IF  "$(CFG)" == "all - Win32 Release"

"CreateDOMDocument - Win32 Release" : 
   cd "..\CreateDOMDocument"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\CreateDOMDocument.mak" CFG="CreateDOMDocument - Win32 Release" 
   cd "..\all"

"CreateDOMDocument - Win32 ReleaseCLEAN" : 
   cd "..\CreateDOMDocument"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\CreateDOMDocument.mak" CFG="CreateDOMDocument - Win32 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win32 Debug"

"CreateDOMDocument - Win32 Debug" : 
   cd "..\CreateDOMDocument"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\CreateDOMDocument.mak" CFG="CreateDOMDocument - Win32 Debug" 
   cd "..\all"

"CreateDOMDocument - Win32 DebugCLEAN" : 
   cd "..\CreateDOMDocument"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\CreateDOMDocument.mak" CFG="CreateDOMDocument - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Debug"

"CreateDOMDocument - Win64 Debug" : 
   cd "..\CreateDOMDocument"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\CreateDOMDocument.mak" CFG="CreateDOMDocument - Win64 Debug" 
   cd "..\all"

"CreateDOMDocument - Win64 DebugCLEAN" : 
   cd "..\CreateDOMDocument"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\CreateDOMDocument.mak" CFG="CreateDOMDocument - Win64 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Release"

"CreateDOMDocument - Win64 Release" : 
   cd "..\CreateDOMDocument"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\CreateDOMDocument.mak" CFG="CreateDOMDocument - Win64 Release" 
   cd "..\all"

"CreateDOMDocument - Win64 ReleaseCLEAN" : 
   cd "..\CreateDOMDocument"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\CreateDOMDocument.mak" CFG="CreateDOMDocument - Win64 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ENDIF 

!IF  "$(CFG)" == "all - Win32 Release"

"DOMCount - Win32 Release" : 
   cd "..\DOMCount"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMCount.mak" CFG="DOMCount - Win32 Release" 
   cd "..\all"

"DOMCount - Win32 ReleaseCLEAN" : 
   cd "..\DOMCount"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMCount.mak" CFG="DOMCount - Win32 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win32 Debug"

"DOMCount - Win32 Debug" : 
   cd "..\DOMCount"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMCount.mak" CFG="DOMCount - Win32 Debug" 
   cd "..\all"

"DOMCount - Win32 DebugCLEAN" : 
   cd "..\DOMCount"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMCount.mak" CFG="DOMCount - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Debug"

"DOMCount - Win64 Debug" : 
   cd "..\DOMCount"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMCount.mak" CFG="DOMCount - Win64 Debug" 
   cd "..\all"

"DOMCount - Win64 DebugCLEAN" : 
   cd "..\DOMCount"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMCount.mak" CFG="DOMCount - Win64 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Release"

"DOMCount - Win64 Release" : 
   cd "..\DOMCount"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMCount.mak" CFG="DOMCount - Win64 Release" 
   cd "..\all"

"DOMCount - Win64 ReleaseCLEAN" : 
   cd "..\DOMCount"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMCount.mak" CFG="DOMCount - Win64 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ENDIF 

!IF  "$(CFG)" == "all - Win32 Release"

"DOMMemTest - Win32 Release" : 
   cd "..\DOMMemTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMMemTest.mak" CFG="DOMMemTest - Win32 Release" 
   cd "..\all"

"DOMMemTest - Win32 ReleaseCLEAN" : 
   cd "..\DOMMemTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMMemTest.mak" CFG="DOMMemTest - Win32 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win32 Debug"

"DOMMemTest - Win32 Debug" : 
   cd "..\DOMMemTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMMemTest.mak" CFG="DOMMemTest - Win32 Debug" 
   cd "..\all"

"DOMMemTest - Win32 DebugCLEAN" : 
   cd "..\DOMMemTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMMemTest.mak" CFG="DOMMemTest - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Debug"

"DOMMemTest - Win64 Debug" : 
   cd "..\DOMMemTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMMemTest.mak" CFG="DOMMemTest - Win64 Debug" 
   cd "..\all"

"DOMMemTest - Win64 DebugCLEAN" : 
   cd "..\DOMMemTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMMemTest.mak" CFG="DOMMemTest - Win64 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Release"

"DOMMemTest - Win64 Release" : 
   cd "..\DOMMemTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMMemTest.mak" CFG="DOMMemTest - Win64 Release" 
   cd "..\all"

"DOMMemTest - Win64 ReleaseCLEAN" : 
   cd "..\DOMMemTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMMemTest.mak" CFG="DOMMemTest - Win64 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ENDIF 

!IF  "$(CFG)" == "all - Win32 Release"

"DOMPrint - Win32 Release" : 
   cd "..\DOMPrint"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMPrint.mak" CFG="DOMPrint - Win32 Release" 
   cd "..\all"

"DOMPrint - Win32 ReleaseCLEAN" : 
   cd "..\DOMPrint"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMPrint.mak" CFG="DOMPrint - Win32 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win32 Debug"

"DOMPrint - Win32 Debug" : 
   cd "..\DOMPrint"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMPrint.mak" CFG="DOMPrint - Win32 Debug" 
   cd "..\all"

"DOMPrint - Win32 DebugCLEAN" : 
   cd "..\DOMPrint"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMPrint.mak" CFG="DOMPrint - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Debug"

"DOMPrint - Win64 Debug" : 
   cd "..\DOMPrint"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMPrint.mak" CFG="DOMPrint - Win64 Debug" 
   cd "..\all"

"DOMPrint - Win64 DebugCLEAN" : 
   cd "..\DOMPrint"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMPrint.mak" CFG="DOMPrint - Win64 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Release"

"DOMPrint - Win64 Release" : 
   cd "..\DOMPrint"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMPrint.mak" CFG="DOMPrint - Win64 Release" 
   cd "..\all"

"DOMPrint - Win64 ReleaseCLEAN" : 
   cd "..\DOMPrint"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMPrint.mak" CFG="DOMPrint - Win64 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ENDIF 

!IF  "$(CFG)" == "all - Win32 Release"

"DOMTest - Win32 Release" : 
   cd "..\DOMTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMTest.mak" CFG="DOMTest - Win32 Release" 
   cd "..\all"

"DOMTest - Win32 ReleaseCLEAN" : 
   cd "..\DOMTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMTest.mak" CFG="DOMTest - Win32 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win32 Debug"

"DOMTest - Win32 Debug" : 
   cd "..\DOMTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMTest.mak" CFG="DOMTest - Win32 Debug" 
   cd "..\all"

"DOMTest - Win32 DebugCLEAN" : 
   cd "..\DOMTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMTest.mak" CFG="DOMTest - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Debug"

"DOMTest - Win64 Debug" : 
   cd "..\DOMTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMTest.mak" CFG="DOMTest - Win64 Debug" 
   cd "..\all"

"DOMTest - Win64 DebugCLEAN" : 
   cd "..\DOMTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMTest.mak" CFG="DOMTest - Win64 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Release"

"DOMTest - Win64 Release" : 
   cd "..\DOMTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMTest.mak" CFG="DOMTest - Win64 Release" 
   cd "..\all"

"DOMTest - Win64 ReleaseCLEAN" : 
   cd "..\DOMTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMTest.mak" CFG="DOMTest - Win64 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ENDIF 

!IF  "$(CFG)" == "all - Win32 Release"

"EncodingTest - Win32 Release" : 
   cd "..\EncodingTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\EncodingTest.mak" CFG="EncodingTest - Win32 Release" 
   cd "..\all"

"EncodingTest - Win32 ReleaseCLEAN" : 
   cd "..\EncodingTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\EncodingTest.mak" CFG="EncodingTest - Win32 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win32 Debug"

"EncodingTest - Win32 Debug" : 
   cd "..\EncodingTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\EncodingTest.mak" CFG="EncodingTest - Win32 Debug" 
   cd "..\all"

"EncodingTest - Win32 DebugCLEAN" : 
   cd "..\EncodingTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\EncodingTest.mak" CFG="EncodingTest - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Debug"

"EncodingTest - Win64 Debug" : 
   cd "..\EncodingTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\EncodingTest.mak" CFG="EncodingTest - Win64 Debug" 
   cd "..\all"

"EncodingTest - Win64 DebugCLEAN" : 
   cd "..\EncodingTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\EncodingTest.mak" CFG="EncodingTest - Win64 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Release"

"EncodingTest - Win64 Release" : 
   cd "..\EncodingTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\EncodingTest.mak" CFG="EncodingTest - Win64 Release" 
   cd "..\all"

"EncodingTest - Win64 ReleaseCLEAN" : 
   cd "..\EncodingTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\EncodingTest.mak" CFG="EncodingTest - Win64 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ENDIF 

!IF  "$(CFG)" == "all - Win32 Release"

"EnumVal - Win32 Release" : 
   cd "..\EnumVal"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\EnumVal.mak" CFG="EnumVal - Win32 Release" 
   cd "..\all"

"EnumVal - Win32 ReleaseCLEAN" : 
   cd "..\EnumVal"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\EnumVal.mak" CFG="EnumVal - Win32 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win32 Debug"

"EnumVal - Win32 Debug" : 
   cd "..\EnumVal"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\EnumVal.mak" CFG="EnumVal - Win32 Debug" 
   cd "..\all"

"EnumVal - Win32 DebugCLEAN" : 
   cd "..\EnumVal"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\EnumVal.mak" CFG="EnumVal - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Debug"

"EnumVal - Win64 Debug" : 
   cd "..\EnumVal"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\EnumVal.mak" CFG="EnumVal - Win64 Debug" 
   cd "..\all"

"EnumVal - Win64 DebugCLEAN" : 
   cd "..\EnumVal"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\EnumVal.mak" CFG="EnumVal - Win64 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Release"

"EnumVal - Win64 Release" : 
   cd "..\EnumVal"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\EnumVal.mak" CFG="EnumVal - Win64 Release" 
   cd "..\all"

"EnumVal - Win64 ReleaseCLEAN" : 
   cd "..\EnumVal"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\EnumVal.mak" CFG="EnumVal - Win64 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ENDIF 

!IF  "$(CFG)" == "all - Win32 Release"

"InitTermTest - Win32 Release" : 
   cd "..\InitTermTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\InitTermTest.mak" CFG="InitTermTest - Win32 Release" 
   cd "..\all"

"InitTermTest - Win32 ReleaseCLEAN" : 
   cd "..\InitTermTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\InitTermTest.mak" CFG="InitTermTest - Win32 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win32 Debug"

"InitTermTest - Win32 Debug" : 
   cd "..\InitTermTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\InitTermTest.mak" CFG="InitTermTest - Win32 Debug" 
   cd "..\all"

"InitTermTest - Win32 DebugCLEAN" : 
   cd "..\InitTermTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\InitTermTest.mak" CFG="InitTermTest - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Debug"

"InitTermTest - Win64 Debug" : 
   cd "..\InitTermTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\InitTermTest.mak" CFG="InitTermTest - Win64 Debug" 
   cd "..\all"

"InitTermTest - Win64 DebugCLEAN" : 
   cd "..\InitTermTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\InitTermTest.mak" CFG="InitTermTest - Win64 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Release"

"InitTermTest - Win64 Release" : 
   cd "..\InitTermTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\InitTermTest.mak" CFG="InitTermTest - Win64 Release" 
   cd "..\all"

"InitTermTest - Win64 ReleaseCLEAN" : 
   cd "..\InitTermTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\InitTermTest.mak" CFG="InitTermTest - Win64 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ENDIF 

!IF  "$(CFG)" == "all - Win32 Release"

"MemParse - Win32 Release" : 
   cd "..\MemParse"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\MemParse.mak" CFG="MemParse - Win32 Release" 
   cd "..\all"

"MemParse - Win32 ReleaseCLEAN" : 
   cd "..\MemParse"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\MemParse.mak" CFG="MemParse - Win32 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win32 Debug"

"MemParse - Win32 Debug" : 
   cd "..\MemParse"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\MemParse.mak" CFG="MemParse - Win32 Debug" 
   cd "..\all"

"MemParse - Win32 DebugCLEAN" : 
   cd "..\MemParse"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\MemParse.mak" CFG="MemParse - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Debug"

"MemParse - Win64 Debug" : 
   cd "..\MemParse"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\MemParse.mak" CFG="MemParse - Win64 Debug" 
   cd "..\all"

"MemParse - Win64 DebugCLEAN" : 
   cd "..\MemParse"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\MemParse.mak" CFG="MemParse - Win64 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Release"

"MemParse - Win64 Release" : 
   cd "..\MemParse"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\MemParse.mak" CFG="MemParse - Win64 Release" 
   cd "..\all"

"MemParse - Win64 ReleaseCLEAN" : 
   cd "..\MemParse"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\MemParse.mak" CFG="MemParse - Win64 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ENDIF 

!IF  "$(CFG)" == "all - Win32 Release"

"PParse - Win32 Release" : 
   cd "..\PParse"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\PParse.mak" CFG="PParse - Win32 Release" 
   cd "..\all"

"PParse - Win32 ReleaseCLEAN" : 
   cd "..\PParse"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\PParse.mak" CFG="PParse - Win32 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win32 Debug"

"PParse - Win32 Debug" : 
   cd "..\PParse"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\PParse.mak" CFG="PParse - Win32 Debug" 
   cd "..\all"

"PParse - Win32 DebugCLEAN" : 
   cd "..\PParse"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\PParse.mak" CFG="PParse - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Debug"

"PParse - Win64 Debug" : 
   cd "..\PParse"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\PParse.mak" CFG="PParse - Win64 Debug" 
   cd "..\all"

"PParse - Win64 DebugCLEAN" : 
   cd "..\PParse"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\PParse.mak" CFG="PParse - Win64 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Release"

"PParse - Win64 Release" : 
   cd "..\PParse"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\PParse.mak" CFG="PParse - Win64 Release" 
   cd "..\all"

"PParse - Win64 ReleaseCLEAN" : 
   cd "..\PParse"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\PParse.mak" CFG="PParse - Win64 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ENDIF 

!IF  "$(CFG)" == "all - Win32 Release"

"RangeTest - Win32 Release" : 
   cd "..\RangeTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\RangeTest.mak" CFG="RangeTest - Win32 Release" 
   cd "..\all"

"RangeTest - Win32 ReleaseCLEAN" : 
   cd "..\RangeTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\RangeTest.mak" CFG="RangeTest - Win32 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win32 Debug"

"RangeTest - Win32 Debug" : 
   cd "..\RangeTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\RangeTest.mak" CFG="RangeTest - Win32 Debug" 
   cd "..\all"

"RangeTest - Win32 DebugCLEAN" : 
   cd "..\RangeTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\RangeTest.mak" CFG="RangeTest - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Debug"

"RangeTest - Win64 Debug" : 
   cd "..\RangeTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\RangeTest.mak" CFG="RangeTest - Win64 Debug" 
   cd "..\all"

"RangeTest - Win64 DebugCLEAN" : 
   cd "..\RangeTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\RangeTest.mak" CFG="RangeTest - Win64 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Release"

"RangeTest - Win64 Release" : 
   cd "..\RangeTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\RangeTest.mak" CFG="RangeTest - Win64 Release" 
   cd "..\all"

"RangeTest - Win64 ReleaseCLEAN" : 
   cd "..\RangeTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\RangeTest.mak" CFG="RangeTest - Win64 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ENDIF 

!IF  "$(CFG)" == "all - Win32 Release"

"Redirect - Win32 Release" : 
   cd "..\Redirect"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\Redirect.mak" CFG="Redirect - Win32 Release" 
   cd "..\all"

"Redirect - Win32 ReleaseCLEAN" : 
   cd "..\Redirect"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\Redirect.mak" CFG="Redirect - Win32 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win32 Debug"

"Redirect - Win32 Debug" : 
   cd "..\Redirect"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\Redirect.mak" CFG="Redirect - Win32 Debug" 
   cd "..\all"

"Redirect - Win32 DebugCLEAN" : 
   cd "..\Redirect"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\Redirect.mak" CFG="Redirect - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Debug"

"Redirect - Win64 Debug" : 
   cd "..\Redirect"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\Redirect.mak" CFG="Redirect - Win64 Debug" 
   cd "..\all"

"Redirect - Win64 DebugCLEAN" : 
   cd "..\Redirect"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\Redirect.mak" CFG="Redirect - Win64 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Release"

"Redirect - Win64 Release" : 
   cd "..\Redirect"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\Redirect.mak" CFG="Redirect - Win64 Release" 
   cd "..\all"

"Redirect - Win64 ReleaseCLEAN" : 
   cd "..\Redirect"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\Redirect.mak" CFG="Redirect - Win64 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ENDIF 

!IF  "$(CFG)" == "all - Win32 Release"

"SAX2Count - Win32 Release" : 
   cd "..\SAX2Count"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SAX2Count.mak" CFG="SAX2Count - Win32 Release" 
   cd "..\all"

"SAX2Count - Win32 ReleaseCLEAN" : 
   cd "..\SAX2Count"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SAX2Count.mak" CFG="SAX2Count - Win32 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win32 Debug"

"SAX2Count - Win32 Debug" : 
   cd "..\SAX2Count"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SAX2Count.mak" CFG="SAX2Count - Win32 Debug" 
   cd "..\all"

"SAX2Count - Win32 DebugCLEAN" : 
   cd "..\SAX2Count"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SAX2Count.mak" CFG="SAX2Count - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Debug"

"SAX2Count - Win64 Debug" : 
   cd "..\SAX2Count"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SAX2Count.mak" CFG="SAX2Count - Win64 Debug" 
   cd "..\all"

"SAX2Count - Win64 DebugCLEAN" : 
   cd "..\SAX2Count"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SAX2Count.mak" CFG="SAX2Count - Win64 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Release"

"SAX2Count - Win64 Release" : 
   cd "..\SAX2Count"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SAX2Count.mak" CFG="SAX2Count - Win64 Release" 
   cd "..\all"

"SAX2Count - Win64 ReleaseCLEAN" : 
   cd "..\SAX2Count"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SAX2Count.mak" CFG="SAX2Count - Win64 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ENDIF 

!IF  "$(CFG)" == "all - Win32 Release"

"SAX2Print - Win32 Release" : 
   cd "..\SAX2Print"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SAX2Print.mak" CFG="SAX2Print - Win32 Release" 
   cd "..\all"

"SAX2Print - Win32 ReleaseCLEAN" : 
   cd "..\SAX2Print"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SAX2Print.mak" CFG="SAX2Print - Win32 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win32 Debug"

"SAX2Print - Win32 Debug" : 
   cd "..\SAX2Print"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SAX2Print.mak" CFG="SAX2Print - Win32 Debug" 
   cd "..\all"

"SAX2Print - Win32 DebugCLEAN" : 
   cd "..\SAX2Print"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SAX2Print.mak" CFG="SAX2Print - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Debug"

"SAX2Print - Win64 Debug" : 
   cd "..\SAX2Print"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SAX2Print.mak" CFG="SAX2Print - Win64 Debug" 
   cd "..\all"

"SAX2Print - Win64 DebugCLEAN" : 
   cd "..\SAX2Print"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SAX2Print.mak" CFG="SAX2Print - Win64 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Release"

"SAX2Print - Win64 Release" : 
   cd "..\SAX2Print"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SAX2Print.mak" CFG="SAX2Print - Win64 Release" 
   cd "..\all"

"SAX2Print - Win64 ReleaseCLEAN" : 
   cd "..\SAX2Print"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SAX2Print.mak" CFG="SAX2Print - Win64 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ENDIF 

!IF  "$(CFG)" == "all - Win32 Release"

"SAXCount - Win32 Release" : 
   cd "..\SAXCount"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SAXCount.mak" CFG="SAXCount - Win32 Release" 
   cd "..\all"

"SAXCount - Win32 ReleaseCLEAN" : 
   cd "..\SAXCount"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SAXCount.mak" CFG="SAXCount - Win32 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win32 Debug"

"SAXCount - Win32 Debug" : 
   cd "..\SAXCount"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SAXCount.mak" CFG="SAXCount - Win32 Debug" 
   cd "..\all"

"SAXCount - Win32 DebugCLEAN" : 
   cd "..\SAXCount"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SAXCount.mak" CFG="SAXCount - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Debug"

"SAXCount - Win64 Debug" : 
   cd "..\SAXCount"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SAXCount.mak" CFG="SAXCount - Win64 Debug" 
   cd "..\all"

"SAXCount - Win64 DebugCLEAN" : 
   cd "..\SAXCount"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SAXCount.mak" CFG="SAXCount - Win64 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Release"

"SAXCount - Win64 Release" : 
   cd "..\SAXCount"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SAXCount.mak" CFG="SAXCount - Win64 Release" 
   cd "..\all"

"SAXCount - Win64 ReleaseCLEAN" : 
   cd "..\SAXCount"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SAXCount.mak" CFG="SAXCount - Win64 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ENDIF 

!IF  "$(CFG)" == "all - Win32 Release"

"SAXPrint - Win32 Release" : 
   cd "..\SAXPrint"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SAXPrint.mak" CFG="SAXPrint - Win32 Release" 
   cd "..\all"

"SAXPrint - Win32 ReleaseCLEAN" : 
   cd "..\SAXPrint"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SAXPrint.mak" CFG="SAXPrint - Win32 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win32 Debug"

"SAXPrint - Win32 Debug" : 
   cd "..\SAXPrint"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SAXPrint.mak" CFG="SAXPrint - Win32 Debug" 
   cd "..\all"

"SAXPrint - Win32 DebugCLEAN" : 
   cd "..\SAXPrint"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SAXPrint.mak" CFG="SAXPrint - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Debug"

"SAXPrint - Win64 Debug" : 
   cd "..\SAXPrint"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SAXPrint.mak" CFG="SAXPrint - Win64 Debug" 
   cd "..\all"

"SAXPrint - Win64 DebugCLEAN" : 
   cd "..\SAXPrint"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SAXPrint.mak" CFG="SAXPrint - Win64 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Release"

"SAXPrint - Win64 Release" : 
   cd "..\SAXPrint"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SAXPrint.mak" CFG="SAXPrint - Win64 Release" 
   cd "..\all"

"SAXPrint - Win64 ReleaseCLEAN" : 
   cd "..\SAXPrint"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SAXPrint.mak" CFG="SAXPrint - Win64 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ENDIF 

!IF  "$(CFG)" == "all - Win32 Release"

"SEnumVal - Win32 Release" : 
   cd "..\SEnumVal"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SEnumVal.mak" CFG="SEnumVal - Win32 Release" 
   cd "..\all"

"SEnumVal - Win32 ReleaseCLEAN" : 
   cd "..\SEnumVal"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SEnumVal.mak" CFG="SEnumVal - Win32 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win32 Debug"

"SEnumVal - Win32 Debug" : 
   cd "..\SEnumVal"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SEnumVal.mak" CFG="SEnumVal - Win32 Debug" 
   cd "..\all"

"SEnumVal - Win32 DebugCLEAN" : 
   cd "..\SEnumVal"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SEnumVal.mak" CFG="SEnumVal - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Debug"

"SEnumVal - Win64 Debug" : 
   cd "..\SEnumVal"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SEnumVal.mak" CFG="SEnumVal - Win64 Debug" 
   cd "..\all"

"SEnumVal - Win64 DebugCLEAN" : 
   cd "..\SEnumVal"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SEnumVal.mak" CFG="SEnumVal - Win64 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Release"

"SEnumVal - Win64 Release" : 
   cd "..\SEnumVal"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SEnumVal.mak" CFG="SEnumVal - Win64 Release" 
   cd "..\all"

"SEnumVal - Win64 ReleaseCLEAN" : 
   cd "..\SEnumVal"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SEnumVal.mak" CFG="SEnumVal - Win64 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ENDIF 

!IF  "$(CFG)" == "all - Win32 Release"

"StdInParse - Win32 Release" : 
   cd "..\StdInParse"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\StdInParse.mak" CFG="StdInParse - Win32 Release" 
   cd "..\all"

"StdInParse - Win32 ReleaseCLEAN" : 
   cd "..\StdInParse"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\StdInParse.mak" CFG="StdInParse - Win32 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win32 Debug"

"StdInParse - Win32 Debug" : 
   cd "..\StdInParse"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\StdInParse.mak" CFG="StdInParse - Win32 Debug" 
   cd "..\all"

"StdInParse - Win32 DebugCLEAN" : 
   cd "..\StdInParse"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\StdInParse.mak" CFG="StdInParse - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Debug"

"StdInParse - Win64 Debug" : 
   cd "..\StdInParse"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\StdInParse.mak" CFG="StdInParse - Win64 Debug" 
   cd "..\all"

"StdInParse - Win64 DebugCLEAN" : 
   cd "..\StdInParse"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\StdInParse.mak" CFG="StdInParse - Win64 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Release"

"StdInParse - Win64 Release" : 
   cd "..\StdInParse"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\StdInParse.mak" CFG="StdInParse - Win64 Release" 
   cd "..\all"

"StdInParse - Win64 ReleaseCLEAN" : 
   cd "..\StdInParse"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\StdInParse.mak" CFG="StdInParse - Win64 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ENDIF 

!IF  "$(CFG)" == "all - Win32 Release"

"ThreadTest - Win32 Release" : 
   cd "..\ThreadTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\ThreadTest.mak" CFG="ThreadTest - Win32 Release" 
   cd "..\all"

"ThreadTest - Win32 ReleaseCLEAN" : 
   cd "..\ThreadTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\ThreadTest.mak" CFG="ThreadTest - Win32 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win32 Debug"

"ThreadTest - Win32 Debug" : 
   cd "..\ThreadTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\ThreadTest.mak" CFG="ThreadTest - Win32 Debug" 
   cd "..\all"

"ThreadTest - Win32 DebugCLEAN" : 
   cd "..\ThreadTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\ThreadTest.mak" CFG="ThreadTest - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Debug"

"ThreadTest - Win64 Debug" : 
   cd "..\ThreadTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\ThreadTest.mak" CFG="ThreadTest - Win64 Debug" 
   cd "..\all"

"ThreadTest - Win64 DebugCLEAN" : 
   cd "..\ThreadTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\ThreadTest.mak" CFG="ThreadTest - Win64 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Release"

"ThreadTest - Win64 Release" : 
   cd "..\ThreadTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\ThreadTest.mak" CFG="ThreadTest - Win64 Release" 
   cd "..\all"

"ThreadTest - Win64 ReleaseCLEAN" : 
   cd "..\ThreadTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\ThreadTest.mak" CFG="ThreadTest - Win64 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ENDIF 

!IF  "$(CFG)" == "all - Win32 Release"

"XSerializerTest - Win32 Release" : 
   cd "..\XSerializerTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XSerializerTest.mak" CFG="XSerializerTest - Win32 Release" 
   cd "..\all"

"XSerializerTest - Win32 ReleaseCLEAN" : 
   cd "..\XSerializerTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XSerializerTest.mak" CFG="XSerializerTest - Win32 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win32 Debug"

"XSerializerTest - Win32 Debug" : 
   cd "..\XSerializerTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XSerializerTest.mak" CFG="XSerializerTest - Win32 Debug" 
   cd "..\all"

"XSerializerTest - Win32 DebugCLEAN" : 
   cd "..\XSerializerTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XSerializerTest.mak" CFG="XSerializerTest - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Debug"

"XSerializerTest - Win64 Debug" : 
   cd "..\XSerializerTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XSerializerTest.mak" CFG="XSerializerTest - Win64 Debug" 
   cd "..\all"

"XSerializerTest - Win64 DebugCLEAN" : 
   cd "..\XSerializerTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XSerializerTest.mak" CFG="XSerializerTest - Win64 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Release"

"XSerializerTest - Win64 Release" : 
   cd "..\XSerializerTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XSerializerTest.mak" CFG="XSerializerTest - Win64 Release" 
   cd "..\all"

"XSerializerTest - Win64 ReleaseCLEAN" : 
   cd "..\XSerializerTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XSerializerTest.mak" CFG="XSerializerTest - Win64 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ENDIF 

!IF  "$(CFG)" == "all - Win32 Release"

"MemHandlerTest - Win32 Release" : 
   cd "..\MemHandlerTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\MemHandlerTest.mak" CFG="MemHandlerTest - Win32 Release" 
   cd "..\all"

"MemHandlerTest - Win32 ReleaseCLEAN" : 
   cd "..\MemHandlerTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\MemHandlerTest.mak" CFG="MemHandlerTest - Win32 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win32 Debug"

"MemHandlerTest - Win32 Debug" : 
   cd "..\MemHandlerTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\MemHandlerTest.mak" CFG="MemHandlerTest - Win32 Debug" 
   cd "..\all"

"MemHandlerTest - Win32 DebugCLEAN" : 
   cd "..\MemHandlerTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\MemHandlerTest.mak" CFG="MemHandlerTest - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Debug"

"MemHandlerTest - Win64 Debug" : 
   cd "..\MemHandlerTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\MemHandlerTest.mak" CFG="MemHandlerTest - Win64 Debug" 
   cd "..\all"

"MemHandlerTest - Win64 DebugCLEAN" : 
   cd "..\MemHandlerTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\MemHandlerTest.mak" CFG="MemHandlerTest - Win64 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Release"

"MemHandlerTest - Win64 Release" : 
   cd "..\MemHandlerTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\MemHandlerTest.mak" CFG="MemHandlerTest - Win64 Release" 
   cd "..\all"

"MemHandlerTest - Win64 ReleaseCLEAN" : 
   cd "..\MemHandlerTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\MemHandlerTest.mak" CFG="MemHandlerTest - Win64 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ENDIF 

!IF  "$(CFG)" == "all - Win32 Release"

"XercesLib - Win32 Release" : 
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win32 Release" 
   cd "..\all"

"XercesLib - Win32 ReleaseCLEAN" : 
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win32 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win32 Debug"

"XercesLib - Win32 Debug" : 
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win32 Debug" 
   cd "..\all"

"XercesLib - Win32 DebugCLEAN" : 
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Debug"

"XercesLib - Win64 Debug" : 
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win64 Debug" 
   cd "..\all"

"XercesLib - Win64 DebugCLEAN" : 
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win64 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Release"

"XercesLib - Win64 Release" : 
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win64 Release" 
   cd "..\all"

"XercesLib - Win64 ReleaseCLEAN" : 
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win64 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ENDIF 

!IF  "$(CFG)" == "all - Win32 Release"

"DOMTraversalTest - Win32 Release" : 
   cd "..\DOMTraversalTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMTraversalTest.mak" CFG="DOMTraversalTest - Win32 Release" 
   cd "..\all"

"DOMTraversalTest - Win32 ReleaseCLEAN" : 
   cd "..\DOMTraversalTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMTraversalTest.mak" CFG="DOMTraversalTest - Win32 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win32 Debug"

"DOMTraversalTest - Win32 Debug" : 
   cd "..\DOMTraversalTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMTraversalTest.mak" CFG="DOMTraversalTest - Win32 Debug" 
   cd "..\all"

"DOMTraversalTest - Win32 DebugCLEAN" : 
   cd "..\DOMTraversalTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMTraversalTest.mak" CFG="DOMTraversalTest - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Debug"

"DOMTraversalTest - Win64 Debug" : 
   cd "..\DOMTraversalTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMTraversalTest.mak" CFG="DOMTraversalTest - Win64 Debug" 
   cd "..\all"

"DOMTraversalTest - Win64 DebugCLEAN" : 
   cd "..\DOMTraversalTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMTraversalTest.mak" CFG="DOMTraversalTest - Win64 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Release"

"DOMTraversalTest - Win64 Release" : 
   cd "..\DOMTraversalTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMTraversalTest.mak" CFG="DOMTraversalTest - Win64 Release" 
   cd "..\all"

"DOMTraversalTest - Win64 ReleaseCLEAN" : 
   cd "..\DOMTraversalTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMTraversalTest.mak" CFG="DOMTraversalTest - Win64 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ENDIF 

!IF  "$(CFG)" == "all - Win32 Release"

"DeprecatedDOMCount - Win32 Release" : 
   cd "..\DeprecatedDOMCount"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DeprecatedDOMCount.mak" CFG="DeprecatedDOMCount - Win32 Release" 
   cd "..\all"

"DeprecatedDOMCount - Win32 ReleaseCLEAN" : 
   cd "..\DeprecatedDOMCount"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DeprecatedDOMCount.mak" CFG="DeprecatedDOMCount - Win32 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win32 Debug"

"DeprecatedDOMCount - Win32 Debug" : 
   cd "..\DeprecatedDOMCount"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DeprecatedDOMCount.mak" CFG="DeprecatedDOMCount - Win32 Debug" 
   cd "..\all"

"DeprecatedDOMCount - Win32 DebugCLEAN" : 
   cd "..\DeprecatedDOMCount"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DeprecatedDOMCount.mak" CFG="DeprecatedDOMCount - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Debug"

"DeprecatedDOMCount - Win64 Debug" : 
   cd "..\DeprecatedDOMCount"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DeprecatedDOMCount.mak" CFG="DeprecatedDOMCount - Win64 Debug" 
   cd "..\all"

"DeprecatedDOMCount - Win64 DebugCLEAN" : 
   cd "..\DeprecatedDOMCount"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DeprecatedDOMCount.mak" CFG="DeprecatedDOMCount - Win64 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Release"

"DeprecatedDOMCount - Win64 Release" : 
   cd "..\DeprecatedDOMCount"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DeprecatedDOMCount.mak" CFG="DeprecatedDOMCount - Win64 Release" 
   cd "..\all"

"DeprecatedDOMCount - Win64 ReleaseCLEAN" : 
   cd "..\DeprecatedDOMCount"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DeprecatedDOMCount.mak" CFG="DeprecatedDOMCount - Win64 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ENDIF 

!IF  "$(CFG)" == "all - Win32 Release"

"DOMTypeInfoTest - Win32 Release" : 
   cd "..\DOMTypeInfoTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMTypeInfoTest.mak" CFG="DOMTypeInfoTest - Win32 Release" 
   cd "..\all"

"DOMTypeInfoTest - Win32 ReleaseCLEAN" : 
   cd "..\DOMTypeInfoTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMTypeInfoTest.mak" CFG="DOMTypeInfoTest - Win32 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win32 Debug"

"DOMTypeInfoTest - Win32 Debug" : 
   cd "..\DOMTypeInfoTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMTypeInfoTest.mak" CFG="DOMTypeInfoTest - Win32 Debug" 
   cd "..\all"

"DOMTypeInfoTest - Win32 DebugCLEAN" : 
   cd "..\DOMTypeInfoTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMTypeInfoTest.mak" CFG="DOMTypeInfoTest - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Debug"

"DOMTypeInfoTest - Win64 Debug" : 
   cd "..\DOMTypeInfoTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMTypeInfoTest.mak" CFG="DOMTypeInfoTest - Win64 Debug" 
   cd "..\all"

"DOMTypeInfoTest - Win64 DebugCLEAN" : 
   cd "..\DOMTypeInfoTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMTypeInfoTest.mak" CFG="DOMTypeInfoTest - Win64 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Release"

"DOMTypeInfoTest - Win64 Release" : 
   cd "..\DOMTypeInfoTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMTypeInfoTest.mak" CFG="DOMTypeInfoTest - Win64 Release" 
   cd "..\all"

"DOMTypeInfoTest - Win64 ReleaseCLEAN" : 
   cd "..\DOMTypeInfoTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMTypeInfoTest.mak" CFG="DOMTypeInfoTest - Win64 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ENDIF 

!IF  "$(CFG)" == "all - Win32 Release"

"DOMNormalizerTest - Win32 Release" : 
   cd "..\DOMNormalizerTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMNormalizerTest.mak" CFG="DOMNormalizerTest - Win32 Release" 
   cd "..\all"

"DOMNormalizerTest - Win32 ReleaseCLEAN" : 
   cd "..\DOMNormalizerTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMNormalizerTest.mak" CFG="DOMNormalizerTest - Win32 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win32 Debug"

"DOMNormalizerTest - Win32 Debug" : 
   cd "..\DOMNormalizerTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMNormalizerTest.mak" CFG="DOMNormalizerTest - Win32 Debug" 
   cd "..\all"

"DOMNormalizerTest - Win32 DebugCLEAN" : 
   cd "..\DOMNormalizerTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMNormalizerTest.mak" CFG="DOMNormalizerTest - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Debug"

"DOMNormalizerTest - Win64 Debug" : 
   cd "..\DOMNormalizerTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMNormalizerTest.mak" CFG="DOMNormalizerTest - Win64 Debug" 
   cd "..\all"

"DOMNormalizerTest - Win64 DebugCLEAN" : 
   cd "..\DOMNormalizerTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMNormalizerTest.mak" CFG="DOMNormalizerTest - Win64 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Release"

"DOMNormalizerTest - Win64 Release" : 
   cd "..\DOMNormalizerTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMNormalizerTest.mak" CFG="DOMNormalizerTest - Win64 Release" 
   cd "..\all"

"DOMNormalizerTest - Win64 ReleaseCLEAN" : 
   cd "..\DOMNormalizerTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\DOMNormalizerTest.mak" CFG="DOMNormalizerTest - Win64 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ENDIF 

!IF  "$(CFG)" == "all - Win32 Release"

"PSVIWriter - Win32 Release" : 
   cd "..\PSVIWriter"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\PSVIWriter.mak" CFG="PSVIWriter - Win32 Release" 
   cd "..\all"

"PSVIWriter - Win32 ReleaseCLEAN" : 
   cd "..\PSVIWriter"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\PSVIWriter.mak" CFG="PSVIWriter - Win32 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win32 Debug"

"PSVIWriter - Win32 Debug" : 
   cd "..\PSVIWriter"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\PSVIWriter.mak" CFG="PSVIWriter - Win32 Debug" 
   cd "..\all"

"PSVIWriter - Win32 DebugCLEAN" : 
   cd "..\PSVIWriter"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\PSVIWriter.mak" CFG="PSVIWriter - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Debug"

"PSVIWriter - Win64 Debug" : 
   cd "..\PSVIWriter"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\PSVIWriter.mak" CFG="PSVIWriter - Win64 Debug" 
   cd "..\all"

"PSVIWriter - Win64 DebugCLEAN" : 
   cd "..\PSVIWriter"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\PSVIWriter.mak" CFG="PSVIWriter - Win64 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Release"

"PSVIWriter - Win64 Release" : 
   cd "..\PSVIWriter"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\PSVIWriter.mak" CFG="PSVIWriter - Win64 Release" 
   cd "..\all"

"PSVIWriter - Win64 ReleaseCLEAN" : 
   cd "..\PSVIWriter"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\PSVIWriter.mak" CFG="PSVIWriter - Win64 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ENDIF 

!IF  "$(CFG)" == "all - Win32 Release"

"SCMPrint - Win32 Release" : 
   cd "..\SCMPrint"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SCMPrint.mak" CFG="SCMPrint - Win32 Release" 
   cd "..\all"

"SCMPrint - Win32 ReleaseCLEAN" : 
   cd "..\SCMPrint"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SCMPrint.mak" CFG="SCMPrint - Win32 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win32 Debug"

"SCMPrint - Win32 Debug" : 
   cd "..\SCMPrint"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SCMPrint.mak" CFG="SCMPrint - Win32 Debug" 
   cd "..\all"

"SCMPrint - Win32 DebugCLEAN" : 
   cd "..\SCMPrint"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SCMPrint.mak" CFG="SCMPrint - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Debug"

"SCMPrint - Win64 Debug" : 
   cd "..\SCMPrint"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SCMPrint.mak" CFG="SCMPrint - Win64 Debug" 
   cd "..\all"

"SCMPrint - Win64 DebugCLEAN" : 
   cd "..\SCMPrint"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SCMPrint.mak" CFG="SCMPrint - Win64 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Release"

"SCMPrint - Win64 Release" : 
   cd "..\SCMPrint"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SCMPrint.mak" CFG="SCMPrint - Win64 Release" 
   cd "..\all"

"SCMPrint - Win64 ReleaseCLEAN" : 
   cd "..\SCMPrint"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\SCMPrint.mak" CFG="SCMPrint - Win64 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ENDIF 

!IF  "$(CFG)" == "all - Win32 Release"

"XercesDeprecatedDOMLib - Win32 Release" : 
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesDeprecatedDOMLib.mak" CFG="XercesDeprecatedDOMLib - Win32 Release" 
   cd "..\all"

"XercesDeprecatedDOMLib - Win32 ReleaseCLEAN" : 
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesDeprecatedDOMLib.mak" CFG="XercesDeprecatedDOMLib - Win32 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win32 Debug"

"XercesDeprecatedDOMLib - Win32 Debug" : 
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesDeprecatedDOMLib.mak" CFG="XercesDeprecatedDOMLib - Win32 Debug" 
   cd "..\all"

"XercesDeprecatedDOMLib - Win32 DebugCLEAN" : 
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesDeprecatedDOMLib.mak" CFG="XercesDeprecatedDOMLib - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Debug"

"XercesDeprecatedDOMLib - Win64 Debug" : 
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesDeprecatedDOMLib.mak" CFG="XercesDeprecatedDOMLib - Win64 Debug" 
   cd "..\all"

"XercesDeprecatedDOMLib - Win64 DebugCLEAN" : 
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesDeprecatedDOMLib.mak" CFG="XercesDeprecatedDOMLib - Win64 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Release"

"XercesDeprecatedDOMLib - Win64 Release" : 
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesDeprecatedDOMLib.mak" CFG="XercesDeprecatedDOMLib - Win64 Release" 
   cd "..\all"

"XercesDeprecatedDOMLib - Win64 ReleaseCLEAN" : 
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesDeprecatedDOMLib.mak" CFG="XercesDeprecatedDOMLib - Win64 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ENDIF 

!IF  "$(CFG)" == "all - Win32 Release"

"XSValueTest - Win32 Release" : 
   cd "..\XSValueTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XSValueTest.mak" CFG="XSValueTest - Win32 Release" 
   cd "..\all"

"XSValueTest - Win32 ReleaseCLEAN" : 
   cd "..\XSValueTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XSValueTest.mak" CFG="XSValueTest - Win32 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win32 Debug"

"XSValueTest - Win32 Debug" : 
   cd "..\XSValueTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XSValueTest.mak" CFG="XSValueTest - Win32 Debug" 
   cd "..\all"

"XSValueTest - Win32 DebugCLEAN" : 
   cd "..\XSValueTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XSValueTest.mak" CFG="XSValueTest - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Debug"

"XSValueTest - Win64 Debug" : 
   cd "..\XSValueTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XSValueTest.mak" CFG="XSValueTest - Win64 Debug" 
   cd "..\all"

"XSValueTest - Win64 DebugCLEAN" : 
   cd "..\XSValueTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XSValueTest.mak" CFG="XSValueTest - Win64 Debug" RECURSE=1 CLEAN 
   cd "..\all"

!ELSEIF  "$(CFG)" == "all - Win64 Release"

"XSValueTest - Win64 Release" : 
   cd "..\XSValueTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XSValueTest.mak" CFG="XSValueTest - Win64 Release" 
   cd "..\all"

"XSValueTest - Win64 ReleaseCLEAN" : 
   cd "..\XSValueTest"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XSValueTest.mak" CFG="XSValueTest - Win64 Release" RECURSE=1 CLEAN 
   cd "..\all"

!ENDIF 


!ENDIF 

