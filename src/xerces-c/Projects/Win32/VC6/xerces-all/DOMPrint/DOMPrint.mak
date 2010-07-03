# Microsoft Developer Studio Generated NMAKE File, Based on DOMPrint.dsp
!IF "$(CFG)" == ""
CFG=DOMPrint - Win32 Debug
!MESSAGE No configuration specified. Defaulting to DOMPrint - Win32 Debug.
!ENDIF

!IF "$(CFG)" != "DOMPrint - Win32 Release" && "$(CFG)" != "DOMPrint - Win32 Debug" && "$(CFG)" != "DOMPrint - Win64 Debug" && "$(CFG)" != "DOMPrint - Win64 Release"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE
!MESSAGE NMAKE /f "DOMPrint.mak" CFG="DOMPrint - Win32 Debug"
!MESSAGE
!MESSAGE Possible choices for configuration are:
!MESSAGE
!MESSAGE "DOMPrint - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "DOMPrint - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "DOMPrint - Win64 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "DOMPrint - Win64 Release" (based on "Win32 (x86) Console Application")
!MESSAGE
!ERROR An invalid configuration is specified.
!ENDIF

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE
NULL=nul
!ENDIF

!IF  "$(CFG)" == "DOMPrint - Win32 Release"

OUTDIR=.\..\..\..\..\..\Build\Win32\VC6\Release
INTDIR=.\..\..\..\..\..\Build\Win32\VC6\Release\obj
# Begin Custom Macros
OutDir=.\..\..\..\..\..\Build\Win32\VC6\Release
# End Custom Macros

!IF "$(RECURSE)" == "0"

ALL : "$(OUTDIR)\DOMPrint.exe"

!ELSE

ALL : "XercesLib - Win32 Release" "$(OUTDIR)\DOMPrint.exe"
!ENDIF

!IF "$(RECURSE)" == "1"
CLEAN :"XercesLib - Win32 ReleaseCLEAN"
!ELSE
CLEAN :
!ENDIF
	-@erase "$(INTDIR)\DOMPrint.obj"
	-@erase "$(INTDIR)\DOMPrintErrorHandler.obj"
	-@erase "$(INTDIR)\DOMPrintFilter.obj"
	-@erase "$(INTDIR)\DOMTreeErrorReporter.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\DOMPrint.exe"
	-@erase "$(OUTDIR)\DOMPrint.ilk"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=$(CPP)
CPP_PROJ=/G6 /MD /W3 /GX /O2 /Ob2 /I "..\..\..\..\..\src" /D "NDEBUG" /D "_CONSOLE" /D "WIN32" /D "_WINDOWS" /D "PLATFORM_WIN32" /FR"$(INTDIR)\\" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c

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

RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/o"$(OUTDIR)\DOMPrint.bsc"
BSC32_SBRS= \

LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib xerces-c_2.lib /version:1.0 /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\DOMPrint.pdb" /machine:I386 /out:"$(OUTDIR)\DOMPrint.exe" /libpath:"..\..\..\..\..\Build\Win32\VC6\Release"
LINK32_OBJS= \
	"$(INTDIR)\DOMPrint.obj" \
	"$(INTDIR)\DOMPrintErrorHandler.obj" \
	"$(INTDIR)\DOMPrintFilter.obj" \
	"$(INTDIR)\DOMTreeErrorReporter.obj" \
	"$(OUTDIR)\xerces-c_2.lib"

"$(OUTDIR)\DOMPrint.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "DOMPrint - Win32 Debug"

OUTDIR=.\..\..\..\..\..\Build\Win32\VC6\Debug
INTDIR=.\..\..\..\..\..\Build\Win32\VC6\Debug\obj
# Begin Custom Macros
OutDir=.\..\..\..\..\..\Build\Win32\VC6\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0"

ALL : "$(OUTDIR)\DOMPrint.exe"

!ELSE

ALL : "XercesLib - Win32 Debug" "$(OUTDIR)\DOMPrint.exe"

!ENDIF

!IF "$(RECURSE)" == "1"
CLEAN :"XercesLib - Win32 DebugCLEAN"
!ELSE
CLEAN :
!ENDIF
	-@erase "$(INTDIR)\DOMPrint.obj"
	-@erase "$(INTDIR)\DOMPrintErrorHandler.obj"
	-@erase "$(INTDIR)\DOMPrintFilter.obj"
	-@erase "$(INTDIR)\DOMTreeErrorReporter.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\DOMPrint.exe"
	-@erase "$(OUTDIR)\DOMPrint.ilk"
	-@erase "$(OUTDIR)\DOMPrint.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=$(CPP)
CPP_PROJ=/G6 /MDd /Za /W3 /Gm /GX /ZI /Od /I "..\..\..\..\..\src" /D "_DEBUG" /D "_CONSOLE" /D "WIN32" /D "_WINDOWS" /D "PLATFORM_WIN32" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c

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

RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/o"$(OUTDIR)\DOMPrint.bsc"
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib xerces-c_2D.lib /version:3.0 /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\DOMPrint.pdb" /debug /machine:I386 /out:"$(OUTDIR)\DOMPrint.exe" /pdbtype:sept /libpath:"..\..\..\..\..\Build\Win32\VC6\Debug"
LINK32_OBJS= \
	"$(INTDIR)\DOMPrint.obj" \
	"$(INTDIR)\DOMPrintErrorHandler.obj" \
	"$(INTDIR)\DOMPrintFilter.obj" \
	"$(INTDIR)\DOMTreeErrorReporter.obj" \
	"$(OUTDIR)\xerces-c_2D.lib"

"$(OUTDIR)\DOMPrint.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "DOMPrint - Win64 Debug"

OUTDIR=.\..\..\..\..\..\Build\Win64\VC6\Debug
INTDIR=.\..\..\..\..\..\Build\Win64\VC6\Debug\obj
# Begin Custom Macros
OutDir=.\..\..\..\..\..\Build\Win64\VC6\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0"

ALL : "$(OUTDIR)\DOMPrint.exe"

!ELSE

ALL : "XercesLib - Win64 Debug" "$(OUTDIR)\DOMPrint.exe"

!ENDIF

!IF "$(RECURSE)" == "1"
CLEAN :"XercesLib - Win64 DebugCLEAN"
!ELSE
CLEAN :
!ENDIF
	-@erase "$(INTDIR)\DOMPrint.obj"
	-@erase "$(INTDIR)\DOMPrintErrorHandler.obj"
	-@erase "$(INTDIR)\DOMPrintFilter.obj"
	-@erase "$(INTDIR)\DOMTreeErrorReporter.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\DOMPrint.exe"
	-@erase "$(OUTDIR)\DOMPrint.ilk"
	-@erase "$(OUTDIR)\DOMPrint.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=$(CPP)
CPP_PROJ=/MDd /W3 /Gm /GX /ZI /Od /I "..\..\..\..\..\src" /D "WIN64" /D "_DEBUG" /D "_CONSOLE" /D "WIN32" /D "_WINDOWS" /D "PLATFORM_WIN32" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c

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

RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/o"$(OUTDIR)\DOMPrint.bsc"
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib xerces-c_2D.lib /version:3.0 /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\DOMPrint.pdb" /debug /machine:IX86 /out:"$(OUTDIR)\DOMPrint.exe" /pdbtype:sept /libpath:"..\..\..\..\..\Build\Win64\VC6\Debug" /machine:IA64
LINK32_OBJS= \
	"$(INTDIR)\DOMPrint.obj" \
	"$(INTDIR)\DOMPrintErrorHandler.obj" \
	"$(INTDIR)\DOMPrintFilter.obj" \
	"$(INTDIR)\DOMTreeErrorReporter.obj" \
	"$(OUTDIR)\xerces-c_2D.lib"

"$(OUTDIR)\DOMPrint.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "DOMPrint - Win64 Release"

OUTDIR=.\..\..\..\..\..\Build\Win64\VC6\Release
INTDIR=.\..\..\..\..\..\Build\Win64\VC6\Release\obj
# Begin Custom Macros
OutDir=.\..\..\..\..\..\Build\Win64\VC6\Release
# End Custom Macros

!IF "$(RECURSE)" == "0"

ALL : "$(OUTDIR)\DOMPrint.exe"

!ELSE

ALL : "XercesLib - Win64 Release" "$(OUTDIR)\DOMPrint.exe"

!ENDIF

!IF "$(RECURSE)" == "1"
CLEAN :"XercesLib - Win64 ReleaseCLEAN"
!ELSE
CLEAN :
!ENDIF
	-@erase "$(INTDIR)\DOMPrint.obj"
	-@erase "$(INTDIR)\DOMPrintErrorHandler.obj"
	-@erase "$(INTDIR)\DOMPrintFilter.obj"
	-@erase "$(INTDIR)\DOMTreeErrorReporter.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\DOMPrint.exe"
	-@erase "$(OUTDIR)\DOMPrint.ilk"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=$(CPP)
CPP_PROJ=/MD /W3 /GX /O2 /Ob2 /I "..\..\..\..\..\src" /D "WIN64" /D "NDEBUG" /D "_CONSOLE" /D "WIN32" /D "_WINDOWS" /D "PLATFORM_WIN32" /FR"$(INTDIR)\\" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c

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

RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/o"$(OUTDIR)\DOMPrint.bsc"
BSC32_SBRS= \

LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib xerces-c_2.lib /version:1.0 /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\DOMPrint.pdb" /machine:IX86 /out:"$(OUTDIR)\DOMPrint.exe" /libpath:"..\..\..\..\..\Build\Win64\VC6\Release" /machine:IA64
LINK32_OBJS= \
	"$(INTDIR)\DOMPrint.obj" \
	"$(INTDIR)\DOMPrintErrorHandler.obj" \
	"$(INTDIR)\DOMPrintFilter.obj" \
	"$(INTDIR)\DOMTreeErrorReporter.obj" \
	"$(OUTDIR)\xerces-c_2.lib"

"$(OUTDIR)\DOMPrint.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("DOMPrint.dep")
!INCLUDE "DOMPrint.dep"
!ELSE
!MESSAGE Warning: cannot find "DOMPrint.dep"
!ENDIF
!ENDIF


!IF "$(CFG)" == "DOMPrint - Win32 Release" || "$(CFG)" == "DOMPrint - Win32 Debug" || "$(CFG)" == "DOMPrint - Win64 Debug" || "$(CFG)" == "DOMPrint - Win64 Release"

!IF  "$(CFG)" == "DOMPrint - Win32 Release"

"XercesLib - Win32 Release" :
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win32 Release"
   cd "..\DOMPrint"

"XercesLib - Win32 ReleaseCLEAN" :
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win32 Release" RECURSE=1 CLEAN
   cd "..\DOMPrint"

!ELSEIF  "$(CFG)" == "DOMPrint - Win32 Debug"

"XercesLib - Win32 Debug" :
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win32 Debug"
   cd "..\DOMPrint"

"XercesLib - Win32 DebugCLEAN" :
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win32 Debug" RECURSE=1 CLEAN
   cd "..\DOMPrint"

!ELSEIF  "$(CFG)" == "DOMPrint - Win64 Debug"

"XercesLib - Win64 Debug" :
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win64 Debug"
   cd "..\DOMPrint"

"XercesLib - Win64 DebugCLEAN" :
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win64 Debug" RECURSE=1 CLEAN
   cd "..\DOMPrint"

!ELSEIF  "$(CFG)" == "DOMPrint - Win64 Release"

"XercesLib - Win64 Release" :
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win64 Release"
   cd "..\DOMPrint"

"XercesLib - Win64 ReleaseCLEAN" :
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win64 Release" RECURSE=1 CLEAN
   cd "..\DOMPrint"

!ENDIF

SOURCE=..\..\..\..\..\samples\DOMPrint\DOMPrint.cpp

"$(INTDIR)\DOMPrint.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\..\samples\DOMPrint\DOMPrintErrorHandler.cpp

"$(INTDIR)\DOMPrintErrorHandler.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)

SOURCE=..\..\..\..\..\samples\DOMPrint\DOMPrintFilter.cpp

"$(INTDIR)\DOMPrintFilter.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)

SOURCE=..\..\..\..\..\samples\DOMPrint\DOMTreeErrorReporter.cpp

"$(INTDIR)\DOMTreeErrorReporter.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF

