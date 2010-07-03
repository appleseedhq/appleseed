# Microsoft Developer Studio Generated NMAKE File, Based on SEnumVal.dsp
!IF "$(CFG)" == ""
CFG=SEnumVal - Win32 Debug
!MESSAGE No configuration specified. Defaulting to SEnumVal - Win32 Debug.
!ENDIF

!IF "$(CFG)" != "SEnumVal - Win32 Release" && "$(CFG)" != "SEnumVal - Win32 Debug" && "$(CFG)" != "SEnumVal - Win64 Debug" && "$(CFG)" != "SEnumVal - Win64 Release"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE
!MESSAGE NMAKE /f "SEnumVal.mak" CFG="SEnumVal - Win32 Debug"
!MESSAGE
!MESSAGE Possible choices for configuration are:
!MESSAGE
!MESSAGE "SEnumVal - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "SEnumVal - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "SEnumVal - Win64 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "SEnumVal - Win64 Release" (based on "Win32 (x86) Console Application")
!MESSAGE
!ERROR An invalid configuration is specified.
!ENDIF

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE
NULL=nul
!ENDIF

!IF  "$(CFG)" == "SEnumVal - Win32 Release"

OUTDIR=.\..\..\..\..\..\Build\Win32\VC6\Release
INTDIR=.\..\..\..\..\..\Build\Win32\VC6\Release\obj
# Begin Custom Macros
OutDir=.\..\..\..\..\..\Build\Win32\VC6\Release
# End Custom Macros

!IF "$(RECURSE)" == "0"

ALL : "$(OUTDIR)\SEnumVal.exe"

!ELSE

ALL : "XercesLib - Win32 Release" "$(OUTDIR)\SEnumVal.exe"

!ENDIF

!IF "$(RECURSE)" == "1"
CLEAN :"XercesLib - Win32 ReleaseCLEAN"
!ELSE
CLEAN :
!ENDIF
	-@erase "$(INTDIR)\SEnumVal.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\SEnumVal.exe"
	-@erase "$(OUTDIR)\SEnumVal.ilk"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=$(CPP)
CPP_PROJ=/G6 /MD /Za /W3 /GX /O2 /Ob2 /I "..\..\..\..\..\src" /D "NDEBUG" /D "_CONSOLE" /D "WIN32" /D "_WINDOWS" /D "PLATFORM_WIN32" /FR"$(INTDIR)\\" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c

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
BSC32_FLAGS=/o"$(OUTDIR)\SEnumVal.bsc"
BSC32_SBRS= \

LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib xerces-c_2.lib /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\SEnumVal.pdb" /machine:I386 /out:"$(OUTDIR)\SEnumVal.exe" /libpath:"..\..\..\..\..\Build\Win32\VC6\Release"
LINK32_OBJS= \
	"$(INTDIR)\SEnumVal.obj" \
	"$(OUTDIR)\xerces-c_2.lib"

"$(OUTDIR)\SEnumVal.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "SEnumVal - Win32 Debug"

OUTDIR=.\..\..\..\..\..\Build\Win32\VC6\Debug
INTDIR=.\..\..\..\..\..\Build\Win32\VC6\Debug\obj
# Begin Custom Macros
OutDir=.\..\..\..\..\..\Build\Win32\VC6\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0"

ALL : "$(OUTDIR)\SEnumVal.exe"

!ELSE

ALL : "XercesLib - Win32 Debug" "$(OUTDIR)\SEnumVal.exe"

!ENDIF

!IF "$(RECURSE)" == "1"
CLEAN :"XercesLib - Win32 DebugCLEAN"
!ELSE
CLEAN :
!ENDIF
	-@erase "$(INTDIR)\SEnumVal.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\SEnumVal.exe"
	-@erase "$(OUTDIR)\SEnumVal.ilk"
	-@erase "$(OUTDIR)\SEnumVal.pdb"

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
BSC32_FLAGS=/o"$(OUTDIR)\SEnumVal.bsc"
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib xerces-c_2D.lib /version:1.0 /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\SEnumVal.pdb" /debug /machine:I386 /out:"$(OUTDIR)\SEnumVal.exe" /pdbtype:sept /libpath:"..\..\..\..\..\Build\Win32\VC6\Debug"
LINK32_OBJS= \
	"$(INTDIR)\SEnumVal.obj" \
	"$(OUTDIR)\xerces-c_2D.lib"

"$(OUTDIR)\SEnumVal.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "SEnumVal - Win64 Debug"

OUTDIR=.\..\..\..\..\..\Build\Win64\VC6\Debug
INTDIR=.\..\..\..\..\..\Build\Win64\VC6\Debug\obj
# Begin Custom Macros
OutDir=.\..\..\..\..\..\Build\Win64\VC6\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0"

ALL : "$(OUTDIR)\SEnumVal.exe"

!ELSE

ALL : "XercesLib - Win64 Debug" "$(OUTDIR)\SEnumVal.exe"

!ENDIF

!IF "$(RECURSE)" == "1"
CLEAN :"XercesLib - Win64 DebugCLEAN"
!ELSE
CLEAN :
!ENDIF
	-@erase "$(INTDIR)\SEnumVal.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\SEnumVal.exe"
	-@erase "$(OUTDIR)\SEnumVal.ilk"
	-@erase "$(OUTDIR)\SEnumVal.pdb"

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
BSC32_FLAGS=/o"$(OUTDIR)\SEnumVal.bsc"
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib xerces-c_2D.lib /version:1.0 /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\SEnumVal.pdb" /debug /machine:IX86 /out:"$(OUTDIR)\SEnumVal.exe" /pdbtype:sept /libpath:"..\..\..\..\..\Build\Win64\VC6\Debug" /machine:IA64
LINK32_OBJS= \
	"$(INTDIR)\SEnumVal.obj" \
	"$(OUTDIR)\xerces-c_2D.lib"

"$(OUTDIR)\SEnumVal.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "SEnumVal - Win64 Release"

OUTDIR=.\..\..\..\..\..\Build\Win64\VC6\Release
INTDIR=.\..\..\..\..\..\Build\Win64\VC6\Release\obj
# Begin Custom Macros
OutDir=.\..\..\..\..\..\Build\Win64\VC6\Release
# End Custom Macros

!IF "$(RECURSE)" == "0"

ALL : "$(OUTDIR)\SEnumVal.exe"

!ELSE

ALL : "XercesLib - Win64 Release" "$(OUTDIR)\SEnumVal.exe"

!ENDIF

!IF "$(RECURSE)" == "1"
CLEAN :"XercesLib - Win64 ReleaseCLEAN"
!ELSE
CLEAN :
!ENDIF
	-@erase "$(INTDIR)\SEnumVal.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\SEnumVal.exe"
	-@erase "$(OUTDIR)\SEnumVal.ilk"

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
BSC32_FLAGS=/o"$(OUTDIR)\SEnumVal.bsc"
BSC32_SBRS= \

LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib xerces-c_2.lib /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\SEnumVal.pdb" /machine:IX86 /out:"$(OUTDIR)\SEnumVal.exe" /libpath:"..\..\..\..\..\Build\Win64\VC6\Release" /machine:IA64
LINK32_OBJS= \
	"$(INTDIR)\SEnumVal.obj" \
	"$(OUTDIR)\xerces-c_2.lib"

"$(OUTDIR)\SEnumVal.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("SEnumVal.dep")
!INCLUDE "SEnumVal.dep"
!ELSE
!MESSAGE Warning: cannot find "SEnumVal.dep"
!ENDIF
!ENDIF


!IF "$(CFG)" == "SEnumVal - Win32 Release" || "$(CFG)" == "SEnumVal - Win32 Debug" || "$(CFG)" == "SEnumVal - Win64 Debug" || "$(CFG)" == "SEnumVal - Win64 Release"

!IF  "$(CFG)" == "SEnumVal - Win32 Release"

"XercesLib - Win32 Release" :
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win32 Release"
   cd "..\SEnumVal"

"XercesLib - Win32 ReleaseCLEAN" :
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win32 Release" RECURSE=1 CLEAN
   cd "..\SEnumVal"

!ELSEIF  "$(CFG)" == "SEnumVal - Win32 Debug"

"XercesLib - Win32 Debug" :
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win32 Debug"
   cd "..\SEnumVal"

"XercesLib - Win32 DebugCLEAN" :
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win32 Debug" RECURSE=1 CLEAN
   cd "..\SEnumVal"

!ELSEIF  "$(CFG)" == "SEnumVal - Win64 Debug"

"XercesLib - Win64 Debug" :
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win64 Debug"
   cd "..\SEnumVal"

"XercesLib - Win64 DebugCLEAN" :
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win64 Debug" RECURSE=1 CLEAN
   cd "..\SEnumVal"

!ELSEIF  "$(CFG)" == "SEnumVal - Win64 Release"

"XercesLib - Win64 Release" :
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win64 Release"
   cd "..\SEnumVal"

"XercesLib - Win64 ReleaseCLEAN" :
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win64 Release" RECURSE=1 CLEAN
   cd "..\SEnumVal"

!ENDIF

SOURCE=..\..\..\..\..\samples\SEnumVal\SEnumVal.cpp

"$(INTDIR)\SEnumVal.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)



!ENDIF

