# Microsoft Developer Studio Generated NMAKE File, Based on SEnumVal.dsp
!IF "$(CFG)" == ""
CFG=SEnumVal - Win64 Debug
!MESSAGE No configuration specified. Defaulting to SEnumVal - Win64 Debug.
!ENDIF 

!IF "$(CFG)" != "SEnumVal - Win32 Debug" && "$(CFG)" != "SEnumVal - Win64 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "SEnumVal.mak" CFG="SEnumVal - Win64 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "SEnumVal - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "SEnumVal - Win64 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "SEnumVal - Win32 Debug"

OUTDIR=.\..\..\..\..\..\bin
INTDIR=.\..\..\..\..\..\bin\obj
# Begin Custom Macros
OutDir=.\..\..\..\..\..\bin
# End Custom Macros

ALL : "$(OUTDIR)\SEnumVal.exe"


CLEAN :
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
CPP_PROJ=/nologo /G5 /MDd /Za /W3 /Gm /GX /Zi /Od /I "..\..\..\..\..\include" /D "PROJ_SAXCOUNT" /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D "PLATFORM_WIN32" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\SEnumVal.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib xerces-c_2D.lib /nologo /version:1.0 /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\SEnumVal.pdb" /debug /machine:I386 /out:"$(OUTDIR)\SEnumVal.exe" /pdbtype:sept /libpath:"..\..\..\..\..\lib" 
LINK32_OBJS= \
	"$(INTDIR)\SEnumVal.obj"

"$(OUTDIR)\SEnumVal.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "SEnumVal - Win64 Debug"

OUTDIR=.\..\..\..\..\..\bin
INTDIR=.\..\..\..\..\..\bin\obj
# Begin Custom Macros
OutDir=.\..\..\..\..\..\bin
# End Custom Macros

ALL : "$(OUTDIR)\SEnumVal.exe"


CLEAN :
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
CPP_PROJ=/MDd /Za /W3 /Gm /GX /Zi /Od /I "..\..\..\..\..\include" /D "WIN64" /D "PROJ_SAXCOUNT" /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D "PLATFORM_WIN32" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /machine:IA64 /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\SEnumVal.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib xerces-c_2D.lib /nologo /version:1.0 /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\SEnumVal.pdb" /debug /machine:IX86 /out:"$(OUTDIR)\SEnumVal.exe" /pdbtype:sept /libpath:"..\..\..\..\..\lib" /machine:IA64 
LINK32_OBJS= \
	"$(INTDIR)\SEnumVal.obj"

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


!IF "$(CFG)" == "SEnumVal - Win32 Debug" || "$(CFG)" == "SEnumVal - Win64 Debug"
SOURCE=..\..\..\..\SEnumVal\SEnumVal.cpp

"$(INTDIR)\SEnumVal.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)



!ENDIF 

