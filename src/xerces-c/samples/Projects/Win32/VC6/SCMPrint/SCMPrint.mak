# Microsoft Developer Studio Generated NMAKE File, Based on SCMPrint.dsp
!IF "$(CFG)" == ""
CFG=SCMPrint - Win64 Debug
!MESSAGE No configuration specified. Defaulting to SCMPrint - Win64 Debug.
!ENDIF 

!IF "$(CFG)" != "SCMPrint - Win32 Debug" && "$(CFG)" != "SCMPrint - Win64 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "SCMPrint.mak" CFG="SCMPrint - Win64 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "SCMPrint - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "SCMPrint - Win64 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

CPP=$(CPP)

!IF  "$(CFG)" == "SCMPrint - Win32 Debug"

OUTDIR=.\..\..\..\..\..\bin
INTDIR=.\..\..\..\..\..\bin\obj
# Begin Custom Macros
OutDir=.\..\..\..\..\..\bin
# End Custom Macros

ALL : "$(OUTDIR)\SCMPrint.exe"


CLEAN :
	-@erase "$(INTDIR)\SCMPrint.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\SCMPrint.exe"
	-@erase "$(OUTDIR)\SCMPrint.ilk"
	-@erase "$(OUTDIR)\SCMPrint.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP_PROJ=/nologo /G5 /MDd /Za /W3 /Gm /GX /Zi /Od /I "..\..\..\..\..\include" /D "PROJ_SCMPrint" /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D "PLATFORM_WIN32" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\SCMPrint.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib xerces-c_2D.lib /nologo /version:1.0 /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\SCMPrint.pdb" /debug /machine:I386 /out:"$(OUTDIR)\SCMPrint.exe" /pdbtype:sept /libpath:"..\..\..\..\..\lib" 
LINK32_OBJS= \
	"$(INTDIR)\SCMPrint.obj"

"$(OUTDIR)\SCMPrint.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "SCMPrint - Win64 Debug"

OUTDIR=.\..\..\..\..\..\bin
INTDIR=.\..\..\..\..\..\bin\obj
# Begin Custom Macros
OutDir=.\..\..\..\..\..\bin
# End Custom Macros

ALL : "$(OUTDIR)\SCMPrint.exe"


CLEAN :
	-@erase "$(INTDIR)\SCMPrint.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\SCMPrint.exe"
	-@erase "$(OUTDIR)\SCMPrint.ilk"
	-@erase "$(OUTDIR)\SCMPrint.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP_PROJ=/MDd /Za /W3 /Gm /GX /Zi /Od /I "..\..\..\..\..\include" /D "WIN64" /D "PROJ_SCMPrint" /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D "PLATFORM_WIN32" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /machine:IA64 /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\SCMPrint.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib xerces-c_2D.lib /nologo /version:1.0 /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\SCMPrint.pdb" /debug /machine:IX86 /out:"$(OUTDIR)\SCMPrint.exe" /pdbtype:sept /libpath:"..\..\..\..\..\lib" /machine:IA64 
LINK32_OBJS= \
	"$(INTDIR)\SCMPrint.obj"

"$(OUTDIR)\SCMPrint.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

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


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("SCMPrint.dep")
!INCLUDE "SCMPrint.dep"
!ELSE 
!MESSAGE Warning: cannot find "SCMPrint.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "SCMPrint - Win32 Debug" || "$(CFG)" == "SCMPrint - Win64 Debug"
SOURCE=..\..\..\..\SCMPrint\SCMPrint.cpp

"$(INTDIR)\SCMPrint.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)



!ENDIF 

