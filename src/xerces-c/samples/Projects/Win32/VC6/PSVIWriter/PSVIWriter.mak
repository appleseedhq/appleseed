# Microsoft Developer Studio Generated NMAKE File, Based on PSVIWriter.dsp
!IF "$(CFG)" == ""
CFG=PSVIWriter - Win64 Debug
!MESSAGE No configuration specified. Defaulting to PSVIWriter - Win64 Debug.
!ENDIF 

!IF "$(CFG)" != "PSVIWriter - Win32 Debug" && "$(CFG)" != "PSVIWriter - Win64 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "PSVIWriter.mak" CFG="PSVIWriter - Win64 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "PSVIWriter - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "PSVIWriter - Win64 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

CPP=$(CPP)

!IF  "$(CFG)" == "PSVIWriter - Win32 Debug"

OUTDIR=.\..\..\..\..\..\bin
INTDIR=.\..\..\..\..\..\bin\obj
# Begin Custom Macros
OutDir=.\..\..\..\..\..\bin
# End Custom Macros

ALL : "$(OUTDIR)\PSVIWriter.exe"


CLEAN :
	-@erase "$(INTDIR)\PSVIUni.obj"
	-@erase "$(INTDIR)\PSVIWriter.obj"
	-@erase "$(INTDIR)\PSVIWriterHandlers.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\PSVIWriter.exe"
	-@erase "$(OUTDIR)\PSVIWriter.ilk"
	-@erase "$(OUTDIR)\PSVIWriter.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP_PROJ=/nologo /G5 /MDd /Za /W3 /Gm /GX /Zi /Od /I "..\..\..\..\..\include" /D "PROJ_PSVIWriter" /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D "PLATFORM_WIN32" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\PSVIWriter.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib xerces-c_2D.lib /nologo /version:1.0 /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\PSVIWriter.pdb" /debug /machine:I386 /out:"$(OUTDIR)\PSVIWriter.exe" /pdbtype:sept /libpath:"..\..\..\..\..\lib" 
LINK32_OBJS= \
	"$(INTDIR)\PSVIUni.obj" \
	"$(INTDIR)\PSVIWriter.obj" \
	"$(INTDIR)\PSVIWriterHandlers.obj"

"$(OUTDIR)\PSVIWriter.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "PSVIWriter - Win64 Debug"

OUTDIR=.\..\..\..\..\..\bin
INTDIR=.\..\..\..\..\..\bin\obj
# Begin Custom Macros
OutDir=.\..\..\..\..\..\bin
# End Custom Macros

ALL : "$(OUTDIR)\PSVIWriter.exe"


CLEAN :
	-@erase "$(INTDIR)\PSVIUni.obj"
	-@erase "$(INTDIR)\PSVIWriter.obj"
	-@erase "$(INTDIR)\PSVIWriterHandlers.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\PSVIWriter.exe"
	-@erase "$(OUTDIR)\PSVIWriter.ilk"
	-@erase "$(OUTDIR)\PSVIWriter.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP_PROJ=/MDd /Za /W3 /Gm /GX /Zi /Od /I "..\..\..\..\..\include" /D "WIN64" /D "PROJ_PSVIWriter" /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D "PLATFORM_WIN32" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /machine:IA64 /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\PSVIWriter.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib xerces-c_2D.lib /nologo /version:1.0 /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\PSVIWriter.pdb" /debug /machine:IX86 /out:"$(OUTDIR)\PSVIWriter.exe" /pdbtype:sept /libpath:"..\..\..\..\..\lib" /machine:IA64 
LINK32_OBJS= \
	"$(INTDIR)\PSVIUni.obj" \
	"$(INTDIR)\PSVIWriter.obj" \
	"$(INTDIR)\PSVIWriterHandlers.obj"

"$(OUTDIR)\PSVIWriter.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
!IF EXISTS("PSVIWriter.dep")
!INCLUDE "PSVIWriter.dep"
!ELSE 
!MESSAGE Warning: cannot find "PSVIWriter.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "PSVIWriter - Win32 Debug" || "$(CFG)" == "PSVIWriter - Win64 Debug"
SOURCE=..\..\..\..\PSVIWriter\PSVIUni.cpp

"$(INTDIR)\PSVIUni.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\PSVIWriter\PSVIWriter.cpp

"$(INTDIR)\PSVIWriter.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\PSVIWriter\PSVIWriterHandlers.cpp

"$(INTDIR)\PSVIWriterHandlers.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)



!ENDIF 

