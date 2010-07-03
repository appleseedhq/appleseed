# Microsoft Developer Studio Generated NMAKE File, Based on DOMPrint.dsp
!IF "$(CFG)" == ""
CFG=DOMPrint - Win64 Debug
!MESSAGE No configuration specified. Defaulting to DOMPrint - Win64 Debug.
!ENDIF 

!IF "$(CFG)" != "DOMPrint - Win32 Debug" && "$(CFG)" != "DOMPrint - Win64 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "DOMPrint.mak" CFG="DOMPrint - Win64 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "DOMPrint - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "DOMPrint - Win64 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "DOMPrint - Win32 Debug"

OUTDIR=.\..\..\..\..\..\bin
INTDIR=.\..\..\..\..\..\bin\obj
# Begin Custom Macros
OutDir=.\..\..\..\..\..\bin
# End Custom Macros

ALL : "$(OUTDIR)\DOMPrint.exe"


CLEAN :
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
CPP_PROJ=/nologo /G5 /MDd /Za /W3 /Gm /GX /Zi /Od /I "..\..\..\..\..\include" /D "PROJ_DOMPRINT" /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D "PLATFORM_WIN32" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\DOMPrint.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib xerces-c_2D.lib /nologo /version:1.0 /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\DOMPrint.pdb" /debug /machine:I386 /out:"$(OUTDIR)\DOMPrint.exe" /pdbtype:sept /libpath:"..\..\..\..\..\lib" 
LINK32_OBJS= \
	"$(INTDIR)\DOMPrint.obj" \
	"$(INTDIR)\DOMPrintErrorHandler.obj" \
	"$(INTDIR)\DOMPrintFilter.obj" \
	"$(INTDIR)\DOMTreeErrorReporter.obj"

"$(OUTDIR)\DOMPrint.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "DOMPrint - Win64 Debug"

OUTDIR=.\..\..\..\..\..\bin
INTDIR=.\..\..\..\..\..\bin\obj
# Begin Custom Macros
OutDir=.\..\..\..\..\..\bin
# End Custom Macros

ALL : "$(OUTDIR)\DOMPrint.exe"


CLEAN :
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
CPP_PROJ=/MDd /Za /W3 /Gm /GX /Zi /Od /I "..\..\..\..\..\include" /D "WIN64" /D "PROJ_DOMPRINT" /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D "PLATFORM_WIN32" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /machine:IA64 /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\DOMPrint.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib xerces-c_2D.lib /nologo /version:1.0 /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\DOMPrint.pdb" /debug /machine:IX86 /out:"$(OUTDIR)\DOMPrint.exe" /pdbtype:sept /libpath:"..\..\..\..\..\lib" /machine:IA64 
LINK32_OBJS= \
	"$(INTDIR)\DOMPrint.obj" \
	"$(INTDIR)\DOMPrintErrorHandler.obj" \
	"$(INTDIR)\DOMPrintFilter.obj" \
	"$(INTDIR)\DOMTreeErrorReporter.obj"

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


!IF "$(CFG)" == "DOMPrint - Win32 Debug" || "$(CFG)" == "DOMPrint - Win64 Debug"
SOURCE=..\..\..\..\DOMPrint\DOMPrint.cpp

"$(INTDIR)\DOMPrint.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\DOMPrint\DOMPrintErrorHandler.cpp

"$(INTDIR)\DOMPrintErrorHandler.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\DOMPrint\DOMPrintFilter.cpp

"$(INTDIR)\DOMPrintFilter.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\..\..\DOMPrint\DOMTreeErrorReporter.cpp

"$(INTDIR)\DOMTreeErrorReporter.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)



!ENDIF 

