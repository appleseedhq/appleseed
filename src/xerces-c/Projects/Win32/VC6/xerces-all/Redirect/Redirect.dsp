# Microsoft Developer Studio Project File - Name="Redirect" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=Redirect - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "Redirect.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Redirect.mak" CFG="Redirect - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Redirect - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "Redirect - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "Redirect - Win64 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "Redirect - Win64 Release" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "Redirect - Win32 Release"

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
# ADD BASE CPP  /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP  /G6 /MD /Za /W3 /GX /O2 /I "..\..\..\..\..\src" /D "NDEBUG" /D "_CONSOLE" /D "WIN32" /D "_WINDOWS" /D "PLATFORM_WIN32" /FD /c
# SUBTRACT CPP /YX
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 
# ADD BSC32 
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib  /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib xerces-c_2.lib  /version:1.0 /subsystem:console /incremental:yes /machine:I386 /libpath:"..\..\..\..\..\Build\Win32\VC6\Release"

!ELSEIF  "$(CFG)" == "Redirect - Win32 Debug"

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
# ADD BASE CPP  /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP  /G6 /MDd /Za /W3 /Gm /GX /ZI /Od /I "..\..\..\..\..\src" /D "_DEBUG" /D "_CONSOLE" /D "WIN32" /D "_WINDOWS" /D "PLATFORM_WIN32" /FD /c
# SUBTRACT CPP /YX
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 
# ADD BSC32 
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib  /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib xerces-c_2D.lib  /version:1.0 /subsystem:console /debug /machine:I386 /pdbtype:sept /libpath:"..\..\..\..\..\Build\Win32\VC6\Debug"

!ELSEIF  "$(CFG)" == "Redirect - Win64 Debug"

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
# ADD BASE CPP  /G6 /MDd /Za /W3 /Gm /GX /ZI /Od /I "..\..\..\..\..\src" /D "_DEBUG" /D "_CONSOLE" /D "WIN32" /D "_WINDOWS" /D "PLATFORM_WIN32" /FD /c
# SUBTRACT BASE CPP /YX
# ADD CPP /MDd /W3 /Gm /GX /ZI /Od /I "..\..\..\..\..\src" /D "WIN64" /D "_DEBUG" /D "_CONSOLE" /D "WIN32" /D "_WINDOWS" /D "PLATFORM_WIN32" /FD /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 
# ADD BSC32 
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib xerces-c_2D.lib  /version:1.0 /subsystem:console /debug  /pdbtype:sept /libpath:"..\..\..\..\..\Build\Win32\VC6\Debug"
# ADD LINK32 kernel32.lib user32.lib xerces-c_2D.lib /version:1.0 /subsystem:console /debug  /pdbtype:sept /libpath:"..\..\..\..\..\Build\Win64\VC6\Debug" /machine:IA64
# SUBTRACT LINK32  /pdb:none

!ELSEIF  "$(CFG)" == "Redirect - Win64 Release"

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
# ADD BASE CPP  /G6 /MD /Za /W3 /GX /O2 /I "..\..\..\..\..\src" /D "NDEBUG" /D "_CONSOLE" /D "WIN32" /D "_WINDOWS" /D "PLATFORM_WIN32" /FD /c
# SUBTRACT BASE CPP /YX
# ADD CPP /MD /Ze /W3 /GX /O2 /I "..\..\..\..\..\src" /D "WIN64" /D "NDEBUG" /D "_CONSOLE" /D "WIN32" /D "_WINDOWS" /D "PLATFORM_WIN32" /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 
# ADD BSC32 
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib xerces-c_2.lib  /version:1.0 /subsystem:console /incremental:yes  /libpath:"..\..\..\..\..\Build\Win32\VC6\Release"
# ADD LINK32 kernel32.lib user32.lib xerces-c_2.lib /version:1.0 /subsystem:console /incremental:yes  /libpath:"..\..\..\..\..\Build\Win64\VC6\Release" /machine:IA64
# SUBTRACT LINK32  /pdb:none

!ENDIF 

# Begin Target

# Name "Redirect - Win32 Release"
# Name "Redirect - Win32 Debug"
# Name "Redirect - Win64 Debug"
# Name "Redirect - Win64 Release"
# Begin Source File

SOURCE=..\..\..\..\..\samples\Redirect\Redirect.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\samples\Redirect\Redirect.hpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\samples\Redirect\RedirectHandlers.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\..\samples\Redirect\RedirectHandlers.hpp
# End Source File
# End Target
# End Project
