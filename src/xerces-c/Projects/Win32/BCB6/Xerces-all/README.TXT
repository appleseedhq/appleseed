Jan 21, 2003 - [Bug 16277] Readme should make note of threaded library problems in BCB6 
=====================================================================================
There's another known problem with BCB6 to do with linking single-threaded
applications to DLLs.  Specifically, DLLs are normally linked to the
multithreaded runtime DLL (CC3260MT.dll); however, if an application is linked
to the single-threaded DLL (CC3260.dll) and consequently attempts to free memory
allocated by the MT DLL there will be an memory access violation.  In particular
this causes problems when using the transcoding utilities that return allocated
memory (which the sample programs use).

Currently there appears to be no way of forcing an application to be linked
against the MT version of the runtime, therefore it is recommended that the
versions of utilities which accept pre-allocated buffers be used.

Feb 7, 2002 - Xerces C and Borland C++Builder6
==============================================

The enclosed project file no longer produce a Xerces library that is dependent upon
the Borland memory manager (borlndmm.dll).  However there is still a dependency upon
the multi-threaded C++ Run Time Library.  The RTL is named cc3260mt.dll and is
included in this distribution for your convenience.

The supplied project files support Borland C++Builder6.  The overwhelming size of the
Xerces project will cause some problems for the new IDE.  The version 6 IDE appends
the source file directories to the include and lib paths, even though it is not
necessary.  The resulting project file becomes corrupted due to the unnatural line
lengths.  The file cannot be edited by hand to correct the situation, as the IDE will
simply "fix" the paths for you when it opens the project.  This will discovered late
in BCB6 field test, at a point in time when it was impossible to alter the project
options dialog box.  A work around was to have the IDE check a registry entry that
would prohibit it from modifying paths.  Set the following value in the registry:

[HKEY_CURRENT_USER\Software\Borland\C++Builder\6.0\Globals]
"DontManagePaths"="1"

or simply double click on the .reg file in this directory.

Don Mastrovito