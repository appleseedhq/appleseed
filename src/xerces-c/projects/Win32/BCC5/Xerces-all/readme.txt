Sep 30, 2004 - Xerces C and Borland C++ Compiler v 5.6.2 (CBuilderX 1.0 Update 1)
========================================================

The Update 1 for CBuilderX 1.0 renames the make.exe found in the bin directory 
to bmake.exe, and adds make.exe (from GNU). To be able to compile Xerces 
rename bmake.exe back to make.exe

Feb 27, 2003 - Xerces C and Borland C++ Compiler v 5.5.1
========================================================

 - changes for Xerces C 2.2.0

How to build dll with Win95 support (tasm32 required, so only for BCB5):
	make -f Xerces-all.mak -DWITHASM=Y

Aug 23, 2002 - Xerces C and Borland C++ Compiler v 5.5.1
========================================================

Before build:
	MakeBuildDirs.bat

How to build dll (without deprecated DOM API) and tests:
	make -f Xerces-all.mak

Vitaly Prapirny (marl@mebius.net)

