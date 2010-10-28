#-----------------------------------------------------------------------------
# Makefile for Windows Versions.
#
# ANN: Approximate Nearest Neighbors
# Version: 1.0 (initial release)
#-----------------------------------------------------------------------------
# Copyright (c) 1997 University of Maryland and Sunil Arya and David Mount
# All Rights Reserved.
# 
# This software and related documentation is part of the 
# Approximate Nearest Neighbor Library (ANN).
# 
# Permission to use, copy, and distribute this software and its 
# documentation is hereby granted free of charge, provided that 
# (1) it is not a component of a commercial product, and 
# (2) this notice appears in all copies of the software and
#     related documentation. 
# 
# The University of Maryland and the authors make no representations
# about the suitability or fitness of this software for any purpose.
# It is provided "as is" without express or implied warranty.
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# This is not used for compiling the dll. It is just used for cleaning
# things up for distribution.  For compilcation, open the Ann.sln
# solution file in Microsoft Windows Visual Studio.NET.
#-----------------------------------------------------------------------------

default:
	@echo "Enter one of the following:"
	@echo "  make clean                remove object files"
	@echo "  make realclean            remove library and executable files"
	@echo " "
	@echo "See file Makefile for other compilation options."

#-----------------------------------------------------------------------------
# Remove .o files and core files
#-----------------------------------------------------------------------------
clean:
	-rm -f -r ann2fig/Debug ann2fig/Release
	-rm -f -r dll/Debug dll/Release
	-rm -f -r sample/Debug sample/Release
	-rm -f -r test/Debug test/Release
	-rm -f Ann.ncb Ann.suo

#-----------------------------------------------------------------------------
# Remove everthing that can be remade
#-----------------------------------------------------------------------------
realclean:  clean
	-rm -f bin/*
