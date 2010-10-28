#-----------------------------------------------------------------------------
# Makefile for ann2fig
#
# ANN: Approximate Nearest Neighbors
# Version: 0.1 (Beta release)
#-----------------------------------------------------------------------------
# Revision 0.1  03/04/98
#	Initial release
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Basic definitions
#		BASEDIR		where include, src, lib, ... are
#		INCDIR		include directory
#		LIBDIR		library directory
#		BINDIR		bin directory
#		LDFLAGS		loader flags
#		ANNLIB		ANN library
#		OTHERLIB	other libraries
#-----------------------------------------------------------------------------

BASEDIR	= ..
INCDIR	= $(BASEDIR)/include
LIBDIR	= $(BASEDIR)/lib
BINDIR	= $(BASEDIR)/bin
LDFLAGS	= -L$(LIBDIR)
ANNLIBS	= -lANN
OTHERLIBS = -lm

#-----------------------------------------------------------------------------
# Some more definitions
#	ANN2FIG		name of executable
#-----------------------------------------------------------------------------

ANN2FIG = ann2fig
SOURCES = ann2fig.cpp
OBJECTS = $(SOURCES:.cpp=.o)

#-----------------------------------------------------------------------------
# Make the program
#-----------------------------------------------------------------------------

default: 
	@echo "Specify a target configuration"

targets: $(BINDIR)/$(ANN2FIG)

$(BINDIR)/$(ANN2FIG): $(OBJECTS)
	$(C++) $(OBJECTS) -o $(ANN2FIG) $(LDFLAGS) $(ANNLIBS) $(OTHERLIBS)
	mv $(ANN2FIG) $(BINDIR)

#-----------------------------------------------------------------------------
# configuration definitions
#-----------------------------------------------------------------------------

include ../Make-config

#-----------------------------------------------------------------------------
# Objects
#-----------------------------------------------------------------------------

ann2fig.o: ann2fig.cpp
	$(C++) -c -I$(INCDIR) ann2fig.cpp

#-----------------------------------------------------------------------------
# Cleaning
#-----------------------------------------------------------------------------

clean:
	-rm -f *.o core

realclean: clean
