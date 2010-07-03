## Process this file with automake to produce Makefile.in

if BUILD_IMFEXAMPLES
noinst_PROGRAMS = imfexamples
endif

INCLUDES = -I$(top_builddir) -I$(top_srcdir)/Iex \
           -I$(top_srcdir)/Half -I$(top_srcdir)/Imath \
           -I$(top_srcdir)/IlmImf -I$(top_srcdir)/config

LDADD = -L$(top_builddir)/Iex -L$(top_builddir)/Half \
	-L$(top_builddir)/Imath -L$(top_builddir)/IlmThread \
	-L$(top_builddir)/IlmImf \
	-lIlmImf -lIlmThread -lImath -lHalf -lIex -lz

imfexamples_SOURCES = main.cpp drawImage.cpp rgbaInterfaceExamples.cpp \
		      rgbaInterfaceTiledExamples.cpp \
		      generalInterfaceExamples.cpp \
		      lowLevelIoExamples.cpp previewImageExamples.cpp \
		      generalInterfaceTiledExamples.cpp \
	              generalInterfaceTiledExamples.h drawImage.h \
		      rgbaInterfaceExamples.h generalInterfaceExamples.h \
		      rgbaInterfaceTiledExamples.h \
		      lowLevelIoExamples.h previewImageExamples.h

examplesdir = $(datadir)/doc/OpenEXR-@OPENEXR_VERSION@/examples
examples_DATA = $(imfexamples_SOURCES)

imfexamplesdir = $(examplesdir)

