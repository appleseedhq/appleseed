## Process this file with automake to produce Makefile.in

bin_PROGRAMS = exrenvmap

INCLUDES = -I$(top_builddir) -I$(top_srcdir)/Iex \
           -I$(top_srcdir)/Half -I$(top_srcdir)/Imath \
           -I$(top_srcdir)/IlmImf -I$(top_srcdir)/config

LDADD = \
	$(top_builddir)/Imath/libImath.la \
	$(top_builddir)/IlmImf/libIlmImf.la \
	$(top_builddir)/Iex/libIex.la \
	$(top_builddir)/Half/libHalf.la \
	$(top_builddir)/IlmThread/libIlmThread.la \
	-lz

exrenvmap_SOURCES = main.cpp EnvmapImage.cpp EnvmapImage.h \
		    makeCubeMap.cpp makeCubeMap.h \
		    makeLatLongMap.cpp makeLatLongMap.h

noinst_HEADERS = EnvmapImage.h makeCubeMap.h makeLatLongMap.h

EXTRA_DIST = main.cpp EnvmapImage.cpp EnvmapImage.h \
	     makeCubeMap.cpp makeCubeMap.h \
	     makeLatLongMap.cpp makeLatLongMap.h
