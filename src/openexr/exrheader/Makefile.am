## Process this file with automake to produce Makefile.in

bin_PROGRAMS = exrheader

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

exrheader_SOURCES = main.cpp

