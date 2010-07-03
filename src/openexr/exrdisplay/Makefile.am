## Process this file with automake to produce Makefile.in

if HAVE_FLTK

bin_PROGRAMS = exrdisplay

INCLUDES = @GL_CXXFLAGS@ @CG_CXXFLAGS@ @FLTK_CXXFLAGS@ -I$(top_builddir) \
	   -I$(top_srcdir)/Iex -I$(top_srcdir)/Half -I$(top_srcdir)/Imath \
           -I$(top_srcdir)/IlmImf -I$(top_srcdir)/config

LDADD = @CG_LDFLAGS@ @FLTK_LDFLAGS@ \
	$(top_builddir)/Iex/libIex.la \
	$(top_builddir)/Half/libHalf.la \
	$(top_builddir)/Imath/libImath.la \
	$(top_builddir)/IlmImf/libIlmImf.la \
	$(top_builddir)/IlmThread/libIlmThread.la \
	-lz

install-exec-hook:
	@FLTK_CONFIG@ --post $(DESTDIR)$(bindir)/exrdisplay

else
install-exec-hook:

endif

if HAVE_CG
fragshader_sources = ImageViewFragShader.cpp ImageViewFragShader.h
else
fragshader_sources =
endif

exrdisplay_SOURCES = main.cpp ImageView.cpp ImageView.h $(fragshader_sources) \
		     loadImage.cpp loadImage.h scaleImage.cpp scaleImage.h

noinst_HEADERS = ImageView.h ImageViewFragShader.h loadImage.h scaleImage.h

EXTRA_DIST = main.cpp ImageView.cpp ImageView.h ImageViewFragShader.cpp \
	     ImageViewFragShader.h exrdisplay.fp30 exrdisplay.cg README \
	     loadImage.h loadImage.cpp scaleImage.cpp scaleImage.h
