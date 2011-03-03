#
# Licensed to the Apache Software Foundation (ASF) under one or more
# contributor license agreements.  See the NOTICE file distributed with
# this work for additional information regarding copyright ownership.
# The ASF licenses this file to You under the Apache License, Version 2.0
# (the "License"); you may not use this file except in compliance with
# the License.  You may obtain a copy of the License at
# 
#      http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

#
# $Id: Makefile.am 883272 2009-11-23 08:17:49Z borisk $
#

bin_PROGRAMS =                      ${sampleprogs}

LDADD =                             ${top_builddir}/src/libxerces-c.la
# the  -I${top_builddir}/src is needed to pick the xercesc/util/Xerces_autoconf_config.hpp 
# header file when doing out-of-tree builds
AM_CPPFLAGS =                       -I${top_builddir}/src -I${top_srcdir}/src

EXTRA_DIST =                        data/personal.dtd \
                                    data/personal.xml \
                                    data/personal.xsd \
                                    data/personal-schema.xml \
                                    data/redirect.dtd

sampleprogs =

sampleprogs +=                      CreateDOMDocument
CreateDOMDocument_SOURCES =	        src/CreateDOMDocument/CreateDOMDocument.cpp

sampleprogs +=                      DOMCount
DOMCount_SOURCES =                  src/DOMCount/DOMCount.cpp \
                                    src/DOMCount/DOMCount.hpp

sampleprogs +=                      DOMPrint
DOMPrint_SOURCES =                  src/DOMPrint/DOMPrint.cpp \
                                    src/DOMPrint/DOMPrintErrorHandler.cpp \
                                    src/DOMPrint/DOMPrintErrorHandler.hpp \
                                    src/DOMPrint/DOMPrintFilter.cpp \
                                    src/DOMPrint/DOMPrintFilter.hpp \
                                    src/DOMPrint/DOMTreeErrorReporter.cpp \
                                    src/DOMPrint/DOMTreeErrorReporter.hpp

sampleprogs +=                      EnumVal
EnumVal_SOURCES =                   src/EnumVal/EnumVal.cpp

sampleprogs +=                      MemParse
MemParse_SOURCES =                  src/MemParse/MemParse.cpp \
                                    src/MemParse/MemParse.hpp \
                                    src/MemParse/MemParseHandlers.cpp \
                                    src/MemParse/MemParseHandlers.hpp

sampleprogs +=                      PParse
PParse_SOURCES =                    src/PParse/PParse.cpp \
                                    src/PParse/PParse.hpp \
                                    src/PParse/PParseHandlers.cpp \
                                    src/PParse/PParseHandlers.hpp

sampleprogs +=                      PSVIWriter
PSVIWriter_SOURCES =                src/PSVIWriter/PSVIWriter.cpp \
                                    src/PSVIWriter/PSVIWriter.hpp \
                                    src/PSVIWriter/PSVIWriterHandlers.cpp \
                                    src/PSVIWriter/PSVIWriterHandlers.hpp

sampleprogs +=                      Redirect
Redirect_SOURCES =                  src/Redirect/Redirect.cpp \
                                    src/Redirect/Redirect.hpp \
                                    src/Redirect/RedirectHandlers.cpp \
                                    src/Redirect/RedirectHandlers.hpp

sampleprogs +=                      SAX2Count
SAX2Count_SOURCES =                 src/SAX2Count/SAX2Count.cpp \
                                    src/SAX2Count/SAX2Count.hpp \
                                    src/SAX2Count/SAX2CountHandlers.cpp \
                                    src/SAX2Count/SAX2CountHandlers.hpp

sampleprogs +=                      SAX2Print
SAX2Print_SOURCES =                 src/SAX2Print/SAX2FilterHandlers.cpp \
                                    src/SAX2Print/SAX2FilterHandlers.hpp \
                                    src/SAX2Print/SAX2Print.cpp \
                                    src/SAX2Print/SAX2Print.hpp \
                                    src/SAX2Print/SAX2PrintHandlers.cpp \
                                    src/SAX2Print/SAX2PrintHandlers.hpp

sampleprogs +=                      SAXCount
SAXCount_SOURCES =                  src/SAXCount/SAXCount.cpp \
                                    src/SAXCount/SAXCount.hpp \
                                    src/SAXCount/SAXCountHandlers.cpp \
                                    src/SAXCount/SAXCountHandlers.hpp

sampleprogs +=                      SAXPrint
SAXPrint_SOURCES =                  src/SAXPrint/SAXPrint.cpp \
                                    src/SAXPrint/SAXPrint.hpp \
                                    src/SAXPrint/SAXPrintHandlers.cpp \
                                    src/SAXPrint/SAXPrintHandlers.hpp

sampleprogs +=                      SCMPrint
SCMPrint_SOURCES =                  src/SCMPrint/SCMPrint.cpp

sampleprogs +=                      SEnumVal
SEnumVal_SOURCES =                  src/SEnumVal/SEnumVal.cpp

sampleprogs +=                      StdInParse
StdInParse_SOURCES =                src/StdInParse/StdInParse.cpp \
                                    src/StdInParse/StdInParse.hpp \
                                    src/StdInParse/StdInParseHandlers.cpp \
                                    src/StdInParse/StdInParseHandlers.hpp

sampleprogs +=                      XInclude
XInclude_SOURCES =                  src/XInclude/XInclude.cpp \
                                    src/XInclude/XInclude.hpp

#
# Override generated rules to provide prettier make rules
#
# Warning: If Automake changes sufficiently, these rules may need to
#          be regenerated from the (new) default output of Automake
#
if XERCES_PRETTY_MAKE
.cpp.o:
@am__fastdepCXX_TRUE@	@depbase=`echo $@ | sed 's|[^/]*$$|$(DEPDIR)/&|;s|\.o$$||'`; \
@am__fastdepCXX_TRUE@	if $(CXXCOMPILE) -MT $@ -MD -MP -MF "$$depbase.Tpo" -c -o $@ $<; \
@am__fastdepCXX_TRUE@	then $(am__mv) "$$depbase.Tpo" "$$depbase.Po"; else rm -f "$$depbase.Tpo"; exit 1; fi
@AMDEP_TRUE@@am__fastdepCXX_FALSE@	@source='$<' object='$@' libtool=no @AMDEPBACKSLASH@
@AMDEP_TRUE@@am__fastdepCXX_FALSE@	DEPDIR=$(DEPDIR) $(CXXDEPMODE) $(depcomp) @AMDEPBACKSLASH@
@am__fastdepCXX_FALSE@	$(CXXCOMPILE) -c -o $@ $<

.cpp.obj:
@am__fastdepCXX_TRUE@	@depbase=`echo $@ | sed 's|[^/]*$$|$(DEPDIR)/&|;s|\.obj$$||'`; \
@am__fastdepCXX_TRUE@	if $(CXXCOMPILE) -MT $@ -MD -MP -MF "$$depbase.Tpo" -c -o $@ `$(CYGPATH_W) '$<'`; \
@am__fastdepCXX_TRUE@	then $(am__mv) "$$depbase.Tpo" "$$depbase.Po"; else rm -f "$$depbase.Tpo"; exit 1; fi
@AMDEP_TRUE@@am__fastdepCXX_FALSE@	@source='$<' object='$@' libtool=no @AMDEPBACKSLASH@
@AMDEP_TRUE@@am__fastdepCXX_FALSE@	DEPDIR=$(DEPDIR) $(CXXDEPMODE) $(depcomp) @AMDEPBACKSLASH@
@am__fastdepCXX_FALSE@	$(CXXCOMPILE) -c -o $@ `$(CYGPATH_W) '$<'`

.cpp.lo:
@am__fastdepCXX_TRUE@	@depbase=`echo $@ | sed 's|[^/]*$$|$(DEPDIR)/&|;s|\.lo$$||'`; \
@am__fastdepCXX_TRUE@	if $(LTCXXCOMPILE) -MT $@ -MD -MP -MF "$$depbase.Tpo" -c -o $@ $<; \
@am__fastdepCXX_TRUE@	then $(am__mv) "$$depbase.Tpo" "$$depbase.Plo"; else rm -f "$$depbase.Tpo"; exit 1; fi
@AMDEP_TRUE@@am__fastdepCXX_FALSE@	@source='$<' object='$@' libtool=yes @AMDEPBACKSLASH@
@AMDEP_TRUE@@am__fastdepCXX_FALSE@	DEPDIR=$(DEPDIR) $(CXXDEPMODE) $(depcomp) @AMDEPBACKSLASH@
@am__fastdepCXX_FALSE@	$(LTCXXCOMPILE) -c -o $@ $<

PRETTY_MAKE=${top_srcdir}/config/pretty-make
PRETTY_COMPILE=${PRETTY_MAKE} Compiling
endif

CXXCOMPILE = ${PRETTY_COMPILE} \
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) \
	$(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS)
LTCXXCOMPILE = ${PRETTY_COMPILE}  \
	$(LIBTOOL) --quiet --tag=CXX --mode=compile \
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) \
	$(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS)



