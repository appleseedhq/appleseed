#------------------------------------------------------------------------------
!ifndef ROOT
ROOT = $(MAKEDIR)\..
!endif
#------------------------------------------------------------------------------
DCC = $(ROOT)\bin\dcc32.exe $**
BRCC = $(ROOT)\bin\brcc32.exe $**
#------------------------------------------------------------------------------
default: all
#------------------------------------------------------------------------------
# Rules for building from command prompt

MakeBuildDirs: MakeBuildDirs.bat
  call $**

MAKEN = $(ROOT)\bin\$(MAKE) -$(MAKEFLAGS) -f
PROJECTNAMES = XercesLib DOMCount DOMPrint SAXCount SAXPrint SAX2Count SAX2Print \
  DOMTest DOMMemTest DOMRangeTest DOMTraversal EncodingTest InitTermTest \
  ThreadTest MemHandlerTest XSerializerTest PSVIWriter SCMPrint MemParse Redirect \
  StdInParse PParse EnumVal SEnumVal CreateDOMDocument XSValueTest \
  DOMTypeInfoTest NetAccessorTest

!include ..\..\..\..\version.incl
XERCESVER=$(VER)

buildall: clearall $(PROJECTNAMES)
all: $(PROJECTNAMES)
clearall:
  del /q ..\..\..\..\Build\Win32\BCC5\*.* ..\..\..\..\Build\Win32\BCC5\obj\*.*

XercesLib: XercesLib\XercesLib.mak
  cd $<
  copy ..\..\..\..\..\src\xercesc\util\Xerces_autoconf_config.borland.hpp ..\..\..\..\..\src\xercesc\util\Xerces_autoconf_config.hpp
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER) -DWITHASM=$(WITHASM)
  cd ..

DOMPrint: DOMPrint\DOMPrint.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER)
  cd ..

DOMCount: DOMCount\DOMCount.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER)
  cd ..

SAXCount: SAXCount\SAXCount.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER)
  cd ..

SAXPrint: SAXPrint\SAXPrint.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER)
  cd ..

SAX2Count: SAX2Count\SAX2Count.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER)
  cd ..

SAX2Print: SAX2Print\SAX2Print.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER)
  cd ..

DOMTest: DOMTest\DOMTest.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER)
  cd ..

DOMMemTest: DOMMemTest\DOMMemTest.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER)
  cd ..

DOMRangeTest: DOMRangeTest\DOMRangeTest.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER)
  cd ..

DOMTraversal: DOMTraversal\DOMTraversal.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER)
  cd ..

EncodingTest: EncodingTest\EncodingTest.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER)
  cd ..

InitTermTest: InitTermTest\InitTermTest.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER)
  cd ..

ThreadTest: ThreadTest\ThreadTest.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER)
  cd ..

MemHandlerTest: MemHandlerTest\MemHandlerTest.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER)
  cd ..

XSerializerTest: XSerializerTest\XSerializerTest.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER)
  cd ..

PSVIWriter: PSVIWriter\PSVIWriter.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER)
  cd ..

SCMPrint: SCMPrint\SCMPrint.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER)
  cd ..

MemParse: MemParse\MemParse.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER)
  cd ..

Redirect: Redirect\Redirect.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER)
  cd ..

StdInParse: StdInParse\StdInParse.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER)
  cd ..

PParse: PParse\PParse.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER)
  cd ..

EnumVal: EnumVal\EnumVal.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER)
  cd ..

SEnumVal: SEnumVal\SEnumVal.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER)
  cd ..

CreateDOMDocument: CreateDOMDocument\CreateDOMDocument.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER)
  cd ..

XSValueTest: XSValueTest\XSValueTest.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER)
  cd ..

DOMTypeInfoTest: DOMTypeInfoTest\DOMTypeInfoTest.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER)
  cd ..

NetAccessorTest: NetAccessorTest\NetAccessorTest.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER)
  cd ..


