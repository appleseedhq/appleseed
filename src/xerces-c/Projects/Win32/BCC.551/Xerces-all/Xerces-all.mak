#------------------------------------------------------------------------------
!ifndef ROOT
ROOT = $(MAKEDIR)\..
!endif
#------------------------------------------------------------------------------
MAKE = $(ROOT)\bin\make.exe -$(MAKEFLAGS) -f$**
DCC = $(ROOT)\bin\dcc32.exe $**
BRCC = $(ROOT)\bin\brcc32.exe $**
#------------------------------------------------------------------------------
default: all
#------------------------------------------------------------------------------
# Rules for building from command prompt

MakeBuildDirs: MakeBuildDirs.bat
  call $**

MAKEN = $(ROOT)\bin\make.exe -$(MAKEFLAGS) -f
PROJECTNAMES = XercesLib DOMCount DOMPrint SAXCount SAXPrint SAX2Count SAX2Print \
  DOMTest DOMMemTest DOMRangeTest DOMTraversal EncodingTest InitTermTest \
  ThreadTest MemHandlerTest XSerializerTest PSVIWriter SCMPrint MemParse Redirect \
  StdInParse PParse EnumVal SEnumVal CreateDOMDocument XSValueTest DeprecatedDOMCount \
  DOMTypeInfoTest

!include ..\..\..\..\version.incl
XERCESVER=$(VER)

buildall: clearall $(PROJECTNAMES)
all: $(PROJECTNAMES)
clearall:
  del /q ..\..\..\..\Build\Win32\BCC.551\*.* ..\..\..\..\Build\Win32\BCC.551\obj\*.*

XercesLib: XercesLib\XercesLib.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER) -DWITHDEPRDOM=$(WITHDEPRDOM) -DWITHASM=$(WITHASM)
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
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER) -DWITHDEPRDOM=$(WITHDEPRDOM)
  cd ..

XSerializerTest: XSerializerTest\XSerializerTest.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER) -DWITHDEPRDOM=$(WITHDEPRDOM)
  cd ..

PSVIWriter: PSVIWriter\PSVIWriter.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER) -DWITHDEPRDOM=$(WITHDEPRDOM)
  cd ..

SCMPrint: SCMPrint\SCMPrint.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER) -DWITHDEPRDOM=$(WITHDEPRDOM)
  cd ..

MemParse: MemParse\MemParse.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER) -DWITHDEPRDOM=$(WITHDEPRDOM)
  cd ..

Redirect: Redirect\Redirect.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER) -DWITHDEPRDOM=$(WITHDEPRDOM)
  cd ..

StdInParse: StdInParse\StdInParse.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER) -DWITHDEPRDOM=$(WITHDEPRDOM)
  cd ..

PParse: PParse\PParse.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER) -DWITHDEPRDOM=$(WITHDEPRDOM)
  cd ..

EnumVal: EnumVal\EnumVal.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER) -DWITHDEPRDOM=$(WITHDEPRDOM)
  cd ..

SEnumVal: SEnumVal\SEnumVal.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER) -DWITHDEPRDOM=$(WITHDEPRDOM)
  cd ..

CreateDOMDocument: CreateDOMDocument\CreateDOMDocument.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER) -DWITHDEPRDOM=$(WITHDEPRDOM)
  cd ..

XSValueTest: XSValueTest\XSValueTest.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER) -DWITHDEPRDOM=$(WITHDEPRDOM)
  cd ..

DeprecatedDOMCount: DeprecatedDOMCount\DeprecatedDOMCount.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER) -DWITHDEPRDOM=$(WITHDEPRDOM)
  cd ..

DOMTypeInfoTest: DOMTypeInfoTest\DOMTypeInfoTest.mak
  cd $<
  $(MAKEN) $<.mak -DXERCESVER=$(XERCESVER) -DWITHDEPRDOM=$(WITHDEPRDOM)
  cd ..

