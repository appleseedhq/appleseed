# ---------------------------------------------------------------------------
!if !$d(BCB)
BCB = $(MAKEDIR)\..
!endif

# ---------------------------------------------------------------------------
TARGETPATH=..\..\..\..\..\Build\Win32\BCC5
PROJECT = $(TARGETPATH)\xerces-bor_$(XERCESVER).dll
OBJFILES = $(TARGETPATH)\obj\XercesLib.obj \
    $(TARGETPATH)\obj\InMemMsgLoader.obj \
    $(TARGETPATH)\obj\Win32TransService.obj \
    $(TARGETPATH)\obj\BinHTTPURLInputStream.obj \
    $(TARGETPATH)\obj\WinSockNetAccessor.obj \
    $(TARGETPATH)\obj\BinHTTPInputStreamCommon.obj \
    $(TARGETPATH)\obj\ASCIIRangeFactory.obj \
    $(TARGETPATH)\obj\BlockRangeFactory.obj \
    $(TARGETPATH)\obj\BMPattern.obj \
    $(TARGETPATH)\obj\CharToken.obj \
    $(TARGETPATH)\obj\ClosureToken.obj \
    $(TARGETPATH)\obj\ConcatToken.obj \
    $(TARGETPATH)\obj\Match.obj \
    $(TARGETPATH)\obj\Op.obj \
    $(TARGETPATH)\obj\OpFactory.obj \
    $(TARGETPATH)\obj\ParenToken.obj \
    $(TARGETPATH)\obj\ParserForXMLSchema.obj \
    $(TARGETPATH)\obj\RangeFactory.obj \
    $(TARGETPATH)\obj\RangeToken.obj \
    $(TARGETPATH)\obj\RangeTokenMap.obj \
    $(TARGETPATH)\obj\RegularExpression.obj \
    $(TARGETPATH)\obj\RegxParser.obj \
    $(TARGETPATH)\obj\RegxUtil.obj \
    $(TARGETPATH)\obj\StringToken.obj \
    $(TARGETPATH)\obj\Token.obj \
    $(TARGETPATH)\obj\TokenFactory.obj \
    $(TARGETPATH)\obj\UnicodeRangeFactory.obj \
    $(TARGETPATH)\obj\UnionToken.obj \
    $(TARGETPATH)\obj\XMLRangeFactory.obj \
    $(TARGETPATH)\obj\XMLUniCharacter.obj \
    $(TARGETPATH)\obj\Base64.obj \
    $(TARGETPATH)\obj\BinFileInputStream.obj \
    $(TARGETPATH)\obj\BinInputStream.obj \
    $(TARGETPATH)\obj\BinMemInputStream.obj \
    $(TARGETPATH)\obj\BitSet.obj \
    $(TARGETPATH)\obj\HeaderDummy.obj \
    $(TARGETPATH)\obj\HexBin.obj \
    $(TARGETPATH)\obj\KVStringPair.obj \
    $(TARGETPATH)\obj\Mutexes.obj \
    $(TARGETPATH)\obj\PlatformUtils.obj \
    $(TARGETPATH)\obj\PSVIUni.obj \
    $(TARGETPATH)\obj\QName.obj \
    $(TARGETPATH)\obj\StringPool.obj \
    $(TARGETPATH)\obj\TransService.obj \
    $(TARGETPATH)\obj\XML256TableTranscoder.obj \
    $(TARGETPATH)\obj\XML88591Transcoder.obj \
    $(TARGETPATH)\obj\XMLAbstractDoubleFloat.obj \
    $(TARGETPATH)\obj\XMLASCIITranscoder.obj \
    $(TARGETPATH)\obj\XMLBigDecimal.obj \
    $(TARGETPATH)\obj\XMLBigInteger.obj \
    $(TARGETPATH)\obj\XMLChTranscoder.obj \
    $(TARGETPATH)\obj\XMLDateTime.obj \
    $(TARGETPATH)\obj\XMLDouble.obj \
    $(TARGETPATH)\obj\XMLEBCDICTranscoder.obj \
    $(TARGETPATH)\obj\XMLException.obj \
    $(TARGETPATH)\obj\XMLFloat.obj \
    $(TARGETPATH)\obj\XMLIBM1140Transcoder.obj \
    $(TARGETPATH)\obj\XMLNumber.obj \
    $(TARGETPATH)\obj\XMLString.obj \
    $(TARGETPATH)\obj\XMLStringTokenizer.obj \
    $(TARGETPATH)\obj\XMLUCS4Transcoder.obj \
    $(TARGETPATH)\obj\XMLUni.obj \
    $(TARGETPATH)\obj\XMLUri.obj \
    $(TARGETPATH)\obj\XMLURL.obj \
    $(TARGETPATH)\obj\XMLUTF16Transcoder.obj \
    $(TARGETPATH)\obj\XMLUTF8Transcoder.obj \
    $(TARGETPATH)\obj\XMLWin1252Transcoder.obj \
    $(TARGETPATH)\obj\LocalFileInputSource.obj \
    $(TARGETPATH)\obj\MemBufInputSource.obj \
    $(TARGETPATH)\obj\StdInInputSource.obj \
    $(TARGETPATH)\obj\URLInputSource.obj \
    $(TARGETPATH)\obj\XMLAttDef.obj \
    $(TARGETPATH)\obj\XMLAttr.obj \
    $(TARGETPATH)\obj\XMLBuffer.obj \
    $(TARGETPATH)\obj\XMLBufferMgr.obj \
    $(TARGETPATH)\obj\XMLContentModel.obj \
    $(TARGETPATH)\obj\XMLElementDecl.obj \
    $(TARGETPATH)\obj\XMLEntityDecl.obj \
    $(TARGETPATH)\obj\XMLFormatter.obj \
    $(TARGETPATH)\obj\XMLNotationDecl.obj \
    $(TARGETPATH)\obj\XMLRecognizer.obj \
    $(TARGETPATH)\obj\XMLValidator.obj \
    $(TARGETPATH)\obj\ElemStack.obj \
    $(TARGETPATH)\obj\ReaderMgr.obj \
    $(TARGETPATH)\obj\VecAttributesImpl.obj \
    $(TARGETPATH)\obj\VecAttrListImpl.obj \
    $(TARGETPATH)\obj\XMLReader.obj \
    $(TARGETPATH)\obj\XMLScanner.obj \
    $(TARGETPATH)\obj\SAX2XMLReaderImpl.obj \
    $(TARGETPATH)\obj\SAX2XMLFilterImpl.obj \
    $(TARGETPATH)\obj\SAXParser.obj \
    $(TARGETPATH)\obj\DOMAttrImpl.obj \
    $(TARGETPATH)\obj\DOMAttrMapImpl.obj \
    $(TARGETPATH)\obj\DOMAttrNSImpl.obj \
    $(TARGETPATH)\obj\DOMCDATASectionImpl.obj \
    $(TARGETPATH)\obj\DOMCharacterDataImpl.obj \
    $(TARGETPATH)\obj\DOMChildNode.obj \
    $(TARGETPATH)\obj\DOMCommentImpl.obj \
    $(TARGETPATH)\obj\DOMDeepNodeListImpl.obj \
    $(TARGETPATH)\obj\DOMDocumentFragmentImpl.obj \
    $(TARGETPATH)\obj\DOMDocumentImpl.obj \
    $(TARGETPATH)\obj\DOMDocumentTypeImpl.obj \
    $(TARGETPATH)\obj\DOMStringPool.obj \
    $(TARGETPATH)\obj\DOMElementImpl.obj \
    $(TARGETPATH)\obj\DOMElementNSImpl.obj \
    $(TARGETPATH)\obj\DOMEntityImpl.obj \
    $(TARGETPATH)\obj\DOMEntityReferenceImpl.obj \
    $(TARGETPATH)\obj\DOMNamedNodeMapImpl.obj \
    $(TARGETPATH)\obj\DOMNodeIDMap.obj \
    $(TARGETPATH)\obj\DOMNodeImpl.obj \
    $(TARGETPATH)\obj\DOMNodeIteratorImpl.obj \
    $(TARGETPATH)\obj\DOMNodeListImpl.obj \
    $(TARGETPATH)\obj\DOMNodeVector.obj \
    $(TARGETPATH)\obj\DOMNotationImpl.obj \
    $(TARGETPATH)\obj\DOMParentNode.obj \
    $(TARGETPATH)\obj\DOMProcessingInstructionImpl.obj \
    $(TARGETPATH)\obj\DOMRangeImpl.obj \
    $(TARGETPATH)\obj\DOMTextImpl.obj \
    $(TARGETPATH)\obj\DOMTreeWalkerImpl.obj \
    $(TARGETPATH)\obj\Dummy.obj \
    $(TARGETPATH)\obj\InputSource.obj \
    $(TARGETPATH)\obj\SAXException.obj \
    $(TARGETPATH)\obj\SAXParseException.obj \
    $(TARGETPATH)\obj\sax2Dummy.obj \
    $(TARGETPATH)\obj\AllContentModel.obj \
    $(TARGETPATH)\obj\CMAny.obj \
    $(TARGETPATH)\obj\CMBinaryOp.obj \
    $(TARGETPATH)\obj\CMUnaryOp.obj \
    $(TARGETPATH)\obj\ContentLeafNameTypeVector.obj \
    $(TARGETPATH)\obj\ContentSpecNode.obj \
    $(TARGETPATH)\obj\DFAContentModel.obj \
    $(TARGETPATH)\obj\GrammarResolver.obj \
    $(TARGETPATH)\obj\MixedContentModel.obj \
    $(TARGETPATH)\obj\SimpleContentModel.obj \
    $(TARGETPATH)\obj\AbstractNumericFacetValidator.obj \
    $(TARGETPATH)\obj\AbstractNumericValidator.obj \
    $(TARGETPATH)\obj\AbstractStringValidator.obj \
    $(TARGETPATH)\obj\AnySimpleTypeDatatypeValidator.obj \
    $(TARGETPATH)\obj\AnyURIDatatypeValidator.obj \
    $(TARGETPATH)\obj\Base64BinaryDatatypeValidator.obj \
    $(TARGETPATH)\obj\BooleanDatatypeValidator.obj \
    $(TARGETPATH)\obj\DateDatatypeValidator.obj \
    $(TARGETPATH)\obj\DateTimeValidator.obj \
    $(TARGETPATH)\obj\DateTimeDatatypeValidator.obj \
    $(TARGETPATH)\obj\DatatypeValidator.obj \
    $(TARGETPATH)\obj\DatatypeValidatorFactory.obj \
    $(TARGETPATH)\obj\DayDatatypeValidator.obj \
    $(TARGETPATH)\obj\DecimalDatatypeValidator.obj \
    $(TARGETPATH)\obj\DoubleDatatypeValidator.obj \
    $(TARGETPATH)\obj\DurationDatatypeValidator.obj \
    $(TARGETPATH)\obj\ENTITYDatatypeValidator.obj \
    $(TARGETPATH)\obj\FloatDatatypeValidator.obj \
    $(TARGETPATH)\obj\HexBinaryDatatypeValidator.obj \
    $(TARGETPATH)\obj\IDDatatypeValidator.obj \
    $(TARGETPATH)\obj\IDREFDatatypeValidator.obj \
    $(TARGETPATH)\obj\ListDatatypeValidator.obj \
    $(TARGETPATH)\obj\MonthDatatypeValidator.obj \
    $(TARGETPATH)\obj\MonthDayDatatypeValidator.obj \
    $(TARGETPATH)\obj\NameDatatypeValidator.obj \
    $(TARGETPATH)\obj\NCNameDatatypeValidator.obj \
    $(TARGETPATH)\obj\NOTATIONDatatypeValidator.obj \
    $(TARGETPATH)\obj\QNameDatatypeValidator.obj \
    $(TARGETPATH)\obj\StringDatatypeValidator.obj \
    $(TARGETPATH)\obj\TimeDatatypeValidator.obj \
    $(TARGETPATH)\obj\UnionDatatypeValidator.obj \
    $(TARGETPATH)\obj\YearDatatypeValidator.obj \
    $(TARGETPATH)\obj\YearMonthDatatypeValidator.obj \
    $(TARGETPATH)\obj\DTDAttDef.obj \
    $(TARGETPATH)\obj\DTDAttDefList.obj \
    $(TARGETPATH)\obj\DTDElementDecl.obj \
    $(TARGETPATH)\obj\DTDGrammar.obj \
    $(TARGETPATH)\obj\DTDScanner.obj \
    $(TARGETPATH)\obj\DTDValidator.obj \
    $(TARGETPATH)\obj\ComplexTypeInfo.obj \
    $(TARGETPATH)\obj\GeneralAttributeCheck.obj \
    $(TARGETPATH)\obj\NamespaceScope.obj \
    $(TARGETPATH)\obj\SchemaAttDef.obj \
    $(TARGETPATH)\obj\SchemaAttDefList.obj \
    $(TARGETPATH)\obj\SchemaElementDecl.obj \
    $(TARGETPATH)\obj\SchemaGrammar.obj \
    $(TARGETPATH)\obj\SchemaInfo.obj \
    $(TARGETPATH)\obj\SchemaSymbols.obj \
    $(TARGETPATH)\obj\SchemaValidator.obj \
    $(TARGETPATH)\obj\SubstitutionGroupComparator.obj \
    $(TARGETPATH)\obj\TraverseSchema.obj \
    $(TARGETPATH)\obj\XercesAttGroupInfo.obj \
    $(TARGETPATH)\obj\XercesElementWildcard.obj \
    $(TARGETPATH)\obj\XercesGroupInfo.obj \
    $(TARGETPATH)\obj\XUtil.obj \
    $(TARGETPATH)\obj\FieldActivator.obj \
    $(TARGETPATH)\obj\FieldValueMap.obj \
    $(TARGETPATH)\obj\IC_Field.obj \
    $(TARGETPATH)\obj\IC_Key.obj \
    $(TARGETPATH)\obj\IC_KeyRef.obj \
    $(TARGETPATH)\obj\IC_Selector.obj \
    $(TARGETPATH)\obj\IC_Unique.obj \
    $(TARGETPATH)\obj\IdentityConstraint.obj \
    $(TARGETPATH)\obj\ValueStore.obj \
    $(TARGETPATH)\obj\ValueStoreCache.obj \
    $(TARGETPATH)\obj\XercesXPath.obj \
    $(TARGETPATH)\obj\XPathMatcher.obj \
    $(TARGETPATH)\obj\XPathMatcherStack.obj \
    $(TARGETPATH)\obj\XPathSymbols.obj \
    $(TARGETPATH)\obj\AbstractDOMParser.obj \
    $(TARGETPATH)\obj\XercesDOMParser.obj \
    $(TARGETPATH)\obj\DOMException.obj \
    $(TARGETPATH)\obj\DefaultPanicHandler.obj \
    $(TARGETPATH)\obj\PanicHandler.obj \
    $(TARGETPATH)\obj\EncodingValidator.obj \
    $(TARGETPATH)\obj\XSDErrorReporter.obj \
    $(TARGETPATH)\obj\XSDDOMParser.obj \
    $(TARGETPATH)\obj\XSDElementNSImpl.obj \
    $(TARGETPATH)\obj\XSDLocator.obj \
    $(TARGETPATH)\obj\DOMRangeException.obj \
    $(TARGETPATH)\obj\DOMImplementationImpl.obj \
    $(TARGETPATH)\obj\DOMImplementationListImpl.obj \
    $(TARGETPATH)\obj\DOMImplementationRegistry.obj \
    $(TARGETPATH)\obj\DOMStringListImpl.obj \
    $(TARGETPATH)\obj\DOMLSParserImpl.obj \
    $(TARGETPATH)\obj\DOMLSSerializerImpl.obj \
    $(TARGETPATH)\obj\DOMLSInputImpl.obj \
    $(TARGETPATH)\obj\DOMLSOutputImpl.obj \
    $(TARGETPATH)\obj\DOMLSException.obj \
    $(TARGETPATH)\obj\Wrapper4InputSource.obj \
    $(TARGETPATH)\obj\Wrapper4DOMLSInput.obj \
    $(TARGETPATH)\obj\DOMLocatorImpl.obj \
    $(TARGETPATH)\obj\DOMErrorImpl.obj \
    $(TARGETPATH)\obj\MemBufFormatTarget.obj \
    $(TARGETPATH)\obj\StdOutFormatTarget.obj \
    $(TARGETPATH)\obj\LocalFileFormatTarget.obj \
    $(TARGETPATH)\obj\XMLChar.obj \
    $(TARGETPATH)\obj\XMLMsgLoader.obj \
    $(TARGETPATH)\obj\DGXMLScanner.obj \
    $(TARGETPATH)\obj\IGXMLScanner.obj \
    $(TARGETPATH)\obj\IGXMLScanner2.obj \
    $(TARGETPATH)\obj\SGXMLScanner.obj \
    $(TARGETPATH)\obj\WFXMLScanner.obj \
    $(TARGETPATH)\obj\XMLScannerResolver.obj \
    $(TARGETPATH)\obj\DOMTypeInfoImpl.obj \
    $(TARGETPATH)\obj\DOMConfigurationImpl.obj \
    $(TARGETPATH)\obj\XMemory.obj \
    $(TARGETPATH)\obj\DOMNormalizer.obj \
    $(TARGETPATH)\obj\MemoryManagerImpl.obj \
    $(TARGETPATH)\obj\XMLGrammarPoolImpl.obj \
    $(TARGETPATH)\obj\XMLDTDDescriptionImpl.obj \
    $(TARGETPATH)\obj\XMLSchemaDescriptionImpl.obj \
    $(TARGETPATH)\obj\PSVIAttribute.obj \
    $(TARGETPATH)\obj\PSVIAttributeList.obj \
    $(TARGETPATH)\obj\PSVIElement.obj \
    $(TARGETPATH)\obj\PSVIItem.obj \
    $(TARGETPATH)\obj\XSAnnotation.obj \
    $(TARGETPATH)\obj\XSAttributeDeclaration.obj \
    $(TARGETPATH)\obj\XSAttributeGroupDefinition.obj \
    $(TARGETPATH)\obj\XSAttributeUse.obj \
    $(TARGETPATH)\obj\XSComplexTypeDefinition.obj \
    $(TARGETPATH)\obj\XSElementDeclaration.obj \
    $(TARGETPATH)\obj\XSFacet.obj \
    $(TARGETPATH)\obj\XSIDCDefinition.obj \
    $(TARGETPATH)\obj\XSModel.obj \
    $(TARGETPATH)\obj\XSModelGroup.obj \
    $(TARGETPATH)\obj\XSModelGroupDefinition.obj \
    $(TARGETPATH)\obj\XSMultiValueFacet.obj \
    $(TARGETPATH)\obj\XSNamespaceItem.obj \
    $(TARGETPATH)\obj\XSNotationDeclaration.obj \
    $(TARGETPATH)\obj\XSObject.obj \
    $(TARGETPATH)\obj\XSParticle.obj \
    $(TARGETPATH)\obj\XSSimpleTypeDefinition.obj \
    $(TARGETPATH)\obj\XSTypeDefinition.obj \
    $(TARGETPATH)\obj\XSWildcard.obj \
    $(TARGETPATH)\obj\XProtoType.obj \
    $(TARGETPATH)\obj\XSerializeEngine.obj \
    $(TARGETPATH)\obj\XTemplateSerializer.obj \
    $(TARGETPATH)\obj\ValidationContextImpl.obj \
    $(TARGETPATH)\obj\XMLRefInfo.obj \
    $(TARGETPATH)\obj\XMLAttDefList.obj \
    $(TARGETPATH)\obj\XMLGrammarDescription.obj \
    $(TARGETPATH)\obj\Grammar.obj \
    $(TARGETPATH)\obj\SynchronizedStringPool.obj \
    $(TARGETPATH)\obj\DTDEntityDecl.obj \
    $(TARGETPATH)\obj\XMLDTDDescription.obj \
    $(TARGETPATH)\obj\XMLSchemaDescription.obj \
    $(TARGETPATH)\obj\BinFileOutputStream.obj \
    $(TARGETPATH)\obj\BinOutputStream.obj \
    $(TARGETPATH)\obj\DOMXPathException.obj \
    $(TARGETPATH)\obj\XSObjectFactory.obj \
    $(TARGETPATH)\obj\XMLIBM1047Transcoder.obj \
    $(TARGETPATH)\obj\XMLCanRepGroup.obj \
    $(TARGETPATH)\obj\BinMemOutputStream.obj \
    $(TARGETPATH)\obj\XSValue.obj \
    $(TARGETPATH)\obj\IdentityConstraintHandler.obj \
    $(TARGETPATH)\obj\XSAXMLScanner.obj \
    $(TARGETPATH)\obj\XMLInitializer.obj \
    $(TARGETPATH)\obj\WindowsFileMgr.obj \
    $(TARGETPATH)\obj\WindowsMutexMgr.obj \
    $(TARGETPATH)\obj\DOMXPathExpressionImpl.obj \
    $(TARGETPATH)\obj\DOMXPathNSResolverImpl.obj \
    $(TARGETPATH)\obj\DOMXPathResultImpl.obj \
    $(TARGETPATH)\obj\XIncludeDOMDocumentProcessor.obj \
    $(TARGETPATH)\obj\XIncludeLocation.obj \
    $(TARGETPATH)\obj\XIncludeUtils.obj
RESFILES = 
MAINSOURCE = XercesLib.cpp
RESDEPEN = $(RESFILES)
LIBFILES =
IDLFILES =
IDLGENFILES =
LIBRARIES =
SPARELIBS =
DEFFILE =
# ---------------------------------------------------------------------------
PATHCPP = .;..\..\..\..\..\src\xercesc\dom;..\..\..\..\..\src\xercesc\dom\impl;..\..\..\..\..\src\xercesc\framework;..\..\..\..\..\src\xercesc\framework\psvi;..\..\..\..\..\src\xercesc\internal;..\..\..\..\..\src\xercesc\parsers;..\..\..\..\..\src\xercesc\sax;..\..\..\..\..\src\xercesc\sax2;..\..\..\..\..\src\xercesc\util;..\..\..\..\..\src\xercesc\util\MsgLoaders\InMemory;..\..\..\..\..\src\xercesc\util\NetAccessors;..\..\..\..\..\src\xercesc\util\NetAccessors\WinSock;..\..\..\..\..\src\xercesc\util\FileManagers;..\..\..\..\..\src\xercesc\util\MutexManagers;..\..\..\..\..\src\xercesc\util\regx;..\..\..\..\..\src\xercesc\util\Transcoders\Win32;..\..\..\..\..\src\xercesc\validators\common;..\..\..\..\..\src\xercesc\validators\datatype;..\..\..\..\..\src\xercesc\validators\DTD;..\..\..\..\..\src\xercesc\validators\schema;..\..\..\..\..\src\xercesc\validators\schema\identity;..\..\..\..\..\src\xercesc\xinclude;
PATHASM = .;
PATHPAS = .;
PATHRC = .;..\..\..\..\..\src\xercesc\util\Platforms\Win32
!if !$d(WITHASM)
XERCES_NO_ASM=;XERCES_NO_ASM
!endif
USERDEFINES = _DEBUG;XERCES_BUILDING_LIBRARY;XERCES_USE_FILEMGR_WINDOWS=1;XERCES_USE_MUTEXMGR_WINDOWS=1;XERCES_USE_NETACCESSOR_WINSOCK=1;XERCES_USE_MSGLOADER_INMEMORY=1;XERCES_USE_TRANSCODER_WINDOWS=1;XERCES_PATH_DELIMITER_BACKSLASH=1;HAVE_LIMITS_H=1;HAVE_SYS_TIMEB_H=1;HAVE_FTIME=1;HAVE_STRICMP=1;HAVE_STRNICMP=1;HAVE_WCSUPR=0;HAVE_WCSLWR=0;HAVE_WCSICMP=0;HAVE_WCSNICMP=0;$(XERCES_NO_ASM)
SYSDEFINES = NO_STRICT;_NO_VCL;_RTLDLL
INCLUDEPATH = ..\..\..\..\..\src;..\..\..\..\..\src\xercesc\dom;..\..\..\..\..\src\xercesc\framework;..\..\..\..\..\src\xercesc\framework\psvi;..\..\..\..\..\src\xercesc\internal;..\..\..\..\..\src\xercesc\parsers;..\..\..\..\..\src\xercesc\sax;..\..\..\..\..\src\xercesc\sax2;..\..\..\..\..\src\xercesc\util;..\..\..\..\..\src\xercesc\util\MsgLoaders\Win32;..\..\..\..\..\src\xercesc\util\NetAccessors;..\..\..\..\..\src\xercesc\util\NetAccessors\WinSock;..\..\..\..\..\src\xercesc\util\regx;..\..\..\..\..\src\xercesc\util\Transcoders\Win32;..\..\..\..\..\src\xercesc\validators\common;..\..\..\..\..\src\xercesc\validators\datatype;..\..\..\..\..\src\xercesc\validators\DTD;..\..\..\..\..\src\xercesc\validators\schema;..\..\..\..\..\src\xercesc\validators\schema\identity;..\..\..\..\..\src\xercesc\xinclude;
LIBPATH = ..\..\..\..\..\src\xercesc\dom;..\..\..\..\..\src\xercesc\framework;..\..\..\..\..\src\xercesc\internal;..\..\..\..\..\src\xercesc\parsers;..\..\..\..\..\src\xercesc\sax;..\..\..\..\..\src\xercesc\sax2;..\..\..\..\..\src\xercesc\util;..\..\..\..\..\src\xercesc\util\MsgLoaders\Win32;..\..\..\..\..\src\xercesc\util\NetAccessors;..\..\..\..\..\src\xercesc\util\NetAccessors\WinSock;..\..\..\..\..\src\xercesc\util\regx;..\..\..\..\..\src\xercesc\util\Transcoders\Win32;..\..\..\..\..\src\xercesc\validators\common;..\..\..\..\..\src\xercesc\validators\datatype;..\..\..\..\..\src\xercesc\validators\DTD;..\..\..\..\..\src\xercesc\validators\schema;..\..\..\..\..\src\xercesc\validators\schema\identity;..\..\..\..\..\src\xercesc\xinclude;
WARNINGS= -w-par
# ---------------------------------------------------------------------------
CFLAG1 = -tWD -Od -Vx -Ve -X- -r- -a8 -4 -b -k -y -v -vi- -c -tWM
IDLCFLAGS =
PFLAGS = -N2$(TARGETPATH)\obj \
    -N0$(TARGETPATH)\obj -$YD -$W -$O- -v -JPHNE -M
RFLAGS =
AFLAGS = /mx /w2 /zd
LFLAGS = -l"$(TARGETPATH)\" -I$(TARGETPATH)\obj \
    -D"" -aa -Tpd -x -Gn -Gi -v
# ---------------------------------------------------------------------------
ALLOBJ = c0d32.obj $(OBJFILES)
ALLRES = $(RESFILES)
ALLLIB = $(LIBFILES) $(LIBRARIES) import32.lib cw32mti.lib
# ---------------------------------------------------------------------------





# ---------------------------------------------------------------------------
# MAKE SECTION
# ---------------------------------------------------------------------------
# This section of the project file is not used by the BCB IDE.  It is for
# the benefit of building from the command-line using the MAKE utility.
# ---------------------------------------------------------------------------

.autodepend
# ---------------------------------------------------------------------------
!if "$(USERDEFINES)" != ""
AUSERDEFINES = -d$(USERDEFINES:;= -d)
!else
AUSERDEFINES =
!endif

!if !$d(BCC32)
BCC32 = bcc32
!endif

!if !$d(CPP32)
CPP32 = cpp32
!endif

!if !$d(DCC32)
DCC32 = dcc32
!endif

!if !$d(TASM32)
TASM32 = tasm32
!endif

!if !$d(LINKER)
LINKER = ilink32
!endif

!if !$d(BRCC32)
BRCC32 = brcc32
!endif


# ---------------------------------------------------------------------------
!if $d(PATHCPP)
.PATH.CPP = $(PATHCPP)
.PATH.C   = $(PATHCPP)
!endif

!if $d(PATHPAS)
.PATH.PAS = $(PATHPAS)
!endif

!if $d(PATHASM)
.PATH.ASM = $(PATHASM)
!endif

!if $d(PATHRC)
.PATH.RC  = $(PATHRC)
!endif
# ---------------------------------------------------------------------------
$(PROJECT): $(IDLGENFILES) $(OBJFILES) $(RESDEPEN) $(DEFFILE)
    $(BCB)\BIN\$(LINKER) @&&!
    $(LFLAGS) -L$(LIBPATH) +
    $(ALLOBJ), +
    $(PROJECT),, +
    $(ALLLIB), +
    $(DEFFILE), +
    $(ALLRES)
!
# ---------------------------------------------------------------------------
.pas.hpp:
    $(BCB)\BIN\$(DCC32) $(PFLAGS) -U$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -O$(INCLUDEPATH) --BCB {$< }

.pas.obj:
    $(BCB)\BIN\$(DCC32) $(PFLAGS) -U$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -O$(INCLUDEPATH) --BCB {$< }

.cpp.obj:
    $(BCB)\BIN\$(BCC32) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n$(@D) {$< }

.c.obj:
    $(BCB)\BIN\$(BCC32) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n$(@D) {$< }

.c.i:
    $(BCB)\BIN\$(CPP32) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n. {$< }

.cpp.i:
    $(BCB)\BIN\$(CPP32) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n. {$< }

.asm.obj:
    $(BCB)\BIN\$(TASM32) $(AFLAGS) -i$(INCLUDEPATH:;= -i) $(AUSERDEFINES) -d$(SYSDEFINES:;= -d) $<, $@

.rc.res:
    $(BCB)\BIN\$(BRCC32) $(RFLAGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -fo$@ $<
# ---------------------------------------------------------------------------




