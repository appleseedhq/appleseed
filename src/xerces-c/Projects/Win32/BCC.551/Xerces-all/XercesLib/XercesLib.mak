# ---------------------------------------------------------------------------
!if !$d(BCB)
BCB = $(MAKEDIR)\..
!endif

# ---------------------------------------------------------------------------
TARGETPATH=..\..\..\..\..\Build\Win32\BCC.551
PROJECT = $(TARGETPATH)\xerces-bor_$(XERCESVER).dll
!if $d(WITHDEPRDOM)
DEPRDOM_PATH=..\..\..\..\..\src\xercesc\dom\deprecated
DEPRDOM_DEFINE=;PROJ_DEPRECATED_DOM
DEPRDOM_OBJFILES = \
    $(TARGETPATH)\obj\AttrImpl.obj \
    $(TARGETPATH)\obj\AttrMapImpl.obj \
    $(TARGETPATH)\obj\AttrNSImpl.obj \
    $(TARGETPATH)\obj\CDATASectionImpl.obj \
    $(TARGETPATH)\obj\CharacterDataImpl.obj \
    $(TARGETPATH)\obj\ChildNode.obj \
    $(TARGETPATH)\obj\CommentImpl.obj \
    $(TARGETPATH)\obj\DeepNodeListImpl.obj \
    $(TARGETPATH)\obj\DocumentFragmentImpl.obj \
    $(TARGETPATH)\obj\DocumentImpl.obj \
    $(TARGETPATH)\obj\DocumentTypeImpl.obj \
    $(TARGETPATH)\obj\DomMemDebug.obj \
    $(TARGETPATH)\obj\DOMParser.obj \
    $(TARGETPATH)\obj\DOMString.obj \
    $(TARGETPATH)\obj\DOM_Attr.obj \
    $(TARGETPATH)\obj\DOM_CDATASection.obj \
    $(TARGETPATH)\obj\DOM_CharacterData.obj \
    $(TARGETPATH)\obj\DOM_Comment.obj \
    $(TARGETPATH)\obj\DOM_Document.obj \
    $(TARGETPATH)\obj\DOM_DocumentFragment.obj \
    $(TARGETPATH)\obj\DOM_DocumentType.obj \
    $(TARGETPATH)\obj\DOM_DOMException.obj \
    $(TARGETPATH)\obj\DOM_DOMImplementation.obj \
    $(TARGETPATH)\obj\DOM_Element.obj \
    $(TARGETPATH)\obj\DOM_Entity.obj \
    $(TARGETPATH)\obj\DOM_EntityReference.obj \
    $(TARGETPATH)\obj\DOM_NamedNodeMap.obj \
    $(TARGETPATH)\obj\DOM_Node.obj \
    $(TARGETPATH)\obj\DOM_NodeFilter.obj \
    $(TARGETPATH)\obj\DOM_NodeIterator.obj \
    $(TARGETPATH)\obj\DOM_NodeList.obj \
    $(TARGETPATH)\obj\DOM_Notation.obj \
    $(TARGETPATH)\obj\DOM_ProcessingInstruction.obj \
    $(TARGETPATH)\obj\DOM_Range.obj \
    $(TARGETPATH)\obj\DOM_RangeException.obj \
    $(TARGETPATH)\obj\DOM_Text.obj \
    $(TARGETPATH)\obj\DOM_TreeWalker.obj \
    $(TARGETPATH)\obj\DOM_XMLDecl.obj \
    $(TARGETPATH)\obj\DStringPool.obj \
    $(TARGETPATH)\obj\ElementDefinitionImpl.obj \
    $(TARGETPATH)\obj\ElementImpl.obj \
    $(TARGETPATH)\obj\ElementNSImpl.obj \
    $(TARGETPATH)\obj\EntityImpl.obj \
    $(TARGETPATH)\obj\EntityReferenceImpl.obj \
    $(TARGETPATH)\obj\NamedNodeMapImpl.obj \
    $(TARGETPATH)\obj\NodeIDMap.obj \
    $(TARGETPATH)\obj\NodeImpl.obj \
    $(TARGETPATH)\obj\NodeIteratorImpl.obj \
    $(TARGETPATH)\obj\NodeListImpl.obj \
    $(TARGETPATH)\obj\NodeVector.obj \
    $(TARGETPATH)\obj\NotationImpl.obj \
    $(TARGETPATH)\obj\ParentNode.obj \
    $(TARGETPATH)\obj\ProcessingInstructionImpl.obj \
    $(TARGETPATH)\obj\RangeImpl.obj \
    $(TARGETPATH)\obj\RefCountedImpl.obj \
    $(TARGETPATH)\obj\TextImpl.obj \
    $(TARGETPATH)\obj\TreeWalkerImpl.obj \
    $(TARGETPATH)\obj\XMLDeclImpl.obj
!endif
OBJFILES = $(TARGETPATH)\obj\XercesLib.obj \
    $(TARGETPATH)\obj\Win32PlatformUtils.obj \
    $(TARGETPATH)\obj\InMemMsgLoader.obj \
    $(TARGETPATH)\obj\Win32TransService.obj \
    $(TARGETPATH)\obj\BinHTTPURLInputStream.obj \
    $(TARGETPATH)\obj\WinSockNetAccessor.obj \
    $(TARGETPATH)\obj\ASCIIRangeFactory.obj \
    $(TARGETPATH)\obj\BlockRangeFactory.obj \
    $(TARGETPATH)\obj\BMPattern.obj \
    $(TARGETPATH)\obj\CharToken.obj \
    $(TARGETPATH)\obj\ClosureToken.obj \
    $(TARGETPATH)\obj\ConcatToken.obj \
    $(TARGETPATH)\obj\ConditionToken.obj \
    $(TARGETPATH)\obj\Match.obj \
    $(TARGETPATH)\obj\ModifierToken.obj \
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
    $(TARGETPATH)\obj\HashPtr.obj \
    $(TARGETPATH)\obj\HashXMLCh.obj \
    $(TARGETPATH)\obj\HeaderDummy.obj \
    $(TARGETPATH)\obj\HexBin.obj \
    $(TARGETPATH)\obj\KVStringPair.obj \
    $(TARGETPATH)\obj\Mutexes.obj \
    $(TARGETPATH)\obj\PlatformUtils.obj \
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
    $(TARGETPATH)\obj\XMLUCSTranscoder.obj \
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
    $(TARGETPATH)\obj\DOMImplementationRegistry.obj \
    $(TARGETPATH)\obj\DOMBuilderImpl.obj \
    $(TARGETPATH)\obj\DOMWriterImpl.obj \
    $(TARGETPATH)\obj\Wrapper4InputSource.obj \
    $(TARGETPATH)\obj\Wrapper4DOMInputSource.obj \
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
    $(TARGETPATH)\obj\MemoryManagerArrayImpl.obj \
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
    $(TARGETPATH)\obj\XMLRegisterCleanup.obj \
    $(TARGETPATH)\obj\XMLInitializer.obj \
    $(DEPRDOM_OBJFILES)
RESFILES = $(TARGETPATH)\obj\Version.res
MAINSOURCE = XercesLib.cpp
RESDEPEN = $(RESFILES)
LIBFILES =
IDLFILES =
IDLGENFILES =
LIBRARIES =
SPARELIBS =
DEFFILE =
# ---------------------------------------------------------------------------
PATHCPP = .;..\..\..\..\..\src\xercesc\dom;..\..\..\..\..\src\xercesc\dom\impl;..\..\..\..\..\src\xercesc\framework;..\..\..\..\..\src\xercesc\framework\psvi;..\..\..\..\..\src\xercesc\internal;..\..\..\..\..\src\xercesc\parsers;..\..\..\..\..\src\xercesc\sax;..\..\..\..\..\src\xercesc\sax2;..\..\..\..\..\src\xercesc\util;..\..\..\..\..\src\xercesc\util\MsgLoaders\InMemory;..\..\..\..\..\src\xercesc\util\NetAccessors\WinSock;..\..\..\..\..\src\xercesc\util\Platforms\Win32;..\..\..\..\..\src\xercesc\util\regx;..\..\..\..\..\src\xercesc\util\Transcoders\Win32;..\..\..\..\..\src\xercesc\validators\common;..\..\..\..\..\src\xercesc\validators\datatype;..\..\..\..\..\src\xercesc\validators\DTD;..\..\..\..\..\src\xercesc\validators\schema;..\..\..\..\..\src\xercesc\validators\schema\identity;$(DEPRDOM_PATH)
PATHASM = .;
PATHPAS = .;
PATHRC = .;..\..\..\..\..\src\xercesc\util\Platforms\Win32
!if !$d(WITHASM)
XERCES_NO_ASM=;XERCES_NO_ASM
!endif
USERDEFINES = _DEBUG;PLATFORM_WIN32;_CRTDBG_MAP_ALLOC;PROJ_XMLPARSER;PROJ_XMLUTIL;PROJ_PARSERS;PROJ_SAX4C;PROJ_SAX2;PROJ_DOM;PROJ_VALIDATORS;XML_SINGLEDLL;XML_USE_WIN32_TRANSCODER;XML_USE_INMEM_MESSAGELOADER;XML_USE_NETACCESSOR_WINSOCK$(DEPRDOM_DEFINE)$(XERCES_NO_ASM)
SYSDEFINES = NO_STRICT;_NO_VCL;_RTLDLL
INCLUDEPATH = ..\..\..\..\..\src;..\..\..\..\..\src\xercesc\dom;..\..\..\..\..\src\xercesc\framework;..\..\..\..\..\src\xercesc\framework\psvi;..\..\..\..\..\src\xercesc\internal;..\..\..\..\..\src\xercesc\parsers;..\..\..\..\..\src\xercesc\sax;..\..\..\..\..\src\xercesc\sax2;..\..\..\..\..\src\xercesc\util;..\..\..\..\..\src\xercesc\util\MsgLoaders\Win32;..\..\..\..\..\src\xercesc\util\NetAccessors\WinSock;..\..\..\..\..\src\xercesc\util\Platforms\Win32;..\..\..\..\..\src\xercesc\util\regx;..\..\..\..\..\src\xercesc\util\Transcoders\Win32;..\..\..\..\..\src\xercesc\validators\common;..\..\..\..\..\src\xercesc\validators\datatype;..\..\..\..\..\src\xercesc\validators\DTD;..\..\..\..\..\src\xercesc\validators\schema;..\..\..\..\..\src\xercesc\validators\schema\identity;$(DEPRDOM_PATH)
LIBPATH = ..\..\..\..\..\src\xercesc\dom;..\..\..\..\..\src\xercesc\framework;..\..\..\..\..\src\xercesc\internal;..\..\..\..\..\src\xercesc\parsers;..\..\..\..\..\src\xercesc\sax;..\..\..\..\..\src\xercesc\sax2;..\..\..\..\..\src\xercesc\util;..\..\..\..\..\src\xercesc\util\MsgLoaders\Win32;..\..\..\..\..\src\xercesc\util\NetAccessors\WinSock;..\..\..\..\..\src\xercesc\util\Platforms\Win32;..\..\..\..\..\src\xercesc\util\regx;..\..\..\..\..\src\xercesc\util\Transcoders\Win32;..\..\..\..\..\src\xercesc\validators\common;..\..\..\..\..\src\xercesc\validators\datatype;..\..\..\..\..\src\xercesc\validators\DTD;..\..\..\..\..\src\xercesc\validators\schema;..\..\..\..\..\src\xercesc\validators\schema\identity;$(DEPRDOM_PATH)
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




