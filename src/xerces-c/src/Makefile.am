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

include ${top_srcdir}/version.incl

DEFS += -DXERCES_BUILDING_LIBRARY
SUBDIRS =

#
#
lib_LTLIBRARIES = libxerces-c.la

# Use AM_* variables instead of per-target ones otherwise the
# make pretty-printing won't work.
#
AM_CPPFLAGS =

libxerces_c_la_LDFLAGS = -release ${INTERFACE_VER_D}

# This sets {install}/include as target for the headers
libxerces_c_ladir = ${includedir}


libxerces_c_la_SOURCES = ${util_sources} ${dom_sources} ${domimpl_sources} \
${framework_sources} ${internal_sources} ${parsers_sources} ${sax_sources} \
${sax2_sources} ${validators_sources} ${xinclude_sources}

nobase_libxerces_c_la_HEADERS = ${util_headers} ${dom_headers} \
${domimpl_headers} ${framework_headers} ${internal_headers} \
${parsers_headers} ${sax_headers} ${sax2_headers} ${validators_headers} \
${xinclude_headers}

# Make sure autoconf-generated Xerces_autoconf_config.hpp ends up in a
# proper place when installed.
#
autoconfheadersdir = $(includedir)/xercesc/util
nodist_autoconfheaders_HEADERS = ${top_builddir}/src/xercesc/util/Xerces_autoconf_config.hpp

# Compatibility library.
#
EXTRA_DIST = stricmp.h strnicmp.h towlower.h towupper.h
libxerces_c_la_LIBADD = ${LTLIBOBJS}

# NetAccessors, conditionally built based on settings from configure
#
if XERCES_USE_NETACCESSOR_CURL
libxerces_c_la_SOURCES += ${curl_sources}
nobase_libxerces_c_la_HEADERS += ${curl_headers}
AM_CPPFLAGS += ${CURL_FLAGS}
endif

if XERCES_USE_NETACCESSOR_SOCKET
libxerces_c_la_SOURCES += ${sockets_sources}
nobase_libxerces_c_la_HEADERS += ${sockets_headers}
endif

if XERCES_USE_NETACCESSOR_CFURL
libxerces_c_la_SOURCES += ${cfurl_sources}
nobase_libxerces_c_la_HEADERS += ${cfurl_headers}
endif

if XERCES_USE_NETACCESSOR_WINSOCK
libxerces_c_la_SOURCES += ${winsock_sources}
nobase_libxerces_c_la_HEADERS += ${winsock_headers}
endif


# Transcoders, conditionally built based on settings from configure
#
if XERCES_USE_TRANSCODER_ICU
libxerces_c_la_SOURCES += ${icu_sources}
nobase_libxerces_c_la_HEADERS += ${icu_headers}
AM_CPPFLAGS += ${ICU_FLAGS}
endif

if XERCES_USE_TRANSCODER_GNUICONV
libxerces_c_la_SOURCES += ${gnuiconv_sources}
nobase_libxerces_c_la_HEADERS += ${gnuiconv_headers}
endif

if XERCES_USE_TRANSCODER_ICONV
libxerces_c_la_SOURCES += ${iconv_sources}
nobase_libxerces_c_la_HEADERS += ${iconv_headers}
endif

if XERCES_USE_TRANSCODER_MACOSUNICODECONVERTER
libxerces_c_la_SOURCES += ${macosunicodeconverter_sources}
nobase_libxerces_c_la_HEADERS += ${macosunicodeconverter_headers}
endif

if XERCES_USE_TRANSCODER_WINDOWS
libxerces_c_la_SOURCES += ${wintrans_sources}
nobase_libxerces_c_la_HEADERS += ${wintrans_headers}
endif


# MsgLoaders, conditionally built based on settings from configure
#
if XERCES_USE_MSGLOADER_INMEMORY
libxerces_c_la_SOURCES += ${msginmemory_sources}
nobase_libxerces_c_la_HEADERS += ${msginmemory_headers}
endif

if XERCES_USE_MSGLOADER_ICU
libxerces_c_la_SOURCES += ${msgicu_sources}
nodist_libxerces_c_la_SOURCES = ${msgicu_nodist_sources}
nobase_libxerces_c_la_HEADERS += ${msgicu_headers}
AM_CPPFLAGS += ${ICU_FLAGS}

EXTRA_DIST += xercesc/util/MsgLoaders/ICU/resources/res-file-list.txt \
xercesc/util/MsgLoaders/ICU/resources/root.txt

# Resource shortname
PKGNAME=xercesc_messages_${INTERFACE_VER_U}

CLEANFILES = xercesc_messages.c root_res.c root.res
BUILT_SOURCES = xercesc_messages.c root_res.c
xercesc_messages.c: ${top_srcdir}/src/xercesc/util/MsgLoaders/ICU/resources/res-file-list.txt
	${ICU_SBIN}gencmn --name ${PKGNAME} -S ${PKGNAME}_dat.c -d . ${top_srcdir}/src/xercesc/util/MsgLoaders/ICU/resources/res-file-list.txt
	@mv ${PKGNAME}_dat.c xercesc_messages.c

root_res.c: ${top_srcdir}/src/xercesc/util/MsgLoaders/ICU/resources/root.txt
	${ICU_BIN}genrb -d . ${top_srcdir}/src/xercesc/util/MsgLoaders/ICU/resources/root.txt
	${ICU_SBIN}genccode --name ${PKGNAME} -d . root.res

endif

if XERCES_USE_MSGLOADER_ICONV
libxerces_c_la_SOURCES += ${msgiconv_sources}
nobase_libxerces_c_la_HEADERS += ${msgiconv_headers}

SUBDIRS += xercesc/util/MsgLoaders/MsgCatalog
endif


# Mutex Managers, conditionally built based on settings from configure
#
if XERCES_USE_MUTEXMGR_NOTHREAD
libxerces_c_la_SOURCES += ${nothreadmmgr_sources}
nobase_libxerces_c_la_HEADERS += ${nothreadmmgr_headers}
endif

if XERCES_USE_MUTEXMGR_POSIX
libxerces_c_la_SOURCES += ${posixmmgr_sources}
nobase_libxerces_c_la_HEADERS += ${posixmmgr_headers}
endif

if XERCES_USE_MUTEXMGR_WINDOWS
libxerces_c_la_SOURCES += ${winmmgr_sources}
nobase_libxerces_c_la_HEADERS += ${winmmgr_headers}
endif


# File Managers, conditionally built based on settings from configure
#
if XERCES_USE_FILEMGR_POSIX
libxerces_c_la_SOURCES += ${posixfmgr_sources}
nobase_libxerces_c_la_HEADERS += ${posixfmgr_headers}
endif

if XERCES_USE_FILEMGR_WINDOWS
libxerces_c_la_SOURCES += ${winfmgr_sources}
nobase_libxerces_c_la_HEADERS += ${winfmgr_headers}
endif

#
# Definition of sources/headers for each convenience library
#

#
#	A line like the following will generate a list of headers
#	for a given directory: (must be issued from src directory)
#		find xercesc/framework -name '*.hpp' -exec echo {} \\ ";"
#		find xercesc/framework -name '*.cpp' -exec echo {} \\ ";"
#

dom_headers	= \
	xercesc/dom/DOM.hpp \
	xercesc/dom/DOMAttr.hpp \
	xercesc/dom/DOMCDATASection.hpp \
	xercesc/dom/DOMCharacterData.hpp \
	xercesc/dom/DOMComment.hpp \
	xercesc/dom/DOMConfiguration.hpp \
	xercesc/dom/DOMDocument.hpp \
	xercesc/dom/DOMDocumentFragment.hpp \
	xercesc/dom/DOMDocumentRange.hpp \
	xercesc/dom/DOMDocumentTraversal.hpp \
	xercesc/dom/DOMDocumentType.hpp \
	xercesc/dom/DOMElement.hpp \
	xercesc/dom/DOMEntity.hpp \
	xercesc/dom/DOMEntityReference.hpp \
	xercesc/dom/DOMError.hpp \
	xercesc/dom/DOMErrorHandler.hpp \
	xercesc/dom/DOMException.hpp \
	xercesc/dom/DOMImplementation.hpp \
	xercesc/dom/DOMImplementationList.hpp \
	xercesc/dom/DOMImplementationLS.hpp \
	xercesc/dom/DOMImplementationRegistry.hpp \
	xercesc/dom/DOMImplementationSource.hpp \
	xercesc/dom/DOMLocator.hpp \
	xercesc/dom/DOMLSException.hpp \
	xercesc/dom/DOMLSInput.hpp \
	xercesc/dom/DOMLSOutput.hpp \
	xercesc/dom/DOMLSParser.hpp \
	xercesc/dom/DOMLSParserFilter.hpp \
	xercesc/dom/DOMLSResourceResolver.hpp \
	xercesc/dom/DOMLSSerializer.hpp \
	xercesc/dom/DOMLSSerializerFilter.hpp \
	xercesc/dom/DOMMemoryManager.hpp \
	xercesc/dom/DOMNamedNodeMap.hpp \
	xercesc/dom/DOMNode.hpp \
	xercesc/dom/DOMNodeFilter.hpp \
	xercesc/dom/DOMNodeIterator.hpp \
	xercesc/dom/DOMNodeList.hpp \
	xercesc/dom/DOMNotation.hpp \
	xercesc/dom/DOMProcessingInstruction.hpp \
	xercesc/dom/DOMPSVITypeInfo.hpp \
	xercesc/dom/DOMRange.hpp \
	xercesc/dom/DOMRangeException.hpp \
	xercesc/dom/DOMStringList.hpp \
	xercesc/dom/DOMText.hpp \
	xercesc/dom/DOMTreeWalker.hpp \
	xercesc/dom/DOMTypeInfo.hpp \
	xercesc/dom/DOMUserDataHandler.hpp \
	xercesc/dom/DOMXPathEvaluator.hpp \
	xercesc/dom/DOMXPathException.hpp \
	xercesc/dom/DOMXPathExpression.hpp \
	xercesc/dom/DOMXPathNamespace.hpp \
	xercesc/dom/DOMXPathNSResolver.hpp \
	xercesc/dom/DOMXPathResult.hpp \
	xercesc/dom/StDOMNode.hpp

dom_sources	= \
	xercesc/dom/DOMException.cpp \
	xercesc/dom/DOMLSException.cpp \
	xercesc/dom/DOMRangeException.cpp \
	xercesc/dom/DOMXPathException.cpp


domimpl_headers	= \
	xercesc/dom/impl/DOMAttrImpl.hpp \
	xercesc/dom/impl/DOMAttrMapImpl.hpp \
	xercesc/dom/impl/DOMAttrNSImpl.hpp \
	xercesc/dom/impl/DOMCasts.hpp \
	xercesc/dom/impl/DOMCDATASectionImpl.hpp \
	xercesc/dom/impl/DOMCharacterDataImpl.hpp \
	xercesc/dom/impl/DOMChildNode.hpp \
	xercesc/dom/impl/DOMCommentImpl.hpp \
	xercesc/dom/impl/DOMConfigurationImpl.hpp \
	xercesc/dom/impl/DOMDeepNodeListImpl.hpp \
	xercesc/dom/impl/DOMDeepNodeListPool.hpp \
	xercesc/dom/impl/DOMDeepNodeListPool.c \
	xercesc/dom/impl/DOMDocumentFragmentImpl.hpp \
	xercesc/dom/impl/DOMDocumentImpl.hpp \
	xercesc/dom/impl/DOMDocumentTypeImpl.hpp \
	xercesc/dom/impl/DOMElementImpl.hpp \
	xercesc/dom/impl/DOMElementNSImpl.hpp \
	xercesc/dom/impl/DOMEntityImpl.hpp \
	xercesc/dom/impl/DOMEntityReferenceImpl.hpp \
	xercesc/dom/impl/DOMErrorImpl.hpp \
	xercesc/dom/impl/DOMImplementationImpl.hpp \
	xercesc/dom/impl/DOMImplementationListImpl.hpp \
	xercesc/dom/impl/DOMLocatorImpl.hpp \
	xercesc/dom/impl/DOMNamedNodeMapImpl.hpp \
	xercesc/dom/impl/DOMNodeIDMap.hpp \
	xercesc/dom/impl/DOMNodeImpl.hpp \
	xercesc/dom/impl/DOMNodeIteratorImpl.hpp \
	xercesc/dom/impl/DOMNodeListImpl.hpp \
	xercesc/dom/impl/DOMNodeVector.hpp \
	xercesc/dom/impl/DOMNormalizer.hpp \
	xercesc/dom/impl/DOMNotationImpl.hpp \
	xercesc/dom/impl/DOMParentNode.hpp \
	xercesc/dom/impl/DOMProcessingInstructionImpl.hpp \
	xercesc/dom/impl/DOMRangeImpl.hpp \
	xercesc/dom/impl/DOMStringListImpl.hpp \
	xercesc/dom/impl/DOMStringPool.hpp \
	xercesc/dom/impl/DOMTextImpl.hpp \
	xercesc/dom/impl/DOMTreeWalkerImpl.hpp \
	xercesc/dom/impl/DOMTypeInfoImpl.hpp \
	xercesc/dom/impl/DOMLSSerializerImpl.hpp \
	xercesc/dom/impl/DOMLSInputImpl.hpp \
	xercesc/dom/impl/DOMLSOutputImpl.hpp \
	xercesc/dom/impl/DOMXPathExpressionImpl.hpp \
	xercesc/dom/impl/DOMXPathNSResolverImpl.hpp \
	xercesc/dom/impl/DOMXPathResultImpl.hpp \
	xercesc/dom/impl/XSDElementNSImpl.hpp

domimpl_sources	= \
	xercesc/dom/impl/DOMAttrImpl.cpp \
	xercesc/dom/impl/DOMAttrMapImpl.cpp \
	xercesc/dom/impl/DOMAttrNSImpl.cpp \
	xercesc/dom/impl/DOMCDATASectionImpl.cpp \
	xercesc/dom/impl/DOMCharacterDataImpl.cpp \
	xercesc/dom/impl/DOMChildNode.cpp \
	xercesc/dom/impl/DOMCommentImpl.cpp \
	xercesc/dom/impl/DOMConfigurationImpl.cpp \
	xercesc/dom/impl/DOMDeepNodeListImpl.cpp \
	xercesc/dom/impl/DOMDocumentFragmentImpl.cpp \
	xercesc/dom/impl/DOMDocumentImpl.cpp \
	xercesc/dom/impl/DOMDocumentTypeImpl.cpp \
	xercesc/dom/impl/DOMElementImpl.cpp \
	xercesc/dom/impl/DOMElementNSImpl.cpp \
	xercesc/dom/impl/DOMEntityImpl.cpp \
	xercesc/dom/impl/DOMEntityReferenceImpl.cpp \
	xercesc/dom/impl/DOMErrorImpl.cpp \
	xercesc/dom/impl/DOMImplementationImpl.cpp \
	xercesc/dom/impl/DOMImplementationListImpl.cpp \
	xercesc/dom/impl/DOMImplementationRegistry.cpp \
	xercesc/dom/impl/DOMLocatorImpl.cpp \
	xercesc/dom/impl/DOMNamedNodeMapImpl.cpp \
	xercesc/dom/impl/DOMNodeIDMap.cpp \
	xercesc/dom/impl/DOMNodeImpl.cpp \
	xercesc/dom/impl/DOMNodeIteratorImpl.cpp \
	xercesc/dom/impl/DOMNodeListImpl.cpp \
	xercesc/dom/impl/DOMNodeVector.cpp \
	xercesc/dom/impl/DOMNormalizer.cpp \
	xercesc/dom/impl/DOMNotationImpl.cpp \
	xercesc/dom/impl/DOMParentNode.cpp \
	xercesc/dom/impl/DOMProcessingInstructionImpl.cpp \
	xercesc/dom/impl/DOMRangeImpl.cpp \
	xercesc/dom/impl/DOMStringListImpl.cpp \
	xercesc/dom/impl/DOMStringPool.cpp \
	xercesc/dom/impl/DOMTextImpl.cpp \
	xercesc/dom/impl/DOMTreeWalkerImpl.cpp \
	xercesc/dom/impl/DOMTypeInfoImpl.cpp \
	xercesc/dom/impl/DOMLSSerializerImpl.cpp \
	xercesc/dom/impl/DOMLSInputImpl.cpp \
	xercesc/dom/impl/DOMLSOutputImpl.cpp \
	xercesc/dom/impl/DOMXPathExpressionImpl.cpp \
	xercesc/dom/impl/DOMXPathNSResolverImpl.cpp \
	xercesc/dom/impl/DOMXPathResultImpl.cpp \
	xercesc/dom/impl/XSDElementNSImpl.cpp


framework_headers = \
	xercesc/framework/BinOutputStream.hpp \
	xercesc/framework/LocalFileFormatTarget.hpp \
	xercesc/framework/LocalFileInputSource.hpp \
	xercesc/framework/MemBufFormatTarget.hpp \
	xercesc/framework/MemBufInputSource.hpp \
	xercesc/framework/MemoryManager.hpp \
	xercesc/framework/psvi/PSVIAttribute.hpp \
	xercesc/framework/psvi/PSVIAttributeList.hpp \
	xercesc/framework/psvi/PSVIElement.hpp \
	xercesc/framework/psvi/PSVIHandler.hpp \
	xercesc/framework/psvi/PSVIItem.hpp \
	xercesc/framework/psvi/XSAnnotation.hpp \
	xercesc/framework/psvi/XSAttributeDeclaration.hpp \
	xercesc/framework/psvi/XSAttributeGroupDefinition.hpp \
	xercesc/framework/psvi/XSAttributeUse.hpp \
	xercesc/framework/psvi/XSComplexTypeDefinition.hpp \
	xercesc/framework/psvi/XSConstants.hpp \
	xercesc/framework/psvi/XSElementDeclaration.hpp \
	xercesc/framework/psvi/XSFacet.hpp \
	xercesc/framework/psvi/XSIDCDefinition.hpp \
	xercesc/framework/psvi/XSModel.hpp \
	xercesc/framework/psvi/XSModelGroup.hpp \
	xercesc/framework/psvi/XSModelGroupDefinition.hpp \
	xercesc/framework/psvi/XSMultiValueFacet.hpp \
	xercesc/framework/psvi/XSNamedMap.hpp \
	xercesc/framework/psvi/XSNamedMap.c \
	xercesc/framework/psvi/XSNamespaceItem.hpp \
	xercesc/framework/psvi/XSNotationDeclaration.hpp \
	xercesc/framework/psvi/XSObject.hpp \
	xercesc/framework/psvi/XSParticle.hpp \
	xercesc/framework/psvi/XSSimpleTypeDefinition.hpp \
	xercesc/framework/psvi/XSTypeDefinition.hpp \
	xercesc/framework/psvi/XSValue.hpp \
	xercesc/framework/psvi/XSWildcard.hpp \
	xercesc/framework/StdInInputSource.hpp \
	xercesc/framework/StdOutFormatTarget.hpp \
	xercesc/framework/URLInputSource.hpp \
	xercesc/framework/ValidationContext.hpp \
	xercesc/framework/Wrapper4DOMLSInput.hpp \
	xercesc/framework/Wrapper4InputSource.hpp \
	xercesc/framework/XMLAttDef.hpp \
	xercesc/framework/XMLAttDefList.hpp \
	xercesc/framework/XMLAttr.hpp \
	xercesc/framework/XMLBuffer.hpp \
	xercesc/framework/XMLBufferMgr.hpp \
	xercesc/framework/XMLContentModel.hpp \
	xercesc/framework/XMLDocumentHandler.hpp \
	xercesc/framework/XMLDTDDescription.hpp \
	xercesc/framework/XMLElementDecl.hpp \
	xercesc/framework/XMLEntityDecl.hpp \
	xercesc/framework/XMLEntityHandler.hpp \
	xercesc/framework/XMLErrorCodes.hpp \
	xercesc/framework/XMLErrorReporter.hpp \
	xercesc/framework/XMLFormatter.hpp \
	xercesc/framework/XMLGrammarDescription.hpp \
	xercesc/framework/XMLGrammarPool.hpp \
	xercesc/framework/XMLGrammarPoolImpl.hpp \
	xercesc/framework/XMLNotationDecl.hpp \
	xercesc/framework/XMLPScanToken.hpp \
	xercesc/framework/XMLRecognizer.hpp \
	xercesc/framework/XMLRefInfo.hpp \
	xercesc/framework/XMLSchemaDescription.hpp \
	xercesc/framework/XMLValidator.hpp \
	xercesc/framework/XMLValidityCodes.hpp

framework_sources = \
	xercesc/framework/BinOutputStream.cpp \
	xercesc/framework/LocalFileFormatTarget.cpp \
	xercesc/framework/LocalFileInputSource.cpp \
	xercesc/framework/MemBufFormatTarget.cpp \
	xercesc/framework/MemBufInputSource.cpp \
	xercesc/framework/psvi/PSVIAttribute.cpp \
	xercesc/framework/psvi/PSVIAttributeList.cpp \
	xercesc/framework/psvi/PSVIElement.cpp \
	xercesc/framework/psvi/PSVIItem.cpp \
	xercesc/framework/psvi/XSAnnotation.cpp \
	xercesc/framework/psvi/XSAttributeDeclaration.cpp \
	xercesc/framework/psvi/XSAttributeGroupDefinition.cpp \
	xercesc/framework/psvi/XSAttributeUse.cpp \
	xercesc/framework/psvi/XSComplexTypeDefinition.cpp \
	xercesc/framework/psvi/XSElementDeclaration.cpp \
	xercesc/framework/psvi/XSFacet.cpp \
	xercesc/framework/psvi/XSIDCDefinition.cpp \
	xercesc/framework/psvi/XSModel.cpp \
	xercesc/framework/psvi/XSModelGroup.cpp \
	xercesc/framework/psvi/XSModelGroupDefinition.cpp \
	xercesc/framework/psvi/XSMultiValueFacet.cpp \
	xercesc/framework/psvi/XSNamespaceItem.cpp \
	xercesc/framework/psvi/XSNotationDeclaration.cpp \
	xercesc/framework/psvi/XSObject.cpp \
	xercesc/framework/psvi/XSParticle.cpp \
	xercesc/framework/psvi/XSSimpleTypeDefinition.cpp \
	xercesc/framework/psvi/XSTypeDefinition.cpp \
	xercesc/framework/psvi/XSValue.cpp \
	xercesc/framework/psvi/XSWildcard.cpp \
	xercesc/framework/StdInInputSource.cpp \
	xercesc/framework/StdOutFormatTarget.cpp \
	xercesc/framework/URLInputSource.cpp \
	xercesc/framework/Wrapper4DOMLSInput.cpp \
	xercesc/framework/Wrapper4InputSource.cpp \
	xercesc/framework/XMLAttDef.cpp \
	xercesc/framework/XMLAttDefList.cpp \
	xercesc/framework/XMLAttr.cpp \
	xercesc/framework/XMLBuffer.cpp \
	xercesc/framework/XMLBufferMgr.cpp \
	xercesc/framework/XMLContentModel.cpp \
	xercesc/framework/XMLDTDDescription.cpp \
	xercesc/framework/XMLElementDecl.cpp \
	xercesc/framework/XMLEntityDecl.cpp \
	xercesc/framework/XMLFormatter.cpp \
	xercesc/framework/XMLGrammarDescription.cpp \
	xercesc/framework/XMLGrammarPoolImpl.cpp \
	xercesc/framework/XMLNotationDecl.cpp \
	xercesc/framework/XMLRecognizer.cpp \
	xercesc/framework/XMLRefInfo.cpp \
	xercesc/framework/XMLSchemaDescription.cpp \
	xercesc/framework/XMLValidator.cpp


internal_headers = \
	xercesc/internal/BinFileOutputStream.hpp \
	xercesc/internal/BinMemOutputStream.hpp \
	xercesc/internal/CharTypeTables.hpp \
	xercesc/internal/DGXMLScanner.hpp \
	xercesc/internal/ElemStack.hpp \
	xercesc/internal/EndOfEntityException.hpp \
	xercesc/internal/IANAEncodings.hpp \
	xercesc/internal/IGXMLScanner.hpp \
	xercesc/internal/MemoryManagerImpl.hpp \
	xercesc/internal/ReaderMgr.hpp \
	xercesc/internal/SGXMLScanner.hpp \
	xercesc/internal/ValidationContextImpl.hpp \
	xercesc/internal/VecAttributesImpl.hpp \
	xercesc/internal/VecAttrListImpl.hpp \
	xercesc/internal/WFXMLScanner.hpp \
	xercesc/internal/XMLInternalErrorHandler.hpp \
	xercesc/internal/XMLReader.hpp \
	xercesc/internal/XMLScanner.hpp \
	xercesc/internal/XMLScannerResolver.hpp \
	xercesc/internal/XProtoType.hpp \
	xercesc/internal/XSAXMLScanner.hpp \
	xercesc/internal/XSerializable.hpp \
	xercesc/internal/XSerializationException.hpp \
	xercesc/internal/XSerializeEngine.hpp \
	xercesc/internal/XSObjectFactory.hpp \
	xercesc/internal/XTemplateSerializer.hpp

internal_sources = \
	xercesc/internal/BinFileOutputStream.cpp \
	xercesc/internal/BinMemOutputStream.cpp \
	xercesc/internal/DGXMLScanner.cpp \
	xercesc/internal/ElemStack.cpp \
	xercesc/internal/IGXMLScanner.cpp \
	xercesc/internal/IGXMLScanner2.cpp \
	xercesc/internal/MemoryManagerImpl.cpp \
	xercesc/internal/ReaderMgr.cpp \
	xercesc/internal/SGXMLScanner.cpp \
	xercesc/internal/ValidationContextImpl.cpp \
	xercesc/internal/VecAttributesImpl.cpp \
	xercesc/internal/VecAttrListImpl.cpp \
	xercesc/internal/WFXMLScanner.cpp \
	xercesc/internal/XMLReader.cpp \
	xercesc/internal/XMLScanner.cpp \
	xercesc/internal/XMLScannerResolver.cpp \
	xercesc/internal/XProtoType.cpp \
	xercesc/internal/XSAXMLScanner.cpp \
	xercesc/internal/XSerializeEngine.cpp \
	xercesc/internal/XSObjectFactory.cpp \
	xercesc/internal/XTemplateSerializer.cpp


parsers_headers = \
	xercesc/parsers/AbstractDOMParser.hpp \
	xercesc/parsers/DOMLSParserImpl.hpp \
	xercesc/parsers/SAX2XMLFilterImpl.hpp \
	xercesc/parsers/SAX2XMLReaderImpl.hpp \
	xercesc/parsers/SAXParser.hpp \
	xercesc/parsers/XercesDOMParser.hpp

parsers_sources = \
	xercesc/parsers/AbstractDOMParser.cpp \
	xercesc/parsers/DOMLSParserImpl.cpp \
	xercesc/parsers/SAX2XMLFilterImpl.cpp \
	xercesc/parsers/SAX2XMLReaderImpl.cpp \
	xercesc/parsers/SAXParser.cpp \
	xercesc/parsers/XercesDOMParser.cpp


sax_headers = \
	xercesc/sax/AttributeList.hpp \
	xercesc/sax/DocumentHandler.hpp \
	xercesc/sax/DTDHandler.hpp \
	xercesc/sax/EntityResolver.hpp \
	xercesc/sax/ErrorHandler.hpp \
	xercesc/sax/HandlerBase.hpp \
	xercesc/sax/InputSource.hpp \
	xercesc/sax/Locator.hpp \
	xercesc/sax/Parser.hpp \
	xercesc/sax/SAXException.hpp \
	xercesc/sax/SAXParseException.hpp

sax_sources = \
	xercesc/sax/Dummy.cpp \
	xercesc/sax/InputSource.cpp \
	xercesc/sax/SAXException.cpp \
	xercesc/sax/SAXParseException.cpp


sax2_headers = \
	xercesc/sax2/Attributes.hpp \
	xercesc/sax2/ContentHandler.hpp \
	xercesc/sax2/DeclHandler.hpp \
	xercesc/sax2/DefaultHandler.hpp \
	xercesc/sax2/LexicalHandler.hpp \
	xercesc/sax2/SAX2XMLFilter.hpp \
	xercesc/sax2/SAX2XMLReader.hpp \
	xercesc/sax2/XMLReaderFactory.hpp

sax2_sources = \
	xercesc/sax2/sax2Dummy.cpp

util_headers = \
	xercesc/util/ArrayIndexOutOfBoundsException.hpp \
	xercesc/util/Base64.hpp \
	xercesc/util/BaseRefVectorOf.hpp \
	xercesc/util/BaseRefVectorOf.c \
	xercesc/util/BinFileInputStream.hpp \
	xercesc/util/BinInputStream.hpp \
	xercesc/util/BinMemInputStream.hpp \
	xercesc/util/BitOps.hpp \
	xercesc/util/BitSet.hpp \
	xercesc/util/CountedPointer.hpp \
	xercesc/util/CountedPointer.c \
	xercesc/util/DefaultPanicHandler.hpp \
	xercesc/util/EmptyStackException.hpp \
	xercesc/util/EncodingValidator.hpp \
	xercesc/util/FlagJanitor.hpp \
	xercesc/util/FlagJanitor.c \
	xercesc/util/Hashers.hpp \
	xercesc/util/Hash2KeysSetOf.hpp \
	xercesc/util/Hash2KeysSetOf.c \
	xercesc/util/HexBin.hpp \
	xercesc/util/IllegalArgumentException.hpp \
	xercesc/util/InvalidCastException.hpp \
	xercesc/util/IOException.hpp \
	xercesc/util/Janitor.hpp \
	xercesc/util/Janitor.c \
	xercesc/util/KeyRefPair.hpp \
	xercesc/util/KeyRefPair.c \
	xercesc/util/KeyValuePair.hpp \
	xercesc/util/KeyValuePair.c \
	xercesc/util/KVStringPair.hpp \
	xercesc/util/Mutexes.hpp \
	xercesc/util/LogicalPath.c \
	xercesc/util/NameIdPool.hpp \
	xercesc/util/NameIdPool.c \
	xercesc/util/NoSuchElementException.hpp \
	xercesc/util/NullPointerException.hpp \
	xercesc/util/NumberFormatException.hpp \
	xercesc/util/OutOfMemoryException.hpp \
	xercesc/util/PanicHandler.hpp \
	xercesc/util/ParseException.hpp \
	xercesc/util/PlatformUtils.hpp \
	xercesc/util/PSVIUni.hpp \
	xercesc/util/QName.hpp \
	xercesc/util/RefArrayOf.hpp \
	xercesc/util/RefArrayOf.c \
	xercesc/util/RefArrayVectorOf.hpp \
	xercesc/util/RefArrayVectorOf.c \
	xercesc/util/RefHash2KeysTableOf.hpp \
	xercesc/util/RefHash2KeysTableOf.c \
	xercesc/util/RefHash3KeysIdPool.hpp \
	xercesc/util/RefHash3KeysIdPool.c \
	xercesc/util/RefHashTableOf.hpp \
	xercesc/util/RefHashTableOf.c \
	xercesc/util/RefStackOf.hpp \
	xercesc/util/RefStackOf.c \
	xercesc/util/RefVectorOf.hpp \
	xercesc/util/RefVectorOf.c \
	xercesc/util/regx/ASCIIRangeFactory.hpp \
	xercesc/util/regx/BlockRangeFactory.hpp \
	xercesc/util/regx/BMPattern.hpp \
	xercesc/util/regx/CharToken.hpp \
	xercesc/util/regx/ClosureToken.hpp \
	xercesc/util/regx/ConcatToken.hpp \
	xercesc/util/regx/Match.hpp \
	xercesc/util/regx/Op.hpp \
	xercesc/util/regx/OpFactory.hpp \
	xercesc/util/regx/ParenToken.hpp \
	xercesc/util/regx/ParserForXMLSchema.hpp \
	xercesc/util/regx/RangeFactory.hpp \
	xercesc/util/regx/RangeToken.hpp \
	xercesc/util/regx/RangeTokenMap.hpp \
	xercesc/util/regx/RegularExpression.hpp \
	xercesc/util/regx/RegxDefs.hpp \
	xercesc/util/regx/RegxParser.hpp \
	xercesc/util/regx/RegxUtil.hpp \
	xercesc/util/regx/StringToken.hpp \
	xercesc/util/regx/Token.hpp \
	xercesc/util/regx/TokenFactory.hpp \
	xercesc/util/regx/TokenInc.hpp \
	xercesc/util/regx/UniCharTable.hpp \
	xercesc/util/regx/UnicodeRangeFactory.hpp \
	xercesc/util/regx/UnionToken.hpp \
	xercesc/util/regx/XMLRangeFactory.hpp \
	xercesc/util/regx/XMLUniCharacter.hpp \
	xercesc/util/RuntimeException.hpp \
	xercesc/util/SchemaDateTimeException.hpp \
	xercesc/util/SecurityManager.hpp \
	xercesc/util/StringPool.hpp \
	xercesc/util/SynchronizedStringPool.hpp \
	xercesc/util/TranscodingException.hpp \
	xercesc/util/TransENameMap.hpp \
	xercesc/util/TransENameMap.c \
	xercesc/util/TransService.hpp \
	xercesc/util/UnexpectedEOFException.hpp \
	xercesc/util/UnsupportedEncodingException.hpp \
	xercesc/util/UTFDataFormatException.hpp \
	xercesc/util/ValueArrayOf.hpp \
	xercesc/util/ValueArrayOf.c \
	xercesc/util/ValueHashTableOf.hpp \
	xercesc/util/ValueHashTableOf.c \
	xercesc/util/ValueStackOf.hpp \
	xercesc/util/ValueStackOf.c \
	xercesc/util/ValueVectorOf.hpp \
	xercesc/util/ValueVectorOf.c \
	xercesc/util/XercesDefs.hpp \
	xercesc/util/XercesVersion.hpp \
	xercesc/util/XMemory.hpp \
	xercesc/util/XML256TableTranscoder.hpp \
	xercesc/util/XML88591Transcoder.hpp \
	xercesc/util/XMLAbstractDoubleFloat.hpp \
	xercesc/util/XMLASCIITranscoder.hpp \
	xercesc/util/XMLBigDecimal.hpp \
	xercesc/util/XMLBigInteger.hpp \
	xercesc/util/XMLChar.hpp \
	xercesc/util/XMLChTranscoder.hpp \
	xercesc/util/XMLDateTime.hpp \
	xercesc/util/XMLDOMMsg.hpp \
	xercesc/util/XMLDouble.hpp \
	xercesc/util/XMLEBCDICTranscoder.hpp \
	xercesc/util/XMLEntityResolver.hpp \
	xercesc/util/XMLEnumerator.hpp \
	xercesc/util/XMLException.hpp \
	xercesc/util/XMLExceptMsgs.hpp \
	xercesc/util/XMLFileMgr.hpp \
	xercesc/util/XMLFloat.hpp \
	xercesc/util/XMLIBM1047Transcoder.hpp \
	xercesc/util/XMLIBM1140Transcoder.hpp \
	xercesc/util/XMLInitializer.hpp \
	xercesc/util/XMLInteger.hpp \
	xercesc/util/XMLMutexMgr.hpp \
	xercesc/util/XMLMsgLoader.hpp \
	xercesc/util/XMLNetAccessor.hpp \
	xercesc/util/XMLNumber.hpp \
	xercesc/util/XMLResourceIdentifier.hpp \
	xercesc/util/XMLString.hpp \
	xercesc/util/XMLStringTokenizer.hpp \
	xercesc/util/XMLUCS4Transcoder.hpp \
	xercesc/util/XMLUni.hpp \
	xercesc/util/XMLUniDefs.hpp \
	xercesc/util/XMLUri.hpp \
	xercesc/util/XMLURL.hpp \
	xercesc/util/XMLUTF16Transcoder.hpp \
	xercesc/util/XMLUTF8Transcoder.hpp \
	xercesc/util/XMLWin1252Transcoder.hpp

util_sources = \
	xercesc/util/Base64.cpp \
	xercesc/util/BinFileInputStream.cpp \
	xercesc/util/BinInputStream.cpp \
	xercesc/util/BinMemInputStream.cpp \
	xercesc/util/BitSet.cpp \
	xercesc/util/DefaultPanicHandler.cpp \
	xercesc/util/EncodingValidator.cpp \
	xercesc/util/HeaderDummy.cpp \
	xercesc/util/HexBin.cpp \
	xercesc/util/KVStringPair.cpp \
	xercesc/util/Mutexes.cpp \
	xercesc/util/PanicHandler.cpp \
	xercesc/util/PlatformUtils.cpp \
	xercesc/util/PSVIUni.cpp \
	xercesc/util/QName.cpp \
	xercesc/util/regx/ASCIIRangeFactory.cpp \
	xercesc/util/regx/BlockRangeFactory.cpp \
	xercesc/util/regx/BMPattern.cpp \
	xercesc/util/regx/CharToken.cpp \
	xercesc/util/regx/ClosureToken.cpp \
	xercesc/util/regx/ConcatToken.cpp \
	xercesc/util/regx/Match.cpp \
	xercesc/util/regx/Op.cpp \
	xercesc/util/regx/OpFactory.cpp \
	xercesc/util/regx/ParenToken.cpp \
	xercesc/util/regx/ParserForXMLSchema.cpp \
	xercesc/util/regx/RangeFactory.cpp \
	xercesc/util/regx/RangeToken.cpp \
	xercesc/util/regx/RangeTokenMap.cpp \
	xercesc/util/regx/RegularExpression.cpp \
	xercesc/util/regx/RegxParser.cpp \
	xercesc/util/regx/RegxUtil.cpp \
	xercesc/util/regx/StringToken.cpp \
	xercesc/util/regx/Token.cpp \
	xercesc/util/regx/TokenFactory.cpp \
	xercesc/util/regx/UnicodeRangeFactory.cpp \
	xercesc/util/regx/UnionToken.cpp \
	xercesc/util/regx/XMLRangeFactory.cpp \
	xercesc/util/regx/XMLUniCharacter.cpp \
	xercesc/util/StringPool.cpp \
	xercesc/util/SynchronizedStringPool.cpp \
	xercesc/util/TransService.cpp \
	xercesc/util/XMemory.cpp \
	xercesc/util/XML256TableTranscoder.cpp \
	xercesc/util/XML88591Transcoder.cpp \
	xercesc/util/XMLAbstractDoubleFloat.cpp \
	xercesc/util/XMLASCIITranscoder.cpp \
	xercesc/util/XMLBigDecimal.cpp \
	xercesc/util/XMLBigInteger.cpp \
	xercesc/util/XMLChar.cpp \
	xercesc/util/XMLChTranscoder.cpp \
	xercesc/util/XMLDateTime.cpp \
	xercesc/util/XMLDouble.cpp \
	xercesc/util/XMLEBCDICTranscoder.cpp \
	xercesc/util/XMLException.cpp \
	xercesc/util/XMLFloat.cpp \
	xercesc/util/XMLIBM1047Transcoder.cpp \
	xercesc/util/XMLIBM1140Transcoder.cpp \
	xercesc/util/XMLInitializer.cpp \
	xercesc/util/XMLMsgLoader.cpp \
	xercesc/util/XMLNumber.cpp \
	xercesc/util/XMLString.cpp \
	xercesc/util/XMLStringTokenizer.cpp \
	xercesc/util/XMLUCS4Transcoder.cpp \
	xercesc/util/XMLUni.cpp \
	xercesc/util/XMLUri.cpp \
	xercesc/util/XMLURL.cpp \
	xercesc/util/XMLUTF16Transcoder.cpp \
	xercesc/util/XMLUTF8Transcoder.cpp \
	xercesc/util/XMLWin1252Transcoder.cpp


validators_headers = \
	xercesc/validators/common/AllContentModel.hpp \
	xercesc/validators/common/CMAny.hpp \
	xercesc/validators/common/CMBinaryOp.hpp \
	xercesc/validators/common/CMLeaf.hpp \
	xercesc/validators/common/CMRepeatingLeaf.hpp \
	xercesc/validators/common/CMNode.hpp \
	xercesc/validators/common/CMStateSet.hpp \
	xercesc/validators/common/CMUnaryOp.hpp \
	xercesc/validators/common/ContentLeafNameTypeVector.hpp \
	xercesc/validators/common/ContentSpecNode.hpp \
	xercesc/validators/common/DFAContentModel.hpp \
	xercesc/validators/common/Grammar.hpp \
	xercesc/validators/common/GrammarResolver.hpp \
	xercesc/validators/common/MixedContentModel.hpp \
	xercesc/validators/common/SimpleContentModel.hpp \
	xercesc/validators/datatype/AbstractNumericFacetValidator.hpp \
	xercesc/validators/datatype/AbstractNumericValidator.hpp \
	xercesc/validators/datatype/AbstractStringValidator.hpp \
	xercesc/validators/datatype/AnySimpleTypeDatatypeValidator.hpp \
	xercesc/validators/datatype/AnyURIDatatypeValidator.hpp \
	xercesc/validators/datatype/Base64BinaryDatatypeValidator.hpp \
	xercesc/validators/datatype/BooleanDatatypeValidator.hpp \
	xercesc/validators/datatype/DatatypeValidator.hpp \
	xercesc/validators/datatype/DatatypeValidatorFactory.hpp \
	xercesc/validators/datatype/DateDatatypeValidator.hpp \
	xercesc/validators/datatype/DateTimeDatatypeValidator.hpp \
	xercesc/validators/datatype/DateTimeValidator.hpp \
	xercesc/validators/datatype/DayDatatypeValidator.hpp \
	xercesc/validators/datatype/DecimalDatatypeValidator.hpp \
	xercesc/validators/datatype/DoubleDatatypeValidator.hpp \
	xercesc/validators/datatype/DurationDatatypeValidator.hpp \
	xercesc/validators/datatype/ENTITYDatatypeValidator.hpp \
	xercesc/validators/datatype/FloatDatatypeValidator.hpp \
	xercesc/validators/datatype/HexBinaryDatatypeValidator.hpp \
	xercesc/validators/datatype/IDDatatypeValidator.hpp \
	xercesc/validators/datatype/IDREFDatatypeValidator.hpp \
	xercesc/validators/datatype/InvalidDatatypeFacetException.hpp \
	xercesc/validators/datatype/InvalidDatatypeValueException.hpp \
	xercesc/validators/datatype/ListDatatypeValidator.hpp \
	xercesc/validators/datatype/MonthDatatypeValidator.hpp \
	xercesc/validators/datatype/MonthDayDatatypeValidator.hpp \
	xercesc/validators/datatype/NameDatatypeValidator.hpp \
	xercesc/validators/datatype/NCNameDatatypeValidator.hpp \
	xercesc/validators/datatype/NOTATIONDatatypeValidator.hpp \
	xercesc/validators/datatype/QNameDatatypeValidator.hpp \
	xercesc/validators/datatype/StringDatatypeValidator.hpp \
	xercesc/validators/datatype/TimeDatatypeValidator.hpp \
	xercesc/validators/datatype/UnionDatatypeValidator.hpp \
	xercesc/validators/datatype/XMLCanRepGroup.hpp \
	xercesc/validators/datatype/YearDatatypeValidator.hpp \
	xercesc/validators/datatype/YearMonthDatatypeValidator.hpp \
	xercesc/validators/DTD/DocTypeHandler.hpp \
	xercesc/validators/DTD/DTDAttDef.hpp \
	xercesc/validators/DTD/DTDAttDefList.hpp \
	xercesc/validators/DTD/DTDElementDecl.hpp \
	xercesc/validators/DTD/DTDEntityDecl.hpp \
	xercesc/validators/DTD/DTDGrammar.hpp \
	xercesc/validators/DTD/DTDScanner.hpp \
	xercesc/validators/DTD/DTDValidator.hpp \
	xercesc/validators/DTD/XMLDTDDescriptionImpl.hpp \
	xercesc/validators/schema/ComplexTypeInfo.hpp \
	xercesc/validators/schema/GeneralAttributeCheck.hpp \
	xercesc/validators/schema/identity/FieldActivator.hpp \
	xercesc/validators/schema/identity/FieldValueMap.hpp \
	xercesc/validators/schema/identity/IC_Field.hpp \
	xercesc/validators/schema/identity/IC_Key.hpp \
	xercesc/validators/schema/identity/IC_KeyRef.hpp \
	xercesc/validators/schema/identity/IC_Selector.hpp \
	xercesc/validators/schema/identity/IC_Unique.hpp \
	xercesc/validators/schema/identity/IdentityConstraint.hpp \
	xercesc/validators/schema/identity/IdentityConstraintHandler.hpp \
	xercesc/validators/schema/identity/ValueStore.hpp \
	xercesc/validators/schema/identity/ValueStoreCache.hpp \
	xercesc/validators/schema/identity/XercesXPath.hpp \
	xercesc/validators/schema/identity/XPathException.hpp \
	xercesc/validators/schema/identity/XPathMatcher.hpp \
	xercesc/validators/schema/identity/XPathMatcherStack.hpp \
	xercesc/validators/schema/identity/XPathSymbols.hpp \
	xercesc/validators/schema/NamespaceScope.hpp \
	xercesc/validators/schema/PSVIDefs.hpp \
	xercesc/validators/schema/SchemaAttDef.hpp \
	xercesc/validators/schema/SchemaAttDefList.hpp \
	xercesc/validators/schema/SchemaElementDecl.hpp \
	xercesc/validators/schema/SchemaGrammar.hpp \
	xercesc/validators/schema/SchemaInfo.hpp \
	xercesc/validators/schema/SchemaSymbols.hpp \
	xercesc/validators/schema/SchemaValidator.hpp \
	xercesc/validators/schema/SubstitutionGroupComparator.hpp \
	xercesc/validators/schema/TraverseSchema.hpp \
	xercesc/validators/schema/XercesAttGroupInfo.hpp \
	xercesc/validators/schema/XercesElementWildcard.hpp \
	xercesc/validators/schema/XercesGroupInfo.hpp \
	xercesc/validators/schema/XMLSchemaDescriptionImpl.hpp \
	xercesc/validators/schema/XSDDOMParser.hpp \
	xercesc/validators/schema/XSDErrorReporter.hpp \
	xercesc/validators/schema/XSDLocator.hpp \
	xercesc/validators/schema/XUtil.hpp

validators_sources = \
	xercesc/validators/common/AllContentModel.cpp \
	xercesc/validators/common/CMAny.cpp \
	xercesc/validators/common/CMBinaryOp.cpp \
	xercesc/validators/common/CMUnaryOp.cpp \
	xercesc/validators/common/ContentLeafNameTypeVector.cpp \
	xercesc/validators/common/ContentSpecNode.cpp \
	xercesc/validators/common/DFAContentModel.cpp \
	xercesc/validators/common/Grammar.cpp \
	xercesc/validators/common/GrammarResolver.cpp \
	xercesc/validators/common/MixedContentModel.cpp \
	xercesc/validators/common/SimpleContentModel.cpp \
	xercesc/validators/datatype/AbstractNumericFacetValidator.cpp \
	xercesc/validators/datatype/AbstractNumericValidator.cpp \
	xercesc/validators/datatype/AbstractStringValidator.cpp \
	xercesc/validators/datatype/AnySimpleTypeDatatypeValidator.cpp \
	xercesc/validators/datatype/AnyURIDatatypeValidator.cpp \
	xercesc/validators/datatype/Base64BinaryDatatypeValidator.cpp \
	xercesc/validators/datatype/BooleanDatatypeValidator.cpp \
	xercesc/validators/datatype/DatatypeValidator.cpp \
	xercesc/validators/datatype/DatatypeValidatorFactory.cpp \
	xercesc/validators/datatype/DateDatatypeValidator.cpp \
	xercesc/validators/datatype/DateTimeDatatypeValidator.cpp \
	xercesc/validators/datatype/DateTimeValidator.cpp \
	xercesc/validators/datatype/DayDatatypeValidator.cpp \
	xercesc/validators/datatype/DecimalDatatypeValidator.cpp \
	xercesc/validators/datatype/DoubleDatatypeValidator.cpp \
	xercesc/validators/datatype/DurationDatatypeValidator.cpp \
	xercesc/validators/datatype/ENTITYDatatypeValidator.cpp \
	xercesc/validators/datatype/FloatDatatypeValidator.cpp \
	xercesc/validators/datatype/HexBinaryDatatypeValidator.cpp \
	xercesc/validators/datatype/IDDatatypeValidator.cpp \
	xercesc/validators/datatype/IDREFDatatypeValidator.cpp \
	xercesc/validators/datatype/ListDatatypeValidator.cpp \
	xercesc/validators/datatype/MonthDatatypeValidator.cpp \
	xercesc/validators/datatype/MonthDayDatatypeValidator.cpp \
	xercesc/validators/datatype/NameDatatypeValidator.cpp \
	xercesc/validators/datatype/NCNameDatatypeValidator.cpp \
	xercesc/validators/datatype/NOTATIONDatatypeValidator.cpp \
	xercesc/validators/datatype/QNameDatatypeValidator.cpp \
	xercesc/validators/datatype/StringDatatypeValidator.cpp \
	xercesc/validators/datatype/TimeDatatypeValidator.cpp \
	xercesc/validators/datatype/UnionDatatypeValidator.cpp \
	xercesc/validators/datatype/XMLCanRepGroup.cpp \
	xercesc/validators/datatype/YearDatatypeValidator.cpp \
	xercesc/validators/datatype/YearMonthDatatypeValidator.cpp \
	xercesc/validators/DTD/DTDAttDef.cpp \
	xercesc/validators/DTD/DTDAttDefList.cpp \
	xercesc/validators/DTD/DTDElementDecl.cpp \
	xercesc/validators/DTD/DTDEntityDecl.cpp \
	xercesc/validators/DTD/DTDGrammar.cpp \
	xercesc/validators/DTD/DTDScanner.cpp \
	xercesc/validators/DTD/DTDValidator.cpp \
	xercesc/validators/DTD/XMLDTDDescriptionImpl.cpp \
	xercesc/validators/schema/ComplexTypeInfo.cpp \
	xercesc/validators/schema/GeneralAttributeCheck.cpp \
	xercesc/validators/schema/identity/FieldActivator.cpp \
	xercesc/validators/schema/identity/FieldValueMap.cpp \
	xercesc/validators/schema/identity/IC_Field.cpp \
	xercesc/validators/schema/identity/IC_Key.cpp \
	xercesc/validators/schema/identity/IC_KeyRef.cpp \
	xercesc/validators/schema/identity/IC_Selector.cpp \
	xercesc/validators/schema/identity/IC_Unique.cpp \
	xercesc/validators/schema/identity/IdentityConstraint.cpp \
	xercesc/validators/schema/identity/IdentityConstraintHandler.cpp \
	xercesc/validators/schema/identity/ValueStore.cpp \
	xercesc/validators/schema/identity/ValueStoreCache.cpp \
	xercesc/validators/schema/identity/XercesXPath.cpp \
	xercesc/validators/schema/identity/XPathMatcher.cpp \
	xercesc/validators/schema/identity/XPathMatcherStack.cpp \
	xercesc/validators/schema/identity/XPathSymbols.cpp \
	xercesc/validators/schema/NamespaceScope.cpp \
	xercesc/validators/schema/SchemaAttDef.cpp \
	xercesc/validators/schema/SchemaAttDefList.cpp \
	xercesc/validators/schema/SchemaElementDecl.cpp \
	xercesc/validators/schema/SchemaGrammar.cpp \
	xercesc/validators/schema/SchemaInfo.cpp \
	xercesc/validators/schema/SchemaSymbols.cpp \
	xercesc/validators/schema/SchemaValidator.cpp \
	xercesc/validators/schema/SubstitutionGroupComparator.cpp \
	xercesc/validators/schema/TraverseSchema.cpp \
	xercesc/validators/schema/XercesAttGroupInfo.cpp \
	xercesc/validators/schema/XercesElementWildcard.cpp \
	xercesc/validators/schema/XercesGroupInfo.cpp \
	xercesc/validators/schema/XMLSchemaDescriptionImpl.cpp \
	xercesc/validators/schema/XSDDOMParser.cpp \
	xercesc/validators/schema/XSDErrorReporter.cpp \
	xercesc/validators/schema/XSDLocator.cpp \
	xercesc/validators/schema/XUtil.cpp

xinclude_headers = \
	xercesc/xinclude/XIncludeDOMDocumentProcessor.hpp \
        xercesc/xinclude/XIncludeLocation.hpp \
        xercesc/xinclude/XIncludeUtils.hpp

xinclude_sources = \
	xercesc/xinclude/XIncludeDOMDocumentProcessor.cpp \
        xercesc/xinclude/XIncludeLocation.cpp \
        xercesc/xinclude/XIncludeUtils.cpp

icu_headers = \
	xercesc/util/Transcoders/ICU/ICUTransService.hpp

icu_sources = \
	xercesc/util/Transcoders/ICU/ICUTransService.cpp

gnuiconv_headers = \
	xercesc/util/Transcoders/IconvGNU/IconvGNUTransService.hpp

gnuiconv_sources = \
	xercesc/util/Transcoders/IconvGNU/IconvGNUTransService.cpp

iconv_headers = \
	xercesc/util/Transcoders/Iconv/IconvTransService.hpp

iconv_sources = \
	xercesc/util/Transcoders/Iconv/IconvTransService.cpp

macosunicodeconverter_headers = \
	xercesc/util/Transcoders/MacOSUnicodeConverter/MacOSUnicodeConverter.hpp

macosunicodeconverter_sources = \
	xercesc/util/Transcoders/MacOSUnicodeConverter/MacOSUnicodeConverter.cpp

wintrans_headers = \
	xercesc/util/Transcoders/Win32/Win32TransService.hpp

wintrans_sources = \
	xercesc/util/Transcoders/Win32/Win32TransService.cpp


curl_headers = \
	xercesc/util/NetAccessors/Curl/CurlNetAccessor.hpp \
	xercesc/util/NetAccessors/Curl/CurlURLInputStream.hpp

curl_sources = \
	xercesc/util/NetAccessors/Curl/CurlNetAccessor.cpp \
	xercesc/util/NetAccessors/Curl/CurlURLInputStream.cpp

sockets_headers = \
	xercesc/util/NetAccessors/Socket/SocketNetAccessor.hpp \
	xercesc/util/NetAccessors/Socket/UnixHTTPURLInputStream.hpp \
	xercesc/util/NetAccessors/BinHTTPInputStreamCommon.hpp

sockets_sources = \
	xercesc/util/NetAccessors/Socket/SocketNetAccessor.cpp \
	xercesc/util/NetAccessors/Socket/UnixHTTPURLInputStream.cpp \
	xercesc/util/NetAccessors/BinHTTPInputStreamCommon.cpp

cfurl_headers = \
	xercesc/util/NetAccessors/MacOSURLAccessCF/MacOSURLAccessCF.hpp \
	xercesc/util/NetAccessors/MacOSURLAccessCF/URLAccessCFBinInputStream.hpp

cfurl_sources = \
	xercesc/util/NetAccessors/MacOSURLAccessCF/MacOSURLAccessCF.cpp \
	xercesc/util/NetAccessors/MacOSURLAccessCF/URLAccessCFBinInputStream.cpp

winsock_headers = \
	xercesc/util/NetAccessors/WinSock/WinSockNetAccessor.hpp \
	xercesc/util/NetAccessors/WinSock/BinHTTPURLInputStream.hpp \
	xercesc/util/NetAccessors/BinHTTPInputStreamCommon.hpp

winsock_sources = \
	xercesc/util/NetAccessors/WinSock/WinSockNetAccessor.cpp \
	xercesc/util/NetAccessors/WinSock/BinHTTPURLInputStream.cpp \
	xercesc/util/NetAccessors/BinHTTPInputStreamCommon.cpp


msginmemory_headers = \
	xercesc/util/MsgLoaders/InMemory/InMemMsgLoader.hpp \
	xercesc/util/MsgLoaders/InMemory/XercesMessages_en_US.hpp

msginmemory_sources = \
	xercesc/util/MsgLoaders/InMemory/InMemMsgLoader.cpp

msgicu_headers = \
	xercesc/util/MsgLoaders/ICU/ICUMsgLoader.hpp

msgicu_sources = \
	xercesc/util/MsgLoaders/ICU/ICUMsgLoader.cpp

msgicu_nodist_sources = \
	xercesc_messages.c \
	root_res.c

msgiconv_headers = \
	xercesc/util/MsgLoaders/MsgCatalog/MsgCatalogLoader.hpp \
	xercesc/util/MsgLoaders/MsgCatalog/XMLMsgCat_Ids.hpp

msgiconv_sources = \
	xercesc/util/MsgLoaders/MsgCatalog/MsgCatalogLoader.cpp


posixfmgr_headers = \
	xercesc/util/FileManagers/PosixFileMgr.hpp

posixfmgr_sources = \
	xercesc/util/FileManagers/PosixFileMgr.cpp

winfmgr_headers = \
	xercesc/util/FileManagers/WindowsFileMgr.hpp

winfmgr_sources = \
	xercesc/util/FileManagers/WindowsFileMgr.cpp


nothreadmmgr_headers = \
	xercesc/util/MutexManagers/NoThreadMutexMgr.hpp

nothreadmmgr_sources = \
	xercesc/util/MutexManagers/NoThreadMutexMgr.cpp

posixmmgr_headers = \
	xercesc/util/MutexManagers/PosixMutexMgr.hpp

posixmmgr_sources = \
	xercesc/util/MutexManagers/PosixMutexMgr.cpp

winmmgr_headers = \
	xercesc/util/MutexManagers/WindowsMutexMgr.hpp

winmmgr_sources = \
	xercesc/util/MutexManagers/WindowsMutexMgr.cpp


# Override generated rules to provide prettier make rules
#
# Warning: If Automake changes sufficiently, these rules may need to
#          be regenerated from the (new) default output of Automake
#
if XERCES_PRETTY_MAKE

.c.lo:
@am__fastdepCC_TRUE@	@depbase=`echo $@ | sed 's|[^/]*$$|$(DEPDIR)/&|;s|\.lo$$||'`; \
@am__fastdepCC_TRUE@	if $(LTCOMPILE) -MT $@ -MD -MP -MF "$$depbase.Tpo" -c -o $@ $<; \
@am__fastdepCC_TRUE@	then $(am__mv) "$$depbase.Tpo" "$$depbase.Plo"; else rm -f "$$depbase.Tpo"; exit 1; fi
@AMDEP_TRUE@@am__fastdepCXX_FALSE@	@source='$<' object='$@' libtool=yes @AMDEPBACKSLASH@
@AMDEP_TRUE@@am__fastdepCXX_FALSE@	DEPDIR=$(DEPDIR) $(CCDEPMODE) $(depcomp) @AMDEPBACKSLASH@
@am__fastdepCC_FALSE@	$(LTCOMPILE) -c -o $@ $<

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

COMPILE = ${PRETTY_COMPILE} \
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) \
	$(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(CFLAGS)
LTCOMPILE = ${PRETTY_COMPILE}  \
	$(LIBTOOL) --quiet --tag=CC --mode=compile \
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) \
	$(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(CFLAGS)

CXXCOMPILE = ${PRETTY_COMPILE} \
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) \
	$(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS)
LTCXXCOMPILE = ${PRETTY_COMPILE}  \
	$(LIBTOOL) --quiet --tag=CXX --mode=compile \
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) \
	$(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS)
