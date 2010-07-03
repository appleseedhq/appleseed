/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/**
*  This file contains code to build the DOM tree. It registers a document
*  handler with the scanner. In these handler methods, appropriate DOM nodes
*  are created and added to the DOM tree.
*
* $Id: DOMBuilderImpl.cpp 568078 2007-08-21 11:43:25Z amassari $
*
*/



// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/parsers/DOMBuilderImpl.hpp>
#include <xercesc/util/IOException.hpp>
#include <xercesc/dom/DOMEntityResolver.hpp>
#include <xercesc/dom/DOMErrorHandler.hpp>
#include <xercesc/dom/impl/DOMErrorImpl.hpp>
#include <xercesc/dom/impl/DOMLocatorImpl.hpp>
#include <xercesc/dom/DOMException.hpp>
#include <xercesc/sax/SAXParseException.hpp>
#include <xercesc/internal/XMLScanner.hpp>
#include <xercesc/framework/Wrapper4DOMInputSource.hpp>
#include <xercesc/framework/XMLGrammarPool.hpp>
#include <xercesc/framework/XMLSchemaDescription.hpp>
#include <xercesc/util/Janitor.hpp>
#include <xercesc/validators/common/GrammarResolver.hpp>
#include <xercesc/util/OutOfMemoryException.hpp>
#include <xercesc/util/XMLEntityResolver.hpp>

XERCES_CPP_NAMESPACE_BEGIN



// ---------------------------------------------------------------------------
//  DOMBuilderImpl: Constructors and Destructor
// ---------------------------------------------------------------------------
DOMBuilderImpl::DOMBuilderImpl( XMLValidator* const   valToAdopt
                              , MemoryManager* const  manager
                              , XMLGrammarPool* const gramPool) :

AbstractDOMParser(valToAdopt, manager, gramPool)
, fAutoValidation(false)
, fValidation(false)
, fEntityResolver(0)
, fXMLEntityResolver(0)
, fErrorHandler(0)
, fFilter(0)
, fCharsetOverridesXMLEncoding(true)
, fUserAdoptsDocument(false)
{
    // dom spec has different default from scanner's default, so set explicitly
    getScanner()->setNormalizeData(false);
}


DOMBuilderImpl::~DOMBuilderImpl()
{
}


// ---------------------------------------------------------------------------
//  DOMBuilderImpl: Setter methods
// ---------------------------------------------------------------------------
void DOMBuilderImpl::setErrorHandler(DOMErrorHandler* const handler)
{
    fErrorHandler = handler;
    if (fErrorHandler) {
        getScanner()->setErrorReporter(this);
    }
    else {
        getScanner()->setErrorReporter(0);
    }
}

void DOMBuilderImpl::setEntityResolver(DOMEntityResolver* const handler)
{
    fEntityResolver = handler;
    if (fEntityResolver) {
        getScanner()->setEntityHandler(this);
        fXMLEntityResolver = 0;
    }
    else {
        getScanner()->setEntityHandler(0);
    }
}

void DOMBuilderImpl::setXMLEntityResolver(XMLEntityResolver* const handler)
{
    fXMLEntityResolver = handler;
    if (fXMLEntityResolver) {
        getScanner()->setEntityHandler(this);
        fEntityResolver = 0;
    }
    else {
        getScanner()->setEntityHandler(0);
    }
}

void DOMBuilderImpl::setFilter(DOMBuilderFilter* const)
{
    throw DOMException(DOMException::NOT_SUPPORTED_ERR, 0, getMemoryManager());
}


// ---------------------------------------------------------------------------
//  DOMBuilderImpl: Feature methods
// ---------------------------------------------------------------------------
void DOMBuilderImpl::setFeature(const XMLCh* const name, const bool state)
{
    if (XMLString::compareIStringASCII(name, XMLUni::fgDOMEntities) == 0) {
        setCreateEntityReferenceNodes(state);
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgDOMComments) == 0) {
        setCreateCommentNodes(state);
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgDOMDatatypeNormalization) == 0) {
        getScanner()->setNormalizeData(state);
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgDOMNamespaces) == 0) {
        setDoNamespaces(state);
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgDOMWhitespaceInElementContent) == 0) {
        setIncludeIgnorableWhitespace(state);
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgDOMValidation) == 0) {

        fValidation = state;

        if (state) {
            if (getValidationScheme() == AbstractDOMParser::Val_Never)
                setValidationScheme(AbstractDOMParser::Val_Always);
        }
        else {
            setValidationScheme(AbstractDOMParser::Val_Never);
        }
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgDOMValidateIfSchema) == 0) {

        fAutoValidation = state;

        if (state) {
            setValidationScheme(AbstractDOMParser::Val_Auto);
        }
        else {
            setValidationScheme(AbstractDOMParser::Val_Never);
        }
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgDOMCharsetOverridesXMLEncoding) == 0) {
        // in fact, setting this has no effect to the parser
        fCharsetOverridesXMLEncoding = state;
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgDOMSupportedMediatypesOnly) == 0 ||
             XMLString::compareIStringASCII(name, XMLUni::fgDOMInfoset) == 0 ||
             XMLString::compareIStringASCII(name, XMLUni::fgDOMCanonicalForm) == 0 ) {
        if (state)
            throw DOMException(DOMException::NOT_SUPPORTED_ERR, 0, getMemoryManager());
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgDOMNamespaceDeclarations) == 0 ||
             XMLString::compareIStringASCII(name, XMLUni::fgDOMCDATASections) == 0 ) {
        if (!state)
            throw DOMException(DOMException::NOT_SUPPORTED_ERR, 0, getMemoryManager());
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesSchema) == 0)
    {
        setDoSchema(state);
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesSchemaFullChecking) == 0)
    {
        setValidationSchemaFullChecking(state);
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesUserAdoptsDOMDocument) == 0)
    {
        if(state)
            fUserAdoptsDocument = true;
        else
            fUserAdoptsDocument = false;
    }

    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesLoadExternalDTD) == 0)
    {
        setLoadExternalDTD(state);
    }

    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesContinueAfterFatalError) == 0)
    {
        setExitOnFirstFatalError(!state);
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesValidationErrorAsFatal) == 0)
    {
        setValidationConstraintFatal(state);
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesCacheGrammarFromParse) == 0)
    {
        getScanner()->cacheGrammarFromParse(state);

        if (state)
            getScanner()->useCachedGrammarInParse(state);
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesUseCachedGrammarInParse) == 0)
    {
        if (state || !getScanner()->isCachingGrammarFromParse())
            getScanner()->useCachedGrammarInParse(state);
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesCalculateSrcOfs) == 0)
    {
        getScanner()->setCalculateSrcOfs(state);
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesStandardUriConformant) == 0)
    {
        getScanner()->setStandardUriConformant(state);
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesDOMHasPSVIInfo) == 0)
    {
        setCreateSchemaInfo(state);
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesGenerateSyntheticAnnotations) == 0)
    {
        getScanner()->setGenerateSyntheticAnnotations(state);
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesValidateAnnotations) == 0)
    {
        getScanner()->setValidateAnnotations(state);
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesIdentityConstraintChecking) == 0)
    {
        getScanner()->setIdentityConstraintChecking(state);
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesIgnoreCachedDTD) == 0)
    {
        getScanner()->setIgnoredCachedDTD(state);
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesIgnoreAnnotations) == 0)
    {
        getScanner()->setIgnoreAnnotations(state);
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesDisableDefaultEntityResolution) == 0)
    {
        getScanner()->setDisableDefaultEntityResolution(state);
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesSkipDTDValidation) == 0)
    {
        getScanner()->setSkipDTDValidation(state);
    }
    else {
        throw DOMException(DOMException::NOT_FOUND_ERR, 0, getMemoryManager());
    }
}

bool DOMBuilderImpl::getFeature(const XMLCh* const name) const
{
    if (XMLString::compareIStringASCII(name, XMLUni::fgDOMEntities) == 0) {
        return getCreateEntityReferenceNodes();
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgDOMComments) == 0) {
        return getCreateCommentNodes();
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgDOMDatatypeNormalization) == 0) {
        return getScanner()->getNormalizeData();
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgDOMNamespaces) == 0) {
        return getDoNamespaces();
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgDOMWhitespaceInElementContent) == 0) {
        return getIncludeIgnorableWhitespace();
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgDOMValidation) == 0) {
        return fValidation;
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgDOMValidateIfSchema) == 0) {
        return fAutoValidation;
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgDOMCharsetOverridesXMLEncoding) == 0) {
        return fCharsetOverridesXMLEncoding;
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgDOMSupportedMediatypesOnly) == 0 ||
             XMLString::compareIStringASCII(name, XMLUni::fgDOMInfoset) == 0 ||
             XMLString::compareIStringASCII(name, XMLUni::fgDOMCanonicalForm) == 0 ) {
        return false;
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgDOMNamespaceDeclarations) == 0 ||
             XMLString::compareIStringASCII(name, XMLUni::fgDOMCDATASections) == 0 ) {
        return true;
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesSchema) == 0)
    {
        return getDoSchema();
    }

    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesSchemaFullChecking) == 0)
    {
        return getValidationSchemaFullChecking();
    }

    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesIdentityConstraintChecking) == 0)
    {
        return getIdentityConstraintChecking();
    }

    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesLoadExternalDTD) == 0)
    {
        return getLoadExternalDTD();
    }

    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesContinueAfterFatalError) == 0)
    {
        return !getExitOnFirstFatalError();
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesValidationErrorAsFatal) == 0)
    {
        return getValidationConstraintFatal();
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesCacheGrammarFromParse) == 0)
    {
        return getScanner()->isCachingGrammarFromParse();
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesUseCachedGrammarInParse) == 0)
    {
        return getScanner()->isUsingCachedGrammarInParse();
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesCalculateSrcOfs) == 0)
    {
        return getScanner()->getCalculateSrcOfs();
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesStandardUriConformant) == 0)
    {
        return getScanner()->getStandardUriConformant();
    }
    else if(XMLString::compareIStringASCII(name, XMLUni::fgXercesUserAdoptsDOMDocument) == 0) {
        return fUserAdoptsDocument;
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesDOMHasPSVIInfo) == 0) {
        return getCreateSchemaInfo();
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesGenerateSyntheticAnnotations) == 0)
    {
        return getScanner()->getGenerateSyntheticAnnotations();
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesValidateAnnotations) == 0)
    {
        return getScanner()->getValidateAnnotations();
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesIgnoreCachedDTD) == 0)
    {
        return getScanner()->getIgnoreCachedDTD();
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesIgnoreAnnotations) == 0)
    {
        return getScanner()->getIgnoreAnnotations();
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesDisableDefaultEntityResolution) == 0)
    {
        return getScanner()->getDisableDefaultEntityResolution();
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesSkipDTDValidation) == 0)
    {
        return getScanner()->getSkipDTDValidation();
    }
    else {
        throw DOMException(DOMException::NOT_FOUND_ERR, 0, getMemoryManager());
    }

    return false;
}

bool DOMBuilderImpl::canSetFeature(const XMLCh* const name, const bool state) const
{
    if ((XMLString::compareIStringASCII(name, XMLUni::fgDOMEntities) == 0) ||
        (XMLString::compareIStringASCII(name, XMLUni::fgDOMComments) == 0) ||
        (XMLString::compareIStringASCII(name, XMLUni::fgDOMDatatypeNormalization) == 0) ||
        (XMLString::compareIStringASCII(name, XMLUni::fgDOMNamespaces) == 0) ||
        (XMLString::compareIStringASCII(name, XMLUni::fgDOMValidation) == 0) ||
        (XMLString::compareIStringASCII(name, XMLUni::fgDOMValidateIfSchema) == 0) ||
        (XMLString::compareIStringASCII(name, XMLUni::fgDOMCharsetOverridesXMLEncoding) == 0) ||
        (XMLString::compareIStringASCII(name, XMLUni::fgDOMWhitespaceInElementContent) == 0) ||
        (XMLString::compareIStringASCII(name, XMLUni::fgXercesUserAdoptsDOMDocument) == 0) ||
        (XMLString::compareIStringASCII(name, XMLUni::fgXercesCalculateSrcOfs) == 0) ||
        (XMLString::compareIStringASCII(name, XMLUni::fgXercesStandardUriConformant) == 0) ||
        (XMLString::compareIStringASCII(name, XMLUni::fgXercesDOMHasPSVIInfo) == 0) ||
        (XMLString::compareIStringASCII(name, XMLUni::fgXercesValidateAnnotations) == 0) ||
        (XMLString::compareIStringASCII(name, XMLUni::fgXercesGenerateSyntheticAnnotations) == 0) ||
        (XMLString::compareIStringASCII(name, XMLUni::fgXercesIdentityConstraintChecking) == 0) ||
        (XMLString::compareIStringASCII(name, XMLUni::fgXercesIgnoreCachedDTD) == 0) ||
        (XMLString::compareIStringASCII(name, XMLUni::fgXercesIgnoreAnnotations) == 0) ||
        (XMLString::compareIStringASCII(name, XMLUni::fgXercesDisableDefaultEntityResolution) == 0) ||
        (XMLString::compareIStringASCII(name, XMLUni::fgXercesSkipDTDValidation) == 0)       
       ) {
        return true;
    }

    else if (XMLString::compareIStringASCII(name, XMLUni::fgDOMSupportedMediatypesOnly) == 0 ||
             XMLString::compareIStringASCII(name, XMLUni::fgDOMInfoset) == 0 ||
             XMLString::compareIStringASCII(name, XMLUni::fgDOMCanonicalForm) == 0 ) {
        if (!state)
            return true;
    }
    else if (XMLString::compareIStringASCII(name, XMLUni::fgDOMNamespaceDeclarations) == 0 ||
             XMLString::compareIStringASCII(name, XMLUni::fgDOMCDATASections) == 0 ) {
        if (state)
            return true;
    }
    else if ((XMLString::compareIStringASCII(name, XMLUni::fgXercesSchema) == 0) ||
             (XMLString::compareIStringASCII(name, XMLUni::fgXercesSchemaFullChecking) == 0) ||
             (XMLString::compareIStringASCII(name, XMLUni::fgXercesLoadExternalDTD) == 0) ||
             (XMLString::compareIStringASCII(name, XMLUni::fgXercesContinueAfterFatalError) == 0) ||
             (XMLString::compareIStringASCII(name, XMLUni::fgXercesValidationErrorAsFatal) == 0) ||
             (XMLString::compareIStringASCII(name, XMLUni::fgXercesCacheGrammarFromParse) == 0) ||
             (XMLString::compareIStringASCII(name, XMLUni::fgXercesUseCachedGrammarInParse) == 0)) {
        return true;
    }
    return false;
}


// ---------------------------------------------------------------------------
//  DOMBuilderImpl: Non standard extension
// ---------------------------------------------------------------------------
void DOMBuilderImpl::setProperty(const XMLCh* const name, void* value)
{
	if (XMLString::compareIStringASCII(name, XMLUni::fgXercesSchemaExternalSchemaLocation) == 0)
	{
		setExternalSchemaLocation((XMLCh*)value);
	}

	else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesSchemaExternalNoNameSpaceSchemaLocation) == 0)
	{
		setExternalNoNamespaceSchemaLocation((XMLCh*)value);
	}
	else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesSecurityManager) == 0)
	{
		setSecurityManager((SecurityManager*)value);
	}
    else if (XMLString::equals(name, XMLUni::fgXercesScannerName))
    {
        AbstractDOMParser::useScanner((const XMLCh*) value);
    }
    else if (XMLString::equals(name, XMLUni::fgXercesParserUseDocumentFromImplementation))
    {
        useImplementation((const XMLCh*) value);
    }

    else
      throw DOMException(DOMException::NOT_FOUND_ERR, 0, getMemoryManager());
}


void* DOMBuilderImpl::getProperty(const XMLCh* const name) const
{
    if (XMLString::compareIStringASCII(name, XMLUni::fgXercesSchemaExternalSchemaLocation) == 0)
        return (void*)getExternalSchemaLocation();
    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesSchemaExternalNoNameSpaceSchemaLocation) == 0)
        return (void*)getExternalNoNamespaceSchemaLocation();
    else if (XMLString::compareIStringASCII(name, XMLUni::fgXercesSecurityManager) == 0)
        return (void*)getSecurityManager();
    else
        throw DOMException(DOMException::NOT_FOUND_ERR, 0, getMemoryManager());
    return 0;
}

void DOMBuilderImpl::release()
{
    DOMBuilderImpl* builder = (DOMBuilderImpl*) this;
    delete builder;
}

void DOMBuilderImpl::resetDocumentPool()
{
    resetPool();
}


// ---------------------------------------------------------------------------
//  DOMBuilderImpl: Parsing methods
// ---------------------------------------------------------------------------
DOMDocument* DOMBuilderImpl::parse(const DOMInputSource& source)
{
    Wrapper4DOMInputSource isWrapper((DOMInputSource*) &source, false, getMemoryManager());

    AbstractDOMParser::parse(isWrapper);
    if (fUserAdoptsDocument)
        return adoptDocument();
    else
        return getDocument();
}

DOMDocument* DOMBuilderImpl::parseURI(const XMLCh* const systemId)
{
    AbstractDOMParser::parse(systemId);
    if (fUserAdoptsDocument)
        return adoptDocument();
    else
        return getDocument();
}

DOMDocument* DOMBuilderImpl::parseURI(const char* const systemId)
{
    AbstractDOMParser::parse(systemId);
    if (fUserAdoptsDocument)
        return adoptDocument();
    else
        return getDocument();
}

void DOMBuilderImpl::parseWithContext(const DOMInputSource&,
                                      DOMNode* const,
                                      const short)
{
    throw DOMException(DOMException::NOT_SUPPORTED_ERR, 0, getMemoryManager());
}


// ---------------------------------------------------------------------------
//  DOMBuilderImpl: Implementation of the XMLErrorReporter interface
// ---------------------------------------------------------------------------
void DOMBuilderImpl::error( const   unsigned int                code
                            , const XMLCh* const
                            , const XMLErrorReporter::ErrTypes  errType
                            , const XMLCh* const                errorText
                            , const XMLCh* const                systemId
                            , const XMLCh* const
                            , const XMLSSize_t                  lineNum
                            , const XMLSSize_t                  colNum)
{
    if (fErrorHandler) {

        short severity = DOMError::DOM_SEVERITY_ERROR;

        if (errType == XMLErrorReporter::ErrType_Warning)
            severity = DOMError::DOM_SEVERITY_WARNING;
        else if (errType == XMLErrorReporter::ErrType_Fatal)
            severity = DOMError::DOM_SEVERITY_FATAL_ERROR;

        DOMLocatorImpl location((int)lineNum, (int) colNum, getCurrentNode(), systemId);
        DOMErrorImpl domError(severity, errorText, &location);

        // if user return false, we should stop the process, so throw an error
        if (!fErrorHandler->handleError(domError) && !getScanner()->getInException())
            throw (XMLErrs::Codes) code;
    }
}

void DOMBuilderImpl::resetErrors()
{
}


// ---------------------------------------------------------------------------
//  DOMBuilderImpl: Implementation of XMLEntityHandler interface
// ---------------------------------------------------------------------------
InputSource*
DOMBuilderImpl::resolveEntity(const XMLCh* const publicId,
                              const XMLCh* const systemId,
                              const XMLCh* const baseURI)
{
    //
    //  Just map it to the SAX entity resolver. If there is not one installed,
    //  return a null pointer to cause the default resolution.
    //
    if (fEntityResolver) {

        DOMInputSource* is = fEntityResolver->resolveEntity(publicId, systemId, baseURI);

        if (is)
            return new (getMemoryManager()) Wrapper4DOMInputSource(is, true, getMemoryManager());
    }

    return 0;
}

InputSource*
DOMBuilderImpl::resolveEntity( XMLResourceIdentifier* resourceIdentifier )
{
    //
    //  Just map it to the SAX entity resolver. If there is not one installed,
    //  return a null pointer to cause the default resolution.
    //
    if (fEntityResolver) {
        DOMInputSource* is = fEntityResolver->resolveEntity(resourceIdentifier->getPublicId(),
                                                            resourceIdentifier->getSystemId(), 
                                                            resourceIdentifier->getBaseURI());
        if (is)
            return new (getMemoryManager()) Wrapper4DOMInputSource(is, true, getMemoryManager());    
    }
    if (fXMLEntityResolver) {
        return(fXMLEntityResolver->resolveEntity(resourceIdentifier));    
    }
    
    return 0;
}

typedef JanitorMemFunCall<DOMBuilderImpl>    ResetParseType;

// ---------------------------------------------------------------------------
//  DOMBuilderImpl: Grammar preparsing methods
// ---------------------------------------------------------------------------
Grammar* DOMBuilderImpl::loadGrammar(const char* const systemId,
                                     const short grammarType,
                                     const bool toCache)
{
    // Avoid multiple entrance
    if (getParseInProgress())
        ThrowXMLwithMemMgr(IOException, XMLExcepts::Gen_ParseInProgress, fMemoryManager);

    ResetParseType  resetParse(this, &DOMBuilderImpl::resetParse);

	Grammar* grammar = 0;

    try
    {
        setParseInProgress(true);
        if (grammarType == Grammar::DTDGrammarType) 
            getScanner()->setDocTypeHandler(0);
        grammar = getScanner()->loadGrammar(systemId, grammarType, toCache);
    }
    catch(const OutOfMemoryException&)
    {
        resetParse.release();

        throw;
    }

    return grammar;
}

Grammar* DOMBuilderImpl::loadGrammar(const XMLCh* const systemId,
                                     const short grammarType,
                                     const bool toCache)
{
    // Avoid multiple entrance
    if (getParseInProgress())
        ThrowXMLwithMemMgr(IOException, XMLExcepts::Gen_ParseInProgress, fMemoryManager);

    ResetParseType  resetParse(this, &DOMBuilderImpl::resetParse);

	Grammar* grammar = 0;

    try
    {
        setParseInProgress(true);
        if (grammarType == Grammar::DTDGrammarType) 
            getScanner()->setDocTypeHandler(0);
        grammar = getScanner()->loadGrammar(systemId, grammarType, toCache);
    }
    catch(const OutOfMemoryException&)
    {
        resetParse.release();

        throw;
    }

    return grammar;
}

Grammar* DOMBuilderImpl::loadGrammar(const DOMInputSource& source,
                                     const short grammarType,
                                     const bool toCache)
{
    // Avoid multiple entrance
    if (getParseInProgress())
        ThrowXMLwithMemMgr(IOException, XMLExcepts::Gen_ParseInProgress, fMemoryManager);

    ResetParseType  resetParse(this, &DOMBuilderImpl::resetParse);

    Grammar* grammar = 0;

    try
    {
        setParseInProgress(true);
        if (grammarType == Grammar::DTDGrammarType) 
            getScanner()->setDocTypeHandler(0);
        Wrapper4DOMInputSource isWrapper((DOMInputSource*) &source, false, getMemoryManager());
        grammar = getScanner()->loadGrammar(isWrapper, grammarType, toCache);
    }
    catch(const OutOfMemoryException&)
    {
        resetParse.release();

        throw;
    }

    return grammar;
}

void DOMBuilderImpl::resetCachedGrammarPool()
{
    getGrammarResolver()->resetCachedGrammar();
}

void DOMBuilderImpl::resetParse()
{
    if (getScanner()->getDocTypeHandler() == 0)
    {
        getScanner()->setDocTypeHandler(this);
    }

    setParseInProgress(false);
}

Grammar* DOMBuilderImpl::getGrammar(const XMLCh* const nameSpaceKey) const
{
    return getGrammarResolver()->getGrammar(nameSpaceKey);
}

Grammar* DOMBuilderImpl::getRootGrammar() const
{
    return getScanner()->getRootGrammar();
}

const XMLCh* DOMBuilderImpl::getURIText(unsigned int uriId) const
{
    return getScanner()->getURIText(uriId);
}

unsigned int DOMBuilderImpl::getSrcOffset() const
{
    return getScanner()->getSrcOffset();
}

XERCES_CPP_NAMESPACE_END

