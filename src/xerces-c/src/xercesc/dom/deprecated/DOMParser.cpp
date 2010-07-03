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
* $Id: DOMParser.cpp 568078 2007-08-21 11:43:25Z amassari $
*
*/



// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/internal/XMLScannerResolver.hpp>
#include <xercesc/sax/EntityResolver.hpp>
#include <xercesc/util/XMLUniDefs.hpp>
#include <xercesc/sax/ErrorHandler.hpp>
#include <xercesc/sax/SAXParseException.hpp>
#include <xercesc/framework/XMLNotationDecl.hpp>
#include <xercesc/util/IOException.hpp>
#include <xercesc/framework/XMLValidator.hpp>
#include <xercesc/validators/common/GrammarResolver.hpp>
#include <xercesc/framework/XMLGrammarPool.hpp>
#include <xercesc/framework/XMLSchemaDescription.hpp>
#include <xercesc/util/Janitor.hpp>
#include <xercesc/util/XMLExceptMsgs.hpp>

#include "DOMParser.hpp"
#include "ElementImpl.hpp"
#include "AttrImpl.hpp"
#include "AttrNSImpl.hpp"
#include "TextImpl.hpp"
#include "DocumentImpl.hpp"
#include "DocumentTypeImpl.hpp"
#include "EntityImpl.hpp"
#include "NotationImpl.hpp"
#include "NamedNodeMapImpl.hpp"
#include "NodeIDMap.hpp"


#include <xercesc/validators/common/ContentSpecNode.hpp>
#include <xercesc/validators/DTD/DTDAttDefList.hpp>
#include <xercesc/util/OutOfMemoryException.hpp>
#include <xercesc/util/XMLEntityResolver.hpp>

XERCES_CPP_NAMESPACE_BEGIN


// ---------------------------------------------------------------------------
//  DOMParser: Constructors and Destructor
// ---------------------------------------------------------------------------
DOMParser::DOMParser( XMLValidator* const   valToAdopt
                    , MemoryManager* const  manager
                    , XMLGrammarPool* const gramPool) :

    fToCreateXMLDeclTypeNode(false)
    , fCreateEntityReferenceNodes(true)
    , fIncludeIgnorableWhitespace(true)
    , fParseInProgress(false)
    , fWithinElement(false)
    , fEntityResolver(0)
    , fXMLEntityResolver(0)
    , fErrorHandler(0)
    , fPSVIHandler(0)
    , fNodeStack(0)
    , fScanner(0)
    , fDocumentType(0)
    , fGrammarResolver(0)
    , fURIStringPool(0)
    , fValidator(valToAdopt)
    , fMemoryManager(manager)
    , fGrammarPool(gramPool)
{
    try
    {
        initialize();
    }
    catch(const OutOfMemoryException&)
    {
        throw;
    }
    catch(...)
    {
        cleanUp();
        throw;
    }
}


DOMParser::~DOMParser()
{
    cleanUp();
}

// ---------------------------------------------------------------------------
//  DOMParser: Initialize/CleanUp methods
// ---------------------------------------------------------------------------
void DOMParser::initialize()
{
    // Create grammar resolver and URI string pool to pass to the scanner
    fGrammarResolver = new (fMemoryManager) GrammarResolver(fGrammarPool, fMemoryManager);
    fURIStringPool = fGrammarResolver->getStringPool();

    //  Create a scanner and tell it what validator to use. Then set us
    //  as the document event handler so we can fill the DOM document.
    fScanner = XMLScannerResolver::getDefaultScanner(fValidator, fGrammarResolver, fMemoryManager);
    fScanner->setDocHandler(this);
    fScanner->setDocTypeHandler(this);
    fScanner->setURIStringPool(fURIStringPool);

    fNodeStack = new (fMemoryManager) ValueStackOf<DOM_Node>(64, fMemoryManager, true);
    this->reset();
}

void DOMParser::cleanUp()
{
    delete fNodeStack;
    delete fScanner;
    delete fGrammarResolver;
    // grammar pool must do this
    //delete fURIStringPool;

    if (fValidator)
        delete fValidator;
}

void DOMParser::reset()
{
    //
    //  Note: DOM Documents are reference counted. Doing this assignment
    //  will cause the old one to go away unless application code is also
    //  holding a reference to it.
    //
    fDocument = DOM_Document::createDocument(fMemoryManager);
    resetDocType();

    fCurrentParent   = 0;
    fCurrentNode     = 0;
    fParseInProgress = false;
    fWithinElement   = false;
    fNodeStack->removeAllElements();
};



// ---------------------------------------------------------------------------
//  DOMParser: Getter methods
// ---------------------------------------------------------------------------
const XMLValidator& DOMParser::getValidator() const
{
    return *fScanner->getValidator();
}

bool DOMParser::getDoNamespaces() const
{
    return fScanner->getDoNamespaces();
}

bool DOMParser::getExitOnFirstFatalError() const
{
    return fScanner->getExitOnFirstFatal();
}

bool DOMParser::getValidationConstraintFatal() const
{
    return fScanner->getValidationConstraintFatal();
}

DOMParser::ValSchemes DOMParser::getValidationScheme() const
{
    const XMLScanner::ValSchemes scheme = fScanner->getValidationScheme();

    if (scheme == XMLScanner::Val_Always)
        return Val_Always;
    else if (scheme == XMLScanner::Val_Never)
        return Val_Never;

    return Val_Auto;
}

bool DOMParser::getDoSchema() const
{
    return fScanner->getDoSchema();
}

bool DOMParser::getValidationSchemaFullChecking() const
{
    return fScanner->getValidationSchemaFullChecking();
}

bool DOMParser::getIdentityConstraintChecking() const
{
    return fScanner->getIdentityConstraintChecking();
}

int DOMParser::getErrorCount() const
{
    return fScanner->getErrorCount();
}

XMLCh* DOMParser::getExternalSchemaLocation() const
{
    return fScanner->getExternalSchemaLocation();
}

XMLCh* DOMParser::getExternalNoNamespaceSchemaLocation() const
{
    return fScanner->getExternalNoNamespaceSchemaLocation();
}

bool DOMParser::isCachingGrammarFromParse() const
{
    return fScanner->isCachingGrammarFromParse();
}

bool DOMParser::isUsingCachedGrammarInParse() const
{
    return fScanner->isUsingCachedGrammarInParse();
}

Grammar* DOMParser::getGrammar(const XMLCh* const nameSpaceKey)
{
    return fGrammarResolver->getGrammar(nameSpaceKey);
}

Grammar* DOMParser::getRootGrammar()
{
    return fScanner->getRootGrammar();
}

const XMLCh* DOMParser::getURIText(unsigned int uriId) const
{
    return fScanner->getURIText(uriId);
}

bool DOMParser::getCalculateSrcOfs() const
{
    return fScanner->getCalculateSrcOfs();
}

bool DOMParser::getStandardUriConformant() const
{
    return fScanner->getStandardUriConformant();
}

unsigned int DOMParser::getSrcOffset() const
{
    return fScanner->getSrcOffset();
}

// ---------------------------------------------------------------------------
//  DOMParser: Setter methods
// ---------------------------------------------------------------------------
void DOMParser::setDoNamespaces(const bool newState)
{
    fScanner->setDoNamespaces(newState);
}

void DOMParser::setErrorHandler(ErrorHandler* const handler)
{
    fErrorHandler = handler;
    if (fErrorHandler) {
        fScanner->setErrorReporter(this);
        fScanner->setErrorHandler(fErrorHandler);
    }
    else {
        fScanner->setErrorReporter(0);
        fScanner->setErrorHandler(0);
    }
}

void DOMParser::setPSVIHandler(PSVIHandler* const handler)
{
    fPSVIHandler = handler;
    if (fPSVIHandler) {
        fScanner->setPSVIHandler(fPSVIHandler);        
    }
    else {
        fScanner->setPSVIHandler(0);       
    }
}

void DOMParser::setEntityResolver(EntityResolver* const handler)
{
    fEntityResolver = handler;
    if (fEntityResolver) {
        fScanner->setEntityHandler(this);
        fXMLEntityResolver = 0;
    }
    else {
        fScanner->setEntityHandler(0);
    }
}

void DOMParser::setXMLEntityResolver(XMLEntityResolver* const handler)
{
    fXMLEntityResolver = handler;
    if (fXMLEntityResolver) {
        fEntityResolver = 0;
        fScanner->setEntityHandler(this);
    }
    else {
        fScanner->setEntityHandler(0);
    }
}

void DOMParser::setExitOnFirstFatalError(const bool newState)
{
    fScanner->setExitOnFirstFatal(newState);
}

void DOMParser::setValidationConstraintFatal(const bool newState)
{
    fScanner->setValidationConstraintFatal(newState);
}

void DOMParser::setValidationScheme(const ValSchemes newScheme)
{
    if (newScheme == Val_Never)
        fScanner->setValidationScheme(XMLScanner::Val_Never);
    else if (newScheme == Val_Always)
        fScanner->setValidationScheme(XMLScanner::Val_Always);
    else
        fScanner->setValidationScheme(XMLScanner::Val_Auto);
}

void DOMParser::setDoSchema(const bool newState)
{
    fScanner->setDoSchema(newState);
}

void DOMParser::setValidationSchemaFullChecking(const bool schemaFullChecking)
{
    fScanner->setValidationSchemaFullChecking(schemaFullChecking);
}

void DOMParser::setIdentityConstraintChecking(const bool identityConstraintChecking)
{
    fScanner->setIdentityConstraintChecking(identityConstraintChecking);
}

void DOMParser::setExternalSchemaLocation(const XMLCh* const schemaLocation)
{
    fScanner->setExternalSchemaLocation(schemaLocation);
}
void DOMParser::setExternalNoNamespaceSchemaLocation(const XMLCh* const noNamespaceSchemaLocation)
{
    fScanner->setExternalNoNamespaceSchemaLocation(noNamespaceSchemaLocation);
}

void DOMParser::setExternalSchemaLocation(const char* const schemaLocation)
{
    fScanner->setExternalSchemaLocation(schemaLocation);
}
void DOMParser::setExternalNoNamespaceSchemaLocation(const char* const noNamespaceSchemaLocation)
{
    fScanner->setExternalNoNamespaceSchemaLocation(noNamespaceSchemaLocation);
}

void DOMParser::cacheGrammarFromParse(const bool newState)
{
    fScanner->cacheGrammarFromParse(newState);

    if (newState)
        fScanner->useCachedGrammarInParse(newState);
}

void DOMParser::useCachedGrammarInParse(const bool newState)
{
    if (newState || !fScanner->isCachingGrammarFromParse())
        fScanner->useCachedGrammarInParse(newState);
}

void DOMParser::setCalculateSrcOfs(const bool newState)
{
    fScanner->setCalculateSrcOfs(newState);
}

void DOMParser::setStandardUriConformant(const bool newState)
{
    fScanner->setStandardUriConformant(newState);
}

void DOMParser::useScanner(const XMLCh* const scannerName)
{
    XMLScanner* tempScanner = XMLScannerResolver::resolveScanner
    (
        scannerName
        , fValidator
        , fGrammarResolver
        , fMemoryManager
    );

    if (tempScanner) {

        tempScanner->setParseSettings(fScanner);
        tempScanner->setURIStringPool(fURIStringPool);
        delete fScanner;
        fScanner = tempScanner;
    }
}

// ---------------------------------------------------------------------------
//  DOMParser: Parsing methods
// ---------------------------------------------------------------------------
void DOMParser::parse(const InputSource& source)
{
    // Avoid multiple entrance
    if (fParseInProgress)
        ThrowXMLwithMemMgr(IOException, XMLExcepts::Gen_ParseInProgress, fMemoryManager);

    try
    {
        fParseInProgress = true;
        fScanner->scanDocument(source);
        fParseInProgress = false;
    }
    catch(const OutOfMemoryException&)
    {
        throw;
    }
    catch(...)
    {
        fParseInProgress = false;
        throw;
    }
}

void DOMParser::parse(const XMLCh* const systemId)
{
    // Avoid multiple entrance
    if (fParseInProgress)
        ThrowXMLwithMemMgr(IOException, XMLExcepts::Gen_ParseInProgress, fMemoryManager);

    try
    {
        fParseInProgress = true;
        fScanner->scanDocument(systemId);
        fParseInProgress = false;
    }
    catch(const OutOfMemoryException&)
    {
        throw;
    }
    catch(...)
    {
        fParseInProgress = false;
        throw;
    }
}

void DOMParser::parse(const char* const systemId)
{
    // Avoid multiple entrance
    if (fParseInProgress)
        ThrowXMLwithMemMgr(IOException, XMLExcepts::Gen_ParseInProgress, fMemoryManager);

    try
    {
        fParseInProgress = true;
        fScanner->scanDocument(systemId);
        fParseInProgress = false;
    }
    catch(const OutOfMemoryException&)
    {
        throw;
    }
    catch(...)
    {
        fParseInProgress = false;
        throw;
    }
}



// ---------------------------------------------------------------------------
//  DOMParser: Progressive parse methods
// ---------------------------------------------------------------------------
bool DOMParser::parseFirst( const   XMLCh* const    systemId
                           ,       XMLPScanToken&  toFill)
{
    //
    //  Avoid multiple entrance. We cannot enter here while a regular parse
    //  is in progress.
    //
    if (fParseInProgress)
        ThrowXMLwithMemMgr(IOException, XMLExcepts::Gen_ParseInProgress, fMemoryManager);

    return fScanner->scanFirst(systemId, toFill);
}

bool DOMParser::parseFirst( const   char* const         systemId
                           ,       XMLPScanToken&      toFill)
{
    //
    //  Avoid multiple entrance. We cannot enter here while a regular parse
    //  is in progress.
    //
    if (fParseInProgress)
        ThrowXMLwithMemMgr(IOException, XMLExcepts::Gen_ParseInProgress, fMemoryManager);

    return fScanner->scanFirst(systemId, toFill);
}

bool DOMParser::parseFirst( const   InputSource&    source
                           ,       XMLPScanToken&  toFill)
{
    //
    //  Avoid multiple entrance. We cannot enter here while a regular parse
    //  is in progress.
    //
    if (fParseInProgress)
        ThrowXMLwithMemMgr(IOException, XMLExcepts::Gen_ParseInProgress, fMemoryManager);

    return fScanner->scanFirst(source, toFill);
}

bool DOMParser::parseNext(XMLPScanToken& token)
{
    return fScanner->scanNext(token);
}

void DOMParser::parseReset(XMLPScanToken& token)
{
    // Reset the scanner, and then reset the parser
    fScanner->scanReset(token);
    reset();
}


// ---------------------------------------------------------------------------
//  DOMParser: Implementation of the XMLErrorReporter interface
// ---------------------------------------------------------------------------
void DOMParser::error(  const   unsigned int              code
                      , const XMLCh* const                /*msgDomain*/
                      , const XMLErrorReporter::ErrTypes  errType
                      , const XMLCh* const                errorText
                      , const XMLCh* const                systemId
                      , const XMLCh* const                publicId
                      , const XMLSSize_t                  lineNum
                      , const XMLSSize_t                  colNum)
{
    SAXParseException toThrow = SAXParseException
        (
        static_cast<XMLExcepts::Codes>(code)
        , errorText
        , publicId
        , systemId
        , lineNum
        , colNum
        , fMemoryManager
        );

    //
    //  If there is an error handler registered, call it, otherwise ignore
    //  all but the fatal errors.
    //
    if (!fErrorHandler)
    {
        if (errType == XMLErrorReporter::ErrType_Fatal)
            throw toThrow;
        return;
    }

    if (errType == XMLErrorReporter::ErrType_Warning)
        fErrorHandler->warning(toThrow);
    else if (errType >= XMLErrorReporter::ErrType_Fatal)
        fErrorHandler->fatalError(toThrow);
    else
        fErrorHandler->error(toThrow);
}

void DOMParser::resetErrors()
{
}


// ---------------------------------------------------------------------------
//  DOMParser: Implementation of XMLEntityHandler interface
// ---------------------------------------------------------------------------
InputSource*
DOMParser::resolveEntity(const XMLCh* const publicId,
                         const XMLCh* const systemId,
                         const XMLCh* const /*baseURI*/)
{
    //
    //  Just map it to the SAX entity resolver. If there is not one installed,
    //  return a null pointer to cause the default resolution.
    //
    if (fEntityResolver)
        return fEntityResolver->resolveEntity(publicId, systemId);
    return 0;
}

InputSource* DOMParser::resolveEntity(XMLResourceIdentifier* resourceIdentifier) 
{
    //
    //  Just map it to the SAX entity resolver. If there is not one installed,
    //  return a null pointer to cause the default resolution.
    //
    if (fEntityResolver)
        return fEntityResolver->resolveEntity(resourceIdentifier->getPublicId(), 
                                                resourceIdentifier->getSystemId());
    if (fXMLEntityResolver)
        return fXMLEntityResolver->resolveEntity(resourceIdentifier);
    return 0;
}

// ---------------------------------------------------------------------------
//  DOMParser: Implementation of XMLDocumentHandler interface
// ---------------------------------------------------------------------------
void DOMParser::docCharacters(  const   XMLCh* const    chars
                              , const unsigned int    length
                              , const bool            cdataSection)
{
    // Ignore chars outside of content
    if (!fWithinElement)
        return;

    if (cdataSection == true)
    {
        DOM_CDATASection node = fDocument.createCDATASection
            (
            DOMString(chars, length)
            );
        fCurrentParent.appendChild(node);
        fCurrentNode = node;
    }
    else
    {
        if (fCurrentNode.getNodeType() == DOM_Node::TEXT_NODE)
        {
            DOM_Text node = (DOM_Text&)fCurrentNode;
            node.appendData(DOMString(chars, length));
        }
        else
        {
            DOM_Text node = fDocument.createTextNode(DOMString(chars, length));
            fCurrentParent.appendChild(node);

            fCurrentNode = node;

        }
    }
}


void DOMParser::docComment(const XMLCh* const comment)
{
    DOM_Comment dcom = fDocument.createComment(comment);
    fCurrentParent.appendChild(dcom);
    fCurrentNode = dcom;
}


void DOMParser::docPI(  const   XMLCh* const    target
                      , const XMLCh* const    data)
{
    DOM_ProcessingInstruction pi = fDocument.createProcessingInstruction
        (
        target
        , data
        );
    fCurrentParent.appendChild(pi);
    fCurrentNode = pi;
}


void DOMParser::endEntityReference(const XMLEntityDecl& entDecl)
{
    if (fCreateEntityReferenceNodes == true)
    {
        if (fCurrentParent.getNodeType() == DOM_Node::ENTITY_REFERENCE_NODE) {
		    // stick the parsed content of this entity reference into the entity definition node
		    EntityImpl* entity = (EntityImpl*)fDocumentType->entities->getNamedItem(entDecl.getName());
		    entity->setEntityRef((EntityReferenceImpl*)fCurrentParent.fImpl);

            ((DOM_EntityReference&)fCurrentParent).fImpl->setReadOnly(true, true);
        }
        fCurrentParent = fNodeStack->pop();
        fCurrentNode   = fCurrentParent;
    }
}


void DOMParser::endElement( const   XMLElementDecl&    /*elemDecl*/
                           , const unsigned int        /*urlId*/
                           , const bool                /*isRoot*/
                           , const XMLCh* const        /*elemPrefix*/)
{
    fCurrentNode   = fCurrentParent;
    fCurrentParent = fNodeStack->pop();

    // If we've hit the end of content, clear the flag
    if (fNodeStack->empty())
        fWithinElement = false;
}


void DOMParser::ignorableWhitespace(const   XMLCh* const    chars
                                    , const unsigned int    length
                                    , const bool            /*cdataSection*/)
{
    // Ignore chars before the root element
    if (!fWithinElement || !fIncludeIgnorableWhitespace)
        return;

    if (fCurrentNode.getNodeType() == DOM_Node::TEXT_NODE)
    {
        DOM_Text node = (DOM_Text&)fCurrentNode;
        node.appendData(DOMString(chars, length));
    }
    else
    {
        DOM_Text node = fDocument.createTextNode(DOMString(chars, length));
        TextImpl *text = (TextImpl *) node.fImpl;
        text -> setIgnorableWhitespace(true);
        fCurrentParent.appendChild(node);

        fCurrentNode = node;
    }
}


void DOMParser::resetDocument()
{
    //
    //  The reset methods are called before a new parse event occurs.
    //  Reset this parsers state to clear out anything that may be left
    //  from a previous use, in particular the DOM document itself.
    //
    this->reset();
}


void DOMParser::startDocument()
{
    // Just set the document as the current parent and current node
    fCurrentParent = fDocument;
    fCurrentNode   = fDocument;
    // set DOM error checking off
    fDocument.setErrorChecking(false);
}


void DOMParser::endDocument()
{
    // set DOM error checking back on
    fDocument.setErrorChecking(true);
}


void DOMParser::startElement(const  XMLElementDecl&         elemDecl
                             , const unsigned int            urlId
                             , const XMLCh* const            elemPrefix
                             , const RefVectorOf<XMLAttr>&   attrList
                             , const unsigned int            attrCount
                             , const bool                    isEmpty
                             , const bool                    isRoot)
{
    DOM_Element     elem;
    DocumentImpl    *docImpl = (DocumentImpl *)fDocument.fImpl;

    if (fScanner -> getDoNamespaces()) {    //DOM Level 2, doNamespaces on
        XMLBuffer buf(1023, fMemoryManager);
        DOMString namespaceURI = 0;
        DOMString elemQName = 0;
        if (urlId != fScanner->getEmptyNamespaceId()) {  //TagName has a prefix
            fScanner->getURIText(urlId, buf);   //get namespaceURI
            namespaceURI = DOMString(buf.getRawBuffer());

            if (elemPrefix && *elemPrefix) {
                elemQName.appendData(elemPrefix);
                elemQName.appendData(chColon);
            }
        }
        elemQName.appendData(elemDecl.getBaseName());

        elem = fDocument.createElementNS(namespaceURI, elemQName);
        ElementImpl *elemImpl = (ElementImpl *) elem.fImpl;
        for (unsigned int index = 0; index < attrCount; ++index) {
            static const XMLCh XMLNS[] = {
            chLatin_x, chLatin_m, chLatin_l, chLatin_n, chLatin_s, chNull
            };
            const XMLAttr* oneAttrib = attrList.elementAt(index);
            unsigned int attrURIId = oneAttrib -> getURIId();
            namespaceURI = 0;
            if (!XMLString::compareString(oneAttrib -> getName(), XMLNS))    //for xmlns=...
                attrURIId = fScanner->getXMLNSNamespaceId();
            if (attrURIId != fScanner->getEmptyNamespaceId()) {  //TagName has a prefix
                fScanner->getURIText(attrURIId, buf);   //get namespaceURI
                namespaceURI = DOMString(buf.getRawBuffer());
            }
            AttrImpl *attr = elemImpl->setAttributeNS(namespaceURI, oneAttrib -> getQName(),
            oneAttrib -> getValue());

            // Attributes of type ID.  If this is one, add it to the hashtable of IDs
            //   that is constructed for use by GetElementByID().
            //
            if (oneAttrib->getType()==XMLAttDef::ID)
            {
                if (docImpl->fNodeIDMap == 0)
                    docImpl->fNodeIDMap = new (fMemoryManager) NodeIDMap(500, fMemoryManager);
                docImpl->fNodeIDMap->add(attr);
                attr->isIdAttr(true);
            }

            attr->setSpecified(oneAttrib->getSpecified());
        }
    }
    else {    //DOM Level 1
        elem = fDocument.createElement(elemDecl.getFullName());
        ElementImpl *elemImpl = (ElementImpl *) elem.fImpl;
        for (unsigned int index = 0; index < attrCount; ++index) {
            const XMLAttr* oneAttrib = attrList.elementAt(index);
            AttrImpl *attr = elemImpl->setAttribute(oneAttrib->getName(), oneAttrib->getValue());
            attr->setSpecified(oneAttrib->getSpecified());

            // Attributes of type ID.  If this is one, add it to the hashtable of IDs
            //   that is constructed for use by GetElementByID().
            //
            if (oneAttrib->getType()==XMLAttDef::ID)
            {
                if (docImpl->fNodeIDMap == 0)
                    docImpl->fNodeIDMap = new (fMemoryManager) NodeIDMap(500, fMemoryManager);
                docImpl->fNodeIDMap->add(attr);
                attr->isIdAttr(true);
            }
        }
    }

    fCurrentParent.appendChild(elem);

    fNodeStack->push(fCurrentParent);
    fCurrentParent = elem;
    fCurrentNode = elem;
    fWithinElement = true;

    // If an empty element, do end right now (no endElement() will be called)
    if (isEmpty)
        endElement(elemDecl, urlId, isRoot, elemPrefix);
}


void DOMParser::startEntityReference(const XMLEntityDecl& entDecl)
{
    if (fCreateEntityReferenceNodes == true)
    {
		DOMString entName(entDecl.getName());
        DOM_EntityReference er = fDocument.createEntityReference(entName);

        //set the readOnly flag to false before appending node, will be reset in endEntityReference
        er.fImpl->setReadOnly(false, true);

        fCurrentParent.appendChild(er);
        fNodeStack->push(fCurrentParent);
        fCurrentParent = er;
        fCurrentNode = er;

    }
}


void DOMParser::XMLDecl(const   XMLCh* const version
                        , const XMLCh* const encoding
                        , const XMLCh* const standalone
                        , const XMLCh* const /*actualEncStr*/)
{
    //This is a non-standard extension to creating XMLDecl type nodes and attching to DOM Tree
    // currently this flag it set to false unless user explicitly asks for it
    // Needs to be revisited after W3C specs are laid out on this issue.

    if (fToCreateXMLDeclTypeNode) {

        DOMString ver(version);
        DOMString enc(encoding);
        DOMString isStd(standalone);
        DOM_XMLDecl xmlDecl = fDocument.createXMLDecl(ver, enc, isStd);

        fCurrentParent.appendChild(xmlDecl);
    }
}



// ---------------------------------------------------------------------------
//  DOMParser: Deprecated methods
// ---------------------------------------------------------------------------
bool DOMParser::getDoValidation() const
{
    //
    //  We don't want to tie the public parser classes to the enum used
    //  by the scanner, so we use a separate one and map.
    //
    //  DON'T mix the new and old methods!!
    //
    const XMLScanner::ValSchemes scheme = fScanner->getValidationScheme();
    if (scheme == XMLScanner::Val_Always)
        return true;
    return false;
}

void DOMParser::setDoValidation(const bool newState)
{
    fScanner->setDoValidation
    (
        newState ? XMLScanner::Val_Always : XMLScanner::Val_Never
    );
}

//doctypehandler interfaces
void DOMParser::attDef
(
    const   DTDElementDecl&     elemDecl
    , const DTDAttDef&          attDef
    , const bool                /*ignoring*/
	)
{	
    if (fDocumentType->isIntSubsetReading())
    {
        DOMString attString;
        if (elemDecl.hasAttDefs())
        {
            attString.appendData(chOpenAngle);
            attString.appendData(chBang);
            attString.appendData(XMLUni::fgAttListString);
            attString.appendData(chSpace);
            attString.appendData(elemDecl.getFullName());

            attString.appendData(chSpace);
            attString.appendData(attDef.getFullName());

            // Get the type and display it
            const XMLAttDef::AttTypes type = attDef.getType();
            switch(type)
            {
            case XMLAttDef::CData :
                attString.appendData(chSpace);
                attString.appendData(XMLUni::fgCDATAString);
                break;
            case XMLAttDef::ID :
                attString.appendData(chSpace);
                attString.appendData(XMLUni::fgIDString);
                break;
            case XMLAttDef::IDRef :
                attString.appendData(chSpace);
                attString.appendData(XMLUni::fgIDRefString);
                break;
            case XMLAttDef::IDRefs :
                attString.appendData(chSpace);
                attString.appendData(XMLUni::fgIDRefsString);
                break;
            case XMLAttDef::Entity :
                attString.appendData(chSpace);
                attString.appendData(XMLUni::fgEntityString);
                break;
            case XMLAttDef::Entities :
                attString.appendData(chSpace);
                attString.appendData(XMLUni::fgEntitiesString);
                break;
            case XMLAttDef::NmToken :
                attString.appendData(chSpace);
                attString.appendData(XMLUni::fgNmTokenString);
                break;
            case XMLAttDef::NmTokens :
                attString.appendData(chSpace);
                attString.appendData(XMLUni::fgNmTokensString);
                break;

            case XMLAttDef::Notation :
                attString.appendData(chSpace);
                attString.appendData(XMLUni::fgNotationString);
                break;

            case XMLAttDef::Enumeration :
                {
                    attString.appendData(chSpace);
                    //  attString.appendData(XMLUni::fgEnumerationString);
                    const XMLCh* enumString = attDef.getEnumeration();
                    int length = XMLString::stringLen(enumString);
                    if (length > 0) {

                        DOMString anotherEnumString;

                        anotherEnumString.appendData(chOpenParen );
                        for(int i=0; i<length; i++) {
                            if (enumString[i] == chSpace)
                                anotherEnumString.appendData(chPipe);
                            else
                                anotherEnumString.appendData(enumString[i]);
                        }
                        anotherEnumString.appendData(chCloseParen);
                        attString.appendData(anotherEnumString);
                    }
                }
                break;
            default:
                // remaining types don't belong to a DTD
                break;
            }
            //get te default types of the attlist
            const XMLAttDef::DefAttTypes def = attDef.getDefaultType();
            switch(def)
            {
            case XMLAttDef::Required :
                attString.appendData(chSpace);
                attString.appendData(XMLUni::fgRequiredString);
                break;
            case XMLAttDef::Implied :
                attString.appendData(chSpace);
                attString.appendData(XMLUni::fgImpliedString);
                break;
            case XMLAttDef::Fixed :
                attString.appendData(chSpace);
                attString.appendData(XMLUni::fgFixedString);
                break;
            default:
                // remaining types don't belong to a DTD
                break;
            }

            const XMLCh* defaultValue = attDef.getValue();
            if (defaultValue != 0) {
                attString.appendData(chSpace);
                attString.appendData(chDoubleQuote);
                attString.appendData(defaultValue);
                attString.appendData(chDoubleQuote);
            }

            attString.appendData(chCloseAngle);
            fDocumentType->internalSubset.appendData(attString);
        }
    }
}

void DOMParser::doctypeComment
(
    const   XMLCh* const    comment
)
{
    if (fDocumentType->isIntSubsetReading())
    {
        if (comment != 0)
        {
            DOMString comString;
            comString.appendData(XMLUni::fgCommentString);
            comString.appendData(chSpace);
            comString.appendData(comment);
            comString.appendData(chSpace);
            comString.appendData(chDash);
            comString.appendData(chDash);
            comString.appendData(chCloseAngle);
            fDocumentType->internalSubset.appendData(comString);
        }
    }
}

void DOMParser::doctypeDecl
(
    const   DTDElementDecl& elemDecl
    , const XMLCh* const    publicId
    , const XMLCh* const    systemId
    , const bool            /*hasIntSubset*/
    , const bool            /*hasExtSubset*/
)
{
	DOM_DocumentType dt;
	dt = fDocument.getImplementation().createDocumentType(elemDecl.getFullName(), publicId, systemId);
    fDocumentType = (DocumentTypeImpl*)dt.fImpl;
	((DocumentImpl*)fDocument.fImpl)->setDocumentType(fDocumentType);

}

void DOMParser::doctypePI
(
    const   XMLCh* const    target
    , const XMLCh* const    data
)
{
    if (fDocumentType->isIntSubsetReading())
	{
		//add these chars to internalSubset variable
        DOMString pi;
        pi.appendData(chOpenAngle);
        pi.appendData(chQuestion);
        pi.appendData(target);
        pi.appendData(chSpace);
        pi.appendData(data);
        pi.appendData(chQuestion);
        pi.appendData(chCloseAngle);

		fDocumentType->internalSubset.appendData(pi);
	}
	
}


void DOMParser::doctypeWhitespace
(
    const   XMLCh* const    chars
    , const unsigned int    /*length*/
)
{
    if (fDocumentType->isIntSubsetReading())
		fDocumentType->internalSubset.appendData(chars);
}

void DOMParser::elementDecl
(
    const   DTDElementDecl& decl
    , const bool            /*isIgnored*/
)
{
    if (fDocumentType->isIntSubsetReading())
	{
        DOMString elemDecl;

        elemDecl.appendData(chOpenAngle);
        elemDecl.appendData(chBang);
        elemDecl.appendData(XMLUni::fgElemString);
        elemDecl.appendData(chSpace);
        elemDecl.appendData(decl.getFullName());

        //get the ContentSpec information
        const XMLCh* contentModel = decl.getFormattedContentModel();
        if (contentModel != 0) {
            elemDecl.appendData(chSpace);
            elemDecl.appendData(contentModel);
        }

        elemDecl.appendData(chCloseAngle);
		fDocumentType->internalSubset.appendData(elemDecl);
	}
}

void DOMParser::endAttList
(
    const   DTDElementDecl& elemDecl
)
{
	// this section sets up default attributes.
	// default attribute nodes are stored in a NamedNodeMap DocumentTypeImpl::elements
	// default attribute data attached to the document is used to conform to the
	// DOM spec regarding creating element nodes & removing attributes with default values
	// see DocumentTypeImpl
	if (elemDecl.hasAttDefs())
	{		
		XMLAttDefList* defAttrs = &elemDecl.getAttDefList();
		XMLAttDef* attr = 0;
		AttrImpl* insertAttr = 0;
		DOM_Element dom_elem = fDocument.createElement(elemDecl.getFullName());
		ElementImpl* elem = (ElementImpl*)(dom_elem.fImpl);

        for(unsigned int i=0; i<defAttrs->getAttDefCount(); i++)
        {
            attr = &defAttrs->getAttDef(i);
            if (attr->getValue() != null)
            {
                if (fScanner->getDoNamespaces())
                {
                    // DOM Level 2 wants all namespace declaration attributes
                    // to be bound to "http://www.w3.org/2000/xmlns/"
                    // So as long as the XML parser doesn't do it, it needs to
                    // done here.
                    DOMString qualifiedName = attr->getFullName();
                    int index = DocumentImpl::indexofQualifiedName(qualifiedName);

                    XMLBuffer buf(1023, fMemoryManager);
                    static const XMLCh XMLNS[] = {
                        chLatin_x, chLatin_m, chLatin_l, chLatin_n, chLatin_s, chNull};

                    if (index > 0) {
                        // there is prefix
                        // map to XML URI for all cases except when prefix == "xmlns"
                        DOMString prefix = qualifiedName.substringData(0, index);

                        if (prefix.equals(XMLNS))
                            buf.append(XMLUni::fgXMLNSURIName);
                        else
                            buf.append(XMLUni::fgXMLURIName);
                    }
                    else {
                        //   No prefix
                        if (qualifiedName.equals(XMLNS))
                            buf.append(XMLUni::fgXMLNSURIName);
                    }

                    insertAttr = new (fMemoryManager) AttrNSImpl((DocumentImpl*)fDocument.fImpl,
                       DOMString(buf.getRawBuffer()),     // NameSpaceURI
                       qualifiedName);   // qualified name

                }
                else
                {
                    // Namespaces is turned off...
                    insertAttr = new (fMemoryManager) AttrImpl((DocumentImpl*)fDocument.fImpl, attr->getFullName());
                }
                insertAttr->setValue(attr->getValue());
                // memory leak here
                AttrImpl * previousAttr = elem->setAttributeNode(insertAttr);
				if ( previousAttr != 0 && previousAttr->nodeRefCount ==0)
					NodeImpl::deleteIf(previousAttr);

                insertAttr->setSpecified(false);
            }
        }
        ElementImpl *previousElem = (ElementImpl *)
                fDocumentType->getElements()->setNamedItem( elem );

        //
        //  If this new element is replacing an element node that was already
        //    in the element named node map, we need to delete the original
        //    element node, assuming no-one else was referencing it.
        //
        if (previousElem != 0 && previousElem->nodeRefCount == 0)
            NodeImpl::deleteIf(previousElem);
    }
}

void DOMParser::endIntSubset()
{
	fDocumentType->intSubsetReading = false;
}

void DOMParser::endExtSubset()
{
}

void DOMParser::entityDecl
(
    const   DTDEntityDecl&  entityDecl
    , const bool            /*isPEDecl*/
    , const bool            /*isIgnored*/
)
{
	EntityImpl* entity = ((DocumentImpl*)fDocument.fImpl)->createEntity(entityDecl.getName());

	entity->setPublicId(entityDecl.getPublicId());
	entity->setSystemId(entityDecl.getSystemId());
	entity->setNotationName(entityDecl.getNotationName());

    EntityImpl *previousDef = (EntityImpl *)
	    fDocumentType->entities->setNamedItem( entity );

    //
    //  If this new entity node is replacing an entity node that was already
    //    in the entities named node map (happens if documents redefine the
    //    predefined entited such as lt), we need to delete the original
    //    entitiy node, assuming no-one else was referencing it.
    //
    if (previousDef != 0 && previousDef->nodeRefCount == 0)
    	        NodeImpl::deleteIf(previousDef);


	if (fDocumentType->isIntSubsetReading())
	{
		//add thes chars to internalSubset variable
		DOMString entityName;
		entityName.appendData(chOpenAngle);
        entityName.appendData(chBang);
		entityName.appendData(XMLUni::fgEntityString);
        entityName.appendData(chSpace);

        entityName.appendData(entityDecl.getName());
        DOMString id = entity->getPublicId();
        if (id != 0) {
            entityName.appendData(chSpace);
            entityName.appendData(XMLUni::fgPubIDString);
            entityName.appendData(chSpace);
            entityName.appendData(chDoubleQuote);
            entityName.appendData(id);
            entityName.appendData(chDoubleQuote);
        }
        id = entity->getSystemId();
        if (id != 0) {
            entityName.appendData(chSpace);
            entityName.appendData(XMLUni::fgSysIDString);
            entityName.appendData(chSpace);
            entityName.appendData(chDoubleQuote);
            entityName.appendData(id);
            entityName.appendData(chDoubleQuote);

        }
        id = entity->getNotationName();
        if (id != 0) {
            entityName.appendData(chSpace);
            entityName.appendData(XMLUni::fgNDATAString);
            entityName.appendData(chSpace);
            entityName.appendData(chDoubleQuote);
            entityName.appendData(id);
            entityName.appendData(chDoubleQuote);
        }
        id = entityDecl.getValue();
        if (id !=0) {
            entityName.appendData(chSpace);
            entityName.appendData(chDoubleQuote);
            entityName.appendData(id);
            entityName.appendData(chDoubleQuote);
        }

        entityName.appendData(chCloseAngle);
        fDocumentType->internalSubset.appendData(entityName);
    }

}

void DOMParser::resetDocType()
{
	fDocumentType = null;
}

void DOMParser::notationDecl
(
    const   XMLNotationDecl&    notDecl
    , const bool                /*isIgnored*/
)
{
	NotationImpl* notation = ((DocumentImpl*)fDocument.fImpl)->createNotation(notDecl.getName());
	notation->setPublicId(notDecl.getPublicId());
	notation->setSystemId(notDecl.getSystemId());

    NotationImpl *previousNot = (NotationImpl *)
       fDocumentType->notations->setNamedItem( notation );

    //
    //  If this new notation is replacing a notation node that was already
    //    in the notation named node map, we need to delete the original
    //    notation node, assuming no-one else was referencing it.
    //
    if (previousNot != 0 && previousNot->nodeRefCount == 0)
        NodeImpl::deleteIf(previousNot);

}

void DOMParser::startAttList
(
    const   DTDElementDecl& /*elemDecl*/
)
{
}

void DOMParser::startIntSubset()
{
	fDocumentType->intSubsetReading = true;
}

void DOMParser::startExtSubset()
{
}

void DOMParser::TextDecl
(
    const   XMLCh* const    /*versionStr*/
    , const XMLCh* const    /*encodingStr*/
)
{
}


// ---------------------------------------------------------------------------
//  DOMParser: Grammar preparsing methods
// ---------------------------------------------------------------------------
Grammar* DOMParser::loadGrammar(const char* const systemId,
                                const short grammarType,
                                const bool toCache)
{
    // Avoid multiple entrance
    if (fParseInProgress)
        ThrowXMLwithMemMgr(IOException, XMLExcepts::Gen_ParseInProgress, fMemoryManager);

    Grammar* grammar = 0;
    try
    {
        fParseInProgress = true;
        grammar = fScanner->loadGrammar(systemId, grammarType, toCache);
        fParseInProgress = false;
    }
    catch(const OutOfMemoryException&)
    {
        throw;
    }    catch(...)
    {
        fParseInProgress = false;
        throw;
    }

    return grammar;
}

Grammar* DOMParser::loadGrammar(const XMLCh* const systemId,
                                const short grammarType,
                                const bool toCache)
{
    // Avoid multiple entrance
    if (fParseInProgress)
        ThrowXMLwithMemMgr(IOException, XMLExcepts::Gen_ParseInProgress, fMemoryManager);

    Grammar* grammar = 0;
    try
    {
        fParseInProgress = true;
        grammar = fScanner->loadGrammar(systemId, grammarType, toCache);
        fParseInProgress = false;
    }
    catch(const OutOfMemoryException&)
    {
        throw;
    }
    catch(...)
    {
        fParseInProgress = false;
        throw;
    }

    return grammar;
}

Grammar* DOMParser::loadGrammar(const InputSource& source,
                                const short grammarType,
                                const bool toCache)
{
    // Avoid multiple entrance
    if (fParseInProgress)
        ThrowXMLwithMemMgr(IOException, XMLExcepts::Gen_ParseInProgress, fMemoryManager);

   Grammar* grammar = 0;
    try
    {
        fParseInProgress = true;
        grammar = fScanner->loadGrammar(source, grammarType, toCache);
        fParseInProgress = false;
    }
    catch(const OutOfMemoryException&)
    {
        throw;
    }
    catch(...)
    {
        fParseInProgress = false;
        throw;
    }

    return grammar;
}

void DOMParser::resetCachedGrammarPool()
{
    fGrammarResolver->resetCachedGrammar();
}

XERCES_CPP_NAMESPACE_END

