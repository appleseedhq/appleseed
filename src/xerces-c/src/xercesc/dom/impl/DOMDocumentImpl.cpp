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

/*
 * $Id: DOMDocumentImpl.cpp 568078 2007-08-21 11:43:25Z amassari $
 */
#include "DOMDocumentImpl.hpp"
#include "DOMCasts.hpp"
#include "DOMConfigurationImpl.hpp"
#include "DOMDocumentTypeImpl.hpp"
#include "DOMAttrImpl.hpp"
#include "DOMAttrNSImpl.hpp"
#include "DOMCDATASectionImpl.hpp"
#include "DOMCommentImpl.hpp"
#include "DOMDeepNodeListImpl.hpp"
#include "DOMDocumentFragmentImpl.hpp"
#include "DOMElementImpl.hpp"
#include "XSDElementNSImpl.hpp"
#include "DOMEntityImpl.hpp"
#include "DOMEntityReferenceImpl.hpp"
#include "DOMNormalizer.hpp"
#include "DOMNotationImpl.hpp"
#include "DOMProcessingInstructionImpl.hpp"
#include "DOMTextImpl.hpp"
#include "DOMStringPool.hpp"
#include "DOMTreeWalkerImpl.hpp"
#include "DOMNodeIteratorImpl.hpp"
#include "DOMNodeIDMap.hpp"
#include "DOMRangeImpl.hpp"
#include "DOMTypeInfoImpl.hpp"

#include <xercesc/dom/DOMImplementation.hpp>
#include <xercesc/util/XMLChar.hpp>
#include <xercesc/framework/MemoryManager.hpp>
#include <xercesc/util/OutOfMemoryException.hpp>

XERCES_CPP_NAMESPACE_BEGIN

// The chunk size to allocate from the system allocator.
static const XMLSize_t kInitialHeapAllocSize =  0x4000;
static const XMLSize_t kMaxHeapAllocSize     = 0x20000;
static const XMLSize_t kMaxSubAllocationSize =  0x1000;  // Any request for more bytes
                                                         // than this will be handled by
                                                         // allocating directly with system.


//
//   Constructors.   Warning - be very careful with the ordering of initialization
//                             of the heap.  Ordering depends on the order of declaration
//                             in the .hpp file, not on the order of initializers here
//                             in the constructor.  The heap declaration can not be
//                             first - fNode and fParent must be first for the casting
//                             functions in DOMCasts to work correctly.  This means that
//                             fNode and fParent constructors used here can not
//                             allocate.
//
DOMDocumentImpl::DOMDocumentImpl(MemoryManager* const manager)
    : fNode(this),
      fParent(this),
      fNodeIDMap(0),
      fActualEncoding(0),
      fEncoding(0),
      fStandalone(false),
      fVersion(0),
      fDocumentURI(0),
      fDOMConfiguration(0),
      fUserDataTableKeys(17, manager),
      fUserDataTable(0),
      fCurrentBlock(0),      
      fFreePtr(0),
      fFreeBytesRemaining(0),
      fHeapAllocSize(kInitialHeapAllocSize),
      fRecycleNodePtr(0),
      fRecycleBufferPtr(0),
      fNodeListPool(0),
      fDocType(0),
      fDocElement(0),
      fNamePool(0),
      fNormalizer(0),      
      fRanges(0),
      fNodeIterators(0),
      fMemoryManager(manager),
      fChanges(0),      
      errorChecking(true)
{
    fNamePool    = new (this) DOMStringPool(257, this);
}


//DOM Level 2
DOMDocumentImpl::DOMDocumentImpl(const XMLCh *fNamespaceURI,
                               const XMLCh *qualifiedName,
                               DOMDocumentType *doctype,
                               MemoryManager* const manager)
    : fNode(this),
      fParent(this),
      fNodeIDMap(0),
      fActualEncoding(0),
      fEncoding(0),
      fStandalone(false),
      fVersion(0),
      fDocumentURI(0),
      fDOMConfiguration(0),
      fUserDataTableKeys(17, manager),
      fUserDataTable(0),
      fCurrentBlock(0),      
      fFreePtr(0),
      fFreeBytesRemaining(0),
      fHeapAllocSize(kInitialHeapAllocSize),
      fRecycleNodePtr(0),
      fRecycleBufferPtr(0),
      fNodeListPool(0),
      fDocType(0),
      fDocElement(0),
      fNamePool(0),
      fNormalizer(0),      
      fRanges(0),
      fNodeIterators(0),
      fMemoryManager(manager),
      fChanges(0),
      errorChecking(true)
{
    fNamePool    = new (this) DOMStringPool(257, this);
    try {
        setDocumentType(doctype);

        if (qualifiedName)
            appendChild(createElementNS(fNamespaceURI, qualifiedName));  //root element
        else if (fNamespaceURI)
            throw DOMException(DOMException::NAMESPACE_ERR, 0, getMemoryManager());
    }
    catch(const OutOfMemoryException&)
    {
        throw;
    }
    catch (...) {
        this->deleteHeap();
        throw;
    }
}

void DOMDocumentImpl::setDocumentType(DOMDocumentType *doctype)
{
    if (!doctype)
        return;

    // New doctypes can be created either with the factory methods on DOMImplementation, in
    //   which case ownerDocument will be 0, or with methods on DocumentImpl, in which case
    //   ownerDocument will be set, but the DocType won't yet be a child of the document.
    if (doctype->getOwnerDocument() != 0 && doctype->getOwnerDocument() != this)
        throw DOMException(    //one doctype can belong to only one DOMDocumentImpl
        DOMException::WRONG_DOCUMENT_ERR, 0, getMemoryManager());

    DOMDocumentTypeImpl* doctypeImpl = (DOMDocumentTypeImpl*) doctype;
    doctypeImpl->setOwnerDocument(this);

    // The doctype can not have any Entities or Notations yet, because they can not
    //   be created except through factory methods on a document.

    // revisit.  What if this doctype is already a child of the document?
    appendChild(doctype);

}

DOMDocumentImpl::~DOMDocumentImpl()
{
    //  Clean up the fNodeListPool
    if (fNodeListPool)
        fNodeListPool->cleanup();

    if (fRanges)
        delete fRanges; //fRanges->cleanup();

    if (fNodeIterators)
        delete fNodeIterators;//fNodeIterators->cleanup();

    if (fUserDataTable)
        delete fUserDataTable;//fUserDataTable->cleanup();

    if (fRecycleNodePtr) {
        fRecycleNodePtr->deleteAllElements();
        delete fRecycleNodePtr;
    }

    if (fRecycleBufferPtr) {
        delete fRecycleBufferPtr;
    }

    delete fNormalizer;

    //  Delete the heap for this document.  This uncerimoniously yanks the storage
    //      out from under all of the nodes in the document.  Destructors are NOT called.
    this->deleteHeap();
}


DOMNode *DOMDocumentImpl::cloneNode(bool deep) const {

    // Note:  the cloned document node goes on the same heap we live in.
    DOMDocumentImpl *newdoc = new (fMemoryManager) DOMDocumentImpl(fMemoryManager);
    if(fEncoding && *fEncoding)
        newdoc->setEncoding(fEncoding);
    if(fVersion && *fVersion)
        newdoc->setVersion(fVersion);
    newdoc->setStandalone(fStandalone);

    // then the children by _importing_ them
    if (deep)
        for (DOMNode *n = this->getFirstChild(); n != 0; n = n->getNextSibling()) {
            newdoc->appendChild(newdoc->importNode(n, true, true));
    }

    fNode.callUserDataHandlers(DOMUserDataHandler::NODE_CLONED, this, newdoc);
    return newdoc;
}


const XMLCh * DOMDocumentImpl::getNodeName() const {
    static const XMLCh nam[] =  // "#document"
        {chPound, chLatin_d, chLatin_o, chLatin_c, chLatin_u, chLatin_m, chLatin_e, chLatin_n, chLatin_t, 0};
    return nam;
}


short DOMDocumentImpl::getNodeType() const {
    return DOMNode::DOCUMENT_NODE;
}


// even though ownerDocument refers to this in this implementation
// the DOM Level 2 spec says it must be 0, so make it appear so
DOMDocument * DOMDocumentImpl::getOwnerDocument() const {
    return 0;
}


DOMAttr *DOMDocumentImpl::createAttribute(const XMLCh *nam)
{
    if(!nam || !isXMLName(nam))
        throw DOMException(DOMException::INVALID_CHARACTER_ERR,0, getMemoryManager());
    return new (this, DOMDocumentImpl::ATTR_OBJECT) DOMAttrImpl(this,nam);
}



DOMCDATASection *DOMDocumentImpl::createCDATASection(const XMLCh *data) {
    return new (this, DOMDocumentImpl::CDATA_SECTION_OBJECT) DOMCDATASectionImpl(this,data);
}



DOMComment *DOMDocumentImpl::createComment(const XMLCh *data)
{
    return new (this, DOMDocumentImpl::COMMENT_OBJECT) DOMCommentImpl(this, data);
}



DOMDocumentFragment *DOMDocumentImpl::createDocumentFragment()
{
    return new (this, DOMDocumentImpl::DOCUMENT_FRAGMENT_OBJECT) DOMDocumentFragmentImpl(this);
}



DOMDocumentType *DOMDocumentImpl::createDocumentType(const XMLCh *nam)
{
    if (!nam || !isXMLName(nam))
        throw DOMException(
        DOMException::INVALID_CHARACTER_ERR, 0, getMemoryManager());

    return new (this, DOMDocumentImpl::DOCUMENT_TYPE_OBJECT) DOMDocumentTypeImpl(this, nam, false);
}



DOMDocumentType *
    DOMDocumentImpl::createDocumentType(const XMLCh *qualifiedName,
                                     const XMLCh *publicId,
                                     const XMLCh *systemId)
{
    if (!qualifiedName || !isXMLName(qualifiedName))
        throw DOMException(
        DOMException::INVALID_CHARACTER_ERR, 0, getMemoryManager());

    return new (this, DOMDocumentImpl::DOCUMENT_TYPE_OBJECT) DOMDocumentTypeImpl(this, qualifiedName, publicId, systemId, false);
}



DOMElement *DOMDocumentImpl::createElement(const XMLCh *tagName)
{
    if(!tagName || !isXMLName(tagName))
        throw DOMException(DOMException::INVALID_CHARACTER_ERR,0, getMemoryManager());

    return new (this, DOMDocumentImpl::ELEMENT_OBJECT) DOMElementImpl(this,tagName);
}


DOMElement *DOMDocumentImpl::createElementNoCheck(const XMLCh *tagName)
{
    return new (this, DOMDocumentImpl::ELEMENT_OBJECT) DOMElementImpl(this, tagName);
}




DOMEntity *DOMDocumentImpl::createEntity(const XMLCh *nam)
{
    if (!nam || !isXMLName(nam))
        throw DOMException(
        DOMException::INVALID_CHARACTER_ERR, 0, getMemoryManager());

    return new (this, DOMDocumentImpl::ENTITY_OBJECT) DOMEntityImpl(this, nam);
}



DOMEntityReference *DOMDocumentImpl::createEntityReference(const XMLCh *nam)
{
    if (!nam || !isXMLName(nam))
        throw DOMException(
        DOMException::INVALID_CHARACTER_ERR, 0, getMemoryManager());

    return new (this, DOMDocumentImpl::ENTITY_REFERENCE_OBJECT) DOMEntityReferenceImpl(this, nam);
}

DOMEntityReference *DOMDocumentImpl::createEntityReferenceByParser(const XMLCh *nam)
{
    if (!nam || !isXMLName(nam))
        throw DOMException(
        DOMException::INVALID_CHARACTER_ERR, 0, getMemoryManager());

    return new (this, DOMDocumentImpl::ENTITY_REFERENCE_OBJECT) DOMEntityReferenceImpl(this, nam, false);
}

DOMNotation *DOMDocumentImpl::createNotation(const XMLCh *nam)
{
    if (!nam || !isXMLName(nam))
        throw DOMException(
        DOMException::INVALID_CHARACTER_ERR, 0, getMemoryManager());

    return new (this, DOMDocumentImpl::NOTATION_OBJECT) DOMNotationImpl(this, nam);
}



DOMProcessingInstruction *DOMDocumentImpl::createProcessingInstruction(
                                          const XMLCh *target, const XMLCh *data)
{
    if(!target || !isXMLName(target))
        throw DOMException(DOMException::INVALID_CHARACTER_ERR,0, getMemoryManager());
    return new (this, DOMDocumentImpl::PROCESSING_INSTRUCTION_OBJECT) DOMProcessingInstructionImpl(this,target,data);
}




DOMText *DOMDocumentImpl::createTextNode(const XMLCh *data)
{
    return new (this, DOMDocumentImpl::TEXT_OBJECT) DOMTextImpl(this,data);
}


DOMNodeIterator* DOMDocumentImpl::createNodeIterator (
          DOMNode *root, unsigned long whatToShow, DOMNodeFilter* filter, bool entityReferenceExpansion)
{
    if (!root) {
        throw DOMException(DOMException::NOT_SUPPORTED_ERR, 0, getMemoryManager());
        return 0;
    }

    DOMNodeIteratorImpl* nodeIterator = new (this) DOMNodeIteratorImpl(this, root, whatToShow, filter, entityReferenceExpansion);

    if (fNodeIterators == 0L) {
        //fNodeIterators = new (this) NodeIterators(1, false);
        fNodeIterators = new (fMemoryManager) NodeIterators(1, false, fMemoryManager);
    }
    fNodeIterators->addElement(nodeIterator);

    return nodeIterator;
}


NodeIterators* DOMDocumentImpl::getNodeIterators() const
{
    return fNodeIterators;
}

void DOMDocumentImpl::removeNodeIterator(DOMNodeIteratorImpl* nodeIterator)
{
    if (fNodeIterators != 0) {
        XMLSize_t sz = fNodeIterators->size();
        if (sz !=0) {
            for (XMLSize_t i =0; i<sz; i++) {
                if (fNodeIterators->elementAt(i) == nodeIterator) {
                    fNodeIterators->removeElementAt(i);
                    break;
                }
            }
        }
    }
}


const DOMXPathExpression* DOMDocumentImpl::createExpression(const XMLCh *, const DOMXPathNSResolver *)
{
    throw DOMException(DOMException::NOT_SUPPORTED_ERR, 0, getMemoryManager());
    return 0;
}

const DOMXPathNSResolver* DOMDocumentImpl::createNSResolver(DOMNode *)

{
    throw DOMException(DOMException::NOT_SUPPORTED_ERR, 0, getMemoryManager());
    return 0;
}

void* DOMDocumentImpl::evaluate(const XMLCh *, DOMNode *, const DOMXPathNSResolver *, 
                           unsigned short, void* ) 
{
    throw DOMException(DOMException::NOT_SUPPORTED_ERR, 0, getMemoryManager());
    return 0;
}



DOMTreeWalker* DOMDocumentImpl::createTreeWalker (DOMNode *root, unsigned long whatToShow, DOMNodeFilter* filter, bool entityReferenceExpansion)
{
    if (!root) {
        throw DOMException(DOMException::NOT_SUPPORTED_ERR, 0, getMemoryManager());
        return 0;
    }

    return new (this) DOMTreeWalkerImpl(root, whatToShow, filter, entityReferenceExpansion);
}




DOMDocumentType *DOMDocumentImpl::getDoctype() const
{
    return fDocType;
}



DOMElement *DOMDocumentImpl::getDocumentElement() const
{
    return fDocElement;
}



DOMNodeList *DOMDocumentImpl::getElementsByTagName(const XMLCh *tagname) const
{
    // cast off the const of this because we will update the fNodeListPool
    return ((DOMDocumentImpl*)this)->getDeepNodeList(this,tagname);
}


DOMImplementation   *DOMDocumentImpl::getImplementation() const {
    return DOMImplementation::getImplementation();
}


DOMNode *DOMDocumentImpl::insertBefore(DOMNode *newChild, DOMNode *refChild)
{
    // Only one such child permitted
    if(
        (newChild->getNodeType() == DOMNode::ELEMENT_NODE  && fDocElement!=0)
        ||
        (newChild->getNodeType() == DOMNode::DOCUMENT_TYPE_NODE  && fDocType!=0)
        )
        throw DOMException(DOMException::HIERARCHY_REQUEST_ERR,0, getMemoryManager());

    // if the newChild is a documenttype node created from domimplementation, set the ownerDoc first
    if ((newChild->getNodeType() == DOMNode::DOCUMENT_TYPE_NODE) && !newChild->getOwnerDocument())
        ((DOMDocumentTypeImpl*)newChild)->setOwnerDocument(this);

    fParent.insertBefore(newChild,refChild);

    // If insert succeeded, cache the kid appropriately
    if(newChild->getNodeType() == DOMNode::ELEMENT_NODE)
        fDocElement=(DOMElement *)newChild;
    else if(newChild->getNodeType() == DOMNode::DOCUMENT_TYPE_NODE)
        fDocType=(DOMDocumentType *)newChild;

    return newChild;
}


DOMNode* DOMDocumentImpl::replaceChild(DOMNode *newChild, DOMNode *oldChild) {
    DOMDocumentType* tempDocType = fDocType;
    DOMElement* tempDocElement = fDocElement;

    if(oldChild->getNodeType() == DOMNode::DOCUMENT_TYPE_NODE)
        fDocType=0;
    else if(oldChild->getNodeType() == DOMNode::ELEMENT_NODE)
        fDocElement=0;

    try {
        insertBefore(newChild, oldChild);
        // changed() already done.

        if((oldChild->getNodeType() == DOMNode::DOCUMENT_TYPE_NODE)
        || (oldChild->getNodeType() == DOMNode::ELEMENT_NODE))
            return fParent.removeChild(oldChild);
        else
            return removeChild(oldChild);
    }
    catch(const OutOfMemoryException&)
    {
        throw;
    }
    catch(...) {
        fDocType = tempDocType;
        fDocElement = tempDocElement;
        throw;
    }
}

bool DOMDocumentImpl::isXMLName(const XMLCh *s)
{
    if (XMLString::equals(fVersion, XMLUni::fgVersion1_1))
        return XMLChar1_1::isValidName(s);
    else
        return XMLChar1_0::isValidName(s);
}




DOMNode *DOMDocumentImpl::removeChild(DOMNode *oldChild)
{
    fParent.removeChild(oldChild);

    // If remove succeeded, un-cache the kid appropriately
    if(oldChild->getNodeType() == DOMNode::ELEMENT_NODE)
        fDocElement=0;
    else if(oldChild->getNodeType() == DOMNode::DOCUMENT_TYPE_NODE)
        fDocType=0;

    return oldChild;
}



void DOMDocumentImpl::setNodeValue(const XMLCh *x)
{
    fNode.setNodeValue(x);
}


//Introduced in DOM Level 2
DOMNode *DOMDocumentImpl::importNode(DOMNode *source, bool deep)
{
    return importNode(source, deep, false);
}


DOMElement *DOMDocumentImpl::createElementNS(const XMLCh *fNamespaceURI,
    const XMLCh *qualifiedName)
{
    if(!qualifiedName || !isXMLName(qualifiedName))
        throw DOMException(DOMException::INVALID_CHARACTER_ERR,0, getMemoryManager());
    //XMLCh * pooledTagName = this->fNamePool->getPooledString(qualifiedName);
    return new (this, DOMDocumentImpl::ELEMENT_NS_OBJECT) DOMElementNSImpl(this, fNamespaceURI, qualifiedName);
}

DOMElement *DOMDocumentImpl::createElementNS(const XMLCh *fNamespaceURI,
                                              const XMLCh *qualifiedName,
                                              const XMLSSize_t lineNo,
                                              const XMLSSize_t columnNo)
{
    if(!qualifiedName || !isXMLName(qualifiedName))
        throw DOMException(DOMException::INVALID_CHARACTER_ERR,0, getMemoryManager());

    return new (this) XSDElementNSImpl(this, fNamespaceURI, qualifiedName, lineNo, columnNo);
}


DOMAttr *DOMDocumentImpl::createAttributeNS(const XMLCh *fNamespaceURI,
    const XMLCh *qualifiedName)
{
    if(!qualifiedName || !isXMLName(qualifiedName))
        throw DOMException(DOMException::INVALID_CHARACTER_ERR,0, getMemoryManager());
    return new (this, DOMDocumentImpl::ATTR_NS_OBJECT) DOMAttrNSImpl(this, fNamespaceURI, qualifiedName);
}


DOMNodeList *DOMDocumentImpl::getElementsByTagNameNS(const XMLCh *fNamespaceURI,
    const XMLCh *fLocalName)  const
{
    // cast off the const of this because we will update the fNodeListPool
    return ((DOMDocumentImpl*)this)->getDeepNodeList(this, fNamespaceURI, fLocalName);
}


DOMElement *DOMDocumentImpl::getElementById(const XMLCh *elementId) const
{
    if (fNodeIDMap == 0)
        return 0;

    DOMAttr *theAttr = fNodeIDMap->find(elementId);
    if (theAttr == 0)
        return 0;

    return theAttr->getOwnerElement();
}


//Return the index > 0 of ':' in the given qualified name qName="prefix:localName".
//Return 0 if there is no ':', or -1 if qName is malformed such as ":abcd" or "abcd:".
int DOMDocumentImpl::indexofQualifiedName(const XMLCh * qName)
{
    int qNameLen = XMLString::stringLen(qName);
    int index = -1, count = 0;
    for (int i = 0; i < qNameLen; ++i) {
        if (qName[i] == chColon) {
            index = i;
            ++count;    //number of ':' found
        }
    }

    if (qNameLen == 0 || count > 1 || index == 0 || index == qNameLen-1)
        return -1;
    return count == 0 ? 0 : index;
}


const XMLCh*     DOMDocumentImpl::getBaseURI() const
{
	  return fDocumentURI;
}


DOMRange* DOMDocumentImpl::createRange()
{

    DOMRangeImpl* range = new (this) DOMRangeImpl(this, fMemoryManager);

    if (fRanges == 0L) {
        //fRanges = new (this) Ranges(1, false);
        fRanges = new (fMemoryManager) Ranges(1, false, fMemoryManager); // XMemory
    }
    fRanges->addElement(range);
    return range;
}

Ranges* DOMDocumentImpl::getRanges() const
{
    return fRanges;
}

void DOMDocumentImpl::removeRange(DOMRangeImpl* range)
{
    if (fRanges != 0) {
        XMLSize_t sz = fRanges->size();
        if (sz !=0) {
            for (XMLSize_t i =0; i<sz; i++) {
                if (fRanges->elementAt(i) == range) {
                    fRanges->removeElementAt(i);
                    break;
                }
            }
        }
    }
}

/** Uses the kidOK lookup table to check whether the proposed
    tree structure is legal.

    ????? It feels like there must be a more efficient solution,
    but for the life of me I can't think what it would be.
*/
bool DOMDocumentImpl::isKidOK(DOMNode *parent, DOMNode *child)
{
      static int kidOK[14];

      if (kidOK[DOMNode::ATTRIBUTE_NODE] == 0)
      {
          kidOK[DOMNode::DOCUMENT_NODE] =
              1 << DOMNode::ELEMENT_NODE |
              1 << DOMNode::PROCESSING_INSTRUCTION_NODE |
              1 << DOMNode::COMMENT_NODE |
              1 << DOMNode::DOCUMENT_TYPE_NODE;

          kidOK[DOMNode::DOCUMENT_FRAGMENT_NODE] =
              kidOK[DOMNode::ENTITY_NODE] =
              kidOK[DOMNode::ENTITY_REFERENCE_NODE] =
              kidOK[DOMNode::ELEMENT_NODE] =
              1 << DOMNode::ELEMENT_NODE |
              1 << DOMNode::PROCESSING_INSTRUCTION_NODE |
              1 << DOMNode::COMMENT_NODE |
              1 << DOMNode::TEXT_NODE |
              1 << DOMNode::CDATA_SECTION_NODE |
              1 << DOMNode::ENTITY_REFERENCE_NODE;

          kidOK[DOMNode::ATTRIBUTE_NODE] =
              1 << DOMNode::TEXT_NODE |
              1 << DOMNode::ENTITY_REFERENCE_NODE;

          kidOK[DOMNode::PROCESSING_INSTRUCTION_NODE] =
              kidOK[DOMNode::COMMENT_NODE] =
              kidOK[DOMNode::TEXT_NODE] =
              kidOK[DOMNode::CDATA_SECTION_NODE] =
              kidOK[DOMNode::NOTATION_NODE] =
              0;
      }
      int p=parent->getNodeType();
      int ch = child->getNodeType();
      return ((kidOK[p] & 1<<ch) != 0) || 
             (p==DOMNode::DOCUMENT_NODE && ch==DOMNode::TEXT_NODE && 
              ((XMLString::equals(((DOMDocument*)parent)->getVersion(), XMLUni::fgVersion1_1))?
                    XMLChar1_1::isAllSpaces(child->getNodeValue(), XMLString::stringLen(child->getNodeValue())):
                    XMLChar1_0::isAllSpaces(child->getNodeValue(), XMLString::stringLen(child->getNodeValue())))
             );
}

void            DOMDocumentImpl::changed()
{
    fChanges++;
}


int             DOMDocumentImpl::changes() const{
    return fChanges;
}



//
//    Delegation for functions inherited from DOMNode
//
           DOMNode*         DOMDocumentImpl::appendChild(DOMNode *newChild)          {return insertBefore(newChild, 0); }
           DOMNamedNodeMap* DOMDocumentImpl::getAttributes() const                   {return fNode.getAttributes (); }
           DOMNodeList*     DOMDocumentImpl::getChildNodes() const                   {return fParent.getChildNodes (); }
           DOMNode*         DOMDocumentImpl::getFirstChild() const                   {return fParent.getFirstChild (); }
           DOMNode*         DOMDocumentImpl::getLastChild() const                    {return fParent.getLastChild (); }
     const XMLCh*           DOMDocumentImpl::getLocalName() const                    {return fNode.getLocalName (); }
     const XMLCh*           DOMDocumentImpl::getNamespaceURI() const                 {return fNode.getNamespaceURI (); }
           DOMNode*         DOMDocumentImpl::getNextSibling() const                  {return fNode.getNextSibling (); }
     const XMLCh*           DOMDocumentImpl::getNodeValue() const                    {return fNode.getNodeValue (); }
     const XMLCh*           DOMDocumentImpl::getPrefix() const                       {return fNode.getPrefix (); }
           DOMNode*         DOMDocumentImpl::getParentNode() const                   {return fNode.getParentNode (); }
           DOMNode*         DOMDocumentImpl::getPreviousSibling() const              {return fNode.getPreviousSibling (); }
           bool             DOMDocumentImpl::hasChildNodes() const                   {return fParent.hasChildNodes (); }
           void             DOMDocumentImpl::normalize()                             {fParent.normalize (); }
           bool             DOMDocumentImpl::isSupported(const XMLCh *feature, const XMLCh *version) const
                                                                                     {return fNode.isSupported (feature, version); }
           void             DOMDocumentImpl::setPrefix(const XMLCh  *prefix)         {fNode.setPrefix(prefix); }
           bool             DOMDocumentImpl::hasAttributes() const                   {return fNode.hasAttributes(); }
           bool             DOMDocumentImpl::isSameNode(const DOMNode* other) const  {return fNode.isSameNode(other);}
           bool             DOMDocumentImpl::isEqualNode(const DOMNode* arg) const   {return fParent.isEqualNode(arg);}
           void*            DOMDocumentImpl::setUserData(const XMLCh* key, void* data, DOMUserDataHandler* handler)
                                                                                     {return fNode.setUserData(key, data, handler); }
           void*            DOMDocumentImpl::getUserData(const XMLCh* key) const     {return fNode.getUserData(key); }
           short            DOMDocumentImpl::compareTreePosition(const DOMNode* other) const {return fNode.compareTreePosition(other); }
           const XMLCh*     DOMDocumentImpl::getTextContent() const                  {return fNode.getTextContent(); }
           void             DOMDocumentImpl::setTextContent(const XMLCh* textContent){fNode.setTextContent(textContent); }
           const XMLCh*     DOMDocumentImpl::lookupNamespacePrefix(const XMLCh* namespaceURI, bool useDefault) const  {return fNode.lookupNamespacePrefix(namespaceURI, useDefault); }
           bool             DOMDocumentImpl::isDefaultNamespace(const XMLCh* namespaceURI) const {return fNode.isDefaultNamespace(namespaceURI); }
           const XMLCh*     DOMDocumentImpl::lookupNamespaceURI(const XMLCh* prefix) const  {return fNode.lookupNamespaceURI(prefix); }
           DOMNode*         DOMDocumentImpl::getInterface(const XMLCh* feature)      {return fNode.getInterface(feature); }




//-----------------------------------------------------------------------
//
//  Per Document Heap and Heap Helper functions
//
//        revisit - this stuff should be a class of its own, rather than
//                       just lying around naked in DocumentImpl.
//
//-----------------------------------------------------------------------

XMLCh * DOMDocumentImpl::cloneString(const XMLCh *src)
{
    if (!src) return 0;
    size_t   len = XMLString::stringLen(src);
    len = (len + 1) * sizeof(XMLCh);
    len = (len % 4) + len;
    XMLCh *newStr = (XMLCh *)this->allocate(len);
    XMLString::copyString(newStr, src);
    return newStr;
}


const XMLCh *  DOMDocumentImpl::getPooledString(const XMLCh *src)
{
    if (!src) return 0;
    else return this->fNamePool->getPooledString(src);
}

void *         DOMDocumentImpl::allocate(size_t amount)
{	
	//	Align the request size so that suballocated blocks
	//	beyond this one will be maintained at the same alignment.
	amount = XMLPlatformUtils::alignPointerForNewBlockAllocation(amount);

    // If the request is for a largish block, hand it off to the system
    //   allocator.  The block still must be linked into the list of
    //   allocated blocks so that it will be deleted when the time comes.
    if (amount > kMaxSubAllocationSize)
    {
		//	The size of the header we add to our raw blocks
		size_t sizeOfHeader = XMLPlatformUtils::alignPointerForNewBlockAllocation(sizeof(void *));

		//	Try to allocate the block
        void* newBlock;
        newBlock = fMemoryManager->allocate((sizeOfHeader + amount) * sizeof(char)); //new char[amount + sizeOfHeader];
        
		//	Link it into the list beyond current block, as current block
		//	is still being subdivided. If there is no current block
		//	then track that we have no bytes to further divide.
        if (fCurrentBlock)
        {
            *(void **)newBlock = *(void **)fCurrentBlock;
            *(void **)fCurrentBlock = newBlock;
        }
        else
        {
            fCurrentBlock = newBlock;
            fFreePtr = 0;
            fFreeBytesRemaining = 0;
        }
		
        void *retPtr = (char *)newBlock + sizeOfHeader;
        return retPtr;
    }

    //	It's a normal (sub-allocatable) request.
	//	Are we out of room in our current block?
	if (amount > fFreeBytesRemaining)
    {
        // Request doesn't fit in the current block.

		// The size of the header we add to our raw blocks
		size_t sizeOfHeader = XMLPlatformUtils::alignPointerForNewBlockAllocation(sizeof(void *));

        // Get a new block from the system allocator.
        void* newBlock;
        newBlock = fMemoryManager->allocate(fHeapAllocSize * sizeof(char)); //new char[kHeapAllocSize];
        
        *(void **)newBlock = fCurrentBlock;
        fCurrentBlock = newBlock;
        fFreePtr = (char *)newBlock + sizeOfHeader;
        fFreeBytesRemaining = fHeapAllocSize - sizeOfHeader;

        if(fHeapAllocSize<kMaxHeapAllocSize)
            fHeapAllocSize*=2;
    }

	//	Subdivide the request off current block
    void *retPtr = fFreePtr;
    fFreePtr += amount;
    fFreeBytesRemaining -= amount;
	
    return retPtr;
}


void    DOMDocumentImpl::deleteHeap()
{
    while (fCurrentBlock != 0)
    {
        void *nextBlock = *(void **)fCurrentBlock;
        fMemoryManager->deallocate(fCurrentBlock); //delete [] (char*) fCurrentBlock;
        fCurrentBlock = nextBlock;
    }
}


DOMNodeList *DOMDocumentImpl::getDeepNodeList(const DOMNode *rootNode, const XMLCh *tagName)
{
    if(!fNodeListPool) {
        fNodeListPool = new (this) DOMDeepNodeListPool<DOMDeepNodeListImpl>(109, false);
    }

    DOMDeepNodeListImpl* retList = fNodeListPool->getByKey(rootNode, tagName, 0);
    if (!retList) {
        int id = fNodeListPool->put((void*) rootNode, (XMLCh*) tagName, 0, new (this) DOMDeepNodeListImpl(rootNode, tagName));
        retList = fNodeListPool->getById(id);
    }

    return retList;
}


DOMNodeList *DOMDocumentImpl::getDeepNodeList(const DOMNode *rootNode,     //DOM Level 2
                                                   const XMLCh *namespaceURI,
                                                   const XMLCh *localName)
{
    if(!fNodeListPool) {
        fNodeListPool = new (this) DOMDeepNodeListPool<DOMDeepNodeListImpl>(109, false);
    }

    DOMDeepNodeListImpl* retList = fNodeListPool->getByKey(rootNode, localName, namespaceURI);
    if (!retList) {
        // the pool will adopt the DOMDeepNodeListImpl
        int id = fNodeListPool->put((void*) rootNode, (XMLCh*) localName, (XMLCh*) namespaceURI, new (this) DOMDeepNodeListImpl(rootNode, namespaceURI, localName));
        retList = fNodeListPool->getById(id);
    }

    return retList;
}


//Introduced in DOM Level 3
const XMLCh* DOMDocumentImpl::getActualEncoding() const {
    return fActualEncoding;
}

void DOMDocumentImpl::setActualEncoding(const XMLCh* actualEncoding){
    fActualEncoding = cloneString(actualEncoding);
}

const XMLCh* DOMDocumentImpl::getEncoding() const {
    return fEncoding;
}

void DOMDocumentImpl::setEncoding(const XMLCh* encoding){
    fEncoding = cloneString(encoding);
}

bool DOMDocumentImpl::getStandalone() const{
    return fStandalone;
}

void DOMDocumentImpl::setStandalone(bool standalone){
    fStandalone = standalone;
}

const XMLCh* DOMDocumentImpl::getVersion() const {
    return fVersion;
}

void DOMDocumentImpl::setVersion(const XMLCh* version){
    if ((version && *version) &&
        !XMLString::equals(version, XMLUni::fgVersion1_0) &&
        !XMLString::equals(version, XMLUni::fgVersion1_1))
        throw DOMException(DOMException::NOT_SUPPORTED_ERR, 0, getMemoryManager());

    fVersion = cloneString(version);
}

const XMLCh* DOMDocumentImpl::getDocumentURI() const
{
    return fDocumentURI;
}

void DOMDocumentImpl::setDocumentURI(const XMLCh* documentURI){
    if (documentURI && *documentURI) {
        XMLCh* temp = (XMLCh*) this->allocate((XMLString::stringLen(documentURI) + 9)*sizeof(XMLCh));
        XMLString::fixURI(documentURI, temp);
        fDocumentURI = temp;
    }
    else
        fDocumentURI = 0;
}

bool DOMDocumentImpl::getStrictErrorChecking() const {
    return getErrorChecking();
}

void DOMDocumentImpl::setStrictErrorChecking(bool strictErrorChecking) {
    setErrorChecking(strictErrorChecking);
}

DOMNode* DOMDocumentImpl::adoptNode(DOMNode*) {
    throw DOMException(DOMException::NOT_SUPPORTED_ERR, 0, getMemoryManager());
    return 0;
}

void DOMDocumentImpl::normalizeDocument() {

    if(!fNormalizer) 
        fNormalizer = new (fMemoryManager) DOMNormalizer(fMemoryManager);

    fNormalizer->normalizeDocument(this);
}

DOMConfiguration* DOMDocumentImpl::getDOMConfiguration() const {
    if(!fDOMConfiguration)
        ((DOMDocumentImpl*)this)->fDOMConfiguration = new ((DOMDocumentImpl*)this) DOMConfigurationImpl(fMemoryManager);

    return fDOMConfiguration;
}

void DOMDocumentImpl::setDOMConfiguration(DOMConfiguration *config) {
    fDOMConfiguration = config;
}

DOMNode *DOMDocumentImpl::importNode(DOMNode *source, bool deep, bool cloningDoc)
{
    DOMNode *newnode=0;
    bool oldErrorCheckingFlag = errorChecking;

    switch (source->getNodeType())
    {
    case DOMNode::ELEMENT_NODE :
        {
            DOMElement *newelement;
            if (source->getLocalName() == 0)
                newelement = createElement(source->getNodeName());
            else
            {
                DOMElementNSImpl* nsElem = (DOMElementNSImpl*)createElementNS(source->getNamespaceURI(), source->getNodeName());
                DOMTypeInfoImpl* clonedTypeInfo=NULL;
                // if the source has type informations, copy them
                DOMPSVITypeInfo* sourcePSVI=(DOMPSVITypeInfo*)source->getInterface(XMLUni::fgXercescInterfacePSVITypeInfo);
                if(sourcePSVI && sourcePSVI->getNumericProperty(DOMPSVITypeInfo::PSVI_Schema_Specified))
                    clonedTypeInfo=new (this) DOMTypeInfoImpl(this, sourcePSVI);
                else
                {
                    const DOMTypeInfo * typeInfo=((DOMElement*)source)->getTypeInfo();
                    // copy it only if it has valid data
                    if(typeInfo && typeInfo->getName()!=NULL)
                        clonedTypeInfo=new (this) DOMTypeInfoImpl(typeInfo->getNamespace(), typeInfo->getName());
                }
                if(clonedTypeInfo)
                    nsElem->setTypeInfo(clonedTypeInfo);
                newelement=nsElem;
            }
            DOMNamedNodeMap *srcattr=source->getAttributes();
            if(srcattr!=0)
                for(XMLSize_t i=0;i<srcattr->getLength();++i)
                {
                    DOMAttr *attr = (DOMAttr *) srcattr->item(i);
                    if (attr -> getSpecified() || cloningDoc) { // not a default attribute or we are in the process of cloning the elements from inside a DOMDocumentType
                        DOMAttr *nattr = (DOMAttr *) importNode(attr, true, cloningDoc);
                        if (attr -> getLocalName() == 0)
                            newelement->setAttributeNode(nattr);
                        else
                            newelement->setAttributeNodeNS(nattr);

                        // if the imported attribute is of ID type, register the new node in fNodeIDMap
                        if (attr->isId()) {
                            castToNodeImpl(nattr)->isIdAttr(true);
                            if (!fNodeIDMap)
                                 fNodeIDMap = new (this) DOMNodeIDMap(500, this);
                            fNodeIDMap->add((DOMAttr*)nattr);
                        }
                    }
                }
            newnode=newelement;

        }
        break;
    case DOMNode::ATTRIBUTE_NODE :
        {
            DOMAttrImpl* newattr=NULL;
            if (source->getLocalName() == 0)
                newattr = (DOMAttrImpl*)createAttribute(source->getNodeName());
            else
                newattr = (DOMAttrImpl*)createAttributeNS(source->getNamespaceURI(), source->getNodeName());
            DOMTypeInfoImpl* clonedTypeInfo=NULL;
            // if the source has type informations, copy them
            DOMPSVITypeInfo* sourcePSVI=(DOMPSVITypeInfo*)source->getInterface(XMLUni::fgXercescInterfacePSVITypeInfo);
            if(sourcePSVI && sourcePSVI->getNumericProperty(DOMPSVITypeInfo::PSVI_Schema_Specified))
                clonedTypeInfo=new (this) DOMTypeInfoImpl(this, sourcePSVI);
            else
            {
                const DOMTypeInfo * typeInfo=((DOMAttr*)source)->getTypeInfo();
                // copy it only if it has valid data
                if(typeInfo && typeInfo->getName()!=NULL)
                    clonedTypeInfo=new (this) DOMTypeInfoImpl(typeInfo->getNamespace(), typeInfo->getName());
            }
            if(clonedTypeInfo)
                newattr->setTypeInfo(clonedTypeInfo);
            newnode=newattr;
        }
        deep = true;
        // Kids carry value

        break;
    case DOMNode::TEXT_NODE :
        newnode = createTextNode(source->getNodeValue());
        break;
    case DOMNode::CDATA_SECTION_NODE :
        newnode = createCDATASection(source->getNodeValue());
        break;
    case DOMNode::ENTITY_REFERENCE_NODE :
        {
            DOMEntityReferenceImpl* newentityRef = (DOMEntityReferenceImpl*)createEntityReference(source->getNodeName());
            newnode=newentityRef;
            errorChecking = false;
            newentityRef->setReadOnly(false, true); //allow deep import temporarily
        }
        break;
    case DOMNode::ENTITY_NODE :
        {
            DOMEntity *srcentity=(DOMEntity *)source;
            DOMEntityImpl *newentity = (DOMEntityImpl *)createEntity(source->getNodeName());
            newentity->setPublicId(srcentity->getPublicId());
            newentity->setSystemId(srcentity->getSystemId());
            newentity->setNotationName(srcentity->getNotationName());
            // Kids carry additional value
            newnode=newentity;
            castToNodeImpl(newentity)->setReadOnly(false, true);// allow deep import temporarily
        }
        break;
    case DOMNode::PROCESSING_INSTRUCTION_NODE :
        newnode = createProcessingInstruction(source->getNodeName(),
            source->getNodeValue());
        break;
    case DOMNode::COMMENT_NODE :
        newnode = createComment(source->getNodeValue());
        break;
    case DOMNode::DOCUMENT_TYPE_NODE :
        {
            // unless this is used as part of cloning a Document
            // forbid it for the sake of being compliant to the DOM spec
            if (!cloningDoc)
                throw DOMException(DOMException::NOT_SUPPORTED_ERR, 0, getMemoryManager());

            DOMDocumentType *srcdoctype = (DOMDocumentType *)source;
            DOMDocumentType *newdoctype = (DOMDocumentType *)
                createDocumentType(srcdoctype->getNodeName(),
                srcdoctype->getPublicId(),
                srcdoctype->getSystemId());
            // Values are on NamedNodeMaps
            DOMNamedNodeMap *smap = srcdoctype->getEntities();
            DOMNamedNodeMap *tmap = newdoctype->getEntities();
            if(smap != 0) {
                for(XMLSize_t i = 0; i < smap->getLength(); i++) {
                    tmap->setNamedItem(importNode(smap->item(i), true, cloningDoc));
                }
            }
            smap = srcdoctype->getNotations();
            tmap = newdoctype->getNotations();
            if (smap != 0) {
                for(XMLSize_t i = 0; i < smap->getLength(); i++) {
                    tmap->setNamedItem(importNode(smap->item(i), true, cloningDoc));
                }
            }
            const XMLCh* intSubset=srcdoctype->getInternalSubset();
            if(intSubset != 0) {
                ((DOMDocumentTypeImpl *)newdoctype)->setInternalSubset(intSubset);
            }

            // detect if the DTD being copied is our own implementation, and use the provided methods
            try
            {
                DOMDocumentTypeImpl* docTypeImpl=(DOMDocumentTypeImpl*)(srcdoctype->getInterface(XMLUni::fgXercescInterfaceDOMDocumentTypeImpl));
                if(docTypeImpl)
                {
                    smap = docTypeImpl->getElements();
                    tmap = ((DOMDocumentTypeImpl *)newdoctype)->getElements();
                    if (smap != 0) {
                        for(XMLSize_t i = 0; i < smap->getLength(); i++) {
                            tmap->setNamedItem(importNode(smap->item(i), true, cloningDoc));
                        }
                    }
                }
            } catch(DOMException&) {
            }

            newnode = newdoctype;
        }
        break;
    case DOMNode::DOCUMENT_FRAGMENT_NODE :
        newnode = createDocumentFragment();
        // No name, kids carry value
        break;
    case DOMNode::NOTATION_NODE :
        {
            DOMNotation *srcnotation=(DOMNotation *)source;
            DOMNotationImpl *newnotation = (DOMNotationImpl *)createNotation(source->getNodeName());
            newnotation->setPublicId(srcnotation->getPublicId());
            newnotation->setSystemId(srcnotation->getSystemId());
            // Kids carry additional value
            newnode=newnotation;
            // No name, no value
            break;
        }

    case DOMNode::DOCUMENT_NODE : // Document can't be child of Document
    default:                       // Unknown node type
        throw DOMException(DOMException::NOT_SUPPORTED_ERR, 0, getMemoryManager());
    }

    // If deep, replicate and attach the kids.
    if (deep)
        for (DOMNode *srckid = source->getFirstChild();
             srckid != 0;
             srckid = srckid->getNextSibling())
        {
            newnode->appendChild(importNode(srckid, true, cloningDoc));
        }

    if (newnode->getNodeType() == DOMNode::ENTITY_REFERENCE_NODE
        || newnode->getNodeType() == DOMNode::ENTITY_NODE) {
        castToNodeImpl(newnode)->setReadOnly(true, true);
        errorChecking = oldErrorCheckingFlag;
    }

    if (cloningDoc)
    {
        // we know for sure that the source node is a DOMNodeImpl, as cloningDoc is set to true when
        // a DOMDocumentImpl is cloned
        castToNodeImpl(source)->callUserDataHandlers(DOMUserDataHandler::NODE_CLONED, source, newnode);
    }
    else
        fNode.callUserDataHandlers(DOMUserDataHandler::NODE_IMPORTED, source, newnode);

    return newnode;
}

// user data utility
void* DOMDocumentImpl::setUserData(DOMNodeImpl* n, const XMLCh* key, void* data, DOMUserDataHandler* handler)
{
    void* oldData = 0;
    unsigned int keyId=fUserDataTableKeys.addOrFind(key);

    if (!fUserDataTable) {
        // create the table on heap so that it can be cleaned in destructor
        fUserDataTable = new (fMemoryManager) RefHash2KeysTableOf<DOMUserDataRecord>
        (
            109
            , true
            , new (fMemoryManager) HashPtr()
            , fMemoryManager
        );
    }
    else {
        DOMUserDataRecord* oldDataRecord = fUserDataTable->get((void*)n, keyId);

        if (oldDataRecord) {
            oldData = oldDataRecord->getKey();
            fUserDataTable->removeKey((void*)n, keyId);
        }
    }

    if (data) {

        // clone the key first, and create the DOMUserDataRecord
        // create on the heap and adopted by the hashtable which will delete it upon removal.
        fUserDataTable->put((void*)n, keyId, new (fMemoryManager) DOMUserDataRecord(data, handler));
    }
    else {
        RefHash2KeysTableOfEnumerator<DOMUserDataRecord> enumKeys(fUserDataTable, false, fMemoryManager);
        enumKeys.setPrimaryKey(n);
        if (!enumKeys.hasMoreElements())
            n->hasUserData(false);
    }

    return oldData;
}

void* DOMDocumentImpl::getUserData(const DOMNodeImpl* n, const XMLCh* key) const
{
    if (fUserDataTable) {
        unsigned int keyId=fUserDataTableKeys.getId(key);
        if(keyId!=0) {
            DOMUserDataRecord* dataRecord = fUserDataTable->get((void*)n, keyId);
            if (dataRecord)
                return dataRecord->getKey();
        }
    }

    return 0;
}

void DOMDocumentImpl::callUserDataHandlers(const DOMNodeImpl* n, DOMUserDataHandler::DOMOperationType operation, const DOMNode* src, const DOMNode* dst) const
{
    if (fUserDataTable) {
        RefHash2KeysTableOfEnumerator<DOMUserDataRecord> userDataEnum(fUserDataTable, false, fMemoryManager);
        userDataEnum.setPrimaryKey(n);
        // Create a snapshot of the handlers to be called, as the "handle" callback could be invalidating the enumerator by calling
        // setUserData on the dst node
        ValueVectorOf< int > snapshot(3, fMemoryManager);
        while (userDataEnum.hasMoreElements()) {
            // get the key
            void* key;
            int key2;
            userDataEnum.nextElementKey(key,key2);
            snapshot.addElement(key2);
        }
        ValueVectorEnumerator< int > snapshotEnum(&snapshot);
        while(snapshotEnum.hasMoreElements())
        {
            int key2=snapshotEnum.nextElement();

            // get the DOMUserDataRecord
            DOMUserDataRecord* userDataRecord = fUserDataTable->get((void*)n,key2);

            // get the handler
            DOMUserDataHandler* handler = userDataRecord->getValue();

            if (handler) {
                // get the data
                void* data = userDataRecord->getKey();
                const XMLCh* userKey = fUserDataTableKeys.getValueForId(key2);
                handler->handle(operation, userKey, data, src, dst);
            }
        }
        // if the operation is NODE_DELETED, we in fact should remove the data from the table
        if (operation == DOMUserDataHandler::NODE_DELETED)
            fUserDataTable->removeKey((void*)n);
    }
}


void DOMDocumentImpl::transferUserData(DOMNodeImpl* n1, DOMNodeImpl* n2)
{
    if (fUserDataTable) {
        fUserDataTable->transferElement((void*)n1, (void*)n2);
        n1->hasUserData(false);
        n2->hasUserData(true);
    }
}


DOMNode* DOMDocumentImpl::renameNode(DOMNode* n, const XMLCh* namespaceURI, const XMLCh* name)
{
    if (n->getOwnerDocument() != this)
        if (n->getNodeType() == DOCUMENT_NODE)
            throw DOMException(DOMException::NOT_SUPPORTED_ERR, 0, getMemoryManager());
        else
            throw DOMException(DOMException::WRONG_DOCUMENT_ERR, 0, getMemoryManager());

    switch (n->getNodeType()) {
        case ELEMENT_NODE:
            return ((DOMElementImpl*)n)->rename(namespaceURI, name);
        case ATTRIBUTE_NODE:
            return ((DOMAttrImpl*)n)->rename(namespaceURI, name);
        default:
            throw DOMException(DOMException::NOT_SUPPORTED_ERR, 0, getMemoryManager());
    }

    return 0;
}

void DOMDocumentImpl::release()
{
    DOMDocument* doc = (DOMDocument*) this;
    fNode.callUserDataHandlers(DOMUserDataHandler::NODE_DELETED, 0, 0);

    // notify userdatahandler first, if we have some
    if (fUserDataTable)
        releaseDocNotifyUserData(this);

    // release the docType in case it was created from heap
    if (fDocType) {
        castToNodeImpl(fDocType)->isToBeReleased(true);
        fDocType->release();
    }

    // delete the document memory pool
    delete doc;
}

void DOMDocumentImpl::releaseDocNotifyUserData(DOMNode* object)
{
    DOMNode *child = object->getFirstChild();
    
    while( child != 0)
    {
            
         DOMNamedNodeMap *attrlist=child->getAttributes(); 
    
         if(attrlist!=0) 
             for(XMLSize_t i=0;i<attrlist->getLength();++i) 
                 releaseDocNotifyUserData(attrlist->item(i)); 
            
        releaseDocNotifyUserData(child);
        child = child->getNextSibling();
    }

    castToNodeImpl(object)->callUserDataHandlers(DOMUserDataHandler::NODE_DELETED, 0, 0);
}

void DOMDocumentImpl::release(DOMNode* object, NodeObjectType type)
{
    if (!fRecycleNodePtr)
        fRecycleNodePtr = new (fMemoryManager) RefArrayOf<DOMNodePtr> (15, fMemoryManager);

    if (!fRecycleNodePtr->operator[](type))
        fRecycleNodePtr->operator[](type) = new (fMemoryManager) RefStackOf<DOMNode> (15, false, fMemoryManager);

    fRecycleNodePtr->operator[](type)->push(object);
}

void DOMDocumentImpl::releaseBuffer(DOMBuffer* buffer)
{
    if (!fRecycleBufferPtr)
        fRecycleBufferPtr = new (fMemoryManager) RefStackOf<DOMBuffer> (15, false, fMemoryManager);

    fRecycleBufferPtr->push(buffer);
}

DOMBuffer* DOMDocumentImpl::popBuffer()
{
    if (!fRecycleBufferPtr || fRecycleBufferPtr->empty())
        return 0;

    return fRecycleBufferPtr->pop();
}


void * DOMDocumentImpl::allocate(size_t amount, NodeObjectType type)
{
    if (!fRecycleNodePtr)
        return allocate(amount);

    DOMNodePtr* ptr = fRecycleNodePtr->operator[](type);
    if (!ptr || ptr->empty())
        return allocate(amount);

    return (void*) ptr->pop();

}

XERCES_CPP_NAMESPACE_END
