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
 * $Id: DOMNodeImpl.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

// This class doesn't support having any children, and implements the behavior
// of an empty NodeList as far getChildNodes is concerned.
// The ParentNode subclass overrides this behavior.


#include "DOMCasts.hpp"

#include "DOMDocumentTypeImpl.hpp"
#include "DOMElementImpl.hpp"
#include "DOMAttrImpl.hpp"

#include <xercesc/dom/DOMImplementation.hpp>
#include <xercesc/dom/DOMException.hpp>

#include <xercesc/util/XMLUniDefs.hpp>
#include <xercesc/util/XMLRegisterCleanup.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/XMLInitializer.hpp>
#include <stdio.h>
#include <assert.h>

XERCES_CPP_NAMESPACE_BEGIN

//Though DOMNodeImpl does not derivate from DOMNode, it shares
//the same GetDOMNodeMemoryManager

const unsigned short DOMNodeImpl::READONLY     = 0x1<<0;
const unsigned short DOMNodeImpl::SYNCDATA     = 0x1<<1;
const unsigned short DOMNodeImpl::SYNCCHILDREN = 0x1<<2;
const unsigned short DOMNodeImpl::OWNED        = 0x1<<3;
const unsigned short DOMNodeImpl::FIRSTCHILD   = 0x1<<4;
const unsigned short DOMNodeImpl::SPECIFIED    = 0x1<<5;
const unsigned short DOMNodeImpl::IGNORABLEWS  = 0x1<<6;
const unsigned short DOMNodeImpl::SETVALUE     = 0x1<<7;
const unsigned short DOMNodeImpl::ID_ATTR      = 0x1<<8;
const unsigned short DOMNodeImpl::USERDATA     = 0x1<<9;
const unsigned short DOMNodeImpl::LEAFNODETYPE = 0x1<<10;
const unsigned short DOMNodeImpl::CHILDNODE    = 0x1<<11;
const unsigned short DOMNodeImpl::TOBERELEASED = 0x1<<12;

// -----------------------------------------------------------------------
//  Reset the singleton gEmptyNodeList
// -----------------------------------------------------------------------
static DOMNodeListImpl *gEmptyNodeList = 0;  // make a singleton empty node list
static XMLMutex* gEmptyNodeListMutex = 0;
static XMLRegisterCleanup emptyNodeListCleanup;

static void reinitEmptyNodeList()
{
    delete gEmptyNodeList;
    gEmptyNodeList = 0;

    delete gEmptyNodeListMutex;
    gEmptyNodeListMutex = 0;
}

void XMLInitializer::initializeEmptyNodeList()
{
    gEmptyNodeList = new DOMNodeListImpl(0);
    if (gEmptyNodeList) {
        emptyNodeListCleanup.registerCleanup(reinitEmptyNodeList);
    }
}

// -----------------------------------------------------------------------
//  DOMNodeImpl Functions
// -----------------------------------------------------------------------
DOMNodeImpl::DOMNodeImpl(DOMNode *ownerNode)
:  fOwnerNode(ownerNode)
{
    this->flags = 0;
    // as long as we do not have any owner, fOwnerNode is our ownerDocument    
}

// This only makes a shallow copy, cloneChildren must also be called for a
// deep clone
DOMNodeImpl::DOMNodeImpl(const DOMNodeImpl &other)
{
    this->flags = other.flags;
    this->isReadOnly(false);

    // Need to break the association w/ original parent
    this->fOwnerNode = other.getOwnerDocument();
    this->isOwned(false);
}



DOMNodeImpl::~DOMNodeImpl() {
}


DOMNode * DOMNodeImpl::appendChild(DOMNode *)
{
    // Only node types that don't allow children will use this default function.
    //   Others will go to DOMParentNode::appendChild.
    throw DOMException(DOMException::HIERARCHY_REQUEST_ERR,0, GetDOMNodeMemoryManager);
    return 0;
    //  return insertBefore(newChild, 0);
}


DOMNamedNodeMap * DOMNodeImpl::getAttributes() const {
    return 0;                   // overridden in ElementImpl
}


DOMNodeList *DOMNodeImpl::getChildNodes() const {

    if (!gEmptyNodeList)
    {
        if (!gEmptyNodeListMutex)
        {
            XMLMutexLock lock(XMLPlatformUtils::fgAtomicMutex);
			
            if (!gEmptyNodeListMutex)
                gEmptyNodeListMutex = new XMLMutex(XMLPlatformUtils::fgMemoryManager);
        }

        // Use a faux scope to synchronize while we do this
        {
            XMLMutexLock lock(gEmptyNodeListMutex);

            if (!gEmptyNodeList)
            {
                gEmptyNodeList = new DOMNodeListImpl(0);
                emptyNodeListCleanup.registerCleanup(reinitEmptyNodeList);
            }
        }
    }

    return (DOMNodeList *)gEmptyNodeList;
}



DOMNode * DOMNodeImpl::getFirstChild() const {
    return 0;                   // overridden in ParentNode
}


DOMNode * DOMNodeImpl::getLastChild() const
{
    return 0;                   // overridden in ParentNode
}


DOMNode * DOMNodeImpl::getNextSibling() const {
    return 0;                // overridden in ChildNode
}


const XMLCh * DOMNodeImpl::getNodeValue() const {
    return 0;                    // Overridden by anything that has a value
}


//
//  Unlike the external getOwnerDocument, this one returns the owner document
//     for document nodes as well as all of the other node types.
//
DOMDocument *DOMNodeImpl::getOwnerDocument() const
{
    if (!this->isLeafNode())
    {
        DOMElementImpl *ep = (DOMElementImpl *)castToNode(this);
        return ep->fParent.fOwnerDocument;
    }

    //  Leaf node types - those that cannot have children, like Text.
    if (isOwned()) {

        DOMDocument* ownerDoc = fOwnerNode->getOwnerDocument();

        if (!ownerDoc) {

            assert (fOwnerNode->getNodeType() == DOMNode::DOCUMENT_NODE);
            return  (DOMDocument *)fOwnerNode;
        }
        else {
            return ownerDoc;
        }
    } else {
        assert (fOwnerNode->getNodeType() == DOMNode::DOCUMENT_NODE);
        return  (DOMDocument *)fOwnerNode;
    }
}


void DOMNodeImpl::setOwnerDocument(DOMDocument *doc) {
    // if we have an owner we rely on it to have it right
    // otherwise fOwnerNode is our ownerDocument
    if (!isOwned()) {
        // revisit.  Problem with storage for doctype nodes that were created
        //                on the system heap in advance of having a document.
        fOwnerNode = doc;
    }
}

DOMNode * DOMNodeImpl::getParentNode() const
{
    return 0;                // overridden in ChildNode
}


DOMNode*  DOMNodeImpl::getPreviousSibling() const
{
    return 0;                // overridden in ChildNode
}

bool DOMNodeImpl::hasChildNodes() const
{
    return false;
}



DOMNode *DOMNodeImpl::insertBefore(DOMNode *, DOMNode *) {
    throw DOMException(DOMException::HIERARCHY_REQUEST_ERR, 0, GetDOMNodeMemoryManager);
    return 0;
}


DOMNode *DOMNodeImpl::removeChild(DOMNode *)
{
    throw DOMException(DOMException::NOT_FOUND_ERR, 0, GetDOMNodeMemoryManager);
    return 0;
}


DOMNode *DOMNodeImpl::replaceChild(DOMNode *, DOMNode *)
{
    throw DOMException(DOMException::HIERARCHY_REQUEST_ERR,0, GetDOMNodeMemoryManager);
    return 0;
}



void DOMNodeImpl::setNodeValue(const XMLCh *)
{
    // Default behavior is to do nothing, overridden in some subclasses
}



void DOMNodeImpl::setReadOnly(bool readOnl, bool deep)
{
    this->isReadOnly(readOnl);

    if (deep) {
        for (DOMNode *mykid = castToNode(this)->getFirstChild();
            mykid != 0;
            mykid = mykid->getNextSibling()) {

            short kidNodeType = mykid->getNodeType();

            switch (kidNodeType) {
            case DOMNode::ENTITY_REFERENCE_NODE:
                break;
            case DOMNode::ELEMENT_NODE:
                ((DOMElementImpl*) mykid)->setReadOnly(readOnl, true);
                break;
            case DOMNode::DOCUMENT_TYPE_NODE:
               ((DOMDocumentTypeImpl*) mykid)->setReadOnly(readOnl, true);
               break;
            default:
                castToNodeImpl(mykid)->setReadOnly(readOnl, true);
                break;
            }
        }
    }
}


//Introduced in DOM Level 2

void DOMNodeImpl::normalize()
{
    // does nothing by default, overridden by subclasses
}


bool DOMNodeImpl::isSupported(const XMLCh *feature, const XMLCh *version) const
{
    return DOMImplementation::getImplementation()->hasFeature(feature, version);
}

const XMLCh *DOMNodeImpl::getNamespaceURI() const
{
    return 0;
}

const XMLCh *DOMNodeImpl::getPrefix() const
{
    return 0;
}

const XMLCh *DOMNodeImpl::getLocalName() const
{
    return 0;
}


void DOMNodeImpl::setPrefix(const XMLCh *)
{
    throw DOMException(DOMException::NAMESPACE_ERR, 0, GetDOMNodeMemoryManager);
}


bool DOMNodeImpl::hasAttributes() const {
    return 0;                   // overridden in ElementImpl
}





const XMLCh *DOMNodeImpl::getXmlString()      {return XMLUni::fgXMLString;}
const XMLCh *DOMNodeImpl::getXmlURIString()   {return XMLUni::fgXMLURIName;}
const XMLCh *DOMNodeImpl::getXmlnsString()    {return XMLUni::fgXMLNSString;}
const XMLCh *DOMNodeImpl::getXmlnsURIString() {return XMLUni::fgXMLNSURIName;}

//Return a URI mapped from the given prefix and namespaceURI as below
//    prefix   namespaceURI    output
//---------------------------------------------------
//    "xml"     xmlURI          xmlURI
//    "xml"     otherwise       NAMESPACE_ERR
//    "xmlns"   xmlnsURI        xmlnsURI (nType = ATTRIBUTE_NODE only)
//    "xmlns"   otherwise       NAMESPACE_ERR (nType = ATTRIBUTE_NODE only)
//    != null   null or ""      NAMESPACE_ERR
//    else      any             namesapceURI
const XMLCh* DOMNodeImpl::mapPrefix(const XMLCh *prefix,
                                     const XMLCh *namespaceURI, short nType)
{
    if (prefix == 0)
        return namespaceURI;

    if (XMLString::equals(prefix, XMLUni::fgXMLString))  {
        if (XMLString::equals(namespaceURI, XMLUni::fgXMLURIName))
            return XMLUni::fgXMLURIName;
        throw DOMException(DOMException::NAMESPACE_ERR, 0);
    } else if (nType == DOMNode::ATTRIBUTE_NODE && XMLString::equals(prefix, XMLUni::fgXMLNSString)) {
        if (XMLString::equals(namespaceURI, XMLUni::fgXMLNSURIName))
            return XMLUni::fgXMLNSURIName;
        throw DOMException(DOMException::NAMESPACE_ERR, 0);
    } else if (namespaceURI == 0 || *namespaceURI == 0)
        throw DOMException(DOMException::NAMESPACE_ERR, 0);
    return namespaceURI;
}

//Introduced in DOM Level 3
void* DOMNodeImpl::setUserData(const XMLCh* key, void* data, DOMUserDataHandler* handler)
{
   if (!data && !hasUserData())
       return 0;

    hasUserData(true);
    return ((DOMDocumentImpl*)getOwnerDocument())->setUserData(this, key, data, handler);
}

void* DOMNodeImpl::getUserData(const XMLCh* key) const
{
   if (hasUserData())
       return ((DOMDocumentImpl*)getOwnerDocument())->getUserData(this, key);
    return 0;
}

void DOMNodeImpl::callUserDataHandlers(DOMUserDataHandler::DOMOperationType operation,
                                       const DOMNode* src,
                                       const DOMNode* dst) const
{
    DOMDocumentImpl* doc=(DOMDocumentImpl*)getOwnerDocument();
    if (doc)
        doc->callUserDataHandlers(this, operation, src, dst);
}

bool DOMNodeImpl::isSameNode(const DOMNode* other) const
{
    return (castToNode(this) == other);
}

bool DOMNodeImpl::isEqualNode(const DOMNode* arg) const
{
    if (!arg)
        return false;

    if (isSameNode(arg)) {
        return true;
    }

    DOMNode* thisNode = castToNode(this);

    if (arg->getNodeType() != thisNode->getNodeType()) {
        return false;
    }

    // the compareString will check null string as well
    if (!XMLString::equals(thisNode->getNodeName(), arg->getNodeName())) {
        return false;
    }

    if (!XMLString::equals(thisNode->getLocalName(),arg->getLocalName())) {
        return false;
    }

    if (!XMLString::equals(thisNode->getNamespaceURI(), arg->getNamespaceURI())) {
        return false;
    }

    if (!XMLString::equals(thisNode->getPrefix(), arg->getPrefix())) {
        return false;
    }

    if (!XMLString::equals(thisNode->getNodeValue(), arg->getNodeValue())) {
        return false;
    }

    if (!XMLString::equals(thisNode->getBaseURI(), arg->getBaseURI())) {
        return false;
    }

    return true;
}

const XMLCh* DOMNodeImpl::lookupNamespacePrefix(const XMLCh* namespaceURI,
                                                bool useDefault) const {
    // REVISIT: When Namespaces 1.1 comes out this may not be true
    // Prefix can't be bound to null namespace
    if (namespaceURI == 0) {
        return 0;
    }

    DOMNode *thisNode = castToNode(this);

    short type = thisNode->getNodeType();

    switch (type) {
    case DOMNode::ELEMENT_NODE: {
        return lookupNamespacePrefix(namespaceURI, useDefault, (DOMElement*)thisNode);
    }
    case DOMNode::DOCUMENT_NODE:{
        return ((DOMDocument*)thisNode)->getDocumentElement()->lookupNamespacePrefix(namespaceURI, useDefault);
    }

    case DOMNode::ENTITY_NODE :
    case DOMNode::NOTATION_NODE:
    case DOMNode::DOCUMENT_FRAGMENT_NODE:
    case DOMNode::DOCUMENT_TYPE_NODE:
        // type is unknown
        return 0;
    case DOMNode::ATTRIBUTE_NODE:{
        if (fOwnerNode->getNodeType() == DOMNode::ELEMENT_NODE) {
            return fOwnerNode->lookupNamespacePrefix(namespaceURI, useDefault);
        }
        return 0;
    }
    default:{
        DOMNode *ancestor = getElementAncestor(thisNode);
        if (ancestor != 0) {
            return ancestor->lookupNamespacePrefix(namespaceURI, useDefault);
        }
        return 0;
    }
    }
}


DOMNode* DOMNodeImpl::getElementAncestor (const DOMNode* currentNode) const {
    DOMNode* parent = currentNode->getParentNode();
    if (parent != 0) {
        short type = parent->getNodeType();
        if (type == DOMNode::ELEMENT_NODE) {
            return parent;
        }
        return getElementAncestor(parent);
    }
    return 0;
}


const XMLCh* DOMNodeImpl::lookupNamespacePrefix(const XMLCh* const namespaceURI, bool useDefault, DOMElement *el) const {
    DOMNode *thisNode = castToNode(this);

    const XMLCh* ns = thisNode->getNamespaceURI();
    // REVISIT: if no prefix is available is it null or empty string, or
    //          could be both?
    const XMLCh* prefix = thisNode->getPrefix();

    if (ns != 0 && XMLString::equals(ns,namespaceURI)) {
        if (useDefault || prefix != 0) {
            const XMLCh* foundNamespace =  el->lookupNamespaceURI(prefix);
            if (foundNamespace != 0 && XMLString::equals(foundNamespace, namespaceURI)) {
                return prefix;
            }
        }
    }
    if (thisNode->hasAttributes()) {
        DOMNamedNodeMap *nodeMap = thisNode->getAttributes();

        if(nodeMap != 0) {
            int length = nodeMap->getLength();

            for (int i = 0;i < length;i++) {
                DOMNode *attr = nodeMap->item(i);
                const XMLCh* attrPrefix = attr->getPrefix();
                const XMLCh* value = attr->getNodeValue();

                ns = attr->getNamespaceURI();

                if (ns != 0 && XMLString::equals(ns, XMLUni::fgXMLNSURIName)) {
                    // DOM Level 2 nodes
                    if ((useDefault && XMLString::equals(attr->getNodeName(), XMLUni::fgXMLNSString)) ||
                        (attrPrefix != 0 && XMLString::equals(attrPrefix, XMLUni::fgXMLNSString)) &&
                        XMLString::equals(value, namespaceURI)) {
                        const XMLCh* localname= attr->getLocalName();
                        const XMLCh* foundNamespace = el->lookupNamespaceURI(localname);
                        if (foundNamespace != 0 && XMLString::equals(foundNamespace, namespaceURI)) {
                            return localname;
                        }
                    }
                }
            }
        }
    }
    DOMNode *ancestor = getElementAncestor(thisNode);
    if (ancestor != 0) {
        return castToNodeImpl(ancestor)->lookupNamespacePrefix(namespaceURI, useDefault, el);
    }
    return 0;
}

const XMLCh* DOMNodeImpl::lookupNamespaceURI(const XMLCh* specifiedPrefix) const  {
    DOMNode *thisNode = castToNode(this);

    short type = thisNode->getNodeType();
    switch (type) {
    case DOMNode::ELEMENT_NODE : {
        const XMLCh* ns = thisNode->getNamespaceURI();
        const XMLCh* prefix = thisNode->getPrefix();
        if (ns != 0) {
            // REVISIT: is it possible that prefix is empty string?
            if (specifiedPrefix == 0 && prefix == specifiedPrefix) {
                // looking for default namespace
                return ns;
            } else if (prefix != 0 && XMLString::equals(prefix, specifiedPrefix)) {
                // non default namespace
                return ns;
            }
        }
        if (thisNode->hasAttributes()) {
            DOMNamedNodeMap *nodeMap = thisNode->getAttributes();
            if(nodeMap != 0) {
                int length = nodeMap->getLength();
                for (int i = 0;i < length;i++) {
                    DOMNode *attr = nodeMap->item(i);
                    const XMLCh *attrPrefix = attr->getPrefix();
                    const XMLCh *value = attr->getNodeValue();
                    ns = attr->getNamespaceURI();

                    if (ns != 0 && XMLString::equals(ns, XMLUni::fgXMLNSURIName)) {
                        // at this point we are dealing with DOM Level 2 nodes only
                        if (specifiedPrefix == 0 &&
                            XMLString::equals(attr->getNodeName(), XMLUni::fgXMLNSString)) {
                            // default namespace
                            return value;
                        } else if (attrPrefix != 0 &&
                                   XMLString::equals(attrPrefix, XMLUni::fgXMLNSString) &&
                                   XMLString::equals(attr->getLocalName(), specifiedPrefix)) {
                            // non default namespace
                            return value;
                        }
                    }
                }
            }
        }
        DOMNode *ancestor = getElementAncestor(thisNode);
        if (ancestor != 0) {
            return ancestor->lookupNamespaceURI(specifiedPrefix);
        }
        return 0;
    }
    case DOMNode::DOCUMENT_NODE : {
        return((DOMDocument*)thisNode)->getDocumentElement()->lookupNamespaceURI(specifiedPrefix);
    }
    case DOMNode::ENTITY_NODE :
    case DOMNode::NOTATION_NODE:
    case DOMNode::DOCUMENT_FRAGMENT_NODE:
    case DOMNode::DOCUMENT_TYPE_NODE:
        // type is unknown
        return 0;
    case DOMNode::ATTRIBUTE_NODE:{
        if (fOwnerNode->getNodeType() == DOMNode::ELEMENT_NODE) {
            return fOwnerNode->lookupNamespaceURI(specifiedPrefix);
        }
        return 0;
    }
    default:{
        DOMNode *ancestor = getElementAncestor(castToNode(this));
        if (ancestor != 0) {
            return ancestor->lookupNamespaceURI(specifiedPrefix);
        }
        return 0;
    }
    }
}


const XMLCh*     DOMNodeImpl::getBaseURI() const{
    DOMNode *thisNode = castToNode(this);
    DOMNode* parent = thisNode->getParentNode();
    if (parent)
        return parent->getBaseURI();
    else
        return 0;
}

short            DOMNodeImpl::compareTreePosition(const DOMNode* other) const {
    // Questions of clarification for this method - to be answered by the
    // DOM WG.   Current assumptions listed - LM
    //
    // 1. How do ENTITY nodes compare?
    //    Current assumption: TREE_POSITION_DISCONNECTED, as ENTITY nodes
    //    aren't really 'in the tree'
    //
    // 2. How do NOTATION nodes compare?
    //    Current assumption: TREE_POSITION_DISCONNECTED, as NOTATION nodes
    //    aren't really 'in the tree'
    //
    // 3. Are TREE_POSITION_ANCESTOR and TREE_POSITION_DESCENDANT
    //    only relevant for nodes that are "part of the document tree"?
    //     <outer>
    //         <inner  myattr="true"/>
    //     </outer>
    //    Is the element node "outer" considered an ancestor of "myattr"?
    //    Current assumption: No.
    //
    // 4. How do children of ATTRIBUTE nodes compare (with eachother, or
    //    with children of other attribute nodes with the same element)
    //    Current assumption: Children of ATTRIBUTE nodes are treated as if
    //    they are the attribute node itself, unless the 2 nodes
    //    are both children of the same attribute.
    //
    // 5. How does an ENTITY_REFERENCE node compare with it's children?
    //    Given the DOM, it should precede its children as an ancestor.
    //    Given "document order",  does it represent the same position?
    //    Current assumption: An ENTITY_REFERENCE node is an ancestor of its
    //    children.
    //
    // 6. How do children of a DocumentFragment compare?
    //    Current assumption: If both nodes are part of the same document
    //    fragment, there are compared as if they were part of a document.



    DOMNode* thisNode = castToNode(this);

    // If the nodes are the same...
    if (thisNode == other)
        return (DOMNode::TREE_POSITION_SAME_NODE | DOMNode::TREE_POSITION_EQUIVALENT);

    // If either node is of type ENTITY or NOTATION, compare as disconnected
    short thisType = thisNode->getNodeType();
    short otherType = other->getNodeType();

    // If either node is of type ENTITY or NOTATION, compare as disconnected
    if (thisType == DOMNode::ENTITY_NODE ||
            thisType == DOMNode::NOTATION_NODE ||
            otherType == DOMNode::ENTITY_NODE ||
            otherType == DOMNode::NOTATION_NODE ) {
        return DOMNode::TREE_POSITION_DISCONNECTED;
    }

    //if this is a custom node, we don't really know what to do, just return
    //user should provide its own compareTreePosition logic, and shouldn't reach here
    if(thisType > 12) {
        return 0;
    }

    //if it is a custom node we must ask it for the order
    if(otherType > 12) {
        return reverseTreeOrderBitPattern(other->compareTreePosition(castToNode(this)));
    }

    // Find the ancestor of each node, and the distance each node is from
    // its ancestor.
    // During this traversal, look for ancestor/descendent relationships
    // between the 2 nodes in question.
    // We do this now, so that we get this info correct for attribute nodes
    // and their children.

    const DOMNode *node;
    const DOMNode *thisAncestor = castToNode(this);
    const DOMNode *otherAncestor = other;
    int thisDepth=0;
    int otherDepth=0;
    for (node = castToNode(this); node != 0; node = node->getParentNode()) {
        thisDepth +=1;
        if (node == other)
            // The other node is an ancestor of this one.
            return (DOMNode::TREE_POSITION_ANCESTOR | DOMNode::TREE_POSITION_PRECEDING);
        thisAncestor = node;
    }

    for (node=other; node != 0; node = node->getParentNode()) {
        otherDepth +=1;
        if (node == castToNode(this))
            // The other node is a descendent of the reference node.
            return (DOMNode::TREE_POSITION_DESCENDANT | DOMNode::TREE_POSITION_FOLLOWING);
        otherAncestor = node;
    }


    const DOMNode *otherNode = other;

    short thisAncestorType = thisAncestor->getNodeType();
    short otherAncestorType = otherAncestor->getNodeType();

    // if the ancestor is an attribute, get owning element.
    // we are now interested in the owner to determine position.

    if (thisAncestorType == DOMNode::ATTRIBUTE_NODE)  {
        thisNode = ((DOMAttrImpl *)thisAncestor)->getOwnerElement();
    }
    if (otherAncestorType == DOMNode::ATTRIBUTE_NODE) {
        otherNode = ((DOMAttrImpl *)otherAncestor)->getOwnerElement();
    }

    // Before proceeding, we should check if both ancestor nodes turned
    // out to be attributes for the same element
    if (thisAncestorType == DOMNode::ATTRIBUTE_NODE &&
            otherAncestorType == DOMNode::ATTRIBUTE_NODE &&
            thisNode==otherNode)
        return DOMNode::TREE_POSITION_EQUIVALENT;

    // Now, find the ancestor of the owning element, if the original
    // ancestor was an attribute

    if (thisAncestorType == DOMNode::ATTRIBUTE_NODE) {
        thisDepth=0;
        for (node=thisNode; node != 0; node = node->getParentNode()) {
            thisDepth +=1;
            if (node == otherNode)
                // The other node is an ancestor of the owning element
                return DOMNode::TREE_POSITION_PRECEDING;
            thisAncestor = node;
        }
        for (node=otherNode; node != 0; node = node->getParentNode()) {
            if (node == thisNode)
                // The other node is an ancestor of the owning element
                return DOMNode::TREE_POSITION_FOLLOWING;
        }
    }

    // Now, find the ancestor of the owning element, if the original
    // ancestor was an attribute
    if (otherAncestorType == DOMNode::ATTRIBUTE_NODE) {
        otherDepth=0;
        for (node=otherNode; node != 0; node = node->getParentNode()) {
            otherDepth +=1;
            if (node == thisNode)
                // The other node is a descendent of the reference
                // node's element
                return DOMNode::TREE_POSITION_FOLLOWING;
            otherAncestor = node;
        }
        for (node=thisNode; node != 0; node = node->getParentNode()) {
            if (node == otherNode)
                // The other node is an ancestor of the owning element
                return DOMNode::TREE_POSITION_PRECEDING;
        }
    }

    // thisAncestor and otherAncestor must be the same at this point,
    // otherwise, we are not in the same tree or document fragment
    if (thisAncestor != otherAncestor)
        return DOMNode::TREE_POSITION_DISCONNECTED;

    // Determine which node is of the greatest depth.
    if (thisDepth > otherDepth) {
        for (int i= 0 ; i < thisDepth - otherDepth; i++)
            thisNode = thisNode->getParentNode();
    }
    else {
        for (int i = 0; i < otherDepth - thisDepth; i++)
            otherNode = otherNode->getParentNode();
    }

    // We now have nodes at the same depth in the tree.  Find a common
    // ancestor.
    DOMNode *thisNodeP, *otherNodeP;
    for (thisNodeP = thisNode->getParentNode(),
                 otherNodeP = otherNode->getParentNode();
             thisNodeP != otherNodeP;) {
        thisNode = thisNodeP;
        otherNode = otherNodeP;
        thisNodeP = thisNodeP->getParentNode();
        otherNodeP = otherNodeP->getParentNode();
    }

    // See whether thisNode or otherNode is the leftmost
    for (DOMNode *current = thisNodeP->getFirstChild();
             current != 0;
             current = current->getNextSibling()) {
        if (current == otherNode) {
            return DOMNode::TREE_POSITION_PRECEDING;
        }
        else if (current == thisNode) {
            return DOMNode::TREE_POSITION_FOLLOWING;
        }
    }
    // REVISIT:  shouldn't get here.   Should probably throw an
    // exception
    return 0;

}

short DOMNodeImpl::reverseTreeOrderBitPattern(short pattern) const {

    if(pattern & DOMNode::TREE_POSITION_PRECEDING) {
        pattern &= !DOMNode::TREE_POSITION_PRECEDING;
        pattern |= DOMNode::TREE_POSITION_FOLLOWING;
    }
    else if(pattern & DOMNode::TREE_POSITION_FOLLOWING) {
        pattern &= !DOMNode::TREE_POSITION_FOLLOWING;
        pattern |= DOMNode::TREE_POSITION_PRECEDING;
    }

    if(pattern & DOMNode::TREE_POSITION_ANCESTOR) {
        pattern &= !DOMNode::TREE_POSITION_ANCESTOR;
        pattern |= DOMNode::TREE_POSITION_DESCENDANT;
    }
    else if(pattern & DOMNode::TREE_POSITION_DESCENDANT) {
        pattern &= !DOMNode::TREE_POSITION_DESCENDANT;
        pattern |= DOMNode::TREE_POSITION_ANCESTOR;
    }

    return pattern;
}

/***
 *
 *   Excerpt from http://www.w3.org/TR/2003/WD-DOM-Level-3-Core-20030226/core.html#Node3-textContent
 *
 *   textContent of type DOMString, introduced in DOM Level 3 
 *   
 *   This attribute returns the text content of this node and its descendants. When it is defined 
 *   to be null, setting it has no effect. 
 *   
 *   When set, any possible children this node may have are removed and replaced by a single Text node 
 *   containing the string this attribute is set to. 
 *
 *   On getting, no serialization is performed, the returned string does not contain any markup. 
 *   No whitespace normalization is performed, the returned string does not contain the element content 
 *   whitespaces Fundamental Interfaces. 
 *
 *   Similarly, on setting, no parsing is performed either, the input string is taken as pure textual content.
 *
 *   The string returned is made of the text content of this node depending on its type, 
 *   as defined below: 
 *
 *       Node type                                           Content          
 *   ====================       ========================================================================
 *     ELEMENT_NODE               concatenation of the textContent attribute value of every child node, 
 *     ENTITY_NODE			      excluding COMMENT_NODE and PROCESSING_INSTRUCTION_NODE nodes. 
 *     ENTITY_REFERENCE_NODE	  This is the empty string if the node has no children. 
 *     DOCUMENT_FRAGMENT_NODE 
 *    --------------------------------------------------------------------------------------------------
 *     ATTRIBUTE_NODE
 *     TEXT_NODE
 *     CDATA_SECTION_NODE
 *     COMMENT_NODE, 
 *     PROCESSING_INSTRUCTION_NODE   nodeValue 
 *    --------------------------------------------------------------------------------------------------
 *     DOCUMENT_NODE, 
 *     DOCUMENT_TYPE_NODE, 
 *     NOTATION_NODE                 null 
 *
 ***/

const XMLCh*     DOMNodeImpl::getTextContent() const
{
	unsigned int nBufferLength = 0;

	getTextContent(NULL, nBufferLength);
	XMLCh* pzBuffer = (XMLCh*)((DOMDocumentImpl*)getOwnerDocument())->allocate((nBufferLength+1) * sizeof(XMLCh));
	getTextContent(pzBuffer, nBufferLength);
	pzBuffer[nBufferLength] = 0;

	return pzBuffer;

}

const XMLCh*    DOMNodeImpl::getTextContent(XMLCh* pzBuffer, unsigned int& rnBufferLength) const
{

	unsigned int nRemainingBuffer = rnBufferLength;
	rnBufferLength = 0;

	if (pzBuffer)   
		*pzBuffer = 0;

	DOMNode *thisNode = castToNode(this);

	switch (thisNode->getNodeType())
	{
	case DOMNode::ELEMENT_NODE:
    case DOMNode::ENTITY_NODE:
    case DOMNode::ENTITY_REFERENCE_NODE:
    case DOMNode::DOCUMENT_FRAGMENT_NODE:
    {
		DOMNode* current = thisNode->getFirstChild();

		while (current != NULL) 
		{
			if (current->getNodeType() != DOMNode::COMMENT_NODE &&
				current->getNodeType() != DOMNode::PROCESSING_INSTRUCTION_NODE)
			{

				if (pzBuffer)
				{
					unsigned int nContentLength = nRemainingBuffer;
					castToNodeImpl(current)->getTextContent(pzBuffer + rnBufferLength, nContentLength);
					rnBufferLength += nContentLength;
					nRemainingBuffer -= nContentLength;
				}
				else 
				{
					unsigned int nContentLength = 0;
					castToNodeImpl(current)->getTextContent(NULL, nContentLength);
					rnBufferLength += nContentLength;
				}
			}

			current = current->getNextSibling();

		}
    }

    break;

    case DOMNode::ATTRIBUTE_NODE:
    case DOMNode::TEXT_NODE:
    case DOMNode::CDATA_SECTION_NODE:
    case DOMNode::COMMENT_NODE:
    case DOMNode::PROCESSING_INSTRUCTION_NODE:
    {
		const XMLCh* pzValue = thisNode->getNodeValue();
		unsigned int nStrLen = XMLString::stringLen(pzValue);

		if (pzBuffer) 
		{
			unsigned int nContentLength = (nRemainingBuffer >= nStrLen) ? nStrLen : nRemainingBuffer;
			XMLString::copyNString(pzBuffer + rnBufferLength, pzValue, nContentLength);
			rnBufferLength += nContentLength;
			nRemainingBuffer -= nContentLength;
		}
		else 
		{
			rnBufferLength += nStrLen;
		}

    }

    break;

	/***
         DOCUMENT_NODE
		 DOCUMENT_TYPE_NODE
		 NOTATION_NODE
	***/
	default:

		break;
	}

	return pzBuffer;

}

void DOMNodeImpl::setTextContent(const XMLCh* textContent){
    DOMNode *thisNode = castToNode(this);
    switch (thisNode->getNodeType()) 
    {
        case DOMNode::ELEMENT_NODE:
        case DOMNode::ENTITY_NODE:
        case DOMNode::ENTITY_REFERENCE_NODE:
        case DOMNode::DOCUMENT_FRAGMENT_NODE:
            {
                if (isReadOnly())
                  throw DOMException(DOMException::NO_MODIFICATION_ALLOWED_ERR, 0, GetDOMNodeMemoryManager);

                // Remove all childs
                DOMNode* current = thisNode->getFirstChild();
                while (current != NULL) 
                {
                    thisNode->removeChild(current);
                    current = thisNode->getFirstChild();
                }
                if (textContent != NULL) 
                {
                    // Add textnode containing data
                    current = ((DOMDocumentImpl*)thisNode->getOwnerDocument())->createTextNode(textContent);
                    thisNode->appendChild(current);
                }
            }
            break;

        case DOMNode::ATTRIBUTE_NODE:
        case DOMNode::TEXT_NODE:
        case DOMNode::CDATA_SECTION_NODE:
        case DOMNode::COMMENT_NODE:
        case DOMNode::PROCESSING_INSTRUCTION_NODE:
            if (isReadOnly())
                throw DOMException(DOMException::NO_MODIFICATION_ALLOWED_ERR, 0, GetDOMNodeMemoryManager);

            thisNode->setNodeValue(textContent);
            break;

        case DOMNode::DOCUMENT_NODE:
        case DOMNode::DOCUMENT_TYPE_NODE:
        case DOMNode::NOTATION_NODE:
            break;

        default:
            throw DOMException(DOMException::NOT_SUPPORTED_ERR, 0, GetDOMNodeMemoryManager);
    }
}


bool DOMNodeImpl::isDefaultNamespace(const XMLCh* namespaceURI) const{
	DOMNode *thisNode = castToNode(this);
    short type = thisNode->getNodeType();
    switch (type) {
    case DOMNode::ELEMENT_NODE: {
        const XMLCh *prefix = thisNode->getPrefix();

        // REVISIT: is it possible that prefix is empty string?
        if (prefix == 0 || !*prefix) {
            return XMLString::equals(namespaceURI, thisNode->getNamespaceURI());
        }

        if (thisNode->hasAttributes()) {
            DOMElement *elem = (DOMElement *)thisNode;
            DOMNode *attr = elem->getAttributeNodeNS(XMLUni::fgXMLNSURIName, XMLUni::fgXMLNSString);
            if (attr != 0) {
                const XMLCh *value = attr->getNodeValue();
                return XMLString::equals(namespaceURI, value);
            }
        }
        DOMNode *ancestor = getElementAncestor(thisNode);
        if (ancestor != 0) {
            return ancestor->isDefaultNamespace(namespaceURI);
        }

        return false;
    }
    case DOMNode::DOCUMENT_NODE:{
        return ((DOMDocument*)thisNode)->getDocumentElement()->isDefaultNamespace(namespaceURI);
    }

    case DOMNode::ENTITY_NODE :
    case DOMNode::NOTATION_NODE:
    case DOMNode::DOCUMENT_FRAGMENT_NODE:
    case DOMNode::DOCUMENT_TYPE_NODE:
        // type is unknown
        return false;
    case DOMNode::ATTRIBUTE_NODE:{
        if (fOwnerNode->getNodeType() == DOMNode::ELEMENT_NODE) {
            return fOwnerNode->isDefaultNamespace(namespaceURI);

        }
        return false;
    }
    default:{
        DOMNode *ancestor = getElementAncestor(thisNode);
        if (ancestor != 0) {
            return ancestor->isDefaultNamespace(namespaceURI);
        }
        return false;
    }

    }
}

DOMNode*         DOMNodeImpl::getInterface(const XMLCh*)      {
    throw DOMException(DOMException::NOT_SUPPORTED_ERR, 0, GetDOMNodeMemoryManager);
    return 0;
}


// non-standard extension
void DOMNodeImpl::release()
{
    // shouldn't reach here
    throw DOMException(DOMException::INVALID_ACCESS_ERR,0, GetDOMNodeMemoryManager);
}

XERCES_CPP_NAMESPACE_END

