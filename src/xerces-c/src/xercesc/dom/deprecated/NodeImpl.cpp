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
 * $Id: NodeImpl.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

// This class doesn't support having any children, and implements the behavior
// of an empty NodeList as far getChildNodes is concerned.
// The ParentNode subclass overrides this behavior.

#include "NodeImpl.hpp"
#include "AttrImpl.hpp"
#include "DOM_DOMException.hpp"
#include "DOM_Node.hpp"
#include "DOM_DOMImplementation.hpp"
#include "DOMString.hpp"
#include "DStringPool.hpp"
#include "DocumentImpl.hpp"
#include "NodeIDMap.hpp"
#include "stdio.h"
#include "TextImpl.hpp"

XERCES_CPP_NAMESPACE_BEGIN


static DOMString *s_xml = null;
static DOMString *s_xmlURI = null;
static DOMString *s_xmlns = null;
static DOMString *s_xmlnsURI = null;

static XMLRegisterCleanup nodeImplCleanup;

const unsigned short NodeImpl::READONLY     = 0x1<<0;
const unsigned short NodeImpl::SYNCDATA     = 0x1<<1;
const unsigned short NodeImpl::SYNCCHILDREN = 0x1<<2;
const unsigned short NodeImpl::OWNED        = 0x1<<3;
const unsigned short NodeImpl::FIRSTCHILD   = 0x1<<4;
const unsigned short NodeImpl::SPECIFIED    = 0x1<<5;
const unsigned short NodeImpl::IGNORABLEWS  = 0x1<<6;
const unsigned short NodeImpl::SETVALUE     = 0x1<<7;
const unsigned short NodeImpl::ID_ATTR      = 0x1<<8;
const unsigned short NodeImpl::USERDATA     = 0x1<<9;
const unsigned short NodeImpl::HASSTRING    = 0x1<<10;


int  NodeImpl::gLiveNodeImpls = 0;         // Counters for debug & tuning.
int  NodeImpl::gTotalNodeImpls= 0;


NodeImpl::NodeImpl(DocumentImpl *ownerDoc)
{
    this->flags = 0;
    // as long as we do not have any owner, ownerNode is our ownerDocument
    this->ownerNode  = ownerDoc;

    this->nodeRefCount = 0;
    NodeImpl::gLiveNodeImpls++;
    NodeImpl::gTotalNodeImpls++;
};

// This only makes a shallow copy, cloneChildren must also be called for a
// deep clone
NodeImpl::NodeImpl(const NodeImpl &other) : NodeListImpl() 
{
    this->flags = other.flags;
    this->isReadOnly(false);

    this->nodeRefCount = 0;
    NodeImpl::gLiveNodeImpls++;
    NodeImpl::gTotalNodeImpls++;

    // Need to break the association w/ original parent
    //    this->ownerNode = other.getOwnerDocument(); this doesn't work???
    this->ownerNode = ((NodeImpl*)&other)->getOwnerDocument();
    this->isOwned(false);
};



NodeImpl::~NodeImpl() {
	if (hasUserData())
	{
		setUserData(null);
	}
    NodeImpl::gLiveNodeImpls--;
};


// Dynamic Cast substitute functions
bool NodeImpl::isAttrImpl()              {return false;};
bool NodeImpl::isCDATASectionImpl()      {return false;};
bool NodeImpl::isDocumentFragmentImpl()  {return false;};
bool NodeImpl::isDocumentImpl()          {return false;};
bool NodeImpl::isDocumentTypeImpl()      {return false;};
bool NodeImpl::isElementImpl()           {return false;};
bool NodeImpl::isEntityReference()       {return false;};
bool NodeImpl::isTextImpl()              {return false;};


void NodeImpl::changed() {
    // we do not actually store this information on every node, we only
    // have a global indicator on the Document. Doing otherwise cost us too
    // much for little gain.
    getDocument()->changed();
}

int NodeImpl::changes()
{
    // we do not actually store this information on every node, we only
    // have a global indicator on the Document. Doing otherwise cost us too
    // much for little gain.
    return getDocument()->changes();
};


NodeImpl * NodeImpl::appendChild(NodeImpl *newChild)
{
    return insertBefore(newChild, null);
};


//  NodeImpl::deleteIf is called when a node's reference count goes
//  to 0.  It is separate function from removeRef because removeRef
//  is likely to be in-lined.
//
//  See comments at RefCountedImpl::removeRef().
//
void NodeImpl::deleteIf(NodeImpl *thisNode)
{
    if (thisNode == 0)
        return;

    if (thisNode->isOwned())
        return;

    // Delete this node.  There should be no siblings, as the DOM
    //  supports no node operations that would detach a node from
    //  its parent while retaining siblings.
    //  The target node may have children, in which case they must
    //  be removed from this node before deleting this node.

    // First, if this node is an ID attribute, we need to remove it
    // from the hashtable of element IDs before removing the Attrs
    //   children.  This is because the Attr's children Text nodes
    //   contain the attr's value, which is the hash table key.
    //
    if (thisNode->isAttrImpl() && ((AttrImpl *)thisNode->isIdAttr()))
    {
        ((AttrImpl *)thisNode)->getOwnerDocument() ->
            getNodeIDMap()->remove((AttrImpl *)thisNode);
    }

    thisNode->isReadOnly(false);   // removeChild requires node not be readonly.
    NodeImpl *theNextChild;
    for (NodeImpl *child = thisNode->getFirstChild(); child != 0;
         child=theNextChild)
    {
        theNextChild = child->getNextSibling();
        thisNode->removeChild(child);
        if (child->nodeRefCount == 0)
            deleteIf(child);
    }
    delete thisNode;
};



NamedNodeMapImpl * NodeImpl::getAttributes() {
    return 0;                   // overridden in ElementImpl
};


NodeListImpl *NodeImpl::getChildNodes() {
    return this;                // overridden in ParentNode
};



NodeImpl * NodeImpl::getFirstChild() {
    return 0;                   // overridden in ParentNode
};


NodeImpl * NodeImpl::getLastChild()
{
    return 0;                   // overridden in ParentNode
};


unsigned int NodeImpl::getLength() {
    return 0;                   // overridden in ParentNode
};


NodeImpl * NodeImpl::getNextSibling() {
    return null;                // overridden in ChildNode
};



DOMString NodeImpl::getNodeValue()
{
    return null;                // overridden in some subclasses
};


DocumentImpl *NodeImpl::getOwnerDocument()
{
    // if we have an owner simply forward the request
    // otherwise ownerNode is our ownerDocument
    if (isOwned()) {
        return ownerNode->getDocument();
    } else {
        return (DocumentImpl *) ownerNode;
    }
};

// unlike getOwnerDocument this is not overriden by DocumentImpl to return null
DocumentImpl *NodeImpl::getDocument()
{
    // if we have an owner simply forward the request
    // otherwise ownerNode is our ownerDocument
    if (isOwned()) {
        return ownerNode->getDocument();
    } else {
        return (DocumentImpl *) ownerNode;
    }
};


void NodeImpl::setOwnerDocument(DocumentImpl *doc) {
    // if we have an owner we rely on it to have it right
    // otherwise ownerNode is our ownerDocument
    if (!isOwned()) {
        ownerNode = doc;
    }
}

NodeImpl * NodeImpl::getParentNode()
{
    return null;                // overridden in ChildNode
}


NodeImpl*  NodeImpl::getPreviousSibling()
{
    return null;                // overridden in ChildNode
}


void *NodeImpl::getUserData()
{
	return (hasUserData()) ? getOwnerDocument()->getUserData(this) : null;
}


bool NodeImpl::hasChildNodes()
{
    return false;
}



NodeImpl *NodeImpl::insertBefore(NodeImpl * /*newChild*/, NodeImpl * /*refChild*/) {
    throw DOM_DOMException(DOM_DOMException::HIERARCHY_REQUEST_ERR,null);
    return 0;
}

NodeImpl *NodeImpl::item(unsigned int /*index*/) {
    return 0;
}


NodeImpl *NodeImpl::removeChild(NodeImpl * /*oldChild*/)
{
    throw DOM_DOMException(DOM_DOMException::NOT_FOUND_ERR, null);
    return 0;
}


NodeImpl *NodeImpl::replaceChild(NodeImpl * /*newChild*/, NodeImpl * /*oldChild*/)
{
    throw DOM_DOMException(DOM_DOMException::HIERARCHY_REQUEST_ERR,null);
    return 0;
}


void NodeImpl::referenced()
{
    RefCountedImpl::addRef(this->getOwnerDocument());
}


//
//    unreferenced  will be called whenever the refernce count on
//            this node goes from 1 to 0.  This node will only be
//            directly deleted here  (by deleteIf) if it is outside
//            of the document tree.
//
void NodeImpl::unreferenced()
{
    DocumentImpl *doc = this->getOwnerDocument();
    deleteIf(this);       // This gets nodes outside of the document -
    //   deleteIf() deletes only if the parent
    //   node is null.

    // If this was the last external reference within the document,
    //    the entire document will be deleted as well.
    RefCountedImpl::removeRef(doc);
}


void NodeImpl::setNodeValue(const DOMString & /*val*/)
{
    // Default behavior is to do nothing, overridden in some subclasses
}



void NodeImpl::setReadOnly(bool readOnly, bool /*deep*/)
{
    this->isReadOnly(readOnly);
    // by default we do not have children, so deep is meaningless
    // this is overridden by ParentNode
}


void NodeImpl::setUserData(void * val)
{
	getOwnerDocument()->setUserData(this, val);
	if (val)
		hasUserData(true);
	else
		hasUserData(false);
};



DOMString NodeImpl::toString()
{
	return DOMString("[")+getNodeName()+": "+getNodeValue()+"]";
	// return getNodeName();
};

//Introduced in DOM Level 2

void NodeImpl::normalize()
{
    // does nothing by default, overridden by subclasses
};


bool NodeImpl::isSupported(const DOMString &feature, const DOMString &version)
{
    return DOM_DOMImplementation::getImplementation().hasFeature(feature, version);
}

DOMString NodeImpl::getNamespaceURI()
{
    return 0;
}

DOMString NodeImpl::getPrefix()
{
    return 0;
}

DOMString NodeImpl::getLocalName()
{
    return 0;
}


void NodeImpl::setPrefix(const DOMString & /*fPrefix*/)
{
    throw DOM_DOMException(DOM_DOMException::NAMESPACE_ERR,null);
}

bool NodeImpl::hasAttributes() {
    return 0;                   // overridden in ElementImpl
};


DOMString NodeImpl::getXmlnsString() {
    return DStringPool::getStaticString("xmlns"
                                      , &s_xmlns
                                      , reinitNodeImpl
                                      , nodeImplCleanup
                                      );
}

DOMString NodeImpl::getXmlnsURIString() {
    return DStringPool::getStaticString("http://www.w3.org/2000/xmlns/"
                                      , &s_xmlnsURI
                                      , reinitNodeImpl
                                      , nodeImplCleanup
                                        );
}

DOMString NodeImpl::getXmlString() {
    return DStringPool::getStaticString("xml"
                                      , &s_xml
                                      , reinitNodeImpl
                                      , nodeImplCleanup
                                      );
}

DOMString NodeImpl::getXmlURIString() {
    return DStringPool::getStaticString("http://www.w3.org/XML/1998/namespace"
                                      , &s_xmlURI
                                      , reinitNodeImpl
                                      , nodeImplCleanup
                                        );
}

//Return a URI mapped from the given prefix and namespaceURI as below
//	prefix   namespaceURI		output
//---------------------------------------------------
//	"xml"      xmlURI            xmlURI
//	"xml"	   otherwise         NAMESPACE_ERR
//	"xmlns"	   xmlnsURI	         xmlnsURI (nType = ATTRIBUTE_NODE only)
//	"xmlns"	   otherwise         NAMESPACE_ERR (nType = ATTRIBUTE_NODE only)
//   != null   null or ""        NAMESPACE_ERR
//	else       any			     namesapceURI
const DOMString& NodeImpl::mapPrefix(const DOMString &prefix,
                                     const DOMString &namespaceURI, short nType)
{
    DOMString xml = DStringPool::getStaticString("xml"
                                               , &s_xml
                                               , reinitNodeImpl
                                               , nodeImplCleanup
                                               );
    DOMString xmlURI = DStringPool::getStaticString("http://www.w3.org/XML/1998/namespace"
                                                  , &s_xmlURI
                                                  , reinitNodeImpl
                                                  , nodeImplCleanup
                                                  );
    DOMString xmlns = DStringPool::getStaticString("xmlns"
                                                 , &s_xmlns
                                                 , reinitNodeImpl
                                                 , nodeImplCleanup
                                                 );
    DOMString xmlnsURI = DStringPool::getStaticString("http://www.w3.org/2000/xmlns/"
                                                    , &s_xmlnsURI
                                                    , reinitNodeImpl
                                                    , nodeImplCleanup
                                                    );

    if (prefix == null)
        return namespaceURI;
    if (prefix.equals(xml)) {
        if (namespaceURI.equals(xmlURI))
            return *s_xmlURI;
        throw DOM_DOMException(DOM_DOMException::NAMESPACE_ERR, null);
    } else if (nType == DOM_Node::ATTRIBUTE_NODE && prefix.equals(xmlns)) {
        if (namespaceURI.equals(xmlnsURI))
            return *s_xmlnsURI;
        throw DOM_DOMException(DOM_DOMException::NAMESPACE_ERR, null);
    } else if (namespaceURI == null || namespaceURI.length() == 0) {
        throw DOM_DOMException(DOM_DOMException::NAMESPACE_ERR, null);
    } else
        return namespaceURI;
    return namespaceURI;
}

// -----------------------------------------------------------------------
//  Notification that lazy data has been deleted
// -----------------------------------------------------------------------
void NodeImpl::reinitNodeImpl() {

    delete s_xml;
    s_xml = 0;

    delete s_xmlURI;
    s_xmlURI = 0;

    delete s_xmlns;
    s_xmlns = 0;

    delete s_xmlnsURI;
    s_xmlnsURI = 0;

}

XERCES_CPP_NAMESPACE_END

