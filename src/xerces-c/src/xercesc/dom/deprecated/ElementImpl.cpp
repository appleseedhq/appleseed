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
 * $Id: ElementImpl.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "DeepNodeListImpl.hpp"
#include "DocumentImpl.hpp"
#include "DocumentTypeImpl.hpp"
#include "DOM_DOMException.hpp"
#include "DStringPool.hpp"
#include "ElementImpl.hpp"
#include "ElementDefinitionImpl.hpp"
#include "NamedNodeMapImpl.hpp"
#include "NodeVector.hpp"

XERCES_CPP_NAMESPACE_BEGIN


static DOMString *gEmptyString = 0;
static XMLRegisterCleanup emptyStringCleanup;

ElementImpl::ElementImpl(DocumentImpl *ownerDoc, const DOMString &eName)
    : ParentNode(ownerDoc)
{
    name = eName.clone();
    attributes = null;
	setupDefaultAttributes();
};


ElementImpl::ElementImpl(const ElementImpl &other, bool deep)
    : ParentNode(other)
{
    name = other.name.clone();
	attributes = null;
	setupDefaultAttributes();
    if (deep)
        cloneChildren(other);
	if (other.attributes != null)
	{
		if (attributes)
		{
			attributes->removeAll();
			NamedNodeMapImpl::removeRef(attributes);
		}
		attributes = other.attributes->cloneAttrMap(this);
	}
};


ElementImpl::~ElementImpl()
{
    if (attributes)
    {
        attributes->removeAll();
        NamedNodeMapImpl::removeRef(attributes);
    }
};


NodeImpl *ElementImpl::cloneNode(bool deep)
{
    return new (getOwnerDocument()->getMemoryManager()) ElementImpl(*this, deep);
};


/**
 * NON-DOM
 * set the ownerDocument of this node, its children, and its attributes
 */
void ElementImpl::setOwnerDocument(DocumentImpl *doc) {
    ParentNode::setOwnerDocument(doc);
	if (attributes != null)
		attributes->setOwnerDocument(doc);
}


DOMString ElementImpl::getNodeName() {
    return name;
};


short ElementImpl::getNodeType() {
    return DOM_Node::ELEMENT_NODE;
};


DOMString ElementImpl::getAttribute(const DOMString &nam)
{
    AttrImpl * attr=null;

    if (attributes != null)
	attr=(AttrImpl *)(attributes->getNamedItem(nam));

    return (attr==null) ? DStringPool::getStaticString(""
                                                     , &gEmptyString
                                                     , reinitElementImpl
                                                     , emptyStringCleanup) : attr->getValue();
};



AttrImpl *ElementImpl::getAttributeNode(const DOMString &nam)
{
    return (attributes == 0) ? null : (AttrImpl *)(attributes->getNamedItem(nam));
};


NamedNodeMapImpl *ElementImpl::getAttributes()
{
    return attributes;
};



DeepNodeListImpl *ElementImpl::getElementsByTagName(const DOMString &tagname)
{
    return new (getOwnerDocument()->getMemoryManager()) DeepNodeListImpl(this,tagname);
};


DOMString ElementImpl::getTagName()
{
    return name;
}


bool ElementImpl::isElementImpl()
{
    return true;
};


void ElementImpl::removeAttribute(const DOMString &nam)
{
    if (getOwnerDocument()->getErrorChecking() && isReadOnly()) {
        throw DOM_DOMException(DOM_DOMException::NO_MODIFICATION_ALLOWED_ERR,
                               null);
    }
    if (attributes != null)
    {
    	AttrImpl *att = (AttrImpl *) attributes->getNamedItem(nam);
    	// Remove it
    	if (att != null)
    	{
    	    attributes->removeNamedItem(nam);
    	    if (att->nodeRefCount == 0)
    	        NodeImpl::deleteIf(att);
    	}
    }
};



AttrImpl *ElementImpl::removeAttributeNode(AttrImpl *oldAttr)
{
    if (getOwnerDocument()->getErrorChecking() && isReadOnly()) {
        throw DOM_DOMException(DOM_DOMException::NO_MODIFICATION_ALLOWED_ERR,
                               null);
    }
    if (attributes != null)
    {
	    AttrImpl *found = (AttrImpl *) attributes->getNamedItem(oldAttr->getName());

	    // If it is in fact the right object, remove it.

	    if (found == oldAttr)
	        attributes->removeNamedItem(oldAttr->getName());
	    else
	        throw DOM_DOMException(DOM_DOMException::NOT_FOUND_ERR, null);

        return found;	
		
	}
	return null;	// just to keep the compiler happy
};



AttrImpl *ElementImpl::setAttribute(const DOMString &nam, const DOMString &val)
{
    if (getOwnerDocument()->getErrorChecking() && isReadOnly()) {
        throw DOM_DOMException(DOM_DOMException::NO_MODIFICATION_ALLOWED_ERR,
                               null);
    }
    AttrImpl* newAttr = (AttrImpl*)getAttributeNode(nam);
    if (!newAttr)
    {
		if (attributes == 0) {
            attributes = new (getOwnerDocument()->getMemoryManager()) AttrMapImpl(this, null);
        }
        newAttr = (AttrImpl*)ownerDocument->createAttribute(nam);
        attributes->setNamedItem(newAttr);
    }

    newAttr->setNodeValue(val);       // Note that setNodeValue on attribute
                                      //   nodes takes care of deleting
                                      //   any previously existing children.
    return newAttr;
};



AttrImpl * ElementImpl::setAttributeNode(AttrImpl *newAttr)
{
    if (getOwnerDocument()->getErrorChecking() && isReadOnly()) {
        throw DOM_DOMException(DOM_DOMException::NO_MODIFICATION_ALLOWED_ERR,
                               null);
    }
    if (!(newAttr->isAttrImpl()))
        throw DOM_DOMException(DOM_DOMException::WRONG_DOCUMENT_ERR, null);
	if (attributes == 0) {
        attributes = new (getOwnerDocument()->getMemoryManager()) AttrMapImpl(this, null);
    }
    AttrImpl *oldAttr =
      (AttrImpl *) attributes->getNamedItem(newAttr->getName());
    // This will throw INUSE if necessary
    attributes->setNamedItem(newAttr);

    // Attr node reference counting note:
    // If oldAttr's refcount is zero at this point, here's what happens...
    //
    //      oldAttr is returned from this function to DOM_Attr::setAttributeNode,
    //      which wraps a DOM_Attr around the returned pointer and sends it
    //      up to application code, incrementing the reference count in the process.
    //      When the app DOM_Attr's destructor runs, the reference count is
    //      decremented back to zero and oldAttr will be deleted at that time.

    return oldAttr;
};


void ElementImpl::setReadOnly(bool readOnl, bool deep)
{
    ParentNode::setReadOnly(readOnl,deep);
    if (attributes != null)
        attributes->setReadOnly(readOnl,true);
};


//Introduced in DOM Level 2
DOMString ElementImpl::getAttributeNS(const DOMString &fNamespaceURI,
	const DOMString &fLocalName)
{
    AttrImpl * attr= (attributes != null) ?
      (AttrImpl *)(attributes->getNamedItemNS(fNamespaceURI, fLocalName))
    : null;
    return (attr==null) ? DOMString(null) : attr->getValue();
}


AttrImpl *ElementImpl::setAttributeNS(const DOMString &fNamespaceURI,
	const DOMString &qualifiedName, const DOMString &fValue)
{
    if (getOwnerDocument()->getErrorChecking() && isReadOnly()) {
        throw DOM_DOMException(DOM_DOMException::NO_MODIFICATION_ALLOWED_ERR,
                               null);
    }
    AttrImpl *newAttr =
      (AttrImpl *) ownerDocument->createAttributeNS(fNamespaceURI,
                                                    qualifiedName);
    newAttr->setNodeValue(fValue);
	if (attributes == 0) {
        attributes = new (getOwnerDocument()->getMemoryManager()) AttrMapImpl(this, null);
    }
    AttrImpl *oldAttr = (AttrImpl *)attributes->setNamedItem(newAttr);

    if (oldAttr) {
	if (oldAttr->nodeRefCount == 0)
	    NodeImpl::deleteIf(oldAttr);
    }
    return newAttr;
}


void ElementImpl::removeAttributeNS(const DOMString &fNamespaceURI,
	const DOMString &fLocalName)
{
    if (getOwnerDocument()->getErrorChecking() && isReadOnly()) {
        throw DOM_DOMException(DOM_DOMException::NO_MODIFICATION_ALLOWED_ERR,
                               null);
    }
    if (attributes != null)
    {
		AttrImpl *att =
		  (AttrImpl *) attributes->getNamedItemNS(fNamespaceURI, fLocalName);
		// Remove it
		if (att != null) {
			attributes->removeNamedItemNS(fNamespaceURI, fLocalName);
			if (att->nodeRefCount == 0)
				NodeImpl::deleteIf(att);
		}
	}
}


AttrImpl *ElementImpl::getAttributeNodeNS(const DOMString &fNamespaceURI,
	const DOMString &fLocalName)
{
    return (attributes == 0) ? null : (AttrImpl *)(attributes->getNamedItemNS(fNamespaceURI, fLocalName));
}


AttrImpl *ElementImpl::setAttributeNodeNS(AttrImpl *newAttr)
{
    if (getOwnerDocument()->getErrorChecking()) {
        if (isReadOnly()) {
            throw DOM_DOMException(
                                 DOM_DOMException::NO_MODIFICATION_ALLOWED_ERR,
                                 null);
        }
        if (newAttr->getOwnerDocument() != this->getOwnerDocument()) {
            throw DOM_DOMException(DOM_DOMException::WRONG_DOCUMENT_ERR, null);
        }
    }
    if (attributes == 0) {
        attributes = new (getOwnerDocument()->getMemoryManager()) AttrMapImpl(this, null);
    }
    AttrImpl *oldAttr = (AttrImpl *) attributes->getNamedItemNS(newAttr->getNamespaceURI(), newAttr->getLocalName());

    // This will throw INUSE if necessary
    attributes->setNamedItemNS(newAttr);

    // Attr node reference counting note:
    // If oldAttr's refcount is zero at this point, here's what happens...
    //
    //      oldAttr is returned from this function to DOM_Attr::setAttributeNode,
    //      which wraps a DOM_Attr around the returned pointer and sends it
    //      up to application code, incrementing the reference count in the process.
    //      When the app DOM_Attr's destructor runs, the reference count is
    //      decremented back to zero and oldAttr will be deleted at that time.

    return oldAttr;
}


DeepNodeListImpl *ElementImpl::getElementsByTagNameNS(const DOMString &fNamespaceURI,
	const DOMString &fLocalName)
{
    return new (getOwnerDocument()->getMemoryManager())DeepNodeListImpl(this,fNamespaceURI, fLocalName);
}

bool ElementImpl::hasAttributes()
{
    return (attributes != null && attributes->getLength() != 0);
};

bool ElementImpl::hasAttribute(const DOMString &name)
{
    return (getAttributeNode(name) != null);
};

bool ElementImpl::hasAttributeNS(const DOMString &namespaceURI,
	const DOMString &localName)
{
    return (getAttributeNodeNS(namespaceURI, localName) != null);
};


// DOM_NamedNodeMap UTILITIES
NamedNodeMapImpl *ElementImpl::NNM_cloneMap(NodeImpl *nnm_ownerNode)
{
	return (getAttributes() == null) ? null : nnm_ownerNode->getAttributes()->cloneMap(nnm_ownerNode);
}

int ElementImpl::NNM_findNamePoint(const DOMString &nnm_name)
{
	return (getAttributes() == null) ? -1 : getAttributes()->findNamePoint(nnm_name);
}

unsigned int ElementImpl::NNM_getLength()
{
	return (getAttributes() == null) ? 0 : getAttributes()->getLength();
}

NodeImpl *ElementImpl::NNM_getNamedItem(const DOMString &nnm_name)
{
	return (getAttributes() == null) ? null : getAttributes()->getNamedItem(nnm_name);
}

NodeImpl *ElementImpl::NNM_item(unsigned int nnm_index)
{
	return (getAttributes() == null) ? null : getAttributes()->item(nnm_index);
}

void ElementImpl::NNM_removeAll()
{
	if (getAttributes() != null)
		getAttributes()->removeAll();
}

NodeImpl *ElementImpl::NNM_removeNamedItem(const DOMString &nnm_name)
{
	if (getAttributes() == null)
		throw DOM_DOMException(DOM_DOMException::NOT_FOUND_ERR, null);
	else
		return getAttributes()->removeNamedItem(nnm_name);
	return null;
}

NodeImpl *ElementImpl::NNM_setNamedItem(NodeImpl *nnm_arg)
{
	if (getAttributes() == null) {
	    attributes = new (getOwnerDocument()->getMemoryManager()) AttrMapImpl(this);
    }
	return attributes->setNamedItem(nnm_arg);
}

void ElementImpl::NNM_setReadOnly(bool nnm_readOnly, bool nnm_deep)
{
	if (getAttributes() != null)
		getAttributes()->setReadOnly(nnm_readOnly, nnm_deep);
}

int ElementImpl::NNM_findNamePoint(const DOMString &nnm_namespaceURI, const DOMString &nnm_localName)
{
	return (getAttributes() == null) ? -1 : getAttributes()->findNamePoint(nnm_namespaceURI, nnm_localName);
}

NodeImpl *ElementImpl::NNM_getNamedItemNS(const DOMString &nnm_namespaceURI, const DOMString &nnm_localName)
{
	return (getAttributes() == null) ? null : getAttributes()->getNamedItemNS(nnm_namespaceURI, nnm_localName);
}

NodeImpl *ElementImpl::NNM_setNamedItemNS(NodeImpl *nnm_arg)
{
	if (getAttributes() == null) {
        attributes = new (getOwnerDocument()->getMemoryManager()) AttrMapImpl(this);
    }
	return getAttributes()->setNamedItemNS(nnm_arg);
}

NodeImpl *ElementImpl::NNM_removeNamedItemNS(const DOMString &nnm_namespaceURI, const DOMString &nnm_localName)
{
	if (getAttributes() == null)
        throw DOM_DOMException(DOM_DOMException::NOT_FOUND_ERR, null);
	else
		return getAttributes()->removeNamedItemNS(nnm_namespaceURI, nnm_localName);
	return null;
}

void ElementImpl::NNM_setOwnerDocument(DocumentImpl *nnm_doc)
{
	if (getAttributes() != null)
		getAttributes()->setOwnerDocument(nnm_doc);
}


// util functions for default attributes
// returns the default attribute map for this node from the owner document
AttrMapImpl *ElementImpl::getDefaultAttributes()
{
	if ((ownerNode == null) || (getOwnerDocument() == null))
		return null;

	DocumentImpl *tmpdoc = getOwnerDocument();
	if (tmpdoc->getDoctype() == null)
		return null;
	
	NodeImpl *eldef = tmpdoc->getDoctype()->getElements()->getNamedItem(getNodeName());
	return (eldef == null) ? null : (AttrMapImpl *)(eldef->getAttributes());
}

// resets all attributes for this node to their default values
void ElementImpl::setupDefaultAttributes()
{
	if ((ownerNode == null) || (getOwnerDocument() == null) || (getOwnerDocument()->getDoctype() == null))
		return;

	if (attributes != 0)
		delete attributes;
	
	AttrMapImpl* defAttrs = getDefaultAttributes();
	if (defAttrs) {
        attributes = new (getOwnerDocument()->getMemoryManager()) AttrMapImpl(this, defAttrs);
    }
}

// -----------------------------------------------------------------------
//  Notification that lazy data has been deleted
// -----------------------------------------------------------------------
void ElementImpl::reinitElementImpl() {

    delete gEmptyString;
    gEmptyString = 0;

}

XERCES_CPP_NAMESPACE_END

