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
 * $Id: NamedNodeMapImpl.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "NamedNodeMapImpl.hpp"
#include "NodeVector.hpp"
#include "AttrImpl.hpp"
#include "DOM_DOMException.hpp"
#include "DocumentImpl.hpp"

XERCES_CPP_NAMESPACE_BEGIN


int        NamedNodeMapImpl::gLiveNamedNodeMaps  = 0;
int        NamedNodeMapImpl::gTotalNamedNodeMaps = 0;

NamedNodeMapImpl::NamedNodeMapImpl(NodeImpl *ownerNod)
{
    this->ownerNode=ownerNod;
    this->nodes = null;
    this->readOnly = false;
    this->refCount = 1;
    gLiveNamedNodeMaps++;
    gTotalNamedNodeMaps++;
};



NamedNodeMapImpl::~NamedNodeMapImpl()
{
    if (nodes)
    {
        // It is the responsibility of whoever was using the named node
        // map to do any cleanup on the nodes contained in the map
        //  before deleting it.
        delete nodes;
        nodes = 0;
    }
    gLiveNamedNodeMaps--;
};


void NamedNodeMapImpl::addRef(NamedNodeMapImpl *This)
{
    if (This)
        ++This->refCount;
};


NamedNodeMapImpl *NamedNodeMapImpl::cloneMap(NodeImpl *ownerNod)
{
    MemoryManager* manager = ownerNod->getDocument()->getMemoryManager();
    NamedNodeMapImpl *newmap = new (manager) NamedNodeMapImpl(ownerNod);
	
    if (nodes != null)
    {
        newmap->nodes = new (manager) NodeVector(nodes->size(), manager);
        for (unsigned int i = 0; i < nodes->size(); ++i)
        {
            NodeImpl *n = nodes->elementAt(i)->cloneNode(true);
			n->isSpecified(nodes->elementAt(i)->isSpecified());
            n->ownerNode = ownerNod;
            n->isOwned(true);
            newmap->nodes->addElement(n);
        }
    }

    return newmap;
};


//
//  removeAll - This function removes all elements from a named node map.
//              It is called from the destructors for Elements and DocumentTypes,
//              to remove the contents when the owning Element or DocType goes
//              away.  The empty NamedNodeMap may persist if the user code
//              has a reference to it.
//
//              AH Revist - the empty map should be made read-only, since
//              adding it was logically part of the [Element, DocumentType]
//              that has been deleted, and adding anything new to it would
//              be meaningless, and almost certainly an error.
//
void NamedNodeMapImpl::removeAll()
{
    if (nodes)
    {

        for (int i=nodes->size()-1; i>=0; i--)
        {
            NodeImpl *n = nodes->elementAt(i);
            n->ownerNode = ownerNode->getOwnerDocument();
            n->isOwned(false);
            if (n->nodeRefCount == 0)
                NodeImpl::deleteIf(n);
        }
        delete nodes;
        nodes = null;
    }
}



int NamedNodeMapImpl::findNamePoint(const DOMString &name)
{

    // Binary search
    int i=0;
    if(nodes!=null)
    {
        int first=0,last=nodes->size()-1;

        while(first<=last)
        {
            i=(first+last)/2;
            int test = name.compareString(nodes->elementAt(i)->getNodeName());
            if(test==0)
                return i; // Name found
            else if(test<0)
                last=i-1;
            else
                first=i+1;
        }
        if(first>i) i=first;
    }
    /********************
    // Linear search
    int i = 0;
    if (nodes != null)
    for (i = 0; i < nodes.size(); ++i)
    {
    int test = name.compareTo(((NodeImpl *) (nodes.elementAt(i))).getNodeName());
    if (test == 0)
    return i;
    else
    if (test < 0)
    {
    break; // Found insertpoint
    }
    }

    *******************/
    return -1 - i; // not-found has to be encoded.
};



unsigned int NamedNodeMapImpl::getLength()
{
    return (nodes != null) ? nodes->size() : 0;
};



NodeImpl * NamedNodeMapImpl::getNamedItem(const DOMString &name)
{
    int i=findNamePoint(name);
    return (i<0) ? null : (NodeImpl *)(nodes->elementAt(i));
};



NodeImpl * NamedNodeMapImpl::item(unsigned int index)
{
    return (nodes != null && index < nodes->size()) ?
        (NodeImpl *) (nodes->elementAt(index)) : null;
};


//
// removeNamedItem() - Remove the named item, and return it.
//                      The caller must arrange for deletion of the
//                      returned item if its refcount has gone to zero -
//                      we can't do it here because the caller would
//                      never see the returned node.
//
NodeImpl * NamedNodeMapImpl::removeNamedItem(const DOMString &name)
{
    if (readOnly)
        throw DOM_DOMException(
            DOM_DOMException::NO_MODIFICATION_ALLOWED_ERR, null);
    int i=findNamePoint(name);
    NodeImpl *n = null;

    if(i<0)
        throw DOM_DOMException(DOM_DOMException::NOT_FOUND_ERR, null);

    n = (NodeImpl *) (nodes->elementAt(i));
    nodes->removeElementAt(i);
    n->ownerNode = ownerNode->getOwnerDocument();
    n->isOwned(false);
    return n;
};



void NamedNodeMapImpl::removeRef(NamedNodeMapImpl *This)
{
    if (This && --This->refCount == 0)
        delete This;
};

//
// setNamedItem()  Put the item into the NamedNodeList by name.
//                  If an item with the same name already was
//                  in the list, replace it.  Return the old
//                  item, if there was one.
//                  Caller is responsible for arranging for
//                  deletion of the old item if its ref count is
//                  zero.
//
NodeImpl * NamedNodeMapImpl::setNamedItem(NodeImpl * arg)
{
    if(arg->getOwnerDocument() != ownerNode->getOwnerDocument())
        throw DOM_DOMException(DOM_DOMException::WRONG_DOCUMENT_ERR,null);
    if (readOnly)
        throw DOM_DOMException(DOM_DOMException::NO_MODIFICATION_ALLOWED_ERR, null);
    if ((arg->getNodeType() == DOM_Node::ATTRIBUTE_NODE) && arg->isOwned() && (arg->ownerNode != ownerNode))
        throw DOM_DOMException(DOM_DOMException::INUSE_ATTRIBUTE_ERR,null);

    arg->ownerNode = ownerNode;
    arg->isOwned(true);
    int i=findNamePoint(arg->getNodeName());
    NodeImpl * previous=null;
    if(i>=0)
    {
        previous = nodes->elementAt(i);
        nodes->setElementAt(arg,i);
    }
    else
    {
        i=-1-i; // Insert point (may be end of list)
        if(null==nodes) {
            MemoryManager* manager = ownerNode->getDocument()->getMemoryManager();
            nodes=new (manager) NodeVector(manager);
        }
        nodes->insertElementAt(arg,i);
    }
    if (previous != null) {
        previous->ownerNode = ownerNode->getOwnerDocument();
        previous->isOwned(false);
    }

    return previous;
};


void NamedNodeMapImpl::setReadOnly(bool readOnl, bool deep)
{
    this->readOnly=readOnl;
    if(deep && nodes!=null)
    {
        //Enumeration e=nodes->elements();
        //while(e->hasMoreElements())
        //      ((NodeImpl)e->nextElement())->setReadOnly(readOnl,deep);
        int sz = nodes->size();
        for (int i=0; i<sz; ++i) {
            nodes->elementAt(i)->setReadOnly(readOnl, deep);
        }
    }
};


//Introduced in DOM Level 2

int NamedNodeMapImpl::findNamePoint(const DOMString &namespaceURI,
	const DOMString &localName)
{
    if (nodes == null)
	return -1;
    // This is a linear search through the same nodes Vector.
    // The Vector is sorted on the DOM Level 1 nodename.
    // The DOM Level 2 NS keys are namespaceURI and Localname,
    // so we must linear search thru it.
    // In addition, to get this to work with nodes without any namespace
    // (namespaceURI and localNames are both null) we then use the nodeName
    // as a secondary key.
    int i, len = nodes -> size();
    for (i = 0; i < len; ++i) {
	NodeImpl *node = nodes -> elementAt(i);
	if (! node -> getNamespaceURI().equals(namespaceURI))	//URI not match
	    continue;
        DOMString nNamespaceURI = node->getNamespaceURI();
        DOMString nLocalName = node->getLocalName();
        if (namespaceURI == null) {
            if (nNamespaceURI == null
                &&
                (localName.equals(nLocalName)
                 ||
                 (nLocalName == null && localName.equals(node->getNodeName()))))
                return i;
        } else {
            if (namespaceURI.equals(nNamespaceURI)
                &&
                localName.equals(nLocalName))
                return i;
        }
    }
    return -1;	//not found
}


NodeImpl *NamedNodeMapImpl::getNamedItemNS(const DOMString &namespaceURI,
	const DOMString &localName)
{
    int i = findNamePoint(namespaceURI, localName);
    return i < 0 ? null : nodes -> elementAt(i);
}


//
// setNamedItemNS()  Put the item into the NamedNodeList by name.
//                  If an item with the same name already was
//                  in the list, replace it.  Return the old
//                  item, if there was one.
//                  Caller is responsible for arranging for
//                  deletion of the old item if its ref count is
//                  zero.
//
NodeImpl * NamedNodeMapImpl::setNamedItemNS(NodeImpl *arg)
{
    if (arg->getOwnerDocument() != ownerNode->getOwnerDocument())
        throw DOM_DOMException(DOM_DOMException::WRONG_DOCUMENT_ERR,null);
    if (readOnly)
        throw DOM_DOMException(DOM_DOMException::NO_MODIFICATION_ALLOWED_ERR, null);
    if (arg->isOwned())
        throw DOM_DOMException(DOM_DOMException::INUSE_ATTRIBUTE_ERR,null);

    arg->ownerNode = ownerNode;
    arg->isOwned(true);
    int i=findNamePoint(arg->getNamespaceURI(), arg->getLocalName());
    NodeImpl *previous=null;
    if(i>=0) {
        previous = nodes->elementAt(i);
        nodes->setElementAt(arg,i);
    } else {
        i=findNamePoint(arg->getNodeName()); // Insert point (may be end of list)
        if (i<0)
          i = -1 - i;
        if(null==nodes) {
            MemoryManager* manager = ownerNode->getDocument()->getMemoryManager();
            nodes=new (manager) NodeVector(manager);
        }
        nodes->insertElementAt(arg,i);
    }
    if (previous != null) {
        previous->ownerNode = ownerNode->getOwnerDocument();
        previous->isOwned(false);
    }

    return previous;
};


// removeNamedItemNS() - Remove the named item, and return it.
//                      The caller must arrange for deletion of the
//                      returned item if its refcount has gone to zero -
//                      we can't do it here because the caller would
//                      never see the returned node.
NodeImpl *NamedNodeMapImpl::removeNamedItemNS(const DOMString &namespaceURI,
	const DOMString &localName)
{
    if (readOnly)
        throw DOM_DOMException(
        DOM_DOMException::NO_MODIFICATION_ALLOWED_ERR, null);
    int i = findNamePoint(namespaceURI, localName);
    if (i < 0)
        throw DOM_DOMException(DOM_DOMException::NOT_FOUND_ERR, null);

    NodeImpl * n = nodes -> elementAt(i);
    nodes -> removeElementAt(i);	//remove n from nodes
    n->ownerNode = ownerNode->getOwnerDocument();
    n->isOwned(false);
    return n;
}

/**
 * NON-DOM
 * set the ownerDocument of this node, its children, and its attributes
 */
void NamedNodeMapImpl::setOwnerDocument(DocumentImpl *doc) {
    if (nodes != null) {
        for (unsigned int i = 0; i < nodes->size(); i++) {
            item(i)->setOwnerDocument(doc);
        }
    }
}


void NamedNodeMapImpl::cloneContent(NamedNodeMapImpl *srcmap) {
   if ((srcmap != null) && (srcmap->nodes != null) && (srcmap->nodes->size() > 0))
	{
		if (nodes != null) {
			delete nodes;
		}

        MemoryManager* manager = ownerNode->getDocument()->getMemoryManager();
        nodes = new (manager) NodeVector(srcmap->nodes->size(), manager);
		for (unsigned int i = 0; i < srcmap->nodes->size(); i++)
		{
			NodeImpl *n = srcmap->nodes->elementAt(i);
 			NodeImpl *clone = n->cloneNode(true);
			clone->isSpecified(n->isSpecified());
			clone->ownerNode = ownerNode;
			clone->isOwned(true);
			nodes->addElement(clone);
//			n = null;
//			clone = null;
		}
	}
}

XERCES_CPP_NAMESPACE_END

