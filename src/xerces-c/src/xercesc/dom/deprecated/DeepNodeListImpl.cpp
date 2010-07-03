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
 * $Id: DeepNodeListImpl.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "DeepNodeListImpl.hpp"
#include "NodeVector.hpp"
#include "NodeImpl.hpp"
#include "ElementImpl.hpp"
#include "DStringPool.hpp"
#include "DocumentImpl.hpp"
#include <limits.h>

XERCES_CPP_NAMESPACE_BEGIN


static DOMString *kAstr = 0;
static XMLRegisterCleanup kAstrCleanup;

DeepNodeListImpl::DeepNodeListImpl(NodeImpl *rootNod, const DOMString &tagNam)
{
    changes = 0;
    this->rootNode = rootNod;
    this->tagName = tagNam;
    MemoryManager* manager= rootNod->getDocument()->getMemoryManager();
    nodes=new (manager) NodeVector(manager);
    matchAll = tagName.equals(DStringPool::getStaticString("*"
                                                         , &kAstr
                                                         , reinitDeepNodeListImpl
                                                         , kAstrCleanup));
    this->namespaceURI = null;	//DOM Level 2
    this->matchAllURI = false;	//DOM Level 2
    this->matchURIandTagname = false;	//DOM Level 2
};


//DOM Level 2
DeepNodeListImpl::DeepNodeListImpl(NodeImpl *rootNod,
    const DOMString &fNamespaceURI, const DOMString &localName)
{
    changes = 0;
    this->rootNode = rootNod;
    this->tagName = localName;
    MemoryManager* manager = rootNod->getDocument()->getMemoryManager();
    nodes=new (manager) NodeVector(manager);
    matchAll = tagName.equals(DStringPool::getStaticString("*"
                                                         , &kAstr
                                                         , reinitDeepNodeListImpl
                                                         , kAstrCleanup));

    this->namespaceURI = fNamespaceURI;
    this->matchAllURI = fNamespaceURI.equals(DStringPool::getStaticString("*"
                                                                        , &kAstr
                                                                        , reinitDeepNodeListImpl
                                                                        , kAstrCleanup));

    this->matchURIandTagname = true;
};


DeepNodeListImpl::~DeepNodeListImpl()
{
    delete nodes;
};


unsigned int DeepNodeListImpl::getLength()
{
    // Preload all matching elements. (Stops when we run out of subtree!)
    item(INT_MAX);
    return nodes->size();
};



// Start from the first child and count forward, 0-based. index>length-1
// should return null.
//
// Attempts to do only work actually requested, cache work already
// done, and to flush that cache when the tree has changed.
//
// LIMITATION: ????? Unable to tell relevant tree-changes from
// irrelevant ones.  Doing so in a really useful manner would seem
// to involve a tree-walk in its own right, or maintaining our data
// in a parallel tree.
NodeImpl *DeepNodeListImpl::item(unsigned int index)
{
    NodeImpl *thisNode;

    if(rootNode->changes() != changes)
    {
        nodes->reset();     // Tree changed. Do it all from scratch!
        changes = rootNode->changes();
    }

    if(index< nodes->size())      // In the cache
        return nodes->elementAt((int) index);
    else                        // Not yet seen
    {
        if(nodes->size()==0)     // Pick up where we left off
            thisNode=rootNode; // (Which may be the beginning)
        else
            thisNode=nodes->lastElement();

        while(thisNode!=null && index >= nodes->size() && thisNode!=null)
        {
            thisNode=nextMatchingElementAfter(thisNode);
            if(thisNode!=null)
                nodes->addElement(thisNode);
        }
        return thisNode;           // Either what we want, or null (not avail.)
    }
};



/* Iterative tree-walker. When you have a Parent link, there's often no
need to resort to recursion. NOTE THAT only Element nodes are matched
since we're specifically supporting getElementsByTagName().
*/

NodeImpl *DeepNodeListImpl::nextMatchingElementAfter(NodeImpl *current)
{
    NodeImpl *next;
    while (current != null)
    {
        // Look down to first child.
        if (current->hasChildNodes())
        {
            current = current->getFirstChild();
        }
        // Look right to sibling (but not from root!)
        else
        {
            if (current != rootNode && null != (next = current->getNextSibling()))
            {
                current = next;
            }
            // Look up and right (but not past root!)
            else
            {
                next = null;
                for (; current != rootNode; // Stop when we return to starting point
                current = current->getParentNode())
                {
                    next = current->getNextSibling();
                    if (next != null)
                        break;
                }
                current = next;
            }
        }

        // Have we found an Element with the right tagName?
        // ("*" matches anything.)
        if (current != null && current != rootNode && current->isElementImpl()) {
	    if (!matchURIandTagname) {	//DOM Level 1
		if (matchAll || ((ElementImpl *)current)->getTagName().equals(tagName))
		    return current;
	    } else {	//DOM Level 2
		if (!matchAllURI && !(current -> getNamespaceURI().equals(namespaceURI)))
		    continue;
		if (matchAll || current -> getLocalName().equals(tagName))
		    return current;
	    }
	}

        // Otherwise continue walking the tree
    }
    // Fell out of tree-walk; no more instances found
    return null;
};


//
//  unreferenced()      The RefCountedImpl base class calls back to this function
//                      when the ref count goes to zero.
//
//
void DeepNodeListImpl::unreferenced()
{
//    delete this;
      DeepNodeListImpl* ptr = this;
      delete ptr;
};

// -----------------------------------------------------------------------
//  Notification that lazy data has been deleted
// -----------------------------------------------------------------------
void DeepNodeListImpl::reinitDeepNodeListImpl() {
	delete kAstr;
	kAstr = 0;
}

XERCES_CPP_NAMESPACE_END

