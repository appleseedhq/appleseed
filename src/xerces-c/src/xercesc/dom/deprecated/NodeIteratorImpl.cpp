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
 * $Id: NodeIteratorImpl.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

// NodeIteratorImpl.cpp: implementation of the NodeIteratorImpl class.
//
//////////////////////////////////////////////////////////////////////

#include "NodeIteratorImpl.hpp"
#include "DOM_Document.hpp"
#include "DOM_DOMException.hpp"
#include "DocumentImpl.hpp"

XERCES_CPP_NAMESPACE_BEGIN


//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

NodeIteratorImpl::NodeIteratorImpl ()
:   fNodeFilter(0),
    fDetached(false)
{
}	

NodeIteratorImpl::~NodeIteratorImpl ()
{
	fDetached = false;
}


void NodeIteratorImpl::detach ()
{
	fDetached = true;
}


NodeIteratorImpl::NodeIteratorImpl (
                                    DOM_Node root,
                                    unsigned long whatToShow,
                                    DOM_NodeFilter* nodeFilter,
                                    bool expandEntityRef)
:   fRoot(root),
    fWhatToShow(whatToShow),
    fNodeFilter(nodeFilter),
    fExpandEntityReferences(expandEntityRef),
    fDetached(false),
    fCurrentNode(0),
    fForward(true)
{
	
}


NodeIteratorImpl::NodeIteratorImpl ( const NodeIteratorImpl& toCopy)
:   RefCountedImpl(),
    fRoot(toCopy.fRoot),
    fWhatToShow(toCopy.fWhatToShow),
    fNodeFilter(toCopy.fNodeFilter),
    fExpandEntityReferences(toCopy.fExpandEntityReferences),
    fDetached(toCopy.fDetached),
    fCurrentNode(toCopy.fCurrentNode),
    fForward(toCopy.fForward)
{
}


NodeIteratorImpl& NodeIteratorImpl::operator= (const NodeIteratorImpl& other) {
    fRoot                   = other.fRoot;
    fCurrentNode            = other.fRoot;
    fWhatToShow             = other.fWhatToShow;
    fNodeFilter             = other.fNodeFilter;
    fForward                = other.fForward;
	fDetached               = other.fDetached;
    fExpandEntityReferences = other.fExpandEntityReferences;
    return *this;
}

/** Return the Root Node. */
DOM_Node NodeIteratorImpl::getRoot () {
    return fRoot;
}

// Implementation Note: Note that the iterator looks at whatToShow
// and filter values at each call, and therefore one _could_ add
// setters for these values and alter them while iterating!

/** Return the whatToShow value */

unsigned long NodeIteratorImpl::getWhatToShow () {
    return fWhatToShow;
}


/** Return the filter */

DOM_NodeFilter* NodeIteratorImpl::getFilter () {
    return fNodeFilter;
}

/** Get the expandEntity reference flag. */
bool NodeIteratorImpl::getExpandEntityReferences()
{
    return fExpandEntityReferences;
}

/** Return the next DOM_Node in the Iterator. The node is the next node in
 *  depth-first order which also passes the filter, and whatToShow.
 *  A null return means either that
 */

DOM_Node NodeIteratorImpl::nextNode () {
	if (fDetached)
		throw DOM_DOMException(DOM_DOMException::INVALID_STATE_ERR, null);

	DOM_Node result;

    // if root is null there is no next node.
    if (fRoot.isNull())
			return result;

    DOM_Node aNextNode = fCurrentNode;
    bool accepted = false; // the next node has not been accepted.

    while (!accepted) {

        // if last direction is not forward, repeat node.
        if (!fForward && !aNextNode.isNull()) {
            //System.out.println("nextNode():!fForward:"+fCurrentNode.getNodeName());
            aNextNode = fCurrentNode;
        } else {
        // else get the next node via depth-first
            aNextNode = nextNode(aNextNode, true);
        }

        fForward = true; //REVIST: should direction be set forward before null check?

        // nothing in the list. return null.
        if (aNextNode.isNull())
					return result;

        // does node pass the filters and whatToShow?
        accepted = acceptNode(aNextNode);
        if (accepted) {
            // if so, then the node is the current node.
            fCurrentNode = aNextNode;
            return fCurrentNode;
				}

    }

    // no nodes, or no accepted nodes.
    return result;
}


/** Return the previous Node in the Iterator. The node is the next node in
 *  _backwards_ depth-first order which also passes the filter, and whatToShow.
 */

DOM_Node NodeIteratorImpl::previousNode () {
	if (fDetached)
		throw DOM_DOMException(DOM_DOMException::INVALID_STATE_ERR, null);
		
	DOM_Node result;

    // if the root is null, or the current node is null, return null.
    if (fRoot.isNull() || fCurrentNode.isNull())
			return result;

    DOM_Node aPreviousNode = fCurrentNode;
    bool accepted = false;

    while (!accepted) {


        if (fForward && ! aPreviousNode.isNull()) {
            //repeat last node.
            aPreviousNode = fCurrentNode;
        } else {
            // get previous node in backwards depth first order.
            aPreviousNode = previousNode(aPreviousNode);
        }

        // we are going backwards
        fForward = false;

        // if the new previous node is null, we're at head or past the root,
        // so return null.
        if (aPreviousNode.isNull())
					return result;

        // check if node passes filters and whatToShow.
        accepted = acceptNode(aPreviousNode);
        if (accepted) {
            // if accepted, update the current node, and return it.
            fCurrentNode = aPreviousNode;
            return fCurrentNode;
        }
    }
    // there are no nodes?
    return result;
}


/** The node is accepted if it passes the whatToShow and the filter. */
bool NodeIteratorImpl::acceptNode (DOM_Node node) {
	if (fDetached)
		throw DOM_DOMException(DOM_DOMException::INVALID_STATE_ERR, null);

    if (fNodeFilter == 0) {
        return ((fWhatToShow & (1 << (node.getNodeType() - 1))) != 0);
    } else {
        return ((fWhatToShow & (1 << (node.getNodeType() - 1))) != 0)
            && fNodeFilter->acceptNode(node) == DOM_NodeFilter::FILTER_ACCEPT;
    }
}


/** Return node, if matches or any parent if matches. */
DOM_Node NodeIteratorImpl::matchNodeOrParent (DOM_Node node) {
		DOM_Node result;

    for (DOM_Node n = fCurrentNode; n != fRoot; n = n.getParentNode()) {
        if (node == n) return n;
    }

    return result;
}


/** The method nextNode(DOM_Node, bool) returns the next node
 *  from the actual DOM tree.
 *
 *  The bool visitChildren determines whether to visit the children.
 *  The result is the nextNode.
 */

DOM_Node NodeIteratorImpl::nextNode (DOM_Node node, bool visitChildren) {
	if (fDetached)
		throw DOM_DOMException(DOM_DOMException::INVALID_STATE_ERR, null);

    if (node.isNull()) return fRoot;

    DOM_Node result;
    // only check children if we visit children.
    if (visitChildren) {
        //if hasChildren, return 1st child.
        if (node.hasChildNodes()) {
            result = node.getFirstChild();
            return result;
        }
    }

    // if hasSibling, return sibling
    if (node != fRoot) {
        result = node.getNextSibling();
        if (! result.isNull()) return result;


        // return parent's 1st sibling.
        DOM_Node parent = node.getParentNode();
        while (!parent.isNull() && parent != fRoot) {
            result = parent.getNextSibling();
            if (!result.isNull()) {
                return result;
            } else {
                parent = parent.getParentNode();
            }

        } // while (parent != null && parent != fRoot) {
    }
    // end of list, return null
    DOM_Node aNull;
    return aNull;
}


/** The method previousNode(DOM_Node) returns the previous node
 *  from the actual DOM tree.
 */

DOM_Node NodeIteratorImpl::previousNode (DOM_Node node) {
	if (fDetached)
		throw DOM_DOMException(DOM_DOMException::INVALID_STATE_ERR, null);

    DOM_Node result;

    // if we're at the root, return null.
    if (node == fRoot)
			return result;

    // get sibling
    result = node.getPreviousSibling();
    if (result.isNull()) {
        //if 1st sibling, return parent
        result = node.getParentNode();
        return result;
    }

    // if sibling has children, keep getting last child of child.
    if (result.hasChildNodes()) {
        while (result.hasChildNodes()) {
            result = result.getLastChild();
        }
    }

    return result;
}


/** Fix-up the iterator on a remove. Called by DOM or otherwise,
 *  before an actual DOM remove.
 */

void NodeIteratorImpl::removeNode (DOM_Node node) {
	if (fDetached)
		throw DOM_DOMException(DOM_DOMException::INVALID_STATE_ERR, null);

    // Implementation note: Fix-up means setting the current node properly
    // after a remove.

    if (node.isNull())
				return;

    DOM_Node deleted = matchNodeOrParent(node);

    if (deleted.isNull()) return;

    if (fForward) {
        fCurrentNode = previousNode(deleted);
    } else
    // if (!fForward)
    {
        DOM_Node next = nextNode(deleted, false);
        if (! next.isNull()) {
            // normal case: there _are_ nodes following this in the iterator.
            fCurrentNode = next;
        } else {
            // the last node in the iterator is to be removed,
            // so we set the current node to be the previous one.
            fCurrentNode = previousNode(deleted);
            fForward = true;
        }

    }

}


void NodeIteratorImpl::unreferenced()
{
    DOM_Document doc = fRoot.getOwnerDocument();
    DocumentImpl* impl;

    if (! doc.isNull()) {
        impl = (DocumentImpl *) doc.fImpl;
    }
    else
        impl = (DocumentImpl *) fRoot.fImpl;

    if (impl->iterators != 0L) {
        int i;
        int sz = impl->iterators->size();
        for (i = 0; i < sz; i++)
            if (impl->iterators->elementAt(i) == this) {
                impl->iterators->removeElementAt(i);
                break;
            }
    }

//    delete this;
    NodeIteratorImpl* ptr = this;
    delete ptr;
}

XERCES_CPP_NAMESPACE_END

