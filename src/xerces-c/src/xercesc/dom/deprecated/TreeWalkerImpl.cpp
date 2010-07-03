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
 * $Id: TreeWalkerImpl.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "TreeWalkerImpl.hpp"
#include "DOM_Document.hpp"
#include "DOM_DOMException.hpp"
#include "DocumentImpl.hpp"

XERCES_CPP_NAMESPACE_BEGIN


/** constructor */
TreeWalkerImpl::TreeWalkerImpl (
                                DOM_Node root,
                                unsigned long whatToShow,
                                DOM_NodeFilter* nodeFilter,
                                bool expandEntityRef)
:   fWhatToShow(whatToShow),
    fNodeFilter(nodeFilter),
    fCurrentNode(root),
    fRoot(root),
    fExpandEntityReferences(expandEntityRef)

{
}


TreeWalkerImpl::TreeWalkerImpl (const TreeWalkerImpl& twi)
:   RefCountedImpl(),
    fWhatToShow(twi.fWhatToShow),
    fNodeFilter(twi.fNodeFilter),
    fCurrentNode(twi.fCurrentNode),
    fRoot(twi.fRoot),
    fExpandEntityReferences(twi.fExpandEntityReferences)
{
}


TreeWalkerImpl& TreeWalkerImpl::operator= (const TreeWalkerImpl& twi) {
    if (this != &twi)
    {
        fCurrentNode            = twi.fCurrentNode;
        fRoot                   = twi.fRoot;
        fWhatToShow             = twi.fWhatToShow;
        fNodeFilter             = twi.fNodeFilter;
		fExpandEntityReferences = twi.fExpandEntityReferences;
    }

    return *this;
}



void TreeWalkerImpl::unreferenced()
{
    DOM_Document doc = fRoot.getOwnerDocument();
    DocumentImpl* impl;

    if (! doc.isNull()) {
        impl = (DocumentImpl *) doc.fImpl;
    }
    else
        impl = (DocumentImpl *) fRoot.fImpl;

    if (impl->treeWalkers != 0L) {
        int i;
        int sz = impl->treeWalkers->size();
        for (i = 0; i < sz; i++)
            if (impl->treeWalkers->elementAt(i) == this) {
                impl->treeWalkers->removeElementAt(i);
                break;
            }
    }

//    delete this;
    TreeWalkerImpl* ptr = this;
    delete ptr;
}


/** Return the Root Node. */
DOM_Node TreeWalkerImpl::getRoot () {

    return fRoot;
}

/** Return the whatToShow value */
unsigned long TreeWalkerImpl::getWhatToShow () {
    return fWhatToShow;
}


/** Return the NodeFilter */
DOM_NodeFilter* TreeWalkerImpl::getFilter () {
    return fNodeFilter;
}

/** Get the expandEntity reference flag. */
bool TreeWalkerImpl::getExpandEntityReferences() {
    return fExpandEntityReferences;
}



/** Return the current Node. */
DOM_Node TreeWalkerImpl::getCurrentNode () {

    return fCurrentNode;
}


/** Return the current Node. */
void TreeWalkerImpl::setCurrentNode (DOM_Node node) {

    fCurrentNode = node;
}


/** Return the parent Node from the current node,
 *  after applying filter, whatToshow.
 *  If result is not null, set the current Node.
 */
DOM_Node TreeWalkerImpl::parentNode () {

	DOM_Node result;

    if (fCurrentNode.isNull()) return result;

    DOM_Node node = getParentNode(fCurrentNode);
    if (node != 0) {
        fCurrentNode = node;
    }
    return node;

}


/** Return the first child Node from the current node,
 *  after applying filter, whatToshow.
 *  If result is not null, set the current Node.
 */
DOM_Node TreeWalkerImpl::firstChild () {

	DOM_Node result;

    if (fCurrentNode.isNull()) return result;

    DOM_Node node = getFirstChild(fCurrentNode);
    if (! node.isNull()) {
        fCurrentNode = node;
    }
    return node;
}


/** Return the last child Node from the current node,
 *  after applying filter, whatToshow.
 *  If result is not null, set the current Node.
 */
DOM_Node TreeWalkerImpl::lastChild () {

    DOM_Node result;

    if (fCurrentNode.isNull()) return result;

    DOM_Node node = getLastChild(fCurrentNode);
    if (! node.isNull()) {
        fCurrentNode = node;
    }
    return node;
}


/** Return the previous sibling Node from the current node,
 *  after applying filter, whatToshow.
 *  If result is not null, set the current Node.
 */

DOM_Node TreeWalkerImpl::previousSibling () {
	
	DOM_Node result;

    if (fCurrentNode.isNull()) return result;

    DOM_Node node = getPreviousSibling(fCurrentNode);
    if (! node.isNull()) {
        fCurrentNode = node;
    }
    return node;
}


/** Return the next sibling Node from the current node,
 *  after applying filter, whatToshow.
 *  If result is not null, set the current Node.
 */

DOM_Node TreeWalkerImpl::nextSibling () {
		
	DOM_Node result;

    if (fCurrentNode.isNull()) return result;

    DOM_Node node = getNextSibling(fCurrentNode);
    if (! node.isNull()) {
        fCurrentNode = node;
    }
    return node;
}


/** Return the previous Node from the current node,
 *  after applying filter, whatToshow.
 *  If result is not null, set the current Node.
 */

DOM_Node TreeWalkerImpl::previousNode () {
	
    DOM_Node result;

    if (fCurrentNode.isNull()) return result;

    // get sibling
    result = getPreviousSibling(fCurrentNode);
    if (result.isNull()) {
        result = getParentNode(fCurrentNode);
        if (! result.isNull()) {
            fCurrentNode = result;
            return fCurrentNode;
        }
        return result;
    }

    // get the lastChild of result.
    DOM_Node lastChild  = getLastChild(result);

    // if there is a lastChild which passes filters return it.
    if (! lastChild.isNull()) {
        fCurrentNode = lastChild;
        return fCurrentNode;
    }

    // otherwise return the previous sibling.
    if (! result.isNull()) {
        fCurrentNode = result;
        return fCurrentNode;
    }

    // otherwise return null.
    return result;
}


/** Return the next Node from the current node,
 *  after applying filter, whatToshow.
 *  If result is not null, set the current Node.
 */

DOM_Node TreeWalkerImpl::nextNode () {
	
	DOM_Node result;

    if (fCurrentNode.isNull()) return result;

    result = getFirstChild(fCurrentNode);

    if (! result.isNull()) {
        fCurrentNode = result;
        return result;
    }

    result = getNextSibling(fCurrentNode);

    if (! result.isNull()) {
        fCurrentNode = result;
        return result;
    }

    // return parent's 1st sibling.
    DOM_Node parent = getParentNode(fCurrentNode);
    while (! parent.isNull()) {
        result = getNextSibling(parent);
        if (! result.isNull()) {
            fCurrentNode = result;
            return result;
        } else {
            parent = getParentNode(parent);
        }
    }

    // end , return null
    return result;
}


/** Internal function.
 *  Return the parent Node, from the input node
 *  after applying filter, whatToshow.
 *  The current node is not consulted or set.
 */

DOM_Node TreeWalkerImpl::getParentNode (DOM_Node node) {
	
	DOM_Node result;

    if (node.isNull() || node == fRoot) return result;

    DOM_Node newNode = node.getParentNode();
    if (newNode.isNull())  return result;

    short accept = acceptNode(newNode);

    if (accept == DOM_NodeFilter::FILTER_ACCEPT)
        return newNode;

    return getParentNode(newNode);

}


/** Internal function.
 *  Return the nextSibling Node, from the input node
 *  after applying filter, whatToshow.
 *  The current node is not consulted or set.
 */

DOM_Node TreeWalkerImpl::getNextSibling (DOM_Node node) {
	
	DOM_Node result;

    if (node.isNull() || node == fRoot) return result;

    DOM_Node newNode = node.getNextSibling();
    if (newNode.isNull()) {

        newNode = node.getParentNode();

        if (newNode.isNull() || node == fRoot)  return result;

        short parentAccept = acceptNode(newNode);

        if (parentAccept == DOM_NodeFilter::FILTER_SKIP) {
            return getNextSibling(newNode);
        }

        return result;
    }

    short accept = acceptNode(newNode);

    if (accept == DOM_NodeFilter::FILTER_ACCEPT)
        return newNode;
    else
    if (accept == DOM_NodeFilter::FILTER_SKIP) {
        DOM_Node fChild =  getFirstChild(newNode);
        if (fChild.isNull()) {
            return getNextSibling(newNode);
        }
        return fChild;
    }
    return getNextSibling(newNode);

}


/** Internal function.
 *  Return the previous sibling Node, from the input node
 *  after applying filter, whatToshow.
 *  The current node is not consulted or set.
 */

DOM_Node TreeWalkerImpl::getPreviousSibling (DOM_Node node) {
		
	DOM_Node result;

    if (node.isNull() || node == fRoot) return result;

    DOM_Node newNode = node.getPreviousSibling();
    if (newNode.isNull()) {

        newNode = node.getParentNode();
        if (newNode.isNull() || node == fRoot)  return result;

        short parentAccept = acceptNode(newNode);

        if (parentAccept == DOM_NodeFilter::FILTER_SKIP) {
            return getPreviousSibling(newNode);
        }

        return result;
    }

    short accept = acceptNode(newNode);

    if (accept == DOM_NodeFilter::FILTER_ACCEPT)
        return newNode;
    else
    if (accept == DOM_NodeFilter::FILTER_SKIP) {
        DOM_Node fChild =  getLastChild(newNode);
        if (fChild.isNull()) {
            return getPreviousSibling(newNode);
        }
        return fChild;
    }
    return getPreviousSibling(newNode);

}


/** Internal function.
 *  Return the first child Node, from the input node
 *  after applying filter, whatToshow.
 *  The current node is not consulted or set.
 */

DOM_Node TreeWalkerImpl::getFirstChild (DOM_Node node) {
		
	DOM_Node result;

    if (node.isNull()) return result;

    DOM_Node newNode = node.getFirstChild();
    if (newNode.isNull())  return result;

    short accept = acceptNode(newNode);

    if (accept == DOM_NodeFilter::FILTER_ACCEPT)
        return newNode;
    else
    if (accept == DOM_NodeFilter::FILTER_SKIP
        && newNode.hasChildNodes())
    {
        return getFirstChild(newNode);
    }
    return getNextSibling(newNode);
}


/** Internal function.
 *  Return the last child Node, from the input node
 *  after applying filter, whatToshow.
 *  The current node is not consulted or set.
 */

DOM_Node TreeWalkerImpl::getLastChild (DOM_Node node) {
	
	DOM_Node result;

    if (node.isNull()) return result;

    DOM_Node newNode = node.getLastChild();
    if (newNode.isNull())  return result;

    short accept = acceptNode(newNode);

    if (accept == DOM_NodeFilter::FILTER_ACCEPT)
        return newNode;
    else
    if (accept == DOM_NodeFilter::FILTER_SKIP
        && newNode.hasChildNodes())
    {
        return getLastChild(newNode);
    }
    return getPreviousSibling(newNode);


}


/** The node is accepted if it passes the whatToShow and the filter. */

short TreeWalkerImpl::acceptNode (DOM_Node node) {
	
    if (fNodeFilter == 0) {
        if ( ( fWhatToShow & (1 << (node.getNodeType() - 1))) != 0)
        {
            return DOM_NodeFilter::FILTER_ACCEPT;
        }
        else
        {
            return DOM_NodeFilter::FILTER_SKIP;
        }
    } else {
        // REVISIT: This logic is unclear from the spec!
        if ((fWhatToShow & (1 << (node.getNodeType() - 1))) != 0 ) {
            return fNodeFilter->acceptNode(node);
        } else {
            // what to show has failed!
            if (fNodeFilter->acceptNode(node) == DOM_NodeFilter::FILTER_REJECT) {
                return DOM_NodeFilter::FILTER_REJECT;
            } else {
                return DOM_NodeFilter::FILTER_SKIP;
            }
        }
    }
}

XERCES_CPP_NAMESPACE_END

