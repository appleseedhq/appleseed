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
 * $Id: RangeImpl.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include <xercesc/util/RefVectorOf.hpp>
#include "NodeImpl.hpp"
#include "RangeImpl.hpp"
#include "TextImpl.hpp"
#include "DocumentImpl.hpp"
#include "DOM_DOMException.hpp"
#include "DOM_Document.hpp"
#include "DocumentFragmentImpl.hpp"
#include "DOM_Document.hpp"
#include "DOM_RangeException.hpp"
#include "DOM_DOMException.hpp"
#include "DOM_Text.hpp"

XERCES_CPP_NAMESPACE_BEGIN


//---------------------
// C'tor and D'tor
//---------------------

RangeImpl::RangeImpl(DOM_Document doc)

    :   fStartContainer(doc),
        fStartOffset(0),
        fEndContainer(doc),
        fEndOffset(0),
        fCollapsed(true),
        fDocument(doc),
        fDetached(false),
        fRemoveChild(0)
{
}

RangeImpl::RangeImpl(const RangeImpl& other) : RefCountedImpl()
{
    fDocument = other.fDocument;
    fStartContainer = other.fStartContainer;
    fStartOffset = other.fStartOffset;
    fEndContainer = other.fEndContainer;
    fEndOffset = other.fEndOffset;
    fDetached = other.fDetached;
    fCollapsed = other.fCollapsed;
    fRemoveChild = other.fRemoveChild;
}

RangeImpl::~RangeImpl()
{
}

void RangeImpl::unreferenced()
{
    if (((DocumentImpl*)fDocument.fImpl)->ranges != 0L) {
        int sz = ((DocumentImpl*)fDocument.fImpl)->ranges->size();
        for (int i=0; i< sz; i++) {
            if (((DocumentImpl*)fDocument.fImpl)->ranges->elementAt(i) == this) {
                ((DocumentImpl*)fDocument.fImpl)->ranges->removeElementAt(i);
                break;
            }
        }
    }
//    delete this;
    RangeImpl* ptr = this;
    delete ptr;
};


//-------------------------------
// Public getter functions
//-------------------------------


DOM_Node RangeImpl::getStartContainer() const
{
    return fStartContainer;
}

unsigned int RangeImpl::getStartOffset() const
{
    return fStartOffset;
}

DOM_Node RangeImpl::getEndContainer() const
{
    return fEndContainer;
}

unsigned int RangeImpl::getEndOffset() const
{
    return fEndOffset;
}



bool RangeImpl::getCollapsed() const
{
    if (fDetached)
    {
        throw DOM_DOMException(
            DOM_DOMException::INVALID_STATE_ERR, null);
    }

    return ((fStartContainer == fEndContainer)
             && (fStartOffset == fEndOffset));
}

//-------------------------------
// Public getter functions
//-------------------------------

void RangeImpl::setStartContainer(const DOM_Node& node)
{
    if (fDetached)
    {
        throw DOM_DOMException(
            DOM_DOMException::INVALID_STATE_ERR, null);
    }

    fStartContainer = node;
}

void RangeImpl::setStartOffset(unsigned int offset)
{
    if (fDetached)
    {
        throw DOM_DOMException(
            DOM_DOMException::INVALID_STATE_ERR, null);
    }

    fStartOffset = offset;
}

void RangeImpl::setEndContainer(const DOM_Node& node)
{
    if (fDetached)
    {
        throw DOM_DOMException(
            DOM_DOMException::INVALID_STATE_ERR, null);
    }

    fEndContainer = node;

}

void RangeImpl::setEndOffset(unsigned int offset)
{
    if (fDetached)
    {
        throw DOM_DOMException(
            DOM_DOMException::INVALID_STATE_ERR, null);
    }

    fEndOffset = offset;
}

void RangeImpl::setStart(const DOM_Node& refNode, unsigned int offset)
{
    validateNode(refNode);
    checkIndex(refNode, offset);

    fStartContainer = refNode;
    fStartOffset    = offset;

    if ((fDocument != refNode.getOwnerDocument() )
        && (refNode.getOwnerDocument().fImpl != 0) )
    {
        fDocument = refNode.getOwnerDocument();
        collapse(true);
    }

    //compare the start and end boundary point
    //collapse if start point is after the end point
    if(compareBoundaryPoints(DOM_Range::END_TO_START, this) == 1)
        collapse(true); //collapse the range positions to start
    else
        fCollapsed = false;
}

void RangeImpl::setEnd(const DOM_Node& refNode, unsigned int offset)
{
    validateNode(refNode);
    checkIndex(refNode, offset);

    fEndContainer   = refNode;
    fEndOffset      = offset;

    if ((fDocument != refNode.getOwnerDocument() )
        && (refNode.getOwnerDocument().fImpl != 0) )
    {
        fDocument = refNode.getOwnerDocument();
        collapse(false);
    }

    //compare the start and end boundary point
    //collapse if start point is after the end point
    if(compareBoundaryPoints(DOM_Range::END_TO_START, this) == 1)
        collapse(false); //collapse the range positions to end
    else
        fCollapsed = false;
}

void RangeImpl::setStartBefore(const DOM_Node& refNode)
{
    if( fDetached) {
        throw DOM_DOMException(
            DOM_DOMException::INVALID_STATE_ERR, null);
    }
    if ( !hasLegalRootContainer(refNode) || !isLegalContainedNode(refNode)) {
        throw DOM_RangeException(
            DOM_RangeException::INVALID_NODE_TYPE_ERR, null);
    }

    fStartContainer = refNode.getParentNode();
   unsigned int i = 0;
    for (DOM_Node n = refNode; n!=null; n = n.getPreviousSibling()) {
        i++;
    }
    if (i == 0)
        fStartOffset = 0;
    else
        fStartOffset = i-1;

    if ((fDocument != refNode.getOwnerDocument())
        && (refNode.getOwnerDocument().fImpl != 0) )
    {
        fDocument = refNode.getOwnerDocument();
        collapse(true);
    }

    //compare the start and end boundary point
    //collapse if start point is after the end point
    if(compareBoundaryPoints(DOM_Range::END_TO_START, this) == 1)
        collapse(true); //collapse the range positions to start
    else
        fCollapsed = false;
}

void RangeImpl::setStartAfter(const DOM_Node& refNode)
{
    if( fDetached) {
        throw DOM_DOMException(
            DOM_DOMException::INVALID_STATE_ERR, null);
    }
    if ( !hasLegalRootContainer(refNode) || !isLegalContainedNode(refNode)) {
        throw DOM_RangeException(
            DOM_RangeException::INVALID_NODE_TYPE_ERR, null);
    }

    fStartContainer = refNode.getParentNode();
    unsigned int i = 0;
    for (DOM_Node n = refNode; n!=null; n = n.getPreviousSibling()) {
        i++;
    }

    fStartOffset = i;

    if ((fDocument != refNode.getOwnerDocument() )
        && (refNode.getOwnerDocument().fImpl != 0) )
    {
        fDocument = refNode.getOwnerDocument();
        collapse(true);
    }

    //compare the start and end boundary point
    //collapse if start point is after the end point
    if(compareBoundaryPoints(DOM_Range::END_TO_START, this) == 1)
        collapse(true); //collapse the range positions to start
    else
        fCollapsed = false;
}

void RangeImpl::setEndBefore(const DOM_Node& refNode)
{
    if( fDetached) {
        throw DOM_DOMException(
            DOM_DOMException::INVALID_STATE_ERR, null);
    }
    if ( !hasLegalRootContainer(refNode) || !isLegalContainedNode(refNode)) {
        throw DOM_RangeException(
            DOM_RangeException::INVALID_NODE_TYPE_ERR, null);
    }

    fEndContainer = refNode.getParentNode();
    unsigned int i = 0;
    for (DOM_Node n = refNode; n!=null; n = n.getPreviousSibling(), i++) ;

    if (i< 1)
        fEndOffset = 0;
    else
        fEndOffset = i-1;

    if ((fDocument != refNode.getOwnerDocument() )
        && (refNode.getOwnerDocument().fImpl != 0) )
    {
        fDocument = refNode.getOwnerDocument();
        collapse(true);
    }

    //compare the start and end boundary point
    //collapse if start point is after the end point
    if(compareBoundaryPoints(DOM_Range::END_TO_START, this) == 1)
        collapse(false); //collapse the range positions to end
    else
        fCollapsed = false;
}

void RangeImpl::setEndAfter(const DOM_Node& refNode)
{
    if( fDetached) {
        throw DOM_DOMException(
            DOM_DOMException::INVALID_STATE_ERR, null);
    }
    if ( !hasLegalRootContainer(refNode) || !isLegalContainedNode(refNode)) {
        throw DOM_RangeException(
            DOM_RangeException::INVALID_NODE_TYPE_ERR, null);
    }

    fEndContainer = refNode.getParentNode();
    unsigned int i = 0;
    for (DOM_Node n = refNode; n!=null; n = n.getPreviousSibling(), i++) ;

    if (i ==0)
        fEndOffset = 0;
    else
        fEndOffset = i;

    if ((fDocument != refNode.getOwnerDocument() )
        && (refNode.getOwnerDocument().fImpl != 0) )
    {
        fDocument = refNode.getOwnerDocument();
        collapse(true);
    }

    //compare the start and end boundary point
    //collapse if start point is after the end point
    if(compareBoundaryPoints(DOM_Range::END_TO_START, this) == 1)
        collapse(false); //collapse the range positions to end
    else
        fCollapsed = false;
}
//-------------------------------
// Public Misc. functions
//-------------------------------
void RangeImpl::detach()
{
    if( fDetached) {
        throw DOM_DOMException(
            DOM_DOMException::INVALID_STATE_ERR, null);
    }

    fDetached = true;

    //nullify nodes
    fStartContainer = 0;
    fStartOffset    = 0;
    fEndContainer   = 0;
    fEndOffset      = 0;
    fCollapsed      = true;

    fRemoveChild    = 0;
}

void RangeImpl::collapse(bool toStart)
{
    if( fDetached) {
        throw DOM_DOMException(
            DOM_DOMException::INVALID_STATE_ERR, null);
    }

    if (toStart) {
        fEndContainer = fStartContainer;
        fEndOffset = fStartOffset;
    } else {
        fStartContainer = fEndContainer;
        fStartOffset = fEndOffset;
    }
    fCollapsed = true;
}

void RangeImpl::selectNode(const DOM_Node& refNode)
{
    validateNode(refNode);
    if ( !isLegalContainedNode(refNode)) {
        throw DOM_RangeException(
            DOM_RangeException::INVALID_NODE_TYPE_ERR, null);
    }
    //First check for the text type node
    if (refNode.getNodeType() ==  DOM_Node::TEXT_NODE)
    {
        //The node itself is the container.
        fStartContainer = refNode;
        fEndContainer   = refNode;

        //Select all the contents of the node
        fStartOffset = 0;
        fEndOffset = ((DOM_Text &)refNode).getLength();
        return;
    }

    DOM_Node parent = refNode.getParentNode();
    if (parent != null ) // REVIST: what to do if it IS null?
    {
        fStartContainer = parent;
        fEndContainer = parent;

        unsigned int i = 0;
        for (DOM_Node n = parent.getFirstChild(); n!=null, n!=refNode; n = n.getNextSibling()) {
            i++;
        }

        fStartOffset = i;
        fEndOffset = fStartOffset+1;
    }
}

void RangeImpl::selectNodeContents(const DOM_Node& node)
{
    validateNode(node);

    fStartContainer = node;
    fEndContainer = node;

    fStartOffset = 0;
    if (node.getNodeType() == DOM_Node::TEXT_NODE ) {
        fEndOffset = ((DOM_Text &)node).getLength();
        return;
    }

    DOM_Node first = node.getFirstChild();
    if (first == null) {
        fEndOffset = 0;
        return;
    }
    unsigned int i = 0;
    for (DOM_Node n = first; n!=null; n = n.getNextSibling()) {
        i++;
    }
    fEndOffset = i;
}

void RangeImpl::surroundContents(DOM_Node& newParent)
{
    if (newParent==null) return;

    //check for elimination criteria
    if( fDetached) {
        throw DOM_DOMException(
            DOM_DOMException::INVALID_STATE_ERR, null);
    }

    if (newParent.getOwnerDocument() !=fDocument) {
        throw DOM_DOMException(
            DOM_DOMException::WRONG_DOCUMENT_ERR, null);
    }

    int type = newParent.getNodeType();
    if ( !isLegalContainedNode(newParent)
        || type == DOM_Node::DOCUMENT_TYPE_NODE)
    {
        throw DOM_RangeException(
            DOM_RangeException::INVALID_NODE_TYPE_ERR, null);
    }

    DOM_Node root = getCommonAncestorContainer();

    DOM_Node realStart = fStartContainer;
    DOM_Node realEnd = fEndContainer;

    if (fStartContainer.getNodeType() == DOM_Node::TEXT_NODE) {
        realStart = fStartContainer.getParentNode();
    }
    if (fEndContainer.getNodeType() == DOM_Node::TEXT_NODE) {
        realEnd = fEndContainer.getParentNode();
    }

    if (realStart != realEnd) {
        throw DOM_RangeException(
            DOM_RangeException::BAD_BOUNDARYPOINTS_ERR, null);
    }

    DOM_DocumentFragment frag = extractContents();
    insertNode(newParent);
    newParent.appendChild(frag);
    selectNode(newParent);
}


short RangeImpl::compareBoundaryPoints(DOM_Range::CompareHow how, RangeImpl* srcRange) const
{
    if (fDocument != srcRange->fDocument) {
        throw DOM_DOMException(
            DOM_DOMException::WRONG_DOCUMENT_ERR, null);
    }
    if( fDetached) {
        throw DOM_DOMException(
            DOM_DOMException::INVALID_STATE_ERR, null);
    }

    DOM_Node pointA, pointB;
    int offsetA, offsetB;

    switch (how)
    {
    case (DOM_Range::START_TO_START) :
        pointB = srcRange->getStartContainer();
        pointA = fStartContainer;
        offsetB = srcRange->getStartOffset();
        offsetA = fStartOffset;
        break;
    case (DOM_Range::START_TO_END) :
        pointB = srcRange->getStartContainer();
        pointA = fEndContainer;
        offsetB = srcRange->getStartOffset();
        offsetA = fEndOffset;
        break;
    case (DOM_Range::END_TO_START) :
        pointB = srcRange->getEndContainer();
        pointA = fStartContainer;
        offsetB = srcRange->getEndOffset();
        offsetA = fStartOffset;
        break;
    case (DOM_Range::END_TO_END) :
        pointB = srcRange->getEndContainer();
        pointA = fEndContainer;
        offsetB = srcRange->getEndOffset();
        offsetA = fEndOffset;
        break;
    default:
        throw DOM_DOMException(
            DOM_DOMException::INVALID_STATE_ERR, null);
    }

    // case 1: same container
    if (pointA == pointB) {
        if (offsetA < offsetB) return -1; //A before B
        if (offsetA == offsetB) return 0; //A equal to B
        return 1; // A after B
    }
    // case 2: Child C of container A is ancestor of B
    for (DOM_Node node = pointA.getFirstChild(); node != null; node=node.getNextSibling()) {
        if (isAncestorOf(node, pointB)) {
            int index = indexOf(node, pointA);
            if (offsetA <=  index) return -1;
            return 1;
        }
    }
    // case 3: Child C of container B is ancestor of A
    for (DOM_Node nd = pointB.getFirstChild(); nd != null; nd=nd.getNextSibling()) {
        if (isAncestorOf(nd, pointA)) {
            int index = indexOf(nd, pointB);
            if (index < offsetB ) return -1;
            return 1; //B strictly before A
        }
    }

    // case 4: preorder traversal of context tree.
    DOM_Node ancestor = commonAncestorOf(pointA, pointB);
    DOM_Node current = ancestor;

    do {
        if (current == pointA) return -1;
        if (current == pointB) return 1;
        current = nextNode(current, true);
    }
    while (current!=null && current!=ancestor);

    return -2; // this should never happen
}


void RangeImpl:: deleteContents()
{
    traverseContents(DELETE_CONTENTS);
}

DOM_DocumentFragment RangeImpl::extractContents()
{
    checkReadOnly(fStartContainer, fEndContainer, fStartOffset, fEndOffset);
    return traverseContents(EXTRACT_CONTENTS);
}

DOM_DocumentFragment RangeImpl::cloneContents() const
{
    // cast off const.
    return ((RangeImpl *)this)->traverseContents(CLONE_CONTENTS);
}


void RangeImpl::insertNode(DOM_Node& newNode)
{
    if (newNode == null) return; //don't have to do anything

    for (DOM_Node aNode = fStartContainer; aNode!=null; aNode = aNode.getParentNode()) {
        if (aNode.fImpl->isReadOnly()) {
        throw DOM_DOMException(
            DOM_DOMException::NO_MODIFICATION_ALLOWED_ERR, null);
    }
    }

    if (fDocument != newNode.getOwnerDocument()) {
        throw DOM_DOMException(
            DOM_DOMException::WRONG_DOCUMENT_ERR, null);
    }

    // Prevent cycles in the tree.
    //isKidOK() is not checked here as its taken care by insertBefore() function
    if (isAncestorOf( newNode, fStartContainer)) {
        throw DOM_DOMException(
            DOM_DOMException::HIERARCHY_REQUEST_ERR, null);
    }

    if( fDetached) {
        throw DOM_DOMException(
            DOM_DOMException::INVALID_STATE_ERR, null);
    }

    int type = newNode.getNodeType();
    if (type == DOM_Node::ATTRIBUTE_NODE
        || type == DOM_Node::ENTITY_NODE
        || type == DOM_Node::NOTATION_NODE
        || type == DOM_Node::DOCUMENT_NODE)
    {
        throw DOM_RangeException(
            DOM_RangeException::INVALID_NODE_TYPE_ERR, null);
    }


    DOM_Node parent;
    DOM_Node next;

    if (fStartContainer.getNodeType() == DOM_Node::TEXT_NODE) {

        //set 'parent' and 'next' here
        parent = fStartContainer.getParentNode();

        //split the text nodes
       if (fStartOffset > 0)
            ((DOM_Text &)fStartContainer).splitText(fStartOffset);

        //update the new start information later. After inserting the first newNode
        if (fStartOffset == 0)
            next = fStartContainer;
        else
            next = fStartContainer.getNextSibling();

    } // end of text handling
    else {
        parent = fStartContainer;

        next = fStartContainer.getFirstChild();
        for(unsigned int i = 0; (i < fStartOffset) && (next != null); i++) {
            next=next.getNextSibling();
        }
    }

    if (parent != null) {
        if (next != null)
            parent.insertBefore(newNode, next);
        else
            parent.appendChild(newNode);
    }
}

RangeImpl* RangeImpl::cloneRange() const
{
    if( fDetached) {
        throw DOM_DOMException(
            DOM_DOMException::INVALID_STATE_ERR, null);
    }

    RangeImpl* range = ((DocumentImpl*)fDocument.fImpl)->createRange();
    range->setStart(fStartContainer, fStartOffset);
    range->setEnd(fEndContainer, fEndOffset);

    return range;
}

DOMString RangeImpl::toString() const
{
    if( fDetached) {
        throw DOM_DOMException(
            DOM_DOMException::INVALID_STATE_ERR, null);
    }

    DOM_Node node = fStartContainer;
    DOM_Node stopNode = fEndContainer;

    DOMString tempString;
    if ( (fStartContainer.getNodeType() == DOM_Node::TEXT_NODE)
        || (fStartContainer.getNodeType() == DOM_Node::CDATA_SECTION_NODE) ) {
        if (fStartContainer == fEndContainer) {
            tempString.appendData(fStartContainer.getNodeValue().substringData(fStartOffset, fEndOffset-fStartOffset));
            return tempString;
        } else {
            int length = fStartContainer.getNodeValue().length();
            tempString.appendData(fStartContainer.getNodeValue().substringData(fStartOffset, length - fStartOffset));
            node = nextNode(node, true);
        }
    }else { //fStartContainer is not a TextNode
        node=node.getFirstChild();
        if (fStartOffset>0) { //find a first node within a range, specified by fStartOffset
            unsigned int counter = 0;
            while (counter<fStartOffset && node!=null) {
                node=node.getNextSibling();
                counter++;
            }
        }
        if (node == null) {
            node = nextNode(fStartContainer,false);
        }
    }

    if ( fEndContainer.getNodeType()!= DOM_Node::TEXT_NODE &&
        fEndContainer.getNodeType()!= DOM_Node::CDATA_SECTION_NODE ){
        int i=fEndOffset;
        stopNode = fEndContainer.getFirstChild();
        while( i>0 && stopNode!=null ){
            --i;
            stopNode = stopNode.getNextSibling();
        }
        if ( stopNode == null )
            stopNode = nextNode( fEndContainer, false );
    }

    while (node != stopNode) {  //look into all kids of the Range
        if (node == null) break;
        if (node.getNodeType() == DOM_Node::TEXT_NODE
            ||  node.getNodeType() == DOM_Node::CDATA_SECTION_NODE) {
            tempString.appendData(node.getNodeValue());
        }
        node = nextNode(node, true);
    }

    if (fEndContainer.getNodeType() == DOM_Node::TEXT_NODE
        || fEndContainer.getNodeType() == DOM_Node::CDATA_SECTION_NODE) {
        tempString.appendData(fEndContainer.getNodeValue().substringData(0,fEndOffset));
    }
    return tempString;
}

DOM_Document RangeImpl::getDocument()
{
    return fDocument;
}

const DOM_Node RangeImpl::getCommonAncestorContainer() const
{
     return commonAncestorOf(fStartContainer, fEndContainer);

}

//---------------------
//private functions
//---------------------

bool RangeImpl::isValidAncestorType(const DOM_Node& node) const
{
    for (DOM_Node aNode = node; aNode!=null; aNode = aNode.getParentNode()) {
        short type = aNode.getNodeType();
        if ( type == DOM_Node::ENTITY_NODE
            || type == DOM_Node::NOTATION_NODE
            || type == DOM_Node::DOCUMENT_TYPE_NODE)
            return false;
    }
    return true;
}

bool RangeImpl::isAncestorOf(const DOM_Node& a, const DOM_Node& b) {
    for (DOM_Node node=b; node != null; node=node.getParentNode()) {
        if  (node == a) return true;
    }
    return false;
}

bool RangeImpl::hasLegalRootContainer(const DOM_Node& node) const {
    if ( node==null )
        return false;

    DOM_Node rootContainer = node;
    for (; rootContainer.getParentNode()!=null; rootContainer = rootContainer.getParentNode())
        ;

    switch( rootContainer.getNodeType() ) {
        case DOM_Node::ATTRIBUTE_NODE:
        case DOM_Node::DOCUMENT_NODE:
        case DOM_Node::DOCUMENT_FRAGMENT_NODE:
        return true;
    }
    return false;
}

bool RangeImpl::isLegalContainedNode(const DOM_Node& node ) const {
   if ( node==null )
       return false;
   switch( node.getNodeType() )
   {
       case DOM_Node::DOCUMENT_NODE:
       case DOM_Node::DOCUMENT_FRAGMENT_NODE:
       case DOM_Node::ATTRIBUTE_NODE:
       case DOM_Node::ENTITY_NODE:
       case DOM_Node::NOTATION_NODE:
       return false;
   }
   return true;
}

unsigned short RangeImpl::indexOf(const DOM_Node& child, const DOM_Node& parent) const
{
    unsigned short i = 0;
    if (child.getParentNode() != parent) return (unsigned short)-1;
    for(DOM_Node node = child.getPreviousSibling(); node!= null; node=node.getPreviousSibling()) {
        i++;
    }
    return i;
}

void RangeImpl::validateNode(const DOM_Node& node) const
{
    if( fDetached) {
        throw DOM_DOMException(
            DOM_DOMException::INVALID_STATE_ERR, null);
    }

    if ( !isValidAncestorType(node)) {
        throw DOM_RangeException(
            DOM_RangeException::INVALID_NODE_TYPE_ERR, null);
    }
}


const DOM_Node RangeImpl::commonAncestorOf(const DOM_Node& pointA, const DOM_Node& pointB) const
{
    if (fDetached)
            throw DOM_DOMException(DOM_DOMException::INVALID_STATE_ERR, null);

    if (pointA.getOwnerDocument() != pointB.getOwnerDocument())
        throw DOM_DOMException( DOM_DOMException::WRONG_DOCUMENT_ERR, null );

    //if the containers are same then it itself is its common ancestor.
    if (pointA == pointB)
        return pointA;

    typedef RefVectorOf<NodeImpl> VectorNodes;
    VectorNodes* startV= new (((DocumentImpl*)fDocument.fImpl)->getMemoryManager()) VectorNodes(1, false, ((DocumentImpl*)fDocument.fImpl)->getMemoryManager());
    DOM_Node node;

    for (node=fStartContainer; node != null; node=node.getParentNode())
    {
        startV->addElement(node.fImpl);
    }
    VectorNodes* endV = new (((DocumentImpl*)fDocument.fImpl)->getMemoryManager()) VectorNodes(1, false, ((DocumentImpl*)fDocument.fImpl)->getMemoryManager());
    for (node=fEndContainer; node != null; node=node.getParentNode())
    {
        endV->addElement(node.fImpl);
    }

    int s = startV->size()-1;
    int e = endV->size()-1;

    NodeImpl* commonAncestor = 0;

    while (s>=0 && e>=0) {
        if (startV->elementAt(s) == endV->elementAt(e)) {
            commonAncestor = startV->elementAt(s);
        }
        else  break;
        --s;
        --e;
    }

    delete startV;
    delete endV;

    return DOM_Node(commonAncestor);
}

void RangeImpl::checkIndex(const DOM_Node& node, unsigned int offset) const
{
    short type = node.getNodeType();

    if((type == DOM_Node::TEXT_NODE
        || type == DOM_Node::CDATA_SECTION_NODE
        || type == DOM_Node::COMMENT_NODE
        || type == DOM_Node::PROCESSING_INSTRUCTION_NODE)) {
        if (offset > node.getNodeValue().length())
            throw DOM_DOMException( DOM_DOMException::INDEX_SIZE_ERR, null );
        else  return;
    }

    DOM_Node child = node.getFirstChild();
    unsigned int i = 0;
    for (; child != null; i++) {
        child = child.getNextSibling();
    }
    if (i < offset) {
        throw DOM_DOMException( DOM_DOMException::INDEX_SIZE_ERR, null );
    }

}

DOM_Node RangeImpl::nextNode(const DOM_Node& node, bool visitChildren) const
{

    if (node == null) return null;

    DOM_Node result;
    if (visitChildren) {
        result = node.getFirstChild();
        if (result != null) {
            return result;
        }
    }

    // if hasSibling, return sibling
    result = node.getNextSibling();
    if (result != null) {
        return result;
    }


    // return parent's 1st sibling.
    DOM_Node parent = node.getParentNode();


    while ( (parent != null) && (parent != fDocument) )
    {
        result = parent.getNextSibling();
        if (result != null) {
            return result;
        } else {
            parent = parent.getParentNode();
            if (parent == fEndContainer) return parent;

        }

    }
    // end of list, return null
    return null;
}


/** This is the master routine invoked to visit the nodes
*   selected by this range.  For each such node, different
*   actions are taken depending on the value of the TraversalType argument.
*/
DOM_DocumentFragment RangeImpl::traverseContents(TraversalType how)
{
    if (fDetached)
            throw DOM_DOMException(DOM_DOMException::INVALID_STATE_ERR, null);

    if (fStartContainer == null || fEndContainer == null) {
        return DOM_DocumentFragment(); // REVIST: Throw exception?
    }

    /* Traversal is accomplished by first determining the
       relationship between the endpoints of the range.
       For each of four significant relationships, we will
       delegate the traversal call to a method that
       can make appropriate assumptions.
    */

    // case 1: same container
    if ( fStartContainer == fEndContainer )
        return traverseSameContainer( how );

    // case 2: Child C of start container is ancestor of end container
    for (DOM_Node node = fStartContainer.getFirstChild(); node != null; node=node.getNextSibling()) {
        if (isAncestorOf(node, fEndContainer))
            return traverseCommonStartContainer( node, how );
    }

    // case 3: Child C of end container  is ancestor of start container
    for (DOM_Node nd = fEndContainer.getFirstChild(); nd != null; nd=nd.getNextSibling()) {
        if (isAncestorOf(nd, fStartContainer))
             return traverseCommonEndContainer( nd, how );
        }

    // case 4: preorder traversal of context tree.
    // There is a common ancestor container.  Find the
    // ancestor siblings that are children of that container.
    DOM_Node ancestor = commonAncestorOf(fStartContainer, fEndContainer);
    return traverseCommonAncestors( ancestor, ancestor, how );
    }

/**
 * Visits the nodes selected by this range when we know
 * a-priori that the start and end containers are the same.
 *
 */
DOM_DocumentFragment RangeImpl::traverseSameContainer( int how )
{
    DOM_DocumentFragment frag = null;
    if ( how!=DELETE_CONTENTS)
        frag = fDocument.createDocumentFragment();

    // If selection is empty, just return the fragment
    if ( fStartOffset==fEndOffset )
            return frag;

    DOM_Node current = fStartContainer;
    DOM_Node cloneCurrent = null;

    // Text node needs special case handling
    if ( fStartContainer.getNodeType()== DOM_Node::TEXT_NODE )
    {
        cloneCurrent = fStartContainer.cloneNode(false);
        cloneCurrent.setNodeValue(
            cloneCurrent.getNodeValue().substringData(fStartOffset, fEndOffset - fStartOffset));

        // set the original text node to its new value
        if ( how != CLONE_CONTENTS )
            ((DOM_Text &)fStartContainer).deleteData(fStartOffset, fEndOffset-fStartOffset);
        if ( how != DELETE_CONTENTS)
            frag.appendChild(cloneCurrent);
    }
    else {
        // Copy nodes between the start/end offsets.
        DOM_Node n = getSelectedNode( fStartContainer, fStartOffset );
        int cnt = fEndOffset - fStartOffset;
        while( cnt > 0 )
        {
            DOM_Node sibling = n.getNextSibling();
            DOM_Node xferNode = traverseFullySelected( n, how );
            if ( frag!=null )
                frag.appendChild( xferNode );
            --cnt;
            n = sibling;
            }
    }

    // Nothing is partially selected, so collapse to start point
    if ( how != CLONE_CONTENTS )
            collapse(true);
    return frag;
}

/**
 * Visits the nodes selected by this range when we know
 * a-priori that the start and end containers are not the
 * same, but the start container is an ancestor of the end container
 *
 */
DOM_DocumentFragment RangeImpl::traverseCommonStartContainer( DOM_Node endAncestor, int how )
{
    DOM_DocumentFragment frag = null;
    if ( how!=DELETE_CONTENTS)
        frag = fDocument.createDocumentFragment();
    DOM_Node n = traverseRightBoundary( endAncestor, how );
    if ( frag!=null )
        frag.appendChild( n );

    int endIdx = indexOf( endAncestor, fStartContainer );
    int cnt = endIdx - fStartOffset;
    if ( cnt <=0 )
    {
        // Collapse to just before the endAncestor, which
        // is partially selected.
        if ( how != CLONE_CONTENTS )
        {
            setEndBefore( endAncestor );
            collapse( false );
        }
        return frag;
    }

    n = endAncestor.getPreviousSibling();
    while( cnt > 0 )
    {
        DOM_Node sibling = n.getPreviousSibling();
        DOM_Node xferNode = traverseFullySelected( n, how );
        if ( frag!=null )
            frag.insertBefore( xferNode, frag.getFirstChild() );
        --cnt;
        n = sibling;
    }
    // Collapse to just before the endAncestor, which
    // is partially selected.
    if ( how != CLONE_CONTENTS )
    {
        setEndBefore( endAncestor );
        collapse( false );
    }
    return frag;
}

/**
 * Visits the nodes selected by this range when we know
 * a-priori that the start and end containers are not the
 * same, but the end container is an ancestor of the start container
 *
 */
DOM_DocumentFragment RangeImpl::traverseCommonEndContainer( DOM_Node startAncestor, int how )
{
    DOM_DocumentFragment frag = null;
    if ( how!=DELETE_CONTENTS)
        frag = fDocument.createDocumentFragment();
    DOM_Node n = traverseLeftBoundary( startAncestor, how );
    if ( frag!=null )
        frag.appendChild( n );
    int startIdx = indexOf( startAncestor, fEndContainer );
    ++startIdx;  // Because we already traversed it....

    int cnt = fEndOffset - startIdx;
    n = startAncestor.getNextSibling();
    while( cnt > 0 )
    {
        DOM_Node sibling = n.getNextSibling();
        DOM_Node xferNode = traverseFullySelected( n, how );
        if ( frag!=null )
            frag.appendChild( xferNode );
        --cnt;
        n = sibling;
    }

    if ( how != CLONE_CONTENTS )
    {
        setStartAfter( startAncestor );
        collapse( true );
    }

    return frag;
}

/**
 * Visits the nodes selected by this range when we know
 * a-priori that the start and end containers are not
 * the same, and we also know that neither the start
 * nor end container is an ancestor of the other.
 */
DOM_DocumentFragment RangeImpl::traverseCommonAncestors( DOM_Node startAncestor, DOM_Node endAncestor, int how )
{
    DOM_DocumentFragment frag = null;
    if ( how!=DELETE_CONTENTS)
        frag = fDocument.createDocumentFragment();

    DOM_Node n = traverseLeftBoundary( startAncestor, how );
    if ( frag!=null )
        frag.appendChild( n );

    DOM_Node commonParent = startAncestor.getParentNode();
    int startOffset = indexOf( startAncestor, commonParent );
    int endOffset = indexOf( endAncestor, commonParent );
    ++startOffset;

    int cnt = endOffset - startOffset;
    DOM_Node sibling = startAncestor.getNextSibling();

    while( cnt > 0 )
    {
        DOM_Node nextSibling = sibling.getNextSibling();
        n = traverseFullySelected( sibling, how );
        if ( frag!=null )
            frag.appendChild( n );
        sibling = nextSibling;
        --cnt;
    }

    n = traverseRightBoundary( endAncestor, how );
    if ( frag!=null )
        frag.appendChild( n );

    if ( how != CLONE_CONTENTS )
    {
        setStartAfter( startAncestor );
        collapse( true );
    }
    return frag;
}

/**
 * Traverses the "right boundary" of this range and
 * operates on each "boundary node" according to the
 * how parameter.  It is a-priori assumed
 * by this method that the right boundary does
 * not contain the range's start container.
 *
 * A "right boundary" is best visualized by thinking
 * of a sample tree:
 *                 A
 *                /|\
 *               / | \
 *              /  |  \
 *             B   C   D
 *            /|\     /|\
 *           E F G   H I J
 *
 * Imagine first a range that begins between the
 * "E" and "F" nodes and ends between the
 * "I" and "J" nodes.  The start container is
 * "B" and the end container is "D".  Given this setup,
 * the following applies:
 *
 * Partially Selected Nodes: B, D<br>
 * Fully Selected Nodes: F, G, C, H, I
 *
 * The "right boundary" is the highest subtree node
 * that contains the ending container.  The root of
 * this subtree is always partially selected.
 *
 * In this example, the nodes that are traversed
 * as "right boundary" nodes are: H, I, and D.
 *
 */
DOM_Node RangeImpl::traverseRightBoundary( DOM_Node root, int how )
{
    DOM_Node next = getSelectedNode( fEndContainer, fEndOffset-1 );
    bool isFullySelected = ( next!=fEndContainer );

    if ( next==root )
        return traverseNode( next, isFullySelected, false, how );

    DOM_Node parent = next.getParentNode();
    DOM_Node clonedParent = traverseNode( parent, false, false, how );

    while( parent!=null )
    {
        while( next!=null )
        {
            DOM_Node prevSibling = next.getPreviousSibling();
            DOM_Node clonedChild =
                traverseNode( next, isFullySelected, false, how );
            if ( how!=DELETE_CONTENTS )
            {
                clonedParent.insertBefore(
                    clonedChild,
                    clonedParent.getFirstChild()
                );
            }
            isFullySelected = true;
            next = prevSibling;
        }
        if ( parent==root )
            return clonedParent;

        next = parent.getPreviousSibling();
        parent = parent.getParentNode();
        DOM_Node clonedGrandParent = traverseNode( parent, false, false, how );
        if ( how!=DELETE_CONTENTS )
            clonedGrandParent.appendChild( clonedParent );
        clonedParent = clonedGrandParent;

    }

    // should never occur
    return null;
}

/**
 * Traverses the "left boundary" of this range and
 * operates on each "boundary node" according to the
 * how parameter.  It is a-priori assumed
 * by this method that the left boundary does
 * not contain the range's end container.
 *
 * A "left boundary" is best visualized by thinking
 * of a sample tree:
 *
 *                 A
 *                /|\
 *               / | \
 *              /  |  \
 *             B   C   D
 *            /|\     /|\
 *           E F G   H I J
 *
 * Imagine first a range that begins between the
 * "E" and "F" nodes and ends between the
 * "I" and "J" nodes.  The start container is
 * "B" and the end container is "D".  Given this setup,
 * the following applies:
 *
 * Partially Selected Nodes: B, D<br>
 * Fully Selected Nodes: F, G, C, H, I
 *
 * The "left boundary" is the highest subtree node
 * that contains the starting container.  The root of
 * this subtree is always partially selected.
 *
 * In this example, the nodes that are traversed
 * as "left boundary" nodes are: F, G, and B.
 *
 */
DOM_Node RangeImpl::traverseLeftBoundary( DOM_Node root, int how )
{
    DOM_Node next = getSelectedNode( getStartContainer(), getStartOffset() );
    bool isFullySelected = ( next!=getStartContainer() );

    if ( next==root )
        return traverseNode( next, isFullySelected, true, how );

    DOM_Node parent = next.getParentNode();
    DOM_Node clonedParent = traverseNode( parent, false, true, how );

    while( parent!=null )
    {
        while( next!=null )
        {
            DOM_Node nextSibling = next.getNextSibling();
            DOM_Node clonedChild =
                traverseNode( next, isFullySelected, true, how );
            if ( how!=DELETE_CONTENTS )
                clonedParent.appendChild(clonedChild);
            isFullySelected = true;
            next = nextSibling;
        }
        if ( parent==root )
            return clonedParent;

        next = parent.getNextSibling();
        parent = parent.getParentNode();
        DOM_Node clonedGrandParent = traverseNode( parent, false, true, how );
        if ( how!=DELETE_CONTENTS )
            clonedGrandParent.appendChild( clonedParent );
        clonedParent = clonedGrandParent;

    }

    // should never occur
    return null;

}

/**
 * Utility method for traversing a single node.
 * Does not properly handle a text node containing both the
 * start and end offsets.  Such nodes should
 * have been previously detected and been routed to traverseTextNode.
 *
 */
DOM_Node RangeImpl::traverseNode( DOM_Node n, bool isFullySelected, bool isLeft, int how )
{
    if ( isFullySelected )
        return traverseFullySelected( n, how );
    if ( n.getNodeType()== DOM_Node::TEXT_NODE )
        return traverseTextNode( n, isLeft, how );
    return traversePartiallySelected( n, how );
}

/**
 * Utility method for traversing a single node when
 * we know a-priori that the node if fully
 * selected.
 *
 */
DOM_Node RangeImpl::traverseFullySelected( DOM_Node n, int how )
{
    switch( how )
    {
    case CLONE_CONTENTS:
        return n.cloneNode( true );
    case EXTRACT_CONTENTS:
        if ( n.getNodeType()== DOM_Node::DOCUMENT_TYPE_NODE )
        {
            throw DOM_DOMException(
                DOM_DOMException::HIERARCHY_REQUEST_ERR, null);
        }
        return n;
    case DELETE_CONTENTS:
        n.getParentNode().removeChild(n);
        return null;
    }
    return null;
}

/**
 * Utility method for traversing a single node when
 * we know a-priori that the node if partially
 * selected and is not a text node.
 *
 */
DOM_Node RangeImpl::traversePartiallySelected( DOM_Node n, int how )
{
    switch( how )
    {
    case DELETE_CONTENTS:
        return null;
    case CLONE_CONTENTS:
    case EXTRACT_CONTENTS:
        return n.cloneNode( false );
    }
    return null;
}

/**
 * Utility method for traversing a text node that we know
 * a-priori to be on a left or right boundary of the range.
 * This method does not properly handle text nodes that contain
 * both the start and end points of the range.
 *
 */
DOM_Node RangeImpl::traverseTextNode( DOM_Node n, bool isLeft, int how )
{
    DOMString txtValue = n.getNodeValue();
    DOMString newNodeValue;
    DOMString oldNodeValue;

    if ( isLeft )
    {
        int offset = getStartOffset();
        newNodeValue = txtValue.substringData( offset , fStartContainer.getNodeValue().length()-offset);
        oldNodeValue = txtValue.substringData( 0, offset );
    }
    else
    {
        int offset = getEndOffset();
        newNodeValue = txtValue.substringData( 0, offset );
        oldNodeValue = txtValue.substringData( offset , fEndContainer.getNodeValue().length()-offset );
    }

    if ( how != CLONE_CONTENTS )
        n.setNodeValue( oldNodeValue );
    if ( how==DELETE_CONTENTS )
        return null;
    DOM_Node newNode = n.cloneNode( false );
    newNode.setNodeValue( newNodeValue );
    return newNode;
}

/**
 * Utility method to retrieve a child node by index.  This method
 * assumes the caller is trying to find out which node is
 * selected by the given index.  Note that if the index is
 * greater than the number of children, this implies that the
 * first node selected is the parent node itself.
 *
 */
DOM_Node RangeImpl::getSelectedNode( DOM_Node container, int offset )
{
    if ( container.getNodeType() == DOM_Node::TEXT_NODE )
        return container;

    // This case is an important convenience for
    // traverseRightBoundary()
    if ( offset<0 )
        return container;

    DOM_Node child = container.getFirstChild();
    while( child!=null && offset > 0 )
    {
        --offset;
        child = child.getNextSibling();
    }
    if ( child!=null )
        return child;
    return container;
}

void RangeImpl::checkReadOnly(DOM_Node& start, DOM_Node& end,
                              unsigned int startOffset, unsigned int endOffset)
{
    if ((start == null) || (end == null) ) return;
    //if both start and end are text check and return
    if (start.getNodeType() == DOM_Node::TEXT_NODE) {
        if (start.fImpl->isReadOnly()) {
            throw DOM_DOMException(
                DOM_DOMException::NO_MODIFICATION_ALLOWED_ERR, null);
        }
        if (start == end)
            return;
    }
    //set the start and end nodes to check
    DOM_Node sNode = start.getFirstChild();
    for(unsigned int i = 0; i<startOffset; i++)
        sNode = sNode.getNextSibling();

    DOM_Node eNode;
    if (end.getNodeType() == DOM_Node::TEXT_NODE) {
        eNode = end; //need to check only till this node
    }
    else { //need to check all the kids that fall before the end offset value
        eNode = end.getFirstChild();
        for (unsigned int i = 0; i<endOffset-1; i++)
            eNode = eNode.getNextSibling();
    }
    //recursivly search if any node is readonly
    recurseTreeAndCheck(sNode, eNode);
}

void RangeImpl::recurseTreeAndCheck(DOM_Node& start, DOM_Node& end)
{
    for(DOM_Node node=start; node != null && node !=end; node=node.getNextSibling())
    {
        if (node.fImpl->isReadOnly()) {
            throw DOM_DOMException(
                DOM_DOMException::NO_MODIFICATION_ALLOWED_ERR, null);
        }

        if (node.hasChildNodes()) {
            node = node.getFirstChild();
            recurseTreeAndCheck(node, end);
        }
    }
}


DOM_Node RangeImpl::removeChild(DOM_Node& parent, DOM_Node& child)
{
    fRemoveChild = child; //only a precaution measure not to update this range data before removal
    DOM_Node n = parent.removeChild(child);
    fRemoveChild = null;
    return n;
}


//
// Mutation functions
//


/* This function is called from DOM.
*  The  text has already beeen replaced.
*  Fix-up any offsets.
*/
void RangeImpl::receiveReplacedText(NodeImpl* node)
{
    if (node == null) return;
    DOM_Node anode(node);
    if (anode == fStartContainer
        && fStartContainer.getNodeType() == DOM_Node::TEXT_NODE) {
        fStartOffset = 0;
    }
    if (anode == fEndContainer
        && fEndContainer.getNodeType() == DOM_Node::TEXT_NODE) {
        fEndOffset = 0;
    }
}


/** This function is called from DOM.
*  The  text has already beeen inserted.
*  Fix-up any offsets.
*/
void RangeImpl::updateRangeForDeletedText(DOM_Node& node, unsigned int offset, int count)
{
    if (node == null) return;

    if (node == fStartContainer
        && fStartContainer.getNodeType() == DOM_Node::TEXT_NODE) {
        if (fStartOffset > offset+count) {
            fStartOffset = fStartOffset-count;
        } else if (fStartOffset > offset) {
            fStartOffset = offset;
        }
    }
    if (node == fEndContainer
        && fEndContainer.getNodeType() == DOM_Node::TEXT_NODE) {
        if (fEndOffset > offset+count) {
            fEndOffset = fEndOffset-count;
        } else if (fEndOffset > offset) {
            fEndOffset = offset;
        }
    }
}



/** This function must be called by the DOM _BEFORE_
*  a node is deleted, because at that time it is
*  connected in the DOM tree, which we depend on.
*/
void RangeImpl::updateRangeForDeletedNode(NodeImpl* node)
{

    if (node == null) return;
    if (fRemoveChild == node) return;

    DOM_Node tNode(node);

    if (node->getParentNode() == fStartContainer.fImpl) {
        unsigned short index = indexOf(tNode, fStartContainer);
        if ( fStartOffset > index) {
            fStartOffset--;
        }
    }

    if (node->getParentNode() == fEndContainer.fImpl) {
        unsigned short index = indexOf(tNode, fEndContainer);
        if ( fEndOffset > index) {
            fEndOffset--;
        }
    }

    if (node->getParentNode() != fStartContainer.fImpl
        ||  node->getParentNode() != fEndContainer.fImpl) {
        if (isAncestorOf(node, fStartContainer)) {
            DOM_Node tpNode(node->getParentNode());
            setStartContainer( tpNode );
            fStartOffset = indexOf( tNode, tpNode);
        }
        if (isAncestorOf(node, fEndContainer)) {
            DOM_Node tpNode(node->getParentNode());
            setEndContainer( tpNode );
            fEndOffset = indexOf( tNode, tpNode);
        }
    }

}

void RangeImpl::updateRangeForInsertedNode(NodeImpl* node) {
    if (node == null) return;

    if (node->getParentNode() == fStartContainer.fImpl) {
        unsigned int index = indexOf(DOM_Node(node), fStartContainer);
        if (index < fStartOffset) {
            fStartOffset++;
        }
    }

    if (node->getParentNode() == fEndContainer.fImpl) {
        unsigned int index = indexOf(DOM_Node(node), fEndContainer);
        if (index < fEndOffset) {
            fEndOffset++;
        }
    }
}


void RangeImpl::updateSplitInfo(TextImpl* oldNode, TextImpl* startNode, unsigned int offset)
{
    if (startNode == null) return;

    DOM_Text oldText(oldNode);
    DOM_Text newText(startNode);

    if (fStartContainer == oldText && fStartOffset > offset) {
          fStartOffset = fStartOffset - offset;
        fStartContainer = newText;
    }

    if (fEndContainer == oldText && fEndOffset > offset) {
            fEndContainer = newText;
       fEndOffset = fEndOffset - offset;
    }
}


XERCES_CPP_NAMESPACE_END


