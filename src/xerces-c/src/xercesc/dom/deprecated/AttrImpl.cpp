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
 * $Id: AttrImpl.cpp 568078 2007-08-21 11:43:25Z amassari $
 *
 * <p><b>WARNING</b>: Some of the code here is partially duplicated in
 * ParentNode, be careful to keep these two classes in sync!
 */

#include "AttrImpl.hpp"
#include "DOM_DOMException.hpp"
#include "DocumentImpl.hpp"
#include "TextImpl.hpp"
#include "ElementImpl.hpp"
#include "DStringPool.hpp"
#include "NodeIDMap.hpp"
#include "RangeImpl.hpp"

XERCES_CPP_NAMESPACE_BEGIN


/*
 * The handling of the value field being either the first child node (a
 * ChildNode*) or directly the value (a DOMString) is rather tricky. In the
 * DOMString case we need to get the field in the right type so that the
 * compiler is happy and the appropriate operator gets called. This is
 * essential for the reference counts of the DOMStrings involved to be updated
 * as due.
 * This is consistently achieved by taking the address of the value field and
 * changing it into a DOMString*, and then dereferencing it to get a DOMString.
 * The typical piece of code is:
 * DOMString *x = (DomString *)&value;
 *  ... use of *x which is the DOMString ...
 * This was amended by neilg after memory management was
 * introduced.  Now a union exists which is either a 
 * DOMString * or a ChildNode *.  This will be less efficient
 * (one more dereference per access) but actually works on all the
 * compilers we support.
 */

AttrImpl::AttrImpl(DocumentImpl *ownerDoc, const DOMString &aName)
    : NodeImpl (ownerDoc)
{
    name = aName.clone();
    isSpecified(true);
    hasStringValue(true);
    value.child = null;
};

AttrImpl::AttrImpl(const AttrImpl &other, bool /*deep*/)
    : NodeImpl(other)
{
    name = other.name.clone();
	
    isSpecified(other.isSpecified());

    /* We must initialize the void* value to null in *all* cases. Failing to do
     * so would cause, in case of assignment to a DOMString later, its content
     * to be derefenced as a DOMString, which would lead the ref count code to
     * be called on something that is not actually a DOMString... Really bad
     * things would then happen!!!
     */
    value.child = null;
    hasStringValue(other.hasStringValue());

    if (other.isIdAttr())
    {
        isIdAttr(true);
        this->getOwnerDocument()->getNodeIDMap()->add(this);
    }

    // take care of case where there are kids
    if (!hasStringValue()) {
        cloneChildren(other);
    }
    else {
        if(other.value.str == null) 
        {
            if(value.str != null)
            {
                *(value.str) = null;
                delete value.str;
                value.str = null;
            }
       }
       else
       {
            // get the address of the value field of this as a DOMString*
            DOMString *x = (value.str == null
                ?(value.str = new (getOwnerDocument()->getMemoryManager()) DOMString())
                :value.str
            );
            // and the address of the value field of other as a DOMString*
            DOMString *y = other.value.str;
            // We can now safely do the cloning and assignement, both operands
            // being a DOMString their ref counts will be updated appropriately
            *x = y->clone();
        }
    }
};


AttrImpl::~AttrImpl() {
    if (hasStringValue()) {
        // if value is a DOMString we must make sure its ref count is updated.
        // this is achieved by changing the address of the value field into a
        // DOMString* and setting the value field to null
        if(value.str != null) 
        {
            *(value.str) = null;
            delete value.str;
            value.str = null;
        }
    }
}


// create a real Text node as child if we don't have one yet
void AttrImpl::makeChildNode() {
    if (hasStringValue()) {
        if (value.child != null) {
            // change the address of the value field into a DOMString*
            DOMString *x = (value.str == null
                ?(value.str = new (getOwnerDocument()->getMemoryManager()) DOMString())
                :value.str
            );
            // create a Text node passing the DOMString it points to
            TextImpl *text =
              (TextImpl *) getOwnerDocument()->createTextNode(*x);
            // get the DOMString ref count to be updated by setting the value
            // field to null
            *x = null;
            delete x;
            // finally reassign the value to the node address
            value.child = text;
            text->isFirstChild(true);
            text->previousSibling = text;
            text->ownerNode = this;
            text->isOwned(true);
        }
        hasStringValue(false);
    }
}

NodeImpl * AttrImpl::cloneNode(bool deep)
{
    return new (getOwnerDocument()->getMemoryManager()) AttrImpl(*this, deep);
};


DOMString AttrImpl::getNodeName() {
    return name;
};


short AttrImpl::getNodeType() {
    return DOM_Node::ATTRIBUTE_NODE;
};


DOMString AttrImpl::getName()
{
    return name;
};


DOMString AttrImpl::getNodeValue()
{
    return getValue();
};


bool AttrImpl::getSpecified()
{
    return isSpecified();
};




DOMString AttrImpl::getValue()
{
    if (value.child == null) {
        return 0; // return "";
    }
    if (hasStringValue()) {
        // change value into a DOMString*
        DOMString *x = (value.str == null
            ?(value.str = new (getOwnerDocument()->getMemoryManager()) DOMString())
            :value.str
        );
        // return the DOMString it points to
        return *x;
    }
    ChildNode *firstChild = value.child;
    ChildNode *node = firstChild->nextSibling;
    if (node == null) {
        return firstChild->getNodeValue().clone();
    }
    int             length = 0;
    for (node = firstChild; node != null; node = node->nextSibling)
        length += node->getNodeValue().length();

    DOMString retString;
    retString.reserve(length);
    for (node = firstChild; node != null; node = node->nextSibling)
    {
        retString.appendData(node->getNodeValue());
    };

    return retString;
};


bool AttrImpl::isAttrImpl()
{
    return true;
};


void AttrImpl::setNodeValue(const DOMString &val)
{
    setValue(val);
};



void AttrImpl::setSpecified(bool arg)
{
    isSpecified(arg);
};



void AttrImpl::setValue(const DOMString &newvalue)
{
    if (isReadOnly())
    {
        throw DOM_DOMException
        (
            DOM_DOMException::NO_MODIFICATION_ALLOWED_ERR, null
        );
    }

    //  If this attribute was of type ID and in the map, take it out,
    //    then put it back in with the new name.  For now, we don't worry
    //    about what happens if the new name conflicts
    //
    if (isIdAttr())
        this->getOwnerDocument()->getNodeIDMap()->remove(this);

    if (!hasStringValue() && value.str != null) {
        NodeImpl *kid;
        while ((kid = value.child) != null) { // Remove existing kids
            removeChild(kid);
            if (kid->nodeRefCount == 0)
                NodeImpl::deleteIf(kid);
        }
    }

    // directly store the string as the value by changing the value field
    // into a DOMString
    DOMString *x = (value.str == null 
        ?(value.str = new (getOwnerDocument()->getMemoryManager()) DOMString())
        :value.str
    );
    if (newvalue != null) {
        *x = newvalue.clone();
    }
    else {
        *x = null;
        delete x;
        value.str = null;
    }
    hasStringValue(true);
    isSpecified(true);
    changed();

    if (isIdAttr())
        this->getOwnerDocument()->getNodeIDMap()->add(this);

};



DOMString AttrImpl::toString()
{
    DOMString retString;

    retString.appendData(name);
    retString.appendData(DOMString("=\""));
    retString.appendData(getValue());
    retString.appendData(DOMString("\""));
    return retString;
}


//Introduced in DOM Level 2

ElementImpl *AttrImpl::getOwnerElement()
{
    // if we have an owner, ownerNode is our ownerElement, otherwise it's
    // our ownerDocument and we don't have an ownerElement
    return (ElementImpl *) (isOwned() ? ownerNode : null);
}


//internal use by parser only
void AttrImpl::setOwnerElement(ElementImpl *ownerElem)
{
    ownerNode = ownerElem;
    isOwned(false);
}


// ParentNode stuff

void AttrImpl::cloneChildren(const NodeImpl &other) {
  //    for (NodeImpl *mykid = other.getFirstChild();
    for (NodeImpl *mykid = ((NodeImpl&)other).getFirstChild();
         mykid != null;
         mykid = mykid->getNextSibling()) {
        this->appendChild(mykid->cloneNode(true));
    }
}

NodeListImpl *AttrImpl::getChildNodes() {
    return this;
}


NodeImpl * AttrImpl::getFirstChild() {
    makeChildNode();
    return value.child;
}


NodeImpl * AttrImpl::getLastChild() {
    return lastChild();
}

ChildNode * AttrImpl::lastChild() {
    // last child is stored as the previous sibling of first child
    makeChildNode();
    return value.child != null ? (value.child)->previousSibling : null;
}

void AttrImpl::lastChild(ChildNode *node) {
    // store lastChild as previous sibling of first child
    if (value.child != null) {
        (value.child)->previousSibling = node;
    }
}

unsigned int AttrImpl::getLength() {
    if (hasStringValue()) {
        return 1;
    }
    ChildNode *node = value.child;
    int length = 0;
    while (node != null) {
        length++;
        node = node->nextSibling;
    }
    return length;
}

bool AttrImpl::hasChildNodes()
{
    return value.child != null;
};



NodeImpl *AttrImpl::insertBefore(NodeImpl *newChild, NodeImpl *refChild) {

    DocumentImpl *ownerDocument = getOwnerDocument();
    bool errorChecking = ownerDocument->getErrorChecking();

    if (newChild->isDocumentFragmentImpl()) {
        // SLOW BUT SAFE: We could insert the whole subtree without
        // juggling so many next/previous pointers. (Wipe out the
        // parent's child-list, patch the parent pointers, set the
        // ends of the list.) But we know some subclasses have special-
        // case behavior they add to insertBefore(), so we don't risk it.
        // This approch also takes fewer bytecodes.

        // NOTE: If one of the children is not a legal child of this
        // node, throw HIERARCHY_REQUEST_ERR before _any_ of the children
        // have been transferred. (Alternative behaviors would be to
        // reparent up to the first failure point or reparent all those
        // which are acceptable to the target node, neither of which is
        // as robust. PR-DOM-0818 isn't entirely clear on which it
        // recommends?????

        // No need to check kids for right-document; if they weren't,
        // they wouldn't be kids of that DocFrag.
        if (errorChecking) {
            for (NodeImpl *kid = newChild->getFirstChild(); // Prescan
                 kid != null; kid = kid->getNextSibling()) {

                if (!DocumentImpl::isKidOK(this, kid)) {
                    throw DOM_DOMException(
                                       DOM_DOMException::HIERARCHY_REQUEST_ERR,
                                       null);
                }
            }
        }

        while (newChild->hasChildNodes()) {    // Move
            insertBefore(newChild->getFirstChild(), refChild);
        }
        return newChild;
    }

    // it's a no-op if refChild is the same as newChild
    if (refChild == newChild) {
        return newChild;
    }

    if (errorChecking) {
        if (isReadOnly()) {
            throw DOM_DOMException(
                                 DOM_DOMException::NO_MODIFICATION_ALLOWED_ERR,
                                 null);
        }
        if (newChild->getOwnerDocument() != ownerDocument) {
            throw DOM_DOMException(DOM_DOMException::WRONG_DOCUMENT_ERR, null);
        }
        if (!DocumentImpl::isKidOK(this, newChild)) {
            throw DOM_DOMException(DOM_DOMException::HIERARCHY_REQUEST_ERR,
                                   null);
        }
        // refChild must be a child of this node (or null)
        if (refChild != null && refChild->getParentNode() != this) {
            throw DOM_DOMException(DOM_DOMException::NOT_FOUND_ERR, null);
        }

        // Prevent cycles in the tree
        // newChild cannot be ancestor of this Node,
        // and actually cannot be this
        bool treeSafe = true;
        for (NodeImpl *a = this; treeSafe && a != null; a = a->getParentNode())
        {
            treeSafe = (newChild != a);
        }
        if (!treeSafe) {
            throw DOM_DOMException(DOM_DOMException::HIERARCHY_REQUEST_ERR,
                                   null);
        }
    }

    makeChildNode(); // make sure we have a node and not a string

    // Convert to internal type, to avoid repeated casting
    ChildNode * newInternal = (ChildNode *)newChild;

    NodeImpl *oldparent = newInternal->getParentNode();
    if (oldparent != null) {
        oldparent->removeChild(newInternal);
    }

    // Convert to internal type, to avoid repeated casting
    ChildNode *refInternal = (ChildNode *)refChild;

    // Attach up
    newInternal->ownerNode = this;
    newInternal->isOwned(true);

    // Attach before and after
    // Note: firstChild.previousSibling == lastChild!!
    ChildNode *firstChild = value.child;
    if (firstChild == null) {
        // this our first and only child
        value.child = newInternal; // firstChild = newInternal
        newInternal->isFirstChild(true);
        newInternal->previousSibling = newInternal;
    }
    else {
        if (refInternal == null) {
            // this is an append
            ChildNode *lastChild = firstChild->previousSibling;
            lastChild->nextSibling = newInternal;
            newInternal->previousSibling = lastChild;
            firstChild->previousSibling = newInternal;
        }
        else {
            // this is an insert
            if (refChild == firstChild) {
                // at the head of the list
                firstChild->isFirstChild(false);
                newInternal->nextSibling = firstChild;
                newInternal->previousSibling = firstChild->previousSibling;
                firstChild->previousSibling = newInternal;
                value.child = newInternal; // firstChild = newInternal;
                newInternal->isFirstChild(true);
            }
            else {
                // somewhere in the middle
                ChildNode *prev = refInternal->previousSibling;
                newInternal->nextSibling = refInternal;
                prev->nextSibling = newInternal;
                refInternal->previousSibling = newInternal;
                newInternal->previousSibling = prev;
            }
        }
    }

    changed();

    if (this->getOwnerDocument() != null) {
        typedef RefVectorOf<RangeImpl> RangeImpls;
        RangeImpls* ranges = this->getOwnerDocument()->getRanges();
        if ( ranges != null) {
            unsigned int sz = ranges->size();
            for (unsigned int i =0; i<sz; i++) {
                ranges->elementAt(i)->updateRangeForInsertedNode(newInternal);
            }
        }
    }

    return newInternal;
}


NodeImpl *AttrImpl::item(unsigned int index) {

    if (hasStringValue()) {
        if (index != 0 || value.child == null) {
            return null;
        }
        else {
            makeChildNode();
            return (NodeImpl *) (value.child);
        }
    }
    ChildNode *nodeListNode = value.child;
    for (unsigned int nodeListIndex = 0;
         nodeListIndex < index && nodeListNode != null;
         nodeListIndex++) {
        nodeListNode = nodeListNode->nextSibling;
    }
    return nodeListNode;
}


NodeImpl *AttrImpl::removeChild(NodeImpl *oldChild) {

    DocumentImpl *ownerDocument = getOwnerDocument();
    if (ownerDocument->getErrorChecking()) {
        if (isReadOnly()) {
            throw DOM_DOMException(
                                 DOM_DOMException::NO_MODIFICATION_ALLOWED_ERR,
                                 null);
        }
        if (oldChild == null || oldChild->getParentNode() != this) {
            throw DOM_DOMException(DOM_DOMException::NOT_FOUND_ERR, null);
        }
    }
    // fix other ranges for change before deleting the node
    if (getOwnerDocument() !=  null) {
        typedef RefVectorOf<RangeImpl> RangeImpls;
        RangeImpls* ranges = this->getOwnerDocument()->getRanges();
        if (ranges != null) {
            unsigned int sz = ranges->size();
            if (sz != 0) {
                for (unsigned int i =0; i<sz; i++) {
                    if (ranges->elementAt(i) != null)
                        ranges->elementAt(i)->updateRangeForDeletedNode(oldChild);
                }
            }
        }
    }

    ChildNode * oldInternal = (ChildNode *) oldChild;

    // Patch linked list around oldChild
    // Note: lastChild == firstChild->previousSibling
    if (oldInternal == value.child) {
        // removing first child
        oldInternal->isFirstChild(false);
        value.child = oldInternal->nextSibling; // firstChild = oldInternal->nextSibling
        ChildNode *firstChild = value.child;
        if (firstChild != null) {
            firstChild->isFirstChild(true);
            firstChild->previousSibling = oldInternal->previousSibling;
        }
    } else {
        ChildNode *prev = oldInternal->previousSibling;
        ChildNode *next = oldInternal->nextSibling;
        prev->nextSibling = next;
        if (next == null) {
            // removing last child
            ChildNode *firstChild = value.child;
            firstChild->previousSibling = prev;
        } else {
            // removing some other child in the middle
            next->previousSibling = prev;
        }
    }

    // Remove oldInternal's references to tree
    oldInternal->ownerNode = getOwnerDocument();
    oldInternal->isOwned(false);
    oldInternal->nextSibling = null;
    oldInternal->previousSibling = null;

    changed();

    return oldInternal;
};


NodeImpl *AttrImpl::replaceChild(NodeImpl *newChild, NodeImpl *oldChild) {
    insertBefore(newChild, oldChild);
    if (newChild != oldChild) {
        removeChild(oldChild);
    }
    // changed() already done.
    return oldChild;
}


void AttrImpl::setReadOnly(bool readOnl, bool deep) {
    NodeImpl::setReadOnly(readOnl, deep);

    if (deep) {
        if (hasStringValue()) {
            return;
        }
        // Recursively set kids
        for (ChildNode *mykid = value.child;
             mykid != null;
             mykid = mykid->nextSibling)
            if(! (mykid->isEntityReference()))
                mykid->setReadOnly(readOnl,true);
    }
}


//Introduced in DOM Level 2

void AttrImpl::normalize()
{
    if (hasStringValue()) {
        return;
    }
    ChildNode *kid, *next;
    for (kid = value.child; kid != null; kid = next)
    {
        next = kid->nextSibling;

        // If kid and next are both Text nodes (but _not_ CDATASection,
        // which is a subclass of Text), they can be merged.
        if (next != null &&
            kid->isTextImpl()   && !(kid->isCDATASectionImpl())  &&
            next->isTextImpl()  && !(next->isCDATASectionImpl()) )
        {
            ((TextImpl *) kid)->appendData(((TextImpl *) next)->getData());
            removeChild(next);
            if (next->nodeRefCount == 0)
                deleteIf(next);
            next = kid; // Don't advance; there might be another.
        }

        // Otherwise it might be an Element, which is handled recursively
        else
            if (kid->isElementImpl())
                kid->normalize();
    };

    // changed() will have occurred when the removeChild() was done,
    // so does not have to be reissued.
};

XERCES_CPP_NAMESPACE_END

