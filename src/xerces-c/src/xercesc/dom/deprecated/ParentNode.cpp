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
 * $Id: ParentNode.cpp 568078 2007-08-21 11:43:25Z amassari $
 *
 * <p><b>WARNING</b>: Some of the code here is partially duplicated in
 * AttrImpl, be careful to keep these two classes in sync!
 */


#include "ParentNode.hpp"
#include "DOM_DOMException.hpp"
#include "TextImpl.hpp"
#include "DocumentImpl.hpp"
#include "RangeImpl.hpp"

XERCES_CPP_NAMESPACE_BEGIN


ParentNode::ParentNode(DocumentImpl *ownerDoc)
    : ChildNode(ownerDoc)
{
    this->ownerDocument = ownerDoc;
    this->firstChild = null;

    fCachedLength = -1;
    fCachedChild = null;
    fCachedChildIndex = -1;
};

// This only makes a shallow copy, cloneChildren must also be called for a
// deep clone
ParentNode::ParentNode(const ParentNode &other)
    : ChildNode(other)
{
    this->ownerDocument = other.ownerDocument;

    // Need to break the association w/ original kids
    this->firstChild = null;

    fCachedLength = -1;
    fCachedChild = null;
    fCachedChildIndex = -1;
};


void ParentNode::cloneChildren(const NodeImpl &other) {
  //    for (NodeImpl *mykid = other.getFirstChild();
    for (NodeImpl *mykid = ((NodeImpl&)other).getFirstChild();
         mykid != null;
         mykid = mykid->getNextSibling()) {
        this->appendChild(mykid->cloneNode(true));
    }
}

DocumentImpl * ParentNode::getOwnerDocument() {
    return ownerDocument;
}

// unlike getOwnerDocument this is not overriden by DocumentImpl to return null
DocumentImpl * ParentNode::getDocument() {
    return ownerDocument;
}

void ParentNode::setOwnerDocument(DocumentImpl *doc) {
    ownerDocument = doc;
    for (NodeImpl *child = firstChild;
         child != null; child = child->getNextSibling()) {
        child->setOwnerDocument(doc);
    }
}


NodeListImpl *ParentNode::getChildNodes() {
    return this;
};


NodeImpl * ParentNode::getFirstChild() {
    return firstChild;
};


NodeImpl * ParentNode::getLastChild()
{
    return lastChild();
};

ChildNode * ParentNode::lastChild()
{
    // last child is stored as the previous sibling of first child
    return firstChild != null ? firstChild->previousSibling : null;
};

void ParentNode::lastChild(ChildNode *node) {
        // store lastChild as previous sibling of first child
        if (firstChild != null) {
            firstChild->previousSibling = node;
        }
    }


unsigned int ParentNode::getLength() {
    if (fCachedLength == -1) { // is the cached length invalid ?
        ChildNode *node;
        // start from the cached node if we have one
        if (fCachedChildIndex != -1 && fCachedChild != null) {
            fCachedLength = fCachedChildIndex;
            node = fCachedChild;
        } else {
            node = firstChild;
            fCachedLength = 0;
        }
        while (node != null) {
            fCachedLength++;
            node = node->nextSibling;
        }
    }
    return fCachedLength;
};


bool ParentNode::hasChildNodes()
{
    return firstChild!=null;
};



NodeImpl *ParentNode::insertBefore(NodeImpl *newChild, NodeImpl *refChild) {

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
            insertBefore(newChild->getFirstChild(),refChild);
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
    if (firstChild == null) {
        // this our first and only child
        firstChild = newInternal;
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
                firstChild = newInternal;
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

    // update cached length if we have any
    if (fCachedLength != -1) {
        fCachedLength++;
    }
    if (fCachedChildIndex != -1) {
        // if we happen to insert just before the cached node, update
        // the cache to the new node to match the cached index
        if (fCachedChild == refInternal) {
            fCachedChild = newInternal;
        }
        else {
            // otherwise just invalidate the cache
            fCachedChildIndex = -1;
        }
    }

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
};


NodeImpl *ParentNode::item(unsigned int uindex) {
    // short way
    int index = uindex;
    if (fCachedChildIndex != -1 && fCachedChild != null) {
        if (fCachedChildIndex < index) {
            while (fCachedChildIndex < index && fCachedChild != null) {
                fCachedChildIndex++;
                fCachedChild = fCachedChild->nextSibling;
            }
        }
        else if (fCachedChildIndex > index) {
            while (fCachedChildIndex > index && fCachedChild != null) {
                fCachedChildIndex--;
                fCachedChild = (ChildNode *)fCachedChild->getPreviousSibling();
            }
        }
        return fCachedChild;
    }

    // long way
    fCachedChild = firstChild;
    for (fCachedChildIndex = 0;
         fCachedChildIndex < index && fCachedChild != null;
         fCachedChildIndex++) {
        fCachedChild = fCachedChild->nextSibling;
    }
    return fCachedChild;
};


NodeImpl *ParentNode::removeChild(NodeImpl *oldChild)
{
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
    //fix other ranges for change before deleting the node
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

    // update cached length if we have any
    if (fCachedLength != -1) {
        fCachedLength--;
    }
    if (fCachedChildIndex != -1) {
        // if the removed node is the cached node
        // move the cache to its (soon former) previous sibling
        if (fCachedChild == oldInternal) {
            fCachedChildIndex--;
            fCachedChild = (ChildNode *)oldInternal->getPreviousSibling();
        } else {
            // otherwise just invalidate the cache
            fCachedChildIndex = -1;
        }
    }

    // Patch linked list around oldChild
    // Note: lastChild == firstChild->previousSibling
    if (oldInternal == firstChild) {
        // removing first child
        oldInternal->isFirstChild(false);
        firstChild = oldInternal->nextSibling;
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
            firstChild->previousSibling = prev;
        } else {
            // removing some other child in the middle
            next->previousSibling = prev;
        }
    }

    // Remove oldInternal's references to tree
    oldInternal->ownerNode = ownerDocument;
    oldInternal->isOwned(false);
    oldInternal->nextSibling = null;
    oldInternal->previousSibling = null;

    changed();

    return oldInternal;
};


NodeImpl *ParentNode::replaceChild(NodeImpl *newChild, NodeImpl *oldChild)
{
    insertBefore(newChild, oldChild);
    if (newChild != oldChild) {
        removeChild(oldChild);
    }
    // changed() already done.
    return oldChild;
};


void ParentNode::setReadOnly(bool readOnl, bool deep)
{
    NodeImpl::setReadOnly(readOnl, deep);

    if (deep)
        // Recursively set kids
        for (ChildNode *mykid = firstChild;
             mykid != null;
             mykid = mykid->nextSibling)
            if(! (mykid->isEntityReference()))
                mykid->setReadOnly(readOnl,true);
};


//Introduced in DOM Level 2

void ParentNode::normalize()
{
    ChildNode *kid, *next;
    for (kid = firstChild; kid != null; kid = next)
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

