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
 * $Id: EntityReferenceImpl.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

/**
* EntityReference models the XML &entityname; syntax, when used for
* entities defined by the DOM. Entities hardcoded into XML, such as
* character entities, should instead have been translated into text
* by the code which generated the DOM tree.
* <P>
* An XML processor has the alternative of fully expanding Entities
* into the normal document tree. If it does so, no EntityReference nodes
* will appear.
* <P>
* Similarly, non-validating XML processors are not required to read
* or process entity declarations made in the external subset or
* declared in external parameter entities. Hence, some applications
* may not make the replacement value available for Parsed Entities
* of these types.
* <P>
* EntityReference behaves as a read-only node, and the children of
* the EntityReference (which reflect those of the Entity, and should
* also be read-only) give its replacement value, if any. They are
* supposed to automagically stay in synch if the DocumentType is
* updated with new values for the Entity.
* <P>
* The defined behavior makes efficient storage difficult for the DOM
* implementor. We can't just look aside to the Entity's definition
* in the DocumentType since those nodes have the wrong parent (unless
* we can come up with a clever "imaginary parent" mechanism). We
* must at least appear to clone those children... which raises the
* issue of keeping the reference synchronized with its parent.
* This leads me back to the "cached image of centrally defined data"
* solution, much as I dislike it.
* <P>
* For now I have decided, since REC-DOM-Level-1-19980818 doesn't
* cover this in much detail, that synchronization doesn't have to be
* considered while the user is deep in the tree. That is, if you're
* looking within one of the EntityReferennce's children and the Entity
* changes, you won't be informed; instead, you will continue to access
* the same object -- which may or may not still be part of the tree.
* This is the same behavior that obtains elsewhere in the DOM if the
* subtree you're looking at is deleted from its parent, so it's
* acceptable here. (If it really bothers folks, we could set things
* up so deleted subtrees are walked and marked invalid, but that's
* not part of the DOM's defined behavior.)
* <P>
* As a result, only the EntityReference itself has to be aware of
* changes in the Entity. And it can take advantage of the same
* structure-change-monitoring code I implemented to support
* DeepNodeList.
*
* @author Rania Y. Khalaf
* @author Joseph Kesselman
* @since  PR-DOM-Level-1-19980818.
*/

#include "DocumentImpl.hpp"
#include "DocumentTypeImpl.hpp"
#include "EntityImpl.hpp"
#include "EntityReferenceImpl.hpp"
#include "DOM_DOMException.hpp"
#include "DOM_Node.hpp"
#include "NamedNodeMapImpl.hpp"

XERCES_CPP_NAMESPACE_BEGIN


EntityReferenceImpl::EntityReferenceImpl(DocumentImpl *ownerDoc,
                                         const DOMString &entityName)
    : ParentNode(ownerDoc)
{
    name = entityName.clone();
    // EntityReference behaves as a read-only node, since its contents
    // reflect the Entity it refers to -- but see setNodeName().

    //retrieve the corresponding entity content
    if (ownerDoc) {
        if (ownerDoc->getDoctype()) {
            if (ownerDoc->getDoctype()->getEntities()) {
                EntityImpl* entity = (EntityImpl*)ownerDoc->getDoctype()->getEntities()->getNamedItem(entityName);
                if (entity) {
                    cloneChildren(*entity);
                }
            }
        }
    }

    setReadOnly(true, true);
}



EntityReferenceImpl::EntityReferenceImpl(const EntityReferenceImpl &other,
                                         bool deep)
    : ParentNode(other)
{
    name = other.name.clone();
    if (deep)
        cloneChildren(other);
    setReadOnly(true, true);
}



EntityReferenceImpl::~EntityReferenceImpl()
{
}


NodeImpl *EntityReferenceImpl::cloneNode(bool deep)
{
    return new (getOwnerDocument()->getMemoryManager()) EntityReferenceImpl(*this, deep);
}


DOMString EntityReferenceImpl::getNodeName()
{
    return name;
};


short EntityReferenceImpl::getNodeType() {
    return DOM_Node::ENTITY_REFERENCE_NODE;
};


bool EntityReferenceImpl::isEntityReference()
{
    return true;
}


/**
* EntityRef is already, and must be, a read-only node. Attempts to change
* that will throw a NO_MODIFICATION_ALLOWED_ERR DOMException.
* <P>
* If you want to alter its contents, edit the Entity definition.
*
* @param readOnly boolean
*/
void EntityReferenceImpl::setReadOnly(bool readOnl,bool deep)
{
    if(getOwnerDocument()->getErrorChecking() && readOnl==false)
        throw DOM_DOMException(DOM_DOMException::NO_MODIFICATION_ALLOWED_ERR,null);
    ParentNode::setReadOnly(readOnl,deep);
}

XERCES_CPP_NAMESPACE_END

