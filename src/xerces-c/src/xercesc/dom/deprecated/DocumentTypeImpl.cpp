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
 * $Id: DocumentTypeImpl.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "DocumentTypeImpl.hpp"
#include "DOM_Node.hpp"
#include "NamedNodeMapImpl.hpp"
#include "DOM_DOMException.hpp"
#include "DocumentImpl.hpp"

XERCES_CPP_NAMESPACE_BEGIN


DocumentTypeImpl::DocumentTypeImpl(DocumentImpl *ownerDoc,
                                   const DOMString &dtName)
    : ParentNode(ownerDoc),
    publicId(null), systemId(null), internalSubset(null) //DOM Level 2
	, intSubsetReading(false)	
{
    name = dtName.clone();
    entities = new NamedNodeMapImpl(this);
    notations = new NamedNodeMapImpl(this);
	elements = new NamedNodeMapImpl(this);

};


//Introduced in DOM Level 2
DocumentTypeImpl::DocumentTypeImpl(DocumentImpl *ownerDoc,
                                   const DOMString &qualifiedName,
                                   const DOMString &pubId,
                                   const DOMString &sysId)
	: ParentNode(ownerDoc),
    publicId(pubId), systemId(sysId), internalSubset(null)
	, intSubsetReading(false)
{
    name = qualifiedName.clone();
    if (DocumentImpl::indexofQualifiedName(qualifiedName) < 0)
        throw DOM_DOMException(DOM_DOMException::NAMESPACE_ERR, null);

    entities = new NamedNodeMapImpl(this);
    notations= new NamedNodeMapImpl(this);
	elements = new NamedNodeMapImpl(this);
};


DocumentTypeImpl::DocumentTypeImpl(const DocumentTypeImpl &other, bool deep)
    : ParentNode(other)
{
    name = other.name.clone();
    if (deep)
        cloneChildren(other);
    entities = other.entities->cloneMap(this);
    notations= other.notations->cloneMap(this);
	elements = other.elements->cloneMap(this);

    //DOM Level 2
    publicId		= other.publicId.clone();
    systemId		= other.systemId.clone();
	internalSubset	= other.internalSubset.clone();
	intSubsetReading = other.intSubsetReading;
};


DocumentTypeImpl::~DocumentTypeImpl()
{
    if (entities != null)
    {
        entities->removeAll();
        NamedNodeMapImpl::removeRef(entities);
    }

    if (notations != null)
    {
        notations->removeAll();
        NamedNodeMapImpl::removeRef(notations);
    }
	if (elements != null)
	{
		elements->removeAll();
		NamedNodeMapImpl::removeRef(elements);
	}
};


NodeImpl *DocumentTypeImpl::cloneNode(bool deep)
{
    return new DocumentTypeImpl(*this, deep);
};

/**
 * NON-DOM
 * set the ownerDocument of this node and its children
 */
void DocumentTypeImpl::setOwnerDocument(DocumentImpl *doc) {
    ParentNode::setOwnerDocument(doc);
    entities->setOwnerDocument(doc);
    notations->setOwnerDocument(doc);
    //    elements->setOwnerDocument(doc);
}

DOMString DocumentTypeImpl::getNodeName()
{
    return name;
};


short DocumentTypeImpl::getNodeType() {
    return DOM_Node::DOCUMENT_TYPE_NODE;
};


NamedNodeMapImpl *DocumentTypeImpl::getEntities()
{
    return entities;
};

NamedNodeMapImpl *DocumentTypeImpl::getElements()
{
    return elements;
};

DOMString DocumentTypeImpl::getName()
{
    return name;
};


NamedNodeMapImpl *DocumentTypeImpl::getNotations()
{
    return notations;
};


bool DocumentTypeImpl::isDocumentTypeImpl()
{
    return true;
};


void DocumentTypeImpl::setReadOnly(bool readOnl, bool deep)
{
    ParentNode::setReadOnly(readOnl,deep);
    entities->setReadOnly(readOnl,true);
    notations->setReadOnly(readOnl,true);
};


//Introduced in DOM Level 2

DOMString DocumentTypeImpl::getPublicId()
{
    return publicId;
}


DOMString DocumentTypeImpl::getSystemId()
{
    return systemId;
}


DOMString DocumentTypeImpl::getInternalSubset()
{
    return internalSubset;
}

bool DocumentTypeImpl::isIntSubsetReading()
{
    return intSubsetReading;
}


//set functions

void        DocumentTypeImpl::setPublicId(const DOMString& value)
{
    if (value == 0)
        return;
    publicId = value.clone();
}

void        DocumentTypeImpl::setSystemId(const DOMString& value)
{
    if (value == 0)
        return;
    systemId = value.clone();
}

void        DocumentTypeImpl::setInternalSubset(const DOMString &value)
{
    if (value == 0)
        return;
    internalSubset = value.clone();
}

XERCES_CPP_NAMESPACE_END

