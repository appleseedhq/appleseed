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
 * $Id: DOM_DocumentType.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "DOM_DocumentType.hpp"
#include "DocumentTypeImpl.hpp"
#include "DOM_NamedNodeMap.hpp"
#include <assert.h>

XERCES_CPP_NAMESPACE_BEGIN


DOM_DocumentType::DOM_DocumentType()
: DOM_Node(null)
{
};


DOM_DocumentType::DOM_DocumentType(int nullPointer)
: DOM_Node(null)
{
    //Note: the assert below has no effect in release build
    //needs to revisit later
    assert(nullPointer == 0);
};


DOM_DocumentType::DOM_DocumentType(const DOM_DocumentType & other)
: DOM_Node(other)
{
};


DOM_DocumentType::DOM_DocumentType(DocumentTypeImpl *impl) :
        DOM_Node(impl)
{
};


DOM_DocumentType::~DOM_DocumentType()
{
};


DOM_DocumentType & DOM_DocumentType::operator = (const DOM_DocumentType & other)
{
     return (DOM_DocumentType &) DOM_Node::operator = (other);
};


DOM_DocumentType & DOM_DocumentType::operator = (const DOM_NullPtr *other)
{
     return (DOM_DocumentType &) DOM_Node::operator = (other);
};


DOMString       DOM_DocumentType::getName() const
{
        return ((DocumentTypeImpl *)fImpl)->getName().clone();
};



DOM_NamedNodeMap DOM_DocumentType::getEntities() const
{
        return DOM_NamedNodeMap(((DocumentTypeImpl *)fImpl)->getEntities());
};



DOM_NamedNodeMap DOM_DocumentType::getNotations() const
{
        return DOM_NamedNodeMap(((DocumentTypeImpl *)fImpl)->getNotations());
};


//Introduced in DOM Level 2

DOMString     DOM_DocumentType::getPublicId() const
{
        return ((DocumentTypeImpl *)fImpl)->getPublicId().clone();
}


DOMString     DOM_DocumentType::getSystemId() const
{
        return ((DocumentTypeImpl *)fImpl)->getSystemId().clone();
}


DOMString     DOM_DocumentType::getInternalSubset() const
{
        return ((DocumentTypeImpl *)fImpl)->getInternalSubset().clone();
}

XERCES_CPP_NAMESPACE_END

