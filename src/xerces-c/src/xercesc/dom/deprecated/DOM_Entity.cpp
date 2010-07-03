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
 * $Id: DOM_Entity.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "DOM_Entity.hpp"
#include "EntityImpl.hpp"
#include "DOM_NodeList.hpp"

XERCES_CPP_NAMESPACE_BEGIN


DOM_Entity::DOM_Entity()
: DOM_Node(null)
{
};


DOM_Entity::DOM_Entity(const DOM_Entity & other)
: DOM_Node(other)
{
};


DOM_Entity::DOM_Entity(EntityImpl *impl) :
        DOM_Node(impl)
{
};


DOM_Entity::~DOM_Entity()
{
};


DOM_Entity & DOM_Entity::operator = (const DOM_Entity & other)
{
     return (DOM_Entity &) DOM_Node::operator = (other);
};


DOM_Entity & DOM_Entity::operator = (const DOM_NullPtr *other)
{
     return (DOM_Entity &) DOM_Node::operator = (other);
};


DOMString  DOM_Entity::getPublicId() const
{
        return ((EntityImpl *)fImpl)->getPublicId().clone();
};

DOMString  DOM_Entity::getSystemId() const
{
        return ((EntityImpl *)fImpl)->getSystemId().clone();
};


DOMString  DOM_Entity::getNotationName() const
{
        return ((EntityImpl *)fImpl)->getNotationName().clone();
};

DOM_Node      DOM_Entity::getFirstChild() const
{
    return DOM_Node( ((EntityImpl*)fImpl)->getFirstChild());
};

DOM_Node      DOM_Entity::getLastChild() const
{
    return DOM_Node(((EntityImpl*)fImpl)->getLastChild());
};

DOM_NodeList      DOM_Entity::getChildNodes() const
{
    return DOM_NodeList((EntityImpl*)fImpl);
};

bool           DOM_Entity::hasChildNodes() const
{
    return ((EntityImpl*)fImpl)->hasChildNodes();
};

DOM_Node      DOM_Entity::getPreviousSibling() const
{
    return DOM_Node(((EntityImpl*)fImpl)->getPreviousSibling());
};


DOM_Node       DOM_Entity::getNextSibling() const
{
    return DOM_Node(((EntityImpl*)fImpl)->getNextSibling());
};

XERCES_CPP_NAMESPACE_END

