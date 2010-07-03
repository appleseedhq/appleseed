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
 * $Id: ElementDefinitionImpl.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "ElementDefinitionImpl.hpp"
#include "DocumentImpl.hpp"
#include "NamedNodeMapImpl.hpp"
#include "NodeImpl.hpp"

XERCES_CPP_NAMESPACE_BEGIN



ElementDefinitionImpl::ElementDefinitionImpl(DocumentImpl *ownerDoc,
                                             const DOMString &nam)
    : NodeImpl(ownerDoc)
{
    name = nam.clone();
    attributes = 0;
};


ElementDefinitionImpl::ElementDefinitionImpl(
                                            const ElementDefinitionImpl& other,
                                            bool /*deep*/)
    : NodeImpl(other)
{
    name = other.name.clone();
    // NamedNodeMap must be explicitly replicated to avoid sharing
    attributes = 0;
    if (other.attributes)
        attributes = other.attributes->cloneMap(this);
};


ElementDefinitionImpl::~ElementDefinitionImpl()
{
};


NodeImpl *ElementDefinitionImpl::cloneNode(bool deep)
{
    return new (getOwnerDocument()->getMemoryManager()) ElementDefinitionImpl(*this, deep);
};


DOMString ElementDefinitionImpl::getNodeName() {
    return name;
};


short ElementDefinitionImpl::getNodeType() {
    return (short)-1;
};


NamedNodeMapImpl *ElementDefinitionImpl::getAttributes() {
    return attributes;
};


XERCES_CPP_NAMESPACE_END


