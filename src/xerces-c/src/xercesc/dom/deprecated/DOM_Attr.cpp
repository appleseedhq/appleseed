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
 * $Id: DOM_Attr.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "DOM_Attr.hpp"
#include "AttrImpl.hpp"

XERCES_CPP_NAMESPACE_BEGIN



DOM_Attr::DOM_Attr()
: DOM_Node(null)
{
};


DOM_Attr::DOM_Attr(const DOM_Attr & other)
: DOM_Node(other)
{
};


DOM_Attr::DOM_Attr(AttrImpl *impl) :
        DOM_Node(impl)
{
};


DOM_Attr::~DOM_Attr()
{
};


DOM_Attr & DOM_Attr::operator = (const DOM_Attr & other)
{
    return (DOM_Attr &) DOM_Node::operator = (other);
};


DOM_Attr & DOM_Attr::operator = (const DOM_NullPtr *other)
{
    return (DOM_Attr &) DOM_Node::operator = (other);
};



DOMString       DOM_Attr::getName() const
{
    return ((AttrImpl *)fImpl)->getName().clone();
};


bool       DOM_Attr::getSpecified() const
{
    return ((AttrImpl *)fImpl)->getSpecified();
};


DOMString   DOM_Attr::getValue() const
{
    // The value of an attribute does not need to be cloned before
    //  returning, because it is computed dynamically from the
    //  children of the attribute.
    //
    return ((AttrImpl *)fImpl)->getValue();
};


void     DOM_Attr::setValue(const DOMString &value) {
    ((AttrImpl *)fImpl)->setValue(value);
};


//Introduced in DOM Level 2

DOM_Element     DOM_Attr::getOwnerElement() const
{
    return DOM_Element(((AttrImpl *)fImpl)->getOwnerElement());
}

XERCES_CPP_NAMESPACE_END

