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
 * $Id: NotationImpl.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "NotationImpl.hpp"
#include "DOM_DOMException.hpp"
#include "DOM_Node.hpp"


XERCES_CPP_NAMESPACE_BEGIN


/**
* Notations are how the Document Type Description (DTD) records hints
* about the format of an XML "unparsed entity" -- in other words,
* non-XML data bound to this document type, which some applications
* may wish to consult when manipulating the document. A Notation
* represents a name-value pair, with its nodeName being set to the
* declared name of the notation.
* <P>
* Notations are also used to formally declare the "targets" of
* Processing Instructions.
* <P>
* Note that the Notation's data is non-DOM information; the DOM only
* records what and where it is.
* <P>
* See the XML 1.0 spec, sections 4.7 and 2.6, for more info.
* <P>
* Level 1 of the DOM does not support editing Notation contents.
*
* @author Rania Y. Khalaf
* @author Joseph Kesselman
* @since  PR-DOM-Level-1-19980818.
*/

NotationImpl::NotationImpl(DocumentImpl *ownerDoc, const DOMString &nName)
    : NodeImpl(ownerDoc)
{
    name = nName.clone();
}

NotationImpl::NotationImpl(const NotationImpl &other, bool /*deep*/)
    : NodeImpl(other)
{
    name = other.name.clone();
}


NotationImpl::~NotationImpl()
{
}


NodeImpl *NotationImpl::cloneNode(bool deep)
{
    return new NotationImpl(*this, deep);
}


DOMString NotationImpl::getNodeName() {
    return name;
}


short NotationImpl::getNodeType() {
    return DOM_Node::NOTATION_NODE;
}


// Notation nodes do not have a parent
NodeImpl * NotationImpl::getParentNode()
{
    return 0;
}


DOMString NotationImpl::getPublicId()
{
    return publicId.clone();
}


DOMString NotationImpl::getSystemId()
{
    return systemId.clone();
}


void NotationImpl::setPublicId(const DOMString &arg)
{
    if(isReadOnly())
        throw DOM_DOMException(
        DOM_DOMException::NO_MODIFICATION_ALLOWED_ERR,null);

    publicId = arg.clone();
}


void NotationImpl::setSystemId(const DOMString &arg)
{
    if(isReadOnly())
        throw DOM_DOMException(
        DOM_DOMException::NO_MODIFICATION_ALLOWED_ERR,null);

    systemId = arg.clone();
}

XERCES_CPP_NAMESPACE_END

